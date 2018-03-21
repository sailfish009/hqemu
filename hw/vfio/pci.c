/*
 * vfio based device assignment support
 *
 * Copyright Red Hat, Inc. 2012
 *
 * Authors:
 *  Alex Williamson <alex.williamson@redhat.com>
 *
 * This work is licensed under the terms of the GNU GPL, version 2.  See
 * the COPYING file in the top-level directory.
 *
 * Based on qemu-kvm device-assignment:
 *  Adapted for KVM by Qumranet.
 *  Copyright (c) 2007, Neocleus, Alex Novik (alex@neocleus.com)
 *  Copyright (c) 2007, Neocleus, Guy Zana (guy@neocleus.com)
 *  Copyright (C) 2008, Qumranet, Amit Shah (amit.shah@qumranet.com)
 *  Copyright (C) 2008, Red Hat, Amit Shah (amit.shah@redhat.com)
 *  Copyright (C) 2008, IBM, Muli Ben-Yehuda (muli@il.ibm.com)
 */

#include <linux/vfio.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "config.h"
#include "hw/pci/msi.h"
#include "hw/pci/msix.h"
#include "hw/pci/pci_bridge.h"
#include "qemu/error-report.h"
#include "qemu/range.h"
#include "sysemu/kvm.h"
#include "sysemu/sysemu.h"
#include "pci.h"
#include "trace.h"

#define MSIX_CAP_LENGTH 12

static void vfio_disable_interrupts(VFIOPCIDevice *vdev);
static void vfio_mmap_set_enabled(VFIOPCIDevice *vdev, bool enabled);

/*
 * Disabling BAR mmaping can be slow, but toggling it around INTx can
 * also be a huge overhead.  We try to get the best of both worlds by
 * waiting until an interrupt to disable mmaps (subsequent transitions
 * to the same state are effectively no overhead).  If the interrupt has
 * been serviced and the time gap is long enough, we re-enable mmaps for
 * performance.  This works well for things like graphics cards, which
 * may not use their interrupt at all and are penalized to an unusable
 * level by read/write BAR traps.  Other devices, like NICs, have more
 * regular interrupts and see much better latency by staying in non-mmap
 * mode.  We therefore set the default mmap_timeout such that a ping
 * is just enough to keep the mmap disabled.  Users can experiment with
 * other options with the x-intx-mmap-timeout-ms parameter (a value of
 * zero disables the timer).
 */
static void vfio_intx_mmap_enable(void *opaque)
{
    VFIOPCIDevice *vdev = opaque;

    if (vdev->intx.pending) {
        timer_mod(vdev->intx.mmap_timer,
                       qemu_clock_get_ms(QEMU_CLOCK_VIRTUAL) + vdev->intx.mmap_timeout);
        return;
    }

    vfio_mmap_set_enabled(vdev, true);
}

static void vfio_intx_interrupt(void *opaque)
{
    VFIOPCIDevice *vdev = opaque;

    if (!event_notifier_test_and_clear(&vdev->intx.interrupt)) {
        return;
    }

    trace_vfio_intx_interrupt(vdev->vbasedev.name, 'A' + vdev->intx.pin);

    vdev->intx.pending = true;
    pci_irq_assert(&vdev->pdev);
    vfio_mmap_set_enabled(vdev, false);
    if (vdev->intx.mmap_timeout) {
        timer_mod(vdev->intx.mmap_timer,
                       qemu_clock_get_ms(QEMU_CLOCK_VIRTUAL) + vdev->intx.mmap_timeout);
    }
}

static void vfio_intx_eoi(VFIODevice *vbasedev)
{
    VFIOPCIDevice *vdev = container_of(vbasedev, VFIOPCIDevice, vbasedev);

    if (!vdev->intx.pending) {
        return;
    }

    trace_vfio_intx_eoi(vbasedev->name);

    vdev->intx.pending = false;
    pci_irq_deassert(&vdev->pdev);
    vfio_unmask_single_irqindex(vbasedev, VFIO_PCI_INTX_IRQ_INDEX);
}

static void vfio_intx_enable_kvm(VFIOPCIDevice *vdev)
{
#ifdef CONFIG_KVM
    struct kvm_irqfd irqfd = {
        .fd = event_notifier_get_fd(&vdev->intx.interrupt),
        .gsi = vdev->intx.route.irq,
        .flags = KVM_IRQFD_FLAG_RESAMPLE,
    };
    struct vfio_irq_set *irq_set;
    int ret, argsz;
    int32_t *pfd;

    if (vdev->no_kvm_intx || !kvm_irqfds_enabled() ||
        vdev->intx.route.mode != PCI_INTX_ENABLED ||
        !kvm_resamplefds_enabled()) {
        return;
    }

    /* Get to a known interrupt state */
    qemu_set_fd_handler(irqfd.fd, NULL, NULL, vdev);
    vfio_mask_single_irqindex(&vdev->vbasedev, VFIO_PCI_INTX_IRQ_INDEX);
    vdev->intx.pending = false;
    pci_irq_deassert(&vdev->pdev);

    /* Get an eventfd for resample/unmask */
    if (event_notifier_init(&vdev->intx.unmask, 0)) {
        error_report("vfio: Error: event_notifier_init failed eoi");
        goto fail;
    }

    /* KVM triggers it, VFIO listens for it */
    irqfd.resamplefd = event_notifier_get_fd(&vdev->intx.unmask);

    if (kvm_vm_ioctl(kvm_state, KVM_IRQFD, &irqfd)) {
        error_report("vfio: Error: Failed to setup resample irqfd: %m");
        goto fail_irqfd;
    }

    argsz = sizeof(*irq_set) + sizeof(*pfd);

    irq_set = g_malloc0(argsz);
    irq_set->argsz = argsz;
    irq_set->flags = VFIO_IRQ_SET_DATA_EVENTFD | VFIO_IRQ_SET_ACTION_UNMASK;
    irq_set->index = VFIO_PCI_INTX_IRQ_INDEX;
    irq_set->start = 0;
    irq_set->count = 1;
    pfd = (int32_t *)&irq_set->data;

    *pfd = irqfd.resamplefd;

    ret = ioctl(vdev->vbasedev.fd, VFIO_DEVICE_SET_IRQS, irq_set);
    g_free(irq_set);
    if (ret) {
        error_report("vfio: Error: Failed to setup INTx unmask fd: %m");
        goto fail_vfio;
    }

    /* Let'em rip */
    vfio_unmask_single_irqindex(&vdev->vbasedev, VFIO_PCI_INTX_IRQ_INDEX);

    vdev->intx.kvm_accel = true;

    trace_vfio_intx_enable_kvm(vdev->vbasedev.name);

    return;

fail_vfio:
    irqfd.flags = KVM_IRQFD_FLAG_DEASSIGN;
    kvm_vm_ioctl(kvm_state, KVM_IRQFD, &irqfd);
fail_irqfd:
    event_notifier_cleanup(&vdev->intx.unmask);
fail:
    qemu_set_fd_handler(irqfd.fd, vfio_intx_interrupt, NULL, vdev);
    vfio_unmask_single_irqindex(&vdev->vbasedev, VFIO_PCI_INTX_IRQ_INDEX);
#endif
}

static void vfio_intx_disable_kvm(VFIOPCIDevice *vdev)
{
#ifdef CONFIG_KVM
    struct kvm_irqfd irqfd = {
        .fd = event_notifier_get_fd(&vdev->intx.interrupt),
        .gsi = vdev->intx.route.irq,
        .flags = KVM_IRQFD_FLAG_DEASSIGN,
    };

    if (!vdev->intx.kvm_accel) {
        return;
    }

    /*
     * Get to a known state, hardware masked, QEMU ready to accept new
     * interrupts, QEMU IRQ de-asserted.
     */
    vfio_mask_single_irqindex(&vdev->vbasedev, VFIO_PCI_INTX_IRQ_INDEX);
    vdev->intx.pending = false;
    pci_irq_deassert(&vdev->pdev);

    /* Tell KVM to stop listening for an INTx irqfd */
    if (kvm_vm_ioctl(kvm_state, KVM_IRQFD, &irqfd)) {
        error_report("vfio: Error: Failed to disable INTx irqfd: %m");
    }

    /* We only need to close the eventfd for VFIO to cleanup the kernel side */
    event_notifier_cleanup(&vdev->intx.unmask);

    /* QEMU starts listening for interrupt events. */
    qemu_set_fd_handler(irqfd.fd, vfio_intx_interrupt, NULL, vdev);

    vdev->intx.kvm_accel = false;

    /* If we've missed an event, let it re-fire through QEMU */
    vfio_unmask_single_irqindex(&vdev->vbasedev, VFIO_PCI_INTX_IRQ_INDEX);

    trace_vfio_intx_disable_kvm(vdev->vbasedev.name);
#endif
}

static void vfio_intx_update(PCIDevice *pdev)
{
    VFIOPCIDevice *vdev = DO_UPCAST(VFIOPCIDevice, pdev, pdev);
    PCIINTxRoute route;

    if (vdev->interrupt != VFIO_INT_INTx) {
        return;
    }

    route = pci_device_route_intx_to_irq(&vdev->pdev, vdev->intx.pin);

    if (!pci_intx_route_changed(&vdev->intx.route, &route)) {
        return; /* Nothing changed */
    }

    trace_vfio_intx_update(vdev->vbasedev.name,
                           vdev->intx.route.irq, route.irq);

    vfio_intx_disable_kvm(vdev);

    vdev->intx.route = route;

    if (route.mode != PCI_INTX_ENABLED) {
        return;
    }

    vfio_intx_enable_kvm(vdev);

    /* Re-enable the interrupt in cased we missed an EOI */
    vfio_intx_eoi(&vdev->vbasedev);
}

static int vfio_intx_enable(VFIOPCIDevice *vdev)
{
    uint8_t pin = vfio_pci_read_config(&vdev->pdev, PCI_INTERRUPT_PIN, 1);
    int ret, argsz;
    struct vfio_irq_set *irq_set;
    int32_t *pfd;

    if (!pin) {
        return 0;
    }

    vfio_disable_interrupts(vdev);

    vdev->intx.pin = pin - 1; /* Pin A (1) -> irq[0] */
    pci_config_set_interrupt_pin(vdev->pdev.config, pin);

#ifdef CONFIG_KVM
    /*
     * Only conditional to avoid generating error messages on platforms
     * where we won't actually use the result anyway.
     */
    if (kvm_irqfds_enabled() && kvm_resamplefds_enabled()) {
        vdev->intx.route = pci_device_route_intx_to_irq(&vdev->pdev,
                                                        vdev->intx.pin);
    }
#endif

    ret = event_notifier_init(&vdev->intx.interrupt, 0);
    if (ret) {
        error_report("vfio: Error: event_notifier_init failed");
        return ret;
    }

    argsz = sizeof(*irq_set) + sizeof(*pfd);

    irq_set = g_malloc0(argsz);
    irq_set->argsz = argsz;
    irq_set->flags = VFIO_IRQ_SET_DATA_EVENTFD | VFIO_IRQ_SET_ACTION_TRIGGER;
    irq_set->index = VFIO_PCI_INTX_IRQ_INDEX;
    irq_set->start = 0;
    irq_set->count = 1;
    pfd = (int32_t *)&irq_set->data;

    *pfd = event_notifier_get_fd(&vdev->intx.interrupt);
    qemu_set_fd_handler(*pfd, vfio_intx_interrupt, NULL, vdev);

    ret = ioctl(vdev->vbasedev.fd, VFIO_DEVICE_SET_IRQS, irq_set);
    g_free(irq_set);
    if (ret) {
        error_report("vfio: Error: Failed to setup INTx fd: %m");
        qemu_set_fd_handler(*pfd, NULL, NULL, vdev);
        event_notifier_cleanup(&vdev->intx.interrupt);
        return -errno;
    }

    vfio_intx_enable_kvm(vdev);

    vdev->interrupt = VFIO_INT_INTx;

    trace_vfio_intx_enable(vdev->vbasedev.name);

    return 0;
}

static void vfio_intx_disable(VFIOPCIDevice *vdev)
{
    int fd;

    timer_del(vdev->intx.mmap_timer);
    vfio_intx_disable_kvm(vdev);
    vfio_disable_irqindex(&vdev->vbasedev, VFIO_PCI_INTX_IRQ_INDEX);
    vdev->intx.pending = false;
    pci_irq_deassert(&vdev->pdev);
    vfio_mmap_set_enabled(vdev, true);

    fd = event_notifier_get_fd(&vdev->intx.interrupt);
    qemu_set_fd_handler(fd, NULL, NULL, vdev);
    event_notifier_cleanup(&vdev->intx.interrupt);

    vdev->interrupt = VFIO_INT_NONE;

    trace_vfio_intx_disable(vdev->vbasedev.name);
}

/*
 * MSI/X
 */
static void vfio_msi_interrupt(void *opaque)
{
    VFIOMSIVector *vector = opaque;
    VFIOPCIDevice *vdev = vector->vdev;
    MSIMessage (*get_msg)(PCIDevice *dev, unsigned vector);
    void (*notify)(PCIDevice *dev, unsigned vector);
    MSIMessage msg;
    int nr = vector - vdev->msi_vectors;

    if (!event_notifier_test_and_clear(&vector->interrupt)) {
        return;
    }

    if (vdev->interrupt == VFIO_INT_MSIX) {
        get_msg = msix_get_message;
        notify = msix_notify;
    } else if (vdev->interrupt == VFIO_INT_MSI) {
        get_msg = msi_get_message;
        notify = msi_notify;
    } else {
        abort();
    }

    msg = get_msg(&vdev->pdev, nr);
    trace_vfio_msi_interrupt(vdev->vbasedev.name, nr, msg.address, msg.data);
    notify(&vdev->pdev, nr);
}

static int vfio_enable_vectors(VFIOPCIDevice *vdev, bool msix)
{
    struct vfio_irq_set *irq_set;
    int ret = 0, i, argsz;
    int32_t *fds;

    argsz = sizeof(*irq_set) + (vdev->nr_vectors * sizeof(*fds));

    irq_set = g_malloc0(argsz);
    irq_set->argsz = argsz;
    irq_set->flags = VFIO_IRQ_SET_DATA_EVENTFD | VFIO_IRQ_SET_ACTION_TRIGGER;
    irq_set->index = msix ? VFIO_PCI_MSIX_IRQ_INDEX : VFIO_PCI_MSI_IRQ_INDEX;
    irq_set->start = 0;
    irq_set->count = vdev->nr_vectors;
    fds = (int32_t *)&irq_set->data;

    for (i = 0; i < vdev->nr_vectors; i++) {
        int fd = -1;

        /*
         * MSI vs MSI-X - The guest has direct access to MSI mask and pending
         * bits, therefore we always use the KVM signaling path when setup.
         * MSI-X mask and pending bits are emulated, so we want to use the
         * KVM signaling path only when configured and unmasked.
         */
        if (vdev->msi_vectors[i].use) {
            if (vdev->msi_vectors[i].virq < 0 ||
                (msix && msix_is_masked(&vdev->pdev, i))) {
                fd = event_notifier_get_fd(&vdev->msi_vectors[i].interrupt);
            } else {
                fd = event_notifier_get_fd(&vdev->msi_vectors[i].kvm_interrupt);
            }
        }

        fds[i] = fd;
    }

    ret = ioctl(vdev->vbasedev.fd, VFIO_DEVICE_SET_IRQS, irq_set);

    g_free(irq_set);

    return ret;
}

static void vfio_add_kvm_msi_virq(VFIOPCIDevice *vdev, VFIOMSIVector *vector,
                                  MSIMessage *msg, bool msix)
{
    int virq;

    if ((msix && vdev->no_kvm_msix) || (!msix && vdev->no_kvm_msi) || !msg) {
        return;
    }

    if (event_notifier_init(&vector->kvm_interrupt, 0)) {
        return;
    }

    virq = kvm_irqchip_add_msi_route(kvm_state, *msg, &vdev->pdev);
    if (virq < 0) {
        event_notifier_cleanup(&vector->kvm_interrupt);
        return;
    }

    if (kvm_irqchip_add_irqfd_notifier_gsi(kvm_state, &vector->kvm_interrupt,
                                       NULL, virq) < 0) {
        kvm_irqchip_release_virq(kvm_state, virq);
        event_notifier_cleanup(&vector->kvm_interrupt);
        return;
    }

    vector->virq = virq;
}

static void vfio_remove_kvm_msi_virq(VFIOMSIVector *vector)
{
    kvm_irqchip_remove_irqfd_notifier_gsi(kvm_state, &vector->kvm_interrupt,
                                          vector->virq);
    kvm_irqchip_release_virq(kvm_state, vector->virq);
    vector->virq = -1;
    event_notifier_cleanup(&vector->kvm_interrupt);
}

static void vfio_update_kvm_msi_virq(VFIOMSIVector *vector, MSIMessage msg,
                                     PCIDevice *pdev)
{
    kvm_irqchip_update_msi_route(kvm_state, vector->virq, msg, pdev);
}

static int vfio_msix_vector_do_use(PCIDevice *pdev, unsigned int nr,
                                   MSIMessage *msg, IOHandler *handler)
{
    VFIOPCIDevice *vdev = DO_UPCAST(VFIOPCIDevice, pdev, pdev);
    VFIOMSIVector *vector;
    int ret;

    trace_vfio_msix_vector_do_use(vdev->vbasedev.name, nr);

    vector = &vdev->msi_vectors[nr];

    if (!vector->use) {
        vector->vdev = vdev;
        vector->virq = -1;
        if (event_notifier_init(&vector->interrupt, 0)) {
            error_report("vfio: Error: event_notifier_init failed");
        }
        vector->use = true;
        msix_vector_use(pdev, nr);
    }

    qemu_set_fd_handler(event_notifier_get_fd(&vector->interrupt),
                        handler, NULL, vector);

    /*
     * Attempt to enable route through KVM irqchip,
     * default to userspace handling if unavailable.
     */
    if (vector->virq >= 0) {
        if (!msg) {
            vfio_remove_kvm_msi_virq(vector);
        } else {
            vfio_update_kvm_msi_virq(vector, *msg, pdev);
        }
    } else {
        vfio_add_kvm_msi_virq(vdev, vector, msg, true);
    }

    /*
     * We don't want to have the host allocate all possible MSI vectors
     * for a device if they're not in use, so we shutdown and incrementally
     * increase them as needed.
     */
    if (vdev->nr_vectors < nr + 1) {
        vfio_disable_irqindex(&vdev->vbasedev, VFIO_PCI_MSIX_IRQ_INDEX);
        vdev->nr_vectors = nr + 1;
        ret = vfio_enable_vectors(vdev, true);
        if (ret) {
            error_report("vfio: failed to enable vectors, %d", ret);
        }
    } else {
        int argsz;
        struct vfio_irq_set *irq_set;
        int32_t *pfd;

        argsz = sizeof(*irq_set) + sizeof(*pfd);

        irq_set = g_malloc0(argsz);
        irq_set->argsz = argsz;
        irq_set->flags = VFIO_IRQ_SET_DATA_EVENTFD |
                         VFIO_IRQ_SET_ACTION_TRIGGER;
        irq_set->index = VFIO_PCI_MSIX_IRQ_INDEX;
        irq_set->start = nr;
        irq_set->count = 1;
        pfd = (int32_t *)&irq_set->data;

        if (vector->virq >= 0) {
            *pfd = event_notifier_get_fd(&vector->kvm_interrupt);
        } else {
            *pfd = event_notifier_get_fd(&vector->interrupt);
        }

        ret = ioctl(vdev->vbasedev.fd, VFIO_DEVICE_SET_IRQS, irq_set);
        g_free(irq_set);
        if (ret) {
            error_report("vfio: failed to modify vector, %d", ret);
        }
    }

    return 0;
}

static int vfio_msix_vector_use(PCIDevice *pdev,
                                unsigned int nr, MSIMessage msg)
{
    return vfio_msix_vector_do_use(pdev, nr, &msg, vfio_msi_interrupt);
}

static void vfio_msix_vector_release(PCIDevice *pdev, unsigned int nr)
{
    VFIOPCIDevice *vdev = DO_UPCAST(VFIOPCIDevice, pdev, pdev);
    VFIOMSIVector *vector = &vdev->msi_vectors[nr];

    trace_vfio_msix_vector_release(vdev->vbasedev.name, nr);

    /*
     * There are still old guests that mask and unmask vectors on every
     * interrupt.  If we're using QEMU bypass with a KVM irqfd, leave all of
     * the KVM setup in place, simply switch VFIO to use the non-bypass
     * eventfd.  We'll then fire the interrupt through QEMU and the MSI-X
     * core will mask the interrupt and set pending bits, allowing it to
     * be re-asserted on unmask.  Nothing to do if already using QEMU mode.
     */
    if (vector->virq >= 0) {
        int argsz;
        struct vfio_irq_set *irq_set;
        int32_t *pfd;

        argsz = sizeof(*irq_set) + sizeof(*pfd);

        irq_set = g_malloc0(argsz);
        irq_set->argsz = argsz;
        irq_set->flags = VFIO_IRQ_SET_DATA_EVENTFD |
                         VFIO_IRQ_SET_ACTION_TRIGGER;
        irq_set->index = VFIO_PCI_MSIX_IRQ_INDEX;
        irq_set->start = nr;
        irq_set->count = 1;
        pfd = (int32_t *)&irq_set->data;

        *pfd = event_notifier_get_fd(&vector->interrupt);

        ioctl(vdev->vbasedev.fd, VFIO_DEVICE_SET_IRQS, irq_set);

        g_free(irq_set);
    }
}

static void vfio_msix_enable(VFIOPCIDevice *vdev)
{
    vfio_disable_interrupts(vdev);

    vdev->msi_vectors = g_new0(VFIOMSIVector, vdev->msix->entries);

    vdev->interrupt = VFIO_INT_MSIX;

    /*
     * Some communication channels between VF & PF or PF & fw rely on the
     * physical state of the device and expect that enabling MSI-X from the
     * guest enables the same on the host.  When our guest is Linux, the
     * guest driver call to pci_enable_msix() sets the enabling bit in the
     * MSI-X capability, but leaves the vector table masked.  We therefore
     * can't rely on a vector_use callback (from request_irq() in the guest)
     * to switch the physical device into MSI-X mode because that may come a
     * long time after pci_enable_msix().  This code enables vector 0 with
     * triggering to userspace, then immediately release the vector, leaving
     * the physical device with no vectors enabled, but MSI-X enabled, just
     * like the guest view.
     */
    vfio_msix_vector_do_use(&vdev->pdev, 0, NULL, NULL);
    vfio_msix_vector_release(&vdev->pdev, 0);

    if (msix_set_vector_notifiers(&vdev->pdev, vfio_msix_vector_use,
                                  vfio_msix_vector_release, NULL)) {
        error_report("vfio: msix_set_vector_notifiers failed");
    }

    trace_vfio_msix_enable(vdev->vbasedev.name);
}

static void vfio_msi_enable(VFIOPCIDevice *vdev)
{
    int ret, i;

    vfio_disable_interrupts(vdev);

    vdev->nr_vectors = msi_nr_vectors_allocated(&vdev->pdev);
retry:
    vdev->msi_vectors = g_new0(VFIOMSIVector, vdev->nr_vectors);

    for (i = 0; i < vdev->nr_vectors; i++) {
        VFIOMSIVector *vector = &vdev->msi_vectors[i];
        MSIMessage msg = msi_get_message(&vdev->pdev, i);

        vector->vdev = vdev;
        vector->virq = -1;
        vector->use = true;

        if (event_notifier_init(&vector->interrupt, 0)) {
            error_report("vfio: Error: event_notifier_init failed");
        }

        qemu_set_fd_handler(event_notifier_get_fd(&vector->interrupt),
                            vfio_msi_interrupt, NULL, vector);

        /*
         * Attempt to enable route through KVM irqchip,
         * default to userspace handling if unavailable.
         */
        vfio_add_kvm_msi_virq(vdev, vector, &msg, false);
    }

    /* Set interrupt type prior to possible interrupts */
    vdev->interrupt = VFIO_INT_MSI;

    ret = vfio_enable_vectors(vdev, false);
    if (ret) {
        if (ret < 0) {
            error_report("vfio: Error: Failed to setup MSI fds: %m");
        } else if (ret != vdev->nr_vectors) {
            error_report("vfio: Error: Failed to enable %d "
                         "MSI vectors, retry with %d", vdev->nr_vectors, ret);
        }

        for (i = 0; i < vdev->nr_vectors; i++) {
            VFIOMSIVector *vector = &vdev->msi_vectors[i];
            if (vector->virq >= 0) {
                vfio_remove_kvm_msi_virq(vector);
            }
            qemu_set_fd_handler(event_notifier_get_fd(&vector->interrupt),
                                NULL, NULL, NULL);
            event_notifier_cleanup(&vector->interrupt);
        }

        g_free(vdev->msi_vectors);

        if (ret > 0 && ret != vdev->nr_vectors) {
            vdev->nr_vectors = ret;
            goto retry;
        }
        vdev->nr_vectors = 0;

        /*
         * Failing to setup MSI doesn't really fall within any specification.
         * Let's try leaving interrupts disabled and hope the guest figures
         * out to fall back to INTx for this device.
         */
        error_report("vfio: Error: Failed to enable MSI");
        vdev->interrupt = VFIO_INT_NONE;

        return;
    }

    trace_vfio_msi_enable(vdev->vbasedev.name, vdev->nr_vectors);
}

static void vfio_msi_disable_common(VFIOPCIDevice *vdev)
{
    int i;

    for (i = 0; i < vdev->nr_vectors; i++) {
        VFIOMSIVector *vector = &vdev->msi_vectors[i];
        if (vdev->msi_vectors[i].use) {
            if (vector->virq >= 0) {
                vfio_remove_kvm_msi_virq(vector);
            }
            qemu_set_fd_handler(event_notifier_get_fd(&vector->interrupt),
                                NULL, NULL, NULL);
            event_notifier_cleanup(&vector->interrupt);
        }
    }

    g_free(vdev->msi_vectors);
    vdev->msi_vectors = NULL;
    vdev->nr_vectors = 0;
    vdev->interrupt = VFIO_INT_NONE;

    vfio_intx_enable(vdev);
}

static void vfio_msix_disable(VFIOPCIDevice *vdev)
{
    int i;

    msix_unset_vector_notifiers(&vdev->pdev);

    /*
     * MSI-X will only release vectors if MSI-X is still enabled on the
     * device, check through the rest and release it ourselves if necessary.
     */
    for (i = 0; i < vdev->nr_vectors; i++) {
        if (vdev->msi_vectors[i].use) {
            vfio_msix_vector_release(&vdev->pdev, i);
            msix_vector_unuse(&vdev->pdev, i);
        }
    }

    if (vdev->nr_vectors) {
        vfio_disable_irqindex(&vdev->vbasedev, VFIO_PCI_MSIX_IRQ_INDEX);
    }

    vfio_msi_disable_common(vdev);

    trace_vfio_msix_disable(vdev->vbasedev.name);
}

static void vfio_msi_disable(VFIOPCIDevice *vdev)
{
    vfio_disable_irqindex(&vdev->vbasedev, VFIO_PCI_MSI_IRQ_INDEX);
    vfio_msi_disable_common(vdev);

    trace_vfio_msi_disable(vdev->vbasedev.name);
}

static void vfio_update_msi(VFIOPCIDevice *vdev)
{
    int i;

    for (i = 0; i < vdev->nr_vectors; i++) {
        VFIOMSIVector *vector = &vdev->msi_vectors[i];
        MSIMessage msg;

        if (!vector->use || vector->virq < 0) {
            continue;
        }

        msg = msi_get_message(&vdev->pdev, i);
        vfio_update_kvm_msi_virq(vector, msg, &vdev->pdev);
    }
}

static void vfio_pci_load_rom(VFIOPCIDevice *vdev)
{
    struct vfio_region_info reg_info = {
        .argsz = sizeof(reg_info),
        .index = VFIO_PCI_ROM_REGION_INDEX
    };
    uint64_t size;
    off_t off = 0;
    ssize_t bytes;

    if (ioctl(vdev->vbasedev.fd, VFIO_DEVICE_GET_REGION_INFO, &reg_info)) {
        error_report("vfio: Error getting ROM info: %m");
        return;
    }

    trace_vfio_pci_load_rom(vdev->vbasedev.name, (unsigned long)reg_info.size,
                            (unsigned long)reg_info.offset,
                            (unsigned long)reg_info.flags);

    vdev->rom_size = size = reg_info.size;
    vdev->rom_offset = reg_info.offset;

    if (!vdev->rom_size) {
        vdev->rom_read_failed = true;
        error_report("vfio-pci: Cannot read device rom at "
                    "%s", vdev->vbasedev.name);
        error_printf("Device option ROM contents are probably invalid "
                    "(check dmesg).\nSkip option ROM probe with rombar=0, "
                    "or load from file with romfile=\n");
        return;
    }

    vdev->rom = g_malloc(size);
    memset(vdev->rom, 0xff, size);

    while (size) {
        bytes = pread(vdev->vbasedev.fd, vdev->rom + off,
                      size, vdev->rom_offset + off);
        if (bytes == 0) {
            break;
        } else if (bytes > 0) {
            off += bytes;
            size -= bytes;
        } else {
            if (errno == EINTR || errno == EAGAIN) {
                continue;
            }
            error_report("vfio: Error reading device ROM: %m");
            break;
        }
    }
}

static uint64_t vfio_rom_read(void *opaque, hwaddr addr, unsigned size)
{
    VFIOPCIDevice *vdev = opaque;
    union {
        uint8_t byte;
        uint16_t word;
        uint32_t dword;
        uint64_t qword;
    } val;
    uint64_t data = 0;

    /* Load the ROM lazily when the guest tries to read it */
    if (unlikely(!vdev->rom && !vdev->rom_read_failed)) {
        vfio_pci_load_rom(vdev);
    }

    memcpy(&val, vdev->rom + addr,
           (addr < vdev->rom_size) ? MIN(size, vdev->rom_size - addr) : 0);

    switch (size) {
    case 1:
        data = val.byte;
        break;
    case 2:
        data = le16_to_cpu(val.word);
        break;
    case 4:
        data = le32_to_cpu(val.dword);
        break;
    default:
        hw_error("vfio: unsupported read size, %d bytes\n", size);
        break;
    }

    trace_vfio_rom_read(vdev->vbasedev.name, addr, size, data);

    return data;
}

static void vfio_rom_write(void *opaque, hwaddr addr,
                           uint64_t data, unsigned size)
{
}

static const MemoryRegionOps vfio_rom_ops = {
    .read = vfio_rom_read,
    .write = vfio_rom_write,
    .endianness = DEVICE_LITTLE_ENDIAN,
};

static void vfio_pci_size_rom(VFIOPCIDevice *vdev)
{
    uint32_t orig, size = cpu_to_le32((uint32_t)PCI_ROM_ADDRESS_MASK);
    off_t offset = vdev->config_offset + PCI_ROM_ADDRESS;
    DeviceState *dev = DEVICE(vdev);
    char name[32];
    int fd = vdev->vbasedev.fd;

    if (vdev->pdev.romfile || !vdev->pdev.rom_bar) {
        /* Since pci handles romfile, just print a message and return */
        if (vfio_blacklist_opt_rom(vdev) && vdev->pdev.romfile) {
            error_printf("Warning : Device at %04x:%02x:%02x.%x "
                         "is known to cause system instability issues during "
                         "option rom execution. "
                         "Proceeding anyway since user specified romfile\n",
                         vdev->host.domain, vdev->host.bus, vdev->host.slot,
                         vdev->host.function);
        }
        return;
    }

    /*
     * Use the same size ROM BAR as the physical device.  The contents
     * will get filled in later when the guest tries to read it.
     */
    if (pread(fd, &orig, 4, offset) != 4 ||
        pwrite(fd, &size, 4, offset) != 4 ||
        pread(fd, &size, 4, offset) != 4 ||
        pwrite(fd, &orig, 4, offset) != 4) {
        error_report("%s(%04x:%02x:%02x.%x) failed: %m",
                     __func__, vdev->host.domain, vdev->host.bus,
                     vdev->host.slot, vdev->host.function);
        return;
    }

    size = ~(le32_to_cpu(size) & PCI_ROM_ADDRESS_MASK) + 1;

    if (!size) {
        return;
    }

    if (vfio_blacklist_opt_rom(vdev)) {
        if (dev->opts && qemu_opt_get(dev->opts, "rombar")) {
            error_printf("Warning : Device at %04x:%02x:%02x.%x "
                         "is known to cause system instability issues during "
                         "option rom execution. "
                         "Proceeding anyway since user specified non zero value for "
                         "rombar\n",
                         vdev->host.domain, vdev->host.bus, vdev->host.slot,
                         vdev->host.function);
        } else {
            error_printf("Warning : Rom loading for device at "
                         "%04x:%02x:%02x.%x has been disabled due to "
                         "system instability issues. "
                         "Specify rombar=1 or romfile to force\n",
                         vdev->host.domain, vdev->host.bus, vdev->host.slot,
                         vdev->host.function);
            return;
        }
    }

    trace_vfio_pci_size_rom(vdev->vbasedev.name, size);

    snprintf(name, sizeof(name), "vfio[%04x:%02x:%02x.%x].rom",
             vdev->host.domain, vdev->host.bus, vdev->host.slot,
             vdev->host.function);

    memory_region_init_io(&vdev->pdev.rom, OBJECT(vdev),
                          &vfio_rom_ops, vdev, name, size);

    pci_register_bar(&vdev->pdev, PCI_ROM_SLOT,
                     PCI_BASE_ADDRESS_SPACE_MEMORY, &vdev->pdev.rom);

    vdev->pdev.has_rom = true;
    vdev->rom_read_failed = false;
}

void vfio_vga_write(void *opaque, hwaddr addr,
                           uint64_t data, unsigned size)
{
    VFIOVGARegion *region = opaque;
    VFIOVGA *vga = container_of(region, VFIOVGA, region[region->nr]);
    union {
        uint8_t byte;
        uint16_t word;
        uint32_t dword;
        uint64_t qword;
    } buf;
    off_t offset = vga->fd_offset + region->offset + addr;

    switch (size) {
    case 1:
        buf.byte = data;
        break;
    case 2:
        buf.word = cpu_to_le16(data);
        break;
    case 4:
        buf.dword = cpu_to_le32(data);
        break;
    default:
        hw_error("vfio: unsupported write size, %d bytes", size);
        break;
    }

    if (pwrite(vga->fd, &buf, size, offset) != size) {
        error_report("%s(,0x%"HWADDR_PRIx", 0x%"PRIx64", %d) failed: %m",
                     __func__, region->offset + addr, data, size);
    }

    trace_vfio_vga_write(region->offset + addr, data, size);
}

uint64_t vfio_vga_read(void *opaque, hwaddr addr, unsigned size)
{
    VFIOVGARegion *region = opaque;
    VFIOVGA *vga = container_of(region, VFIOVGA, region[region->nr]);
    union {
        uint8_t byte;
        uint16_t word;
        uint32_t dword;
        uint64_t qword;
    } buf;
    uint64_t data = 0;
    off_t offset = vga->fd_offset + region->offset + addr;

    if (pread(vga->fd, &buf, size, offset) != size) {
        error_report("%s(,0x%"HWADDR_PRIx", %d) failed: %m",
                     __func__, region->offset + addr, size);
        return (uint64_t)-1;
    }

    switch (size) {
    case 1:
        data = buf.byte;
        break;
    case 2:
        data = le16_to_cpu(buf.word);
        break;
    case 4:
        data = le32_to_cpu(buf.dword);
        break;
    default:
        hw_error("vfio: unsupported read size, %d bytes", size);
        break;
    }

    trace_vfio_vga_read(region->offset + addr, size, data);

    return data;
}

static const MemoryRegionOps vfio_vga_ops = {
    .read = vfio_vga_read,
    .write = vfio_vga_write,
    .endianness = DEVICE_LITTLE_ENDIAN,
};

/*
 * PCI config space
 */
uint32_t vfio_pci_read_config(PCIDevice *pdev, uint32_t addr, int len)
{
    VFIOPCIDevice *vdev = DO_UPCAST(VFIOPCIDevice, pdev, pdev);
    uint32_t emu_bits = 0, emu_val = 0, phys_val = 0, val;

    memcpy(&emu_bits, vdev->emulated_config_bits + addr, len);
    emu_bits = le32_to_cpu(emu_bits);

    if (emu_bits) {
        emu_val = pci_default_read_config(pdev, addr, len);
    }

    if (~emu_bits & (0xffffffffU >> (32 - len * 8))) {
        ssize_t ret;

        ret = pread(vdev->vbasedev.fd, &phys_val, len,
                    vdev->config_offset + addr);
        if (ret != len) {
            error_report("%s(%04x:%02x:%02x.%x, 0x%x, 0x%x) failed: %m",
                         __func__, vdev->host.domain, vdev->host.bus,
                         vdev->host.slot, vdev->host.function, addr, len);
            return -errno;
        }
        phys_val = le32_to_cpu(phys_val);
    }

    val = (emu_val & emu_bits) | (phys_val & ~emu_bits);

    trace_vfio_pci_read_config(vdev->vbasedev.name, addr, len, val);

    return val;
}

void vfio_pci_write_config(PCIDevice *pdev,
                           uint32_t addr, uint32_t val, int len)
{
    VFIOPCIDevice *vdev = DO_UPCAST(VFIOPCIDevice, pdev, pdev);
    uint32_t val_le = cpu_to_le32(val);

    trace_vfio_pci_write_config(vdev->vbasedev.name, addr, val, len);

    /* Write everything to VFIO, let it filter out what we can't write */
    if (pwrite(vdev->vbasedev.fd, &val_le, len, vdev->config_offset + addr)
                != len) {
        error_report("%s(%04x:%02x:%02x.%x, 0x%x, 0x%x, 0x%x) failed: %m",
                     __func__, vdev->host.domain, vdev->host.bus,
                     vdev->host.slot, vdev->host.function, addr, val, len);
    }

    /* MSI/MSI-X Enabling/Disabling */
    if (pdev->cap_present & QEMU_PCI_CAP_MSI &&
        ranges_overlap(addr, len, pdev->msi_cap, vdev->msi_cap_size)) {
        int is_enabled, was_enabled = msi_enabled(pdev);

        pci_default_write_config(pdev, addr, val, len);

        is_enabled = msi_enabled(pdev);

        if (!was_enabled) {
            if (is_enabled) {
                vfio_msi_enable(vdev);
            }
        } else {
            if (!is_enabled) {
                vfio_msi_disable(vdev);
            } else {
                vfio_update_msi(vdev);
            }
        }
    } else if (pdev->cap_present & QEMU_PCI_CAP_MSIX &&
        ranges_overlap(addr, len, pdev->msix_cap, MSIX_CAP_LENGTH)) {
        int is_enabled, was_enabled = msix_enabled(pdev);

        pci_default_write_config(pdev, addr, val, len);

        is_enabled = msix_enabled(pdev);

        if (!was_enabled && is_enabled) {
            vfio_msix_enable(vdev);
        } else if (was_enabled && !is_enabled) {
            vfio_msix_disable(vdev);
        }
    } else {
        /* Write everything to QEMU to keep emulated bits correct */
        pci_default_write_config(pdev, addr, val, len);
    }
}

/*
 * Interrupt setup
 */
static void vfio_disable_interrupts(VFIOPCIDevice *vdev)
{
    /*
     * More complicated than it looks.  Disabling MSI/X transitions the
     * device to INTx mode (if supported).  Therefore we need to first
     * disable MSI/X and then cleanup by disabling INTx.
     */
    if (vdev->interrupt == VFIO_INT_MSIX) {
        vfio_msix_disable(vdev);
    } else if (vdev->interrupt == VFIO_INT_MSI) {
        vfio_msi_disable(vdev);
    }

    if (vdev->interrupt == VFIO_INT_INTx) {
        vfio_intx_disable(vdev);
    }
}

static int vfio_msi_setup(VFIOPCIDevice *vdev, int pos)
{
    uint16_t ctrl;
    bool msi_64bit, msi_maskbit;
    int ret, entries;

    if (pread(vdev->vbasedev.fd, &ctrl, sizeof(ctrl),
              vdev->config_offset + pos + PCI_CAP_FLAGS) != sizeof(ctrl)) {
        return -errno;
    }
    ctrl = le16_to_cpu(ctrl);

    msi_64bit = !!(ctrl & PCI_MSI_FLAGS_64BIT);
    msi_maskbit = !!(ctrl & PCI_MSI_FLAGS_MASKBIT);
    entries = 1 << ((ctrl & PCI_MSI_FLAGS_QMASK) >> 1);

    trace_vfio_msi_setup(vdev->vbasedev.name, pos);

    ret = msi_init(&vdev->pdev, pos, entries, msi_64bit, msi_maskbit);
    if (ret < 0) {
        if (ret == -ENOTSUP) {
            return 0;
        }
        error_report("vfio: msi_init failed");
        return ret;
    }
    vdev->msi_cap_size = 0xa + (msi_maskbit ? 0xa : 0) + (msi_64bit ? 0x4 : 0);

    return 0;
}

/*
 * We don't have any control over how pci_add_capability() inserts
 * capabilities into the chain.  In order to setup MSI-X we need a
 * MemoryRegion for the BAR.  In order to setup the BAR and not
 * attempt to mmap the MSI-X table area, which VFIO won't allow, we
 * need to first look for where the MSI-X table lives.  So we
 * unfortunately split MSI-X setup across two functions.
 */
static int vfio_msix_early_setup(VFIOPCIDevice *vdev)
{
    uint8_t pos;
    uint16_t ctrl;
    uint32_t table, pba;
    int fd = vdev->vbasedev.fd;
    VFIOMSIXInfo *msix;

    pos = pci_find_capability(&vdev->pdev, PCI_CAP_ID_MSIX);
    if (!pos) {
        return 0;
    }

    if (pread(fd, &ctrl, sizeof(ctrl),
              vdev->config_offset + pos + PCI_CAP_FLAGS) != sizeof(ctrl)) {
        return -errno;
    }

    if (pread(fd, &table, sizeof(table),
              vdev->config_offset + pos + PCI_MSIX_TABLE) != sizeof(table)) {
        return -errno;
    }

    if (pread(fd, &pba, sizeof(pba),
              vdev->config_offset + pos + PCI_MSIX_PBA) != sizeof(pba)) {
        return -errno;
    }

    ctrl = le16_to_cpu(ctrl);
    table = le32_to_cpu(table);
    pba = le32_to_cpu(pba);

    msix = g_malloc0(sizeof(*msix));
    msix->table_bar = table & PCI_MSIX_FLAGS_BIRMASK;
    msix->table_offset = table & ~PCI_MSIX_FLAGS_BIRMASK;
    msix->pba_bar = pba & PCI_MSIX_FLAGS_BIRMASK;
    msix->pba_offset = pba & ~PCI_MSIX_FLAGS_BIRMASK;
    msix->entries = (ctrl & PCI_MSIX_FLAGS_QSIZE) + 1;

    /*
     * Test the size of the pba_offset variable and catch if it extends outside
     * of the specified BAR. If it is the case, we need to apply a hardware
     * specific quirk if the device is known or we have a broken configuration.
     */
    if (msix->pba_offset >= vdev->bars[msix->pba_bar].region.size) {
        /*
         * Chelsio T5 Virtual Function devices are encoded as 0x58xx for T5
         * adapters. The T5 hardware returns an incorrect value of 0x8000 for
         * the VF PBA offset while the BAR itself is only 8k. The correct value
         * is 0x1000, so we hard code that here.
         */
        if (vdev->vendor_id == PCI_VENDOR_ID_CHELSIO &&
            (vdev->device_id & 0xff00) == 0x5800) {
            msix->pba_offset = 0x1000;
        } else {
            error_report("vfio: Hardware reports invalid configuration, "
                         "MSIX PBA outside of specified BAR");
            g_free(msix);
            return -EINVAL;
        }
    }

    trace_vfio_msix_early_setup(vdev->vbasedev.name, pos, msix->table_bar,
                                msix->table_offset, msix->entries);
    vdev->msix = msix;

    return 0;
}

static int vfio_msix_setup(VFIOPCIDevice *vdev, int pos)
{
    int ret;

    ret = msix_init(&vdev->pdev, vdev->msix->entries,
                    &vdev->bars[vdev->msix->table_bar].region.mem,
                    vdev->msix->table_bar, vdev->msix->table_offset,
                    &vdev->bars[vdev->msix->pba_bar].region.mem,
                    vdev->msix->pba_bar, vdev->msix->pba_offset, pos);
    if (ret < 0) {
        if (ret == -ENOTSUP) {
            return 0;
        }
        error_report("vfio: msix_init failed");
        return ret;
    }

    return 0;
}

static void vfio_teardown_msi(VFIOPCIDevice *vdev)
{
    msi_uninit(&vdev->pdev);

    if (vdev->msix) {
        msix_uninit(&vdev->pdev,
                    &vdev->bars[vdev->msix->table_bar].region.mem,
                    &vdev->bars[vdev->msix->pba_bar].region.mem);
    }
}

/*
 * Resource setup
 */
static void vfio_mmap_set_enabled(VFIOPCIDevice *vdev, bool enabled)
{
    int i;

    for (i = 0; i < PCI_ROM_SLOT; i++) {
        VFIOBAR *bar = &vdev->bars[i];

        if (!bar->region.size) {
            continue;
        }

        memory_region_set_enabled(&bar->region.mmap_mem, enabled);
        if (vdev->msix && vdev->msix->table_bar == i) {
            memory_region_set_enabled(&vdev->msix->mmap_mem, enabled);
        }
    }
}

static void vfio_unregister_bar(VFIOPCIDevice *vdev, int nr)
{
    VFIOBAR *bar = &vdev->bars[nr];

    if (!bar->region.size) {
        return;
    }

    vfio_bar_quirk_teardown(vdev, nr);

    memory_region_del_subregion(&bar->region.mem, &bar->region.mmap_mem);

    if (vdev->msix && vdev->msix->table_bar == nr) {
        memory_region_del_subregion(&bar->region.mem, &vdev->msix->mmap_mem);
    }
}

static void vfio_unmap_bar(VFIOPCIDevice *vdev, int nr)
{
    VFIOBAR *bar = &vdev->bars[nr];

    if (!bar->region.size) {
        return;
    }

    vfio_bar_quirk_free(vdev, nr);

    munmap(bar->region.mmap, memory_region_size(&bar->region.mmap_mem));

    if (vdev->msix && vdev->msix->table_bar == nr) {
        munmap(vdev->msix->mmap, memory_region_size(&vdev->msix->mmap_mem));
    }
}

static void vfio_map_bar(VFIOPCIDevice *vdev, int nr)
{
    VFIOBAR *bar = &vdev->bars[nr];
    uint64_t size = bar->region.size;
    char name[64];
    uint32_t pci_bar;
    uint8_t type;
    int ret;

    /* Skip both unimplemented BARs and the upper half of 64bit BARS. */
    if (!size) {
        return;
    }

    snprintf(name, sizeof(name), "VFIO %04x:%02x:%02x.%x BAR %d",
             vdev->host.domain, vdev->host.bus, vdev->host.slot,
             vdev->host.function, nr);

    /* Determine what type of BAR this is for registration */
    ret = pread(vdev->vbasedev.fd, &pci_bar, sizeof(pci_bar),
                vdev->config_offset + PCI_BASE_ADDRESS_0 + (4 * nr));
    if (ret != sizeof(pci_bar)) {
        error_report("vfio: Failed to read BAR %d (%m)", nr);
        return;
    }

    pci_bar = le32_to_cpu(pci_bar);
    bar->ioport = (pci_bar & PCI_BASE_ADDRESS_SPACE_IO);
    bar->mem64 = bar->ioport ? 0 : (pci_bar & PCI_BASE_ADDRESS_MEM_TYPE_64);
    type = pci_bar & (bar->ioport ? ~PCI_BASE_ADDRESS_IO_MASK :
                                    ~PCI_BASE_ADDRESS_MEM_MASK);

    /* A "slow" read/write mapping underlies all BARs */
    memory_region_init_io(&bar->region.mem, OBJECT(vdev), &vfio_region_ops,
                          bar, name, size);
    pci_register_bar(&vdev->pdev, nr, type, &bar->region.mem);

    /*
     * We can't mmap areas overlapping the MSIX vector table, so we
     * potentially insert a direct-mapped subregion before and after it.
     */
    if (vdev->msix && vdev->msix->table_bar == nr) {
        size = vdev->msix->table_offset & qemu_real_host_page_mask;
    }

    strncat(name, " mmap", sizeof(name) - strlen(name) - 1);
    if (vfio_mmap_region(OBJECT(vdev), &bar->region, &bar->region.mem,
                      &bar->region.mmap_mem, &bar->region.mmap,
                      size, 0, name)) {
        error_report("%s unsupported. Performance may be slow", name);
    }

    if (vdev->msix && vdev->msix->table_bar == nr) {
        uint64_t start;

        start = REAL_HOST_PAGE_ALIGN((uint64_t)vdev->msix->table_offset +
                                     (vdev->msix->entries *
                                      PCI_MSIX_ENTRY_SIZE));

        size = start < bar->region.size ? bar->region.size - start : 0;
        strncat(name, " msix-hi", sizeof(name) - strlen(name) - 1);
        /* VFIOMSIXInfo contains another MemoryRegion for this mapping */
        if (vfio_mmap_region(OBJECT(vdev), &bar->region, &bar->region.mem,
                          &vdev->msix->mmap_mem,
                          &vdev->msix->mmap, size, start, name)) {
            error_report("%s unsupported. Performance may be slow", name);
        }
    }

    vfio_bar_quirk_setup(vdev, nr);
}

static void vfio_map_bars(VFIOPCIDevice *vdev)
{
    int i;

    for (i = 0; i < PCI_ROM_SLOT; i++) {
        vfio_map_bar(vdev, i);
    }

    if (vdev->has_vga) {
        memory_region_init_io(&vdev->vga.region[QEMU_PCI_VGA_MEM].mem,
                              OBJECT(vdev), &vfio_vga_ops,
                              &vdev->vga.region[QEMU_PCI_VGA_MEM],
                              "vfio-vga-mmio@0xa0000",
                              QEMU_PCI_VGA_MEM_SIZE);
        memory_region_init_io(&vdev->vga.region[QEMU_PCI_VGA_IO_LO].mem,
                              OBJECT(vdev), &vfio_vga_ops,
                              &vdev->vga.region[QEMU_PCI_VGA_IO_LO],
                              "vfio-vga-io@0x3b0",
                              QEMU_PCI_VGA_IO_LO_SIZE);
        memory_region_init_io(&vdev->vga.region[QEMU_PCI_VGA_IO_HI].mem,
                              OBJECT(vdev), &vfio_vga_ops,
                              &vdev->vga.region[QEMU_PCI_VGA_IO_HI],
                              "vfio-vga-io@0x3c0",
                              QEMU_PCI_VGA_IO_HI_SIZE);

        pci_register_vga(&vdev->pdev, &vdev->vga.region[QEMU_PCI_VGA_MEM].mem,
                         &vdev->vga.region[QEMU_PCI_VGA_IO_LO].mem,
                         &vdev->vga.region[QEMU_PCI_VGA_IO_HI].mem);
        vfio_vga_quirk_setup(vdev);
    }
}

static void vfio_unregister_bars(VFIOPCIDevice *vdev)
{
    int i;

    for (i = 0; i < PCI_ROM_SLOT; i++) {
        vfio_unregister_bar(vdev, i);
    }

    if (vdev->has_vga) {
        vfio_vga_quirk_teardown(vdev);
        pci_unregister_vga(&vdev->pdev);
    }
}

static void vfio_unmap_bars(VFIOPCIDevice *vdev)
{
    int i;

    for (i = 0; i < PCI_ROM_SLOT; i++) {
        vfio_unmap_bar(vdev, i);
    }

    if (vdev->has_vga) {
        vfio_vga_quirk_free(vdev);
    }
}

/*
 * General setup
 */
static uint8_t vfio_std_cap_max_size(PCIDevice *pdev, uint8_t pos)
{
    uint8_t tmp, next = 0xff;

    for (tmp = pdev->config[PCI_CAPABILITY_LIST]; tmp;
         tmp = pdev->config[tmp + 1]) {
        if (tmp > pos && tmp < next) {
            next = tmp;
        }
    }

    return next - pos;
}

static void vfio_set_word_bits(uint8_t *buf, uint16_t val, uint16_t mask)
{
    pci_set_word(buf, (pci_get_word(buf) & ~mask) | val);
}

static void vfio_add_emulated_word(VFIOPCIDevice *vdev, int pos,
                                   uint16_t val, uint16_t mask)
{
    vfio_set_word_bits(vdev->pdev.config + pos, val, mask);
    vfio_set_word_bits(vdev->pdev.wmask + pos, ~mask, mask);
    vfio_set_word_bits(vdev->emulated_config_bits + pos, mask, mask);
}

static void vfio_set_long_bits(uint8_t *buf, uint32_t val, uint32_t mask)
{
    pci_set_long(buf, (pci_get_long(buf) & ~mask) | val);
}

static void vfio_add_emulated_long(VFIOPCIDevice *vdev, int pos,
                                   uint32_t val, uint32_t mask)
{
    vfio_set_long_bits(vdev->pdev.config + pos, val, mask);
    vfio_set_long_bits(vdev->pdev.wmask + pos, ~mask, mask);
    vfio_set_long_bits(vdev->emulated_config_bits + pos, mask, mask);
}

static int vfio_setup_pcie_cap(VFIOPCIDevice *vdev, int pos, uint8_t size)
{
    uint16_t flags;
    uint8_t type;

    flags = pci_get_word(vdev->pdev.config + pos + PCI_CAP_FLAGS);
    type = (flags & PCI_EXP_FLAGS_TYPE) >> 4;

    if (type != PCI_EXP_TYPE_ENDPOINT &&
        type != PCI_EXP_TYPE_LEG_END &&
        type != PCI_EXP_TYPE_RC_END) {

        error_report("vfio: Assignment of PCIe type 0x%x "
                     "devices is not currently supported", type);
        return -EINVAL;
    }

    if (!pci_bus_is_express(vdev->pdev.bus)) {
        PCIBus *bus = vdev->pdev.bus;
        PCIDevice *bridge;

        /*
         * Traditionally PCI device assignment exposes the PCIe capability
         * as-is on non-express buses.  The reason being that some drivers
         * simply assume that it's there, for example tg3.  However when
         * we're running on a native PCIe machine type, like Q35, we need
         * to hide the PCIe capability.  The reason for this is twofold;
         * first Windows guests get a Code 10 error when the PCIe capability
         * is exposed in this configuration.  Therefore express devices won't
         * work at all unless they're attached to express buses in the VM.
         * Second, a native PCIe machine introduces the possibility of fine
         * granularity IOMMUs supporting both translation and isolation.
         * Guest code to discover the IOMMU visibility of a device, such as
         * IOMMU grouping code on Linux, is very aware of device types and
         * valid transitions between bus types.  An express device on a non-
         * express bus is not a valid combination on bare metal systems.
         *
         * Drivers that require a PCIe capability to make the device
         * functional are simply going to need to have their devices placed
         * on a PCIe bus in the VM.
         */
        while (!pci_bus_is_root(bus)) {
            bridge = pci_bridge_get_device(bus);
            bus = bridge->bus;
        }

        if (pci_bus_is_express(bus)) {
            return 0;
        }

    } else if (pci_bus_is_root(vdev->pdev.bus)) {
        /*
         * On a Root Complex bus Endpoints become Root Complex Integrated
         * Endpoints, which changes the type and clears the LNK & LNK2 fields.
         */
        if (type == PCI_EXP_TYPE_ENDPOINT) {
            vfio_add_emulated_word(vdev, pos + PCI_CAP_FLAGS,
                                   PCI_EXP_TYPE_RC_END << 4,
                                   PCI_EXP_FLAGS_TYPE);

            /* Link Capabilities, Status, and Control goes away */
            if (size > PCI_EXP_LNKCTL) {
                vfio_add_emulated_long(vdev, pos + PCI_EXP_LNKCAP, 0, ~0);
                vfio_add_emulated_word(vdev, pos + PCI_EXP_LNKCTL, 0, ~0);
                vfio_add_emulated_word(vdev, pos + PCI_EXP_LNKSTA, 0, ~0);

#ifndef PCI_EXP_LNKCAP2
#define PCI_EXP_LNKCAP2 44
#endif
#ifndef PCI_EXP_LNKSTA2
#define PCI_EXP_LNKSTA2 50
#endif
                /* Link 2 Capabilities, Status, and Control goes away */
                if (size > PCI_EXP_LNKCAP2) {
                    vfio_add_emulated_long(vdev, pos + PCI_EXP_LNKCAP2, 0, ~0);
                    vfio_add_emulated_word(vdev, pos + PCI_EXP_LNKCTL2, 0, ~0);
                    vfio_add_emulated_word(vdev, pos + PCI_EXP_LNKSTA2, 0, ~0);
                }
            }

        } else if (type == PCI_EXP_TYPE_LEG_END) {
            /*
             * Legacy endpoints don't belong on the root complex.  Windows
             * seems to be happier with devices if we skip the capability.
             */
            return 0;
        }

    } else {
        /*
         * Convert Root Complex Integrated Endpoints to regular endpoints.
         * These devices don't support LNK/LNK2 capabilities, so make them up.
         */
        if (type == PCI_EXP_TYPE_RC_END) {
            vfio_add_emulated_word(vdev, pos + PCI_CAP_FLAGS,
                                   PCI_EXP_TYPE_ENDPOINT << 4,
                                   PCI_EXP_FLAGS_TYPE);
            vfio_add_emulated_long(vdev, pos + PCI_EXP_LNKCAP,
                                   PCI_EXP_LNK_MLW_1 | PCI_EXP_LNK_LS_25, ~0);
            vfio_add_emulated_word(vdev, pos + PCI_EXP_LNKCTL, 0, ~0);
        }

        /* Mark the Link Status bits as emulated to allow virtual negotiation */
        vfio_add_emulated_word(vdev, pos + PCI_EXP_LNKSTA,
                               pci_get_word(vdev->pdev.config + pos +
                                            PCI_EXP_LNKSTA),
                               PCI_EXP_LNKCAP_MLW | PCI_EXP_LNKCAP_SLS);
    }

    pos = pci_add_capability(&vdev->pdev, PCI_CAP_ID_EXP, pos, size);
    if (pos >= 0) {
        vdev->pdev.exp.exp_cap = pos;
    }

    return pos;
}

static void vfio_check_pcie_flr(VFIOPCIDevice *vdev, uint8_t pos)
{
    uint32_t cap = pci_get_long(vdev->pdev.config + pos + PCI_EXP_DEVCAP);

    if (cap & PCI_EXP_DEVCAP_FLR) {
        trace_vfio_check_pcie_flr(vdev->vbasedev.name);
        vdev->has_flr = true;
    }
}

static void vfio_check_pm_reset(VFIOPCIDevice *vdev, uint8_t pos)
{
    uint16_t csr = pci_get_word(vdev->pdev.config + pos + PCI_PM_CTRL);

    if (!(csr & PCI_PM_CTRL_NO_SOFT_RESET)) {
        trace_vfio_check_pm_reset(vdev->vbasedev.name);
        vdev->has_pm_reset = true;
    }
}

static void vfio_check_af_flr(VFIOPCIDevice *vdev, uint8_t pos)
{
    uint8_t cap = pci_get_byte(vdev->pdev.config + pos + PCI_AF_CAP);

    if ((cap & PCI_AF_CAP_TP) && (cap & PCI_AF_CAP_FLR)) {
        trace_vfio_check_af_flr(vdev->vbasedev.name);
        vdev->has_flr = true;
    }
}

static int vfio_add_std_cap(VFIOPCIDevice *vdev, uint8_t pos)
{
    PCIDevice *pdev = &vdev->pdev;
    uint8_t cap_id, next, size;
    int ret;

    cap_id = pdev->config[pos];
    next = pdev->config[pos + 1];

    /*
     * If it becomes important to configure capabilities to their actual
     * size, use this as the default when it's something we don't recognize.
     * Since QEMU doesn't actually handle many of the config accesses,
     * exact size doesn't seem worthwhile.
     */
    size = vfio_std_cap_max_size(pdev, pos);

    /*
     * pci_add_capability always inserts the new capability at the head
     * of the chain.  Therefore to end up with a chain that matches the
     * physical device, we insert from the end by making this recursive.
     * This is also why we pre-caclulate size above as cached config space
     * will be changed as we unwind the stack.
     */
    if (next) {
        ret = vfio_add_std_cap(vdev, next);
        if (ret) {
            return ret;
        }
    } else {
        /* Begin the rebuild, use QEMU emulated list bits */
        pdev->config[PCI_CAPABILITY_LIST] = 0;
        vdev->emulated_config_bits[PCI_CAPABILITY_LIST] = 0xff;
        vdev->emulated_config_bits[PCI_STATUS] |= PCI_STATUS_CAP_LIST;
    }

    /* Use emulated next pointer to allow dropping caps */
    pci_set_byte(vdev->emulated_config_bits + pos + 1, 0xff);

    switch (cap_id) {
    case PCI_CAP_ID_MSI:
        ret = vfio_msi_setup(vdev, pos);
        break;
    case PCI_CAP_ID_EXP:
        vfio_check_pcie_flr(vdev, pos);
        ret = vfio_setup_pcie_cap(vdev, pos, size);
        break;
    case PCI_CAP_ID_MSIX:
        ret = vfio_msix_setup(vdev, pos);
        break;
    case PCI_CAP_ID_PM:
        vfio_check_pm_reset(vdev, pos);
        vdev->pm_cap = pos;
        ret = pci_add_capability(pdev, cap_id, pos, size);
        break;
    case PCI_CAP_ID_AF:
        vfio_check_af_flr(vdev, pos);
        ret = pci_add_capability(pdev, cap_id, pos, size);
        break;
    default:
        ret = pci_add_capability(pdev, cap_id, pos, size);
        break;
    }

    if (ret < 0) {
        error_report("vfio: %04x:%02x:%02x.%x Error adding PCI capability "
                     "0x%x[0x%x]@0x%x: %d", vdev->host.domain,
                     vdev->host.bus, vdev->host.slot, vdev->host.function,
                     cap_id, size, pos, ret);
        return ret;
    }

    return 0;
}

static int vfio_add_capabilities(VFIOPCIDevice *vdev)
{
    PCIDevice *pdev = &vdev->pdev;

    if (!(pdev->config[PCI_STATUS] & PCI_STATUS_CAP_LIST) ||
        !pdev->config[PCI_CAPABILITY_LIST]) {
        return 0; /* Nothing to add */
    }

    return vfio_add_std_cap(vdev, pdev->config[PCI_CAPABILITY_LIST]);
}

static void vfio_pci_pre_reset(VFIOPCIDevice *vdev)
{
    PCIDevice *pdev = &vdev->pdev;
    uint16_t cmd;

    vfio_disable_interrupts(vdev);

    /* Make sure the device is in D0 */
    if (vdev->pm_cap) {
        uint16_t pmcsr;
        uint8_t state;

        pmcsr = vfio_pci_read_config(pdev, vdev->pm_cap + PCI_PM_CTRL, 2);
        state = pmcsr & PCI_PM_CTRL_STATE_MASK;
        if (state) {
            pmcsr &= ~PCI_PM_CTRL_STATE_MASK;
            vfio_pci_write_config(pdev, vdev->pm_cap + PCI_PM_CTRL, pmcsr, 2);
            /* vfio handles the necessary delay here */
            pmcsr = vfio_pci_read_config(pdev, vdev->pm_cap + PCI_PM_CTRL, 2);
            state = pmcsr & PCI_PM_CTRL_STATE_MASK;
            if (state) {
                error_report("vfio: Unable to power on device, stuck in D%d",
                             state);
            }
        }
    }

    /*
     * Stop any ongoing DMA by disconecting I/O, MMIO, and bus master.
     * Also put INTx Disable in known state.
     */
    cmd = vfio_pci_read_config(pdev, PCI_COMMAND, 2);
    cmd &= ~(PCI_COMMAND_IO | PCI_COMMAND_MEMORY | PCI_COMMAND_MASTER |
             PCI_COMMAND_INTX_DISABLE);
    vfio_pci_write_config(pdev, PCI_COMMAND, cmd, 2);
}

static void vfio_pci_post_reset(VFIOPCIDevice *vdev)
{
    vfio_intx_enable(vdev);
}

static bool vfio_pci_host_match(PCIHostDeviceAddress *host1,
                                PCIHostDeviceAddress *host2)
{
    return (host1->domain == host2->domain && host1->bus == host2->bus &&
            host1->slot == host2->slot && host1->function == host2->function);
}

static int vfio_pci_hot_reset(VFIOPCIDevice *vdev, bool single)
{
    VFIOGroup *group;
    struct vfio_pci_hot_reset_info *info;
    struct vfio_pci_dependent_device *devices;
    struct vfio_pci_hot_reset *reset;
    int32_t *fds;
    int ret, i, count;
    bool multi = false;

    trace_vfio_pci_hot_reset(vdev->vbasedev.name, single ? "one" : "multi");

    vfio_pci_pre_reset(vdev);
    vdev->vbasedev.needs_reset = false;

    info = g_malloc0(sizeof(*info));
    info->argsz = sizeof(*info);

    ret = ioctl(vdev->vbasedev.fd, VFIO_DEVICE_GET_PCI_HOT_RESET_INFO, info);
    if (ret && errno != ENOSPC) {
        ret = -errno;
        if (!vdev->has_pm_reset) {
            error_report("vfio: Cannot reset device %04x:%02x:%02x.%x, "
                         "no available reset mechanism.", vdev->host.domain,
                         vdev->host.bus, vdev->host.slot, vdev->host.function);
        }
        goto out_single;
    }

    count = info->count;
    info = g_realloc(info, sizeof(*info) + (count * sizeof(*devices)));
    info->argsz = sizeof(*info) + (count * sizeof(*devices));
    devices = &info->devices[0];

    ret = ioctl(vdev->vbasedev.fd, VFIO_DEVICE_GET_PCI_HOT_RESET_INFO, info);
    if (ret) {
        ret = -errno;
        error_report("vfio: hot reset info failed: %m");
        goto out_single;
    }

    trace_vfio_pci_hot_reset_has_dep_devices(vdev->vbasedev.name);

    /* Verify that we have all the groups required */
    for (i = 0; i < info->count; i++) {
        PCIHostDeviceAddress host;
        VFIOPCIDevice *tmp;
        VFIODevice *vbasedev_iter;

        host.domain = devices[i].segment;
        host.bus = devices[i].bus;
        host.slot = PCI_SLOT(devices[i].devfn);
        host.function = PCI_FUNC(devices[i].devfn);

        trace_vfio_pci_hot_reset_dep_devices(host.domain,
                host.bus, host.slot, host.function, devices[i].group_id);

        if (vfio_pci_host_match(&host, &vdev->host)) {
            continue;
        }

        QLIST_FOREACH(group, &vfio_group_list, next) {
            if (group->groupid == devices[i].group_id) {
                break;
            }
        }

        if (!group) {
            if (!vdev->has_pm_reset) {
                error_report("vfio: Cannot reset device %s, "
                             "depends on group %d which is not owned.",
                             vdev->vbasedev.name, devices[i].group_id);
            }
            ret = -EPERM;
            goto out;
        }

        /* Prep dependent devices for reset and clear our marker. */
        QLIST_FOREACH(vbasedev_iter, &group->device_list, next) {
            if (vbasedev_iter->type != VFIO_DEVICE_TYPE_PCI) {
                continue;
            }
            tmp = container_of(vbasedev_iter, VFIOPCIDevice, vbasedev);
            if (vfio_pci_host_match(&host, &tmp->host)) {
                if (single) {
                    ret = -EINVAL;
                    goto out_single;
                }
                vfio_pci_pre_reset(tmp);
                tmp->vbasedev.needs_reset = false;
                multi = true;
                break;
            }
        }
    }

    if (!single && !multi) {
        ret = -EINVAL;
        goto out_single;
    }

    /* Determine how many group fds need to be passed */
    count = 0;
    QLIST_FOREACH(group, &vfio_group_list, next) {
        for (i = 0; i < info->count; i++) {
            if (group->groupid == devices[i].group_id) {
                count++;
                break;
            }
        }
    }

    reset = g_malloc0(sizeof(*reset) + (count * sizeof(*fds)));
    reset->argsz = sizeof(*reset) + (count * sizeof(*fds));
    fds = &reset->group_fds[0];

    /* Fill in group fds */
    QLIST_FOREACH(group, &vfio_group_list, next) {
        for (i = 0; i < info->count; i++) {
            if (group->groupid == devices[i].group_id) {
                fds[reset->count++] = group->fd;
                break;
            }
        }
    }

    /* Bus reset! */
    ret = ioctl(vdev->vbasedev.fd, VFIO_DEVICE_PCI_HOT_RESET, reset);
    g_free(reset);

    trace_vfio_pci_hot_reset_result(vdev->vbasedev.name,
                                    ret ? "%m" : "Success");

out:
    /* Re-enable INTx on affected devices */
    for (i = 0; i < info->count; i++) {
        PCIHostDeviceAddress host;
        VFIOPCIDevice *tmp;
        VFIODevice *vbasedev_iter;

        host.domain = devices[i].segment;
        host.bus = devices[i].bus;
        host.slot = PCI_SLOT(devices[i].devfn);
        host.function = PCI_FUNC(devices[i].devfn);

        if (vfio_pci_host_match(&host, &vdev->host)) {
            continue;
        }

        QLIST_FOREACH(group, &vfio_group_list, next) {
            if (group->groupid == devices[i].group_id) {
                break;
            }
        }

        if (!group) {
            break;
        }

        QLIST_FOREACH(vbasedev_iter, &group->device_list, next) {
            if (vbasedev_iter->type != VFIO_DEVICE_TYPE_PCI) {
                continue;
            }
            tmp = container_of(vbasedev_iter, VFIOPCIDevice, vbasedev);
            if (vfio_pci_host_match(&host, &tmp->host)) {
                vfio_pci_post_reset(tmp);
                break;
            }
        }
    }
out_single:
    vfio_pci_post_reset(vdev);
    g_free(info);

    return ret;
}

/*
 * We want to differentiate hot reset of mulitple in-use devices vs hot reset
 * of a single in-use device.  VFIO_DEVICE_RESET will already handle the case
 * of doing hot resets when there is only a single device per bus.  The in-use
 * here refers to how many VFIODevices are affected.  A hot reset that affects
 * multiple devices, but only a single in-use device, means that we can call
 * it from our bus ->reset() callback since the extent is effectively a single
 * device.  This allows us to make use of it in the hotplug path.  When there
 * are multiple in-use devices, we can only trigger the hot reset during a
 * system reset and thus from our reset handler.  We separate _one vs _multi
 * here so that we don't overlap and do a double reset on the system reset
 * path where both our reset handler and ->reset() callback are used.  Calling
 * _one() will only do a hot reset for the one in-use devices case, calling
 * _multi() will do nothing if a _one() would have been sufficient.
 */
static int vfio_pci_hot_reset_one(VFIOPCIDevice *vdev)
{
    return vfio_pci_hot_reset(vdev, true);
}

static int vfio_pci_hot_reset_multi(VFIODevice *vbasedev)
{
    VFIOPCIDevice *vdev = container_of(vbasedev, VFIOPCIDevice, vbasedev);
    return vfio_pci_hot_reset(vdev, false);
}

static void vfio_pci_compute_needs_reset(VFIODevice *vbasedev)
{
    VFIOPCIDevice *vdev = container_of(vbasedev, VFIOPCIDevice, vbasedev);
    if (!vbasedev->reset_works || (!vdev->has_flr && vdev->has_pm_reset)) {
        vbasedev->needs_reset = true;
    }
}

static VFIODeviceOps vfio_pci_ops = {
    .vfio_compute_needs_reset = vfio_pci_compute_needs_reset,
    .vfio_hot_reset_multi = vfio_pci_hot_reset_multi,
    .vfio_eoi = vfio_intx_eoi,
};

static int vfio_populate_device(VFIOPCIDevice *vdev)
{
    VFIODevice *vbasedev = &vdev->vbasedev;
    struct vfio_region_info reg_info = { .argsz = sizeof(reg_info) };
    struct vfio_irq_info irq_info = { .argsz = sizeof(irq_info) };
    int i, ret = -1;

    /* Sanity check device */
    if (!(vbasedev->flags & VFIO_DEVICE_FLAGS_PCI)) {
        error_report("vfio: Um, this isn't a PCI device");
        goto error;
    }

    if (vbasedev->num_regions < VFIO_PCI_CONFIG_REGION_INDEX + 1) {
        error_report("vfio: unexpected number of io regions %u",
                     vbasedev->num_regions);
        goto error;
    }

    if (vbasedev->num_irqs < VFIO_PCI_MSIX_IRQ_INDEX + 1) {
        error_report("vfio: unexpected number of irqs %u", vbasedev->num_irqs);
        goto error;
    }

    for (i = VFIO_PCI_BAR0_REGION_INDEX; i < VFIO_PCI_ROM_REGION_INDEX; i++) {
        reg_info.index = i;

        ret = ioctl(vbasedev->fd, VFIO_DEVICE_GET_REGION_INFO, &reg_info);
        if (ret) {
            error_report("vfio: Error getting region %d info: %m", i);
            goto error;
        }

        trace_vfio_populate_device_region(vbasedev->name, i,
                                          (unsigned long)reg_info.size,
                                          (unsigned long)reg_info.offset,
                                          (unsigned long)reg_info.flags);

        vdev->bars[i].region.vbasedev = vbasedev;
        vdev->bars[i].region.flags = reg_info.flags;
        vdev->bars[i].region.size = reg_info.size;
        vdev->bars[i].region.fd_offset = reg_info.offset;
        vdev->bars[i].region.nr = i;
        QLIST_INIT(&vdev->bars[i].quirks);
    }

    reg_info.index = VFIO_PCI_CONFIG_REGION_INDEX;

    ret = ioctl(vdev->vbasedev.fd, VFIO_DEVICE_GET_REGION_INFO, &reg_info);
    if (ret) {
        error_report("vfio: Error getting config info: %m");
        goto error;
    }

    trace_vfio_populate_device_config(vdev->vbasedev.name,
                                      (unsigned long)reg_info.size,
                                      (unsigned long)reg_info.offset,
                                      (unsigned long)reg_info.flags);

    vdev->config_size = reg_info.size;
    if (vdev->config_size == PCI_CONFIG_SPACE_SIZE) {
        vdev->pdev.cap_present &= ~QEMU_PCI_CAP_EXPRESS;
    }
    vdev->config_offset = reg_info.offset;

    if ((vdev->features & VFIO_FEATURE_ENABLE_VGA) &&
        vbasedev->num_regions > VFIO_PCI_VGA_REGION_INDEX) {
        struct vfio_region_info vga_info = {
            .argsz = sizeof(vga_info),
            .index = VFIO_PCI_VGA_REGION_INDEX,
         };

        ret = ioctl(vdev->vbasedev.fd, VFIO_DEVICE_GET_REGION_INFO, &vga_info);
        if (ret) {
            error_report(
                "vfio: Device does not support requested feature x-vga");
            goto error;
        }

        if (!(vga_info.flags & VFIO_REGION_INFO_FLAG_READ) ||
            !(vga_info.flags & VFIO_REGION_INFO_FLAG_WRITE) ||
            vga_info.size < 0xbffff + 1) {
            error_report("vfio: Unexpected VGA info, flags 0x%lx, size 0x%lx",
                         (unsigned long)vga_info.flags,
                         (unsigned long)vga_info.size);
            goto error;
        }

        vdev->vga.fd_offset = vga_info.offset;
        vdev->vga.fd = vdev->vbasedev.fd;

        vdev->vga.region[QEMU_PCI_VGA_MEM].offset = QEMU_PCI_VGA_MEM_BASE;
        vdev->vga.region[QEMU_PCI_VGA_MEM].nr = QEMU_PCI_VGA_MEM;
        QLIST_INIT(&vdev->vga.region[QEMU_PCI_VGA_MEM].quirks);

        vdev->vga.region[QEMU_PCI_VGA_IO_LO].offset = QEMU_PCI_VGA_IO_LO_BASE;
        vdev->vga.region[QEMU_PCI_VGA_IO_LO].nr = QEMU_PCI_VGA_IO_LO;
        QLIST_INIT(&vdev->vga.region[QEMU_PCI_VGA_IO_LO].quirks);

        vdev->vga.region[QEMU_PCI_VGA_IO_HI].offset = QEMU_PCI_VGA_IO_HI_BASE;
        vdev->vga.region[QEMU_PCI_VGA_IO_HI].nr = QEMU_PCI_VGA_IO_HI;
        QLIST_INIT(&vdev->vga.region[QEMU_PCI_VGA_IO_HI].quirks);

        vdev->has_vga = true;
    }

    irq_info.index = VFIO_PCI_ERR_IRQ_INDEX;

    ret = ioctl(vdev->vbasedev.fd, VFIO_DEVICE_GET_IRQ_INFO, &irq_info);
    if (ret) {
        /* This can fail for an old kernel or legacy PCI dev */
        trace_vfio_populate_device_get_irq_info_failure();
        ret = 0;
    } else if (irq_info.count == 1) {
        vdev->pci_aer = true;
    } else {
        error_report("vfio: %s "
                     "Could not enable error recovery for the device",
                     vbasedev->name);
    }

error:
    return ret;
}

static void vfio_put_device(VFIOPCIDevice *vdev)
{
    g_free(vdev->vbasedev.name);
    if (vdev->msix) {
        object_unparent(OBJECT(&vdev->msix->mmap_mem));
        g_free(vdev->msix);
        vdev->msix = NULL;
    }
    vfio_put_base_device(&vdev->vbasedev);
}

static void vfio_err_notifier_handler(void *opaque)
{
    VFIOPCIDevice *vdev = opaque;

    if (!event_notifier_test_and_clear(&vdev->err_notifier)) {
        return;
    }

    /*
     * TBD. Retrieve the error details and decide what action
     * needs to be taken. One of the actions could be to pass
     * the error to the guest and have the guest driver recover
     * from the error. This requires that PCIe capabilities be
     * exposed to the guest. For now, we just terminate the
     * guest to contain the error.
     */

    error_report("%s(%04x:%02x:%02x.%x) Unrecoverable error detected.  "
                 "Please collect any data possible and then kill the guest",
                 __func__, vdev->host.domain, vdev->host.bus,
                 vdev->host.slot, vdev->host.function);

    vm_stop(RUN_STATE_INTERNAL_ERROR);
}

/*
 * Registers error notifier for devices supporting error recovery.
 * If we encounter a failure in this function, we report an error
 * and continue after disabling error recovery support for the
 * device.
 */
static void vfio_register_err_notifier(VFIOPCIDevice *vdev)
{
    int ret;
    int argsz;
    struct vfio_irq_set *irq_set;
    int32_t *pfd;

    if (!vdev->pci_aer) {
        return;
    }

    if (event_notifier_init(&vdev->err_notifier, 0)) {
        error_report("vfio: Unable to init event notifier for error detection");
        vdev->pci_aer = false;
        return;
    }

    argsz = sizeof(*irq_set) + sizeof(*pfd);

    irq_set = g_malloc0(argsz);
    irq_set->argsz = argsz;
    irq_set->flags = VFIO_IRQ_SET_DATA_EVENTFD |
                     VFIO_IRQ_SET_ACTION_TRIGGER;
    irq_set->index = VFIO_PCI_ERR_IRQ_INDEX;
    irq_set->start = 0;
    irq_set->count = 1;
    pfd = (int32_t *)&irq_set->data;

    *pfd = event_notifier_get_fd(&vdev->err_notifier);
    qemu_set_fd_handler(*pfd, vfio_err_notifier_handler, NULL, vdev);

    ret = ioctl(vdev->vbasedev.fd, VFIO_DEVICE_SET_IRQS, irq_set);
    if (ret) {
        error_report("vfio: Failed to set up error notification");
        qemu_set_fd_handler(*pfd, NULL, NULL, vdev);
        event_notifier_cleanup(&vdev->err_notifier);
        vdev->pci_aer = false;
    }
    g_free(irq_set);
}

static void vfio_unregister_err_notifier(VFIOPCIDevice *vdev)
{
    int argsz;
    struct vfio_irq_set *irq_set;
    int32_t *pfd;
    int ret;

    if (!vdev->pci_aer) {
        return;
    }

    argsz = sizeof(*irq_set) + sizeof(*pfd);

    irq_set = g_malloc0(argsz);
    irq_set->argsz = argsz;
    irq_set->flags = VFIO_IRQ_SET_DATA_EVENTFD |
                     VFIO_IRQ_SET_ACTION_TRIGGER;
    irq_set->index = VFIO_PCI_ERR_IRQ_INDEX;
    irq_set->start = 0;
    irq_set->count = 1;
    pfd = (int32_t *)&irq_set->data;
    *pfd = -1;

    ret = ioctl(vdev->vbasedev.fd, VFIO_DEVICE_SET_IRQS, irq_set);
    if (ret) {
        error_report("vfio: Failed to de-assign error fd: %m");
    }
    g_free(irq_set);
    qemu_set_fd_handler(event_notifier_get_fd(&vdev->err_notifier),
                        NULL, NULL, vdev);
    event_notifier_cleanup(&vdev->err_notifier);
}

static void vfio_req_notifier_handler(void *opaque)
{
    VFIOPCIDevice *vdev = opaque;

    if (!event_notifier_test_and_clear(&vdev->req_notifier)) {
        return;
    }

    qdev_unplug(&vdev->pdev.qdev, NULL);
}

static void vfio_register_req_notifier(VFIOPCIDevice *vdev)
{
    struct vfio_irq_info irq_info = { .argsz = sizeof(irq_info),
                                      .index = VFIO_PCI_REQ_IRQ_INDEX };
    int argsz;
    struct vfio_irq_set *irq_set;
    int32_t *pfd;

    if (!(vdev->features & VFIO_FEATURE_ENABLE_REQ)) {
        return;
    }

    if (ioctl(vdev->vbasedev.fd,
              VFIO_DEVICE_GET_IRQ_INFO, &irq_info) < 0 || irq_info.count < 1) {
        return;
    }

    if (event_notifier_init(&vdev->req_notifier, 0)) {
        error_report("vfio: Unable to init event notifier for device request");
        return;
    }

    argsz = sizeof(*irq_set) + sizeof(*pfd);

    irq_set = g_malloc0(argsz);
    irq_set->argsz = argsz;
    irq_set->flags = VFIO_IRQ_SET_DATA_EVENTFD |
                     VFIO_IRQ_SET_ACTION_TRIGGER;
    irq_set->index = VFIO_PCI_REQ_IRQ_INDEX;
    irq_set->start = 0;
    irq_set->count = 1;
    pfd = (int32_t *)&irq_set->data;

    *pfd = event_notifier_get_fd(&vdev->req_notifier);
    qemu_set_fd_handler(*pfd, vfio_req_notifier_handler, NULL, vdev);

    if (ioctl(vdev->vbasedev.fd, VFIO_DEVICE_SET_IRQS, irq_set)) {
        error_report("vfio: Failed to set up device request notification");
        qemu_set_fd_handler(*pfd, NULL, NULL, vdev);
        event_notifier_cleanup(&vdev->req_notifier);
    } else {
        vdev->req_enabled = true;
    }

    g_free(irq_set);
}

static void vfio_unregister_req_notifier(VFIOPCIDevice *vdev)
{
    int argsz;
    struct vfio_irq_set *irq_set;
    int32_t *pfd;

    if (!vdev->req_enabled) {
        return;
    }

    argsz = sizeof(*irq_set) + sizeof(*pfd);

    irq_set = g_malloc0(argsz);
    irq_set->argsz = argsz;
    irq_set->flags = VFIO_IRQ_SET_DATA_EVENTFD |
                     VFIO_IRQ_SET_ACTION_TRIGGER;
    irq_set->index = VFIO_PCI_REQ_IRQ_INDEX;
    irq_set->start = 0;
    irq_set->count = 1;
    pfd = (int32_t *)&irq_set->data;
    *pfd = -1;

    if (ioctl(vdev->vbasedev.fd, VFIO_DEVICE_SET_IRQS, irq_set)) {
        error_report("vfio: Failed to de-assign device request fd: %m");
    }
    g_free(irq_set);
    qemu_set_fd_handler(event_notifier_get_fd(&vdev->req_notifier),
                        NULL, NULL, vdev);
    event_notifier_cleanup(&vdev->req_notifier);

    vdev->req_enabled = false;
}

static int vfio_initfn(PCIDevice *pdev)
{
    VFIOPCIDevice *vdev = DO_UPCAST(VFIOPCIDevice, pdev, pdev);
    VFIODevice *vbasedev_iter;
    VFIOGroup *group;
    char path[PATH_MAX], iommu_group_path[PATH_MAX], *group_name;
    ssize_t len;
    struct stat st;
    int groupid;
    int ret;

    /* Check that the host device exists */
    snprintf(path, sizeof(path),
             "/sys/bus/pci/devices/%04x:%02x:%02x.%01x/",
             vdev->host.domain, vdev->host.bus, vdev->host.slot,
             vdev->host.function);
    if (stat(path, &st) < 0) {
        error_report("vfio: error: no such host device: %s", path);
        return -errno;
    }

    vdev->vbasedev.ops = &vfio_pci_ops;

    vdev->vbasedev.type = VFIO_DEVICE_TYPE_PCI;
    vdev->vbasedev.name = g_strdup_printf("%04x:%02x:%02x.%01x",
                                          vdev->host.domain, vdev->host.bus,
                                          vdev->host.slot, vdev->host.function);

    strncat(path, "iommu_group", sizeof(path) - strlen(path) - 1);

    len = readlink(path, iommu_group_path, sizeof(path));
    if (len <= 0 || len >= sizeof(path)) {
        error_report("vfio: error no iommu_group for device");
        return len < 0 ? -errno : -ENAMETOOLONG;
    }

    iommu_group_path[len] = 0;
    group_name = basename(iommu_group_path);

    if (sscanf(group_name, "%d", &groupid) != 1) {
        error_report("vfio: error reading %s: %m", path);
        return -errno;
    }

    trace_vfio_initfn(vdev->vbasedev.name, groupid);

    group = vfio_get_group(groupid, pci_device_iommu_address_space(pdev));
    if (!group) {
        error_report("vfio: failed to get group %d", groupid);
        return -ENOENT;
    }

    snprintf(path, sizeof(path), "%04x:%02x:%02x.%01x",
            vdev->host.domain, vdev->host.bus, vdev->host.slot,
            vdev->host.function);

    QLIST_FOREACH(vbasedev_iter, &group->device_list, next) {
        if (strcmp(vbasedev_iter->name, vdev->vbasedev.name) == 0) {
            error_report("vfio: error: device %s is already attached", path);
            vfio_put_group(group);
            return -EBUSY;
        }
    }

    ret = vfio_get_device(group, path, &vdev->vbasedev);
    if (ret) {
        error_report("vfio: failed to get device %s", path);
        vfio_put_group(group);
        return ret;
    }

    ret = vfio_populate_device(vdev);
    if (ret) {
        return ret;
    }

    /* Get a copy of config space */
    ret = pread(vdev->vbasedev.fd, vdev->pdev.config,
                MIN(pci_config_size(&vdev->pdev), vdev->config_size),
                vdev->config_offset);
    if (ret < (int)MIN(pci_config_size(&vdev->pdev), vdev->config_size)) {
        ret = ret < 0 ? -errno : -EFAULT;
        error_report("vfio: Failed to read device config space");
        return ret;
    }

    /* vfio emulates a lot for us, but some bits need extra love */
    vdev->emulated_config_bits = g_malloc0(vdev->config_size);

    /* QEMU can choose to expose the ROM or not */
    memset(vdev->emulated_config_bits + PCI_ROM_ADDRESS, 0xff, 4);

    /*
     * The PCI spec reserves vendor ID 0xffff as an invalid value.  The
     * device ID is managed by the vendor and need only be a 16-bit value.
     * Allow any 16-bit value for subsystem so they can be hidden or changed.
     */
    if (vdev->vendor_id != PCI_ANY_ID) {
        if (vdev->vendor_id >= 0xffff) {
            error_report("vfio: Invalid PCI vendor ID provided");
            return -EINVAL;
        }
        vfio_add_emulated_word(vdev, PCI_VENDOR_ID, vdev->vendor_id, ~0);
        trace_vfio_pci_emulated_vendor_id(vdev->vbasedev.name, vdev->vendor_id);
    } else {
        vdev->vendor_id = pci_get_word(pdev->config + PCI_VENDOR_ID);
    }

    if (vdev->device_id != PCI_ANY_ID) {
        if (vdev->device_id > 0xffff) {
            error_report("vfio: Invalid PCI device ID provided");
            return -EINVAL;
        }
        vfio_add_emulated_word(vdev, PCI_DEVICE_ID, vdev->device_id, ~0);
        trace_vfio_pci_emulated_device_id(vdev->vbasedev.name, vdev->device_id);
    } else {
        vdev->device_id = pci_get_word(pdev->config + PCI_DEVICE_ID);
    }

    if (vdev->sub_vendor_id != PCI_ANY_ID) {
        if (vdev->sub_vendor_id > 0xffff) {
            error_report("vfio: Invalid PCI subsystem vendor ID provided");
            return -EINVAL;
        }
        vfio_add_emulated_word(vdev, PCI_SUBSYSTEM_VENDOR_ID,
                               vdev->sub_vendor_id, ~0);
        trace_vfio_pci_emulated_sub_vendor_id(vdev->vbasedev.name,
                                              vdev->sub_vendor_id);
    }

    if (vdev->sub_device_id != PCI_ANY_ID) {
        if (vdev->sub_device_id > 0xffff) {
            error_report("vfio: Invalid PCI subsystem device ID provided");
            return -EINVAL;
        }
        vfio_add_emulated_word(vdev, PCI_SUBSYSTEM_ID, vdev->sub_device_id, ~0);
        trace_vfio_pci_emulated_sub_device_id(vdev->vbasedev.name,
                                              vdev->sub_device_id);
    }

    /* QEMU can change multi-function devices to single function, or reverse */
    vdev->emulated_config_bits[PCI_HEADER_TYPE] =
                                              PCI_HEADER_TYPE_MULTI_FUNCTION;

    /* Restore or clear multifunction, this is always controlled by QEMU */
    if (vdev->pdev.cap_present & QEMU_PCI_CAP_MULTIFUNCTION) {
        vdev->pdev.config[PCI_HEADER_TYPE] |= PCI_HEADER_TYPE_MULTI_FUNCTION;
    } else {
        vdev->pdev.config[PCI_HEADER_TYPE] &= ~PCI_HEADER_TYPE_MULTI_FUNCTION;
    }

    /*
     * Clear host resource mapping info.  If we choose not to register a
     * BAR, such as might be the case with the option ROM, we can get
     * confusing, unwritable, residual addresses from the host here.
     */
    memset(&vdev->pdev.config[PCI_BASE_ADDRESS_0], 0, 24);
    memset(&vdev->pdev.config[PCI_ROM_ADDRESS], 0, 4);

    vfio_pci_size_rom(vdev);

    ret = vfio_msix_early_setup(vdev);
    if (ret) {
        return ret;
    }

    vfio_map_bars(vdev);

    ret = vfio_add_capabilities(vdev);
    if (ret) {
        goto out_teardown;
    }

    /* QEMU emulates all of MSI & MSIX */
    if (pdev->cap_present & QEMU_PCI_CAP_MSIX) {
        memset(vdev->emulated_config_bits + pdev->msix_cap, 0xff,
               MSIX_CAP_LENGTH);
    }

    if (pdev->cap_present & QEMU_PCI_CAP_MSI) {
        memset(vdev->emulated_config_bits + pdev->msi_cap, 0xff,
               vdev->msi_cap_size);
    }

    if (vfio_pci_read_config(&vdev->pdev, PCI_INTERRUPT_PIN, 1)) {
        vdev->intx.mmap_timer = timer_new_ms(QEMU_CLOCK_VIRTUAL,
                                                  vfio_intx_mmap_enable, vdev);
        pci_device_set_intx_routing_notifier(&vdev->pdev, vfio_intx_update);
        ret = vfio_intx_enable(vdev);
        if (ret) {
            goto out_teardown;
        }
    }

    vfio_register_err_notifier(vdev);
    vfio_register_req_notifier(vdev);
    vfio_setup_resetfn_quirk(vdev);

    return 0;

out_teardown:
    pci_device_set_intx_routing_notifier(&vdev->pdev, NULL);
    vfio_teardown_msi(vdev);
    vfio_unregister_bars(vdev);
    return ret;
}

static void vfio_instance_finalize(Object *obj)
{
    PCIDevice *pci_dev = PCI_DEVICE(obj);
    VFIOPCIDevice *vdev = DO_UPCAST(VFIOPCIDevice, pdev, pci_dev);
    VFIOGroup *group = vdev->vbasedev.group;

    vfio_unmap_bars(vdev);
    g_free(vdev->emulated_config_bits);
    g_free(vdev->rom);
    vfio_put_device(vdev);
    vfio_put_group(group);
}

static void vfio_exitfn(PCIDevice *pdev)
{
    VFIOPCIDevice *vdev = DO_UPCAST(VFIOPCIDevice, pdev, pdev);

    vfio_unregister_req_notifier(vdev);
    vfio_unregister_err_notifier(vdev);
    pci_device_set_intx_routing_notifier(&vdev->pdev, NULL);
    vfio_disable_interrupts(vdev);
    if (vdev->intx.mmap_timer) {
        timer_free(vdev->intx.mmap_timer);
    }
    vfio_teardown_msi(vdev);
    vfio_unregister_bars(vdev);
}

static void vfio_pci_reset(DeviceState *dev)
{
    PCIDevice *pdev = DO_UPCAST(PCIDevice, qdev, dev);
    VFIOPCIDevice *vdev = DO_UPCAST(VFIOPCIDevice, pdev, pdev);

    trace_vfio_pci_reset(vdev->vbasedev.name);

    vfio_pci_pre_reset(vdev);

    if (vdev->resetfn && !vdev->resetfn(vdev)) {
        goto post_reset;
    }

    if (vdev->vbasedev.reset_works &&
        (vdev->has_flr || !vdev->has_pm_reset) &&
        !ioctl(vdev->vbasedev.fd, VFIO_DEVICE_RESET)) {
        trace_vfio_pci_reset_flr(vdev->vbasedev.name);
        goto post_reset;
    }

    /* See if we can do our own bus reset */
    if (!vfio_pci_hot_reset_one(vdev)) {
        goto post_reset;
    }

    /* If nothing else works and the device supports PM reset, use it */
    if (vdev->vbasedev.reset_works && vdev->has_pm_reset &&
        !ioctl(vdev->vbasedev.fd, VFIO_DEVICE_RESET)) {
        trace_vfio_pci_reset_pm(vdev->vbasedev.name);
        goto post_reset;
    }

post_reset:
    vfio_pci_post_reset(vdev);
}

static void vfio_instance_init(Object *obj)
{
    PCIDevice *pci_dev = PCI_DEVICE(obj);
    VFIOPCIDevice *vdev = DO_UPCAST(VFIOPCIDevice, pdev, PCI_DEVICE(obj));

    device_add_bootindex_property(obj, &vdev->bootindex,
                                  "bootindex", NULL,
                                  &pci_dev->qdev, NULL);
}

static Property vfio_pci_dev_properties[] = {
    DEFINE_PROP_PCI_HOST_DEVADDR("host", VFIOPCIDevice, host),
    DEFINE_PROP_UINT32("x-intx-mmap-timeout-ms", VFIOPCIDevice,
                       intx.mmap_timeout, 1100),
    DEFINE_PROP_BIT("x-vga", VFIOPCIDevice, features,
                    VFIO_FEATURE_ENABLE_VGA_BIT, false),
    DEFINE_PROP_BIT("x-req", VFIOPCIDevice, features,
                    VFIO_FEATURE_ENABLE_REQ_BIT, true),
    DEFINE_PROP_BOOL("x-no-mmap", VFIOPCIDevice, vbasedev.no_mmap, false),
    DEFINE_PROP_BOOL("x-no-kvm-intx", VFIOPCIDevice, no_kvm_intx, false),
    DEFINE_PROP_BOOL("x-no-kvm-msi", VFIOPCIDevice, no_kvm_msi, false),
    DEFINE_PROP_BOOL("x-no-kvm-msix", VFIOPCIDevice, no_kvm_msix, false),
    DEFINE_PROP_UINT32("x-pci-vendor-id", VFIOPCIDevice, vendor_id, PCI_ANY_ID),
    DEFINE_PROP_UINT32("x-pci-device-id", VFIOPCIDevice, device_id, PCI_ANY_ID),
    DEFINE_PROP_UINT32("x-pci-sub-vendor-id", VFIOPCIDevice,
                       sub_vendor_id, PCI_ANY_ID),
    DEFINE_PROP_UINT32("x-pci-sub-device-id", VFIOPCIDevice,
                       sub_device_id, PCI_ANY_ID),
    /*
     * TODO - support passed fds... is this necessary?
     * DEFINE_PROP_STRING("vfiofd", VFIOPCIDevice, vfiofd_name),
     * DEFINE_PROP_STRING("vfiogroupfd, VFIOPCIDevice, vfiogroupfd_name),
     */
    DEFINE_PROP_END_OF_LIST(),
};

static const VMStateDescription vfio_pci_vmstate = {
    .name = "vfio-pci",
    .unmigratable = 1,
};

static void vfio_pci_dev_class_init(ObjectClass *klass, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(klass);
    PCIDeviceClass *pdc = PCI_DEVICE_CLASS(klass);

    dc->reset = vfio_pci_reset;
    dc->props = vfio_pci_dev_properties;
    dc->vmsd = &vfio_pci_vmstate;
    dc->desc = "VFIO-based PCI device assignment";
    set_bit(DEVICE_CATEGORY_MISC, dc->categories);
    pdc->init = vfio_initfn;
    pdc->exit = vfio_exitfn;
    pdc->config_read = vfio_pci_read_config;
    pdc->config_write = vfio_pci_write_config;
    pdc->is_express = 1; /* We might be */
}

static const TypeInfo vfio_pci_dev_info = {
    .name = "vfio-pci",
    .parent = TYPE_PCI_DEVICE,
    .instance_size = sizeof(VFIOPCIDevice),
    .class_init = vfio_pci_dev_class_init,
    .instance_init = vfio_instance_init,
    .instance_finalize = vfio_instance_finalize,
};

static void register_vfio_pci_dev_type(void)
{
    type_register_static(&vfio_pci_dev_info);
}

type_init(register_vfio_pci_dev_type)
