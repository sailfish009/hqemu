/*
 * ARM Platform Bus device tree generation helpers
 *
 * Copyright (c) 2014 Linaro Limited
 *
 * Authors:
 *  Alex Graf <agraf@suse.de>
 *  Eric Auger <eric.auger@linaro.org>
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms and conditions of the GNU General Public License,
 * version 2 or later, as published by the Free Software Foundation.
 *
 * This program is distributed in the hope it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#include "hw/arm/sysbus-fdt.h"
#include "qemu/error-report.h"
#include "sysemu/device_tree.h"
#include "hw/platform-bus.h"
#include "sysemu/sysemu.h"
#include "hw/vfio/vfio-platform.h"
#include "hw/vfio/vfio-calxeda-xgmac.h"
#include "hw/arm/fdt.h"

/*
 * internal struct that contains the information to create dynamic
 * sysbus device node
 */
typedef struct PlatformBusFDTData {
    void *fdt; /* device tree handle */
    int irq_start; /* index of the first IRQ usable by platform bus devices */
    const char *pbus_node_name; /* name of the platform bus node */
    PlatformBusDevice *pbus;
} PlatformBusFDTData;

/*
 * struct used when calling the machine init done notifier
 * that constructs the fdt nodes of platform bus devices
 */
typedef struct PlatformBusFDTNotifierParams {
    Notifier notifier;
    ARMPlatformBusFDTParams *fdt_params;
} PlatformBusFDTNotifierParams;

/* struct that associates a device type name and a node creation function */
typedef struct NodeCreationPair {
    const char *typename;
    int (*add_fdt_node_fn)(SysBusDevice *sbdev, void *opaque);
} NodeCreationPair;

/* Device Specific Code */

/**
 * add_calxeda_midway_xgmac_fdt_node
 *
 * Generates a simple node with following properties:
 * compatible string, regs, interrupts, dma-coherent
 */
static int add_calxeda_midway_xgmac_fdt_node(SysBusDevice *sbdev, void *opaque)
{
    PlatformBusFDTData *data = opaque;
    PlatformBusDevice *pbus = data->pbus;
    void *fdt = data->fdt;
    const char *parent_node = data->pbus_node_name;
    int compat_str_len, i, ret = -1;
    char *nodename;
    uint32_t *irq_attr, *reg_attr;
    uint64_t mmio_base, irq_number;
    VFIOPlatformDevice *vdev = VFIO_PLATFORM_DEVICE(sbdev);
    VFIODevice *vbasedev = &vdev->vbasedev;

    mmio_base = platform_bus_get_mmio_addr(pbus, sbdev, 0);
    nodename = g_strdup_printf("%s/%s@%" PRIx64, parent_node,
                               vbasedev->name, mmio_base);
    qemu_fdt_add_subnode(fdt, nodename);

    compat_str_len = strlen(vdev->compat) + 1;
    qemu_fdt_setprop(fdt, nodename, "compatible",
                          vdev->compat, compat_str_len);

    qemu_fdt_setprop(fdt, nodename, "dma-coherent", "", 0);

    reg_attr = g_new(uint32_t, vbasedev->num_regions * 2);
    for (i = 0; i < vbasedev->num_regions; i++) {
        mmio_base = platform_bus_get_mmio_addr(pbus, sbdev, i);
        reg_attr[2 * i] = cpu_to_be32(mmio_base);
        reg_attr[2 * i + 1] = cpu_to_be32(
                                memory_region_size(&vdev->regions[i]->mem));
    }
    ret = qemu_fdt_setprop(fdt, nodename, "reg", reg_attr,
                           vbasedev->num_regions * 2 * sizeof(uint32_t));
    if (ret) {
        error_report("could not set reg property of node %s", nodename);
        goto fail_reg;
    }

    irq_attr = g_new(uint32_t, vbasedev->num_irqs * 3);
    for (i = 0; i < vbasedev->num_irqs; i++) {
        irq_number = platform_bus_get_irqn(pbus, sbdev , i)
                         + data->irq_start;
        irq_attr[3 * i] = cpu_to_be32(GIC_FDT_IRQ_TYPE_SPI);
        irq_attr[3 * i + 1] = cpu_to_be32(irq_number);
        irq_attr[3 * i + 2] = cpu_to_be32(GIC_FDT_IRQ_FLAGS_LEVEL_HI);
    }
    ret = qemu_fdt_setprop(fdt, nodename, "interrupts",
                     irq_attr, vbasedev->num_irqs * 3 * sizeof(uint32_t));
    if (ret) {
        error_report("could not set interrupts property of node %s",
                     nodename);
    }
    g_free(irq_attr);
fail_reg:
    g_free(reg_attr);
    g_free(nodename);
    return ret;
}

/* list of supported dynamic sysbus devices */
static const NodeCreationPair add_fdt_node_functions[] = {
    {TYPE_VFIO_CALXEDA_XGMAC, add_calxeda_midway_xgmac_fdt_node},
    {"", NULL}, /* last element */
};

/* Generic Code */

/**
 * add_fdt_node - add the device tree node of a dynamic sysbus device
 *
 * @sbdev: handle to the sysbus device
 * @opaque: handle to the PlatformBusFDTData
 *
 * Checks the sysbus type belongs to the list of device types that
 * are dynamically instantiable and if so call the node creation
 * function.
 */
static int add_fdt_node(SysBusDevice *sbdev, void *opaque)
{
    int i, ret;

    for (i = 0; i < ARRAY_SIZE(add_fdt_node_functions); i++) {
        if (!strcmp(object_get_typename(OBJECT(sbdev)),
                    add_fdt_node_functions[i].typename)) {
            ret = add_fdt_node_functions[i].add_fdt_node_fn(sbdev, opaque);
            assert(!ret);
            return 0;
        }
    }
    error_report("Device %s can not be dynamically instantiated",
                     qdev_fw_name(DEVICE(sbdev)));
    exit(1);
}

/**
 * add_all_platform_bus_fdt_nodes - create all the platform bus nodes
 *
 * builds the parent platform bus node and all the nodes of dynamic
 * sysbus devices attached to it.
 */
static void add_all_platform_bus_fdt_nodes(ARMPlatformBusFDTParams *fdt_params)
{
    const char platcomp[] = "qemu,platform\0simple-bus";
    PlatformBusDevice *pbus;
    DeviceState *dev;
    gchar *node;
    uint64_t addr, size;
    int irq_start, dtb_size;
    struct arm_boot_info *info = fdt_params->binfo;
    const ARMPlatformBusSystemParams *params = fdt_params->system_params;
    const char *intc = fdt_params->intc;
    void *fdt = info->get_dtb(info, &dtb_size);

    /*
     * If the user provided a dtb, we assume the dynamic sysbus nodes
     * already are integrated there. This corresponds to a use case where
     * the dynamic sysbus nodes are complex and their generation is not yet
     * supported. In that case the user can take charge of the guest dt
     * while qemu takes charge of the qom stuff.
     */
    if (info->dtb_filename) {
        return;
    }

    assert(fdt);

    node = g_strdup_printf("/platform@%"PRIx64, params->platform_bus_base);
    addr = params->platform_bus_base;
    size = params->platform_bus_size;
    irq_start = params->platform_bus_first_irq;

    /* Create a /platform node that we can put all devices into */
    qemu_fdt_add_subnode(fdt, node);
    qemu_fdt_setprop(fdt, node, "compatible", platcomp, sizeof(platcomp));

    /* Our platform bus region is less than 32bits, so 1 cell is enough for
     * address and size
     */
    qemu_fdt_setprop_cells(fdt, node, "#size-cells", 1);
    qemu_fdt_setprop_cells(fdt, node, "#address-cells", 1);
    qemu_fdt_setprop_cells(fdt, node, "ranges", 0, addr >> 32, addr, size);

    qemu_fdt_setprop_phandle(fdt, node, "interrupt-parent", intc);

    dev = qdev_find_recursive(sysbus_get_default(), TYPE_PLATFORM_BUS_DEVICE);
    pbus = PLATFORM_BUS_DEVICE(dev);

    /* We can only create dt nodes for dynamic devices when they're ready */
    assert(pbus->done_gathering);

    PlatformBusFDTData data = {
        .fdt = fdt,
        .irq_start = irq_start,
        .pbus_node_name = node,
        .pbus = pbus,
    };

    /* Loop through all dynamic sysbus devices and create their node */
    foreach_dynamic_sysbus_device(add_fdt_node, &data);

    g_free(node);
}

static void platform_bus_fdt_notify(Notifier *notifier, void *data)
{
    PlatformBusFDTNotifierParams *p = DO_UPCAST(PlatformBusFDTNotifierParams,
                                                notifier, notifier);

    add_all_platform_bus_fdt_nodes(p->fdt_params);
    g_free(p->fdt_params);
    g_free(p);
}

void arm_register_platform_bus_fdt_creator(ARMPlatformBusFDTParams *fdt_params)
{
    PlatformBusFDTNotifierParams *p = g_new(PlatformBusFDTNotifierParams, 1);

    p->fdt_params = fdt_params;
    p->notifier.notify = platform_bus_fdt_notify;
    qemu_add_machine_init_done_notifier(&p->notifier);
}
