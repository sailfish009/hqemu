/*
 *  (C) 2016 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#include <unistd.h>
#include <sys/syscall.h>
#include "utils.h"


/*
 * DeleteCFG()
 *  Remove a CFG starting from Root.
 *  CAUTION: The CFG is no longer available after deleting. The caller should
 *  not access the CFG when this function is returned.
 */
void GraphNode::DeleteCFG(GraphNode *Root)
{
    NodeVec VisitStack;
    NodeSet Visited;
    VisitStack.push_back(Root);
    do {
        GraphNode *Parent = VisitStack.back();
        VisitStack.pop_back();
        if (Visited.find(Parent) == Visited.end()) {
            Visited.insert(Parent);
            for (auto Child : Parent->getChildren())
                VisitStack.push_back(Child);
        }
    } while(!VisitStack.empty());

    for (auto I = Visited.begin(), E = Visited.end(); I != E; ++I)
        delete *I;
}

#ifdef LOCK_FREE
/*  Lock-free FIFO queue algorithm of Michael and Scott (MS-queue).
 *  The code is based on the paper published in PODC'96:
 *      Maged M. Michael and Michael L. Scott, "Simple, Fast, and Practical
 *      Non-Blocking and Blocking Concurrent Queue Algorithms," Proc. 15th ACM
 *      Symp. on Principles of Distributed Computing, pages 267-275, 1996.
 */
static inline char CAS2(volatile struct pointer_t *ptr,
                        struct pointer_t _old,
                        struct pointer_t _new)
{
    char flag = 0;

#if defined(__i386__)
    asm volatile("lock; cmpxchg8b %0; setz %1;"
        : "=m" (*ptr), "=q" (flag)
        : "d" (_old.count), "a" (_old.ptr), "c" (_new.count), "b" (_new.ptr)
        : "memory", "cc");
#elif defined(__x86_64__)
    asm volatile("lock; cmpxchg16b %0; setz %1;"
        : "=m" (*ptr), "=q" (flag)
        : "d" (_old.count), "a" (_old.ptr), "c" (_new.count), "b" (_new.ptr)
        : "memory", "cc");
#elif defined(__arm__)
    unsigned long oldval, res;
    asm volatile("ldrex  %1, [%3]\n"
                 "mov    %0, #0\n"
                 "teq    %1, %4\n"
                 "strexeq %0, %5, [%3]\n"
        : "=&r" (res), "=&r" (oldval), "+Qo" (*ptr->ptr)
        : "r" (ptr->ptr), "Ir" (_old.ptr), "r" (_new.ptr)
        : "cc");
    flag = !res;
#endif
    return flag;
}

Queue::Queue()
{
    node_t *dummy = new_node(nullptr);
    Q.head.ptr = Q.tail.ptr = dummy;
    Q.head.count = Q.tail.count = 0;
}

void Queue::enqueue(void *data)
{
    pointer_t tail, next, insert;
    node_t *node = new_node(data);
    insert.ptr = node;

    for (;;) {
        tail = Q.tail;
        next = tail.ptr->next;
        
        /* If Tail is consistent (addresses and versions are not changed),
           continue to enqueue. */
        if (CAS2(&Q.tail, tail, Q.tail)) {
            /* If Tail is pointing to the last node, continue to enqueue.
               Otherwise, try to advance Tail because it might be pointing
               to the second last node. */
            if (next.ptr == nullptr) {  /* Last node */
                /* Try to insert node at the end of the linked list.
                   if it succeeds, exit the loop. */
                insert.count = next.count + 1;
                if (CAS2(&(tail.ptr->next), next, insert))
                    break;
            } else {
                next.count = tail.count + 1;
                CAS2(&Q.tail, tail, next);
            }
        }
    }
    
    /* Enqueue is done, try to swing Tail to the inserted node. */
    insert.count = tail.count + 1;
    CAS2(&Q.tail, tail, insert);
}

void *Queue::dequeue()
{
    pointer_t head, tail, next;
    void *data;

    for (;;) {
        head = Q.head;
        tail = Q.tail;
        next = head.ptr->next;
        
        /* If Head is consistent (addresses and versions are not changed),
           continue to dequeue. */
        if (CAS2(&Q.head, head, Q.head)) {
            /* If Queue is empty, stop dequeueing. If Tail falling behind, 
               try to advance it. Otherwise, continue to dequeue. */
            if (head.ptr == tail.ptr) {
                if (next.ptr == nullptr) /* Queue is empty */
                    return nullptr;
                
                /* Tail is falling behand, try to advance it. */
                next.count = tail.count + 1;
                CAS2(&Q.tail, tail, next);
            } else {
                /* We must read value before CAS, otherwise another dequeue 
                   might free the next node. */
                data = next.ptr->value;
                next.count = head.count + 1;
                if (CAS2(&Q.head, head, next))
                    break;
            }
        }
    }
    
    /* Dequeue succeeded. It is safe to free the dummy node.
       Node pointed by Head becomes the new dummy node */
    delete_node(head.ptr);

    return data;
}
#else
Queue::Queue(void)
{
    node_t *dummy = new node_t(nullptr);
    Q.head = Q.tail = dummy;
    pthread_mutex_init(&lock, nullptr);
}

void Queue::enqueue(void *data)
{
    node_t *node = new node_t(data);

    pthread_mutex_lock(&lock);
    Q.tail->next = node;
    Q.tail = node;
    pthread_mutex_unlock(&lock);
}

void *Queue::dequeue()
{
    node_t *node, *new_head;
    void *data;

    pthread_mutex_lock(&lock);
    node = Q.head;
    new_head = node->next;
    if (new_head == nullptr) {
        pthread_mutex_unlock(&lock);
        return nullptr;
    }

    data = new_head->value;
    Q.head = new_head;
    pthread_mutex_unlock(&lock);

    delete node;
    return data;
}
#endif

/*
 * gettid()
 *  Get the thread ID.
 */
pid_t gettid()
{
#ifdef SYS_gettid
    return (pid_t)syscall(SYS_gettid);
#elif defined(__NR_gettid)
    return (pid_t)syscall(__NR_gettid);
#else
    return -1;
#endif
}


/*
 * patch_jmp()
 *  Patch a direct jump from patch_addr to addr.
 */
void patch_jmp(volatile uintptr_t patch_addr, volatile uintptr_t addr)
{
#if defined(__i386__) || defined(__x86_64__)
    tb_set_jmp_target1(patch_addr + 1, addr);
#elif defined(__arm__)
    *(uintptr_t *)patch_addr = addr;
#elif defined(_ARCH_PPC)
    ppc_tb_set_jmp_target(patch_addr, addr);
#endif
    barrier();
}

void patch_jmp(volatile uintptr_t patch_addr, volatile void *addr)
{
    patch_jmp(patch_addr, (uintptr_t)addr);
}

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */

