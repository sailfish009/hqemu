/*
 *  (C) 2016 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __UTILS_H
#define __UTILS_H

#include <cstdint>
#include <cstdlib>
#include <sstream>
#include <set>
#include <map>
#include <vector>
#include "qemu-types.h"


#ifndef timersub
# define timersub(a, b, result)                                               \
  do {                                                                        \
    (result)->tv_sec = (a)->tv_sec - (b)->tv_sec;                             \
    (result)->tv_usec = (a)->tv_usec - (b)->tv_usec;                          \
    if ((result)->tv_usec < 0) {                                              \
      --(result)->tv_sec;                                                     \
      (result)->tv_usec += 1000000;                                           \
    }                                                                         \
  } while (0)
#endif

#if !defined(__i386__) && !defined(__x86_64__)
#define USE_PTHREAD_MUTEX
#endif

#if defined(USE_PTHREAD_MUTEX)
#  define hqemu_lock_t           pthread_mutex_t
#  define hqemu_lock_init(lock)  pthread_mutex_init(lock, nullptr)
#  define hqemu_lock(lock)       pthread_mutex_lock(lock)
#  define hqemu_unlock(lock)     pthread_mutex_unlock(lock)
#else
#  define hqemu_lock_t           volatile int
#  define hqemu_lock_init(lock)  do { *lock = 0; } while(0)
#  define hqemu_lock(lock)    \
      do {                    \
          while (!Atomic<int>::testandset(lock,0,1)) { \
              while(*(lock)) _mm_pause();              \
          }                                            \
      } while(0)
#  define hqemu_unlock(lock)  \
      do {                    \
          barrier();          \
          *(lock) = 0;        \
      } while(0)
#endif  /* USE_PTHREAD_MUTEX */


/*
 * Atomic Utilities
 */
template<class T>
class Atomic {
public:
    static int inc_return(volatile T *p) {
        return __sync_fetch_and_add(p, 1) + 1;
    }
    static bool testandset(volatile T *p, T _old, T _new) {
        return __sync_bool_compare_and_swap(p, _old, _new);
    }
};


/*
 * Mutex
 */
namespace hqemu {
class Mutex {
    hqemu_lock_t M;
public:
    Mutex() { hqemu_lock_init(&M); }
    inline void acquire() { hqemu_lock(&M);   }
    inline void release() { hqemu_unlock(&M); }
};

class MutexGuard {
    Mutex &M;
public:
    MutexGuard(Mutex &M) : M(M) { M.acquire(); }
    ~MutexGuard() { M.release(); }
};
};


/*
 * GraphNode is used to describe the information of one node in a CFG.
 */
class GraphNode;
typedef std::vector<GraphNode *> NodeVec;
typedef std::set<GraphNode *> NodeSet;

class GraphNode {
    TranslationBlock *TB;
    NodeVec Children;

public:
    GraphNode(TranslationBlock *tb) : TB(tb) {}

    TranslationBlock *getTB()   { return TB;        }
    target_ulong getGuestPC()   { return TB->pc;    }
    NodeVec &getChildren()      { return Children;  }
    void insertChild(GraphNode *Node) {
        Children.push_back(Node);
    }

    static void DeleteCFG(GraphNode *Root);
};

/*
 * ControlFlowGraph is used to build the whole program control flow graph (CFG).
 * GlobalCFG uses this structure to maintain a whole program CFG connected by
 * direct branches.
 */
class ControlFlowGraph {
    hqemu::Mutex lock;

public:
    typedef std::vector<TranslationBlock *> TBVec;
    typedef std::map<TranslationBlock*, TBVec> SuccMap;
    SuccMap SuccCFG;

    ControlFlowGraph() {}

    hqemu::Mutex &getLock() { return lock; }
    TBVec &getSuccessor(TranslationBlock *tb) {
        return SuccCFG[tb];
    }

    void reset() {
        hqemu::MutexGuard locked(lock);
        SuccCFG.clear();
    }
    void insertLink(TranslationBlock *src, TranslationBlock *dst) {
        hqemu::MutexGuard locked(lock);
        SuccCFG[src].push_back(dst);
    }
};


/*
 * Queue
 */
#if defined(__x86_64__)
#define LOCK_FREE
#endif

#ifdef LOCK_FREE
struct pointer_t {
    struct node_t *ptr;
    unsigned long int count;
};

struct node_t {
    struct pointer_t next;
    void *value;
};

/* Lock-free MS-queue */
class Queue {
    struct queue_t {
        struct pointer_t head;
        struct pointer_t tail;
    };

    node_t *new_node(void *value) {
        node_t *node = new node_t;
        node->next.ptr = nullptr;
        node->value = value;
        return node;
    }
    void delete_node(node_t *node) {
        delete node;
    }

    queue_t Q;

public:
    Queue();
    void enqueue(void *data);
    void *dequeue();
};
#else
class Queue {
    struct node_t  {
        struct node_t *next;
        void *value;
        node_t(void *v) : next(nullptr), value(v) {}
    };
    struct queue_t  {
        struct node_t *head;
        struct node_t *tail;
    };

    pthread_mutex_t lock;
    queue_t Q;

public:
    Queue();
    void enqueue(void *data);
    void *dequeue();
};
#endif


class UUID {
    static uint64_t uuid;

public:
#if defined(__x86_64__)
    static uint64_t gen() {
        uint64_t i = 1;
        asm volatile("lock; xaddq %0, %1"
            : "+r" (i), "+m" (uuid) :: "memory");
        return i + 1;
    }
#else
    static uint64_t gen() {
        static pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;
        pthread_mutex_lock(&uuid_lock);
        uint64_t id = uuid++;
        pthread_mutex_unlock(&uuid_lock);
        return id;
    }
#endif
};


/* Misc utilities */
pid_t gettid();
void patch_jmp(volatile uintptr_t patch_addr, volatile uintptr_t addr);
void patch_jmp(volatile uintptr_t patch_addr, volatile void *addr);

#endif
/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */

