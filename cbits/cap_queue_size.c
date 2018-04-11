#include <Rts.h>

typedef struct CapabilityPrivate {
    // State required by the STG virtual machine when running Haskell
    // code.  During STG execution, the BaseReg register always points
    // to the StgRegTable of the current Capability (&cap->r).
    StgFunTable f;
    StgRegTable r;

    uint32_t no;  // capability number.

    // The NUMA node on which this capability resides.  This is used to allocate
    // node-local memory in allocate().
    //
    // Note: this is always equal to cap->no % n_numa_nodes.
    // The reason we slice it this way is that if we add or remove capabilities
    // via setNumCapabilities(), then we keep the number of capabilities on each
    // NUMA node balanced.
    uint32_t node;

    // The Task currently holding this Capability.  This task has
    // exclusive access to the contents of this Capability (apart from
    // returning_tasks_hd/returning_tasks_tl).
    // Locks required: cap->lock.
    void *running_task;

    // true if this Capability is running Haskell code, used for
    // catching unsafe call-ins.
    bool in_haskell;

    // Has there been any activity on this Capability since the last GC?
    uint32_t idle;

    bool disabled;

    // The run queue.  The Task owning this Capability has exclusive
    // access to its run queue, so can wake up threads without
    // taking a lock, and the common path through the scheduler is
    // also lock-free.
    StgTSO *run_queue_hd;
    StgTSO *run_queue_tl;
    uint32_t n_run_queue;
} CapabilityPrivate;

uint32_t get_mycap_queue_size(){
    return ((CapabilityPrivate*)rts_unsafeGetMyCapability())->n_run_queue;
}
