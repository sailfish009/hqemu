/*
 *  (C) 2015 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __LLVM_ANNOTATE_H
#define __LLVM_ANNOTATE_H

#include <map>
#include <cstdint>
#include "qemu-types.h"
#include "llvm-types.h"
#include "utils.h"

struct LoopMetadata {
    LoopMetadata()
        : Address(-1), Length(-1), VS(-1), VF(-1), Distance(INT_MIN), Start(-1),
          End(-1), Stride(-1) {}
    target_ulong Address;
    uint32_t Length;
    uint32_t VS, VF;
    int Distance;
    int Start, End;
    int Stride;
};

class AnnotationFactory {
    typedef std::map<uintptr_t, LoopMetadata*> LoopList;

    std::string MetaFile;

    int ParseXML(const char *name);

public:
    AnnotationFactory();
    ~AnnotationFactory();

    LoopList Loops;
    LoopMetadata *getLoopAnnotation(target_ulong addr);
    bool hasLoopAnnotation(target_ulong addr);
};

#endif

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
