/*
 *  (C) 2010 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __LLVM_TARGET_H
#define __LLVM_TARGET_H

#include "llvm/ExecutionEngine/JITEventListener.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm-types.h"
#include "llvm-translator.h"

#ifndef __PRI64_PREFIX
#  if __WORDSIZE == 64
#    define __PRI64_PREFIX  "l"
#  else
#    define __PRI64_PREFIX  "ll"
#  endif
#endif

#if TARGET_LONG_BITS == 32
#  define PRId  "d"
#  define PRIx  "x"
#else
#  define PRId  __PRI64_PREFIX "d"
#  define PRIx  __PRI64_PREFIX "x"
#endif

#define PRId64  __PRI64_PREFIX "d"
#define PRIu64  __PRI64_PREFIX "u"

class code_ostream {
    char *OutBufStart;
    char *OutBufCur;
public:
    void Skip(unsigned Size) {
        OutBufCur += Size;
    }

    code_ostream(uintptr_t Ptr)
        : OutBufStart((char *)Ptr), OutBufCur((char *)Ptr) {}
    code_ostream &operator<<(char C) {
        *OutBufCur = C;
        OutBufCur++;
        return *this;
    }
    code_ostream &operator<<(unsigned char C) {
        *(unsigned char *)OutBufCur = C;
        OutBufCur++;
        return *this;
    }
    code_ostream &operator<<(unsigned int C) {
        *(unsigned int *)OutBufCur = C;
        OutBufCur += sizeof(unsigned int);
        return *this;
    }
    code_ostream &operator<<(unsigned long C) {
        *(unsigned long *)OutBufCur = C;
        OutBufCur += sizeof(unsigned long);
        return *this;
    }
};

static inline void EmitByte(code_ostream &OS, unsigned char C)
{
    OS << (char)C;
}
static inline void EmitConstant(code_ostream &OS, uint64_t Val, unsigned Size)
{
    for (unsigned i = 0; i != Size; ++i) {
        EmitByte(OS, Val & 255);
        Val >>= 8;
    }
}

/*
 * EventListener is used by the JIT to notify clients about significant events
 * during compilation.
 */
class EventListener : public JITEventListener {
    NotifyInfo &NI;

public:
    EventListener(NotifyInfo &NI) : NI(NI) {}
    ~EventListener() {}
    virtual void NotifyFunctionEmitted(const Function &F, void *Code, size_t Size,
                                       const EmittedFunctionDetails &Details);
#if defined(LLVM_V35)
    virtual void NotifyObjectEmitted(const ObjectImage &Obj);
#else
    virtual void NotifyObjectEmitted(const object::ObjectFile &Obj,
                                     const RuntimeDyld::LoadedObjectInfo &L);
#endif
};


const char *getMMUFName(const void *func);
bool isMMUFunction(std::string &Name);
bool isLMTFunction(std::string &Name);
bool isIllegalHelper(const void *func);
bool isLibcall(std::string &Name);
bool isSoftFPcall(std::string &Name);
void AddDependentSymbols(LLVMTranslator *Translator);
Value *StripPointer(Value *Ptr);
Value *StripPointer(const DataLayout *DL, Value *Ptr, APInt &Offset);
Value *getBaseWithConstantOffset(const DataLayout *DL, Value *Ptr, intptr_t &Offset);
void ProcessErase(IVec &toErase);

#endif

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */

