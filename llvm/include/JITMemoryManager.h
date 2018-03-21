//===-- JITMemoryManager.cpp - Memory Allocator for JIT'd code ------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the DefaultJITMemoryManager class.
//
//===----------------------------------------------------------------------===//

#ifndef __JITMEMORYMANAGER_H
#define __JITMEMORYMANAGER_H

#include <sys/mman.h>
#include "llvm/Support/DynamicLibrary.h"
#include "llvm/ExecutionEngine/JITMemoryManager.h"
#include "llvm-debug.h"
#include "utils.h"

using namespace llvm;

#define MIN_CODE_CACHE_SIZE         (1 * 1024 * 1024)
#define DEFAULT_GLOBAL_SIZE         (64 * 1024)
#define DEFAULT_THRESHOLD           (32 * 1024)


// AtExitHandlers - List of functions to call when the program exits,
// registered with the atexit() library function.
static std::vector<void (*)()> AtExitHandlers;

/// runAtExitHandlers - Run any functions registered by the program's
/// calls to atexit(3), which we intercept and store in
/// AtExitHandlers.
///
static void runAtExitHandlers() {
  while (!AtExitHandlers.empty()) {
    void (*Fn)() = AtExitHandlers.back();
    AtExitHandlers.pop_back();
    Fn();
  }
}

//===----------------------------------------------------------------------===//
// Function stubs that are invoked instead of certain library calls
//
// Force the following functions to be linked in to anything that uses the
// JIT. This is a hack designed to work around the all-too-clever Glibc
// strategy of making these functions work differently when inlined vs. when
// not inlined, and hiding their real definitions in a separate archive file
// that the dynamic linker can't see. For more info, search for
// 'libc_nonshared.a' on Google, or read http://llvm.org/PR274.
#if defined(__linux__) && defined(__GLIBC__)
/* stat functions are redirecting to __xstat with a version number.  On x86-64
 * linking with libc_nonshared.a and -Wl,--export-dynamic doesn't make 'stat'
 * available as an exported symbol, so we have to add it explicitly.
 */
namespace {
class StatSymbols {
public:
  StatSymbols() {
    sys::DynamicLibrary::AddSymbol("stat", (void*)(intptr_t)stat);
    sys::DynamicLibrary::AddSymbol("fstat", (void*)(intptr_t)fstat);
    sys::DynamicLibrary::AddSymbol("lstat", (void*)(intptr_t)lstat);
    sys::DynamicLibrary::AddSymbol("stat64", (void*)(intptr_t)stat64);
    sys::DynamicLibrary::AddSymbol("\x1stat64", (void*)(intptr_t)stat64);
    sys::DynamicLibrary::AddSymbol("\x1open64", (void*)(intptr_t)open64);
    sys::DynamicLibrary::AddSymbol("\x1lseek64", (void*)(intptr_t)lseek64);
    sys::DynamicLibrary::AddSymbol("fstat64", (void*)(intptr_t)fstat64);
    sys::DynamicLibrary::AddSymbol("lstat64", (void*)(intptr_t)lstat64);
    sys::DynamicLibrary::AddSymbol("atexit", (void*)(intptr_t)atexit);
    sys::DynamicLibrary::AddSymbol("mknod", (void*)(intptr_t)mknod);
  }
};
}
static StatSymbols initStatSymbols;
#endif // __linux__

// jit_exit - Used to intercept the "exit" library call.
static void jit_exit(int Status) {
  runAtExitHandlers();   // Run atexit handlers...
  exit(Status);
}

// jit_atexit - Used to intercept the "atexit" library call.
static int jit_atexit(void (*Fn)()) {
  AtExitHandlers.push_back(Fn);    // Take note of atexit handler...
  return 0;  // Always successful
}

static int jit_noop() {
  return 0;
}


/// DefaultJITMemoryManager - Manage trace cache memory  for the JIT code generation.
class DefaultJITMemoryManager : public JITMemoryManager {
  uint8_t *TraceCache;
  size_t TraceCacheSize;

  uint8_t *GlobalBase;  /* section for global data used by QEMU helpers */
  uint8_t *CodeBase;    /* section for emitting trace code */
  uint8_t *CodeGenPtr;

  size_t GlobalRemain;
  size_t CodeRemain;
  size_t Threshold;

  hqemu::Mutex lock;

public:
  DefaultJITMemoryManager(uint8_t *Cache, size_t Size)
    : TraceCache(Cache), TraceCacheSize(Size), Threshold(DEFAULT_THRESHOLD)
  {
    GlobalBase = TraceCache;
    GlobalRemain = DEFAULT_GLOBAL_SIZE;
 
    CodeBase = GlobalBase + DEFAULT_GLOBAL_SIZE;
    CodeBase = (uint8_t *)(((uintptr_t)CodeBase + CODE_GEN_ALIGN - 1) & ~(CODE_GEN_ALIGN - 1));
    CodeRemain = (uintptr_t)TraceCache + TraceCacheSize - (uintptr_t)CodeBase;
    CodeGenPtr = CodeBase;
  }

  ~DefaultJITMemoryManager() {}

  //===----------------------------------------------------------------------===//
  //
  /// getPointerToNamedFunction - This method returns the address of the specified
  /// function by using the dynamic loader interface.  As such it is only useful
  /// for resolving library symbols, not code generated symbols.
  ///
  void *getPointerToNamedFunction(const std::string &Name,
                                  bool AbortOnFailure = true) override {
    // Check to see if this is one of the functions we want to intercept.  Note,
    // we cast to intptr_t here to silence a -pedantic warning that complains
    // about casting a function pointer to a normal pointer.
    if (Name == "exit") return (void*)(intptr_t)&jit_exit;
    if (Name == "atexit") return (void*)(intptr_t)&jit_atexit;
  
    // We should not invoke parent's ctors/dtors from generated main()!
    // On Mingw and Cygwin, the symbol __main is resolved to
    // callee's(eg. tools/lli) one, to invoke wrong duplicated ctors
    // (and register wrong callee's dtors with atexit(3)).
    // We expect ExecutionEngine::runStaticConstructorsDestructors()
    // is called before ExecutionEngine::runFunctionAsMain() is called.
    if (Name == "__main") return (void*)(intptr_t)&jit_noop;
  
    const char *NameStr = Name.c_str();
    // If this is an asm specifier, skip the sentinal.
    if (NameStr[0] == 1) ++NameStr;
  
    // If it's an external function, look it up in the process image...
    void *Ptr = sys::DynamicLibrary::SearchForAddressOfSymbol(NameStr);
    if (Ptr) return Ptr;
  
    // If it wasn't found and if it starts with an underscore ('_') character,
    // try again without the underscore.
    if (NameStr[0] == '_') {
      Ptr = sys::DynamicLibrary::SearchForAddressOfSymbol(NameStr+1);
      if (Ptr) return Ptr;
    }
  
    // Darwin/PPC adds $LDBLStub suffixes to various symbols like printf.  These
    // are references to hidden visibility symbols that dlsym cannot resolve.
    // If we have one of these, strip off $LDBLStub and try again.
#if defined(__APPLE__) && defined(__ppc__)
    if (Name.size() > 9 && Name[Name.size()-9] == '$' &&
        memcmp(&Name[Name.size()-8], "LDBLStub", 8) == 0) {
      // First try turning $LDBLStub into $LDBL128. If that fails, strip it off.
      // This mirrors logic in libSystemStubs.a.
      std::string Prefix = std::string(Name.begin(), Name.end()-9);
      if (void *Ptr = getPointerToNamedFunction(Prefix+"$LDBL128", false))
        return Ptr;
      if (void *Ptr = getPointerToNamedFunction(Prefix, false))
        return Ptr;
    }
#endif
  
    if (AbortOnFailure) {
      report_fatal_error("Program used external function '"+Name+
                        "' which could not be resolved!");
    }
    return nullptr;
  }

  void AllocateGOT() override { hqemu_error("fixme.\n"); }

  // Testing methods.
  bool CheckInvariants(std::string &ErrorStr) override { hqemu_error("fixme.\n"); return false; }
  size_t GetDefaultCodeSlabSize() override { hqemu_error("fixme.\n"); return 0; }
  size_t GetDefaultDataSlabSize() override { hqemu_error("fixme.\n"); return 0; }
  size_t GetDefaultStubSlabSize() override { hqemu_error("fixme.\n"); return 0; }
  unsigned GetNumCodeSlabs() override { hqemu_error("fixme.\n"); return 0; }
  unsigned GetNumDataSlabs() override { hqemu_error("fixme.\n"); return 0; }
  unsigned GetNumStubSlabs() override { hqemu_error("fixme.\n"); return 0; }

  /// startFunctionBody - When a function starts, allocate a block of free
  /// executable memory, returning a pointer to it and its actual size.
  uint8_t *startFunctionBody(const Function *F,
                             uintptr_t &ActualSize) override {
    lock.acquire();
    if (unlikely(CodeRemain < Threshold))
      hqemu_error("internal error (fixme).\n");

    ActualSize = CodeRemain;
    return CodeGenPtr;
  }

  /// endFunctionBody - The function F is now allocated, and takes the memory
  /// in the range [FunctionStart,FunctionEnd).
  void endFunctionBody(const Function *F, uint8_t *FunctionStart,
                       uint8_t *FunctionEnd) override {
    assert(FunctionEnd > FunctionStart);

    size_t GenSize = FunctionEnd - FunctionStart;
    if (unlikely(GenSize > CodeRemain))
        hqemu_error("exceeds available cache size.\n");

    CodeGenPtr = (uint8_t *)(((uintptr_t)CodeGenPtr + GenSize + CODE_GEN_ALIGN - 1)
                             & ~(CODE_GEN_ALIGN - 1));
    CodeRemain = (uintptr_t)TraceCache + TraceCacheSize - (uintptr_t)CodeGenPtr;
    lock.release();
  }

  /// allocateSpace - Allocate a memory block of the given size.  This method
  /// cannot be called between calls to startFunctionBody and endFunctionBody.
  uint8_t *allocateSpace(intptr_t Size, unsigned Alignment) override {
    hqemu_error("fixme.\n");
    return nullptr;
  }

  /// allocateStub - Allocate memory for a function stub.
  uint8_t *allocateStub(const GlobalValue* F, unsigned StubSize,
                        unsigned Alignment) override {
    return allocateGlobal(StubSize, Alignment);
  }

  /// allocateGlobal - Allocate memory for a global.
  uint8_t *allocateGlobal(uintptr_t Size, unsigned Alignment) override {
    hqemu::MutexGuard locked(lock);

    if (!Alignment)
      Alignment = 16;
    if (Alignment & (Alignment - 1))
      hqemu_error("alignment must be a power of two.\n");

    unsigned MisAligned = ((intptr_t)GlobalBase & (Alignment - 1));
    if (MisAligned)
      MisAligned = Alignment - MisAligned;

    if (GlobalRemain < Size + MisAligned)
      hqemu_error("exceeds available global size.\n");

    uint8_t *GlobalPtr = GlobalBase + MisAligned;
    GlobalBase = GlobalPtr + Size;
    GlobalRemain -= (Size + MisAligned);
    return GlobalPtr;
  }

  /// allocateCodeSection - Allocate memory for a code section.
  uint8_t *allocateCodeSection(uintptr_t Size, unsigned Alignment,
                               unsigned SectionID,
                               StringRef SectionName) override {
    hqemu_error("fixme.\n"); return nullptr;
  }

  /// allocateDataSection - Allocate memory for a data section.
  uint8_t *allocateDataSection(uintptr_t Size, unsigned Alignment,
                               unsigned SectionID, StringRef SectionName,
                               bool IsReadOnly) override {
    hqemu_error("fixme.\n"); return nullptr;
  }

  bool finalizeMemory(std::string *ErrMsg) override { return false; }

  uint8_t *getGOTBase() const override { return nullptr; }

  void deallocateBlock(void *Block) {}

  /// deallocateFunctionBody - Deallocate all memory for the specified
  /// function body.
  void deallocateFunctionBody(void *Body) override {}

  /// setMemoryWritable - When code generation is in progress,
  /// the code pages may need permissions changed.
  void setMemoryWritable() override {}
  /// setMemoryExecutable - When code generation is done and we're ready to
  /// start execution, the code pages may need permissions changed.
  void setMemoryExecutable() override {}

  /// setPoisonMemory - Controls whether we write garbage over freed memory.
  ///
  void setPoisonMemory(bool poison) override {}

  size_t getCodeSize()      { return CodeGenPtr - CodeBase; }
  bool isSizeAvailable()    {
    hqemu::MutexGuard locked(lock);
    return CodeRemain >= Threshold ? 1 : 0;
  }
  void Flush() {
    CodeGenPtr = CodeBase;
    CodeRemain = (uintptr_t)TraceCache + TraceCacheSize - (uintptr_t)CodeBase;
  }

  static DefaultJITMemoryManager *Create(uint8_t *Cache, size_t Size) {
    if (Size < MIN_CODE_CACHE_SIZE)
      hqemu_error("Trace cache size is too small.\n");
    return new DefaultJITMemoryManager(Cache, Size);
  }
};

#endif

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
