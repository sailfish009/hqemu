//===-- MCJITMemoryManager.cpp - Memory manager for MC-JIT -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Interface of the MCJIT memory manager base class.
//
//===----------------------------------------------------------------------===//

#ifndef __MCJITMEMORYMANAGER_H
#define __MCJITMEMORYMANAGER_H

#include "llvm/ExecutionEngine/RTDyldMemoryManager.h"
#include "llvm-debug.h"
#include "utils.h"

using namespace llvm;

#define MIN_CODE_CACHE_SIZE         (1 * 1024 * 1024)
#define DEFAULT_GLOBAL_SIZE         (64 * 1024)
#define DEFAULT_THRESHOLD           (32 * 1024)

// RuntimeDyld clients often want to handle the memory management of
// what gets placed where. For JIT clients, this is the subset of
// JITMemoryManager required for dynamic loading of binaries.
//
// FIXME: As the RuntimeDyld fills out, additional routines will be needed
//        for the varying types of objects to be allocated.
class DefaultMCJITMemoryManager : public RTDyldMemoryManager {
  uint8_t *TraceCache;
  size_t TraceCacheSize;

  uint8_t *GlobalBase;  /* section for global data used by QEMU helpers */
  uint8_t *CodeBase;    /* section for emitting trace code */
  uint8_t *CodeGenPtr;

  size_t GlobalRemain;
  size_t CodeRemain;
  size_t Threshold;

  hqemu::Mutex lock;

  SymbolMap Symbols;

public:
  DefaultMCJITMemoryManager(uint8_t *Cache, size_t Size)
    : TraceCache(Cache), TraceCacheSize(Size), Threshold(DEFAULT_THRESHOLD)
  {
    GlobalBase = TraceCache;
    GlobalRemain = DEFAULT_GLOBAL_SIZE;

    CodeBase = GlobalBase + DEFAULT_GLOBAL_SIZE;
    CodeBase = (uint8_t *)(((uintptr_t)CodeBase + CODE_GEN_ALIGN - 1) & ~(CODE_GEN_ALIGN - 1));
    CodeRemain = (uintptr_t)TraceCache + TraceCacheSize - (uintptr_t)CodeBase;
    CodeGenPtr = CodeBase;
  }
  ~DefaultMCJITMemoryManager() {}

  /// Allocate a memory block of (at least) the given size suitable for
  /// executable code. The SectionID is a unique identifier assigned by the JIT
  /// engine, and optionally recorded by the memory manager to access a loaded
  /// section.
  uint8_t *allocateCodeSection(
    uintptr_t Size, unsigned Alignment, unsigned SectionID,
    StringRef SectionName) override {
    hqemu::MutexGuard locked(lock);

    if (!Alignment)
      Alignment = 16;

    if (Alignment & (Alignment - 1))
      hqemu_error("Alignment must be a power of two.\n");

    uintptr_t CurGenPtr = (uintptr_t)CodeGenPtr;
    CurGenPtr = (CurGenPtr + Alignment - 1) & ~(uintptr_t)(Alignment - 1);
    CodeGenPtr = (uint8_t *)((CurGenPtr + Size + CODE_GEN_ALIGN - 1) &
                             ~(uintptr_t)(CODE_GEN_ALIGN - 1));
    CodeRemain = (uintptr_t)TraceCache + TraceCacheSize - (uintptr_t)CodeGenPtr;
    return (uint8_t *)CurGenPtr;
  }

  /// Allocate a memory block of (at least) the given size suitable for data.
  /// The SectionID is a unique identifier assigned by the JIT engine, and
  /// optionally recorded by the memory manager to access a loaded section.
  uint8_t *allocateDataSection(
    uintptr_t Size, unsigned Alignment, unsigned SectionID,
    StringRef SectionName, bool IsReadOnly) override {
    return allocateCodeSection(Size, Alignment, SectionID, SectionName);
  }

  /// Inform the memory manager about the total amount of memory required to
  /// allocate all sections to be loaded:
  /// \p CodeSize - the total size of all code sections
  /// \p DataSizeRO - the total size of all read-only data sections
  /// \p DataSizeRW - the total size of all read-write data sections
  /// 
  /// Note that by default the callback is disabled. To enable it
  /// redefine the method needsToReserveAllocationSpace to return true.
  void reserveAllocationSpace(
    uintptr_t CodeSize, uintptr_t DataSizeRO, uintptr_t DataSizeRW) {
    hqemu_error("fixme.\n");
  }
  
  /// Override to return true to enable the reserveAllocationSpace callback.
  bool needsToReserveAllocationSpace() { return false; }

  /// Register the EH frames with the runtime so that c++ exceptions work.
  ///
  /// \p Addr parameter provides the local address of the EH frame section
  /// data, while \p LoadAddr provides the address of the data in the target
  /// address space.  If the section has not been remapped (which will usually
  /// be the case for local execution) these two values will be the same.
  void registerEHFrames(uint8_t *Addr, uint64_t LoadAddr, size_t Size) {
    hqemu_error("fixme.\n");
  }

  void deregisterEHFrames(uint8_t *Addr, uint64_t LoadAddr, size_t Size) {
    hqemu_error("fixme.\n");
  }

  /// This method returns the address of the specified function or variable.
  /// It is used to resolve symbols during module linking.
  uint64_t getSymbolAddress(const std::string &Name) {
    hqemu::MutexGuard locked(lock);
    if (Symbols.find(Name) == Symbols.end()) {
      std::string ErrMsg = "Program used external symbol '" + Name +
                           "'which could not be resolved!\n";
      hqemu_error(ErrMsg.c_str());
    }
    return Symbols[Name];
  }

  /// This method returns the address of the specified function. As such it is
  /// only useful for resolving library symbols, not code generated symbols.
  ///
  /// If \p AbortOnFailure is false and no function with the given name is
  /// found, this function returns a null pointer. Otherwise, it prints a
  /// message to stderr and aborts.
  ///
  /// This function is deprecated for memory managers to be used with
  /// MCJIT or RuntimeDyld.  Use getSymbolAddress instead.
  void *getPointerToNamedFunction(const std::string &Name,
                                  bool AbortOnFailure = true) {
    if (AbortOnFailure) {
      std::string ErrMsg = "Program used external symbol '" + Name +
                           "'which could not be resolved!\n";
      hqemu_error(ErrMsg.c_str());
    }
    return nullptr;
  }

  /// This method is called after an object has been loaded into memory but
  /// before relocations are applied to the loaded sections.  The object load
  /// may have been initiated by MCJIT to resolve an external symbol for another
  /// object that is being finalized.  In that case, the object about which
  /// the memory manager is being notified will be finalized immediately after
  /// the memory manager returns from this call.
  ///
  /// Memory managers which are preparing code for execution in an external
  /// address space can use this call to remap the section addresses for the
  /// newly loaded object.
#if defined(LLVM_V35)
  void notifyObjectLoaded(ExecutionEngine *EE,
                          const ObjectImage *Obj) {
  }
#else
  void notifyObjectLoaded(RuntimeDyld &RTDyld,
		          const object::ObjectFile &Obj) {
  }
#endif

  /// This method is called when object loading is complete and section page
  /// permissions can be applied.  It is up to the memory manager implementation
  /// to decide whether or not to act on this method.  The memory manager will
  /// typically allocate all sections as read-write and then apply specific
  /// permissions when this method is called.  Code sections cannot be executed
  /// until this function has been called.  In addition, any cache coherency
  /// operations needed to reliably use the memory are also performed.
  ///
  /// Returns true if an error occurred, false otherwise.
  bool finalizeMemory(std::string *ErrMsg = nullptr) override {
    return false;
  }

  void AddSymbols(SymbolMap &symbols) {
    Symbols = symbols;
  }

  size_t getCodeSize()      { return CodeGenPtr - CodeBase; }
  bool isSizeAvailable()    {
    hqemu::MutexGuard locked(lock);
    return CodeRemain >= Threshold ? 1 : 0;
  }
  void Flush() {
    CodeGenPtr = CodeBase;
    CodeRemain = (uintptr_t)TraceCache + TraceCacheSize - (uintptr_t)CodeBase;
  }

  static DefaultMCJITMemoryManager *Create(uint8_t *Cache, size_t Size) {
    if (Size < MIN_CODE_CACHE_SIZE) {
      std::string ErrMsg = "Trace cache size is too small (" +
                           std::to_string(Size) + ")\n.";
      hqemu_error(ErrMsg.c_str());
    }
    return new DefaultMCJITMemoryManager(Cache, Size);
  }
};

#endif
