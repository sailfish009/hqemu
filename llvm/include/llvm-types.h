/*
 *  (C) 2010 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __LLVM_TYPES_H
#define __LLVM_TYPES_H

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/ValueHandle.h"
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Analysis/CodeMetrics.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/DynamicLibrary.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/CommandLine.h"

#if defined(LLVM_V35)
#include "llvm/MC/MCDisassembler.h"
#include "llvm/ExecutionEngine/ObjectImage.h"
#elif defined(LLVM_V38)
#include "llvm/MC/MCDisassembler.h"
#include "llvm/Object/SymbolSize.h"
#include "llvm/DebugInfo/DWARF/DWARFContext.h"
#else
#include "llvm/MC/MCDisassembler/MCDisassembler.h"
#include "llvm/Object/SymbolSize.h"
#include "llvm/DebugInfo/DWARF/DWARFContext.h"
#endif

#include <vector>
#include <set>
#include <map>
#include "llvm-macro.h"
#include "qemu-types.h"

using namespace llvm;

class HelperInfo;

typedef std::vector<TranslationBlock *> TBVec;
typedef std::vector<std::pair<BlockID, uint16_t> > RestoreVec;
typedef std::map<uintptr_t, std::string> TCGHelperMap;   /* <func_ptr, func_name> */
typedef std::map<std::string, HelperInfo*> HelperMap;
typedef std::map<std::string, uintptr_t> SymbolMap;
typedef std::map<intptr_t, Type *> FlatType;       /* <state_off, state_ty> */
typedef std::vector<Instruction *> IVec;
typedef std::vector<BasicBlock *> BBVec;


static inline const DataLayout *getDataLayout(Module *Mod) {
#if defined(LLVM_V35)
    return Mod->getDataLayout();
#else
    return &Mod->getDataLayout();
#endif
}

#endif

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
