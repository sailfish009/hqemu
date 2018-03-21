/*
 *  (C) 2010 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#include <iostream>
#include <sstream>
#include "tracer.h"
#include "utils.h"
#include "llvm.h"
#include "llvm-target.h"
#include "llvm-profile.h"


extern LLVMEnv *LLEnv;

/*
 * ProfileFactory
 */
void ProfileFactory::ParseProfileMode(std::string &ProfileLevel)
{
    static std::string profile_str[PROFILE_NUM] = {
        "none", "basic", "trace", "cache", "pass", "hotspot", "all"
    };
    static uint64_t profile_enum[PROFILE_NUM] = {
        PROFILE_NONE, PROFILE_BASIC, PROFILE_TRACE, PROFILE_CACHE,
        PROFILE_PASS, PROFILE_HOTSPOT, PROFILE_ALL,
    };

    if (ProfileLevel.empty())
        return;

    std::istringstream ss(ProfileLevel);
    std::string token;
    while(getline(ss, token, ',')) {
        for (int i = 0; i != PROFILE_NUM; ++i) {
            if (token == profile_str[i]) {
                Mode |= profile_enum[i];
                break;
            }
        }
    }
}

void ProfileFactory::printProfile()
{
    if (!isEnabled())
        return;

    if (LLVMEnv::TransMode == TRANS_MODE_NONE ||
        LLVMEnv::TransMode == TRANS_MODE_INVALID)
        return;

    if (LLVMEnv::TransMode == TRANS_MODE_BLOCK)
        printBlockProfile();
    else
        printTraceProfile();
}

void ProfileFactory::printBlockProfile()
{
    LLVMEnv::TransCodeList &TransCode = LLEnv->getTransCode();
    uint32_t GuestSize = 0, GuestICount = 0, HostSize = 0;
    uint64_t TransTime = 0, MaxTime = 0;

    for (auto TC : TransCode) {
        TraceInfo *Trace = TC->Trace;
        TranslationBlock *TB = TC->EntryTB;
        GuestSize += TB->size;
        GuestICount += TB->icount;
        HostSize += TC->Size;
        TransTime += Trace->TransTime;
        if (Trace->TransTime > MaxTime)
            MaxTime = Trace->TransTime;
    }

    auto &OS = DM.debug();
    OS << "\nBlock statistic:\n"
       << "Num of Blocks    : " << TransCode.size() << "\n"
       << "G/H Code Size    : " << GuestSize << "/" << HostSize << "bytes\n"
       << "Guest ICount     : " << GuestICount << "\n"
       << "Translation Time : " << format("%.6f", (double)TransTime * 1e-6)
                                << " seconds (max=" << MaxTime /1000 << " ms)\n";
}

static void printBasic(LLVMEnv::TransCodeList &TransCode)
{
    uint32_t GuestSize = 0, GuestICount = 0, HostSize = 0;
    uint32_t NumBlock = 0, NumLoop = 0, NumExit = 0, NumIndirectBr = 0;
    uint32_t MaxBlock = 0, MaxLoop = 0, MaxExit = 0, MaxIndirectBr = 0;
    uint64_t TransTime = 0, MaxTime = 0;
    unsigned NumTraces = TransCode.size();
    std::map<unsigned, unsigned> LenDist;

    for (auto TC : TransCode) {
        TraceInfo *Trace = TC->Trace;
        TBVec &TBs = Trace->TBs;
        for (unsigned i = 0, e = TBs.size(); i != e; ++i) {
            GuestSize += TBs[i]->size;
            GuestICount += TBs[i]->icount;
        }
        HostSize += TC->Size;

        NumBlock += TBs.size();
        NumLoop += Trace->NumLoop;
        NumExit += Trace->NumExit;
        NumIndirectBr += Trace->NumIndirectBr;
        TransTime += Trace->TransTime;

        if (TBs.size() > MaxBlock)
            MaxBlock = TBs.size();
        if (Trace->NumLoop > MaxLoop)
            MaxLoop = Trace->NumLoop;
        if (Trace->NumExit > MaxExit)
            MaxExit = Trace->NumExit;
        if (Trace->NumIndirectBr > MaxIndirectBr)
            MaxIndirectBr = Trace->NumIndirectBr;
        if (Trace->TransTime > MaxTime)
            MaxTime = Trace->TransTime;
        LenDist[TBs.size()]++;
    }

    auto &OS = DM.debug();
    OS << "\nTrace statistic:\n"
       << "Num of Traces    : " << NumTraces << "\n"
       << "Profile Thres.   : " << PROFILE_THRESHOLD << "\n"
       << "Predict Thres.   : " << PREDICT_THRESHOLD << "\n"
       << "G/H Code Size    : " << GuestSize << "/" << HostSize << " bytes\n"
       << "Translation Time : " << format("%.6f", (double)TransTime * 1e-6)
                                << " seconds (max=" << MaxTime /1000 << " ms)\n"
       << "Average # Blocks : " << format("%.1f", (double)NumBlock / NumTraces)
                                << " (max=" << MaxBlock << ")\n"
       << "Average # Loops  : " << format("%.1f", (double)NumLoop / NumTraces)
                                << " (max=" << MaxLoop << ")\n"
       << "Average # Exits  : " << format("%.1f", (double)NumExit / NumTraces)
                                << " (max=" << MaxExit << ")\n"
       << "Average # IBs    : " << format("%.1f", (double)NumIndirectBr / NumTraces)
                                << " (max=" << MaxIndirectBr << ")\n"
       << "Flush Count      : " << LLEnv->getNumFlush() << "\n"
       << "\n";

    OS << "Length distribution: (1-" << MaxBlock << ")\n    ";
    for (unsigned i = 1; i <= MaxBlock; i++)
        OS << LenDist[i] << " ";
    OS << "\n";
}

static void printTraceExec(LLVMEnv::TransCodeList &TransCode)
{
    unsigned NumThread = 0;
    for (auto next_cpu = first_cpu; next_cpu != nullptr;
         next_cpu = CPU_NEXT(next_cpu))
        NumThread++;

    /* Detailed trace information and runtime counters. */
    auto &OS = DM.debug();
    OS << "----------------------------\n"
       << "Trace execution information:\n";

    unsigned NumTraces = TransCode.size();
    for (unsigned i = 0; i != NumThread; ++i) {
        unsigned TraceUsed = 0;

        OS << ">\n"
           << "Thread " << i << ":\n"
           << "                                   dynamic exec count\n"
           << "  id      pc      #loop:#exit      loop      ibtc      exit\n";
        for (unsigned j = 0; j != NumTraces; ++j) {
            TraceInfo *Trace = TransCode[j]->Trace;
            uint64_t *Counter = Trace->ExecCount[i];
            if (Counter[0] + Counter[1] + Counter[2] == 0)
                continue;
            TraceUsed++;
            OS << format("%4d", j) << ") "
               << format("0x%08" PRIx, Trace->getEntryPC()) << "    "
               << format("%2d", Trace->NumLoop)   << "    "
               << format("%2d", Trace->NumExit)   << "   "
               << format("%8" PRId64, Counter[0]) << "  "
               << format("%8" PRId64, Counter[1]) << "  "
               << format("%8" PRId64, Counter[2]) << "\n";
        }
        OS << "Trace used: " << TraceUsed << "/" << NumTraces <<"\n";
    }
}

void ProfileFactory::printTraceProfile()
{
    auto &OS = DM.debug();
    unsigned NumTraces = LLEnv->getTransCode().size();
    if (NumTraces == 0) {
        OS << "\nTrace statistic:\n"
           << "Num of Traces  : " << NumTraces << "\n\n";
        return;
    }

    /* Static information */
    if (Mode & PROFILE_BASIC)
        printBasic(LLEnv->getTransCode());

    /* Code cache infomation - start address and size */
    if (Mode & PROFILE_CACHE) {
        size_t BlockSize = (uintptr_t)tcg_ctx_global.code_gen_ptr -
                           (uintptr_t)tcg_ctx_global.code_gen_buffer;
        size_t TraceSize = LLEnv->getMemoryManager()->getCodeSize();

        OS << "-------------------------\n"
           << "Block/Trace Cache information:\n";
        OS << "Block: start=" << tcg_ctx_global.code_gen_buffer
           << " size=" << tcg_ctx_global.code_gen_buffer_size
           << " code=" << format("%8d", BlockSize) << " (ratio="
           << format("%.2f", (double)BlockSize * 100 / tcg_ctx_global.code_gen_buffer_size)
           << "%)\n";
        OS << "Trace: start=" << LLVMEnv::TraceCache
           << " size=" << LLVMEnv::TraceCacheSize
           << " code=" << format("%8d", TraceSize) << " (ratio="
           << format("%.2f", (double)TraceSize * 100 / LLVMEnv::TraceCacheSize)
           << "%)\n\n";
    }

    if (Mode & PROFILE_TRACE)
        printTraceExec(LLEnv->getTransCode());

    if (Mode & PROFILE_PASS) {
        OS << "\n-------------------------\n"
           << "Passes information:\n";
        for (unsigned i = 0, e = ExitFunc.size(); i != e; ++i)
            (*ExitFunc[i])();
    }
}

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */

