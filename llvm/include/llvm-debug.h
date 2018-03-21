/*
 *  (C) 2010 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __LLVM_DEBUG_H
#define __LLVM_DEBUG_H

#include <cstdint>
#include <cstring>
#include <iostream>
#include <sstream>
#include <cstdarg>
#include <unistd.h>
#include <sys/time.h>
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/MCInstrAnalysis.h"
#include "utils.h"


struct DebugMode {
    uint64_t Mode;

    DebugMode(uint64_t M) : Mode(M) {}

    bool operator==(const DebugMode &RHS) const {
        return Mode == RHS.Mode;
    }
    bool operator&(const DebugMode &RHS) const {
        return Mode & RHS.Mode;
    }
    DebugMode operator|(const DebugMode &RHS) {
        return DebugMode(Mode | RHS.Mode);
    }
    DebugMode &operator|=(const DebugMode &RHS) {
        Mode |= RHS.Mode;
        return *this;
    }
};

/*
 * LLVMDebug provides facilities to debug the LLVM translator, based on the
 * debug levels.
 */
class LLVMDebug {
public:
    enum LLVMDebugMode {
        D_NONE     = ((uint64_t)0),
        D_LLVM     = ((uint64_t)1 << 0),
        D_INASM    = ((uint64_t)1 << 1),
        D_OP       = ((uint64_t)1 << 2),
        D_OUTASM   = ((uint64_t)1 << 3),
        D_IR       = ((uint64_t)1 << 4),
        D_IR_OPT   = ((uint64_t)1 << 5),
        D_ENTRY    = ((uint64_t)1 << 6),
        D_VERIFY   = ((uint64_t)1 << 7),
        D_PASS     = ((uint64_t)1 << 8),
        D_ANNOTATE = ((uint64_t)1 << 9),
        D_ASM      = (D_INASM | D_OP | D_OUTASM),
        D_DEBUG    = (D_LLVM | D_IR_OPT | D_OUTASM | D_PASS),
        D_ALL      = (D_LLVM | D_INASM | D_OP | D_OUTASM | D_IR | D_IR_OPT |
                      D_ENTRY | D_VERIFY | D_PASS | D_ANNOTATE),
    };

    LLVMDebug()
        : hqemu_stdout(STDOUT_FILENO, false, true),
          hqemu_stderr(STDERR_FILENO, false, true), Mode(D_NONE)
    {
        std::string Str("");
        gettimeofday(&uptime, nullptr); 
        ParseDebugMode(Str, false);
        hqemu_null.SetUnbuffered();
    }

    DebugMode &getDebugMode() {
        return Mode;
    }

    DebugMode &getDebugMode(LLVMDebugMode M) {
        if (Modes.find(M) == Modes.end())
            M = D_NONE;
        return *Modes[M];
    }

    void setDebugMode(std::string &DebugLevel) {
        ParseDebugMode(DebugLevel);
    }

    void error(const char *fname, const char *fmt, ...) {
        static char str[256] = {'\0'};
        va_list ap;
        va_start(ap, fmt);
        vsprintf(str, fmt, ap);
        va_end(ap);
        hqemu_stderr << timestamp() << " Error: " << fname << " - " << str;
        exit(0);
    }

    llvm::raw_ostream &output() {
        return hqemu_stdout;
    }

    llvm::raw_ostream &debug() {
        return hqemu_stderr;
    }

    llvm::raw_ostream &operator<<(DebugMode &M) {
        if (M & Mode) {
            hqemu_stderr << timestamp() << " ";
            return hqemu_stderr;
        }
        return hqemu_null;
    };

private:
    llvm::raw_null_ostream hqemu_null;
    llvm::raw_fd_ostream hqemu_stdout;
    llvm::raw_fd_ostream hqemu_stderr;
    struct timeval uptime; /* The startup time of the DBT */
    DebugMode Mode;       /* The debug level */
    std::map<LLVMDebugMode, DebugMode*> Modes;

    std::string timestamp() {
        struct timeval tv;
        char timestamp[32];
        gettimeofday(&tv, 0);
        timersub(&tv, &uptime, &tv);
        strftime(timestamp, 32, "[%H:%M:%S", gmtime(&tv.tv_sec));
        sprintf(timestamp + 9, ".%06ld]", tv.tv_usec);
        return timestamp;
    }

    void ParseDebugMode(std::string &DebugLevel, bool Update=true) {
        static std::string debug_str[] = {
            "none", "llvm", "in_asm", "op", "out_asm", "ir", "ir_opt", 
            "entry", "verify", "pass", "annotate", "asm", "debug",
            "all"
        };
        static LLVMDebugMode debug_enum[] = {
            D_NONE, D_LLVM, D_INASM, D_OP, D_OUTASM, D_IR, D_IR_OPT,
            D_ENTRY, D_VERIFY, D_PASS, D_ANNOTATE, D_ASM, D_DEBUG,
            D_ALL
        };

        if (!Update) {
            for (auto M : debug_enum)
                Modes[M] = new DebugMode(M);
            return;
        }

        if (DebugLevel.empty())
            return;

	std::istringstream ss(DebugLevel);
        std::string token;
        while(std::getline(ss, token, ',')) {
            for (unsigned i = 0, e = sizeof(debug_enum); i != e; ++i) {
                if (token == debug_str[i]) {
                    Mode |= getDebugMode(debug_enum[i]);
                    break;
                }
            }
        }
    }
};

extern LLVMDebug DM;

/* Print messages to stdout. Should not use this function in release mode. */
static inline llvm::raw_ostream &out() {
    return DM.output();
}
/* Print messages to stderr, controlled by DebugMode. */
static inline LLVMDebug &dbg() {
    return DM;
}
/* Print error messages to stderr and terminate the process. */
#define hqemu_error(msg,args...) do { DM.error(__func__,msg,##args); } while(0)

/* Macros to get defined DebugMode. */
#define DEBUG_NONE      DM.getDebugMode(LLVMDebug::LLVMDebugMode::D_NONE)
#define DEBUG_LLVM      DM.getDebugMode(LLVMDebug::LLVMDebugMode::D_LLVM)
#define DEBUG_INASM     DM.getDebugMode(LLVMDebug::LLVMDebugMode::D_INASM)
#define DEBUG_OP        DM.getDebugMode(LLVMDebug::LLVMDebugMode::D_OP)
#define DEBUG_OUTASM    DM.getDebugMode(LLVMDebug::LLVMDebugMode::D_OUTASM)
#define DEBUG_IR        DM.getDebugMode(LLVMDebug::LLVMDebugMode::D_IR)
#define DEBUG_IR_OPT    DM.getDebugMode(LLVMDebug::LLVMDebugMode::D_IR_OPT)
#define DEBUG_ENTRY     DM.getDebugMode(LLVMDebug::LLVMDebugMode::D_ENTRY)
#define DEBUG_VERIFY    DM.getDebugMode(LLVMDebug::LLVMDebugMode::D_VERIFY)
#define DEBUG_PASS      DM.getDebugMode(LLVMDebug::LLVMDebugMode::D_PASS)
#define DEBUG_ANNOTATE  DM.getDebugMode(LLVMDebug::LLVMDebugMode::D_ANNOTATE)
#define DEBUG_ASM       DM.getDebugMode(LLVMDebug::LLVMDebugMode::D_ASM)
#define DEBUG_DEBUG     DM.getDebugMode(LLVMDebug::LLVMDebugMode::D_DEBUG)
#define DEBUG_ALL       DM.getDebugMode(LLVMDebug::LLVMDebugMode::D_ALL)



/*
 * Binary disassembler using MCDisassembler.
 */
class MCDisasm {
    const llvm::MCDisassembler *DisAsm;
    const llvm::MCSubtargetInfo *STI;
    llvm::MCInstPrinter *IP;
    const llvm::MCInstrAnalysis *MIA;
    bool HostDisAsm;
    bool NoShowRawInsn;

    MCDisasm(const llvm::Target *TheTarget, std::string TripleName,
             bool isHost);

    void DumpBytes(llvm::ArrayRef<uint8_t> bytes, llvm::raw_ostream &OS);

public:
    ~MCDisasm();
    void PrintInAsm(uint64_t Addr, uint64_t Size, uint64_t GuestAddr);
    void PrintOutAsm(uint64_t Addr, uint64_t Size);

    static MCDisasm *CreateMCDisasm(std::string TripleName, bool isHost);
};

#endif

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
