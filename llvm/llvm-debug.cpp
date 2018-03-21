/*
 *  (C) 2016 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#include "llvm/ADT/Triple.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/MCInstrAnalysis.h"
#include "llvm/Support/MemoryObject.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm-debug.h"
#include "llvm.h"


static const Target *getTarget(std::string TripleName)
{
    /* Get the target specific parser. */
    std::string Error;
    const Target *TheTarget = TargetRegistry::lookupTarget(
            TripleName.c_str(), Error);
    if (!TheTarget)
        return nullptr;

    return TheTarget;
}

MCDisasm *MCDisasm::CreateMCDisasm(std::string TripleName, bool isHost)
{
    if (TripleName.empty() || TripleName == "UnknownArch")
        return nullptr;

    const Target *TheTarget = getTarget(TripleName);
    if (!TheTarget)
        return nullptr;

    return new MCDisasm(TheTarget, TripleName, isHost);
}

MCDisasm::MCDisasm(const llvm::Target *TheTarget, std::string TripleName,
                   bool isHost)
    : HostDisAsm(isHost), NoShowRawInsn(false)
{
    const char *triple = TripleName.c_str();
    Triple TheTriple(Triple::normalize(TripleName));

    switch (TheTriple.getArch()) {
    case Triple::x86:
    case Triple::x86_64:
        NoShowRawInsn = true;
        break;
    default:
        NoShowRawInsn = false;
        break;
    }

    const MCRegisterInfo *MRI = TheTarget->createMCRegInfo(TripleName);
    if (!MRI)
        hqemu_error("no register info for target %s.\n", triple);
    const MCAsmInfo *MAI = TheTarget->createMCAsmInfo(*MRI, TripleName);
    if (!MAI)
        hqemu_error("no assembly info for target %s\n", triple);
    const MCSubtargetInfo *STI = TheTarget->createMCSubtargetInfo(TripleName, "", "");
    if (!STI)
        hqemu_error("no subtarget info for target %s\n", triple);
    const MCInstrInfo *MII = TheTarget->createMCInstrInfo();
    if (!MII)
        hqemu_error("no instruction info for target %s\n", triple);

    MCContext Ctx(MAI, MRI, nullptr);
    const MCDisassembler *DisAsm = TheTarget->createMCDisassembler(*STI, Ctx);

    if (!DisAsm)
        hqemu_error("no disassembler for target %s\n", TripleName.c_str());

    const MCInstrAnalysis *MIA = TheTarget->createMCInstrAnalysis(MII);

    int AsmPrinterVariant = MAI->getAssemblerDialect();
#if defined(LLVM_V35)
    MCInstPrinter *IP = TheTarget->createMCInstPrinter(
            AsmPrinterVariant, *MAI, *MII, *MRI, *STI);
#else
    MCInstPrinter *IP = TheTarget->createMCInstPrinter(Triple(TripleName),
            AsmPrinterVariant, *MAI, *MII, *MRI);
#endif
    if (!IP)
        hqemu_error("no instruction printer for target %s\n", TripleName.c_str());

    IP->setPrintImmHex(true);

    this->DisAsm = DisAsm;
    this->STI = STI;
    this->IP = IP;
    this->MIA = MIA;
}

MCDisasm::~MCDisasm()
{
}


void MCDisasm::DumpBytes(ArrayRef<uint8_t> bytes, raw_ostream &OS)
{
    if (NoShowRawInsn)
        return;

    static const char hex_rep[] = "0123456789abcdef";
    OS << "  ";
    for (auto I = bytes.rbegin(), E = bytes.rend(); I != E; ++I) {
        char c = *I;
        OS << hex_rep[(c & 0xF0) >> 4];
        OS << hex_rep[c & 0xF];
        OS << ' ';
    }
}

#if defined(LLVM_V35)
class DisasmMemoryObject : public MemoryObject {
    uint8_t *Bytes;
    uint64_t Size;
    uint64_t BasePC;
public:
    DisasmMemoryObject(uint8_t *bytes, uint64_t size, uint64_t basePC) :
                       Bytes(bytes), Size(size), BasePC(basePC) {}

    uint64_t getBase() const override { return BasePC; }
    uint64_t getExtent() const override { return Size; }

    int readByte(uint64_t Addr, uint8_t *Byte) const override {
        if (Addr - BasePC >= Size)
            return -1;
        *Byte = Bytes[Addr - BasePC];
        return 0;
    }
    ArrayRef<uint8_t> slice(size_t N, size_t M) const {
        return makeArrayRef<uint8_t>(Bytes+N, M);
    }
};

void MCDisasm::PrintInAsm(uint64_t Addr, uint64_t Size, uint64_t GuestAddr)
{
    uint64_t Len;
    DisasmMemoryObject MemoryObject((uint8_t *)Addr, Size, Addr);

    for (uint64_t Start = 0; Start < Size; Start += Len) {
        MCInst Inst;
        std::string Str;
        raw_string_ostream OS(Str);
        if (DisAsm->getInstruction(Inst, Len, MemoryObject,
                                   Addr + Start, nulls(), nulls())) {
            OS << format("0x%08" PRIx64 ":", GuestAddr);

            DumpBytes(MemoryObject.slice(Start, Len), OS);
            IP->printInst(&Inst, OS, "");

            if (MIA && (MIA->isCall(Inst) || MIA->isUnconditionalBranch(Inst) ||
                MIA->isConditionalBranch(Inst))) {
                uint64_t Target;
                if (MIA->evaluateBranch(Inst, GuestAddr, Len, Target)) {
                    OS << " <" << format("0x%08" PRIx64, Target) << ">";
                    if (HostDisAsm) {
                        if (Target == (uint64_t)tb_ret_addr)
                            OS << " !tb_ret_addr";
                    }
                }
            }
        } else {
            OS << "\t<internal disassembler error>";
            if (Len == 0)
                Len = 1;
        }

        DM.debug() << OS.str() << "\n";
        GuestAddr += Len;
    }
}
#else
void MCDisasm::PrintInAsm(uint64_t Addr, uint64_t Size, uint64_t GuestAddr)
{
    uint64_t Len;
    ArrayRef<uint8_t> Bytes(reinterpret_cast<const uint8_t *>(Addr), Size);

    for (uint64_t Start = 0; Start < Size; Start += Len) {
        MCInst Inst;
        std::string Str;
        raw_string_ostream OS(Str);
        if (DisAsm->getInstruction(Inst, Len, Bytes.slice(Start),
                                   Addr + Start, nulls(), nulls())) {
            OS << format("0x%08" PRIx64 ":", GuestAddr);

            DumpBytes(Bytes.slice(Start, Len), OS);
            IP->printInst(&Inst, OS, "", *STI);

            if (MIA && (MIA->isCall(Inst) || MIA->isUnconditionalBranch(Inst) ||
                MIA->isConditionalBranch(Inst))) {
                uint64_t Target;
                if (MIA->evaluateBranch(Inst, GuestAddr, Len, Target)) {
                    OS << " <" << format("0x%08" PRIx64, Target) << ">";
                    if (HostDisAsm) {
                        if (Target == (uint64_t)tb_ret_addr)
                            OS << " !tb_ret_addr";
                    }
                }
            }
        } else {
            OS << "\t<internal disassembler error>";
            if (Len == 0)
                Len = 1;
        }

        DM.debug() << OS.str() << "\n";
        GuestAddr += Len;
    }
}
#endif

void MCDisasm::PrintOutAsm(uint64_t Addr, uint64_t Size)
{
    auto &OS = DM.debug();
    OS << "\nOUT: [size=" << Size << "]\n";
    PrintInAsm(Addr, Size, Addr);
    OS << "\n";
}

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
