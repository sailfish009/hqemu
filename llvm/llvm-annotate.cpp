/*
 *  (C) 2015 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#include "xml/tinyxml2.h"
#include "optimization.h"
#include "llvm-debug.h"
#include "llvm-annotate.h"


using namespace tinyxml2;
static hqemu::Mutex Lock;

#if defined(CONFIG_USER_ONLY)
extern "C" const char *filename;
#endif

AnnotationFactory::AnnotationFactory()
{
#if defined(CONFIG_USER_ONLY)
    int ret;
    MetaFile = std::string(filename).append(".xml");
    ret = ParseXML(MetaFile.c_str());
    if (ret != 0)
        return;
#endif
}

AnnotationFactory::~AnnotationFactory()
{
    for (auto L : Loops)
        delete L.second;
}

static inline const char *getAttrName(XMLElement *Attr)
{
    return Attr->Name();
}

static inline const char *getAttrValue(XMLElement *Attr)
{
    return Attr->FirstChild() ? Attr->FirstChild()->ToText()->Value() : "";
}

static LoopMetadata *ParseXMLLoop(XMLElement *LoopNode)
{
    if (LoopNode == nullptr)
        return nullptr;

    LoopMetadata *LoopMD = new LoopMetadata();
    XMLElement *Attr = LoopNode->FirstChildElement();
    while (Attr) {
        std::string Name = getAttrName(Attr);
        const char *Val = getAttrValue(Attr);
        if (strlen(Val) == 0)
            goto next;

        if (Name == "address")
            LoopMD->Address = (target_ulong)strtoull(Val, nullptr, 16);
        else if (Name == "length")
            LoopMD->Length = (uint32_t)strtoul(Val, nullptr, 10);
        else if (Name == "vs")
            LoopMD->VS = (uint32_t)strtoul(Val, nullptr, 10);
        else if (Name == "vf")
            LoopMD->VF = (uint32_t)strtoul(Val, nullptr, 10);
        else if (Name == "distance") {
            LoopMD->Distance = atoi(Val);
            if (LoopMD->Distance == 0)
                LoopMD->Distance = INT_MAX;
        }
        else if (Name == "start")    LoopMD->Start = atoi(Val);
        else if (Name == "end")      LoopMD->End = atoi(Val);
        else if (Name == "stride")   LoopMD->Stride = atoi(Val);
next:
        Attr = Attr->NextSiblingElement();
    }

    if (LoopMD->Address == (target_ulong)-1) {
        delete LoopMD;
        return nullptr;
    }

    return LoopMD;
}

int AnnotationFactory::ParseXML(const char *name)
{
    XMLDocument Doc;
    XMLElement *RootNode, *LoopNode;

    if (Doc.LoadFile(name) != 0) {
        dbg() << DEBUG_ANNOTATE << "Disable annotation support."
              << " (cannot find " << name << ")\n";
        return 1;
    }

    dbg() << DEBUG_ANNOTATE << "Found an annotation file " << name << "\n";

    /* A legal annoation should be embedded within the <hqemu> tag. For example:
     *   <hqemu><loop><addr>...</addr></loop></hqemu> */
    RootNode = Doc.FirstChildElement("hqemu");
    if (RootNode == nullptr)
        return 1;

    LoopNode = RootNode->FirstChildElement("loop");
    while (LoopNode) {
        LoopMetadata *LoopMD = ParseXMLLoop(LoopNode);
        if (LoopMD)
            Loops[LoopMD->Address] = LoopMD;
        LoopNode = LoopNode->NextSiblingElement();
    }

    dbg() << DEBUG_ANNOTATE
          << "Found " << Loops.size() << " loop annotation(s).\n";
    return 0;
}

LoopMetadata *AnnotationFactory::getLoopAnnotation(target_ulong addr)
{
    hqemu::MutexGuard locked(Lock);

    if (Loops.find(addr) == Loops.end())
        return nullptr;
    return Loops[addr];
}

bool AnnotationFactory::hasLoopAnnotation(target_ulong addr)
{
    hqemu::MutexGuard locked(Lock);
    return Loops.count(addr) ? true : false;
}

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
