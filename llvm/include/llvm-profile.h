/*
 *  (C) 2010 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __LLVM_PROFILE_H
#define __LLVM_PROFILE_H

#include "utils.h"

#define MAX_PROFILE_THREADS 256

#define PROFILE_NONE        (uint64_t)0
#define PROFILE_BASIC       ((uint64_t)1 << 0)
#define PROFILE_TRACE       ((uint64_t)1 << 1)
#define PROFILE_CACHE       ((uint64_t)1 << 2)
#define PROFILE_PASS        ((uint64_t)1 << 3)
#define PROFILE_HOTSPOT     ((uint64_t)1 << 4)
#define PROFILE_ALL         PROFILE_BASIC | PROFILE_TRACE | PROFILE_CACHE | \
                            PROFILE_PASS | PROFILE_HOTSPOT
#define PROFILE_NUM         7


class ProfileFactory {
public:
    typedef void (*ExitFuncPtr)(void);

    ProfileFactory() : Mode(PROFILE_NONE) {}

    void registerExitFn(ExitFuncPtr F) {
        ExitFunc.push_back(F);
    }
    uint64_t getProfile() {
        return Mode;
    }
    bool isEnabled() {
        return Mode != PROFILE_NONE;
    }
    void setProfileMode(std::string &ProfileLevel) {
        ParseProfileMode(ProfileLevel);
    }

    void printProfile();

private:
    uint64_t Mode;
    std::vector<ExitFuncPtr> ExitFunc;

    void ParseProfileMode(std::string &ProfileLevel);
    void printBlockProfile();
    void printTraceProfile();
};

extern ProfileFactory PF;

#endif

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
