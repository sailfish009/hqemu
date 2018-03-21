# Makefile for HQEMU.

QEMU_CFLAGS += -I$(SRC_PATH)/llvm -I$(SRC_PATH)/llvm/include -I$(SRC_PATH)/llvm/atomic
QEMU_CXXFLAGS += -std=c++11 -Wno-narrowing
obj-y += llvm/optimization.o llvm/tracer.o llvm/utils.o llvm/hqemu-helper.o

# LLVM
ifdef CONFIG_LLVM

LLVM_EXTRA_FLAGS += -Wall -DNEED_CPU_H -D$(LLVM_VERSION) -I..

LLVM_CXXFLAGS := $(patsubst -Wcast-qual, ,$(LLVM_CXXFLAGS))
LLVM_CXXFLAGS := $(patsubst -fno-exceptions, ,$(LLVM_CXXFLAGS))
LLVM_CXXFLAGS := $(patsubst -pedantic, ,$(LLVM_CXXFLAGS))
LLVM_CXXFLAGS += -Wno-unused-local-typedefs -Wno-cast-qual -fno-rtti
LLVM_CFLAGS += $(patsubst -O2, ,$(patsubst -g, ,$(CFLAGS)))
LLVM_CFLAGS += $(LLVM_EXTRA_FLAGS) $(QEMU_INCLUDES) \
	       -I$(SRC_PATH)/linux-user -I$(SRC_PATH)/linux-user/$(TARGET_ABI_DIR) \
	       -I$(SRC_PATH)/target-$(TARGET_BASE_ARCH) -I$(SRC_PATH)/llvm -I$(SRC_PATH)/llvm/atomic
LLVM_CFLAGS := $(patsubst -pedantic, ,$(LLVM_CFLAGS))
LLVM_CFLAGS := $(patsubst -g, ,$(LLVM_CFLAGS))

PASS := llvm/pass
ANALYSIS := llvm/analysis
QEMU_CXXFLAGS += $(LLVM_CXXFLAGS) $(LLVM_EXTRA_FLAGS) -Wno-undef
LDFLAGS += $(LLVM_LDFLAGS)
LIBS += $(LLVM_LIBS) -ldl -lz -lncurses


ifeq ($(CONFIG_WIN32), y)
LIBS += -lpthread -limagehlp -lpsapi
endif

obj-y += llvm/xml/tinyxml2.o
obj-y += llvm/llvm.o            \
         llvm/llvm-translator.o \
         llvm/llvm-opc.o        \
         llvm/llvm-opc-vector.o \
         llvm/llvm-opc-mmu.o    \
         llvm/llvm-debug.o      \
         llvm/llvm-target.o     \
         llvm/llvm-profile.o    \
         llvm/llvm-annotate.o
obj-y += $(PASS)/ProfileExec.o        \
         $(PASS)/ReplaceIntrinsic.o   \
         $(PASS)/CombineGuestMemory.o \
         $(PASS)/CombineCasts.o       \
         $(PASS)/CombineZExtTrunc.o   \
         $(PASS)/FastMathPass.o       \
         $(PASS)/StateMappingPass.o   \
         $(PASS)/SimplifyPointer.o
obj-y += $(ANALYSIS)/InnerLoopAnalysis.o

#
# LLVM Bitcode file
#

ifdef CONFIG_SOFTMMU
BCSUF = _softmmu
MMU_HELPER = $(TARGET_PATH)/mmu_helper.bc
endif

LLVM_BITCODE = llvm_helper_${TARGET_NAME}${BCSUF}.bc
TARGET_PATH = target-$(TARGET_BASE_ARCH)
LLVM_HELPER += tcg-runtime.bc llvm/hqemu-helper.bc $(TARGET_PATH)/helper.bc

ifeq ($(TARGET_I386), y)
LLVM_HELPER += $(TARGET_PATH)/cc_helper.bc   \
	       $(TARGET_PATH)/int_helper.bc  \
	       $(TARGET_PATH)/smm_helper.bc  \
	       $(TARGET_PATH)/excp_helper.bc \
	       $(TARGET_PATH)/mem_helper.bc  \
	       $(TARGET_PATH)/svm_helper.bc  \
	       $(TARGET_PATH)/fpu_helper.bc  \
	       $(TARGET_PATH)/misc_helper.bc \
	       $(TARGET_PATH)/seg_helper.bc  \
	       $(TARGET_PATH)/bpt_helper.bc
endif
ifeq ($(TARGET_X86_64), y)
LLVM_HELPER += $(TARGET_PATH)/cc_helper.bc   \
	       $(TARGET_PATH)/int_helper.bc  \
	       $(TARGET_PATH)/smm_helper.bc  \
	       $(TARGET_PATH)/excp_helper.bc \
	       $(TARGET_PATH)/mem_helper.bc  \
	       $(TARGET_PATH)/svm_helper.bc  \
	       $(TARGET_PATH)/fpu_helper.bc  \
	       $(TARGET_PATH)/misc_helper.bc \
	       $(TARGET_PATH)/seg_helper.bc
endif
ifeq ($(TARGET_ALPHA), y)
LLVM_HELPER += $(TARGET_PATH)/fpu_helper.bc \
	       $(TARGET_PATH)/int_helper.bc \
	       $(TARGET_PATH)/mem_helper.bc \
	       $(TARGET_PATH)/sys_helper.bc
endif
ifeq ($(TARGET_ARM), y)
LLVM_HELPER += $(TARGET_PATH)/op_helper.bc \
	       $(TARGET_PATH)/neon_helper.bc
endif
ifeq ($(TARGET_AARCH64), y)
LLVM_HELPER += $(TARGET_PATH)/op_helper.bc \
	       $(TARGET_PATH)/helper-a64.bc \
	       $(TARGET_PATH)/neon_helper.bc
endif
ifeq ($(TARGET_MICROBLAZE), y)
LLVM_HELPER += $(TARGET_PATH)/op_helper.bc
endif
ifeq ($(TARGET_MIPS), y)
LLVM_HELPER += $(TARGET_PATH)/op_helper.bc  \
	       $(TARGET_PATH)/dsp_helper.bc \
	       $(TARGET_PATH)/lmi_helper.bc
endif
ifeq ($(TARGET_OPENRISC), y)
LLVM_HELPER += $(TARGET_PATH)/exception_helper.bc \
	       $(TARGET_PATH)/fpu_helper.bc \
	       $(TARGET_PATH)/interrupt_helper.bc \
	       $(TARGET_PATH)/int_helper.bc \
	       $(TARGET_PATH)/sys_helper.bc \
	       $(MMU_HELPER)
endif
ifeq ($(TARGET_PPC), y)
LLVM_HELPER += $(TARGET_PATH)/excp_helper.bc \
	       $(TARGET_PATH)/int_helper.bc  \
	       $(TARGET_PATH)/misc_helper.bc \
	       $(TARGET_PATH)/fpu_helper.bc  \
	       $(TARGET_PATH)/mem_helper.bc  \
	       $(TARGET_PATH)/timebase_helper.bc \
	       $(MMU_HELPER)
endif
ifeq ($(TARGET_SH4), y)
LLVM_HELPER += $(TARGET_PATH)/op_helper.bc
endif
ifeq ($(TARGET_SPARC), y)
LLVM_HELPER += $(TARGET_PATH)/cc_helper.bc    \
	       $(TARGET_PATH)/fop_helper.bc   \
	       $(TARGET_PATH)/int32_helper.bc \
	       $(TARGET_PATH)/ldst_helper.bc  \
	       $(TARGET_PATH)/vis_helper.bc   \
	       $(TARGET_PATH)/win_helper.bc   \
	       $(MMU_HELPER)
endif
ifeq ($(TARGET_SPARC64), y)
LLVM_HELPER += $(TARGET_PATH)/cc_helper.bc    \
	       $(TARGET_PATH)/fop_helper.bc   \
	       $(TARGET_PATH)/int64_helper.bc \
	       $(TARGET_PATH)/ldst_helper.bc  \
	       $(TARGET_PATH)/vis_helper.bc   \
	       $(TARGET_PATH)/win_helper.bc   \
	       $(MMU_HELPER)
endif

ifneq ($(CONFIG_DRAGONEGG), )
LOCAL_BC := $(CC)
LOCAL_BC_CFLAGS := -S -fplugin=${CONFIG_DRAGONEGG} -I$(SRC_PATH)/llvm/include    \
		   -flto -fplugin-arg-dragonegg-enable-gcc-optzns $(LLVM_CFLAGS) \
		   -Wno-missing-prototypes -Wno-sign-compare
else
LOCAL_BC := clang
LOCAL_BC_CFLAGS := -S -emit-llvm $(BCFLAGS) -I$(SRC_PATH)/llvm/include $(LLVM_CFLAGS) \
	           -Wno-missing-prototypes -Wno-sign-compare -Wno-unused-function \
		   -Wno-constant-conversion
endif

%.bc: %.c
	$(call quiet-command,$(LOCAL_BC) $(LOCAL_BC_CFLAGS) -c -o $@ $<, "  LCC   $(TARGET_DIR)$@")


$(LLVM_BITCODE): $(LLVM_HELPER)
	$(call quiet-command,llvm-link -o $@ $^, "  LCC   $(TARGET_DIR)$@")

endif
