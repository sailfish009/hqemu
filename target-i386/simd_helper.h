
static inline void gen_vector_op3(TCGOpcode opc, TCGArg arg1, TCGArg arg2,
                                  TCGArg arg3)
{
    int pi = tcg_ctx.gen_next_parm_idx;
    tcg_emit_op(&tcg_ctx, opc, pi);
    *tcg_ctx.vec_opparam_ptr++ = arg1;
    *tcg_ctx.vec_opparam_ptr++ = arg2;
    *tcg_ctx.vec_opparam_ptr++ = arg3;
}

#define gen_vector_arith(op,etype,size) \
do {                                    \
    TCGOpcode _opc = 0;                 \
    TCGArg _rd = offsetof(CPUX86State, xmm_regs[rd]);              \
    TCGArg _rn = offsetof(CPUX86State, xmm_regs[rn]);              \
    TCGArg _rm = (rm == -1) ? offsetof(CPUX86State, xmm_t0) :      \
                              offsetof(CPUX86State, xmm_regs[rm]); \
    switch(size) {                  \
    case 0: _opc = INDEX_op_##op##_##etype##8_128;  break; \
    case 1: _opc = INDEX_op_##op##_##etype##16_128; break; \
    case 2: _opc = INDEX_op_##op##_##etype##32_128; break; \
    case 3: _opc = INDEX_op_##op##_##etype##64_128; break; \
    default:                        \
        fprintf(stderr, "%s:%d: tcg fatal error: size=%d\n", \
                        __FILE__, __LINE__, size);           \
        exit(0);                    \
        break;                      \
    }                               \
    gen_vector_op3(_opc, _rd, _rn, _rm); \
} while (0)

#define gen_vector_fop(op,size)        \
do {                                   \
    TCGOpcode _opc = 0;                \
    TCGArg _rd = offsetof(CPUX86State, xmm_regs[rd]);              \
    TCGArg _rn = offsetof(CPUX86State, xmm_regs[rn]);              \
    TCGArg _rm = (rm == -1) ? offsetof(CPUX86State, xmm_t0) :      \
                              offsetof(CPUX86State, xmm_regs[rm]); \
    if(size == 0)                      \
        _opc = INDEX_op_##op##_f32_128;\
    else                               \
	_opc = INDEX_op_##op##_f64_128;\
    gen_vector_op3(_opc, _rd, _rn, _rm); \
} while (0)	

#define gen_vector_logical(op)	       \
do {                                   \
    TCGOpcode _opc = INDEX_op_##op##_128;                          \
    TCGArg _rd = offsetof(CPUX86State, xmm_regs[rd]);              \
    TCGArg _rn = offsetof(CPUX86State, xmm_regs[rn]);              \
    TCGArg _rm = (rm == -1) ? offsetof(CPUX86State, xmm_t0) :      \
                              offsetof(CPUX86State, xmm_regs[rm]); \
    gen_vector_op3(_opc, _rd, _rn, _rm); \
} while (0)

#define gen_vector_cvt(op,size)        \
do {                                   \
    TCGOpcode _opc = INDEX_op_##op##_128;                          \
    TCGArg _rd = offsetof(CPUX86State, xmm_regs[rd]);              \
    TCGArg _rm = (rm == -1) ? offsetof(CPUX86State, xmm_t0) :      \
                              offsetof(CPUX86State, xmm_regs[rm]); \
    gen_vector_op3(_opc, _rd, _rm, size); \
} while (0)
