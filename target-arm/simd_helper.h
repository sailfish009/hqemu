
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
    TCGArg _rd = offsetof(CPUARMState, vfp.regs[rd]); \
    TCGArg _rn = offsetof(CPUARMState, vfp.regs[rn]); \
    TCGArg _rm = offsetof(CPUARMState, vfp.regs[rm]); \
    if (q == 1) {                       \
        switch(size) {                  \
        case 0: _opc = INDEX_op_##op##_##etype##8_128;  break; \
        case 1: _opc = INDEX_op_##op##_##etype##16_128; break; \
        case 2: _opc = INDEX_op_##op##_##etype##32_128; break; \
        case 3: _opc = INDEX_op_##op##_##etype##64_128; break; \
        default:                        \
            fprintf(stderr, "%s:%d: tcg fatal error: size=%d q=%d\n", \
                            __FILE__, __LINE__, size, q);              \
            exit(0);                    \
            break;                      \
	}                               \
    } else {			        \
        switch(size) {                  \
	case 0: _opc = INDEX_op_##op##_##etype##8_64;   break; \
	case 1: _opc = INDEX_op_##op##_##etype##16_64;  break; \
	case 2: _opc = INDEX_op_##op##_##etype##32_64;  break; \
        default:                        \
            fprintf(stderr, "%s:%d: tcg fatal error: size=%d q=%d\n", \
                            __FILE__, __LINE__, size, q);             \
            exit(0);                    \
            break;                      \
        }			        \
    }                                   \
    gen_vector_op3(_opc, _rd, _rn, _rm); \
} while (0)

#define gen_vector_fop(op)             \
do {                                   \
    TCGOpcode _opc = 0;                \
    TCGArg _rd = offsetof(CPUARMState, vfp.regs[rd]); \
    TCGArg _rn = offsetof(CPUARMState, vfp.regs[rn]); \
    TCGArg _rm = offsetof(CPUARMState, vfp.regs[rm]); \
    if(q == 1)                         \
        _opc = INDEX_op_##op##_f32_128;\
    else                               \
	_opc = INDEX_op_##op##_f32_64; \
    gen_vector_op3(_opc, _rd, _rn, _rm);              \
} while (0)	

#define gen_vector_fop2(op)            \
do {                                   \
    TCGOpcode _opc = 0;                \
    TCGArg _rd = offsetof(CPUARMState, vfp.regs[rd]); \
    TCGArg _rn = offsetof(CPUARMState, vfp.regs[rn]); \
    TCGArg _rm = offsetof(CPUARMState, vfp.regs[rm]); \
    if(q == 1)                         \
        _opc = (size) ? INDEX_op_##op##_f64_128 : INDEX_op_##op##_f32_128;\
    else                               \
        _opc = INDEX_op_##op##_f32_64; \
    gen_vector_op3(_opc, _rd, _rn, _rm);              \
} while (0)	

#define gen_vector_logical(op)         \
do {                                   \
    TCGOpcode _opc = 0;                \
    TCGArg _rd = offsetof(CPUARMState, vfp.regs[rd]); \
    TCGArg _rn = offsetof(CPUARMState, vfp.regs[rn]); \
    TCGArg _rm = offsetof(CPUARMState, vfp.regs[rm]); \
    if(q == 1) \
        _opc = INDEX_op_##op##_128;    \
    else                               \
        _opc = INDEX_op_##op##_64;     \
    gen_vector_op3(_opc, _rd, _rn, _rm);              \
} while (0)

#define gen_vector_cvt(op,size)        \
do {                                   \
    TCGOpcode _opc = INDEX_op_##op##_128;             \
    TCGArg _rd = offsetof(CPUARMState, vfp.regs[rd]); \
    TCGArg _rm = offsetof(CPUARMState, vfp.regs[rm]); \
    gen_vector_op3(_opc, _rd, _rm, size); \
} while (0)

