#pragma once

#include <stddef.h>
#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <assert.h>
#include <setjmp.h>

#define R return
#define C case
#define B break
#define szof sizeof
#define ma malloc
#define ca calloc
#define ra realloc
#define mc memcpy

typedef uint8_t U8;
typedef int8_t I8;
typedef uint16_t U16;
typedef int16_t I16;
typedef uint32_t U32;
typedef int32_t I32;
typedef uint64_t U64;
typedef int64_t I64;
typedef size_t USZ;
typedef uintptr_t UIP;
typedef double F64;

typedef bool P;

typedef U32 CP; // codepoint
typedef U8 U8B; // utf8 byte

#define max(a,b) ({__typeof(a)_x=(a);__typeof(b)_y=(b);_x>_y?_x:_y;})
#define min(a,b) ({__typeof(a)_x=(a);__typeof(b) _y=(b);_x<_y?_x:_y;})

#define sgn(a) (((a)!=0)|(a>>sizeof(a)*8-1))

#define swap(a,b) do{__typeof(a)_a=(a);a=b;b=_a;}while(0)

//#define clz(a) _Generic((a),unsigned int:__builtin_clz((a)),unsigned long:__builtin_clzl((a)),unsigned long long:__builtin_clzll(a))

// Cute trick from Chromium to error in some cases where a pointer is passed to countof() instead of an array. Causes an error if
// sizeof(ptr) % sizeof(*ptr) != 0.
#define cntof(a) ((sizeof(a)/sizeof 0[a])/(USZ)(!(sizeof(a)%sizeof 0[a])))
#define msz(T,m) (sizeof(((T*)0)->m)) // member size

#define DO(n,...) {__typeof(n)i=0;for(__typeof(n)i_=(n);i<i_;++i){__VA_ARGS__;}}
#define DO2(n,...) {__typeof(n)j=0;for(__typeof(n)j_=(n);j<j_;++j){__VA_ARGS__;}}
#define DO3(n,...) {__typeof(n)k=0;for(__typeof(n)k_=(n);k<k_;++k){__VA_ARGS__;}}
#define DO4(n,...) {__typeof(n)l=0;for(__typeof(n)l_=(n);l<l_;++l){__VA_ARGS__;}}

#define VA_NARGS_IMPL(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,N,...) N
#define VA_NARGS(...) VA_NARGS_IMPL(_,##__VA_ARGS__,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0)
#define CATIMPL(a,...) a##__VA_ARGS__
#define CAT(a,...)CATIMPL(a,__VA_ARGS__)

#define ANYIMPL0()
#define ANYIMPL1(x) x
#define ANYIMPL2(x,y) x:case ANYIMPL1(y)
#define ANYIMPL3(x,...) x:case ANYIMPL2(__VA_ARGS__)
#define ANYIMPL4(x,...) x:case ANYIMPL3(__VA_ARGS__)
#define ANYIMPL5(x,...) x:case ANYIMPL4(__VA_ARGS__)
#define ANYIMPL6(x,...) x:case ANYIMPL5(__VA_ARGS__)
#define ANYIMPL7(x,...) x:case ANYIMPL6(__VA_ARGS__)
#define ANYIMPL8(x,...) x:case ANYIMPL7(__VA_ARGS__)
#define ANYIMPL9(x,...) x:case ANYIMPL8(__VA_ARGS__)
#define ANYIMPL10(x,...) x:case ANYIMPL9(__VA_ARGS__)
#define ANY(...) CAT(ANYIMPL,VA_NARGS(__VA_ARGS__))(__VA_ARGS__)

#define CONDIMPL0()
#define CONDIMPL3(y,f,d) C y:f;B;default:d;
#define CONDIMPL5(y,f,...) C y:f;B;CONDIMPL3(__VA_ARGS__)
#define CONDIMPL7(y,f,...) C y:f;B;CONDIMPL5(__VA_ARGS__)
#define CONDIMPL9(y,f,...) C y:f;B;CONDIMPL7(__VA_ARGS__)
#define CONDIMPL11(y,f,...) C y:f;B;CONDIMPL9(__VA_ARGS__)
#define CONDIMPL13(y,f,...) C y:f;B;CONDIMPL11(__VA_ARGS__)
#define CONDIMPL15(y,f,...) C y:f;B;CONDIMPL13(__VA_ARGS__)
#define CONDIMPL17(y,f,...) C y:f;B;CONDIMPL15(__VA_ARGS__)
// even # args = no default
#define CONDIMPL2(y,f) C y:f;B;
#define CONDIMPL4(y,f,...) C y:f;B;CONDIMPL2(__VA_ARGS__)
#define CONDIMPL6(y,f,...) C y:f;B;CONDIMPL4(__VA_ARGS__)
#define CONDIMPL8(y,f,...) C y:f;B;CONDIMPL6(__VA_ARGS__)
#define CONDIMPL10(y,f,...) C y:f;B;CONDIMPL8(__VA_ARGS__)
#define CONDIMPL12(y,f,...) C y:f;B;CONDIMPL10(__VA_ARGS__)
#define CONDIMPL14(y,f,...) C y:f;B;CONDIMPL12(__VA_ARGS__)
#define CONDIMPL16(y,f,...) C y:f;B;CONDIMPL14(__VA_ARGS__)
#define CONDIMPL18(y,f,...) C y:f;B;CONDIMPL16(__VA_ARGS__)
#define COND(x,...) switch(x){CAT(CONDIMPL,VA_NARGS(__VA_ARGS__))(__VA_ARGS__)}

#define CONDEIMPL0()
#define CONDEIMPL3(p,f,e) (p)?({f;}):({e;})
#define CONDEIMPL5(p,f,...) (p)?({f;}):CONDEIMPL3(__VA_ARGS__)
#define CONDEIMPL7(p,f,...) (p)?({f;}):CONDEIMPL5(__VA_ARGS__)
#define CONDEIMPL9(p,f,...) (p)?({f;}):CONDEIMPL7(__VA_ARGS__)
#define CONDE(...) (CAT(CONDEIMPL,VA_NARGS(__VA_ARGS__))(__VA_ARGS__))

typedef enum{ eOK, eSO, eT, eL }E;
extern jmp_buf ej;
void ae(E);
