/**
 * v.c - value representation
 */

#pragma once

#include "a.h"

/**
 * Strings (S) are a U32 length followed by a \0-terminated sequence of utf8-encoded bytes.
 * Symbols (Y) are a perfect hash.
 * Dictionaries (D) are a qp-trie.
 * Quotations (Q) are an index into the bytecode array.
 *
 * Each corresponding array type (except for string arrays) is a shape followed
 * by a row-major sequence of values.
 *
 * A string array (AS) represents an array of strings s by a shape, followed by
 * a table of indexes i of the starting position of each string, followed by a 
 * size z, followed by a length z sequence of characters c such that c[i[n]] = s[n].
 * Lengths are not stored in c, since the length of any string s[n] is i[n+1]-i[n] 
 * (z is the last element of i, so this works for the final string as well).
 *
 * An array of dictionaries is an array of pointers, so is currently not supported
 * as an mmapped type.
 */

typedef enum{ vI, vF, vS, vY, vD, vQ }VT;

typedef U8B *S;
typedef U32 Q;
typedef U32 Y;

typedef void *D; // TODO implement dicts

#define ARE_MAX_RANK 4
#define AMR ARE_MAX_RANK
typedef USZ AZ; // array size

//typedef struct{AZ s[AMR];VT t;U8 r;union{I32 *ai;F64 *af;U8B *as;Y *ay;D *ad;Q *aq;};}A;
typedef struct{AZ s[AMR];USZ l;VT t;U8 r;U8 a[];}A;
#define ai(x) ((I32*)((x)->a))
#define af(x) ((F64*)((x)->a))
#define as(x) ((S*)((x)->a))
#define ay(x) ((Y*)((x)->a))
#define aq(x) ((Q*)((x)->a))

USZ elsz(VT t){COND(t, vI,R szof(I32), vF,R szof(F64), vS,R szof(S), vY,R szof(Y), vD,R szof(D), vQ,R szof(Q))}
USZ nel(U8 r,AZ s[r]){USZ n=1;DO(r,n*=s[i])R n;}
void ass(A *a,U8 r,AZ s[r]){DO(AMR,a->s[i]=1)DO(r,a->s[i]=s[i])a->r=r;a->l=nel(r,s);}
A *anew(VT t,U8 r,AZ s[r]){A *a=ma(offsetof(A,a)+nel(r,s)*elsz(t));a->t=t;ass(a,r,s);R a;}
void afree(A *a){free(a);}

#define ATIMPL3(F,a,i) F(a)[i]
#define ATIMPL4(F,a,i,j) F(a)[i+a->s[0]*j]

U32 slen(S s){R ((U32 *)s)[-1];}
S snew(U32 z, U8B b[z]){S s=ma(z+1+szof(U32));*((U32 *)s)=z;s+=szof(U32);mc(s,b,z);s[z]='\0';R s;}
void sfree(S s){free(s-szof(U32));}

typedef union {
	U64 u64;
	F64 f;
	A *a;
	I32 i;
	S s;
	Y y;
	D d;
	Q q;
	struct {
		uint32_t p; // payload
		uint32_t tag;
	} bits;
} V;

/*
 * 64-bit platforms
 *
 * This range of NaN space is represented by 64-bit numbers begining with
 * 13 bits of ones. That is, the first 16 bits are 0xFFF8 or higher.  In
 * practice, no higher value is used for NaNs.  We rely on the fact that no
 * valid double-precision numbers will be "higher" than this (compared as an
 * uint64).
 *
 * By adding 7 * 2^48 as a 64-bit integer addition, we shift the first 16 bits
 * in the doubles from the range 0000..FFF8 to the range 0007..FFFF.  Doubles
 * are decoded by reversing this operation, i.e. substracting the same number.
 *
 * The top 16-bits denote the type of the encoded nanbox_t:
 *
 *     Pointer {  0000:PPPP:PPPP:PPPP
 *             /  0001:xxxx:xxxx:xxxx
 *     Aux.   {           ...
 *             \  0005:xxxx:xxxx:xxxx
 *     Integer {  0006:0000:IIII:IIII
 *              / 0007:****:****:****
 *     Double  {          ...
 *              \ FFFF:****:****:****
 *
 * 32-bit signed integers are marked with the 16-bit tag 0x0006.
 *
 * The tags 0x0001..0x0005 can be used to store five additional types of
 * 48-bit auxillary data, each storing up to 48 bits of payload.
 *
 * The tag 0x0000 denotes a pointer, or another form of tagged immediate.
 * Boolean, 'null', 'undefined' and 'deleted' are represented by specific,
 * invalid pointer values:
 *
 *     False:     0x06
 *     True:      0x07
 *     Undefined: 0x0a
 *     Null:      0x02
 *     Empty:     0x00
 *     Deleted:   0x05
 *
 * All of these except Empty have bit 0 or bit 1 set.
 */

// This value is 7 * 2^48, used to encode doubles such that the encoded value
// will begin with a 16-bit pattern within the range 0x0007..0xFFFF.
#define NANBOX_DOUBLE_ENCODE_OFFSET 0x0007000000000000llu
// If the 16 first bits are 0x0002, this indicates an integer number.  Any
// larger value is a double, so we can use >= to check for either integer or
// double.
#define NANBOX_MIN_NUMBER           0x0006000000000000llu
#define NANBOX_HIGH16_TAG           0xffff000000000000llu

// There are 5 * 2^48 auxillary values can be stored in the 64-bit integer
// range NANBOX_MIN_AUX..NANBOX_MAX_AUX.
#define NANBOX_MIN_AUX_TAG          0x00010000
#define NANBOX_MAX_AUX_TAG          0x0005ffff
#define NANBOX_MIN_AUX              0x0001000000000000llu
#define NANBOX_MAX_AUX              0x0005ffffffffffffllu

#define NANBOX_TAG_Q                0x0001000000000000llu

// NANBOX_MASK_POINTER defines the allowed non-zero bits in a pointer.
#define NANBOX_MASK_POINTER         0x0000fffffffffffcllu

// The 'empty' value is guarranteed to consist of a repeated single byte,
// so that it should be easy to memset an array of nanboxes to 'empty' using
// NANBOX_EMPTY_BYTE as the value for every byte.
#define NANBOX_EMPTY_BYTE           0x0

P vnp(V v){R v.u64==0ull;}
V vn(void){V v;v.u64=0ull;R v;}

/* true if val is a double or an int */
P vnump(V v){R v.u64>=NANBOX_MIN_NUMBER;}

P vip(V v){R (v.u64&NANBOX_HIGH16_TAG)==NANBOX_MIN_NUMBER;}
V i2v(I32 i){V v;v.u64=NANBOX_MIN_NUMBER|(U32)i;R v;}
I32 v2i(V v){assert(vip(v));R (I32)v.u64;}

P vfp(V v){R vnump(v)&&!vip(v);}
V f2v(F64 f){V v;v.f=f;v.u64+=NANBOX_DOUBLE_ENCODE_OFFSET;R v;}
F64 v2f(V v){assert(vfp(v));v.u64-=NANBOX_DOUBLE_ENCODE_OFFSET;R v.f;}

P vap(V v){R !(v.u64 & ~NANBOX_MASK_POINTER) && v.u64!=0ull;}
V a2v(A *a){V v;v.a=a;R v;}
A *v2a(V v){assert(vap(v));R v.a;}

P vqp(V v){R v.u64&NANBOX_TAG_Q;}
V q2v(Q q){V v;v.u64=NANBOX_TAG_Q|q;R v;}
Q v2q(V v){assert(vqp(v));R (Q)v.u64;}

void pv(V v)																				{
	if(vip(v)){printf("%"PRIi32,v2i(v));}
	else if(vfp(v)){printf("%f",v2f(v));}
	else if(vqp(v)){printf("$%"PRIu32,v2q(v));}
	else if(vap(v))																			{
		A *a=v2a(v);
		for(AZ i=0,rl=a->s[a->r-1];i<a->l;++i)												{
			if(i>0&&i%rl==0){putchar('\n');}
			if(a->r>1&&i>0&&i%(a->s[a->r-2]*a->s[a->r-1])==0){putchar('\n');}
			switch(a->t)																	{
				C vI:printf("%"PRIi32" ",ai(a)[i]);B;
				C vF:printf("%f ",af(a)[i]);B;
				C vQ:printf("$%"PRIu32" ",aq(a)[i]);B;
				default:puts("other value ");												}}}
	else {puts("other value");}																}

#if 0
int main(int argc,char**argv){
	V v=i2v(42);
	if(vnump(v)){puts("number");}else{puts("err: should be a number");}
	if(vip(v)){printf("int %"PRIi32"\n",v2i(v));}else{puts("err: unknown type");}
	if(vap(v)){puts("err: both i and a");}if(vfp(v)){puts("err: both i and f");}
	v=f2v(1.23);
	if(vnump(v)){puts("number");}else{puts("err: should be a number");}
	if(vfp(v)){printf("double %f\n",v2f(v));}else{puts("err: unknown type");}
	if(vap(v)){puts("err: both f and a");}if(vip(v)){puts("err: both f and i");}
	v=q2v(56);
	if(vnump(v)){puts("err: should not be a number");}else{puts("not a number");}
	if(vqp(v)){printf("quot: $%"PRIu32"\n",v2q(v));}else{puts("err: unknown type");}
	if(vap(v)){puts("err: both q and a");}if(vip(v)){puts("err: both q and i");}
	A *a=anew(vI,3,(AZ[]){3,3,3});
	DO(a->l,ai(a)[i]=i+1)
	v=a2v(a);
	if(vap(v)){puts("array");printf("rank: %"PRIu8", length: %zu\n", a->r,a->l);pv(v);putchar('\n');}else{puts("err: unknown type");}
	if(vip(v)){puts("err: both a and i");}if(vfp(v)){puts("err: both a and f");}
	R 0;
}
#endif
