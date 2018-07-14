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

typedef enum{ vA=0, vQ=1, vY=2, vS=3, vD=4, vI=6, vF=7 }VT;

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

typedef union {U64 u64;F64 f;A *a;} V;

/**
 * This range of NaN space is represented by 64-bit numbers begining with
 * 13 bits of ones. That is, the first 16 bits are 0xFFF8 or higher.  In
 * practice, no higher value is used for NaNs.  We rely on the fact that no
 * valid double-precision numbers will be "higher" than this (compared to an
 * uint64).
 *
 * By adding 7 * 2^48 as a 64-bit integer addition, we shift the first 16 bits
 * in the doubles from the range 0000..FFF8 to the range 0007..FFFF.  Doubles
 * are decoded by reversing this operation, i.e. substracting the same number.
 *
 * The top 16-bits denote the type of the encoded value:
 *
 *     Array    { 0000:xxxx:xxxx:xxxx
 *     Quot     { 0001:0000:xxxx:xxxx
 *     Symbol   { 0002:0000:xxxx:xxxx
 *     String   { 0003:xxxx:xxxx:xxxx
 *     Dict     { 0004:pppp:xxxx:xxxx
 * (unassigned) { 0005:xxxx:xxxx:xxxx
 *     Integer  { 0006:0000:xxxx:xxxx
 *              / 0007:xxxx:xxxx:xxxx
 *     Double  {         ...
 *              \ FFFF:xxxx:xxxx:xxxx
 *
 * Integers are stored right before doubles. This means any value greater than
 * 0x0006000000000000ull is a number.
 */

#define VTAG                        0xffff000000000000ull
#define VTQ                         ((U64)vQ<<48)
#define VTY                         ((U64)vY<<48)
#define VTS                         ((U64)vS<<48)
#define VTD                         ((U64)vD<<48)
#define VTI                         ((U64)vI<<48)
// No tag for VTF since its upper 16 bits are 0x0007-0xffff. Instead use an offset
// which gets added to double values to ensure that the encoded value begins with
// a 16-bit pattern in the range 0x0007-0xFFFF.
#define VFOFF                       0x0007000000000000ull

// Allowed non-zero bits in a pointer.
#define VMASKPTR         0x0000fffffffffffcull

P vnp(V v){R v.u64==0ull;}
V vn(void){V v;v.u64=0ull;R v;}

VT vt(V v){R min((v.u64&VTAG)>>48,7);}

/**
 * True if v is an integer or double. Can check using >= since all number
 * values will be greater than or equal to vI<<48. This is why vI is 6, so
 * that int values are encoded contiguous to doubles.
 */
P vnump(V v){R v.u64>=VTI;}

P vip(V v){R (v.u64&VTAG)==VTI;}
V i2v(I32 i){V v;v.u64=VTI|(U32)i;R v;}
I32 v2i(V v){assert(vip(v));R (I32)v.u64;}

P vfp(V v){R vnump(v)&&!vip(v);}
V f2v(F64 f){V v;v.f=f;v.u64+=VFOFF;R v;}
F64 v2f(V v){assert(vfp(v));v.u64-=VFOFF;R v.f;}

P vap(V v){R !(v.u64 & ~VMASKPTR) && v.u64!=0ull;}
V a2v(A *a){V v;v.a=a;R v;}
A *v2a(V v){assert(vap(v));R v.a;}

P vqp(V v){R (v.u64&VTAG)==VTQ;}
V q2v(Q q){V v;v.u64=VTQ|q;R v;}
Q v2q(V v){assert(vqp(v));R (Q)v.u64;}

P vyp(V v){R (v.u64&VTAG)==VTY;}
V y2v(Y y){V v;v.u64=VTY|y;R v;}
Y v2y(V v){assert(vyp(v));R (Y)v.u64;}

P vsp(V v){R (v.u64&VTAG)==VTS;}
V s2v(S s){V v;v.u64=VTS|(UIP)s;R v;}
S v2s(V v){assert(vsp(v));R (S)(v.u64&VMASKPTR);}

void pv(V v)																				{
	if(vip(v)){printf("%"PRIi32,v2i(v));}
	else if(vfp(v)){printf("%f",v2f(v));}
	else if(vqp(v)){printf("$%"PRIu32,v2q(v));}
	else if(vyp(v)){printf(".%"PRIu32,v2y(v));}
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
	if(vt(v)!=vI){puts("ERR: wrong tag");}
	if(vnump(v)){puts("number");}else{puts("ERR: should be a number");}
	if(vip(v)){printf("int %"PRIi32"\n",v2i(v));}else{puts("ERR: unknown type");}
	if(vap(v)){puts("ERR: both i and a");}if(vfp(v)){puts("ERR: both i and f");}
	v=f2v(1.23);
	if(vt(v)!=vF){puts("ERR: wrong tag");}
	if(vnump(v)){puts("number");}else{puts("ERR: should be a number");}
	if(vfp(v)){printf("double %f\n",v2f(v));}else{puts("ERR: unknown type");}
	if(vap(v)){puts("ERR: both f and a");}if(vip(v)){puts("ERR: both f and i");}
	v=q2v(56);
	if(vt(v)!=vQ){puts("ERR: wrong tag");}
	if(vnump(v)){puts("ERR: should not be a number");}else{puts("not a number");}
	if(vqp(v)){printf("quot: $%"PRIu32"\n",v2q(v));}else{puts("ERR: unknown type");}
	if(vap(v)){puts("ERR: both q and a");}if(vyp(v)){puts("ERR: both q and y");}
	v=y2v(1);
	if(vt(v)!=vY){puts("ERR: wrong tag");}
	if(vnump(v)){puts("ERR: should not be a number");}else{puts("not a number");}
	if(vyp(v)){printf("sym: .%"PRIu32"\n",v2y(v));}else{puts("ERR: unknown type");}
	if(vqp(v)){puts("ERR: both y and q");}if(vip(v)){puts("ERR: both y and i");}
	S s=snew(11,(U8B *)"hello world");
	v=s2v(s);
	if(vt(v)!=vS){puts("ERR: wrong tag");}
	if(vnump(v)){puts("ERR: should not be a number");}else{puts("not a number");}
	if(vsp(v)){printf("str: %.*s\n",slen(v2s(v)),v2s(v));}else{puts("ERR: unknown type");}
	if(vfp(v)){puts("ERR: both s and f");}if(vap(v)){puts("ERR: both s and a");}
	A *a=anew(vI,3,(AZ[]){3,3,3});
	DO(a->l,ai(a)[i]=i+1)
	v=a2v(a);
	if(vt(v)!=vA){puts("ERR: wrong tag");}
	if(vap(v)){puts("array");printf("rank: %"PRIu8", length: %zu\n", a->r,a->l);pv(v);putchar('\n');}else{puts("ERR: unknown type");}
	if(vip(v)){puts("ERR: both a and i");}if(vfp(v)){puts("ERR: both a and f");}
	R 0;
}
#endif
