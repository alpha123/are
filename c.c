/**
 * c.c - compile strings to bytecode
 */

#pragma once

#include "a.h"
#include "u.c"
#include "v.c"
#include "qp.c"

/**
 * token types:
 * - integer literals (I)
 * - floating point literals (F)
 * - string literals (S)
 * - symbol literals (Y)
 * - words
 * - short word (1 codepoint)
 * - punctuation: ()[]{};
 *
 * There's a trick used here: enum values for the punctuation tokens are
 * '()[]{};' - '(' (the lowest UTF-8 value), which means the tokenizer can
 * set the token value for those by simply subtracting '(' from the current
 * character.
 */
typedef enum{ tLP=0, tRP=1, tSC=19, tLB=51, tRB=53, tLC=83, tRC=85, tW, tSW, tI, tF, tS, tY }TT;
typedef struct{union{I32 i;F64 f;CP c;S s;};TT t;}T;

#define rfn(n,...) USZ n(T *t,USZ z,U8B s[z]){USZ l=0;__VA_ARGS__;R l;}
rfn(rw, t->t=tW;while(l<z&&isalpha(s[l])){++l;}t->s=snew((U32)l,s))
rfn(rsw, t->t=tSW;E e=u8d(z,s,&t->c,&l);assert(e==0))
rfn(ri, t->t=tI;t->i=0;while(isdigit(*s)){t->i*=10;t->i+=*s-'0';++s;++l;})
rfn(rf, t->t=tF;F64 wp=t->i,f=0.0,d=10.0;while(isdigit(*s)){f+=(F64)(*s-'0')/d;d*=10.0;++s;++l;}t->f=wp+f)
rfn(rnum, l=ri(t,z,s);if(s[1]=='.'&&z>2){l+=rf(t,z-2,s+2)+1;})
rfn(ry, l=rw(t,z-1,s+1)+1;t->t=tY)
rfn(rsl, t->t=tS;while(l<z&&s[l]!='}'){++l;}t->s=snew((U32)l-1,s+1);++l)
USZ nt(T *t,USZ z,U8B s[z])																	{
	USZ l=0;while(isspace(*s)){++l;++s;}
	switch(*s)																				{
		C ANY('(',')','[',']',';'):t->t=*s-'(';R l+1;
		C '.':R l+ry(t,z,s);
		C '{':R l+rsl(t,z,s);
		default:R l+CONDE(isdigit(*s),rnum(t,z,s), isalpha(*s),rw(t,z,s), rsw(t,z,s));		}}

typedef enum{
	bcPushI, bcPushF, bcPushS, bcMkArray, bcJmp, bcRet, bcCall, bcCallQ, bcCallPower,
	bcSwap, bcDrop, bcDup, bcDip, bcKeep, bcPopRP, bcQuot, bcIota, bcIndex, bcShape, bcReshape,
	bcNeg, bcNot, bcSgn, bcAdd, bcMul, bcSub, bcDiv, bcMod, bcMin, bcMax, bcEq, bcGt, bcLt, bcGte, bcLte,
	bcCat, bcReplicate, bcTake, bcEach, bcReduce
}BCT;
typedef struct{U32 z,l;U8 *b;}BC;
BC *bcnew(USZ iz){BC *b=ca(1,szof(BC));b->z=iz;b->b=ma(iz);R b;}
void bcfree(BC *b){free(b->b);free(b);}
void bcgrw(BC *b){b->b=ra(b->b,b->z+=100);}
#define emit(x) bcemit(b,x)
#define emiti(x) bcemiti(b,x)
#define emitp(x) bcemitp(b,x)
void bcemit(BC *b,U8 w){if(b->l==b->z){bcgrw(b);}b->b[b->l++]=w;}
void bcemiti(BC *b,U32 i){DO(4,emit(0))*(U32 *)(b->b+b->l-4)=i;}
void bcemitf(BC *b,F64 f){DO(8,emit(0))*(F64 *)(b->b+b->l-8)=f;}
void bcemitp(BC *b,UIP p){DO(szof(UIP),emit(0))*(UIP *)(b->b+b->l-szof(UIP))=p;}
#define seti(i,x) (*(U32 *)(b->b+(i))=x)

#define rt() (tl=nt(&t,z-i,s+i))
USZ ct(BC *b,WD **d,USZ z,U8B s[z]);
void ci(BC *b,I32 i){emit(bcPushI);emiti((U32)i);}
void cf(BC *b,F64 f){emit(bcPushF);bcemitf(b,f);}
void cs(BC *b,S s){emit(bcPushS);emitp((UIP)s);}
USZ cr(BC *b,WD **d,USZ z,U8B s[z])																	{
	T t;USZ i=0,tl;U32 tc=0;
	while(i<z){rt();if(t.t==tRP){i+=tl;B;}++tc;i+=ct(b,d,z-i,s+i);}emit(bcMkArray);emiti(tc);R i;	}
USZ cq_(BC *b,WD **d,USZ z,U8B s[z],U32 *nq)														{
	T t;USZ i=0,tl;emit(bcQuot);emiti(0);U32 o=b->l;
	while(i<z)																						{
		rt();
		if(t.t==tRB){i+=tl;emit(bcRet);B;}
		else if(t.t==tSC){i+=tl;++*nq;emit(bcRet);seti(o-szof(U32),b->l-o);i+=cq_(b,d,z-i,s+i,nq);R i;}
		else{i+=ct(b,d,z-i,s+i);}																	}
	seti(o-szof(U32),b->l-o);R i;																	}
USZ cq(BC *b,WD **d,USZ z,U8B s[z]){U32 nq=1;USZ l=cq_(b,d,z,s,&nq);if(nq>1){emit(bcMkArray);emiti(nq);}R l;}
USZ csq(BC *b,WD **d,USZ z,U8B s[z])																{
	emit(bcQuot);emiti(0);U32 o=b->l;USZ i=ct(b,d,z,s);emit(bcRet);seti(o-szof(U32),b->l-o);R i;	}
void csw(BC *b,CP c){}
USZ cd(BC *b,WD **d,USZ z,U8B s[z])															{
	USZ i=0,tl;T t;rt();i+=tl;if(t.t!=tW){puts("invalid word name");R i;}
	emit(bcJmp);emiti(0);U32 o=b->l;
	*d=Tsetl(*d,t.s,o);while(i<z){rt();i+=ct(b,d,z-i,s+i);if(t.t==tSW&&t.c==0x3B){B;}}
	seti(o-szof(U32),b->l-o);R i;															}
void cw(BC *b,WD *d,S w)																	{
	U32 j;if(!Tgetkv(d,w,&w,&j)){printf("unknown word %.*s\n",(int)slen(w),w);R;}
	emit(bcCall);emiti(j);																	}
USZ ct(BC *b,WD **d,USZ z,U8B s[z])															{
	USZ i=0,tl;T t;rt();i+=tl;
	switch(t.t)																				{
		C tI:ci(b,t.i);B;
		C tF:cf(b,t.f);B;
		C tS:cs(b,t.s);B;
		C tLB:i+=cq(b,d,z-tl,s+tl);B;
		C tLP:i+=cr(b,d,z-tl,s+tl);B;
		C tSC:emit(bcRet);B;
		C tSW:switch(t.c)																	{
			C 0x22:emit(bcIndex);B;
			C 0x27:i+=csq(b,d,z-tl,s+tl);B;
			C 0x2B:emit(bcAdd);B;
			C 0x2C:emit(bcCat);B;
			C 0x2D:emit(bcSub);B;
			C 0xD7:emit(bcMul);B;
			C 0xF7:emit(bcDiv);B;
			C 0x7C:emit(bcMod);B;
			C 0x7E:emit(bcNeg);B;
			C 0xA8:emit(bcEach);B;
			C 0xAC:emit(bcNot);B;
			C 0x3D:emit(bcEq);B;
			C 0x2260:emit(bcEq);emit(bcNot);B;
			C 0x3E:emit(bcGt);B;
			C 0x3C:emit(bcLt);B;
			C 0x2265:emit(bcGte);B;
			C 0x2264:emit(bcLte);B;
			C 0x2F:emit(bcReduce);B;
			C 0x3A:i+=cd(b,d,z-tl,s+tl);B;
			C 0x3B9:emit(bcIota);B;
			C 0x3C1:emit(bcReshape);B;
			C 0x3C3:emit(bcShape);B;
			C 0x2218:emit(bcCallQ);B;
			C 0x2365:emit(bcCallPower);B;
			C 0x2B59:emit(bcSgn);B;
			C 0x2021:emit(bcDup);B;
			C 0x21A7:emit(bcDrop);B;
			C 0x296F:emit(bcSwap);B;
			C 0x2193:emit(bcDip);emit(bcPopRP);B;
			C 0x2B71:emit(bcKeep);B;
			C 0x25B3:emit(bcMax);B;
			C 0x25BD:emit(bcMin);B;
			C 0x2191:emit(bcTake);B;
			C 0x3003:emit(bcReplicate);B;
			default:csw(b,t.c);																}
			B;
		C tW:cw(b,*d,t.s);B;
	}
	R i;																					}
void cmpl(BC *b,WD **d,USZ z,U8B s[z]){for(USZ i=0;i<z;){i+=ct(b,d,z-i,s+i);}}

#define decodei() (*(U32 *)(b->b+i+1))
#define decodef() (*(F64 *)(b->b+i+1))
#define decodep() (*(UIP *)(b->b+i+1))
void dumpbc(BC *b)																			{
	for(USZ i=0;i<b->l;++i)																	{
		printf("%zu\t",i);
		switch(b->b[i])																		{
			C bcPushI:printf("PUSHI %"PRIi32"\n",(I32)decodei());i+=4;B;
			C bcPushF:printf("PUSHF %f\n",decodef());i+=8;B;
			C bcPushS:printf("PUSHS 0x%"PRIxPTR"\n",decodep());i+=szof(UIP);B;
			C bcMkArray:printf("MKARRAY %"PRIu32"\n",decodei());i+=4;B;
			C bcQuot:printf("QUOT %"PRIu32"\n",decodei());i+=4;B;
			C bcJmp:printf("JMP %"PRIu32"\n",decodei());i+=4;B;
			C bcRet:puts("RET");B;
			C bcCall:printf("CALL %"PRIu32"\n",decodei());i+=4;B;
			C bcCallQ:puts("CALLQ");B;
			C bcSwap:puts("SWAP");B;C bcDrop:puts("DROP");B;C bcDup:puts("DUP");B;
			C bcDip:puts("DIP");B;C bcPopRP:puts("POPRP");B;
			C bcIota:puts("IOTA");B;C bcIndex:puts("INDEX");B;
			C bcShape:puts("SHAPE");B;C bcReshape:puts("RESHAPE");B;
			C bcNeg:puts("NEG");B;C bcNot:puts("NOT");B;C bcSgn:puts("SGN");B;
			C bcAdd:puts("ADD");B;C bcMul:puts("MUL");B;C bcSub:puts("SUB");B;
			C bcDiv:puts("DIV");B;C bcMod:puts("MOD");B;
			C bcMin:puts("MIN");B;C bcMax:puts("MAX");B;
			C bcEq:puts("EQ");B;C bcGt:puts("GT");B;C bcLt:puts("LT");B;
			C bcGte:puts("GTE");B;C bcLte:puts("LTE");B;
			C bcReplicate:puts("REPLICATE");B;C bcCat:puts("CAT");B;C bcTake:puts("TAKE");B;
			C bcEach:puts("EACH");B;C bcReduce:puts("REDUCE");B;
			default:puts("?");																}}}
#undef decodei
#undef decodef
#undef decodep

#if AREPL
#include "linenoise.c"
#include "linenoise-utf8.h"
#include "linenoise-utf8.c"

int main(int argc,char**argv)																{
	char*line;
	T t;
	linenoiseSetEncodingFunctions(linenoiseUtf8PrevCharLen,linenoiseUtf8NextCharLen,linenoiseUtf8ReadCode);
	while((line=linenoise("a> ")))															{
		USZ z=strlen(line);
		for(USZ i=0;i<z;)																	{
			i+=nt(&t,z-i,line+i);
			//printf("span %zu\n",i);
			switch(t.t)																		{
				C tW: printf("%.*s\n",slen(t.s),t.s);sfree(t.s);B;
				C tSW: printf("0x%"PRIx32"\n",t.c); B;
				C tI: printf("%"PRIi32"\n",t.i); B;
				C tF: printf("%f\n",t.f); B;
				C tY: printf(".%.*s\n",slen(t.s),t.s);sfree(t.s);B;
				C tS: printf("{%.*s}\n",slen(t.s),t.s);sfree(t.s);B;
				default: putchar('('+(char)t.t);putchar('\n');								}}
		WD *d=NULL;BC *b=bcnew(256);cmpl(b,&d,z,line);
		dumpbc(b);
		bcfree(b);
		linenoiseFree(line);																}
	R 0;																					}
#endif
