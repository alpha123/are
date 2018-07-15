/**
 * a.c - Aré interpreter
 */

#include "a.h"
#include "v.c"
#include "u.c"
#include "c.c"

//#include <sys/mman.h>
//
//#define PAGE_SIZE 4096

#define ARE_STACK_SIZE 1024

#define ARE_MAX_RSTACK_DEPTH 128
#define AMD ARE_MAX_RSTACK_DEPTH

#define ARE_RPUSH_SIZE 64

V s[ARE_STACK_SIZE];
U32 sp=0;
U32 rs[AMD];
U16 rsp=0;
V rp[ARE_RPUSH_SIZE];
U16 rpp=0;
jmp_buf ej;

void ae(E e){longjmp(ej,e);}

void pu(V a)																				{
	if(sp==ARE_STACK_SIZE){ae(eSO);}
	s[sp++]=a;																				}
V po(void)																					{
	if(sp==0){ae(eSO);}
	R s[--sp];																				}

Q poq(void){V v=po();if(vqp(v)){R v2q(v);}pu(v);ae(eT);R 0;}

void he(E e){COND(e, eSO,puts("stack overflow"), eT,puts("type error"), eL,puts("length error"))}

void chka(A *a,A *w){U8 pfl=min(a->r,w->r);DO(pfl,if(a->s[i]!=w->s[i]){ae(eL);})}

// array set value
void asv(A *a,AZ i,V v)																		{
	if(vt(v)!=a->t){he(eT);}
	COND(a->t, vI,ai(a)[i]=v2i(v), vF,af(a)[i]=v2f(v), vQ,aq(a)[i]=v2q(v),
	     vY,ay(a)[i]=v2y(v), vS,as(a)[i]=v2s(v), vD,ad(a)[i]=v2d(v))						}
void mka(U32 n)																				{
	assert(n>0);
	AZ s=n;V fst=po();VT t=vt(fst);--n;A *a=anew(t,1,&s);
	asv(a,n,fst);DO(n,asv(a,n-i-1,po()))pu(a2v(a));											}

V add(V a,V w)																				{
	VT at=vt(a),wt=vt(w);A *aa,*wa;I32 wi;F64 wf;
	switch((1<<at)|(1<<wt))																	{
		C 1<<vI:R i2v(v2i(a)+v2i(w));
		C 1<<vF:R f2v(v2f(a)+v2f(w));
		C (1<<vA)|(1<<vI):
			if(vap(w)){swap(a,w);}
			aa=v2a(a);wi=v2i(w);DO(aa->l,ai(aa)[i]+=wi)R a;
		C (1<<vA)|(1<<vF):
			if(vap(w)){swap(a,w);}
			aa=v2a(a);wf=v2f(w);DO(aa->l,af(aa)[i]+=wf)R a;
		C 1<<vA:
			aa=v2a(a);wa=v2a(w);
			if(aa->t!=wa->t){ae(eT);}chka(aa,wa);if(aa->r<wa->r){swap(a,w);swap(aa,wa);}
			if(aa->t==vI)																	{
				U8 rd=aa->r-wa->r;if(rd==0){DO(aa->l,ai(aa)[i]+=ai(wa)[i])R a;}
				USZ ofl=nel(rd,aa->s),ifl=nel(wa->r,aa->s+rd);
				DO(ofl,DO2(ifl,ai(aa)[j+i*ifl]+=ai(wa)[j]))									}
			else if(aa->t==vF)																{
				U8 rd=aa->r-wa->r;if(rd==0){DO(aa->l,af(aa)[i]+=af(wa)[i])R a;}
				USZ ofl=nel(rd,aa->s),ifl=nel(wa->r,aa->s+rd);
				DO(ofl,DO2(ifl,af(aa)[j+i*ifl]+=af(wa)[j]))									}
			else{ae(eT);}
			R a;
		default:ae(eT);																		}}

#define di() (*(U32 *)(b+pc+1))
#define df() (*(F64 *)(b+pc+1))
#define call(inc,p) do{assert(rsp<AMD);rs[rsp++]=pc+inc;pc=(p);}while(0)
void eval(BC *bc,U32 pc)																	{
	U32 i;E e;Q q;U8 *b=bc->b;V a,w;
	if((e=setjmp(ej))){he(e);}
	else while(pc<bc->l)switch(b[pc])														{
		C bcPushI:pu(i2v((I32)di()));pc+=5;B;
		C bcPushF:pu(f2v(df()));pc+=9;B;
		C bcMkArray:mka(di());pc+=5;B;
		C bcQuot:pu(q2v(pc+5));pc+=5+di();B;
		C bcJmp:pc+=5+di();B;
		C bcRet:assert(rsp>0);pc=rs[--rsp];B;
		C bcCall:call(5,di());B;
		C bcCallQ:call(1,poq());B;
		C bcDip:q=poq();assert(rpp<ARE_RPUSH_SIZE);rp[rpp++]=po();call(1,q);B;
		C bcPopRP:assert(rpp>0);pu(rp[--rpp]);++pc;B;
		C bcAdd:w=po();a=po();pu(add(a,w));++pc;B;
		default:puts("unimplemented opcode");++pc;											}}

//void initstack(void){
//	s=mmap();
//	sp=0;
//}

#include "linenoise.c"
#include "linenoise-utf8.h"
#include "linenoise-utf8.c"

int main(int argc, const char **argv)														{
	char *ln;
	BC *b=bcnew(256);
	WD *d=NULL;
	U32 obl=0;
	linenoiseSetEncodingFunctions(linenoiseUtf8PrevCharLen,linenoiseUtf8NextCharLen,linenoiseUtf8ReadCode);
	// uncomment to get some default arrays (3x3 and 3x3x3) for testing
	/*A *mat=anew(vI,2,(AZ[2]){3,3});
	DO(9,ai(mat)[i]=i+1.0)
	pu(a2v(mat));
	A *ddd=anew(vI,3,(AZ[3]){3,3,3});
	DO(27,ai(ddd)[i]=i+1.0)
	pu(a2v(ddd));*/
	while((ln=linenoise("are> ")))															{
		cmpl(b,&d,strlen(ln),ln);
		dumpbc(b);
		eval(b,obl);
		obl=b->l;
		for(U32 i=sp;i>0;--i){pv(s[i-1]);putchar('\n');}
		linenoiseFree(ln);																	}}
