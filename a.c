/**
 * a.c - Aré interpreter
 */

#include "a.h"
#include "v.c"
#include "u.c"
#include "c.c"

#include <math.h>

#define ARE_STACK_SIZE 1024

#define ARE_MAX_RSTACK_DEPTH 128
#define AMD ARE_MAX_RSTACK_DEPTH

#define ARE_RPUSH_SIZE 64

#define ARE_NESTED_ADICITY_CALC_DEPTH 16
#define ANACD ARE_NESTED_ADICITY_CALC_DEPTH

#define ARE_MAX_RESULT_ADICITY 16
#define AMRA ARE_MAX_RESULT_ADICITY

// index origin
#define IO 1

V s[ARE_STACK_SIZE];
U32 sp=0;
U32 rs[AMD];
U16 rsp=0;
V rp[ARE_RPUSH_SIZE];
U16 rpp=0;
U8 pucs[ANACD]; // push count stack
U8 pocs[ANACD]; // pop count stack
U8 pcsp=0; // counter for both stacks
jmp_buf ej;

/**
* Saturating subtraction by correcting overflow, gcc and clang generate optimal code from this.
*/
U8 ssub8(U8 a,U8 w){U8 o=a-w;if(o>a){o=0;}R o;}

void ae(E e){longjmp(ej,e);}

void pu(V a){if(sp==ARE_STACK_SIZE){ae(eSO);}s[sp++]=a;++pucs[pcsp];}
V po(void)																					{
	if(sp==0){ae(eSO);}
	pocs[pcsp]+=pucs[pcsp]==0;pucs[pcsp]=ssub8(pucs[pcsp],1);
	R s[--sp];																				}

Q poq(void){V v=po();if(vqp(v)){R v2q(v);}pu(v);ae(eT);R 0;}

void he(E e)																				{
	COND(e, eSO,puts("stack overflow"), eT,puts("type error"), eL,puts("length error"),
	        eR,puts("rank error"), eD,puts("domain error"))									}

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

V iota(V a){if(vt(a)!=vI){ae(eT);}AZ l=v2i(a);A *o=anew(vI,1,&l);DO(l,ai(o)[i]=i+IO)R a2v(o);}
V shp(V a)																					{
	if(vt(a)!=vA){R i2v(1);}
	A *aa=v2a(a);AZ ar=aa->r;A *s=anew(vI,1,&ar);DO(ar,ai(s)[i]=aa->s[i]);R a2v(s);			}
V rshp(V a,V w)																						{
	A *aa,*wa;AZ s[AMR];U8 r;
	if(vip(w)){if(v2i(w)<0){ae(eD);}r=1;s[0]=v2i(w);}
	else if(vap(w)&&(wa=v2a(w))->t==vI)																{
		if(wa->r>1){ae(eR);}if(wa->l>AMR){ae(eD);}r=wa->l;DO(r,s[i]=ai(wa)[i])afree(wa);			}
	else{ae(eT);}
	if(vap(a)){
		aa=v2a(a);
		if(nel(r,s)<=aa->l){ass(aa,r,s);}
		else{
			AZ ol=aa->l;aa=ra(aa,offsetof(A,a)+nel(r,s)*elsz(aa->t));ass(aa,r,s);
			COND(aa->t, vI,DO(aa->l,ai(aa)[i]=ai(aa)[i%ol]), vF,DO(aa->l,af(aa)[i]=af(aa)[i%ol]),
			            vQ,DO(aa->l,aq(aa)[i]=aq(aa)[i%ol]), vY,DO(aa->l,ay(aa)[i]=ay(aa)[i%ol]),
			            vS,DO(aa->l,as(aa)[i]=as(aa)[i%ol]), vD,DO(aa->l,ad(aa)[i]=ad(aa)[i%ol]))	}
		R a2v(aa);																					}
	else{A *oa=anew(vt(a),r,s);DO(nel(r,s),asv(oa,i,a))R a2v(oa);}									}

V idx(V a,V w)																								{
	if(!vap(a)){ae(eT);}A *aa=v2a(a);if(aa->r>1){ae(eR);}
	if(vip(w)){USZ i=v2i(w)-IO;COND(aa->t, vI,R i2v(ai(aa)[i]), vF,R f2v(af(aa)[i]), vQ,R q2v(aq(aa)[i]),
	                                    vY,R y2v(ay(aa)[i]), vS,R s2v(as(aa)[i]), vD,R d2v(ad(aa)[i]))}
	else if(vap(w))																							{
		A *wa=v2a(w),*oa=anew(aa->t,wa->r,wa->s);
		switch(aa->t)																						{
			C vI:DO(wa->l,ai(oa)[i]=ai(aa)[ai(wa)[i]-IO])B;C vF:DO(wa->l,af(oa)[i]=af(aa)[ai(wa)[i]-IO])B;
			C vQ:DO(wa->l,aq(oa)[i]=aq(aa)[ai(wa)[i]-IO])B;C vY:DO(wa->l,ay(oa)[i]=ay(aa)[ai(wa)[i]-IO])B;
			C vS:DO(wa->l,as(oa)[i]=as(aa)[ai(wa)[i]-IO])B;C vD:DO(wa->l,ad(oa)[i]=ad(aa)[ai(wa)[i]-IO])B;	}
		R a2v(oa);																							}
	else{ae(eT);}																							}

V rplct(V a,V w)																			{
	if(!(vap(a)&&vap(w))){ae(eT);}
	A *aa=v2a(a),*wa=v2a(w);chka(aa,wa);if(wa->t!=vI){ae(eT);}if(wa->r>aa->r){ae(eR);}
	AZ wr=wa->s[wa->r-1];AZ rl=0;DO(wr,rl+=ai(wa)[i])
	for(USZ i=rl;i<wa->l;i+=wr){AZ tl=0;DO(wr,tl+=ai(wa)[i])if(tl!=rl){ae(eL);}}
	AZ os[AMR];mc(os,aa->s,szof(AZ)*AMR);os[aa->r-1]=rl;A *oa=anew(aa->t,aa->r,os);
	U8 rd=aa->r-wa->r;USZ ofl=nel(rd,aa->s),ifl=nel(wa->r,aa->s+rd);
	USZ oi=0;
	switch(aa->t)																			{
		C vI:DO(ofl,DO2(ifl,DO3(ai(wa)[j],ai(oa)[oi++]=ai(aa)[j+i*ifl])))B;
		C vF:DO(ofl,DO2(ifl,DO3(ai(wa)[j],af(oa)[oi++]=af(aa)[j+i*ifl])))B;
		C vQ:DO(ofl,DO2(ifl,DO3(ai(wa)[j],aq(oa)[oi++]=aq(aa)[j+i*ifl])))B;
		C vY:DO(ofl,DO2(ifl,DO3(ai(wa)[j],ay(oa)[oi++]=ay(aa)[j+i*ifl])))B;
		C vS:DO(ofl,DO2(ifl,DO3(ai(wa)[j],as(oa)[oi++]=as(aa)[j+i*ifl])))B;
		C vD:DO(ofl,DO2(ifl,DO3(ai(wa)[j],ad(oa)[oi++]=ad(aa)[j+i*ifl])))B;					}
	R a2v(oa);																				}

A *cataa(A *a,A *w)																			{
	AZ s[AMR];mc(s,a->s,szof s);s[a->r-1]+=w->s[w->r-1];A *o=anew(a->t,a->r,s);
	USZ apf=nel(a->r-1,a->s),wpf=nel(w->r-1,w->s),an=a->s[a->r-1],wn=w->s[w->r-1],z=elsz(a->t);
	DO(apf,USZ j=i%wpf;mc(o->a+(an*i+wn*i)*z,a->a+an*i*z,an*z);mc(o->a+(an*i+wn*i+an)*z,w->a+wn*j*z,wn*z))
	afree(a);afree(w);R o;																	}

A *catas(A *a,V w)																			{
	AZ s[AMR];mc(s,a->s,szof s);AZ rl=s[a->r-1],nr=++s[a->r-1];A *o=anew(a->t,a->r,s);USZ z=elsz(a->t);
	DO(nel(a->r-1,s),mc(o->a+nr*i*z,a->a+rl*i*z,rl*z);asv(o,nr*i+rl,w))afree(a);R o;		}
V cat(V a,V w)																				{
	VT at=vt(a),wt=vt(w);A *aa,*wa,*oa;
	if(at==vA&&wt==vA){aa=v2a(a);wa=v2a(w);if(wa->r>aa->r){ae(eR);}oa=cataa(aa,wa);}
	else if(at==vA){aa=v2a(a);if(wt!=aa->t){ae(eT);}oa=catas(aa,w);}
	else{if(at!=wt){ae(eT);}oa=anew(at,1,(AZ[]){2});asv(oa,0,a);asv(oa,1,w);}
	R a2v(oa);																				}

V take(V a,V w)																						{
	if(!vip(w)&&(!vap(w)||v2a(w)->t!=vI)){ae(eT);}A *aa,*oa;
	if(!vap(a))																						{
		AZ s[]={[0 ... AMR-1]=1};U8 r;AZ idx;
		if(vip(w)){I32 wi=v2i(w);r=1;s[0]=abs(wi);idx=wi<0?-wi-IO:0;}
		else																						{
			aa=v2a(w);if(aa->r>1){ae(eR);}if(aa->l>AMR){ae(eD);}
			r=aa->l;DO(r,s[i]=abs(ai(aa)[i]))idx=ai(aa)[r-1]<0?-ai(aa)[r-1]-1:0;
			DO(r-1,idx+=(ai(aa)[r-i-2]<0?-ai(aa)[r-i-2]-1:0)*nel(r-(r-i-1),s+(r-i-1)))				}
		oa=anew(vt(a),r,s);memset(oa->a,0,oa->l*elsz(oa->t));asv(oa,idx,a);afree(aa);R a2v(oa);		}
	else if(vip(w))																					{
		aa=v2a(a);I32 n=v2i(w);if(n==0){ae(eD);}USZ ol=aa->l,z=elsz(aa->t);aa->s[0]=abs(n);aa->l=nel(aa->r,aa->s);
		if(aa->l>ol)																				{
			aa=ra(aa,offsetof(A,a)+nel(aa->r,aa->s)*z);
			if(n<0){memmove(aa->a+(aa->l-ol)*z,aa->a,ol*z);memset(aa->a,0,(aa->l-ol)*z);}
			else{memset(aa->a+ol*z,0,(aa->l-ol)*z);}												}
		else if(n<0){memmove(aa->a,aa->a+(ol-aa->l)*z,aa->l*z);}R a2v(aa);							}
	else																							{
		aa=v2a(a);A *wa=v2a(w);if(wa->r>1){ae(eR);}if(wa->l!=aa->r){ae(eL);}
		AZ s[AMR];U8 r=wa->l;DO(r,s[i]=abs(ai(wa)[i]))oa=anew(aa->t,r,s);
		USZ nra=nel(r-1,aa->s),nrb=nel(r-1,s),rla=aa->s[r-1],rlb=s[r-1],z=elsz(aa->t),off=0;
		//DO(r,off+=abs(ai(wa)[i]))DO(r,off+=ai(wa)[i]<0?ai(wa)[i]:0)memset(oa->a,0,oa->l*z);
		DO(r,if(ai(wa)[i]<0){DO2(r,off+=abs(ai(wa)[j])*(j!=i))})
		DO(min(nra,nrb),mc(oa->a+(off+i*rlb)*z,aa->a+i*rla*z,min(rla,rlb)*z))
		afree(aa);afree(wa);R a2v(oa);																}}

#define r0dc(name,opi,opf)																		\
	V name(V a,V w)																			{	\
	VT at=vt(a),wt=vt(w);A *aa,*wa;I32 wi;F64 wf;												\
	switch((1<<at)|(1<<wt))																	{	\
		C 1<<vI:R i2v(opi(v2i(a),v2i(w)));														\
		C 1<<vF:R f2v(opf(v2f(a),v2f(w)));														\
		C (1<<vA)|(1<<vI):																		\
			if(vap(w)){swap(a,w);}																\
			aa=v2a(a);wi=v2i(w);DO(aa->l,ai(aa)[i]=opi(ai(aa)[i],wi))R a;						\
		C (1<<vA)|(1<<vF):																		\
			if(vap(w)){swap(a,w);}																\
			aa=v2a(a);wf=v2f(w);DO(aa->l,af(aa)[i]=opf(af(aa)[i],wf))R a;						\
		C 1<<vA:																				\
			aa=v2a(a);wa=v2a(w);																\
			if(aa->t!=wa->t){ae(eT);}chka(aa,wa);if(aa->r<wa->r){swap(a,w);swap(aa,wa);}		\
			if(aa->t==vI)																	{	\
				U8 rd=aa->r-wa->r;if(rd==0){DO(aa->l,ai(aa)[i]=opi(ai(aa)[i],ai(wa)[i]))goto done;}	\
				USZ ofl=nel(rd,aa->s),ifl=nel(wa->r,aa->s+rd);									\
				DO(ofl,DO2(ifl,ai(aa)[j+i*ifl]=opi(ai(aa)[j+i*ifl],ai(wa)[j])))				}	\
			else if(aa->t==vF)																{	\
				U8 rd=aa->r-wa->r;if(rd==0){DO(aa->l,af(aa)[i]=opf(af(aa)[i],af(wa)[i]))goto done;}	\
				USZ ofl=nel(rd,aa->s),ifl=nel(wa->r,aa->s+rd);									\
				DO(ofl,DO2(ifl,af(aa)[j+i*ifl]=opf(af(aa)[j+i*ifl],af(wa)[j])))				}	\
			else{ae(eT);}																		\
			done:afree(wa);R a;																	\
		default:ae(eT);																		}}

#define r0dnc(name,opi,opf)																		\
	V name(V a,V w)																			{	\
	VT at=vt(a),wt=vt(w);A *aa,*wa,*lg,*sm;V lgv,smv;I32 ii;F64 f;								\
	switch((1<<at)|(1<<wt))																	{	\
		C 1<<vI:R i2v(opi(v2i(a),v2i(w)));														\
		C 1<<vF:R f2v(opf(v2f(a),v2f(w)));														\
		C (1<<vA)|(1<<vI):																		\
			if(vap(w)){wa=v2a(w);ii=v2i(a);DO(wa->l,ai(wa)[i]=opi(ii,ai(wa)[i]))R w;}			\
			aa=v2a(a);ii=v2i(w);DO(aa->l,ai(aa)[i]=opi(ai(aa)[i],ii))R a;						\
		C (1<<vA)|(1<<vF):																		\
			if(vap(w)){wa=v2a(w);f=v2f(a);DO(wa->l,ai(wa)[i]=opf(f,ai(wa)[i]))R w;}				\
			aa=v2a(a);f=v2f(w);DO(aa->l,af(aa)[i]=opf(af(aa)[i],f))R a;							\
		C 1<<vA:																				\
			lg=aa=v2a(lgv=a);sm=wa=v2a(smv=w);													\
			if(aa->t!=wa->t){ae(eT);}chka(aa,wa);if(aa->r<wa->r){swap(lgv,smv);swap(lg,sm);}	\
			if(aa->t==vI)																	{	\
				U8 rd=lg->r-sm->r;if(rd==0){DO(aa->l,ai(aa)[i]=opi(ai(aa)[i],ai(wa)[i]))goto done;}	\
				USZ ofl=nel(rd,lg->s),ifl=nel(sm->r,lg->s+rd);									\
				if(lg==aa){DO(ofl,DO2(ifl,ai(aa)[j+i*ifl]=opi(ai(aa)[j+i*ifl],ai(wa)[j])))}		\
				else{DO(ofl,DO2(ifl,ai(wa)[j+i*ifl]=opi(ai(aa)[j],ai(wa)[j+i*ifl])))}		}	\
			else if(aa->t==vF)																{	\
				U8 rd=lg->r-sm->r;if(rd==0){DO(aa->l,af(aa)[i]=opf(af(aa)[i],af(wa)[i]))goto done;}	\
				USZ ofl=nel(rd,aa->s),ifl=nel(wa->r,aa->s+rd);									\
				if(lg==aa){DO(ofl,DO2(ifl,af(aa)[j+i*ifl]=opf(af(aa)[j+i*ifl],af(wa)[j])))}		\
				else{DO(ofl,DO2(ifl,af(wa)[j+i*ifl]=opf(af(aa)[j],af(wa)[j+i*ifl])))}		}	\
			else{ae(eT);}																		\
			done:afree(sm);R lgv;																\
		default:ae(eT);																		}}

#define opp(x,y) ((x)+(y))
r0dc(add,opp,opp)
#define opm(x,y) ((x)*(y))
r0dc(mul,opm,opm)
r0dc(imin,min,min)
r0dc(imax,max,max)
#define ops(x,y) ((x)-(y))
r0dnc(sub,ops,ops)
#define opd(x,y) ((x)/(y))
r0dnc(idiv,opd,opd)
#define opr(x,y) ((x)%(y))
r0dnc(mod,opr,fmod)

#define opeq(x,y) ((x)==(y))
r0dc(eq,opeq,opeq)
#define opgt(x,y) ((x)>(y))
r0dnc(gt,opgt,opgt)
#define oplt(x,y) ((x)<(y))
r0dnc(lt,oplt,oplt)
#define opgte(x,y) ((x)>=(y))
r0dnc(gte,opgte,opgte)
#define oplte(x,y) ((x)<=(y))
r0dnc(lte,oplte,oplte)

#define liftr0mnum(iop,fop) \
	switch(aa->t){C vI:DO(aa->l,ai(aa)[i]=iop(ai(aa)[i]))B;C vF:DO(aa->l,af(aa)[i]=fop(af(aa)[i]))B;default:ae(eT);}

#define opneg(a) -(a)
V neg(V a)																					{
	A *aa;
	switch(vt(a))																			{
		C vI:R i2v(-v2i(a));C vF:R f2v(-v2f(a));
		C vA:aa=v2a(a);liftr0mnum(opneg,opneg)R a;
		default:ae(eT);																		}}

#define opnoti(a) CONDE((a)==0,1,0)
#define opnotf(a) CONDE((a)==0.0,1.0,0.0)
V not(V a)																					{
	A *aa;
	switch(vt(a)){
		//C vI:R CONDE(v2i(a)==0,i2v(1),i2v(0));C vF:R CONDE(v2f(a)==0.0,f2v(1.0),f2v(0.0));
		C vI:R i2v(opnoti(v2i(a)));C vF:R f2v(opnotf(v2f(a)));
		C vA:aa=v2a(a);liftr0mnum(opnoti,opnotf)R a;
		default:ae(eT);																		}}

#define opsgnf(a) CONDE((a)>0.0,1.0,(a)<0.0,-1.0,0.0)
V isgn(V a)																					{
	A *aa;
	switch(vt(a))																			{
		C vI:R i2v(sgn(v2i(a)));C vF:R f2v(opsgnf(v2f(a)));
		C vA:aa=v2a(a);liftr0mnum(sgn,opsgnf)R a;
		default:ae(eT);																		}}

void eval(BC *bc,U32 pc);
// nested eval, handle setting and restoring the error jump location
void nevq(BC *bc,Q q)																		{
	assert(rsp<AMD);jmp_buf oe;mc(&oe,&ej,sizeof(jmp_buf)); // not guaranteed to work
	rs[rsp++]=bc->l;eval(bc,q);mc(&ej,&oe,sizeof(jmp_buf));									}

void ego(BC *bc,Q q){++pcsp;assert(pcsp<ANACD);nevq(bc,q);}
void egc(void){pocs[pcsp]=pucs[pcsp]=0;--pcsp;}
#define weg(bc,q,f) ego(bc,q);f;egc();
void cqa(BC *bc,A *q)																		{
	V h[ARE_STACK_SIZE];U32 hsp=sp;DO(sp,h[i]=cv(s[i]))Q fst=aq(q)[0];weg(bc,fst,U8 ad=pocs[pcsp])
	DO(q->l-1,DO2(ad,pu(i==i_-1?h[hsp-ad+j]:cv(h[hsp-ad+j])))nevq(bc,aq(q)[i+1]))			
	DO(hsp-ad,vfree(h[i]))																	}

void callpwr(BC *bc,I32 n,Q q){if(n<0){ae(eD);}DO(n,nevq(bc,q))}
void callwhile(BC *bc,Q c,Q q)																{
	V hl[AMRA],ht[AMRA],cr;weg(bc,q,U8 ra=pucs[pcsp])DO(ra,hl[ra-1-i]=cv(s[sp-1-i]))
	while(1)																				{
		nevq(bc,q);DO(ra,ht[ra-1-i]=po())DO(ra,pu(hl[i]))DO(ra,pu(cv(ht[i])))mc(hl,ht,szof hl);
		nevq(bc,c);cr=po();DO(ra,pu(cv(ht[i])))if(!vip(cr)){ae(eT);}if(v2i(cr)!=0){B;}		}
	DO(ra,vfree(hl[i]))																		}

#define dord(ax,x2v,v2x)DO(l,pu(x2v(ax(a)[i*rl]));DO2(rl-1,pu(x2v(ax(a)[j+1+i*rl]));nevq(bc,q))ax(o)[i]=v2x(po()))
A *rd(BC *bc,Q q,A *a)																		{
	if(a->l<1){ae(eL);}
	A *o=a->r==1?anew(a->t,1,(AZ[1]){1}):anew(a->t,a->r-1,a->s);USZ l=o->l,rl=a->s[a->r-1];
	COND(a->t, vI,dord(ai,i2v,v2i), vF,dord(af,f2v,v2f), vQ,dord(aq,q2v,v2q), vY,dord(ay,y2v,v2y),
	           vS,dord(as,s2v,v2s), vD,dord(ad,d2v,v2d))
	R o;																					}

#define di() (*(U32 *)(b+pc+1))
#define df() (*(F64 *)(b+pc+1))
#define dp() (*(UIP *)(b+pc+1))
#define call(inc,p) do{assert(rsp<AMD);rs[rsp++]=pc+inc;pc=(p);}while(0)
#define mo(bc,fn) C bc:pu(fn(po()));++pc;B;
#define dy(bc,fn) C bc:w=po();a=po();pu(fn(a,w));++pc;B;
void eval(BC *bc,U32 pc)																	{
	U32 i;E e;Q q;U8 *b=bc->b;V a,w;
	if((e=setjmp(ej))){he(e);}
	else while(pc<bc->l)switch(b[pc])														{
		C bcPushI:pu(i2v((I32)di()));pc+=5;B;
		C bcPushF:pu(f2v(df()));pc+=9;B;
		C bcPushS:pu(s2v((S)dp()));pc+=1+szof(UIP);B;
		C bcMkArray:mka(di());pc+=5;B;
		C bcQuot:pu(q2v(pc+5));pc+=5+di();B;
		C bcJmp:pc+=5+di();B;
		C bcRet:assert(rsp>0);pc=rs[--rsp];B;
		C bcCall:call(5,di());B;
		C bcCallQ:a=po();COND(vt(a), vQ,call(1,v2q(a)), vA,cqa(bc,v2a(a));++pc, ae(eT));B;
		C bcCallPower:a=po();q=poq();COND(vt(a), vI,callpwr(bc,v2i(a),q), vQ,callwhile(bc,v2q(a),q), ae(eT))++pc;B;
		C bcDrop:po();++pc;B;
		C bcSwap:a=po();w=po();pu(a);pu(w);++pc;B;
		C bcDup:pu(cv(s[sp-1]));++pc;B;
		C bcDip:q=poq();assert(rpp<ARE_RPUSH_SIZE);rp[rpp++]=po();call(1,q);B;
		C bcPopRP:assert(rpp>0);pu(rp[--rpp]);++pc;B;
		C bcIota:pu(iota(po()));++pc;B;
		C bcReduce:q=poq();a=po();if(!vap(a)){ae(eT);}pu(a2v(rd(bc,q,v2a(a))));++pc;B;
		mo(bcNeg,neg)mo(bcNot,not)mo(bcSgn,isgn)
		mo(bcShape,shp)dy(bcReshape,rshp)
		dy(bcIndex,idx)dy(bcReplicate,rplct)dy(bcCat,cat)dy(bcTake,take)
		dy(bcAdd,add)dy(bcMul,mul)dy(bcSub,sub)dy(bcDiv,idiv)dy(bcMod,mod)
		dy(bcMin,imin)dy(bcMax,imax)
		dy(bcEq,eq)dy(bcGt,gt)dy(bcLt,lt)dy(bcGte,gte)dy(bcLte,lte)
		default:puts("unimplemented opcode");++pc;											}}

#if USE_LINENOISE
#include "linenoise.c"
#include "linenoise-utf8.h"
#include "linenoise-utf8.c"
#endif

int main(int argc, const char **argv)														{
	BC *b=bcnew(256);
	WD *d=NULL;
	U32 obl=0;
	// uncomment to get some default arrays (3x3 and 3x3x3) for testing
	/*A *mat=anew(vI,2,(AZ[2]){3,3});
	DO(9,ai(mat)[i]=i+1.0)
	pu(a2v(mat));
	A *ddd=anew(vI,3,(AZ[3]){3,3,3});
	DO(27,ai(ddd)[i]=i+1.0)
	pu(a2v(ddd));*/

#if USE_LINENOISE
	char *ln;
	linenoiseSetEncodingFunctions(linenoiseUtf8PrevCharLen,linenoiseUtf8NextCharLen,linenoiseUtf8ReadCode);
	while((ln=linenoise("are> ")))															
#else
	char ln[128];
	printf("are> ");
	while(fgets(ln,szof ln,stdin))
#endif
	{
		cmpl(b,&d,strlen(ln),ln);
		//dumpbc(b);
		eval(b,obl);
		obl=b->l;
		for(U32 i=sp;i>0;--i){pv(s[i-1]);putchar('\n');}
#if USE_LINENOISE
		linenoiseFree(ln);
#else
		printf("are> ");
#endif
																		}}
