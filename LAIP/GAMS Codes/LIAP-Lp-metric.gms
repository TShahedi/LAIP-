$inlineCom [ ]
$eolCom //
$title example model definitions
$funcLibin stolib stodclib
Function cdfnorm  /stolib.cdfnormal/
         icdfnorm  /stolib.icdfnormal/;
************************** LAIP ************************************************
*-*-*-*-*-*-*-*-*-*-*-* Set *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
sets
    i               index of suppliers /1*3/
    s               index of sellers /1*6/
    str(s)          index of traditional sellers  /1*3/
    son(s)          index of online sellers  /4*6/
    k               index for potential locations for distribution centers /1*4/
    j               index of plants /1*4/
    t               index of period time  /1*3/
**  G               Set of permissible review intervals /1*3/
    iter iteration /1*25/
    alias(s,l)
    ;
*-*-*-*-*-*-*-*-*-*-*-* scalar*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
scalars
    Landa           conversion coefficient of raw material to product/0.4/
    M               A big number/1000/
    N               Min units of raw materials produced by supplier i and sent to plant j in period t /5/
;
scalars
    SS1             Safety Stock
    SSH1            Shortage
;
*-*-*-*-*-*-*-*-*-*-*-* parameters *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
parameter
    ww /-0.0416/
    result(iter,*)
    ;

parameter
    R(k)            Review interval at distribution center k
    ;
    loop((k),
    R(k)=round(uniform(1,2));
    );
parameter
    alpha(k)        service level at distribution center k
    ;
    loop((k),
    alpha(k)=uniform(0.2,0.5);
    );
parameter
    ZRk(k)          'Value of the accumulated standard normal distribution such that P(Z<ZRk(k))=alpha(k)'
    ;
    loop((k),
    ZRk(k)=cdfnorm(alpha(k),0,1);
    );
parameter
    phiZRk(k)       standard normal cumulative distribution function
    ;
    loop((k),
    phiZRk(k)=icdfnorm(ZRk(k),0,1);
    );
parameter
    teta(i)         Defective probability of raw material provided from supplier i
    ;
    loop((i),
    teta(i)=uniform(0.1,0.5);
    );
parameters
    zi(t)           Inflation rate in period t
    YY(t)           Number of distribution centers that can be opened in period t
    ;
    loop((t),
    zi(t)=uniform(0.1,0.5);
    YY(t)=round(uniform(1,3));
    );
parameter
    rou(s,l,t)      Correlation coefficient between demands at seller s and at seller l in period t
    ;
    loop((s,l,t),
    if ((ord(s)ne ord(l)),
    rou(s,l,t)=uniform(0,1);
    );
    );
*-*-*-*-*-*-*-*-*-*-*-*-*-*Stochastic Part*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
parameters
    Miu(s,t)        Average demand of seller s in period t
    Sigma2(s,t)     Variance in demand of seller s in period t
    ;
    loop((s,t),
    Miu(s,t)=uniform(1,7);
    Sigma2(s,t)=uniform(1,2);
    );
*-*-*-*-*-*-*-*-*-*-*-* Capacity *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
parameter
   CS(i,t)          Capacity of supplier i in period t
   ;
   loop((i,t),
   CS(i,t)=round(uniform(20,26));
   );
parameter
   CD(k,t)          Capacity of distribution center k in period t
   ;
   loop((k,t),
   CD(k,t)=round(uniform(80,225));
   );
parameter
   CP(j,t)          Capacity of plant j in period t
   ;
   loop((j,t),
   CP(j,t)=round(uniform(80,225));
   );
parameter
   Captr(str,t)     Cpacity of traditional seller str in period t
   ;
   loop((str,t),
   Captr(str,t)=round(uniform(22,25));
   );
parameter
   Capon(son,t)     Capacity of online seller son in period t
   ;
   loop((son,t),
   Capon(son,t)=round(uniform(22,25));
   );
*-*-*-*-*-*-*-*-*-*-*-*-* Fixed-Cost *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
parameter
   FCd(k,t)         Fixed cost of opening distribution center k in period t
   ;
   loop((k,t),
   FCd(k,t)=round(uniform(2,5));
   );
parameter
   FCs(str,t)       Fixed cost of opening traditional seller str in period t
   ;
   loop((str,t),
   FCs(str,t)=round(uniform(2,5));
   );
parameter
   FC(j,k,t)        Fixed cost of a shipment between plant j and distribution center k in period t
   ;
   loop((j,k,t),
   FC(j,k,t)=round(uniform(2,5));
   );
parameter
   TPfs(k,s,t)      Fixed cost of a shipment between distribution center k and seller s in period t
   ;
   loop((k,s,t),
   TPfs(k,s,t)=round(uniform(2,5));
   );
parameter
   OCp(i,j,t)       Fix ordering cost from supplier i to plant j in period t
   ;
   loop((i,j,t),
   OCp(i,j,t)=round(uniform(1,2));
   );
parameter
   OCd(j,k,t)       Fix ordering cost from plant j to distribution center k in period t
   ;
   loop((j,k,t),
   OCd(j,k,t)=round(uniform(2,5));
   );
*-*-*-*-*-*-*-*-*-*-*-* Holding Cost *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
parameter
   HCp(j,t)         Inventory holding cost per unit at plant j in period t
   ;
   loop((j,t),
   HCp(j,t)=round(uniform(3,4.5));
   );
parameter
   HCd(k,t)         Inventory holding cost per unit at distribution center k in period t
   ;
   loop((k,t),
   HCd(k,t)=round(uniform(3,4.5));
   );
parameter
   HCst(str,t)      Inventory holding cost per unit at traditional seller str in period t
   ;
   loop((str,t),
   HCst(str,t)=round(uniform(3,4.5));
   );
parameter
   HCso(son,t)      Inventory holding cost per unit at online seller s^on in period t
   ;
   loop((son,t),
   HCso(son,t)=round(uniform(3,5));
   );
*-*-*-*-*-*-*-*-*-* Transportation Cost (volume-dependent) *-*-*-*-*-*-*-*-*-*-*
parameter
   TC(i,j,t)        Transportation cost (volume-dependent) per unit raw material per each transferred unit from supplier i to plant j in period t
   ;
   loop((i,j,t),
   TC(i,j,t)=uniform(0.05,0.1);
   );
parameter
   TPp(j,k,t)       Transportation cost (volume-dependent) per unit from plant j to distribution center k in period t
   ;
   loop((j,k,t),
   TPp(j,k,t)=uniform(0.05,1);
   );
parameter
   TPd(k,s,t)       Transportation cost (volume-dependent) per unit from distribution center k to seller s in period t
   ;
   loop((k,s,t),
   TPd(k,s,t)=uniform(0.05,1);
   );
parameter
   TPdtr(k,str,t)   Transportation cost (volume-dependent) per unit from distribution center k to traditional seller str in period t
   ;
   loop((k,str,t),
   TPdtr(k,str,t)=uniform(0.05,1);
   );
parameter
   TPdon(k,son,t)   Transportation cost (volume-dependent) per unit from distribution center k to online seller son in period t
   ;
   loop((k,son,t),
   TPdon(k,son,t)=uniform(0.05,1);
   );
*-*-*-*-*-*-*-*-*-*-*-* Shortage Cost *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
parameter
   Pai(k,t)         Shortage cost per unit short at distribution center k in period t
   ;
   loop((k,t),
   Pai(k,t)=uniform(5,6);
   );
*-*-*-*-*-*-*-*-*-*-*-* Selling price *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
parameter
   Pai0(k,t)        Unit selling price at distribution center k in period t
   ;
   loop((k,t),
   Pai0(k,t)=uniform(2,3);
   );
*-*-*-*-*-*-*-*-*-*-*-* Purchase cost *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
parameter
   UC(i,j,t)        Purchase cost per unit from supplier i in period t
   ;
   loop((i,j,t),
   UC(i,j,t)=uniform(1,2);
   );
*-*-*-*-*-*-*-*-*-*-*-* Lead time *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
parameter
   LT(j,t)          Maximum lead time from selected suppliers to plant j in period t
   ;
   loop((j,t),
   LT(j,t)=round(uniform(8,15));
   );
parameter
   LTt(s,t)         Maximum lead time from distribution centers to seller s in period t
   ;
   loop((s,t),
   LTt(s,t)=round(uniform(8,15));
   );
parameter
   LTd(j,k,t)       Lead time from plant j to distribution center k in period t
   ;
   loop((j,k,t),
   LTd(j,k,t)=round(uniform(1,2));
   );
parameter
   MaxLTd(k);
   MaxLTd(k)=Smax((j,t),LTd(j,k,t));
parameter
   LTp(i,j,t)       Lead time from supplier i to plant j in period t
   ;
   loop((i,j,t),
   LTp(i,j,t)=round(uniform(1,2));
   );
parameter
   LTs(k,s,t)       Lead time from distribution center k to seller s in period t
   ;
   loop((k,s,t),
   LTs(k,s,t)=round(uniform(1,2));
   );
*-*-*-*-*-*-*-*-*-*-*-* Environmental impact *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
parameter
   EIS(i,j,t)       Environmental impacts per each transferred unit from supplier i to plant j in period t
   ;
   loop((i,j,t),
   EIS(i,j,t)=uniform(0.002,0.005);
   );
parameter
   EIP(j,k,t)       Environmental impacts per each transferred unit from the plant j to distribution center k in period t
   ;
   loop((j,k,t),
   EIP(j,k,t)=uniform(0.4,0.5);
   );
parameter
   EId(k,str,t)     Environmental impacts per each transferred unit from distribution center k to traditional seller str in period t
   ;
   loop((k,str,t),
   EId(k,str,t)=uniform(0.4,0.5);
   );
parameter
   EIds(k,son,t)    Environmental impacts per each transferred unit from distribution center k to online seller son in period t
   ;
   loop((k,son,t),
   EIds(k,son,t)=uniform(0.4,0.5);
   );
parameter
   EIod(k,t)        Environmental impacts associated with opening distribution center k in period t
   ;
   loop((k,t),
   EIod(k,t)=uniform(0.4,0.5);
   );
parameter
   EIos(str,t)      Environmental impacts associated with opening traditional seller str in period t
   ;
   loop((str,t),
   EIos(str,t)=uniform(0.4,0.5);
   );
*-*-*-*-*-*-*-*-*-*-*-* initial inventory *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

parameters
   II0(j)          initial inventory of plant j in period t
   B0(k)           initial inventory of distribution center k in period t
   HH0(str)        initial inventory of traditional seller s^tr in period t
   Hs0(son)        initial inventory of online seller s^on in period t
   ;
   loop((j),
   II0(j)=uniform(1,5);
   );
   loop((k),
   B0(k)=uniform(1,5);
   );
   loop((str),
   HH0(str)=uniform(2,8);
   );
   loop((son),
   Hs0(son)=uniform(2,8);
   );
*-*-*-*-*-*-*-*-*-*-*-* Auxiliary Variables *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
variables
         Zf
         Z1
         Z2
   positive variables
         Miup(j,t)           Mean demand assigned to plant j in period t
         Miud(k,t)           Mean demand assigned to distribution center k in period t
         Sigma2p(j,t)        Variance in demand assigned to plant j in period t
         Sigma2d(k,t)        Variance in demand assigned to distribution center k in period t
         II(j,t)             End of period inventory of plant j in period t
         B(k,t)              End of period inventory of distribution center k in period t
         HH(str,t)           End of period inventory of traditional seller s^tr in period t
         Hs(son,t)           End of period inventory of online seller s^on in period t
         ss(k,t)             Safety stock level at distribution center k in period t
   integer variables
         Ld(k)               Order lead-time at distribution center k
         Lp(j)               Order lead-time at plant j
         Ls(s)               Order lead-time at seller s
   Binary variables
         rr(i,j,t)            'Binary variable indicating if supplier i serves plant j in period t, in which case it has a value of 1, It is 0 otherwise'

*-*-*-*-*-*-*-*-*-*-*-* Decision Variables *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

   positive variables

         bb(k,t)             Fraction of demand back-ordered during shortage in period t at distribution center k in period t
         Beta(i,j,t)         Number of raw material units produced by supplier i and sent to plant j in period t
         F(j,k,t)            Number of product that plant j sent to distribution center k in period t
         Q(k,str,t)          Number of product that distribution center k sent to traditional seller s^tr in period t
         Qs(k,son,t)         Number of product that distribution center k sent to online seller s^on in period t
    Binary variables
         ZZ(j,k,t)           '1 if plant j serves distribution center k in period t; 0 otherwise'
         w(k,s,t)            '1 if distribution center k serves seller s in period t; 0 otherwise'
         y(k,t)              '1 if distribution center k is open in period t; 0 otherwise'
         x(str,t)            '1 if traditional seller s^tr open in period t; 0 otherwise'
         v(son,t)            '1 if online seller s^on select in period t; 0 otherwise'
;

*-*-*-*-*-*-*-*-*-*-*-* Equations *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
Equations
         obj1
         obj2
         objf

         co1(s,t)
         co2(str,t)
         co3(son,t)
         co4(str,t)
         eq4(str)
         co5(son,t)
         eq5(son)
         co6(str,t)
         eq6(str)
         co7(son,t)
         eq7(son)
         co8(k,t)
         co9(k,t)
         co10(j,t)
         co11(j,t)
         co12(k,t)
         eq12(k,t)
         co13(t)
         co14(k,t)
         eq14(k)
         co15(j,t)
         eq15(j)
         co16(j,t)
         eq16(j)
         co17(i,t)
         co18(i,j,t)
*         eq18(i,j,t)
         co19(j,t)
         co20(s,t)
         co21(k,t)
         co22(k,t)
         co23(k,t)
;
*-*-*-*-*-*-*-*-*-*-*-* Objective functions *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

 obj1    .. Z1 =e=sum((i,j,t),TC(i,j,t)*Beta(i,j,t)*(1+zi(t)))+
                   sum((i,j,t),OCp(i,j,t)*rr(i,j,t)*(1+zi(t)))+
                   sum((i,j,t),UC(i,j,t)*Beta(i,j,t)*(1+zi(t))*(1-teta(i)))+
                   sum((k,t),FCd(k,t)*y(k,t)*(1+zi(t)))+
                   sum((j,k,t),(OCd(j,k,t)+FC(j,k,t)/R(k))*ZZ(j,k,t)*(1+zi(t)))+
                   sum((j,k,t),F(j,k,t)*TPp(j,k,t)*ZZ(j,k,t)*(1+zi(t)))+
                   sum((j,k,t),HCp(j,t)*II(j,t)*ZZ(j,k,t)*(1+zi(t)))+
                   sum((k,t),HCd(k,t)*(R(k)*Miud(k,t)/2)*(1+zi(t)))+
                   sum((k,t),HCd(k,t)*ZRk(k)*((sum((s,l)$(not sameas(s,l)),rou(s,l,t)*(Sigma2(s,t)**0.5)*(Sigma2(l,t)**0.5)*w(k,s,t)*w(k,l,t)*sum(j,(R(k)+LTd(j,k,t))*ZZ(j,k,t)))**0.5)*(1+zi(t))))+
                   sum((k,t),((Pai(k,t)+(1-bb(k,t))*(HCd(k,t)*R(k)+Pai0(k,t)-sum(s,TPd(k,s,t))))/R(k))*(phiZRk(k))*(((sum((s,l)$(not sameas(s,l)),rou(s,l,t)*(Sigma2(s,t)**0.5)*(Sigma2(l,t)**0.5)*w(k,s,t)*w(k,l,t)*sum(j,(R(k)+LTd(j,k,t))*ZZ(j,k,t)))**0.5)*(1+zi(t)))))+
                   sum((str,t),FCs(str,t)*x(str,t)*(1+zi(t)))+
                   sum((k,str,t),TPdtr(k,str,t)*Q(k,str,t)*(1+zi(t)))+
                   sum((k,son,t),TPdon(k,son,t)*Qs(k,son,t)*(1+zi(t)))+
                   sum((k,str,t),HCst(str,t)*HH(str,t)*w(k,str,t)*(1+zi(t)))+
                   sum((k,son,t),HCso(son,t)*Hs(son,t)*w(k,son,t)*(1+zi(t)))
                   ;


obj2    ..  Z2=e=sum((i,j,t),EIS(i,j,t)*Beta(i,j,t))+sum((j,k,t),EIP(j,k,t)*F(j,k,t))+
                   sum((k,t),EIod(k,t)*y(k,t))+sum((k,str,t),EId(k,str,t)*Q(k,str,t))+
                   sum((k,son,t),EIds(k,son,t)*Qs(k,son,t))+sum((str,t),EIos(str,t)*x(str,t))
                   ;

objf    ..  Zf=e=ww*(Z1.l-(sum((i,j,t),TC(i,j,t)*Beta(i,j,t)*(1+zi(t)))+
                   sum((i,j,t),OCp(i,j,t)*rr(i,j,t)*(1+zi(t)))+
                   sum((i,j,t),UC(i,j,t)*Beta(i,j,t)*(1+zi(t))*(1-teta(i)))+
                   sum((k,t),FCd(k,t)*y(k,t)*(1+zi(t)))+
                   sum((j,k,t),(OCd(j,k,t)+FC(j,k,t)/R(k))*ZZ(j,k,t)*(1+zi(t)))+
                   sum((j,k,t),F(j,k,t)*TPp(j,k,t)*ZZ(j,k,t)*(1+zi(t)))+
                   sum((j,k,t),HCp(j,t)*II(j,t)*ZZ(j,k,t)*(1+zi(t)))+
                   sum((k,t),HCd(k,t)*(R(k)*Miud(k,t)/2)*(1+zi(t)))+
                   sum((k,t),HCd(k,t)*ZRk(k)*((sum((s,l)$(not sameas(s,l)),rou(s,l,t)*(Sigma2(s,t)**0.5)*(Sigma2(l,t)**0.5)*w(k,s,t)*w(k,l,t)*sum(j,(R(k)+LTd(j,k,t))*ZZ(j,k,t)))**0.5)*(1+zi(t))))+
                   sum((k,t),((Pai(k,t)+(1-bb(k,t))*(HCd(k,t)*R(k)+Pai0(k,t)-sum(s,TPd(k,s,t))))/R(k))*(phiZRk(k))*(((sum((s,l)$(not sameas(s,l)),rou(s,l,t)*(Sigma2(s,t)**0.5)*(Sigma2(l,t)**0.5)*w(k,s,t)*w(k,l,t)*sum(j,(R(k)+LTd(j,k,t))*ZZ(j,k,t)))**0.5)*(1+zi(t)))))+
                   sum((str,t),FCs(str,t)*x(str,t)*(1+zi(t)))+
                   sum((k,str,t),TPdtr(k,str,t)*Q(k,str,t)*(1+zi(t)))+
                   sum((k,son,t),TPdon(k,son,t)*Qs(k,son,t)*(1+zi(t)))+
                   sum((k,str,t),HCst(str,t)*HH(str,t)*w(k,str,t)*(1+zi(t)))+
                   sum((k,son,t),HCso(son,t)*Hs(son,t)*w(k,son,t)*(1+zi(t))))/Z1.l)
                   +(1-ww)*(Z2.l-(sum((i,j,t),EIS(i,j,t)*Beta(i,j,t))+sum((j,k,t),EIP(j,k,t)*F(j,k,t))+
                   sum((k,t),EIod(k,t)*y(k,t))+sum((k,str,t),EId(k,str,t)*Q(k,str,t))+
                   sum((k,son,t),EIds(k,son,t)*Qs(k,son,t))+sum((str,t),EIos(str,t)*x(str,t)))/Z2.l)
                   ;
*-*-*-*-*-*-*-*-*-*-*-* Constraints *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
    co1(s,t)..                      sum(k,w(k,s,t))=e=1;
    co2(str,t)..                    sum(k,w(k,str,t))=l=x(str,t);
    co3(son,t)..                    sum(k,w(k,son,t))=l=v(son,t);
    co4(str,t) $ (ord(t) ge 2)..    HH(str,t)=e=sum(k,Q(k,str,t))+HH(str,t-1)-Miu(str,t);
    eq4(str)..                      HH(str,'1')=e=sum(k,Q(k,str,'1'))+HH0(str)-Miu(str,'1');
    co5(son,t) $ (ord(t) ge 2)..    Hs(son,t)=e=sum(k,Qs(k,son,t))+Hs(son,t-1)-Miu(son,t);
    eq5(son)..                      Hs(son,'1')=e=sum(k,Qs(k,son,'1'))+Hs0(son)-Miu(son,'1');
    co6(str,t) $ (ord(t) ge 2)..    sum(k,Q(k,str,t))+HH(str,t-1)=l= Captr(str,t)*x(str,t);
    eq6(str)..                      sum(k,Q(k,str,'1'))+HH0(str)=l= Captr(str,'1')*x(str,'1');
    co7(son,t) $ (ord(t) ge 2)..    sum(k,Qs(k,son,t))+Hs(son,t-1)=l= Capon(son,t)*v(son,t);
    eq7(son)..                      sum(k,Qs(k,son,'1'))+Hs0(son)=l= Capon(son,'1')*v(son,'1');
    co8(k,t)..                      sum(s,Miu(s,t)*w(k,s,t))=e=Miud(k,t);
    co9(k,t)..                      sum(s,sigma2(s,t)**w(k,s,t))=e=Sigma2d(k,t);
    co10(j,t)..                     sum(k,Miud(k,t)*ZZ(j,k,t))=e=Miup(j,t);
    co11(j,t)..                     sum(k,Sigma2d(k,t)*ZZ(j,k,t))=e=Sigma2p(j,t);
    co12(k,t)..                     sum(s,w(k,s,t))=g=sum(j,ZZ(j,k,t));
    eq12(k,t)..                     sum(j,ZZ(j,k,t))=l=y(k,t);
    co13(t)..                       sum(k,y(k,t))=l=YY(t);
    co14(k,t) $ (ord(t) ge 2)..     B(k,t)=e=sum(j,F(j,k,t)*ZZ(j,k,t))+B(k,t-1)-Miud(k,t)*(R(k)+MaxLTd(k))+(ZRk(k)+(1-bb(k,t))*(phiZRk(k)))*(((sum((s,l)$(not sameas(s,l)),rou(s,l,t)*(Sigma2(s,t)**0.5)*(Sigma2(l,t)**0.5)*w(k,s,t)*w(k,l,t)*sum(j,(R(k)+LTd(j,k,t))*ZZ(j,k,t)))**0.5)));
    eq14(k)..                       B(k,'1')=e=sum(j,F(j,k,'1')*ZZ(j,k,'1'))+B0(k)-Miud(k,'1')*(R(k)+MaxLTd(k))+(ZRk(k)+(1-bb(k,'1'))*(phiZRk(k)))*(((sum((s,l)$(not sameas(s,l)),rou(s,l,'1')*(Sigma2(s,'1')**0.5)*(Sigma2(l,'1')**0.5)*w(k,s,'1')*w(k,l,'1')*sum(j,(R(k)+LTd(j,k,'1'))*ZZ(j,k,'1')))**0.5)));
    co15(j,t) $ (ord(t) ge 2)..     II(j,t)=e=sum(i,Beta(i,j,t)*(1-teta(i))*Landa)+II(j,t-1)-Miup(j,t);
    eq15(j)..                       II(j,'1')=e=sum(i,Beta(i,j,'1')*(1-teta(i))*Landa)+II0(j)-Miup(j,'1');
    co16(j,t) $ (ord(t) ge 2)..     sum(i,Beta(i,j,t)*(1-teta(i))*Landa)+II(j,t-1)=l=Cp(j,t);
    eq16(j)..                       sum(i,Beta(i,j,'1')*(1-teta(i))*Landa)+II0(j)=l=Cp(j,'1');
    co17(i,t)..                     sum(j,Beta(i,j,t))=l=CS(i,t);
    co18(i,j,t)..                   Beta(i,j,t)=l=rr(i,j,t)*M;
*   eq18(i,j,t)..                   Beta(i,j,t)=g=rr(i,j,t)*N;
    co19(j,t)..                     sum(i,LTp(i,j,t)*rr(i,j,t))=l=LT(j,t);
    co20(s,t)..                     sum(k,(sum(j,LTd(j,k,t)*ZZ(j,k,t))+LTs(k,s,t))*w(k,s,t))=l=LTt(s,t);
    co21(k,t)..                     ZRk(k)*((sum((s,l)$(not sameas(s,l)),rou(s,l,t)*(Sigma2(s,t)**0.5)*(Sigma2(l,t)**0.5)*w(k,s,t)*w(k,l,t)*sum(j,(R(k)+LTd(j,k,t))*ZZ(j,k,t)))**0.5))+sum(j,F(j,k,t)*ZZ(j,k,t))+B(k,t-1)=l=CD(k,t)*y(k,t);
    co22(k,t)..                     bb(k,t)=g=0;
    co23(k,t)..                     bb(k,t)=l=1;

model LAIPZ1 /obj1,co1,co2,co3,co4,eq4,co5,eq5,co6,eq6,co7,eq7,co8,co9,co10,co11,co12,eq12,co13,co14,eq14,co15,eq15,co16,eq16,co17,co18,co19,co20,co21,co22,co23/
option optca=0,optcr=0,minlp=BARON,reslim=100

model LAIPZ2 /obj2,co1,co2,co3,co4,eq4,co5,eq5,co6,eq6,co7,eq7,co8,co9,co10,co11,co12,eq12,co13,co14,eq14,co15,eq15,co16,eq16,co17,co18,co19,co20,co21,co22,co23/
option optca=0,optcr=0,minlp=BARON,reslim=100;

model LAIPLPMetric /objf,co1,co2,co3,co4,eq4,co5,eq5,co6,eq6,co7,eq7,co8,co9,co10,co11,co12,eq12,co13,co14,eq14,co15,eq15,co16,eq16,co17,co18,co19,co20,co21,co22,co23/
option optca=0,optcr=0,minlp=BARON,reslim=100;

loop(iter,
ww=ww+0.0416;

solve LAIPZ1 using minlp min Z1;
solve LAIPZ2 using minlp max Z2;
solve LAIPLPMetric using minlp min Zf;

result(iter,'Z1')=sum((i,j,t),TC(i,j,t)*Beta.l(i,j,t)*(1+zi(t)))+
                   sum((i,j,t),OCp(i,j,t)*rr.l(i,j,t)*(1+zi(t)))+
                   sum((i,j,t),UC(i,j,t)*Beta.l(i,j,t)*(1+zi(t))*(1-teta(i)))+
                   sum((k,t),FCd(k,t)*y.l(k,t)*(1+zi(t)))+
                   sum((j,k,t),(OCd(j,k,t)+FC(j,k,t)/R(k))*ZZ.l(j,k,t)*(1+zi(t)))+
                   sum((j,k,t),F.l(j,k,t)*TPp(j,k,t)*ZZ.l(j,k,t)*(1+zi(t)))+
                   sum((j,k,t),HCp(j,t)*II.l(j,t)*ZZ.l(j,k,t)*(1+zi(t)))+
                   sum((k,t),HCd(k,t)*(R(k)*Miud.l(k,t)/2)*(1+zi(t)))+
                   sum((k,t),HCd(k,t)*ZRk(k)*((sum((s,l)$(not sameas(s,l)),rou(s,l,t)*(Sigma2(s,t)**0.5)*(Sigma2(l,t)**0.5)*w.l(k,s,t)*w.l(k,l,t)*sum(j,(R(k)+LTd(j,k,t))*ZZ.l(j,k,t)))**0.5)*(1+zi(t))))+
                   sum((k,t),((Pai(k,t)+(1-bb.l(k,t))*(HCd(k,t)*R(k)+Pai0(k,t)-sum(s,TPd(k,s,t))))/R(k))*(phiZRk(k))*(((sum((s,l)$(not sameas(s,l)),rou(s,l,t)*(Sigma2(s,t)**0.5)*(Sigma2(l,t)**0.5)*w.l(k,s,t)*w.l(k,l,t)*sum(j,(R(k)+LTd(j,k,t))*ZZ.l(j,k,t)))**0.5)*(1+zi(t)))))+
                   sum((str,t),FCs(str,t)*x.l(str,t)*(1+zi(t)))+
                   sum((k,str,t),TPdtr(k,str,t)*Q.l(k,str,t)*(1+zi(t)))+
                   sum((k,son,t),TPdon(k,son,t)*Qs.l(k,son,t)*(1+zi(t)))+
                   sum((k,str,t),HCst(str,t)*HH.l(str,t)*w.l(k,str,t)*(1+zi(t)))+
                   sum((k,son,t),HCso(son,t)*Hs.l(son,t)*w.l(k,son,t)*(1+zi(t)));

result(iter,'Z2')=sum((i,j,t),EIS(i,j,t)*Beta.l(i,j,t))+sum((j,k,t),EIP(j,k,t)*F.l(j,k,t))+
                   sum((k,t),EIod(k,t)*y.l(k,t))+sum((k,str,t),EId(k,str,t)*Q.l(k,str,t))+
                   sum((k,son,t),EIds(k,son,t)*Qs.l(k,son,t))+sum((str,t),EIos(str,t)*x.l(str,t));

result(iter,'ww1')=ww;
result(iter,'ww2')=1-ww;

);

display result,Zf.l;

execute_unload 'weighted_Lpmetric.gdx';
