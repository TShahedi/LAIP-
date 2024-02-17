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
    s               index of sellers /1*5/
    str(s)          index of traditional sellers  /1*3/
    son(s)          index of online sellers  /4*5/
    k               index for potential locations for distribution centers /1*4/
    j               index of plants /1*4/
    t               index of period time  /1*3/
**  G               Set of permissible review intervals /1*3/
    h               objective function  /obj01,obj02/
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
    dir(h)          direction of objective function
    /obj01 -1
     obj02 -1/
    Results(*)
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
         Z
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
         obj11
         obj2
         obj21
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
 obj1(H)$(ord(H)=1)..  Z(H)=e=z1;
 obj11    .. Z1 =e=sum((i,j,t),TC(i,j,t)*Beta(i,j,t)*(1+zi(t)))+
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

obj2(H)$(ord(H)=2)..  Z(H)=e=z2 ;
obj21    ..  Z2=e=sum((i,j,t),EIS(i,j,t)*Beta(i,j,t))+sum((j,k,t),EIP(j,k,t)*F(j,k,t))+
                  sum((k,t),EIod(k,t)*y(k,t))+sum((k,str,t),EId(k,str,t)*Q(k,str,t))+
                  sum((k,son,t),EIds(k,son,t)*Qs(k,son,t))+sum((str,t),EIos(str,t)*x(str,t))
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
*    eq18(i,j,t)..                   Beta(i,j,t)=g=rr(i,j,t)*N;
    co19(j,t)..                     sum(i,LTp(i,j,t)*rr(i,j,t))=l=LT(j,t);
    co20(s,t)..                     sum(k,(sum(j,LTd(j,k,t)*ZZ(j,k,t))+LTs(k,s,t))*w(k,s,t))=l=LTt(s,t);
    co21(k,t)..                     ZRk(k)*((sum((s,l)$(not sameas(s,l)),rou(s,l,t)*(Sigma2(s,t)**0.5)*(Sigma2(l,t)**0.5)*w(k,s,t)*w(k,l,t)*sum(j,(R(k)+LTd(j,k,t))*ZZ(j,k,t)))**0.5))+sum(j,F(j,k,t)*ZZ(j,k,t))+B(k,t-1)=l=CD(k,t)*y(k,t);
    co22(k,t)..                     bb(k,t)=g=0;
    co23(k,t)..                     bb(k,t)=l=1;


model example /all/;

$sTitle eps-constraint Method
Set
   k1(h)  'the first element of k'
   km1(h) 'all but the first elements of k'
   kk(h)  'active objective function in constraint allobj';

k1(h)$(ord(h) = 1) = yes;
km1(h)  = yes;
km1(k1) =  no;

Parameter
   rhs(h)    'right hand side of the constrained obj functions in eps-constraint'
   maxobj(h) 'maximum value from the payoff table'
   minobj(h) 'minimum value from the payoff table'
   numk(h)   'ordinal value of k starting with 1';
Scalar
   iter         'total number of iterations'
   infeas       'total number of infeasibilities'
   elapsed_time 'elapsed time for payoff and e-sonstraint'
   start        'start time'
   finish       'finish time';

Variable
   a_objval 'auxiliary variable for the objective function'
   obj      'auxiliary variable during the construction of the payoff table'
   sl(h)    'slack or surplus variables for the eps-constraints'
;

Positive Variable sl;

Equation
   con_obj(h) 'constrained objective functions'
   augm_obj   'augmented objective function to avoid weakly efficient solutions'
   allobj     'all the objective functions in one expression';

con_obj(km1).. z(km1) - dir(km1)*sl(km1) =e= rhs(km1);

* We optimize the first objective function and put the others as constraints
* the second term is for avoiding weakly efficient points

augm_obj..
   a_objval =e= sum(k1,dir(k1)*z(k1))
         + 1e-3*sum(km1,power(10,-(numk(km1) - 1))*sl(km1)/(maxobj(km1) - minobj(km1)));

allobj .. sum(kk, dir(kk)*z(kk))=e= obj ;

Model
   mod_payoff    / example, allobj            /
   mod_epsmethod / example, con_obj, augm_obj /;

Parameter payoff(h,h) 'payoff tables entries';

Alias (h,kp);

option optcr=0,optca=0 ,reslim=36,iterlim=1000, limRow = 0, limCol = 0, solPrint = on, solveLink = %solveLink.LoadLibrary%;

* Generate payoff table applying lexicographic optimization
loop(kp,
   kk(kp) = yes;
   repeat
      option reslim=36;
      option iterlim=1000;
      solve mod_payoff using minlp maximizing obj;
      payoff(kp,kk) = z.l(kk);
      z.fx(kk) = z.l(kk); // freeze the value of the last objective optimized
      kk(h++1) = kk(h);   // cycle through the objective functions
   until kk(kp);
   kk(kp) = no;
*  release the fixed values of the objective functions for the new iteration
   z.up(h) =  inf;
   z.lo(h) = -inf;
);
if(mod_payoff.modelStat <> %modelStat.Optimal% and
   mod_payoff.modelStat <> %modelStat.Integer Solution%,
   abort 'no optimal solution for mod_payoff');

File fx / proj.txt /;
put  fx ' PAYOFF TABLE'/;
loop(kp,
   loop(h, put payoff(kp,h):12:2);
   put /;
);

minobj(h) = smin(kp,payoff(kp,h));
maxobj(h) = smax(kp,payoff(kp,h));

* gridpoints are calculated as the range (difference between max and min) of
* the 2nd objective function from the payoff table
$if not set gridpoints $set gridpoints 7
Set
   g1         'grid points' / g0*g%gridpoints% /
   grid(h,g1) 'grid';

Parameter
   gridrhs(h,g1) 'RHS of eps-constraint at grid point'
   maxg(h)      'maximum point in grid for objective'
   posg(h)      'grid position of objective'
   firstOffMax  'some counters'
   lastZero     'some counters'
*  numk(k) 'ordinal value of k starting with 1'
   numg(g1)      'ordinal value of g starting with 0'
   step(h)      'step of grid points in objective functions'
   jump(h)      'jumps in the grid points traversing';

lastZero = 1;
loop(km1,
   numk(km1) = lastZero;
   lastZero  = lastZero + 1;
);
numg(g1) = ord(g1) - 1;

grid(km1,g1) = yes; // Here we could define different grid intervals for different objectives
maxg(km1)   = smax(grid(km1,g1), numg(g1));
step(km1)   = (maxobj(km1) - minobj(km1))/maxg(km1);
gridrhs(grid(km1,g1))$(dir(km1) = -1) = maxobj(km1) - numg(g1)/maxg(km1)*(maxobj(km1) - minobj(km1));
gridrhs(grid(km1,g1))$(dir(km1) =  1) = minobj(km1) + numg(g1)/maxg(km1)*(maxobj(km1) - minobj(km1));

put / ' Grid points' /;
loop(g1,
   loop(km1, put gridrhs(km1,g1):12:2);
   put /;
);
put / 'Efficient solutions' /;

* Walk the grid points and take shortcuts if the model becomes infeasible or
* if the calculated slack variables are greater than the step size
posg(km1) = 0;
iter   = 0;
infeas = 0;
start  = jnow;

repeat
   rhs(km1) = sum(grid(km1,g1)$(numg(g1) = posg(km1)), gridrhs(km1,g1));
   option reslim=36;
   option iterlim=1000;
   solve mod_epsmethod maximizing a_objval using minlp;
   iter = iter + 1;
   if(mod_epsmethod.modelStat<>%modelStat.Optimal% and
      mod_epsmethod.modelStat<>%modelStat.Integer Solution%,
      infeas = infeas + 1; // not optimal is in this case infeasible
      put iter:5:0, '  infeasible' /;
      lastZero = 0;
      loop(km1$(posg(km1)  > 0 and lastZero = 0), lastZero = numk(km1));
      posg(km1)$(numk(km1) <= lastZero) = maxg(km1); // skip all solves for more demanding values of rhs(km1)
   else
      put iter:5:0;
      loop(h, put z.l(h):12:2);
      jump(km1) = 1;
*     find the first off max (obj function that hasn't reach the final grid point).
*     If this obj.fun is k then assign jump for the 1..k-th objective functions
*     The jump is calculated for the innermost objective function (km=1)
      jump(km1)$(numk(km1) = 1) = 1 + floor(sl.L(km1)/step(km1));
      put '    'z1.l,z2.l :12:2;
      loop(km1$(jump(km1)  > 1), put '   jump');
      put /;
   );
*  Proceed forward in the grid
   firstOffMax = 0;
   loop(km1$(posg(km1) < maxg(km1) and firstOffMax = 0),
      posg(km1)   = min((posg(km1) + jump(km1)),maxg(km1));
      firstOffMax = numk(km1);
   );
   posg(km1)$(numk(km1) < firstOffMax) = 0;
   abort$(iter > 1000) 'more than 1000 iterations, something seems to go wrong'
until sum(km1$(posg(km1) = maxg(km1)),1) = card(km1) and firstOffMax = 0;
finish = jnow;
display payoff,Miu;
elapsed_time = (finish - start)*60*60*24;
SS1=sum((k,t),ZRk(k)*((sum((s,l)$(not sameas(s,l)),rou(s,l,t)*(Sigma2(s,t)**0.5)*(Sigma2(l,t)**0.5)*w.l(k,s,t)*w.l(k,l,t)*sum(j,(R(k)+LTd(j,k,t))*ZZ.l(j,k,t)))**0.5)));
SSH1=sum((k,t),bb.l(k,t)*Miud.l(k,t));
Results('Safety Stock')=SS1;
Results('Shortage')=SSH1;
display Results;
put /;
put 'Infeasibilities = ', infeas:5:0 /;
put 'Elapsed time: ',elapsed_time:10:2, ' seconds' /;
execute_unload 'proj.gdx'



