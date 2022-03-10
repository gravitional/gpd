(* ::Package:: *)

(* ::Section::Closed:: *)
(*propagator struct*)


mergeRule::usage="\:5bf9\:4e8e\:8f93\:5165\:7684\:4f20\:64ad\:5b50\:8bb0\:5f55\:ff0c\:4f8b\:5982:{k,\[CapitalLambda]}->1,{k,\[CapitalLambda]}->1, \:8fdb\:884c\:5408\:5e76,\:5c06\:91cd\:590d\:7684\:952e\:503c\:6c42\:548c\:ff0c\:5b58\:5165\:5217\:8868";
mergeRule[rules:__Association|__Rule]:=Normal@Merge[{rules},Total];


(* \:4f20\:64ad\:5b50\[Rule]\:5e42\:6b21 \:7684\:8bb0\:5f55, \:4e0d\:4f9d\:8d56\:4e8e\:53c2\:6570\:6b21\:5e8f ------------------------------*)
SetAttributes[{fadTmp1,fadTmp2},Orderless];
fadTmp1::usage="\:8bb0\:5f55\:4f20\:64ad\:5b50\:7ed3\:6784\:ff0c\:81ea\:52a8\:5408\:5e76\:4e24\:4e2a\:76f8\:4e58\:7684\:4f20\:64ad\:5b50\:51fd\:6570";
(*\:6574\:7406\:4f20\:64ad\:5b50, \:5408\:5e76\:4e24\:4e2a\:4f20\:64ad\:5b50\:8bb0\:5f55 ----------------------------*)
fadTmp1[ruleA:__Rule]*fadTmp1[ruleB:__Rule]^:=fadTmp1@@mergeRule[ruleA,ruleB];
(* \:5c06\:4f20\:64ad\:5b50\:8bb0\:5f55\:7684\:5e42\:6b21,\:6574\:5408\:5230\:53c2\:6570\:4e2d, \:4f20\:64ad\:5b50\[Rule]n*\:5e42\:6b21 -------------------------*)
Power[fadTmp1[rules:__Rule],n_]^:=fadTmp1@@mergeRule[n*Association@{rules}] 
(*\:4f20\:64ad\:5b50\:7684\:6392\:7248\:ff0c\:5b9a\:4e49\:6613\:8bfb\:7684\:6837\:5f0f -----------------------*)
Format[fadTmp1[rules__Rule],TraditionalForm]:=\[DoubleStruckCapitalD][Values[{rules}]];


(*\:5c06\:4f20\:64ad\:5b50\:4ee5\:5173\:8054\:7684\:5f62\:5f0f\:4fdd\:5b58\:8d77\:6765 <{k+\[CapitalDelta],\[CapitalLambda]}\[Rule]-2...>*)
fad::usage="\:8bb0\:5f55\:4f20\:64ad\:5b50\:7ed3\:6784\:ff0c\:4f5c\:4e3a\:8f93\:5165\:63a5\:53e3";
fad[rules:__Rule]:=fadTmp1@@mergeRule[rules]; (*\:5c06\:91cd\:590d\:7684\:4f20\:64ad\:5b50\:7684\:5e42\:6b21,\:7d2f\:52a0\:8d77\:6765*)


fadTmp2::usage="fadTmp1\[Rule]fadTmp2,\:628a\:4f20\:64ad\:5b50\:8f6c\:6362\:5230\:5149\:9525\:5206\:91cf\:5f62\:5f0f\:ff0c\:4e5f\:5c31\:662f\:5177\:4f53\:5f62\:5f0f";
fadTmp2[rules__]:=Times@@KeyValueMap[
(*Power[propagator,num] \:663e\:793a\:8868\:793a\:51fa\:4f20\:64ad\:5b50\:7684\:5e42\:6b21*)
Power,
(*\:4ee3\:5165\:4f20\:64ad\:5b50\:7684\:5149\:9525\:5f62\:5f0f\:ff0ckey\[Rule]val,val \:662f\:4f20\:64ad\:5b50\:7684\:5e42\:6b21*)
KeyMap[LConegator,Association[rules]]]


(*fadTmp3 \:8fd9\:4e2a\:51fd\:6570\:7528\:6765\:6446\:8131\:7cfb\:6570,\:53ea\:5c55\:793a\:5404\:79cd\:4f20\:64ad\:5b50\:7ed3\:6784------------------*)
Times[coe__,fadTmp3[expr__]]^:=fadTmp3[expr]
(*\:4f20\:64ad\:5b50\:7684\:6392\:7248,\:5b9a\:4e49\:6613\:8bfb\:7684\:6837\:5f0f------------------------------------*)
Format[fadTmp3[rules__Rule],TraditionalForm]:=Times@@KeyValueMap[
Power,
KeyMap[\[DoubleStruckCapitalD],Association[rules]]];
(*\:66ff\:6362\:5b8c\:6574\:8868\:8fbe\:5f0f\:4e2d\:7684 fadTmp1 \:4e3a fadTmp3 --------------------*)
gatorShow[expr_]:=(expr/.fadTmp1->fadTmp3)


(* ::Section::Closed:: *)
(*LightCone  Struct*)


(*\:6700\:540e\:4e24\:4e2a\:5206\:91cf\:4f5c\:6b27\:51e0\:91cc\:5f97\:5185\:79ef;\:5185\:79ef\:4e0d\:4f9d\:8d56\:4e8e\:6b21\:5e8f*)
SetAttributes[ed,Orderless];
ed[p_+q__,l_]:=ed[p,l]+ed[q,l](*\:7ebf\:6027\:5f8b*)
ed[num_*p_,q_]:=num*ed[p,q]/;NumberQ[num](*\:5206\:914d\:5f8b*)


(*\:52a8\:91cf\:5934\:90e8, \:52a8\:91cf\:7684\:8fd0\:7b97\:4e0d\:4f9d\:8d56\:4e8e\:53c2\:6570\:6b21\:5e8f*)
SetAttributes[momentum,Orderless];
momentum[p_+q__]:=momentum[p]+momentum[q](*\:7ebf\:6027\:5f8b*)
momentum[num_*p_]:=num*momentum[p]/;NumberQ[num](*\:5206\:914d\:5f8b*)


lConeMember::usage="\:5bf9\:7ed9\:5b9a\:7684\:7b26\:53f7,\:751f\:6210\:4e09\:4e2a\:5149\:9525\:5206\:91cf";
lConeMember[momentum_]:=momentum/@Range[3];


(*\:5149\:9525\:6807\:91cf\:79ef:light-cone scalar product\:ff0c1\:8868\:793a+\:5206\:91cf\:ff0c2\:8868\:793a-\:5206\:91cf\:ff0c3\:8868\:793a\[Perpendicular]\:5206\:91cf*)
lightConeSP[mom1_,mom2_]:=Module[{
lcp=momentum[mom1]/.momentum->lConeMember,
lcq=momentum[mom2]/.momentum->lConeMember
},
(*\:4f7f\:7528 *)
Expand[
(lcp[[1]]*lcq[[2]]+lcp[[2]]*lcq[[1]])-ed[lcp[[3]],lcq[[3]]]
]]


(*gpd \:4e2d\:4f7f\:7528\:7684\:5149\:9525\:52a8\:91cf\:53c2\:6570\:5316*)
lConeKinematics={
(*\:8d28\:5b50\:521d\:6001\:52a8\:91cf-------------------*)
p1[1]->(1+\[Xi])P[1],
p1[2]->(mN^2+ed[\[CapitalDelta][3],\[CapitalDelta][3]]/4)/(2(1+\[Xi])P[1]),
p1[3]->\[CapitalDelta][3]/2,
(*\:4ecb\:5b50\:521d\:6001\:52a8\:91cf*)
k[1]->(y+\[Xi])P[1],
(*k[2]\[Rule]k[2],*)
k[3]->k[3]+\[CapitalDelta][3]/2,
(*\:8d28\:5b50\:672b\:6001\:52a8\:91cf------------------*)
p2[1]->(1-\[Xi])P[1],
p2[2]->(mN^2+ed[\[CapitalDelta][3],\[CapitalDelta][3]]/4)/(2(1-\[Xi])P[1]),
p2[3]->-\[CapitalDelta][3]/2,
(*\:4ecb\:5b50\:672b\:6001\:52a8\:91cf*)
k2[1]->(y-\[Xi])P[1],
(*k2[2]\[Rule]k2[2],*)
k2[3]->k[3]-\[CapitalDelta][3]/2,

\[CapitalDelta][1]->\[Xi](2P[1]),
\[CapitalDelta][2]->(\[CapitalDelta]2+ed[\[CapitalDelta][3],\[CapitalDelta][3]])/(4\[Xi]*P[1])
(*\[CapitalDelta][3]\[Rule]\[CapitalDelta][3]*)
};


kTIntegralKinematics={
ed[\[CapitalDelta][3],\[CapitalDelta][3]]->(-4mN^2 \[Xi]^2+(1-\[Xi]^2)(-\[CapitalDelta]2)),
ed[k[3],\[CapitalDelta][3]]->0,
ed[k[3],k[3]]->ktr^2
};


(*\:4f20\:64ad\:5b50\:7684\:5149\:9525\:5f62\:5f0f--------------*)
LConegator[{k_,\[CapitalLambda]_}]:=lightConeSP[k,k]-\[CapitalLambda]^2;


(* ::Section:: *)
(*propagator poles*)


(*++++++++++++++++++++++++++++++++++++++++++++*)
(*\:4f20\:64ad\:5b50\:53ef\:80fd\:914d\:7f6e\:7684\:5217\:8868*)
gatorConfigs={
{k,\[CapitalLambda]},{k,mm1},(*1,2*)
{k-\[CapitalDelta],\[CapitalLambda]},{k-\[CapitalDelta],mm1},(*3,4*)
{k+\[CapitalDelta],\[CapitalLambda]},{k+\[CapitalDelta],mm1},(*5,6*)
{p1-k,mo1},{p2-k,mo2},(*7,8*)
{p1-k,md1},{p2-k,md2}(*7,8*)
};
(*\:4f20\:64ad\:5b50\:5728\:5149\:9525\:5750\:6807\:4e0b\:7684\:5f62\:5f0f,\:5173\:8054*)
gatorAssoc=AssociationMap[LConegator,gatorConfigs];


(*k- \:5206\:91cf\:524d\:7684\:7cfb\:6570-------------------*)
kminusCoes=Query[(*<propagator\[Rule]LightCone exprs>*)All
(*LightCone exprs*)
,(Coefficient[#1,k[2],1]&)/*Simplify
]@gatorAssoc;
(*\:4f20\:64ad\:5b50\:5149\:9525\:5f62\:5f0f \[Equal]0 \:65f6, k- \:7684\:89e3-----------------*)
gatorZeros=Query[All,
List@@First@First@Solve[#==0,{k[2]}]&
]@gatorAssoc;


(*\:6c42\:51faFAFB\:8868\:8fbe\:5f0f\:ff0c\:5728 k- \:96f6\:70b9\:7684\:7559\:6570;Echo \:6253\:5370\:8fdb\:5ea6*)
FAFBResidue[point_,FAFB_]:=Residue[#,point//Echo]&/@FAFB


(* ::Section:: *)
(*gamma struct*)


decGamma3::usage="decGamma3[\[Mu]_,\[Nu]_,\[Rho]_],\:5341\:91cd\:6001F1\:9876\:89d2\:4f3d\:9a6c\:77e9\:9635";
decGamma3[\[Mu]_,\[Nu]_,\[Rho]_]:=(-I)/2*(
DiracSigma[GA[\[Mu]],GA[\[Nu]]] . GA[\[Rho]]+GA[\[Rho]] . DiracSigma[GA[\[Mu]],GA[\[Nu]]])


\[CapitalTheta]::usage="\:5341\:91cd\:6001\:79bb\:58f3\:53c2\:6570\:6240\:5728\:7684 \[Gamma] \:77e9\:9635";
\[CapitalTheta][\[Mu]_,\[Nu]_]:=I*DiracSigma[GA[\[Mu]],GA[\[Nu]]]


decGator::usage="decGator[{\[Alpha]_,\[Beta]_},{p_,md_}],\:5341\:91cd\:6001\:4f20\:64ad\:5b50\:7684\:65cb\:91cf\:90e8\:5206";
decGator[{\[Alpha]_,\[Beta]_},{p_,md_}]:=(-(GS[p]+md) . (
MT[\[Alpha],\[Beta]]-1/3 GA[\[Alpha]] . GA[\[Beta]]-(GA[\[Alpha]] . FV[p,\[Beta]]-GA[\[Beta]] . FV[p,\[Alpha]])/(3md)-(2FV[p,\[Alpha]] . FV[p,\[Beta]])/(3md^2)))


(* ::Section:: *)
(*splitting function projectors*)


(*\:8fd0\:52a8\:5b66\:5173\:7cfb\:ff0c\:6807\:91cffix*)
SP[p1,p1]=mN^2;SP[p2,p2]=mN^2;
SP[p1,\[CapitalDelta]]=\[CapitalDelta]2/2;
SP[p2,\[CapitalDelta]]=-\[CapitalDelta]2/2;
SP[p1,p2]=mN^2-\[CapitalDelta]2/2;


ruleMomToExternal={\[CapitalDelta]->p1-p2};


(*\:628a\:81ea\:7531\:6307\:6807\:7684\:52a8\:91cf\:90fd\:66ff\:6362\:6210 plus \:5206\:91cf*)
RulePlusMom=FCI[{
MT[\[Mu]_,\[Nu]_]->0,(*\:5ea6\:89c4\:7684++\:5206\:91cf\:4e3a\:96f6*)
FV[k,\[Mu]_]->k[1],
FV[p1,\[Mu]_]->p1[1],
FV[p2,\[Mu]_]->p2[1],
FV[\[CapitalDelta],\[Mu]_]->\[CapitalDelta][1]
}];


(*\:6295\:5f71\:7b97\:7b26,\:6784\:9020fa,fb,\:518d\:7ebf\:6027\:7ec4\:5408\:6210f1,f2*)
projector["FA",\[Nu]_]:=(GS[p1]+mN) . (GA[\[Nu]]) . (GS[p2]+mN)
projector["FB",\[Nu]_]:=(FV[p1+p2,\[Nu]]/(2mN)) . (GS[p1]+mN) . (GS[p2]+mN)
(*\:5bf9\:632f\:5e45\:6c42 Trace,\:6295\:5f71\:51fa\:7ed3\:6784\:51fd\:6570fa,fb*)
traceExpr[expr_]:=(DiracSimplify[DiracTrace[expr]]/.RulePlusMom)
(*\:5c06\:6295\:5f71\:7b97\:5b50\:4f5c\:7528\:5230\:632f\:5e45\:4e0a,\:5e76\:5316\:7b80\:5230\:6807\:91cf\:8868\:8fbe\:5f0f; \:5f97\:5230\:632f\:5e45\:7684\:4e24\:4e2a\:6295\:5f71fa,fb*)
projToFAFB[Amp\[Mu]_,\[Nu]_]:={
traceExpr[Amp\[Mu] . projector["FA",\[Nu]]],
traceExpr[Amp\[Mu] . projector["FB",\[Nu]]]
}


(*\:5c06FA,FB\:4f5c\:7ebf\:6027\:7ec4\:5408,\:91cd\:65b0\:7ec4\:6210f1,f2---------------------*)
FAFBToF1F2[{FA_,FB_}]:=Module[
(*\:5e38\:89c1\:7684plus\:52a8\:91cf\:7ec4\:5408*)
{Pp=(p1[1]+p2[1])/2,\[CapitalDelta]p=p1[1]-p2[1],p12p=p1[1]*p2[1]},
(*-----*)
{
(FA*\[CapitalDelta]2*Pp^2+4FB*mN^2*\[CapitalDelta]p^2)/(8Pp^2(mN^2*\[CapitalDelta]p^2+p12p*\[CapitalDelta]2)),
(-FA*mN^2*Pp^2+FB*mN^2*p12p)/(2Pp^2(mN^2*\[CapitalDelta]p^2+p12p*\[CapitalDelta]2))
}]
(*FA,FB \:4f5c\:7ec4\:5408\:6210f1,f2,\:7559\:4f5c\:989d\:5916\:5904\:7406 --------------------*)
projToF1F2[FAFBList_]:=FAFBToF1F2@FAFBList


SetOptions[Simplify,TimeConstraint->1];
