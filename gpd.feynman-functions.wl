(* ::Package:: *)

(* ::Title:: *)
(*Feynman functions*)


(* ::Section:: *)
(*symbols,Feynman parameterization*)


(*\:516b\:91cd\:6001\:90e8\:5206\:547d\:540d------------*)
ClearAll[
(*\:4ecb\:5b50\:8870\:53d8\:5e38\:6570*)
f
(*\:521d\:672b\:6001\:6838\:5b50\:8d28\:91cf;\:4ecb\:5b50\:8d28\:91cf;\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf*)
,mN,mm1,mo1,mo2
(*MB-mN; mN+MB*)
,m\[CapitalDelta],mS
(*\:52a8\:91cf*)
,p1,p2
(*(p1+p2)/2,p1-p2*)
,P,\[CapitalDelta],
(*\:8d39\:66fc\:53c2\:6570-----------*)
f
];


(* ::Section:: *)
(*Feynman parameter*)


(*\:4ee4 f \:662f\:6570\:503c\:51fd\:6570,\:8fd9\:6837\:5728\:7ebf\:6027\:51fd\:6570\:4e2d,\:53ef\:4ee5\:81ea\:52a8\:63d0\:5230\:51fd\:6570\:5916*)
SetAttributes[f,NumericFunction]
(*\:8d39\:66fc\:53c2\:6570\:5316\:ff1a\:4f20\:64ad\:5b50\:7684\:7ec4\:5408*)
feynmanParameter[props_List]:=Module[{len=Length@props,coes},
coes=Prepend[f/@Range[len-1],1]-Append[f/@Range[len-1],0];
props . coes
]
(*\:8d39\:66fc\:53c2\:6570 f[n] \:8f6c\:6362\:5230\:7eaf\:7b26\:53f7\:5f62\:5f0f fn *)
ruleFeynParaToSymbol=AssociationThread[f/@Range[10],Symbol@StringTemplate["f``"]@#&/@Range@10];
(*\:9006\:53d8\:6362----------*)
ruleSymbolToFeynPara=Association[Normal@ruleFeynParaToSymbol/.Rule->OperatorApplied[Rule]];


(* ::Section:: *)
(*Parameter convex Space*)


(*\:751f\:6210\:8d39\:66fc\:53c2\:6570\:7a7a\:95f4\:7684\:51e0\:4f55\:5f62\:72b6: \:5e93\:6069\:5355\:5f62*)
KuhnSimplex[n_]:=Simplex@Table[If[j<i,1,0],{i,1,n+1},{j,n}]


(* ::Section:: *)
(*print latex Form*)


(*\:5c06 Symbol \:663e\:793a\:6210 Humam \:6837\:5f0f*)
ruleTeX={
p1->Subscript[p,1],p2->Subscript[p, 2]
,l->\[ScriptL],\[CapitalDelta]2->\[CapitalDelta]^2
,mN->Subscript[M,N]
,mo1->Subscript[M,B],mo2->Subscript[M,B2]
,mm1->Subscript[m,\[Phi]]
(*\:8d39\:66fc\:53c2\:6570\:5316*)
,Splice@Normal@AssociationThread[f/@Range[10],ToExpression@CharacterRange[97,106]]
};
(*\:9006\:66ff\:6362\:89c4\:5219\:ff0c\:4ea4\:6362 ruleTeX \:4e2d\:4e24\:4e2a\:53c2\:6570\:7684\:4f4d\:7f6e ----------------*)
ruleTexRev=ruleTeX/.Rule->ReverseApplied[Rule];


(*ward \:6052\:7b49\:5f0f\:ff0c\:65b9\:4fbf\:53bb\:6389\:6b63\:6bd4\:4e8e \[CapitalDelta] \:7684\:9879*)
ruleMomentCombine=MomentumExpand@{
Momentum[p1]->Momentum[P]+Momentum[\[CapitalDelta]]/2,
Momentum[p2]->Momentum[P]-Momentum[\[CapitalDelta]]/2
};


(* ::Section:: *)
(*Reduce rule*)


(*\:5149\:9525\:53d1\:6563\:79ef\:5206\:6b63\:6bd4\:4e8e \[Delta][l+], \:6700\:7ec8\:5c06\:7ea6\:675f {\:8d39\:66fc\:53c2\:6570,y,\[Xi]}, \:8fd9\:91cc\:53bb\:6389\:516c\:56e0\:5b50\:5916\:52a8\:91cf*)
rulePlusMom\[Delta]Reduce=FCI@{FV[P,p]->1};


(* ::Section:: *)
(*kinematics*)


(*\:8fd0\:52a8\:5b66\:4e0d\:53d8\:91cf*)
SP[p1,p1]=mN^2;
SP[p2,p2]=mN^2;
SP[p1,\[CapitalDelta]]=\[CapitalDelta]2/2;
SP[p2,\[CapitalDelta]]=-\[CapitalDelta]2/2;
SP[p1,p2]=mN^2-\[CapitalDelta]2/2;
(*-----------*)
SP[P,P]=mN^2-\[CapitalDelta]2/4;
SP[\[CapitalDelta],\[CapitalDelta]]=\[CapitalDelta]2;
SP[P,\[CapitalDelta]]=0;


(*\:66ff\:6362\:89c4\:5219*)
ruleMomToExternal={\[CapitalDelta]->p1-p2};


(*\:53bb\:6389 Dirac Spinor \:4e24\:7aef\:7684\:65cb\:91cf*)
dropSpinorDot[ubar_Spinor,expr__,u_Spinor]:=dropSpinorDot[expr]
dropSpinorDot[ubar_Spinor,u_Spinor]:=1
dropSpinor[expr_]:=expr/.{Dot->dropSpinorDot}/.{dropSpinorDot->Dot}


(* ::Section:: *)
(*LightCone  Form*)


(*\:6700\:540e\:4e24\:4e2a\:5206\:91cf\:4f5c\:6b27\:51e0\:91cc\:5f97\:5185\:79ef;\:5185\:79ef\:4e0d\:4f9d\:8d56\:4e8e\:6b21\:5e8f*)
SetAttributes[ed,Orderless];
ed[p_+q_,l_]:=ed[p,l]+ed[q,l](*\:7ebf\:6027\:5f8b*)
ed[num_*p_,q_]:=num*ed[p,q]/;NumericQ[num](*\:5206\:914d\:5f8b*)


(*gpd \:4e2d\:4f7f\:7528\:7684\:5149\:9525\:52a8\:91cf\:53c2\:6570\:5316:
\:4f7f\:7528 p,m,t \:5206\:522b\:8868\:793a\:5149\:9525 Plus, Minus, Transverse \:52a8\:91cf\:5206\:91cf*)
lConeKinematics=FCI@{
(*\:8d28\:5b50\:521d\:6001\:52a8\:91cf-------------------*)
FV[p1,p]->(1+\[Xi])FV[P,p],
FV[p1,m]->(mN^2+SP[\[CapitalDelta]T,\[CapitalDelta]T]/4)/(2(1+\[Xi])FV[P,p]),
FV[p1,t]->FV[\[CapitalDelta]T,t]/2,
(*\:4ecb\:5b50\:521d\:6001\:52a8\:91cf*)
FV[k1,p]->(y+\[Xi])FV[P,p],
(*k1[m]\[Rule]k[2],*)
FV[k1,t]->FV[k,t]+FV[\[CapitalDelta]T,t]/2,
(*\:8d28\:5b50\:672b\:6001\:52a8\:91cf------------------*)
FV[p2,p]->(1-\[Xi])FV[P,p],
FV[p2,m]->(mN^2+SP[\[CapitalDelta]T,\[CapitalDelta]T]/4)/(2(1-\[Xi])FV[P,p]),
FV[p2,t]->-FV[\[CapitalDelta]T,t]/2,
(*\:4ecb\:5b50\:672b\:6001\:52a8\:91cf*)
FV[k2,p]->(y-\[Xi])FV[P,p],
(*k2[m]\[Rule]k2[m],*)
FV[k2,t]->FV[k,t]-FV[\[CapitalDelta]T,t]/2,
(*\[CapitalDelta]=p1-(p2 --------------------*)
FV[\[CapitalDelta],p]->\[Xi](2FV[P,p]),
FV[\[CapitalDelta],m]->(\[CapitalDelta]2+SP[\[CapitalDelta]T,\[CapitalDelta]T])/(4\[Xi]*FV[P,p]),
FV[\[CapitalDelta],t]->\[CapitalDelta]T
};


(* ::Section:: *)
(*Feynman basis*)


(*\:4f20\:64ad\:5b50\:7684\:5b9a\:4e49\:ff0c\:53ef\:4ee5\:5177\:6709\:5e42\:6b21----------------------*)
propg[k_,m_,pow_:1]:=(SP[k,k]-m^2)^pow
(*\:4f20\:64ad\:5b50\:7684\:57fa\:5143, \:4ee5\:53ca\:5e42\:6b21 -------------------*)
feynPropg[k_,m_,pow_:1]:=<|
"basis"->propg[k,m],
"power"->pow
|>


(*CompleteSquare[a p^2+b p+c, p, q] -> {a q^2-b^2/(4 a)+c, q\[Rule]p+b/(2 a)}.*)
(*\:5c06\:5408\:5e76\:540e\:7684\:4f20\:64ad\:5b50\:914d\:9f50\:6210\:5b8c\:5168\:5e73\:65b9*)
toSquare[expr_,k_,l_]:=Module[
{res,kRule},
res=CompleteSquare[expr,k,l];
kRule=First@First@Solve[Equal@@Last@FCI@res,Momentum[k]];
Append[kRule]@res
]


(* ::Section::Closed:: *)
(*Propagator representation*)


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
propgShow[expr_]:=(expr/.fadTmp1->fadTmp3)
