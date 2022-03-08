(* ::Package:: *)

(* ::Title:: *)
(*gpd.loop.feynman-parameter.wl*)


(* ::Chapter:: *)
(*initial*)


(*\:672c\:6587\:4ef6\:7684\:540d\:79f0*)
$fileName=If[$Notebooks,NotebookFileName[],$InputFileName];
(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c\:ff0c\:5c31\:5237\:65b0\:7b14\:8bb0\:672c\:7684\:6807\:9898*)
Once@If[$Notebooks,NotebookWrite[Cells[][[1]],Cell[Last@FileNameSplit[$fileName],"Title"]]];
(*\:67e5\:627e init.wl, \:5bfc\:5165\:6839\:76ee\:5f55\:548c\:51fd\:6570\:5b9a\:4e49.*)
Once@Catch@Module[{recurFind,start=1,depMax},
depMax=FileNameDepth[$fileName];(*\:8def\:5f84\:7684\:6700\:5927\:5c42\:6b21*)
(*-------\:5b9a\:4e49\:9012\:5f52\:51fd\:6570-------*)
recurFind[dep_Integer]:=If[dep<=depMax,
SetDirectory[DirectoryName[$fileName,dep]];(*SetDirectory[]:\:8bbe\:7f6e\:5de5\:4f5c\:76ee\:5f55\:4e3a\:5bb6\:76ee\:5f55*)
(*\:5982\:679c\:5728\:5f53\:524d\:5c42\:80fd\:627e\:5230 init.wl,\:5c31\:8fd0\:884c\:5b83,\:5e76\:628a\:6839\:76ee\:5f55\:6dfb\:52a0\:5230\:641c\:7d22\:8def\:5f84*)
If[FileExistsQ["init.wl"],
Get["init.wl"];PrependTo[$Path,$srcRoot];
Throw["The base directory is : "<>$srcRoot];,
(*\:5982\:679c\:8fd9\:4e00\:5c42\:627e\:4e0d\:5230\:ff0c\:5c31\:4e0a\:5347\:4e00\:5c42*)
recurFind[dep+1]];
ResetDirectory[];(*\:91cd\:8bbe\:4e3a\:4e4b\:524d\:7684\:76ee\:5f55*),
Throw["I cann't find any init.wl in this project"]
];
recurFind[start];
]
(* \:8bb0\:5f55 master Kernel \:7684\:8fd0\:884c\:6a21\:5f0f, \:53ef\:5728\:5e76\:884c\:8ba1\:7b97\:4e2d\:4f7f\:7528 *)
$inNBook=$Notebooks;echo[DateString[]," <<",$fileName];


(* ::Section:: *)
(*<<modules*)


(*\:5bfc\:5165\:65cb\:91cf\:8ba1\:7b97\:7a0b\:5e8f--------------*)
Once[<<"FeynCalc`"]


(*\:67e5\:770b FeynCalc External,Interal \:7684\:6807\:51c6\:5f62\:5f0f------------------*)
fceStd[x_]:=x//FCE//StandardForm;
fciStd[x_]:=x//FCI//StandardForm;


Get["gpd.interface.wl"];
(*\:8d39\:66fc\:56fe\:7684\:540d\:79f0\:548c\:6837\:5f0f---------------------*)
Get["gen.integral-TagList.wl"];


(* ::Section:: *)
(*symbols*)


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


(*\:4ee4 f \:662f\:6570\:503c\:51fd\:6570*)
SetAttributes[f,NumericFunction]
(*\:8d39\:66fc\:53c2\:6570\:5316\:ff1a\:4f20\:64ad\:5b50\:7684\:7ec4\:5408*)
feynmanParameter[props_List]:=Module[{len=Length@props,coes},
coes=Prepend[f/@Range[len-1],1]-Append[f/@Range[len-1],0];
props . coes
]


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


(*gpd \:4e2d\:4f7f\:7528\:7684\:5149\:9525\:52a8\:91cf\:53c2\:6570\:5316*)
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
(*Feynman parameterization*)


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


(* ::Chapter:: *)
(*Loop integral*)


(* ::Section:: *)
(*Rainbow,meson,octet*)


(* ::Input:: *)
(*diagIllus@chTag@{"RB","mes","oct"}*)


(*\:6d89\:53ca\:5230\:7684\:52a8\:91cf\:ff0c\:5747\:7528\:5916\:52a8\:91cf p1,p2 \:8868\:793a\:ff0c\:4fbf\:4e8e\:5e94\:7528\:72c4\:62c9\:514b\:65b9\:7a0b\:5316\:7b80\:65cb\:91cf *)
(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"RB","mes","oct"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
split[{fyTag,"spin",\[Mu]}]=DiracSimplify[
ExpandScalarProduct[
SpinorUBar[p2,mN] . ( 
FV[2k1-\[CapitalDelta],\[Mu]] . GS[k1-\[CapitalDelta]] . GA5 . (GS[p1-k1]+mo1) . GS[k1] . GA5
) .
SpinorU[p1,mN]/.ruleMomToExternal
],
DiracEquation->True
]


(* ::Input:: *)
(*(*TeXForm*)*)
(*Simplify[*)
(*dropSpinor@split[{fyTag,"spin",\[Mu]}]/.ruleTeX*)
(*]*)


(*\:4f20\:64ad\:5b50\:5217\:8868------------------*)
split[{fyTag,"propg"}]={
feynPropg[k1-\[CapitalDelta],\[CapitalLambda],2],feynPropg[k1,\[CapitalLambda],2]
,feynPropg[k1-\[CapitalDelta],mm1],feynPropg[k1,mm1]
,feynPropg[p1-k1,mo1]
}/.ruleMomToExternal;
(*\:4f20\:64ad\:5b50\:57fa------*)
feynIntBasis=Query[All
,(Key@"basis")
]@split[{fyTag,"propg"}];
split[{fyTag,"basis"}];
(*\:4f20\:64ad\:5b50\:5e42\:6b21\:4e4b\:548c ------*)
feynIntPower=Query[Total
,(Key@"power")
]@split[{fyTag,"propg"}];


(* ::Input:: *)
(*split[{fyTag,"propg"}]/.ruleTeX*)


(*TeXForm*)
split[{fyTag,"square"}]=Simplify@ExpandScalarProduct[
toSquare[
(*\:8d39\:66fc\:53c2\:6570\:5316*)
feynmanParameter@feynIntBasis
,k1,l]];


(*\:8d39\:66fc\:53c2\:6570\:5316,\:5e73\:79fb\:79ef\:5206\:53d8\:91cf----------*)
ruleMomentShift=FCI[Last@split[{fyTag,"square"}]];
(*\:52a8\:91cf\:5e73\:79fb, l\[Rule]k*)
ruleMomentShiftTok=FCI@split[{fyTag,"square"}][[2]];
(*\:5408\:5e76\:4f20\:64ad\:5b50\:4e4b\:540e,\:5708\:79ef\:5206\:5206\:6bcd l^2-\[Beta] \:7684\:5e38\:6570\[Beta],  *)
denominator=First@split[{fyTag,"square"}]-FCI@SP[l,l];
(* \:6a2a\:5411\:52a8\:91cf\:7684\:66ff\:6362\:89c4\:5219------------------- *)
ruleTransMomShiftTok=KeyValueMap[
RuleDelayed@@{
(*Key of Rule*)
#1/.{Momentum[l_]:>l[t]},
(*Value of Rule*)
#2/.{Momentum[p_]:>FCI@FV[p,t]}
}&
]@Association@ruleMomentShiftTok;


(* ::Input:: *)
(*split[{fyTag,"square"}]/.ruleTeX//TableForm*)


ruleIntNumerator={
(*Ward \:6052\:7b49\:5f0f, \[CapitalDelta]\[Mu].\[CapitalGamma]\[Mu]=0-----------*)
Pair[LorentzIndex[\[Mu]],Momentum[\[CapitalDelta]]]->0,

(*\:5708\:79ef\:5206\:5bf9\:79f0\:6027: (l.p1)(l.p2)\[Rule]1/4(l^2)(p1.p2) -------*)
Pair[p1_,Momentum[l]] Pair[p2_,Momentum[l]]/;!MemberQ[{p1,p2},Momentum[l]]:>1/4*
Pair[p1,p2] Pair[Momentum[l],Momentum[l]],
(*\:5708\:79ef\:5206\:5bf9\:79f0\:6027: (\[Gamma].l)(l.p)\[Rule]1/4(l^2)(p.\[Gamma]) ------*)
DiracGamma[Momentum[l]] Pair[p_,Momentum[l]]:>1/4*
DiracGamma[p] Pair[Momentum[l],Momentum[l]]

(*Ward \:6052\:7b49\:5f0f,\:4ee5\:53ca\:72c4\:62c9\:514b\:65b9\:7a0b*)
,coes__*DiracGamma[Momentum[\[CapitalDelta]]]:>0
,coes__*DiracGamma[Momentum[P]]:>coes*mN
};


(*\:5e94\:7528 Ward \:6052\:7b49\:5f0f\:ff0c\:4ee5\:53ca\:6839\:636e\:5708\:79ef\:5206\:5947\:5076\:6027\:ff0c\:91cf\:7eb2\:63a8\:5bfc\:7684\:66ff\:6362\:89c4\:5219*)
refineLoop[numerator_]:=Nest[
(*\:5708\:79ef\:5206 l\[Mu] l\[Nu]\[Rule]1/4 g\[Mu]\[Nu] l^2----------*)
ReplaceAll[ruleIntNumerator],
(*\:5c55\:5f00\:6807\:91cf\:79ef\:548c\:52a8\:91cf\:7ec4\:5408*)
ExpandAll2@ExpandScalarProduct[
(*\:4e22\:5f03\:8d39\:7c73\:5b50\:94fe\:4e24\:7aef\:65cb\:91cf-------*)
dropSpinor@
(*\:4f7f\:7528\:72c4\:62c9\:514b\:65b9\:7a0b\:5316\:7b80,\:5916\:52a8\:91cf\:7684 \[Gamma] \:77e9\:9635*)
DiracSimplify[
(*\:4f7f\:7528\:5708\:79ef\:5206\:5f20\:91cf\:6027\:8d28 l\[Mu] l\[Nu]\[Rule]1/4 g\[Mu]\[Nu] l^2*)
Nest[ReplaceAll[ruleIntNumerator],
ExpandAll2@ExpandScalarProduct[
(*\:52a8\:91cf\:5e73\:79fb k\[Rule]l ----*)
FCI@numerator/.ruleMomentShift
]
(*\:91cd\:590d\:66ff\:6362 n \:6b21------*)
,2]
,DiracEquation->True
]/.ruleMomentCombine]
,5]


(*\:5316\:7b80\:4e00\:6b21\:7684\:7ed3\:679c--------------*)
split[{fyTag,"spinTmp",\[Mu]}]=Simplify@
refineLoop@
split[{fyTag,"spin",\[Mu]}];


(* ::Input:: *)
(*split[{fyTag,"spinTmp",\[Mu]}]/.ruleTeX*)


symmetryLoop[expr_]:=ExpandAll2[1/2(
expr+
(FCI@expr/.{Momentum[l]->Momentum[-l]})
)]


split[{fyTag,"FAFB",\[Mu]}]=Module[{
(*\:8003\:8651\:5708\:79ef\:5206\:7684\:5947\:5076\:6027*)
expr=symmetryLoop@split[{fyTag,"spinTmp",\[Mu]}]
},

<|
"expr"->Collect[expr,FCI/@{FV[P,\[Mu]],GA[\[Mu]]},Simplify],
"P\[Mu]"->Simplify@Coefficient[expr,FCI@FV[P, \[Mu]]],
"\[Gamma]\[Mu]"->Simplify@Coefficient[expr,FCI@GA[ \[Mu]]]
|>
];


(* ::Input:: *)
(*split[{fyTag,"FAFB",\[Mu]}]/.ruleTeX*)


ted=(split[{fyTag,"FAFB",\[Mu]}][["P\[Mu]"]]/.{
FCI@SP[l,l]->fad[{l,\[Beta]}->-1]+\[Beta]
})*fad[{l,\[Beta]}->-7]//Expand//Simplify


tef[{k_,m2_}->n_]:=(-\[Pi]/\[Delta])/(-n-1)! D[
FCI@Log[
FV[k,p]+I*\[Delta](ed[k[t],k[t]]+m2)
]
,{m2,-n}]/;n<=-1


teq=ted/.fadTmp1->tef//Simplify


(* ::Section:: *)
(*dasf*)


lConeKinematics
ruleTransMomShiftTok


tew=(
ExpandScalarProduct[
FCI@teq/.ruleMomentShiftTok/.ruleTransMomShiftTok
]/.lConeKinematics
)/.FCI@{
ed[FV[k,t],FV[k,t]]->ktr^2,
ed[FV[k,t],FV[\[CapitalDelta]T,t]]->ktr Sqrt[(-4mN^2 \[Xi]^2+(1-\[Xi]^2)(-\[CapitalDelta]2))] Cos[\[Phi]],
ed[FV[\[CapitalDelta]T,t],FV[\[CapitalDelta]T,t]]-> (-4mN^2 \[Xi]^2+(1-\[Xi]^2)(-\[CapitalDelta]2))
}


(* ::Chapter::Closed:: *)
(*test*)


(* ::Input:: *)
(*(*Peskin6.3,Higgs \:73bb\:8272\:5b50\:5bf91\[Dash]loop QED \:9876\:70b9\:7684\:4fee\:6b63*)*)
(*tet=dropSpinor[*)
(*DiracSimplify[*)
(*ScalarProductExpand[*)
(*FCE@DiracSimplify[*)
(*SpinorUBar[p2,mN] . (GS[k+p2-p]+mN) . GA[\[Mu]] . (GS[k]+mN) . SpinorU[p,mN]*)
(*]/.{*)
(*FV[k,\[Mu]]->FV[l,\[Mu]]+x*FV[p,\[Mu]]-y(FV[p2-p,\[Mu]]),*)
(*GS[k]->GS[l]+x*GS[p]-y*GS[p2-p],*)
(*SP[k,p]->SP[l,p]+x*SP[p,p]-y*SP[p2-p,p],*)
(*SP[k,k]->( *)
(*SP[l,l]+x^2*SP[p,p]+y^2*SP[p2-p,p2-p]*)
(*+2(x*SP[l,p]-y*SP[l,p2-p]-x*y*SP[p,p2-p])*)
(*)*)
(*}*)
(*]]]*)


(* ::Input:: *)
(*(*\:6d88\:53bb l \:7684\:5947\:6b21\:5e42*)*)
(*Simplify[*)
(*1/2(Expand[tet/.{l->-l}]+Expand[tet])*)
(*]*)


(* ::Chapter::Closed:: *)
(*backup*)


(* ::Input:: *)
(*(*\:5e94\:7528 Ward \:6052\:7b49\:5f0f\:ff0c\:4ee5\:53ca\:6839\:636e\:5708\:79ef\:5206\:5947\:5076\:6027\:ff0c\:91cf\:7eb2\:63a8\:5bfc\:7684\:66ff\:6362\:89c4\:5219*)*)
(*refineLoop[numerator_]:=Nest[*)
(*(*\:628a\:79ef\:5206\:4e2d\:5206\:5b50\:66ff\:6362\:6210\:5316\:7b80\:7684\:7ed3\:679c,\:4f7f\:7528 Ward-Identity \:7b49\:7ed3\:679c --------*)*)
(*ReplaceAll[ruleIntNumerator],*)
(*(*\:5c55\:5f00\:5206\:5b50\:4e2d\:7684\:6807\:91cf\:4e58\:79ef-----------*)*)
(*ExpandAll2[*)
(*ExpandScalarProduct[*)
(*DiracSimplify[*)
(*(*\:4f5c\:52a8\:91cf\:5e73\:79fbk\[Rule]l\:ff0c\:518d\:6b21\:4f7f\:7528 Dirac\:65b9\:7a0b\:5316\:7b80*)*)
(*FCI@numerator/.ruleMomentShift,*)
(*DiracEquation->True*)
(*(*\:51c6\:5907\:4f7f\:7528 Ward-Identity\:ff0c\:53bb\:6389\:6b63\:6bd4\:4e8e \[CapitalDelta]\[Mu] \:7684\:9879*)*)
(*]/.ruleMomentCombine*)
(*]]*)
(*(*\:91cd\:590d\:66ff\:6362\:4e09\:6b21------*)*)
(*,2]*)


(* ::Input:: *)
(*(*\:666e\:901a\:6570\:5b57\:7684\:4e8c\:6b21\:591a\:9879\:5f0f\:914d\:9f50-----------------*)*)
(*toSquare[expr_,k_,l_]:=Module[*)
(*{a=Coefficient[expr,k,2],*)
(*b=Coefficient[expr,k,1],*)
(*c=Coefficient[expr,k,0]*)
(*},*)
(*(*\:52a8\:91cf\:914d\:9f50\:9700\:8981\:6ee1\:8db3\:7684\:5173\:7cfb---------*)*)
(*{*)
(*l->k+b/(2a),*)
(*k->l-b/(2a),*)
(*-l^2->b^2/(4a)-c*)
(*}*)
(*]*)


(* ::Section::Closed:: *)
(*lConeKinematics,FV,number*)


(* ::Input:: *)
(*lConeKinematics=FCI@{*)
(*(*\:8d28\:5b50\:521d\:6001\:52a8\:91cf-------------------*)*)
(*FV[p1,1]->(1+\[Xi])FV[P,1],*)
(*FV[p1,2]->(mN^2+ed[FV[\[CapitalDelta],3],FV[\[CapitalDelta],3]]/4)/(2(1+\[Xi])FV[P,1]),*)
(*FV[p1,3]->FV[\[CapitalDelta],3]/2,*)
(*(*\:4ecb\:5b50\:521d\:6001\:52a8\:91cf*)*)
(*FV[k,1]->(y+\[Xi])FV[P,1],*)
(*(*k[2]\[Rule]k[2],*)*)
(*FV[k,3]->FV[k,3]+FV[\[CapitalDelta],3]/2,*)
(*(*\:8d28\:5b50\:672b\:6001\:52a8\:91cf------------------*)*)
(*FV[p2,1]->(1-\[Xi])FV[P,1],*)
(*FV[p2,2]->(mN^2+ed[FV[\[CapitalDelta],3],FV[\[CapitalDelta],3]]/4)/(2(1-\[Xi])FV[P,1]),*)
(*FV[p2,3]->-FV[\[CapitalDelta],3]/2,*)
(*(*\:4ecb\:5b50\:672b\:6001\:52a8\:91cf*)*)
(*FV[k2,1]->(y-\[Xi])FV[P,1],*)
(*(*k2[2]\[Rule]k2[2],*)*)
(*FV[k2,3]->FV[k,3]-FV[\[CapitalDelta],3]/2,*)
(**)
(*FV[\[CapitalDelta],1]->\[Xi](2FV[P,1]),*)
(*FV[\[CapitalDelta],2]->(\[CapitalDelta]2+ed[FV[\[CapitalDelta],3],FV[\[CapitalDelta],3]])/(4\[Xi]*FV[P,1])*)
(*(*\[CapitalDelta][3]\[Rule]\[CapitalDelta][3]*)*)
(*};*)


(* ::Section::Closed:: *)
(*lConeKinematics,FV*)


(* ::Input:: *)
(*(*gpd \:4e2d\:4f7f\:7528\:7684\:5149\:9525\:52a8\:91cf\:53c2\:6570\:5316*)*)
(*lConeKinematics=FCI@{*)
(*(*\:8d28\:5b50\:521d\:6001\:52a8\:91cf-------------------*)*)
(*FV[p1,p]->(1+\[Xi])FV[P,p],*)
(*FV[p1,m]->(mN^2+SP[\[CapitalDelta]T,\[CapitalDelta]T]/4)/(2(1+\[Xi])FV[P,p]),*)
(*FV[p1,t]->FV[\[CapitalDelta]T,t]/2,*)
(*(*\:4ecb\:5b50\:521d\:6001\:52a8\:91cf*)*)
(*FV[k1,p]->(y+\[Xi])FV[P,p],*)
(*(*k1[m]\[Rule]k[2],*)*)
(*FV[k1,t]->FV[k,t]+FV[\[CapitalDelta]T,t]/2,*)
(*(*\:8d28\:5b50\:672b\:6001\:52a8\:91cf------------------*)*)
(*FV[p2,p]->(1-\[Xi])FV[P,p],*)
(*FV[p2,m]->(mN^2+SP[\[CapitalDelta]T,\[CapitalDelta]T]/4)/(2(1-\[Xi])FV[P,p]),*)
(*FV[p2,t]->-FV[\[CapitalDelta]T,t]/2,*)
(*(*\:4ecb\:5b50\:672b\:6001\:52a8\:91cf*)*)
(*FV[k2,p]->(y-\[Xi])FV[P,p],*)
(*(*k2[m]\[Rule]k2[m],*)*)
(*FV[k2,t]->FV[k,t]-FV[\[CapitalDelta]T,t]/2,*)
(*(*\[CapitalDelta]=p1-(p2 --------------------*)*)
(*FV[\[CapitalDelta],p]->\[Xi](2FV[P,p]),*)
(*FV[\[CapitalDelta],m]->(\[CapitalDelta]2+SP[\[CapitalDelta]T,\[CapitalDelta]T])/(4\[Xi]*FV[P,p]),*)
(*FV[\[CapitalDelta],t]->\[CapitalDelta]T*)
(*};*)


(* ::Section::Closed:: *)
(*integral*)


(* ::Input:: *)
(*tef[{k_,m2_}->n_]:=(-2\[Pi] I)/(-n-1)! D[*)
(*FCI@Sqrt[*)
(*FV[k,p]^2+ed[k[t],k[t]]+m2*)
(*]*)
(*,{m2,-n}]/;n<=-1*)
