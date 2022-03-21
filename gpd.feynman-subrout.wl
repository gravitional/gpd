(* ::Package:: *)

(* ::Title:: *)
(*Feynman subroutine*)


(* ::Section:: *)
(*propagators analysis*)


(*\:4f20\:64ad\:5b50\:72ec\:7acb\:7684\:57fa, p^2-m^2: p or m \:4e0d\:540c --------------*)
feynIntBasis=Query[All
,(Key@"basis")
]@splitPropag;
(*\:6240\:6709\:4f20\:64ad\:5b50\:5e42\:6b21\:4e4b\:548c ---------------*)
feynIntPowerSum=Query[Total
,(Key@"power")
]@splitPropag;
(*\:72ec\:7acb\:4f20\:64ad\:5b50\:7684\:4e2a\:6570---------------------*)
feynIntBasisNum=splitPropag//Length;


(*\:5c06\:8d39\:66fc\:53c2\:6570\:5316\:4e4b\:540e\:7684\:4f20\:64ad\:5b50\:914d\:9f50\:6210 l^2-\[Beta] *)
splitPropagSquared=Simplify@ExpandScalarProduct[
toSquare[
(*\:8d39\:66fc\:53c2\:6570\:5316*)
feynmanParameter@feynIntBasis
,k1,l]];


(*\:8d39\:66fc\:53c2\:6570\:5316,\:5e73\:79fb\:79ef\:5206\:53d8\:91cf, k\[Rule]l -------*)
ruleMomentShift=FCI[Last@splitPropagSquared];
(*\:52a8\:91cf\:5e73\:79fb, l\[Rule]k *)
ruleMomentShiftBack=FCI@splitPropagSquared[[2]];
(*\:5408\:5e76\:4f20\:64ad\:5b50\:4e4b\:540e,\:5708\:79ef\:5206\:5206\:6bcd l^2-\[Beta] \:7684\:5e38\:6570\[Beta],  *)
denominator=First@splitPropagSquared-FCI@SP[l,l];
(* \:6a2a\:5411\:52a8\:91cf\:7684\:66ff\:6362\:89c4\:5219------------------- *)
ruleTransMomShiftBack=KeyValueMap[
RuleDelayed@@{
(*Key of Rule*)
#1/.{Momentum[l_]:>l[t]},
(*Value of Rule*)
#2/.{Momentum[p_]:>FCI@FV[p,t]}
}&
]@Association@ruleMomentShiftBack;


ruleIntNumerator={
(*Ward \:6052\:7b49\:5f0f, \[CapitalDelta]\[Mu].\[CapitalGamma]\[Mu]=0-----------*)
Pair[LorentzIndex[\[Mu]],Momentum[\[CapitalDelta]]]->0,

(*\:5708\:79ef\:5206\:5bf9\:79f0\:6027: (l.p1)(l.p2)\[Rule]1/4(l^2)(p1.p2) -------*)
Pair[p1_,Momentum[l]] Pair[p2_,Momentum[l]]/;!MemberQ[{p1,p2},Momentum[l]]:>1/4*
Pair[p1,p2] Pair[Momentum[l],Momentum[l]],
(*\:5708\:79ef\:5206\:5bf9\:79f0\:6027: (\[Gamma].l)(l.p)\[Rule]1/4(l^2)(p.\[Gamma]) ------*)
DiracGamma[Momentum[l]] Pair[p_,Momentum[l]]:>1/4*
DiracGamma[p] Pair[Momentum[l],Momentum[l]]

(*Ward \:6052\:7b49\:5f0f,\:4ee5\:53ca\:72c4\:62c9\:514b\:65b9\:7a0b------------*)
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


(*\:4f7f\:7528\:72c4\:62c9\:514b\:65b9\:7a0b\:ff0c\:5708\:79ef\:5206\:5bf9\:79f0\:6027\:ff0c\:5316\:7b80\:5708\:79ef\:5206\:7684\:7ed3\:679c--------------*)
splitSpinTmp=Simplify@
refineLoop@
splitSpin;


(*\:53d8\:91cf\:5e73\:79fb\:4e4b\:540e\:ff0c\:5206\:6bcd\:662f\:5708\:52a8\:91cfl\:7684\:5076\:51fd\:6570\:ff0c\:6d88\:53bb\:5206\:5b50\:4e2d l \:7684\:5947\:6570\:6b21\:9879*)
symmetryLoop[expr_]:=ExpandAll2[1/2(
expr+
(FCI@expr/.{Momentum[l]->Momentum[-l]})
)]


split[{fyTag,"FAFB"}]=Module[{
(*\:8003\:8651\:5708\:79ef\:5206\:7684\:5947\:5076\:6027*)
expr=symmetryLoop@splitSpinTmp
},
(*\:63d0\:53d6\:5316\:7b80\:540e\:5f97\:5230\:7684\:5404\:79cd\:7ed3\:6784*)
<|
"expr"->Collect[expr,FCI/@{FV[P,\[Mu]],GA[\[Mu]]},Simplify],
"P\[Mu]"->Simplify@Coefficient[expr,FCI@FV[P, \[Mu]]],
"\[Gamma]\[Mu]"->Simplify@Coefficient[expr,FCI@GA[\[Mu]]]
|>
];
