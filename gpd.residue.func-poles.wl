(* ::Package:: *)

(* ::Section:: *)
(*Propagator Dataset*)


deltaPart={
"\[Delta][y-\[Xi]]",
"\[Delta][y+\[Xi]]",
"\[Delta][y,\[Xi]]",
"\[Delta][1-y,1-\[Xi]]"
};


gpdRegion=<|
"itv1"->-\[Xi]<y<\[Xi],
"itv2"->\[Xi]<y<1
|>;


(*\:4f20\:64ad\:5b50 Dataset \:7684 record. 
\:6c42 km \:5373 k[2] \:7684\:7cfb\:6570\:ff0clConeKinematics \:662f\:5177\:4f53\:7684\:5149\:9525\:53c2\:6570\:5316,
ruleReduce \:662f\:5bf9\:7cfb\:6570\:5316\:7b80\:7684\:89c4\:5219, \:6bd4\:5982\:53bb\:6389\:6574\:4f53\:7684 P[1] \:5373 P+.
\:4f20\:64ad\:5b50\:662f k^2-m^2+I \[CurlyEpsilon]\:ff0c\:6240\:4ee5 k- \:6781\:70b9\:7684\:4f4d\:7f6e \:53d6\:51b3\:4e8e \:7cfb\:6570\:7684\:6b63\:8d1f:
\:6b63\:7cfb\:6570\[Rule]\:4e0b\:534a\:5e73\:9762\:ff0c\:8d1f\:7cfb\:6570\[Rule] \:4e0a\:534a\:5e73\:9762, \:7cfb\:6570\:4e3a\:96f6->\:5947\:5f02\:70b9*)
recordPropa[km_,lConeKinematics_,ruleReduce_:{}][propa_->pow_]:=Module[
{mom,kmCoe,poleDistribute},
(*----------*)
mom=First@propa;
kmCoe=Simplify[
Coefficient[(*Echo@*)lightConeSP[mom,mom]/.lConeKinematics,km]
/.ruleReduce];
(*-----------------------------*)
If[pow==0,
Nothing,
<|
"propa"->propa,
"pow"->pow,
"kmCoe"->kmCoe,
(*-------------------*)
"itv1"->Association@Simplify[{
"up"->kmCoe<0,(*pole \:5728\:4e0a\:65b9*)
"down"->kmCoe>0(*pole \:5728\:4e0b\:65b9*)
},
{-\[Xi]<y<\[Xi]&&0<\[Xi]<=1}],
(*-------------------*)
"itv2"->Association@Simplify[{
"up"->kmCoe<0,(*pole \:5728\:4e0a\:65b9*)
"down"->kmCoe>0(*pole \:5728\:4e0b\:65b9*)
},
{\[Xi]<y<1&&0<=\[Xi]<1}]
(*-------------------*)
|>~Join~AssociationThread[
deltaPart,
Simplify[
kmCoe==0,(*k- \:7cfb\:6570\:4e3a\:96f6\:ff0c\:5947\:5f02\:70b9*)
#]&/@{
0<\[Xi]<1&&y==\[Xi]
,0<\[Xi]<1&&y==-\[Xi]
,y==\[Xi]==0
,y==\[Xi]==1
}
]
(*-----------*)
]
]


(*\:751f\:6210\:4f20\:64ad\:5b50\:7684 Dataset *)
dataPropa[paras__][propas__Rule]:=(
recordPropa[paras]/@List@@fad[propas]
)


(* ::Section:: *)
(*find propagator poles*)


(*\:627e\:51fa\:8d39\:66fc\:56fe\:6240\:6709\:4f20\:64ad\:5b50\:7684 poles*)
propaPoles[feynPropgator_]:=(feynPropgator/.
fadTmp1->dataPropa[k[2],lConeKinematics,ruleKmReduce]);


(*\:627e\:51fa\:6240\:6709\:72ec\:7acb\:7684\:79ef\:5206*)
findIntegrate[expr_]:=Module[{vars,coes},
vars=DeleteDuplicates@Cases[expr,
fadTmp1[__],Infinity];
coes=Coefficient[expr,vars];
AssociationThread[vars->coes]
]


(* ::Section:: *)
(*find delta contribution*)


(*\:8f93\:5165\:4f20\:64ad\:5b50\:7684\:6570\:636e\:96c6 {<>...}\:ff0c\:8fd4\:56de\:53ef\:80fd\:7684 Delta \:7ed3\:6784 -----------*)
findDelta[propaDataset_]:=<|
(*\:79ef\:5206\:662f\:5426\:6b63\:6bd4\:4e8e DiracDelta -----*)
"delta"->Query[
Merge[Apply@And]/*
(Select[#,Identity]&)/*Keys,
KeyTake[deltaPart]
]@propaDataset,
(*\:79ef\:5206\:8868\:8fbe\:5f0f+++++++++*)
"int"->fad@@Query[All,
#@"propa"->#@"pow"&
]@propaDataset
|>


buildDeltaAssoc[key_,val_]:=Module[
{deltaRes=findDelta@propaPoles@key},
If[
deltaRes@"delta"==={},
Nothing,
Append["coe"->val]@deltaRes
]
]
(*----------------*)
picDeltaContrib[propaCoesAssoc_]:=KeyValueMap[
buildDeltaAssoc]@propaCoesAssoc
