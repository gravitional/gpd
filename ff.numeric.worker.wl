(* ::Package:: *)

(* ::Title:: *)
(*ff.numeric.wl.worker*)


(* ::Chapter:: *)
(*initiate*)


(* ::Section:: *)
(*Read coes and exprs*)


If[$parallelQ,
(*+++++++++++++++++++ \:542f\:52a8\:5e76\:884c\:5185\:6838 +++++++++++++++++++*)
Needs["X`"];ParallelNeeds["X`"];
CloseKernels[];(*\:542f\:52a8\:5e76\:884c\:5185\:6838*)
Switch[{$MachineName,$System},
{"OP7050","Linux x86 (64-bit)"},LaunchKernels[6],
_,LaunchKernels[]
];,
Needs["X`"];]


coesDir=FileNameJoin[{$srcRoot,"coes"}];
mfilesDir=FileNameJoin[{$srcRoot,"mfiles"}];
(* 1st \:5bfc\:5165\:6b21\:5e8f: \:8bfb\:5165\:6392\:7248 *)
(*Once@Get["gen.format.wl"];*)
(*\:5bfc\:5165\:6240\:6709\:8d39\:66fc\:56fe tag \:7684\:5217\:8868: fyAmpLoopLst,fyAmpTreeLst*)
Once@Get["gen.integral.TagList.wl"];
fyAmpPart=fyAmpLoopLst;
(*\:8bfb\:5165\:5404\:79cd\:8f93\:5165\:63a5\:53e3*)
Once@Get["coes.interface.wl"];


(*\:58f0\:660e\:4e00\:4e9b\:8fd0\:52a8\:5b66\:53d8\:91cf,\:4f7f\:7528 atom \:8868\:8fbe\:5f0f\:ff0cpackage-X \:7684 loopIntegrate \:9700\:8981\:79ef\:5206\:52a8\:91cf\:4e3a\:539f\:5b50\:8868\:8fbe\:5f0f*)
\[CapitalLambda];(*\:6b63\:89c4\:5b50\:8d28\:91cf*)
mE;(*\:5916\:817f\:7c92\:5b50\:7684\:8d28\:91cf*);
mm1;mm2;(*\:4e2d\:95f4\:516b\:91cd\:6001\:4ecb\:5b50*)
mo1;mo2;(*\:4e2d\:95f4\:516b\:91cd\:6001\:91cd\:5b50*)
md1;md2;(*\:4e2d\:95f4\:5341\:91cd\:6001\:91cd\:5b50*)
Q2;(*Q2=-q^2,\:8f6c\:79fb\:52a8\:91cf\:5e73\:65b9\:7684\:8d1f\:503c*)


(* ::Section:: *)
(*Kinematic & Couplings*)


(*------------------- \:5c06\:8026\:5408\:5e38\:6570\:7684\:5177\:4f53\:6570\:503c\:4ee3\:5165 -------------------*)
numCoupLst={
cc["f"]->0.093`20,
cc["D"]->0.76`20, cc["F"]->0.50`20,
cc["b9"]->1.36,cc["b10"]->1.24,cc["b11"]->0.46,
\[CapitalLambda]->$par\[CapitalLambda],
Sequence@@coesRule,
(* c1,c2, C \:7684\:503c*)
Sequence@@numCCC
};


medRule::usage="medRule[x], \:4f20\:5165\:4e00\:4e2a\:5173\:8054\:ff0c\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:7684\:66ff\:6362\:89c4\:5219, \:52a0\:4e0a\:5404\:79cd\:8026\:5408\:5e38\:6570";
medRule[x_]:=Dispatch@Merge[{KeyTake[x,{mE,mm1,mo1,mo2,md1,md2}]/.numMass,numMass,numCoupLst},Last];
toGEGM::usage="toGEGM[x], \:4f20\:5165 {F1,F2} ";
toGEGM[x_?ListQ]:={First[x]-Q2/(4*mE)*Last[x], Total[x]};


paraInitial=Hold[
(*\:8bbe\:7f6e\:5316\:7b80\:65f6\:95f4\:9650\:5236, \:5173\:95ed Simplify \:5316\:7b80\:65f6\:95f4\:8d85\:51fa \:7684\:4fe1\:606f*)
SetOptions[Simplify,TimeConstraint->1];
SetOptions[Refine,TimeConstraint->1];
Off[Simplify::time];Off[Refine::time];
];
ReleaseHold@paraInitial


(*\:5e76\:884c\:8ba1\:7b97\:521d\:59cb\:5316*)
If[$parallelQ,
ParallelEvaluate[ReleaseHold@paraInitial];
DistributeDefinitions[
$srcRoot,$fileName,echo,enList,enString,$inNBook,
$parOrdStr,$par\[CapitalLambda]Str,$parCStr,$fitScheme,$erroBar,
coesDir,mfilesDir,fyAmpPart,
massV,numMass,numCoupLst,numPaVe,otherCoes,
quaCharge,medRule,toGEGM,
chopLimit,chop,precision
];]


(* ::Chapter:: *)
(*tree level contributions*)


treeFsGs=Query[All,
(*\:6dfb\:52a0 \:5f62\:72b6\:56e0\:5b50 F1F2*)
(Append[#,ffsF1F2->Simplify[chop[{#@fyCoeKeycAllF1,#@fyCoeKeycAllF2}/.medRule[#]]]]&)/*
(*\:6dfb\:52a0 \:5f62\:72b6\:56e0\:5b50 GEGM *)
(Append[#,ffsGEGM->Simplify[chop[{#@fyCoeKeycAllF1,#@fyCoeKeycAllF2}/.medRule[#]]]]&)
]@Import[FileNameJoin[{coesDir,"coe.chpt."<>StringRiffle[#,"."]<>".wdx"}]]&@fyAmpTree;


(*\:53ea\:9009\:51fa\:521d\:672b\:6001\:4e3a\:76f8\:540c\:91cd\:5b50\:7684\:8bb0\:5f55*)
sameOctQ[assoc_]:=SameQ[assoc@inOct,assoc@outOct/.conjOct];
(*++++++++++++++++++++++++++  \:63d0\:53d6\:51fa\:6811\:56fe\:9636\:7684\:6570\:503c\:7ed3\:679c  +++++++++++++++++++++++++++++++++++*)
treeSum=Association@Query[Select@sameOctQ,
#@inOct->KeyTake[#,{inOct,ffsF1F2,ffsGEGM}]&]@treeFsGs;


(* ::Chapter:: *)
(*Loops numeric*)


(* ::Section:: *)
(*parallel IO*)


import$Eva[tag_]:=Block[{coes,expr,assocLst},
coes=Import[FileNameJoin[{coesDir,"coe.chpt."<>StringRiffle[tag,"."]<>".wdx"}]];
expr={Import[FileNameJoin[{mfilesDir,"analytic.strange."<>$parOrdStr<>"."<>StringRiffle[tag,"."]<>".wdx"}]]};
(* JoinAcross \:5229\:7528\:516c\:5171\:7684 chpt \:8d39\:66fc\:56fe tag\:ff0c\:8fde\:63a5\:7cfb\:6570\:4e0e\:5708\:79ef\:5206\:8868\:8fbe\:5f0f *)
assocLst=JoinAcross[coes,expr,Key@chTagKey["chTag"]];
(*++++++++++++++++++++++++ \:6dfb\:52a0\:5404\:79cd\:5b57\:6bb5 ++++++++++++++++++++++++*)
Query[All,
(*-------------- \:6dfb\:52a0\:4fee\:6b63\:8fc7\:7684\:7cfb\:6570 --------------*)
(Append[#,fyCoeKeycAll->Times[
#@fyCoeKeycAll,otherCoes[#@chTagKey["chTag"]],I/(16\[Pi]^2)
]]&)/*
(*-------------- \:5708\:79ef\:5206*\:7cfb\:6570,\:5e76\:6570\:503c\:5316 --------------*)
(Append[#,
ffsF1F2->Simplify[chop[Times[
Normal[#@ffsF1F2]/.numPaVe,#@fyCoeKeycAll
]/.medRule[#]]]
]&)/*
(*-------------- \:7531 F1F2 expr \:7ebf\:6027\:7ec4\:5408\:51fa GE,GM --------------*)
(Append[#,ffsGEGM->(toGEGM[#@ffsF1F2]/.medRule[#])]&)/*
(*-------------- \:5220\:9664\:5197\:4f59\:5b57\:6bb5 --------------*)
KeyDrop[{
mm1,mo1,mo2,md1,md2,"time",fyCoeKey["cStr"],fyCoeKey["cEM"]
}]
]@assocLst]


(*\:5bfc\:5165\:7cfb\:6570\:548c\:5708\:79ef\:5206*)
If[$parallelQ,
SetAttributes[paraEnvIO,HoldAll];
paraEnvIO[tag_]:=ParallelSubmit[import$Eva[tag]],
paraEnvIO[tag_]:=import$Eva[tag]
]


(*++++++++++++++++++++ \:5e76\:884c\:8ba1\:7b97 ++++++++++++++++++++*)
If[$parallelQ,
numAssoc=WaitAll[paraEnvIO/@fyAmpPart],(*\:5e76\:884c\:60c5\:51b5\:ff0c\:9700\:8981 WaitAll \:4efb\:52a1*)
numAssoc=paraEnvIO/@fyAmpPart
];


(*+++++++++ \:7b5b\:9009\:51fa\:7279\:5b9a\:7684\:4fe1\:606f\:ff0c\:4fbf\:4e8e\:67e5\:770b\:6bcf\:4e2a\:8d39\:66fc\:56fe\:ff0c\:6bcf\:4e2a\:53cd\:5e94\:9053\:7684\:8d21\:732e +++++++++*)
pickGroup[x_]:=Query[All,<|
Lookup[#,{medMes1,medOct1,medDec1},Nothing]->#[[{
Key@ffsF1F2,Key@ffsGEGM,Key@fyCoeKeycAll
}]]|>&]@x;
loopChans=Query[
GroupBy[#,{Key@inOct,Key@chTagKey["chTag"]},pickGroup]&
]@Catenate@numAssoc;


(* GroupBy \:8fed\:4ee3\:5206\:7ec4\:7684\:7ed3\:679c\:662f\:4e00\:4e2a\:4e2a\:7684\:5c0f\:7ec4,\:5143\:7d20\:88ab\:653e\:5165\:62ec\:53f7\:4e2d, \:518d\:6b21\:5f62\:6210 {assoc,...} \:7684\:7ed3\:6784\:ff0c
\:4f7f\:7528 Reduce \:51fd\:6570\:5904\:7406\:8fd9\:4e9b\:7ed3\:679c: \:5148\:901a\:8fc7 Query \:5728\:5c0f\:7ec4\:4e2d\:53d6\:51fa\:7279\:5b9a\:7684\:952e\:ff0c\:5728\:8fd9\:91cc\:662f F1F2,GEGM \:7684\:6570\:503c\:7ed3\:679c,
\:53d6\:51fa\:7684\:7ed3\:679c\:4ecd\:662f {assoc,...} \:7684\:7ed3\:6784\:ff0c\:518d\:4f7f\:7528 Merge \:51fd\:6570\:5408\:5e76\:5c0f\:7ec4\:7684\:7ed3\:679c\:ff0c\:4f20\:5165 Total \:51fd\:6570\:6267\:884c\:6c42\:548c. 
\:8fd9\:91cc\:7684\:6c42\:548c,\:5c06\:5355\:4e2a\:8d39\:66fc\:56fe\:4e2d\:4e0d\:540c\:7684\:53cd\:5e94\:9053\:76f8\:52a0.
*)
sumGroup[x_?ListQ]:=Merge[Query[All,{Key@ffsF1F2,Key@ffsGEGM}]@x,Total];
(*GroupBy \:6309\:7167\:5217\:8868\:4e2d\:7684\:5206\:7c7b\:51fd\:6570\:ff0c\:751f\:6210\:4e00\:4e2a\:5d4c\:5957\:5173\:8054,\:8fd9\:91cc\:662f\:6309{\:5165\:5c04\:7c92\:5b50,\:8d39\:66fc\:56fe}, 
\:7136\:540e\:5c06 sumGroup \:4f5c\:4e3a Reduce \:51fd\:6570\:4f5c\:7528\:5230\:6700\:7ec8\:5c42\:7684\:5c0f\:7ec4\:4e0a*)
loopChanSum=Query[
GroupBy[#,{Key@inOct,Key@chTagKey["chTag"]},sumGroup]&
]@Catenate@numAssoc;
(*+++++++++++++++++++ \:8fd9\:91cc\:7684\:6c42\:548c\:ff0c\:5c06\:6bcf\:4e2a\:5165\:5c04\:7c92\:5b50\:7684\:ff0c\:6240\:6709\:8d39\:66fc\:56fe\:7684\:7ed3\:679c\:76f8\:52a0 +++++++++++++++++++*)
loopAmpSum=Query[All,
Simplify[Merge[Values@#,Total]]&
]@loopChanSum;
(* \:6700\:7ec8\:7ed3\:679c\:7684\:7ed3\:6784\:5927\:81f4\:5982\:4e0b\:ff1a
\[LeftAssociation]fd[2,1,0]\[Rule]\[LeftAssociation]Total"\[Rule]\[LeftAssociation]ffsF1F2\[Rule]{F1,F2},ffsGEGM\[Rule]{GE,GM}\[RightAssociation]\[RightAssociation],
(\:5176\:4ed6\:7c92\:5b50\:7684\:7ed3\:679c,\:7ed3\:6784\:7c7b\:4f3c)\[RightAssociation]*)


(* ::Chapter:: *)
(*renorm const*)


(* \:5c06 \:6811\:56fe\:548c\:5708\:56fe\:7684\:7ed3\:679c\:653e\:5728\:4e00\:8d77\:ff0c\:5e76\:5c55\:5e73\:5d4c\:5957\:7ed3\:6784*)
ffsMerged=Merge[{treeSum,loopAmpSum},<|
AssociationThread[{ffsTreeF1F2,ffsLoopF1F2},Lookup[#,ffsF1F2]],
AssociationThread[{ffsTreeGEGM,ffsLoopGEGM},Lookup[#,ffsGEGM]],
KeyTake[First@#,{inOct}](*First@# \:4e3a treeSum *)
|>&];


(*\:6311\:9009\:51fa tree,loop f1 \:7684\:8d21\:732e\:ff0c\:8ba1\:7b97\:91cd\:6b63\:5316\:5e38\:6570*)
fnRenorm[fd_,quarkQ_]:=Abs@chopQ2[
ffsMerged[[Key@fd,Key@ffsTreeF1F2,1]]-
ffsMerged[[Key@fd,Key@ffsLoopF1F2,1]]/.quarkQ];
(*\:5bf9\:4e8e\:5e26\:7535\:91cd\:5b50\:ff0c\:4f7f\:7528\:81ea\:5df1\:7684F1 \:8ba1\:7b97\:573a\:5f3a\:91cd\:6b63\:5316\:5e38\:6570 *)
fnRenormSelf[fd_]:=fd->fnRenorm[fd,quaCharge["uds"]];
(*+++++++++++++++++++++ \:8bb0\:5f55\:91cd\:6b63\:5316\:5e38\:6570 +++++++++++++++++++++*)
renormRule=KeySort@<|
(*----------- \:5e26\:7535 oct \:91cd\:5b50 -----------*)
fnRenormSelf@fd[2,1,0],
fnRenormSelf@fd[2,3,0],
fnRenormSelf@fd[2,5,0],
fnRenormSelf@fd[2,7,0],
(*----------- \:4e2d\:6027 oct \:91cd\:5b50 -----------*)
fd[2,2,0]->fnRenorm[fd[2,1,0],quaCharge["uds"]],
fd[2,4,0]->fnRenorm[fd[2,3,0],quaCharge["uds"]],
fd[2,6,0]->fnRenorm[fd[2,7,0],quaCharge["uds"]],
fd[2,8,0]->fnRenorm[fd[2,8,0],quaCharge["u"]](*\:4f7f\:7528 u flavor EM\:6d41\:8ba1\:7b97*)
|>;


(*+++++++++++++++++ \:52a0\:4e0a\:91cd\:6b63\:5316\:5e38\:6570 *+++++++++++++++++*)
ffsWithRen=Query[All,
Append[#,recon->renormRule@#@inOct]&
]@ffsMerged;


(*\:6253\:5370\:573a\:5f3a\:91cd\:6b63\:5316\:5e38\:6570*)
If[!$fittingQ,
Query[Values/*StringRiffle,Key@recon,chopQ2/*(N[#,4]&)]@ffsWithRen
]


(* ::Input:: *)
(*(*\:624b\:52a8\:67e5\:770b recons*)*)
(*Query[All,{Key@recon}]@ffsWithRen*)


numFFs=If[!$fittingQ,
Query[All,<|
tagNum["tr","uds"]->chopQ2Val[#@ffsTreeGEGM/.quaCharge["uds"]],
tagNum["tr","u"]->chopQ2Val[#@ffsTreeGEGM/.quaCharge["u"]],
tagNum["tr","d"]->chopQ2Val[#@ffsTreeGEGM/.quaCharge["d"]],
tagNum["tr","s"]->chopQ2Val[#@ffsTreeGEGM/.quaCharge["s"]],
(*loop*)
tagNum["lo","uds"]->chopQ2Val[#@ffsLoopGEGM/.quaCharge["uds"]],
tagNum["lo","u"]->chopQ2Val[#@ffsLoopGEGM/.quaCharge["u"]],
tagNum["lo","d"]->chopQ2Val[#@ffsLoopGEGM/.quaCharge["d"]],
tagNum["lo","s"]->chopQ2Val[#@ffsLoopGEGM/.quaCharge["s"]],
(*total = tree +(Z-1)*tree+loop*)
tagNum["tr+lo","uds"]->chopQ2Val[
#@recon*#@ffsTreeGEGM+#@ffsLoopGEGM/.quaCharge["uds"]]
|>&
]@ffsWithRen,
(*-------------- fitting \:4f7f\:7528\:7684\:8868\:8fbe\:5f0f --------------*)
Query[All,<|
(*total = tree +(Z-1)*tree+loop*)
tagNum["tr+lo","uds"]->chopQ2Val[
#@recon*#@ffsTreeGEGM+#@ffsLoopGEGM/.quaCharge["uds"]]
|>&
]@ffsWithRen];
