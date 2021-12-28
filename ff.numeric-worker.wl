(* ::Package:: *)

(* ::Title:: *)
(*ff.numeric-worker.wl*)


(* ::Chapter:: *)
(*initiate*)


(* ::Section:: *)
(*Couplings*)


(*------------------- \:7cfb\:6570\:5934\:90e8\:7684\:663e\:5f0f rule -------------------*)
fyCoesRule={fyCoe->Times,vtxCoe->Identity};


(*------------------- \:5c06\:8026\:5408\:5e38\:6570\:7684\:5177\:4f53\:6570\:503c\:4ee3\:5165 -------------------*)
numCoupLst={
\[CapitalLambda]->$LambdaNum,(*\:6b63\:89c4\:5b50\:80fd\:6807\:53c2\:6570*)
cc["f"]->0.093`30,cc["D"]->0.76`30, cc["F"]->0.50`30,
cc["b9"]->1.36`30,cc["b10"]->1.24`30,cc["b11"]->0.46`30,
Sequence@@fyCoesRule,
(* c1,c2, C \:7684\:503c*)
Sequence@@magCCRelation
};


medRule::usage="medRule[x], \:4f20\:5165\:4e00\:4e2a\:5173\:8054,\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:7684\:66ff\:6362\:89c4\:5219, \:52a0\:4e0a\:5404\:79cd\:8026\:5408\:5e38\:6570";
(*\:5982\:679c\:4e0d\:662f\:62df\:5408,\:53ef\:4ee5\:7a0d\:5fae\:964d\:4f4e\:7cbe\:5ea6*)
medRule[x_]:=Dispatch[SetPrecision[Merge[
{KeyTake[x,{mE,mm1,mo1,mo2,md1,md2}]/.numMass,numMass,numCoupLst},Last],$precision]];
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
If[$parallel$couplsQ,
ParallelEvaluate[ReleaseHold@paraInitial];
DistributeDefinitions[
$srcRoot,$fileName,echo,enList,enString,$inNBook,
$parOrdStr,$LambdaNumStr,$parCStr,$fitScheme,$LambdaFit,
coesDir,mfilesDir,fyAmpPart,
massV,numMass,numCoupLst,numPaVe,otherCoes,
quaCharge,medRule,toGEGM,
$chopLimit,chop,$precision
];]


(* ::Section:: *)
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


(*tag \:662f\:6307\:5b9a\:7684\:8d39\:66fc\:56fe,$parOrdStr \:662f\:5708\:79ef\:5206\:7684\:7ea7\:6570\:7684 order*)
import$Eva[tag_,$parOrdStr_]:=Block[{coes,expr,assocLst},
coes=Import[FileNameJoin[{coesDir,"coe.chpt."<>StringRiffle[tag,"."]<>".wdx"}]];
expr={Import[FileNameJoin[{mfilesDir,"analytic.strange."<>$parOrdStr<>"."<>StringRiffle[tag,"."]<>".wdx"}]]};
If[$parOrdStr===$ordFull,echo[DateString[],": ",tag]];
(* JoinAcross \:5229\:7528\:516c\:5171\:7684 chpt \:8d39\:66fc\:56fe tag,\:8fde\:63a5\:7cfb\:6570\:4e0e\:5708\:79ef\:5206\:8868\:8fbe\:5f0f *)
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


(*+++++++++ \:7b5b\:9009\:51fa\:7279\:5b9a\:7684\:4fe1\:606f,\:4fbf\:4e8e\:67e5\:770b\:6bcf\:4e2a\:8d39\:66fc\:56fe,\:6bcf\:4e2a\:53cd\:5e94\:9053\:7684\:8d21\:732e +++++++++*)
pickGroup[x_]:=Query[All,<|
Lookup[#,{medMes1,medOct1,medDec1},Nothing]->#[[{
Key@ffsF1F2,Key@ffsGEGM,Key@fyCoeKeycAll
}]]|>&]@x;
(* GroupBy \:8fed\:4ee3\:5206\:7ec4\:7684\:7ed3\:679c\:662f\:4e00\:4e2a\:4e2a\:7684\:5c0f\:7ec4,\:5143\:7d20\:88ab\:653e\:5165\:62ec\:53f7\:4e2d, \:518d\:6b21\:5f62\:6210 {assoc,...} \:7684\:7ed3\:6784,
\:4f7f\:7528 Reduce \:51fd\:6570\:5904\:7406\:8fd9\:4e9b\:7ed3\:679c: \:5148\:901a\:8fc7 Query \:5728\:5c0f\:7ec4\:4e2d\:53d6\:51fa\:7279\:5b9a\:7684\:952e,\:5728\:8fd9\:91cc\:662f F1F2,GEGM \:7684\:6570\:503c\:7ed3\:679c,
\:53d6\:51fa\:7684\:7ed3\:679c\:4ecd\:662f {assoc,...} \:7684\:7ed3\:6784,\:518d\:4f7f\:7528 Merge \:51fd\:6570\:5408\:5e76\:5c0f\:7ec4\:7684\:7ed3\:679c,\:4f20\:5165 Total \:51fd\:6570\:6267\:884c\:6c42\:548c. 
\:8fd9\:91cc\:7684\:6c42\:548c,\:5c06\:5355\:4e2a\:8d39\:66fc\:56fe\:4e2d\:4e0d\:540c\:7684\:53cd\:5e94\:9053\:76f8\:52a0.
*)
sumGroup[x_?ListQ]:=Merge[Query[All,{Key@ffsF1F2,Key@ffsGEGM}]@x,Total];
(*GroupBy \:6309\:7167\:5217\:8868\:4e2d\:7684\:5206\:7c7b\:51fd\:6570,\:751f\:6210\:4e00\:4e2a\:5d4c\:5957\:5173\:8054,\:8fd9\:91cc\:662f\:6309{\:5165\:5c04\:7c92\:5b50,\:8d39\:66fc\:56fe}, 
\:7136\:540e\:5c06 sumGroup \:4f5c\:4e3a Reduce \:51fd\:6570\:4f5c\:7528\:5230\:6700\:7ec8\:5c42\:7684\:5c0f\:7ec4\:4e0a,\:8fd9\:91cc\:53ea\:63d0\:53d6\:4e86 F1F2,GEGM*)


(* \:751f\:6210\:4e00\:4e2a\:5173\:8054: <|loopChan,loopChanSum,loopAmpSum|>*)
loopResuGather[numAssoc_]:=Module[{loopChanSum},
<|
(*\:7c92\:5ea6\:4e3a,\:6bcf\:4e2a\:8d39\:66fc\:56fe\:7684\:6bcf\:4e2a\:53cd\:5e94\:9053*)
kLoopChannel->Query[
GroupBy[#,{Key@inOct,Key@chTagKey["chTag"]},pickGroup]&
]@Catenate@numAssoc,
(*\:7c92\:5ea6\:4e3a,\:5c06\:540c\:4e00\:8d39\:66fc\:56fe\:4e0b\:7684\:53cd\:5e94\:9053\:6c42\:548c*)
kLoopChanSum->(loopChanSum=Query[
GroupBy[#,{Key@inOct,Key@chTagKey["chTag"]},sumGroup]&
]@Catenate@numAssoc),
(*\:7c92\:5ea6\:4e3a,\:5c06\:540c\:4e00\:4e2a\:5165\:5c04\:7c92\:5b50\:4e0b\:7684\:6240\:6709\:8d39\:66fc\:56fe\:7ed3\:679c\:76f8\:52a0*)
kLoopAmpSum->Query[All,
Simplify[Merge[Values@#,Total]]&
]@loopChanSum
|>]
(* loopAmpSum \:7684\:5927\:81f4\:7ed3\:6784\:5982\:4e0b\:ff1a
\[LeftAssociation]fd[2,1,0]\[Rule]\[LeftAssociation]Total"\[Rule]\[LeftAssociation]ffsF1F2\[Rule]{F1,F2},ffsGEGM\[Rule]{GE,GM}\[RightAssociation]\[RightAssociation],
(\:5176\:4ed6\:7c92\:5b50\:7684\:7ed3\:679c,\:7ed3\:6784\:7c7b\:4f3c)\[RightAssociation]*)


(*\:6839\:636e\:662f\:5426\:9700\:8981\:5e76\:884c,\:8c03\:7528\:4e0a\:9762\:7684\:51fd\:6570,\:4ee3\:5165\:8026\:5408\:5e38\:6570,\:8ba1\:7b97\:5708\:79ef\:5206\:548c\:7cfb\:6570*)
loopResults[$parOrdStr_]:=Module[{numAssoc},
numAssoc=If[$parallel$couplsQ,
(* HoldAll \:6240\:6709\:8868\:8fbe\:5f0f,\:4f20\:7ed9\:5b50\:6838\:8ba1\:7b97*)
SetAttributes[paraEnvIO,HoldAll];
paraEnvIO[tag_]:=ParallelSubmit[import$Eva[tag,$parOrdStr]];
(*\:5e76\:884c\:60c5\:51b5,\:9700\:8981 WaitAll \:4efb\:52a1*)
WaitAll[paraEnvIO/@fyAmpPart],
(*\:975e\:5e76\:884c\:60c5\:51b5*)
paraEnvIO[tag_]:=import$Eva[tag,$parOrdStr];
paraEnvIO/@fyAmpPart];
(*\:8fd4\:56de\:5708\:56fe\:7ed3\:679c\:7684\:5173\:8054,<|loopChans,loopChansSum,loopAmpSum,loopAssoc|>*)
Append[loopResuGather[numAssoc],
kLoopAssoc->numAssoc]]


loopResults["v"]=If[$parOrdStr===$ord0,
(*\:5982\:679c\:8ba1\:7b97 ord0 \:7684\:7ed3\:679c,\:53ea\:7528\:8ba1\:7b97 ord0\:672c\:8eab*)
<|$ord0->loopResults[$ord0]|>,
(*\:5982\:679c\:8ba1\:7b97 ord1,ordFull \:7684\:7ed3\:679c,\:9700\:8981\:989d\:5916\:8ba1\:7b97 ord0 *)
<|$ord0->loopResults[$ord0],
$parOrdStr->loopResults[$parOrdStr]|>];


(* ::Chapter:: *)
(*merge tree & loop*)


(* \:5c06 \:6811\:56fe\:548c\:5708\:56fe\:7684\:7ed3\:679c\:653e\:5728\:4e00\:8d77,\:5e76\:5c55\:5e73\:5d4c\:5957\:7ed3\:6784*)
ffsMerged["fn"][treeSum_Association,loopAmpSum_Association]:=Merge[{treeSum,loopAmpSum},<|
(*Lookup[{assoc1,assoc_2,\[Ellipsis]},key], \:5206\:522b\:7ed9\:51fa\:6bcf\:4e2a\:5173\:8054\:4e2d\:5bf9\:5e94 key \:7684\:503c*)
AssociationThread[{ffsTreeF1F2,ffsLoopF1F2},Lookup[#,ffsF1F2]],
AssociationThread[{ffsTreeGEGM,ffsLoopGEGM},Lookup[#,ffsGEGM]],
KeyTake[First@#,{inOct}](*First@# \:4e3a treeSum *)
|>&]


(*\:540c\:6837,\:6839\:636e $parOrdStr \:7684\:503c,\:5224\:65ad\:662f\:5426\:989d\:5916\:8ba1\:7b97 ord0 *)
ffsMerged["v",keyTreeAndLoop]=If[$parOrdStr===$ord0,
<|$ord0->ffsMerged["fn"][treeSum,
Query[$ord0,kLoopAmpSum]@loopResults["v"]
]|>,
<|$ord0->ffsMerged["fn"][treeSum,
Query[$ord0,kLoopAmpSum]@loopResults["v"]
],
$parOrdStr->ffsMerged["fn"][treeSum,
Query[$parOrdStr,kLoopAmpSum]@loopResults["v"]
]|>];


(* ::Section:: *)
(*renorm const*)


(*\:6311\:9009\:51fa tree,loop f1 \:7684\:8d21\:732e,\:8ba1\:7b97\:91cd\:6b63\:5316\:5e38\:6570*)
fnRenorm[fd_,quarkQ_]:=Abs@chopQ2[
ffsMerged["v",keyTreeAndLoop][[Key@$ord0,Key@fd,Key@ffsTreeF1F2,1]]-
ffsMerged["v",keyTreeAndLoop][[Key@$ord0,Key@fd,Key@ffsLoopF1F2,1]]/.quarkQ];
(*\:5bf9\:4e8e\:5e26\:7535\:91cd\:5b50,\:4f7f\:7528\:81ea\:5df1\:7684 F1 \:8ba1\:7b97\:573a\:5f3a\:91cd\:6b63\:5316\:5e38\:6570 *)
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
ffsMerged["WithRen",keyTreeAndLoop]=Query[All,All,
Append[#,recon->renormRule@#@inOct]&
]@ffsMerged["v",keyTreeAndLoop];
