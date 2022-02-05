(* ::Package:: *)

(* ::Title:: *)
(*ff.numeric-worker.wl*)


(* ::Chapter:: *)
(*initiate*)


(* ::Section:: *)
(*Couplings*)


(*------------------- \:7cfb\:6570\:5934\:90e8\:7684\:663e\:5f0f rule -------------------*)
fyCoesRule={fyCoe->Times,vtxCoe->Identity
,(*\:521b\:5efa\:7cfb\:6570\:65f6\:ff0c\:6709\:4e9b\:91cf\:4e0d\:5b58\:5728\:ff0c\:8bbe\:7f6e\:4e3a\:96f6*)undefined->0};


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
massV,numMass,numCoupLst,numPaVe,fyCoesOther,
quaCharge,medRule,toGEGM,
$chopLimit,chop,$precision
];]


(* ::Section:: *)
(*coes.chpt*)


fyCoesAssoc=Import@localPath["coes"]["coes.chpt.wdx"];


(* ::Section:: *)
(*tree level contributions*)


treeFsGs=Query[Key@fyAmpTree
,(*{octet}*)All
(*\:6dfb\:52a0 \:5f62\:72b6\:56e0\:5b50 F1F2*)
,(Append[#,ffsF1F2->Simplify[chop[{#@fyCoeKeycAllF1,#@fyCoeKeycAllF2}/.medRule[#]]]]&)/*
(*\:6dfb\:52a0 \:5f62\:72b6\:56e0\:5b50 GEGM *)
(Append[#,ffsGEGM->Simplify[chop[{#@fyCoeKeyGE,#@fyCoeKeyGM}/.medRule[#]]]]&)
]@fyCoesAssoc;


(*\:53ea\:9009\:51fa\:521d\:672b\:6001\:4e3a\:76f8\:540c\:91cd\:5b50\:7684\:8bb0\:5f55*)
sameOctQ[assoc_]:=SameQ[assoc@inOct,assoc@outOct/.conjOct];
(*\:63d0\:53d6\:51fa\:6811\:56fe\:9636\:7684\:6570\:503c\:7ed3\:679c  +++++++++++++++++++++++++++++++++++*)
treeSum=Association@Query[Select@sameOctQ,
#@inOct->KeyTake[#,{inOct,ffsF1F2,ffsGEGM}]&]@treeFsGs;


(* ::Chapter:: *)
(*Loops numeric*)


(* ::Section:: *)
(*parallel IO*)


(*tag \:662f\:6307\:5b9a\:7684\:8d39\:66fc\:56fe,$parOrdStr \:662f\:5708\:79ef\:5206\:7684\:7ea7\:6570\:7684 order*)
import$Eva[tag_,$parOrdStr_]:=Block[{coes,expr,assocLst},
(*coes=Import[FileNameJoin[{coesDir,"coe.chpt."<>StringRiffle[tag,"."]<>".wdx"}]];*)
coes=fyCoesAssoc@tag;
expr={Import[FileNameJoin[{mfilesDir,"analytic.strange."<>$parOrdStr<>"."<>StringRiffle[tag,"."]<>".wdx"}]]};
If[$parOrdStr===$ordFull,echo[DateString[],": ",tag]];
(* JoinAcross \:5229\:7528\:516c\:5171\:7684 chpt \:8d39\:66fc\:56fe tag,\:8fde\:63a5\:7cfb\:6570\:4e0e\:5708\:79ef\:5206\:8868\:8fbe\:5f0f *)
assocLst=JoinAcross[coes,expr,Key@chTagKey["chTag"]];
Association@Table[
(*++++++++++++++++++++++++ \:6dfb\:52a0\:5404\:79cd\:5b57\:6bb5 ++++++++++++++++++++++++*)
adjust->Query[
(*{channels}*)All
(*-------------- \:6dfb\:52a0\:4fee\:6b63\:8fc7\:7684\:7cfb\:6570 --------------*)
,(Append[#,fyCoeKeycAll->Times[
(*\:8d39\:66fc\:9876\:70b9\:4e58\:79ef*)#@fyCoeKeycAll
,(*\:4f20\:64ad\:5b50\:989d\:5916\:76f8\:4f4d*)fyCoesOther[#@chTagKey["chTag"]]
,(*\:6307\:5b9a\:8981\:6c42\:548c\:7684\:56fe*)fyCoesAdjust[adjust,#@chTagKey["chTag"]]
,(*PackageX \:7ea6\:5b9a*)I/(16\[Pi]^2)
]]&)/*
(*-------------- \:5708\:79ef\:5206*\:7cfb\:6570,\:5e76\:6570\:503c\:5316 --------------*)
(Append[#,
ffsF1F2->Simplify[chop[Times[
(*\:79ef\:5206\:89e3\:6790\:5f0f;Normal\:5904\:7406SeriesData;\:81f3\:591a Map \:5230\:7b2c\:4e8c\:5c42\:ff0c\:5982\:679c\:8fd8\:6709 PieceWise \:7ed3\:6784*)
Map[Normal,#@ffsF1F2,2]/.numPaVe,
(*\:79ef\:5206\:5bf9\:5e94\:7684\:7cfb\:6570*)#@fyCoeKeycAll
]/.medRule[#]]]
]&)/*
(*-------------- \:7531 F1F2 expr \:7ebf\:6027\:7ec4\:5408\:51fa GE,GM --------------*)
(Append[#,ffsGEGM->(toGEGM[#@ffsF1F2]/.medRule[#])]&)/*
(*-------------- \:5220\:9664\:5197\:4f59\:5b57\:6bb5 --------------*)
KeyDrop[{
mm1,mo1,mo2,md1,md2,"time",fyCoeKey["cStr"],fyCoeKey["cEM"]
}]
]@assocLst
(*\:624b\:52a8\:6c42\:548c\:65b9\:6848\:7684 Range*)
,{adjust,Keys@fyCoesAdjust}]
]


(* ::Section:: *)
(*Refine data structure*)


(*\:4ee3\:5165\:8026\:5408\:5e38\:6570\:4e4b\:540e\:ff0c\:8fdb\:884c\:6570\:636e\:7ed3\:6784\:6574\:7406\:9700\:8981\:7684\:51fd\:6570\:ff0c\:8f93\:5165\:4e3a {<>,<>..}\:ff0c\:5373\:5173\:8054\:7684\:5217\:8868
\:7b5b\:9009\:51fa\:7279\:5b9a\:7684\:4fe1\:606f,\:4fbf\:4e8e\:67e5\:770b\:6bcf\:4e2a\:8d39\:66fc\:56fe,\:6bcf\:4e2a\:53cd\:5e94\:9053\:7684\:8d21\:732e*)
pickGroup[assocLst_]:=Query[(*{assoc}*)All,
(*<|k\[Rule]v|>*)
<|Lookup[#,{medMes1,medOct1,medDec1},Nothing(*default*)]->
#[[{Key@ffsF1F2,Key@ffsGEGM,Key@fyCoeKeycAll}]]
|>&
]@assocLst;
(* GroupBy \:8fed\:4ee3\:5206\:7ec4\:7684\:7ed3\:679c\:662f\:4e00\:4e2a\:4e2a\:7684\:5c0f\:7ec4,\:5143\:7d20\:88ab\:653e\:5165\:62ec\:53f7\:4e2d, \:518d\:6b21\:5f62\:6210 {assoc,...} \:7684\:7ed3\:6784,
\:4f7f\:7528 Reduce \:51fd\:6570\:5904\:7406\:8fd9\:4e9b\:7ed3\:679c: \:5148\:901a\:8fc7 Query \:5728\:5c0f\:7ec4\:4e2d\:53d6\:51fa\:7279\:5b9a\:7684\:952e,\:5728\:8fd9\:91cc\:662f F1F2,GEGM \:7684\:6570\:503c\:7ed3\:679c,
\:53d6\:51fa\:7684\:7ed3\:679c\:4ecd\:662f {assoc,...} \:7684\:7ed3\:6784,\:518d\:4f7f\:7528 Merge \:51fd\:6570\:5408\:5e76\:5c0f\:7ec4\:7684\:7ed3\:679c,\:4f20\:5165 Total \:51fd\:6570\:6267\:884c\:6c42\:548c. 
\:8fd9\:91cc\:7684\:6c42\:548c,\:5c06\:5355\:4e2a\:8d39\:66fc\:56fe\:4e2d\:4e0d\:540c\:7684\:53cd\:5e94\:9053\:76f8\:52a0.
*)
sumGroup[assocLst_?ListQ]:=Merge[
Query[(*{assoc}*)All,(*<|k\[Rule]v|>*){Key@ffsF1F2,Key@ffsGEGM}]@assocLst,
Total];
(*GroupBy \:6309\:7167\:5217\:8868\:4e2d\:7684\:5206\:7c7b\:51fd\:6570,\:751f\:6210\:4e00\:4e2a\:5d4c\:5957\:5173\:8054,\:8fd9\:91cc\:662f\:6309{\:5165\:5c04\:7c92\:5b50,\:8d39\:66fc\:56fe}, 
\:7136\:540e\:5c06 sumGroup \:4f5c\:4e3a Reduce \:51fd\:6570\:4f5c\:7528\:5230\:6700\:7ec8\:5c42\:7684\:5c0f\:7ec4\:4e0a,\:8fd9\:91cc\:53ea\:63d0\:53d6\:4e86 F1F2,GEGM*)


(* ::Subsection:: *)
(*Groupy*)


(*\:5904\:7406\:4ee3\:5165\:8026\:5408\:5e38\:6570\:7684\:7ed3\:679c, \:6309\:7167\:7279\:5f81\:5982octet \:6807\:7b7e, \:8fdb\:884c\:5206\:7ec4*)
(* \:751f\:6210\:5173\:8054: <|loopChan\[Rule]v,loopChanSum\[Rule]v,loopAmpSum\[Rule]v|>*)
(*{channAssoc:= <|\:4efb\:4f55\:8d39\:66fc\:56fe\:53cd\:5e94\:9053|>..}*)
loopResultGroup[channAssoc_]:=Module[{loopChanSum},
(*\:5c06\:540c\:4e00\:8d39\:66fc\:56fe\:4e0b\:7684\:53cd\:5e94\:9053\:6c42\:548c-------------*)
loopChanSum=GroupBy[channAssoc,{Key@inOct,Key@chTagKey["chTag"]},sumGroup];
(*\:751f\:6210\:5173\:8054\:6570\:7ec4---------------*)
<|
(*\:7c92\:5ea6\:4e3a,\:6bcf\:4e2a\:8d39\:66fc\:56fe\:7684\:6bcf\:4e2a\:53cd\:5e94\:9053---------------*)
kLoopChannel->GroupBy[channAssoc,{Key@inOct,Key@chTagKey["chTag"]},pickGroup],
(*\:7c92\:5ea6\:4e3a,\:5c06\:540c\:4e00\:8d39\:66fc\:56fe\:4e0b\:7684\:53cd\:5e94\:9053\:6c42\:548c-------------*)
kLoopChanSum->loopChanSum,
(*\:7c92\:5ea6\:4e3a,\:5c06\:540c\:4e00\:4e2a\:5165\:5c04\:7c92\:5b50\:4e0b\:7684\:6240\:6709\:8d39\:66fc\:56fe\:7ed3\:679c\:76f8\:52a0----------*)
kLoopAmpSum->Query[
(*<|octet|>*)All,
(*<|diagrams|>*)Simplify[Merge[Values@#,Total]]&]@loopChanSum
|>
]
(* loopAmpSum \:7684\:5927\:81f4\:7ed3\:6784\:5982\:4e0b\:ff1a
\[LeftAssociation]fd[2,1,0]\[Rule]\[LeftAssociation]Total"\[Rule]\[LeftAssociation]ffsF1F2\[Rule]{F1,F2},ffsGEGM\[Rule]{GE,GM}\[RightAssociation]\[RightAssociation],
(\:5176\:4ed6\:7c92\:5b50\:7684\:7ed3\:679c,\:7ed3\:6784\:7c7b\:4f3c)\[RightAssociation]*)


(* ::Section:: *)
(*Generate*)


(*\:6b64\:51fd\:6570\:8d1f\:8d23\:8fdb\:884c\:5b9e\:9645\:8ba1\:7b97\:ff0c\:4ee5\:53ca\:6839\:636e\:6807\:7b7e\:5206\:7ec4
\:53e6\:5916\:ff0c\:6839\:636e $parOrdStr \:53c2\:6570\:51b3\:5b9a\:662f\:5426\:987a\:4fbf\:8ba1\:7b97 order0 \:7684\:7ed3\:679c*)
loopResults[$parOrdStr_]:=Block[{channAssocTemp,channAssoc},

(*\:6839\:636e\:662f\:5426\:9700\:8981\:5e76\:884c,\:8c03\:7528\:4e0a\:9762\:7684\:51fd\:6570,\:4ee3\:5165\:8026\:5408\:5e38\:6570,\:8ba1\:7b97\:5708\:79ef\:5206\:548c\:7cfb\:6570-----------------*)
channAssocTemp=If[$parallel$couplsQ,
(* HoldAll \:6240\:6709\:8868\:8fbe\:5f0f,\:4f20\:7ed9\:5b50\:6838\:8ba1\:7b97*)
SetAttributes[paraEnvIO,HoldAll];
paraEnvIO[tag_]:=ParallelSubmit[import$Eva[tag,$parOrdStr]];
(*\:5e76\:884c\:60c5\:51b5,\:9700\:8981 WaitAll \:4efb\:52a1*)
WaitAll[paraEnvIO/@fyAmpPart],
(*\:975e\:5e76\:884c\:60c5\:51b5*)
paraEnvIO[tag_]:=import$Eva[tag,$parOrdStr];
paraEnvIO/@fyAmpPart];

(*\:6574\:7406 channAssocTemp \:7684\:6b21\:5e8f, \:628a\:4eba\:4e3a\:6c42\:548c\:9009\:62e9\:653e\:5728\:6700\:5916\:5c42 -------------*)
channAssoc=Association@Table[
(*\:5c06\:4e0d\:540c\:8d39\:66fc\:56fe\:7684\:53cd\:5e94\:9053\:653e\:8fdb\:540c\:4e00\:5217\:8868\:4e2d*)
adjust->Catenate@Query[
(*{diagram}*)All
,(*<|bub,nobub|>*)adjust
]@channAssocTemp
,{adjust,Keys@fyCoesAdjust}];

(*\:5bf9\:7ed3\:679c\:8fdb\:884c\:6574\:7406,\:8fd4\:56de\:5708\:56fe\:7ed3\:679c\:7684\:5173\:8054,------------------------
<|loopChans,loopChansSum,loopAmpSum,loopAssoc|>*)
Query[(*<bub>*)All
,(*{<channel>..},\:6dfb\:52a0 kLoopAssoc \:7b49 k\[Rule]v *)
Append[loopResultGroup[#],kLoopAssoc->#]&
]@channAssoc

]


loopResults["v","tmp"]=If[$parOrdStr===$ord0,
(*\:5982\:679c\:8ba1\:7b97 ord0 \:7684\:7ed3\:679c,\:53ea\:7528\:8ba1\:7b97 ord0\:672c\:8eab*)
<|$ord0->loopResults[$ord0]|>,
(*\:5982\:679c\:8ba1\:7b97 ord1,ordFull \:7684\:7ed3\:679c,\:9700\:8981\:989d\:5916\:8ba1\:7b97 ord0*)
<|$ord0->loopResults[$ord0],
$parOrdStr->loopResults[$parOrdStr]|>];


(*\:6574\:7406\:5d4c\:5957\:5c42\:6b21\:7684\:987a\:5e8f*)
loopResults["v"]=Association@Table[
adjust->Query[
(*<order>*)All
,(*<|bub,nobub|>*)adjust
]@loopResults["v","tmp"]
,{adjust,Keys@fyCoesAdjust}];


(* ::Chapter:: *)
(*merge tree & loop*)


(* \:5c06\:6811\:56fe\:548c\:5708\:56fe\:7684\:7ed3\:679c\:653e\:5728\:4e00\:8d77,\:5e76\:5c55\:5e73\:5d4c\:5957\:7ed3\:6784,\:8f93\:5165 loopAmpSum *)
ffsMerged["fn"][treeSum_Association,loopAmpSum_Association]:=Merge[
{treeSum,loopAmpSum},
<|
(*Lookup[{assoc1,assoc_2,\[Ellipsis]},key], \:5206\:522b\:7ed9\:51fa\:6bcf\:4e2a\:5173\:8054\:4e2d\:5bf9\:5e94 key \:7684\:503c*)
AssociationThread[{ffsTreeF1F2,ffsLoopF1F2},Lookup[#,ffsF1F2]],
AssociationThread[{ffsTreeGEGM,ffsLoopGEGM},Lookup[#,ffsGEGM]],
KeyTake[First@#,{inOct}](*First@# \:4e3a treeSum *)
|>&]


(*\:540c\:6837,\:6839\:636e $parOrdStr \:7684\:503c,\:5224\:65ad\:662f\:5426\:989d\:5916\:8ba1\:7b97 ord0 *)
ffsMerged["v",keyTreeAndLoop]=Query[
(*<|bub,nobub|>*)All,
If[$parOrdStr===$ord0,
(*\:5982\:679c\:8ba1\:7b97 ord0 \:7684\:7ed3\:679c,\:53ea\:7528\:8ba1\:7b97 ord0\:672c\:8eab*)
<|
$ord0->ffsMerged["fn"][
treeSum,
Query[$ord0,kLoopAmpSum]@#
]|>,
(*\:5982\:679c\:8ba1\:7b97 ord1,ordFull \:7684\:7ed3\:679c,\:9700\:8981\:989d\:5916\:8ba1\:7b97 ord0 *)
<|
$ord0->ffsMerged["fn"][treeSum,
Query[$ord0,kLoopAmpSum]@#
],
$parOrdStr->ffsMerged["fn"][treeSum,
Query[$parOrdStr,kLoopAmpSum]@#
]
|>]&
]@loopResults["v"];


(* ::Section:: *)
(*renorm const*)


(*\:6311\:9009\:51fa tree,loop f1 \:7684\:8d21\:732e,\:8ba1\:7b97\:91cd\:6b63\:5316\:5e38\:6570*)
(*\:8fd4\:56de\:51fd\:6570\:ff0c# \:7684\:5d4c\:5957\:7ed3\:6784\:662f {order}\[Rule]{octet}\[Rule]{FFs}\[Rule]{pair}*)
(*\:5bf9\:4e8e\:4e2d\:6027\:91cd\:5b50\:ff0c\:9700\:8981\:6307\:5b9a\:540c\:4f4d\:65cb brother--------------------*)
fnRenormBy[fd_,quarkQ_]:=Function[{ffsMerged},
Abs[chopQ2[
(*\:6811\:56fe*)Query[Key@$ord0,Key@fd,Key@ffsTreeF1F2,1
]@ffsMerged-
(*\:5708\:56fe*)Query[Key@$ord0,Key@fd,Key@ffsLoopF1F2,1
]@ffsMerged/.quarkQ]]
];
(*\:5bf9\:4e8e\:5e26\:7535\:91cd\:5b50,\:4f7f\:7528\:81ea\:5df1\:7684 F1 \:8ba1\:7b97\:573a\:5f3a\:91cd\:6b63\:5316\:5e38\:6570-----------------*)
fnRenorm[fd_]:=fnRenormBy[fd,quaCharge["uds"]]


(*\:8bb0\:5f55\:91cd\:6b63\:5316\:5e38\:6570 ++++++++++++++++++++++++++++++*)
renormRule[ffsMerged_]:=KeySort@AssociationThread[
(*\:5e26\:7535*)
{fd[2,1,0],fd[2,3,0],fd[2,5,0],fd[2,7,0]
(*\:4e2d\:6027*)
,fd[2,2,0],fd[2,4,0],fd[2,6,0],fd[2,8,0]
}
,Through[{
(*\:5e26\:7535 oct \:91cd\:5b50 -----------*)
fnRenorm[fd[2,1,0]]
,fnRenorm[fd[2,3,0]]
,fnRenorm[fd[2,5,0]]
,fnRenorm[fd[2,7,0]]
(*\:4e2d\:6027 oct \:91cd\:5b50 -----------*)
,fnRenormBy[fd[2,1,0],quaCharge["uds"]]
,fnRenormBy[fd[2,3,0],quaCharge["uds"]]
,fnRenormBy[fd[2,7,0],quaCharge["uds"]]
(*\:4f7f\:7528 u flavor EM\:6d41\:8ba1\:7b97*)
,fnRenormBy[fd[2,8,0],quaCharge["u"]]
}@ffsMerged]
];


appendRenorm[ffsMerged_]:=With[{renormVal=renormRule@ffsMerged},
Query[
(*<order>*)All,
(*<octet>*)All,
(*<FFs>*)
Append[#,recon->renormVal@#@inOct]&
]@ffsMerged
]


(*+++++++++++++++++ \:52a0\:4e0a\:91cd\:6b63\:5316\:5e38\:6570 *+++++++++++++++++*)
ffsMerged["WithRen",keyTreeAndLoop]=Query[
(*<bub,nobub>*)All,
(*<order>*)appendRenorm
]@ffsMerged["v",keyTreeAndLoop];
