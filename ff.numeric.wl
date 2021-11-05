(* ::Package:: *)

(* ::Title:: *)
(*ff.numeric.wl*)


(* ::Chapter:: *)
(*initial*)


(*\:672c\:6587\:4ef6\:7684\:540d\:79f0*)
fileName=If[$Notebooks,NotebookFileName[],$InputFileName];
(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c\:ff0c\:5c31\:5237\:65b0\:7b14\:8bb0\:672c\:7684\:6807\:9898*)
Once@If[$Notebooks,NotebookWrite[Cells[][[1]],Cell[Last@FileNameSplit[fileName],"Title"]]];
(*\:67e5\:627e init.wl, \:5bfc\:5165\:6839\:76ee\:5f55\:548c\:51fd\:6570\:5b9a\:4e49.*)
Catch@Module[{recurFind,start=1,depMax},
depMax=FileNameDepth[fileName];(*\:8def\:5f84\:7684\:6700\:5927\:5c42\:6b21*)
(*-------\:5b9a\:4e49\:9012\:5f52\:51fd\:6570-------*)
recurFind[dep_Integer]:=If[dep<=depMax,
SetDirectory[DirectoryName[fileName,dep]];(*SetDirectory[]\:8bbe\:7f6e\:5de5\:4f5c\:76ee\:5f55\:4e3a\:5bb6\:76ee\:5f55*)
If[FileExistsQ["init.wl"],Get["init.wl"];Throw["The base directory is : "<>gitLocalName],recurFind[dep+1](*\:5982\:679c\:8fd9\:4e00\:5c42\:627e\:4e0d\:5230\:ff0c\:5c31\:4e0a\:5347\:4e00\:5c42*)];
ResetDirectory[];(*\:91cd\:8bbe\:4e3a\:4e4b\:524d\:7684\:76ee\:5f55*),
Throw["I cann't find any init.wl in this project"]
];
recurFind[start];
]
(* \:8bb0\:5f55 master Kernel \:7684\:8fd0\:884c\:6a21\:5f0f*)
$inNBook=$Notebooks;echo[DateString[]," <<",fileName];


(* ::Section:: *)
(*cmd arguments*)


(* \:5904\:7406\:811a\:672c\:53c2\:6570,\:6a21\:62df\:547d\:4ee4\:884c\:8f93\:5165\:53c2\:6570\:7684\:60c5\:5f62 *)
If[!$Notebooks,
inputCml=$ScriptCommandLine,(*\:5982\:679c\:5728\:547d\:4ee4\:884c\:6267\:884c*)
(*++++++++++++++++++++++++++++++++++++++++*)
inputCml={
fileName,(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c, \:6a21\:4eff\:547d\:4ee4\:884c, \:7b2c\:4e00\:4e2a\:53c2\:6570\:662f\:811a\:672c\:7684\:7edd\:5bf9\:8def\:5f84*)
(* \:5728\:8fd9\:91cc\:63d0\:4f9b\:5176\:4ed6\:53c2\:6570, \:4f7f\:7528 mathematica \:8bed\:6cd5\:4e0b\:7684\:5f62\:5f0f\:ff0c\:5916\:9762\:7684 enString \:4f1a\:81ea\:52a8\:8f6c\:6362\:6210\:5b57\:7b26\:4e32, \:5c3d\:91cf\:591a\:4f7f\:7528Association\:7ed3\:6784*)
"ord0",0.90`20,1.50`20,"Baryons","notbar"
}];
echo["the input parameter is:\n",inputCml];


(*++++++++++++++++++++++++++++++++++++++++ \:63a5\:6536\:53c2\:6570, \:4fdd\:5b58\:5230\:53d8\:91cf, \:6216\:8005\:8fdb\:884c\:8fdb\:4e00\:6b65\:5904\:7406 ++++++++++++++++++++++++++++++++++++++++*)
{parOrder,par\[CapitalLambda],parC,cFitting,errorbarQ}={
enString@inputCml[[2]],
enString@NumberForm[ToExpression@inputCml[[3]],{3,2}],
enString@NumberForm[ToExpression@inputCml[[4]],{3,2}],
enString@inputCml[[5]],
enString@inputCml[[6]]
}
(*++++++++++++++++++++++++++++++++++++++++ \:68c0\:67e5\:8f93\:5165\:7684\:53c2\:6570\:662f\:5426\:5408\:6cd5 ++++++++++++++++++++++++++++++++++++++++*)
If[Nand[
StringMatchQ[cFitting,{"Sigma1","Sigma2","Nucleon","Cascade","Baryons"}],
StringMatchQ[errorbarQ,{"notbar","L-"~~NumberString~~".ci-"~~NumberString}] (*eg."L_0.90_ci_1.50"*)
],
echo["Please check the input parameters"];Abort[]
]


(*\:5982\:679c\:8fd8\:4e0d\:5b58\:5728\:ff0c\:5219\:521b\:5efa\:76ee\:5f55*)
echo[fittingsDir=FileNameJoin[{gitLocalName,"fittings"}]];
If[!DirectoryQ[fittingsDir],CreateDirectory[fittingsDir];echo["Create a new directory: ./fittings/"]] ;
(*++++++++++++++++++++++++++++++++ \:8bfb\:53d6c1,c2\:7684\:53d6\:503c ++++++++++++++++++++++++++++++++*)
c3=c2-c1;
echo["c1,c2 configuration"]
If[errorbarQ==="notbar",
(*++++++++++++++++++++++++++++++++ \:5982\:679c\:4e0d\:662f\:4e3a\:4e86\:8ba1\:7b97Error\:ff0c\:5c31\:4f7f\:7528\:5339\:914d\:7684c1c2\:8fdb\:884c\:8ba1\:7b97 ++++++++++++++++++++++++++++++++*)
echo[cFittingPath=FileNameJoin[{fittingsDir,"c1c2-magfit.L-"<>par\[CapitalLambda]<>".ci-"<>parC<>".wdx"}]];
(*++++++++++++++++++++++++++++++++ \:4f7f\:7528\:7b2c\:4e8c\:79cd\:91cd\:6574\:5316\:65b9\:6848,Z*tree+loop ++++++++++++++++++++++++++++++++*)
echo[configc1c2=Import[cFittingPath][cFitting][[2,2]]];,
(*++++++++++++++++++++++++++++++++ \:5982\:679c\:662f\:4e3a\:4e86\:8ba1\:7b97Error\:ff0c\:5c31\:4f7f\:7528\:6307\:5b9a \[CapitalLambda],ci \:5bf9\:5e94\:7684c1c2\:8fdb\:884c\:8ba1\:7b97 ++++++++++++++++++++++++++++++++*)
echo[cFittingPath=FileNameJoin[{fittingsDir,"c1c2-magfit."<>errorbarQ<>".wdx"}]];
echo[configc1c2=Import[cFittingPath][cFitting][[2,2]]];
]


(* ::Section:: *)
(*Read coes and exprs*)


$parallelQ=False;(*\:662f\:5426\:5f00\:59cb\:5e76\:884c\:5185\:6838*)
If[$parallelQ,
(*+++++++++++++++++++ \:542f\:52a8\:5e76\:884c\:5185\:6838 +++++++++++++++++++*)
Needs["X`"];ParallelNeeds["X`"];
CloseKernels[];(*\:542f\:52a8\:5e76\:884c\:5185\:6838*)
Switch[{$MachineName,$System},
{"OP7050","Linux x86 (64-bit)"},LaunchKernels[6],
_,LaunchKernels[]
];,
Needs["X`"];]


echo[coesDir=FileNameJoin[{gitLocalName,"coes"}]];
echo[mfilesDir=FileNameJoin[{gitLocalName,"mfiles"}]];
(* 1st \:5bfc\:5165\:6b21\:5e8f: \:8bfb\:5165\:6392\:7248 *)
Once@Get[FileNameJoin[{gitLocalName,"gen.format.wl"}]];
(*\:5bfc\:5165\:6240\:6709\:8d39\:66fc\:56fe tag \:7684\:5217\:8868: fyAmpLoopLst,fyAmpTreeLst*)
Get[FileNameJoin@{gitLocalName,"gen.integral.TagList.wl"}];
fyAmpPart=fyAmpLoopLst;
(*\:8bfb\:5165\:4e00\:4e9b\:8f93\:5165\:63a5\:53e3*)
Get[FileNameJoin[{gitLocalName,"coes.interface.wl"}]];


(* ::Section:: *)
(*Kinematic & Couplings*)


(*\:58f0\:660e\:4e00\:4e9b\:8fd0\:52a8\:5b66\:53d8\:91cf,\:4f7f\:7528 atom \:8868\:8fbe\:5f0f\:ff0cpackage-X \:7684 loopIntegrate \:9700\:8981\:79ef\:5206\:52a8\:91cf\:4e3a\:539f\:5b50\:8868\:8fbe\:5f0f*)
\[CapitalLambda];(*\:6b63\:89c4\:5b50\:8d28\:91cf*)
mE;(*\:5916\:817f\:7c92\:5b50\:7684\:8d28\:91cf*);
mm1;mm2;(*\:4e2d\:95f4\:516b\:91cd\:6001\:4ecb\:5b50*)
mo1;mo2;(*\:4e2d\:95f4\:516b\:91cd\:6001\:91cd\:5b50*)
md1;md2;(*\:4e2d\:95f4\:5341\:91cd\:6001\:91cd\:5b50*)
Q2;(*Q2=-q^2,\:8f6c\:79fb\:52a8\:91cf\:5e73\:65b9\:7684\:8d1f\:503c*)


(* \:5c06\:8d28\:91cf\:8f6c\:6362\:6210\:66f4\:7b80\:6d01\:7684\:539f\:5b50\:8868\:8fbe\:5f0f *)
(*  octet mesons *)
massV@fd[1,0,2]=mass\[Eta]0;
massV@fd[1,1,2]=mass\[Pi]; massV@fd[1,2,2]=mass\[Pi]; massV@fd[1,3,2]=mass\[Pi];
massV@fd[1,4,2]=massK; massV@fd[1,5,2]=massK; massV@fd[1,6,2]=massK; massV@fd[1,7,2]=massK;
massV@fd[1,8,2]=mass\[Eta]8;
(* octet baryons *)
massV@fd[2,1,2]=massN; massV@fd[2,2,2]=massN;
massV@fd[2,3,2]=mass\[CapitalSigma]; massV@fd[2,4,2]=mass\[CapitalSigma]; massV@fd[2,5,2]=mass\[CapitalSigma];
massV@fd[2,6,2]=mass\[CapitalXi]; massV@fd[2,7,2]=mass\[CapitalXi];
massV@fd[2,8,2]=mass\[CapitalLambda];
(* decuplet baryons *)
massV@fd[3,1,2]=mass\[CapitalDelta]; massV@fd[3,2,2]=mass\[CapitalDelta];massV@fd[3,3,2]=mass\[CapitalDelta]; massV@fd[3,4,2]=mass\[CapitalDelta]; 
massV@fd[3,5,2]=mass\[CapitalSigma]s;massV@fd[3,6,2]=mass\[CapitalSigma]s; massV@fd[3,7,2]=mass\[CapitalSigma]s;
massV@fd[3,8,2]=mass\[CapitalXi]s;massV@fd[3,9,2]=mass\[CapitalXi]s;
massV@fd[3,10,2]=mass\[CapitalOmega];


(*\:7ed9\:51fa \:8d28\:91cf\:5177\:4f53\:6570\:503c \:7684\:66ff\:6362\:89c4\:5219*)
numMass={
\[CapitalLambda]->SetPrecision[ToExpression@par\[CapitalLambda],20],
(*  octet mesons *)
mass\[Pi]->0.1381`20,massK->0.4956`20,mass\[Eta]8->0.5693`20,mass\[Eta]0->0.9452`20,
(*  octet baryons *)
mass\[CapitalSigma]->1.193`20,massN->0.939`20,mass\[CapitalXi]->1.315`20,
mass\[CapitalLambda]->1.116`20,mass\[CapitalLambda]\[CapitalSigma]->1.155`20,
massUUU->0.939`20,massDDD->0.939`20,masssss->1.315`20,
(*  decuplet baryons *)
mass\[CapitalDelta]->1.232`20,mass\[CapitalSigma]s->1.385`20,mass\[CapitalXi]s->1.530`20,mass\[CapitalOmega]->1.672`20
};


(*c1\[TildeTilde]3/2 \[Mu]u, c2\[TildeTilde]2/3c1-1, c3->c2-c1, cT=3/2c2+1/2,*)
configc1c2={
cc["c1"]->1.6766`20,cc["c2"]->0.4984`20,
cc["c4"]->1.6766`20/Sqrt[3],cc["cT"]->0.4984`20*3/2+1/2
};
(*------------------- \:5c06\:7cfb\:6570\:7684\:5177\:4f53\:6570\:503c\:4ee3\:5165 -------------------*)
numCoupLst={
cc["f"]->0.093`20,
cc["D"]->0.76`20, cc["F"]->0.5`20,
cc["C"]->SetPrecision[ToExpression@parC,20],
cc["b9"]->1.36,cc["b10"]->1.24,cc["b11"]->0.46,
\[CapitalLambda]->SetPrecision[ToExpression@par\[CapitalLambda],20],
ch["u"]->2/3,ch["d"]->-1/3,ch["s"]->-1/3,
fyCoe->Times,vtxCoe->Identity,
Sequence@@configc1c2
};


medRule::usage="medRule[x], \:4f20\:5165\:4e00\:4e2a\:5173\:8054\:ff0c\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:7684\:66ff\:6362\:89c4\:5219, \:52a0\:4e0a\:5404\:79cd\:8026\:5408\:5e38\:6570";
medRule[x_]:=Dispatch@Merge[{KeyTake[x,{mE,mm1,mo1,mo2,md1,md2}]/.numMass,numMass,numCoupLst},Last];
toGEGM::usage="toGEGM[x], \:4f20\:5165 {F1,F2} ";
toGEGM[x_?ListQ]:={First[x]-Q2/(4*mE)*Last[x], Total[x]};


chopLimit=10^-10;(*cut\:7cbe\:5ea6*)precision=20;(*\:7cbe\:786e\:5ea6*)
(*----------- PaVe\:4e3b\:79ef\:5206 \:89e3\:6790\:5f0f\:4e2d\:7684\:7279\:6b8a\:51fd\:6570, \:5ef6\:8fdf Chop \:907f\:514dDiscB\:5e26\:6765\:7684\:5fae\:5c0f\:5047\:865a\:90e8 -----------*)
DiscBChop[x__]:=Chop[DiscB[x],chopLimit]/;And@@NumericQ/@{x}(*\:5f53\:8f93\:5165\:662f\:6570\:5b57\:7684\:65f6\:5019\:ff0c\:624d\:8fdb\:884cchop*)
ScalarC0Chop[x__]:=Chop[ScalarC0[x],chopLimit]/;And@@NumericQ/@{x}(*\:5f53\:8f93\:5165\:662f\:6570\:5b57\:7684\:65f6\:5019\:ff0c\:624d\:8fdb\:884cchop*)
(*\:7279\:6b8a\:51fd\:6570\:7684 \:5ef6\:8fdfChop*)
numPaVe={DiscB->DiscBChop,ScalarC0->ScalarC0Chop};
chop[x_]:=Chop[x,chopLimit]


paraInitial=Hold[
(*\:8bbe\:7f6e\:5316\:7b80\:65f6\:95f4\:9650\:5236, \:5173\:95ed Simplify \:5316\:7b80\:65f6\:95f4\:8d85\:51fa \:4fe1\:606f*)
SetOptions[Simplify,TimeConstraint->1];
SetOptions[Refine,TimeConstraint->1];
Off[Simplify::time];Off[Refine::time];
];
ReleaseHold@paraInitial


(*\:5e76\:884c\:8ba1\:7b97\:521d\:59cb\:5316*)
If[$parallelQ,
ParallelEvaluate[ReleaseHold@paraInitial];
DistributeDefinitions[
gitLocalName,fileName,echo,enList,enString,$inNBook,
parOrder,par\[CapitalLambda],parC,cFitting,errorbarQ,
coesDir,mfilesDir,fyAmpPart,
massV,numMass,numCoupLst,numPaVe,otherCoes,
chopLimit,chop,precision,
medRule,toGEGM
];]


(* ::Chapter:: *)
(*Loops numeric*)


(* ::Section:: *)
(*parallel IO*)


import$Eva[tag_]:=Block[{coes,expr,assocLst},
coes=Import[FileNameJoin[{coesDir,"coe.chpt."<>StringRiffle[tag,"."]<>".wdx"}]];
expr={Import[FileNameJoin[{mfilesDir,"analytic.strange."<>parOrder<>"."<>StringRiffle[tag,"."]<>".wdx"}]]};
(* JoinAcross \:5229\:7528\:516c\:5171\:7684 chpt \:8d39\:66fc\:56fe tag\:ff0c\:8fde\:63a5\:7cfb\:6570\:4e0e\:5708\:79ef\:5206\:8868\:8fbe\:5f0f *)
assocLst=JoinAcross[coes,expr,Key@chTagKey["chTag"]];
(*++++++++++++++++++++++++ \:6dfb\:52a0\:5404\:79cd\:5b57\:6bb5 ++++++++++++++++++++++++*)
Query[All,
(*-------------- \:5708\:79ef\:5206*\:7cfb\:6570,\:5e76\:6570\:503c\:5316 --------------*)
(Append[#,
ffsF1F2->Simplify[chop[Times[
Normal[#@ffsF1F2]/.numPaVe,#@fyCoeKeycAll,otherCoes[#@chTagKey["chTag"]]
]/.medRule[#]]]
])&/*
(*-------------- \:7531 F1F2 expr \:7ebf\:6027\:7ec4\:5408\:51fa GE,GM --------------*)
(Append[#,ffsGEGM->(toGEGM[#@ffsF1F2]/.medRule[#])]&)/*
(*-------------- \:5220\:9664\:5197\:4f59\:5b57\:6bb5 --------------*)
KeyDrop[{
mm1,mo1,mo2,md1,md2,"time",fyCoeKey["cStr"],fyCoeKey["cEM"]
}]
]@assocLst]


(*I I/(16\[Pi]^2)*)
echo["start import coeffs and loop-exprs "];
(* import coes *)
If[$parallelQ,
SetAttributes[paraEnvIO,HoldAll];
paraEnvIO[tag_]:=ParallelSubmit[import$Eva[tag]],
paraEnvIO[tag_]:=import$Eva[tag]
]


(* ::Input:: *)
(*numAssoc=WaitAll[paraEnvIO/@fyAmpPart];*)


numAssoc=paraEnvIO/@fyAmpPart;


(* \:7b5b\:9009\:51fa\:7279\:5b9a\:7684\:4fe1\:606f\:ff0c\:4fbf\:4e8e\:67e5\:770b\:6bcf\:4e2a\:8d39\:66fc\:56fe\:ff0c\:6bcf\:4e2a\:53cd\:5e94\:9053\:7684\:8d21\:732e*)
pickGroup[x_]:=Query[All,<|Lookup[#,{medMes1,medOct1},"None"]->#[[{Key@ffsF1F2,Key@ffsGEGM}]]|>&]@x;
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
(*tree level contributions*)


treeFsGs=Query[All,
(Append[#,ffsF1F2->Simplify[chop[{#@fyCoeKeycAllF1,#@fyCoeKeycAllF2}/.medRule[#]]]]&)/*
(Append[#,ffsGEGM->Simplify[chop[{#@fyCoeKeycAllF1,#@fyCoeKeycAllF2}/.medRule[#]]]]&)
]@Import[FileNameJoin[{coesDir,"coe.chpt."<>StringRiffle[#,"."]<>".wdx"}]]&@fyAmpTree;
(*++++++++++++++++++++++++++  \:63d0\:53d6\:51fa\:6811\:56fe\:9636\:7684\:6570\:503c\:7ed3\:679c  +++++++++++++++++++++++++++++++++++*)
treeSum=Association@Query[All,
#@inOct->KeyTake[#,{ffsF1F2,ffsGEGM}]&]@treeFsGs;


Merge[{treeSum,loopAmpSum},<|"tree"->First[#],"loop"->Last[#]|>&];


(* ::Chapter:: *)
(*test neutral*)


(*\:68c0\:67e5\:662f\:5426\:6ee1\:8db3 \:4e2d\:6027\:7c92\:5b50 \:7535\:8377\:5b88\:6052*)
sectOct=Key/@{
chTag@{"RB","mes","oct"},
chTag@{"KR","mes","oct","left"},
chTag@{"KR","mes","oct","add","left"},
chTag@{"RB","oct","F1"},
chTag@{"RB","oct","F2"}
};
sectDec=Key/@{
chTag@{"RB","mes","dec"},
chTag@{"RB","dec","F1"},
chTag@{"RB","dec","F2"},
chTag@{"RB","trans","left"},
chTag@{"KR","mes","dec","left"},
chTag@{"KR","mes","dec","add","left"}
};
sectBub=Key/@{
chTag@{"tad","oct","F1"},
chTag@{"tad","oct","F1","add"},
chTag@{"tad","oct","F2"},
chTag@{"bub","mes","o2"},
chTag@{"bub","mes","ten","o2"}
};
sectMag=Key/@{
chTag@{"RB","oct","F2"},
chTag@{"tad","oct","F2"},
chTag@{"bub","mes","ten","o2"},
chTag@{"RB","dec","F2"},
chTag@{"RB","trans","left"}
};


(* ::Input:: *)
(*(*\:5c55\:793a\:7c92\:5b50\:7684\:603b\:7ed3\:679c*)*)
(*Query[KeySort/*Normal/*(TableForm[#,TableSpacing->{3.5, 1}]&),*)
(*Normal/*(TableForm[#,TableSpacing->{1.5, 1}]&),*)
(*Simplify[chop[I/(16\[Pi]^2)#/.Q2->0]]&*)
(*]@loopAmpSum*)


(* ::Input:: *)
(*(* \:5bf9\:67d0\:4e9b\:56fe\:7684\:7ed3\:679c\:6c42\:548c\:ff0c*)*)
(*Query[{Key@fd[2,1,0]},sectOct/*Total,({Key@ffsF1F2}),Extract[1],Simplify[chop[I/(16\[Pi]^2)#/.Q2->0]]&*)
(*]@loopChanSum*)


(* ::Input:: *)
(*(* \:5c55\:793a\:6bcf\:4e2a\:56fe\:7684\:7ed3\:679c *)*)
(*Query[{Key@fd[2,1,0]}/*Normal/*(Column[#,Spacings->2]&),*)
(*sectOct/*Normal/*(Column[#,Spacings->1,Alignment->"\[Rule]"]&),*)
(*Normal/*(TableForm[#,TableSpacing->{2, 1}]&),*)
(*Simplify[chop[I/(16\[Pi]^2)#/.Q2->0]]&*)
(*]@loopChanSum*)


(* ::Input:: *)
(*(* \:5c55\:793a\:6bcf\:4e2a\:56fe\:6bcf\:4e2a\:53cd\:5e94\:9053\:7684\:7ed3\:679c *)*)
(*Query[{Key@fd[2,1,0]}/*Normal/*(Column[#,Spacings->2,Frame->None]&),*)
(*sectOct/*Normal/*(Column[#,Spacings->1,Alignment->"\[Rule]",Frame->All,FrameStyle->{Blue,Opacity[.5]}]&),*)
(*Normal/*(TableForm[#,TableSpacing->{2, 1}]&),*)
(*All,*)
(*{Key@ffsF1F2},Simplify[chop[I/(16\[Pi]^2)#/.Q2->0]]&*)
(*]@loopChans*)


(* ::Chapter:: *)
(*EOF*)


echo[DateString[]," : finished, SessionTime : ",SessionTime[]];
