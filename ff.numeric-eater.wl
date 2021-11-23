(* ::Package:: *)

(* ::Title:: *)
(*ff.numeric-eater.wl*)


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
SetDirectory[DirectoryName[$fileName,dep]];(*SetDirectory[]\:8bbe\:7f6e\:5de5\:4f5c\:76ee\:5f55\:4e3a\:5bb6\:76ee\:5f55*)
If[FileExistsQ["init.wl"],
(*\:5c06\:4e3b\:76ee\:5f55\:6dfb\:52a0\:5230\:641c\:7d22\:8def\:5f84*)
Get["init.wl"];PrependTo[$Path,$gitLocalName];
Throw["The base directory is : "<>$gitLocalName];,
recurFind[dep+1](*\:5982\:679c\:8fd9\:4e00\:5c42\:627e\:4e0d\:5230\:ff0c\:5c31\:4e0a\:5347\:4e00\:5c42*)];
ResetDirectory[];(*\:91cd\:8bbe\:4e3a\:4e4b\:524d\:7684\:76ee\:5f55*),
Throw["I cann't find any init.wl in this project"]
];
recurFind[start];
]
(* \:8bb0\:5f55 master Kernel \:7684\:8fd0\:884c\:6a21\:5f0f*)
$inNBook=$Notebooks;echo[DateString[]," <<",$fileName];


(* ::Section:: *)
(*cmd arguments*)


(* \:5904\:7406\:811a\:672c\:53c2\:6570,\:6a21\:62df\:547d\:4ee4\:884c\:8f93\:5165\:53c2\:6570\:7684\:60c5\:5f62 *)
If[!$Notebooks,
$inputCml=$ScriptCommandLine,(*\:5982\:679c\:5728\:547d\:4ee4\:884c\:6267\:884c*)
(*++++++++++++++++++++++++++++++++++++++++*)
$inputCml={
$fileName,(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c, \:6a21\:4eff\:547d\:4ee4\:884c, \:7b2c\:4e00\:4e2a\:53c2\:6570\:662f\:811a\:672c\:7684\:7edd\:5bf9\:8def\:5f84*)
(* \:5728\:8fd9\:91cc\:63d0\:4f9b\:5176\:4ed6\:53c2\:6570, \:4f7f\:7528 mathematica \:8bed\:6cd5\:4e0b\:7684\:5f62\:5f0f\:ff0c\:5916\:9762\:7684 enString \:4f1a\:81ea\:52a8\:8f6c\:6362\:6210\:5b57\:7b26\:4e32, \:5c3d\:91cf\:591a\:4f7f\:7528Association\:7ed3\:6784*)
"ord0",0.90`20,1.50`20,"Baryons","notbar"
}];
echo["the input parameter is:\n",$inputCml];


(*----------------- \:662f\:5426\:5f00\:59cb\:5e76\:884c\:5185\:6838 -----------------*)
$parallelQ=False;
(*----------------- \:662f\:5426\:5f00\:59cb\:62df\:5408\:7a0b\:5e8f -----------------*)
$fittingQ=True;
(*\:5176\:4ed6\:53c2\:6570\:8bbe\:7f6e*)
$parOrdStr="ord0";
$parC=0.90`20;
$parCStr=enString@NumberForm[$parC,{3,2}];
$par\[CapitalLambda]=1.50`20;
$par\[CapitalLambda]Str=enString@NumberForm[$par\[CapitalLambda],{3,2}];
$fitScheme="Baryons";
$erroBar="notbar";


(* ::Input:: *)
(*(*++++++++++++++++++++++++++++++++++++++++ \:63a5\:6536\:53c2\:6570, \:4fdd\:5b58\:5230\:53d8\:91cf, \:6216\:8005\:8fdb\:884c\:8fdb\:4e00\:6b65\:5904\:7406 ++++++++++++++++++++++++++++++++++++++++*)*)
(*{$parOrdStr,$par\[CapitalLambda]Str,$parCStr,$fitScheme,$erroBar}={*)
(*enString@$inputCml[[2]],*)
(*enString@NumberForm[$inputCml[[3]],{3,2}],*)
(*enString@NumberForm[$inputCml[[4]],{3,2}],*)
(*enString@$inputCml[[5]],*)
(*enString@$inputCml[[6]]*)
(*}*)
(*(*++++++++++++++++++++++++++++++++++++++++ \:68c0\:67e5\:8f93\:5165\:7684\:53c2\:6570\:662f\:5426\:5408\:6cd5 ++++++++++++++++++++++++++++++++++++++++*)*)
(*If[Nand[*)
(*StringMatchQ[$fitScheme,{"Sigma1","Sigma2","Nucleon","Cascade","Baryons"}],*)
(*StringMatchQ[$erroBar,{"notbar","L-"~~NumberString~~".ci-"~~NumberString}] (*eg."L_0.90_ci_1.50"*)*)
(*],*)
(*echo["Please check the input parameters"];Abort[]*)
(*]*)
(*(* \:5904\:7406\:6570\:5b57 *)*)
(*$parC=SetPrecision[ToExpression@$parCStr,20]*)
(*$par\[CapitalLambda]=SetPrecision[ToExpression@$par\[CapitalLambda]Str,20]*)


(* ::Input:: *)
(*(*\:5982\:679c\:8fd8\:4e0d\:5b58\:5728\:ff0c\:5219\:521b\:5efa\:76ee\:5f55*)*)
(*echo[fittingsDir=FileNameJoin[{$gitLocalName,"fittings"}]];*)
(*If[!DirectoryQ[fittingsDir],CreateDirectory[fittingsDir];echo["Create a new directory: ./fittings/"]] ;*)
(*(*++++++++++++++++++++++++++++++++ \:8bfb\:53d6c1,c2\:7684\:53d6\:503c ++++++++++++++++++++++++++++++++*)*)
(*(*c3=c2-c1;*)*)
(*echo["c1,c2 configuration"]*)
(*If[$erroBar==="notbar",*)
(*(*++++++++++++++++++++++++++++++++ \:5982\:679c\:4e0d\:662f\:4e3a\:4e86\:8ba1\:7b97Error\:ff0c\:5c31\:4f7f\:7528\:5339\:914d\:7684c1c2\:8fdb\:884c\:8ba1\:7b97 ++++++++++++++++++++++++++++++++*)*)
(*echo[$fitSchemePath=FileNameJoin[{fittingsDir,"c1c2-magfit.L-"<>$par\[CapitalLambda]Str<>".ci-"<>$parCStr<>".wdx"}]];*)
(*(*++++++++++++++++++++++++++++++++ \:4f7f\:7528\:7b2c\:4e8c\:79cd\:91cd\:6574\:5316\:65b9\:6848,Z*tree+loop ++++++++++++++++++++++++++++++++*)*)
(*echo[numCCC=Import[$fitSchemePath][$fitScheme][[2,2]]];,*)
(*(*++++++++++++++++++++++++++++++++ \:5982\:679c\:662f\:4e3a\:4e86\:8ba1\:7b97Error\:ff0c\:5c31\:4f7f\:7528\:6307\:5b9a \[CapitalLambda],ci \:5bf9\:5e94\:7684c1c2\:8fdb\:884c\:8ba1\:7b97 ++++++++++++++++++++++++++++++++*)*)
(*echo[$fitSchemePath=FileNameJoin[{fittingsDir,"c1c2-magfit."<>$erroBar<>".wdx"}]];*)
(*echo[numCCC=Import[$fitSchemePath][$fitScheme][[2,2]]];*)
(*]*)


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


echo[coesDir=FileNameJoin[{$gitLocalName,"coes"}]];
echo[mfilesDir=FileNameJoin[{$gitLocalName,"mfiles"}]];
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


(*c1\[TildeTilde]3/2 \[Mu]u, c2\[TildeTilde]2/3c1-1, c3->c2-c1, cT=3/2c2+1/2,*)
numCCC={
cc["c1"]->1.6766`20,cc["c2"]->0.4984`20,
cc["c4"]->1.6766`20/Sqrt[3],cc["cT"]->0.4984`20*3/2+1/2,
cc["C"]->$parC
};


(*------------------- \:5c06\:8026\:5408\:5e38\:6570\:7684\:5177\:4f53\:6570\:503c\:4ee3\:5165 -------------------*)
numCoupLst={
cc["f"]->0.093`20,
cc["D"]->0.76`20, cc["F"]->0.50`20,
cc["b9"]->1.36,cc["b10"]->1.24,cc["b11"]->0.46,
\[CapitalLambda]->$par\[CapitalLambda],
Sequence@@coesRule,
(*------------------- \:5982\:679c\:662f\:62df\:5408\:ff0c\:5219\:8fd9\:91cc\:4e0d\:6307\:5b9a c1,c2 \:7684\:503c -----------------*)
If[$fittingQ,
Sequence@@{cc["c4"]->cc["c1"]/Sqrt[3],cc["cT"]->(3cc["c2"]+1)/2},(*\:62df\:5408\:7684\:60c5\:51b5\:ff0c\:4e0d\:7ed9\:51fa c1,c2,C \:7684\:6570\:503c*)
Sequence@@numCCC
]};


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
$gitLocalName,$fileName,echo,enList,enString,$inNBook,
$parOrdStr,$par\[CapitalLambda]Str,$parCStr,$fitScheme,$erroBar,
coesDir,mfilesDir,fyAmpPart,
massV,numMass,numCoupLst,numPaVe,otherCoes,
quaCharge,medRule,toGEGM,
chopLimit,chop,precision
];]


(* ::Chapter:: *)
(*worker*)


(* \:5bfc\:5165\:5177\:4f53\:8ba1\:7b97\:7684\:7a0b\:5e8f,\:6811\:56fe\:ff0c\:5708\:56fe\:ff0c\:91cd\:6b63\:5316\:5e38\:6570 *)
Get["ff.numeric.worker.wl"];


(* ::Chapter::Closed:: *)
(*testing*)


(* ::Input:: *)
(*(*\:5c55\:793a\:7c92\:5b50\:7684\:603b\:7ed3\:679c*)*)
(*Query[KeySort/*Normal/*(TableForm[#,TableSpacing->{3.5, 1}]&),*)
(*Normal/*(TableForm[#,TableSpacing->{1.5,1}]&),*)
(*chopQ2Val/*ReplaceAll[quaCharge["uds"]]*)
(*]@loopAmpSum*)


(* ::Input:: *)
(*(* \:5bf9\:67d0\:4e9b\:56fe\:7684\:7ed3\:679c\:6c42\:548c\:ff0c*)*)
(*Query[{Key@fd[2,1,0]},sectOct/*Total,({Key@ffsF1F2}),All,*)
(*chopQ2Val/*ReplaceAll[quaCharge["uds"]]*)
(*]@loopChanSum*)


(* ::Input:: *)
(*(* \:5c55\:793a\:6bcf\:4e2a\:56fe\:7684\:7ed3\:679c *)*)
(*Query[{Key@fd[2,1,0]}/*Normal/*(Column[#,Spacings->2]&),*)
(*sectOct/*Normal/*(Column[#,Spacings->1,Alignment->"\[Rule]"]&),*)
(*Normal/*(TableForm[#,TableSpacing->{2, 1}]&),*)
(*chopQ2Val/*ReplaceAll[quaCharge["uds"]]*)
(*]@loopChanSum*)


(* ::Input:: *)
(*(* \:5c55\:793a\:6bcf\:4e2a\:56fe\:6bcf\:4e2a\:53cd\:5e94\:9053\:7684\:7ed3\:679c *)*)
(*Query[{Key@fd[2,1,0]}/*Normal/*(Column[#,Spacings->2]&),*)
(*sectOct/*Normal/*(Column[#,Spacings->1,Alignment->"\[Rule]"]&),*)
(*Normal/*(TableForm[#,TableSpacing->{2, 1}]&),*)
(*chopQ2Val/*ReplaceAll[quaCharge["uds"]]*)
(*]@loopChanSum*)


(* ::Chapter::Closed:: *)
(*Display*)


(* \:5bf9 data \:4e2d\:7684 head \:8fdb\:884c\:8f6c\:6362\:ff0c\:8f93\:51fa\:663e\:793a\:683c\:5f0f*)
numDisp={fd->fdDisp,numKey->StringRiffle,numVal->(N[#,3]&)};
dataDsip[x_]:=Dataset[x/.{Association->assoc}
/.numDisp/.{assoc->Association}
];
(* \:5c06\:5d4c\:5957\:7684 {Assoc,Assoc} \:8f6c\:6362\:6210\:4e8c\:7ef4\:5217\:8868\:5f62\:5f0f *)
dataToGrid::usage="dataToGrid[title,dataset], title \:5c06\:4f5c\:4e3a\:5217\:8868\:7684\:6807\:9898";
dataToGrid[title_,x_]:=Prepend[
KeyValueMap[Prepend[Values[#2],#1]&,x],
Prepend[Query[First,Keys]@x,title]
]
(* Curry \:5f62\:5f0f *)
dataToGrid[title_]:=dataToGrid[title,#]&


(* \:4f7f\:7528 Grid \:663e\:793a \:4e8c\:7ef4\:5217\:8868 *)
gridTable[dataSet_,title_,background_]:=Grid[
dataToGrid[title]@dataSet/.numDisp,
ItemSize->Automatic,
Frame->{All,All},
Spacings->{1,1.5},
Background->background
]


(*\:80cc\:666f\:8272\:914d\:7f6e*)
dataBackground={
None,(* color horizontal: x1, x2, x3...*)
{
LightCyan,{None,LightBlue}
}(* color vertical: y1, y2, y3...*)
};
If[$inNBook &&!$fittingQ,
gridTable[
numFFs,"GEGM",
dataBackground]]


(* ::Chapter:: *)
(*Fitting*)


If[$fittingQ,
(*++++++++++++++++++ \:8ba1\:7b97\:6570\:636e - \:5b9e\:9a8c\:6570\:636e ++++++++++++++++++++++*)
testMagMerged=Merge[{
Query[All,(Key@tagNum["tr+lo","uds"])
/*ReplaceAll[numVal->Identity]/*Last]@numFFs,
(-1)numOctMaget
},Total];
(*------------------- \:5bf9\:5404\:79cd\:53ef\:80fd\:7684 fitting \:5e8f\:5217, \:6c42\:89e3 fitting \:503c -------------------*)
testFit[testList_]:=With[{ccNum=1.0},
NMinimize[{
Query[(Key/@testList)/*ReplaceAll[cc["C"]->ccNum]/*Total,Power[#,2]&]@testMagMerged,
{cc["c1"],cc["c2"]}\[Element]Reals},
{cc["c1"],cc["c2"]},
WorkingPrecision->MachinePrecision]];
(*------------ \:8ba1\:7b97 fitting \:7ed3\:679c -----------------*)
KeyValueMap[#1->testFit[#2]&,tagOctfds](* \:5728interfaces \:4e2d\:5b9a\:4e49*)
]


(* ::Section:: *)
(*export*)


(*\:5982\:679c\:8fd8\:4e0d\:5b58\:5728\:ff0c\:5219\:521b\:5efa\:76ee\:5f55*)
echo[resultsDir=FileNameJoin[{$gitLocalName,"results"}]];
If[!DirectoryQ[resultsDir],CreateDirectory[resultsDir];echo["Create a new directory: ",resultsDir]];


(*\:4fdd\:5b58\:7ed3\:679c\:5230\:672c\:5730\:6587\:4ef6*)
serialize[numFFs_]:=Block[{path},
path=FileNameJoin[{resultsDir,"nums."<>StringRiffle[{$parOrdStr,$par\[CapitalLambda]Str,$parCStr,$fitScheme,$erroBar},"-"]<>".wdx"}];
Export[path,numFFs];
echo["Exporting finished: ", path];
]


(* ::Chapter:: *)
(*EOF*)


echo[DateString[]," : finished, SessionTime : ",SessionTime[]];
