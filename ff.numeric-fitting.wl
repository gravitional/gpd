(* ::Package:: *)

(* ::Title:: *)
(*ff.numeric-fitting.wl*)


(* ::Chapter:: *)
(*initiate*)


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
Get["init.wl"];PrependTo[$Path,$srcRoot];
Throw["The base directory is : "<>$srcRoot];,
recurFind[dep+1](*\:5982\:679c\:8fd9\:4e00\:5c42\:627e\:4e0d\:5230\:ff0c\:5c31\:4e0a\:5347\:4e00\:5c42*)];
ResetDirectory[];(*\:91cd\:8bbe\:4e3a\:4e4b\:524d\:7684\:76ee\:5f55*),
Throw["I cann't find any init.wl in this project"]
];
recurFind[start];
]
(* \:8bb0\:5f55 master Kernel \:7684\:8fd0\:884c\:6a21\:5f0f,\:5e76\:884c\:8ba1\:7b97\:4e2d\:4f7f\:7528 *)
$inNBook=$Notebooks;echo[DateString[]," <<",$fileName];


(* ::Section:: *)
(*import module*)


coesDir=FileNameJoin[{$srcRoot,"coes"}];
mfilesDir=FileNameJoin[{$srcRoot,"mfiles"}];
(* 1st \:5bfc\:5165\:6b21\:5e8f: \:8bfb\:5165\:6392\:7248 *)
(*Once@Get["gen.format.wl"];*)
(*\:5bfc\:5165\:6240\:6709\:8d39\:66fc\:56fe tag \:7684\:5217\:8868: fyAmpLoopLst,fyAmpTreeLst*)
Once@Get["gen.integral.TagList.wl"];
(*\:8981\:8ba1\:7b97\:7684\:8d39\:66fc\:56fe\:5217\:8868*)
fyAmpPart=fyAmpLoopLst;
(*\:8bfb\:5165\:5404\:79cd\:8f93\:5165\:63a5\:53e3*)
Once@Get["coes.interface.wl"];


(* ::Section:: *)
(*num chop*)


(*+++++++++++++++++++++++++++ \:6570\:503c\:7cbe\:5ea6\:7684\:76f8\:5173\:8bbe\:7f6e +++++++++++++++++++++++++++*)
chopLimit=10^-10;(*cut\:7cbe\:5ea6*)$precision=MachinePrecision;(*\:7cbe\:786e\:5ea6*)


(*-------------\:662f\:5426\:5f00\:59cb\:5e76\:884c\:5185\:6838--------------------*)
$parallelQ=False;
(*------------------------\:5176\:4ed6\:53c2\:6570\:8bbe\:7f6e--------------------*)
$parOrdStr=$ord0;
$erroBar="notbar";
(*----------------- flag, \:662f\:5426\:8fdb\:884c\:62df\:5408---------------------*)
$fittingQ=True;
If[$fittingQ,$parOrdStr=$ord0];


(*\:5982\:679c\:8fd8\:4e0d\:5b58\:5728\:ff0c\:5219\:521b\:5efa\:76ee\:5f55*)
echo[fittingsDir=FileNameJoin[{$srcRoot,"fittings"}]];enDir[fittingsDir];


(*c1\[TildeTilde]3/2 \[Mu]u, c2\[TildeTilde]2/3c1-1, c3->c2-c1, cT=3/2c2+1/2,*)
numCCRelation={cc["c4"]->cc["c1"]/Sqrt[3],cc["cT"]->(3cc["c2"]+1)/2};
(*\:62df\:5408\:ff0c\:8fd9\:91cc\:4e0d\:6307\:5b9a c1,c2,C \:7684\:503c*)
fittedParas=numCCRelation;


(* \:975e\:62df\:5408\:7684\:60c5\:51b5\:ff0c\:7ed9\:51fa c1,c2, C \:7684\:5177\:4f53\:6570\:503c
Join[ccfitted,numCCRelation/.ccfitted,{cc["C"]->$parC}]
*)


(*\:58f0\:660e\:4e00\:4e9b\:8fd0\:52a8\:5b66\:53d8\:91cf,\:4f7f\:7528 atom \:8868\:8fbe\:5f0f\:ff0cpackage-X \:7684 loopIntegrate \:9700\:8981\:79ef\:5206\:52a8\:91cf\:4e3a\:539f\:5b50\:8868\:8fbe\:5f0f*)
\[CapitalLambda];(*\:6b63\:89c4\:5b50\:8d28\:91cf*)
mE;(*\:5916\:817f\:7c92\:5b50\:7684\:8d28\:91cf*);
mm1;mm2;(*\:4e2d\:95f4\:516b\:91cd\:6001\:4ecb\:5b50*)
mo1;mo2;(*\:4e2d\:95f4\:516b\:91cd\:6001\:91cd\:5b50*)
md1;md2;(*\:4e2d\:95f4\:5341\:91cd\:6001\:91cd\:5b50*)
Q2;(*Q2=-q^2,\:8f6c\:79fb\:52a8\:91cf\:5e73\:65b9\:7684\:8d1f\:503c*)


(* ::Chapter:: *)
(*num FFs*)


(*\:5224\:65ad\:662f\:5426\:5904\:4e8e fitting \:6a21\:5f0f, \:5904\:7406\:4f20\:5165\:7684 ffsMergedWithRen, \:7ed9\:51fa\:5404\:79cd\:5473\:9053\:7684\:7ed3\:679c,\:8fd9\:91cc\:53ea\:63d0\:53d6\:4e86 GEGM *)
(*-------------------- fitting \:65f6,\:4f7f\:7528\:7684\:8868\:8fbe\:5f0f --------------*)
numFFs[<|"fit"->$fittingQ_|>,ffsMergedWithRen_,chopQ2Val_]:=Query[All,<|
(*total = tree +(Z-1)*tree+loop*)
tagNum["tr+lo","uds"]->chopQ2Val[
#@recon*#@ffsTreeGEGM+#@ffsLoopGEGM/.quaCharge["uds"]]
|>&
]@ffsMergedWithRen


(*\:76ee\:524d\:7684 fitting, \:53ea\:8003\:8651 order 0 \:9636*)
(*numVal \:662f\:9884\:7559\:63a5\:53e3, \:7528\:6765\:63a7\:5236\:6570\:5b57\:683c\:5f0f\:5316*)
numFFs[<|"ord"->$parOrdStr_|>,ffsMergedWithRen_]:=Module[{chopQ2Val},
(*\:8ba1\:7b97 order 0 \:7684\:6570\:636e,\:9009\:62e9 chopQ2, \:5373\:4ee4 Q2\[Rule]0*)
chopQ2Val[x_]:=numVal@chopQ2[x];
numFFs[<|"fit"->$fittingQ|>,ffsMergedWithRen,chopQ2Val]
];


(*\:751f\:6210\:6700\:7ec8\:7684\:6570\:503c\:8868\:793a,order 0 \:7684\:8ba1\:7b97\:7ed3\:679c*)
numFFs["v","fit"]=<|
$ord0->numFFs[<|"ord"->$ord0|>,ffsMerged["WithRen"][[$ord0]]],
Nothing
|>;


(* ::Chapter:: *)
(*Fitting*)


(*\:5bf9\:672a\:5b9a\:53c2\:6570\:8fdb\:884c\:62df\:5408*)
Module[{testMagMerged,testFit,$par\[CapitalLambda]Str,ccNumStr},
(*\:6700\:540e\:7684\:7ed3\:679c\:4fdd\:5b58\:5728\:5173\:8054\:4e2d*)
ccFittings=Association@Table[
(*\:521d\:59cb\:5316 \[CapitalLambda] \:5b57\:7b26\:5f62\:5f0f *)
$par\[CapitalLambda]Str=enString@NumberForm[$par\[CapitalLambda],{3,2}];
(* \:5bf9\:7279\:5b9a \[CapitalLambda], \:5bfc\:5165\:5177\:4f53\:8ba1\:7b97\:7684\:7a0b\:5e8f,\:6811\:56fe\:ff0c\:5708\:56fe\:ff0c\:91cd\:6b63\:5316\:5e38\:6570 *)
Get["ff.numeric.worker.wl"];
(*\:5bf9\:7279\:5b9a \[CapitalLambda], \:8ba1\:7b97: \:6570\:503c\:7ed3\:679c - \:5b9e\:9a8c\:7ed3\:679c, numFFs \:5728 worker \:4e2d\:8ba1\:7b97 *)
testMagMerged=Merge[{
Query[All,(Key@tagNum["tr+lo","uds"])
/*ReplaceAll[numVal->Identity]/*Last]@numFFs["v","fit"][[$ord0]],
numOctMaget
},(First[#]-Last[#])/Last[#]&];(* (clac-expr)/expr*)
(* fitting \:51fd\:6570: \:5bf9\:4e8e\:7ed9\:5b9a\:7684 C \:503c, \:8fdb\:884c\:62df\:5408; \:5bf9\:5404\:79cd\:53ef\:80fd\:7684 fitting \:5e8f\:5217,\:6c42\:89e3c1,c2 \:7684fitting \:503c*)
testFit[ccNum_,testList_]:=NMinimize[{
Query[(Key/@testList)/*ReplaceAll[cc["C"]->ccNum]/*Total,Power[#,2]&]@testMagMerged,
{cc["c1"],cc["c2"]}\[Element]Reals},
{cc["c1"],cc["c2"]},
WorkingPrecision->$precision];(*\:7ed9\:5b9a\:6b64\:5904\:62df\:5408\:7684\:7cbe\:5ea6*)
(* \:6b21\:7ea7\:5173\:8054, \[CapitalLambda] \:6307\:5411\:5404\:79cd C fitting \:51fa\:7684\:7ed3\:679c*)
cc["\[CapitalLambda]",$par\[CapitalLambda]Str]->Association@Table[
(*\:521d\:59cb\:5316 C \:5b57\:7b26\:5f62\:5f0f*)
ccNumStr=enString@NumberForm[ccNum,{3,2}];
(*\:5faa\:73af\:8fdb\:5ea6\:63d0\:793a*)
echo["Lambda: ",$par\[CapitalLambda]Str,", C: ",ccNumStr];
(* \:6b21\:7ea7\:5173\:8054, C \:6307\:5411\:5404\:79cd \:62df\:5408\:5b50\:96c6\:7684\:7ed3\:679c, \:5982 \[CapitalSigma]+0-, pN, All,*)
cc["C",ccNumStr]->Association@KeyValueMap[#1->testFit[ccNum,#2]&,tagOctfds]
,{ccNum,{1.0`30,1.1`30,1.2`30,1.3`30,1.4`30,1.5`30}}
]
,{$par\[CapitalLambda],{0.80`30,0.85`30,0.90`30,0.95`30,1.00`30}}];
];


(* ::Section:: *)
(*export*)


(*\:4fdd\:5b58\:7ed3\:679c\:5230\:672c\:5730\:6587\:4ef6*)
serialize["ccFittings",result_]:=Block[{path},
path=FileNameJoin[{fittingsDir,"nums.ccFittings.wdx"}];
Export[path,result];
echo["Exporting finished: ", path];]


(* ::Input:: *)
(*serialize["ccFittings",ccFittings]*)


(* ::Chapter:: *)
(*EOF*)


echo[DateString[]," : finished, SessionTime : ",SessionTime[]];