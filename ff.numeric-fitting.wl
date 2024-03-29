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
(*cmd args*)


(*\:5bfc\:5165\:6b64\:8ba1\:7b97\:7a0b\:5e8f\:7684\:53c2\:6570 --------------*)
Get["ff.numeric-setup.wl"];


(* ::Input:: *)
(*(*\:5f3a\:5236\:6307\:5b9a\:547d\:4ee4\:884c\:53c2\:6570*)*)
(*$inputCml=CmdParser["pseudo"];*)


(*\:547d\:4ee4\:884c\:53c2\:6570\:6a21\:677f*)
CmdParser["template"]=<|
"opt"-><|
{"fit"}->{"True","\:662f\:5426\:5904\:4e8e fitting \:53c2\:6570\:6a21\:5f0f"}
,{"update"}->{"True","\:662f\:5426\:91cd\:65b0\:8ba1\:7b97 ffsMerged,\:8d39\:66fc\:56fe\:90e8\:5206\:6570\:503c\:7684\:7ed3\:679c"}
,{"parallel-Lbd"}->{"True","\:4ee3\:5165\:8026\:5408\:5e38\:6570\:6570\:503c\:65f6,\:662f\:5426\:8fd0\:884c\:5e76\:884c\:5185\:6838."}
,{"interp"}->{"False","\:662f\:5426\:8fd0\:884c\:5bf9 full order \:7684\:63d2\:503c\:7a0b\:5e8f."}
,{"parallel-interp"}->{"False","\:8ba1\:7b97 order full \:63d2\:503c\:51fd\:6570\:65f6,\:662f\:5426\:8fd0\:884c\:5e76\:884c\:5185\:6838"}
,{"ord"}->{"$ord0","\:5708\:79ef\:5206\:7684\:7ea7\:6570 order: \:6709 ord0, ord1, ordFull"}
|>
,"pos"->{}
|>;


CmdParser["pseudo"]={$fileName,
"--fit","True"(*\:662f\:5426\:5904\:4e8e fitting \:6a21\:5f0f*)
,"--update","True"(*\:662f\:5426\:91cd\:65b0\:8ba1\:7b97 ffsMerged,\:8d39\:66fc\:56fe\:90e8\:5206\:6570\:503c\:7684\:7ed3\:679c*)
,"--parallel-Lbd","True"(*\:4ee3\:5165\:8026\:5408\:5e38\:6570\:6570\:503c\:65f6,\:662f\:5426\:8fd0\:884c\:5e76\:884c\:5185\:6838*)
,"--ord","$ord0"(*$ordFull","\:5708\:79ef\:5206\:7684\:7ea7\:6570 order:\:6709 ord0,ord1,ordFull*)
};


(*\:89e3\:6790\:547d\:4ee4\:884c\:53c2\:6570\:ff0c\:6216\:8005\:7b14\:8bb0\:672c\:4f2a\:53c2\:6570*)
parseCml[]


(* ::Section:: *)
(*<< module*)


(* 1st \:5bfc\:5165\:6b21\:5e8f: \:8bfb\:5165\:6392\:7248 *)
(*Once@Get["gen.format.wl"];*)
(*\:5bfc\:5165\:6240\:6709\:8d39\:66fc\:56fe tag \:7684\:5217\:8868: fyAmpLoopLst,fyAmpTreeLst*)
Once@Get["gen.integral-TagList.wl"];
(*\:8981\:8ba1\:7b97\:7684\:8d39\:66fc\:56fe\:5217\:8868*)
fyAmpPart=fyAmpLoopLst;
(*\:8bfb\:5165\:5404\:79cd\:8f93\:5165\:63a5\:53e3*)
Get["coes.interface.wl"];
Get["ff.numeric-interface.wl"];


(* ::Section:: *)
(*num chop*)


(*+++++++++++++++++++++++++++ \:6570\:503c\:7cbe\:5ea6\:7684\:76f8\:5173\:8bbe\:7f6e +++++++++++++++++++++++++++*)
$chopLimit=10^-10;(*cut\:7cbe\:5ea6*)$precision=MachinePrecision;(*\:7cbe\:786e\:5ea6*)


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
numFFs["fit"]["fit"->$fittingQ_][chopQ2Val_]:=<|
(*total = tree +(Z-1)*tree+loop*)
tagNum["tr+lo","uds"]->chopQ2Val[
#@recon*#@ffsTreeGEGM+#@ffsLoopGEGM/.quaCharge["uds"]]
|>&


(*\:76ee\:524d\:7684 fitting, \:53ea\:8003\:8651 order 0 \:9636*)
(*numVal \:662f\:9884\:7559\:63a5\:53e3, \:7528\:6765\:63a7\:5236\:6570\:5b57\:683c\:5f0f\:5316*)
numFFs["fit"]["ord"->$parOrdStr_]:=Module[{chopQ2Val},
(*\:8ba1\:7b97 order 0 \:7684\:6570\:636e,\:9009\:62e9 chopQ2, \:5373\:4ee4 Q2\[Rule]0*)
chopQ2Val[x_]:=numVal@chopQ2[x];
numFFs["fit"]["fit"->$fittingQ][chopQ2Val]
];


(*\:751f\:6210\:6700\:7ec8\:7684\:6570\:503c\:8868\:793a,order 0 \:7684\:8ba1\:7b97\:7ed3\:679c,
\:6b64\:7ed3\:679c\:9700\:8981\:5148\:8fd0\:884c ff.numeric-worker.wl, \:56e0\:6b64\:9700\:8981\:5ef6\:8fdf\:8ba1\:7b97*)
numFFs["fit","val"][ffsMerged_]:=Query[
(*<bub,nobub>*)All
,(*<order>*){Key@$ord0}
,(*<octet>*)All
,(*<tree-loop-uds>*)numFFs["fit"]["ord"->$ord0]
]@ffsMerged;


(* ::Chapter:: *)
(*Fitting*)


(*\:5bf9\:672a\:5b9a\:53c2\:6570\:8fdb\:884c\:62df\:5408*)
Module[{testMagMerged,testFit,$LambdaNumStr,ccNumStr},
(*\:6700\:540e\:7684\:7ed3\:679c\:4fdd\:5b58\:5728\:5173\:8054\:4e2d*)
ccFittings["tmp"]=Association@Table[
(*\:5bf9 \[CapitalLambda] \:7684\:53d6\:503c\:8fed\:4ee3 ---------------------------------*)
(*\:8ba1\:7b97 \[CapitalLambda] \:7684\:5b57\:7b26\:5f62\:5f0f*)
$LambdaNumStr=enString@NumberForm[$LambdaNum,{3,2}];
(* \:5bf9\:7279\:5b9a \[CapitalLambda], \:5bfc\:5165\:5177\:4f53\:8ba1\:7b97\:7684\:7a0b\:5e8f,\:6811\:56fe,\:5708\:56fe,\:91cd\:6b63\:5316\:5e38\:6570 *)
Get["ff.numeric-worker.wl"];
(*\:5bf9\:7279\:5b9a \[CapitalLambda], \:8ba1\:7b97: \:6570\:503c\:7ed3\:679c - \:5b9e\:9a8c\:7ed3\:679c, numFFs \:5728 worker \:4e2d\:8ba1\:7b97 *)
(*<\[CapitalLambda]\[Rule]v>, \[CapitalLambda] \:6307\:5411 bub,nobub -------------------------*)
cc["\[CapitalLambda]",$LambdaNumStr]->Association@Table[
(*<bub\[Rule]v,nobub\[Rule]v>, \:6307\:5411\:5404\:79cd C fitting \:51fa\:7684\:7ed3\:679c -------------*)
adjust->Association@Table[
(*\:5b9e\:9a8c\:503c\:4e0e\:8ba1\:7b97\:503c\:7684 \[Chi]^2 ---*)
testMagMerged=Merge[{
Query[
(*<bub,nobub>*)Key@adjust
,(*<order>*)Key@$ord0
,(*<octet>}*)All
,(Key@tagNum["tr+lo","uds"])/*ReplaceAll[numVal->Identity]/*Last
]@numFFs["fit","val"][ffsMerged["WithRen",keyTreeAndLoop]],
numOctMaget
},(First[#]-Last[#])/Last[#]&];(* (clac-expr)/expr *)
(* fitting \:51fd\:6570: \:5bf9\:4e8e\:7ed9\:5b9a\:7684 C \:503c, \:8fdb\:884c\:62df\:5408; \:5bf9\:5404\:79cd\:53ef\:80fd\:7684 fitting \:5e8f\:5217,\:6c42\:89e3c1,c2 \:7684fitting \:503c*)
testFit[ccNum_,testList_]:=NMinimize[{
(*\:62df\:5408\:7684\:76ee\:6807\:51fd\:6570-----------------*)
Query[
(*\:53d6\:51fa\:62df\:5408\:65b9\:6848\:4e2d\:7684oct\:7ed3\:679c\:7684 diff^2;\:6c42\:548c*)
(Key/@testList)/*ReplaceAll[cc["C"]->ccNum]/*Total
(*\:5bf9\:6bcf\:4e2adiff\:5e73\:65b9*)
,Power[#,2]&]@testMagMerged,
(*constraints: \:7ed9\:51fa\:53c2\:6570\:53d6\:503c\:7684\:9650\:5236\:8303\:56f4-------------------*)
(*\:5404\:53c2\:6570\:5747\:5c5e\:4e8e\:5b9e\:6570\:57df*)
{cc["c1"],cc["c2"],cc["b9"],cc["b10"],cc["b11"]}\[Element]Reals &&
(*\:4e0d\:5bf9 b9,b10,b11 \:8fdb\:884cfit, \:7b49\:4ef7\:4e8e\:56fa\:5b9a\:5b83\:4eec\:7684\:53d6\:503c*)
cc["b9"]==1.36 &&
cc["b10"]==1.24 &&
cc["b11"]==0.46
}
(*\:8981\:62df\:5408\:7684\:53d8\:91cf*)
,{cc["c1"],cc["c2"],cc["b9"],cc["b10"],cc["b11"]}
,WorkingPrecision->$precision];(*\:7ed9\:5b9a\:6b64\:5904\:62df\:5408\:7684\:7cbe\:5ea6*)
(*\:521d\:59cb\:5316 C \:7684\:5b57\:7b26\:5f62\:5f0f-----*)
ccNumStr=enString@NumberForm[ccNum,{3,2}];
(*\:5faa\:73af\:8fdb\:5ea6\:63d0\:793a*)
echo["Lambda: ",$LambdaNumStr,", C: ",ccNumStr];
(*<cc["C",1.00]\[Rule]v>, \:6307\:5411\:5404\:79cd\:62df\:5408\:65b9\:6848\:7684\:7ed3\:679c, \:5982 \[CapitalSigma]+0-, pN, All,----------------*)
cc["C",ccNumStr]->Association@
KeyValueMap[#1->testFit[ccNum,#2]&,
$fittingScheme]
(*\:5bf9 C \:7684\:53d6\:503c\:8fed\:4ee3---------------------*)
,{ccNum,{1.0`30,1.1`30,1.2`30,1.3`30,1.4`30,1.5`30}}]
(*\:5bf9\:624b\:52a8\:6c42\:548c\:65b9\:6848 {bub,nobub} \:8fed\:4ee3---------------------*)
,{adjust,Keys@fyCoesAdjust}]
(*\:5bf9 \[CapitalLambda] \:7684\:53d6\:503c\:8303\:56f4\:8fed\:4ee3---------------------*)
,{$LambdaNum,{
0.70`30,0.75`30,
0.80`30,0.90`30,0.95`30,
1.00`30,1.1`30
}}];
];


(* ::Section:: *)
(*add manual configuration*)


ccFittings["v"]=Query[(*<cc[\[CapitalLambda],0.70]>*)All
,(*<bub>*)All
,(*<cc["C","1.00"]>*)All
(*<scheme>;\:6dfb\:52a0\:624b\:52a8\:6307\:5b9a\:7684\:4f4e\:80fd\:8026\:5408\:5e38\:6570\:53d6\:503c*)
,Append[{
"he1"->{1,{cc["c1"]->2.081,cc["c2"]->0.788,cc["b9"]->1.36,cc["b10"]->1.24,cc["b11"]->0.46}}
,"he2"->{1,{cc["c1"]->3.077,cc["c2"]->1.05133,cc["b9"]->1.36,cc["b10"]->1.24,cc["b11"]->0.46}}
,"he2202"->{1,{cc["c1"]->1.40,cc["c2"]->0.54,cc["b9"]->1.36,cc["b10"]->1.24,cc["b11"]->0.46}}
}]
]@ccFittings["tmp"];


(* ::Section:: *)
(*export*)


temp`fitName="nums.ccFittings.wdx";
serialize[fittingsDir]["nums.ccFittings.wdx",ccFittings["v"]]


(* ::Section:: *)
(*scan*)


(* ::Input:: *)
(*Query[*)
(*(*<\[CapitalLambda] value>*)All/*Normal/*(Column[#,Frame->All]&)*)
(*,(*<bub>*){coesAdjBub}*)
(*,(*<C value>*)All/*Normal/*TableForm*)
(*,(*<scheme>*){"many"}*)
(*]@Import@localPath[fittingsDir]["nums.ccFittings.wdx"]*)


(* ::Chapter:: *)
(*EOF*)


echo[DateString[]," : finished, SessionTime : ",SessionTime[]];
