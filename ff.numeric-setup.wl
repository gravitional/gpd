(* ::Package:: *)

(* ::Section:: *)
(*local cache directory*)


(*\:7cfb\:6570\:6587\:4ef6\:7684\:6587\:4ef6\:5939*)
coesDir=FileNameJoin[{$srcRoot,"coes"}];
(*\:79ef\:5206\:8868\:8fbe\:5f0f\:7684\:6587\:4ef6\:5939*)
mfilesDir=FileNameJoin[{$srcRoot,"mfiles"}];
(*\:4fdd\:5b58\:8ba1\:7b97\:7ed3\:679c\:7684\:6587\:4ef6\:5939*)
resultsDir=FileNameJoin[{$srcRoot,"results"}];enDir[resultsDir];


(* ::Section:: *)
(*num chop*)


(*+++++++++++++++++++++++++++ \:6570\:503c\:7cbe\:5ea6\:7684\:76f8\:5173\:8bbe\:7f6e +++++++++++++++++++++++++++*)
$chopLimit=10^-10;(*cut\:7cbe\:5ea6*)$precision=MachinePrecision;(*\:7cbe\:786e\:5ea6*)
$Q2Cut=0.0001;


(* ::Section:: *)
(*cmd arguments*)


(* \:5904\:7406\:811a\:672c\:53c2\:6570,\:6a21\:62df\:547d\:4ee4\:884c\:8f93\:5165\:53c2\:6570\:7684\:60c5\:5f62 *)
If[!$Notebooks,(*\:5982\:679c\:5728\:547d\:4ee4\:884c\:6267\:884c*)
$inputCml=$ScriptCommandLine,
(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c, \:6a21\:4eff\:547d\:4ee4\:884c, \:7b2c\:4e00\:4e2a\:53c2\:6570\:662f\:811a\:672c\:7684\:7edd\:5bf9\:8def\:5f84*)
$inputCml={$fileName,
(*\:5728\:8fd9\:91cc\:63d0\:4f9b\:5176\:4ed6\:53c2\:6570, \:4f7f\:7528 mathematica \:8bed\:6cd5\:4e0b\:7684\:5f62\:5f0f\:ff0c\:5916\:9762\:7684 enString \:4f1a\:81ea\:52a8\:8f6c\:6362\:6210\:5b57\:7b26\:4e32, \:5c3d\:91cf\:591a\:4f7f\:7528Association\:7ed3\:6784*)
$ordFull,0.90`30,1.50`30,"Baryons","notbar"
}];
echo["the input parameter is:\n",$inputCml];


(*\:5982\:679c\:8fd8\:4e0d\:5b58\:5728\:ff0c\:5219\:521b\:5efa\:76ee\:5f55*)
echo[fittingsDir=FileNameJoin[{$srcRoot,"fittings"}]];enDir[fittingsDir];
(*\:8bfb\:53d6 c1,c2 \:7684\:62df\:5408\:503c*)
ccfitted$Err=Query[Key@cc["\[CapitalLambda]",$par\[CapitalLambda]Str],All,$fitScheme
]@Import@FileNameJoin[{fittingsDir,"nums.ccFittings.wdx"}];


(*c1\[TildeTilde]3/2 \[Mu]u, c2\[TildeTilde]2/3c1-1, c3->c2-c1, cT=3/2c2+1/2,*)
numCCRelation={cc["c4"]->cc["c1"]/Sqrt[3],cc["cT"]->(3cc["c2"]+1)/2};
(* \:4e3a\:4e86\:91cd\:590d\:5229\:7528\:7ed3\:6784\:ff0c\:8fd9\:91cc\:4e0d\:6307\:5b9a c1,c2, C \:7684\:5177\:4f53\:6570\:503c*)
fittedParas=numCCRelation;
recordLocationInMessage@numCCRelation;


(*\:58f0\:660e\:4e00\:4e9b\:8fd0\:52a8\:5b66\:53d8\:91cf,\:4f7f\:7528 atom \:8868\:8fbe\:5f0f\:ff0cpackage-X \:7684 loopIntegrate \:9700\:8981\:79ef\:5206\:52a8\:91cf\:4e3a\:539f\:5b50\:8868\:8fbe\:5f0f*)
\[CapitalLambda];(*\:6b63\:89c4\:5b50\:8d28\:91cf*)
mE;(*\:5916\:817f\:7c92\:5b50\:7684\:8d28\:91cf*);
mm1;mm2;(*\:4e2d\:95f4\:516b\:91cd\:6001\:4ecb\:5b50*)
mo1;mo2;(*\:4e2d\:95f4\:516b\:91cd\:6001\:91cd\:5b50*)
md1;md2;(*\:4e2d\:95f4\:5341\:91cd\:6001\:91cd\:5b50*)
Q2;(*Q2=-q^2,\:8f6c\:79fb\:52a8\:91cf\:5e73\:65b9\:7684\:8d1f\:503c*)


(*--------------------------\:6682\:5b58\:7ed3\:679c--------------------------*)
(*\:5982\:679c\:8fd8\:4e0d\:5b58\:5728\:ff0c\:5219\:521b\:5efa\:76ee\:5f55*)
echo["save in directory: ",resultsDir=FileNameJoin[{$srcRoot,"results"}]];enDir[resultsDir];
(*\:7ed9\:51fa\:672c\:5730\:7f13\:5b58\:6587\:4ef6\:7684\:8def\:5f84*)
localCachePath[filename_String]:=FileNameJoin[{resultsDir,StringRiffle[{filename,$parOrdStr,"Lambda",$par\[CapitalLambda]Str,$erroBar},"-"]<>".wdx"}];
(*io \:51fd\:6570, \:4fdd\:5b58\:7ed3\:679c\:5230\:672c\:5730\:6587\:4ef6*)
serialize[filename_String,result_]:=With[{path=localCachePath[filename]},
Export[path,result];echo["Exporting finished: ", path];]
recordLocationInMessage@serialize;


(* ::Section:: *)
(*numeric cc RuleList*)


(* \:62df\:5408\:51fa\:7684 c1,c2 \:503c\:88ab\:653e\:5728 ccfitted$Err \:4e2d, \:4f7f\:7528 Query \:67e5\:8be2; \:8003\:8651 \:53c2\:6570\:4e4b\:95f4\:7684\:9650\:5236\:ff0c\:7ed9\:51fa\:6240\:6709\:53c2\:6570\:7684\:6570\:503c*)
apply$cc$numeric::usage="apply$cc$numeric[$parC][ffsMerged,ccfitted]
$parC \:662f C\:53c2\:6570 \:7684\:503c, ffsMerged \:662f\:51fd\:6570, \:6d88\:8d39\:751f\:6210\:7684 {C,c1,c2} \:7684\:66ff\:6362\:89c4\:5219, 
ccfitted \:662f fit \:7684\:7ed3\:679c, {c1,c2}\:7684\:6570\:503c\:5217\:8868";
apply$cc$numeric[$parC_][ffsMerged_,ccfitted_]:=Module[{fittedParas},
fittedParas=Join[ccfitted,numCCRelation/.ccfitted,{cc["C"]->$parC}];
(*\:5c06\:53c2\:6570 c1,c2,C \:7684\:5177\:4f53\:503c\:4ee3\:5165\:8868\:8fbe\:5f0f, ffsMerged \:662f\:51fd\:6570*)
ffsMerged@fittedParas
];
recordLocationInMessage@apply$cc$numeric;


(* ::Section:: *)
(*numeric FormFactors*)


recordLocationInMessage@numFFs


(*\:5904\:7406\:4f20\:5165\:7684 ffsMergedWithRen,\:4ee3\:5165\:5938\:514b\:7535\:8377\:914d\:7f6e,\:7ed9\:51fa\:5404\:79cd\:5473\:9053\:7684\:7ed3\:679c,\:8fd9\:91cc\:53ea\:63d0\:53d6\:4e86 GEGM *)
(*\:666e\:901a\:8ba1\:7b97, \:4f7f\:7528\:7684\:6570\:503c\:8868\:8fbe\:5f0f*)
numFFs[chopQ2Val_]:=<|
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


(*\:6839\:636e\:8ba1\:7b97\:7684\:5c55\:5f00\:9636\:6570, $parOrdStr, \:9009\:62e9\:4e0d\:540c\:7684\:6570\:503c\:5904\:7406\:65b9\:5f0f; numVal \:662f\:9884\:7559\:63a5\:53e3, \:7528\:6765\:63a7\:5236\:6570\:5b57\:683c\:5f0f\:5316*)
numFFs[<|"ord"->$parOrdStr_|>]:=Module[{chopQ2Val},
Switch[$parOrdStr,
(*\:5982\:679c\:8ba1\:7b97 order 0 \:7684\:6570\:636e,\:9009\:62e9 chopQ2, \:5373\:4ee4 Q2\[Rule]0*)
$ord0,
chopQ2Val[x_]:=numVal@chopQ2[x];
numFFs[chopQ2Val],
(*\:5982\:679c\:8ba1\:7b97 order 1 order full \:7684\:6570\:636e, \:9009\:62e9 chop, \:4fdd\:7559 Q2 \:4f9d\:8d56*)
_,
chopQ2Val[x_]:=numVal@chop[x];
numFFs[chopQ2Val]
]];
