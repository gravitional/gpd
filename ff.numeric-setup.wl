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


(*\:8ba1\:7b97\:6570\:503c\:65f6,\:662f\:5426\:8fd0\:884c\:5e76\:884c\:5185\:6838-------------*)
$parallelQ=False;
(*\:8ba1\:7b97 order full \:63d2\:503c\:51fd\:6570\:65f6,\:662f\:5426\:8fd0\:884c\:5e76\:884c\:5185\:6838-------------*)
$parallel$interpoQ=True;
(*------------------------\:5176\:4ed6\:53c2\:6570\:8bbe\:7f6e--------------------*)
$parOrdStr=$ordFull;
$par\[CapitalLambda]=1.00;
$par\[CapitalLambda]Str=enString@NumberForm[$par\[CapitalLambda],{3,2}];
(* fitScheme \:5b9a\:4e49\:89c1: tagOctfds*)
(*$fitScheme={"\[CapitalSigma]+-","\[CapitalSigma]","\[CapitalSigma]-p","\[CapitalSigma]N","\[CapitalSigma]-\[CapitalXi]-","N","p\[CapitalXi]-","\[CapitalXi]","charged","many","most","all"};*)
(*$fitScheme={"\[CapitalSigma]N","most"};*)
$fitScheme={"\[CapitalSigma]+-","\[CapitalSigma]","\[CapitalSigma]N","N","p\[CapitalXi]-","charged","many","most","all"};
$erroBar="notbar";


(*\:5982\:679c\:8fd8\:4e0d\:5b58\:5728\:ff0c\:5219\:521b\:5efa\:76ee\:5f55*)
echo[fittingsDir=FileNameJoin[{$srcRoot,"fittings"}]];enDir[fittingsDir];
(*\:8bfb\:53d6 c1,c2 \:7684\:62df\:5408\:503c*)
ccfitted$Err=Query[Key@cc["\[CapitalLambda]",$par\[CapitalLambda]Str],All,$fitScheme
]@Import@FileNameJoin[{fittingsDir,"nums.ccFittings.wdx"}];


(*c1\[TildeTilde]3/2 \[Mu]u, c2\[TildeTilde]2/3c1-1, c3->c2-c1, cT=3/2c2+1/2,*)
numCCRelation={cc["c4"]->cc["c1"]/Sqrt[3],cc["cT"]->(3cc["c2"]+1)/2};
(* \:4e3a\:4e86\:91cd\:590d\:5229\:7528\:7ed3\:6784\:ff0c\:8fd9\:91cc\:4e0d\:6307\:5b9a c1,c2, C \:7684\:5177\:4f53\:6570\:503c*)
fittedParas=numCCRelation;


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
