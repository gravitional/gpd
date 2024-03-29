(* ::Package:: *)

(* ::Chapter:: *)
(*numeric interface*)


(* ::Section:: *)
(*num chop*)


(*+++++++++++++++++++++++++++ \:6570\:503c\:7cbe\:5ea6\:7684\:76f8\:5173\:8bbe\:7f6e +++++++++++++++++++++++++++*)
$chopLimit=10^-10;(*cut\:7cbe\:5ea6*)$precision=MachinePrecision;(*\:7cbe\:786e\:5ea6*)
$Q2Cut=0.0001;


(* ::Section:: *)
(*<< modules*)


If[$parallel$couplsQ,
(*\:65b0\:542f\:52a8\:7684\:5185\:6838\:ff0c\:4f1a\:81ea\:52a8\:52a0\:8f7d X`*)
Needs["X`"];ParallelNeeds["X`"];
CloseKernels[];(*\:542f\:52a8\:5e76\:884c\:5185\:6838*)
Switch[{$MachineName,$System},
{"OP7050","Linux x86 (64-bit)"},LaunchKernels[6],
_,LaunchKernels[]
];,
Needs["X`"];]


(*\:53d6\:6574\:51fd\:6570\:ff0c\:820d\:5f03\:5fae\:5c0f\:7684\:6570\:503c\:8bef\:5dee*)
Default[chop,2]=$chopLimit;
chop[expr_,limit_.]:=Chop[expr,limit]
(*\:6839\:636e\:6c42\:89e3\:7684\:7ea7\:6570 level, \:8bbe\:5b9a\:4e0d\:540c\:7684 chop \:65b9\:5f0f*)
chopQ2[x_]:=Simplify@chop[x/.Q2->0];
(*----------- PaVe\:4e3b\:79ef\:5206 \:89e3\:6790\:5f0f\:4e2d\:7684\:7279\:6b8a\:51fd\:6570, \:5ef6\:8fdf Chop \:907f\:514dDiscB\:5e26\:6765\:7684\:5fae\:5c0f\:5047\:865a\:90e8 -----------*)
DiscBChop[x__]:=chop[DiscB[x]]/;And@@NumericQ/@{x}(*\:5f53\:8f93\:5165\:662f\:6570\:5b57\:7684\:65f6\:5019\:ff0c\:624d\:8fdb\:884cchop*)
ScalarC0Chop[x__]:=chop[ScalarC0[x]]/;And@@NumericQ/@{x}(*\:5f53\:8f93\:5165\:662f\:6570\:5b57\:7684\:65f6\:5019\:ff0c\:624d\:8fdb\:884cchop*)
(*\:7279\:6b8a\:51fd\:6570\:7684 \:5ef6\:8fdfChop*)
numPaVe={DiscB->DiscBChop,ScalarC0->ScalarC0Chop};


(* ::Section:: *)
(*<< coupling relations*)


recordLocationInMessage[ccfitted$Err];


(*\:8bfb\:53d6 c1,c2 \:7684\:62df\:5408\:503c, \:6839\:636e $LambdaFitStr \:7684\:503c\:6765\:9009\:53d6 ---------------*)
(*\:53ea\:5728\:975e\:62df\:5408\:60c5\:5f62,\:5bfc\:5165\:62df\:5408\:597d\:7684\:53c2\:6570*)
If[!$fittingQ,
ccfitted$Err=Query[
(*<\[CapitalLambda] value>*)Key@cc["\[CapitalLambda]",$LambdaFitStr]
,(*<bub>*)All
,(*<C value>*)All
,(*fit-scheme*)$fitScheme
]@Import@FileNameJoin[{fittingsDir,"nums.ccFittings.wdx"}];
]


recordLocationInMessage[magCCRelation];
(* \:4e3a\:4e86\:91cd\:590d\:5229\:7528\:7ed3\:6784\:ff0c\:8fd9\:91cc\:4e0d\:6307\:5b9a c1,c2, C \:7684\:5177\:4f53\:6570\:503c*)
(*c1\[TildeTilde]3/2 \[Mu]u, c2\[TildeTilde]2/3c1-1, c3->c2-c1, cT=3/2c2+1/2,*)
magCCRelation={cc["c4"]->cc["c1"]/Sqrt[3],cc["cT"]->(3cc["c2"]+1)/2};


(*\:58f0\:660e\:4e00\:4e9b\:8fd0\:52a8\:5b66\:53d8\:91cf,\:4f7f\:7528 atom \:8868\:8fbe\:5f0f\:ff0cpackage-X \:7684 loopIntegrate \:9700\:8981\:79ef\:5206\:52a8\:91cf\:4e3a\:539f\:5b50\:8868\:8fbe\:5f0f*)
\[CapitalLambda];(*\:6b63\:89c4\:5b50\:8d28\:91cf*)
mE;(*\:5916\:817f\:7c92\:5b50\:7684\:8d28\:91cf*);
mm1;mm2;(*\:4e2d\:95f4\:516b\:91cd\:6001\:4ecb\:5b50*)
mo1;mo2;(*\:4e2d\:95f4\:516b\:91cd\:6001\:91cd\:5b50*)
md1;md2;(*\:4e2d\:95f4\:5341\:91cd\:6001\:91cd\:5b50*)
Q2;(*Q2=-q^2,\:8f6c\:79fb\:52a8\:91cf\:5e73\:65b9\:7684\:8d1f\:503c*)


(* ::Section:: *)
(*numeric cc RuleList*)


recordLocationInMessage@apply$cc$numeric;


(* \:62df\:5408\:51fa\:7684 c1,c2 \:503c\:88ab\:653e\:5728 ccfitted$Err \:4e2d, \:4f7f\:7528 Query \:67e5\:8be2; \:8003\:8651 \:53c2\:6570\:4e4b\:95f4\:7684\:9650\:5236\:ff0c\:7ed9\:51fa\:6240\:6709\:53c2\:6570\:7684\:6570\:503c*)
(*\:4e4b\:540e\:53ef\:4ee5\:5c06 \:53c2\:6570 c1,c2,C \:7684 \:66ff\:6362\:89c4\:5219\:4ee3\:5165\:8868\:8fbe\:5f0f, ffsMerged \:662f\:51fd\:6570*)
apply$cc$numeric::usage="apply$cc$numeric[$parC_][ccfitted_]
$parC \:662f C\:53c2\:6570 \:7684\:503c, ccfitted \:662f fit \:7684\:7ed3\:679c, {c1,c2}\:7684\:6570\:503c\:5217\:8868";
apply$cc$numeric[$parC_][ccfitted_]:=Join[
ccfitted,magCCRelation/.ccfitted,{cc["C"]->$parC}];


(* ::Section:: *)
(*numeric FormFactors*)


recordLocationInMessage@numFFs


(*\:6839\:636e\:8ba1\:7b97\:7684\:5c55\:5f00\:9636\:6570, $parOrdStr, \:9009\:62e9\:4e0d\:540c\:7684\:6570\:503c\:5904\:7406\:65b9\:5f0f; numVal \:662f\:9884\:7559\:63a5\:53e3, \:7528\:6765\:63a7\:5236\:6570\:5b57\:683c\:5f0f\:5316*)
(* numFFsChopQ2Val \:5373\:4e0a\:9762\:7684 numFFs["tree+loop"][chopQ2Val_]*)
numFFs["fn"][<|"ord"->$parOrdStr_|>][numFFs_]:=Module[{chopQ2Val},
Switch[$parOrdStr,
(*\:5982\:679c\:8ba1\:7b97 order 0 \:7684\:6570\:636e,\:9009\:62e9 chopQ2, \:5373\:4ee4 Q2\[Rule]0*)
$ord0,
chopQ2Val[x_]:=numVal@chopQ2[x];
numFFs@chopQ2Val,
(*\:5982\:679c\:8ba1\:7b97 order 1 order full \:7684\:6570\:636e, \:9009\:62e9 chop, \:4fdd\:7559 Q2 \:4f9d\:8d56*)
_,
chopQ2Val[x_]:=numVal@chop[x];
numFFs@chopQ2Val
]];
