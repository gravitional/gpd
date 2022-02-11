(* ::Package:: *)

(* ::Title:: *)
(*ff.numeric-interpo.wl*)


(* ::Section:: *)
(*setup parallel environment*)


If[$parallel$interpQ,
(*\:65b0\:542f\:52a8\:7684\:5185\:6838\:ff0c\:4f1a\:81ea\:52a8\:52a0\:8f7d X`*)
Needs["X`"];ParallelNeeds["X`"];
CloseKernels[];(*\:5173\:95ed\:539f\:5148\:7684\:5e76\:884c\:5185\:6838*)
Switch[{$MachineName,$System},
{"OP7050","Linux x86 (64-bit)"},LaunchKernels[6],
_,LaunchKernels[]
];,
Needs["X`"];]


(*\:5e76\:884c\:8ba1\:7b97\:521d\:59cb\:5316*)
If[$parallel$interpQ,
(*\:4fee\:6539\:5e76\:884c\:73af\:5883\:ff1b\:526f\:4f5c\:7528*)
ParallelEvaluate[ReleaseHold@paraInitial];
(*\:5206\:914d\:5b9a\:4e49\:5230\:5e76\:884c\:5185\:6838\:4e2d*)
DistributeDefinitions[
(*\:516c\:5171\:73af\:5883\:53d8\:91cf*)
$srcRoot,$fileName,echo,enList,enString,$inNBook,
(*\:5c40\:90e8\:73af\:5883\:53d8\:91cf*)
$parOrdStr,$LambdaNumStr,$fitScheme,$LambdaFit,
$chopLimit,chop,$precision,
(*\:81ea\:5b9a\:4e49\:51fd\:6570*)
numPaVe
];
(*\:5173\:95ed CompiledFunction \:8b66\:544a\:ff0c\:4e0d\:5f71\:54cd\:7ed3\:679c*)
ParallelEvaluate[Off[CompiledFunction::cfn]];
]
(*\:5173\:95ed \:4e3b\:6838 CompiledFunction \:8b66\:544a*)
Off[CompiledFunction::cfn];


(* ::Section:: *)
(*print evaluation progress*)


newIter[iter_Symbol]:=Module[(*\:5c40\:90e8\:53d8\:91cf index \:5b58\:50a8 \:5f53\:524d\:7684\:6570\:5b57,\:521d\:59cb\:5316\:4e3a 0*){index=0},
(*next \:65b9\:6cd5,\:5c06 index \:7684\:503c \:52a0\:52a0*)
iter["next"]:=(index++;);
(*\:76f8\:5f53\:4e8e get \:65b9\:6cd5,\:8fd4\:56de \:5f53\:524d index*)
iter[]:=index;
(*\:6700\:540e\:8fd4\:56de\:6784\:9020\:7684\:961f\:5217\:5b9e\:4f8b*)iter];
(*\:521b\:5efa\:8ba1\:6570\:5668, \:8bbe\:7f6e\:4e3a\:5171\:4eab\:51fd\:6570*)
newIter[$counter];SetSharedFunction[$counter]


(*\:88c5\:9970\:5668\:ff0c\:6253\:5370\:51fd\:6570\:6267\:884c\:8fdb\:5ea6*)
echoProgress[$counter_][FunctionInterpolation_]:=Extract[{2,1,1}]@Reap[
(*\:8c03\:7528\:51fd\:6570\:8ba1\:7b97,\:8fd4\:56de\:5f97\:5230\:7684\:503c*)
Sow@FunctionInterpolation;
(*\:8bb0\:5f55\:8c03\:7528\:6b21\:6570\:ff0c\:6267\:884c\:591a\:6b21\:540e\:ff0c\:6253\:5370\:4e00\:6b21\:63d0\:793a*)
$counter["next"];
(*\:6253\:5370\:95f4\:9694;fits:2,CC:6,oct:8,contr:9,GEGM:2*)
If[Divisible[$counter[],8*9],echo[DateString[],": $count: ",$counter[]]];
];
(*\:5c06\:5b9a\:4e49\:5206\:53d1\:5230\:5b50\:6838*)
DistributeDefinitions[echoProgress];


(* ::Section:: *)
(*interpolation*)


(*\:7531\:4e8e Association \:5177\:6709 HoldAllComplete \:5c5e\:6027, \:4f1a\:5361\:4f4f\:8ba1\:7b97\:ff0c\:6240\:4ee5\:9700\:8981\:5148\:66ff\:6362\:6210\:666e\:901a\:51fd\:6570\:5934\:90e8 *)
waitAssoc[data_]:=Module[{assoc},
WaitAll[data/.Association->assoc]/.assoc->Association]


echo[DateString[],": $start interpolation on curves"]


(* \:5bf9\:5f97\:5230\:7684\:5206\:6bb5\:51fd\:6570\:8fdb\:884c \:63d2\:503c *)
(*\:6839\:636e\:5e76\:884c\:8bbe\:7f6e\:ff0cevaluation*)
interpoGEGM["v",keyTreeAndLoop]=If[$parallel$interpQ,
(*\:5e76\:884c\:60c5\:51b5\:ff0c\:5206\:53d1\:5230\:5b50\:6838\:8ba1\:7b97------------------------------*)
waitAssoc@Query[
(*<bub,nobub>*)All
,(*<ccValue>*)All
,(*<fitScheme>*)All
,(*<octet>*)All
,(*<tree-loop-uds>*)All
,(*<GEGM>*)All,
ParallelSubmit[echoProgress[$counter][FunctionInterpolation[#,{Q2,0,1}]]]&
]@fullGEGM["v",keyTreeAndLoop],
(*\:975e\:5e76\:884c\:60c5\:51b5\:ff1a\:5355\:6838\:987a\:5e8f\:6267\:884c--------------------------*)
Query[
(*<bub,nobub>*)All
,(*<ccValue>*)All
,(*<fitScheme>*)All
,(*<octet>*)All
,(*<contribution>*)All
,(*<GEGM>*)All,
echoProgress[$counter][FunctionInterpolation[#,{Q2,0,1}]]&
]@fullGEGM["v",keyTreeAndLoop]
];
