(* ::Package:: *)

(* ::Title:: *)
(*ff.numeric-interpo.wl*)


(*\:5bf9\:4e8e ordFull \:7684\:6570\:636e, \:5728 Q2=0\:5904\:9700\:8981\:4f7f\:7528\:5206\:6bb5\:51fd\:6570\:ff0c\:5c06 ord0 \:7684\:503c\:5408\:5e76\:8fdb\:6765. 
\:76f4\:63a5\:5bf9 ordFull \:7684\:8868\:8fbe\:5f0f\:53d6 Q2\[Rule]0 \:4f1a\:4ea7\:751f\:6570\:503c\:9519\:8bef*)
(*\:5206\:6bb5\:51fd\:6570, \:5219 Q2\[Equal]0 \:5904\:4f7f\:7528\:7cbe\:786e\:503c, \:5728 Q2>Q2Cut \:65f6\:4f7f\:7528\:5168\:8868\:8fbe\:5f0f\:4f5c\:56fe*)
(*\:53c2\:6570; root: Q2\[Equal]0 \:7684\:503c; branch: Q2 >0 \:65f6\:7684\:8868\:8fbe\:5f0f*)
pieceWise[root_,branch_]:=Piecewise[{
{root,0<=Q2<=$Q2Cut},
{branch,Q2>$Q2Cut}
}];
(*\:5c06 \:5206\:6bb5\:51fd\:6570 MapThread \:5230 ge,gm \:4e0a*)
mapthread[{ge_,gm_}]:=MapThread[pieceWise,{ge,gm}/.{numVal->Identity}]


(*\:9012\:5f52 Merge \:5173\:8054, \:4e00\:76f4\:5408\:5e76\:5230 numKey->numVal \:5c42, \:5c06\:8fd9\:4e00\:5c42\:7684\:6570\:636e\:653e\:5165\:5206\:6bb5\:51fd\:6570*)
mergeRecur[{x:KeyValuePattern[{_numKey->_numVal}],y:KeyValuePattern[{_numKey->_numVal}]}]:=Merge[{x,y},mapthread];
mergeRecur[{x_Association,y_Association}]:=Merge[{x,y},mergeRecur];


(*\:751f\:6210 GEGM \:7684\:5206\:6bb5\:51fd\:6570*)
fullGEGM["v"]=Merge[{
Query[$ord0]@numFFs["v"],
Query[$ordFull]@numFFs["v"]
},
mergeRecur
];


(* ::Section:: *)
(*setup parallel environment*)


If[$parallel$interpoQ,
(*\:65b0\:542f\:52a8\:7684\:5185\:6838\:ff0c\:4f1a\:81ea\:52a8\:52a0\:8f7d X`*)
Needs["X`"];ParallelNeeds["X`"];
CloseKernels[];(*\:5173\:95ed\:539f\:5148\:7684\:5e76\:884c\:5185\:6838*)
Switch[{$MachineName,$System},
{"OP7050","Linux x86 (64-bit)"},LaunchKernels[6],
_,LaunchKernels[]
];,
Needs["X`"];]


(*\:5e76\:884c\:8ba1\:7b97\:521d\:59cb\:5316*)
If[$parallel$interpoQ,
(*\:4fee\:6539\:5e76\:884c\:73af\:5883\:ff1b\:526f\:4f5c\:7528*)
ParallelEvaluate[ReleaseHold@paraInitial];
(*\:5206\:914d\:5b9a\:4e49\:5230\:5e76\:884c\:5185\:6838\:4e2d*)
DistributeDefinitions[
(*\:516c\:5171\:73af\:5883\:53d8\:91cf*)
$srcRoot,$fileName,echo,enList,enString,$inNBook,
(*\:5c40\:90e8\:73af\:5883\:53d8\:91cf*)
$parOrdStr,$par\[CapitalLambda]Str,$fitScheme,$erroBar,
chopLimit,chop,$precision,
(*\:81ea\:5b9a\:4e49\:51fd\:6570*)
numPaVe
];
(*\:5173\:95ed CompiledFunction \:8b66\:544a\:ff0c\:4e0d\:5f71\:54cd\:7ed3\:679c*)
ParallelEvaluate[Off[CompiledFunction::cfn]];
]


(* ::Section:: *)
(*interpolation*)


(*\:5173\:95ed CompiledFunction \:8b66\:544a\:ff0c\:4e0d\:5f71\:54cd\:7ed3\:679c*)
Off[CompiledFunction::cfn];


ParallelEvaluate[
echoProgress[$seqNumber_][interpolation_]:=(
$seqNumber++;If[Divisible[$seqNumber,2],echo[DateString[],": ",$seqNumber]];
interpolation);
$seqNumber=0;
]


(* \:5bf9\:5f97\:5230\:7684\:5206\:6bb5\:51fd\:6570\:8fdb\:884c \:63d2\:503c *)
Module[{withEcho},
(*\:88c5\:9970\:5668*)
withEcho[parallelsubmit_]:=Query[All,All,All,All,All,
parallelsubmit[
echoProgress[$seqNumber][FunctionInterpolation][#,{Q2,0,1}]
]&
]@fullGEGM["v"];
(*eval*)
interpoGEGM["v"]=If[$parallel$interpoQ,
(*\:5e76\:884c\:60c5\:51b5\:ff0c\:5206\:53d1\:5230\:5b50\:6838\:8ba1\:7b97*)
WaitAll[withEcho[ParallelSubmit]],
(*\:975e\:5e76\:884c\:60c5\:51b5\:ff1a\:5355\:6838\:987a\:5e8f\:6267\:884c*)
withEcho[Identity]
];
]
