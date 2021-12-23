(* ::Package:: *)

(* ::Title:: *)
(*ff.numeric-eval.wl*)


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
(*para & import module*)


(*\:8ba1\:7b97\:6570\:503c\:65f6,\:662f\:5426\:8fd0\:884c\:5e76\:884c\:5185\:6838-------------*)
$parallelQ=False;
(*\:8ba1\:7b97 order full \:63d2\:503c\:51fd\:6570\:65f6,\:662f\:5426\:8fd0\:884c\:5e76\:884c\:5185\:6838-------------*)
$parallel$interpoQ=True;
(*------------------------\:5176\:4ed6\:53c2\:6570\:8bbe\:7f6e--------------------*)
$parOrdStr=$ordFull;
$par\[CapitalLambda]=0.90;
$par\[CapitalLambda]Str=enString@NumberForm[$par\[CapitalLambda],{3,2}];
(* fitScheme \:5b9a\:4e49\:89c1: $fittingScheme*)
(*$fitScheme={"\[CapitalSigma]+-","\[CapitalSigma]","\[CapitalSigma]-p","\[CapitalSigma]N","\[CapitalSigma]-\[CapitalXi]-","N","p\[CapitalXi]-","\[CapitalXi]","charged","many","most","all"};*)
(*$fitScheme={"\[CapitalSigma]N","most"};*)
$fitScheme={"\[CapitalSigma]+-","\[CapitalSigma]","\[CapitalSigma]N","N","p\[CapitalXi]-","charged","many","most","all"};
$erroBar="notbar";


(*\:5bfc\:5165\:6240\:6709\:8d39\:66fc\:56fe tag \:7684\:5217\:8868: fyAmpLoopLst,fyAmpTreeLst*)
Once@Get["gen.integral-TagList.wl"];
(*\:8981\:8ba1\:7b97\:7684\:8d39\:66fc\:56fe\:5217\:8868*)
fyAmpPart=fyAmpLoopLst;
(*\:8bfb\:5165\:5404\:79cd\:8f93\:5165\:63a5\:53e3*)
Once@Get["coes.interface.wl"];
(*\:5bfc\:5165\:6b64\:8ba1\:7b97\:7a0b\:5e8f\:7684\:53c2\:6570----------------------------------------------------------*)
Get["ff.numeric-setup.wl"];


(* ::Section:: *)
(*import loop result*)


(* \:5bfc\:5165\:5177\:4f53\:8ba1\:7b97\:7684\:7a0b\:5e8f,\:6811\:56fe\:ff0c\:5708\:56fe\:ff0c\:91cd\:6b63\:5316\:5e38\:6570 *)
(*order full, Intel i7-6700 (8): \:4ee3\:5165\:6240\:6709\:6570\:503c, ~ 4m30s ; \:4ee3\:5165\:90e8\:5206\:6570\:503c, ~3m50s *)
Get["ff.numeric-worker.wl"];


(*\:4fdd\:5b58\:5230\:78c1\:76d8\:6587\:4ef6\:ff1a\:5708\:56fe\:7684\:8ba1\:7b97\:7ed3\:679c*)
serialize["loop-result",loopResults["v"]];


(* ::Chapter:: *)
(*numeric Form Factors Merged; all series*)


(*\:63d0\:53d6\:51fa ffsMerged \:8ba1\:7b97\:7ed3\:679c\:4e2d, \:5173\:5fc3\:7684\:90e8\:5206, TreeGEGM,LoopGEGM,\:91cd\:6b63\:5316\:5e38\:6570*)
ffsMerged["trimed"]=Query[All,All,Key/@{ffsTreeGEGM,ffsLoopGEGM,recon}]@ffsMerged["WithRen"];


(*\:4fdd\:5b58\:5230\:78c1\:76d8\:6587\:4ef6\:ff1a\:5708\:56fe\:7684\:8ba1\:7b97\:7ed3\:679c*)
(*serialize["ffsMerged-trimed",ffsMerged["trimed"]];*)


(* \:62df\:5408\:51fa\:7684 c1,c2 \:503c\:88ab\:653e\:5728 ccfitted$Err \:4e2d, \:4f7f\:7528 Query \:67e5\:8be2; \:8003\:8651 \:53c2\:6570\:4e4b\:95f4\:7684\:9650\:5236\:ff0c\:7ed9\:51fa\:6240\:6709\:53c2\:6570\:7684\:6570\:503c*)
(*\:5404\:79cd C,c1,c2 \:914d\:7f6e\:4e0b\:ff0c\:8ba1\:7b97\:51fa\:7684\:6570\:503c\:7ed3\:679c\:ff0c\:5938\:514b\:7535\:8377\:548c Q2 \:672a\:4ee3\:5165\:5177\:4f53\:6570\:503c*)
Module[{ccNumStr},
ffsMerged["confs"]=Association@Table[
(*\:521d\:59cb\:5316 C \:5b57\:7b26\:5f62\:5f0f*)
ccNumStr=enString@NumberForm[ccNum,{3,2}];
(*\:4ee3\:5165\:62df\:5408\:786e\:5b9a\:7684\:6570\:503c*)
Query[{Key@cc["C",ccNumStr]},All(*\:8ba1\:7b97\:6240\:6709\:62df\:5408\:65b9\:6848*),
(* \:5e94\:7528\:5230 ccfitted$Err \:6570\:636e\:5c42\:7684\:51fd\:6570*)
apply$cc$numeric[ccNum][(*\:9996\:53c2\:6570\:662f\:51fd\:6570\:ff0c\:6d88\:8d39\:4ea7\:751f \:53c2\:6570\:89c4\:5219*)
Query[All,All,All,ReplaceAll[#]]@ffsMerged["trimed"]&,
(*ccfitted \:7684\:9996\:5143\:7d20\:662f\:8bef\:5dee\:ff0c\:672b\:5143\:7d20\:662f c \:7684\:66ff\:6362\:89c4\:5219 *)
Last@#]&
]@ccfitted$Err
(* C\:7684\:8fed\:4ee3\:5217\:8868 *)
,{ccNum,{1.0,1.1,1.2,1.3,1.4,1.5}} 
]];


(* ::Section:: *)
(*renormalization constant*)


(*\:6253\:5370\:573a\:5f3a\:91cd\:6b63\:5316\:5e38\:6570*)
If[$inNBook,
Query[Normal/*TableForm,
(*fitting Scheme \:5c42*)1,
Key@$ord0,Values/*StringRiffle,
Key@recon,NumberForm[#,{4,3}]&
]@ffsMerged["confs"]
]


renormalConst["v"]=Query[(*CC \:5c42*)All,(*fitting Scheme \:5c42*)1,$ord0,
(*octet \:5c42*)All,(*\:6811 \:5708 \:91cd\:6b63\:5316\:5c42*)Key@recon,(*F1F2/GEGM \:5c42*)All
]@ffsMerged["confs"];


(* ::Chapter:: *)
(*numeric FFs; each contribution; all series*)


(*\:751f\:6210\:6700\:7ec8\:7684\:6570\:503c\:8868\:793a, \:8003\:8651\:5173\:8054\:4e2d\:4e0d\:540c\:7684\:5c55\:5f00 order*)
numFFs["v"]=With[
(*\:8ba1\:7b97\:51fa order \:7684\:679a\:4e3e\:8303\:56f4*)
{orderLst=Query[1,1,Keys]@ffsMerged["confs"]},
(*\:904d\:5386 order \:7684\:96c6\:5408 ordLst, \:9009\:62e9\:76f8\:5e94\:7684 numFFs*)
Association@Map[#->
Query[(*cc["C"]*)All,(*fitScheme*)All,
(*order*)Key@#,(*octet*)All,
(*tree,loop,recon*)
numFFs[<|"ord"->#|>]
]@ffsMerged["confs"]&,
orderLst
]
];


(* ::Chapter:: *)
(*interpolation; order full*)


(*\:5bf9 Q2 \:7684 full \:9636\:8868\:8fbe\:5f0f\:505a\:63d2\:503c, \:65b9\:4fbf\:753b\:56fe\:548c\:7ec4\:5408*)
If[$parOrdStr===$ordFull,
Get["ff.numeric-interpo.wl"];]
(*$total:864, time: 20 min;*)


(*\:4fdd\:5b58\:5230\:78c1\:76d8\:6587\:4ef6*)
serialize["interpo",interpoGEGM["v"]]


(* ::Chapter:: *)
(*Grid display *)


(*\:80cc\:666f\:8272\:914d\:7f6e*)
dataBackground={
None,(* color horizontal: x1, x2, x3...*)
{
LightCyan,{None,LightBlue}
}(* color vertical: y1, y2, y3...*)
};
(*\:5e94\:7528\:8868\:683c\:6392\:7248*)
If[$inNBook,
gridTable["GEGM",dataBackground]@Query[
$ord0,Key@cc["C","1.50"],Key@"\[CapitalSigma]N"]@numFFs["v"]
]


(* ::Chapter:: *)
(*EOF*)


echo[DateString[]," : finished, SessionTime : ",SessionTime[]];
