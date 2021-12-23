(* ::Package:: *)

(* ::Title:: *)
(*ff.numeric-checking.wl*)


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
$par\[CapitalLambda]=1.00;
$par\[CapitalLambda]Str=enString@NumberForm[$par\[CapitalLambda],{3,2}];
(* fitScheme \:5b9a\:4e49\:89c1: $fittingScheme*)
(*$fitScheme={"\[CapitalSigma]+-","\[CapitalSigma]","\[CapitalSigma]-p","\[CapitalSigma]N","\[CapitalSigma]-\[CapitalXi]-","N","p\[CapitalXi]-","\[CapitalXi]","charged","many","most","all"};*)
(*$fitScheme={"\[CapitalSigma]N","most"};*)
$fitScheme={"\[CapitalSigma]+-","\[CapitalSigma]","\[CapitalSigma]N","N","p\[CapitalXi]-","charged","many","most","all"};
$erroBar="notbar";


(*\:5bfc\:5165\:6b64\:8ba1\:7b97\:7a0b\:5e8f\:7684\:53c2\:6570----------------------------------------------------------*)
Get["ff.numeric-setup.wl"];
(*\:5bfc\:5165\:6240\:6709\:8d39\:66fc\:56fe tag \:7684\:5217\:8868: fyAmpLoopLst,fyAmpTreeLst*)
Once@Get["gen.integral-TagList.wl"];
(*\:8981\:8ba1\:7b97\:7684\:8d39\:66fc\:56fe\:5217\:8868*)
fyAmpPart=fyAmpLoopLst;
(*\:8bfb\:5165\:5404\:79cd\:8f93\:5165\:63a5\:53e3*)
Once@Get["coes.interface.wl"];


(* ::Chapter:: *)
(*numeric Form Factors Merged; all series*)


(* ::Input:: *)
(*(* \:5bfc\:5165\:5177\:4f53\:8ba1\:7b97\:7684\:7a0b\:5e8f,\:6811\:56fe\:ff0c\:5708\:56fe\:ff0c\:91cd\:6b63\:5316\:5e38\:6570 *)*)
(*(*order full, Intel i7-6700 (8): \:4ee3\:5165\:6240\:6709\:6570\:503c, ~ 4m30s ; \:4ee3\:5165\:90e8\:5206\:6570\:503c, ~3m50s *)*)
(*Get["ff.numeric-worker.wl"];*)


(* ::Chapter:: *)
(*checking*)


chopQ2Val[x_]:=numVal[chopQ2[x]]


(*\:5c55\:793a\:7c92\:5b50\:7684\:603b\:7ed3\:679c*)
Query[$ord0,kLoopAmpSum,
KeySort/*Normal/*(TableForm[#,TableSpacing->{3.5, 1}]&),
Normal/*(TableForm[#,TableSpacing->{1.5,1}]&),
chopQ2Val/*ReplaceAll[quaCharge["uds"]]
]@loopResults["v"]


(* \:5bf9\:67d0\:4e9b\:56fe\:7684\:7ed3\:679c\:6c42\:548c\:ff0c*)
Query[$ord0,kLoopChanSum,
{Key@fd[2,1,0]},
sectOct/*Total,
({Key@ffsF1F2}),All,
chopQ2Val/*ReplaceAll[quaCharge["uds"]]
]@loopResults["v"]
(* \:5c55\:793a\:6bcf\:4e2a\:56fe\:7684\:7ed3\:679c *)
Query[$ord0,kLoopChanSum,
(*\:9009\:62e9 octet *)
{Key@fd[2,1,0]}/*Normal/*(Column[#,Spacings->2]&),
(*\:9009\:62e9 \:8d39\:66fc\:56fe*)
All/*Normal/*(Column[#,Spacings->1,Alignment->"\[Rule]"]&),
(*\:9009\:62e9 \:5f62\:72b6\:56e0\:5b50*)
{Key@ffsGEGM}/*Normal/*(TableForm[#,TableSpacing->{2, 1}]&),
(*\:5904\:7406\:6210\:5bf9\:6570\:636e,\:5982 F1,F2*)
chopQ2Val/*ReplaceAll[quaCharge["uds"]]
]@loopResults["v"]


(* \:5c55\:793a\:6bcf\:4e2a\:56fe\:6bcf\:4e2a\:53cd\:5e94\:9053\:7684\:7ed3\:679c *)
Query[$ord0,kLoopChannel,
{Key@fd[2,1,0]}/*Normal/*(Column[#,Spacings->2]&),
sectOct/*Normal/*(Column[#,Spacings->1,Alignment->"\[Rule]"]&),
Normal/*(TableForm[#,TableSpacing->{2, 1}]&),
chopQ2Val/*ReplaceAll[quaCharge["uds"]]
]@loopResults["v"]


(*\:67e5\:770b\:7279\:5b9a\:7c92\:5b50\:ff0c\:7279\:5b9a\:56fe\:7684 GEGM*)
Query[$ord0,kLoopChanSum,
{Key@fd[2,1,0]},
Key@chTag@{"RB","oct","F1"},
Key@ffsGEGM,
ReplaceAll[quaCharge["uds"]]
]@loopResults["v"]


(* ::Chapter:: *)
(*EOF*)


echo[DateString[]," : finished, SessionTime : ",SessionTime[]];
