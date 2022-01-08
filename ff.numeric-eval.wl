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
(*cmd args*)


(*\:5bfc\:5165\:6b64\:8ba1\:7b97\:7a0b\:5e8f\:7684\:53c2\:6570 --------------*)
Get["ff.numeric-setup.wl"];


(*\:5f53\:5728\:7b14\:8bb0\:672c\:4e2d\:8fd0\:884c\:65f6\:ff0c\:4f7f\:7528 \:547d\:4ee4\:884c\:8f93\:5165\:6a21\:62df*)
CmdParser["pseudo"]={
"--update","True",
"--para-coupl","True",
"--ord","$ordFull",
"--lbd-num","0.90"
};


parseCml[]


(* ::Section:: *)
(*import module*)


(*\:5bfc\:5165\:6240\:6709\:8d39\:66fc\:56fe tag \:7684\:5217\:8868: fyAmpLoopLst,fyAmpTreeLst*)
Get["gen.integral-TagList.wl"];
(*\:8981\:8ba1\:7b97\:7684\:8d39\:66fc\:56fe\:5217\:8868 --------------*)
fyAmpPart=fyAmpLoopLst;
(*\:8bfb\:5165\:5404\:79cd\:8f93\:5165\:63a5\:53e3 --------------*)
Get["coes.interface.wl"];
Get["ff.numeric-interface.wl"];


(* ::Chapter:: *)
(*import loop result*)


If[!$renew$ffsMergedQ&&
FileExistsQ@FindFile@localPathResult[resultsDir]["loop-result.wdx"]&&
FileExistsQ@FindFile@localPathResult[resultsDir]["ffsMerged.wdx"],
(*\:5982\:679c\:6709\:4e4b\:524d\:7f13\:5b58\:7684\:7ed3\:679c\:ff0c\:5c31\:76f4\:63a5\:8bfb\:5165*)
loopResults["v"]=Import@localPathResult[resultsDir]["loop-result.wdx"];
ffsMerged["WithRen",keyTreeAndLoop]=Import@localPathResult[resultsDir]["ffsMerged.wdx"];,
(*\:5982\:679c\:9700\:8981\:91cd\:65b0\:8ba1\:7b97,\:5c31\:5bfc\:5165\:5177\:4f53\:8ba1\:7b97\:7684\:7a0b\:5e8f,\:6811\:56fe\:ff0c\:5708\:56fe\:ff0c\:91cd\:6b63\:5316\:5e38\:6570 *)
(*order full, Intel i7-6700 (8): \:4ee3\:5165\:6240\:6709\:6570\:503c, ~ 4m30s ; \:4ee3\:5165\:90e8\:5206\:6570\:503c, ~3m50s *)
Get["ff.numeric-worker.wl"];//AbsoluteTiming//echo;
(*\:4fdd\:5b58\:5230\:78c1\:76d8\:6587\:4ef6\:ff1a\:5708\:56fe\:7684\:8ba1\:7b97\:7ed3\:679c*)
serializeResult[resultsDir]["loop-result.wdx",loopResults["v"]];
serializeResult[resultsDir]["ffsMerged.wdx",ffsMerged["WithRen",keyTreeAndLoop]];
]


(* ::Section:: *)
(*Form Factors Merged; all series*)


(*\:63d0\:53d6\:51fa ffsMerged \:8ba1\:7b97\:7ed3\:679c\:4e2d, \:5173\:5fc3\:7684\:90e8\:5206, TreeGEGM,LoopGEGM,\:91cd\:6b63\:5316\:5e38\:6570*)
ffsMerged["trimed",keyTreeAndLoop]=Query[(*order*)All,(*octet*)All,
(*FFactors,recon,etc.*)Key/@{ffsTreeGEGM,ffsLoopGEGM,recon}
]@ffsMerged["WithRen",keyTreeAndLoop];


(*\:4fdd\:5b58\:5230\:78c1\:76d8\:6587\:4ef6\:ff1a\:5708\:56fe\:7684\:8ba1\:7b97\:7ed3\:679c*)
(*serializeResult[resultsDir]["ffsMerged-trimed.wdx",ffsMerged["trimed",keyTreeAndLoop]];*)


(* \:62df\:5408\:51fa\:7684 c1,c2 \:503c\:88ab\:653e\:5728 ccfitted$Err \:4e2d, \:4f7f\:7528 Query \:67e5\:8be2; \:8003\:8651 \:53c2\:6570\:4e4b\:95f4\:7684\:9650\:5236\:ff0c\:7ed9\:51fa\:6240\:6709\:53c2\:6570\:7684\:6570\:503c*)
(*\:5404\:79cd C,c1,c2 \:914d\:7f6e\:4e0b\:ff0c\:8ba1\:7b97\:51fa\:7684\:6570\:503c\:7ed3\:679c\:ff0c\:5938\:514b\:7535\:8377\:548c Q2 \:672a\:4ee3\:5165\:5177\:4f53\:6570\:503c*)
Module[{ccNumStr},
ffsMerged["configs",keyTreeAndLoop]=Association@Table[
(*\:521d\:59cb\:5316 C \:5b57\:7b26\:5f62\:5f0f*)
ccNumStr=enString@NumberForm[ccNum,{3,2}];
(*\:4ee3\:5165\:62df\:5408\:786e\:5b9a\:7684\:6570\:503c*)
Query[(*cc-values*){Key@cc["C",ccNumStr]},(*fitting-scheme*)All,
(*ccfitted$Err \:6570\:636e\:5c42, ccfitted \:7684\:9996\:5143\:7d20\:662f\:8bef\:5dee\:ff0c\:672b\:5143\:7d20\:662f c \:7684\:66ff\:6362\:89c4\:5219, \:5982:
{-5.551115123125783`*^-17,{cc["c1"]\[Rule]1.9165025864434668`,cc["c2"]\[Rule]0.5439905713541093`}}
\:5c06\:751f\:6210\:7684 ccc \:66ff\:6362\:89c4\:5219\:5e94\:7528\:5230 \:5f62\:72b6\:56e0\:5b50\:7684\:8868\:8fbe\:5f0f\:4e0a*)
Query[(*order*)All,(*octet*)All,(*tree-loop-FFactors,recons*)All,
(*FFactors pair*)ReplaceAll[apply$cc$numeric[ccNum][Last@#]]
]@ffsMerged["trimed",keyTreeAndLoop]&
(*Query on \:62df\:5408\:7684\:6570\:636e\:5217\:8868*)
]@ccfitted$Err
(*C\:7684\:8fed\:4ee3\:5217\:8868*)
,{ccNum,{1.0,1.1,1.2,1.3,1.4,1.5}} 
]];//AbsoluteTiming


(* ::Section:: *)
(*renormalization constant*)


renormalConst["v"]=Query[(*cc-values*)All,(*fitting-scheme*)1,
(*order*)Key@$ord0,(*octet*)All,(*tree-loop-FFactors,recons*)Key@recon,
(*FFactors pair*)All
]@ffsMerged["configs",keyTreeAndLoop];


(*\:4fdd\:5b58\:5230\:78c1\:76d8\:6587\:4ef6\:ff1a\:5708\:56fe\:7684\:8ba1\:7b97\:7ed3\:679c*)
serializeResult[resultsDir]["recons.wdx",renormalConst["v"]];


(*\:6253\:5370\:573a\:5f3a\:91cd\:6b63\:5316\:5e38\:6570*)
If[$inNBook,
gridTable["recons",dataBackground]@
Query[(*cc-values*)All,
(*octet*)All,
(*FFactors pair*)NumberForm[#,{4,3}]&]@renormalConst["v"]
]


(* ::Chapter:: *)
(*numeric FFs; each contribution; all series*)


(*\:5904\:7406\:4f20\:5165\:7684 ffsMergedWithRen,\:4ee3\:5165\:5938\:514b\:7535\:8377\:914d\:7f6e,\:7ed9\:51fa\:5404\:79cd\:5473\:9053\:7684\:7ed3\:679c,\:8fd9\:91cc\:53ea\:63d0\:53d6\:4e86 GEGM *)
(*\:666e\:901a\:8ba1\:7b97, \:4f7f\:7528\:7684\:6570\:503c\:8868\:8fbe\:5f0f*)
numFFs["fn"][keyTreeAndLoop][chopQ2Val_]:=<|
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
(#@recon*#@ffsTreeGEGM+#@ffsLoopGEGM)/.quaCharge["uds"]]
|>&


(*\:751f\:6210\:6700\:7ec8\:7684\:6570\:503c\:8868\:793a, \:8003\:8651\:5173\:8054\:4e2d\:4e0d\:540c\:7684\:5c55\:5f00 order*)
numFFs["v",keyTreeAndLoop]=With[
(*\:8ba1\:7b97\:51fa order \:7684\:679a\:4e3e\:8303\:56f4*)
{orderLst=Query[1,1,Keys]@ffsMerged["configs",keyTreeAndLoop]},
(*\:904d\:5386 order \:7684\:96c6\:5408 ordLst, \:9009\:62e9\:76f8\:5e94\:7684 numFFs["fn"]*)
Association@Table[ord->
Query[(*cc-values*)All,(*fitting-scheme*)All,
(*order*)Key@ord,(*octet*)All,
(*tree-loop-FFactors,recons*)
numFFs["fn"][<|"ord"->ord|>]@numFFs["fn"][keyTreeAndLoop]
]@ffsMerged["configs",keyTreeAndLoop]
,{ord,orderLst}
]];//AbsoluteTiming


(* ::Section:: *)
(*Grid display *)


(*\:5e94\:7528\:8868\:683c\:6392\:7248*)
If[$inNBook,
gridTable["GEGM",dataBackground]@Query[
(*order*)$ord0,
(*cc-values*)Key@cc["C","1.50"],
(*fitting-scheme*)Key@"\[CapitalSigma]+-",
(*octet*)All,
(*tree-loop-uds-contribution*)All,
NumberForm[#,{4,3}]&
]@numFFs["v",keyTreeAndLoop]
]


(* ::Chapter:: *)
(*interpolation; order full*)


(*\:5bf9 Q2 \:7684 full \:9636\:8868\:8fbe\:5f0f\:505a\:63d2\:503c, \:65b9\:4fbf\:753b\:56fe\:548c\:7ec4\:5408*)
If[$parOrdStr===$ordFull&&$interpolateQ,
Get["ff.numeric-interpo.wl"];]
(*$total:864, time: 20 min;*)


(*\:4fdd\:5b58\:5230\:78c1\:76d8\:6587\:4ef6*)
serializeResult[resultsDir]["interpo.wdx",interpoGEGM["v",keyTreeAndLoop]]


(* ::Chapter:: *)
(*EOF*)


echo[DateString[]," : finished, SessionTime : ",SessionTime[]];
