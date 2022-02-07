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
(*cmd args*)


(*\:5bfc\:5165\:6b64\:8ba1\:7b97\:7a0b\:5e8f\:7684\:53c2\:6570 --------------*)
Get["ff.numeric-setup.wl"];


(*\:5f53\:5728\:7b14\:8bb0\:672c\:4e2d\:8fd0\:884c\:65f6\:ff0c\:4f7f\:7528 \:547d\:4ee4\:884c\:8f93\:5165\:6a21\:62df*)
CmdParser["pseudo"]={$fileName
,"--fit","False"(*\:662f\:5426\:5904\:4e8e fitting \:6a21\:5f0f*)
,"--update","False"(*\:662f\:5426\:91cd\:65b0\:8ba1\:7b97 ffsMerged,\:8d39\:66fc\:56fe\:90e8\:5206\:6570\:503c\:7684\:7ed3\:679c*)
,"--parallel-Lbd","False"(*\:4ee3\:5165\:8026\:5408\:5e38\:6570\:6570\:503c\:65f6,\:662f\:5426\:8fd0\:884c\:5e76\:884c\:5185\:6838*)
,"--interp","False"(*\:662f\:5426\:8fd0\:884c\:5bf9 full order \:7684\:63d2\:503c\:7a0b\:5e8f*)
,"--ord","$ordFull"(*$ordFull","\:5708\:79ef\:5206\:7684\:7ea7\:6570 order: \:6709 ord0, ord1, ordFull*)
,"--Lbd-num","0.90"(*\:6570\:503c\:8ba1\:7b97\:4e2d Lambda \:7684\:53d6\:503c: 0.80,0.90,1.00*)
,"--Lbd-fit","Undefined"(*\:5f15\:7528\:7684 fitting \:57fa\:4e8e\:7684 Lambda, \:800c\:4e0d\:662f\:6570\:503c\:8ba1\:7b97\:4e2d\:4f7f\:7528\:7684 Lambda: 0.80,0.90,1.00*)
};


(*\:89e3\:6790\:547d\:4ee4\:884c\:53c2\:6570\:ff0c\:6216\:8005\:7b14\:8bb0\:672c\:4f2a\:53c2\:6570*)
parseCml[]


(* ::Section:: *)
(*<< module*)


(*\:5bfc\:5165\:6240\:6709\:8d39\:66fc\:56fe tag \:7684\:5217\:8868: fyAmpLoopLst,fyAmpTreeLst*)
Once@Get["gen.integral-TagList.wl"];
(*\:8981\:8ba1\:7b97\:7684\:8d39\:66fc\:56fe\:5217\:8868*)
fyAmpPart=fyAmpLoopLst;
(*\:8bfb\:5165\:5404\:79cd\:8f93\:5165\:63a5\:53e3*)
Get["coes.interface.wl"];
Get["ff.numeric-interface.wl"];


(*\:5bfc\:5165\:4e4b\:524d\:603b\:8d21\:732e\:7684\:63d2\:503c\:51fd\:6570*)
interpoGEGM["v"]=Import@localPathResult[resultsDir]["interpo.wdx"];


(*diagram tag \:683c\:5f0f\:5316\:ff0c\:573a\:5934\:90e8\:683c\:5f0f\:5316*)
legendDisp[x_]:=x/.{chTag->StringRiffle,fd->fdDisp}


(* ::Chapter:: *)
(*numeric Form Factors Merged; all series*)


(* ::Input:: *)
(*(* \:5bfc\:5165\:5177\:4f53\:8ba1\:7b97\:7684\:7a0b\:5e8f,\:6811\:56fe\:ff0c\:5708\:56fe\:ff0c\:91cd\:6b63\:5316\:5e38\:6570 *)*)
(*(*order full, Intel i7-6700 (8): \:4ee3\:5165\:6240\:6709\:6570\:503c, ~ 4m30s ; \:4ee3\:5165\:90e8\:5206\:6570\:503c, ~3m50s *)*)


If[!$updateLambdaQ&&
FileExistsQ@FindFile@localPathResult[resultsDir]["loop-result.wdx"],
(*\:5982\:679c\:6709\:4e4b\:524d\:7f13\:5b58\:7684\:7ed3\:679c\:ff0c\:5c31\:76f4\:63a5\:8bfb\:5165*)
loopResults["v"]=Import@localPathResult[resultsDir]["loop-result.wdx"];,
(*\:5982\:679c\:9700\:8981\:91cd\:65b0\:8ba1\:7b97,\:5c31\:5bfc\:5165\:5177\:4f53\:8ba1\:7b97\:7684\:7a0b\:5e8f,\:6811\:56fe\:ff0c\:5708\:56fe\:ff0c\:91cd\:6b63\:5316\:5e38\:6570 *)
(*order full, Intel i7-6700 (8): \:4ee3\:5165\:6240\:6709\:6570\:503c, ~ 4m30s ; \:4ee3\:5165\:90e8\:5206\:6570\:503c, ~3m50s *)
Get["ff.numeric-worker.wl"];//AbsoluteTiming//echo;
(*\:4fdd\:5b58\:5230\:78c1\:76d8\:6587\:4ef6\:ff1a\:5708\:56fe\:7684\:8ba1\:7b97\:7ed3\:679c*)
serializeResult[resultsDir]["loop-result.wdx",loopResults["v"]];
]


(* ::Section:: *)
(*Form Factors Merged; all series*)


(* ::Input:: *)
(*ffsLoopChanSum=Query[(*order*)All,{Key@kLoopChanSum}*)
(*]@loopResults["v"];*)
(*ffsLoopChannel=Query[(*order*)All,{Key@kLoopChannel}*)
(*]@loopResults["v"];*)


(* \:62df\:5408\:51fa\:7684 c1,c2 \:503c\:88ab\:653e\:5728 ccfitted$Err \:4e2d, \:4f7f\:7528 Query \:67e5\:8be2; \:8003\:8651 \:53c2\:6570\:4e4b\:95f4\:7684\:9650\:5236\:ff0c\:7ed9\:51fa\:6240\:6709\:53c2\:6570\:7684\:6570\:503c*)
(*\:5404\:79cd C,c1,c2 \:914d\:7f6e\:4e0b\:ff0c\:8ba1\:7b97\:51fa\:7684\:6570\:503c\:7ed3\:679c\:ff0c\:5938\:514b\:7535\:8377\:548c Q2 \:672a\:4ee3\:5165\:5177\:4f53\:6570\:503c*)
Module[{ccNumStr},
ffsMerged[kLoopChanSum,"configs"]=Association@Table[
adjust->Association@Table[
(*\:521d\:59cb\:5316 C \:5b57\:7b26\:5f62\:5f0f*)
ccNumStr=enString@NumberForm[ccNum,{3,2}];
(*\:4ee3\:5165\:62df\:5408\:786e\:5b9a\:7684\:6570\:503c------------*)
cc["C",ccNumStr]->Query[
(*<bub,nobub>*)Key@adjust
,(*<cc-values>*)Key@cc["C",ccNumStr]
,(*<fitting-scheme>*)All
(*ccfitted$Err \:6570\:636e\:5c42, ccfitted \:7684\:9996\:5143\:7d20\:662f\:8bef\:5dee\:ff0c\:672b\:5143\:7d20\:662f c \:7684\:66ff\:6362\:89c4\:5219, \:5982:
{-5.551115123125783`*^-17,{cc["c1"]\[Rule]1.9165025864434668`,cc["c2"]\[Rule]0.5439905713541093`}}
\:5c06\:751f\:6210\:7684 ccc \:66ff\:6362\:89c4\:5219\:5e94\:7528\:5230 \:5f62\:72b6\:56e0\:5b50\:7684\:8868\:8fbe\:5f0f\:4e0a*)
,Query[
(*<bub,nobub>*)Key@adjust
,(*<order>*)All
,(*<sum-level>*)Key@kLoopChanSum
,(*<octet>*)All
,(*<diagram>*)All
,(*<F1F2-GEGM-FFactors>*){Key@ffsGEGM}
,(*<FFactors pair>*)ReplaceAll[apply$cc$numeric[ccNum][Last@#]]
]@loopResults["v"]&
(*Query on \:62df\:5408\:7684\:6570\:636e\:5217\:8868*)
]@ccfitted$Err

(*C\:7684\:8fed\:4ee3\:5217\:8868*)
,{ccNum,{1.0,1.1,1.2,1.3,1.4,1.5}}]
,{adjust,Keys@fyCoesAdjust}]
];//AbsoluteTiming


(* ::Section:: *)
(*numeric FFs; each contribution; all series*)


(*\:5904\:7406\:4f20\:5165\:7684 loopResults["v"],\:4ee3\:5165\:5938\:514b\:7535\:8377\:914d\:7f6e,\:7ed9\:51fa\:5404\:79cd\:5473\:9053\:7684\:7ed3\:679c,\:8fd9\:91cc\:53ea\:63d0\:53d6\:4e86 GEGM *)
(*\:666e\:901a\:8ba1\:7b97, \:4f7f\:7528\:7684\:6570\:503c\:8868\:8fbe\:5f0f*)
numFFs["fn"][keyLoop][chopQ2Val_]:=<|
(*loop*)
tagNum["lo","uds"]->chopQ2Val[#@ffsGEGM/.quaCharge["uds"]],
tagNum["lo","u"]->chopQ2Val[#@ffsGEGM/.quaCharge["u"]],
tagNum["lo","d"]->chopQ2Val[#@ffsGEGM/.quaCharge["d"]],
tagNum["lo","s"]->chopQ2Val[#@ffsGEGM/.quaCharge["s"]]
|>&


(*\:751f\:6210\:6700\:7ec8\:7684\:6570\:503c\:8868\:793a, \:8003\:8651\:5173\:8054\:4e2d\:4e0d\:540c\:7684\:5c55\:5f00 order, ~30s*)
numFFs["v",kLoopChanSum,"tmp"]=With[
(*\:8ba1\:7b97\:51fa order \:7684\:679a\:4e3e\:8303\:56f4------------------------------*)
{orderLst=Query[1,1,1,Keys]@ffsMerged[kLoopChanSum,"configs"]},
(*\:904d\:5386 order \:7684\:96c6\:5408 ordLst, \:9009\:62e9\:76f8\:5e94\:7684 numFFs["fn"]*)
Association@Table[ord->
Query[
(*<bub,nobub>*)All
,(*cc-values*)All
,(*fitting-scheme*)All
,(*order*)Key@ord
,(*octet*)All
,(*diagram*)All
,(*loop-FFactors*)
numFFs["fn"][<|"ord"->ord|>]@numFFs["fn"][keyLoop]
]@ffsMerged[kLoopChanSum,"configs"]
,{ord,orderLst}
]];//AbsoluteTiming


numFFs["v",kLoopChanSum]=Association@Table[
(*\:6574\:7406 ffsMerged \:7684\:5d4c\:5957\:6b21\:5e8f, \:628a\:4eba\:4e3a\:6c42\:548c\:9009\:62e9\:653e\:5728\:6700\:5916\:5c42 -------------*)
adjust->Query[
(*<order>*)All
,(*<bub,nobub>*)Key@adjust
]@numFFs["v",kLoopChanSum,"tmp"]
,{adjust,Keys@fyCoesAdjust}];


(* ::Chapter:: *)
(*checking*)


chopQ2Val[x_]:=numVal[chopQ2[x]]


(* ::Input:: *)
(*(*\:5c55\:793a\:7c92\:5b50\:7684\:603b\:7ed3\:679c*)*)
(*Query[(*<bub>*)coesAdjNoBub,(*<order>*)$ord0,(*<sum>*)kLoopAmpSum*)
(*,(*<octet>*)KeySort/*Normal/*(TableForm[#,TableSpacing->{3.5,1}]&)*)
(*,(*<FFactors>*)Normal/*(TableForm[#,TableSpacing->{1.5,1}]&)*)
(*,(*{F1F2 pairs}*)ReplaceAll[quaCharge["s"]]/*chopQ2Val*)
(*]@loopResults["v"]*)


(* ::Input:: *)
(*(* \:5bf9\:67d0\:4e9b\:56fe\:7684\:7ed3\:679c\:6c42\:548c\:ff0c*)*)
(*Query[coesAdjBub,$ord0,kLoopChanSum,*)
(*{Key@fd[2,1,0]},*)
(*sectOct/*Total,*)
(*({Key@ffsF1F2}),All,*)
(*chopQ2Val/*ReplaceAll[quaCharge["uds"]]*)
(*]@loopResults["v"]*)


(* ::Input:: *)
(*(* \:5c55\:793a\:6bcf\:4e2a\:56fe\:7684\:7ed3\:679c *)*)
(*Query[(*<bub>*)coesAdjBub*)
(*,(*<order>*)$ord0*)
(*,(*<loopChannels>*)kLoopChanSum*)
(*,(*<octet>*)*)
(*{Key@ff["n"]}/*Normal/*(Column[#,Spacings->2]&)*)
(*,(*<diagram>*)*)
(*All/*Normal/*(Column[#,Spacings->1,Alignment->"\[Rule]"]&)*)
(*,(*<FormFactors>*)*)
(*{Key@ffsGEGM}/*Normal/*(TableForm[#,TableSpacing->{2,1}]&)*)
(*,(*<F1F2 pairs>*)*)
(*chopQ2Val/*ReplaceAll[quaCharge["s"]]*)
(*]@loopResults["v"]*)


(* ::Input:: *)
(*(* \:5c55\:793a\:6bcf\:4e2a\:56fe\:6bcf\:4e2a\:53cd\:5e94\:9053\:7684\:7ed3\:679c *)*)
(*Query[(*<bub>*)coesAdjBub*)
(*,(*<order>*)$ord0*)
(*,(*<loopChannels>*)kLoopChannel*)
(*,(*<octet>*)(Key/@{ff["p"],ff["n"]})/*Normal/*(Column[#,Spacings->2]&)*)
(*,(*<diagram>*)({4})/*Normal/*(Column[#,Spacings->1,Alignment->"\[Rule]",Frame->All]&)*)
(*,(*<channels>*)Normal/*(TableForm[legendDisp@#,TableSpacing->{1,1}]&)*)
(*,(*key\[Rule]<channel>*)All,(*<FormFactors>*)All*)
(*,(*<F1F2 pairs>*)chop/*ReplaceAll[quaCharge["s"]]*)
(*]@loopResults["v"]*)


(* ::Input:: *)
(*(*\:67e5\:770b\:7279\:5b9a\:7c92\:5b50\:ff0c\:7279\:5b9a\:56fe\:7684 GEGM*)*)
(*Query[coesAdjNoBub,$ordFull,kLoopChanSum,*)
(*{Key@ff["p"]},*)
(*Key@chTag@{"RB","oct","F1"},*)
(*Key@ffsGEGM,*)
(*ReplaceAll[quaCharge["uds"]]*)
(*]@loopResults["v"]*)


(* ::Chapter:: *)
(*diagram curves*)


(* ::Section:: *)
(*plot specification*)


(*\:5173\:95ed\:6570\:503c\:51fd\:6570\:8b66\:544a*)
Off[CompiledFunction::cfn]


(*\:5c06\:5173\:8054\:5217\:8868\:4e2d\:7684\:5143\:7d20\:ff0c\:8f6c\:6362\:6210\:5e26\:6ce8\:91ca\:7684 wrapper \:8868\:8fbe\:5f0f, \:4f20\:5165 plot \:4f5c\:56fe*)
annotated[Legended_][assoc_Association]:=KeyValueMap[Legended,assoc]


(*\:5bf9 \:5355\:72ec\:8d39\:66fc\:56fe\:8d21\:732e\:4f5c\:56fe*)
plotData["sub"][Q2_][fnList_]:=Plot[fnList,{Q2,$Q2Cut,0.9}
,ImageSize->800
,PlotTheme->{"Scientific","FrameGrid","MediumLines"}
,PlotRange->Full
,Axes->True
,PlotLegends->None
];
(* \:5bf9 \:5708\:56fe\:603b\:8d21\:732e\:4f5c\:56fe*)
plotData["tot"][Q2_][lst_]:=Plot[Evaluate@lst,{Q2,$Q2Cut,0.9}
,ImageSize->Large
,PlotTheme->{"Scientific","FrameGrid","MediumLines"}
,PlotRange->Full
,PlotStyle->{Red,Dashed}
,PlotLegends->None
];


(* ::Section:: *)
(*examples*)


(*\:516c\:5171\:8bbe\:7f6e,\:5168\:5c40\:53d8\:91cf*)
(*coesAdjBub, coesAdjNoBub*)
tmp`bub=Key@coesAdjBub;
(* $ord0,$ordFull*)
tmp`ord=Key@$ordFull;
(*cc["C","1.00"],cc["C","1.10"],cc["C","1.20"],cc["C","1.30"],cc["C","1.40"],cc["C","1.50"]*)
tmp`cc=Key@cc["C","1.50"];
(*"all","charged","many","most","N","p\[CapitalXi]-","\[CapitalSigma]","\[CapitalSigma]+-","\[CapitalSigma]N"*)
tmp`scheme=Key@"many";
(*"p","n","\[CapitalSigma]+","\[CapitalSigma]0","\[CapitalSigma]-","\[CapitalXi]0","\[CapitalXi]-","\[CapitalLambda]"*)
tmp`oct=Key@ff["n"];
(*\:516b\:91cd\:6001, bub+tadu, \:5341\:91cd\:6001\:ff0c\:78c1\:77e9\:76f8\:5173\:56fe\:5206\:522b\:662f\:ff1asectOct,sectBub,sectDec,sectMag*)
tmp`diag=sectOct~Join~sectBub~Join~sectDec;
(*tagNum["lo","uds"],tagNum["lo","u"],tagNum["lo","d"],tagNum["lo","s"]*)
tmp`conf=Key@tagNum["lo","s"];
(*tagNum["lo","uds"],tagNum["lo","u"]..d,s,
tagNum["tr","uds"],tagNum["tr","u"]..d,s,
tagNum["tr+lo","uds"]*)
tmp`confTot=Key/@{tagNum["lo","s"]};
(*1:GE,2:GM*)
tmp`gegm=1;


(*\:5c06 key\[Rule]val \:8f6c\:6362\:6210 Callout[f,label] \:7684\:683c\:5f0f*)
legendFn[key_,val_]:=Callout[val,key//legendDisp,Before]
(*\:4f5c\:56fe*)
{
(*\:5355\:72ec\:8d39\:66fc\:56fe\:7684\:8d21\:732e--------------------------------*)
figGroup["diag"]=plotData["sub"][Q2]@
(*\:7ed9\:6570\:636e\:6dfb\:52a0\:6ce8\:91ca*)
annotated[legendFn]@
(*\:5c55\:5e73\:5d4c\:5957\:5173\:8054*)
flatAssoc@
(*\:67e5\:8be2\:6240\:9700\:6570\:636e*)
Query[
(*<bub>*)tmp`bub
,(*order*)tmp`ord
,(*cc-values*)tmp`cc
,(*fitting-scheme*)tmp`scheme
,(*octet*)tmp`oct
,(*diagram*)({4})/*SortBy[Abs@ReplaceAll[Q2->0.1]]
,(*loop-FFactors*)tmp`conf
,(*numVal*)ReplaceAll[{numVal->Identity}]
,(*GEGM*)tmp`gegm
]@numFFs["v",kLoopChanSum]
(*\:6240\:6709\:8d39\:66fc\:5355\:5708\:56fe\:76f8\:52a0\:7684\:8d21\:732e,\:9700\:8981\:66f4\:65b0\:63d2\:503c\:6570\:636e ---------------------------*)
(*\:6807\:6ce8\:6570\:636e*)
,legendFn[key_,val_]:=Legended[val@Q2(*/#2[0]*),
(* Placed[expr,{pos,epos}] epos in expr, in \:76f8\:5bf9\:4f4d\:7f6e pos*)
Placed[key/.{numKey->StringRiffle},{{1,0.58},{0.,0.}}]];
(*\:4f5c\:56fe*)
figGroup["total"]=Query[
(*<bub>*)tmp`bub
,(*cc-value*)tmp`cc
,(*fit-scheme*)Key@"N"
,(*octet*)tmp`oct
,(*loop-tree-uds*)tmp`confTot/*annotated[legendFn]/*plotData["tot"][Q2]
(*GEGM pair*),tmp`gegm
]@interpoGEGM["v"]
}//Row


(*\:5408\:5e76 \:5708\:56fe\:603b\:8d21\:732e\:66f2\:7ebf, \:5355\:72ec\:8d39\:66fc\:5708\:56fe\:8d21\:732e*)
Show[
figGroup["total"],figGroup["diag"]
,PlotRange->Full
,PlotRangePadding->{{Scaled[.16],0},Scaled[.05]}
,ImageSize->800
]


(* ::Section:: *)
(*coes tune*)


(*\:6765\:81ea\:9876\:70b9\:4e4b\:5916\:7684\:7cfb\:6570\:ff0c\:5982\:4f20\:64ad\:5b50\:5e26\:6765\:7684 I\:ff0c\:5fae\:6270\:5c55\:5f00\:4e2d\:7684 I,
\:5e76\:8003\:8651\:65f6\:95f4\:53cd\:6f14\:5bf9\:79f0\:6027\:ff0c\:5c06 left \:56fe\:7684\:7ed3\:679c*2 *)
fyCoesTune=<|
(* \:516b\:91cd\:6001\:4e2d\:95f4\:6001 *)
chTag@{"RB","mes","oct"}->1
,chTag@{"RB","oct","F1"}->1
,chTag@{"KR","mes","oct","left"}->1
,chTag@{"RB","oct","F2"}->1

,chTag@{"KR","mes","oct","add","left"}->1
,chTag@{"tree","oct","F1F2"}->1

,chTag@{"tad","oct","F1"}->1
,chTag@{"tad","oct","F1","add"}->1
,chTag@{"tad","oct","F2"}->1
,chTag@{"bub","mes","o2"}->1
,chTag@{"bub","mes","ten","o2"}->1

(* \:5341\:91cd\:6001\:4e2d\:95f4\:6001 *)
,chTag@{"RB","mes","dec"}->1
,chTag@{"RB","dec","F1"}->1
,chTag@{"RB","dec","F2"}->1
,chTag@{"RB","trans","left"}->1
,chTag@{"KR","mes","dec","left"}->1
,chTag@{"KR","mes","dec","add","left"}->1
|>;


plotData["tot"][Q2]@Total@Values@Query[
(*<bub>*)Key@coesAdjBub
,(*<order>*)tmp`ord
,(*<cc-values>*)tmp`cc
,(*<fitting-scheme>*)Key@"many"
,(*<octet>*)Key@ff["n"]
,(*<diagram>*)All/*(Association@KeyValueMap[#1->(Identity@@fyCoesTune[#1]*#2)&,#]&)
,(*<loop-FFactors>*)Key@tagNum["lo","s"]
,(*<numVal>*)ReplaceAll[{numVal->Identity}]
,(*{GeGm pair}*)1
]@numFFs["v",kLoopChanSum]


(* ::Section:: *)
(*Grid display*)


(*\:80cc\:666f\:8272\:914d\:7f6e*)
dataBackground={
None,(* color horizontal: x1, x2, x3...*)
{
LightCyan,{None,LightBlue}
}(* color vertical: y1, y2, y3...*)
};


(*coesAdjBub, coesAdjNoBub*)
tmp`bub=Key@coesAdjBub;
(*cc["C","1.00"],cc["C","1.10"],cc["C","1.20"],cc["C","1.30"],cc["C","1.40"],cc["C","1.50"]*)
tmp`cc=Key@cc["C","1.50"];
(*"all","charged","many","most","N","p\[CapitalXi]-","\[CapitalSigma]","\[CapitalSigma]+-","\[CapitalSigma]N"*)
tmp`scheme=Key@"many";
(*\:663e\:793a\:8868\:683c---------------------------------------------*)
With[{
(*\:5408\:5e76\:6570\:636e, \:5d4c\:5957\:5173\:8054\:ff0c\:9012\:5f52 Merge*)
data=Nest[Merge,Identity,2]@{
(*\:8ba1\:7b97\:503c*)
Query[
(*<bub>*)tmp`bub
,(*cc-values*)tmp`cc
,(*fitting-scheme*)tmp`scheme
,(*octet*)All
,(*{contrib}*)All
,(*{GEGM}*)All
,(*InterpolatingFunction*)NumberForm[Chop[#@0],4]&
]@interpoGEGM["v"],
(*\:5b9e\:9a8c\:503c*)
Query["exp."]@numExper/.{numAround->Around,$tempNone->0}
}},
(*\:5e94\:7528\:8868\:683c\:6392\:7248++++++++++++++++++++++++++++++++++++++++*)
If[$inNBook,
gridTable["GEGM",dataBackground]@data]
]


(* ::Section:: *)
(*tags illustration*)


(* ::Input:: *)
(*(*\:66f2\:7ebf\:56fe\:4e2d Callout/Lable/Feynman diagram Key \:7684\:542b\:4e49*)*)
(*Multicolumn[*)
(*annotated[Legended[#2,Placed[#1/.chTag->StringRiffle,Below]]&]@diagIllus*)
(*,4,Appearance->"Horizontal"*)
(*]*)


(* ::Section:: *)
(*scan fittings*)


(* ::Input:: *)
(*Query[Normal/*(TableForm[#,TableSpacing->{2,1}]&)*)
(*]@flatAssoc@Query[*)
(*(*<\[CapitalLambda] value>*)All*)
(*,(*<bub>*)All*)
(*,(*<C value>*)All*)
(*,(*<scheme>*){"many"}*)
(*]@Import@localPath[fittingsDir]["nums.ccFittings.wdx"]*)


(* ::Chapter:: *)
(*EOF*)


echo[DateString[]," : finished, SessionTime : ",SessionTime[]];
