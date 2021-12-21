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
(*import module*)


(*\:5bfc\:5165\:6b64\:8ba1\:7b97\:7a0b\:5e8f\:7684\:53c2\:6570----------------------------------------------------------*)
Get["ff.numeric-setup.wl"];
(*\:5bfc\:5165\:6240\:6709\:8d39\:66fc\:56fe tag \:7684\:5217\:8868: fyAmpLoopLst,fyAmpTreeLst*)
Once@Get["gen.integral-TagList.wl"];
(*\:8981\:8ba1\:7b97\:7684\:8d39\:66fc\:56fe\:5217\:8868*)
fyAmpPart=fyAmpLoopLst;
(*\:8bfb\:5165\:5404\:79cd\:8f93\:5165\:63a5\:53e3*)
Once@Get["coes.interface.wl"];


(* ::Chapter::Closed:: *)
(*checking*)


(* ::Input:: *)
(*(*chop \:96f6\:70b9*)*)
(*chopQ2Val[x_]:=numVal@chopQ2[x]*)


(* ::Input:: *)
(*(*\:5c55\:793a\:7c92\:5b50\:7684\:603b\:7ed3\:679c*)*)
(*Query[KeySort/*Normal/*(TableForm[#,TableSpacing->{3.5, 1}]&),*)
(*Normal/*(TableForm[#,TableSpacing->{1.5,1}]&),*)
(*chopQ2Val/*ReplaceAll[quaCharge["uds"]]*)
(*]@loopResults["v"][[$parOrdStr,kLoopAmpSum]]*)


(* ::Input:: *)
(*(* \:5bf9\:67d0\:4e9b\:56fe\:7684\:7ed3\:679c\:6c42\:548c\:ff0c*)*)
(*Query[{Key@fd[2,1,0]},sectOct/*Total,({Key@ffsF1F2}),All,*)
(*chopQ2Val/*ReplaceAll[quaCharge["uds"]]*)
(*]@loopResults["v"][[$parOrdStr,kLoopChanSum]]*)


(* ::Input:: *)
(*(* \:5c55\:793a\:6bcf\:4e2a\:56fe\:7684\:7ed3\:679c *)*)
(*Query[{Key@fd[2,1,0]}/*Normal/*(Column[#,Spacings->2]&),*)
(*sectOct/*Normal/*(Column[#,Spacings->1,Alignment->"\[Rule]"]&),*)
(*Normal/*(TableForm[#,TableSpacing->{2, 1}]&),*)
(*chopQ2Val/*ReplaceAll[quaCharge["uds"]]*)
(*]@loopResults["v"][[$parOrdStr,kLoopChanSum]]*)


(* ::Input:: *)
(*(* \:5c55\:793a\:6bcf\:4e2a\:56fe\:6bcf\:4e2a\:53cd\:5e94\:9053\:7684\:7ed3\:679c *)*)
(*Query[{Key@fd[2,1,0]}/*Normal/*(Column[#,Spacings->2]&),*)
(*sectOct/*Normal/*(Column[#,Spacings->1,Alignment->"\[Rule]"]&),*)
(*Normal/*(TableForm[#,TableSpacing->{2, 1}]&),*)
(*chopQ2Val/*ReplaceAll[quaCharge["uds"]]*)
(*]@loopResults["v"][[$parOrdStr,kLoopChannel]]*)


(* ::Input:: *)
(*(*\:67e5\:770b\:7279\:5b9a\:7c92\:5b50\:ff0c\:7279\:5b9a\:56fe\:7684 GEGM*)*)
(*Query[{Key@fd[2,1,0]},*)
(*Key@chTag@{"RB","oct","F1"},*)
(*Key@ffsGEGM,*)
(*ReplaceAll[quaCharge["uds"]]*)
(*]@loopResults["v"][[$parOrdStr,kLoopChanSum]]*)


(* ::Chapter:: *)
(*numeric Form Factors Merged; all series*)


(* \:5bfc\:5165\:5177\:4f53\:8ba1\:7b97\:7684\:7a0b\:5e8f,\:6811\:56fe\:ff0c\:5708\:56fe\:ff0c\:91cd\:6b63\:5316\:5e38\:6570 *)
(*order full, Intel i7-6700 (8): \:4ee3\:5165\:6240\:6709\:6570\:503c, ~ 4m30s ; \:4ee3\:5165\:90e8\:5206\:6570\:503c, ~3m50s *)
If[$inNBook,Get["ff.numeric-worker.wl"];]


(*\:63d0\:53d6\:51fa ffsMerged \:8ba1\:7b97\:7ed3\:679c\:4e2d, \:5173\:5fc3\:7684\:90e8\:5206, TreeGEGM,LoopGEGM,\:91cd\:6b63\:5316\:5e38\:6570*)
ffsMerged["trimed"]=Query[All,All,Key/@{ffsTreeGEGM,ffsLoopGEGM,recon}]@ffsMerged["WithRen"];


(* \:62df\:5408\:51fa\:7684 c1,c2 \:503c\:88ab\:653e\:5728 DataSet \:4e2d, \:4f7f\:7528 Query \:67e5\:8be2; \:8003\:8651 \:53c2\:6570\:4e4b\:95f4\:7684\:9650\:5236\:ff0c\:7ed9\:51fa\:6240\:6709\:53c2\:6570\:7684\:6570\:503c*)
toNumFFs[ffsMerged_][$parC_][ccFitWithError_]:=Module[{ccfitted,fittedParas},
ccfitted=Last@ccFitWithError;
fittedParas=Join[ccfitted,numCCRelation/.ccfitted,{cc["C"]->$parC}];
(*\:5c06\:53c2\:6570 c1,c2,C \:7684\:5177\:4f53\:503c*)
Query[All,All,All,ReplaceAll[fittedParas]]@ffsMerged
]


(*\:5404\:79cd C,c1,c2 \:914d\:7f6e\:4e0b\:ff0c\:8ba1\:7b97\:51fa\:7684\:6570\:503c\:7ed3\:679c\:ff0c\:5938\:514b\:7535\:8377\:548c Q2 \:672a\:4ee3\:5165\:5177\:4f53\:6570\:503c*)
Module[{ccNumStr},
ffsMerged["confs"]=Association@Table[
(*\:521d\:59cb\:5316 C \:5b57\:7b26\:5f62\:5f0f*)
ccNumStr=enString@NumberForm[ccNum,{3,2}];
(*\:4ee3\:5165\:62df\:5408\:786e\:5b9a\:7684\:6570\:503c*)
Query[{Key@cc["C",ccNumStr]},{"\[CapitalSigma]N"},
toNumFFs[ffsMerged["trimed"]][ccNum]
]@ccfitted$Err
(*{ccNum,{1.0,1.1,1.2,1.3,1.4,1.5}}*)
,{ccNum,{1.0,1.1,1.2,1.3,1.4,1.5}} 
]];


(*\:6253\:5370\:573a\:5f3a\:91cd\:6b63\:5316\:5e38\:6570*)
Query[Normal/*TableForm,All,Key@$ord0,Values/*StringRiffle,
Key@recon,NumberForm[#,{4,3}]&
]@ffsMerged["confs"]


renormalConst["v"]=Query[All,All,Key@$ord0,All,Key@recon,All
]@ffsMerged["confs"];


(* ::Chapter:: *)
(*numeric FFs; each contribution; all series*)


(*\:5904\:7406\:4f20\:5165\:7684 ffsMergedWithRen,\:4ee3\:5165\:5938\:514b\:7535\:8377\:914d\:7f6e,\:7ed9\:51fa\:5404\:79cd\:5473\:9053\:7684\:7ed3\:679c,\:8fd9\:91cc\:53ea\:63d0\:53d6\:4e86 GEGM *)
(*\:666e\:901a\:8ba1\:7b97, \:4f7f\:7528\:7684\:6570\:503c\:8868\:8fbe\:5f0f*)
numFFs[ffsMergedWithRen_,chopQ2Val_]:=Query[
(*fd[2,1,0]: oct \:6807\:7b7e\:5c42*)All,
<|
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
]@ffsMergedWithRen;


(*\:6839\:636e\:8ba1\:7b97\:7684\:5c55\:5f00\:9636\:6570, $parOrdStr, \:9009\:62e9\:4e0d\:540c\:7684\:6570\:503c\:5904\:7406\:65b9\:5f0f; numVal \:662f\:9884\:7559\:63a5\:53e3, \:7528\:6765\:63a7\:5236\:6570\:5b57\:683c\:5f0f\:5316*)
numFFs[<|"ord"->$parOrdStr_|>][ffsMergedWithRen_]:=Module[{chopQ2Val},
Switch[$parOrdStr,
(*\:5982\:679c\:8ba1\:7b97 order 0 \:7684\:6570\:636e,\:9009\:62e9 chopQ2, \:5373\:4ee4 Q2\[Rule]0*)
$ord0,
chopQ2Val[x_]:=numVal@chopQ2[x];
numFFs[ffsMergedWithRen,chopQ2Val],
(*\:5982\:679c\:8ba1\:7b97 order 1 order full \:7684\:6570\:636e, \:9009\:62e9 chop, \:4fdd\:7559 Q2 \:4f9d\:8d56*)
_,
chopQ2Val[x_]:=numVal@chop[x];
numFFs[ffsMergedWithRen,chopQ2Val]
]];


(*\:751f\:6210\:6700\:7ec8\:7684\:6570\:503c\:8868\:793a, \:8003\:8651\:5173\:8054\:4e2d\:4e0d\:540c\:7684 series*)
numFFs["v"]=Module[
{serLst=Query[1,1,Keys]@ffsMerged["confs"]},(*\:8ba1\:7b97\:51fa series \:7684\:679a\:4e3e\:8303\:56f4*)
Association@Map[
(* \:904d\:5386 series \:7684\:96c6\:5408 serLst, \:9009\:62e9\:76f8\:5e94\:7684 numFFs *)
#->Query[All,All,Key@#,numFFs[<|"ord"->#|>]
]@ffsMerged["confs"]&,
serLst]
];


(* ::Chapter:: *)
(*interpolation; order full*)


(*\:5bf9 Q2 \:7684 full \:9636\:8868\:8fbe\:5f0f\:505a\:63d2\:503c, \:65b9\:4fbf\:753b\:56fe\:548c\:7ec4\:5408*)
If[$parOrdStr===$ordFull,
Get["ff.numeric-interpo.wl"];]
(*$total:864, time: 20 min;*)


If[$parOrdStr===$ordFull,
Module[{plotLst,annotated,contribTag},
(* \:5bf9 \:51fd\:6570\:7684\:5217\:8868 \:753b\:56fe *)
plotLst[lst_]:=Plot[Evaluate@lst,{Q2,0,1},PlotTheme->{"Scientific"},
PlotRange->{{0,1},All},Ticks->{Automatic,None},
ImageSize->Large,
PlotLegends->None];
(*\:5c06\:5173\:8054\:5217\:8868\:4e2d\:7684\:5143\:7d20\:ff0c\:8f6c\:6362\:6210\:5e26\:6ce8\:91ca\:7684 wrapper \:8868\:8fbe\:5f0f, \:4f20\:5165 plot \:4f5c\:56fe*)
annotated[assoc_Association]:=KeyValueMap[Legended[#2@Q2,#1/.{numKey->StringRiffle}]&,assoc];
(*\:6311\:51fa\:8981\:5c55\:793a\:7684\:8d21\:732e, tree,loop,uds, sea,valence*)
contribTag=Key/@{tagNum["lo","uds"],
tagNum["lo","u"],tagNum["lo","d"],tagNum["lo","s"],
tagNum["tr+lo","uds"]};
(*\:901a\:8fc7 Query \:8bed\:6cd5\:ff0c\:8fdb\:884c\:7ed8\:56fe*)
Query[Key@cc["C","1.00"],Key@"\[CapitalSigma]N",Key@fd[2,1,0],
contribTag/*annotated/*plotLst,
(*\:7b2cn\:4e2a*)2
][interpoGEGM["v"]]
]]


(* ::Input:: *)
(*(*\:4fdd\:5b58\:5230\:78c1\:76d8\:6587\:4ef6*)*)
(*serialize["interpo",interpoGEGM["v"]]*)


(* ::Chapter::Closed:: *)
(*Grid display *)


(* \:5bf9 data \:4e2d\:7684 head \:8fdb\:884c\:8f6c\:6362\:ff0c\:8f93\:51fa\:663e\:793a\:683c\:5f0f*)
numDisp={fd->fdDisp,numKey->StringRiffle,numVal->(N[#,3]&)};
dataDsip[data_]:=Dataset[data/.{Association->assoc}
/.numDisp/.{assoc->Association}
];
(* \:5c06\:5d4c\:5957\:7684 {Assoc,Assoc} \:8f6c\:6362\:6210\:4e8c\:7ef4\:5217\:8868\:5f62\:5f0f *)
(* Curry \:5f62\:5f0f *)
dataToGrid::usage="dataToGrid[title,dataset], title \:5c06\:4f5c\:4e3a\:5217\:8868\:7684\:6807\:9898";
dataToGrid[title_][data_]:=Prepend[
KeyValueMap[Prepend[Values[#2],#1]&,data],
Prepend[Query[First,Keys]@data,title]
]


(* \:4f7f\:7528 Grid \:663e\:793a \:4e8c\:7ef4\:5217\:8868 *)
gridTable[title_,background_][dataSet_]:=Grid[
dataToGrid[title]@dataSet/.numDisp,
ItemSize->Automatic,
Frame->{All,All},
Spacings->{1,1.5},
Background->background
]


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
