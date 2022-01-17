(* ::Package:: *)

(* ::Title:: *)
(*ff.numeric-experi-merged.wl*)


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
,"--update","False"
,"--parallel-Lbd","False"
,"--ord","$ordFull"
,"--Lbd-num","0.80"
};


parseCml[]


(* ::Section:: *)
(*import module*)


(*\:5bfc\:5165\:6240\:6709\:8d39\:66fc\:56fe tag \:7684\:5217\:8868: fyAmpLoopLst,fyAmpTreeLst*)
Once@Get["gen.integral-TagList.wl"];
(*\:8981\:8ba1\:7b97\:7684\:8d39\:66fc\:56fe\:5217\:8868*)
fyAmpPart=fyAmpLoopLst;
(*\:8bfb\:5165\:5404\:79cd\:8f93\:5165\:63a5\:53e3*)
Get["coes.interface.wl"];
Get["ff.numeric-interface.wl"];


interpoGEGM["v"]=Import@localPathResult[resultsDir]["interpo.wdx"];


(* \:5bf9 \:51fd\:6570\:7684\:5217\:8868 \:753b\:56fe *)
plotList[Q2_][lst_]:=Plot[Evaluate@lst,{Q2,0,1},
PlotTheme->{"Scientific"},
PlotRange->{{0,1},Full},
ImageSize->Large,
PlotLegends->None];


(*\:5c06\:5173\:8054\:5217\:8868\:4e2d\:7684\:5143\:7d20\:ff0c\:8f6c\:6362\:6210\:5e26\:6ce8\:91ca\:7684 wrapper \:8868\:8fbe\:5f0f, \:4f20\:5165 plot \:4f5c\:56fe*)
annotated[Legended_][assoc_Association]:=KeyValueMap[Legended,assoc];


(*\:6311\:51fa\:8981\:5c55\:793a\:7684\:8d21\:732e, tree,loop,uds, sea,valence*)
contribTag=Key/@{tagNum["lo","uds"],
tagNum["lo","u"],tagNum["lo","d"],tagNum["lo","s"],
tagNum["tr+lo","uds"]};


(* ::Chapter:: *)
(*import experiment*)


experiDataset=Import@localPath["experiment"]["nucleon-data.auth-year.non-normalized.wl"];


annotated["experi"][legPos_][assoc_Association]:=KeyValueMap[
Legended[#2,Placed[#1,legPos]]&,assoc];


(*\:516c\:5171\:8bbe\:7f6e,\:5168\:5c40\:53d8\:91cf*)
(*cc["C","1.00"],cc["C","1.10"],cc["C","1.20"],cc["C","1.30"],cc["C","1.40"],cc["C","1.50"]*)
tmp`cc=Key@cc["C","1.00"];
(*"all","charged","many","most","N","p\[CapitalXi]-","\[CapitalSigma]","\[CapitalSigma]+-","\[CapitalSigma]N"*)
tmp`scheme=Key@"most";
(*"p","n","\[CapitalSigma]+","\[CapitalSigma]0","\[CapitalSigma]-","\[CapitalXi]0","\[CapitalXi]-","\[CapitalLambda]"*)
tmp`oct=Key@ff["p"];
(*1:GE,2:GM*)
tmp`gegm=2;
(*\:53c2\:6570\:68c0\:67e5-----*)
tmp`oct::OutRange="Only \"p\", \"n\" experiment data aquired for now";
If[!MemberQ[Key/@{ff["p"],ff["n"]},tmp`oct],
Message[tmp`oct::OutRange];Abort[];
]


(*\:6807\:6ce8\:6570\:636e*)
(*{{0.58,0.58},{0.,0.}}*)
legendFn[key_,val_]:=Legended[val,Placed[key,After]];
(* \:7ed8\:5236\:5b9e\:9a8c\:70b9\:6570\:636e*)
figGroup["exper"]=ListPlot[Query[
(*octet*)tmp`oct
,(*FFactors*)ffsGEGM
,(*GEGM*)tmp`gegm/*annotated[legendFn]
]@experiDataset,
(*\:753b\:56fe\:9009\:9879*)
PlotTheme->{"Scientific"},
ImageSize->Large,
PlotRange->{{0,1},Full},
AxesOrigin->{0,0},
PlotRangePadding->{{0,0},{Scaled[0.09],Scaled[0.12]}},
PlotRangeClipping->True,
ClippingStyle->Automatic,
PlotMarkers->Automatic,
IntervalMarkers->Automatic,
IntervalMarkersStyle->Automatic
]


(* ::Section:: *)
(*merge*)


(*\:6807\:6ce8\:6570\:636e*)
(*{{1,0.58},{0.,0.}}*)
legendFn[key_,val_]:=Legended[val@Q2,
Placed[key/.{numKey->StringRiffle},After]];
(*\:901a\:8fc7 Query \:8bed\:6cd5\:ff0c\:8fdb\:884c\:7ed8\:56fe*)
Show[figGroup["exper"]
(*\:8ba1\:7b97\:56fe*)
,Query[
(*cc-value*)tmp`cc
,(*fit-scheme*)tmp`scheme
,(*octet*)tmp`oct
,(*loop-tree-uds*)contribTag/*annotated[legendFn]/*plotList[Q2]
,(*GEGM pair*)tmp`gegm
]@interpoGEGM["v"],
PlotRange->{{0,1},Automatic}
]


(* ::Chapter:: *)
(*Grid display*)


(*\:80cc\:666f\:8272\:914d\:7f6e*)
dataBackground={
None,(* color horizontal: x1, x2, x3...*)
{
LightCyan,{None,LightBlue}
}(* color vertical: y1, y2, y3...*)
};


With[{
(*\:5408\:5e76\:6570\:636e, \:5d4c\:5957\:5173\:8054\:ff0c\:9012\:5f52 Merge ++++++++++++++++++++++++++++++*)
data=Nest[Merge,Identity,2]@{
(*\:8ba1\:7b97\:503c*)
Query[
(*cc-values*)Key@cc["C","1.50"]
,(*fitting-scheme*)Key@"most"
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
