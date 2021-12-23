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
(*import module*)


(*------------------------\:5176\:4ed6\:53c2\:6570\:8bbe\:7f6e--------------------*)
$parOrdStr=$ordFull;
$par\[CapitalLambda]=1.00;
$par\[CapitalLambda]Str=enString@NumberForm[$par\[CapitalLambda],{3,2}];
$erroBar="notbar";


(*\:5bfc\:5165\:6b64\:8ba1\:7b97\:7a0b\:5e8f\:7684\:53c2\:6570*)
Get["ff.numeric-setup.wl"];
(*\:8bfb\:5165\:5404\:79cd\:8f93\:5165\:63a5\:53e3*)
Once@Get["coes.interface.wl"];


interpoGEGM["v"]=Import@localCachePath["interpo"];


(* \:5bf9 \:51fd\:6570\:7684\:5217\:8868 \:753b\:56fe *)
plotLst[lst_]:=Plot[Evaluate@lst,{Q2,0,1},
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


experiDir=FileNameJoin[{$srcRoot,"experiment"}];
experiDataset=Get[FileNameJoin[{experiDir,"nucleon-data.auth-year.wl"}]];


annotated["experi"][leg$pos_][assoc_Association]:=KeyValueMap[
Legended[#2,Placed[#1,leg$pos]]&,assoc];


legend$function:=Legended[#2,Placed[#1,{{0.58,0.58},{0.,0.}}]]&
(* \:7ed8\:5236\:5b9e\:9a8c\:70b9\:6570\:636e*)
teb=ListPlot[Query[
Key@ff["n"],ffsGEGM,1/*annotated[legend$function]
]@experiDataset,
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


(* ::Chapter:: *)
(*merge*)


legend$function:=Legended[#2@Q2(*/#2[0]*),Placed[#1/.{numKey->StringRiffle},{{1,0.58},{0.,0.}}]]&
(*\:901a\:8fc7 Query \:8bed\:6cd5\:ff0c\:8fdb\:884c\:7ed8\:56fe*)
Show[teb,
(*\:8ba1\:7b97\:56fe*)
Query[Key@cc["C","1.50"],Key@"N",Key@ff["n"],
contribTag/*annotated[legend$function]
/*plotLst,
(*\:7b2cn\:4e2a*)1
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
Key@cc["C","1.50"],Key@"\[CapitalSigma]+-",
All,All,All,NumberForm[Chop[#@0],4]&
]@interpoGEGM["v"],
(*\:5b9e\:9a8c\:503c*)
Query["exp."]@numExper/.{numAround->Around,$tempNone->0}
}},
(*\:5e94\:7528\:8868\:683c\:6392\:7248++++++++++++++++++++++++++++++++++++++++*)
If[$inNBook,
gridTable["GEGM",dataBackground]@data]
]
