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


(*\:5bfc\:5165\:6b64\:8ba1\:7b97\:7a0b\:5e8f\:7684\:53c2\:6570*)
Get["ff.numeric-setup.wl"];
(*\:8bfb\:5165\:5404\:79cd\:8f93\:5165\:63a5\:53e3*)
Once@Get["coes.interface.wl"];


interpoGEGM["v"]=Import@localCachePath["interpo"];


(* ::Chapter:: *)
(*merge experi*)


experiDir=FileNameJoin[{$srcRoot,"experiment"}];
experiDataset=Get[FileNameJoin[{experiDir,"nucleon-data.auth-year.wl"}]];


ListPlot[
Values[assoc`expr[[inde]]],
PlotRange->{Full,Full},
AxesOrigin->{0,0},
PlotRangePadding->{{0,0},{Scaled[0.09],Scaled[0.12]}},
PlotRangeClipping->True,
ClippingStyle->Automatic,
Sequence@@marker`expr`sequence[expr`errobar`style,inde]
]


ListPlot[Query[
Key@ff["n"],ffsGEGM,1
]@experiDataset]
