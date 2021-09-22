(* ::Package:: *)

(* ::Title:: *)
(*integral_strange.wl*)


(* ::Chapter:: *)
(*initial*)


(*\:672c\:6587\:4ef6\:7684\:540d\:79f0*)
fileName=If[$Notebooks,NotebookFileName[],$InputFileName];
(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c\:ff0c\:5c31\:5237\:65b0\:7b14\:8bb0\:672c\:7684\:6807\:9898*)
Once@If[$Notebooks,NotebookWrite[Cells[][[1]],Cell[Last@FileNameSplit[fileName],"Title"]]];
(*\:67e5\:627e init.wl, \:5bfc\:5165\:6839\:76ee\:5f55\:548c\:51fd\:6570\:5b9a\:4e49.*)
Catch@Module[{recurFind,start=1,depMax},
depMax=FileNameDepth[fileName];(*\:8def\:5f84\:7684\:6700\:5927\:5c42\:6b21*)
(*-------\:5b9a\:4e49\:9012\:5f52\:51fd\:6570-------*)
recurFind[dep_Integer]:=If[dep<=depMax,
SetDirectory[DirectoryName[fileName,dep]];(*SetDirectory[]\:8bbe\:7f6e\:5de5\:4f5c\:76ee\:5f55\:4e3a\:5bb6\:76ee\:5f55*)
If[FileExistsQ["init.wl"],Get["init.wl"];Throw["The base directory is : "<>gitLocalName],recurFind[dep+1](*\:5982\:679c\:8fd9\:4e00\:5c42\:627e\:4e0d\:5230\:ff0c\:5c31\:4e0a\:5347\:4e00\:5c42*)];
ResetDirectory[];,
Throw["I cann't find any init.wl in this project"]
];
recurFind[start];
]


(* ::Chapter:: *)
(*Package-X*)


(* ::Input:: *)
(*SetOptions[EvaluationNotebook[],CommonDefaultFormatTypes->{"Output"->StandardForm}](*\:8bbe\:7f6e\:663e\:5f0f\:683c\:5f0f\:4e3a\:6807\:51c6\:683c\:5f0f*)*)


Remove["Global`\[Sigma]"];(* \[Sigma] \:662f package-X \:7684\:4fdd\:7559\:6807\:8bc6\:7b26*)
<<X`(*\:5bfc\:5165 Package-X *)
SetOptions[Simplify,TimeConstraint->2];(*\:8bbe\:7f6e\:5316\:7b80\:65f6\:95f4\:9650\:5236*)
SetOptions[Refine,TimeConstraint->2];


(* ::Section:: *)
(*kinematic quantities*)


(* \:58f0\:660e\:8fd9\:4e9b\:5e38\:91cf\:662f\:6d1b\:4f26\:5179\:6807\:91cf lorentz scalars *)
LScalarQ[\[CapitalLambda]0] = True;
LScalarQ[\[CapitalLambda]m] = True;
LScalarQ[mN] = True;

LScalarQ[mo] = True;
LScalarQ[mo1] = True;
LScalarQ[mo2] = True;

LScalarQ[mm] = True;
LScalarQ[mm1] = True;
LScalarQ[mm2] = True;

LScalarQ[md] = True;
LScalarQ[md1] = True;
LScalarQ[md2] = True;

LScalarQ[Q2] = True;


(* \:521d\:672b\:6001\:5728\:58f3\:5173\:7cfb\:ff0c\:4ee5\:53ca\:5176\:4ed6\:8fd0\:52a8\:5b66\:5173\:7cfb*)
onshell={
p1 . p1->mN^2,
p2 . p2->mN^2,
p1 . p2->1/2 Q2+mN^2,
\[CapitalLambda]m->Sqrt[\[CapitalLambda]0^2+mm^2]
};


(* ::Section:: *)
(*storage name*)


SetDirectory[
FileNameJoin[{
ParentDirectory[NotebookDirectory[]],
"analytic-storage.strange.series-o2"
}]
]


intdirectory=FileNameJoin[{
ParentDirectory[NotebookDirectory[]],
"integral-storage.strange"
}]


fuintf1nm=Table["0",{diord,1,11,1}];
fuintf2nm=Table["0",{diord,1,11,1}];
fuff1nm=Table["0",{diord,1,11,1}];
fuff2nm=Table["0",{diord,1,11,1}];


(* ::Text:: *)
(*Get int expressions*)


Table[
fuintf1nm[[diord]]=FileNameJoin[{intdirectory,"f1."<>"simplify."<>ToString[diord]<>".m"}];
fuintf2nm[[diord]]=FileNameJoin[{intdirectory,"f2."<>"simplify."<>ToString[diord]<>".m"}];
,{diord,1,11,1}];


(* ::Text:: *)
(*analytic names*)


Table[
fuff1nm[[diord]]=FileNameJoin[{Directory[],"f1."<>"analytic."<>ToString[diord]<>".m"}];
fuff2nm[[diord]]=FileNameJoin[{Directory[],"f2."<>"analytic."<>ToString[diord]<>".m"}];
,{diord,1,11,1}];


(* ::Section:: *)
(*fua matrix element*)


(* ::Input:: *)
(*fuaff1=LoopRefineSeries[fuaintf1,{Q2,0,2},Organization->Function];//AbsoluteTiming*)


(* ::Input:: *)
(*fuaff2=LoopRefineSeries[fuaintf2,{Q2,0,2},Organization->Function];//AbsoluteTiming*)


(* ::Input:: *)
(*fuaff1//Dimensions*)
(*fuaff2//Dimensions*)
(*fuaff1//Head*)
(*fuaff2//Head*)


(* ::Input:: *)
(*Put[fuaff1,fuff1nm[[1]]];*)
(*Put[fuaff2,fuff2nm[[1]]];*)
