(* ::Package:: *)

(* ::Title:: *)
(*analytic_strange.wl*)


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
ResetDirectory[];(*\:91cd\:8bbe\:4e3a\:4e4b\:524d\:7684\:76ee\:5f55*),
Throw["I cann't find any init.wl in this project"]
];
recurFind[start];
]


(* ::Chapter:: *)
(*Package-X*)


(* ::Input:: *)
(*SetOptions[EvaluationNotebook[],CommonDefaultFormatTypes->{"Output"->StandardForm}](*\:8bbe\:7f6e\:663e\:5f0f\:683c\:5f0f\:4e3a\:6807\:51c6\:683c\:5f0f*)*)


If[NameQ["\[Sigma]"],echo["please remove the definitions of \[Sigma], \[Sigma] will be used in package-X"];Remove["Global`\[Sigma]"]];(* \[Sigma] \:662f package-X \:7684\:4fdd\:7559\:6807\:8bc6\:7b26,\:9700\:8981\:6e05\:9664*)
echo["launch parallel kernels"];
(* \:5e76\:884c\:8fd0\:7b97\:51c6\:5907*)
Needs["X`"];ParallelNeeds["X`"];
(*\:542f\:52a8\:5e76\:884c\:5185\:6838*)
Switch[{$MachineName,$System},
{"OP7050","Linux x86 (64-bit)"},LaunchKernels[6],
_,LaunchKernels[]
];
SetOptions[Simplify,TimeConstraint->1];(*\:8bbe\:7f6e\:5316\:7b80\:65f6\:95f4\:9650\:5236*)
SetOptions[Refine,TimeConstraint->1];


(* ::Section:: *)
(*Import Integrals*)


echo[mfilesDir=FileNameJoin[{gitLocalName,"mfiles"}]];
(* \:6240\:6709\:8d39\:66fc\:56fe\:7684 tag \:5217\:8868 *)
fyAmpTagLst=Get[FileNameJoin@{gitLocalName,"integral_TagList.wl"}];


(* \:9009\:5b9a\:5bfc\:51fa\:683c\:5f0f\:ff0c\:5e76\:6253\:5370\:4fdd\:5b58\:4fe1\:606f *)
echoSave[path_,expr_]:=(Export[path,expr];echo["Exporting finished: ",path])


(* ::Section:: *)
(*kinematic quantities*)


(*\:58f0\:660e\:4e00\:4e9b\:8fd0\:52a8\:5b66\:53d8\:91cf,\:4f7f\:7528 atom \:8868\:8fbe\:5f0f\:ff0cpackage-X \:7684 loopIntegrate \:9700\:8981\:79ef\:5206\:52a8\:91cf\:4e3a\:539f\:5b50\:8868\:8fbe\:5f0f*)
\[CapitalLambda];(*\:6b63\:89c4\:5b50\:8d28\:91cf*)
mE;(*\:5916\:817f\:7c92\:5b50\:7684\:8d28\:91cf*);
mm1;mm2;(*\:4e2d\:95f4\:516b\:91cd\:6001\:4ecb\:5b50*)
mo1;mo2;(*\:4e2d\:95f4\:516b\:91cd\:6001\:91cd\:5b50*)
md1;md2;(*\:4e2d\:95f4\:5341\:91cd\:6001\:91cd\:5b50*)
Q2;(*Q2=-q^2,\:8f6c\:79fb\:52a8\:91cf\:5e73\:65b9\:7684\:8d1f\:503c*)
p1;p2;(*\:521d\:6001\:52a8\:91cf\:ff0c\:672b\:6001\:52a8\:91cf*)
k;(*\:5355\:5708\:56fe\:7684\:5708\:52a8\:91cf, \:79ef\:5206\:53d8\:91cf\:9700\:8981\:662f atomatic \:8868\:8fbe\:5f0f*)


(* \:58f0\:660e\:8fd9\:4e9b\:5e38\:91cf\:662f\:6d1b\:4f26\:5179\:6807\:91cf lorentz scalars *)
kinematics=HoldComplete[
LScalarQ[\[CapitalLambda]]=True;LScalarQ[mE]=True;
LScalarQ[mm1]=True;LScalarQ[mm2]=True;
LScalarQ[mo1]=True;LScalarQ[mo2]=True;
LScalarQ[md1]=True;LScalarQ[md2]=True;
LScalarQ[Q2]=True;
(* \:521d\:672b\:6001,\:8fd0\:52a8\:5b66\:5173\:7cfb*)
onShell={p1 . p1->mE^2,p2 . p2->mE^2,p1 . p2->Q2/2+mE^2};
];


DistributeDefinitions["Global`"];
(*\:5e76\:884c\:521d\:59cb\:5316*)
ReleaseHold@kinematics
ParallelEvaluate[ReleaseHold@kinematics];


(* ::Section:: *)
(*parallel LoopRefine*)


paraLRefine[tag_]:=Module[{int,intTag,analytic},
(*\:8bfb\:53d6\:79ef\:5206\:7684 wdx \:6587\:4ef6 *)
int=Import[FileNameJoin[{mfilesDir,"integral.strange."<>StringRiffle[tag,"."]<>".wdx"}]];
(*\:63d0\:53d6 integral Tag, \:8ba1\:7b97\:89e3\:6790\:8868\:8fbe\:5f0f*)
intTag=int//First;
analytic={intTag,
ParallelMap[
LoopRefineSeries[#,{Q2,0,0},Organization->Function]&,#]&/@Rest[int],
Method->"FinestGrained"
};
(*\:4fdd\:5b58\:8ba1\:7b97\:51fa\:7684\:7ed3\:679c*)
echoSave[FileNameJoin[{mfilesDir,"analytic.strange."<>StringRiffle[intTag,"."]<>".wdx"}],
analytic]
]


(* ::Section:: *)
(*LoopRefineSeries*)


{echo["LoopRefineSeries on: ",#],paraLRefine[#]}&/@fyAmpTagLst
