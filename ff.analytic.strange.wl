(* ::Package:: *)

(* ::Title:: *)
(*analytic_strange.wl*)


(* ::Chapter:: *)
(*initial*)


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
(*cmd arguments*)


(*\:5982\:679c\:8fd8\:4e0d\:5b58\:5728\:ff0c\:5219\:521b\:5efa\:76ee\:5f55*)
echo[mfilesDir=FileNameJoin[{$srcRoot,"mfiles"}]];
(*\:5bfc\:5165\:6240\:6709\:8d39\:66fc\:56fe tag \:7684\:5217\:8868: fyAmpLoopLst,fyAmpTreeLst*)
Get["gen.integral-TagList.wl"];
(*\:5bfc\:5165\:4e00\:4e9b\:8f93\:5165\:63a5\:53e3*)
Get["coes.interface.wl"];
Get["ff.numeric-interface.wl"];


(* \:5904\:7406\:811a\:672c\:53c2\:6570,\:6a21\:62df\:547d\:4ee4\:884c\:8f93\:5165\:53c2\:6570\:7684\:60c5\:5f62 *)
If[!$Notebooks,
inputCml=$ScriptCommandLine,(*\:5982\:679c\:5728\:547d\:4ee4\:884c\:6267\:884c*)
(*++++++++++++++++++++++++++++++++++++++++*)
inputCml={
$fileName,(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c, \:6a21\:4eff\:547d\:4ee4\:884c, \:4ee4\:7b2c\:4e00\:4e2a\:53c2\:6570\:662f\:6b64\:811a\:672c\:7684\:7edd\:5bf9\:8def\:5f84*)
(* \:5728\:8fd9\:91cc\:63d0\:4f9b\:5176\:4ed6\:53c2\:6570, \:4f7f\:7528 mathematica \:8bed\:6cd5\:4e0b\:7684\:5f62\:5f0f\:ff0c\:5916\:9762\:7684 enString \:4f1a\:81ea\:52a8\:8f6c\:6362\:6210\:5b57\:7b26\:4e32, \:5c3d\:91cf\:591a\:4f7f\:7528Association\:7ed3\:6784*)
"full",
"{16}",
"coarse"
}
];
echo["the input parameter is:\n",inputCml];


(*\:63a5\:6536\:53c2\:6570, \:4fdd\:5b58\:5230\:53d8\:91cf, \:6216\:8005\:8fdb\:884c\:8fdb\:4e00\:6b65\:5904\:7406*)
(*+++++++++++++++++++++++++++++++++++++ \:9ed8\:8ba4\:503c +++++++++++++++++++++++++++++++++++++*)
$fineSubmit=False;
fyAmpTagPart=fyAmpLoopLst;(*\:79ef\:5206\:90e8\:5206\:6307\:5b9a\:7684\:9ed8\:8ba4\:503c\:ff1aAll*)
parOrder="ord0";
(*+++++++++++++++++++++++++++++++++++++ \:53c2\:6570 4 +++++++++++++++++++++++++++++++++++++*)
If[Length@inputCml>=4,
Switch[inputCml[[4]],
"coarse",$fineSubmit=False,
"fine",$fineSubmit=True,
_,echo["para 4: Parallel scheme, one of: 'coarse', 'fine'"];Abort[]
]]
(*+++++++++++++++++++++++++++++++++++++ \:53c2\:6570 3 +++++++++++++++++++++++++++++++++++++*)
If[Length@inputCml>=3,
Check[
fyAmpTagPart=fyAmpLoopLst[[ToExpression@inputCml[[3]]]],
echo["para 3: Part speciation is not valid"];Abort[]
]]
(*+++++++++++++++++++++++++++++++++++++ \:53c2\:6570 2 +++++++++++++++++++++++++++++++++++++*)
If[
Length@inputCml>=2,
parOrder=inputCml[[2]];
(*\:68c0\:67e5\:8f93\:5165\:7684\:53c2\:6570\:662f\:5426\:5408\:6cd5,*)
If[Not@StringMatchQ[parOrder,{"ord0","ord1","full"}],
echo["para 2: specify Refine orders, must be one of 'ord0', 'ord1', 'full'"];
Abort[]
]]


(* ::Section:: *)
(*Package-X*)


(* ::Input:: *)
(*SetOptions[EvaluationNotebook[],CommonDefaultFormatTypes->{"Output"->StandardForm}](*\:8bbe\:7f6e\:663e\:5f0f\:683c\:5f0f\:4e3a\:6807\:51c6\:683c\:5f0f*)*)


If[NameQ["\[Sigma]"],echo["please remove the definitions of \[Sigma], \[Sigma] will be used in package-X"];Remove["Global`\[Sigma]"]];(* \[Sigma] \:662f package-X \:7684\:4fdd\:7559\:6807\:8bc6\:7b26,\:9700\:8981\:6e05\:9664*)
(* \:5e76\:884c\:8fd0\:7b97\:51c6\:5907*)
Needs["X`"];ParallelNeeds["X`"];
(*\:542f\:52a8\:5e76\:884c\:5185\:6838*)
CloseKernels[];
Switch[{$MachineName,$System},
{"OP7050","Linux x86 (64-bit)"},LaunchKernels[6],
_,LaunchKernels[]
];


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


paraInitial=Hold[
(* \:58f0\:660e\:8fd9\:4e9b\:5e38\:91cf\:662f\:6d1b\:4f26\:5179\:6807\:91cf lorentz scalars *)
LScalarQ[\[CapitalLambda]]=True;LScalarQ[mE]=True;
LScalarQ[mm1]=True;LScalarQ[mm2]=True;
LScalarQ[mo1]=True;LScalarQ[mo2]=True;
LScalarQ[md1]=True;LScalarQ[md2]=True;
LScalarQ[Q2]=True;
(*\:8bbe\:7f6e\:5316\:7b80\:65f6\:95f4\:9650\:5236, \:5173\:95ed Simplify \:5316\:7b80\:65f6\:95f4\:8d85\:51fa \:4fe1\:606f*)
SetOptions[Simplify,TimeConstraint->1];
SetOptions[Refine,TimeConstraint->1];
Off[Simplify::time];Off[Refine::time];
];


(*\:5e76\:884c\:8ba1\:7b97\:521d\:59cb\:5316*)
DistributeDefinitions[$srcRoot,$fileName,echo,enList,enString,$inNBook,
mfilesDir,
parOrder,ffsF1F2
];
ReleaseHold@paraInitial
ParallelEvaluate[ReleaseHold@paraInitial];


(* ::Chapter:: *)
(*parallel LoopRefine*)


(* \:5bf9\:5708\:79ef\:5206\:7684\:5b50\:9879\:8fdb\:884c\:5e76\:884c\:7684\:7248\:672c, \:66f4\:52a0\:7ec6\:5316 $fineSubmit == True *)
(*\:8bbe\:7f6e\:73af\:5883:\:8bfb\:53d6\:79ef\:5206\:8868\:8fbe\:5f0f\:ff0c\:4ee5\:53ca\:5c06\:8ba1\:7b97\:7684\:7ed3\:679c\:5199\:5165\:78c1\:76d8, \:53c2\:6570: \:5708\:79ef\:5206tag, \:5177\:4f53\:5904\:7406\:79ef\:5206\:7684\:51fd\:6570*)
If[$fineSubmit,
paraEnvIO[tag_,loopRefine_]:=Block[{int,intTag,intExpr,time0Result,anaExpr,path},
(*\:8bfb\:53d6\:79ef\:5206\:7684 wdx \:6587\:4ef6 *)
echo[DateString[],": Refine loop integral of: ",tag];
int=Import[FileNameJoin[{mfilesDir,"integral.strange."<>StringRiffle[tag,"."]<>".wdx"}]];
intTag=int[["tag"]];(*\:63d0\:53d6 Loop Integral Tag*)
intExpr=int@ffsF1F2;(*\:63d0\:53d6 Loop Integral \:8868\:8fbe\:5f0f*)
(* \:5982\:679c\:5708\:79ef\:5206\:7684\:5934\:90e8\:662f Plus\:ff0c\:624d\:80fd\:4f7f\:7528 ParallelMap *)
If[AllTrue[MatchQ[Head[#],Plus]&/@intExpr,Identity],
intExpr=Map[Cancel,intExpr,{2}];(* \:5bf9\:5708\:79ef\:5206\:7684\:8868\:8fbe\:5f0f\:8fdb\:884c\:9884\:5316\:7b80*)
time0Result=ParallelMap[
loopRefine,#,{2},(* \:8ba1\:7b97\:89e3\:6790\:8868\:8fbe\:5f0f, Mapping loopRefine \:5230 \:5708\:79ef\:5206\:8868\:8fbe\:5f0f\:7684\:6bcf\:4e00\:9879\:4e0a*)
Method->"FinestGrained"(*Method->Automatic*)
]&@intExpr//AbsoluteTiming;
anaExpr=<|
chTagKey["chTag"]->chTag[intTag],
"time"->First@time0Result,
ffsF1F2->Last@time0Result
|>;
(*\:9009\:5b9a\:5bfc\:51fa\:683c\:5f0f\:ff0c\:4fdd\:5b58\:8ba1\:7b97\:51fa\:7684\:7ed3\:679c*)
path=FileNameJoin[{mfilesDir,"analytic.strange."<>parOrder<>"."<>StringRiffle[intTag,"."]<>".wdx"}];
Export[path,anaExpr];
echo[DateString[],": Exporting finished: ", path];
(*\:5982\:679c\:5728\:7b14\:8bb0\:672c\:754c\:9762,\:8fd4\:56de\:8ba1\:7b97\:51fa\:7684\:89e3\:6790\:8868\:8fbe\:5f0f*)
If[$inNBook,anaExpr],
(* +++++++++++++++++ \:5982\:679c\:5708\:79ef\:5206\:4e0d\:662f Plus[...] \:7684\:5f62\:5f0f +++++++++++++++++++ *)
echo["Check the loop integral, it is not the form of plus[...], Plese use coarse method"];
Abort[];]]]


(* \:5bf9\:6574\:4e2a\:5708\:79ef\:5206\:8868\:8fbe\:5f0f\:8fdb\:884c\:5e76\:884c\:7684\:7248\:672c *)
(*\:8bbe\:7f6e\:73af\:5883:\:8bfb\:53d6\:79ef\:5206\:8868\:8fbe\:5f0f\:ff0c\:4ee5\:53ca\:5c06\:8ba1\:7b97\:7684\:7ed3\:679c\:5199\:5165\:78c1\:76d8, \:53c2\:6570: \:5708\:79ef\:5206tag, \:5177\:4f53\:5904\:7406\:79ef\:5206\:7684\:51fd\:6570*)
If[!$fineSubmit,
SetAttributes[paraEnvIO,HoldAll];
paraEnvIO[tag_,loopRefine_]:=ParallelSubmit[
Block[{int,intTag,intExpr,time0Result,anaExpr,path},
(*\:8bfb\:53d6\:79ef\:5206\:7684 wdx \:6587\:4ef6 *)
echo[DateString[],": Refine loop integral of: ",tag];
int=Import[FileNameJoin[{mfilesDir,"integral.strange."<>StringRiffle[tag,"."]<>".wdx"}]];
(* \:4ece\:5173\:8054\:4e2d\:63d0\:53d6\:8868\:8fbe\:5f0f\:ff0c\:4f7f\:7528 Part \:8bed\:6cd5\:66f4\:5feb,\:76f8\:6bd4\:4e8e\:51fd\:6570\:8bed\:6cd5 *)
intTag=int[["tag"]];(*\:63d0\:53d6 Loop Integral Tag*)
intExpr=int@ffsF1F2;(*\:63d0\:53d6 Loop Integral \:8868\:8fbe\:5f0f*)
(* \:5bf9\:5708\:79ef\:5206\:7684\:8868\:8fbe\:5f0f\:8fdb\:884c\:9884\:5316\:7b80*)
intExpr=If[MatchQ[Head[#],Plus],Cancel/@#,Cancel@#
]&/@intExpr;
(* \:8ba1\:7b97\:89e3\:6790\:8868\:8fbe\:5f0f, loopRefine \:5c06\:5708\:79ef\:5206\:8f6c\:6362\:6210 \:89e3\:6790\:8868\:8fbe\:5f0f*)
(* loopRefine or LoopRefineSeries \:81ea\:52a8 Mapping \:5230 F1,F2 \:4e24\:4e2a \:5708\:79ef\:5206\:4e0a*)
time0Result=loopRefine[intExpr]//AbsoluteTiming;
(* \:5c06\:7ed3\:679c\:8868\:793a\:6210 Association *)
anaExpr=<|
chTagKey["chTag"]->chTag[intTag],
"time"->First@time0Result,
ffsF1F2->Last@time0Result
|>;
(*\:9009\:5b9a\:5bfc\:51fa\:683c\:5f0f\:ff0c\:4fdd\:5b58\:8ba1\:7b97\:51fa\:7684\:7ed3\:679c*)
path=FileNameJoin[{mfilesDir,"analytic.strange."<>parOrder<>"."<>StringRiffle[intTag,"."]<>".wdx"}];
Export[path,anaExpr];
echo[DateString[],": Exporting finished: ", path];
(*\:5982\:679c\:5728\:7b14\:8bb0\:672c\:754c\:9762,\:8fd4\:56de\:8ba1\:7b97\:51fa\:7684\:89e3\:6790\:8868\:8fbe\:5f0f*)
If[$inNBook,anaExpr]
]]]


(*\:6839\:636e\:811a\:672c\:53c2\:6570\:ff0c\:7ed9\:51fa\:5e76\:884c\:8ba1\:7b97\:65f6 paraLRefine \:7684\:5177\:4f53\:5b9a\:4e49, \:8fdb\:884c\:7ea7\:6570\:5c55\:5f00\:ff0c\:6216\:8005\:8ba1\:7b97\:5b8c\:6574\:8868\:8fbe\:5f0f *)
Switch[parOrder,
(* +++++++++++++++++++++++++++++++++++++ order0 +++++++++++++++++++++++++++++++++++++  *)
"ord0",
paraLRefine[tag_]:=paraEnvIO[tag,
LoopRefineSeries[#,{Q2,0,0},Organization->Function]&
],
(* +++++++++++++++++++ order1  ++++++++++++++++++ *)
"ord1",
paraLRefine[tag_]:=paraEnvIO[tag,
LoopRefineSeries[#,{Q2,0,1},Organization->Function]&
],
(* +++++++++++++++++++ full  ++++++++++++++++++ *)
"full",
paraLRefine[tag_]:=paraEnvIO[tag,
LoopRefine[#,Organization->Function]&
]
];


(* ::Section:: *)
(*LoopRefineSeries*)


analyLst=WaitAll[paraLRefine/@fyAmpTagPart];


echo[DateString[]," : finished, SessionTime : ",SessionTime[]];
