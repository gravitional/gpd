(* ::Package:: *)

(* ::Title:: *)
(*ff.numeric-eater.wl*)


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
SetDirectory[DirectoryName[$fileName,dep]];(*SetDirectory[]\:8bbe\:7f6e\:5de5\:4f5c\:76ee\:5f55\:4e3a\:5bb6\:76ee\:5f55*)
If[FileExistsQ["init.wl"],
(*\:5c06\:4e3b\:76ee\:5f55\:6dfb\:52a0\:5230\:641c\:7d22\:8def\:5f84*)
Get["init.wl"];PrependTo[$Path,$srcRoot];
Throw["The base directory is : "<>$srcRoot];,
recurFind[dep+1](*\:5982\:679c\:8fd9\:4e00\:5c42\:627e\:4e0d\:5230\:ff0c\:5c31\:4e0a\:5347\:4e00\:5c42*)];
ResetDirectory[];(*\:91cd\:8bbe\:4e3a\:4e4b\:524d\:7684\:76ee\:5f55*),
Throw["I cann't find any init.wl in this project"]
];
recurFind[start];
]
(* \:8bb0\:5f55 master Kernel \:7684\:8fd0\:884c\:6a21\:5f0f,\:5e76\:884c\:8ba1\:7b97\:4e2d\:4f7f\:7528 *)
$inNBook=$Notebooks;echo[DateString[]," <<",$fileName];


(* ::Section:: *)
(*import module*)


coesDir=FileNameJoin[{$srcRoot,"coes"}];
mfilesDir=FileNameJoin[{$srcRoot,"mfiles"}];
(* 1st \:5bfc\:5165\:6b21\:5e8f: \:8bfb\:5165\:6392\:7248 *)
(*Once@Get["gen.format.wl"];*)
(*\:5bfc\:5165\:6240\:6709\:8d39\:66fc\:56fe tag \:7684\:5217\:8868: fyAmpLoopLst,fyAmpTreeLst*)
Once@Get["gen.integral.TagList.wl"];
(*\:8981\:8ba1\:7b97\:7684\:8d39\:66fc\:56fe\:5217\:8868*)
fyAmpPart=fyAmpLoopLst;
(*\:8bfb\:5165\:5404\:79cd\:8f93\:5165\:63a5\:53e3*)
Once@Get["coes.interface.wl"];


(* ::Section:: *)
(*num chop*)


(*+++++++++++++++++++++++++++ \:6570\:503c\:7cbe\:5ea6\:7684\:76f8\:5173\:8bbe\:7f6e +++++++++++++++++++++++++++*)
chopLimit=10^-10;(*cut\:7cbe\:5ea6*)$precision=MachinePrecision;(*\:7cbe\:786e\:5ea6*)


(* ::Section:: *)
(*cmd arguments*)


(* \:5904\:7406\:811a\:672c\:53c2\:6570,\:6a21\:62df\:547d\:4ee4\:884c\:8f93\:5165\:53c2\:6570\:7684\:60c5\:5f62 *)
If[!$Notebooks,
$inputCml=$ScriptCommandLine,(*\:5982\:679c\:5728\:547d\:4ee4\:884c\:6267\:884c*)
(*++++++++++++++++++++++++++++++++++++++++*)
$inputCml={
$fileName,(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c, \:6a21\:4eff\:547d\:4ee4\:884c, \:7b2c\:4e00\:4e2a\:53c2\:6570\:662f\:811a\:672c\:7684\:7edd\:5bf9\:8def\:5f84*)
(* \:5728\:8fd9\:91cc\:63d0\:4f9b\:5176\:4ed6\:53c2\:6570, \:4f7f\:7528 mathematica \:8bed\:6cd5\:4e0b\:7684\:5f62\:5f0f\:ff0c\:5916\:9762\:7684 enString \:4f1a\:81ea\:52a8\:8f6c\:6362\:6210\:5b57\:7b26\:4e32, \:5c3d\:91cf\:591a\:4f7f\:7528Association\:7ed3\:6784*)
$ordFull,0.90`30,1.50`30,"Baryons","notbar"
}];
echo["the input parameter is:\n",$inputCml];


(*-------------\:662f\:5426\:5f00\:59cb\:5e76\:884c\:5185\:6838--------------------*)
$parallelQ=False;
(*------------------------\:5176\:4ed6\:53c2\:6570\:8bbe\:7f6e--------------------*)
$parOrdStr=$ordFull;
$parC=SetPrecision[1.50`30,$precision];
$parCStr=enString@NumberForm[$parC,{3,2}];
$par\[CapitalLambda]=SetPrecision[0.90`30,$precision];
$par\[CapitalLambda]Str=enString@NumberForm[$par\[CapitalLambda],{3,2}];
$fitScheme="\[CapitalSigma]N";
$erroBar="notbar";
(*-----------------\:662f\:5426\:5f00\:59cb\:62df\:5408\:7a0b\:5e8f---------------------*)
$fittingQ=False;
(*\:5982\:679c\:62df\:5408\:ff0c\:53ea\:9700\:8ba1\:7b97\:96f6\:70b9\:503c\:ff0c\:5373 series order 0*)
If[$fittingQ,$parOrdStr=$ord0];


(*\:5982\:679c\:8fd8\:4e0d\:5b58\:5728\:ff0c\:5219\:521b\:5efa\:76ee\:5f55*)
echo[fittingsDir=FileNameJoin[{$srcRoot,"fittings"}]];enDir[fittingsDir];
(*\:8bfb\:53d6 c1,c2 \:7684\:62df\:5408\:503c*)
ccfitted$Err=Query[Key@cc["\[CapitalLambda]",$par\[CapitalLambda]Str],Key@cc["C",$parCStr],$fitScheme
]@Import@FileNameJoin[{fittingsDir,"nums.ccFittings.wdx"}]
ccfitted=Last@ccfitted$Err;


(*c1\[TildeTilde]3/2 \[Mu]u, c2\[TildeTilde]2/3c1-1, c3->c2-c1, cT=3/2c2+1/2,*)
numCCRelation={cc["c4"]->cc["c1"]/Sqrt[3],cc["cT"]->(3cc["c2"]+1)/2};
fittedParas=If[$fittingQ,
(*\:5982\:679c\:662f\:62df\:5408\:ff0c\:5219\:8fd9\:91cc\:4e0d\:6307\:5b9a c1,c2,C \:7684\:503c*)
numCCRelation,
(* \:975e\:62df\:5408\:7684\:60c5\:51b5\:ff0c\:7ed9\:51fa c1,c2, C \:7684\:5177\:4f53\:6570\:503c*)
Join[ccfitted,numCCRelation/.ccfitted,{cc["C"]->$parC}]
];


(*\:58f0\:660e\:4e00\:4e9b\:8fd0\:52a8\:5b66\:53d8\:91cf,\:4f7f\:7528 atom \:8868\:8fbe\:5f0f\:ff0cpackage-X \:7684 loopIntegrate \:9700\:8981\:79ef\:5206\:52a8\:91cf\:4e3a\:539f\:5b50\:8868\:8fbe\:5f0f*)
\[CapitalLambda];(*\:6b63\:89c4\:5b50\:8d28\:91cf*)
mE;(*\:5916\:817f\:7c92\:5b50\:7684\:8d28\:91cf*);
mm1;mm2;(*\:4e2d\:95f4\:516b\:91cd\:6001\:4ecb\:5b50*)
mo1;mo2;(*\:4e2d\:95f4\:516b\:91cd\:6001\:91cd\:5b50*)
md1;md2;(*\:4e2d\:95f4\:5341\:91cd\:6001\:91cd\:5b50*)
Q2;(*Q2=-q^2,\:8f6c\:79fb\:52a8\:91cf\:5e73\:65b9\:7684\:8d1f\:503c*)


(* ::Chapter:: *)
(*testing*)


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
(*Display*)


(* \:5bfc\:5165\:5177\:4f53\:8ba1\:7b97\:7684\:7a0b\:5e8f,\:6811\:56fe\:ff0c\:5708\:56fe\:ff0c\:91cd\:6b63\:5316\:5e38\:6570 *)
(*order full, \:5927\:7ea6 4m30s *)
If[$inNBook && !$fittingQ,Get["ff.numeric.worker.wl"];]


(*\:6253\:5370\:573a\:5f3a\:91cd\:6b63\:5316\:5e38\:6570*)
If[!$fittingQ,
Query[Values/*StringRiffle,Key@recon,chopQ2/*(N[#,4]&)]@ffsMerged["WithRen"][[Key@$ord0]]
]


(* ::Input:: *)
(*(*\:624b\:52a8\:67e5\:770b recons*)*)
(*Query[All,{Key@recon}]@ffsWithRen*)


(* \:5bf9 data \:4e2d\:7684 head \:8fdb\:884c\:8f6c\:6362\:ff0c\:8f93\:51fa\:663e\:793a\:683c\:5f0f*)
numDisp={fd->fdDisp,numKey->StringRiffle,numVal->(N[#,3]&)};
dataDsip[x_]:=Dataset[x/.{Association->assoc}
/.numDisp/.{assoc->Association}
];
(* \:5c06\:5d4c\:5957\:7684 {Assoc,Assoc} \:8f6c\:6362\:6210\:4e8c\:7ef4\:5217\:8868\:5f62\:5f0f *)
dataToGrid::usage="dataToGrid[title,dataset], title \:5c06\:4f5c\:4e3a\:5217\:8868\:7684\:6807\:9898";
dataToGrid[title_,x_]:=Prepend[
KeyValueMap[Prepend[Values[#2],#1]&,x],
Prepend[Query[First,Keys]@x,title]
]
(* Curry \:5f62\:5f0f *)
dataToGrid[title_]:=dataToGrid[title,#]&


(* \:4f7f\:7528 Grid \:663e\:793a \:4e8c\:7ef4\:5217\:8868 *)
gridTable[dataSet_,title_,background_]:=Grid[
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
If[$inNBook && !$fittingQ,
gridTable[numFFs["v"][[$ord0]],
"GEGM",dataBackground]]


(* ::Section:: *)
(*export*)


(*\:5982\:679c\:8fd8\:4e0d\:5b58\:5728\:ff0c\:5219\:521b\:5efa\:76ee\:5f55*)
echo[resultsDir=FileNameJoin[{$srcRoot,"results"}]];enDir[resultsDir];
(*\:4fdd\:5b58\:7ed3\:679c\:5230\:672c\:5730\:6587\:4ef6*)
serialize["result",result_]:=Block[{path},
path=FileNameJoin[{resultsDir,"nums."<>StringRiffle[{$parOrdStr,$par\[CapitalLambda]Str,$parCStr,$fitScheme,$erroBar},"-"]<>".wdx"}];
Export[path,result];
echo["Exporting finished: ", path];]


(* ::Chapter:: *)
(*Fitting*)


(*\:5bf9\:672a\:5b9a\:53c2\:6570\:8fdb\:884c\:62df\:5408*)
If[$fittingQ,
Module[{testMagMerged,testFit,$par\[CapitalLambda]Str,ccNumStr},
(*\:6700\:540e\:7684\:7ed3\:679c\:4fdd\:5b58\:5728\:5173\:8054\:4e2d*)
ccFittings=Association@Table[
(*\:521d\:59cb\:5316 \[CapitalLambda] \:5b57\:7b26\:5f62\:5f0f *)
$par\[CapitalLambda]Str=enString@NumberForm[$par\[CapitalLambda],{3,2}];
(* \:5bf9\:7279\:5b9a \[CapitalLambda], \:5bfc\:5165\:5177\:4f53\:8ba1\:7b97\:7684\:7a0b\:5e8f,\:6811\:56fe\:ff0c\:5708\:56fe\:ff0c\:91cd\:6b63\:5316\:5e38\:6570 *)
Get["ff.numeric.worker.wl"];
(*\:5bf9\:7279\:5b9a \[CapitalLambda], \:8ba1\:7b97: \:6570\:503c\:7ed3\:679c - \:5b9e\:9a8c\:7ed3\:679c, numFFs \:5728 worker \:4e2d\:8ba1\:7b97 *)
testMagMerged=Merge[{
Query[All,(Key@tagNum["tr+lo","uds"])
/*ReplaceAll[numVal->Identity]/*Last]@numFFs["v"][[$ord0]],
(-1)numOctMaget
},Total];
(* fitting \:51fd\:6570: \:5bf9\:4e8e\:7ed9\:5b9a\:7684 C \:503c, \:8fdb\:884c\:62df\:5408; \:5bf9\:5404\:79cd\:53ef\:80fd\:7684 fitting \:5e8f\:5217,\:6c42\:89e3c1,c2 \:7684fitting \:503c*)
testFit[ccNum_,testList_]:=NMinimize[{
Query[(Key/@testList)/*ReplaceAll[cc["C"]->ccNum]/*Total,Power[#,2]&]@testMagMerged,
{cc["c1"],cc["c2"]}\[Element]Reals},
{cc["c1"],cc["c2"]},
WorkingPrecision->$precision];(*\:7ed9\:5b9a\:6b64\:5904\:62df\:5408\:7684\:7cbe\:5ea6*)
(* \:6b21\:7ea7\:5173\:8054, \[CapitalLambda] \:6307\:5411\:5404\:79cd C fitting \:51fa\:7684\:7ed3\:679c*)
cc["\[CapitalLambda]",$par\[CapitalLambda]Str]->Association@Table[
(*\:521d\:59cb\:5316 C \:5b57\:7b26\:5f62\:5f0f*)
ccNumStr=enString@NumberForm[ccNum,{3,2}];
(*\:5faa\:73af\:8fdb\:5ea6\:63d0\:793a*)
echo["Lambda: ",$par\[CapitalLambda]Str,", C: ",ccNumStr];
(* \:6b21\:7ea7\:5173\:8054, C \:6307\:5411\:5404\:79cd \:62df\:5408\:5b50\:96c6\:7684\:7ed3\:679c, \:5982 \[CapitalSigma]+0-, pN, All,*)
cc["C",ccNumStr]->KeyValueMap[#1->testFit[ccNum,#2]&,tagOctfds]
,{ccNum,{1.0`30,1.1`30,1.2`30,1.3`30,1.4`30,1.5`30}}
]
,{$par\[CapitalLambda],{0.80`30,0.85`30,0.90`30,0.95`30,1.00`30}}];
]];


(* ::Section:: *)
(*export*)


(*\:4fdd\:5b58\:7ed3\:679c\:5230\:672c\:5730\:6587\:4ef6*)
serialize["ccFittings",result_]:=Block[{path},
path=FileNameJoin[{fittingsDir,"nums.ccFittings.wdx"}];
Export[path,result];
echo["Exporting finished: ", path];]


(* ::Input:: *)
(*serialize["ccFittings",ccFittings]*)


(* ::Chapter:: *)
(*EOF*)


echo[DateString[]," : finished, SessionTime : ",SessionTime[]];
