(* ::Package:: *)

(* ::Title:: *)
(*ff.numeric.wl*)


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
(* \:8bb0\:5f55 master Kernel \:7684\:8fd0\:884c\:6a21\:5f0f*)
$inNBook=$Notebooks;echo[DateString[]," <<",fileName];


(* ::Section:: *)
(*cmd arguments*)


(* \:5904\:7406\:811a\:672c\:53c2\:6570,\:6a21\:62df\:547d\:4ee4\:884c\:8f93\:5165\:53c2\:6570\:7684\:60c5\:5f62 *)
If[!$Notebooks,
inputCml=$ScriptCommandLine,(*\:5982\:679c\:5728\:547d\:4ee4\:884c\:6267\:884c*)
(*++++++++++++++++++++++++++++++++++++++++*)
inputCml={
fileName,(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c, \:6a21\:4eff\:547d\:4ee4\:884c, \:7b2c\:4e00\:4e2a\:53c2\:6570\:662f\:811a\:672c\:7684\:7edd\:5bf9\:8def\:5f84*)
(* \:5728\:8fd9\:91cc\:63d0\:4f9b\:5176\:4ed6\:53c2\:6570, \:4f7f\:7528 mathematica \:8bed\:6cd5\:4e0b\:7684\:5f62\:5f0f\:ff0c\:5916\:9762\:7684 enString \:4f1a\:81ea\:52a8\:8f6c\:6362\:6210\:5b57\:7b26\:4e32, \:5c3d\:91cf\:591a\:4f7f\:7528Association\:7ed3\:6784*)
"full",0.80`20,1.00`20,"Baryons","L-0.90.ci-1.00"
}];
echo["the input parameter is:\n",inputCml];


(*++++++++++++++++++++++++++++++++++++++++ \:63a5\:6536\:53c2\:6570, \:4fdd\:5b58\:5230\:53d8\:91cf, \:6216\:8005\:8fdb\:884c\:8fdb\:4e00\:6b65\:5904\:7406 ++++++++++++++++++++++++++++++++++++++++*)
{parOrder,par\[CapitalLambda],parC,cFitting,errorbarQ}={
enString@inputCml[[2]],
enString@NumberForm[ToExpression@inputCml[[3]],{3,2}],
enString@NumberForm[ToExpression@inputCml[[4]],{3,2}],
enString@inputCml[[5]],
enString@inputCml[[6]]
}
(*++++++++++++++++++++++++++++++++++++++++ \:68c0\:67e5\:8f93\:5165\:7684\:53c2\:6570\:662f\:5426\:5408\:6cd5 ++++++++++++++++++++++++++++++++++++++++*)
If[Nand[
StringMatchQ[cFitting,{"Sigma1","Sigma2","Nucleon","Cascade","Baryons"}],
StringMatchQ[errorbarQ,{"notbar","L-"~~NumberString~~".ci-"~~NumberString}] (*eg."L_0.90_ci_1.50"*)
],
echo["Please check the input parameters"];Abort[]
]


(*\:5982\:679c\:8fd8\:4e0d\:5b58\:5728\:ff0c\:5219\:521b\:5efa\:76ee\:5f55*)
echo[fittingsDir=FileNameJoin[{gitLocalName,"fittings"}]];
If[!DirectoryQ[fittingsDir],CreateDirectory[fittingsDir];echo["Create a new directory: ./fittings/"]] ;
(*++++++++++++++++++++++++++++++++ \:8bfb\:53d6c1,c2\:7684\:53d6\:503c ++++++++++++++++++++++++++++++++*)
c3=c2-c1;
echo["c1,c2 configuration"]
If[errorbarQ==="notbar",
(*++++++++++++++++++++++++++++++++ \:5982\:679c\:4e0d\:662f\:4e3a\:4e86\:8ba1\:7b97Error\:ff0c\:5c31\:4f7f\:7528\:5339\:914d\:7684c1c2\:8fdb\:884c\:8ba1\:7b97 ++++++++++++++++++++++++++++++++*)
echo[cFittingPath=FileNameJoin[{fittingsDir,"c1c2-magfit.L-"<>par\[CapitalLambda]<>".ci-"<>parC<>".wdx"}]];
(*++++++++++++++++++++++++++++++++ \:4f7f\:7528\:7b2c\:4e8c\:79cd\:91cd\:6574\:5316\:65b9\:6848,Z*tree+loop ++++++++++++++++++++++++++++++++*)
echo[configc1c2=Import[cFittingPath][cFitting][[2,2]]];,
(*++++++++++++++++++++++++++++++++ \:5982\:679c\:662f\:4e3a\:4e86\:8ba1\:7b97Error\:ff0c\:5c31\:4f7f\:7528\:6307\:5b9a \[CapitalLambda],ci \:5bf9\:5e94\:7684c1c2\:8fdb\:884c\:8ba1\:7b97 ++++++++++++++++++++++++++++++++*)
echo[cFittingPath=FileNameJoin[{fittingsDir,"c1c2-magfit."<>errorbarQ<>".wdx"}]];
echo[configc1c2=Import[cFittingPath][cFitting][[2,2]]];
]


(* ::Section:: *)
(*Read coes and exprs*)


(*\:8f7d\:5165 package-X*)
Needs["X`"];(*ParallelNeeds["X`"];CloseKernels[];LaunchKernels[];*)
chopLimit=10^-10;(*cut\:7cbe\:5ea6*)
precision=20;(*\:7cbe\:786e\:5ea6*)


echo[coesDir=FileNameJoin[{gitLocalName,"coes"}]];
echo[mfilesDir=FileNameJoin[{gitLocalName,"mfiles"}]];
(*\:5bfc\:5165\:6240\:6709\:8d39\:66fc\:56fe tag \:7684\:5217\:8868*)
fyAmpTagLst=Get[FileNameJoin@{gitLocalName,"gen.integral.TagList.wl"}];
fyAmpPart=fyAmpTagLst[[1;;3]];


(*\:8bfb\:5165\:7cfb\:6570\:63a5\:53e3*)
Get[FileNameJoin[{gitLocalName,"coes.interface.wl"}]];


(* ::Section:: *)
(*Kinematic & Couplings*)


(*\:58f0\:660e\:4e00\:4e9b\:8fd0\:52a8\:5b66\:53d8\:91cf,\:4f7f\:7528 atom \:8868\:8fbe\:5f0f\:ff0cpackage-X \:7684 loopIntegrate \:9700\:8981\:79ef\:5206\:52a8\:91cf\:4e3a\:539f\:5b50\:8868\:8fbe\:5f0f*)
\[CapitalLambda];(*\:6b63\:89c4\:5b50\:8d28\:91cf*)
mE;(*\:5916\:817f\:7c92\:5b50\:7684\:8d28\:91cf*);
mm1;mm2;(*\:4e2d\:95f4\:516b\:91cd\:6001\:4ecb\:5b50*)
mo1;mo2;(*\:4e2d\:95f4\:516b\:91cd\:6001\:91cd\:5b50*)
md1;md2;(*\:4e2d\:95f4\:5341\:91cd\:6001\:91cd\:5b50*)
Q2;(*Q2=-q^2,\:8f6c\:79fb\:52a8\:91cf\:5e73\:65b9\:7684\:8d1f\:503c*)


(* \:5c06\:8d28\:91cf\:8f6c\:6362\:6210\:66f4\:7b80\:6d01\:7684\:539f\:5b50\:8868\:8fbe\:5f0f *)
(*  octet mesons *)
massV@fd[1,0,2]=mass\[Eta]0;
massV@fd[1,1,2]=mass\[Pi]; massV@fd[1,2,2]=mass\[Pi]; massV@fd[1,3,2]=mass\[Pi];
massV@fd[1,4,2]=massK; massV@fd[1,5,2]=massK; massV@fd[1,6,2]=massK; massV@fd[1,7,2]=massK;
massV@fd[1,8,2]=mass\[Eta]8;
(* octet baryons *)
massV@fd[2,1,2]=massN; massV@fd[2,2,2]=massN;
massV@fd[2,3,2]=mass\[CapitalSigma]; massV@fd[2,4,2]=mass\[CapitalSigma]; massV@fd[2,5,2]=mass\[CapitalSigma];
massV@fd[2,6,2]=mass\[CapitalXi]; massV@fd[2,7,2]=mass\[CapitalXi];
massV@fd[2,8,2]=mass\[CapitalLambda];
(* decuplet baryons *)
massV@fd[3,1,2]=mass\[CapitalDelta]; massV@fd[3,2,2]=mass\[CapitalDelta];massV@fd[3,3,2]=mass\[CapitalDelta]; massV@fd[3,4,2]=mass\[CapitalDelta]; 
massV@fd[3,5,2]=mass\[CapitalSigma]s;massV@fd[3,6,2]=mass\[CapitalSigma]s; massV@fd[3,7,2]=mass\[CapitalSigma]s;
massV@fd[3,8,2]=mass\[CapitalXi]s;massV@fd[3,7,2]=mass\[CapitalXi]s;
massV@fd[3,10,2]=mass\[CapitalOmega];


numMass={
\[CapitalLambda]->0.80`20,
(*  octet mesons *)
mass\[Pi]->0.1381`20,massK->0.4956`20,mass\[Eta]8->0.5693`20,mass\[Eta]0->0.9452`20,
(*  octet baryons *)
mass\[CapitalSigma]->1.193`20,massN->0.939`20,mass\[CapitalXi]->1.315`20,
mass\[CapitalLambda]->1.116`20,mass\[CapitalLambda]\[CapitalSigma]->1.155`20,
massUUU->0.939`20,massDDD->0.939`20,masssss->1.315`20,
(*  decuplet baryons *)
mass\[CapitalDelta]->1.232`20,mass\[CapitalSigma]s->1.385`20,mass\[CapitalXi]s->1.530`20,mass\[CapitalOmega]->1.672`20
};


(*c1->2.081,c2->(2/3 c1-1),c3->(-1/3 c1-1)*)
configc1c2={c1->1.7356`20,c2->0.329`20};
numCoupLst={
cc["f"]->0.093`20,
cc["D"]->0.76`20, cc["F"]->0.5`20,
cc["C"]->parC,
\[CapitalLambda]->par\[CapitalLambda],
ch["u"]->2/3,ch["d"]->-1/3,ch["s"]->-1/3,
fyCoe->Times,vtxCoe->Identity,
Sequence@@configc1c2
};


DiscBChop[x__]:=Chop[DiscB[x],chopLimit]/;And@@NumericQ/@{x}(*\:5f53\:8f93\:5165\:662f\:6570\:5b57\:7684\:65f6\:5019\:ff0c\:624d\:8fdb\:884cchop*)
ScalarC0Chop[x__]:=Chop[ScalarC0[x],chopLimit]/;And@@NumericQ/@{x}(*\:5f53\:8f93\:5165\:662f\:6570\:5b57\:7684\:65f6\:5019\:ff0c\:624d\:8fdb\:884cchop*)
numPaVe={DiscB->DiscBChop,ScalarC0->ScalarC0Chop};


paraInitial=Hold[
(*\:8bbe\:7f6e\:5316\:7b80\:65f6\:95f4\:9650\:5236, \:5173\:95ed Simplify \:5316\:7b80\:65f6\:95f4\:8d85\:51fa \:4fe1\:606f*)
SetOptions[Simplify,TimeConstraint->1];
SetOptions[Refine,TimeConstraint->1];
Off[Simplify::time];Off[Refine::time];
];
ReleaseHold@paraInitial
ParallelEvaluate[ReleaseHold@paraInitial];


(*\:5e76\:884c\:8ba1\:7b97\:521d\:59cb\:5316*)
DistributeDefinitions[
gitLocalName,fileName,echo,enList,enString,$inNBook,
parOrder,par\[CapitalLambda],parC,cFitting,errorbarQ,
coesDir,mfilesDir,fyAmpPart,
massV,numMass,numCoupLst,numPaVe
];


(* ::Chapter:: *)
(*a*)


(* ::Section:: *)
(*a*)


(*I I/(16\[Pi]^2)*)
echo["start import coeffs and loop-exprs "];
(* import coes *)
SetAttributes[paraEnvIO,HoldAll];
paraEnvIO[tag_]:=ParallelSubmit[
Block[{coes,expr,assocLst},
coes=Import[FileNameJoin[{coesDir,"coe.chpt."<>StringRiffle[tag,"."]<>".wdx"}]];
expr={Import[FileNameJoin[{mfilesDir,"analytic.strange."<>parOrder<>"."<>StringRiffle[tag,"."]<>".wdx"}]]};
(* JoinAcross \:5229\:7528\:516c\:5171\:7684 chpt \:8d39\:66fc\:56fe tag\:ff0c\:8fde\:63a5\:7cfb\:6570\:4e0e\:5708\:79ef\:5206\:8868\:8fbe\:5f0f *)
assocLst=JoinAcross[coes,expr,Key@chTagKey["chTag"]];
(*++++++++++++++++++++*)
teb=Query[All,KeyDrop[{
mE,mm1,mo1,mo2,md1,md2,"time",fyCoeKey["cStr"],fyCoeKey["cEM"]
}]]@Query[All,
Append[#,"expr"->
(#[["expr"]]/.numPaVe/.#/.numMass)*(#[[Key@fyCoeKeycAll]]/.numCoupLst)
]&]@assocLst]]


(* ::Chapter:: *)
(*tree level*)
