(* ::Package:: *)

(* ::Chapter:: *)
(*interfaces*)


(* ::Section:: *)
(*Association Table*)


recordLocationInMessage[AssocTable];
(*\:7c7b\:4f3c Table \:5faa\:73af\:ff0c\:751f\:6210\:5173\:8054\:7684\:5d4c\:5957\:5217\:8868*)
AssocTable[expr_,{a_,aList_List},other:{_,_List}..
]:=Association@Table[a->AssocTable[expr,other],{a,aList}];
AssocTable[expr_,{a_,aList_List}]:=Association@Table[a->expr,{a,aList}]


(* ::Section:: *)
(*flatten Assoc Recursively*)


recordLocationInMessage[flatAssoc];


(*\:9012\:5f52\:5c55\:5e73\:5d4c\:5957\:5173\:8054, \:5c06\:6d45\:5c42 key \:524d\:7f00\:5230\:6df1\:5c42 key \:4e0a*)
SetAttributes[flatAssocRec,Orderless];
flatAssocRec[x___Rule,key_->flatAssocRec[rules__Rule]]:=flatAssocRec@@Join[{x},
Normal@KeyMap[Join[enList@key,enList@#]&]@Association@rules]
(*\:5982\:679c\:662f\:5d4c\:5957\:5173\:8054\:ff0c\:5c31\:6267\:884c\:66ff\:6362\:64cd\:4f5c--------------*)
flatAssoc[assoc_Association]:=If[
(*\:5224\:65ad\:662f\:5426\:6709\:5d4c\:5957\:7684 Association*)
AnyTrue[AssociationQ]@assoc,
assoc/.Association->flatAssocRec/.flatAssocRec->Association,
assoc
]


(* ::Section:: *)
(*query Dataset Skeleton*)


(*\:5168\:5c3a\:5bf8 Dataset \:7684\:6982\:7565\:67e5\:8be2, n \:6307\:5b9a\:67e5\:8be2\:7684\:5c42\:6570 *)
dataSkel[n_Integer][data_]:=dataSkel[n-1][data]->
Union@Flatten@Query[Sequence@@ConstantArray[Values,n],Keys]@data;
dataSkel[0][data_]:=Keys@data;
(*\:4f7f\:7528 \:6570\:7ec4\:6df1\:5ea6 \:8c03\:7528*)
dataStrut[data_]:=dataSkel[ArrayDepth[data,AllowedHeads->Association]-1]@data


(* ::Section:: *)
(*fitting scheme*)


(*\:5404\:79cd octet baryons \:7684 tag*)
$fittingScheme=<|
"\[CapitalSigma]+-"->{ff["\[CapitalSigma]+"],ff["\[CapitalSigma]-"]},
"\[CapitalSigma]"->{ff["\[CapitalSigma]+"],ff["\[CapitalSigma]0"],ff["\[CapitalSigma]-"]},
"\[CapitalSigma]-p"->{ff["\[CapitalSigma]-"],ff["p"]},
"\[CapitalSigma]N"->{ff["\[CapitalSigma]+"],ff["\[CapitalSigma]-"],ff["p"],ff["n"]},
"\[CapitalSigma]-\[CapitalXi]-"->{ff["\[CapitalSigma]-"],ff["\[CapitalXi]-"]},
"N"->{ff["p"],ff["n"]},
"p\[CapitalXi]-"->{ff["p"],ff["\[CapitalXi]-"]},
"\[CapitalXi]"->{ff["\[CapitalXi]0"],ff["\[CapitalXi]-"]},
"charged"->{ff["p"],ff["\[CapitalSigma]+"],ff["\[CapitalSigma]-"],ff["\[CapitalXi]-"]},
"many"->{ff["p"],ff["n"],ff["\[CapitalSigma]+"],ff["\[CapitalSigma]-"],ff["\[CapitalXi]0"],ff["\[CapitalXi]-"]},
"most"->{ff["p"],ff["n"],ff["\[CapitalSigma]+"],ff["\[CapitalSigma]-"],ff["\[CapitalXi]0"],ff["\[CapitalXi]-"],ff["\[CapitalLambda]"]},
"all"->{ff["p"],ff["n"],ff["\[CapitalSigma]+"],ff["\[CapitalSigma]0"],ff["\[CapitalSigma]-"],ff["\[CapitalXi]0"],ff["\[CapitalXi]-"],ff["\[CapitalLambda]"]}
|>;


(* ::Section:: *)
(*mass charge*)


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
massV@fd[3,8,2]=mass\[CapitalXi]s;massV@fd[3,9,2]=mass\[CapitalXi]s;
massV@fd[3,10,2]=mass\[CapitalOmega];


(*\:7ed9\:51fa \:8d28\:91cf\:5177\:4f53\:6570\:503c \:7684\:66ff\:6362\:89c4\:5219*)
numMass={
(*octet mesons*)
mass\[Pi]->0.1381`30,massK->0.4956`30,mass\[Eta]8->0.5693`30,mass\[Eta]0->0.9452`30,
(*  octet baryons *)
mass\[CapitalSigma]->1.193`30,massN->0.939`30,mass\[CapitalXi]->1.315`30,
mass\[CapitalLambda]->1.116`30,mass\[CapitalLambda]\[CapitalSigma]->1.155`30,
massUUU->0.939`30,massDDD->0.939`30,massSSS->1.315`30,
(*  decuplet baryons *)
mass\[CapitalDelta]->1.232`30,mass\[CapitalSigma]s->1.385`30,mass\[CapitalXi]s->1.530`30,mass\[CapitalOmega]->1.672`30
};


(*------------------- \:7535\:8377\:7684\:66ff\:6362\:89c4\:5219 -------------------*)
quaCharge["uds"]={ch["u"]->2/3,ch["d"]->-1/3,ch["s"]->-1/3};
quaCharge["u"]={ch["u"]->1,ch["d"]->0,ch["s"]->0};
quaCharge["d"]={ch["u"]->0,ch["d"]->1,ch["s"]->0};
quaCharge["s"]={ch["u"]->0,ch["d"]->0,ch["s"]->1};


(* ::Section:: *)
(*Grid display*)


recordLocationInMessage[numDisp,dataToGrid,gridTable]


(* \:5bf9 data \:4e2d\:7684 head \:8fdb\:884c\:8f6c\:6362\:ff0c\:8f93\:51fa\:663e\:793a\:683c\:5f0f*)
numDisp={fd->fdDisp,numKey->StringRiffle,numVal->(N[#,{4,3}]&)};
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


(* ::Section:: *)
(*local cache directory*)


(*\:5982\:679c\:8fd8\:4e0d\:5b58\:5728\:ff0c\:5219\:521b\:5efa\:76ee\:5f55*)
(*\:7cfb\:6570\:6587\:4ef6\:7684\:6587\:4ef6\:5939*)
coesDir=FileNameJoin[{$srcRoot,"coes"}];
enDir[coesDir];
(*\:79ef\:5206\:8868\:8fbe\:5f0f\:7684\:6587\:4ef6\:5939*)
mfilesDir=FileNameJoin[{$srcRoot,"mfiles"}];
enDir[mfilesDir];
(*\:4fdd\:5b58\:8ba1\:7b97\:7ed3\:679c\:7684\:6587\:4ef6\:5939*)
resultsDir=FileNameJoin[{$srcRoot,"results"}];
enDir[resultsDir];
(*fittings \:76ee\:5f55*)
fittingsDir=FileNameJoin[{$srcRoot,"fittings"}];
enDir[fittingsDir];
(*\:5b9e\:9a8c\:6570\:636e\:4fdd\:5b58\:76ee\:5f55*)
experiDir=FileNameJoin[{$srcRoot,"experiment"}];
enDir[experiDir];
(*\:4fdd\:5b58 gpd-\:7559\:6570\:7ed3\:679c\:7684\:6587\:4ef6\:5939*)
gpdResidueDir=FileNameJoin[{$srcRoot,"gpdRes"}];
enDir[gpdResidueDir];


(* ::Section:: *)
(*export*)


recordLocationInMessage[localPath,serialize]


(*\:7ed9\:51fa\:672c\:5730\:7f13\:5b58\:6587\:4ef6\:7684\:8def\:5f84,\:7ed9\:51fa\:6587\:4ef6\:548c\:62d3\:5c55\:540d*)
localPath[Directory_][filename_]:=FileNameJoin[{
Directory,StringRiffle[enList@filename,"-"]}];
(*io \:51fd\:6570, \:4fdd\:5b58\:7ed3\:679c\:5230\:672c\:5730\:6587\:4ef6*)
serialize[Directory_][filename_,result_]:=With[
{path=localPath[Directory][filename]},
Export[path,result];echo["Exporting finished: ", path];]
