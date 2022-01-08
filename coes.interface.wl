(* ::Package:: *)

(* ::Chapter:: *)
(*interfaces*)


(* ::Section:: *)
(*flatten Assoc Recursively*)


(*\:9012\:5f52\:5c55\:5e73\:5d4c\:5957\:5173\:8054, \:5c06\:6d45\:5c42 key \:524d\:7f00\:5230\:6df1\:5c42 key \:4e0a*)
SetAttributes[flatAssocRec,Orderless];
flatAssocRec[x___Rule,key_->flatAssocRec[rules__Rule]]:=flatAssocRec@@Join[{x},
Normal@KeyMap[Join[enList@key,enList@#]&]@Association@rules]
(*\:5982\:679c\:662f\:5d4c\:5957\:5173\:8054\:ff0c\:5c31\:6267\:884c\:66ff\:6362\:64cd\:4f5c*)
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
(*static*)


(*\:5708\:56fe\:7ed3\:679c\:7ea7\:6570\:5c55\:5f00\:7684 level*)
$ord0="ord0";
$ord1="ord1";
$ordFull="full";


(*\:5708\:79ef\:5206 Keys\:ff0c\:4e0d\:540c\:7c92\:5ea6\:7684\:8ba1\:7b97\:7ed3\:679c*)
kLoopAssoc="loopAssoc";
kLoopChannel="loopChan";
kLoopChanSum="loopChanSum";
kLoopAmpSum="loopAmpSum";


(*\:8868\:793a\:7ed3\:679c\:4e2d\:65e2\:5305\:542b \:6811\:56fe\:ff0c\:4e5f\:5305\:542b\:5708\:56fe*)
keyTreeAndLoop="tree+loop";
keyLoop="loop";


(* ::Section:: *)
(*ff, fd, fdDisp*)


Once@Get["gen.format-interface.wl"];


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
(*keys*)


(*+++++++++++++++++++++++++ \:5173\:8054\:4e2d\:5404\:79cd Key \:7684\:5b9a\:4e49,or \:8f93\:5165\:63a5\:53e3 +++++++++++++++++++++++++*)
fdTypeOct=fdType["oct"];fdTypeOctb=fdType["octb"];
fdTypeDec=fdType["dec"];fdTypeDecb=fdType["decb"];
fdTypeMes=fdType["mes"];fdTypeMesOut=fdType["mes","out"];
(* --------- \:8d39\:66fc\:56fe\:9876\:70b9tag\:7684\:63a5\:53e3 --------- *)
fyVtx1[x_]:=fyVtx[x,"v1"];
fyVtx2[x_]:=fyVtx[x,"v2"];
fyVtx3[x_]:=fyVtx[x,"v3"];
(* --------- \:9876\:70b9\:4fee\:9970\:7684\:63a5\:53e3 --------- *)
vtxJoin1[x_]:=fyVtx[x,"v1"]
vtxJoin2[x_]:=fyVtx[x,"v2"]
vtxJointTmp1[x_]:=fyVtx[x,"tmp1"]
vtxJointTmp2[x_]:=fyVtx[x,"tmp2"]
(* --------- \:8026\:5408\:5e38\:6570\:4e58\:79ef\:7684\:63a5\:53e3 --------- *)
fyCoeKeycAll=fyCoeKey["cAll"];
fyCoeKeycEM=fyCoeKey["cEM"];
fyCoeKeycStr=fyCoeKey["cStr"];
(* --------- \:53cd\:5e38\:78c1\:77e9 --------- *)
(*\:6240\:6709\:8026\:5408\:7cfb\:6570\:4e58\:79ef, F1,F2*)
fyCoeKeycAllF1=fyCoeKey["cAll","F1"];
fyCoeKeycAllF2=fyCoeKey["cAll","F2"];
(*\:5f3a\:4e92\:4f5c\:7528\:7cfb\:6570\:4e58\:79ef,F1,F2*)
fyCoeKeycStrF1=fyCoeKey["cStr","F1"];
fyCoeKeycStrF2=fyCoeKey["cStr","F2"];
(*\:7535\:78c1\:76f8\:4e92\:4f5c\:7528\:7cfb\:6570\:4e58\:79ef,F1,F2*)
fyCoeKeycEMF1=fyCoeKey["cEM","F1"];
fyCoeKeycEMF2=fyCoeKey["cEM","F2"];
(*\:7535\:78c1\:4f5c\:7528\:7cfb\:6570 GEGM---------*)
fyCoeKeyGE=fyCoeKey["cEM","GE"];
fyCoeKeyGM=fyCoeKey["cEM","GM"];
(* --------- \:6807\:8bb0\:5165\:5c04\:ff0c\:4e2d\:95f4\:6001\:4fe1\:606f --------- *)
inOct;outOct;
medOct1;medOct2;
medMes1;medMes2;
medDec1;medDec2;


(* --------- \:4e3a\:4e86\:65b9\:4fbf\:5339\:914d \:5708\:79ef\:5206\:8868\:8fbe\:5f0f\:4e2d\:7684\:540d\:79f0, \:8d28\:91cf\:6539\:5199\:4e3a --------- *)
MassIn=mE;MassOut;
MassOct1=mo1;MassOct2=mo2;(*\:4e2d\:95f4 oct \:91cd\:5b50 1,2*)
MassMes1=mm1;MassMes2=mm2;(*\:4e2d\:95f4 oct \:4ecb\:5b50 1,2*)
MassDec1=md1;MassDec2=md2;(*\:4e2d\:95f4 dec \:91cd\:5b501,2*)


(*++++++++++++++ \:5f62\:72b6\:56e0\:5b50\:540d\:79f0 Keys ++++++++++++++*)
ffsF1F2="F1F2";ffsF1="F1";ffsF2="F2";
(*\:7535\:78c1\:5f62\:72b6\:56e0\:5b50*)
ffsGEGM="GEGM";ffsGE="GE";ffsGM="GM";
(*\:6811\:56fe\:4e0e\:5708\:56fe*)
ffsTreeF1F2="TreeF1F2";ffsLoopF1F2="LoopF1F2";
ffsTreeGEGM="TreeGEGM";ffsLoopGEGM="LoopGEGM";
recon="reNormConst";


(*++++++++++++++ \:6570\:503c\:5316\:ff0c\:5404\:79cd\:60c5\:51b5\:6240\:4f7f\:7528\:7684 Key ++++++++++++++*)
tagNum["tr","uds"]=numKey@{"tree","uds"};
tagNum["tr","u"]=numKey@{"tree","u"};
tagNum["tr","d"]=numKey@{"tree","d"};
tagNum["tr","s"]=numKey@{"tree","s"};

tagNum["lo","uds"]=numKey@{"loop","uds"};
tagNum["lo","u"]=numKey@{"loop","u"};
tagNum["lo","d"]=numKey@{"loop","d"};
tagNum["lo","s"]=numKey@{"loop","s"};

tagNum["qch","u"]=numKey@{"quench","u"};
tagNum["qch","d"]=numKey@{"quench","d"};
tagNum["qch","d"]=numKey@{"quench","s"};

tagNum["sea","u"]=numKey@{"sea","u"};
tagNum["sea","d"]=numKey@{"sea","d"};
tagNum["sea","s"]=numKey@{"sea","s"};

tagNum["tr+lo","uds"]=numKey@{"tr+lo","uds"};
recordLocationInMessage@tagNum;


(*++++++++++++++ octet baryons \:7535\:8377\:5171\:8f6d\:7684\:89c4\:5219\:8868 ++++++++++++++*)
conjToOctBar=MapThread[Rule,{Flatten@Array[fd,{1,8,1},{2,1,0}],Flatten@Array[fd,{1,8,1},{2,1,1}]}];
conjToOct=MapThread[Rule,{Flatten@Array[fd,{1,8,1},{2,1,1}],Flatten@Array[fd,{1,8,1},{2,1,0}]}];
conjOct=conjToOctBar~Join~conjToOct;


(*+++++++++++++++ \:8d39\:66fc\:56fe\:7684\:5206\:7c7b\:ff1a\:5728\:6bcf\:4e2a\:7c7b\:522b\:4e2d\:ff0c\:5206\:522b\:68c0\:67e5\:662f\:5426\:6ee1\:8db3 \:4e2d\:6027\:7c92\:5b50 \:7535\:8377\:5b88\:6052 +++++++++++++++*)
sectOct=Key/@{
chTag@{"RB","mes","oct"},
chTag@{"KR","mes","oct","left"},
chTag@{"KR","mes","oct","add","left"},
chTag@{"RB","oct","F1"},
chTag@{"RB","oct","F2"}
};
sectDec=Key/@{
chTag@{"RB","mes","dec"},
chTag@{"RB","dec","F1"},
chTag@{"RB","dec","F2"},
chTag@{"RB","trans","left"},
chTag@{"KR","mes","dec","left"},
chTag@{"KR","mes","dec","add","left"}
};
sectBub=Key/@{
chTag@{"tad","oct","F1"},
chTag@{"tad","oct","F1","add"},
chTag@{"tad","oct","F2"},
chTag@{"bub","mes","o2"},
chTag@{"bub","mes","ten","o2"}
};
sectMag=Key/@{
chTag@{"RB","oct","F2"},
chTag@{"tad","oct","F2"},
chTag@{"bub","mes","ten","o2"},
chTag@{"RB","dec","F2"},
chTag@{"RB","trans","left"}
};


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
(*experiment*)


recordLocationInMessage@{numOctMaget,numExper}


numOctMaget=<|
ff["p"]->2.7928473446`30,ff["n"]->\[Minus]1.9130427`30,
ff["\[CapitalSigma]+"]->2.458`30,ff["\[CapitalSigma]0"]->0.60`30,ff["\[CapitalSigma]-"]->\[Minus]1.160`30,
ff["\[CapitalXi]0"]->\[Minus]1.250`30,ff["\[CapitalXi]-"]->\[Minus]0.6507`30,
ff["\[CapitalLambda]"]->\[Minus]0.613`30
|>;


(*\:6682\:65f6\:6ca1\:6709\:7684\:6570\:636e*)
Module[{tagOctfds=Array[fd[2,#,0]&,8],
tempNone=$tempNone,ffsGEGM},
ffsGEGM="Exp.";
numExper=<|
(*experiment data*)
"exp."->AssociationThread[tagOctfds,
{
<|ffsGEGM->{numAround[1,0],numAround[2.7928473446,tempNone]}|>,
<|ffsGEGM->{numAround[0,0],numAround[\[Minus]1.9130427,tempNone]}|>,

<|ffsGEGM->{numAround[1,0],numAround[2.458,0.010]}|>,
<|ffsGEGM->{numAround[0,0],numAround[0.60,tempNone]}|>,
<|ffsGEGM->{numAround[-1,0],numAround[\[Minus]1.160,0.025]}|>,

<|ffsGEGM->{numAround[0,0],numAround[\[Minus]1.250,0.014]}|>,
<|ffsGEGM->{numAround[-1,0],numAround[\[Minus]0.6507,0.080]}|>,
<|ffsGEGM->{numAround[0,0],numAround[\[Minus]0.613,0.004]}|>
}],
(* Lattice data*)
ffsGEGM="Lat.";
"Lat."->AssociationThread[tagOctfds,
{
<|ffsGEGM->{numAround[1,0],numAround[2.4,0.02]}|>,
<|ffsGEGM->{numAround[0,0],numAround[-1.59,0.0017]}|>,

<|ffsGEGM->{numAround[1,0],numAround[2.27,0.0016]}|>,
<|ffsGEGM->{numAround[0,0],numAround[tempNone,tempNone]}|>,
<|ffsGEGM->{numAround[-1,0],numAround[\[Minus]0.88,0.008]}|>,

<|ffsGEGM->{numAround[0,0],numAround[\[Minus]1.32,0.004]}|>,
<|ffsGEGM->{numAround[-1,0],numAround[\[Minus]0.71,0.003]}|>,
<|ffsGEGM->{numAround[0,0],numAround[tempNone,tempNone]}|>
}],
(*paper1 data*)
ffsGEGM="Paper";
"paper"->AssociationThread[tagOctfds,
{
<|ffsGEGM->{numAround[1,0],numAround[2.735,0.121]}|>,
<|ffsGEGM->{numAround[0,0],numAround[-1.956,0.103]}|>,

<|ffsGEGM->{numAround[1,0],numAround[2.537,0.201]}|>,
<|ffsGEGM->{numAround[0,0],numAround[0.838,0.091]}|>,
<|ffsGEGM->{numAround[-1,0],numAround[-0.861,0.040]}|>,

<|ffsGEGM->{numAround[0,0],numAround[-1.690,0.142]}|>,
<|ffsGEGM->{numAround[-1,0],numAround[-0.840,0.087]}|>,
<|ffsGEGM->{numAround[0,0],numAround[-0.867,0.074]}|>
}]
|>];


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
echo[fittingsDir=FileNameJoin[{$srcRoot,"fittings"}]];
enDir[fittingsDir];
