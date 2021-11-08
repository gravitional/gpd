(* ::Package:: *)

(* ::Section:: *)
(*interfaces*)


ff::usage="human-readable \:5f62\:5f0f\:7684\:8f93\:5165\:63a5\:53e3\:ff0c\:589e\:52a0\:7a0b\:5e8f\:53ef\:8bfb\:6027";


(*\:516b\:91cd\:6001\:4ecb\:5b50\:7684\:8f93\:5165\:63a5\:53e3*)
type="mes";
fdStr[type]={(*\:4ecb\:5b50\:573a\:7684\:5b57\:7b26\:4e32\:8868\:793a*)
"\[Eta]0",
"\[Pi]+","\[Pi]0","\[Pi]-",
"K+","K-","K0","K0b",
"\[Eta]8"
};
(* ++++++++ \:5b9a\:4e49\:4ecb\:5b50\:8f93\:5165\:63a5\:53e3, ff["\[Pi]+"]=fd[1,1,0] ++++++++ *)
Once@MapThread[Set,{ff/@fdStr[type],Array[fd[1,#,0]&,9,{0,8}]}];
ff["\[Pi]\[Eta]"]=fd[1,{0,2,8},0];(*\[Eta]0,\[Pi]0,\[Eta]8,\:7684\:7b80\:5e76\:8868\:793a*)


(* ++++++++++++++++++++++++ \:516b\:91cd\:6001\:91cd\:5b50\:7684\:8f93\:5165\:63a5\:53e3 ++++++++++++++++++++++++ *)
type="oct";
fdStr[type]={
"p","n",
"\[CapitalSigma]+","\[CapitalSigma]0","\[CapitalSigma]-",
"\[CapitalXi]0","\[CapitalXi]-",
"\[CapitalLambda]"};
(* ----- \:5b9a\:4e49\:8f93\:5165\:63a5\:53e3,  ff["p"]=fd[2,1,0] ----- *)
Once@MapThread[Set,{ff/@fdStr[type],Array[fd[2,#,0]&,8]}];
ff["\[CapitalSigma]0\[CapitalLambda]"]=fd[2,{4,8},0];(*\[CapitalSigma]0,\[CapitalLambda],\:7684\:7b80\:5e76\:8868\:793a*)
(* ++++++++++++++++++++++++ \:516b\:91cd\:6001\:91cd\:5b50 anti field, \:5e26 bar \:573a ++++++++++++++++++++++++ *)
type="octb";
fdStr[type]={
"pb","nb",
"\[CapitalSigma]+b","\[CapitalSigma]0b","\[CapitalSigma]-b",
"\[CapitalXi]0b","\[CapitalXi]-b",
"\[CapitalLambda]b"};
(* ------ \:5b9a\:4e49\:8f93\:5165\:63a5\:53e3,  ff["p"]=fd[2,1,0] ------ *)
Once@MapThread[Set,{ff/@fdStr[type], Array[fd[2,#,1]&,8]}];
ff["\[CapitalSigma]0b\[CapitalLambda]b"]=fd[2,{4,8},1];(*\[CapitalSigma]0b,\[CapitalLambda]b \:7684\:7b80\:5e76\:8868\:793a*)


(*\:5341\:91cd\:6001\:91cd\:5b50\:7684\:8f93\:5165\:63a5\:53e3*)
type="dec";
fdStr[type]={
"\[CapitalDelta]++","\[CapitalDelta]+","\[CapitalDelta]0","\[CapitalDelta]-",
"\[CapitalSigma]*+","\[CapitalSigma]*0","\[CapitalSigma]*-",
"\[CapitalXi]*0","\[CapitalXi]*-",
"\[CapitalOmega]-"
};
(* ++++++++ \:5b9a\:4e49\:8f93\:5165\:63a5\:53e3, ff["\[CapitalDelta]++"]=fd[3,1,0] ++++++++ *)
Once@MapThread[Set,{ff/@fdStr[type], Array[fd[3,#,0]&,10]}];
(*\:5341\:91cd\:6001\:91cd\:5b50 anti field, \:5e26 bar \:573a*)
type="decb";
fdStr[type]={
"\[CapitalDelta]++b","\[CapitalDelta]+b","\[CapitalDelta]0b","\[CapitalDelta]-b",
"\[CapitalSigma]*+b","\[CapitalSigma]*0b","\[CapitalSigma]*-b",
"\[CapitalXi]*0b","\[CapitalXi]*-b",
"\[CapitalOmega]-b"
};
Once@MapThread[Set,{ff/@fdStr[type], Array[fd[3,#,1]&,10]}];


type="qua";
fdStr[type]={"u","d","s"};
(* ++++++++ \:5b9a\:4e49\:8f93\:5165\:63a5\:53e3, quark human-readable, ff["u"]=fd[4,1,0] ++++++++ *)
Once@MapThread[Set,{ff/@fdStr[type], Array[fd[4,#,0]&,3]}];
(* -----------\:53cd\:5938\:514b\:7684\:8f93\:5165\:63a5\:53e3 ----------- *)
type="quab";
fdStr[type]={"ub","db","sb"};
Once@MapThread[Set,{ff/@fdStr[type], Array[fd[4,#,1]&,3]}];


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
fyCoeKeycAllF1=fyCoeKey["cAll","F1"];(*\:8ddf\:53cd\:573a\:78c1\:77e9\:6709\:5173\:7684\:7c7b\:578b F1 *)
fyCoeKeycAllF2=fyCoeKey["cAll","F2"];
fyCoeKeycStrF1=fyCoeKey["cStr","F1"];(*\:8ddf\:53cd\:573a\:78c1\:77e9\:6709\:5173\:7684\:7c7b\:578b F1 *)
fyCoeKeycStrF2=fyCoeKey["cStr","F2"];
fyCoeKeycEMF1=fyCoeKey["cEM","F1"];(* EM \:7cfb\:6570*)
fyCoeKeycEMF2=fyCoeKey["cEM","F2"];
(*----------- GE,GM tag -----------*)
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


(* ::Input:: *)
(*ffsF1F2=formFactor["F1F2"];*)
(*ffsGEGM=formFactor["GEGM"];*)


(*++++++++++++++ \:5f62\:72b6\:56e0\:5b50\:540d\:79f0 Keys ++++++++++++++*)
ffsF1F2="F1F2";
ffsGEGM="GEGM";
ffsTreeF1F2="TreeF1F2";ffsLoopF1F2="LoopF1F2";
ffsTreeGEGM="TreeGEGM";ffsLoopGEGM="LoopGEGM";
recon="reNormConst";


(*++++++++++++++ \:6570\:503c\:5316\:ff0c\:5404\:79cd\:60c5\:51b5\:6240\:4f7f\:7528\:7684 Key ++++++++++++++*)
tagNum["tr","uds"]={"tree","uds"};
tagNum["tr","u"]={"tree","u"};
tagNum["tr","d"]={"tree","d"};
tagNum["tr","s"]={"tree","s"};

tagNum["lo","uds"]={"loop","uds"};
tagNum["lo","u"]={"loop","u"};
tagNum["lo","d"]={"loop","d"};
tagNum["lo","s"]={"loop","s"};

tagNum["qch","u"]={"quench","u"};
tagNum["qch","d"]={"quench","d"};
tagNum["qch","d"]={"quench","s"};

tagNum["sea","u"]={"sea","u"};
tagNum["sea","d"]={"sea","d"};
tagNum["sea","s"]={"sea","s"};

tagNum["tr+lo","uds"]={"tr+lo","uds"};


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
\[CapitalLambda]->SetPrecision[ToExpression@par\[CapitalLambda],20],
(*  octet mesons *)
mass\[Pi]->0.1381`20,massK->0.4956`20,mass\[Eta]8->0.5693`20,mass\[Eta]0->0.9452`20,
(*  octet baryons *)
mass\[CapitalSigma]->1.193`20,massN->0.939`20,mass\[CapitalXi]->1.315`20,
mass\[CapitalLambda]->1.116`20,mass\[CapitalLambda]\[CapitalSigma]->1.155`20,
massUUU->0.939`20,massDDD->0.939`20,masssss->1.315`20,
(*  decuplet baryons *)
mass\[CapitalDelta]->1.232`20,mass\[CapitalSigma]s->1.385`20,mass\[CapitalXi]s->1.530`20,mass\[CapitalOmega]->1.672`20
};


(*------------------- \:7cfb\:6570\:5934\:90e8\:7684\:663e\:5f0f rule -------------------*)
coesRule={fyCoe->Times,vtxCoe->Identity};


(*------------------- \:7535\:8377\:7684\:66ff\:6362\:89c4\:5219 -------------------*)
quaCharge["uds"]={ch["u"]->2/3,ch["d"]->-1/3,ch["s"]->-1/3};
quaCharge["u"]={ch["u"]->1,ch["d"]->0,ch["s"]->0};
quaCharge["d"]={ch["u"]->0,ch["d"]->1,ch["s"]->0};
quaCharge["s"]={ch["u"]->0,ch["d"]->0,ch["s"]->1};


(*+++++++++++++++++++++++++++ \:6570\:503c\:7cbe\:5ea6\:7684\:76f8\:5173\:8bbe\:7f6e +++++++++++++++++++++++++++*)
chopLimit=10^-10;(*cut\:7cbe\:5ea6*)precision=20;(*\:7cbe\:786e\:5ea6*)
(*----------- PaVe\:4e3b\:79ef\:5206 \:89e3\:6790\:5f0f\:4e2d\:7684\:7279\:6b8a\:51fd\:6570, \:5ef6\:8fdf Chop \:907f\:514dDiscB\:5e26\:6765\:7684\:5fae\:5c0f\:5047\:865a\:90e8 -----------*)
DiscBChop[x__]:=Chop[DiscB[x],chopLimit]/;And@@NumericQ/@{x}(*\:5f53\:8f93\:5165\:662f\:6570\:5b57\:7684\:65f6\:5019\:ff0c\:624d\:8fdb\:884cchop*)
ScalarC0Chop[x__]:=Chop[ScalarC0[x],chopLimit]/;And@@NumericQ/@{x}(*\:5f53\:8f93\:5165\:662f\:6570\:5b57\:7684\:65f6\:5019\:ff0c\:624d\:8fdb\:884cchop*)
(*\:7279\:6b8a\:51fd\:6570\:7684 \:5ef6\:8fdfChop*)
numPaVe={DiscB->DiscBChop,ScalarC0->ScalarC0Chop};
chop[x_]:=Chop[x,chopLimit]
chopQ2[x_]:=Simplify@Chop[x/.Q2->0,chopLimit]


(* ::Section:: *)
(*experiment*)


(*\:6682\:65f6\:6ca1\:6709\:7684\:6570\:636e*)
tempNone;


tagOctfds={
ff["p"],ff["n"],ff["\[CapitalSigma]+"],ff["\[CapitalSigma]0"],ff["\[CapitalSigma]-"],
ff["\[CapitalXi]0"],ff["\[CapitalXi]-"],ff["\[CapitalLambda]"]};

numExper=<|
(*experiment data*)
"exp."->AssociationThread[tagOctfds,
{
<|ffsGEGM->{numArd[1,0],numArd[2.7928473446`20,tempNone]}|>,
<|ffsGEGM->{numArd[0,0],numArd[\[Minus]1.9130427`20,tempNone]}|>,

<|ffsGEGM->{numArd[1,0],numArd[2.458`20,0.010]}|>,
<|ffsGEGM->{numArd[0,0],numArd[0.60`20,tempNone]}|>,
<|ffsGEGM->{numArd[-1,0],numArd[\[Minus]1.160`20,0.025]}|>,

<|ffsGEGM->{numArd[0,0],numArd[\[Minus]1.250`20,0.014]}|>,
<|ffsGEGM->{numArd[-1,0],numArd[\[Minus]0.6507`20,0.080]}|>,
<|ffsGEGM->{numArd[0,0],numArd[\[Minus]0.613`20,0.004]}|>
}],
(* Lattice data*)
"Lat."->AssociationThread[tagOctfds,
{
<|ffsGEGM->{numArd[1,0],numArd[2.4,0.02]}|>,
<|ffsGEGM->{numArd[0,0],numArd[-1.59,0.0017]}|>,

<|ffsGEGM->{numArd[1,0],numArd[2.27,0.0016]}|>,
<|ffsGEGM->{numArd[0,0],numArd[tempNone,tempNone]}|>,
<|ffsGEGM->{numArd[-1,0],numArd[\[Minus]0.88,0.008]}|>,

<|ffsGEGM->{numArd[0,0],numArd[\[Minus]1.32,0.004]}|>,
<|ffsGEGM->{numArd[-1,0],numArd[\[Minus]0.71,0.003]}|>,
<|ffsGEGM->{numArd[0,0],numArd[tempNone,tempNone]}|>
}],
(*paper1 data*)
"exp."->AssociationThread[tagOctfds,
{
<|ffsGEGM->{numArd[1,0],numArd[2.735,0.121]}|>,
<|ffsGEGM->{numArd[0,0],numArd[-1.956,0.103]}|>,

<|ffsGEGM->{numArd[1,0],numArd[2.537,0.201]}|>,
<|ffsGEGM->{numArd[0,0],numArd[0.838,0.091]}|>,
<|ffsGEGM->{numArd[-1,0],numArd[-0.861,0.040]}|>,

<|ffsGEGM->{numArd[0,0],numArd[-1.690,0.142]}|>,
<|ffsGEGM->{numArd[-1,0],numArd[-0.840,0.087]}|>,
<|ffsGEGM->{numArd[0,0],numArd[-0.867,0.074]}|>
}]
|>;


About
