(* ::Package:: *)

(* ::Title:: *)
(*gpd_loop_convergence.nb*)


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
(*packages*)


Once[<<"FeynCalc`"]


fceStd[x_]:=x//FCE//StandardForm;
fciStd[x_]:=x//FCI//StandardForm;


(* ::Section:: *)
(*kinematics*)


(*\:516b\:91cd\:6001\:90e8\:5206\:547d\:540d*)
Clear[
mN,(*\:521d\:672b\:6001\:6838\:5b50\:8d28\:91cf*)
f,(*\:4ecb\:5b50\:8870\:53d8\:5e38\:6570*)
\[CapitalDelta],(*MB-mN*)
mo1,(*\:4e2d\:95f4\:6001\:91cd\:5b501\:7684\:8d28\:91cf*)
mo2,(*\:4e2d\:95f4\:6001\:91cd\:5b502\:7684\:8d28\:91cf*)
mSum(*mN+MB\:ff0c\:8d28\:91cf\:4e4b\:548c*)
];


(* ::Section:: *)
(*\:4f20\:64ad\:5b50\:7ed3\:6784*)


mergeRule::usage="\:8f85\:52a9\:51fd\:6570\:ff0c\:5408\:5e76\:591a\:4e2a\:5173\:8054,\:5c06\:91cd\:590d\:7684\:952e\:503c\:6c42\:548c\:ff0c\:5b58\:5165\:5217\:8868";
mergeRule[rules:__Association|__Rule]:=Normal@Merge[{rules},Total];


(*\:5c06\:4f20\:64ad\:5b50\:4ee5\:5173\:8054\:7684\:5f62\:5f0f\:4fdd\:5b58\:8d77\:6765 <{k+q,\[CapitalLambda]}\[Rule]-2...>*)
fad::usage="\:8bb0\:5f55\:4f20\:64ad\:5b50\:7ed3\:6784\:ff0c\:4f5c\:4e3a\:8f93\:5165\:63a5\:53e3";
fad[rules:__Rule]:=fadTmp1@@mergeRule[rules]; (*\:5c06\:91cd\:590d\:7684\:4f20\:64ad\:5b50\:7684\:5e42\:6b21,\:7d2f\:52a0\:8d77\:6765*)


SetAttributes[{fadTmp1,fadTmp2},Orderless];(*\:8ba1\:7b97\:89e3\:6790\:7ed3\:679c\:7684\:51fd\:6570\:4e0d\:4f9d\:8d56\:4e8e\:53c2\:6570\:6b21\:5e8f*)


(*\:6574\:7406\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:ff0c\:5408\:5e76\:76f8\:540c\:7684\:9879*)
fadTmp1::usage="\:8bb0\:5f55\:4f20\:64ad\:5b50\:7ed3\:6784\:ff0c\:81ea\:52a8\:5408\:5e76\:4e24\:4e2a\:76f8\:4e58\:7684\:4f20\:64ad\:5b50\:51fd\:6570";
(*\:5408\:5e76\:4e24\:4e2a\:4f20\:64ad\:5b50\:8bb0\:5f55*)
fadTmp1[ruleA:__Rule]*fadTmp1[ruleB:__Rule]^:=fadTmp1@@mergeRule[ruleA,ruleB];
(*\:5c06\:4f20\:64ad\:5b50\:7684\:5e42\:6b21,\:89c4\:6574\:5230\:53c2\:6570\:4e2d*)
Power[fadTmp1[rules:__Rule],n_]^:=fadTmp1@@mergeRule[n*Association@{rules}] 
(*\:4f20\:64ad\:5b50\:7684\:6392\:7248\:ff0c\:5b9a\:4e49\:6613\:8bfb\:7684\:6837\:5f0f*)
Format[fadTmp1[rules__Rule],TraditionalForm]:=\[DoubleStruckCapitalD][Values[{rules}]];


(* ::Chapter:: *)
(*functions*)


(* ::Section:: *)
(*\:5149\:9525\:5206\:91cf\:8868\:793a*)


(*\:6700\:540e\:4e24\:4e2a\:5206\:91cf\:4f5c\:6b27\:51e0\:91cc\:5f97\:5185\:79ef*)
SetAttributes[dot,Orderless];(*\:8ba1\:7b97\:89e3\:6790\:7ed3\:679c\:7684\:51fd\:6570\:4e0d\:4f9d\:8d56\:4e8e\:53c2\:6570\:6b21\:5e8f*)
dot[p_+q__,l_]:=dot[p,l]+dot[q,l](*\:7ebf\:6027\:5f8b*)
dot[num_*p_,q_]:=num*dot[p,q]/;NumberQ[num](*\:5206\:914d\:5f8b*)


(*\:52a8\:91cf\:5934\:90e8, \:52a8\:91cf\:7684\:8fd0\:7b97\:4e0d\:4f9d\:8d56\:4e8e\:53c2\:6570\:6b21\:5e8f*)
SetAttributes[momentum,Orderless];
momentum[p_+q__]:=momentum[p]+momentum[q](*\:7ebf\:6027\:5f8b*)
momentum[num_*p_]:=num*momentum[p]/;NumberQ[num](*\:5206\:914d\:5f8b*)


lConeMember::usage="\:5bf9\:7ed9\:5b9a\:7684\:7b26\:53f7,\:751f\:6210\:4e09\:4e2a\:5149\:9525\:5206\:91cf";
lConeMember[momentum_]:=momentum/@Range[3];


(*\:5149\:9525\:6807\:91cf\:79ef:light-cone scalar product\:ff0c1\:8868\:793a+\:5206\:91cf\:ff0c2\:8868\:793a-\:5206\:91cf\:ff0c3\:8868\:793a\[Perpendicular]\:5206\:91cf*)
lightConeSP[momentP_,momentQ_]:=Module[{
lcp=momentum[momentP]/.momentum->lConeMember,
lcq=momentum[momentQ]/.momentum->lConeMember
},
Expand[
1/2 (lcp[[1]]*lcq[[2]]+lcp[[2]]*lcq[[1]])-dot[lcp[[3]],lcq[[3]]]
]]


(* ::Input:: *)
(*(*gpd \:4e2d\:4f7f\:7528\:7684\:5149\:9525\:52a8\:91cf\:53c2\:6570\:5316*)*)
(*lightCone[k]:={y*p1p,km,kt}(*\:4ecb\:5b50\:5708\:52a8\:91cf*)*)
(*lightCone[p1]:={p1p,(1-\[Zeta])p1p,pt}(*GDP \:4e2d\:7684\:6838\:5b50\:521d\:672b\:6001\:52a8\:91cf\:ff0c\:4ee5\:53ca\:52a8\:91cf\:4e4b\:5dee*)*)
(*lightCone[p2]:={(1-\[Zeta])p1p,p1p,pt}*)
(*lightCone[q]:={-\[Zeta]*p1p,\[Zeta]*p1p,0}*)


(*\:4f20\:64ad\:5b50\:7684\:5f62\:5f0f*)
gator[{k_,\[CapitalLambda]_}]:=lightConeSP[k,k]-\[CapitalLambda]^2;


(*++++++++++++++++++++++++++++++++++++++++++++*)
(*\:4f20\:64ad\:5b50\:53ef\:80fd\:914d\:7f6e\:7684\:5217\:8868*)
propaConfs={
{k,\[CapitalLambda]},{k,mmo1},(*1,2*)
{k-q,\[CapitalLambda]},{k-q,mmo1},(*3,4*)
{p1-k,mo1},{p2-k,mo2},(*5,6*)
{k+q,\[CapitalLambda]},{k+q,mmo1}(*7,8*)
};
(*\:4f20\:64ad\:5b50\:5728\:5149\:9525\:5750\:6807\:4e0b\:7684\:5f62\:5f0f,\:5173\:8054*)
gatorAssoc=AssociationMap[gator,propaConfs];


(*k- \:5206\:91cf\:524d\:7684\:7cfb\:6570-------------------*)
momCoes["k-"]=Query[(*<propagator\[Rule]LightCone exprs>*)All
(*LightCone exprs*)
,(Coefficient[#1,k[2],1]&)/*Simplify
]@gatorAssoc;
(*\:4f20\:64ad\:5b50\:5149\:9525\:5f62\:5f0f \[Equal]0 \:65f6, k- \:7684\:89e3-----------------*)
gatorZeros["k-"]=Query[All,
First@First@Solve[#==0,{k[2]}]&
]@gatorAssoc;


fadTmp2::usage="fadTmp1\[Rule]fadTmp2,\:628a\:4f20\:64ad\:5b50\:8f6c\:6362\:5230\:5149\:9525\:5206\:91cf\:5f62\:5f0f\:ff0c\:4e5f\:5c31\:662f\:5177\:4f53\:5f62\:5f0f";
fadTmp2[rules__]:=Times@@KeyValueMap[
(*Power[propagator,num] \:663e\:793a\:8868\:793a\:51fa\:4f20\:64ad\:5b50\:7684\:5e42\:6b21*)
Power,
(*\:4ee3\:5165\:4f20\:64ad\:5b50\:7684\:5149\:9525\:5f62\:5f0f\:ff0ckey\[Rule]val,val \:662f\:4f20\:64ad\:5b50\:7684\:5e42\:6b21*)
KeyMap[gator,Association[rules]]]


(* ::Section:: *)
(*splitting function projectors*)


(*\:8fd0\:52a8\:5b66\:5173\:7cfb\:ff0c\:6807\:91cffix*)
SP[p1,p1]=mN^2;SP[p2,p2]=mN^2;SP[p1,p2]=Q2/2+mN^2;
SP[p1,q]=Q2/2;SP[p2,q]=-Q2/2;


(*\:628a\:81ea\:7531\:6307\:6807\:7684\:52a8\:91cf\:90fd\:66ff\:6362\:6210 plus \:5206\:91cf*)
RulePlusMom=FCI[{
MT[\[Mu]_,\[Nu]_]->0,(*\:5ea6\:89c4\:7684++\:5206\:91cf\:4e3a\:96f6*)
FV[k,\[Mu]_]->k[1],
FV[p1,\[Mu]_]->p1[1],
FV[p2,\[Mu]_]->p2[1],
FV[q,\[Mu]_]->q[1]
}];


(*\:6295\:5f71\:7b97\:7b26,\:6784\:9020fa,fb,\:518d\:7ebf\:6027\:7ec4\:5408\:6210f1,f2*)
projector["FA",\[Nu]_]:=(GS[p1]+mN) . (GA[\[Nu]]) . (GS[p2]+mN)
projector["FB",\[Nu]_]:=(FV[p1+p2,\[Nu]]/(2mN)) . (GS[p1]+mN) . (GS[p2]+mN)
(*\:5bf9\:632f\:5e45\:6c42 Trace,\:6295\:5f71\:51fa\:7ed3\:6784\:51fd\:6570fa,fb*)
traceExpr[expr_]:=(DiracSimplify[DiracTrace[expr]]/.RulePlusMom)
(*\:5c06\:6295\:5f71\:7b97\:5b50\:4f5c\:7528\:5230\:632f\:5e45\:4e0a,\:5e76\:5316\:7b80\:5230\:6807\:91cf\:8868\:8fbe\:5f0f; \:5f97\:5230\:632f\:5e45\:7684\:4e24\:4e2a\:6295\:5f71fa,fb*)
projToFaFb[Amp\[Mu]_,\[Nu]_]:=<|
"FA"->traceExpr[Amp\[Mu] . projector["FA",\[Nu]]],
"FB"->traceExpr[Amp\[Mu] . projector["FB",\[Nu]]]
|>


(*\:5c06fa,fb\:4f5c\:7ebf\:6027\:7ec4\:5408,\:91cd\:65b0\:7ec4\:6210f1,f2*)
FAFBToF1F2[{fa_,fb_}]:=Module[
(*\:5e38\:89c1\:7684plus\:52a8\:91cf\:7ec4\:5408*)
{pSum=p1[1]+p2[1],pMin=p1[1]-p2[1],pTimes=p1[1]*p2[1]},
(*-----*)
{
(-fa*Q2*pSum^2+4fb*mN^2*pMin^2)/(8pSum^2(mN^2*pMin^2-pTimes*Q2)),
(-fa*mN^2*pSum^2+4fb*pTimes)/(2pSum^2(mN^2*pMin^2-pTimes*Q2))
}]
(*fa,fb \:4f5c\:7ec4\:5408\:6210f1,f2 --------------------*)
projToF1F2[assoc_]:=AssociationThread[
Keys@assoc->FAFBToF1F2@Values@assoc]


(* ::Section:: *)
(*show propagator type*)


(*fadTmp3 \:8fd9\:4e2a\:51fd\:6570\:7528\:6765\:6446\:8131\:7cfb\:6570,\:53ea\:5c55\:793a\:5404\:79cd\:4f20\:64ad\:5b50\:7ed3\:6784------------------*)
Times[coe__,fadTmp3[expr__]]^:=fadTmp3[expr]
(*\:4f20\:64ad\:5b50\:7684\:6392\:7248,\:5b9a\:4e49\:6613\:8bfb\:7684\:6837\:5f0f------------------------------------*)
Format[fadTmp3[rules__Rule],TraditionalForm]:=Times@@KeyValueMap[Power,KeyMap[\[DoubleStruckCapitalD],Association[rules]]];
(*\:66ff\:6362\:5b8c\:6574\:8868\:8fbe\:5f0f\:4e2d\:7684 fadTmp1 \:4e3a fadTmp3 --------------------*)
gatorShow[expr_]:=(expr/.fadTmp1->fadTmp3)


(* ::Chapter:: *)
(*split octet*)


SetOptions[Simplify,TimeConstraint->1];


(* ::Section:: *)
(*a*)


(* ::Text:: *)
(*\:4ecb\:5b50\:5f69\:8679\:56fe*)


(*\:56fe\:7684\:7f16\:53f7;\:4e2d\:95f4\:6001\:91cd\:5b50\:7684\:8d28\:91cf*)
fig="a";mo1;mo2;
(*\:989d\:5916\:56e0\:5b50*)
preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;
(*-\:5708\:79ef\:5206\:7684\:5206\:6bcd\:90e8\:5206\:ff0c\:5373\:4f20\:64ad\:5b50-----------------------*)
splt[{fig,"cls"}]=preFactor*fad[
{k+q,\[CapitalLambda]}->-2,{k,\[CapitalLambda]}->-2,{k+q,mm1}->-1,{k,mm1}->-1,{p1-k,mo1}->-1
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fig,"spr",\[Mu]}]=GS[k+q] . GA5 . (FV[2k+q,\[Mu]]) . (GS[p1-k]+mo1) . GA5 . GS[k];
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fig,"FAFB","spr"}]=projToFaFb[splt[{fig,"spr",\[Mu]}],\[Nu]];


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2,(*\:5bf9k.k\:4f5c\:66ff\:6362*)
SP[k,q]->1/2(fad[{k+q,\[CapitalLambda]}->1]-fad[{k,\[CapitalLambda]}->1]+Q2),
SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{p1-k,mo1}->1]+\[CapitalLambda]^2-mo1^2+mN^2),(*\:5bf9k.p1\:4f5c\:66ff\:6362*)
SP[k,p2]->1/2(fad[{k+q,\[CapitalLambda]}->1]+Q2-fad[{p1-k,mo1}->1]+\[CapitalLambda]^2-mo1^2+mN^2)
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
splt[{fig,"FAFB"}]=Collect[
Expand[(splt[{fig,"cls"}])*(splt[{fig,"FAFB","spr"}]/.ruleScalar)],
fadTmp1[__],Simplify];


(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)
splt[{fig,"FAFB"}][[1]]//gatorShow


splt[{fig,"FAFB","lcone"}]=Query[(*<FA\[Rule]val..>*)All
(*FA\[Rule]val*),ReplaceAll[fadTmp1->fadTmp2]
]@splt[{fig,"FAFB"}];


Monitor[
splt[fig,"FAFB","res"]=Table[
propaConfs[[res]]->Residue[
splt[{fig,"FAFB","lcone"}][[ff]],
{k[2],gatorZeros["k-"][[res]]}
]
,{res,{1,2,5}}
,{ff,1,2,1}
]
,{res,ff}(*\:76d1\:89c6\:8fd0\:884c\:8fc7\:7a0b*)
];//AbsoluteTiming
splt[fig,"FAFB","res"]//Dimensions


fig="a";
splt[{fig,"f1f2","res"}]=projToF1F2/@splt[{fig,"FAFB","res"}];
splt[{fig,"f1f2","res"}]//Dimensions


(* ::Input:: *)
(*(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)*)
(*splt[if,"f1f2","anl","0<y<\[Zeta]"]=(-2\[Pi]*I)(splt[if,"f1f2","res"][[1,All]]+splt[if,"f1f2","res"][[2,All]]);*)
(*splt[if,"f1f2","anl","\[Zeta]<y<1"]=(2\[Pi]*I)(splt[if,"f1f2","res"][[5,All]]);*)


(* ::Input:: *)
(*if="a";$outDir=FileNameJoin[{$srcRoot,"/mfiles/"}]*)
(*Export[FileNameJoin[{$outDir,"residue_f1f2_"<>if<>".m"}],splt[if,"f1f2","res"]](*\:5bfc\:51fa\:5230\:786c\:76d8*)*)


(* ::Section:: *)
(*b*)


(* ::Text:: *)
(*\:91cd\:5b50\:5f69\:8679\:56fe*)


prfactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;if="b";(*\:56fe\:7684\:7f16\:53f7*)mo1=MB;mo2=MB;(*\:4e2d\:95f4\:6001\:91cd\:5b50\:7684\:8d28\:91cf*)
splt[if,"cls"]=(prfactor*fad[
{k,\[CapitalLambda]}->-4,{k,mm1}->-1,{p1-k,mo1}->-1,{p2-k,mo2}->-1
]
)


if="b";
\[CapitalGamma][if,"spr",\[Mu]]:=GS[k+q] . GA5 . (GS[p2-k]+mo2) . GA[\[Mu]] . (GS[p1-k]+mo1) . GS[k] . GA5


if="b";
splt[if,"FAFB","spr"]=toFaFb[\[CapitalGamma][if,"spr",\[Mu]],\[Nu]];(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf*)


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00,\:5bf9\:4e8e\:4e0d\:540c\:7684\:56fe\:8981\:8c03\:6574*)
scalars:=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2,(*\:5bf9k.k\:4f5c\:66ff\:6362*)
SP[k,q]->1/2(fad[{p1-k,mo1}->1]-fad[{p2-k,mo2}->1]),
SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{p1-k,mo1}->1]+\[CapitalLambda]^2-mo1^2+mN^2),(*\:5bf9k.p1\:4f5c\:66ff\:6362*)
SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{p2-k,mo2}->1]+\[CapitalLambda]^2-mo2^2+mN^2)
}]


if="b";
splt[if,"FAFB"]=Collect[
Expand[(splt[if,"cls"])*(splt[if,"FAFB","spr"]/.scalars)],(*\:6700\:7ec8\:7ed3\:679c*)
fadTmp1[__](*\:6574\:7406\:4e00\:4e0b*)
,Simplify];


if="b";
splt[if, "FAFB"][[1]]//gatorShow(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)


if="b";
splt[if,"FAFB","lcc"]=Simplify[splt[if,"FAFB"]/.fadTmp1->fadTmp2];(*\:7ed9\:51fa\:5149\:9525\:5750\:6807\:4e0b\:7684\:5f62\:5f0f*)


if="b";
Monitor[
splt[if,"FAFB","res"]=Table[
propaConfs[[res]]->Residue[splt[if,"FAFB","lcc"][[ff]],
{km,gatorZs[[res]]}
]
,{res,{5,6}}
,{ff,1,2,1}
]
,{res,ff}(*\:76d1\:89c6\:8fd0\:884c\:8fc7\:7a0b*)
];//AbsoluteTiming
splt[if,"FAFB","res"]//Dimensions


if="b";
splt[if,"f1f2","res"]=toF1F2/@splt[if,"FAFB","res"];
splt[if,"f1f2","res"]//Dimensions


(* ::Input:: *)
(*if="b";$outDir=FileNameJoin[{$srcRoot,"/mfiles/"}]*)
(*Export[FileNameJoin[{$outDir,"residue_f1f2_"<>if<>".m"}],splt[if,"f1f2","res"]](*\:5bfc\:51fa\:5230\:786c\:76d8*)*)


(* ::Section:: *)
(*c*)


(* ::Text:: *)
(*\:91cd\:5b50\:78c1\:77e9\:5f69\:8679\:56fe*)


(* ::Section:: *)
(*d*)


(* ::Text:: *)
(*KR\:56fe\:53f3*)


prfactor=(-I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;if="d";(*\:56fe\:7684\:7f16\:53f7*)mo1=MB;mo2=MB;(*\:4e2d\:95f4\:6001\:91cd\:5b50\:7684\:8d28\:91cf*)
splt[if,"cls"]=(prfactor*fad[
{k+q,\[CapitalLambda]}->-2,{k,\[CapitalLambda]}->-2,{k,mm1}->-1,{p1-k,mo1}->-1
]
)


if="d";
\[CapitalGamma][if,"spr",\[Mu]]:=GA[\[Mu]] . GA5 . (GS[p1-k]+mo1) . GS[k] . GA5


if="d";
splt[if,"FAFB","spr"]=toFaFb[\[CapitalGamma][if,"spr",\[Mu]],\[Nu]];(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf*)


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
scalars:=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2,(*\:5bf9k.k\:4f5c\:66ff\:6362*)
SP[k,q]->1/2(fad[{k+q,\[CapitalLambda]}->1]-fad[{k,\[CapitalLambda]}->1]+Q2),
SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{p1-k,mo1}->1]+\[CapitalLambda]^2-mo1^2+mN^2),(*\:5bf9k.p1\:4f5c\:66ff\:6362*)
SP[k,p2]->1/2(fad[{k+q,\[CapitalLambda]}->1]+Q2-fad[{p1-k,mo1}->1]+\[CapitalLambda]^2-mo1^2+mN^2)
}]


if="d";
splt[if,"FAFB"]=Collect[
Expand[(splt[if,"cls"])*(splt[if,"FAFB","spr"]/.scalars)],(*\:6700\:7ec8\:7ed3\:679c*)
fadTmp1[__],(*\:6574\:7406\:4e00\:4e0b*)
Simplify
];


if="d";
splt[if, "FAFB"][[1]]//gatorShow(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)


if="d";
splt[if,"FAFB","lcc"]=splt[if,"FAFB"]/.fadTmp1->fadTmp2;


if="d";
Monitor[
splt[if,"FAFB","res"]=Table[
propaConfs[[res]]->Residue[splt[if,"FAFB","lcc"][[ff]],
{km,gatorZs[[res]]}
]
,{res,{1,2,5}}
,{ff,1,2,1}
]
,{res,ff}(*\:76d1\:89c6\:8fd0\:884c\:8fc7\:7a0b*)
];//AbsoluteTiming
splt[if,"FAFB","res"]//Dimensions


if="d";
splt[if,"f1f2","res"]=toF1F2/@splt[if,"FAFB","res"];
splt[if,"f1f2","res"]//Dimensions


(* ::Input:: *)
(*if="d";$outDir=FileNameJoin[{$srcRoot,"/mfiles/"}];*)
(*Export[FileNameJoin[{$outDir,"residue_f1f2_"<>if<>".m"}],splt[if,"f1f2","res"]](*\:5bfc\:51fa\:5230\:786c\:76d8*)*)


(* ::Section:: *)
(*e*)


(* ::Text:: *)
(*KR\:56fe\:5de6*)


prfactor=(-I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;if="e";(*\:56fe\:7684\:7f16\:53f7*)mo1=MB;mo2=MB;(*\:4e2d\:95f4\:6001\:91cd\:5b50\:7684\:8d28\:91cf*)
splt[if,"cls"]=(prfactor*fad[
{k-q,\[CapitalLambda]}->-2,{k,\[CapitalLambda]}->-2,{k,mm1}->-1,{p2-k,mo2}->-1
]
)


if="e";
\[CapitalGamma][if,"spr",\[Mu]]:=GS[k] . GA5 . (GS[p2-k]+mo2) . GA[\[Mu]] . GA5


if="e";
splt[if,"FAFB","spr"]=toFaFb[\[CapitalGamma][if,"spr",\[Mu]],\[Nu]];(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf*)


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00,\:5bf9\:4e8e\:4e0d\:540c\:7684\:56fe\:8981\:8c03\:6574*)
scalars:=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2,(*\:5bf9k.k\:4f5c\:66ff\:6362*)
SP[k,q]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{k-q,\[CapitalLambda]}->1]+Q2),
SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{p2-k,mo2}->1]+\[CapitalLambda]^2-mo2^2+mN^2),
SP[k,p1]->1/2(fad[{k-q,\[CapitalLambda]}->1]-Q2-fad[{p2-k,mo2}->1]+\[CapitalLambda]^2-mo2^2+mN^2)(*\:5bf9k.p1\:4f5c\:66ff\:6362*)
}]


if="e";
splt[if,"FAFB"]=Collect[
Expand[(splt[if,"cls"])*(splt[if,"FAFB","spr"]/.scalars)],(*\:6700\:7ec8\:7ed3\:679c*)
fadTmp1[__],(*\:6574\:7406\:4e00\:4e0b*)
Simplify
];


if="e";
splt[if, "FAFB"][[1]]//gatorShow(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)


if="e";
splt[if,"FAFB","lcc"]=splt[if,"FAFB"]/.fadTmp1->fadTmp2;


if="e";
Monitor[
splt[if,"FAFB","res"]=Table[
propaConfs[[res]]->Residue[splt[if,"FAFB","lcc"][[ff]],
{km,gatorZs[[res]]}
]
,{res,{6}}
,{ff,1,2,1}
]
,{res,ff}(*\:76d1\:89c6\:8fd0\:884c\:8fc7\:7a0b*)
];//AbsoluteTiming
splt[if,"FAFB","res"]//Dimensions


if="e";
splt[if,"f1f2","res"]=toF1F2/@splt[if,"FAFB","res"];
splt[if,"f1f2","res"]//Dimensions


(* ::Input:: *)
(*$outDir=FileNameJoin[{$srcRoot,"/mfiles/"}]*)
(*Export[FileNameJoin[{$outDir,"residue_f1f2_"<>if<>".m"}],splt[if,"f1f2","res"]](*\:5bfc\:51fa\:5230\:786c\:76d8*)*)


(* ::Section:: *)
(*f*)


(* ::Text:: *)
(*\:89c4\:8303\:94fe\:63a5KR\:56fe\:70b9\:5728\:53f3*)


prfactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;if="f";(*\:56fe\:7684\:7f16\:53f7*)mo1=MB;mo2=MB;(*\:4e2d\:95f4\:6001\:91cd\:5b50\:7684\:8d28\:91cf*)
splt[if,"cls"]=Expand[(prfactor*fad[{k,\[CapitalLambda]}->-4,{k+q,\[CapitalLambda]}->-2,{k,mm1}->-1,{p1-k,mo1}->-1])*(fad[{k,\[CapitalLambda]}->1]+fad[{k+q,\[CapitalLambda]}->1])]


if="f";
\[CapitalGamma][if,"spr",\[Mu]]:=FV[2k+q,\[Mu]] . GS[k] . GA5 . (GS[p1-k]+mo1) . GS[k] . GA5


if="f";
splt[if,"FAFB","spr"]=toFaFb[\[CapitalGamma][if,"spr",\[Mu]],\[Nu]];(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf*)


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
scalars:=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2,(*\:5bf9k.k\:4f5c\:66ff\:6362*)
SP[k,q]->1/2(fad[{k+q,\[CapitalLambda]}->1]-fad[{k,\[CapitalLambda]}->1]+Q2),
SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{p1-k,mo1}->1]+\[CapitalLambda]^2-mo1^2+mN^2),(*\:5bf9k.p1\:4f5c\:66ff\:6362*)
SP[k,p2]->1/2(fad[{k+q,\[CapitalLambda]}->1]+Q2-fad[{p1-k,mo1}->1]+\[CapitalLambda]^2-mo1^2+mN^2)
}]


if="f";
splt[if,"FAFB"]=Collect[
Expand[(splt[if,"cls"])*(splt[if,"FAFB","spr"]/.scalars)],(*\:6700\:7ec8\:7ed3\:679c*)
fadTmp1[__],(*\:6574\:7406\:4e00\:4e0b*)
Simplify
];


if="f";
splt[if, "FAFB"][[1]]//gatorShow(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)


if="f";
splt[if,"FAFB","lcc"]=splt[if,"FAFB"]/.fadTmp1->fadTmp2;


if="f";
Monitor[
splt[if,"FAFB","res"]=Table[
propaConfs[[res]]->Residue[splt[if,"FAFB","lcc"][[ff]],
{km,gatorZs[[res]]}
]
,{res,{5,7}}
,{ff,1,2,1}
]
,{res,ff}(*\:76d1\:89c6\:8fd0\:884c\:8fc7\:7a0b*)
];//AbsoluteTiming
splt[if,"FAFB","res"]//Dimensions


if="f";
splt[if,"f1f2","res"]=toF1F2/@splt[if,"FAFB","res"];
splt[if,"f1f2","res"]//Dimensions


(* ::Input:: *)
(*$outDir=FileNameJoin[{$srcRoot,"/mfiles/"}]*)
(*Export[FileNameJoin[{$outDir,"residue_f1f2_"<>if<>".m"}],splt[if,"f1f2","res"]](*\:5bfc\:51fa\:5230\:786c\:76d8*)*)


(* ::Section:: *)
(*g*)


(* ::Text:: *)
(*\:89c4\:8303\:94fe\:63a5KR\:56fe\:70b9\:5728\:5de6*)


prfactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;if="g";(*\:56fe\:7684\:7f16\:53f7*)mo1=MB;mo2=MB;(*\:4e2d\:95f4\:6001\:91cd\:5b50\:7684\:8d28\:91cf*)
splt[if,"cls"]=Expand[(prfactor*fad[{k,\[CapitalLambda]}->-4,{k-q,\[CapitalLambda]}->-2,{k,mm1}->-1,{p2-k,mo2}->-1])*(fad[{k,\[CapitalLambda]}->1]+fad[{k-q,\[CapitalLambda]}->1])]


if="g";
\[CapitalGamma][if,"spr",\[Mu]]:=FV[2k-q,\[Mu]] . GS[k] . GA5 . (GS[p2-k]+mo2) . GS[k] . GA5


if="g";
splt[if,"FAFB","spr"]=toFaFb[\[CapitalGamma][if,"spr",\[Mu]],\[Nu]];(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf*)


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00,\:5bf9\:4e8e\:4e0d\:540c\:7684\:56fe\:8981\:8c03\:6574*)
scalars:=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2,(*\:5bf9k.k\:4f5c\:66ff\:6362*)
SP[k,q]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{k-q,\[CapitalLambda]}->1]+Q2),
SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{p2-k,mo2}->1]+\[CapitalLambda]^2-mo2^2+mN^2),
SP[k,p1]->1/2(fad[{k-q,\[CapitalLambda]}->1]-Q2-fad[{p2-k,mo2}->1]+\[CapitalLambda]^2-mo2^2+mN^2)(*\:5bf9k.p1\:4f5c\:66ff\:6362*)
}]


if="g";
splt[if,"FAFB"]=Collect[
Expand[(splt[if,"cls"])*(splt[if,"FAFB","spr"]/.scalars)],(*\:6700\:7ec8\:7ed3\:679c*)
fadTmp1[__],(*\:6574\:7406\:4e00\:4e0b*)
Simplify
];


if="g";
splt[if, "FAFB"][[1]]//gatorShow(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)


if="g";
splt[if,"FAFB","lcc"]=splt[if,"FAFB"]/.fadTmp1->fadTmp2;


if="g";
Monitor[
splt[if,"FAFB","res"]=Table[
propaConfs[[res]]->Residue[splt[if,"FAFB","lcc"][[ff]],
{km,gatorZs[[res]]}
]
,{res,{6}}
,{ff,1,2,1}
]
,{res,ff}(*\:76d1\:89c6\:8fd0\:884c\:8fc7\:7a0b*)
];//AbsoluteTiming
splt[if,"FAFB","res"]//Dimensions


if="g";
splt[if,"f1f2","res"]=toF1F2/@splt[if,"FAFB","res"];
splt[if,"f1f2","res"]//Dimensions


(* ::Input:: *)
(*$outDir=FileNameJoin[{$srcRoot,"/mfiles/"}]*)
(*Export[FileNameJoin[{$outDir,"residue_f1f2_"<>if<>".m"}],splt[if,"f1f2","res"]](*\:5bfc\:51fa\:5230\:786c\:76d8*)*)


(* ::Section:: *)
(*h*)


(* ::Text:: *)
(*tadpole,*)


prfactor=(-I*C\[Phi]\[Phi])/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;if="h";(*\:56fe\:7684\:7f16\:53f7*)mo1=MB;mo2=MB;(*\:4e2d\:95f4\:6001\:91cd\:5b50\:7684\:8d28\:91cf*)
splt[if,"cls"]=(prfactor*fad[{k,\[CapitalLambda]}->-4,{k,mm1}->-1])


if="h";
\[CapitalGamma][if,"spr",\[Mu]]:=GA[\[Mu]]


if="h";
splt[if,"FAFB","spr"]=toFaFb[\[CapitalGamma][if,"spr",\[Mu]],\[Nu]];(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf*)


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
scalars:=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2,(*\:5bf9k.k\:4f5c\:66ff\:6362*)
SP[k,q]->1/2(fad[{k+q,\[CapitalLambda]}->1]-fad[{k,\[CapitalLambda]}->1]+Q2),
SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{p1-k,mo1}->1]+\[CapitalLambda]^2-mo1^2+mN^2),(*\:5bf9k.p1\:4f5c\:66ff\:6362*)
SP[k,p2]->1/2(fad[{k+q,\[CapitalLambda]}->1]+Q2-fad[{p1-k,mo1}->1]+\[CapitalLambda]^2-mo1^2+mN^2)
}]


if="h";
splt[if,"FAFB"]=Collect[
Expand[(splt[if,"cls"])*(splt[if,"FAFB","spr"]/.scalars)],(*\:6700\:7ec8\:7ed3\:679c*)
fadTmp1[__],(*\:6574\:7406\:4e00\:4e0b*)
Simplify
];


if="h";
splt[if, "FAFB"][[1]]//gatorShow(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)


if="h";
splt[if,"FAFB","lcc"]=splt[if,"FAFB"]/.fadTmp1->fadTmp2;


if="h";
Monitor[
splt[if,"FAFB","res"]=Table[
propaConfs[[res]]->Residue[splt[if,"FAFB","lcc"][[ff]],
{km,gatorZs[[res]]}
]
,{res,{1,2}}
,{ff,1,2,1}
]
,{res,ff}(*\:76d1\:89c6\:8fd0\:884c\:8fc7\:7a0b*)
];//AbsoluteTiming
splt[if,"FAFB","res"]//Dimensions


if="h";
splt[if,"f1f2","res"]=toF1F2/@splt[if,"FAFB","res"];
splt[if,"f1f2","res"]//Dimensions


(* ::Input:: *)
(*if="h";$outDir=FileNameJoin[{$srcRoot,"/mfiles/"}]*)
(*Export[FileNameJoin[{$outDir,"residue_f1f2_"<>if<>".m"}],splt[if,"f1f2","res"]](*\:5bfc\:51fa\:5230\:786c\:76d8*)*)


(* ::Section:: *)
(*i*)


(* ::Text:: *)
(*tadpole \:89c4\:8303\:94fe\:63a5*)


prfactor=(I*C\[Phi]\[Phi])/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;if="i";(*\:56fe\:7684\:7f16\:53f7*)mo1=MB;mo2=MB;(*\:4e2d\:95f4\:6001\:91cd\:5b50\:7684\:8d28\:91cf*)
splt[if,"cls"]=prfactor*fad[{k+q,\[CapitalLambda]}->-2,{k,\[CapitalLambda]}->-4,{k,mm1}->-1]*(fad[{k,\[CapitalLambda]}->1]+fad[{k+q,\[CapitalLambda]}->1])*(FV[2k+q,\[Mu]])*2


if="i";
\[CapitalGamma][if,"spr",\[Mu]]:=GS[k]


if="i";
splt[if,"FAFB","spr"]=toFaFb[\[CapitalGamma][if,"spr",\[Mu]],\[Nu]];(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf*)


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
scalars:=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2,(*\:5bf9k.k\:4f5c\:66ff\:6362*)
SP[k,q]->1/2(fad[{k+q,\[CapitalLambda]}->1]-fad[{k,\[CapitalLambda]}->1]+Q2),
SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{p1-k,mo1}->1]+\[CapitalLambda]^2-mo1^2+mN^2),(*\:5bf9k.p1\:4f5c\:66ff\:6362*)
SP[k,p2]->1/2(fad[{k+q,\[CapitalLambda]}->1]+Q2-fad[{p1-k,mo1}->1]+\[CapitalLambda]^2-mo1^2+mN^2)
}]


if="i";
splt[if,"FAFB"]=Collect[
Expand[(splt[if,"cls"])*(splt[if,"FAFB","spr"]/.scalars)],(*\:6700\:7ec8\:7ed3\:679c*)
fadTmp1[__],(*\:6574\:7406\:4e00\:4e0b*)
Simplify
];


if="i";
splt[if, "FAFB"][[1]]//gatorShow(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)


if="i";
splt[if,"FAFB","lcc"]=splt[if,"FAFB"]/.fadTmp1->fadTmp2;


if="i";
Monitor[
splt[if,"FAFB","res"]=Table[
propaConfs[[res]]->Residue[splt[if,"FAFB","lcc"][[ff]],
{km,gatorZs[[res]]}
]
,{res,{7}}
,{ff,1,2,1}
]
,{res,ff}(*\:76d1\:89c6\:8fd0\:884c\:8fc7\:7a0b*)
];//AbsoluteTiming
splt[if,"FAFB","res"]//Dimensions


if="i";
splt[if,"f1f2","res"]=toF1F2/@splt[if,"FAFB","res"];
splt[if,"f1f2","res"]//Dimensions


(* ::Input:: *)
(*if="i";$outDir=FileNameJoin[{$srcRoot,"/mfiles/"}]*)
(*Export[FileNameJoin[{$outDir,"residue_f1f2_"<>if<>".m"}],splt[if,"f1f2","res"]](*\:5bfc\:51fa\:5230\:786c\:76d8*)*)


(* ::Section:: *)
(*j*)


(* ::Text:: *)
(*bubble \:56fe*)


prfactor=(I*C\[Phi]\[Phi])/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;if="j";(*\:56fe\:7684\:7f16\:53f7*)mo1=MB;mo2=MB;(*\:4e2d\:95f4\:6001\:91cd\:5b50\:7684\:8d28\:91cf*)
splt[if,"cls"]=(prfactor*fad[
{k,\[CapitalLambda]}->-4,{k,mm1}->-1,{k+q,mm1}->-1
])


if="j";
\[CapitalGamma][if,"spr",\[Mu]]:=FV[2k+q,\[Mu]] . GS[k]


if="j";
splt[if,"FAFB","spr"]=toFaFb[\[CapitalGamma][if,"spr",\[Mu]],\[Nu]];(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf*)


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
scalars:=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2,(*\:5bf9k.k\:4f5c\:66ff\:6362*)
SP[k,q]->1/2(fad[{k+q,\[CapitalLambda]}->1]-fad[{k,\[CapitalLambda]}->1]+Q2),
SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{p1-k,mo1}->1]+\[CapitalLambda]^2-mo1^2+mN^2),(*\:5bf9k.p1\:4f5c\:66ff\:6362*)
SP[k,p2]->1/2(fad[{k+q,\[CapitalLambda]}->1]+Q2-fad[{p1-k,mo1}->1]+\[CapitalLambda]^2-mo1^2+mN^2)
}]


if="j";
splt[if,"FAFB"]=Collect[
Expand[(splt[if,"cls"])*(splt[if,"FAFB","spr"]/.scalars)],(*\:6700\:7ec8\:7ed3\:679c*)
fadTmp1[__],(*\:6574\:7406\:4e00\:4e0b*)
Simplify
];


if="j";
splt[if, "FAFB"][[1]]//gatorShow(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)


if="j";
splt[if,"FAFB","lcc"]=splt[if,"FAFB"]/.fadTmp1->fadTmp2;


if="j";
Monitor[
splt[if,"FAFB","res"]=Table[
propaConfs[[res]]->Residue[splt[if,"FAFB","lcc"][[ff]],
{km,gatorZs[[res]]}
]
,{res,{1,2}}
,{ff,1,2,1}
]
,{res,ff}(*\:76d1\:89c6\:8fd0\:884c\:8fc7\:7a0b*)
];//AbsoluteTiming
splt[if,"FAFB","res"]//Dimensions


if="j";
splt[if,"f1f2","res"]=toF1F2/@splt[if,"FAFB","res"];
splt[if,"f1f2","res"]//Dimensions


(* ::Input:: *)
(*if="j";$outDir=FileNameJoin[{$srcRoot,"/mfiles/"}]*)
(*Export[FileNameJoin[{$outDir,"residue_f1f2_"<>if<>".m"}],splt[if,"f1f2","res"]](*\:5bfc\:51fa\:5230\:786c\:76d8*)*)


(* ::Section:: *)
(*k*)


(* ::Text:: *)
(*tadpole \:78c1\:77e9*)


(* ::Section:: *)
(*l*)


(* ::Text:: *)
(*\:9ad8\:9636 bubble*)


(* ::Chapter::Closed:: *)
(*split decuplet*)


(* ::Section:: *)
(*m*)


(* ::Section:: *)
(*n*)


(* ::Section:: *)
(*o*)


(* ::Section:: *)
(*p*)


(* ::Section:: *)
(*q*)


(* ::Section:: *)
(*r*)


(* ::Section:: *)
(*s*)


(* ::Section:: *)
(*t*)


(* ::Section:: *)
(*u*)
