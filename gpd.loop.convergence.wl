(* ::Package:: *)

(* ::Title:: *)
(*gpd.loop.convergence.wl*)


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
(*<<modules*)


(*\:5bfc\:5165\:65cb\:91cf\:8ba1\:7b97\:7a0b\:5e8f--------------*)
Once[<<"FeynCalc`"]


(*\:67e5\:770b FeynCalc External,Interal \:7684\:6807\:51c6\:5f62\:5f0f------------------*)
fceStd[x_]:=x//FCE//StandardForm;
fciStd[x_]:=x//FCI//StandardForm;


Get["gpd.interface.wl"];
(*\:8d39\:66fc\:56fe\:7684\:540d\:79f0\:548c\:6837\:5f0f---------------------*)
Get["gen.integral-TagList.wl"];


(* ::Section:: *)
(*symbols*)


(*\:516b\:91cd\:6001\:90e8\:5206\:547d\:540d------------*)
ClearAll[
(*\:4ecb\:5b50\:8870\:53d8\:5e38\:6570*)
f
(*\:521d\:672b\:6001\:6838\:5b50\:8d28\:91cf;\:4ecb\:5b50\:8d28\:91cf;\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf*)
,mN,mm1,mo1,mo2
(*MB-mN; mN+MB*)
,\[CapitalDelta],mSum
];


(* ::Section:: *)
(*\:4f20\:64ad\:5b50\:7ed3\:6784*)


mergeRule::usage="\:5bf9\:4e8e\:8f93\:5165\:7684\:4f20\:64ad\:5b50\:8bb0\:5f55\:ff0c\:4f8b\:5982:{k,\[CapitalLambda]}->1,{k,\[CapitalLambda]}->1, \:8fdb\:884c\:5408\:5e76,\:5c06\:91cd\:590d\:7684\:952e\:503c\:6c42\:548c\:ff0c\:5b58\:5165\:5217\:8868";
mergeRule[rules:__Association|__Rule]:=Normal@Merge[{rules},Total];


(* \:4f20\:64ad\:5b50\[Rule]\:5e42\:6b21 \:7684\:8bb0\:5f55, \:4e0d\:4f9d\:8d56\:4e8e\:53c2\:6570\:6b21\:5e8f ------------------------------*)
SetAttributes[{fadTmp1,fadTmp2},Orderless];
fadTmp1::usage="\:8bb0\:5f55\:4f20\:64ad\:5b50\:7ed3\:6784\:ff0c\:81ea\:52a8\:5408\:5e76\:4e24\:4e2a\:76f8\:4e58\:7684\:4f20\:64ad\:5b50\:51fd\:6570";
(*\:6574\:7406\:4f20\:64ad\:5b50, \:5408\:5e76\:4e24\:4e2a\:4f20\:64ad\:5b50\:8bb0\:5f55 ----------------------------*)
fadTmp1[ruleA:__Rule]*fadTmp1[ruleB:__Rule]^:=fadTmp1@@mergeRule[ruleA,ruleB];
(* \:5c06\:4f20\:64ad\:5b50\:8bb0\:5f55\:7684\:5e42\:6b21,\:6574\:5408\:5230\:53c2\:6570\:4e2d, \:4f20\:64ad\:5b50\[Rule]n*\:5e42\:6b21 -------------------------*)
Power[fadTmp1[rules:__Rule],n_]^:=fadTmp1@@mergeRule[n*Association@{rules}] 
(*\:4f20\:64ad\:5b50\:7684\:6392\:7248\:ff0c\:5b9a\:4e49\:6613\:8bfb\:7684\:6837\:5f0f -----------------------*)
Format[fadTmp1[rules__Rule],TraditionalForm]:=\[DoubleStruckCapitalD][Values[{rules}]];


(*\:5c06\:4f20\:64ad\:5b50\:4ee5\:5173\:8054\:7684\:5f62\:5f0f\:4fdd\:5b58\:8d77\:6765 <{k+\[CapitalDelta],\[CapitalLambda]}\[Rule]-2...>*)
fad::usage="\:8bb0\:5f55\:4f20\:64ad\:5b50\:7ed3\:6784\:ff0c\:4f5c\:4e3a\:8f93\:5165\:63a5\:53e3";
fad[rules:__Rule]:=fadTmp1@@mergeRule[rules]; (*\:5c06\:91cd\:590d\:7684\:4f20\:64ad\:5b50\:7684\:5e42\:6b21,\:7d2f\:52a0\:8d77\:6765*)


(* ::Section:: *)
(*LightCone  Form*)


(*\:6700\:540e\:4e24\:4e2a\:5206\:91cf\:4f5c\:6b27\:51e0\:91cc\:5f97\:5185\:79ef;\:5185\:79ef\:4e0d\:4f9d\:8d56\:4e8e\:6b21\:5e8f*)
SetAttributes[ed,Orderless];
ed[p_+q__,l_]:=ed[p,l]+ed[q,l](*\:7ebf\:6027\:5f8b*)
ed[num_*p_,q_]:=num*ed[p,q]/;NumberQ[num](*\:5206\:914d\:5f8b*)


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
(*\:4f7f\:7528 *)
Expand[
(lcp[[1]]*lcq[[2]]+lcp[[2]]*lcq[[1]])-ed[lcp[[3]],lcq[[3]]]
]]


(*gpd \:4e2d\:4f7f\:7528\:7684\:5149\:9525\:52a8\:91cf\:53c2\:6570\:5316*)
lConeKinematics={
(*\:8d28\:5b50\:521d\:6001\:52a8\:91cf-------------------*)
p1[1]->(1+\[Xi])P[1],
p1[2]->(mN^2+ed[\[CapitalDelta][3],\[CapitalDelta][3]]/4)/(2(1+\[Xi])P[1]),
p1[3]->\[CapitalDelta][3]/2,
(*\:4ecb\:5b50\:521d\:6001\:52a8\:91cf*)
k[1]->(y+\[Xi])P[1],
(*k[2]\[Rule]k[2],*)
k[3]->k[3]+\[CapitalDelta][3]/2,
(*\:8d28\:5b50\:672b\:6001\:52a8\:91cf------------------*)
p2[1]->(1-\[Xi])P[1],
p2[2]->(mN^2+ed[\[CapitalDelta][3],\[CapitalDelta][3]]/4)/(2(1-\[Xi])P[1]),
p2[3]->-\[CapitalDelta][3]/2,
(*\:4ecb\:5b50\:672b\:6001\:52a8\:91cf*)
k2[1]->(y-\[Xi])P[1],
(*k2[2]\[Rule]k2[2],*)
k2[3]->k[3]-\[CapitalDelta][3]/2,

\[CapitalDelta][1]->\[Xi](2P[1]),
\[CapitalDelta][2]->(\[CapitalDelta]2+ed[\[CapitalDelta][3],\[CapitalDelta][3]])/(4\[Xi]*P[1])
(*\[CapitalDelta][3]\[Rule]\[CapitalDelta][3]*)
};


kTIntegralKinematics={
ed[\[CapitalDelta][3],\[CapitalDelta][3]]->(-4mN^2 \[Xi]^2+(1-\[Xi]^2)(-\[CapitalDelta]2)),
ed[k[3],\[CapitalDelta][3]]->0,
ed[k[3],k[3]]->ktr^2
};


(*\:4f20\:64ad\:5b50\:7684\:5149\:9525\:5f62\:5f0f--------------*)
LConegator[{k_,\[CapitalLambda]_}]:=lightConeSP[k,k]-\[CapitalLambda]^2;


(*++++++++++++++++++++++++++++++++++++++++++++*)
(*\:4f20\:64ad\:5b50\:53ef\:80fd\:914d\:7f6e\:7684\:5217\:8868*)
gatorConfigs={
{k,\[CapitalLambda]},{k,mm1},(*1,2*)
{k-\[CapitalDelta],\[CapitalLambda]},{k-\[CapitalDelta],mm1},(*3,4*)
{k+\[CapitalDelta],\[CapitalLambda]},{k+\[CapitalDelta],mm1},(*5,6*)
{p1-k,mo1},{p2-k,mo2},(*7,8*)
{p1-k,md1},{p2-k,md2}(*7,8*)
};
(*\:4f20\:64ad\:5b50\:5728\:5149\:9525\:5750\:6807\:4e0b\:7684\:5f62\:5f0f,\:5173\:8054*)
gatorAssoc=AssociationMap[LConegator,gatorConfigs];


fadTmp2::usage="fadTmp1\[Rule]fadTmp2,\:628a\:4f20\:64ad\:5b50\:8f6c\:6362\:5230\:5149\:9525\:5206\:91cf\:5f62\:5f0f\:ff0c\:4e5f\:5c31\:662f\:5177\:4f53\:5f62\:5f0f";
fadTmp2[rules__]:=Times@@KeyValueMap[
(*Power[propagator,num] \:663e\:793a\:8868\:793a\:51fa\:4f20\:64ad\:5b50\:7684\:5e42\:6b21*)
Power,
(*\:4ee3\:5165\:4f20\:64ad\:5b50\:7684\:5149\:9525\:5f62\:5f0f\:ff0ckey\[Rule]val,val \:662f\:4f20\:64ad\:5b50\:7684\:5e42\:6b21*)
KeyMap[LConegator,Association[rules]]]


(*k- \:5206\:91cf\:524d\:7684\:7cfb\:6570-------------------*)
kminusCoes=Query[(*<propagator\[Rule]LightCone exprs>*)All
(*LightCone exprs*)
,(Coefficient[#1,k[2],1]&)/*Simplify
]@gatorAssoc;
(*\:4f20\:64ad\:5b50\:5149\:9525\:5f62\:5f0f \[Equal]0 \:65f6, k- \:7684\:89e3-----------------*)
gatorZeros=Query[All,
List@@First@First@Solve[#==0,{k[2]}]&
]@gatorAssoc;


(*\:6c42\:51faFAFB\:8868\:8fbe\:5f0f\:ff0c\:5728 k- \:96f6\:70b9\:7684\:7559\:6570;Echo \:6253\:5370\:8fdb\:5ea6*)
FAFBResidue[point_,FAFB_]:=Residue[#,point//Echo]&/@FAFB


(*fadTmp3 \:8fd9\:4e2a\:51fd\:6570\:7528\:6765\:6446\:8131\:7cfb\:6570,\:53ea\:5c55\:793a\:5404\:79cd\:4f20\:64ad\:5b50\:7ed3\:6784------------------*)
Times[coe__,fadTmp3[expr__]]^:=fadTmp3[expr]
(*\:4f20\:64ad\:5b50\:7684\:6392\:7248,\:5b9a\:4e49\:6613\:8bfb\:7684\:6837\:5f0f------------------------------------*)
Format[fadTmp3[rules__Rule],TraditionalForm]:=Times@@KeyValueMap[
Power,
KeyMap[\[DoubleStruckCapitalD],Association[rules]]];
(*\:66ff\:6362\:5b8c\:6574\:8868\:8fbe\:5f0f\:4e2d\:7684 fadTmp1 \:4e3a fadTmp3 --------------------*)
gatorShow[expr_]:=(expr/.fadTmp1->fadTmp3)


(* ::Section:: *)
(*regulator*)


decGamma3::usage="decGamma3[\[Mu]_,\[Nu]_,\[Rho]_],\:5341\:91cd\:6001F1\:9876\:89d2\:4f3d\:9a6c\:77e9\:9635";
decGamma3[\[Mu]_,\[Nu]_,\[Rho]_]:=(-I)/2*(
DiracSigma[GA[\[Mu]],GA[\[Nu]]] . GA[\[Rho]]+GA[\[Rho]] . DiracSigma[GA[\[Mu]],GA[\[Nu]]])


\[CapitalTheta]::usage="\:5341\:91cd\:6001\:79bb\:58f3\:53c2\:6570\:6240\:5728\:7684 \[Gamma] \:77e9\:9635";
\[CapitalTheta][\[Mu]_,\[Nu]_]:=I*DiracSigma[GA[\[Mu]],GA[\[Nu]]]


decGator::usage="decGator[{\[Alpha]_,\[Beta]_},{p_,md_}],\:5341\:91cd\:6001\:4f20\:64ad\:5b50\:7684\:65cb\:91cf\:90e8\:5206";
decGator[{\[Alpha]_,\[Beta]_},{p_,md_}]:=(-(GS[p]+md) . (
MT[\[Alpha],\[Beta]]-1/3 GA[\[Alpha]] . GA[\[Beta]]-(GA[\[Alpha]] . FV[p,\[Beta]]-GA[\[Beta]] . FV[p,\[Alpha]])/(3md)-(2FV[p,\[Alpha]] . FV[p,\[Beta]])/(3md^2)))


(* ::Section::Closed:: *)
(*splitting function projectors*)


(*\:8fd0\:52a8\:5b66\:5173\:7cfb\:ff0c\:6807\:91cffix*)
SP[p1,p1]=mN^2;SP[p2,p2]=mN^2;
SP[p1,\[CapitalDelta]]=\[CapitalDelta]2/2;
SP[p2,\[CapitalDelta]]=-\[CapitalDelta]2/2;
SP[p1,p2]=mN^2-\[CapitalDelta]2/2;


(*\:628a\:81ea\:7531\:6307\:6807\:7684\:52a8\:91cf\:90fd\:66ff\:6362\:6210 plus \:5206\:91cf*)
RulePlusMom=FCI[{
MT[\[Mu]_,\[Nu]_]->0,(*\:5ea6\:89c4\:7684++\:5206\:91cf\:4e3a\:96f6*)
FV[k,\[Mu]_]->k[1],
FV[p1,\[Mu]_]->p1[1],
FV[p2,\[Mu]_]->p2[1],
FV[\[CapitalDelta],\[Mu]_]->\[CapitalDelta][1]
}];


(*\:6295\:5f71\:7b97\:7b26,\:6784\:9020fa,fb,\:518d\:7ebf\:6027\:7ec4\:5408\:6210f1,f2*)
projector["FA",\[Nu]_]:=(GS[p1]+mN) . (GA[\[Nu]]) . (GS[p2]+mN)
projector["FB",\[Nu]_]:=(FV[p1+p2,\[Nu]]/(2mN)) . (GS[p1]+mN) . (GS[p2]+mN)
(*\:5bf9\:632f\:5e45\:6c42 Trace,\:6295\:5f71\:51fa\:7ed3\:6784\:51fd\:6570fa,fb*)
traceExpr[expr_]:=(DiracSimplify[DiracTrace[expr]]/.RulePlusMom)
(*\:5c06\:6295\:5f71\:7b97\:5b50\:4f5c\:7528\:5230\:632f\:5e45\:4e0a,\:5e76\:5316\:7b80\:5230\:6807\:91cf\:8868\:8fbe\:5f0f; \:5f97\:5230\:632f\:5e45\:7684\:4e24\:4e2a\:6295\:5f71fa,fb*)
projToFAFB[Amp\[Mu]_,\[Nu]_]:={
traceExpr[Amp\[Mu] . projector["FA",\[Nu]]],
traceExpr[Amp\[Mu] . projector["FB",\[Nu]]]
}


(*\:5c06FA,FB\:4f5c\:7ebf\:6027\:7ec4\:5408,\:91cd\:65b0\:7ec4\:6210f1,f2---------------------*)
FAFBToF1F2[{FA_,FB_}]:=Module[
(*\:5e38\:89c1\:7684plus\:52a8\:91cf\:7ec4\:5408*)
{Pp=(p1[1]+p2[1])/2,\[CapitalDelta]p=p1[1]-p2[1],p12p=p1[1]*p2[1]},
(*-----*)
{
(FA*\[CapitalDelta]2*Pp^2+4FB*mN^2*\[CapitalDelta]p^2)/(8Pp^2(mN^2*\[CapitalDelta]p^2+p12p*\[CapitalDelta]2)),
(-FA*mN^2*Pp^2+FB*mN^2*p12p)/(2Pp^2(mN^2*\[CapitalDelta]p^2+p12p*\[CapitalDelta]2))
}]
(*FA,FB \:4f5c\:7ec4\:5408\:6210f1,f2,\:7559\:4f5c\:989d\:5916\:5904\:7406 --------------------*)
projToF1F2[FAFBList_]:=FAFBToF1F2@FAFBList


SetOptions[Simplify,TimeConstraint->1];


(* ::Chapter:: *)
(*Split function octet*)


(* ::Section:: *)
(*Rainbow,meson,octet*)


(* ::Input:: *)
(*diagIllus@chTag@{"RB","mes","oct"}*)


(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"RB","mes","oct"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*-\:5708\:79ef\:5206\:7684\:5206\:6bcd\:90e8\:5206\:ff0c\:5373\:4f20\:64ad\:5b50-----------------------*)
splt[{fyTag,"cls"}]=preFactor*fad[
{k-\[CapitalDelta],\[CapitalLambda]}->-2,{k,\[CapitalLambda]}->-2,{k-\[CapitalDelta],mm1}->-1,{k,mm1}->-1,{p1-k,mo1}->-1
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spr",\[Mu]}]=(FV[2k-\[CapitalDelta],\[Mu]] . GS[k-\[CapitalDelta]] . GA5 . (GS[p1-k]+mo1) . GS[k] . GA5);
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fyTag,"FAFB","spr"}]=projToFAFB[splt[{fyTag,"spr",\[Mu]}],\[Nu]];


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2,(*\:5bf9k.k\:4f5c\:66ff\:6362*)
SP[k,\[CapitalDelta]]->1/2(fad[{k,\[CapitalLambda]}->1]-(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2)),
SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2)),(*\:5bf9k.p1\:4f5c\:66ff\:6362*)
SP[k,p2]->1/2(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2))
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
splt[{fyTag,"FAFB","integ"}]=Collect[
Expand[
splt[{fyTag,"cls"}]*
(splt[{fyTag,"FAFB","spr"}]/.ruleScalar)
],
fadTmp1[__],Simplify];


(* ::Input:: *)
(*(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)*)
(*splt[{fyTag,"FAFB","integ"}]//First//gatorShow*)


splt[{fyTag,"FAFB","lcone"}]=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@splt[{fyTag,"FAFB","integ"}];


splt[{fyTag,"FAFB","res"}]=Query[
(*<propgator\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@{
{k,\[CapitalLambda]},{k,mm1},
{k-\[CapitalDelta],\[CapitalLambda]},{k-\[CapitalDelta],mm1},
{p1-k,mo1}
}
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570----------------*)
,FAFBResidue[#,splt[{fyTag,"FAFB","lcone"}]]&
]@gatorZeros;


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2 *)
splt[{fyTag,"F1F2","res","tmp1"}]=projToF1F2/@splt[{fyTag,"FAFB","res"}];
(*\:7ffb\:8f6c\:6b21\:5e8f----*)
splt[{fyTag,"F1F2","res"}]=Table[splt[{fyTag,"F1F2","res","tmp1"}][[All,ffs]],{ffs,2}];


(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)
splt[{fyTag,"F1F2","pw"}]=Query[(*{f1f2}*)All,
(*<propgator\[Rule]expr>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570--------------*)
Piecewise[{
{
(-2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1}}]],
-\[Xi]<=y<\[Xi]
},
{
(2\[Pi]*I)Total@#[[{Key@{p1-k,mo1}}]],
\[Xi]<=y<=1
}
}]&
]@splt[{fyTag,"F1F2","res"}];


(* ::Input:: *)
(*(*\:5bfc\:51fa\:5230\:786c\:76d8*)*)
(*serialize[gpdResidueDir][Flatten@{"residue-F1F2",fyTag,".wdx"},splt[{fyTag,"F1F2","pw"}]]*)


(* ::Input:: *)
(*ruleMass={\[Xi]->0.1,y->0.5,\[CapitalDelta]2->-0.036,mN->0.94,mo1->0.94,mm1->0.138,\[CapitalLambda]->0.9};*)
(*tec=Simplify@Chop@Cancel[*)
(*(splt[{fyTag,"F1F2","pw"}][[1]]*I*ktr*P[1])/.lConeKinematics/.kTIntegralKinematics/.ruleMass];*)
(*tec//fceStd*)
(*(-4\[Xi]^2mN^2)/(1-\[Xi]^2)/.ruleMass*)
(*Plot[tec*)
(*,{ktr,0,5}*)
(*,PlotRange->{Automatic,Full}*)
(*]*)


(* ::Section:: *)
(*KR,mes,octet,left*)


(* ::Input:: *)
(*diagIllus@chTag@{"KR","mes","oct","left"}*)


(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"KR","mes","oct","left"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*-\:5708\:79ef\:5206\:7684\:5206\:6bcd\:90e8\:5206\:ff0c\:5373\:4f20\:64ad\:5b50-----------------------*)
splt[{fyTag,"cls"}]=preFactor*fad[
{k,\[CapitalLambda]}->-4,{k,mm1}->-1,{p2-k,mo2}->-1
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spr",\[Mu]}]=GS[k] . GA5 . (GS[p2-k]+mo2) . GA[\[Mu]] . GA5;
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fyTag,"FAFB","spr"}]=projToFAFB[splt[{fyTag,"spr",\[Mu]}],\[Nu]];


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*)
,SP[k,\[CapitalDelta]]->1/2(fad[{k+\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2-fad[{k,\[CapitalLambda]}->1])
,SP[k,p1]->1/2(fad[{k+\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2+\[CapitalLambda]^2-(fad[{p2-k,mo2}->1]+mo2^2-mN^2))
,SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p2-k,mo2}->1]+mo2^2-mN^2))
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
splt[{fyTag,"FAFB","integ"}]=Collect[
Expand[
splt[{fyTag,"cls"}]*
(splt[{fyTag,"FAFB","spr"}]/.ruleScalar)
],
fadTmp1[__],Simplify];


(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)
splt[{fyTag,"FAFB","integ"}]//First//gatorShow


splt[{fyTag,"FAFB","lcone"}]=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@splt[{fyTag,"FAFB","integ"}];


splt[{fyTag,"FAFB","res"}]=Query[
(*<propgator\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@{
{k,\[CapitalLambda]},{k,mm1},
{p2-k,mo2}
}
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570----------------*)
,FAFBResidue[#,splt[{fyTag,"FAFB","lcone"}]]&
]@gatorZeros;


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2 *)
splt[{fyTag,"F1F2","res","tmp1"}]=projToF1F2/@splt[{fyTag,"FAFB","res"}];
(*\:7ffb\:8f6c\:6b21\:5e8f----*)
splt[{fyTag,"F1F2","res"}]=Table[splt[{fyTag,"F1F2","res","tmp1"}][[All,ffs]],{ffs,2}];


(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)
splt[{fyTag,"F1F2","pw"}]=Query[(*{f1f2}*)All,
(*<propgator\[Rule]expr>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570--------------*)
Piecewise[{
{
(-2\[Pi]*I)Total@#[[Key/@{
{k,\[CapitalLambda]},{k,mm1}
}]],
-\[Xi]<=y<1-2\[Xi]
},
{
(2\[Pi]*I)Total@#[[Key/@{
{k,\[CapitalLambda]},{k,mm1},{p2-k,mo2}
}]],
1-2\[Xi]<=y<=1
}
}]&
]@splt[{fyTag,"F1F2","res"}];


(* ::Input:: *)
(*(*\:5bfc\:51fa\:5230\:786c\:76d8*)*)
(*serialize[gpdResidueDir][Flatten@{"residue-F1F2",fyTag,".wdx"},splt[{fyTag,"F1F2","pw"}]]*)


(* ::Section:: *)
(*KR,mes,octet,right*)


(* ::Input:: *)
(*diagIllus@chTag@{"KR","mes","oct","right"}*)


(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"KR","mes","oct","right"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*-\:5708\:79ef\:5206\:7684\:5206\:6bcd\:90e8\:5206\:ff0c\:5373\:4f20\:64ad\:5b50-----------------------*)
splt[{fyTag,"cls"}]=preFactor*fad[
{k,\[CapitalLambda]}->-4,{k,mm1}->-1,{p1-k,mo1}->-1
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spr",\[Mu]}]=GA[\[Mu]] . GA5 . (GS[p1-k]+mo1) . GS[k] . GA5;
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fyTag,"FAFB","spr"}]=projToFAFB[splt[{fyTag,"spr",\[Mu]}],\[Nu]];


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*)
,SP[k,\[CapitalDelta]]->1/2(fad[{k,\[CapitalLambda]}->1]-(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2))
,SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2))
,SP[k,p2]->1/2(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2))
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
splt[{fyTag,"FAFB","integ"}]=Collect[
Expand[
splt[{fyTag,"cls"}]*
(splt[{fyTag,"FAFB","spr"}]/.ruleScalar)
],
fadTmp1[__],Simplify];


(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)
splt[{fyTag,"FAFB","integ"}]//First//gatorShow


splt[{fyTag,"FAFB","lcone"}]=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@splt[{fyTag,"FAFB","integ"}];


splt[{fyTag,"FAFB","res"}]=Query[
(*<propgator\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@{
{k,\[CapitalLambda]},{k,mm1},
{p1-k,mo1}
}
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570----------------*)
,FAFBResidue[#,splt[{fyTag,"FAFB","lcone"}]]&
]@gatorZeros;


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2 *)
splt[{fyTag,"F1F2","res","tmp1"}]=projToF1F2/@splt[{fyTag,"FAFB","res"}];
(*\:7ffb\:8f6c\:6b21\:5e8f----*)
splt[{fyTag,"F1F2","res"}]=Table[splt[{fyTag,"F1F2","res","tmp1"}][[All,ffs]],{ffs,2}];


(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)
splt[{fyTag,"F1F2","pw"}]=Query[(*{f1f2}*)All,
(*<propgator\[Rule]expr>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570--------------*)
Piecewise[{
{
(-2\[Pi]*I)Total@#[[Key/@{
{k,\[CapitalLambda]},{k,mm1}
}]],
-\[Xi]<=y<=1
}
}]&
]@splt[{fyTag,"F1F2","res"}];


(* ::Input:: *)
(*(*\:5bfc\:51fa\:5230\:786c\:76d8*)*)
(*serialize[gpdResidueDir][Flatten@{"residue-F1F2",fyTag,".wdx"},splt[{fyTag,"F1F2","pw"}]]*)


(* ::Section:: *)
(*KR,mes,oct,add,left*)


(* ::Input:: *)
(*diagIllus@chTag@{"KR","mes","oct","add","left"}*)


(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"KR","mes","oct","add","left"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*-\:5708\:79ef\:5206\:7684\:5206\:6bcd\:90e8\:5206\:ff0c\:5373\:4f20\:64ad\:5b50-----------------------*)
splt[{fyTag,"cls"}]=preFactor*fad[
{k,\[CapitalLambda]}->-2,{k,mm1}->-1,{p2-k,mo2}->-1,
(*\:6b63\:89c4\:5b50 -R\[Mu](-k,q) *)
{k,\[CapitalLambda]}->-2,{k+\[CapitalDelta],\[CapitalLambda]}->-2
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spr",\[Mu]}]=(
(*\:6b63\:89c4\:5b50\:7ec4\:5408: -R\[Mu](-k,q) *)
(-1)(SP[k,k]+SP[k+\[CapitalDelta],k+\[CapitalDelta]]-2\[CapitalLambda]^2)*(-1)FV[-2k+p2-p1,\[Mu]] .
GS[k] . GA5 . (GS[p2-k]+mo2) . GS[k-p2+p1] . GA5);
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fyTag,"FAFB","spr"}]=projToFAFB[splt[{fyTag,"spr",\[Mu]}],\[Nu]];


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*)
,SP[k,\[CapitalDelta]]->1/2(fad[{k+\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2-fad[{k,\[CapitalLambda]}->1])
,SP[k,p1]->1/2(fad[{k+\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2+\[CapitalLambda]^2-(fad[{p2-k,mo2}->1]+mo2^2-mN^2))
,SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p2-k,mo2}->1]+mo2^2-mN^2))
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
splt[{fyTag,"FAFB","integ"}]=Collect[
Expand[
splt[{fyTag,"cls"}]*
(splt[{fyTag,"FAFB","spr"}]/.ruleScalar)
],
fadTmp1[__],Simplify];


(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)
splt[{fyTag,"FAFB","integ"}]//First//gatorShow


splt[{fyTag,"FAFB","lcone"}]=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@splt[{fyTag,"FAFB","integ"}];


splt[{fyTag,"FAFB","res"}]=Query[
(*<propgator\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@{
{k,\[CapitalLambda]},{k+\[CapitalDelta],\[CapitalLambda]}->-2,{k,mm1}
,{p2-k,mo2}
}
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570----------------*)
,FAFBResidue[#,splt[{fyTag,"FAFB","lcone"}]]&
]@gatorZeros;


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2 *)
splt[{fyTag,"F1F2","res","tmp1"}]=projToF1F2/@splt[{fyTag,"FAFB","res"}];
(*\:7ffb\:8f6c\:5d4c\:5957\:6b21\:5e8f----*)
splt[{fyTag,"F1F2","res"}]=Table[splt[{fyTag,"F1F2","res","tmp1"}][[All,ffs]],{ffs,2}];


(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)
splt[{fyTag,"F1F2","pw"}]=Query[(*{f1f2}*)All,
(*<propgator\[Rule]expr>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570--------------*)
Piecewise[{
(*-----*)
{
(+2\[Pi]*I)Total@#[[Key/@{
{p2-k,mo2}
}]],
-\[Xi]<=y<1-2\[Xi]
},
(*-----*)
{
(*(-2\[Pi]*I)Total@#[[Key/@{
{k,\[CapitalLambda]},{k,mm1},{k+\[CapitalDelta],\[CapitalLambda]},{p2-k,mo2}
}]]*)
0,
1-2\[Xi]<=y<=1
}
(*-----*)
}]&
]@splt[{fyTag,"F1F2","res"}];


(* ::Input:: *)
(*(*\:5bfc\:51fa\:5230\:786c\:76d8*)*)
(*serialize[gpdResidueDir][Flatten@{"residue-F1F2",fyTag,".wdx"},splt[{fyTag,"F1F2","pw"}]]*)


(* ::Section:: *)
(*KR,mes,oct,add,right*)


(* ::Input:: *)
(*diagIllus@chTag@{"KR","mes","oct","add","right"}*)


(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"KR","mes","oct","add","right"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*-\:5708\:79ef\:5206\:7684\:5206\:6bcd\:90e8\:5206\:ff0c\:5373\:4f20\:64ad\:5b50-----------------------*)
splt[{fyTag,"cls"}]=preFactor*fad[
{k,\[CapitalLambda]}->-2,{k,mm1}->-1,{p1-k,mo1}->-1,
(*\:6b63\:89c4\:5b50 R\[Mu](k,q)*)
{k,\[CapitalLambda]}->-2,{k-\[CapitalDelta],\[CapitalLambda]}->-2
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spr",\[Mu]}]=(
(*\:6b63\:89c4\:5b50\:7ec4\:5408: R\[Mu](k,q) *)
(SP[k,k]+SP[k-\[CapitalDelta],k-\[CapitalDelta]]-2\[CapitalLambda]^2)(-1)FV[2k+p2-p1,\[Mu]] .
GS[k+p2-p1] . GA5 . (GS[p1-k]+mo1) . GS[k] . GA5);
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fyTag,"FAFB","spr"}]=projToFAFB[splt[{fyTag,"spr",\[Mu]}],\[Nu]];


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*)
,SP[k,\[CapitalDelta]]->1/2(fad[{k,\[CapitalLambda]}->1]-(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2))
,SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2))
,SP[k,p2]->1/2(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2))
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
splt[{fyTag,"FAFB","integ"}]=Collect[
Expand[
splt[{fyTag,"cls"}]*
(splt[{fyTag,"FAFB","spr"}]/.ruleScalar)
],
fadTmp1[__],Simplify];


(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)
splt[{fyTag,"FAFB","integ"}]//First//gatorShow


splt[{fyTag,"FAFB","lcone"}]=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@splt[{fyTag,"FAFB","integ"}];


splt[{fyTag,"FAFB","res"}]=Query[
(*<propgator\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@{
{k,\[CapitalLambda]},{k-\[CapitalDelta],\[CapitalLambda]},{k,mm1}
,{p1-k,mo1}
}
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570----------------*)
,FAFBResidue[#,splt[{fyTag,"FAFB","lcone"}]]&
]@gatorZeros;


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2 *)
splt[{fyTag,"F1F2","res","tmp1"}]=projToF1F2/@splt[{fyTag,"FAFB","res"}];
(*\:7ffb\:8f6c\:6b21\:5e8f----*)
splt[{fyTag,"F1F2","res"}]=Table[splt[{fyTag,"F1F2","res","tmp1"}][[All,ffs]],{ffs,2}];


(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)
splt[{fyTag,"F1F2","pw"}]=Query[(*{f1f2}*)All,
(*<propgator\[Rule]expr>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570--------------*)
Piecewise[{
(*--------------*)
{
(-2\[Pi]*I)Total@#[[Key/@{
{k,\[CapitalLambda]},{k,mm1}
}]],
-\[Xi]<=y<\[Xi]
},
(*--------------*)
{
(+2\[Pi]*I)Total@#[[Key/@{
{p1-k,mo1}
}]],
\[Xi]<=y<=1
}
}]&
]@splt[{fyTag,"F1F2","res"}];


(* ::Input:: *)
(*(*\:5bfc\:51fa\:5230\:786c\:76d8*)*)
(*serialize[gpdResidueDir][Flatten@{"residue-F1F2",fyTag,".wdx"},splt[{fyTag,"F1F2","pw"}]]*)


(* ::Section:: *)
(*RainBow,A-octet,F1,nonlocal*)


(* ::Input:: *)
(*diagIllus@chTag@{"RB","oct","F1"}*)


(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"RB","oct","F1"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*-\:5708\:79ef\:5206\:7684\:5206\:6bcd\:90e8\:5206\:ff0c\:5373\:4f20\:64ad\:5b50-----------------------*)
splt[{fyTag,"cls"}]=preFactor*fad[
{k,\[CapitalLambda]}->-2,{k,mm1}->-1,{p2-k,mo2}->-1,{p1-k,mo1}->-1
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spr",\[Mu]}]=(GS[k] . GA5 . (GS[p2-k]+mo2) .
GA[\[Mu]] . (GS[p1-k]+mo1) . GS[k] . GA5);
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fyTag,"FAFB","spr"}]=projToFAFB[splt[{fyTag,"spr",\[Mu]}],\[Nu]];


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*)
,SP[k,\[CapitalDelta]]->1/2(fad[{k,\[CapitalLambda]}->1]-(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2))
,SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2))
,SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p2-k,mo2}->1]+mo2^2-mN^2))
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
splt[{fyTag,"FAFB","integ"}]=Collect[
Expand[
splt[{fyTag,"cls"}]*
(splt[{fyTag,"FAFB","spr"}]/.ruleScalar)
],
fadTmp1[__],Simplify];


(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)
splt[{fyTag,"FAFB","integ"}]//First//gatorShow


splt[{fyTag,"FAFB","lcone"}]=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@splt[{fyTag,"FAFB","integ"}];


splt[{fyTag,"FAFB","res"}]=Query[
(*<propgator\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@{
{k,\[CapitalLambda]},{k,mm1}
,{p2-k,mo2},{p1-k,mo1}
}
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570----------------*)
,FAFBResidue[#,splt[{fyTag,"FAFB","lcone"}]]&
]@gatorZeros;


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2 *)
splt[{fyTag,"F1F2","res","tmp1"}]=projToF1F2/@splt[{fyTag,"FAFB","res"}];
(*\:7ffb\:8f6c\:6b21\:5e8f----*)
splt[{fyTag,"F1F2","res"}]=Table[splt[{fyTag,"F1F2","res","tmp1"}][[All,ffs]],{ffs,2}];


(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)
splt[{fyTag,"F1F2","pw"}]=Query[(*{f1f2}*)All,
(*<propgator\[Rule]expr>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570--------------*)
Piecewise[{
(*--------------*)
{
(-2\[Pi]*I)Total@#[[Key/@{
{k,\[CapitalLambda]},{k,mm1}
}]],
-\[Xi]<=y<1-2\[Xi]
},
(*--------------*)
{
(+2\[Pi]*I)Total@#[[Key/@{
{p1-k,mo1}
}]],
1-2\[Xi]<=y<=1
}
}]&
]@splt[{fyTag,"F1F2","res"}];


(* ::Input:: *)
(*(*\:5bfc\:51fa\:5230\:786c\:76d8*)*)
(*serialize[gpdResidueDir][Flatten@{"residue-F1F2",fyTag,".wdx"},splt[{fyTag,"F1F2","pw"}]]*)


(* ::Section:: *)
(*RainBow,A-octet,F2,nonlocal*)


(* ::Input:: *)
(*diagIllus@chTag@{"RB","oct","F2"}*)


(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"RB","oct","F2"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*-\:5708\:79ef\:5206\:7684\:5206\:6bcd\:90e8\:5206\:ff0c\:5373\:4f20\:64ad\:5b50-----------------------*)
splt[{fyTag,"cls"}]=preFactor*fad[
{k,\[CapitalLambda]}->-2,{k,mm1}->-1,{p2-k,mo2}->-1,{p1-k,mo1}->-1
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spr",\[Mu]}]=(I (FV[p2-p1,\[Nu]]/(2mN)) . GS[k] . GA5 . (GS[p2-k]+mo2) .
DiracSigma[GA[\[Mu]],GA[\[Nu]]] . (GS[p1-k]+mo1) . GS[k] . GA5);
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fyTag,"FAFB","spr"}]=projToFAFB[splt[{fyTag,"spr",\[Mu]}],\[Nu]];


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*)
,SP[k,\[CapitalDelta]]->1/2(fad[{k,\[CapitalLambda]}->1]-(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2))
,SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2))
,SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p2-k,mo2}->1]+mo2^2-mN^2))
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
splt[{fyTag,"FAFB","integ"}]=Collect[
Expand[
splt[{fyTag,"cls"}]*
(splt[{fyTag,"FAFB","spr"}]/.ruleScalar)
],
fadTmp1[__],Simplify];


(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)
splt[{fyTag,"FAFB","integ"}]//First//gatorShow


splt[{fyTag,"FAFB","lcone"}]=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@splt[{fyTag,"FAFB","integ"}];


splt[{fyTag,"FAFB","res"}]=Query[
(*<propgator\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@{
{k,\[CapitalLambda]},{k,mm1}
,{p2-k,mo2},{p1-k,mo1}
}
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570----------------*)
,FAFBResidue[#,splt[{fyTag,"FAFB","lcone"}]]&
]@gatorZeros;


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2 *)
splt[{fyTag,"F1F2","res","tmp1"}]=projToF1F2/@splt[{fyTag,"FAFB","res"}];
(*\:7ffb\:8f6c\:6b21\:5e8f----*)
splt[{fyTag,"F1F2","res"}]=Table[splt[{fyTag,"F1F2","res","tmp1"}][[All,ffs]],{ffs,2}];


(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)
splt[{fyTag,"F1F2","pw"}]=Query[(*{f1f2}*)All,
(*<propgator\[Rule]expr>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570--------------*)
Piecewise[{
(*--------------*)
{
(-2\[Pi]*I)Total@#[[Key/@{
{k,\[CapitalLambda]},{k,mm1}
}]],
-\[Xi]<=y<1-2\[Xi]
},
(*--------------*)
{
(+2\[Pi]*I)Total@#[[Key/@{
{p1-k,mo1}
}]],
1-2\[Xi]<=y<=1
}
}]&
]@splt[{fyTag,"F1F2","res"}];


(* ::Input:: *)
(*(*\:5bfc\:51fa\:5230\:786c\:76d8*)*)
(*serialize[gpdResidueDir][Flatten@{"residue-F1F2",fyTag,".wdx"},splt[{fyTag,"F1F2","pw"}]]*)


(* ::Section:: *)
(*tadpole,A-octet,F1,nonlocal*)


(* ::Input:: *)
(*diagIllus@chTag@{"tad","oct","F1"}*)


(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"tad","oct","F1"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*-\:5708\:79ef\:5206\:7684\:5206\:6bcd\:90e8\:5206\:ff0c\:5373\:4f20\:64ad\:5b50-----------------------*)
splt[{fyTag,"cls"}]=preFactor*fad[
{k,\[CapitalLambda]}->-4,{k,mm1}->-1
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spr",\[Mu]}]=GA[\[Mu]];
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fyTag,"FAFB","spr"}]=projToFAFB[splt[{fyTag,"spr",\[Mu]}],\[Nu]];


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*)
,SP[k,\[CapitalDelta]]->1/2(fad[{k,\[CapitalLambda]}->1]-(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2))
,SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2))
,SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p2-k,mo2}->1]+mo2^2-mN^2))
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
splt[{fyTag,"FAFB","integ"}]=Collect[
Expand[
splt[{fyTag,"cls"}]*
(splt[{fyTag,"FAFB","spr"}]/.ruleScalar)
],
fadTmp1[__],Simplify];


(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)
splt[{fyTag,"FAFB","integ"}]//First//gatorShow


splt[{fyTag,"FAFB","lcone"}]=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@splt[{fyTag,"FAFB","integ"}];


splt[{fyTag,"FAFB","res"}]=Query[
(*<propgator\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@{
{k,\[CapitalLambda]},{k,mm1}
}
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570----------------*)
,FAFBResidue[#,splt[{fyTag,"FAFB","lcone"}]]&
]@gatorZeros;


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2 *)
splt[{fyTag,"F1F2","res","tmp1"}]=projToF1F2/@splt[{fyTag,"FAFB","res"}];
(*\:7ffb\:8f6c\:6b21\:5e8f----*)
splt[{fyTag,"F1F2","res"}]=Table[splt[{fyTag,"F1F2","res","tmp1"}][[All,ffs]],{ffs,2}];


(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)
splt[{fyTag,"F1F2","pw"}]=Query[(*{f1f2}*)All,
(*<propgator\[Rule]expr>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570--------------*)
Piecewise[{
(*---------------*)
{
(*(-2\[Pi]*I)Total@#[[Key/@{
{k,\[CapitalLambda]},{k,mm1}
}]]*)
0,
-\[Xi]<=y<1-2\[Xi]
},
(*---------------*)
{
(*(+2\[Pi]*I)Total@#[[Key/@{
{k,\[CapitalLambda]},{k,mm1}
}]]*)
0,
1-2\[Xi]<=y<=1
}
(*---------------*)
}]&
]@splt[{fyTag,"F1F2","res"}];


(* ::Input:: *)
(*(*\:5bfc\:51fa\:5230\:786c\:76d8*)*)
(*serialize[gpdResidueDir][Flatten@{"residue-F1F2",fyTag,".wdx"},splt[{fyTag,"F1F2","pw"}]]*)


(* ::Section::Closed:: *)
(*tadpole,A-octet,F1,addition*)


(* ::Input:: *)
(*diagIllus@chTag@{"tad","oct","F1","add"}*)


(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"tad","oct","F1","add"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*-\:5708\:79ef\:5206\:7684\:5206\:6bcd\:90e8\:5206\:ff0c\:5373\:4f20\:64ad\:5b50-----------------------*)
splt[{fyTag,"cls"}]=preFactor*fad[
{k,\[CapitalLambda]}->-2,{k,mm1}->-1,
{k-\[CapitalDelta],\[CapitalLambda]}->-2,{k,\[CapitalLambda]}->-2
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spr",\[Mu]}]=(fad[{k,\[CapitalLambda]}->1]+fad[{k-\[CapitalDelta],\[CapitalLambda]}->1])*2GS[k] . (-FV[2k-\[CapitalDelta],\[Mu]]);
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fyTag,"FAFB","spr"}]=projToFAFB[splt[{fyTag,"spr",\[Mu]}],\[Nu]];


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*)
,SP[k,\[CapitalDelta]]->1/2(fad[{k,\[CapitalLambda]}->1]-(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2))
,SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2))
,SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p2-k,mo2}->1]+mo2^2-mN^2))
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
splt[{fyTag,"FAFB","integ"}]=Collect[
Expand[
splt[{fyTag,"cls"}]*
(splt[{fyTag,"FAFB","spr"}]/.ruleScalar)
],
fadTmp1[__],Simplify];


(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)
splt[{fyTag,"FAFB","integ"}]//First//gatorShow


splt[{fyTag,"FAFB","lcone"}]=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@splt[{fyTag,"FAFB","integ"}];


splt[{fyTag,"FAFB","res"}]=Query[
(*<propgator\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@{
{k,\[CapitalLambda]},{k,mm1},
{p2-k,mo2}
}
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570----------------*)
,FAFBResidue[#,splt[{fyTag,"FAFB","lcone"}]]&
]@gatorZeros;


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2 *)
splt[{fyTag,"F1F2","res","tmp1"}]=projToF1F2/@splt[{fyTag,"FAFB","res"}];
(*\:7ffb\:8f6c\:6b21\:5e8f----*)
splt[{fyTag,"F1F2","res"}]=Table[splt[{fyTag,"F1F2","res","tmp1"}][[All,ffs]],{ffs,2}];


(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)
splt[{fyTag,"F1F2","pw"}]=Query[(*{f1f2}*)All,
(*<propgator\[Rule]expr>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570--------------*)
Piecewise[{
{
(-2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1}}]],
-\[Xi]<=y<1-2\[Xi]
},
{
(2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1},Key@{p1-k,mo1}}]],
1-2\[Xi]<=y<=1
}
}]&
]@splt[{fyTag,"F1F2","res"}];


(* ::Input:: *)
(*(*\:5bfc\:51fa\:5230\:786c\:76d8*)*)
(*serialize[gpdResidueDir][Flatten@{"residue-F1F2",fyTag,".wdx"},splt[{fyTag,"F1F2","pw"}]]*)


(* ::Section::Closed:: *)
(*tadpole,A-octet,F2,nonlocal*)


(* ::Input:: *)
(*diagIllus@chTag@{"tad","oct","F2"}*)


(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"tad","oct","F2"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*-\:5708\:79ef\:5206\:7684\:5206\:6bcd\:90e8\:5206\:ff0c\:5373\:4f20\:64ad\:5b50-----------------------*)
splt[{fyTag,"cls"}]=preFactor*fad[
{k,\[CapitalLambda]}->-4,{k,mm1}->-1
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spr",\[Mu]}]=I/(2mN)*DiracSigma[GA[\[Mu]],GA[\[Nu]]] . FV[-\[CapitalDelta],\[Nu]];
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fyTag,"FAFB","spr"}]=projToFAFB[splt[{fyTag,"spr",\[Mu]}],\[Nu]];


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*)
,SP[k,\[CapitalDelta]]->1/2(fad[{k,\[CapitalLambda]}->1]-(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2))
,SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2))
,SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p2-k,mo2}->1]+mo2^2-mN^2))
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
splt[{fyTag,"FAFB","integ"}]=Collect[
Expand[
splt[{fyTag,"cls"}]*
(splt[{fyTag,"FAFB","spr"}]/.ruleScalar)
],
fadTmp1[__],Simplify];


(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)
splt[{fyTag,"FAFB","integ"}]//First//gatorShow


splt[{fyTag,"FAFB","lcone"}]=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@splt[{fyTag,"FAFB","integ"}];


splt[{fyTag,"FAFB","res"}]=Query[
(*<propgator\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@{
{k,\[CapitalLambda]},{k,mm1},
{p2-k,mo2}
}
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570----------------*)
,FAFBResidue[#,splt[{fyTag,"FAFB","lcone"}]]&
]@gatorZeros;


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2 *)
splt[{fyTag,"F1F2","res","tmp1"}]=projToF1F2/@splt[{fyTag,"FAFB","res"}];
(*\:7ffb\:8f6c\:6b21\:5e8f----*)
splt[{fyTag,"F1F2","res"}]=Table[splt[{fyTag,"F1F2","res","tmp1"}][[All,ffs]],{ffs,2}];


(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)
splt[{fyTag,"F1F2","pw"}]=Query[(*{f1f2}*)All,
(*<propgator\[Rule]expr>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570--------------*)
Piecewise[{
{
(-2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1}}]],
-\[Xi]<=y<1-2\[Xi]
},
{
(2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1},Key@{p1-k,mo1}}]],
1-2\[Xi]<=y<=1
}
}]&
]@splt[{fyTag,"F1F2","res"}];


(* ::Input:: *)
(*(*\:5bfc\:51fa\:5230\:786c\:76d8*)*)
(*serialize[gpdResidueDir][Flatten@{"residue-F1F2",fyTag,".wdx"},splt[{fyTag,"F1F2","pw"}]]*)


(* ::Section::Closed:: *)
(*bubble, A-meson,order2*)


(* ::Input:: *)
(*diagIllus@chTag@{"bub","mes","o2"}*)


(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"bub","mes","o2"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*-\:5708\:79ef\:5206\:7684\:5206\:6bcd\:90e8\:5206\:ff0c\:5373\:4f20\:64ad\:5b50-----------------------*)
splt[{fyTag,"cls"}]=preFactor*fad[
{k,\[CapitalLambda]}->-2,{k-\[CapitalDelta],\[CapitalLambda]}->-2,
{k,mm1}->-1,{k-\[CapitalDelta],mm1}->-1
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spr",\[Mu]}]=4(GS[k] . FV[k,\[Mu]]);
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fyTag,"FAFB","spr"}]=projToFAFB[splt[{fyTag,"spr",\[Mu]}],\[Nu]];


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*)
,SP[k,\[CapitalDelta]]->1/2(fad[{k,\[CapitalLambda]}->1]-(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2))
,SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2))
,SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p2-k,mo2}->1]+mo2^2-mN^2))
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
splt[{fyTag,"FAFB","integ"}]=Collect[
Expand[
splt[{fyTag,"cls"}]*
(splt[{fyTag,"FAFB","spr"}]/.ruleScalar)
],
fadTmp1[__],Simplify];


(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)
splt[{fyTag,"FAFB","integ"}]//First//gatorShow


splt[{fyTag,"FAFB","lcone"}]=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@splt[{fyTag,"FAFB","integ"}];


splt[{fyTag,"FAFB","res"}]=Query[
(*<propgator\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@{
{k,\[CapitalLambda]},{k,mm1},
{p2-k,mo2}
}
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570----------------*)
,FAFBResidue[#,splt[{fyTag,"FAFB","lcone"}]]&
]@gatorZeros;


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2 *)
splt[{fyTag,"F1F2","res","tmp1"}]=projToF1F2/@splt[{fyTag,"FAFB","res"}];
(*\:7ffb\:8f6c\:6b21\:5e8f----*)
splt[{fyTag,"F1F2","res"}]=Table[splt[{fyTag,"F1F2","res","tmp1"}][[All,ffs]],{ffs,2}];


(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)
splt[{fyTag,"F1F2","pw"}]=Query[(*{f1f2}*)All,
(*<propgator\[Rule]expr>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570--------------*)
Piecewise[{
{
(-2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1}}]],
-\[Xi]<=y<1-2\[Xi]
},
{
(2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1},Key@{p1-k,mo1}}]],
1-2\[Xi]<=y<=1
}
}]&
]@splt[{fyTag,"F1F2","res"}];


(* ::Input:: *)
(*(*\:5bfc\:51fa\:5230\:786c\:76d8*)*)
(*serialize[gpdResidueDir][Flatten@{"residue-F1F2",fyTag,".wdx"},splt[{fyTag,"F1F2","pw"}]]*)


(* ::Section::Closed:: *)
(*bubble, A-meson,order2*)


(* ::Input:: *)
(*diagIllus@chTag@{"bub","mes","ten","o2"}*)


(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"bub","mes","ten","o2"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*-\:5708\:79ef\:5206\:7684\:5206\:6bcd\:90e8\:5206\:ff0c\:5373\:4f20\:64ad\:5b50-----------------------*)
splt[{fyTag,"cls"}]=preFactor*fad[
{k,\[CapitalLambda]}->-2,{k-\[CapitalDelta],\[CapitalLambda]}->-2,
{k,mm1}->-1,{k-\[CapitalDelta],mm1}->-1
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spr",\[Mu]}]=FV[k-\[CapitalDelta],\[Rho]] . FV[k,\[Nu]]*DiracSigma[GA[\[Rho]],GA[\[Nu]]] . FV[2k-\[CapitalDelta],\[Mu]];
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fyTag,"FAFB","spr"}]=projToFAFB[splt[{fyTag,"spr",\[Mu]}],\[Nu]];


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*)
,SP[k,\[CapitalDelta]]->1/2(fad[{k,\[CapitalLambda]}->1]-(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2))
,SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2))
,SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p2-k,mo2}->1]+mo2^2-mN^2))
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
splt[{fyTag,"FAFB","integ"}]=Collect[
Expand[
splt[{fyTag,"cls"}]*
(splt[{fyTag,"FAFB","spr"}]/.ruleScalar)
],
fadTmp1[__],Simplify];


(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)
splt[{fyTag,"FAFB","integ"}]//First//gatorShow


splt[{fyTag,"FAFB","lcone"}]=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@splt[{fyTag,"FAFB","integ"}];


splt[{fyTag,"FAFB","res"}]=Query[
(*<propgator\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@{
{k,\[CapitalLambda]},{k,mm1},
{p2-k,mo2}
}
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570----------------*)
,FAFBResidue[#,splt[{fyTag,"FAFB","lcone"}]]&
]@gatorZeros;


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2 *)
splt[{fyTag,"F1F2","res","tmp1"}]=projToF1F2/@splt[{fyTag,"FAFB","res"}];
(*\:7ffb\:8f6c\:6b21\:5e8f----*)
splt[{fyTag,"F1F2","res"}]=Table[splt[{fyTag,"F1F2","res","tmp1"}][[All,ffs]],{ffs,2}];


(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)
splt[{fyTag,"F1F2","pw"}]=Query[(*{f1f2}*)All,
(*<propgator\[Rule]expr>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570--------------*)
Piecewise[{
{
(-2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1}}]],
-\[Xi]<=y<1-2\[Xi]
},
{
(2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1},Key@{p1-k,mo1}}]],
1-2\[Xi]<=y<=1
}
}]&
]@splt[{fyTag,"F1F2","res"}];


(* ::Input:: *)
(*(*\:5bfc\:51fa\:5230\:786c\:76d8*)*)
(*serialize[gpdResidueDir][Flatten@{"residue-F1F2",fyTag,".wdx"},splt[{fyTag,"F1F2","pw"}]]*)


(* ::Chapter:: *)
(*split decuplet*)


(* ::Section:: *)
(*bubble, A-meson,order2*)


(* ::Input:: *)
(*diagIllus@chTag@{"RB","mes","dec"}*)


(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"RB","mes","dec"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*-\:5708\:79ef\:5206\:7684\:5206\:6bcd\:90e8\:5206\:ff0c\:5373\:4f20\:64ad\:5b50-----------------------*)
splt[{fyTag,"cls"}]=preFactor*fad[
{k,\[CapitalLambda]}->-2,{k-\[CapitalDelta],\[CapitalLambda]}->-2,
{k-\[CapitalDelta],mm1}->-1,{k,mm1}->-1,{p1-k,md1}->-1
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spr",\[Mu]}]=(FV[k-\[CapitalDelta],\[Alpha]] . \[CapitalTheta][\[Alpha],\[Beta]] .
decGator[{\[Beta],\[Rho]},{p1-k,md1}] . \[CapitalTheta][\[Rho],\[Nu]] . FV[k,\[Nu]] . FV[2k-\[CapitalDelta],\[Mu]]);
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fyTag,"FAFB","spr"}]=projToFAFB[splt[{fyTag,"spr",\[Mu]}],\[Nu]];


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*)
,SP[k,\[CapitalDelta]]->1/2(fad[{k,\[CapitalLambda]}->1]-(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2))
,SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2))
,SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p2-k,mo2}->1]+mo2^2-mN^2))
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
splt[{fyTag,"FAFB","integ"}]=Collect[
Expand[
splt[{fyTag,"cls"}]*
(splt[{fyTag,"FAFB","spr"}]/.ruleScalar)
],
fadTmp1[__],Simplify];


(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)
splt[{fyTag,"FAFB","integ"}]//First//gatorShow


splt[{fyTag,"FAFB","lcone"}]=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@splt[{fyTag,"FAFB","integ"}];


splt[{fyTag,"FAFB","res"}]=Query[
(*<propgator\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@{
{k,\[CapitalLambda]},{k,mm1},
{p2-k,mo2}
}
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570----------------*)
,FAFBResidue[#,splt[{fyTag,"FAFB","lcone"}]]&
]@gatorZeros;


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2 *)
splt[{fyTag,"F1F2","res","tmp1"}]=projToF1F2/@splt[{fyTag,"FAFB","res"}];
(*\:7ffb\:8f6c\:6b21\:5e8f----*)
splt[{fyTag,"F1F2","res"}]=Table[splt[{fyTag,"F1F2","res","tmp1"}][[All,ffs]],{ffs,2}];


(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)
splt[{fyTag,"F1F2","pw"}]=Query[(*{f1f2}*)All,
(*<propgator\[Rule]expr>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570--------------*)
Piecewise[{
{
(-2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1}}]],
-\[Xi]<=y<1-2\[Xi]
},
{
(2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1},Key@{p1-k,mo1}}]],
1-2\[Xi]<=y<=1
}
}]&
]@splt[{fyTag,"F1F2","res"}];


(* ::Input:: *)
(*(*\:5bfc\:51fa\:5230\:786c\:76d8*)*)
(*serialize[gpdResidueDir][Flatten@{"residue-F1F2",fyTag,".wdx"},splt[{fyTag,"F1F2","pw"}]]*)


(* ::Section::Closed:: *)
(*RainBow,A-decuplet,F1*)


(* ::Input:: *)
(*diagIllus@chTag@{"RB","dec","F1"}*)


(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"RB","dec","F1"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*-\:5708\:79ef\:5206\:7684\:5206\:6bcd\:90e8\:5206\:ff0c\:5373\:4f20\:64ad\:5b50-----------------------*)
splt[{fyTag,"cls"}]=preFactor*fad[
{k,\[CapitalLambda]}->-4,{k,mm1}->-1,{p2-k,md2}->-1,{p1-k,md1}->-1
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spr",\[Mu]}]=(FV[k,\[Alpha]] . \[CapitalTheta][\[Alpha],\[Beta]] . decGator[{\[Beta],\[Theta]},{p2-k,md2}] .
decGamma3[\[Theta],\[Nu],\[Mu]] . decGator[{\[Nu],\[Rho]},{p1-k,md1}] . \[CapitalTheta][\[Rho],\[Eta]] . FV[k,\[Eta]]);
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fyTag,"FAFB","spr"}]=projToFAFB[splt[{fyTag,"spr",\[Mu]}],\[Nu]];


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*)
,SP[k,\[CapitalDelta]]->1/2(fad[{k,\[CapitalLambda]}->1]-(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2))
,SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2))
,SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p2-k,mo2}->1]+mo2^2-mN^2))
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
splt[{fyTag,"FAFB","integ"}]=Collect[
Expand[
splt[{fyTag,"cls"}]*
(splt[{fyTag,"FAFB","spr"}]/.ruleScalar)
],
fadTmp1[__],Simplify];


(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)
splt[{fyTag,"FAFB","integ"}]//First//gatorShow


splt[{fyTag,"FAFB","lcone"}]=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@splt[{fyTag,"FAFB","integ"}];


splt[{fyTag,"FAFB","res"}]=Query[
(*<propgator\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@{
{k,\[CapitalLambda]},{k,mm1},
{p2-k,mo2}
}
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570----------------*)
,FAFBResidue[#,splt[{fyTag,"FAFB","lcone"}]]&
]@gatorZeros;


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2 *)
splt[{fyTag,"F1F2","res","tmp1"}]=projToF1F2/@splt[{fyTag,"FAFB","res"}];
(*\:7ffb\:8f6c\:6b21\:5e8f----*)
splt[{fyTag,"F1F2","res"}]=Table[splt[{fyTag,"F1F2","res","tmp1"}][[All,ffs]],{ffs,2}];


(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)
splt[{fyTag,"F1F2","pw"}]=Query[(*{f1f2}*)All,
(*<propgator\[Rule]expr>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570--------------*)
Piecewise[{
{
(-2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1}}]],
-\[Xi]<=y<1-2\[Xi]
},
{
(2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1},Key@{p1-k,mo1}}]],
1-2\[Xi]<=y<=1
}
}]&
]@splt[{fyTag,"F1F2","res"}];


(* ::Input:: *)
(*(*\:5bfc\:51fa\:5230\:786c\:76d8*)*)
(*serialize[gpdResidueDir][Flatten@{"residue-F1F2",fyTag,".wdx"},splt[{fyTag,"F1F2","pw"}]]*)


(* ::Section::Closed:: *)
(*RainBow,A-decuplet,F2*)


(* ::Input:: *)
(*diagIllus@chTag@{"RB","dec","F2"}*)


(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"RB","dec","F2"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*-\:5708\:79ef\:5206\:7684\:5206\:6bcd\:90e8\:5206\:ff0c\:5373\:4f20\:64ad\:5b50-----------------------*)
splt[{fyTag,"cls"}]=preFactor*fad[
{k,\[CapitalLambda]}->-4,{k,mm1}->-1,{p2-k,md2}->-1,{p1-k,md1}->-1
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spr",\[Mu]}]=I/(2mN) (FV[k,\[Alpha]] . \[CapitalTheta][\[Alpha],\[Beta]] . decGator[{\[Beta],\[Theta]},{p2-k,md2}] .
DiracSigma[GA[\[Mu]],GA[\[Nu]]] . FV[-\[CapitalDelta],\[Nu]] . decGator[{\[Theta],\[Rho]},{p1-k,md1}] . \[CapitalTheta][\[Rho],\[Eta]] . FV[k,\[Eta]]);
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fyTag,"FAFB","spr"}]=projToFAFB[splt[{fyTag,"spr",\[Mu]}],\[Nu]];


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*)
,SP[k,\[CapitalDelta]]->1/2(fad[{k,\[CapitalLambda]}->1]-(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2))
,SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2))
,SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p2-k,mo2}->1]+mo2^2-mN^2))
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
splt[{fyTag,"FAFB","integ"}]=Collect[
Expand[
splt[{fyTag,"cls"}]*
(splt[{fyTag,"FAFB","spr"}]/.ruleScalar)
],
fadTmp1[__],Simplify];


(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)
splt[{fyTag,"FAFB","integ"}]//First//gatorShow


splt[{fyTag,"FAFB","lcone"}]=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@splt[{fyTag,"FAFB","integ"}];


splt[{fyTag,"FAFB","res"}]=Query[
(*<propgator\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@{
{k,\[CapitalLambda]},{k,mm1},
{p2-k,mo2}
}
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570----------------*)
,FAFBResidue[#,splt[{fyTag,"FAFB","lcone"}]]&
]@gatorZeros;


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2 *)
splt[{fyTag,"F1F2","res","tmp1"}]=projToF1F2/@splt[{fyTag,"FAFB","res"}];
(*\:7ffb\:8f6c\:6b21\:5e8f----*)
splt[{fyTag,"F1F2","res"}]=Table[splt[{fyTag,"F1F2","res","tmp1"}][[All,ffs]],{ffs,2}];


(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)
splt[{fyTag,"F1F2","pw"}]=Query[(*{f1f2}*)All,
(*<propgator\[Rule]expr>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570--------------*)
Piecewise[{
{
(-2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1}}]],
-\[Xi]<=y<1-2\[Xi]
},
{
(2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1},Key@{p1-k,mo1}}]],
1-2\[Xi]<=y<=1
}
}]&
]@splt[{fyTag,"F1F2","res"}];


(* ::Input:: *)
(*(*\:5bfc\:51fa\:5230\:786c\:76d8*)*)
(*serialize[gpdResidueDir][Flatten@{"residue-F1F2",fyTag,".wdx"},splt[{fyTag,"F1F2","pw"}]]*)


(* ::Section:: *)
(*RainBow,A-decuplet,trans,left*)


(* ::Input:: *)
(*diagIllus@chTag@{"RB","trans","left"}*)


(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"RB","trans","left"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*-\:5708\:79ef\:5206\:7684\:5206\:6bcd\:90e8\:5206\:ff0c\:5373\:4f20\:64ad\:5b50-----------------------*)
splt[{fyTag,"cls"}]=preFactor*fad[
{k,\[CapitalLambda]}->-4,{k,mm1}->-1,{p2-k,md1}->-1,{p1-k,mo1}->-1
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spr",\[Mu]}]=(
GS[k] . GA5 . (GS[p2-k]+mo1) . FV[-\[CapitalDelta],\[Nu]] . (
GA[\[Nu]] . GA5 . decGator[{\[Mu],\[Beta]},{p1-k,md1}]-GA[\[Mu]] . GA5 . decGator[{\[Nu],\[Beta]},{p1-k,md1}]) .
\[CapitalTheta][\[Beta],\[Alpha]] . FV[k,\[Alpha]]);
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fyTag,"FAFB","spr"}]=projToFAFB[splt[{fyTag,"spr",\[Mu]}],\[Nu]];


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*)
,SP[k,\[CapitalDelta]]->1/2(fad[{k,\[CapitalLambda]}->1]-(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2))
,SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2))
,SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p2-k,mo2}->1]+mo2^2-mN^2))
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
splt[{fyTag,"FAFB","integ"}]=Collect[
Expand[
splt[{fyTag,"cls"}]*
(splt[{fyTag,"FAFB","spr"}]/.ruleScalar)
],
fadTmp1[__],Simplify];


(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)
splt[{fyTag,"FAFB","integ"}]//First//gatorShow


splt[{fyTag,"FAFB","lcone"}]=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@splt[{fyTag,"FAFB","integ"}];


splt[{fyTag,"FAFB","res"}]=Query[
(*<propgator\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@{
{k,\[CapitalLambda]},{k,mm1},
{p2-k,mo2}
}
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570----------------*)
,FAFBResidue[#,splt[{fyTag,"FAFB","lcone"}]]&
]@gatorZeros;


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2 *)
splt[{fyTag,"F1F2","res","tmp1"}]=projToF1F2/@splt[{fyTag,"FAFB","res"}];
(*\:7ffb\:8f6c\:6b21\:5e8f----*)
splt[{fyTag,"F1F2","res"}]=Table[splt[{fyTag,"F1F2","res","tmp1"}][[All,ffs]],{ffs,2}];


(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)
splt[{fyTag,"F1F2","pw"}]=Query[(*{f1f2}*)All,
(*<propgator\[Rule]expr>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570--------------*)
Piecewise[{
{
(-2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1}}]],
-\[Xi]<=y<1-2\[Xi]
},
{
(2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1},Key@{p1-k,mo1}}]],
1-2\[Xi]<=y<=1
}
}]&
]@splt[{fyTag,"F1F2","res"}];


(* ::Input:: *)
(*(*\:5bfc\:51fa\:5230\:786c\:76d8*)*)
(*serialize[gpdResidueDir][Flatten@{"residue-F1F2",fyTag,".wdx"},splt[{fyTag,"F1F2","pw"}]]*)


(* ::Section:: *)
(*RainBow,A-decuplet,trans,right*)


(* ::Input:: *)
(*diagIllus@chTag@{"RB","trans","right"}*)


(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"RB","trans","right"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*-\:5708\:79ef\:5206\:7684\:5206\:6bcd\:90e8\:5206\:ff0c\:5373\:4f20\:64ad\:5b50-----------------------*)
splt[{fyTag,"cls"}]=preFactor*fad[
{k,\[CapitalLambda]}->-4,{k,mm1}->-1,{p1-k,md1}->-1,{p2-k,mo1}->-1
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spr",\[Mu]}]=(FV[k,\[Alpha]] . \[CapitalTheta][\[Alpha],\[Beta]] . FV[-\[CapitalDelta],\[Nu]] . (
decGator[{\[Beta],\[Nu]},{p2-k,md1}] . GA[\[Mu]] . GA5-decGator[{\[Beta],\[Mu]},{p2-k,md1}] . GA[\[Nu]] . GA5) .
(GS[p1-k]+mo1) . GS[k] . GA5);
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fyTag,"FAFB","spr"}]=projToFAFB[splt[{fyTag,"spr",\[Mu]}],\[Nu]];


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*)
,SP[k,\[CapitalDelta]]->1/2(fad[{k,\[CapitalLambda]}->1]-(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2))
,SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2))
,SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p2-k,mo2}->1]+mo2^2-mN^2))
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
splt[{fyTag,"FAFB","integ"}]=Collect[
Expand[
splt[{fyTag,"cls"}]*
(splt[{fyTag,"FAFB","spr"}]/.ruleScalar)
],
fadTmp1[__],Simplify];


(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)
splt[{fyTag,"FAFB","integ"}]//First//gatorShow


splt[{fyTag,"FAFB","lcone"}]=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@splt[{fyTag,"FAFB","integ"}];


splt[{fyTag,"FAFB","res"}]=Query[
(*<propgator\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@{
{k,\[CapitalLambda]},{k,mm1},
{p2-k,mo2}
}
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570----------------*)
,FAFBResidue[#,splt[{fyTag,"FAFB","lcone"}]]&
]@gatorZeros;


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2 *)
splt[{fyTag,"F1F2","res","tmp1"}]=projToF1F2/@splt[{fyTag,"FAFB","res"}];
(*\:7ffb\:8f6c\:6b21\:5e8f----*)
splt[{fyTag,"F1F2","res"}]=Table[splt[{fyTag,"F1F2","res","tmp1"}][[All,ffs]],{ffs,2}];


(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)
splt[{fyTag,"F1F2","pw"}]=Query[(*{f1f2}*)All,
(*<propgator\[Rule]expr>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570--------------*)
Piecewise[{
{
(-2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1}}]],
-\[Xi]<=y<1-2\[Xi]
},
{
(2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1},Key@{p1-k,mo1}}]],
1-2\[Xi]<=y<=1
}
}]&
]@splt[{fyTag,"F1F2","res"}];


(* ::Input:: *)
(*(*\:5bfc\:51fa\:5230\:786c\:76d8*)*)
(*serialize[gpdResidueDir][Flatten@{"residue-F1F2",fyTag,".wdx"},splt[{fyTag,"F1F2","pw"}]]*)


(* ::Section::Closed:: *)
(*Kroll-Ruderman, A-meson,decuplet,left*)


(* ::Input:: *)
(*diagIllus@chTag@{"KR","mes","dec","left"}*)


(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"KR","mes","dec","left"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*-\:5708\:79ef\:5206\:7684\:5206\:6bcd\:90e8\:5206\:ff0c\:5373\:4f20\:64ad\:5b50-----------------------*)
splt[{fyTag,"cls"}]=preFactor*fad[
{k,\[CapitalLambda]}->-4,{k,mm1}->-1,{p2-k,md1}->-1
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spr",\[Mu]}]=FV[k,\[Rho]] . \[CapitalTheta][\[Rho],\[Beta]] . decGator[{\[Beta],\[Nu]},{p2-k,md1}] . \[CapitalTheta][\[Nu],\[Mu]];
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fyTag,"FAFB","spr"}]=projToFAFB[splt[{fyTag,"spr",\[Mu]}],\[Nu]];


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*)
,SP[k,\[CapitalDelta]]->1/2(fad[{k,\[CapitalLambda]}->1]-(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2))
,SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2))
,SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p2-k,mo2}->1]+mo2^2-mN^2))
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
splt[{fyTag,"FAFB","integ"}]=Collect[
Expand[
splt[{fyTag,"cls"}]*
(splt[{fyTag,"FAFB","spr"}]/.ruleScalar)
],
fadTmp1[__],Simplify];


(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)
splt[{fyTag,"FAFB","integ"}]//First//gatorShow


splt[{fyTag,"FAFB","lcone"}]=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@splt[{fyTag,"FAFB","integ"}];


splt[{fyTag,"FAFB","res"}]=Query[
(*<propgator\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@{
{k,\[CapitalLambda]},{k,mm1},
{p2-k,mo2}
}
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570----------------*)
,FAFBResidue[#,splt[{fyTag,"FAFB","lcone"}]]&
]@gatorZeros;


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2 *)
splt[{fyTag,"F1F2","res","tmp1"}]=projToF1F2/@splt[{fyTag,"FAFB","res"}];
(*\:7ffb\:8f6c\:6b21\:5e8f----*)
splt[{fyTag,"F1F2","res"}]=Table[splt[{fyTag,"F1F2","res","tmp1"}][[All,ffs]],{ffs,2}];


(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)
splt[{fyTag,"F1F2","pw"}]=Query[(*{f1f2}*)All,
(*<propgator\[Rule]expr>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570--------------*)
Piecewise[{
{
(-2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1}}]],
-\[Xi]<=y<1-2\[Xi]
},
{
(2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1},Key@{p1-k,mo1}}]],
1-2\[Xi]<=y<=1
}
}]&
]@splt[{fyTag,"F1F2","res"}];


(* ::Input:: *)
(*(*\:5bfc\:51fa\:5230\:786c\:76d8*)*)
(*serialize[gpdResidueDir][Flatten@{"residue-F1F2",fyTag,".wdx"},splt[{fyTag,"F1F2","pw"}]]*)


(* ::Section:: *)
(*Kroll-Ruderman, A-meson,decuplet,right*)


(* ::Input:: *)
(*diagIllus@chTag@{"KR","mes","dec","right"}*)


(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"KR","mes","dec","right"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*-\:5708\:79ef\:5206\:7684\:5206\:6bcd\:90e8\:5206\:ff0c\:5373\:4f20\:64ad\:5b50-----------------------*)
splt[{fyTag,"cls"}]=preFactor*fad[
{k,\[CapitalLambda]}->-4,{k,mm1}->-1,{p1-k,md1}->-1
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spr",\[Mu]}]=\[CapitalTheta][\[Mu],\[Nu]] . decGator[{\[Nu],\[Rho]},{p1-k,md1}] . \[CapitalTheta][\[Rho],\[Beta]] . FV[k,\[Beta]];
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fyTag,"FAFB","spr"}]=projToFAFB[splt[{fyTag,"spr",\[Mu]}],\[Nu]];


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*)
,SP[k,\[CapitalDelta]]->1/2(fad[{k,\[CapitalLambda]}->1]-(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2))
,SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2))
,SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p2-k,mo2}->1]+mo2^2-mN^2))
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
splt[{fyTag,"FAFB","integ"}]=Collect[
Expand[
splt[{fyTag,"cls"}]*
(splt[{fyTag,"FAFB","spr"}]/.ruleScalar)
],
fadTmp1[__],Simplify];


(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)
splt[{fyTag,"FAFB","integ"}]//First//gatorShow


splt[{fyTag,"FAFB","lcone"}]=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@splt[{fyTag,"FAFB","integ"}];


splt[{fyTag,"FAFB","res"}]=Query[
(*<propgator\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@{
{k,\[CapitalLambda]},{k,mm1},
{p2-k,mo2}
}
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570----------------*)
,FAFBResidue[#,splt[{fyTag,"FAFB","lcone"}]]&
]@gatorZeros;


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2 *)
splt[{fyTag,"F1F2","res","tmp1"}]=projToF1F2/@splt[{fyTag,"FAFB","res"}];
(*\:7ffb\:8f6c\:6b21\:5e8f----*)
splt[{fyTag,"F1F2","res"}]=Table[splt[{fyTag,"F1F2","res","tmp1"}][[All,ffs]],{ffs,2}];


(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)
splt[{fyTag,"F1F2","pw"}]=Query[(*{f1f2}*)All,
(*<propgator\[Rule]expr>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570--------------*)
Piecewise[{
{
(-2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1}}]],
-\[Xi]<=y<1-2\[Xi]
},
{
(2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1},Key@{p1-k,mo1}}]],
1-2\[Xi]<=y<=1
}
}]&
]@splt[{fyTag,"F1F2","res"}];


(* ::Input:: *)
(*(*\:5bfc\:51fa\:5230\:786c\:76d8*)*)
(*serialize[gpdResidueDir][Flatten@{"residue-F1F2",fyTag,".wdx"},splt[{fyTag,"F1F2","pw"}]]*)


(* ::Section::Closed:: *)
(*Kroll-Ruderman, A-meson,decuplet,addition,left*)


(* ::Input:: *)
(*diagIllus@chTag@{"KR","mes","dec","add","left"}*)


(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"KR","mes","dec","add","left"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*-\:5708\:79ef\:5206\:7684\:5206\:6bcd\:90e8\:5206\:ff0c\:5373\:4f20\:64ad\:5b50-----------------------*)
splt[{fyTag,"cls"}]=preFactor*fad[
{k,\[CapitalLambda]}->-2,{k,mm1}->-1,{p2-k,md1}->-1
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spr",\[Mu]}]=FV[k,\[Alpha]] . \[CapitalTheta][\[Alpha],\[Beta]] . decGator[{\[Beta],\[Nu]},{p2-k,md1}] . \[CapitalTheta][\[Nu],\[Rho]] . FV[k+\[CapitalDelta],\[Rho]];
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fyTag,"FAFB","spr"}]=projToFAFB[splt[{fyTag,"spr",\[Mu]}],\[Nu]];


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*)
,SP[k,\[CapitalDelta]]->1/2(fad[{k,\[CapitalLambda]}->1]-(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2))
,SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2))
,SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p2-k,mo2}->1]+mo2^2-mN^2))
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
splt[{fyTag,"FAFB","integ"}]=Collect[
Expand[
splt[{fyTag,"cls"}]*
(splt[{fyTag,"FAFB","spr"}]/.ruleScalar)
],
fadTmp1[__],Simplify];


(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)
splt[{fyTag,"FAFB","integ"}]//First//gatorShow


splt[{fyTag,"FAFB","lcone"}]=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@splt[{fyTag,"FAFB","integ"}];


splt[{fyTag,"FAFB","res"}]=Query[
(*<propgator\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@{
{k,\[CapitalLambda]},{k,mm1},
{p2-k,mo2}
}
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570----------------*)
,FAFBResidue[#,splt[{fyTag,"FAFB","lcone"}]]&
]@gatorZeros;


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2 *)
splt[{fyTag,"F1F2","res","tmp1"}]=projToF1F2/@splt[{fyTag,"FAFB","res"}];
(*\:7ffb\:8f6c\:6b21\:5e8f----*)
splt[{fyTag,"F1F2","res"}]=Table[splt[{fyTag,"F1F2","res","tmp1"}][[All,ffs]],{ffs,2}];


(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)
splt[{fyTag,"F1F2","pw"}]=Query[(*{f1f2}*)All,
(*<propgator\[Rule]expr>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570--------------*)
Piecewise[{
{
(-2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1}}]],
-\[Xi]<=y<1-2\[Xi]
},
{
(2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1},Key@{p1-k,mo1}}]],
1-2\[Xi]<=y<=1
}
}]&
]@splt[{fyTag,"F1F2","res"}];


(* ::Input:: *)
(*(*\:5bfc\:51fa\:5230\:786c\:76d8*)*)
(*serialize[gpdResidueDir][Flatten@{"residue-F1F2",fyTag,".wdx"},splt[{fyTag,"F1F2","pw"}]]*)


(* ::Section:: *)
(*Kroll-Ruderman, A-meson,decuplet,addition,right*)


(* ::Input:: *)
(*diagIllus@chTag@{"KR","mes","dec","add","right"}*)


(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"KR","mes","dec","add","right"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*-\:5708\:79ef\:5206\:7684\:5206\:6bcd\:90e8\:5206\:ff0c\:5373\:4f20\:64ad\:5b50-----------------------*)
splt[{fyTag,"cls"}]=preFactor*fad[
{k,\[CapitalLambda]}->-2,{k,mm1}->-1,{p1-k,md1}->-1
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spr",\[Mu]}]=FV[k-\[CapitalDelta],\[Rho]] . \[CapitalTheta][\[Rho],\[Nu]] . decGator[{\[Nu],\[Alpha]},{p1-k,md1}] . \[CapitalTheta][\[Alpha],\[Beta]] . FV[k,\[Beta]];
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fyTag,"FAFB","spr"}]=projToFAFB[splt[{fyTag,"spr",\[Mu]}],\[Nu]];


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*)
,SP[k,\[CapitalDelta]]->1/2(fad[{k,\[CapitalLambda]}->1]-(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2))
,SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2))
,SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p2-k,mo2}->1]+mo2^2-mN^2))
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
splt[{fyTag,"FAFB","integ"}]=Collect[
Expand[
splt[{fyTag,"cls"}]*
(splt[{fyTag,"FAFB","spr"}]/.ruleScalar)
],
fadTmp1[__],Simplify];


(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)
splt[{fyTag,"FAFB","integ"}]//First//gatorShow


splt[{fyTag,"FAFB","lcone"}]=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@splt[{fyTag,"FAFB","integ"}];


splt[{fyTag,"FAFB","res"}]=Query[
(*<propgator\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@{
{k,\[CapitalLambda]},{k,mm1},
{p2-k,mo2}
}
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570----------------*)
,FAFBResidue[#,splt[{fyTag,"FAFB","lcone"}]]&
]@gatorZeros;


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2 *)
splt[{fyTag,"F1F2","res","tmp1"}]=projToF1F2/@splt[{fyTag,"FAFB","res"}];
(*\:7ffb\:8f6c\:6b21\:5e8f----*)
splt[{fyTag,"F1F2","res"}]=Table[splt[{fyTag,"F1F2","res","tmp1"}][[All,ffs]],{ffs,2}];


(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)
splt[{fyTag,"F1F2","pw"}]=Query[(*{f1f2}*)All,
(*<propgator\[Rule]expr>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570--------------*)
Piecewise[{
{
(-2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1}}]],
-\[Xi]<=y<1-2\[Xi]
},
{
(2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1},Key@{p1-k,mo1}}]],
1-2\[Xi]<=y<=1
}
}]&
]@splt[{fyTag,"F1F2","res"}];


(* ::Input:: *)
(*(*\:5bfc\:51fa\:5230\:786c\:76d8*)*)
(*serialize[gpdResidueDir][Flatten@{"residue-F1F2",fyTag,".wdx"},splt[{fyTag,"F1F2","pw"}]]*)
