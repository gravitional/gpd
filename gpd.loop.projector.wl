(* ::Package:: *)

(* ::Title:: *)
(*gpd.loop.projector.wl*)


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
(*\:5149\:9525\:5750\:6807\:7cfb\:51fd\:6570------------------*)
Get["gpd.loop.lightcone.wl"];
(*make Propagator Dataset*)
Get["gpd.loop.poles.wl"];


(* ::Section:: *)
(*symbols*)


(*\:516b\:91cd\:6001\:90e8\:5206\:547d\:540d------------*)
{
(*\:4ecb\:5b50\:8870\:53d8\:5e38\:6570*)
f
(*\:521d\:672b\:6001\:6838\:5b50\:8d28\:91cf;\:4ecb\:5b50\:8d28\:91cf;\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf*)
,mN,mm1,mo1,mo2
(*MB-mN; mN+MB*)
,\[CapitalDelta],mSum
};


(*\:5149\:9525\:53c2\:6570\:7684\:53d6\:503c\:8303\:56f4*)
lConeParaConstraint={0<=\[Xi]<=1,-\[Xi]<=y<=1};


ruleKmReduce={P[1]->1};


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
splt[{fyTag,"class"}]=preFactor*fad[
{k-\[CapitalDelta],\[CapitalLambda]}->-2,{k,\[CapitalLambda]}->-2,{k-\[CapitalDelta],mm1}->-1,{k,mm1}->-1,{p1-k,mo1}->-1
]
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spr",\[Mu]}]=(FV[2k-\[CapitalDelta],\[Mu]] . GS[k-\[CapitalDelta]] . GA5 . (GS[p1-k]+mo1) . GS[k] . GA5);
(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf-----------------*)
splt[{fyTag,"FAFB","spr"}]=projToFAFB[splt[{fyTag,"spr",\[Mu]}],\[Nu]];


pickPoles=AssociationThread[
{"itv1","itv2"},
Query[All,KeyTake[{"propa","pow","kmCoe"}]
]@{
Select[#,#[["itvl1","up"]]&],
Select[#,#[["itvl2","up"]]&]
}
]&@propaPoles@splt[{fyTag,"class"}];
(*----------------*)
regularPoles=Query[All,All,"propa"]@pickPoles


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
ruleScalar=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2,(*\:5bf9k.k\:4f5c\:66ff\:6362*)
SP[k,\[CapitalDelta]]->1/2(fad[{k,\[CapitalLambda]}->1]-(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2)),
SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2)),(*\:5bf9k.p1\:4f5c\:66ff\:6362*)
SP[k,p2]->1/2(fad[{k-\[CapitalDelta],\[CapitalLambda]}->1]-\[CapitalDelta]2+\[CapitalLambda]^2-(fad[{p1-k,mo1}->1]+mo1^2-mN^2))
}];


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
spltTmp=Expand[
splt[{fyTag,"class"}]*
(splt[{fyTag,"FAFB","spr"}]/.ruleScalar)
];
(*\:72ec\:7acb\:79ef\:5206\:5f0f\[Rule]\:7cfb\:6570 \:7684\:5173\:8054*)
spltIntAssoc=findIntegrate/@spltTmp;
(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
spltIntScalar=Collect[spltTmp,fadTmp1[__],Simplify];


(* ::Input:: *)
(*(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)*)
(*spltTmp//First//gatorShow*)


picDeltaContrib/@spltIntAssoc


spltIntLCone=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@spltIntScalar;


(*\:8fd4\:56de\:7684\:7ed3\:679c\:662f <pole->{FAFB}..>*)
spltIntResidue=Query[
(*<propa-poles\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@DeleteDuplicates@Values@Map[Splice]@regularPoles
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570---------------*)
,FAFBResidue[#,spltIntLCone]&
]@gatorZeros;


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2, <pole->{FAFB}..>*)
spltF1F2ResTmp=projToF1F2/@spltIntResidue;
(*\:7ffb\:8f6c\:6b21\:5e8f----*)
spltF1F2Res=Table[spltF1F2ResTmp[[All,ffs]],{ffs,2}];


splt[{fyTag,"F1F2","pw"}]=Query[
(*<propa-poles\[Rule]{FAFB}>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570*)All
,(KeyTake[spltF1F2ResTmp,#]&)
]@regularPoles;


(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)
splt[{fyTag,"F1F2","pw"}]=Query[
(*<propa-poles\[Rule]{FAFB}>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570*)
All,
(*{FAFB}------*)
Piecewise[{
{
-1<=y<=-\[Xi],
0
},
{
(-2\[Pi]*I)Total@#[[{Key@{k,\[CapitalLambda]},Key@{k,mm1}}]],
-\[Xi]<=y<\[Xi]
},
{
(2\[Pi]*I)Total@#[[{Key@{p1-k,mo1}}]],
\[Xi]<=y<=1
}
}]&
]@spltF1F2Res;


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


(* ::Chapter:: *)
(*backup*)


(* ::Section:: *)
(*recordPropa*)


(* ::Input:: *)
(*(*\:4f20\:64ad\:5b50 Dataset \:7684 record. *)
(*\:6c42 km \:5373 k[2] \:7684\:7cfb\:6570\:ff0clConeKinematics \:662f\:5177\:4f53\:7684\:5149\:9525\:53c2\:6570\:5316,*)
(*ruleReduce \:662f\:5bf9\:7cfb\:6570\:5316\:7b80\:7684\:89c4\:5219, \:6bd4\:5982\:53bb\:6389\:6574\:4f53\:7684 P[1] \:5373 P+.*)
(*\:4f20\:64ad\:5b50\:662f k^2-m^2+I \[CurlyEpsilon]\:ff0c\:6240\:4ee5 k- \:6781\:70b9\:7684\:4f4d\:7f6e \:53d6\:51b3\:4e8e \:7cfb\:6570\:7684\:6b63\:8d1f:*)
(*\:6b63\:7cfb\:6570\[Rule]\:4e0b\:534a\:5e73\:9762\:ff0c\:8d1f\:7cfb\:6570\[Rule] \:4e0a\:534a\:5e73\:9762, \:7cfb\:6570\:4e3a\:96f6->\:5947\:5f02\:70b9*)*)
(*recordPropa[km_,lConeKinematics_,ruleReduce_:{}][propa_->pow_]:=Module[*)
(*{mom,kmCoe,poleDistribute},*)
(*(*----------*)*)
(*mom=First@propa;*)
(*kmCoe=Simplify[*)
(*Coefficient[(*Echo@*)lightConeSP[mom,mom]/.lConeKinematics,km]*)
(*/.ruleReduce];*)
(*(**)*)
(*poleDistribute=AssociationThread[*)
(*intervalPart,*)
(*(*-----------*)*)
(*Simplify[{*)
(*kmCoe<0,(*pole \:5728\:4e0a\:65b9*)*)
(*kmCoe>0,(*pole \:5728\:4e0b\:65b9*)*)
(*kmCoe==0(*k- \:7cfb\:6570\:4e3a\:96f6\:ff0c\:5947\:5f02\:70b9*)*)
(*},*)
(*{#}]&/@{*)
(*-\[Xi]<y<\[Xi]&&0<\[Xi]<=1*)
(*,\[Xi]<y<1&&0<=\[Xi]<1*)
(*,0<\[Xi]<1&&y==\[Xi]*)
(*,0<\[Xi]<1&&y==-\[Xi]*)
(*,y==\[Xi]==0*)
(*,y==\[Xi]==1*)
(*}];*)
(*(****)If[pow==0,*)
(*Nothing,*)
(*<|*)
(*"propa"->propa,*)
(*"pow"->pow,*)
(*"kmCoe"->kmCoe,*)
(*"pole"->poleDistribute*)
(*|>*)
(*]*)
(*]*)


(* ::Section:: *)
(*findIntegrate*)


(* ::Input:: *)
(*(*\:53bb\:6389\:6240\:6709\:672a\:5b9a\:4e49\:60c5\:51b5*)*)
(*undef/:{expr1___,undef,expr2___}:={undefined}*)
(*sing/:{Except[sing]...,sing..,Except[sing]...}:={0}*)
