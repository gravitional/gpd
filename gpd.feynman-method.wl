(* ::Package:: *)

(* ::Title:: *)
(*gpd.feynman-method.wl*)


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
(*\:8d39\:66fc\:53c2\:6570\:5316\:76f8\:5173\:7684\:51fd\:6570\:ff0c\:66ff\:6362\:89c4\:5219\:ff0c\:5708\:79ef\:5206\:5316\:7b80 ----------------*)
Get["gpd.feynman-functions.wl"];


(* ::Chapter:: *)
(*Loop integral*)


(* ::Section:: *)
(*Rainbow,meson,octet*)


(* ::Input:: *)
(*diagIllus@chTag@{"RB","mes","oct"}*)


(*\:6d89\:53ca\:5230\:7684\:52a8\:91cf\:ff0c\:5747\:7528\:5916\:52a8\:91cf p1,p2 \:8868\:793a\:ff0c\:4fbf\:4e8e\:5e94\:7528\:72c4\:62c9\:514b\:65b9\:7a0b\:5316\:7b80\:65cb\:91cf *)
(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"RB","mes","oct"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splitSpin=DiracSimplify[
ExpandScalarProduct[
SpinorUBar[p2,mN] . ( 
FV[2k1-\[CapitalDelta],\[Mu]] . GS[k1-\[CapitalDelta]] . GA5 . (GS[p1-k1]+mo1) . GS[k1] . GA5
) .
SpinorU[p1,mN]/.ruleMomToExternal
],
DiracEquation->True
];


(*\:4f20\:64ad\:5b50\:5217\:8868------------------*)
splitPropag={
feynPropg[k1-\[CapitalDelta],\[CapitalLambda],2],feynPropg[k1,\[CapitalLambda],2]
,feynPropg[k1-\[CapitalDelta],mm1],feynPropg[k1,mm1]
,feynPropg[p1-k1,mo1]
}/.ruleMomToExternal;


(*\:8c03\:7528\:5bf9\:8d39\:66fc\:56fe\:7684\:5206\:6790\:7a0b\:5e8f--------------*)
Get["gpd.feynman-subrout.wl"];


(* ::Input:: *)
(*(*\:67e5\:770b\:5206\:5b50\:4e2d\:7684\:65cb\:91cf\:5316\:7b80\:7684\:7ed3\:679c*)*)
(*Simplify[*)
(*dropSpinor@splitSpin/.ruleTeX*)
(*]*)


(* ::Input:: *)
(*(*\:67e5\:770b\:5206\:6bcd\:4e2d\:4f20\:64ad\:5b50\:7684\:5206\:6790\:7ed3\:679c*)*)
(*{feynIntBasis*)
(*,splitPropag*)
(*,feynIntBasisNum*)
(*,feynIntPowerSum*)
(*}/.ruleTeX//Column*)


(* ::Input:: *)
(*(*\:8d39\:66fc\:53c2\:6570\:5316\:4e4b\:540e\:7684\:4f20\:64ad\:5b50\:914d\:9f50\:4e8c\:6b21\:578b*)*)
(*splitPropagSquared/.ruleTeX//TableForm*)


(* ::Input:: *)
(*(*\:4f7f\:7528\:72c4\:62c9\:514b\:65b9\:7a0b\:ff0c\:5708\:79ef\:5206\:5bf9\:79f0\:6027\:ff0c\:5316\:7b80\:5708\:79ef\:5206\:7684\:7ed3\:679c*)*)
(*splitSpinTmp/.ruleTeX*)


(* ::Input:: *)
(*(*\:5316\:7b80\:540e\:5f97\:5230\:7684\:5404\:79cd\:7ed3\:6784-----------*)*)
(*split[{fyTag,"FAFB"}]/.ruleTeX*)


(* ::Section:: *)
(*test*)


(*\:5c06\:7236\:6bcd\:4e2d\:7684 l^2 \:66ff\:6362\:6210\:4f20\:64ad\:5b50\:548c\:5e38\:6570\:7684\:7ec4\:5408*)
teFA=Expand[(
split[{fyTag,"FAFB"}][["P\[Mu]"]]/.{
FCI@SP[l,l]->fad[{l,\[Beta]}->-1]+\[Beta]}
)*fad[{l,\[Beta]}->-feynIntPowerSum]
]//Simplify


lCone\[Delta]Fun[{l_,m2_}->n_]:=(I*\[Pi])/(-n-1)!(D[
(*\[CapitalLambda]cut \:662f\:4e0d\:5f71\:54cd\:8ba1\:7b97\:7684\:65e0\:5173\:5e38\:6570*)
FCI[(
m2*Log[\[CapitalLambda]cut/(ed[l@t,l@t]+m2)]+ed[l@t,l@t]+m2)(**DiracDelta[FV[l,p]]*)
]
,{m2,-n}])/;n<=-1


tea=teFA/.fadTmp1->lCone\[Delta]Fun//Simplify


(* ::Subsection:: *)
(*ssss*)


(*\:5149\:9525\:53d1\:6563\:79ef\:5206\:7ed3\:679c\:6b63\:6bd4\:4e8e \[Delta][l+], \:7b49\:4ef7\:4e8e\:5bf9{\:8d39\:66fc\:53c2\:6570,y,\[Xi]}\:7684\:7ea6\:675f*)
LC\[Delta]TermConstrain=(ExpandScalarProduct[
FCI@FV[l,p]/.ruleMomentShiftBack/.ruleTransMomShiftBack
]/.lConeKinematics)/.rulePlusMom\[Delta]Reduce//Simplify


(*\[Delta][..]\:5c06\:9650\:5236\:8d39\:66fc\:53c2\:6570\:7684\:53d6\:503c, \:53bb\:6389\:6700\:540e\:4e00\:4e2a\:8d39\:66fc\:53c2\:6570*)
ruleLC\[Delta]Constrain=First[
Solve@@({LC\[Delta]TermConstrain==0,f[feynIntBasisNum-1]}/.ruleFeynParaToSymbol)
];
(*\:63d0\:53d6\:51fa\:4f8b\:5982 f4 \:7684\:8868\:8fbe\:5f0f\:ff1a\:6700\:672b\:5c3e\:8d39\:66fc\:53c2\:6570\:7684\:8868\:8fbe\:5f0f --------*)
fyParaExprLast1=ruleLC\[Delta]Constrain//First//Last;
fyParaExprLast2=f[feynIntBasisNum-2]/.ruleFeynParaToSymbol;


lConeKinematics
ruleTransMomShiftBack


teb=Assuming[\[Beta]>0,
Integrate[
(2\[Pi])ltr*tea/.{ed[l[t],l[t]]->ltr^2}
,{ltr,0,\[Infinity]}]
];


(*\:5206\:6bb5\:51fd\:6570\:ff0c\:6ee1\:8db3 \[Delta][l+] \:9650\:5236\:624d\:6709\:503c\:ff0c\:5426\:5219\:4e3a\:96f6*)
tec=Piecewise[{
{teb/.{\[Beta]->denominator}/.ruleFeynParaToSymbol,
0<=fyParaExprLast1<=fyParaExprLast2(*&& \[CapitalDelta]2\[LessEqual](-4\[Xi]^2 mN^2)/(1-\[Xi]^2)*)
}
}];


ted=Table[
NIntegrate[
tec/.ruleLC\[Delta]Constrain/.{
mN->0.939,mo1->0.939,mm1->0.139,\[CapitalLambda]->0.9,\[CapitalDelta]2->dnum
,\[Xi]->0.1,y->ynum
}
,{f1,f2,f3}\[Element]KuhnSimplex[3]
]
,{dnum,Range[-0.2,-0.4,-0.05]}
,{ynum,Range[0.1,0.3,0.05]}
]


ListPlot[Im/@ted
,PlotLegends->Automatic,
PlotRange->Full
]


(* ::Chapter:: *)
(*sdafdws*)


tew=(
ExpandScalarProduct[
FCI@teq/.ruleMomentShiftBack/.ruleTransMomShiftBack
]/.lConeKinematics
)/.FCI@{
ed[FV[k,t],FV[k,t]]->ktr^2,
ed[FV[k,t],FV[\[CapitalDelta]T,t]]->ktr Sqrt[(-4mN^2 \[Xi]^2+(1-\[Xi]^2)(-\[CapitalDelta]2))] Cos[\[Phi]],
ed[FV[\[CapitalDelta]T,t],FV[\[CapitalDelta]T,t]]->(-4mN^2 \[Xi]^2+(1-\[Xi]^2)(-\[CapitalDelta]2))
}


(* ::Input:: *)
(*ted=tec/.ruleLC\[Delta]Constrain/.{*)
(*mN->0.939,mo1->0.939,mm1->0.139,\[CapitalLambda]->0.9,\[CapitalDelta]2->-0.2*)
(*,\[Xi]->0.1,y->0.2,f1->0.8*)
(*}*)


(* ::Input:: *)
(*Plot3D[Im@ted*)
(*,{f2,0,0.8}*)
(*,{f3,0,0.8}*)
(*,PlotRange->Automatic,*)
(*AxesLabel->Automatic,*)
(*ImageSize->Large*)
(*]*)


(* ::Chapter::Closed:: *)
(*test*)


(* ::Input:: *)
(*(*Peskin6.3,Higgs \:73bb\:8272\:5b50\:5bf91\[Dash]loop QED \:9876\:70b9\:7684\:4fee\:6b63*)*)
(*tet=dropSpinor[*)
(*DiracSimplify[*)
(*ScalarProductExpand[*)
(*FCE@DiracSimplify[*)
(*SpinorUBar[p2,mN] . (GS[k+p2-p]+mN) . GA[\[Mu]] . (GS[k]+mN) . SpinorU[p,mN]*)
(*]/.{*)
(*FV[k,\[Mu]]->FV[l,\[Mu]]+x*FV[p,\[Mu]]-y(FV[p2-p,\[Mu]]),*)
(*GS[k]->GS[l]+x*GS[p]-y*GS[p2-p],*)
(*SP[k,p]->SP[l,p]+x*SP[p,p]-y*SP[p2-p,p],*)
(*SP[k,k]->( *)
(*SP[l,l]+x^2*SP[p,p]+y^2*SP[p2-p,p2-p]*)
(*+2(x*SP[l,p]-y*SP[l,p2-p]-x*y*SP[p,p2-p])*)
(*)*)
(*}*)
(*]]]*)


(* ::Input:: *)
(*(*\:6d88\:53bb l \:7684\:5947\:6b21\:5e42*)*)
(*Simplify[*)
(*1/2(Expand[tet/.{l->-l}]+Expand[tet])*)
(*]*)


(* ::Chapter:: *)
(*backup*)


(* ::Input:: *)
(*(*\:5e94\:7528 Ward \:6052\:7b49\:5f0f\:ff0c\:4ee5\:53ca\:6839\:636e\:5708\:79ef\:5206\:5947\:5076\:6027\:ff0c\:91cf\:7eb2\:63a8\:5bfc\:7684\:66ff\:6362\:89c4\:5219*)*)
(*refineLoop[numerator_]:=Nest[*)
(*(*\:628a\:79ef\:5206\:4e2d\:5206\:5b50\:66ff\:6362\:6210\:5316\:7b80\:7684\:7ed3\:679c,\:4f7f\:7528 Ward-Identity \:7b49\:7ed3\:679c --------*)*)
(*ReplaceAll[ruleIntNumerator],*)
(*(*\:5c55\:5f00\:5206\:5b50\:4e2d\:7684\:6807\:91cf\:4e58\:79ef-----------*)*)
(*ExpandAll2[*)
(*ExpandScalarProduct[*)
(*DiracSimplify[*)
(*(*\:4f5c\:52a8\:91cf\:5e73\:79fbk\[Rule]l\:ff0c\:518d\:6b21\:4f7f\:7528 Dirac\:65b9\:7a0b\:5316\:7b80*)*)
(*FCI@numerator/.ruleMomentShift,*)
(*DiracEquation->True*)
(*(*\:51c6\:5907\:4f7f\:7528 Ward-Identity\:ff0c\:53bb\:6389\:6b63\:6bd4\:4e8e \[CapitalDelta]\[Mu] \:7684\:9879*)*)
(*]/.ruleMomentCombine*)
(*]]*)
(*(*\:91cd\:590d\:66ff\:6362\:4e09\:6b21------*)*)
(*,2]*)


(* ::Input:: *)
(*(*\:666e\:901a\:6570\:5b57\:7684\:4e8c\:6b21\:591a\:9879\:5f0f\:914d\:9f50-----------------*)*)
(*toSquare[expr_,k_,l_]:=Module[*)
(*{a=Coefficient[expr,k,2],*)
(*b=Coefficient[expr,k,1],*)
(*c=Coefficient[expr,k,0]*)
(*},*)
(*(*\:52a8\:91cf\:914d\:9f50\:9700\:8981\:6ee1\:8db3\:7684\:5173\:7cfb---------*)*)
(*{*)
(*l->k+b/(2a),*)
(*k->l-b/(2a),*)
(*-l^2->b^2/(4a)-c*)
(*}*)
(*]*)


(* ::Section::Closed:: *)
(*lConeKinematics,FV,number*)


(* ::Input:: *)
(*lConeKinematics=FCI@{*)
(*(*\:8d28\:5b50\:521d\:6001\:52a8\:91cf-------------------*)*)
(*FV[p1,1]->(1+\[Xi])FV[P,1],*)
(*FV[p1,2]->(mN^2+ed[FV[\[CapitalDelta],3],FV[\[CapitalDelta],3]]/4)/(2(1+\[Xi])FV[P,1]),*)
(*FV[p1,3]->FV[\[CapitalDelta],3]/2,*)
(*(*\:4ecb\:5b50\:521d\:6001\:52a8\:91cf*)*)
(*FV[k,1]->(y+\[Xi])FV[P,1],*)
(*(*k[2]\[Rule]k[2],*)*)
(*FV[k,3]->FV[k,3]+FV[\[CapitalDelta],3]/2,*)
(*(*\:8d28\:5b50\:672b\:6001\:52a8\:91cf------------------*)*)
(*FV[p2,1]->(1-\[Xi])FV[P,1],*)
(*FV[p2,2]->(mN^2+ed[FV[\[CapitalDelta],3],FV[\[CapitalDelta],3]]/4)/(2(1-\[Xi])FV[P,1]),*)
(*FV[p2,3]->-FV[\[CapitalDelta],3]/2,*)
(*(*\:4ecb\:5b50\:672b\:6001\:52a8\:91cf*)*)
(*FV[k2,1]->(y-\[Xi])FV[P,1],*)
(*(*k2[2]\[Rule]k2[2],*)*)
(*FV[k2,3]->FV[k,3]-FV[\[CapitalDelta],3]/2,*)
(**)
(*FV[\[CapitalDelta],1]->\[Xi](2FV[P,1]),*)
(*FV[\[CapitalDelta],2]->(\[CapitalDelta]2+ed[FV[\[CapitalDelta],3],FV[\[CapitalDelta],3]])/(4\[Xi]*FV[P,1])*)
(*(*\[CapitalDelta][3]\[Rule]\[CapitalDelta][3]*)*)
(*};*)


(* ::Section::Closed:: *)
(*lConeKinematics,FV*)


(* ::Input:: *)
(*(*gpd \:4e2d\:4f7f\:7528\:7684\:5149\:9525\:52a8\:91cf\:53c2\:6570\:5316*)*)
(*lConeKinematics=FCI@{*)
(*(*\:8d28\:5b50\:521d\:6001\:52a8\:91cf-------------------*)*)
(*FV[p1,p]->(1+\[Xi])FV[P,p],*)
(*FV[p1,m]->(mN^2+SP[\[CapitalDelta]T,\[CapitalDelta]T]/4)/(2(1+\[Xi])FV[P,p]),*)
(*FV[p1,t]->FV[\[CapitalDelta]T,t]/2,*)
(*(*\:4ecb\:5b50\:521d\:6001\:52a8\:91cf*)*)
(*FV[k1,p]->(y+\[Xi])FV[P,p],*)
(*(*k1[m]\[Rule]k[2],*)*)
(*FV[k1,t]->FV[k,t]+FV[\[CapitalDelta]T,t]/2,*)
(*(*\:8d28\:5b50\:672b\:6001\:52a8\:91cf------------------*)*)
(*FV[p2,p]->(1-\[Xi])FV[P,p],*)
(*FV[p2,m]->(mN^2+SP[\[CapitalDelta]T,\[CapitalDelta]T]/4)/(2(1-\[Xi])FV[P,p]),*)
(*FV[p2,t]->-FV[\[CapitalDelta]T,t]/2,*)
(*(*\:4ecb\:5b50\:672b\:6001\:52a8\:91cf*)*)
(*FV[k2,p]->(y-\[Xi])FV[P,p],*)
(*(*k2[m]\[Rule]k2[m],*)*)
(*FV[k2,t]->FV[k,t]-FV[\[CapitalDelta]T,t]/2,*)
(*(*\[CapitalDelta]=p1-(p2 --------------------*)*)
(*FV[\[CapitalDelta],p]->\[Xi](2FV[P,p]),*)
(*FV[\[CapitalDelta],m]->(\[CapitalDelta]2+SP[\[CapitalDelta]T,\[CapitalDelta]T])/(4\[Xi]*FV[P,p]),*)
(*FV[\[CapitalDelta],t]->\[CapitalDelta]T*)
(*};*)


(* ::Section::Closed:: *)
(*integral*)


(* ::Input:: *)
(*tef[{k_,m2_}->n_]:=(-2\[Pi] I)/(-n-1)! D[*)
(*FCI@Sqrt[*)
(*FV[k,p]^2+ed[k[t],k[t]]+m2*)
(*]*)
(*,{m2,-n}]/;n<=-1*)


(* ::Section:: *)
(*Feynman parameter*)


(* ::Subsection:: *)
(*scheme 1*)


(* ::Input:: *)
(*(*\:4ee4 f \:662f\:6570\:503c\:51fd\:6570,\:8fd9\:6837\:5728\:7ebf\:6027\:51fd\:6570\:4e2d,\:53ef\:4ee5\:81ea\:52a8\:63d0\:5230\:51fd\:6570\:5916*)*)
(*SetAttributes[f,NumericFunction]*)
(*(*\:8d39\:66fc\:53c2\:6570\:5316\:ff1a\:4f20\:64ad\:5b50\:7684\:7ec4\:5408*)*)
(*feynmanParameter[props_List]:=Module[{len=Length@props,coes},*)
(*coes=Prepend[f/@Range[len-1],1]-Append[f/@Range[len-1],0];*)
(*props . coes*)
(*]*)


(* ::Input:: *)
(*(*\:8d39\:66fc\:53c2\:6570 f[n] \:8f6c\:6362\:5230\:7eaf\:7b26\:53f7\:5f62\:5f0f fn *)*)
(*ruleFeynParaToSymbol=AssociationThread[f/@Range[10],Symbol@StringTemplate["f``"]@#&/@Range@10];*)
(*(*\:9006\:53d8\:6362----------*)*)
(*ruleSymbolToFeynPara=Association[Normal@ruleFeynParaToSymbol/.Rule->OperatorApplied[Rule]];*)


(* ::Subsection:: *)
(*scheme 2*)


(* ::Input:: *)
(*(*\:8f93\:5165\:4e00\:4e2a\:6574\:6570 n\:ff0c\:751f\:6210\:53d8\:91cf fn\:ff0c\:8868\:793a\:7b2c n \:4e2a\:8d39\:66fc\:53c2\:6570 *)*)
(*genFeynmanParameter[integer_]:=Symbol@StringTemplate["f``"]@integer*)
(*(*\:8d39\:66fc\:53c2\:6570\:5316\:ff1a\:4f20\:64ad\:5b50\:7684\:7ec4\:5408*)*)
(*feynmanParameter[props_List]:=Module[{len=Length@props,coes},*)
(*coes=(Prepend[genFeynmanParameter/@Range[len-1],1]-*)
(*Append[*)
(*genFeynmanParameter/@Range[len-1],0]);*)
(*props . coes*)
(*]*)
(*(* \:544a\:8bc9 NumericQ \:51fd\:6570, \:8d39\:66fc\:53c2\:6570 f1...f10 \:90fd\:662f\:6570\:5b57----------*)*)
(*UpSetDelayed[NumericQ[genFeynmanParameter@#],True]&/@Range[10];*)


(* ::Section:: *)
(*scheme 3*)


(* ::Input:: *)
(*(*\:8d39\:66fc\:53c2\:6570 f[n] \:8f6c\:6362\:5230\:7eaf\:7b26\:53f7\:5f62\:5f0f fn *)*)
(*ruleFeynParaToSymbol=AssociationThread[f/@Range[10],Indexed[\[Alpha],#]&/@Range[10]];*)
(*(*\:9006\:53d8\:6362----------*)*)
(*ruleSymbolToFeynPara=Association[Normal@ruleFeynParaToSymbol/.Rule->OperatorApplied[Rule]];*)
