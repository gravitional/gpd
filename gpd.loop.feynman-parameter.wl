(* ::Package:: *)

(* ::Title:: *)
(*gpd.loop.feynman-parameter.wl*)


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
,m\[CapitalDelta],mS
(*\:52a8\:91cf*)
,p1,p2
(*(p1+p2)/2,p1-p2*)
,P,\[CapitalDelta],
(*\:8d39\:66fc\:53c2\:6570-----------*)
fp
];


(*\:8d39\:66fc\:53c2\:6570\:5316\:ff1a\:4f20\:64ad\:5b50\:7684\:7ec4\:5408*)
feynmanParameter[props_List]:=Module[{len=Length@props,coes},
coes=Prepend[fp/@Range[len-1],1]-Append[fp/@Range[len-1],0];
props . coes
]


(*\:5c06 Symbol \:663e\:793a\:6210 Humam \:6837\:5f0f*)
ruleTeX={
p1->Subscript[p,1],p2->Subscript[p, 2]
,l->\[ScriptL],\[CapitalDelta]2->\[CapitalDelta]^2
,mN->Subscript[M,N]
,mo1->Subscript[M,B],mo2->Subscript[M,B2]
,mm1->Subscript[m,\[Phi]]
(*\:8d39\:66fc\:53c2\:6570\:5316*)
,Splice@Normal@AssociationThread[fp/@Range[10],ToExpression@CharacterRange[97,106]]
};
(*\:9006\:66ff\:6362\:89c4\:5219\:ff0c\:4ea4\:6362 ruleTeX \:4e2d\:4e24\:4e2a\:53c2\:6570\:7684\:4f4d\:7f6e ----------------*)
ruleTexRev=ruleTeX/.Rule->ReverseApplied[Rule];


(*ward \:6052\:7b49\:5f0f\:ff0c\:65b9\:4fbf\:53bb\:6389\:6b63\:6bd4\:4e8e \[CapitalDelta] \:7684\:9879*)
ruleWard=MomentumExpand@{
Momentum[p1]->Momentum[P]+Momentum[\[CapitalDelta]]/2,
Momentum[p2]->Momentum[P]-Momentum[\[CapitalDelta]]/2
};


(* ::Section:: *)
(*kinematics*)


(*\:8fd0\:52a8\:5b66\:4e0d\:53d8\:91cf*)
SP[p1,p1]=mN^2;
SP[p2,p2]=mN^2;
SP[p1,\[CapitalDelta]]=\[CapitalDelta]2/2;
SP[p2,\[CapitalDelta]]=-\[CapitalDelta]2/2;
SP[p1,p2]=mN^2-\[CapitalDelta]2/2;
(*-----------*)
SP[P,P]=mN^2-\[CapitalDelta]2/4;
SP[\[CapitalDelta],\[CapitalDelta]]=\[CapitalDelta]2;
SP[P,\[CapitalDelta]]=0;


(*\:53bb\:6389 Dirac Spinor \:4e24\:7aef\:7684\:65cb\:91cf*)
dropSpinorDot[ubar_Spinor,expr__,u_Spinor]:=dropSpinorDot[expr]
dropSpinorDot[ubar_Spinor,u_Spinor]:=1
dropSpinor[expr_]:=expr/.{Dot->dropSpinorDot}/.{dropSpinorDot->Dot}


(* ::Section:: *)
(*Feynman parameterization*)


(*\:4f20\:64ad\:5b50*)
propg[k_,m_]:=SP[k,k]-m^2


(*CompleteSquare[a p^2+b p+c, p, q] -> {a q^2-b^2/(4 a)+c, q\[Rule]p+b/(2 a)}.*)
(*\:5c06\:5408\:5e76\:540e\:7684\:4f20\:64ad\:5b50\:914d\:9f50\:6210\:5b8c\:5168\:5e73\:65b9*)
toSquare[expr_,k_,l_]:=Module[
{res,kRule},
res=CompleteSquare[expr,k,l];
kRule=First@First@Solve[Equal@@Last@FCI@res,Momentum[k]];
Append[kRule]@res
]


(* ::Chapter:: *)
(*Loop integral*)


(* ::Section:: *)
(*Rainbow,meson,octet*)


(* ::Input:: *)
(*diagIllus@chTag@{"RB","mes","oct"}*)


(*\:56fe\:7684\:7f16\:53f7---------------------*)
fyTag={"RB","mes","oct"};
(*\:989d\:5916\:56e0\:5b50; preFactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;*)
preFactor=1;
(*\:5708\:79ef\:5206\:7684\:65cb\:91cf\:90e8\:5206-------------------------*)
splt[{fyTag,"spin",\[Mu]}]=DiracSimplify[
ExpandScalarProduct[
SpinorUBar[p2,mN] . ( 
FV[2k-\[CapitalDelta],\[Mu]] . GS[k-\[CapitalDelta]] . GA5 . (GS[p1-k]+mo1) . GS[k] . GA5
) . SpinorU[p1,mN]/.\[CapitalDelta]->p1-p2],
DiracEquation->True
]


(* ::Input:: *)
(*(*TeXForm*)*)
(*Simplify[*)
(*dropSpinor@splt[{fyTag,"spin",\[Mu]}]/.ruleTeX*)
(*]*)


(*\:4f20\:64ad\:5b50------------------*)
splt[{fyTag,"propg"}]=Collect[
feynmanParameter[{
propg[k-\[CapitalDelta],\[CapitalLambda]],propg[k,\[CapitalLambda]]
,propg[k-\[CapitalDelta],mm1],propg[k,mm1]
,propg[p1-k,mo1]
}],
k,Simplify
];


(* ::Input:: *)
(*splt[{fyTag,"propg"}]/.ruleTeX*)


(*TeXForm*)
splt[{fyTag,"square"}]=Simplify[
toSquare[splt[{fyTag,"propg"}],k,l]
];
(*\:8d39\:66fc\:53c2\:6570\:5316,\:5e73\:79fb\:79ef\:5206\:53d8\:91cf----------*)
ruleMoment=Last@splt[{fyTag,"square"}];


(* ::Input:: *)
(*splt[{fyTag,"square"}]/.ruleTeX//TableForm*)


ruleNumerator=MomentumExpand@FCI@{
FV[\[CapitalDelta],\[Mu]]->0,
SP[l,\[CapitalDelta]]->0,
GS[l] FV[l,\[Mu]]->1/4*GA[\[Mu]]*SP[l,l],
SP[l,P]*FV[l,\[Mu]]->1/4*FV[P,\[Mu]]*SP[l,l],
SP[l,P]GS[l]->1/4*mN*SP[l,l],
GS[l]FV[P,\[Mu]]->1/4*mN*SP[l,l]
};


(*\:5e94\:7528 Ward \:6052\:7b49\:5f0f\:ff0c\:4ee5\:53ca\:6839\:636e\:5708\:79ef\:5206\:5947\:5076\:6027\:ff0c\:91cf\:7eb2\:63a8\:5bfc\:7684\:66ff\:6362\:89c4\:5219*)
refineLoop[numerator_]:=Nest[
(*\:628a\:79ef\:5206\:4e2d\:5206\:5b50\:66ff\:6362\:6210\:5316\:7b80\:7684\:7ed3\:679c,\:4f7f\:7528 Ward-Identity \:7b49\:7ed3\:679c --------*)
ReplaceAll[ruleNumerator],
(*\:5c55\:5f00\:5206\:5b50\:4e2d\:7684\:6807\:91cf\:4e58\:79ef-----------*)
ExpandAll2[
ExpandScalarProduct[
DiracSimplify[
(*\:4f5c\:52a8\:91cf\:5e73\:79fbk\[Rule]l\:ff0c\:518d\:6b21\:4f7f\:7528 Dirac\:65b9\:7a0b\:5316\:7b80*)
FCI@numerator/.ruleMoment,
DiracEquation->True
(*\:51c6\:5907\:4f7f\:7528 Ward-Identity\:ff0c\:53bb\:6389\:6b63\:6bd4\:4e8e \[CapitalDelta]\[Mu] \:7684\:9879*)
]/.ruleWard
]]
(*\:91cd\:590d\:66ff\:6362\:4e09\:6b21------*)
,4]


(*\:5316\:7b80\:4e00\:6b21\:7684\:7ed3\:679c--------------*)
splt[{fyTag,"spinTmp",\[Mu]}]=Simplify[
dropSpinor@
refineLoop@
splt[{fyTag,"spin",\[Mu]}]
];


(* ::Input:: *)
(*splt[{fyTag,"spinTmp",\[Mu]}]/.ruleTeX*)


tec=Collect[
1/2(
(splt[{fyTag,"spinTmp",\[Mu]}]/.{Momentum[l]->Momentum[-l]})+splt[{fyTag,"spinTmp",\[Mu]}]
),
FCI/@{FV[P,\[Mu]],GA[\[Mu]]},
Simplify
]/.ruleTeX


(* ::Chapter:: *)
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
