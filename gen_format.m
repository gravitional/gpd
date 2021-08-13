(* ::Package:: *)

BeginPackage["gpd`"]


fd::usage="fd[kind,num,anti],{kind::\:573a\:7684\:7c7b\:578b\:ff0c1:meson\:ff0c2:octet,3:decuplet},{num::\:573a\:7684\:7f16\:53f7},{anti,0:\:6b63\:573a,1:\:5e26bar\:573a,2:\:573a\:7684\:8d28\:91cf}\:ff0c
\:5176\:4e2d\:5404\:79cd\:7c92\:5b50\:7684 num \:987a\:5e8f\:662f\:ff0c
meson
\[Eta]0,::0
\[Pi]p,\[Pi]0,\[Pi]m,::1,2,3
kp,km,::4,5
k0,k0b,::6,7
\[Eta]8::8
\[Pi]0-\[Eta]8::28
octet
p,n::1,2
\[CapitalSigma]p,\[CapitalSigma]0,\[CapitalSigma]m::3,4,5
\[CapitalXi]0,\[CapitalXi]m::6,7
\[CapitalLambda]::8
\[CapitalSigma]0--\[CapitalLambda]::48
decuplet
\[CapitalDelta]pp,\[CapitalDelta]p,\[CapitalDelta]0,\[CapitalDelta]m::1,2,3,4
\[CapitalSigma]sp,\[CapitalSigma]s0,\[CapitalSigma]sm::5,6,7
\[CapitalXi]s0,\[CapitalXi]sm::8,9
\[CapitalOmega]m::10";


(* ::Subtitle:: *)
(*Formatting*)


Begin["`Private`"]
(*\:7ed9\:51fa\:7c92\:5b50\:548c\:53cd\:7c92\:5b50\:7684\:8bb0\:53f7,fieldScript,\:573a\:7684\:5934\:90e8,\:628a\:53c2\:6570\:6392\:7248\:6210\:7c92\:5b50\:573a\:7684\:5f62\:5f0f,\:4e00\:822c\:901a\:8fc7 SuperScript \:5b9e\:73b0*)
(*\:5728\:8fd9\:91cc\:5b9a\:4e49\:597d\:663e\:793a\:683c\:5f0f\:4e4b\:540e\:ff0c\:4e4b\:540e\:66ff\:6362\:6389\:5185\:90e8\:51fd\:6570\:7684\:5934\:90e8*)
fdptc[0]=AssociationThread[(*\:7c92\:5b50\:7684\:8bb0\:53f7*)
Range[1,3,1],(*\:7c92\:5b50\:ff0c\:53cd\:7c92\:5b50\:ff0c\:8d28\:91cf*)
{
AssociationThread[
Range[0,8,1],(*\:516b\:79cd\:4ecb\:5b50*)
{
fieldScript["\[Eta]","0"],
fieldScript["\[Pi]","+"],fieldScript["\[Pi]","0"],fieldScript["\[Pi]","-"],
fieldScript["K","+"],fieldScript["K","-"],
fieldScript["K","0"],OverBar[fieldScript["K","0"]],
fieldScript["\[Eta]","8"]
}],
AssociationThread[
Range[1,8,1],(*\:516b\:91cd\:6001\:91cd\:5b50*)
{
fieldScript["p",""],fieldScript["n",""],
fieldScript["\[CapitalSigma]","+"],fieldScript["\[CapitalSigma]","0"],fieldScript["\[CapitalSigma]","-"],
fieldScript["\[CapitalXi]","0"],fieldScript["\[CapitalXi]","-"],
fieldScript["\[CapitalLambda]",""]
}],
AssociationThread[
Range[1,10,1],(*\:5341\:91cd\:6001\:91cd\:5b50*)
{
fieldScript["\[CapitalDelta]","++"],fieldScript["\[CapitalDelta]","+"],fieldScript["\[CapitalDelta]","0"],fieldScript["\[CapitalDelta]","-"],
fieldScript["\[CapitalSigma]","\[SixPointedStar]+"],fieldScript["\[CapitalSigma]","\[SixPointedStar]0"],fieldScript["\[CapitalSigma]","\[SixPointedStar]-"],
fieldScript["\[CapitalXi]","\[SixPointedStar]0"],fieldScript["\[CapitalXi]","\[SixPointedStar]-"],
fieldScript["\[CapitalOmega]","-"]
}]
}
];
fdptc[1]=AssociationThread[(*\:53cd\:7c92\:5b50\:7684\:8bb0\:53f7*)
Range[1,3,1],
{
AssociationThread[
Range[0,8,1],(*\:516b\:4e2a\:4ecb\:5b50\:7684\:53cd\:7c92\:5b50*)
{
fieldScript["\[Eta]","0"],
fieldScript["\[Pi]","-"],fieldScript["\[Pi]","0"],fieldScript["\[Pi]","+"],
fieldScript["K","-"],fieldScript["K","+"],
OverBar[fieldScript["K","0"]],fieldScript["K","0"],
fieldScript["\[Eta]","8"]
}],
OverBar/@fdptc[0][2],(*\:516b\:91cd\:6001\:91cd\:5b50\:6dfb\:52a0\:9876\:6760\:5373\:53ef*)
OverBar/@fdptc[0][3](*\:5341\:91cd\:6001\:91cd\:5b50\:6dfb\:52a0\:9876\:6760\:5373\:53ef*)
}
];
(*\:5404\:79cd\:7c92\:5b50\:7684\:8d28\:91cf*)
fdptc[2]=Map[
massScript["M",#1]&,fdptc[0],{2}(*"\:573a\:7684\:8d28\:91cf\:7684\:5934\:90e8,\:628a\:53c2\:6570\:6392\:7248\:6210\:8d28\:91cf\:7684\:5f62\:5f0f,\:4e00\:822c\:901a\:8fc7 Subscript \:5b9e\:73b0"*)
];
(*\:5b9e\:73b0\:573a\:6392\:7248\:7684\:51fd\:6570*)
fdFmt[kind_,num_,anti_]:=fdptc[anti][kind,num]
fdFmt[1,{2,8},2]=massScript["M",fieldScript["\[Pi]","0"],fieldScript["\[Eta]","8"]];(*\:7279\:6b8a\:60c5\:51b5 \[Pi]0 \[Eta]8 \:6df7\:5408\:7684\:8d28\:91cf*)
fdFmt[2,{4,8},2]=massScript["M",fieldScript["\[CapitalSigma]","0"],fieldScript["\[CapitalLambda]",""]];(*\:7279\:6b8a\:60c5\:51b5 \[CapitalSigma]0 \[CapitalLambda] \:6df7\:5408\:7684\:8d28\:91cf*)
lecsFmt[x:_]:=<|(*\:8026\:5408\:5e38\:6570\:7684\:5177\:4f53\:5b9e\:73b0*)
"1"->1,"1/f"->1/Subscript["f","\[Phi]"],"f"->Subscript["f","\[Phi]"],
"D"->Subscript["D",""],"F"->Subscript["F",""],
"C"->Subscript["\[ScriptCapitalC]",""],"H"->Subscript["\[ScriptCapitalH]",""],
"c1"->Subscript["c","1"],"c2"->Subscript["c","2"],"c3"->Subscript["c","3"],
"c4"->Subscript["c","4"],"cT"->Superscript["c","T"],
"b9"->Subscript["b","9"],"b10"->Subscript["b","10"],"b11"->Subscript["b","11"]
|>[x];
Lorentz\:5bf9\:8c61
vfdFmt[idx_]:=Superscript["v",idx];(*\:5b9e\:73b0\:5916\:6e90\:6392\:7248\:7684\:51fd\:6570*)
fvFmt[q_,\[Mu]_]:=Subscript[q,\[Mu]](*\:5149\:5b50\:573aA\:7684\:8868\:793a*)
F\[Mu]\[Nu]Fmt[\[Mu]_,\[Nu]_]:=Subscript["F",enString[\[Mu]]<>enString[\[Nu]]](*\:5149\:5b50\:573a\:5f3a\:5f20\:91cf\:7684\:8868\:793a*)
pdeFmt[pd:_,sym:_]:=CenterDot[pd,sym](*gamma \:548c\:504f\:5bfc\:6570\:7684\:663e\:5f0f*)
gmaFmt[gma:_,sym:_]:=CenterDot[gma,sym]
Lagrangian 
\:76f8\:4e92\:4f5c\:7528\:62c9\:6c0f\:91cf\:7684\:5934\:90e8 laginter\:ff0c\:4ee5\:53ca\:7cfb\:6570 lagcoe
lagintFmt[x:__]:=Grid[Values[Association@x],
Frame->All,FrameStyle->Directive[Lighter[Black,.7]],ItemStyle->{"InlineFormula"}
];
lagcoeFmt[x:__]:=Style[x,"InlineFormula"]
\:5e94\:7528\:6392\:7248
\:4f7f\:7528 mma \:7684 Format \:51fd\:6570\:5bf9\:7ed3\:679c\:8fdb\:884c\:8f93\:51fa\:6392\:7248 . \:5b9a\:4e49\:6392\:7248\:7684\:8bed\:6cd5\:4e3a\:ff1a
Format[expr[x__],StandardForm]:=display[x]
assApp[expr:_,display:_]:=(Format[expr[x__],StandardForm]:=display[x])
\:7ed9\:51fa\:81ea\:5b9a\:4e49\:51fd\:6570\:548c\:6392\:7248\:51fd\:6570\:7684\:5bf9\:5e94\:5173\:7cfb
assFmt=<|
fd->fdFmt,vfd->vfdFmt,fv->fvFmt,F\[Mu]\[Nu]->F\[Mu]\[Nu]Fmt,
lecs->lecsFmt,
massScript->Subscript,fieldScript->Superscript,
pde->pdeFmt,ltzScript->Subscript,
gma->gmaFmt,(*ldx\[Rule]ldxFmt,*)
lagint->lagintFmt,lagcoe->lagcoeFmt
|>;
\:8fdb\:884c\:6392\:7248\:6620\:5c04,\:5de6\:8fb9\:662f\:81ea\:5b9a\:4e49\:51fd\:6570,\:53f3\:8fb9\:662f\:6392\:7248\:51fd\:6570,assApp[key->Value]
KeyValueMap[assApp,assFmt];


End[ ]
EndPackage[ ]
