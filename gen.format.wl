(* ::Package:: *)

(*+++++++++++++++++++++++++++++++++++++++++++++++++++++*)
BeginPackage["gpd`"]
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++*)


(* raw \:7f16\:7801\:7684\:573a\:ff0c\:4f7f\:7528\:6570\:5b57\:6307\:5b9a\:573a\:7684\:5177\:4f53\:7c7b\:578b\:ff0c\:5177\:4f53\:4f8b\:5b50\:ff0c\:6b63\:53cd\:573a\:ff0c\:4ee5\:53ca\:8d28\:91cf\:7b49\:6027\:8d28.*)
fd::usage="fd[class,num,anti],{class::\:573a\:7684\:7c7b\:578b\:ff0c1:meson\:ff0c2:octet,3:decuplet,4:quark},
{num::\:573a\:7684\:7f16\:53f7},{anti,0:\:6b63\:573a,1:\:5e26bar\:573a,2:\:573a\:7684\:8d28\:91cf}\:ff0c
\:573a\:7684\:79cd\:7c7b1,2,3\:662f\:56e0\:4e3a\:4f7f\:7528\:4e86\:5217\:8868\:ff0c\:4ece1\:5f00\:59cb\:6bd4\:8f83\:65b9\:4fbf.
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
\[CapitalOmega]m::10
quark
u,d,s";


ff::usage="human-readable \:5f62\:5f0f\:7684\:8f93\:5165\:63a5\:53e3\:ff0c\:589e\:52a0\:7a0b\:5e8f\:53ef\:8bfb\:6027";
fdStr::usage="\:5404\:79cd\:573a\:7684\:5b57\:7b26\:4e32\:8868\:793a";
fdDisp::usage="\:573a\:7684\:663e\:5f0f\:683c\:5f0f";


lecs::usage="\:5404\:79cd\:8026\:5408\:5e38\:6570,
1,1/f,f,D,F,
calC,calH,
c1,c2,c3,c4,F2T
b9,b10,b11
\:5176\:4e2d lecs[1]\:4fdd\:7559\:4e3a\:52a8\:80fd\:9879\:7684\:8026\:5408\:5e38\:6570";
cc::usage="\:8026\:5408\:5e38\:6570\:7684\:7b80\:77ed\:540d\:5b57";
ch::usage="\:7535\:8377\:77e9\:9635\:7684\:5934\:90e8";


mes::usage="\:4ecb\:5b50\:7684 human-readable \:8f93\:5165\:5f62\:5f0f";
oct::usage="\:516b\:91cd\:6001\:91cd\:5b50\:7684 human-readable \:8f93\:5165\:5f62\:5f0f";
dec::usage="\:5341\:91cd\:6001\:91cd\:5b50\:7684 human-readable \:8f93\:5165\:5f62\:5f0f";


mesRule::usage="\:663e\:5f0f\:66ff\:6362\:89c4\:5219, \:5c06\:4ecb\:5b50\:573a\:66ff\:6362\:6210\:5bf9\:5e94\:7684\:53cd\:7c92\:5b50";
octetRule::usage="\:516b\:91cd\:6001 <->\:516b\:91cd\:6001 bar \:76f8\:5173\:7684\:53d8\:6362\:89c4\:5219";
decupletRule::usage="\:5341\:91cd\:6001 <->\:5341\:91cd\:6001 bar \:76f8\:5173\:7684\:53d8\:6362\:89c4\:5219";


vfd::usage="\:77e2\:91cf\:573a";
fv::usage="\:5149\:5b50\:573aA";
F\[Mu]\[Nu]::usage="\:5149\:5b50\:573aF\[Mu]\[Nu]";


pde::usage="\:504f\:5bfc\:6570";
ltzScript::usage="\:6d1b\:4f26\:5179\:6307\:6807";
gma::usage="\[Gamma]\:77e9\:9635";
lagint::usage="\:62c9\:683c\:6717\:65e5\:91cf\:7684\:5934\:90e8";
lagcoe::usage="\:62c9\:683c\:6717\:65e5\:91cf\:7cfb\:6570\:7684\:5934\:90e8";


fdType::usage="\:573a\:7684\:79cd\:7c7b";
vtxCoe::usage="\:8d39\:66fc\:9876\:70b9\:7cfb\:6570\:7684\:5934\:90e8";
vtxType::usage="\:8d39\:66fc\:9876\:70b9\:79cd\:7c7b\:7684\:5934\:90e8";
chTagKey::usage="\:8d39\:66fc\:56fechpt \:6807\:8bb0\:7684key";
chTag::usage="\:8d39\:66fc\:56fe chpt \:6807\:8bb0\:7684value";
MassKey::usage="\:9876\:70b9,\:8d39\:66fc\:56fe\:7b49\:7b49\:7c92\:5b50\:8d28\:91cf\:7684key";
fyCoeKey::usage="\:8d39\:66fc\:56fe\:7cfb\:6570\:4e58\:79ef\:7684\:952e";
fyCoe::usage="\:8d39\:66fc\:56fe\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8";
fyCoeTag::usage="\:8d39\:66fc\:56fe\:7cfb\:6570\:4e58\:79ef, \:4ecb\:5b50\:7684\:5934\:90e8";
fyVtx::usage="\:8d39\:66fc\:56fe\:67d0\:4e2a\:9876\:70b9\:7684\:5934\:90e8\:ff0c\:4f8b\:5982fyVtx[...,v1]";


qua::usage="\:5938\:514b\:573a\:7684 human-readable \:8f93\:5165\:5f62\:5f0f";
quarkRule::usage="\:4e0e \:5938\:514b<->\:5938\:514bbar \:76f8\:5173\:7684\:53d8\:6362\:89c4\:5219";


qwData::usage="\:5404\:79cd\:7c92\:5b50\:5938\:514b\:7ec4\:6210\:7684\:6570\:636e";
qwKey::usage="\:7c92\:5b50\:5938\:514b\:7ec4\:6210\:6570\:636e\:7684Key\:5934\:90e8";
qwave::usage="\:7c92\:5b50\:5938\:514b\:7ec4\:6210\:6570\:636e\:7684Value\:5934\:90e8";
fqdKey::usage="\:8d39\:66fc\:56fe\:5938\:514b\:7ec4\:6210Key";
fqd::usage="\:5f3a\:5b50\:5938\:514b\:7ec4\:6210\:7684\:5934\:90e8";
fqdList::usage="\:5217\:8868\:7684\:5934\:90e8\:ff0c\:5938\:514b\:7ec4\:5408\:7684\:5217\:8868";
fqdList2::usage="2\:7ea7\:5217\:8868\:7684\:5934\:90e8\:ff0c\:5938\:514b\:7ec4\:5408\:7684\:5217\:8868";
fqdpos::usage="\:8d39\:66fc\:56fe\:5938\:514b\:4f4d\:7f6e\:7684\:5934\:90e8";
fqdTag::usage="quarkflow \:56fe\:7684tag";
eqList::usage="quarkflow \:65b9\:7a0b\:7ec4\:6210\:7684\:5217\:8868\:7684\:5934\:90e8";


kn::usage="kinematics,\:8fd0\:52a8\:5b66\:53d8\:91cf\:7684\:5934\:90e8\:ff0c\:4f8b\:5982 kn[q]";


(*+++++++++++++++++++++++++++++++++++++++++++++++++++++*)
Begin["`Private`"]
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:5b9a\:4e49\:51fd\:6570,\:786e\:4fdd\:53c2\:6570\:88ab\:5217\:8868\:5316*)
enList[x_List]:=x
enList[x_]:={x}
enList[x__]:={x}
(*\:786e\:4fdd\:53c2\:6570\:6210\:4e3a\:5b57\:7b26\:4e32*)
enString[x__]:=StringJoin[ToString/@enList[x]]


(* ::Section:: *)
(*chpt fields*)


fdProps::usage="\:5c06\:573a\:6309\:7167\:5c5e\:6027\:6536\:96c6\:5728\:4e00\:8d77\:ff0c\:4e3b\:8981\:7528\:4e8e\:6392\:7248\:663e\:793a";
fieldScript::usage="\:573a\:7684\:5934\:90e8,\:628a\:53c2\:6570\:6392\:7248\:6210\:7c92\:5b50\:573a\:7684\:5f62\:5f0f,\:4e00\:822c\:901a\:8fc7 SuperScript \:5b9e\:73b0";
massScript::usage="\:573a\:7684\:8d28\:91cf\:7684\:5934\:90e8,\:628a\:53c2\:6570\:6392\:7248\:6210\:8d28\:91cf\:7684\:5f62\:5f0f,\:4e00\:822c\:901a\:8fc7 Subscript \:5b9e\:73b0";


(* ::Section:: *)
(*<< module*)


Once@Get["gen.format-interface.wl"];


(* ::Section:: *)
(*wave function in quark*)


(* \:4ecb\:5b50\:7684\:5938\:514b\:7ec4\:6210\:ff0c\:5938\:514b\:662f\:7528\:7f16\:53f7\:8868\:793a\:7684, toqwave \:5c06\:7f16\:53f7\:8f6c\:6362\:6210\:5938\:514b\:573a *)
toqwave["mes"][{x_,y_}]:=qwave[fd[4,x,0],fd[4,y,1]];
qwaveIdx["mes"]=Map[toqwave["mes"],
{
{{1,1},{2,2},{3,3}},(*\[Eta]0*)
{{1,2}},(*\[Pi]+*){{1,1},{2,2}},(*\[Pi]0*){{2,1}},(*\[Pi]-*)
{{1,3}},(*K+*){{3,1}},(*K-*){{2,3}},(*K0*){{3,2}},(*K0b*)
{{1,1},{2,2},{3,3}}(*\[Eta]8*)
}
,{2}];
qwData["mes"]=AssociationThread[
qwKey/@Values[mes/@fdStr["mes"]],
fqdList@@@qwaveIdx["mes"]
];


(* oct\:7684\:5938\:514b\:7ec4\:6210 *)
toqwave["oct"][{x_,y_,z_}]:=fqdList@@Permutations[qwave[fd[4,x,0],fd[4,y,0],fd[4,z,0]]];
qwaveIdx["oct"]=toqwave["oct"]/@{
{1,1,2},{1,2,2},
{1,1,3},{1,2,3},{2,2,3},
{1,3,3},{2,3,3},
{1,2,3}
};
qwData["oct"]=AssociationThread[
qwKey/@Values[oct/@fdStr["oct"]],
qwaveIdx["oct"]
];


(*\:5341\:91cd\:6001\:91cd\:6001\:91cd\:5b50\:7684\:5938\:514b\:7ec4\:6210*)
Block[{type="dec"},
toqwave[type][{x_,y_,z_}]:=qwave[fd[4,x,0],fd[4,y,0],fd[4,z,0]];
qwaveIdx[type]=toqwave[type]/@{
{1,1,1},{1,1,2},{1,2,2},{2,2,2},
{1,1,3},{1,2,3},{2,2,3},
{1,3,3},{2,3,3},
{3,3,3}
};
qwData[type]=AssociationThread[qwKey/@fdStr[type],qwaveIdx[type]];
]


(* ::Section:: *)
(*field format*)


(*\:8fd9\:91cc\:7ed9\:51fa\:7684\:51fd\:6570\:53ea\:6709\:6392\:7248\:4f5c\:7528\:ff0c\:4e0d\:53c2\:52a0\:8fd0\:7b97, \:7ed9\:51fa\:7c92\:5b50\:548c\:53cd\:7c92\:5b50\:7684\:5934\:90e8,fieldScript,\:628a\:53c2\:6570\:6392\:7248\:6210\:7c92\:5b50\:573a\:7684\:5f62\:5f0f,\:4e00\:822c\:901a\:8fc7 SuperScript \:5b9e\:73b0*)
(*\:5728\:8fd9\:91cc\:5b9a\:4e49\:597d\:663e\:793a\:683c\:5f0f\:4e4b\:540e\:ff0c\:4e4b\:540e\:5b9a\:4e49\:663e\:793a\:7684\:66ff\:6362\:89c4\:5219*)
fdClassTotal=4;(*\:4ecb\:5b50\:ff0c\:516b\:91cd\:6001\:ff0c\:5341\:91cd\:6001\:ff0c\:5938\:514b\:573a\:ff0c\:4e00\:51714\:79cd*)
fdProps[0]=AssociationThread[(*\:7c92\:5b50\:7684\:8bb0\:53f7*)
Range[fdClassTotal],
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
}],
<|(*\:5938\:514b\:573a*)
1->fieldScript["u"],2->fieldScript["d"],3->fieldScript["s"]
|>
}
];


fdProps[1]=AssociationThread[(*\:53cd\:7c92\:5b50\:7684\:8bb0\:53f7*)
Range[fdClassTotal],
{
AssociationThread[Range[0,8,1],{(*\:5982\:679c\:65bd\:52a0\:4ecb\:5b50\:53cd\:7c92\:5b50\:5173\:7cfb\:ff0c\:8fd9\:91cc\:5176\:5b9e\:7528\:4e0d\:5230*)
fieldScript["\[Eta]","0"],
fieldScript["\[Pi]","-"],fieldScript["\[Pi]","0"],fieldScript["\[Pi]","+"],
fieldScript["K","-"],fieldScript["K","+"],
OverBar[fieldScript["K","0"]],fieldScript["K","0"],
fieldScript["\[Eta]","8"]
}],(*\:516b\:91cd\:6001\:4ecb\:5b50\:53cd\:7c92\:5b50\:7684\:8868\:793a*)
OverBar/@fdProps[0][2],(*\:516b\:91cd\:6001\:91cd\:5b50\:6dfb\:52a0\:9876\:6760\:5373\:53ef*)
OverBar/@fdProps[0][3],(*\:5341\:91cd\:6001\:91cd\:5b50\:6dfb\:52a0\:9876\:6760\:5373\:53ef*)
OverBar/@fdProps[0][4](*\:5341\:91cd\:6001\:91cd\:5b50\:6dfb\:52a0\:9876\:6760\:5373\:53ef*)
}
];
(*OverBar/@fdProps[0][1],(*\:516b\:91cd\:6001\:4ecb\:5b50\:4e5f\:6dfb\:52a0\:9876\:6760*)*)


(*\:5404\:79cd\:7c92\:5b50\:7684\:8d28\:91cf*)
fdProps[2]=Map[
massScript["M",#1]&,fdProps[0],{2}(*"\:573a\:7684\:8d28\:91cf\:7684\:5934\:90e8,\:628a\:53c2\:6570\:6392\:7248\:6210\:8d28\:91cf\:7684\:5f62\:5f0f,\:4e00\:822c\:901a\:8fc7 Subscript \:5b9e\:73b0"*)
];


(*\:5b9e\:73b0\:573a\:6392\:7248\:7684\:51fd\:6570*)
fdFmt[class_,num_,props_]:=fdProps[props][class,num]
(*------------\:7279\:6b8a\:573a\:9879 \[Pi]0,\[Eta]8 ------------*)
fdFmt[1,{0,2,8},0]=fieldScript["\[Pi]0","\[Eta]"];
fdFmt[1,{0,2,8},1]=fieldScript["\[Pi]0","\[Eta]"];
(*------------\:7279\:6b8a\:573a\:9879 \[CapitalSigma]0,\[CapitalLambda] ------------*)
fdFmt[2,{4,8},0]=fieldScript["\[CapitalSigma]0","\[CapitalLambda]"];
fdFmt[2,{4,8},1]=OverBar@fieldScript["\[CapitalSigma]0","\[CapitalLambda]"];
(*------------\:7279\:6b8a\:8d28\:91cf\:9879------------*)
fdFmt[1,{0,2,8},2]=massScript["M",fieldScript["\[Pi]","0"],fieldScript["\[Eta]","08"]];(*\:7279\:6b8a\:60c5\:51b5 \[Pi]0 \[Eta]8 \:6df7\:5408\:7684\:8d28\:91cf*)
fdFmt[2,{4,8},2]=massScript["M",fieldScript["\[CapitalSigma]","0"],fieldScript["\[CapitalLambda]",""]];(*\:7279\:6b8a\:60c5\:51b5 \[CapitalSigma]0 \[CapitalLambda] \:6df7\:5408\:7684\:8d28\:91cf*)


lecsScript=enString;(*\:4e4b\:524d\:662f\:4e0b\:89d2\:6807\:683c\:5f0f: lecsScript=Subscript*)
lecsFmt[x:_]:=<|(*\:8026\:5408\:5e38\:6570\:7684\:5177\:4f53\:5b9e\:73b0*)
"1"->1,
"e"->lecsScript["e",""],(*\:7535\:5b50\:7535\:8377,e>0*)
"1/f"->1/lecsScript["f","\[Phi]"],"f"->lecsScript["f","\[Phi]"],"f\[Phi]"->lecsScript["f","\[Phi]"],
"D"->lecsScript["D",""],"F"->lecsScript["F",""],
"C"->lecsScript["\[ScriptCapitalC]",""],"H"->lecsScript["\[ScriptCapitalH]",""],
"c1"->lecsScript["c","1"],"c2"->lecsScript["c","2"],"c3"->lecsScript["c","3"],
"c4"->lecsScript["c","4"],"cT"->Superscript["c","T"],
"b9"->lecsScript["b","9"],"b10"->lecsScript["b","10"],"b11"->lecsScript["b","11"]
|>[x];
(*\:7535\:8377\:77e9\:9635\:7684\:5b9e\:73b0*)
chFmt[x_]:=<|"u"->lecsScript["u",""],"d"->lecsScript["d",""],"s"->lecsScript["s",""]|>[x]


(* ::Section:: *)
(*Lorentz\:5bf9\:8c61*)


vfdFmt[idx_]:=Superscript["v",idx];(*\:5b9e\:73b0\:5916\:6e90\:6392\:7248\:7684\:51fd\:6570*)
fvFmt[q_,\[Mu]_]:=Subscript[q,\[Mu]](*\:5149\:5b50\:573aA\:7684\:8868\:793a*)
F\[Mu]\[Nu]Fmt[\[Mu]_,\[Nu]_]:=Subscript["F",enString[\[Mu]]<>enString[\[Nu]]](*\:5149\:5b50\:573a\:5f3a\:5f20\:91cf\:7684\:8868\:793a*)


pdeFmt[pd:_,sym:_]:=CenterDot[pd,sym](*gamma \:548c\:504f\:5bfc\:6570\:7684\:663e\:5f0f*)
gmaFmt[gma:_,sym:_]:=CenterDot[gma,sym]


(* ::Section:: *)
(*Lagrangian *)


(* ::Text:: *)
(*\:76f8\:4e92\:4f5c\:7528\:62c9\:6c0f\:91cf\:7684\:5934\:90e8 laginter\:ff0c\:4ee5\:53ca\:7cfb\:6570 lagcoe*)


lagintFmt[x:__]:=Grid[Values[Association@x],
Frame->All,FrameStyle->Directive[Lighter[Black,.7]],ItemStyle->{"InlineFormula"}
];
lagcoeFmt[x:__]:=Style[x,"InlineFormula"]


pL[x__]:=pL[x]=Subscript["f",x](*\:533a\:5206\:4e0d\:540c\:51fd\:6570\:7684\:663e\:793a*)
(* ++++++++++++++++++++++++++++++++++++ *)
fdTypeFmt[x__]:=pL[1][x](*\:8868\:793a\:573a\:7684\:79cd\:7c7b*)
vtxCoeFmt[x__]:=pL[2][x](*\:8d39\:66fc\:9876\:70b9\:7cfb\:6570\:7684\:663e\:793a\:683c\:5f0f*)
vtxTypeFmt[x__]:=pL[3][x](*\:8d39\:66fc\:9876\:70b9\:7c7b\:578b\:7684\:663e\:793a\:683c\:5f0f*)


chTagKeyFmt[x__]:=pL[4][x](*\:8d39\:66fc\:56fe chpt \:6807\:8bb0\:7684key*)
chTagFmt[x__]:=pL[5][x](*\:8d39\:66fc\:56fe chpt tag \:7684\:503c*)
fyCoeKeyFmt[x__]:=pL[6][x](*\:8d39\:66fc\:56fe\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:952e*)
fyCoeFmt[x__]:=pL[7][x](*\:8d39\:66fc\:56fe\:8026\:5408\:7cfb\:6570\:7684\:4e58\:79ef\:7684\:663e\:793a*)
fyCoeTagFmt[x__]:=pL[8][x](*\:8d39\:66fc\:56fe\:8026\:5408\:7cfb\:6570\:7684\:4e58\:79ef\:7684\:663e\:793a*)
MassKeyFmt[x__]:=pL[9][x](*\:8d39\:66fc\:56fe\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:7684\:663e\:793a\:683c\:5f0f*)
fyVtxFmt[x__]:=pL[10][x](*\:8d39\:66fc\:56fe\:9876\:70b9\:4f4d\:7f6e\:7684\:6807\:8bb0*)


qwDataFmt[x__]:=pL[11][x](*\:5404\:79cd\:7c92\:5b50\:5938\:514b\:7ec4\:6210\:7684\:6570\:636e*)
qwKeyFmt[x__]:=pL[12][x](*\:7c92\:5b50\:5938\:514b\:7ec4\:6210\:6570\:636e\:7684Key\:5934\:90e8*)
qwaveFmt[x__]:=pL[13][x](*\:7c92\:5b50\:5938\:514b\:7ec4\:6210\:6570\:636e\:7684Value\:5934\:90e8*)
fqdListFmt[x__]:=pL[14][x](*\:5938\:514b\:7ec4\:5408\:7684\:7f6e\:6362\:5f62\:6210\:7684\:5217\:8868\:5934\:90e8*)
fqdList2Fmt[x__]:=pL[15][x](*\:4e8c\:7ea7\:5217\:8868\:7684\:5934\:90e8,\:5938\:514b\:7ec4\:5408\:7684\:5217\:8868*)
fqdposFmt[x__]:=pL[16][x](*\:8d39\:66fc\:56fe\:5938\:514b\:56fe,\:5938\:514b\:4f4d\:7f6e\:7684\:5934\:90e8*)
fqdKeyFmt[x__]:=pL[17][x](*\:8d39\:66fc\:56fe\:5938\:514b\:7ec4\:6210Key*)
fqdFmt[x__]:=pL[18][x](*\:8d39\:66fc\:56fe\:5938\:514b\:7ec4\:6210\:7684value*)
fqdTagFmt[x__]:=pL[19][x](* quarkflow \:56fe\:7684Tag *)
eqListFmt[x__]:=pL[20][x](* quarkflow \:65b9\:7a0b\:7ec4\:7684\:5934\:90e8 *)


knFmt[x__]:=pL[21][x](*\:8fd0\:52a8\:5b66\:53d8\:91cf\:7684\:663e\:793a*)


(* ::Section:: *)
(*\:5e94\:7528\:6392\:7248*)


(* ::Text:: *)
(*\:4f7f\:7528 mma \:7684 Format \:51fd\:6570\:5bf9\:7ed3\:679c\:8fdb\:884c\:8f93\:51fa\:6392\:7248.\:5b9a\:4e49\:6392\:7248\:7684\:8bed\:6cd5\:4e3a\:ff1a*)


(* ::DisplayFormula:: *)
(*Format[expr[x__],StandardForm]:=display[x]*)


assApp[expr:_,display:_]:=(Format[expr[x__],StandardForm]:=display[x])


(* ::Text:: *)
(*\:7ed9\:51fa\:81ea\:5b9a\:4e49\:51fd\:6570\:548c\:6392\:7248\:51fd\:6570\:7684\:5bf9\:5e94\:5173\:7cfb*)


assFmt=<|
lecs->lecsFmt,cc->lecsFmt,ch->chFmt,
fd->fdFmt,vfd->vfdFmt,fv->fvFmt,F\[Mu]\[Nu]->F\[Mu]\[Nu]Fmt,
(*\:573a\:7684\:663e\:793a\:65b9\:5f0f\:ff0c\:4e4b\:524d\:662f fieldScript->Superscript,massScript->Subscript *)
massScript->Subscript,fieldScript->enString,
pde->pdeFmt,ltzScript->Subscript,gma->gmaFmt,(*ldx\[Rule]ldxFmt,*)
lagint->lagintFmt,lagcoe->lagcoeFmt,
fdType->fdTypeFmt,vtxType->vtxTypeFmt,vtxCoe->vtxCoeFmt,
MassKey->MassKeyFmt,fyCoeKey->fyCoeKeyFmt,fyCoe->fyCoeFmt,fyCoeTag->fyCoeTagFmt,
chTagKey->chTagKeyFmt,chTag->chTagFmt,fyVtx->fyVtxFmt,
qwData->qwDataFmt,qwKey->qwKeyFmt,qwave->qwaveFmt,
fqdKey->fqdKeyFmt,fqd->fqdFmt,
fqdList->fqdListFmt,fqdList2->fqdList2Fmt,
fqdpos->fqdposFmt,fqdTag->fqdTagFmt,eqList->eqListFmt,
kn->knFmt
|>;


(* ::Text:: *)
(*\:8fdb\:884c\:6392\:7248\:6620\:5c04,\:5de6\:8fb9\:662f\:81ea\:5b9a\:4e49\:51fd\:6570,\:53f3\:8fb9\:662f\:6392\:7248\:51fd\:6570,assApp[key->Value]*)


KeyValueMap[assApp,assFmt];


(*+++++++++++++++++++++++++++++++++++++++++++++++++++++*)
End[ ]
EndPackage[ ]
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++*)
