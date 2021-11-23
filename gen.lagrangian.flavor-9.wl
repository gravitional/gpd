(* ::Package:: *)

(* ::Title:: *)
(*gen_lagrangian.nb*)


(* ::Text:: *)
(*notebook \:5907\:5fd8\:5f55:*)
(*\:5c55\:5f00\:62c9\:683c\:6717\:65e5\:ff0c\:8bb0\:5f55\:63a8\:5bfc\:51fa\:7684\:9876\:70b9\:89c4\:5219\:ff0c\:4ee5\:53ca\:5b88\:6052\:6d41. \:8ba1\:7b97\:51fa\:5bf9\:5e94SU(3)\:6bcf\:4e2a\:751f\:6210\:5143\:7684\:5b88\:6052\:6d41\:ff0c\:4e5f\:5c31\:662f8+1\:4e2a*)
(*\:6240\:6709\:5473\:7a7a\:95f4\:7684\:77e9\:9635\:6309\:7167 Gellman \:77e9\:9635\:5206\:89e3\:ff0c\:4e5f\:5c31\:662f SU(3) \:751f\:6210\:5143\:76842\:500d*)
(*\:5c1d\:8bd5\:4f7f\:7528\:51fd\:6570\:63a5\:53e3: f[i,j,k]\:ff0c\:5c06\:4e0d\:592a\:786e\:5b9a\:7684\:5b9e\:73b0\:90fd\:5b9a\:4e49\:6210\:51fd\:6570.*)
(*\:4f7f\:7528\:7684\:8bb0\:53f7\:53c2\:8003\:ff1ahttps://arxiv.org/pdf/1806.07551.pdf*)


(* ::Chapter:: *)
(*initial*)


(*\:672c\:6587\:4ef6\:7684\:540d\:79f0*)
$fileName=If[$Notebooks,NotebookFileName[],$InputFileName];
(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c\:ff0c\:5c31\:5237\:65b0\:7b14\:8bb0\:672c\:7684\:6807\:9898*)
Once@If[!$Notebooks,NotebookWrite[Cells[][[1]],Cell[Last@FileNameSplit[$fileName],"Title"]]];
(*\:67e5\:627e init.wl, \:5bfc\:5165\:6839\:76ee\:5f55\:548c\:51fd\:6570\:5b9a\:4e49.*)
Catch@Module[{recurFind,start=1,depMax},
depMax=FileNameDepth[$fileName];(*\:8def\:5f84\:7684\:6700\:5927\:5c42\:6b21*)
(*-------\:5b9a\:4e49\:9012\:5f52\:51fd\:6570-------*)
recurFind[dep_Integer]:=If[dep<=depMax,
SetDirectory[DirectoryName[$fileName,dep]];(*SetDirectory[]\:8bbe\:7f6e\:5de5\:4f5c\:76ee\:5f55\:4e3a\:5bb6\:76ee\:5f55*)
If[FileExistsQ["init.wl"],Get["init.wl"];Throw["The base directory is : "<>$srcRoot],recurFind[dep+1](*\:5982\:679c\:8fd9\:4e00\:5c42\:627e\:4e0d\:5230\:ff0c\:5c31\:4e0a\:5347\:4e00\:5c42*)];
ResetDirectory[];,
Throw["I cann't find any init.wl in this project"]
];
recurFind[start];
]


(* ::Chapter:: *)
(*Lagrangian*)


(* ::Section::Closed:: *)
(*convention*)


(* ::DisplayFormula:: *)
(*kind::1,2,3*)
(*anti::0,1*)


(* ::Section::Closed:: *)
(*constants*)


(* ::Text:: *)
(*\:5e38\:6570\:77e9\:9635\:ff0c\:6bd4\:5982 Gellman \:77e9\:9635*)


(* ::DisplayFormula:: *)
(*SU(3)\:7fa4\:57fa\:672c\:8868\:793a\:7684\:751f\:6210\:5143\:ff0c=1/2 Gellman*)


dim=3;
dim::usage="\:4e3a\:5bf9\:79f0\:7fa4SU(3)\:9009\:53d6\:7684\:8868\:793a\:7684\:7ef4\:5ea6";


gellmann[num_]:={
IdentityMatrix[dim],
{{0,1,0},{1,0,0},{0,0,0}},
{{0,-I,0},{I,0,0},{0,0,0}},
{{1,0,0},{0,-1,0},{0,0,0}},
{{0,0,1},{0,0,0},{1,0,0}},
{{0,0,-I},{0,0,0},{I,0,0}},
{{0,0,0},{0,0,1},{0,1,0}},
{{0,0,0},{0,0,-I},{0,I,0}},
{{1/Sqrt[3],0,0},{0,1/Sqrt[3],0},{0,0,-(2/Sqrt[3])}}
}[[num+1]];
gellmann::usage="gellmann[1]\:ff0c\:7ed9\:51fa\:7b2c1\:4e2agellmann\:77e9\:9635";


(* ::Text:: *)
(*Levi - Civita \:5f20\:91cf *)


levi=LeviCivitaTensor[3];


(* ::Text:: *)
(*\:5b9a\:4e49\:5e7f\:4e49\:7684\:5bf9\:6613\:548c\:53cd\:5bf9\:6613\:ff0c\:5bf9\:4e8e\:67d0\:79cd\:8fd0\:7b97f\:ff0c*)


cmtm[f_,x_,y_]:=f[x,y]-f[y,x];
cmtm::usage="cmtm[f,x,y]=f[x,y]-f[y,x] \:8fdb\:884c\:5bf9\:6613\:8fd0\:7b97";


cmtp[f_,x_,y_]:=f[x,y]+f[y,x];
cmtp::usage="cmtm[f,x,y]=f[x,y]+f[y,x] \:8fdb\:884c\:53cd\:5bf9\:6613\:8fd0\:7b97";


(* ::Text:: *)
(*\:5404\:79cd\:8026\:5408\:5e38\:6570*)


lecs::usage="lecs[1]\:ff0c\:901a\:8fc7\:6574\:6570\:ff0c\:6307\:5b9a\:8981\:4f7f\:7528\:7684\:8026\:5408\:5e38\:6570\:ff0c\:987a\:5e8f
1,1/f,f,D,F,::1,5
calC,calH,::6,7
c1,c2,c3,c4,F2T::8,12
b9,b10,b11::13,15
\:5176\:4e2d lecs[1] \:4fdd\:7559\:4e3a\:52a8\:80fd\:9879\:7684\:8026\:5408\:5e38\:6570";


(* ::Section::Closed:: *)
(*fields matrix*)


(* ::Text:: *)
(*\:4f7f\:7528\:7684\:5355\:4e2a\:7684\:7c92\:5b50\:573a*)


fd::usage="fd[kind,num,anti],{kind::\:573a\:7684\:7c7b\:578b\:ff0c0,meson\:ff0c1\:ff0coctet,2,decuplet},{num::\:573a\:7684\:7f16\:53f7},{anti::0,\:7c92\:5b50,1,\:53cd\:7c92\:5b50,2,\:573a\:7684\:8d28\:91cf}\:ff0c
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


(* ::Text:: *)
(*\:6536\:96c6\:5355\:4e2a\:573a\:505a\:6210\:7684\:77e9\:9635*)


(* ::Text:: *)
(*\:4ecb\:5b50\:77e9\:9635*)


mat::usage="mat[kind,num,anti],{kind::\:573a\:7684\:7c7b\:578b\:ff0c0,meson\:ff0c1\:ff0coctet,2,decuplet},{num::\:573a\:7684\:7f16\:53f7},
{anti::0,\:6b63\:5e38\:7684\:77e9\:9635,1,\:573a\:7684\:5171\:8f6d,2,\:573a\:7684\:8d28\:91cf}\:ff0c
\:5176\:4e2d\:4ece\:5404\:79cd\:7c92\:5b50\:6536\:96c6\:5230\:7684\:77e9\:9635\:987a\:5e8f\:662f
meson
1,1,0,::\[Phi]
1,2,0,::u
1,3,0,::U
1,1,1,::\[Phi]\[ConjugateTranspose]
1,2,1,::u\[ConjugateTranspose]
1,3,1,::U\[ConjugateTranspose]
1,1,2,::M\[Phi]
octet
2,1,0,::B
2,1,1,::\!\(\*OverscriptBox[\(B\), \(_\)]\)
2,1,2,::MB
decuplet
3,1,0,::T
3,1,1,::\!\(\*OverscriptBox[\(T\), \(_\)]\)
3,1,2,::MT";


(* ::Text:: *)
(*\:4ecb\:5b50\:77e9\:9635 and dagger*)


(* ::Text:: *)
(*\:8d28\:91cf\:9879*)


mat[1,1,2]={
{fd[1,28,2],fd[1,1,2],fd[1,4,2]},
{fd[1,3,2],fd[1,28,2],fd[1,6,2]},
{fd[1,5,2],fd[1,7,2],fd[1,8,2]}
};


mat[1,1,0]={
{1/Sqrt[2] fd[1,2,0]+1/Sqrt[6] fd[1,8,0],fd[1,1,0],fd[1,4,0]},
{fd[1,3,0],-1/Sqrt[2] fd[1,2,0]+1/Sqrt[6] fd[1,8,0],fd[1,6,0]},
{fd[1,5,0],fd[1,7,0],-2/Sqrt[6] fd[1,8,0]}
};
mat[1,2,0]=IdentityMatrix[dim]+(I*lecs[2]/Sqrt[2])*mat[1,1,0];
mat[1,3,0]=IdentityMatrix[dim]+(I*lecs[2]*Sqrt[2])*mat[1,1,0];


(* ::DisplayFormula:: *)
(*1,2,0,::u*)
(*1,3,0,::U*)


mat[1,1,1]=mat[1,1,0];
mat[1,2,1]=IdentityMatrix[dim]-(I*lecs[2]/Sqrt[2])*mat[1,1,0];
mat[1,3,1]=IdentityMatrix[dim]-(I*lecs[2]*Sqrt[2])*mat[1,1,0];


(* ::Text:: *)
(*\:516b\:91cd\:6001\:77e9\:9635*)


(* ::Text:: *)
(*\:8d28\:91cf\:9879*)


mat[2,1,2]={
{fd[2,48,2],fd[2,3,2],fd[2,1,2]},
{fd[2,5,2],fd[2,48,2],fd[2,2,2]},
{fd[2,7,2],fd[2,6,2],fd[2,8,2]}
};


mat[2,1,0]={
{1/Sqrt[2] fd[2,4,0]+1/Sqrt[6] fd[2,8,0],fd[2,3,0],fd[2,1,0]},
{fd[2,5,0],-1/Sqrt[2] fd[2,4,0]+1/Sqrt[6] fd[2,8,0],fd[2,2,0]},
{fd[2,7,0],fd[2,6,0],-2/Sqrt[6] fd[2,8,0]}
};
mat[2,1,1]={
{1/Sqrt[2] fd[2,4,1]+1/Sqrt[6] fd[2,8,1],fd[2,5,1],fd[2,7,1]},
{fd[2,3,1],-1/Sqrt[2] fd[2,4,1]+1/Sqrt[6] fd[2,8,1],fd[2,6,1]},
{fd[2,1,1],fd[2,2,1],-2/Sqrt[6] fd[2,8,1]}
};


(* ::Text:: *)
(*\:5341\:91cd\:6001\:77e9\:9635*)


(* ::Text:: *)
(*\:8d28\:91cf\:9879*)


mat[3,1,2]=SymmetrizedArray[
{
{1,1,1}->fd[3,1,2],{1,1,2}->fd[3,2,2],{1,2,2}->fd[3,3,2],{2,2,2}->fd[3,4,2],
{1,1,3}->fd[3,5,2],{1,2,3}->fd[3,6,2],{2,2,3}->fd[3,7,2],
{1,3,3}->fd[3,8,2],{2,3,3}->fd[3,9,2],
{3,3,3}->fd[3,10,2]
}, {3,3,3}, 
Symmetric[All]
];


mat[3,1,0]=SymmetrizedArray[
{
{1,1,1}->fd[3,1,0],{1,1,2}->1/Sqrt[3]*fd[3,2,0],{1,2,2}->1/Sqrt[3]*fd[3,3,0],{2,2,2}->fd[3,4,0],
{1,1,3}->1/Sqrt[3] fd[3,5,0],{1,2,3}->1/Sqrt[6] fd[3,6,0],{2,2,3}->1/Sqrt[3] fd[3,7,0],
{1,3,3}->1/Sqrt[3] fd[3,8,0],{2,3,3}->1/Sqrt[3] fd[3,9,0],
{3,3,3}->fd[3,10,0]
}, {3,3,3}, 
Symmetric[All]
];
mat[3,1,1]=SymmetrizedArray[
{
{1,1,1}->fd[3,1,1],{1,1,2}->1/Sqrt[3]*fd[3,2,1],{1,2,2}->1/Sqrt[3]*fd[3,3,1],{2,2,2}->fd[3,4,1],
{1,1,3}->1/Sqrt[3] fd[3,5,1],{1,2,3}->1/Sqrt[6] fd[3,6,1],{2,2,3}->1/Sqrt[3] fd[3,7,1],
{1,3,3}->1/Sqrt[3] fd[3,8,1],{2,3,3}->1/Sqrt[3] fd[3,9,1],
{3,3,3}->fd[3,10,1]
}, {3,3,3}, 
Symmetric[All]
];


(* ::Section:: *)
(*gamma and derivative*)


(* ::Text:: *)
(*\:6d1b\:4f26\:5179\:5bf9\:8c61,\:901a\:8fc7\:4e0a\:503c ^:= \:5b9a\:4e49\:5b83\:548c\:5bf9\:8c61\:7684\:4f5c\:7528\:ff0c\:6bd4\:5982*)


(* ::DisplayFormula:: *)
(*\[Gamma]\[Mu]\[Nu],\[Gamma]\[Mu]\[Nu]\[Alpha],\[CapitalTheta]\[Mu]\[Nu]*)


(* ::Text:: *)
(*\:6d1b\:4ed1\:5179\:6307\:6807\:7684\:96c6\:5408*)


ldx::usage="ldx[index] \:7ed9\:51fa\:9884\:5b9a\:4e49\:7684\:7b2c index \:4e2a\:6d1b\:4f26\:5179\:6307\:6807,\:53ef\:4ee5\:4f7f\:7528{1,2,3}\:7b49 Part \:8bed\:6cd5\:6307\:5b9a\:591a\:4e2a
\:9ed8\:8ba4\:7ed9\:51fa\:4e86 enString/@{\[Mu],\[Nu],\[Alpha],\[Beta],5}";


(* ::Text:: *)
(*\:7ed9\:5bf9\:8c61\:52a0\:4e0a\:6307\:6807\:ff0c\:5e26\:6709ip# \:540e\:7f00\:7684\:51fd\:6570\:ff0c\:4f5c\:7528\:662f\:5bf9\:8f93\:5165\:7684\:53c2\:6570\:8fdb\:884c\:9884\:5904\:7406 iput pre-processing*)


Options[ltz]={"type"->"ltz","index"->"","direction"->"r"};
Attributes[ltz]={Listable};
ltz[sym:_,OptionsPattern[ltz]]:=Module[{idxes,type},
type=OptionValue["type"];
idxes=ldx[OptionValue["index"]];

Which[
MemberQ[{"\[PartialD]"},type],
pdeip1[ltzScriptip1[type,idxes],sym],

MemberQ[{"\[Gamma]","\[CapitalTheta]","\[Sigma]"},type],
gmaip1[ltzScriptip1[type,idxes],sym],

MemberQ[{"ltz"},type],ltzScriptip1[sym,idxes]
]
]
ltz::usage="\:7ed9\:7b26\:53f7\:52a0\:4e0a\:9884\:5b9a\:4e49\:7684\:6d1b\:4f26\:5179\:6307\:6807,\:6d1b\:4f26\:5179\:504f\:5bfc\:6570,\:4f3d\:9a6c\:77e9\:9635,\:7b49\:7b49\:3002\:4e5f\:53ef\:4ee5\:4f20\:5165\:81ea\:5b9a\:4e49\:7684\:6307\:6807,\:9ed8\:8ba4\:7ed9\:51fa\:4e86 enString/@{\[Mu],\[Nu],\[Alpha],\[Beta],5}
ltz[B,\"type\"\[Rule]\"ltz\",\"index\"\[Rule]{1,2}] 
ltz[B,\"type\"\[Rule]\"\[PartialD]\",\"index\"\[Rule]{1,2}] 
ltz[B,\"type\"\[Rule]\"\[Gamma]\",\"index\"\[Rule]{1,2}]
ltz[B,\"type\"\[Rule]\"\[CapitalTheta]\",\"index\"\[Rule]{1,2}]";


gmltz[{sym:_,idx1:_},{gamma:_,idx2:_}]:=ltz[
ltz[sym,"type"->"ltz","index"->idx1],
"type"->gamma,"index"->idx2]
gmltz::usage="gmltz[sym:_,gamma:_,idxes:_]\:ff0c\:7ed9\:5bf9\:8c61\:540c\:65f6\:52a0\:4e0a\:6d1b\:4f26\:5179\:6307\:6807\:548c\[Gamma]\:77e9\:9635\:ff0c\:5982
gmltz[{mat[2,1,0],{1}},{\"\[Gamma]\",{2,3,4}}]";


constantHeadList::usage="\:8ba1\:7b97\:4e2d\:4f7f\:7528\:7684\:5404\:79cd\:5e38\:6570\:7684\:5934\:90e8\:7684\:5217\:8868";
constantHeadList={Integer,Real,Rational,Complex,lecs};
constantQ[sym:_]:=Or[
NumericQ[sym],MatchQ[Head[sym],Alternatives@@constantHeadList]
]
constantQ::usage="constantQ[sym:_],\:68c0\:6d4b sym \:662f\:5426\:4e3a\:5e38\:6570";


(*\:4e0a\:9762\:4f7f\:7528\:4e86 pde,gma \:51fd\:6570*)
pdeip1::usage="pde[\[PartialD]\[Mu],B] \:504f\:5bfc\:6570\:51fd\:6570,\:5bf9\:5e38\:6570\:6c42\:5bfc\:7b49\:4e8e\:96f6,\[PartialD].(A.B)=A*\[PartialD]B+B*\[PartialD]A,\[PartialD].(A+B)=\[PartialD]A+\[PartialD]B";
pdeip1[pd:_,Plus[x:_,y:__]]:=Plus[pdeip1[pd,x],pdeip1[pd,Plus[y]]];
pdeip1[pd:_,Times[x:_,y:__]]:=Times[x,pdeip1[pd,Times[y]]]+Times[Times[y],pdeip1[pd,x]];
pdeip1[pd:_,sym:_]:=If[constantQ[sym],
0,pde[pd,sym]
];
(*\:4e0a\:9762\:4f7f\:7528\:4e86 pde,gma \:51fd\:6570*)
gmaip1::usage="gmaip1[\[Gamma]\[Mu],B,\"direction\"] \:4e58\:4e0a\[Gamma]\:51fd\:6570,\:9ed8\:8ba4\:662f\:4ece\:5de6\:8fb9\:4e58\:4e0a\:7684,\[Gamma].(A+B)=\[Gamma].A+\[Gamma].B, \[Gamma]C*B=C*\[Gamma].B";
gmaip1[gm:_,Plus[x:_,y:__]]:=Plus[gmaip1[gm,x],gmaip1[gm,Plus[y]]];
gmaip1[gm:_,Times[x:_,y:__]]:=Times[x,gmaip1[gm,Times[y]]];
gmaip1[gm:_,sym:_]:=If[constantQ[sym],
sym*gm,gma[gm,sym]
];
(*\:4e0a\:9762\:4f7f\:7528\:4e86 ltzScriptip1 \:51fd\:6570*)
ltzScriptip1::usage="\:7ed9\:5bf9\:8c61\:52a0\:4e0a\:6d1b\:4f26\:5179\:6307\:6807\:ff0c\:6ee1\:8db3\:52a0\:6cd5\:ff0c\:4e58\:6cd5\:5206\:914d\:ff0c\:65e0\:5e8f\:6027";
ltzScriptip1[Plus[x:_,y:__],index:_]:=Plus[ltzScriptip1[x,index],ltzScriptip1[Plus[y],index]];
ltzScriptip1[Times[x:_,y:__],index:_]:=Times[ltzScriptip1[x,index],ltzScriptip1[Times[y],index]];
ltzScriptip1[sym:_,idxes:__]:=If[constantQ[sym],
sym,ltzScript[sym,idxes]
];


(* ::Section:: *)
(*external currents*)


(* ::Text:: *)
(*\:5916\:6e90\:8026\:5408\:5230\:7684\:5404\:4e2a\:5f3a\:5b50\:6d41*)


vfd::usage="vfd[a,index],\:5404\:4e2a\:5916\:77e2\:91cf\:573a,\:4ece0\:52308
a \:662f SU(3)\:6307\:6807\:ff0cindex \:662f\:6d1b\:4f26\:5179\:6307\:6807";


(* ::Text:: *)
(*\:5404\:79cd\:6d41,\:77e2\:91cf\:6d41,\:8f74\:77e2\:6d41,\:6807\:91cf\:4ecb\:5b50\:534f\:53d8\:5bfc\:6570\:4e2d\:7684\:6d41*)


(* ::DisplayFormula:: *)
(*Subscript[D, \[Mu]]U=\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]U\)+I*[U,\[Lambda]^a]\!\(\*SubsuperscriptBox[\(v\), \(\[Mu]\), \(a\)]\)*)


(* meson \:77e2\:91cf\:6d41 *)
crt[1,index:_]:=(1/2)*(
mat[1,2,0] . ltz[mat[1,2,1],"type"->"\[PartialD]","index"->index]+
mat[1,2,1] . ltz[mat[1,2,0],"type"->"\[PartialD]","index"->index]
)+(-I/2)*Sum[(
mat[1,2,0] . gellmann[a] . mat[1,2,1]+
mat[1,2,1] . gellmann[a] . mat[1,2,0]
)*vfd[a,index]
,{a,1,8,1}];
(* meson \:8f74\:77e2\:6d41 *)
crt[2,index:_]:=(I/2)*(
mat[1,2,0] . ltz[mat[1,2,1],"type"->"\[PartialD]","index"->index]-
mat[1,2,1] . ltz[mat[1,2,0],"type"->"\[PartialD]","index"->index]
)+(1/2)*Sum[(
mat[1,2,0] . gellmann[a] . mat[1,2,1]-
mat[1,2,1] . gellmann[a] . mat[1,2,0]
)*vfd[a,index]
,{a,1,8,1}];
(*current 3 gellmann[a] v[a]\[Mu]*)
crt[3,index:_]:=Sum[gellmann[a]*vfd[a,index],{a,1,8,1}]


(* ::Text:: *)
(*\:53cd\:5e38\:78c1\:77e9\:7684\:6d41*)


vfduv[a_,id1_,id2_]:=ltz[vfd[a,id2],"type"->"\[PartialD]","index"->id1]-ltz[vfd[a,id1],"type"->"\[PartialD]","index"->id2]
vfduv::usage="vfduv[a_,id1_,id2_],\:4ea7\:751f\:7535\:78c1\:573a\:5f3a\:5f20\:91cf,\:5982 vfduv[a,1,2]";


crt[4,id1_,id2_]:=-1/2 Sum[
vfduv[a,id1,id2]*
(mat[1,2,1] . gellmann[a] . mat[1,2,0]+mat[1,2,0] . gellmann[a] . mat[1,2,1])
,{a,1,8,1}]


crt::usage="crt[num,index] \:7ed9\:51fa\:9884\:5b9a\:4e49\:7684\:7b2c num \:4e2a\:5f3a\:5b50\:6d41,\:5e26\:6709\:6307\:6807index,\:987a\:5e8f\:5982\:4e0b\:ff1a
\[CapitalGamma]\[Mu]::crt[1,index]
u\[Mu]::crt[2,index]
gellmann[a]*v[a]\[Mu] ::3
crt[4,id1,id2],Fuv\[ConjugateTranspose]";


(* ::Section:: *)
(*\:89c4\:8303\:534f\:53d8\:5bfc\:6570*)


(* ::Text:: *)
(*\:4ecb\:5b50\:7684\:534f\:53d8\:5bfc\:6570*)


gcd[1,vec:_,index:_,sym:_]:=ltz[lecs[1]*sym,"type"->"\[PartialD]","index"->index]-I*cmtm[Dot,vec,sym];


(* ::Text:: *)
(*\:516b\:91cd\:6001\:7684\:534f\:53d8\:5bfc\:6570*)


gcd[2,vec:_,index:_,sym:_]:=ltz[lecs[1]*sym,"type"->"\[PartialD]","index"->index]+
cmtm[Dot,vec,sym]-I*3*vfd[index,0]*gellmann[0] . sym;


(* ::Text:: *)
(*\:5341\:91cd\:6001\:7684\:534f\:53d8\:5bfc\:6570*)


gcd[3,vec:_,index:_,sym:_]:=Module[{inner},
CompoundExpression[
inner=Dot[vec,sym],
ltz[lecs[1]*sym,"type"->"\[PartialD]","index"->index]+
inner+TensorTranspose[inner,{2,3,1}]+TensorTranspose[inner,{3,1,2}]]-
I*3*vfd[index,0]*gellmann[0] . sym
]


gcd::usage="\:91cd\:5b50\:573a\:7684\:534f\:53d8\:5bfc\:6570\:9879,gcd[type,vec,index,sym]
crt[kind,index] [\:6d41\:7684\:7c7b\:578b,\:6d41\:7684\:6d1b\:4f26\:5179\:6307\:6807]
gcd[1,crt[3,1],1,mat[1,3,0]]\:7ed9\:51fa D\[Mu]U,
gcd[1,crt[3,1],1,mat[1,3,1]]\:7ed9\:51fa D\[Mu]U\[ConjugateTranspose],
gcd[2,crt[1,1],1,mat[2,1,0]] \:7ed9\:51fa D\[Mu]B,
gcd[3,crt[1,1],1,mat[3,1,0]] \:7ed9\:51fa D\[Mu]T,
#3::\:7ed9\:504f\:5bfc\:6570\:52a0\:4e0a\:6d1b\:4f26\:5179\:6307\:6807";


(* ::Section::Closed:: *)
(*hadron Lagrangians*)


(* ::Text:: *)
(*\:624b\:5f81 Lagrangian \:7684\:5404\:9879,\:6309\:7167\:8026\:5408\:5e38\:6570\:8fdb\:884c\:6392\:5217\:ff1a*)


lag=<||>;


(* ::Text:: *)
(*octet*)


(* ::DisplayFormula:: *)
(*Tr[\!\(\*OverscriptBox[\(B\), \(_\)]\)(I*\[Gamma] . D-MB)B]*)


lag["oct"]=Expand[Tr[mat[2,1,1] . (
gcd[2(*oct \:504f\:5bfc\:6570*),crt[1,1(*\:6d41\:7684\:6307\:6807*)],1(*\:504f\:5bfc\:7684\:6307\:6807*),I*ltz[mat[2,1,0],"type"->"\[Gamma]","index"->1]]
-mat[2,1,2]*mat[2,1,0](*\:8d28\:91cf\:9879*)
)]
];


(* ::Text:: *)
(*decuplet*)


(* ::DisplayFormula:: *)
(*\!\( *)
(*\*SubsuperscriptBox[\(T\), \(\[Mu]\), \(ijk\)]\ \((I**)
(*\*SuperscriptBox[\(\[Gamma]\), \(\[Mu]\[Nu]\[Alpha]\)] . *)
(*\*SubscriptBox[\(D\), \(\[Alpha]\)] - *)
(*\*SubscriptBox[\(M\), \(T\)] *)
(*\*SuperscriptBox[\(\[Gamma]\), \(\[Mu]\[Nu]\)])\) *)
(*\*SubsuperscriptBox[\(T\), \(\[Nu]\), \(ijk\)]\)*)


Module[{temp},
CompoundExpression[
temp=I*gcd[3,crt[1,3(*\:6d41\:7684\:6307\:6807*)],3(*\:504f\:5bfc\:7684\:6307\:6807*),ltz[mat[3,1,0],"type"->"\[Gamma]","index"->{1,2,3}]]-
mat[3,1,2]*ltz[mat[3,1,0],"type"->"\[Gamma]","index"->{1,2}],
lag["dec"]=Expand[Flatten[mat[3,1,1]] . Flatten[temp]]
]
];


(* ::Text:: *)
(*meson*)


(* ::DisplayFormula:: *)
(*f^2/4* Tr[Subscript[D, \[Mu]]U . (Subscript[D, \[Mu]]U)\[ConjugateTranspose]]=f^2/4* Tr[Subscript[D, \[Mu]]U . (Subscript[D, \[Mu]]U\[ConjugateTranspose])]*)


lag["mes"]=Expand[(lecs[3]^2/4)*Tr[gcd[1,crt[3,1],1,mat[1,3,0]] . gcd[1,crt[3,1],1,mat[1,3,1]]]];


(* ::Text:: *)
(*D,F*)


(* ::DisplayFormula:: *)
(*D*Tr[\!\(\*OverscriptBox[\(B\), \(_\)]\)\[Gamma]\[Mu]\[Gamma]5{u\[Mu],B}]+F*Tr[\!\(\*OverscriptBox[\(B\), \(_\)]\)\[Gamma]\[Mu]\[Gamma]5[u\[Mu],B]]*)


lag["DF"]=Expand[
(lecs[4])*Tr[mat[2,1,1] . cmtp[Dot,crt[2,1],ltz[mat[2,1,0],"type"->"\[Gamma]","index"->{1,5}]]]+
(lecs[5])*Tr[mat[2,1,1] . cmtm[Dot,crt[2,1],ltz[mat[2,1,0],"type"->"\[Gamma]","index"->{1,5}]]]
];


(* ::Text:: *)
(*calC,*)


(* ::Text:: *)
(*\:8fd9\:4e00\:9879\:6bd4\:8f83\:590d\:6742\:ff0c\:6c42\:548c\:5728\:4e09\:4e2a\:5c42\:6b21\:4e0a\:8fdb\:884c\:ff0cSU(3)\:5473\:9053\:7a7a\:95f4\:ff0cltz \:6d1b\:4f26\:5179\:6307\:6807\:7a7a\:95f4\:ff0cSpinor \:65cb\:91cf\:5206\:91cf\:3002\:4ed6\:4eec\:662f\:76f8\:4e92\:72ec\:7acb\:7684\:3002*)


(* ::DisplayFormula:: *)
(*\[CapitalTheta]\[Mu]\[Nu]=g\[Mu]\[Nu]-(z+1/2)\[Gamma]\[Mu]*\[Gamma]\[Nu],\:5728\:8ba1\:7b97\:4e2d\:9009\:53d6,z=1/2,*)


(* ::DisplayFormula:: *)
(*\[CapitalTheta]\[Mu]\[Nu]=g\[Mu]\[Nu]-\[Gamma]\[Mu].\[Gamma]\[Nu]=1/2 \[Gamma]\[Mu].\[Gamma]\[Nu]+1/2 \[Gamma]\[Nu].\[Gamma]\[Mu]-\[Gamma]\[Mu].\[Gamma]\[Nu]=1/2 \[Gamma]\[Nu].\[Gamma]\[Mu]-\[Gamma]\[Mu].\[Gamma]\[Nu]=I*\[Sigma]\[Mu]\[Nu],*)


(* ::DisplayFormula:: *)
(*\[Gamma]0.(I*\[Sigma]\[Mu]\[Nu])\[ConjugateTranspose].\[Gamma]0=-I*\[Sigma]\[Mu]\[Nu],*)


(* ::DisplayFormula:: *)
(*\[ScriptCapitalC]Tr[Contract[\[CurlyEpsilon] . Subscript[\!\(\*OverscriptBox[\(T\), \(_\)]\), \[Mu]] . Subscript[u, \[Nu]],{{1,4}}] . (\[CapitalTheta]^\[Mu]\[Nu] . B)]+\[ScriptCapitalC]Tr[\!\(\*OverscriptBox[\(B\), \(_\)]\) . Contract[Subscript[u, \[Mu]] . \[CapitalTheta]^\[Mu]\[Nu] . Subscript[T, \[Nu]] . (-\[CurlyEpsilon]),{{1,4}}]]*)


lag["calC"]=Expand[(lecs[6])*(
Tr[
TensorContract[
levi . ltz[mat[3,1,1],"type"->"ltz","index"->1(*dec\:7684\:6307\:6807*)] . crt[2,2(*\:6d41\:7684\:6307\:6807*)]
,{{1,4}}(*1,4\:6307\:6807\:7f29\:5e76*)] .
ltz[mat[2,1,0],"type"->"\[CapitalTheta]","index"->{1,2}]
(*\:6700\:540e\:6c42 Trace*)]
+Tr[
mat[2,1,1] .
TensorContract[
crt[2,1(*\:6d41\:7684\:6307\:6807*)] .
gmltz[{mat[3,1,0],{2}},{"\[CapitalTheta]",{1,2}}] . (-levi)(*\:8fd9\:91cc\:4f1a\:6709\:4e00\:4e2a\:8d1f\:53f7*)
,{{1,4}}(*1,4\:6307\:6807\:7f29\:5e76*)]
]
)];


(* ::Text:: *)
(*\:5341\:91cd\:6001\:4ecb\:5b50\:8026\:5408\:9879 calH,*)


(* ::DisplayFormula:: *)
(*T\[Nu]^ijl u\[Alpha]^kl \!\(\*OverscriptBox[\(T\[Mu]\), \(_\)]\)^ijk . \[Gamma]\[Mu]\[Nu]\[Alpha] . \[Gamma]5*)


Module[{temp1,temp2},
CompoundExpression[
temp1=ltz[mat[3,1,1],"type"->"\[Gamma]","index"->{1}],
temp2=crt[2,3(*\:6d41\:7684\:6307\:6807*)] . gmltz[{mat[3,1,0],{2}(*\:573a\:7684\:6307\:6807*)},{"\[Gamma]",{1,2,3,5}}],
lag["calH"]=Expand[(-lecs[7])*(Flatten[temp1] . Flatten[temp2])];
];
]


(* ::Section::Closed:: *)
(*magnetic Lagrangian*)


(* ::Text:: *)
(*\:516b\:91cd\:6001\:53cd\:5e38\:78c1\:77e9*)


(* ::DisplayFormula:: *)
(*1/(4mN) (c1*Tr[\!\(\*OverscriptBox[\(B\), \(_\)]\) . \[Sigma]^\[Mu]\[Nu] . {\!\( *)
(*\*SubsuperscriptBox[\(F\), \(\[Mu]\[Nu]\), \(+\)], B\)}]+c2*Tr[\!\(\*OverscriptBox[\(B\), \(_\)]\) . \[Sigma]^\[Mu]\[Nu] . [\!\( *)
(*\*SubsuperscriptBox[\(F\), \(\[Mu]\[Nu]\), \(+\)], B\)]]+c3*Tr[\!\(\*OverscriptBox[\(B\), \(_\)]\) . \[Sigma]^\[Mu]\[Nu] . B]*Tr[\!\(\*SubsuperscriptBox[\(F\), \(\[Mu]\[Nu]\), \(0\)]\)])*)


lag["magoct"]=Expand[
1/(4*mat[2,1,2][[1,3]])(*normarlized to nucleon mass magneton *) (
lecs[8]*Tr[mat[2,1,1] . cmtp[Dot,crt[4,1,2],ltz[mat[2,1,0],"type"->"\[Sigma]","index"->{1,2}]]]+
lecs[9]*Tr[mat[2,1,1] . cmtm[Dot,crt[4,1,2],ltz[mat[2,1,0],"type"->"\[Sigma]","index"->{1,2}]]]+
lecs[10]*Tr[mat[2,1,1] . ltz[mat[2,1,0],"type"->"\[Sigma]","index"->{1,2}]]*vfduv[0,1,2]
)
];


(* ::Text:: *)
(*\:516b\:91cd\:6001--\:5341\:91cd\:6001\:8f6c\:79fb\:78c1\:77e9*)


(* ::DisplayFormula:: *)
(*(i*e*c4)/(4mN) Subscript[F, \[Mu]\[Nu]]*(Subscript[\[Epsilon], ijk] Subscript[Q, il] Subscript[\!\(\*OverscriptBox[\(B\), \(_\)]\), jm]\[Gamma]^\[Mu] . \[Gamma]5 . \!\(\*SubsuperscriptBox[\(T\), \(klm\), \(\[Nu]\)]\)+Subscript[\[Epsilon], ijk] Subscript[Q, li] \!\(\*SubsuperscriptBox[*)
(*OverscriptBox[\(T\), \(_\)], \(klm\), \(\[Mu]\)]\)\[Gamma]^\[Nu] . \[Gamma]5 . Subscript[B, mj])*)


(* ::DisplayFormula:: *)
(*=(i*e*c4)/(4mN)*(\[Epsilon]^jki \!\(\*OverscriptBox[\(T\), \(_\)]\)^(\[Mu],iml) (\!\( *)
(*\*SubsuperscriptBox[\(F\), \(\[Mu]\[Nu]\), \(a\)] *)
(*\*SuperscriptBox[\(\[Lambda]\), \(a\)]\))^lj . \[Gamma]^\[Nu] . \[Gamma]^5 . B^mk+\!\(\*OverscriptBox[\(B\), \(_\)]\)^km (-\!\(\*SubsuperscriptBox[\(F\), \(\[Mu]\[Nu]\), \(a\)]\) \[Lambda]^a)^jl . (\[Gamma]^\[Mu] . \[Gamma]5) . T^(\[Nu],lmi) . (-\[Epsilon]^ikj))*)


(* ::DisplayFormula:: *)
(*=(i*e*c4)/(4mN) (Tr[\!\(\*SuperscriptBox[\((\[CurlyEpsilon] . *)
(*\*SuperscriptBox[*)
(*OverscriptBox[\(T\), \(_\)], \(\[Mu]\)] . *)
(*\*SubscriptBox[\(F\), \(\[Mu]\[Nu]\)])\), \({1, 4}\)]\) . \[Gamma]^\[Nu] . \[Gamma]^5 . B]+Tr[\!\(\*OverscriptBox[\(B\), \(_\)]\) . \[Gamma]^\[Mu] . \[Gamma]^5 . \!\(\*SuperscriptBox[\(( *)
(*\*SubscriptBox[\(F\), \(\[Mu]\[Nu]\)] . *)
(*\*SuperscriptBox[\(T\), \(\[Nu]\)] . \((\[CurlyEpsilon])\))\), \({1, 4}\)]\)])*)


lag["magodt"]=Expand[
(I*e*lecs[11]/(4*massN))*((*normarlized to nucleon mass magneton *) 
Sum[
vfduv[a,1,2]*Tr[
TensorContract[
levi . ltz[mat[3,1,1],"type"->"ltz","index"->{1}] .
gellmann[a]
,{{1,4}}] .
ltz[mat[2,1,0],"type"->"\[Gamma]","index"->{2,5}]
]
,{a,1,8,1}
]+
Sum[
vfduv[a,1,2]*Tr[
mat[2,1,1] .
TensorContract[
gellmann[a] .
gmltz[{mat[3,1,0],{2}},{"\[Gamma]",{1,5}}] . (levi)
,{{1,4}}]
]
,{a,0,8,1}
]
)
];


(* ::Text:: *)
(*\:5341\:91cd\:6001\:53cd\:5e38\:78c1\:77e9*)


(* ::DisplayFormula:: *)
(*-(F2T/(4mT)) \!\(\*SubsuperscriptBox[*)
(*OverscriptBox[\(T\), \(_\)], \(\[Mu]\), \(abc\)]\) \[Sigma]^\[Mu]\[Nu] Subscript[F, \[Mu]\[Nu]] Q^cd \!\(\*SubsuperscriptBox[\(T\), \(\[Nu]\), \(dba\)]\)=-(F2T/(4mT)) \!\(\*SubsuperscriptBox[*)
(*OverscriptBox[\(T\), \(_\)], \(\[Mu]\), \(abc\)]\) \[Sigma]^\[Mu]\[Nu] \!\(\*SubsuperscriptBox[\(F\), \(\[Mu]\[Nu]\), \(a\)]\) \[Lambda]^(a,cd) \!\(\*SubsuperscriptBox[\(T\), \(\[Nu]\), \(dba\)]\)*)


lag["magdec"]=Expand[TensorContract[
(-lecs[12]/(4*massT))*Sum[
vfduv[a,1,2]*(ltz[mat[3,1,1],"type"->"ltz","index"->{1}]) .
gellmann[a] . ((1/mat[3,1,2])*ltz[mat[3,1,0],"type"->"ltz","index"->{2}])
,{a,0,8,1}]
,{{1,3},{2,4}}]
];


(* ::Text:: *)
(*\:5f20\:91cf\:8026\:5408\:9879*)


(* ::DisplayFormula:: *)
(*\[ScriptCapitalL]=I/2 b9*Tr[\!\(\*OverscriptBox[\(B\), \(_\)]\) Subscript[A, \[Mu]]] . \[Sigma]^\[Mu]\[Nu] . Tr[Subscript[A, \[Nu]] . B]+I/2 b10*Tr[\!\(\*OverscriptBox[\(B\), \(_\)]\)[Subscript[A, \[Mu]],Subscript[A, \[Nu]]] . \[Sigma]^\[Mu]\[Nu] . B]+I/2 b11*Tr[\!\(\*OverscriptBox[\(B\), \(_\)]\){Subscript[A, \[Mu]],Subscript[A, \[Nu]]} . \[Sigma]^\[Mu]\[Nu] . B]*)


Module[{antiu,sigmaB},
antiu=cmtm[Dot,crt[2,1],crt[2,2]];(*\:624b\:5f81\:8f74\:77e2\:6d41\:7684\:5bf9\:6613\:5f0f*)
sigmaB=ltz[mat[2,1,0],"type"->"\[Sigma]","index"->{1,2}];(*\[Sigma]\[Mu]\[Nu].B*)
lag["bbb"]=Expand[
(-I)*lecs[13]*Tr[mat[2,1,1] . crt[2,1]]*Tr[crt[2,2] . sigmaB]+
(-I)*lecs[14]*Tr[mat[2,1,1] . cmtm[Dot,antiu,mat[2,1,0]] . sigmaB]+
(-I)*lecs[14]*Tr[mat[2,1,1] . cmtp[Dot,antiu,mat[2,1,0]] . sigmaB]
]
];


(* ::Text:: *)
(*\:628a\:76f8\:4e92\:4f5c\:7528\:9879\:7684 Lagrangian \:52a0\:8d77\:6765*)


lag["int"]=(
lag["oct"]+lag["dec"]+lag["mes"]+lag["D"]+lag["F"]+lag["calC"]
)/.{lecs[1]->0,lecs[3]->0,lecs[7]->0,vfd[__]->0,fd[_,_,2]->0};(*\:53bb\:6389\:52a8\:80fd\:9879,\[ScriptCapitalH]\:9879,\:8d28\:91cf\:9879\:5f97\:5230 \[ScriptCapitalL]int*)


(* ::Chapter:: *)
(*\:5b88\:6052\:6d41*)


(* ::Section:: *)
(*\:57fa\:672c\:5355\:5143*)


(* \:4ecb\:5b50\:7684\:77e2\:91cf\:6d41 *)
ntblk[1,a:_]:=(mat[1,2,0] . gellmann[a] . mat[1,2,1]+mat[1,2,1] . gellmann[a] . mat[1,2,0])
(* \:4ecb\:5b50\:7684\:8f74\:77e2\:6d41 *)
ntblk[2,a:_]:=(mat[1,2,0] . gellmann[a] . mat[1,2,1]-mat[1,2,1] . gellmann[a] . mat[1,2,0])
(*current 3 gellmann[a] v[a]\[Mu]*)
(*\:4e00\:822c\:7684\:6d41\:ff0cgellman \:77e9\:9635*)
ntblk[3,a:_]:=gellmann[a]


ntblk[4,a:_]:=Sum[
(mat[1,2,1] . gellmann[a] . mat[1,2,0]+mat[1,2,0] . gellmann[a] . mat[1,2,1])
,{a,1,8,1}]


ntblk::usage="ntblk[num,a] \:7ed9\:51fa\:6784\:6210\:8bfa\:7279\:6d41\:7684\:57fa\:672c\:5355\:5143,a \:5bf9\:5e94gellman\:77e9\:9635a,\:987a\:5e8f\:5982\:4e0b\:ff1a
1::u.\!\(\*SuperscriptBox[\(\[Lambda]\), \(a\)]\).u\[ConjugateTranspose]+u\[ConjugateTranspose].\!\(\*SuperscriptBox[\(\[Lambda]\), \(a\)]\).u
2::u.\!\(\*SuperscriptBox[\(\[Lambda]\), \(a\)]\).u\[ConjugateTranspose]-u\[ConjugateTranspose].\!\(\*SuperscriptBox[\(\[Lambda]\), \(a\)]\).u
3::\[Lambda][a]
4::u.\!\(\*SuperscriptBox[\(\[Lambda]\), \(a\)]\).u\[ConjugateTranspose]+u\[ConjugateTranspose].\!\(\*SuperscriptBox[\(\[Lambda]\), \(a\)]\).u";


(* ::Section:: *)
(*\:5b88\:6052\:6d41*)


(* ::Text:: *)
(*\:624b\:5f81 Lagrangian \:7684\:5b88\:6052\:6d41*)


(* ::Text:: *)
(*octet*)


(* ::DisplayFormula:: *)
(*1/2 Tr[\!\(\*OverscriptBox[\(B\), \(_\)]\)\[Gamma]^\[Mu] . [u . \[Lambda]^a . u\[ConjugateTranspose]+u\[ConjugateTranspose] . \[Lambda]^a . u,B]]*)


ntct["oct",a:_]:=Expand[(1/2)*lecs[1]*(
Tr[mat[2,1,1] . cmtm[
Dot,ntblk[1,a],ltz[mat[2,1,0],"type"->"\[Gamma]","index"->{1}]
]
]
)]


(* ::DisplayFormula:: *)
(*+(D/2)Tr[\!\(\*OverscriptBox[\(B\), \(_\)]\)\[Gamma]^\[Mu] . Subscript[\[Gamma], 5] . {u . \[Lambda]^a . u\[ConjugateTranspose]-u\[ConjugateTranspose] . \[Lambda]^a . u,B}]+F/2 Tr[\!\(\*OverscriptBox[\(B\), \(_\)]\)\[Gamma]^\[Mu] . Subscript[\[Gamma], 5] . [u . \[Lambda]^a . u\[ConjugateTranspose]-u\[ConjugateTranspose] . \[Lambda]^a . u,B]]+*)


ntct["DF",a:_]:=Expand[
(lecs[4]/2)*Tr[mat[2,1,1] . cmtp[Dot,ntblk[2,a],ltz[mat[2,1,0],"type"->"\[Gamma]","index"->{1,5}]]]+
(lecs[5]/2)*Tr[mat[2,1,1] . cmtm[Dot,ntblk[2,a],ltz[mat[2,1,0],"type"->"\[Gamma]","index"->{1,5}]]]
]


(* ::DisplayFormula:: *)
(*+(1/2) Subscript[\!\(\*OverscriptBox[\(T\), \(_\)]\), \[Nu]] \[Gamma]^\[Nu]\[Alpha]\[Mu] (u . \[Lambda]^a . u\[ConjugateTranspose]+u\[ConjugateTranspose] . \[Lambda]^a . u,Subscript[T, \[Alpha]])*)


ntct["dec",a:_]:=Module[{inner},
inner=(ntblk[1,a]) . gmltz[{mat[3,1,0],{3}},{"\[Gamma]",{2,3,1}}];
Expand[(1/2)*lecs[1]*(
Total[
ltz[mat[3,1,1],"type"->"ltz","index"->{2}]*(
inner+TensorTranspose[inner,{2,3,1}]+TensorTranspose[inner,{3,1,2}]
)
,3]
)]
]


(* ::Text:: *)
(*decuplet*)


(* ::DisplayFormula:: *)
(*\[ScriptCapitalC]\[CurlyEpsilon]^ijk \!\(\*SubsuperscriptBox[*)
(*OverscriptBox[\(T\), \(_\)], \(\[Mu]\), \(ilm\)]\) \[CapitalTheta]^\[Mu]\[Nu] \!\(\*SubsuperscriptBox[\(u\), \(\[Nu]\), \(lj\)]\) B^mk+\[ScriptCapitalC]\[CurlyEpsilon]^ijk \!\(\*OverscriptBox[\(B\), \(_\)]\)^km \[CapitalTheta]^\[Mu]\[Nu] \!\(\*SubsuperscriptBox[\(T\), \(\[Mu]\), \(ilm\)]\) \!\(\*SubsuperscriptBox[\(u\), \(\[Nu]\), \(lj\)]\)*)


(* ::DisplayFormula:: *)
(*\[ScriptCapitalC]\[CurlyEpsilon]^jki \!\(\*SubsuperscriptBox[*)
(*OverscriptBox[\(T\), \(_\)], \(\[Mu]\), \(iml\)]\) \!\(\*SubsuperscriptBox[\(u\), \(\[Nu]\), \(lj\)]\) \[CapitalTheta]^\[Mu]\[Nu] B^mk+\[ScriptCapitalC] \!\(\*OverscriptBox[\(B\), \(_\)]\)^km \!\( *)
(*\*SubsuperscriptBox[\(u\), \(\[Mu]\), \(jl\)] . *)
(*\*SuperscriptBox[\(\[CapitalTheta]\), \(\[Mu]\[Nu]\)] . *)
(*\*SubsuperscriptBox[\(T\), \(\[Nu]\), \(lmi\)]\)(-\[CurlyEpsilon]^ikj)*)


(* ::DisplayFormula:: *)
(*\[ScriptCapitalC]Tr[Contract[\[CurlyEpsilon] . Subscript[\!\(\*OverscriptBox[\(T\), \(_\)]\), \[Mu]] . Subscript[u, \[Nu]],{{1,4}}] . (\[CapitalTheta]^\[Mu]\[Nu] . B)]+\[ScriptCapitalC]Tr[\!\(\*OverscriptBox[\(B\), \(_\)]\) . Contract[Subscript[u, \[Mu]] . \[CapitalTheta]^\[Mu]\[Nu] . Subscript[T, \[Nu]] . (-\[CurlyEpsilon]),{{1,4}}]]*)


(* ::Text:: *)
(*\:5bf9\:5e94\:7684\:5b88\:6052\:6d41\:4e3a*)


(* ::DisplayFormula:: *)
(*+(\[ScriptCapitalC]/2)Tr[Contract[\[CurlyEpsilon] . Subscript[\!\(\*OverscriptBox[\(T\), \(_\)]\), \[Nu]] . (u . \[Lambda]^a . u\[ConjugateTranspose]-u\[ConjugateTranspose] . \[Lambda]^a . u),{{1,4}}] . (\[CapitalTheta]^\[Nu]\[Mu] . B)]+\[ScriptCapitalC]/2 Tr[\!\(\*OverscriptBox[\(B\), \(_\)]\) . Contract[(u . \[Lambda]^a . u\[ConjugateTranspose]-u\[ConjugateTranspose] . \[Lambda]^a . u) . \[CapitalTheta]^\[Mu]\[Nu] . Subscript[T, \[Nu]] . (-\[CurlyEpsilon]),{{1,4}}]]*)


ntct["calC",a:_]:=Expand[(lecs[6]/2)*(
Tr[
TensorContract[
levi . ltz[mat[3,1,1],"type"->"ltz","index"->{2}] . ntblk[2,a]
,{{1,4}}] .
ltz[mat[2,1,0],"type"->"\[CapitalTheta]","index"->{2,1}]]+
Tr[
mat[2,1,1] .
TensorContract[
ntblk[2,a] . gmltz[{mat[3,1,0],{2}},{"\[CapitalTheta]",{1,2}}] . (-levi)
,{{1,4}}]
]
)
]


(* ::Text:: *)
(*meson*)


(* ::DisplayFormula:: *)
(*f^2/4*I* Tr[\[PartialD]^\[Mu]U . (U\[ConjugateTranspose] . \[Lambda]^a-\[Lambda]^a . U\[ConjugateTranspose])+(U . \[Lambda]^a-\[Lambda]^a . U) . \[PartialD]^\[Mu]U\[ConjugateTranspose]]*)


(* ::DisplayFormula:: *)
(*f^2/4*I* Tr[\[PartialD]^\[Mu]U . [U\[ConjugateTranspose],\[Lambda]^a]+[U,\[Lambda]^a] . \[PartialD]^\[Mu]U\[ConjugateTranspose]]*)


ntct["mes",a:_]:=Expand[
(lecs[1])*(I*lecs[3]^2/4)*Tr[
ltz[mat[1,3,0],"type"->"\[PartialD]","index"->{1}] . cmtm[Dot,mat[1,3,1],gellmann[a]]+
cmtm[Dot,mat[1,3,0],gellmann[a]] . ltz[mat[1,3,1],"type"->"\[PartialD]","index"->{1}]
]
]


(* ::DisplayFormula:: *)
(*\!\( *)
(*\*SubsuperscriptBox[\(J\), \(0\), \(\[Mu]\)] = \(Tr[*)
(*\*SuperscriptBox[\(\[Lambda]\), \(0\)]]*Tr[*)
(*\*OverscriptBox[\(B\), \(_\)] . *)
(*\*SuperscriptBox[\(\[Gamma]\), \(\[Mu]\)] . B] + Tr[*)
(*\*SuperscriptBox[\(\[Lambda]\), \(0\)]]**)
(*\*SubscriptBox[*)
(*OverscriptBox[\(T\), \(_\)], \(\[Nu]\)] . *)
(*\*SuperscriptBox[\(\[Gamma]\), \(\[Nu]\[Alpha]\[Mu]\)] . *)
(*\*SubscriptBox[\(T\), \(\[Alpha]\)]\)\)*)


ntct["hadron0"]:=Expand[lecs[1]*(
(Tr[gellmann[0]])*Tr[mat[2,1,1] . ltz[mat[2,1,0],"type"->"\[Gamma]","index"->{1}]]+
(Tr[gellmann[0]])*Flatten[ltz[mat[3,1,1],"type"->"ltz","index"->{2}]] .
Flatten[gmltz[{mat[3,1,0],{3}},{"\[Gamma]",{2,3,1}}]]
)]


(* ::Text:: *)
(*\:5b88\:6052\:6d41\:7684\:516b\:91cd\:6001*)


ntct["hadron8",a:_]:=ntct["oct",a]+ntct["DF",a]+ntct["dec",a]+ntct["calC",a]+ntct["mes",a]/.{
lecs[3]->1/lecs[2]
}


(* ::Chapter:: *)
(*Gather and sort*)


(* ::Section::Closed:: *)
(*fields and coefficients*)


(* ::Text:: *)
(*\:628a\:573a\:5bf9\:8c61\:548c\:8026\:5408\:7cfb\:6570\:5206\:5f00*)


fdHeads={fd,gma,pde,ltzScript};
fdHeads::usage="\:573a\:5bf9\:8c61\:7684\:5934\:90e8";
fdForms:=Alternatives@@(Blank/@fdHeads);
fdForms::usage="\:573a\:5bf9\:8c61\:7684\:6a21\:5f0f";


funcPure::usage="funcPure[expr:_,patt:_] \:5c06\:8868\:8fbe\:5f0f\:4e2d\:7684 patt \:5bf9\:8c61\:53d6\:51fa\:6765\:ff0c\:5230\:6700\:6df1\:7684\:5c42\:6b21,\:9ed8\:8ba4\:7ed9\:51fa\:4e00\:4e2a\:5217\:8868";
funcPure::error="\:6240\:7ed9\:7684\:53c2\:6570`1`\:4e2d\:6ca1\:6709\:5bf9\:8c61 `2`";
funcPure[expr:_,patt:_]:=Module[{temp},
temp=forcelist[Cases[forcelist[expr],patt,Infinity]];
If[temp==={},
Return[Null[0]],
temp
]
]
(*\:5bf9\:53c2\:6570\:8fdb\:884c\:8fc7\:6ee4\:ff0c\:53ea\:8f93\:51fa\:5176\:4e2d\:7684fd[__]\:5bf9\:8c61*)


(* ::Text:: *)
(*\:5bf9\:5355\:4e2a\:9879\:63d0\:53d6\:51fa\:573a\:548c\:7cfb\:6570\:7684\:51fd\:6570\:ff0cDF \:9879\:5bf9\:5e94\:7684\:573a\:5bf9\:8c61\:76f8\:540c\:ff0c\:901a\:8fc7 Merge \:51fd\:6570\:8fdb\:884c\:5408\:5e76*)


fieldCoeDetach::check="\:68c0\:67e5\:8f93\:5165\:7684\:53d8\:91cf\:662f\:5426\:4e3a\:573a\:548c\:7cfb\:6570\:4e58\:8d77\:6765\:7684\:5355\:4e2a\:9879";
fieldCoeDetach[term:_]:=Module[{field,fieldexp,fieldcoe},
field=Cases[Variables[term],fdForms];(*\:7ed9\:51fa\:5f0f\:5b50\:4e2d\:7684\:573a\:5bf9\:8c61*)
fieldexp=Exponent[term,#1]&/@field;(*\:7ed9\:51fa\:573a\:5bf9\:8c61\:7684\:5e42\:6b21*)
fieldcoe=First[Values[CoefficientRules[term,field]]];
If[Length[fieldexp]===Length[field],
Rule[(*\:6700\:540e\:751f\:6210\:4e00\:4e2a\:5173\:8054\:ff0c\:5de6\:8fb9\:662f\:573a\:4ee5\:53ca\:6b21\:6570\:ff0c\:53f3\:8fb9\:662f\:7cfb\:6570*)
(SortBy[MapThread[Power,{field,fieldexp}],(*\:5c06\:573a\:548c\:5b83\:4eec\:7684\:5e42\:6b21\:91cd\:65b0\:7ec4\:5408\:5230\:4e00\:8d77*)
{(*\:63d0\:53d6\:51fa\:6bcf\:4e00\:7ec4\:573a\:5bf9\:8c61\:4e2d\:7684\:7c92\:5b50,\:5e76\:8fdb\:884c\:6392\:5e8f,\:4f9d\:6b21\:6309\:7167\:7c92\:5b50\:79cd\:7c7b,\:6b63\:53cd\:7c92\:5b50,\:4f8b\:5b50\:7f16\:53f7\:6392\:5e8f*)
(funcPure[#1,fd[__]][[1,1]])&,
(funcPure[#1,fd[__]][[1,3]])&,
(funcPure[#1,fd[__]][[1,2]])&
}
]
),fieldcoe
]
,Message[fieldCoeDetach::check]
]
]


addCoe[x:_,y:_]:=Module[{lowecs,lecexp},
lowecs=Cases[Variables[y],lecs[__]];(*\:7ed9\:51fa\:5f0f\:5b50\:4e2d\:7684\:8026\:5408\:5e38\:6570*)
lecexp=(Exponent[y,#1]&)/@lowecs;(*\:8026\:5408\:5e38\:6570\:7684\:6700\:9ad8\:5e42\:6b21*)
If[Length[lecexp]===Length[lowecs],
Rule[(*\:6700\:540e\:751f\:6210\:4e00\:4e2aRule\:ff0c\:5de6\:8fb9\:662f\:573a\:4ee5\:53ca\:6b21\:6570\:ff0c\:53f3\:8fb9\:662f\:7cfb\:6570*)
lagint["fds"->x,
"lec"->SortBy[MapThread[Power,{lowecs,lecexp}],(*\:5c06\:8026\:5408\:5e38\:6570\:548c\:5b83\:4eec\:7684\:5e42\:6b21\:91cd\:65b0\:7ec4\:5408\:5230\:4e00\:8d77,\:6700\:540e\:5f97\:5230\:4e00\:4e2a\:5217\:8868*)
{(*\:6309\:7167\:8026\:5408\:5e38\:6570\:7684\:5e8f\:53f7\:8fdb\:884c\:6392\:5e8f*)
(funcPure[#1,lecs[__]][[1,1]])&
}
]
]
,lagcoe[y]
]
,Message[fieldCoeDetach::check]
]
]


lagAssoc[lags:_]:=Module[{ass1},
ass1=Merge[fieldCoeDetach/@(List@@lags),Collect[Total[#1],lecs[2],(Simplify[#1,TimeConstraint->1]&)]&];
(*\:5c06\:76f8\:540c\:573a/\:76f8\:540c\:9876\:70b9\:5bf9\:5e94\:7684\:7cfb\:6570\:6536\:96c6\:8d77\:6765,\:6309\:7167 lecs[2]\:7684\:5e42\:6b21\:6392\:5217,\:5e76\:7528 Simplify \:5316\:7b80\:7cfb\:6570,keys\:662f\:573a\:5bf9\:8c61,values\:662f\:7cfb\:6570,*)
Merge[(*\:6700\:540e\:751f\:6210\:4e00\:4e2a\:5173\:8054\:ff0c\:5982\:679c\:7531\:91cd\:590d\:7684key\:ff0c\:5c31\:628a\:7cfb\:6570\:52a0\:8d77\:6765*)
KeyValueMap[addCoe,ass1(*\:751f\:6210\:7b2c\:4e8c\:4e2a\:5173\:8054\:ff0ckey \:4e2d\:5305\:62ec\:573a\:5bf9\:8c61\:ff0c\:548c\:8026\:5408\:7cfb\:6570\:ff0c\:65b9\:4fbf\:67e5\:627e*)
(*\:751f\:6210\:521d\:7ea7\:5173\:8054,\:628a\:76f8\:540c\:573a\:5bf9\:8c61\:5bf9\:5e94\:7684\:7cfb\:6570\:52a0\:8d77\:6765*)
],Collect[Total[#1],lecs[2],(Simplify[#1,TimeConstraint->1]&)]&
]
]


(* ::Section:: *)
(*\:67e5\:627e\:5c55\:793a*)


(* ::Text:: *)
(*\:67e5\:627e\:7279\:5b9a\:7684\:503c\:ff0c\:4ee5\:53ca\:5c55\:793a*)


(*\:5c55\:793a\:65f6\:7684\:6392\:7248\:6837\:5f0f*)
lagshow[lag:_]:=Multicolumn[lag,
4(*\:628a\:7ed3\:679c\:6392\:6210n\:5217*),Spacings->{1,1},
Background->{Automatic,{{LightOrange,White}}},Frame->All,FrameStyle->White
,Appearance->"Horizontal"
]


laglkp1[lag:_,fds:_List,lec:_List]:=lagshow[
Normal[
SortBy[(*\:5bf9\:7ed3\:679c\:8fdb\:884c\:6392\:5e8f*)
KeySelect[
lagAssoc[lag],
Module[{keyassc},
keyassc=Association@@#1;
And[
ContainsAll[funcPure[keyassc["fds"],fd[__]],
fds(*\:627e\:51fa\:5305\:542b\:6240\:6709\:7279\:5b9a\:573a\:5bf9\:8c61\:7684\:9879*)
],
ContainsOnly[
forcelist[keyassc["lec"]],
lec(*\:627e\:51fa\:53ea\:5305\:542b\:7279\:5b9a\:8026\:5408\:5e38\:6570\:7684\:9879*)
]
]
]&
],
{
Last[FactorTermsList@@#1]&
}
]
]
]
laglkp1::usage="laglkp1[lag:_,fds:_List,lec:_List],\:5bf9\:4e8e\:67d0\:4e2alag\:6c42\:548c\:5f0f\:ff0c\:628a\:6ee1\:8db3\:6761\:4ef6\:7684\:9879\:6311\:9009\:51fa\:6765.
\:7ed3\:679c\:6309\:7167\:8fd9\:4e00\:9879\:4e2d\:8026\:5408\:7cfb\:6570\:7684\:53d6\:503c\:6392\:5217\:ff0c\:5982
laglkp1[lag,
{},
{lecs[1],lecs[2],lecs[4],lecs[5]}
]";


(* ::Input:: *)
(**)


(* ::Chapter::Closed:: *)
(*\:683c\:5f0f\:5316\:8f93\:51fa*)


(* ::Text:: *)
(*\:5bf9 Lagrangian \:8fdb\:884c\:683c\:5f0f\:5316\:8f93\:51fa\:7684\:51fd\:6570*)


(* ::Section::Closed:: *)
(*field*)


(* ::Text:: *)
(*\:7ed9\:51fa\:7c92\:5b50\:548c\:53cd\:7c92\:5b50\:7684\:8bb0\:53f7*)


fieldScript::usage="\:573a\:7684\:5934\:90e8,\:628a\:53c2\:6570\:6392\:7248\:6210\:7c92\:5b50\:573a\:7684\:5f62\:5f0f,\:4e00\:822c\:901a\:8fc7 SuperScript \:5b9e\:73b0";


(*\:7c92\:5b50\:7684\:8bb0\:53f7*)
fdptc[0]=AssociationThread[
Range[1,3,1],
{
AssociationThread[
Range[0,8,1],
{
fieldScript["\[Eta]","0"],
fieldScript["\[Pi]","+"],fieldScript["\[Pi]","0"],fieldScript["\[Pi]","-"],
fieldScript["K","+"],fieldScript["K","-"],
fieldScript["K","0"],OverBar[fieldScript["K","0"]],
fieldScript["\[Eta]","8"]
}],
AssociationThread[
Range[1,8,1],
{
fieldScript["p",""],fieldScript["n",""],
fieldScript["\[CapitalSigma]","+"],fieldScript["\[CapitalSigma]","0"],fieldScript["\[CapitalSigma]","-"],
fieldScript["\[CapitalXi]","0"],fieldScript["\[CapitalXi]","-"],
fieldScript["\[CapitalLambda]",""]
}],
AssociationThread[
Range[1,10,1],
{
fieldScript["\[CapitalDelta]","++"],fieldScript["\[CapitalDelta]","+"],fieldScript["\[CapitalDelta]","0"],fieldScript["\[CapitalDelta]","-"],
fieldScript["\[CapitalSigma]","\[SixPointedStar]+"],fieldScript["\[CapitalSigma]","\[SixPointedStar]0"],fieldScript["\[CapitalSigma]","\[SixPointedStar]-"],
fieldScript["\[CapitalXi]","\[SixPointedStar]0"],fieldScript["\[CapitalXi]","\[SixPointedStar]-"],
fieldScript["\[CapitalOmega]","-"]
}]
}
];


(*\:53cd\:7c92\:5b50\:7684\:8bb0\:53f7*)
fdptc[1]=AssociationThread[
Range[1,3,1],
{
AssociationThread[
Range[0,8,1],
{
fieldScript["\[Eta]","0"],
fieldScript["\[Pi]","-"],fieldScript["\[Pi]","0"],fieldScript["\[Pi]","+"],
fieldScript["K","-"],fieldScript["K","+"],
OverBar[fieldScript["K","0"]],fieldScript["K","0"],
fieldScript["\[Eta]","8"]
}],
OverBar/@fdptc[0][2],
OverBar/@fdptc[0][3]
}
];


massScript::usage="\:573a\:7684\:8d28\:91cf\:7684\:5934\:90e8,\:628a\:53c2\:6570\:6392\:7248\:6210\:8d28\:91cf\:7684\:5f62\:5f0f,\:4e00\:822c\:901a\:8fc7 Subscript \:5b9e\:73b0";


(*\:5404\:79cd\:7c92\:5b50\:7684\:8d28\:91cf*)
fdptc[2]=Map[
massScript["M",#1]&,fdptc[0],{2}
];


(* ::Text:: *)
(*\:573a\:6392\:7248\:7684\:5b9e\:73b0\:51fd\:6570*)


fdFmt[kind_,num_,anti_]:=fdptc[anti][kind,num]


(*\:7279\:6b8a\:60c5\:51b5 \[Pi]0 \[Eta]8 \:6df7\:5408*)
fdFmt[1,28,2]=\!\(\*
TagBox[
StyleBox[
RowBox[{"massScript", "[", 
RowBox[{"\"\<M\>\"", ",", 
RowBox[{"fieldScript", "[", 
RowBox[{"\"\<\\[Pi]\>\"", ",", "\"\<0\>\""}], "]"}], ",", 
RowBox[{"fieldScript", "[", 
RowBox[{"\"\<\\[Eta]\>\"", ",", "\"\<8\>\""}], "]"}]}], "]"}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);
(*\:7279\:6b8a\:60c5\:51b5 \[CapitalSigma]0 \[CapitalLambda] \:6df7\:5408*)
fdFmt[2,48,2]=\!\(\*
TagBox[
StyleBox[
RowBox[{"massScript", "[", 
RowBox[{"\"\<M\>\"", ",", 
RowBox[{"fieldScript", "[", 
RowBox[{"\"\<\\[CapitalSigma]\>\"", ",", "\"\<0\>\""}], "]"}], ",", 
RowBox[{"fieldScript", "[", 
RowBox[{"\"\<\\[CapitalLambda]\>\"", ",", "\"\<\>\""}], "]"}]}], "]"}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);


(* ::Section::Closed:: *)
(*Lorentz*)


(* ::Text:: *)
(*\:5916\:6e90\:6392\:7248\:7684\:5b9e\:73b0\:51fd\:6570*)


vfdFmt[a_,index_]:=Subsuperscript["v",ldx[index],a];
vfdFmt::usage="vfdFmt[a,index],\:5c06\:8026\:5408\:7684\:5916\:90e8\:6d41\:683c\:5f0f\:5316";


(* ::Text:: *)
(*\:6d1b\:4f26\:5179\:6307\:6807\:7684\:5b9e\:73b0*)


ldxFmt[idx:_]:=If[MatchQ[idx,Alternatives[_Integer,_List,_Span]],
StringJoin[{"\[Mu]","\[Nu]","\[Alpha]","\[Beta]","5"}[[idx]]],
List[idx]
]


(* ::Text:: *)
(*gamma \:548c partial derivative \:7684\:5b9e\:73b0*)


pdeFmt[pd:_,sym:_]:=CenterDot[pd,sym]
gmaFmt[gma:_,sym:_]:=CenterDot[gma,sym]


(* ::Text:: *)
(*\:8026\:5408\:5e38\:6570\:7684\:5177\:4f53\:5b9e\:73b0*)


lecsFmt[x:_]:=<|
1->1,2->1/Subscript["f",""],3->Subscript["f",""],
4->Subscript["D",""],5->Subscript["F",""],
6->Subscript["\[ScriptCapitalC]",""],7->Subscript["\[ScriptCapitalH]",""],
8->Subscript["c","1"],9->Subscript["c","2"],10->Subscript["c","3"],
11->Subscript["c","4"],12->Superscript["F2","T"],
13->Subscript["b","9"],14->Subscript["b","10"],15->Subscript["b","11"]
|>[x];


(* ::Section:: *)
(*Lagrangian *)


(* ::Text:: *)
(*\:76f8\:4e92\:4f5c\:7528\:62c9\:6c0f\:91cf\:7684\:5934\:90e8 laginter\:ff0c\:4ee5\:53ca\:7cfb\:6570 lagcoe*)


lagintFmt[x:__]:=Grid[Values[Association@x],
Frame->All,FrameStyle->Directive[Lighter[Black,.7]],ItemStyle->{"InlineFormula"}
];
lagcoeFmt[x:__]:=Style[x,"InlineFormula"]


(* ::Text:: *)
(*\:4f7f\:7528 mma \:7684 Format \:51fd\:6570\:5bf9\:7ed3\:679c\:8fdb\:884c\:8f93\:51fa\:6392\:7248*)


(* ::Text:: *)
(*\:5b9a\:4e49\:6392\:7248\:7684\:8bed\:6cd5*)


(* ::DisplayFormula:: *)
(*Format[expr[x__],StandardForm]:=display[x]*)


assApp[expr:_,display:_]:=(Format[expr[x__],StandardForm]:=display[x])


(* ::Text:: *)
(*\:7ed9\:51fa\:81ea\:5b9a\:4e49\:51fd\:6570\:548c\:6392\:7248\:51fd\:6570\:7684\:5bf9\:5e94\:5173\:7cfb*)


assFmt=<|
fd->fdFmt,vfd->vfdFmt,lecs->lecsFmt,
massScript->Subscript,fieldScript->Superscript,
pde->pdeFmt,ltzScript->Subscript,
gma->gmaFmt,ldx->ldxFmt,
lagint->lagintFmt,lagcoe->lagcoeFmt
|>;


(* ::Text:: *)
(*\:8fdb\:884c\:6392\:7248\:6620\:5c04,\:5de6\:8fb9\:662f\:81ea\:5b9a\:4e49\:51fd\:6570,\:53f3\:8fb9\:662f\:6392\:7248\:51fd\:6570,assApp[key->Value]*)


KeyValueMap[assApp,assFmt];
