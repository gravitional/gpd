(* ::Package:: *)

(* ::Title:: *)
(*gen_lagrangian.wl*)


(* ::Text:: *)
(*notebook \:5907\:5fd8\:5f55*)


(* ::Text:: *)
(*\:5c55\:5f00\:62c9\:683c\:6717\:65e5\:ff0c\:8bb0\:5f55\:63a8\:5bfc\:51fa\:7684\:89c4\:5219*)


(* ::Text:: *)
(*\:5c1d\:8bd5\:4f7f\:7528\:51fd\:6570\:63a5\:53e3: f[i,j,k]\:ff0c\:5c06\:4e0d\:592a\:786e\:5b9a\:7684\:5b9e\:73b0\:90fd\:5b9a\:4e49\:6210\:51fd\:6570*)


(* ::Chapter::Closed:: *)
(*initial*)


(* ::Text:: *)
(*\:5f00\:59cb\:6b64\:5305*)


BeginPackage["chptgpd`"];


(* ::Section:: *)
(*path*)


(* ::Text:: *)
(*\:5b9a\:4e49\:4e00\:4e9b\:5e38\:7528\:7684\:51fd\:6570*)


(*\:5b9a\:4e49\:4e00\:4e2a\:786e\:4fdd\:5b57\:7b26\:4e32\:7684\:51fd\:6570*)
forcestr[x_,form_:InputForm]:=If[StringQ[x],x,ToString[x,form]]


(*\:5b9a\:4e49\:4e00\:4e2a\:786e\:4fdd\:5bf9\:8c61\:662f\:5217\:8868\:7684\:51fd\:6570*)
forcelist::usage="forcelist \:786e\:4fdd\:5bf9\:8c61\:53d8\:6210\:5217\:8868";
forcelist[x_]:=If[ListQ[x],x,{x}]


(*\:5b9a\:4e49\:4e00\:4e2a\:6253\:5370\:51fd\:6570*)
echo[x_]:=Print["----------------------------","\n",forcestr[x],"\n","----------------------------"];


(* ::Text:: *)
(*\:521d\:59cb\:5316\:7528\:6765\:8f93\:51fa\:7684\:5173\:8054*)


echo["initial export variables"]
export`vars=<||>


(* ::Text:: *)
(*\:8ba1\:7b97\:73af\:5883\:53c2\:91cf\:ff0c\:6bd4\:5982\:8def\:5f84*)


(*\:7ed9\:51fa\:8fdc\:7a0bgit\:4ed3\:5e93\:7684\:540d\:5b57\:ff0c\:53ea\:80fd\:662f\:4e00\:4e2a\:5b57\:7b26\:4e32\:ff0c\:4e0d\:5305\:542b/ or \ *)
git`remote`name=forcestr["gpd"];
(*\:811a\:672c\:7684\:8fd0\:884c\:6a21\:5f0f\:5224\:65ad\:ff0cTrue\:4ee3\:8868\:547d\:4ee4\:884c\:ff0cFalse\:4ee3\:8868\:524d\:7aef*)
boole`incmd=Not[SameQ[$ScriptCommandLine,{}]];


(*\:7ed9\:51fa\:7b14\:8bb0\:672c\:7684\:7edd\:5bf9\:8def\:5f84*)
filename=If[Not[boole`incmd],NotebookFileName[],$ScriptCommandLine[[1]]]


(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c\:ff0c\:5c31\:5237\:65b0\:7b14\:8bb0\:672c\:7684\:540d\:5b57\:ff0c\:5e76\:8c03\:51fa EditBar\:ff0c\:8bbe\:7f6e\:7f29\:653e\:6bd4\:4f8b\:4e3a1.6 *)
Once[If[
(* if $ScriptCommandLine==={}, the environment is frontend*)
Not[boole`incmd],
(*if execute in the frontend mode, refresh the title name*)
CompoundExpression[
(*\:5355\:5143\:5bf9\:8c61,\:7b2c\:4e00\:4e2a\:5355\:5143*)
cell`title=(Cells[][[1]]),
(*\:5237\:65b0\:7b2c\:4e00\:4e2a\:5355\:5143\:7684\:540d\:5b57*)
NotebookWrite[cell`title,Cell[FileNameSplit[filename][[-1]],"Title"]],
(*\:8c03\:51fa EditBar*)
SetOptions[EvaluationNotebook[],WindowToolbars->{"EditBar"},Magnification:>1.6]
];
]];
(*\:5982\:679c\:5728\:547d\:4ee4\:884c\:6267\:884c\:ff0c\:5c31\:6253\:5370\:63d0\:793a\:4fe1\:606f*)
If[boole`incmd,echo["Ready to execute this script"]]


(* ::Text:: *)
(*\:5b9a\:4e49\:672c\:5730git\:76ee\:5f55\:ff0c\:4e5f\:5c31\:662f\:7a0b\:5e8f\:7684\:6839\:76ee\:5f55*)


echo["the git`local`name is"]
If[MemberQ[FileNameSplit[ExpandFileName[filename]],git`remote`name],
git`local`name=FileNameJoin[Append[TakeWhile[FileNameSplit[ExpandFileName[filename]],
UnsameQ[#1,git`remote`name]&],git`remote`name]],
echo["this *.wl is in inappropriate directory"]
]


(* ::Text:: *)
(*\:5982\:679c\:5728\:547d\:4ee4\:884c\:6267\:884c\:ff0c\:5c31\:76f4\:63a5\:63a5\:53d7\:547d\:4ee4\:884c\:53d8\:91cf*)


(* ::Text:: *)
(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c\:ff0c\:5c31\:6a21\:62df\:547d\:4ee4\:884c\:8f93\:5165\:ff0c\:8ba1\:7b97\:6a21\:62df\:53d8\:91cf\:7684\:5b57\:7b26\:4e32\:5f62\:5f0f\:3002\:56e0\:4e3a\:547d\:4ee4\:884c\:4f20\:5165\:7684\:4e00\:822c\:662f\:5b57\:7b26\:4e32\:ff0c\:8fd9\:6837\:53ef\:4ee5\:7edf\:4e00\:5f62\:5f0f\:3002*)


If[boole`incmd,
(*\:5982\:679c\:5728\:547d\:4ee4\:884c\:6267\:884c*)
input`cml=$ScriptCommandLine,
(*++++++++++++++++++++++++++++++++++++++++*)
(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c*)
input`cml={filename,(*\:547d\:4ee4\:884c\:7b2c\:4e00\:4e2a\:53c2\:6570\:6c38\:8fdc\:662f\:6b64\:811a\:672c\:7684\:7edd\:5bf9\:8def\:5f84*)
(*\:5176\:4ed6\:53c2\:6570*)
input`simulation=forcestr[
(*\:5728\:8fd9\:91cc\:7ed9\:51fa\:7b2c2\[Rule]n\:4e2a\:53c2\:6570\:5728mathematica\:8bed\:6cd5\:4e0b\:7684\:5f62\:5f0f\:ff0c\:5916\:9762\:7684ToString\:4f1a\:81ea\:52a8\:8f6c\:6362*)
(*\:5c3d\:91cf\:591a\:4f7f\:7528Association\:7ed3\:6784*)
<|"unmerge"-><|
"normal"-><|"ge_charge"->"fig.baryons.normal.ge.charge.L-0.90.ci-1.50.m",
"ge_neutral"->"fig.baryons.normal.ge.neutral.L-0.90.ci-1.50.m"|>
|>,
(*++++++++++++++++++++++++++++*)
"merge"-><|
"normal"-><|
"gm_charge"->"fig.baryons.normal.gm.charge.L-0.90.ci-1.50.m",
"gm_neutral"->"fig.baryons.normal.gm.neutral.L-0.90.ci-1.50.m"
|>,
"unnormal"-><|
"gm_charge"->"fig.baryons.unnormal.gm.charge.L-0.90.ci-1.50.m",
"gm_neutral"->"fig.baryons.unnormal.gm.neutral.L-0.90.ci-1.50.m"
|>
|>
|>
(*\:5728\:8fd9\:91cc\:7ed9\:51fa\:7b2c2\[Rule]n\:4e2a\:53c2\:6570\:5728mathematica\:8bed\:6cd5\:4e0b\:7684\:5f62\:5f0f\:ff0c\:5916\:9762\:7684ToString\:4f1a\:81ea\:52a8\:8f6c\:6362*)
,InputForm]
}
];


input`cml


(* ::Section:: *)
(*processing parameters*)


(* ::Text:: *)
(*\:683c\:5f0f\:5316\:53c2\:6570*)


(* ::Text:: *)
(*\:5c06\:7b2c\:4e8c\:4e2a\:53c2\:6570\:8f6c\:6362\:6210\:5408\:6cd5\:7684\:8868\:8fbe\:5f0f*)


echo["the input parameter mfile list is:"]


mfiles`list=ToExpression[input`cml[[2]]]


(* ::Text:: *)
(*\:811a\:672c\:5185\:7f6e\:9884\:5b9a\:4e49\:53c2\:6570*)


parameter`order`string="full"
parameter`lambda0`string=ToString[NumberForm[0.90,{3,2}]]
parameter`ci`string=ToString[NumberForm[1.50,{3,2}]]


(* ::Chapter:: *)
(*Lagrangian*)


(* ::Section:: *)
(*constants*)


(* ::Text:: *)
(*Constant matrix, such as \[Lambda], Gellman *)


(* ::Text:: *)
(*Matrix, according to the definition of conserved current in the following text, we can know that \[Lambda] in the paper is twice the generator,*)


(* ::Text:: *)
(*The generator of SU(3) group, Gellman=\[Lambda]/2*)


dim=3;
dim::usage="\:4e3a\:5bf9\:79f0\:7fa4SU(3)\:9009\:53d6\:7684\:8868\:793a\:7684\:7ef4\:5ea6";


\[Lambda][num_]:={
IdentityMatrix[dim],
{{0,1,0},{1,0,0},{0,0,0}},
{{0,-I,0},{I,0,0},{0,0,0}},
{{1,0,0},{0,-1,0},{0,0,0}},
{{0,0,1},{0,0,0},{1,0,0}},
{{0,0,-I},{0,0,0},{I,0,0}},
{{0,0,0},{0,0,1},{0,1,0}},
{{0,0,0},{0,0,-I},{0,I,0}},
{{1/Sqrt[3],0,0},{0,1/Sqrt[3],0},{0,0,-(2/Sqrt[3])}}
}[[num]];
\[Lambda]::usage="\[Lambda][1]\:ff0c\:7ed9\:51fa\:7b2c1\:4e2a\[Lambda]\:77e9\:9635\:ff0c\:5b83\:662fGell-mann\:77e9\:9635\:76842\:500d";


glmn[x_]:=1/2*\[Lambda][x];
glmn::usage="glmn[1]\:ff0c\:7ed9\:51fa\:7b2c1\:4e2aGell-mann\:77e9\:9635";


(* ::Text:: *)
(*Levi - Civita \:5f20\:91cf *)


levi=LeviCivitaTensor[3];


(* ::Text:: *)
(*\:5b9a\:4e49\:5e7f\:4e49\:7684\:5bf9\:6613\:548c\:53cd\:5bf9\:6613\:ff0c\:5bf9\:4e8e\:67d0\:79cd\:8fd0\:7b97f\:ff0c*)


cmt`m[f_,x_,y_]:=f[x,y]-f[y,x];
cmt`m::usage="cmt`m[f,x,y]=f[x,y]-f[y,x] \:8fdb\:884c\:5bf9\:6613\:8fd0\:7b97";


cmt`p[f_,x_,y_]:=f[x,y]+f[y,x];
cmt`p::usage="cmt`m[f,x,y]=f[x,y]+f[y,x] \:8fdb\:884c\:53cd\:5bf9\:6613\:8fd0\:7b97";


(* ::Text:: *)
(*\:5404\:79cd\:8026\:5408\:5e38\:6570*)


lecs::usage="lecs[1]\:ff0c\:901a\:8fc7\:6574\:6570\:ff0c\:6307\:5b9a\:8981\:4f7f\:7528\:7684\:8026\:5408\:5e38\:6570\:ff0c\:987a\:5e8f
1,f,D,F,::1,4
calC,calH,::5,6
c1,c2,c3,::7,9
b9,b10,b11::10,2";


(* ::Section:: *)
(*matrix and Lorentz objects*)


(* ::Subsection:: *)
(*convention*)


(* ::DisplayFormula:: *)
(*kind::1,2,3*)
(*anti::0,1*)


(* ::Subsection:: *)
(*fields*)


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
(*\:7ed9\:51fa\:7c92\:5b50\:548c\:53cd\:7c92\:5b50\:7684\:8bb0\:53f7*)


(*\:7c92\:5b50\:7684\:8bb0\:53f7*)
fdptc[0]=AssociationThread[
Range[1,3,1],
{
AssociationThread[
Range[0,8,1],
{
FieldScript["\[Eta]","0"],
FieldScript["\[Pi]","+"],FieldScript["\[Pi]","0"],FieldScript["\[Pi]","-"],
FieldScript["K","+"],FieldScript["K","-"],
FieldScript["K","0"],OverBar[FieldScript["K","0"]],
FieldScript["\[Eta]","8"]
}],
AssociationThread[
Range[1,8,1],
{
FieldScript["p",""],FieldScript["n",""],
FieldScript["\[CapitalSigma]","+"],FieldScript["\[CapitalSigma]","0"],FieldScript["\[CapitalSigma]","-"],
FieldScript["\[CapitalXi]","0"],FieldScript["\[CapitalXi]","-"],
FieldScript["\[CapitalLambda]",""]
}],
AssociationThread[
Range[1,10,1],
{
FieldScript["\[CapitalDelta]","++"],FieldScript["\[CapitalDelta]","+"],FieldScript["\[CapitalDelta]","0"],FieldScript["\[CapitalDelta]","-"],
FieldScript["\[CapitalSigma]","\[SixPointedStar]+"],FieldScript["\[CapitalSigma]","\[SixPointedStar]0"],FieldScript["\[CapitalSigma]","\[SixPointedStar]-"],
FieldScript["\[CapitalXi]","\[SixPointedStar]0"],FieldScript["\[CapitalXi]","\[SixPointedStar]-"],
FieldScript["\[CapitalOmega]","-"]
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
FieldScript["\[Eta]","0"],
FieldScript["\[Pi]","-"],FieldScript["\[Pi]","0"],FieldScript["\[Pi]","+"],
FieldScript["K","-"],FieldScript["K","+"],
OverBar[FieldScript["K","0"]],FieldScript["K","0"],
FieldScript["\[Eta]","8"]
}],
OverBar/@fdptc[0][2],
OverBar/@fdptc[0][3]
}
];


(*\:5404\:79cd\:7c92\:5b50\:7684\:8d28\:91cf*)
fdptc[2]=Map[
MassScript["M",#1]&,fdptc[0],{2}
];


fd[kind_,num_,anti_]:=fdptc[anti][kind,num]


(*\:7279\:6b8a\:60c5\:51b5 \[Pi]0 \[Eta]8 \:6df7\:5408*)
fd[1,28,2]=\!\(\*
TagBox[
StyleBox[
RowBox[{"MassScript", "[", 
RowBox[{"\"\<M\>\"", ",", 
RowBox[{"FieldScript", "[", 
RowBox[{"\"\<\\[Pi]\>\"", ",", "\"\<0\>\""}], "]"}], ",", 
RowBox[{"FieldScript", "[", 
RowBox[{"\"\<\\[Eta]\>\"", ",", "\"\<8\>\""}], "]"}]}], "]"}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);
(*\:7279\:6b8a\:60c5\:51b5 \[CapitalSigma]0 \[CapitalLambda] \:6df7\:5408*)
fd[2,48,2]=\!\(\*
TagBox[
StyleBox[
RowBox[{"MassScript", "[", 
RowBox[{"\"\<M\>\"", ",", 
RowBox[{"FieldScript", "[", 
RowBox[{"\"\<\\[CapitalSigma]\>\"", ",", "\"\<0\>\""}], "]"}], ",", 
RowBox[{"FieldScript", "[", 
RowBox[{"\"\<\\[CapitalLambda]\>\"", ",", "\"\<\>\""}], "]"}]}], "]"}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);


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
mat[1,2,0]=IdentityMatrix[dim]+I/Sqrt[2]*mat[1,1,0];
mat[1,3,0]=IdentityMatrix[dim]+I*Sqrt[2]*mat[1,1,0];


(* ::DisplayFormula:: *)
(*1,2,0,::u*)
(*1,3,0,::U*)


mat[1,1,1]=mat[1,1,0];
mat[1,2,1]=IdentityMatrix[dim]-I/Sqrt[2]*mat[1,1,0];
mat[1,3,1]=IdentityMatrix[dim]-I*Sqrt[2]*mat[1,1,0];


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


(* ::Text:: *)
(*\:6d1b\:4f26\:5179\:5bf9\:8c61,\:901a\:8fc7\:4e0a\:503c ^:= \:5b9a\:4e49\:5b83\:548c\:5bf9\:8c61\:7684\:4f5c\:7528\:ff0c\:6bd4\:5982*)


(* ::DisplayFormula:: *)
(* \[Gamma]\[Mu]\[Nu],\[Gamma]\[Mu]\[Nu]\[Alpha],\[CapitalTheta]\[Mu]\[Nu]*)


(* ::Text:: *)
(*\:6d1b\:4ed1\:5179\:6307\:6807\:7684\:96c6\:5408*)


ltzidx[index:_]:=If[StringQ[index],
index,
Sequence@@((forcestr/@{\[Mu],\[Nu],\[Alpha],\[Beta],5})[[index]])
];
ltzidx::usage="ltzidx[index] \:7ed9\:51fa\:9884\:5b9a\:4e49\:7684\:7b2c index \:4e2a\:6d1b\:4f26\:5179\:6307\:6807,\:53ef\:4ee5\:6307\:5b9a\:591a\:4e2a
\:9ed8\:8ba4\:7ed9\:51fa\:4e86 forcestr/@{\[Mu],\[Nu],\[Alpha],\[Beta],5}";


(* ::Text:: *)
(*\:8d1f\:8d23\:7ed9\:5355\:4e2a\:5bf9\:8c61\:52a0\:4e0a\:6307\:6807*)


Options[ltz]={"index"->""};
ltz[sym:_,OptionsPattern[ltz]]:=Subscript[sym,OptionValue["index"]]
Attributes[ltz]={Listable};
ltz::usage=" ltz[\[Gamma],\"index\"\[Rule]ltzidx[{1,2}]] \:7ed9\:7b26\:53f7\:52a0\:4e0a\:9884\:5b9a\:4e49\:7684\:6d1b\:4f26\:5179\:6307\:6807\:ff0c\:4e5f\:53ef\:4ee5\:4f20\:5165\:81ea\:5b9a\:4e49\:7684\:6307\:6807
\:9ed8\:8ba4\:7ed9\:51fa\:4e86 forcestr/@{\[Mu],\[Nu],\[Alpha],\[Beta],5}";


(* ::Text:: *)
(*\:534f\:53d8\:5bfc\:6570\:ff0c\:901a\:8fc7\:4e0a\:503c ^:= \:5b9a\:4e49\:5b83\:548c\:7c92\:5b50\:ff0c\:4e5f\:5c31\:662f particle \:7684\:4f5c\:7528*)


Options[cvd]={"index"->""};
cvd[sym:_,OptionsPattern[cvd]]:=CenterDot[Subscript["\[PartialD]",OptionValue["index"]],sym]
Attributes[cvd]={Listable};
cvd::usage=" cvd[u,\"index\"\[Rule]ltzidx[{1,2}]] \:7ed9\:7b26\:53f7\:52a0\:4e0a\:6d1b\:4f26\:5179\:534f\:53d8\:7684\:5bfc\:6570\:ff0c\:4e5f\:53ef\:4ee5\:4f20\:5165\:81ea\:5b9a\:4e49\:7684\:6307\:6807
\:9ed8\:8ba4\:7ed9\:51fa\:4e86 forcestr/@{\[Mu],\[Nu],\[Alpha],\[Beta],5}";


(* ::Text:: *)
(*\:5404\:4e2a\:5916\:77e2\:91cf\:573a*)


Options[vfd]={"index"->""};
vfd::usage="vfd[8],\:5404\:4e2a\:5916\:77e2\:91cf\:573a\:ff0c\:4ece0\:52308";
vfd[a_,OptionsPattern[vfd]]:=Subsuperscript["v",OptionValue["index"],forcestr[a]] 


Options[gma]={"index"->""};
gma[sym:_,OptionsPattern[gma]]:=CenterDot[Subscript["\[Gamma]",OptionValue["index"]],sym]
Attributes[gma]={Listable};
gma::usage="gma[T,\"index\"\[Rule]ltzidx[{1,2}]] \:5728T\:7684\:5de6\:8fb9\:4e58\:4e0a\:4e00\:4e2a \!\(\*SuperscriptBox[\(\[Gamma]\), \(\[Mu]\[Nu]\)]\),\:4e5f\:53ef\:4ee5\:4f20\:5165\:81ea\:5b9a\:4e49\:7684\:6307\:6807
\:9ed8\:8ba4\:7ed9\:51fa\:4e86 forcestr/@{\[Mu],\[Nu],\[Alpha],\[Beta],5}";


CenterDot[x:_,Times[y:_,z:_]]:=Times[y,CenterDot[x,z]]
CenterDot::usage="CenterDot \:4e0e \:4e58\:6cd5\:53ef\:4ee5\:4ea4\:6362\:6b21\:5e8f";


(* ::Text:: *)
(*\:5404\:79cd\:6d41\:ff0c\:77e2\:91cf\:6d41\:ff0c\:8f74\:77e2\:6d41\:ff0c\:6807\:91cf\:6d41*)


crt::usage="crt[num] \:7ed9\:51fa\:9884\:5b9a\:4e49\:7684\:7b2c num \:4e2a\:5f3a\:5b50\:6d41\:ff0c\:987a\:5e8f\:5982\:4e0b\:ff1a
\!\(\*SubscriptBox[\(\[CapitalGamma]\), \(\[Mu]\)]\)::1
\!\(\*SubscriptBox[\(u\), \(\[Mu]\)]\)::2";


crt[1,index:_]:=1/2 (mat[1,2,0].cvd[mat[1,2,1],"index"->ltzidx[index]]+mat[1,2,1].cvd[mat[1,2,0],"index"->ltzidx[index]])-I/2 Sum[
(mat[1,2,0].\[Lambda][a].mat[1,2,1]+mat[1,2,1].\[Lambda][a].mat[1,2,0])*vfd[a,"index"->ltzidx[index]]
,{a,1,8,1}];
crt[2,index:_]:=I/2 (mat[1,2,0].cvd[mat[1,2,1],"index"->ltzidx[index]]-mat[1,2,1].cvd[mat[1,2,0],"index"->ltzidx[index]])+1/2 Sum[
(mat[1,2,0].\[Lambda][a].mat[1,2,1]-mat[1,2,1].\[Lambda][a].mat[1,2,0])*vfd[a,"index"->ltzidx[index]]
,{a,1,8,1}];


(* ::Subsection:: *)
(*gauge covariant derivative \:89c4\:8303\:534f\:53d8\:5bfc\:6570*)


gcd[2,vec:_,index:_,sym:_]:=cvd[sym,"index"->ltzidx[index]]+cmt`m[Dot,vec,sym]-I*ltz[vfd[0],"index"->ltzidx[index]]*IdentityMatrix[dim].sym;
gcd[3,vec:_,index:_,sym:_]:=Module[{inner},
CompoundExpression[
inner=Dot[vec,sym],
cvd[sym,"index"->ltzidx[index]]+inner+TensorTranspose[inner,{2,3,1}]+TensorTranspose[inner,{3,1,2}]]-
I*ltz[vfd[0],"index"->ltzidx[index]]*IdentityMatrix[dim].sym
]
gcd::usage="\:91cd\:5b50\:573a\:7684\:534f\:53d8\:5bfc\:6570\:9879\:ff0c
gcd[2,crt[1],1,mat[2,1,0]] \:7ed9\:51fa \!\(\*SubscriptBox[\(D\), \(\[Mu]\)]\)B,
gcd[3,crt[1],1,mat[3,1,0]] \:7ed9\:51fa \!\(\*SubscriptBox[\(D\), \(\[Mu]\)]\)T,
#3->1::\:7ed9\:504f\:5bfc\:6570\:52a0\:4e0a\:6d1b\:4f26\:5179\:6307\:6807";


(* ::Section:: *)
(*Lagrangian*)


(* ::Text:: *)
(*\:624b\:5f81 Lagrangian \:7684\:5404\:9879\:ff0c\:6309\:7167\:8026\:5408\:5e38\:6570\:8fdb\:884c\:6392\:5217\:ff1a*)


lag=<||>;


lag[lecs[1]]=Tr[I*mat[2,1,1].gcd[2,crt[1,1],1,mat[2,1,0]]-mat[2,1,2]*mat[2,1,1].mat[2,1,0]]+
CompoundExpression[
temp=Flatten[
mat[3,1,1].(
I*gma[gcd[3,crt[1,1],1,mat[3,1,0]],"index"->ltzidx[{1,2,3}]]-
mat[3,1,2]*gma[mat[3,1,0],"index"->ltzidx[{1,2}]]
)],
temp.temp
];


(* ::Text:: *)
(*\:4ecb\:5b50\:9879*)


lag[lecs[2]]=f^2/4*Tr[covde["mes",{"\[Mu]"}].matr["par","U"].ConjugateTranspose(covde["mes",{"\[Mu]"}].matr["dag","U"])]


(* ::Text:: *)
(*\:91cd\:5b50\:516b\:91cd\:6001\:9879*)


lag["oct"]=Tr[matr["anti","oct"].(I*covde["oct"]-mass["oct"]).matr["par","oct"]]


lag["D"]=lecs["D"]*Tr[matr["ant","oct"].lore["gamma",{"\[Mu]","5"}].
antitate[lore[curr["axial"],{"\[Mu]"}],matr["par","oct"]]]


lag["F"]=lecs["F"]*Tr[matr["ant","oct"].lore["gamma",{"\[Mu]","5"}].
comutate[lore[curr["axial"],{"\[Mu]"}],matr["par","oct"]]]


(* ::Text:: *)
(*\:91cd\:5b50\:5341\:91cd\:6001\:9879*)


Module[{decnumber},
decnumber=Length[matr["par","dec"]];
lag["dec"]=Sum[
lore[matr["ant","dec",{i,j,k}],{"\[Mu]"}]*
(I*lore["gamma",{"\[Mu]","\[Nu]","\[Alpha]"}]*covde["dec",{"\[Alpha]"}]-mass["dec"]*lore["gamma",{"\[Mu]","\[Nu]"}])*
lore[matr["par","dec",{i,j,k}],{"\[Nu]"}]
,{i,1,decnumber,1}
,{j,1,decnumber,1}
,{k,1,decnumber,1}
]
]


(* ::Text:: *)
(*BT\[Phi]\:8026\:5408\:9879 calC*)


Module[{decnumber},
decnumber=Length[matr["par","dec",All]];
lag["calC"]=Sum[
lecs["calC"]*levi[i,j,k]*
lore[matr["ant","dec",{i,l,m}],{"\[Mu]"}]*
lore["\[CapitalTheta]",{"\[Mu]","\[Nu]"}]*
lore[curr["axial",{l,j}],{"\[Nu]"}]*
lore[matr["par","oct",{m,k}]]
,{i,1,decnumber,1}
,{j,1,decnumber,1}
,{k,1,decnumber,1}
,{l,1,decnumber,1}
,{m,1,decnumber,1}
]
]


(* ::Text:: *)
(*\:5341\:91cd\:6001\:4ecb\:5b50\:8026\:5408\:9879 calH*)


Module[{decnumber},
decnumber=Length[matr["par","dec",All]];
lag["calH"]=Sum[
lecs["calH"]*
lore[matr["ant","dec",{i,j,k}],{"\[Mu]"}]*
lore["gamma",{"\[Mu]","\[Nu]","\[Alpha]","5"}]*
lore[curr["axial",{k,l}],{"\[Alpha]"}]*
lore[matr["par","dec",{i,j,l}],{"\[Nu]"}]
,{i,1,decnumber,1}
,{j,1,decnumber,1}
,{k,1,decnumber,1}
,{l,1,decnumber,1}
]
]


(* ::Chapter:: *)
(*formatting*)


(* ::Text:: *)
(*\:7ed9\:51fa\:8f93\:51fa\:5f62\:5f0f\:ff0c\:7ed3\:5408\:5404\:79cd\:65e0\:5185\:7f6e\:610f\:4e49\:7684\:7ed3\:6784\:ff0c\:5982 Superscript[]*)


(* ::Text:: *)
(*\:7136\:540e\:5bf9\:4e8e\:67d0\:4e00\:7c7b\:ff0c\:5b83\:7684\:5206\:91cf\:6709\:4e24\:79cd\:7f16\:7801\:65b9\:5f0f\:ff0c\:4e00\:79cd\:662f\:6570\:5b57\:ff0c\:4e00\:79cd\:662f\:5b57\:7b26\:4e32\:3002*)


setmode[x:_Integer:1]:=CompoundExpression[
mes
];


(* ::Chapter:: *)
(*export*)


(*\:8f93\:51fa\:76ee\:5f55*)
export`dir=FileNameJoin[{git`local`name,"/expression-results/"}]


echo["the output name list"]


(*\:5bfc\:51fa\:5230\:786c\:76d8\:65f6\:7684\:6587\:4ef6\:540d\:79f0*)
export`name`list=<|
"ge_charge"->FileNameJoin[{
export`dir,"fig.baryons.ge.charge.L-"<>parameter`lambda0`string<>".ci-"<>parameter`ci`string<>".pdf"
}],
"ge_neutral"->FileNameJoin[{
export`dir,"fig.baryons.ge.neutral.L-"<>parameter`lambda0`string<>".ci-"<>parameter`ci`string<>".pdf"
}],
"gm_charge"->FileNameJoin[{
export`dir,"fig.baryons.gm.charge.L-"<>parameter`lambda0`string<>".ci-"<>parameter`ci`string<>".pdf"
}],
"gm_neutral"->FileNameJoin[{
export`dir,"fig.baryons.gm.neutral.L-"<>parameter`lambda0`string<>".ci-"<>parameter`ci`string<>".pdf"
}]
|>


echo["output status"]


(*\:5185\:5b58\:4e2d\:4fdd\:5b58\:6570\:636e\:7684\:5173\:8054\:ff0c\:5c06\:8981\:8f93\:51fa\:7684\:540d\:5b57*)
export`vars//Keys


Module[{namelists},
namelists=Keys[export`vars];

Do[
Export[export`name`list[name],export`vars[name]];

(*\:5982\:679c\:8fdb\:884c\:5230\:6700\:540e\:4e00\:4e2a\:ff0c\:5c31\:6253\:5370\:8f93\:51fa\:5b8c\:6210*)
If[SameQ[name,Last[namelists]],
echo["Done"]
]
,{name,namelists} (*\:5bf9\:5bfc\:51fa\:5217\:8868\:4e2d\:7684\:6bcf\:4e00\:4e2a\:53d8\:91cf\:8fdb\:884c\:5bfc\:51fa*)
]

]


(* ::Text:: *)
(*\:7ed3\:675f\:6b64\:5305*)


EndPackage[] 


(* ::Text:: *)
(*\:5982\:679c\:5728\:524d\:7aef\:7b14\:8bb0\:672c\:4e2d\:6267\:884c\:ff0c\:6e05\:9664\:6240\:6709\:8f93\:51fa\:7ed3\:679c\:ff0c\:51cf\:5c0f\:6587\:4ef6\:4f53\:79ef*)


If[Not[boole`incmd],FrontEndExecute[FrontEndToken["DeleteGeneratedCells"]]]
