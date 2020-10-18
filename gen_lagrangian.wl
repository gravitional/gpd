(* ::Package:: *)

(* ::Title:: *)
(*gen_lagrangian.wl*)


(* ::Text:: *)
(*notebook \:5907\:5fd8\:5f55*)


(* ::Text:: *)
(*\:5c55\:5f00\:62c9\:683c\:6717\:65e5\:ff0c\:8bb0\:5f55\:63a8\:5bfc\:51fa\:7684\:89c4\:5219*)


(* ::Text:: *)
(*\:5c1d\:8bd5\:4f7f\:7528\:51fd\:6570\:63a5\:53e3: f[i,j,k]\:ff0c\:5c06\:4e0d\:592a\:786e\:5b9a\:7684\:5b9e\:73b0\:90fd\:5b9a\:4e49\:6210\:51fd\:6570*)


(* ::Chapter:: *)
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
exportvars=<||>


(* ::Text:: *)
(*\:8ba1\:7b97\:73af\:5883\:53c2\:91cf\:ff0c\:6bd4\:5982\:8def\:5f84*)


(*\:7ed9\:51fa\:8fdc\:7a0bgit\:4ed3\:5e93\:7684\:540d\:5b57\:ff0c\:53ea\:80fd\:662f\:4e00\:4e2a\:5b57\:7b26\:4e32\:ff0c\:4e0d\:5305\:542b/ or \ *)
gitRemoteName=forcestr["gpd"];
(*\:811a\:672c\:7684\:8fd0\:884c\:6a21\:5f0f\:5224\:65ad\:ff0cTrue\:4ee3\:8868\:547d\:4ee4\:884c\:ff0cFalse\:4ee3\:8868\:524d\:7aef*)
booleincmd=Not[SameQ[$ScriptCommandLine,{}]];


(*\:7ed9\:51fa\:7b14\:8bb0\:672c\:7684\:7edd\:5bf9\:8def\:5f84*)
filename=If[Not[booleincmd],NotebookFileName[],$ScriptCommandLine[[1]]]


(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c\:ff0c\:5c31\:5237\:65b0\:7b14\:8bb0\:672c\:7684\:540d\:5b57\:ff0c\:5e76\:8c03\:51fa EditBar\:ff0c\:8bbe\:7f6e\:7f29\:653e\:6bd4\:4f8b\:4e3a1.6 *)
Once[If[
(* if $ScriptCommandLine==={}, the environment is frontend*)
Not[booleincmd],
(*if execute in the frontend mode, refresh the title name*)
CompoundExpression[
(*\:5355\:5143\:5bf9\:8c61,\:7b2c\:4e00\:4e2a\:5355\:5143*)
celltitle=(Cells[][[1]]),
(*\:5237\:65b0\:7b2c\:4e00\:4e2a\:5355\:5143\:7684\:540d\:5b57*)
NotebookWrite[celltitle,Cell[FileNameSplit[filename][[-1]],"Title"]],
(*\:8c03\:51fa EditBar*)
SetOptions[EvaluationNotebook[],WindowToolbars->{"EditBar"},Magnification:>1.6]
];
]];
(*\:5982\:679c\:5728\:547d\:4ee4\:884c\:6267\:884c\:ff0c\:5c31\:6253\:5370\:63d0\:793a\:4fe1\:606f*)
If[booleincmd,echo["Ready to execute this script"]]


(* ::Text:: *)
(*\:5b9a\:4e49\:672c\:5730git\:76ee\:5f55\:ff0c\:4e5f\:5c31\:662f\:7a0b\:5e8f\:7684\:6839\:76ee\:5f55*)


echo["the gitlocalname is"]
If[MemberQ[FileNameSplit[ExpandFileName[filename]],gitremotename],
gitlocalname=FileNameJoin[Append[TakeWhile[FileNameSplit[ExpandFileName[filename]],
UnsameQ[#1,gitremotename]&],gitremotename]],
echo["this *.wl is in inappropriate directory"]
]


(* ::Text:: *)
(*\:5982\:679c\:5728\:547d\:4ee4\:884c\:6267\:884c\:ff0c\:5c31\:76f4\:63a5\:63a5\:53d7\:547d\:4ee4\:884c\:53d8\:91cf*)


(* ::Text:: *)
(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c\:ff0c\:5c31\:6a21\:62df\:547d\:4ee4\:884c\:8f93\:5165\:ff0c\:8ba1\:7b97\:6a21\:62df\:53d8\:91cf\:7684\:5b57\:7b26\:4e32\:5f62\:5f0f\:3002\:56e0\:4e3a\:547d\:4ee4\:884c\:4f20\:5165\:7684\:4e00\:822c\:662f\:5b57\:7b26\:4e32\:ff0c\:8fd9\:6837\:53ef\:4ee5\:7edf\:4e00\:5f62\:5f0f\:3002*)


If[booleincmd,
(*\:5982\:679c\:5728\:547d\:4ee4\:884c\:6267\:884c*)
inputcml=$ScriptCommandLine,
(*++++++++++++++++++++++++++++++++++++++++*)
(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c*)
inputcml={filename,(*\:547d\:4ee4\:884c\:7b2c\:4e00\:4e2a\:53c2\:6570\:6c38\:8fdc\:662f\:6b64\:811a\:672c\:7684\:7edd\:5bf9\:8def\:5f84*)
(*\:5176\:4ed6\:53c2\:6570*)
inputsimulation=forcestr[
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


inputcml


(* ::Section::Closed:: *)
(*processing parameters*)


(* ::Text:: *)
(*\:683c\:5f0f\:5316\:53c2\:6570*)


(* ::Text:: *)
(*\:5c06\:7b2c\:4e8c\:4e2a\:53c2\:6570\:8f6c\:6362\:6210\:5408\:6cd5\:7684\:8868\:8fbe\:5f0f*)


echo["the input parameter mfile list is:"]


mfileslist=ToExpression[inputcml[[2]]]


(* ::Text:: *)
(*\:811a\:672c\:5185\:7f6e\:9884\:5b9a\:4e49\:53c2\:6570*)


(* ::Chapter:: *)
(*Lagrangian*)


(* ::Section:: *)
(*convention*)


(* ::DisplayFormula:: *)
(*kind::1,2,3*)
(*anti::0,1*)


(* ::Section::Closed:: *)
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


cmtm[f_,x_,y_]:=f[x,y]-f[y,x];
cmtm::usage="cmtm[f,x,y]=f[x,y]-f[y,x] \:8fdb\:884c\:5bf9\:6613\:8fd0\:7b97";


cmtp[f_,x_,y_]:=f[x,y]+f[y,x];
cmtp::usage="cmtm[f,x,y]=f[x,y]+f[y,x] \:8fdb\:884c\:53cd\:5bf9\:6613\:8fd0\:7b97";


(* ::Text:: *)
(*\:5404\:79cd\:8026\:5408\:5e38\:6570*)


lecs::usage="lecs[1]\:ff0c\:901a\:8fc7\:6574\:6570\:ff0c\:6307\:5b9a\:8981\:4f7f\:7528\:7684\:8026\:5408\:5e38\:6570\:ff0c\:987a\:5e8f
1,1/f,f,D,F,::1,5
calC,calH,::6,7
c1,c2,c3,::8,10
b9,b10,b11::11,13
\:5176\:4e2d lecs[1] \:4fdd\:7559\:4e3a\:52a8\:80fd\:9879\:7684\:8026\:5408\:5e38\:6570";


(* ::Section::Closed:: *)
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


fieldScript::usage="\:573a\:7684\:5934\:90e8\:ff0c\:628a\:53c2\:6570\:6392\:7248\:6210\:7c92\:5b50\:573a\:7684\:5f62\:5f0f\:ff0c\:4e00\:822c\:901a\:8fc7 SuperScript \:5b9e\:73b0";


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


massScript::usage="\:573a\:7684\:8d28\:91cf\:7684\:5934\:90e8\:ff0c\:628a\:53c2\:6570\:6392\:7248\:6210\:8d28\:91cf\:7684\:5f62\:5f0f\:ff0c\:4e00\:822c\:901a\:8fc7 Subscript \:5b9e\:73b0";


(*\:5404\:79cd\:7c92\:5b50\:7684\:8d28\:91cf*)
fdptc[2]=Map[
massScript["M",#1]&,fdptc[0],{2}
];


fd[kind_,num_,anti_]:=fdptc[anti][kind,num]


(*\:7279\:6b8a\:60c5\:51b5 \[Pi]0 \[Eta]8 \:6df7\:5408*)
fd[1,28,2]=\!\(\*
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
fd[2,48,2]=\!\(\*
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
(*Lorentz objects*)


(* ::Text:: *)
(*\:6d1b\:4f26\:5179\:5bf9\:8c61,\:901a\:8fc7\:4e0a\:503c ^:= \:5b9a\:4e49\:5b83\:548c\:5bf9\:8c61\:7684\:4f5c\:7528\:ff0c\:6bd4\:5982*)


(* ::DisplayFormula:: *)
(* \[Gamma]\[Mu]\[Nu],\[Gamma]\[Mu]\[Nu]\[Alpha],\[CapitalTheta]\[Mu]\[Nu]*)


(* ::Text:: *)
(*\:6d1b\:4ed1\:5179\:6307\:6807\:7684\:96c6\:5408*)


ltzidx[index:_]:=If[StringQ[index],
index,
StringJoin[{"\[Mu]","\[Nu]","\[Alpha]","\[Beta]","5"}[[index]]]
];
ltzidx::usage="ltzidx[index] \:7ed9\:51fa\:9884\:5b9a\:4e49\:7684\:7b2c index \:4e2a\:6d1b\:4f26\:5179\:6307\:6807,\:53ef\:4ee5\:4f7f\:7528{1,2,3}\:7b49 Part \:8bed\:6cd5\:6307\:5b9a\:591a\:4e2a
\:9ed8\:8ba4\:7ed9\:51fa\:4e86 forcestr/@{\[Mu],\[Nu],\[Alpha],\[Beta],5}";


(* ::Text:: *)
(*\:7ed9\:5bf9\:8c61\:52a0\:4e0a\:6307\:6807\:ff0c\:5e26\:6709ip# \:540e\:7f00\:7684\:51fd\:6570\:ff0c\:4f5c\:7528\:662f\:5bf9\:8f93\:5165\:7684\:53c2\:6570\:8fdb\:884c\:9884\:5904\:7406 iput pre-processing*)


Options[ltz]={"type"->"ltz","index"->"","direction"->"r"};
Attributes[ltz]={Listable};
ltz[sym:_,OptionsPattern[ltz]]:=Module[{idxes},
idxes=ltzidx[OptionValue["index"]];
Switch[OptionValue["type"],
"ltz",ltzScriptip1[sym,idxes],
"\[PartialD]",pdeip1[ltzScriptip1["\[PartialD]",idxes],sym],
"\[Gamma]",gmaip1[ltzScriptip1["\[Gamma]",idxes],sym],
"\[CapitalTheta]",gmaip1[ltzScriptip1["\[CapitalTheta]",idxes],sym]
]
]
ltz::usage="\:7ed9\:7b26\:53f7\:52a0\:4e0a\:9884\:5b9a\:4e49\:7684\:6d1b\:4f26\:5179\:6307\:6807,\:6d1b\:4f26\:5179\:504f\:5bfc\:6570,\:4f3d\:9a6c\:77e9\:9635,\:7b49\:7b49\:3002\:4e5f\:53ef\:4ee5\:4f20\:5165\:81ea\:5b9a\:4e49\:7684\:6307\:6807,\:9ed8\:8ba4\:7ed9\:51fa\:4e86 forcestr/@{\[Mu],\[Nu],\[Alpha],\[Beta],5}
ltz[B,\"type\"\[Rule]\"ltz\",\"index\"\[Rule]{1,2}] 
ltz[B,\"type\"\[Rule]\"\[PartialD]\",\"index\"\[Rule]{1,2}] 
ltz[B,\"type\"\[Rule]\"\[Gamma]\",\"index\"\[Rule]{1,2}]
ltz[B,\"type\"\[Rule]\"\[CapitalTheta]\",\"index\"\[Rule]{1,2}]";


constantHeadList::usage="\:8ba1\:7b97\:4e2d\:4f7f\:7528\:7684\:5404\:79cd\:5e38\:6570\:7684\:5934\:90e8\:7684\:5217\:8868";
constantHeadList={Integer,Real,Rational,Complex,lecs};


(*\:4e0a\:9762\:4f7f\:7528\:4e86 pde,gma \:51fd\:6570*)
pdeip1::usage="pde[\[PartialD]\[Mu],B] \:504f\:5bfc\:6570\:51fd\:6570,\:5bf9\:5e38\:6570\:6c42\:5bfc\:7b49\:4e8e\:96f6,\[PartialD].(A.B)=A*\[PartialD]B+B*\[PartialD]A,\[PartialD].(A+B)=\[PartialD]A+\[PartialD]B";
pdeip1[pd:_,Plus[x:_,y:__]]:=Plus[pdeip1[pd,x],pdeip1[pd,Plus[y]]]
pdeip1[pd:_,Times[x:_,y:__]]:=Times[x,pdeip1[pd,Times[y]]]+Times[Times[y],pdeip1[pd,x]]
pdeip1[pd:_,sym:_]:=If[MemberQ[constantHeadList,Head[N[sym,3]]],
0,pde[pd,sym]
]
(*\:4e0a\:9762\:4f7f\:7528\:4e86 pde,gma \:51fd\:6570*)
gmaip1::usage="gma[\[Gamma]\[Mu],B,\"direction\"] \:4e58\:4e0a\[Gamma]\:51fd\:6570,\:9ed8\:8ba4\:662f\:4ece\:5de6\:8fb9\:4e58\:4e0a\:7684,\[Gamma].(A+B)=\[Gamma].A+\[Gamma].B, \[Gamma]C*B=C*\[Gamma].B";
gmaip1[gm:_,Plus[x:_,y:__]]:=Plus[gmaip1[gm,x],gmaip1[gm,Plus[y]]]
gmaip1[gm:_,Times[x:_,y:__]]:=Times[x,gmaip1[gm,Times[y]]]
gmaip1[gm:_,sym:_]:=If[MemberQ[constantHeadList,Head[N[sym,3]]],
sym*gm,gma[gm,sym]
]
(*\:4e0a\:9762\:4f7f\:7528\:4e86 ltzScriptip1 \:51fd\:6570*)
ltzScriptip1::usage="\:7ed9\:5bf9\:8c61\:52a0\:4e0a\:6d1b\:4f26\:5179\:6307\:6807\:ff0c\:6ee1\:8db3\:52a0\:6cd5\:ff0c\:4e58\:6cd5\:5206\:914d\:ff0c\:65e0\:5e8f\:6027";
ltzScriptip1[Plus[x:_,y:__],index:_]:=Plus[ltzScriptip1[x,index],ltzScriptip1[Plus[y],index]]
ltzScriptip1[Times[x:_,y:__],index:_]:=Times[ltzScriptip1[x,index],ltzScriptip1[Times[y],index]]
ltzScriptip1[sym:_,idxes:__]:=If[MemberQ[constantHeadList,Head[N[sym,3]]],
sym,ltzScript[sym,idxes]
]


(* ::Text:: *)
(*\:5404\:4e2a\:5916\:77e2\:91cf\:573a*)


vfd::usage="vfd[index,a],\:5404\:4e2a\:5916\:77e2\:91cf\:573a,\:4ece0\:52308
a \:662f SU(3)\:6307\:6807\:ff0cindex \:662f\:6d1b\:4f26\:5179\:6307\:6807";


(* ::Text:: *)
(*\:5404\:79cd\:6d41,\:77e2\:91cf\:6d41,\:8f74\:77e2\:6d41,\:6807\:91cf\:6d41*)


crt::usage="crt[num,index] \:7ed9\:51fa\:9884\:5b9a\:4e49\:7684\:7b2c num \:4e2a\:5f3a\:5b50\:6d41,\:5e26\:6709\:6307\:6807index,\:987a\:5e8f\:5982\:4e0b\:ff1a
\!\(\*SubscriptBox[\(\[CapitalGamma]\), \(\[Mu]\)]\)::1
\!\(\*SubscriptBox[\(u\), \(\[Mu]\)]\)::2
\[Lambda][a]*v[a]\[Mu] ::3";


(* meson vector current *)
crt[1,index:_]:=1/2*(
mat[1,2,0].ltz[mat[1,2,1],"type"->"\[PartialD]","index"->index]+
mat[1,2,1].ltz[mat[1,2,0],"type"->"\[PartialD]","index"->index]
)-
I/2*Sum[(
mat[1,2,0].\[Lambda][a].mat[1,2,1]+mat[1,2,1].\[Lambda][a].mat[1,2,0]
)*vfd[index,a]
,{a,1,8,1}];
(* meson axial current *)
crt[2,index:_]:=I/2 (
mat[1,2,0].ltz[mat[1,2,1],"type"->"\[PartialD]","index"->index]-
mat[1,2,1].ltz[mat[1,2,0],"type"->"\[PartialD]","index"->index]
)+
1/2*Sum[(
mat[1,2,0].\[Lambda][a].mat[1,2,1]-mat[1,2,1].\[Lambda][a].mat[1,2,0]
)*vfd[index,a]
,{a,1,8,1}];
(*current 3 \[Lambda][a] v[a]\[Mu]*)
crt[3,index:_]:=Sum[\[Lambda][a]*vfd[index,a]
,{a,1,8,1}]


(* ::Section:: *)
(*gauge covariant derivative \:89c4\:8303\:534f\:53d8\:5bfc\:6570*)


gcd[1,vec:_,index:_,sym:_]:=ltz[lecs[1]*sym,"type"->"\[PartialD]","index"->index]-I*cmtm[Dot,vec,sym];


gcd[2,vec:_,index:_,sym:_]:=ltz[lecs[1]*sym,"type"->"\[PartialD]","index"->index]+
cmtm[Dot,vec,sym]-I*vfd[index,0]*IdentityMatrix[dim].sym;


gcd[3,vec:_,index:_,sym:_]:=Module[{inner},
CompoundExpression[
inner=Dot[vec,sym],
ltz[lecs[1]*sym,"type"->"\[PartialD]","index"->index]+
inner+TensorTranspose[inner,{2,3,1}]+TensorTranspose[inner,{3,1,2}]]-
I*vfd[index,0]*IdentityMatrix[dim].sym
]


gcd::usage="\:91cd\:5b50\:573a\:7684\:534f\:53d8\:5bfc\:6570\:9879,gcd[2|3,vec,index,sym]
crt[kind,index] [\:6d41\:7684\:7c7b\:578b\:ff0c\:6d41\:7684\:6d1b\:4f26\:5179\:6307\:6807]
gcd[1,crt[3,1],1,mat[1,3,0]]\:7ed9\:51fa \!\(\*SubscriptBox[\(D\), \(\[Mu]\)]\)U,
gcd[1,crt[3,1],1,mat[1,3,1]]\:7ed9\:51fa \!\(\*SubscriptBox[\(D\), \(\[Mu]\)]\)U\[ConjugateTranspose],
gcd[2,crt[1,1],1,mat[2,1,0]] \:7ed9\:51fa \!\(\*SubscriptBox[\(D\), \(\[Mu]\)]\)B,
gcd[3,crt[1,1],1,mat[3,1,0]] \:7ed9\:51fa \!\(\*SubscriptBox[\(D\), \(\[Mu]\)]\)T,
#3->1::\:7ed9\:504f\:5bfc\:6570\:52a0\:4e0a\:6d1b\:4f26\:5179\:6307\:6807";


(* ::Section:: *)
(*Lagrangian*)


(* ::Text:: *)
(*\:5bf9 Lagrangian \:8fdb\:884c\:683c\:5f0f\:5316\:8f93\:51fa\:7684\:51fd\:6570*)


vfdFmt[x_,y_]:=Subsuperscript["v",ltzidx[x],y];


lagfmt=ReplaceAll[{
massScript->Subscript,fieldScript->Superscript,pde->CenterDot,ltzScript->Subscript,gma->CenterDot,vfd->vfdFmt,
lecs[1]->1,lecs[2]->1/Subscript["f",""],lecs[3]->Subscript["f",""],lecs[4]->Subscript["D",""],lecs[5]->Subscript["F",""],
lecs[6]->Subscript["\[ScriptCapitalC]",""],lecs[7]->Subscript["\[ScriptCapitalH]",""]
}];


lagfmt2=ReplaceAll[{

}];


(* ::Text:: *)
(*\:624b\:5f81 Lagrangian \:7684\:5404\:9879,\:6309\:7167\:8026\:5408\:5e38\:6570\:8fdb\:884c\:6392\:5217\:ff1a*)


lag=<||>;


(* ::Text:: *)
(*octet*)


(* ::DisplayFormula:: *)
(*Tr[\!\(\*OverscriptBox[\(B\), \(_\)]\)(iD-MB)B]*)


(*(Times[lecs[1],#1]&)/@*)
lag["oct"]=Tr[mat[2,1,1].(
gcd[2,crt[1,1(*\:6d41\:7684\:6307\:6807*)],1(*\:504f\:5bfc\:7684\:6307\:6807*),I*ltz[mat[2,1,0],"type"->"\[Gamma]","index"->1]]
-mat[2,1,2](*\:8d28\:91cf\:9879*)*mat[2,1,0]
)];


(* ::Text:: *)
(*decuplet*)


(* ::DisplayFormula:: *)
(*T\[Mu]^ijk (I*\[Gamma]\[Mu]\[Nu]\[Alpha].D\[Alpha]-MT)T\[Nu]^ijk*)


Module[{temp},
CompoundExpression[
temp=I*gcd[3,crt[1,3(*\:6d41\:7684\:6307\:6807*)],3(*\:504f\:5bfc\:7684\:6307\:6807*),ltz[mat[3,1,0],"type"->"\[Gamma]","index"->{1,2,3}]]-
mat[3,1,2]*ltz[mat[3,1,0],"type"->"\[Gamma]","index"->{1,2}],
lag["dec"]=Flatten[mat[3,1,1]].Flatten[temp]
]];


(* ::Text:: *)
(*meson*)


(* ::DisplayFormula:: *)
(*f^2/4 Tr[D\[Mu]U (D\[Mu]U)\[ConjugateTranspose]]*)


lag["mes"]=lecs[3]^2/4*Tr[gcd[1,crt[3,1],1,mat[1,3,0]].gcd[1,crt[3,1],1,mat[1,3,1]]];


(* ::Text:: *)
(*D,F*)


(* ::DisplayFormula:: *)
(*D*Tr[\!\(\*OverscriptBox[\(B\), \(_\)]\)\[Gamma]\[Mu]\[Gamma]5{u\[Mu],B}]+F*Tr[\!\(\*OverscriptBox[\(B\), \(_\)]\)\[Gamma]\[Mu]\[Gamma]5[u\[Mu],B]]*)


lag["D"]=lecs[4]*Tr[mat[2,1,1].cmtp[Dot,crt[2,1],ltz[mat[2,1,0],"type"->"\[Gamma]","index"->{1,5}]]];


lag["F"]=lecs[5]*Tr[mat[2,1,1].cmtm[Dot,crt[2,1],ltz[mat[2,1,0],"type"->"\[Gamma]","index"->{1,5}]]];


(* ::Text:: *)
(*calC,*)


(* ::DisplayFormula:: *)
(*\[CapitalTheta]\[Mu]\[Nu]=g\[Mu]\[Nu]-(z+1/2)\[Gamma]\[Mu]*\[Gamma]\[Nu],set,z=-1,*)


(* ::DisplayFormula:: *)
(*\[CapitalTheta]\[Mu]\[Nu]=g\[Mu]\[Nu]+1/2\[Gamma]\[Mu]*\[Gamma]\[Nu],\[Gamma]0.\[CapitalTheta]\[Mu]\[Nu].\[Gamma]0=g\[Mu]\[Nu]+1/2\[Gamma]\[Nu]*\[Gamma]\[Mu]=-g\[Mu]\[Nu]-1/2\[Gamma]\[Mu]*\[Gamma]\[Nu]=-\[CapitalTheta]\[Mu]\[Nu]*)


(* ::DisplayFormula:: *)
(*\:5176\:4ed6\:7684\:9879,\[CurlyEpsilon][ijk],u[jl],\:5747\:4e3a\:5b9e\:6570\:ff0c\:4f5c\:590d\:5171\:8f6d\:4e0d\:4f1a\:51fa\:8d1f\:53f7\:ff0c\:6240\:4ee5\:8fd9\:4e00\:9879\:53ef\:80fd\:6709\:4e2a\:989d\:5916\:8d1f\:53f7\:3002*)


lag["calC"]=lecs[6]*(
TensorContract[TensorContract[levi.
ltz[mat[3,1,1],"type"->"ltz","index"->1].
crt[2,2],{{1,4}}(*1,4\:6307\:6807\:7f29\:5e76*)].
ltz[mat[2,1,0],"type"->"\[CapitalTheta]","index"->{1,2}],
{{1,2}}(*1,2\:6307\:6807\:7f29\:5e76*)]-
TensorContract[TensorContract[levi.
ltz[mat[3,1,0],"type"->"ltz","index"->1].
crt[2,2],{{1,4}}(*1,4\:6307\:6807\:7f29\:5e76*)].
ltz[mat[2,1,1],"type"->"\[CapitalTheta]","index"->{1,2}],
{{1,2}}(*1,2\:6307\:6807\:7f29\:5e76*)]
);


(* ::Text:: *)
(*\:5341\:91cd\:6001\:4ecb\:5b50\:8026\:5408\:9879 calH,*)


(* ::DisplayFormula:: *)
(*\!\(\*OverscriptBox[\(T\[Mu]\), \(_\)]\)^ijk.\[Gamma]\[Mu]\[Nu]\[Alpha].\[Gamma]5 (u\[Alpha])^kl T\[Nu]^ijl*)


Module[{temp1,temp2},
CompoundExpression[
temp1=ltz[mat[3,1,1],"type"->"\[Gamma]","index"->{1}],
temp2=crt[2,3(*\:6d41\:7684\:6307\:6807*)].ltz[ltz[mat[3,1,0],"type"->"ltz","index"->{2}(*\:573a\:7684\:6307\:6807*)],"type"->"\[Gamma]","index"->{1,2,3,5}],
lag["calH"]=lecs[7]*(Flatten[temp1].Flatten[temp2])
];
]


lag["gpd"]=lag["oct"]+lag["dec"]+lag["mes"]+lag["D"]+lag["F"]+lag["calC"];


(* ::Chapter:: *)
(*formatting*)


(* ::Text:: *)
(*\:7ed9\:51fa\:8f93\:51fa\:5f62\:5f0f,\:7ed3\:5408\:5404\:79cd\:65e0\:5185\:7f6e\:610f\:4e49\:7684\:7ed3\:6784,\:5982 Superscript[]*)


(* ::Text:: *)
(*\:7136\:540e\:5bf9\:4e8e\:67d0\:4e00\:7c7b,\:5b83\:7684\:5206\:91cf\:6709\:4e24\:79cd\:7f16\:7801\:65b9\:5f0f,\:4e00\:79cd\:662f\:6570\:5b57,\:4e00\:79cd\:662f\:5b57\:7b26\:4e32\:3002*)


setmode[x:_Integer:1]:=CompoundExpression[
mes
];


(* ::Chapter:: *)
(*export*)


(*\:8f93\:51fa\:76ee\:5f55*)
exportdir=FileNameJoin[{gitlocalname,"/expression-results/"}]


echo["the output name list"]


(*\:5bfc\:51fa\:5230\:786c\:76d8\:65f6\:7684\:6587\:4ef6\:540d\:79f0*)
exportnamelist=<|
"ge_charge"->FileNameJoin[{
exportdir,"fig.baryons.ge.charge.L-"<>".pdf"
}]
|>


echo["output status"]


(*\:5185\:5b58\:4e2d\:4fdd\:5b58\:6570\:636e\:7684\:5173\:8054,\:5c06\:8981\:8f93\:51fa\:7684\:540d\:5b57*)
exportvars//Keys


Module[{namelists},
namelists=Keys[exportvars];

Do[
Export[exportnamelist[name],exportvars[name]];

(*\:5982\:679c\:8fdb\:884c\:5230\:6700\:540e\:4e00\:4e2a,\:5c31\:6253\:5370\:8f93\:51fa\:5b8c\:6210*)
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
(*\:5982\:679c\:5728\:524d\:7aef\:7b14\:8bb0\:672c\:4e2d\:6267\:884c,\:6e05\:9664\:6240\:6709\:8f93\:51fa\:7ed3\:679c,\:51cf\:5c0f\:6587\:4ef6\:4f53\:79ef*)


If[Not[booleincmd],FrontEndExecute[FrontEndToken["DeleteGeneratedCells"]]]
