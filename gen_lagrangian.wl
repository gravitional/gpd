(* ::Package:: *)

(* ::Title:: *)
(*gen_lagrangian.wl*)


(* ::Text:: *)
(*notebook \:5907\:5fd8\:5f55*)


(* ::Text:: *)
(*\:5c55\:5f00\:62c9\:683c\:6717\:65e5\:ff0c\:8bb0\:5f55\:63a8\:5bfc\:51fa\:7684\:89c4\:5219*)


(* ::Text:: *)
(*\:5c1d\:8bd5\:4f7f\:7528\:51fd\:6570\:63a5\:53e3: f[i,j,k]\:ff0c\:5c06\:4e0d\:592a\:786e\:5b9a\:7684\:5b9e\:73b0\:90fd\:5b9a\:4e49\:6210\:51fd\:6570*)


(* ::Text:: *)
(*\:5f00\:59cb\:6b64\:5305*)


BeginPackage["chptgpd`"]


(* ::Chapter:: *)
(*usage*)


(* ::Text:: *)
(*\:8fd9\:4e2a\:5305\:6253\:7b97\:63d0\:4f9b\:7684\:51fd\:6570\:ff0c\:8fd9\:91cc\:4ecb\:7ecd\:4e00\:4e0b\:7528\:6cd5\:3002*)


f::usage="aaaa"


(* ::Chapter:: *)
(*initial*)


(* ::Text:: *)
(*\:5f00\:59cb\:5305\:7684\:79c1\:6709\:4e0a\:4e0b\:6587\:ff0c\:4e5f\:5c31\:662f\:5185\:90e8\:8ba1\:7b97\:7ec6\:8282*)


Begin["`Private`"]


(* ::Section:: *)
(*path*)


(* ::Text:: *)
(*\:5b9a\:4e49\:4e00\:4e9b\:5e38\:7528\:7684\:51fd\:6570*)


(*\:5b9a\:4e49\:4e00\:4e2a\:786e\:4fdd\:5b57\:7b26\:4e32\:7684\:51fd\:6570*)
forcestr[x_,form_:InputForm]:=If[StringQ[x],x,ToString[x,form]]


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


(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c\:ff0c\:5c31\:5237\:65b0\:7b14\:8bb0\:672c\:7684\:540d\:5b57\:ff0c\:5e76\:8c03\:51fa EditBar *)
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
SetOptions[EvaluationNotebook[],WindowToolbars->{"EditBar"}]
]
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
(*encode input*)


(* ::Text:: *)
(*\:5bf9 Lagrangian \:4e2d\:7684\:5bf9\:8c61\:8fdb\:884c\:7f16\:7801\:ff0c\:5f53\:524d\:4e0a\:4e0b\:6587\:4e3a `lag`*)


Begin["`lag`"]


(* ::Subsection:: *)
(*constants*)


(* ::Text:: *)
(*\:5e38\:6570\:77e9\:9635\:ff0c\:6bd4\:5982 Gellman \:77e9\:9635*)


gellman[]


(* ::Text:: *)
(*Levi - Civita \:5f20\:91cf *)


levi=LeviCivitaTensor[3];


(* ::Text:: *)
(*\:5bf9\:6613 \:548c \:53cd\:5bf9\:6613*)


comutate[]
antitate[]


(* ::Text:: *)
(*\:7c92\:5b50\:8d28\:91cf\:ff0c*)
(*\:4ecb\:5b50::{meson, 0}*)
(*\:516b\:91cd\:6001::{octet, 1}*)
(*\:5341\:91cd\:6001::{decuplet, 2}*)


mass["mes"]
mass["oct"]
mass["dec"]


(* ::Text:: *)
(*\:4f4e\:80fd\:8026\:5408\:5e38\:6570, low energy constants,lecs*)


(* ::DisplayFormula:: *)
(*decay constant :: f*)


(* ::DisplayFormula:: *)
(*veilbein D,veilbein F,*)


(* ::DisplayFormula:: *)
(*mix constant:: \[ScriptCapitalC]*)


(* ::DisplayFormula:: *)
(*decuplet costant::\[ScriptCapitalH]*)


lecs["f"]
lecs["D"]
lecs["F"]
lecs["calC"]
lecs["calH"]


(* ::Subsection:: *)
(*particles lorentz*)


(* ::Text:: *)
(*\:7c92\:5b50\:77e9\:9635\:ff0c\:5f20\:91cf\:ff0cparticle,*)


(* ::Text:: *)
(*\:7b2c\:4e00\:4e2a\:53c2\:6570\:53d6\:5b57\:7b26\:4e32\:ff0c"par"\:4ee3\:8868\:7c92\:5b50\:ff0c"ant"\:4ee3\:8868\:53cd\:7c92\:5b50*)


(* ::Text:: *)
(*\:7b2c\:4e8c\:4e2a\:53c2\:6570\:53d6\:5b57\:7b26\:4e32\:ff0c\:542b\:4e49\:5982\:4e0b:*)
(*\:4ecb\:5b50::meson, "mes"*)
(*\:516b\:91cd\:6001::octet,"oct" *)
(*\:5341\:91cd\:6001::decuplet, "dec"*)


(* ::Text:: *)
(*\:53ef\:4ee5\:8c03\:7528\:7684\:5355\:4e2a\:7c92\:5b50\:573a \:548c \:6536\:96c6\:5355\:4e2a\:573a\:505a\:6210\:7684\:77e9\:9635*)


(* ::Text:: *)
(*\:4ecb\:5b50\:77e9\:9635*)


parti["part","mes",{i,j}]
matr["part","mes",{i,j}]


(* ::Text:: *)
(*\:4ecb\:5b50 u \:77e9\:9635 and dagger*)


matr["par","u",{i,j,k}]
matr["dag","u",{i,j,k}]


(* ::Text:: *)
(*\:4ecb\:5b50 U \:77e9\:9635 and dagger*)


matr["par","U",{i,j,k}]
matr["dag","U",{i,j,k}]


(* ::Text:: *)
(*\:516b\:91cd\:6001\:77e9\:9635*)


parti["par","oct",{i,j}]
matr["par","oct",{i,j}]


(* ::Text:: *)
(*\:5341\:91cd\:6001\:77e9\:9635*)


parti["par","dec",{i,j,k}]
matr["par","dec",{i,j,k}]


(* ::Text:: *)
(*\:5404\:79cd\:6d41\:ff0c\:77e2\:91cf\:6d41\:ff0c\:8f74\:77e2\:6d41\:ff0c\:6807\:91cf\:6d41*)


curr["axial",{l,j}]
curr["vector",{i,j}]


(* ::Text:: *)
(*\:6d1b\:4f26\:5179\:5bf9\:8c61,\:901a\:8fc7\:4e0a\:503c ^:= \:5b9a\:4e49\:5b83\:548c\:5bf9\:8c61\:7684\:4f5c\:7528*)


(* ::DisplayFormula:: *)
(*\:6bd4\:5982 \[Gamma]\[Mu]\[Nu],\[Gamma]\[Mu]\[Nu]\[Alpha],\[CapitalTheta]\[Mu]\[Nu]*)


(* ::Text:: *)
(*\:8d1f\:8d23\:7ed9\:5355\:4e2a\:5bf9\:8c61\:52a0\:4e0a\:6307\:6807*)


lore["gamma",{"\[Mu]","\[Nu]"}]
lore["gamma",{"\[Mu]","5"}]
lore["gamma",{"\[Mu]","\[Nu]","\[Alpha]"}]
lore["\[CapitalTheta]",{"\[Mu]","\[Nu]"}]


lore[parti["ant","dec",{i,j,k}],{"\[Mu]"}]
lore[parti["par","dec",{i,j,k}],{"\[Nu]"}]


lore[curr["axial",{l,j}],{"\[Mu]"}]
lore[curr["vector",{i,j}],{"\[Mu]"}]


(* ::Text:: *)
(*\:534f\:53d8\:5bfc\:6570\:ff0c\:901a\:8fc7\:4e0a\:503c ^:= \:5b9a\:4e49\:5b83\:548c\:7c92\:5b50\:ff0c\:4e5f\:5c31\:662f particle \:7684\:4f5c\:7528*)


covde["oct",{"\[Alpha]"}]


(* ::Section:: *)
(*Lagrangian*)


(* ::Text:: *)
(*\:624b\:5f81 Lagrangian \:7684\:5404\:9879*)


(* ::Text:: *)
(*\:4ecb\:5b50\:9879*)


lag["mes"]=f^2/4*Tr[covde["mes",{"\[Mu]"}].matr["par","U"].ConjugateTranspose(covde["mes",{"\[Mu]"}].matr["dag","U"])]


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
lore[matr["par","oct",{i,j,l}],{"\[Nu]"}]
,{i,1,decnumber,1}
,{j,1,decnumber,1}
,{k,1,decnumber,1}
,{l,1,decnumber,1}
]
]


(* ::Text:: *)
(*\:7ed3\:675f\:5f53\:524d\:4e0a\:4e0b\:6587 *)


End[(*`lag`*)] 


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
(*\:7ed3\:675f\:79c1\:6709\:4e0a\:4e0b\:6587*)


End[]


(* ::Text:: *)
(*\:7ed3\:675f\:6b64\:5305*)


EndPackage[] 
