(* ::Package:: *)

(* ::Title:: *)
(*gen.lagrangian.uds.wl*)


(* ::Text:: *)
(*\:5c55\:5f00\:62c9\:683c\:6717\:65e5\:ff0c\:5c06\:5473\:9053\:77e9\:9635\:4ee3\:5165\:6210 u,d,s \:7535\:8377\:77e9\:9635\:3002\:8bb0\:5f55\:63a8\:5bfc\:51fa\:7684\:9876\:70b9\:7cfb\:6570\:ff0c\:4ee5\:53ca\:5b88\:6052\:6d41.*)
(*\:6240\:6709\:5473\:7a7a\:95f4\:7684\:77e9\:9635\:6309\:7167 Gellman \:77e9\:9635\:5206\:89e3\:ff0c\:4e5f\:5c31\:662f SU(3) \:751f\:6210\:5143Ta \:76842\:500d.*)
(*\:5c1d\:8bd5\:4f7f\:7528\:51fd\:6570\:63a5\:53e3: f[i,j,k]\:ff0c\:5c06\:4e0d\:592a\:786e\:5b9a\:7684\:5b9e\:73b0\:90fd\:5b9a\:4e49\:6210\:51fd\:6570.*)
(*\:4f7f\:7528\:7684\:8bb0\:53f7\:53c2\:8003\:ff1ahttps://arxiv.org/pdf/1806.07551.pdf*)


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


Get["gen.format.wl"];(*\:8bfb\:5165\:5b9a\:4e49\:4ee5\:53ca\:6392\:7248\:76f8\:5173\:7684\:6587\:4ef6*)


(* ::Chapter:: *)
(*implement*)


(* ::Section:: *)
(*constants*)


(*SU(3)\:7fa4\:57fa\:672c\:8868\:793a\:7684\:751f\:6210\:5143\:ff0c=1/2 Gellman*)
dim=3;(*\:4e3a\:5bf9\:79f0\:7fa4SU(3)\:9009\:53d6\:7684\:8868\:793a\:7684\:7ef4\:5ea6*)


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


(*Levi - Civita \:5f20\:91cf *)
levi=LeviCivitaTensor[3];


(*\:5bf9\:4e8e\:67d0\:79cd\:8fd0\:7b97f\:ff0c\:5b9a\:4e49\:5bf9\:6613\:548c\:53cd\:5bf9\:6613 *)
cmt[f_,x_,y_]:=f[x,y]-f[y,x](*\:8fdb\:884c\:5bf9\:6613\:8fd0\:7b97*)
acmt[f_,x_,y_]:=f[x,y]+f[y,x](*\:8fdb\:884c\:53cd\:5bf9\:6613\:8fd0\:7b97*)


(* ::Section:: *)
(*fields*)


(* \:4ecb\:5b50\:77e9\:9635 *)
mat::usage="mat[\[Phi]],\:5176\:4e2d\:5404\:7c92\:5b50\:6536\:96c6\:5230\:7684\:77e9\:9635tag\:662f
meson::
\[Phi],u,U,\[Phi]\[ConjugateTranspose],u\[ConjugateTranspose],U\[ConjugateTranspose],M\[Phi]
octet::
B,Bbar,MB
decuplet::
T,Tbar,MT";


(*\:4ecb\:5b50\:77e9\:9635 \[Phi],\[Phi]\[Dagger] \:7684\:8d28\:91cf\:9879*)
mat["m\[Phi]"]={
{fd[1,{2,8},2],fd[1,1,2],fd[1,4,2]},
{fd[1,3,2],fd[1,{2,8},2],fd[1,6,2]},
{fd[1,5,2],fd[1,7,2],fd[1,8,2]}
};


mat["\[Phi]"]={
{1/Sqrt[2] fd[1,2,0]+1/Sqrt[6] fd[1,8,0],fd[1,1,0],fd[1,4,0]},
{fd[1,3,0],-1/Sqrt[2] fd[1,2,0]+1/Sqrt[6] fd[1,8,0],fd[1,6,0]},
{fd[1,5,0],fd[1,7,0],-2/Sqrt[6] fd[1,8,0]}
};
mat["u"]=IdentityMatrix[dim]+(I/(Sqrt[2]lecs["f"]))*mat["\[Phi]"]+(I/(Sqrt[2]lecs["f"]))^2/2!*mat["\[Phi]"] . mat["\[Phi]"];
mat["U"]=IdentityMatrix[dim]+((I*Sqrt[2])/lecs["f"])*mat["\[Phi]"]+((I*Sqrt[2])/lecs["f"])^2/2!*mat["\[Phi]"] . mat["\[Phi]"];
(*\:4ecb\:5b50\:573a\:77e9\:9635\:7684\:5384\:7c73\:5171\:8f6d*)
mat["\[Phi]\[Dagger]"]=mat["\[Phi]"];
mat["u\[Dagger]"]=IdentityMatrix[dim]+(-I/(Sqrt[2]lecs["f"]))*mat["\[Phi]"]+(-I/(Sqrt[2]lecs["f"]))^2/2!*mat["\[Phi]"] . mat["\[Phi]"];
mat["U\[Dagger]"]=IdentityMatrix[dim]+(-(I*Sqrt[2])/lecs["f"])*mat["\[Phi]"]+(-(I*Sqrt[2])/lecs["f"])^2/2!*mat["\[Phi]"] . mat["\[Phi]"];


(* ::Text:: *)
(*\:516b\:91cd\:6001\:77e9\:9635*)


(* ::Text:: *)
(*\:8d28\:91cf\:9879*)


mat["MB"]={
{fd[2,{4,8},2],fd[2,3,2],fd[2,1,2]},
{fd[2,5,2],fd[2,{4,8},2],fd[2,2,2]},
{fd[2,7,2],fd[2,6,2],fd[2,8,2]}
};


mat["B"]={
{1/Sqrt[2] fd[2,4,0]+1/Sqrt[6] fd[2,8,0],fd[2,3,0],fd[2,1,0]},
{fd[2,5,0],-1/Sqrt[2] fd[2,4,0]+1/Sqrt[6] fd[2,8,0],fd[2,2,0]},
{fd[2,7,0],fd[2,6,0],-2/Sqrt[6] fd[2,8,0]}
};
mat["Bbar"]={
{1/Sqrt[2] fd[2,4,1]+1/Sqrt[6] fd[2,8,1],fd[2,5,1],fd[2,7,1]},
{fd[2,3,1],-1/Sqrt[2] fd[2,4,1]+1/Sqrt[6] fd[2,8,1],fd[2,6,1]},
{fd[2,1,1],fd[2,2,1],-2/Sqrt[6] fd[2,8,1]}
};


(* ::Text:: *)
(*\:5341\:91cd\:6001\:77e9\:9635 \:8d28\:91cf\:9879*)


mat["MT"]=SymmetrizedArray[
{
{1,1,1}->fd[3,1,2],{1,1,2}->fd[3,2,2],{1,2,2}->fd[3,3,2],{2,2,2}->fd[3,4,2],
{1,1,3}->fd[3,5,2],{1,2,3}->fd[3,6,2],{2,2,3}->fd[3,7,2],
{1,3,3}->fd[3,8,2],{2,3,3}->fd[3,9,2],
{3,3,3}->fd[3,10,2]
}, {3,3,3}, 
Symmetric[All]
];


mat["T"]=SymmetrizedArray[
{
{1,1,1}->fd[3,1,0],{1,1,2}->1/Sqrt[3]*fd[3,2,0],{1,2,2}->1/Sqrt[3]*fd[3,3,0],{2,2,2}->fd[3,4,0],
{1,1,3}->1/Sqrt[3] fd[3,5,0],{1,2,3}->1/Sqrt[6] fd[3,6,0],{2,2,3}->1/Sqrt[3] fd[3,7,0],
{1,3,3}->1/Sqrt[3] fd[3,8,0],{2,3,3}->1/Sqrt[3] fd[3,9,0],
{3,3,3}->fd[3,10,0]
}, {3,3,3}, 
Symmetric[All]
];
mat["Tbar"]=SymmetrizedArray[
{
{1,1,1}->fd[3,1,1],{1,1,2}->1/Sqrt[3]*fd[3,2,1],{1,2,2}->1/Sqrt[3]*fd[3,3,1],{2,2,2}->fd[3,4,1],
{1,1,3}->1/Sqrt[3] fd[3,5,1],{1,2,3}->1/Sqrt[6] fd[3,6,1],{2,2,3}->1/Sqrt[3]*fd[3,7,1],
{1,3,3}->1/Sqrt[3] fd[3,8,1],{2,3,3}->1/Sqrt[3] fd[3,9,1],
{3,3,3}->fd[3,10,1]
}, {3,3,3}, 
Symmetric[All]
];


(* ::Section:: *)
(*Partial derivative & Gamma matrices*)


(*\:7ed9\:7b26\:53f7\:52a0\:4e0a\:9884\:5b9a\:4e49\:7684\:6d1b\:4f26\:5179\:6307\:6807,\:6d1b\:4f26\:5179\:504f\:5bfc\:6570,\:4f3d\:9a6c\:77e9\:9635,\:7b49\:7b49\:3002*)
Options[ltz]={"tp"->"ltz","idx"->"","dir"->"r"};(*\:9009\:9879*)
Attributes[ltz]={Listable};(*\:6dfb\:52a0\:81ea\:52a8\:7ebf\:6027\:4e8e\:5217\:8868\:5c5e\:6027*)
ltz[sym:_,OptionsPattern[ltz]]:=Module[{idxes,type},
type=OptionValue["tp"];
idxes=OptionValue["idx"];
Which[
MemberQ[{"\[PartialD]"},type],
pde1[ltzScript1[type,idxes],sym],

MemberQ[{"\[Gamma]","\[CapitalTheta]","\[Sigma]"},type],
gma1[ltzScript1[type,idxes],sym],

MemberQ[{"ltz"},type],ltzScript1[sym,idxes]
]
]
(*

ltz[B,"tp"\[Rule]"ltz","idx"\[Rule]"\[Mu]\[Nu]"]
ltz[B,"tp"\[Rule]"\[PartialD]","idx"\[Rule]"\[Mu]\[Nu]"]
ltz[B,"tp"\[Rule]"\[Gamma]","idx"\[Rule]"\[Mu]\[Nu]"]
ltz[B,"tp"\[Rule]"\[CapitalTheta]","idx"\[Rule]"\[Mu]\[Nu]"]
*)


ltz2[{sym1:_,idx1:_},{sym2:_,idx2:_}]:=ltz[ltz[sym1,"tp"->"ltz","idx"->idx1],"tp"->sym2,"idx"->idx2]
(*\:7ed9\:5bf9\:8c61\:540c\:65f6\:52a0\:4e0a\:6d1b\:4f26\:5179\:6307\:6807\:548c\[Gamma]\:77e9\:9635\:ff0c\:5982 gmltz[{mat["Bb"],"\[Mu]"},{"\[Gamma]","\[Nu]\[Alpha]\[Beta]"}]*)
ltz3[{sym1:_,idx1:_},{sym2:_,idx2:_},{sym3:_,idx3:_}]:=ltz[
ltz[ltz[sym1,"tp"->"ltz","idx"->idx1],"tp"->sym2,"idx"->idx2],
"tp"->sym3,"idx"->idx3]


constantHeadList={Integer,Real,Rational,Complex,lecs};(*\:8ba1\:7b97\:4e2d\:4f7f\:7528\:7684\:5404\:79cd\:5e38\:6570\:7684\:5934\:90e8\:7684\:5217\:8868*)
constantQ[sym:_]:=NumericQ[sym]||MatchQ[Head[sym],Alternatives@@constantHeadList]
constantQ::usage="constantQ[sym:_],\:68c0\:6d4b sym \:662f\:5426\:4e3a\:5e38\:6570";


(*\:4e0a\:9762\:4f7f\:7528\:4e86 pde,gma \:51fd\:6570*)
pde1::usage="pde[\[PartialD]\[Mu],B] \:504f\:5bfc\:6570\:51fd\:6570,\:5bf9\:5e38\:6570\:6c42\:5bfc\:7b49\:4e8e\:96f6,\[PartialD].(A.B)=A*\[PartialD]B+B*\[PartialD]A,\[PartialD].(A+B)=\[PartialD]A+\[PartialD]B";
pde1[pd_,Plus[x_,y__]]:=Plus[pde1[pd,x],pde1[pd,Plus[y]]]
pde1[pd_,Times[x_,y__]]:=Times[x,pde1[pd,Times[y]]]+Times[Times[y],pde1[pd,x]]
pde1[pd_,Power[sym_,m_]]:=If[constantQ[sym],0,m*Power[sym,m-1]*pde1[pd,sym]](* \:5bf9 power \:7684\:4f5c\:7528*)
pde1[pd_,sym_]:=If[constantQ[sym],0,pde[pd,sym]]
(*\:4e0a\:9762\:4f7f\:7528\:4e86 pde,gma \:51fd\:6570*)
gma1::usage="gma1[\[Gamma]\[Mu],B,direction],\:4e58\:4e0a\[Gamma]\:51fd\:6570,\:9ed8\:8ba4\:662f\:4ece\:5de6\:8fb9\:4e58\:4e0a\:7684,\[Gamma].(A+B)=\[Gamma].A+\[Gamma].B, \[Gamma]C*B=C*\[Gamma].B";
gma1[gm_,Plus[x_,y__]]:=Plus[gma1[gm,x],gma1[gm,Plus[y]]]
gma1[gm_,Times[x_,y__]]:=Times[x,gma1[gm,Times[y]]]
gma1[gm_,Power[sym_,m_.]]:=If[constantQ[sym],Power[sym,m]*gm,gma[gm,Power[sym,m]]](*\:8003\:8651power\:7684\:60c5\:51b5\:66f4\:52a0\:5168\:9762\:ff0c\:5e76\:4f7f\:7528power\:9ed8\:8ba4\:503c1*)
(*\:4e0a\:9762\:4f7f\:7528\:4e86 ltzScript1 \:51fd\:6570*)
ltzScript1::usage="\:7ed9\:5bf9\:8c61\:52a0\:4e0a\:6d1b\:4f26\:5179\:6307\:6807\:ff0c\:6ee1\:8db3\:52a0\:6cd5\:ff0c\:4e58\:6cd5\:5206\:914d\:ff0c\:65e0\:5e8f\:6027";
ltzScript1[Plus[x:_,y:__],idx:_]:=Plus[ltzScript1[x,idx],ltzScript1[Plus[y],idx]]
ltzScript1[Times[x:_,y:__],idx:_]:=Times[ltzScript1[x,idx],ltzScript1[Times[y],idx]]
ltzScript1[sym:_,idxes:__]:=If[constantQ[sym],sym,ltzScript[sym,idxes]]


(* ::Section::Closed:: *)
(*Gather & Sort*)


(* ::Text:: *)
(*\:628a\:573a\:5bf9\:8c61\:548c\:8026\:5408\:7cfb\:6570\:5206\:5f00*)


fdForms:=_fd|_gma|_pde|_ltzScript(*\:573a\:5bf9\:8c61\:7684\:6a21\:5f0f*)


(*funcPure[expr,patt] \:5c06\:8868\:8fbe\:5f0f\:4e2d\:7684 patt \:5bf9\:8c61\:53d6\:51fa\:6765\:ff0c\:5230\:6700\:6df1\:7684\:5c42\:6b21,\:9ed8\:8ba4\:7ed9\:51fa\:4e00\:4e2a\:5217\:8868*)
funcPure::error="\:6240\:7ed9\:7684\:53c2\:6570`1`\:4e2d\:6ca1\:6709\:5bf9\:8c61 `2`";
funcPure[expr_,patt_]:=Module[{temp},
temp=enList[Cases[enList[expr],patt,Infinity]];
If[temp==={},
Null[0],
temp
]
]
(*\:5bf9\:53c2\:6570\:8fdb\:884c\:8fc7\:6ee4\:ff0c\:53ea\:8f93\:51fa\:5176\:4e2d\:7684fd[__]\:5bf9\:8c61*)


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


(*\:5bf9\:7ed3\:679c\:8fdb\:884c\:7b5b\:9009\:5e76\:5c55\:793a\:ff0clagAssoc[lag]\:7684\:5355\:5143\:7ed3\:6784\:4e3a\:ff1a
<|lagint["fds"\[Rule]{gma[ltzScript["\[Sigma]","\[Mu]\[Nu]"],fd[2,1,0]], fd[2,1,1]},"lec"\[Rule] {lecs["c1"],lecs["c2"],lecs["c3"]}]\[Rule]
lagcoe[((I/4)*e*((s+u)*lecs["c1"]+(-s+ u)*lecs["c2"]+(d+s+u)*lecs["c3"])*(fv[q,"\[Nu]"]*vfd["\[Mu]"]-fv[q,"\[Mu]"]*vfd["\[Nu]"]))/MN]|>
++++++++++++++++++++++++++++++++++++++
laglkp1[lag:_,fds:_List],\:5bf9\:4e8e\:67d0\:4e2alag\:6c42\:548c\:5f0f\:ff0c\:628a\:6ee1\:8db3\:6761\:4ef6\:7684\:9879\:6311\:9009\:51fa\:6765.\:7ed3\:679c\:6309\:7167\:8fd9\:4e00\:9879\:4e2d\:8026\:5408\:7cfb\:6570\:7684\:53d6\:503c\:6392\:5217\:ff0c\:5982
laglkp1[lag["mag8"],
{fd[2,1,0],fd[2,2,0]}
]
*)
laglkp1[lag_,crit_,fds_List]:=lagshow[
SortBy[(*\:5bf9\:7ed3\:679c\:8fdb\:884c\:6392\:5e8f*)
KeySelect[(*\:5bf9\:7ed3\:679c\:8fdb\:884c\:7b5b\:9009,\:6309\:7167 key \:8fdb\:884c\:7b5b\:9009*)
lagAssoc[lag],(*\:628a\:62c9\:6c0f\:91cf\:6574\:7406\:6210\:5173\:8054\:7684\:5f62\:5f0f*)
crit[
funcPure[(*funcPure\:7684\:4f5c\:7528\:662f\:63d0\:53d6\:51fa\:573a\:5bf9\:8c61*)
(Association@@#1)["fds"](*\:628a key \:53d8\:6210\:5173\:8054\:ff0c\:63d0\:53d6fiels \:90e8\:5206\:7684\:4fe1\:606f*)
,fd[__]],
fds
]&
],
{
Last[FactorTermsList@@#1]&
}
]
]


(*\:5c06\:524d\:9762\:6574\:7406\:8fc7\:7684\:7ed3\:679c\:6392\:7248\:5c55\:793a*)
lagshow[lag:_]:=Multicolumn[
Normal[lag],(*\:4f20\:5165\:4e00\:4e2a\:5173\:8054*)
4(*\:628a\:7ed3\:679c\:6392\:6210n\:5217*),
Spacings->{1,1},
Background->{Automatic,{{LightOrange,White}}},Frame->All,FrameStyle->White
,Appearance->"Horizontal"
]


(* ::Chapter:: *)
(*Chiral Strong Interaction*)


(*\:624b\:5f81 Lagrangian \:7684\:5404\:9879,\:6309\:7167\:8026\:5408\:5e38\:6570\:8fdb\:884c\:6392\:5217\:ff1a*)
lag=<||>;
ordRule={lecs["f"]^x_/;(x<-2)->0};


(*\:624b\:5f81\:5f3a\:5b50\:6d41*)
crt["\[CapitalGamma]",\[Mu]_,"hd"]:=(1/2Expand[mat["u"] . ltz[mat["u\[Dagger]"],"tp"->"\[PartialD]","idx"->\[Mu]]+
mat["u\[Dagger]"] . ltz[mat["u"],"tp"->"\[PartialD]","idx"->\[Mu]]]/.ordRule)(*\:624b\:5f81\:77e2\:91cf\:6d41,\:5f3a\:76f8\:4e92\:4f5c\:7528\:90e8\:5206*)
crt["u",\[Mu]_,"hd"]:=((I/2)Expand[mat["u"] . ltz[mat["u\[Dagger]"],"tp"->"\[PartialD]","idx"->\[Mu]]
-mat["u\[Dagger]"] . ltz[mat["u"],"tp"->"\[PartialD]","idx"->\[Mu]]]/.ordRule)(*\:624b\:5f81\:8f74\:77e2\:6d41*)


(*\:516b\:91cd\:6001\:ff1aTr[Overscript[B, _](I*\[Gamma].\[PartialD]-MB)B]*)
lag["oct"]=Expand[Tr[
mat["Bbar"] . (I*ltz3[{mat["B"],"\[Mu]"},{"\[PartialD]","\[Mu]"},{"\[Gamma]","\[Mu]"}]-mat["MB"]*mat["B"])
]
];
(*\:6311\:9009\:51fa\:5176\:4e2d\:67d0\:4e9b\:9879*)
laglkp1[
lag["oct"],
ContainsAny,Flatten[Array[fd,{1,8,1},{{2,2},{1,8},{0,0}}]](*\:516b\:91cd\:6001\:91cd\:5b50\:573a\:ff0c\:6b63\:573a*)
]


(*\:516b\:91cd\:6001\:ff1aTr[Overscript[B, _].(I*\[Gamma].[\[CapitalGamma]\[Mu],B])],\:534f\:53d8\:5bfc\:6570\:9879\:ff0c\:4ecb\:5b50\:8026\:5408*)
lag["oct,\[CapitalGamma]\[Mu]"]=Expand[Tr[
mat["Bbar"] . (I*cmt[Dot,crt["\[CapitalGamma]","\[Mu]","hd"],ltz[mat["B"],"tp"->"\[Gamma]","idx"->"\[Mu]"]])
]
];
(*\:6311\:9009\:51fa\:5176\:4e2d\:67d0\:4e9b\:9879*)
laglkp1[
lag["oct,\[CapitalGamma]\[Mu]"],
ContainsAll,{fd[2,1,0],fd[2,1,1]}
]


(*\:5341\:91cd\:6001:\!\(
\*SubsuperscriptBox[\(T\), \(\[Mu]\), \(ijk\)]\ \((I*
\*SuperscriptBox[\(\[Gamma]\), \(\[Mu]\[Nu]\[Alpha]\)] . 
\*SubscriptBox[\(\[PartialD]\), \(\[Alpha]\)]\(-MT\)*
\*SuperscriptBox[\(\[Gamma]\), \(\[Mu]\[Nu]\)])\)
\*SubsuperscriptBox[\(T\), \(\[Nu]\), \(ijk\)]\),\:8fd9\:91cc\:662f\:52a8\:80fd\:9879*)
lag["dec"]=Expand[(
Flatten[ltz[mat["Tbar"],"tp"->"ltz","idx"->"\[Mu]"]] .
Flatten[I*ltz3[{mat["T"],"\[Nu]"},{"\[PartialD]","\[Alpha]"},{"\[Gamma]","\[Mu]\[Nu]\[Alpha]"}]-mat["MT"]*ltz2[{mat["T"],"\[Nu]"},{"\[Gamma]","\[Mu]\[Nu]"}]]
)];
(*\:6311\:9009\:51fa\:5176\:4e2d\:67d0\:4e9b\:9879*)
laglkp1[
lag["dec"],
ContainsAny,Flatten[Array[fd,{1,10,1},{{3,3},{1,10},{0,0}}]](*\:5341\:91cd\:6001\:91cd\:5b50\:573a\:ff0c\:6b63\:573a*)
]


(* ::Text:: *)
(*meson*)


(* ::DisplayFormula:: *)
(*f^2/4* Tr[Subscript[D, \[Mu]]U . (Subscript[D, \[Mu]]U)\[ConjugateTranspose]]=f^2/4* Tr[Subscript[D, \[Mu]]U . (Subscript[D, \[Mu]]U\[ConjugateTranspose])]*)


(*\:4ecb\:5b50\:52a8\:80fd\:9879*)
lag["mes"]=Expand[
lecs["f"]^2/4*Tr[
ltz[mat["U"],"tp"->"\[PartialD]","idx"->"\[Mu]"] .
ltz[mat["U\[Dagger]"],"tp"->"\[PartialD]","idx"->"\[Mu]"]
]
];
(*\:6311\:9009\:51fa\:5176\:4e2d\:67d0\:4e9b\:9879*)
laglkp1[
lag["mes"],
ContainsAny,{fd[1,1,0],fd[1,1,1]}(*\:4ecb\:5b50\:573a\:ff0c\:6b63\:573a*)
]


(*D*Tr[Overscript[B, _]\[Gamma]\[Mu]\[Gamma]5{u\[Mu],B}]+F*Tr[Overscript[B, _]\[Gamma]\[Mu]\[Gamma]5[u\[Mu],B]]*)
lag["DF"]=Expand[
(lecs["D"])*Tr[mat["Bbar"] . acmt[Dot,crt["u","\[Mu]","hd"],ltz[mat["B"],"tp"->"\[Gamma]","idx"->"\[Mu]5"]]]+
(lecs["F"])*Tr[mat["Bbar"] . cmt[Dot,crt["u","\[Mu]","hd"],ltz[mat["B"],"tp"->"\[Gamma]","idx"->"\[Mu]5"]]]
];
(*\:6311\:9009\:51fa\:5176\:4e2d\:67d0\:4e9b\:9879*)
laglkp1[
lag["DF"],
ContainsAny,{(*fd[2,1,0],*)fd[2,8,1]}(*\:53ea\:67e5\:770b\:6838\:5b50*)
]


(* ::Text:: *)
(*calC, \:8fd9\:4e00\:9879\:6bd4\:8f83\:590d\:6742\:ff0c\:6c42\:548c\:5728\:4e09\:4e2a\:5c42\:6b21\:4e0a\:8fdb\:884c\:ff0cSU(3)\:5473\:9053\:7a7a\:95f4\:ff0cltz \:6d1b\:4f26\:5179\:6307\:6807\:7a7a\:95f4\:ff0cSpinor \:65cb\:91cf\:5206\:91cf\:3002\:4ed6\:4eec\:662f\:76f8\:4e92\:72ec\:7acb\:7684\:3002*)


(* ::DisplayFormula:: *)
(*\[CapitalTheta]\[Mu]\[Nu]=g\[Mu]\[Nu]-(z+1/2)\[Gamma]\[Mu]*\[Gamma]\[Nu],\:5728\:8ba1\:7b97\:4e2d\:9009\:53d6,z=1/2,*)


(* ::DisplayFormula:: *)
(*\[CapitalTheta]\[Mu]\[Nu]=g\[Mu]\[Nu]-\[Gamma]\[Mu].\[Gamma]\[Nu]=1/2 \[Gamma]\[Mu].\[Gamma]\[Nu]+1/2 \[Gamma]\[Nu].\[Gamma]\[Mu]-\[Gamma]\[Mu].\[Gamma]\[Nu]=1/2 \[Gamma]\[Nu].\[Gamma]\[Mu]-\[Gamma]\[Mu].\[Gamma]\[Nu]=I*\[Sigma]\[Mu]\[Nu],*)


(* ::DisplayFormula:: *)
(*\[Gamma]0.(I*\[Sigma]\[Mu]\[Nu])\[ConjugateTranspose].\[Gamma]0=-I*\[Sigma]\[Mu]\[Nu],*)


(*\[ScriptCapitalC]Tr[Bbar.Contract[Subscript[u, \[Mu]].\[CapitalTheta]^\[Mu]\[Nu].Subscript[T, \[Nu]].(-\[CurlyEpsilon]),{{1,4}}]]+\[ScriptCapitalC]Tr[Contract[\[CurlyEpsilon].Subscript[Tbar, \[Mu]].Subscript[u, \[Nu]],{{1,4}}].\[CapitalTheta]^\[Mu]\[Nu].B]*)
lag["C"]=Expand[(lecs["C"])*(
Tr[
mat["Bbar"] .
TensorContract[crt["u","\[Mu]","hd"] . ltz2[{mat["T"],"\[Nu]"},{"\[CapitalTheta]","\[Mu]\[Nu]"}] . (-levi)(*\:8fd9\:91cc\:4f1a\:6709\:4e00\:4e2a\:8d1f\:53f7*),{{1,4}}(*1,4\:6307\:6807\:7f29\:5e76*)]
]+
Tr[
TensorContract[
levi .
ltz[mat["Tbar"],"tp"->"ltz","idx"->"\[Mu]"] . crt["u","\[Nu]","hd"],{{1,4}}(*1,4\:6307\:6807\:7f29\:5e76*)] .
ltz[mat["B"],"tp"->"\[CapitalTheta]","idx"->"\[Mu]\[Nu]"]
(*\:6700\:540e\:6c42 Trace*)]
)];
(*\:6311\:9009\:51fa\:5176\:4e2d\:67d0\:4e9b\:9879*)
laglkp1[
lag["C"],
ContainsAny,{(*fd[2,1,0],*)fd[2,1,1]}
]


(*\:5341\:91cd\:6001\:4ecb\:5b50\:8026\:5408\:9879 calH, T\[Nu]^ijl u\[Alpha]^kl Overscript[T\[Mu], _]^ijk.\[Gamma]\[Mu]\[Nu]\[Alpha].\[Gamma]5*)
lag["H"]=Expand[(-lecs["H"])*(
Flatten[ltz[mat["Tbar"],"tp"->"ltz","idx"->"\[Mu]"]] .
Flatten[crt["u","\[Alpha]","hd"] . ltz2[{mat["T"],"\[Nu]"(*\:573a\:7684\:6307\:6807*)},{"\[Gamma]","\[Mu]\[Nu]\[Alpha]5"}]]
)
];
(*\:6311\:9009\:51fa\:5176\:4e2d\:67d0\:4e9b\:9879*)
laglkp1[
lag["H"],
ContainsAny,{(*fd[3,2,0],*)fd[3,1,1]}(*\:53ea\:67e5\:770b\[CapitalDelta]++*)
]


(*\:5f20\:91cf\:8026\:5408\:9879, 1/2 \[ImaginaryI] b9 Tr[Bbar Subscript[A, \[Mu]]].\[Sigma]^\[Mu]\[Nu].Tr[Subscript[A, \[Nu]].B]+1/2 \[ImaginaryI] b10 Tr[Bbar[Subscript[A, \[Mu]],Subscript[A, \[Nu]]].\[Sigma]^\[Mu]\[Nu].B]+1/2 \[ImaginaryI] b11 Tr[Bbar {Subscript[A, \[Mu]],Subscript[A, \[Nu]]}.\[Sigma]^\[Mu]\[Nu].B] *)
(*4 \:6765\:81ea\:4e8e\:4e0d\:540c\:7684\:7ea6\:5b9a*)
lag["bbb"]=Expand[4*I/2 (
lecs["b9"]*(Tr[mat["Bbar"] . crt["u","\[Mu]","hd"]] * Tr[crt["u","\[Nu]","hd"] . ltz[mat["B"],"tp"->"\[Sigma]","idx"->"\[Mu]\[Nu]"]])
+lecs["b10"]*Tr[mat["Bbar"] . acmt[Dot,2*(crt["u","\[Mu]","hd"] . crt["u","\[Nu]","hd"]),ltz[mat["B"],"tp"->"\[Sigma]","idx"->"\[Mu]\[Nu]"]]]
+lecs["b11"]*Tr[mat["Bbar"] . cmt[Dot,2*(crt["u","\[Mu]","hd"] . crt["u","\[Nu]","hd"]),ltz[mat["B"],"tp"->"\[Sigma]","idx"->"\[Mu]\[Nu]"]]]
)
];
(*\:6311\:9009\:51fa\:5176\:4e2d\:67d0\:4e9b\:9879*)
laglkp1[
lag["bbb"],
ContainsAll,{fd[2,1,0],fd[2,1,1](*\:53ea\:67e5\:770b\:6838\:5b50*)}
]


(* ::Chapter:: *)
(*\:5b88\:6052\:6d41*)


(*\:4e09\:79cd\:5938\:514b\:7684\:7535\:8377\:77e9\:9635*)
Qqk:=DiagonalMatrix[{qk[1],qk[2],qk[3]}]
Format[qk[1],StandardForm]:=Subscript[Q,u]
Format[qk[2],StandardForm]:=Subscript[Q,d]
Format[qk[3],StandardForm]:=Subscript[Q,s]
(*\:4e09\:79cd\:5938\:514b\:7684\:7535\:8377\:77e9\:9635*)
Qqk:=DiagonalMatrix[{ch["u"],ch["d"],ch["s"]}]
(*+++++++++++++++++++++++++++ \:8ddf\:5916\:90e8\:7535\:78c1\:573a\:8026\:5408\:7684\:7535\:78c1\:6d41 +++++++++++++++++++++++++++*)
(*F\[Mu]\[Nu],\:7535\:78c1\:573a\:5f20\:91cf*)
vfd2[\[Mu]_,\[Nu]_]:=ltz[vfd[\[Nu]],"tp"->"\[PartialD]","idx"->\[Mu]]-ltz[vfd[\[Mu]],"tp"->"\[PartialD]","idx"->\[Nu]]
(*F\[Mu]\[Nu],\:7535\:78c1\:573a\:5f20\:91cf\:ff0c\:5728\:52a8\:91cf\:7a7a\:95f4\:7684\:5f62\:5f0f\:ff0c\:5149\:5b50\:52a8\:91cf\:4e3a-Iq\[Nu]*)
vfd2["-Iq\[Nu]",\[Mu]_,\[Nu]_]:=(-I*fv[q,\[Mu]]*vfd[\[Nu]]+I*fv[q,\[Nu]]*vfd[\[Mu]])
(*F\[Mu]\[Nu],\:7535\:78c1\:573a\:5f20\:91cf\:ff0c\:5728\:4f4d\:7f6e\:7a7a\:95f4\:7684\:5f62\:5f0f*)
vfd2["F\[Mu]\[Nu]",\[Mu]_,\[Nu]_]:=F\[Mu]\[Nu]["\[Mu]","\[Nu]"]
crt[{"F\[Mu]\[Nu]",0},\[Mu]_,\[Nu]_]:=vfd2["F\[Mu]\[Nu]",\[Mu],\[Nu]]*(Qqk)(*\:5c55\:5f00\:5230\:7b2c0\:9636*)
crt[{"F\[Mu]\[Nu]",2},\[Mu]_,\[Nu]_]:=vfd2["F\[Mu]\[Nu]",\[Mu],\[Nu]]*(1/2*Expand[mat["u\[Dagger]"] . Qqk . mat["u"]+
mat["u"] . Qqk . mat["u\[Dagger]"]]/.ordRule)(*\:5c55\:5f00\:5230\:7b2c2\:9636*)


(* ::Text:: *)
(*\:624b\:5f81\:62c9\:6c0f\:91cf\:7684\:5b88\:6052\:6d41*)


(* ::DisplayFormula:: *)
(*\:516b\:91cd\:6001:1/2 Tr[Bbar\[Gamma]^\[Mu] . [u . \[Lambda]^a . u\[ConjugateTranspose]+u\[ConjugateTranspose] . \[Lambda]^a . u,B]]*)


ntct["oct"]=Expand[lecs["1"]/2*Tr[(*lecs["1"]\:5c31\:662f1\:ff0c\:52a8\:80fd\:9879\:7684\:7279\:5f81*)
mat["Bbar"] . cmt[Dot,mat["u"] . Qqk . mat["u\[Dagger]"]+
mat["u\[Dagger]"] . Qqk . mat["u"],ltz[mat["B"],"tp"->"\[Gamma]","idx"->"\[Mu]"]]
]+lecs["1"]*Tr[Qqk]*Tr[mat["Bbar"] . ltz[mat["B"],"tp"->"\[Gamma]","idx"->"\[Mu]"]]
];
(*\:6311\:9009\:51fa\:5176\:4e2d\:67d0\:4e9b\:9879*)
laglkp1[
ntct["oct"]/.ordRule,
ContainsAll,{fd[2,8,0],fd[2,8,1](*\:53ea\:67e5\:770b\:6838\:5b50*)}
]


(*(D/2)Tr[Bbar.\[Gamma]\[Mu] .\[Gamma]5 . {u . \[Lambda]^a . u\[Dagger]-u\[Dagger]. \[Lambda]^a . u,B}]+
F/2 Tr[Bbar.\[Gamma]\[Mu] . \[Gamma]5 . [u . \[Lambda]^a . u\[Dagger]-u\[Dagger]. \[Lambda]^a . u,B]]*)
ntct["DF"]=Expand[
lecs["D"]/2*Tr[mat["Bbar"] . acmt[Dot,mat["u"] . Qqk . mat["u\[Dagger]"]-
mat["u\[Dagger]"] . Qqk . mat["u"],ltz[mat["B"],"tp"->"\[Gamma]","idx"->"\[Mu]5"]]]+
lecs["F"]/2*Tr[mat["Bbar"] . cmt[Dot,mat["u"] . Qqk . mat["u\[Dagger]"]-
mat["u\[Dagger]"] . Qqk . mat["u"],ltz[mat["B"],"tp"->"\[Gamma]","idx"->"\[Mu]5"]]]
];
(*\:6311\:9009\:51fa\:5176\:4e2d\:67d0\:4e9b\:9879*)
laglkp1[
ntct["DF"]/.ordRule,
ContainsAny,{fd[2,8,0](*,fd[2,1,1]*)}(*\:53ea\:67e5\:770b\:6838\:5b50*)
]


(*1/2 Subscript[Tbar, \[Nu]].\[Gamma]^\[Nu]\[Alpha]\[Mu].(u.\[Lambda]^a.u\[Dagger]+u\[Dagger].\[Lambda]^a.u.Subscript[T, \[Alpha]])*)
ntct["dec"]=Module[{inner},
inner=(mat["u"] . Qqk . mat["u\[Dagger]"]+mat["u\[Dagger]"] . Qqk . mat["u"]) . ltz2[{mat["T"],"\[Alpha]"},{"\[Gamma]","\[Nu]\[Alpha]\[Mu]"}];
Expand[lecs["1"]/2 (
Total[
ltz[mat["Tbar"],"tp"->"ltz","idx"->"\[Nu]"]*(inner+TensorTranspose[inner,{2,3,1}]+TensorTranspose[inner,{3,1,2}])
,3]
)
]
];
(*\:6311\:9009\:51fa\:5176\:4e2d\:67d0\:4e9b\:9879*)
laglkp1[
ntct["dec"]/.ordRule,
ContainsAny,{(*fd[3,1,0],*)fd[3,10,1]}
]


(* Decuplet
\[ScriptCapitalC].Bbar^km .\[CurlyEpsilon]^ijk .\[CapitalTheta]^\[Mu]\[Nu]. Subsuperscript[T, \[Mu], ilm] .Subsuperscript[u, \[Nu], lj].+\[ScriptCapitalC].B^mk.\[CurlyEpsilon]^ijk. \[CapitalTheta]^\[Mu]\[Nu]. Subsuperscript[u, \[Nu], lj]. Subsuperscript[Overscript[T, _], \[Mu], ilm]
\[ScriptCapitalC].\[CurlyEpsilon]^jki. Subsuperscript[Overscript[T, _], \[Mu], iml] .Subsuperscript[u, \[Nu], lj] .\[CapitalTheta]^\[Mu]\[Nu] .B^mk.+\[ScriptCapitalC] .Bbar^km. Subsuperscript[u, \[Mu], jl].\[CapitalTheta]^\[Mu]\[Nu].Subsuperscript[T, \[Nu], lmi].(-\[CurlyEpsilon]^ikj)
\[ScriptCapitalC].Tr[Bbar.Contract[Subscript[u, \[Mu]].\[CapitalTheta]^\[Mu]\[Nu].Subscript[T, \[Nu]].(-\[CurlyEpsilon]),{{1,4}}]]+\[ScriptCapitalC].Tr[Contract[\[CurlyEpsilon].Subscript[Tbar, \[Mu]].Subscript[u, \[Nu]],{{1,4}}].\[CapitalTheta]^\[Mu]\[Nu].B]
\:5bf9\:5e94\:7684\:5b88\:6052\:6d41\:4e3a:
1/2 \[ScriptCapitalC] Tr[Bbar.Contract[(u.\[Lambda]^a.u\[Dagger]-u\[Dagger].\[Lambda]^a.u).\[CapitalTheta]^\[Mu]\[Nu].Subscript[T, \[Nu]].(-\[CurlyEpsilon]),{{1,4}}]]+1/2 \[ScriptCapitalC] Tr[Contract[\[CurlyEpsilon].Subscript[Tbar, \[Nu]].(u.\[Lambda]^a.u\[Dagger]-u\[Dagger].\[Lambda]^a.u),{{1,4}}].\[CapitalTheta]^\[Nu]\[Mu].B]
*)
ntct["C"]=Expand[lecs["C"]/2*(
Tr[TensorContract[levi . ltz[mat["Tbar"],"tp"->"ltz","idx"->"\[Nu]"] .
(mat["u"] . Qqk . mat["u\[Dagger]"]-mat["u\[Dagger]"] . Qqk . mat["u"]),{{1,4}}] . ltz[mat["B"],"tp"->"\[CapitalTheta]","idx"->"\[Nu]\[Mu]"]]+
Tr[mat["Bbar"] . TensorContract[(mat["u"] . Qqk . mat["u\[Dagger]"]-mat["u\[Dagger]"] . Qqk . mat["u"]) .
ltz2[{mat["T"],"\[Nu]"},{"\[CapitalTheta]","\[Mu]\[Nu]"}] . (-levi),{{1,4}}]
])];
(*\:6311\:9009\:51fa\:5176\:4e2d\:67d0\:4e9b\:9879*)
laglkp1[
ntct["C"]/.ordRule,
ContainsAny,{(*fd[2,1,0],*)fd[2,8,1]}
]


(* ::Text:: *)
(*meson*)


(* ::DisplayFormula:: *)
(*f^2/4*I* Tr[\[PartialD]^\[Mu]U . (U\[ConjugateTranspose] . \[Lambda]^a-\[Lambda]^a . U\[ConjugateTranspose])+(U . \[Lambda]^a-\[Lambda]^a . U) . \[PartialD]^\[Mu]U\[ConjugateTranspose]]*)


(* ::DisplayFormula:: *)
(*f^2/4*I* Tr[\[PartialD]^\[Mu]U . [U\[ConjugateTranspose],\[Lambda]^a]+[U,\[Lambda]^a] . \[PartialD]^\[Mu]U\[ConjugateTranspose]]*)


ntct["mes"]=Expand[
(lecs["1"])*(I*lecs["f"]^2)/4*Tr[
ltz[mat["U"],"tp"->"\[PartialD]","idx"->"\[Mu]"] . cmt[Dot,mat["U\[Dagger]"],Qqk]+
cmt[Dot,mat["U"],Qqk] . ltz[mat["U\[Dagger]"],"tp"->"\[PartialD]","idx"->"\[Mu]"]
]
];
(*\:6311\:9009\:51fa\:5176\:4e2d\:67d0\:4e9b\:9879*)
laglkp1[
ntct["mes"],
ContainsAny,Flatten[Array[fd,{1,8,1},{{1,1},{1,8},{0,0}}]](*\:516b\:91cd\:6001\:4ecb\:5b50\:ff0c\:6b63\:573a*)
]


(* ::Section:: *)
(*\:53cd\:5e38\:78c1\:77e9*)


(* ::DisplayFormula:: *)
(*fv[q,"\[Mu]"]*gma[ltzScript["\[Sigma]","\[Mu]\[Nu]"],x_]*vfd["\[Nu]"]^:=-fv[q,"\[Nu]"]*gma[ltzScript["\[Sigma]","\[Mu]\[Nu]"],x]*vfd["\[Mu]"]*)
(*(*F\[Mu]\[Nu]*\[Sigma]\[Mu]\[Nu]\:6536\:7f29\:8fd9\:4e00\:9879\:662f\:5bf9\:79f0\:7684*)*)


(* ::Text:: *)
(*\:516b\:91cd\:6001\:53cd\:5e38\:78c1\:77e9*)


(* ::DisplayFormula:: *)
(*e/(4mN) (c1*Tr[Bbar . \[Sigma]^\[Mu]\[Nu] . {\!\( *)
(*\*SubsuperscriptBox[\(F\), \(\[Mu]\[Nu]\), \(+\)], B\)}]+c2*Tr[Bbar . \[Sigma]^\[Mu]\[Nu] . [\!\( *)
(*\*SubsuperscriptBox[\(F\), \(\[Mu]\[Nu]\), \(+\)], B\)]]+c3*Tr[Bbar . \[Sigma]^\[Mu]\[Nu] . B]*Tr[Q])*)


lag["mag8"]=Expand[e/(4MN)*(*\:5f52\:4e00\:5316\:5230\:6838\:5b50\:8d28\:91cf*)
(
lecs["c1"]*Tr[mat["Bbar"] . acmt[Dot,crt[{"F\[Mu]\[Nu]",2},"\[Mu]","\[Nu]"],ltz[mat["B"],"tp"->"\[Sigma]","idx"->"\[Mu]\[Nu]"]]]+
lecs["c2"]*Tr[mat["Bbar"] . cmt[Dot,crt[{"F\[Mu]\[Nu]",2},"\[Mu]","\[Nu]"],ltz[mat["B"],"tp"->"\[Sigma]","idx"->"\[Mu]\[Nu]"]]]+
(lecs["c3"])*Tr[crt[{"F\[Mu]\[Nu]",2},"\[Mu]","\[Nu]"]]*Tr[mat["Bbar"] . ltz[mat["B"],"tp"->"\[Sigma]","idx"->"\[Mu]\[Nu]"]]
)
];
(*\:6311\:9009\:51fa\:5176\:4e2d\:67d0\:4e9b\:9879*)
laglkp1[
lag["mag8"]/.{lecs["c3"]->lecs["c2"]-lecs["c1"]},
ContainsAll,{fd[2,8,1],fd[2,4,0]}
]


(* ::Text:: *)
(*\:5341\:91cd\:6001\:53cd\:5e38\:78c1\:77e9*)


(* ::DisplayFormula:: *)
(*(3Subscript[c, T]e)/(4mT) \!\(\*SubsuperscriptBox[*)
(*OverscriptBox[\(T\), \(_\)], \(\[Mu]\), \(abc\)]\) \[Sigma]^\[Mu]\[Nu] Subscript[F, \[Mu]\[Nu]] Q^cd \!\(\*SubsuperscriptBox[\(T\), \(\[Nu]\), \(dba\)]\)=-(F2T/(4mT)) \!\(\*SubsuperscriptBox[*)
(*OverscriptBox[\(T\), \(_\)], \(\[Mu]\), \(abc\)]\) \[Sigma]^\[Mu]\[Nu] \!\(\*SubsuperscriptBox[\(F\), \(\[Mu]\[Nu]\), \(a\)]\) \[Lambda]^(a,cd) \!\(\*SubsuperscriptBox[\(T\), \(\[Nu]\), \(dba\)]\)*)


lag["mag10"]=Expand[TensorContract[
(3*e*lecs["cT"])/(4MT)*(
vfd2["F\[Mu]\[Nu]","\[Mu]","\[Nu]"]*(ltz[mat["Tbar"],"tp"->"ltz","idx"->"\[Alpha]"]) . Qqk . ltz2[{mat["T"],"\[Alpha]"},{"\[Sigma]","\[Mu]\[Nu]"}]
)
,{{1,3},{2,4}}]
];
(*\:6311\:9009\:51fa\:5176\:4e2d\:67d0\:4e9b\:9879*)
laglkp1[
lag["mag10"],
ContainsAny,Flatten[Array[fd,{1,10,1},{{3,3},{1,10},{0,0}}]](*\:5341\:91cd\:6001\:91cd\:5b50\:573a\:ff0c\:6b63\:573a*)
]


(*\:516b\:91cd\:6001--\:5341\:91cd\:6001\:8f6c\:79fb\:78c1\:77e9:
(-I*e*Sqrt[3]c4)/mN .F_\[Mu]\[Nu]*(\[Epsilon]_ijk.Q_il.Bbar_jm . \[Gamma]\[Mu].\[Gamma]5.Subsuperscript[T, klm, \[Nu]]-\[Epsilon]_ijk.Q_li.Tbar_klm^\[Nu].\[Gamma]\[Mu].\[Gamma]^5.B_mj)
=(-I*e*Sqrt[3]c4)/mN*(Bbar^jm (\!\(
\*SubsuperscriptBox[\(F\), \(\[Mu]\[Nu]\), \(a\)]
\*SuperscriptBox[\(\[Lambda]\), \(a\)]\))^il . \[Gamma]^\[Mu] . \[Gamma]^5 . T^(\[Nu],lmk) .
 (-\[Epsilon]^kji)-\[Epsilon]^ijk Tbar^(\[Nu],kml) (\!\(
\*SubsuperscriptBox[\(F\), \(\[Mu]\[Nu]\), \(a\)]
\*SuperscriptBox[\(\[Lambda]\), \(a\)]\))^li . \[Gamma]^\[Mu] . \[Gamma]^5 . B^mj)
=(I*e*Sqrt[3]c4)/mN (Tr[Bbar . \[Gamma]^\[Mu] . \[Gamma]^5 . (Subscript[F, \[Mu]\[Nu]].T^\[Nu].\[CurlyEpsilon])^{1,4}]+Tr[(\[CurlyEpsilon].Overscript[T, _]^\[Nu].Subscript[F, \[Mu]\[Nu]])^{1,4} . \[Gamma]^\[Mu] . \[Gamma]^5 . B])
*)
lag["mag8,10"]=Expand[
(I*e*Sqrt[3]*lecs["c4"])/MN*
vfd2["F\[Mu]\[Nu]","\[Mu]","\[Nu]"]*(
Tr[
mat["Bbar"] . TensorContract[Qqk . ltz2[{mat["T"],"\[Nu]"},{"\[Gamma]","\[Mu]5"}] . levi,{{1,4}}]
]+
Tr[
TensorContract[(levi) . ltz[mat["Tbar"],"tp"->"ltz","idx"->"\[Nu]"] . Qqk,{{1,4}}] .
ltz[mat["B"],"tp"->"\[Gamma]","idx"->"\[Mu]5"]
]
)
];
(*\:6311\:9009\:51fa\:5176\:4e2d\:67d0\:4e9b\:9879*)
laglkp1[
lag["mag8,10"],
ContainsAny,Flatten[Array[fd,{1,8,2},{{2,2},{1,8},{0,1}}]](*\:516b\:91cd\:6001\:91cd\:5b50\:573a\:ff0c\:6b63\:53cd\:573a*)
]
