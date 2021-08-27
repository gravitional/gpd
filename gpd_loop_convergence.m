(* ::Package:: *)

(* ::Title:: *)
(*gpd_loop_convergence.nb*)


(* ::Chapter:: *)
(*initial*)


git`remote`name="gpd";(*\:7ed9\:51fa\:8fdc\:7a0bgit\:4ed3\:5e93\:7684\:540d\:5b57*)
cmdQ=Not[$Notebooks];(*\:811a\:672c\:7684\:8fd0\:884c\:6a21\:5f0f\:5224\:65ad\:ff0cTrue\:4ee3\:8868\:547d\:4ee4\:884c\:ff0cFalse\:4ee3\:8868\:524d\:7aef*)
filename=If[Not[cmdQ],NotebookFileName[],$InputFileName](*\:7ed9\:51fa\:7b14\:8bb0\:672c\:7684\:7edd\:5bf9\:8def\:5f84*)


forcestr[x__]:=StringJoin[ToString[#1]&/@Flatten[{x}]](*\:5b9a\:4e49\:4e00\:4e2a\:786e\:4fdd\:5b57\:7b26\:4e32\:7684\:51fd\:6570*)
If[cmdQ,
echo[x__]:=Print["----------------------------","\n\033[1;44m\033[1;37m",forcestr[x],"\033[0;0m\n","----------------------------"],(*\:5b9a\:4e49\:7ec8\:7aef\:7684\:6253\:5370\:51fd\:6570*)
echo[x__]:=Print[x](*\:5b9a\:4e49\:7b14\:8bb0\:672c\:7684\:6253\:5370\:51fd\:6570*)
]


(* ::Text:: *)
(*\:8ba1\:7b97\:73af\:5883\:53c2\:91cf\:ff0c\:6bd4\:5982\:8def\:5f84*)


(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c\:ff0c\:5c31\:5237\:65b0\:7b14\:8bb0\:672c\:7684\:540d\:5b57*)
Once[If[
(* if $ScriptCommandLine==={}, the environment is frontend*)
Not[cmdQ],
(*if execute in the frontend mode, refresh the title name*)
CompoundExpression[
cell`title=(Cells[][[1]]),(*\:5355\:5143\:5bf9\:8c61,\:7b2c\:4e00\:4e2a\:5355\:5143*)
NotebookWrite[cell`title,Cell[FileNameSplit[filename][[-1]],"Title"]](*\:5237\:65b0\:7b2c\:4e00\:4e2a\:5355\:5143\:7684\:540d\:5b57*)
]
]];
If[cmdQ,echo["Ready to execute this script"]];(*\:5982\:679c\:5728\:547d\:4ee4\:884c\:6267\:884c\:ff0c\:5c31\:6253\:5370\:63d0\:793a\:4fe1\:606f*)
git`local`name=FileNameJoin[Append[TakeWhile[FileNameSplit[ExpandFileName[filename]],UnsameQ[#1,git`remote`name]&],git`remote`name]];
echo["the git`local`name is: ",git`local`name];


(* ::Text:: *)
(*\:5982\:679c\:5728\:547d\:4ee4\:884c\:6267\:884c\:ff0c\:5c31\:76f4\:63a5\:63a5\:53d7\:547d\:4ee4\:884c\:53d8\:91cf*)


(* ::Text:: *)
(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c\:ff0c\:5c31\:6a21\:62df\:547d\:4ee4\:884c\:8f93\:5165\:ff0c\:8ba1\:7b97\:6a21\:62df\:53d8\:91cf\:7684\:5b57\:7b26\:4e32\:5f62\:5f0f\:3002\:56e0\:4e3a\:547d\:4ee4\:884c\:4f20\:5165\:7684\:4e00\:822c\:662f\:5b57\:7b26\:4e32\:ff0c\:8fd9\:6837\:53ef\:4ee5\:7edf\:4e00\:5f62\:5f0f\:3002*)


If[cmdQ,
(*\:5982\:679c\:5728\:547d\:4ee4\:884c\:6267\:884c*)
input`cml=$ScriptCommandLine,
(*++++++++++++++++++++++++++++++++++++++++*)
(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c*)
input`cml={filename,(*\:547d\:4ee4\:884c\:7b2c\:4e00\:4e2a\:53c2\:6570\:6c38\:8fdc\:662f\:6b64\:811a\:672c\:7684\:7edd\:5bf9\:8def\:5f84*)
(*\:5176\:4ed6\:53c2\:6570*)
input`simulation=forcestr[
(*\:5728\:8fd9\:91cc\:7ed9\:51fa\:5176\:4ed6\:53c2\:6570\:5728mathematica\:8bed\:6cd5\:4e0b\:7684\:5f62\:5f0f\:ff0c\:5916\:9762\:7684ToString\:4f1a\:81ea\:52a8\:8f6c\:6362*)
(*\:5c3d\:91cf\:591a\:4f7f\:7528Association\:7ed3\:6784*)
",a"
(*\:5728\:8fd9\:91cc\:7ed9\:51fa\:5176\:4ed6\:53c2\:6570\:5728mathematica\:8bed\:6cd5\:4e0b\:7684\:5f62\:5f0f\:ff0c\:5916\:9762\:7684ToString\:4f1a\:81ea\:52a8\:8f6c\:6362*)
]
}
];
echo["the input parameter is:\n"];enString[inputCml]


(* ::Section:: *)
(*packages*)


Once[<<"FeynCalc`"]


fcestd[x_]:=x//FCE//StandardForm;
fcistd[x_]:=x//FCI//StandardForm;


(* ::Section:: *)
(*kinematics*)


(*\:516b\:91cd\:6001\:90e8\:5206\:547d\:540d*)
Clear[
MN,(*\:521d\:672b\:6001\:6838\:5b50\:8d28\:91cf*)
f,(*\:4ecb\:5b50\:8870\:53d8\:5e38\:6570*)
\[CapitalDelta],(*MB-MN*)
M1,(*\:4e2d\:95f4\:6001\:91cd\:5b501\:7684\:8d28\:91cf*)
M2,(*\:4e2d\:95f4\:6001\:91cd\:5b502\:7684\:8d28\:91cf*)
Ml(*MN+MB\:ff0c\:8d28\:91cf\:4e4b\:548c*)
];


(* ::Section:: *)
(*\:4f20\:64ad\:5b50\:7ed3\:6784*)


ats[x:__Association|__Rule]:=Sequence@@Normal[Merge[{x},Total]];
ats::usage="\:8f85\:52a9\:51fd\:6570\:ff0c\:5408\:5e76\:591a\:4e2a\:5173\:8054,\:5c06\:91cd\:590d\:7684\:952e\:503c\:6c42\:548c\:ff0c\:5c06\:89c4\:5219\:5217\:8868\:7684sequence";


fad::usage="\:8bb0\:5f55\:4f20\:64ad\:5b50\:7ed3\:6784\:ff0c\:4f5c\:4e3a\:8f93\:5165\:63a5\:53e3";
fad[x:__Rule]:=fad`a[ats[x]]; (*\:5c06\:91cd\:590d\:7684\:4f20\:64ad\:5b50\:5e42\:6b21\:7d2f\:52a0\:8d77\:6765*)


SetAttributes[{fad`a,fad`b},Orderless];(*\:8ba1\:7b97\:89e3\:6790\:7ed3\:679c\:7684\:51fd\:6570\:4e0d\:4f9d\:8d56\:4e8e\:53c2\:6570\:6b21\:5e8f*)


(* ::Text:: *)
(*\:6574\:7406\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:ff0c\:5408\:5e76\:76f8\:540c\:7684\:9879*)


fad`a::usage="\:8bb0\:5f55\:4f20\:64ad\:5b50\:7ed3\:6784\:ff0c\:81ea\:52a8\:5408\:5e76\:4e24\:4e2a\:76f8\:4e58\:7684\:4f20\:64ad\:5b50\:51fd\:6570";
fad`a[x:__Rule]*fad`a[y:__Rule]^:=fad`a[ats[x,y]];
Power[fad`a[x:__Rule],n_]^:=fad`a[ats[n*Association[List[x]]]] (*\:5c06\:4f20\:64ad\:5b50\:7684\:5e42\:6b21\:653e\:5230\:53c2\:6570\:4e2d*)


(*\:4f20\:64ad\:5b50\:7684\:6392\:7248\:ff0c\:5b9a\:4e49\:4e00\:4e2a\:6613\:8bfb\:7684\:6837\:5f0f*)
Format[fad`a[x__Rule],TraditionalForm]:=\[DoubleStruckCapitalD][Values[{x}]];


(* ::Chapter:: *)
(*functions*)


(* ::Section:: *)
(*\:5149\:9525\:5206\:91cf\:8868\:793a*)


(*\:6700\:540e\:4e24\:4e2a\:5206\:91cf\:4f5c\:6b27\:51e0\:91cc\:5f97\:5185\:79ef*)
SetAttributes[edot,Orderless];(*\:8ba1\:7b97\:89e3\:6790\:7ed3\:679c\:7684\:51fd\:6570\:4e0d\:4f9d\:8d56\:4e8e\:53c2\:6570\:6b21\:5e8f*)
edot[p_+q__,l_]:=edot[p,l]+edot[q,l](*\:7ebf\:6027\:5f8b*)
edot[(a_Integer|a_Rational|a_Reals)*p_,q_]:=a*edot[p,q](*\:5206\:914d\:5f8b*)
(*\:5149\:9525\:6807\:91cf\:79ef:light-cone scalar product\:ff0c1\:8868\:793a+\:5206\:91cf\:ff0c2\:8868\:793a-\:5206\:91cf\:ff0c3\:8868\:793a\[Perpendicular]\:5206\:91cf*)
lgsp[p_,q_]:=Expand[p[[1]]*q[[2]]+p[[2]]*q[[1]]-edot[p[[3]],q[[3]]]]


(*lgc \:7ed9\:51fa\:52a8\:91cf\:7684\:5149\:9525\:5206\:91cf*)
SetAttributes[lg,Orderless];(*\:8ba1\:7b97\:89e3\:6790\:7ed3\:679c\:7684\:51fd\:6570\:4e0d\:4f9d\:8d56\:4e8e\:53c2\:6570\:6b21\:5e8f*)
lg[p_+q__]:=lg[p]+lg[q](*\:7ebf\:6027\:5f8b*)
lg[(a_Integer|a_Rational|a_Reals)*p_]:=a*lg[p](*\:5206\:914d\:5f8b*)
(*gpd \:4e2d\:4f7f\:7528\:7684\:5149\:9525\:52a8\:91cf\:53c2\:6570\:5316*)
lg[k]:={y*p1p,km,kt}(*\:4ecb\:5b50\:5708\:52a8\:91cf*)
lg[p1]:={p1p,(1-\[Zeta])p1p,pt}(*GDP \:4e2d\:7684\:6838\:5b50\:521d\:672b\:6001\:52a8\:91cf\:ff0c\:4ee5\:53ca\:52a8\:91cf\:4e4b\:5dee*)
lg[p2]:={(1-\[Zeta])p1p,p1p,pt}
lg[q]:={-\[Zeta]*p1p,\[Zeta]*p1p,0}


(*\:4f20\:64ad\:5b50*)
ppg[{k_,\[CapitalLambda]_}]:=lgsp[lg[k],lg[k]]-\[CapitalLambda]^2;
(*++++++++++++++++++++++++++++++++++++++++++++*)
confs:={(*\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:914d\:7f6e*)
{k,\[CapitalLambda]},{k,m\[Phi]},(*1,2*)
{k-q,\[CapitalLambda]},{k-q,m\[Phi]},(*3,4*)
{p1-k,M1},{p2-k,M2},(*5,6*)
{k+q,\[CapitalLambda]},{k+q,m\[Phi]}(*7,8*)
}
ppgs:=ppg/@confs(*\:4f20\:64ad\:5b50\:5728\:5149\:9525\:5750\:6807\:4e0b\:7684\:5f62\:5f0f*)
(*++++++++++++++++++++++++++++++++++++++++++++*)
kmCoes:=Simplify[(Coefficient[#1,km,1]&/@ppgs)](*k^-\:5206\:91cf\:524d\:7684\:7cfb\:6570*)
ppgZs:=Simplify[Values[Solve[#1==0,{km}][[1,1]]&/@ppgs]](*\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:503c*)
fad`b[x__]:=Times@@KeyValueMap[Power,KeyMap[ppg,Association[x]]](*fad`a\[Rule]fad`b,\:628a\:4f20\:64ad\:5b50\:7684\:8868\:793a\:8f6c\:6362\:6210\:5149\:9525\:5206\:91cf\:8868\:793a\:ff0c\:4e5f\:5c31\:662f\:5177\:4f53\:5f62\:5f0f*)


(* ::Section:: *)
(*\:5206\:88c2\:51fd\:6570\:6295\:5f71*)


(* ::Text:: *)
(*\:8ba1\:7b97\:5206\:88c2\:51fd\:6570*)


(*\:8fd0\:52a8\:5b66\:5173\:7cfb\:ff0c\:6807\:91cffix*)
SP[p1,p1]=MN^2;SP[p2,p2]=MN^2;SP[p1,p2]=Q2/2+MN^2;
SP[p1,q]=Q2/2;SP[p2,q]=-Q2/2;


(*\:628a\:81ea\:7531\:6307\:6807\:7684\:52a8\:91cf\:90fd\:66ff\:6362\:6210plus\:5206\:91cf*)
rl`moment:=FCI[{
MT[\[Mu]_,\[Nu]_]->0,(*\:5ea6\:89c4++\:5206\:91cf\:4e3a\:96f6*)
FV[k,\[Mu]_]->lg[k][[1]],
FV[p1,\[Mu]_]->lg[p1][[1]],
FV[p2,\[Mu]_]->lg[p2][[1]],
FV[q,\[Mu]_]->lg[q][[1]]
}]
(*\:6295\:5f71\:7b97\:7b26,\:6784\:9020fa,fb,\:518d\:7ebf\:6027\:7ec4\:5408\:6210f1,f2*)
prj["fa",\[Nu]_]:=(GS[p1]+MN) . (GA[\[Nu]]) . (GS[p2]+MN)
prj["fb",\[Nu]_]:=(FV[p1+p2,\[Nu]]/(2MN)) . (GS[p1]+MN) . (GS[p2]+MN)
(*\:5bf9\:632f\:5e45\:4f5ctrace,\:6295\:5f71\:51fa\:7ed3\:6784\:51fd\:6570fa,fb*)
app`prj[x_]:=(DiracSimplify[DiracTrace[x]]/.rl`moment)
(*\:5c06\:6295\:5f71\:7b97\:5b50\:4f5c\:7528\:5230\:632f\:5e45\:4e0a,\:5e76\:5316\:7b80\:5230\:6807\:91cf\:8868\:8fbe\:5f0f*)
to`fafb[\[CapitalGamma]\[Mu]_,\[Nu]_]:={app`prj[\[CapitalGamma]\[Mu] . prj["fa",\[Nu]]],app`prj[\[CapitalGamma]\[Mu] . prj["fb",\[Nu]]]}(*\:5f97\:5230\:632f\:5e45\:7684\:4e24\:4e2a\:6295\:5f71fa,fb*)


(*\:5c06fa,fb\:4f5c\:7ebf\:6027\:7ec4\:5408,\:91cd\:65b0\:7ec4\:6210f1,f2*)
psum:=lg[p1][[1]]+lg[p2][[1]](*\:5e38\:89c1\:7684plus\:52a8\:91cf\:7ec4\:5408*)
pmin:=lg[p1][[1]]-lg[p2][[1]]
ptms:=lg[p1][[1]]*lg[p2][[1]]
combine[{A_,B_}]:={(-A*Q2*psum^2+4B*MN^2 pmin^2)/(8psum^2(MN^2 pmin^2-ptms*Q2)),(-A*MN^2*psum^2+4B*ptms)/(2psum^2(MN^2 pmin^2-ptms*Q2))}
to`f1f2[list_]:=First[Keys[list]]->combine[Values[list]](*fa,fb\:7ec4\:5408\:6210f1,f2*)


(* ::Text:: *)
(*\:67e5\:770b\:4f20\:64ad\:5b50\:7c7b\:578b*)


Times[x__,fad`c[y__]]^:=fad`c[y](*fad`c \:8fd9\:4e2a\:51fd\:6570\:7528\:6765\:6446\:8131\:7cfb\:6570,\:53ea\:5c55\:793a\:5404\:79cd\:4f20\:64ad\:5b50\:7ed3\:6784*)
ppg`show[x_]:=List@@(x/.fad`a->fad`c)(*\:66ff\:6362\:5b8c\:6574\:8868\:8fbe\:5f0f\:4e2d\:7684fad`a\:4e3afad`c*)
Format[fad`c[x__Rule],TraditionalForm]:=Times@@KeyValueMap[Power,KeyMap[\[DoubleStruckCapitalD],Association[x]]];(*\:4f20\:64ad\:5b50\:7684\:6392\:7248,\:5b9a\:4e49\:4e00\:4e2a\:6613\:8bfb\:7684\:6837\:5f0f*)


(* ::Chapter:: *)
(*split octet*)


SetOptions[Simplify,TimeConstraint->1];


(* ::Section:: *)
(*a*)


(* ::Text:: *)
(*\:4ecb\:5b50\:5f69\:8679\:56fe*)


prfactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;if="a";(*\:56fe\:7684\:7f16\:53f7*)M1=MB;M2=MB;(*\:4e2d\:95f4\:6001\:91cd\:5b50\:7684\:8d28\:91cf*)
splt[if,"cls"]=(prfactor*fad[
{k+q,\[CapitalLambda]}->-2,{k,\[CapitalLambda]}->-2,{k+q,m\[Phi]}->-1,{k,m\[Phi]}->-1,{p1-k,M1}->-1
]
)


if="a";
\[CapitalGamma][if,"spr",\[Mu]]:=GS[k+q] . GA5 . (FV[2k+q,\[Mu]]) . (GS[p1-k]+M1) . GA5 . GS[k]


if="a";
splt[if,"fafb","spr"]=to`fafb[\[CapitalGamma][if,"spr",\[Mu]],\[Nu]];(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf*)


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
rl`scalar:=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2,(*\:5bf9k.k\:4f5c\:66ff\:6362*)
SP[k,q]->1/2(fad[{k+q,\[CapitalLambda]}->1]-fad[{k,\[CapitalLambda]}->1]+Q2),
SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{p1-k,M1}->1]+\[CapitalLambda]^2-M1^2+MN^2),(*\:5bf9k.p1\:4f5c\:66ff\:6362*)
SP[k,p2]->1/2(fad[{k+q,\[CapitalLambda]}->1]+Q2-fad[{p1-k,M1}->1]+\[CapitalLambda]^2-M1^2+MN^2)
}]


if="a";
splt[if,"fafb"]=Collect[
Expand[(splt[if,"cls"])*(splt[if,"fafb","spr"]/.rl`scalar)],(*\:6700\:7ec8\:7ed3\:679c*)
fad`a[__](*\:6574\:7406\:4e00\:4e0b*)
,Simplify];


if="a";
splt[if, "fafb"][[1]]//ppg`show(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)


if="a";
splt[if,"fafb","lgc"]=splt[if,"fafb"]/.fad`a->fad`b;


if="a";
Monitor[
splt[if,"fafb","res"]=Table[
confs[[res]]->Residue[splt[if,"fafb","lgc"][[ff]],
{km,ppgZs[[res]]}
]
,{res,{1,2,5}}
,{ff,1,2,1}
]
,{res,ff}(*\:76d1\:89c6\:8fd0\:884c\:8fc7\:7a0b*)
];//AbsoluteTiming
splt[if,"fafb","res"]//Dimensions


if="a";
splt[if,"f1f2","res"]=to`f1f2/@splt[if,"fafb","res"];
splt[if,"f1f2","res"]//Dimensions


(* ::Text:: *)
(*\:5bf9 y, \[Zeta]\:5206\:7c7b\:8ba8\:8bba,\:5bf9\:7559\:6570\:6c42\:548c,\:7ed9\:51fa\:7ed3\:679c*)


(* ::DisplayFormula:: *)
(*splt[if,"f1f2","anl","0<y<\[Zeta]"]=(-2\[Pi]*I)(splt[if,"f1f2","res"][[1,All]]+splt[if,"f1f2","res"][[2,All]]);*)
(*splt[if,"f1f2","anl","\[Zeta]<y<1"]=(2\[Pi]*I)(splt[if,"f1f2","res"][[5,All]]);*)


(* ::Input:: *)
(*if="a";output`dir=FileNameJoin[{git`local`name,"/mfiles/"}]*)
(*Export[FileNameJoin[{output`dir,"residue_f1f2_"<>if<>".m"}],splt[if,"f1f2","res"]](*\:5bfc\:51fa\:5230\:786c\:76d8*)*)


(* ::Section:: *)
(*b*)


(* ::Text:: *)
(*\:91cd\:5b50\:5f69\:8679\:56fe*)


prfactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;if="b";(*\:56fe\:7684\:7f16\:53f7*)M1=MB;M2=MB;(*\:4e2d\:95f4\:6001\:91cd\:5b50\:7684\:8d28\:91cf*)
splt[if,"cls"]=(prfactor*fad[
{k,\[CapitalLambda]}->-4,{k,m\[Phi]}->-1,{p1-k,M1}->-1,{p2-k,M2}->-1
]
)


if="b";
\[CapitalGamma][if,"spr",\[Mu]]:=GS[k+q] . GA5 . (GS[p2-k]+M2) . GA[\[Mu]] . (GS[p1-k]+M1) . GS[k] . GA5


if="b";
splt[if,"fafb","spr"]=to`fafb[\[CapitalGamma][if,"spr",\[Mu]],\[Nu]];(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf*)


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00,\:5bf9\:4e8e\:4e0d\:540c\:7684\:56fe\:8981\:8c03\:6574*)
rl`scalar:=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2,(*\:5bf9k.k\:4f5c\:66ff\:6362*)
SP[k,q]->1/2(fad[{p1-k,M1}->1]-fad[{p2-k,M2}->1]),
SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{p1-k,M1}->1]+\[CapitalLambda]^2-M1^2+MN^2),(*\:5bf9k.p1\:4f5c\:66ff\:6362*)
SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{p2-k,M2}->1]+\[CapitalLambda]^2-M2^2+MN^2)
}]


if="b";
splt[if,"fafb"]=Collect[
Expand[(splt[if,"cls"])*(splt[if,"fafb","spr"]/.rl`scalar)],(*\:6700\:7ec8\:7ed3\:679c*)
fad`a[__](*\:6574\:7406\:4e00\:4e0b*)
,Simplify];


if="b";
splt[if, "fafb"][[1]]//ppg`show(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)


if="b";
splt[if,"fafb","lgc"]=Simplify[splt[if,"fafb"]/.fad`a->fad`b];(*\:7ed9\:51fa\:5149\:9525\:5750\:6807\:4e0b\:7684\:5f62\:5f0f*)


if="b";
Monitor[
splt[if,"fafb","res"]=Table[
confs[[res]]->Residue[splt[if,"fafb","lgc"][[ff]],
{km,ppgZs[[res]]}
]
,{res,{5,6}}
,{ff,1,2,1}
]
,{res,ff}(*\:76d1\:89c6\:8fd0\:884c\:8fc7\:7a0b*)
];//AbsoluteTiming
splt[if,"fafb","res"]//Dimensions


if="b";
splt[if,"f1f2","res"]=to`f1f2/@splt[if,"fafb","res"];
splt[if,"f1f2","res"]//Dimensions


(* ::Input:: *)
(*if="b";output`dir=FileNameJoin[{git`local`name,"/mfiles/"}]*)
(*Export[FileNameJoin[{output`dir,"residue_f1f2_"<>if<>".m"}],splt[if,"f1f2","res"]](*\:5bfc\:51fa\:5230\:786c\:76d8*)*)


(* ::Section:: *)
(*c*)


(* ::Text:: *)
(*\:91cd\:5b50\:78c1\:77e9\:5f69\:8679\:56fe*)


(* ::Section:: *)
(*d*)


(* ::Text:: *)
(*KR\:56fe\:53f3*)


prfactor=(-I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;if="d";(*\:56fe\:7684\:7f16\:53f7*)M1=MB;M2=MB;(*\:4e2d\:95f4\:6001\:91cd\:5b50\:7684\:8d28\:91cf*)
splt[if,"cls"]=(prfactor*fad[
{k+q,\[CapitalLambda]}->-2,{k,\[CapitalLambda]}->-2,{k,m\[Phi]}->-1,{p1-k,M1}->-1
]
)


if="d";
\[CapitalGamma][if,"spr",\[Mu]]:=GA[\[Mu]] . GA5 . (GS[p1-k]+M1) . GS[k] . GA5


if="d";
splt[if,"fafb","spr"]=to`fafb[\[CapitalGamma][if,"spr",\[Mu]],\[Nu]];(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf*)


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
rl`scalar:=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2,(*\:5bf9k.k\:4f5c\:66ff\:6362*)
SP[k,q]->1/2(fad[{k+q,\[CapitalLambda]}->1]-fad[{k,\[CapitalLambda]}->1]+Q2),
SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{p1-k,M1}->1]+\[CapitalLambda]^2-M1^2+MN^2),(*\:5bf9k.p1\:4f5c\:66ff\:6362*)
SP[k,p2]->1/2(fad[{k+q,\[CapitalLambda]}->1]+Q2-fad[{p1-k,M1}->1]+\[CapitalLambda]^2-M1^2+MN^2)
}]


if="d";
splt[if,"fafb"]=Collect[
Expand[(splt[if,"cls"])*(splt[if,"fafb","spr"]/.rl`scalar)],(*\:6700\:7ec8\:7ed3\:679c*)
fad`a[__],(*\:6574\:7406\:4e00\:4e0b*)
Simplify
];


if="d";
splt[if, "fafb"][[1]]//ppg`show(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)


if="d";
splt[if,"fafb","lgc"]=splt[if,"fafb"]/.fad`a->fad`b;


if="d";
Monitor[
splt[if,"fafb","res"]=Table[
confs[[res]]->Residue[splt[if,"fafb","lgc"][[ff]],
{km,ppgZs[[res]]}
]
,{res,{1,2,5}}
,{ff,1,2,1}
]
,{res,ff}(*\:76d1\:89c6\:8fd0\:884c\:8fc7\:7a0b*)
];//AbsoluteTiming
splt[if,"fafb","res"]//Dimensions


if="d";
splt[if,"f1f2","res"]=to`f1f2/@splt[if,"fafb","res"];
splt[if,"f1f2","res"]//Dimensions


(* ::Input:: *)
(*if="d";output`dir=FileNameJoin[{git`local`name,"/mfiles/"}];*)
(*Export[FileNameJoin[{output`dir,"residue_f1f2_"<>if<>".m"}],splt[if,"f1f2","res"]](*\:5bfc\:51fa\:5230\:786c\:76d8*)*)


(* ::Section:: *)
(*e*)


(* ::Text:: *)
(*KR\:56fe\:5de6*)


prfactor=(-I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;if="e";(*\:56fe\:7684\:7f16\:53f7*)M1=MB;M2=MB;(*\:4e2d\:95f4\:6001\:91cd\:5b50\:7684\:8d28\:91cf*)
splt[if,"cls"]=(prfactor*fad[
{k-q,\[CapitalLambda]}->-2,{k,\[CapitalLambda]}->-2,{k,m\[Phi]}->-1,{p2-k,M2}->-1
]
)


if="e";
\[CapitalGamma][if,"spr",\[Mu]]:=GS[k] . GA5 . (GS[p2-k]+M2) . GA[\[Mu]] . GA5


if="e";
splt[if,"fafb","spr"]=to`fafb[\[CapitalGamma][if,"spr",\[Mu]],\[Nu]];(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf*)


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00,\:5bf9\:4e8e\:4e0d\:540c\:7684\:56fe\:8981\:8c03\:6574*)
rl`scalar:=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2,(*\:5bf9k.k\:4f5c\:66ff\:6362*)
SP[k,q]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{k-q,\[CapitalLambda]}->1]+Q2),
SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{p2-k,M2}->1]+\[CapitalLambda]^2-M2^2+MN^2),
SP[k,p1]->1/2(fad[{k-q,\[CapitalLambda]}->1]-Q2-fad[{p2-k,M2}->1]+\[CapitalLambda]^2-M2^2+MN^2)(*\:5bf9k.p1\:4f5c\:66ff\:6362*)
}]


if="e";
splt[if,"fafb"]=Collect[
Expand[(splt[if,"cls"])*(splt[if,"fafb","spr"]/.rl`scalar)],(*\:6700\:7ec8\:7ed3\:679c*)
fad`a[__],(*\:6574\:7406\:4e00\:4e0b*)
Simplify
];


if="e";
splt[if, "fafb"][[1]]//ppg`show(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)


if="e";
splt[if,"fafb","lgc"]=splt[if,"fafb"]/.fad`a->fad`b;


if="e";
Monitor[
splt[if,"fafb","res"]=Table[
confs[[res]]->Residue[splt[if,"fafb","lgc"][[ff]],
{km,ppgZs[[res]]}
]
,{res,{6}}
,{ff,1,2,1}
]
,{res,ff}(*\:76d1\:89c6\:8fd0\:884c\:8fc7\:7a0b*)
];//AbsoluteTiming
splt[if,"fafb","res"]//Dimensions


if="e";
splt[if,"f1f2","res"]=to`f1f2/@splt[if,"fafb","res"];
splt[if,"f1f2","res"]//Dimensions


(* ::Input:: *)
(*output`dir=FileNameJoin[{git`local`name,"/mfiles/"}]*)
(*Export[FileNameJoin[{output`dir,"residue_f1f2_"<>if<>".m"}],splt[if,"f1f2","res"]](*\:5bfc\:51fa\:5230\:786c\:76d8*)*)


(* ::Section:: *)
(*f*)


(* ::Text:: *)
(*\:89c4\:8303\:94fe\:63a5KR\:56fe\:70b9\:5728\:53f3*)


prfactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;if="f";(*\:56fe\:7684\:7f16\:53f7*)M1=MB;M2=MB;(*\:4e2d\:95f4\:6001\:91cd\:5b50\:7684\:8d28\:91cf*)
splt[if,"cls"]=Expand[(prfactor*fad[{k,\[CapitalLambda]}->-4,{k+q,\[CapitalLambda]}->-2,{k,m\[Phi]}->-1,{p1-k,M1}->-1])*(fad[{k,\[CapitalLambda]}->1]+fad[{k+q,\[CapitalLambda]}->1])]


if="f";
\[CapitalGamma][if,"spr",\[Mu]]:=FV[2k+q,\[Mu]] . GS[k] . GA5 . (GS[p1-k]+M1) . GS[k] . GA5


if="f";
splt[if,"fafb","spr"]=to`fafb[\[CapitalGamma][if,"spr",\[Mu]],\[Nu]];(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf*)


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
rl`scalar:=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2,(*\:5bf9k.k\:4f5c\:66ff\:6362*)
SP[k,q]->1/2(fad[{k+q,\[CapitalLambda]}->1]-fad[{k,\[CapitalLambda]}->1]+Q2),
SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{p1-k,M1}->1]+\[CapitalLambda]^2-M1^2+MN^2),(*\:5bf9k.p1\:4f5c\:66ff\:6362*)
SP[k,p2]->1/2(fad[{k+q,\[CapitalLambda]}->1]+Q2-fad[{p1-k,M1}->1]+\[CapitalLambda]^2-M1^2+MN^2)
}]


if="f";
splt[if,"fafb"]=Collect[
Expand[(splt[if,"cls"])*(splt[if,"fafb","spr"]/.rl`scalar)],(*\:6700\:7ec8\:7ed3\:679c*)
fad`a[__],(*\:6574\:7406\:4e00\:4e0b*)
Simplify
];


if="f";
splt[if, "fafb"][[1]]//ppg`show(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)


if="f";
splt[if,"fafb","lgc"]=splt[if,"fafb"]/.fad`a->fad`b;


if="f";
Monitor[
splt[if,"fafb","res"]=Table[
confs[[res]]->Residue[splt[if,"fafb","lgc"][[ff]],
{km,ppgZs[[res]]}
]
,{res,{5,7}}
,{ff,1,2,1}
]
,{res,ff}(*\:76d1\:89c6\:8fd0\:884c\:8fc7\:7a0b*)
];//AbsoluteTiming
splt[if,"fafb","res"]//Dimensions


if="f";
splt[if,"f1f2","res"]=to`f1f2/@splt[if,"fafb","res"];
splt[if,"f1f2","res"]//Dimensions


(* ::Input:: *)
(*output`dir=FileNameJoin[{git`local`name,"/mfiles/"}]*)
(*Export[FileNameJoin[{output`dir,"residue_f1f2_"<>if<>".m"}],splt[if,"f1f2","res"]](*\:5bfc\:51fa\:5230\:786c\:76d8*)*)


(* ::Section:: *)
(*g*)


(* ::Text:: *)
(*\:89c4\:8303\:94fe\:63a5KR\:56fe\:70b9\:5728\:5de6*)


prfactor=(I*CB\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;if="g";(*\:56fe\:7684\:7f16\:53f7*)M1=MB;M2=MB;(*\:4e2d\:95f4\:6001\:91cd\:5b50\:7684\:8d28\:91cf*)
splt[if,"cls"]=Expand[(prfactor*fad[{k,\[CapitalLambda]}->-4,{k-q,\[CapitalLambda]}->-2,{k,m\[Phi]}->-1,{p2-k,M2}->-1])*(fad[{k,\[CapitalLambda]}->1]+fad[{k-q,\[CapitalLambda]}->1])]


if="g";
\[CapitalGamma][if,"spr",\[Mu]]:=FV[2k-q,\[Mu]] . GS[k] . GA5 . (GS[p2-k]+M2) . GS[k] . GA5


if="g";
splt[if,"fafb","spr"]=to`fafb[\[CapitalGamma][if,"spr",\[Mu]],\[Nu]];(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf*)


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00,\:5bf9\:4e8e\:4e0d\:540c\:7684\:56fe\:8981\:8c03\:6574*)
rl`scalar:=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2,(*\:5bf9k.k\:4f5c\:66ff\:6362*)
SP[k,q]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{k-q,\[CapitalLambda]}->1]+Q2),
SP[k,p2]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{p2-k,M2}->1]+\[CapitalLambda]^2-M2^2+MN^2),
SP[k,p1]->1/2(fad[{k-q,\[CapitalLambda]}->1]-Q2-fad[{p2-k,M2}->1]+\[CapitalLambda]^2-M2^2+MN^2)(*\:5bf9k.p1\:4f5c\:66ff\:6362*)
}]


if="g";
splt[if,"fafb"]=Collect[
Expand[(splt[if,"cls"])*(splt[if,"fafb","spr"]/.rl`scalar)],(*\:6700\:7ec8\:7ed3\:679c*)
fad`a[__],(*\:6574\:7406\:4e00\:4e0b*)
Simplify
];


if="g";
splt[if, "fafb"][[1]]//ppg`show(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)


if="g";
splt[if,"fafb","lgc"]=splt[if,"fafb"]/.fad`a->fad`b;


if="g";
Monitor[
splt[if,"fafb","res"]=Table[
confs[[res]]->Residue[splt[if,"fafb","lgc"][[ff]],
{km,ppgZs[[res]]}
]
,{res,{6}}
,{ff,1,2,1}
]
,{res,ff}(*\:76d1\:89c6\:8fd0\:884c\:8fc7\:7a0b*)
];//AbsoluteTiming
splt[if,"fafb","res"]//Dimensions


if="g";
splt[if,"f1f2","res"]=to`f1f2/@splt[if,"fafb","res"];
splt[if,"f1f2","res"]//Dimensions


(* ::Input:: *)
(*output`dir=FileNameJoin[{git`local`name,"/mfiles/"}]*)
(*Export[FileNameJoin[{output`dir,"residue_f1f2_"<>if<>".m"}],splt[if,"f1f2","res"]](*\:5bfc\:51fa\:5230\:786c\:76d8*)*)


(* ::Section:: *)
(*h*)


(* ::Text:: *)
(*tadpole,*)


prfactor=(-I*C\[Phi]\[Phi])/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;if="h";(*\:56fe\:7684\:7f16\:53f7*)M1=MB;M2=MB;(*\:4e2d\:95f4\:6001\:91cd\:5b50\:7684\:8d28\:91cf*)
splt[if,"cls"]=(prfactor*fad[{k,\[CapitalLambda]}->-4,{k,m\[Phi]}->-1])


if="h";
\[CapitalGamma][if,"spr",\[Mu]]:=GA[\[Mu]]


if="h";
splt[if,"fafb","spr"]=to`fafb[\[CapitalGamma][if,"spr",\[Mu]],\[Nu]];(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf*)


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
rl`scalar:=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2,(*\:5bf9k.k\:4f5c\:66ff\:6362*)
SP[k,q]->1/2(fad[{k+q,\[CapitalLambda]}->1]-fad[{k,\[CapitalLambda]}->1]+Q2),
SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{p1-k,M1}->1]+\[CapitalLambda]^2-M1^2+MN^2),(*\:5bf9k.p1\:4f5c\:66ff\:6362*)
SP[k,p2]->1/2(fad[{k+q,\[CapitalLambda]}->1]+Q2-fad[{p1-k,M1}->1]+\[CapitalLambda]^2-M1^2+MN^2)
}]


if="h";
splt[if,"fafb"]=Collect[
Expand[(splt[if,"cls"])*(splt[if,"fafb","spr"]/.rl`scalar)],(*\:6700\:7ec8\:7ed3\:679c*)
fad`a[__],(*\:6574\:7406\:4e00\:4e0b*)
Simplify
];


if="h";
splt[if, "fafb"][[1]]//ppg`show(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)


if="h";
splt[if,"fafb","lgc"]=splt[if,"fafb"]/.fad`a->fad`b;


if="h";
Monitor[
splt[if,"fafb","res"]=Table[
confs[[res]]->Residue[splt[if,"fafb","lgc"][[ff]],
{km,ppgZs[[res]]}
]
,{res,{1,2}}
,{ff,1,2,1}
]
,{res,ff}(*\:76d1\:89c6\:8fd0\:884c\:8fc7\:7a0b*)
];//AbsoluteTiming
splt[if,"fafb","res"]//Dimensions


if="h";
splt[if,"f1f2","res"]=to`f1f2/@splt[if,"fafb","res"];
splt[if,"f1f2","res"]//Dimensions


(* ::Input:: *)
(*if="h";output`dir=FileNameJoin[{git`local`name,"/mfiles/"}]*)
(*Export[FileNameJoin[{output`dir,"residue_f1f2_"<>if<>".m"}],splt[if,"f1f2","res"]](*\:5bfc\:51fa\:5230\:786c\:76d8*)*)


(* ::Section:: *)
(*i*)


(* ::Text:: *)
(*tadpole \:89c4\:8303\:94fe\:63a5*)


prfactor=(I*C\[Phi]\[Phi])/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;if="i";(*\:56fe\:7684\:7f16\:53f7*)M1=MB;M2=MB;(*\:4e2d\:95f4\:6001\:91cd\:5b50\:7684\:8d28\:91cf*)
splt[if,"cls"]=prfactor*fad[{k+q,\[CapitalLambda]}->-2,{k,\[CapitalLambda]}->-4,{k,m\[Phi]}->-1]*(fad[{k,\[CapitalLambda]}->1]+fad[{k+q,\[CapitalLambda]}->1])*(FV[2k+q,\[Mu]])*2


if="i";
\[CapitalGamma][if,"spr",\[Mu]]:=GS[k]


if="i";
splt[if,"fafb","spr"]=to`fafb[\[CapitalGamma][if,"spr",\[Mu]],\[Nu]];(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf*)


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
rl`scalar:=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2,(*\:5bf9k.k\:4f5c\:66ff\:6362*)
SP[k,q]->1/2(fad[{k+q,\[CapitalLambda]}->1]-fad[{k,\[CapitalLambda]}->1]+Q2),
SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{p1-k,M1}->1]+\[CapitalLambda]^2-M1^2+MN^2),(*\:5bf9k.p1\:4f5c\:66ff\:6362*)
SP[k,p2]->1/2(fad[{k+q,\[CapitalLambda]}->1]+Q2-fad[{p1-k,M1}->1]+\[CapitalLambda]^2-M1^2+MN^2)
}]


if="i";
splt[if,"fafb"]=Collect[
Expand[(splt[if,"cls"])*(splt[if,"fafb","spr"]/.rl`scalar)],(*\:6700\:7ec8\:7ed3\:679c*)
fad`a[__],(*\:6574\:7406\:4e00\:4e0b*)
Simplify
];


if="i";
splt[if, "fafb"][[1]]//ppg`show(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)


if="i";
splt[if,"fafb","lgc"]=splt[if,"fafb"]/.fad`a->fad`b;


if="i";
Monitor[
splt[if,"fafb","res"]=Table[
confs[[res]]->Residue[splt[if,"fafb","lgc"][[ff]],
{km,ppgZs[[res]]}
]
,{res,{7}}
,{ff,1,2,1}
]
,{res,ff}(*\:76d1\:89c6\:8fd0\:884c\:8fc7\:7a0b*)
];//AbsoluteTiming
splt[if,"fafb","res"]//Dimensions


if="i";
splt[if,"f1f2","res"]=to`f1f2/@splt[if,"fafb","res"];
splt[if,"f1f2","res"]//Dimensions


(* ::Input:: *)
(*if="i";output`dir=FileNameJoin[{git`local`name,"/mfiles/"}]*)
(*Export[FileNameJoin[{output`dir,"residue_f1f2_"<>if<>".m"}],splt[if,"f1f2","res"]](*\:5bfc\:51fa\:5230\:786c\:76d8*)*)


(* ::Section:: *)
(*j*)


(* ::Text:: *)
(*bubble \:56fe*)


prfactor=(I*C\[Phi]\[Phi])/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1;if="j";(*\:56fe\:7684\:7f16\:53f7*)M1=MB;M2=MB;(*\:4e2d\:95f4\:6001\:91cd\:5b50\:7684\:8d28\:91cf*)
splt[if,"cls"]=(prfactor*fad[
{k,\[CapitalLambda]}->-4,{k,m\[Phi]}->-1,{k+q,m\[Phi]}->-1
])


if="j";
\[CapitalGamma][if,"spr",\[Mu]]:=FV[2k+q,\[Mu]] . GS[k]


if="j";
splt[if,"fafb","spr"]=to`fafb[\[CapitalGamma][if,"spr",\[Mu]],\[Nu]];(*\:91c7\:7528\:5f20\:91cf\:7ea6\:5316\:7684\:505a\:6cd5,\:6700\:540e\:518d\:628a\:6307\:6807\:90fd\:6362\:6210plus\:5206\:91cf*)


(*\:632f\:5e45\:5206\:5b50\:4e0a\:7684\:6807\:91cf\:79ef\:6309\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:7ebf\:6027\:5c55\:5f00*)
rl`scalar:=FCI[{
SP[k,k]->fad[{k,\[CapitalLambda]}->1]+\[CapitalLambda]^2,(*\:5bf9k.k\:4f5c\:66ff\:6362*)
SP[k,q]->1/2(fad[{k+q,\[CapitalLambda]}->1]-fad[{k,\[CapitalLambda]}->1]+Q2),
SP[k,p1]->1/2(fad[{k,\[CapitalLambda]}->1]-fad[{p1-k,M1}->1]+\[CapitalLambda]^2-M1^2+MN^2),(*\:5bf9k.p1\:4f5c\:66ff\:6362*)
SP[k,p2]->1/2(fad[{k+q,\[CapitalLambda]}->1]+Q2-fad[{p1-k,M1}->1]+\[CapitalLambda]^2-M1^2+MN^2)
}]


if="j";
splt[if,"fafb"]=Collect[
Expand[(splt[if,"cls"])*(splt[if,"fafb","spr"]/.rl`scalar)],(*\:6700\:7ec8\:7ed3\:679c*)
fad`a[__],(*\:6574\:7406\:4e00\:4e0b*)
Simplify
];


if="j";
splt[if, "fafb"][[1]]//ppg`show(*\:67e5\:770b\:4f20\:64ad\:5b50\:7684\:53ef\:80fd\:7ed3\:6784*)


if="j";
splt[if,"fafb","lgc"]=splt[if,"fafb"]/.fad`a->fad`b;


if="j";
Monitor[
splt[if,"fafb","res"]=Table[
confs[[res]]->Residue[splt[if,"fafb","lgc"][[ff]],
{km,ppgZs[[res]]}
]
,{res,{1,2}}
,{ff,1,2,1}
]
,{res,ff}(*\:76d1\:89c6\:8fd0\:884c\:8fc7\:7a0b*)
];//AbsoluteTiming
splt[if,"fafb","res"]//Dimensions


if="j";
splt[if,"f1f2","res"]=to`f1f2/@splt[if,"fafb","res"];
splt[if,"f1f2","res"]//Dimensions


(* ::Input:: *)
(*if="j";output`dir=FileNameJoin[{git`local`name,"/mfiles/"}]*)
(*Export[FileNameJoin[{output`dir,"residue_f1f2_"<>if<>".m"}],splt[if,"f1f2","res"]](*\:5bfc\:51fa\:5230\:786c\:76d8*)*)


(* ::Section:: *)
(*k*)


(* ::Text:: *)
(*tadpole \:78c1\:77e9*)


(* ::Section:: *)
(*l*)


(* ::Text:: *)
(*\:9ad8\:9636 bubble*)


(* ::Chapter::Closed:: *)
(*split decuplet*)


(* ::Section:: *)
(*m*)


(* ::Section:: *)
(*n*)


(* ::Section:: *)
(*o*)


(* ::Section:: *)
(*p*)


(* ::Section:: *)
(*q*)


(* ::Section:: *)
(*r*)


(* ::Section:: *)
(*s*)


(* ::Section:: *)
(*t*)


(* ::Section:: *)
(*u*)