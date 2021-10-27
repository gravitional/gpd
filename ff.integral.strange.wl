(* ::Package:: *)

(* ::Title:: *)
(*integral_strange.wl*)


(* ::Chapter::Closed:: *)
(*initial*)


(*\:672c\:6587\:4ef6\:7684\:540d\:79f0*)
fileName=If[$Notebooks,NotebookFileName[],$InputFileName];
(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c\:ff0c\:5c31\:5237\:65b0\:7b14\:8bb0\:672c\:7684\:6807\:9898*)
Once@If[$Notebooks,NotebookWrite[Cells[][[1]],Cell[Last@FileNameSplit[fileName],"Title"]]];
(*\:67e5\:627e init.wl, \:5bfc\:5165\:6839\:76ee\:5f55\:548c\:51fd\:6570\:5b9a\:4e49.*)
Catch@Module[{recurFind,start=1,depMax},
depMax=FileNameDepth[fileName];(*\:8def\:5f84\:7684\:6700\:5927\:5c42\:6b21*)
(*-------\:5b9a\:4e49\:9012\:5f52\:51fd\:6570-------*)
recurFind[dep_Integer]:=If[dep<=depMax,
SetDirectory[DirectoryName[fileName,dep]];(*SetDirectory[]\:8bbe\:7f6e\:5de5\:4f5c\:76ee\:5f55\:4e3a\:5bb6\:76ee\:5f55*)
If[FileExistsQ["init.wl"],Get["init.wl"];Throw["The base directory is : "<>gitLocalName],recurFind[dep+1](*\:5982\:679c\:8fd9\:4e00\:5c42\:627e\:4e0d\:5230\:ff0c\:5c31\:4e0a\:5347\:4e00\:5c42*)];
ResetDirectory[];(*\:91cd\:8bbe\:4e3a\:4e4b\:524d\:7684\:76ee\:5f55*),
Throw["I cann't find any init.wl in this project"]
];
recurFind[start];
]
(* \:8bb0\:5f55 master Kernel \:7684\:8fd0\:884c\:6a21\:5f0f*)
$inNBook=$Notebooks;


(* ::Section::Closed:: *)
(*cmd argumnets*)


echo[mfilesDir=FileNameJoin[{gitLocalName,"mfiles"}]];
(*\:5982\:679c\:8fd8\:4e0d\:5b58\:5728\:ff0c\:5219\:521b\:5efa\:76ee\:5f55*)
If[!DirectoryQ[mfilesDir],CreateDirectory[mfilesDir];echo["Create a new directory: ./mfiles/"]] ;
(* \:6240\:6709\:8d39\:66fc\:56fe\:7684 tag \:5217\:8868 *)
fyAmpTagLst=Get[FileNameJoin@{gitLocalName,"gen.integral.TagList.wl"}];


(* \:5904\:7406\:811a\:672c\:53c2\:6570,\:6a21\:62df\:547d\:4ee4\:884c\:8f93\:5165\:53c2\:6570\:7684\:60c5\:5f62 *)
If[!$Notebooks,
inputCml=$ScriptCommandLine,(*\:5982\:679c\:5728\:547d\:4ee4\:884c\:6267\:884c*)
(*++++++++++++++++++++++++++++++++++++++++*)
inputCml={fileName,(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c, \:6a21\:4eff\:547d\:4ee4\:884c, \:4ee4\:7b2c\:4e00\:4e2a\:53c2\:6570\:662f\:6b64\:811a\:672c\:7684\:7edd\:5bf9\:8def\:5f84*)
(*\:5176\:4ed6\:53c2\:6570*)
inSimul=Nothing
}
];
echo["the input parameter is:\n",inputCml];


(*\:5982\:679c\:547d\:4ee4\:884c\:6307\:5b9a\:4e86 part\:ff0c\:5316\:7b80\:5708\:79ef\:5206\:5217\:8868\:6307\:5b9a\:7684part\:ff0c\:5426\:5219\:ff0c\:9ed8\:8ba4\:5316\:7b80\:6240\:6709\:5708\:79ef\:5206*)
Block[{pSpec},
If[Length@inputCml>1,
fyAmpTagPart=Check[
pSpec=ToExpression@inputCml[[2]];fyAmpTagLst[[pSpec]],
Abort[]
],
fyAmpTagPart=fyAmpTagLst;
]]


(*
\:53ef\:4ee5\:4f7f\:7528 LooprefineSeries \:5c55\:5f00\:5230 order 2, \:4e5f\:5c31\:662f Q2^2, Q2= - q^2.
\:4f7f\:7528 strange \:62c9\:5f00\:65b9\:5f0f.
\:7ea7\:6570\:5c55\:5f00\:7684\:7cfb\:6570\:53ef\:4ee5\:4f7f\:7528 SeriesCoefficient[expr,order] \:6765\:53d1\:73b0.
\:53d1\:6563\:68c0\:67e5\:ff0c\:786e\:4fdd\:6ca1\:6709\:53d1\:6563.
\:4f7f\:7528\:7684\:6b63\:89c4\:5b50\:4e3a (\[CapitalLambda]^2-m\[Phi]^2)^2/(k^2-\[CapitalLambda]^2+i \[CurlyEpsilon])^2,
\:7535\:78c1\:9876\:70b9\:4f7f\:7528\:7684\:6b63\:89c4\:5b50\:9700\:8981\:4f5c\:7ebf\:6027\:53d8\:6362.
\:6839\:636e Package-X \:7684\:7ea6\:5b9a\:ff0c\:5708\:79ef\:5206\:524d\:9762\:4e0d\:9700\:8981\:8f93\:5165,1/(2\[Pi])^4
++++++++++++++++++++++++++++++++++++++++++++++++
\:5982\:679c\:8fd0\:884c\:901f\:5ea6\:6bd4\:8f83\:6162\:ff0c\:53ef\:4ee5\:8bbe\:7f6e\:9009\:9879 LoopRefine[xxx,Organization -> None],\:8fd9\:6837\:53ef\:4ee5\:6539\:5584\:8ba1\:7b97\:901f\:5ea6.
Package - X \:7684\:4e0d\:540c\:53ea\:5904\:5728\:4e8e, \:5b83\:7684\:5206\:5b50\:4f5c\:4e3a\:4e00\:4e2a\:6574\:4f53\:8f93\:5165, \:4f7f\:7528\:51fd\:6570 LTensor,DiracMatrix \:7b49\:7b49.
\:800c\:5206\:6bcd\:4ee5\:5217\:8868\:7684\:5f62\:5f0f\:8f93\:5165\:7ed9 LoopIntegrate,  {{a},{b},{b},...}
++++++++++++++++++++++++++++++++++++++++++++++++
{p1.p1 \[Rule]mN^2,p2.p2 \[Rule]mN^2,p1.p2\[Rule]-q.q/2+mN^2}
{q.p1 \[Rule]-q.q/2,q.p2\[Rule]q.q/2}
p2=p1+q;
LTensor[DiracS,\[Mu],\[Nu]] or Subscript[\[Sigma], \[Mu],\[Nu]] \:8868\:793a\:4f3d\:9a6c\:77e9\:9635\:7684\:5bf9\:6613\:5b50,(I/2)[\[Sigma]\[Mu],\[Sigma]\[Nu]].
LoopIntegrate[\:5206\:5b50,k,{p2-k,m},{p1-k,m},{k,0}], \:4f8b\:5982
LoopIntegrate[{k.k - m^2, k.p - m^2, p.p - m^2}, k, {k, m}, {k + p, m}]
LoopIntegrate \:53ef\:4ee5\:7ebf\:6027\:4f5c\:7528\:4e8e\:5206\:5b50\:5217\:8868\:ff0c
\:5728 Spur \:548c DiracMatrix \:4e2d\:7684 \:77e9\:9635 \:53ef\:4ee5\:7528 DiracMatrix \:5305\:88f9\:8d77\:6765.
+++++++++++++++++++++++++++++++++++++++++++++++
\:89e3\:51b3\:53ef\:7591\:7684\:8fd0\:52a8\:5b66\:5947\:70b9:
delayedNumerator := Contract[Spur[LTensor[\[Gamma], \[Rho]], (p2.\[Gamma] - k.\[Gamma] + m Dirac1), 
LTensor[\[Gamma], \[Mu]], (p1.\[Gamma] - k.\[Gamma] + m Dirac1), LTensor[\[Gamma], \[Nu]], Projector["F2", \[Mu]][{p1, m}, {p2, m}]] LTensor[\[DoubleStruckG], \[Rho], \[Nu]]];
LoopIntegrate[delayedNumerator, k, {k - p2, m}, {k - p1, m}, {k, 0}] /. {p1.p1 -> m^2, p2.p2 -> m^2, p1.p2 -> -q.q/2 + m^2}
*)


(* ::Section::Closed:: *)
(*Package-X*)


(* ::Input:: *)
(*SetOptions[EvaluationNotebook[],CommonDefaultFormatTypes->{"Output"->StandardForm}](*\:8bbe\:7f6e\:663e\:5f0f\:683c\:5f0f\:4e3a\:6807\:51c6\:683c\:5f0f*)*)


If[NameQ["\[Sigma]"],echo["please remove the definitions of \[Sigma], \[Sigma] will be used in package-X"];Remove["Global`\[Sigma]"]];(* \[Sigma] \:662f package-X \:7684\:4fdd\:7559\:6807\:8bc6\:7b26,\:9700\:8981\:6e05\:9664*)
echo["launch parallel kernels"];
(* \:5e76\:884c\:8fd0\:7b97\:51c6\:5907*)
Needs["X`"];ParallelNeeds["X`"];
(*\:542f\:52a8\:5e76\:884c\:5185\:6838*)
CloseKernels[];
Switch[{$MachineName,$System},
{"OP7050","Linux x86 (64-bit)"},LaunchKernels[6],
_,LaunchKernels[]
];


reg::usage="\:6b63\:89c4\:5b50 F[k]=(\[CapitalLambda]^2-m\[Phi]^2)^2/(k^2-\[CapitalLambda]^2+I*\[CurlyEpsilon])^2, \:5176\:4e2d m\[Phi] \:662f\:4ecb\:5b50\:8d28\:91cf\:ff0c\:5bf9\:4e8e\:5149\:5b50,\:6b64\:8d28\:91cf\:4e3a\:96f6\:3002\:6b63\:89c4\:5b50\:5f52\:4e00\:5316\:5230 F[m\[Phi]]=1. \:751f\:6210\:5217\:8868\:ff0c\:7b2c\:4e00\:9879\:662f\:5206\:5b50\:ff0c\:7b2c\:4e8c\:9879\:662f\:5206\:6bcd";
prp::usage="prp[{k,\[CapitalLambda],2}],\:591a\:4e2a\:4f20\:64ad\:5b50\:53ef\:4ee5\:4f9d\:6b21\:63d0\:4f9b,prp[]...";
reg[q_,\[CapitalLambda]_]:=intgd[num[\[CapitalLambda]^4],prp[{q,\[CapitalLambda],2}]](* \:5bf9\:4e8e\:5149\:5b50\:ff0c\:8d28\:91cf\:4e3a\:96f6 *)
reg[k_,m\[Phi]_,\[CapitalLambda]_]:=intgd[num[(\[CapitalLambda]^2-m\[Phi]^2)^2],prp[{k,\[CapitalLambda],2}]] (*\:5bf9\:4e8e\:4ecb\:5b50\:ff0c\:9700\:8981\:63d0\:4f9b\:8d28\:91cf*)
(* +++++++++++++++++++++++++++++++++++ *)
prp1::usage="\:5206\:5b50\:4e3a1\:7684\:666e\:901a\:4f20\:64ad\:5b50";
prp1[k_,m_]:=intgd[num[1],prp[{k,m}]]


dprop::usage="\:5341\:91cd\:6001\:91cd\:5b50\:4f20\:64ad\:5b50";
dprop[p_,m_,\[Mu]_,\[Nu]_]:=(-LTensor[MetricG,\[Mu],\[Nu]]*Dirac1(*\:5355\:4f4d\:77e9\:9635*)
+DiracMatrix[LTensor[DiracG,\[Mu]],LTensor[DiracG,\[Nu]]]/3+
(2 LTensor[p,\[Mu]]*LTensor[p,\[Nu]]*Dirac1)/(3 m^2)+
(LTensor[DiracG, \[Mu]]*LTensor[p, \[Nu]]-LTensor[DiracG,\[Nu]]*LTensor[p, \[Mu]])/(3 m)
)


cltcom::usage="\:6b63\:89c4\:5b50\:7684\:7ec4\:5408";
cltcom[{k_,q_},{m\[Phi]_,\[CapitalLambda]_}]:=intgd[
num[(\[CapitalLambda]^2-m\[Phi]^2)^2*(LDot[k,k]+LDot[k+q,k+q]-2 \[CapitalLambda]^2)(-1)],(*(2k+q)^\[Mu] \:653e\:5728\:65cb\:91cf\:90e8\:5206\:8003\:8651 *)
prp[{k,\[CapitalLambda],2}],prp[{k+q,\[CapitalLambda],2}]
] 
(*(Overscript[\[CapitalLambda], _]^4(k.k+(k+q).(k+q)-2\[CapitalLambda]^2))/((k.k-\[CapitalLambda]^2)^2((k+q).(k+q)-\[CapitalLambda]^2)^2)(-1)(2k+q)^\[Mu], \:989d\:5916\:9876\:70b9\:7684\:6b63\:89c4\:5b50\:7ec4\:5408.*)


dgam3::usage="\:5341\:91cd\:6001F1\:9876\:89d2\:4f3d\:9a6c\:77e9\:9635";
dgam3[\[Mu]_,\[Nu]_,\[Rho]_]:=(-I)/2 (
DiracMatrix[LTensor[DiracS,\[Mu],\[Nu]],LTensor[DiracG, \[Rho]]]+
DiracMatrix[LTensor[DiracG,\[Rho]],LTensor[DiracS,\[Mu],\[Nu]]]
)


\[CapitalTheta]::usage="\:5341\:91cd\:6001\:79bb\:58f3\:53c2\:6570\:6240\:5728\:7684 \[Gamma] \:77e9\:9635";
\[CapitalTheta][\[Mu]_,\[Nu]_]:=I LTensor[DiracS,\[Mu],\[Nu]]
spDec::usage="\:5341\:91cd\:6001\:4f20\:64ad\:5b50\:7684\:65cb\:91cf\:90e8\:5206";
spDec[{\[Alpha]_,\[Beta]_},{p_,md_}]:=DiracMatrix[-(LDot[DiracG,p] +md Dirac1),
LTensor[MetricG,\[Alpha],\[Beta]] Dirac1 -1/3 DiracMatrix[LTensor[DiracG,\[Alpha]],LTensor[DiracG,\[Beta]]]
-(LTensor[DiracG,\[Alpha]] LTensor[p,\[Beta]]-LTensor[DiracG,\[Beta]] LTensor[p,\[Alpha]])/(3md)
-(2LTensor[p,\[Alpha]] LTensor[p,\[Beta]] Dirac1)/(3md^2) 
 ]


(* ::Section::Closed:: *)
(*kinematic quantities*)


(*\:58f0\:660e\:4e00\:4e9b\:8fd0\:52a8\:5b66\:53d8\:91cf,\:4f7f\:7528 atom \:8868\:8fbe\:5f0f\:ff0cpackage-X \:7684 loopIntegrate \:9700\:8981\:79ef\:5206\:52a8\:91cf\:4e3a\:539f\:5b50\:8868\:8fbe\:5f0f*)
\[CapitalLambda];(*\:6b63\:89c4\:5b50\:8d28\:91cf*)
mE;(*\:5916\:817f\:7c92\:5b50\:7684\:8d28\:91cf*);
mm1;mm2;(*\:4e2d\:95f4\:516b\:91cd\:6001\:4ecb\:5b50*)
mo1;mo2;(*\:4e2d\:95f4\:516b\:91cd\:6001\:91cd\:5b50*)
md1;md2;(*\:4e2d\:95f4\:5341\:91cd\:6001\:91cd\:5b50*)
Q2;(*Q2=-q^2,\:8f6c\:79fb\:52a8\:91cf\:5e73\:65b9\:7684\:8d1f\:503c*)
p1;p2;(*\:521d\:6001\:52a8\:91cf\:ff0c\:672b\:6001\:52a8\:91cf*)
k;(*\:5355\:5708\:56fe\:7684\:5708\:52a8\:91cf, \:79ef\:5206\:53d8\:91cf\:9700\:8981\:662f atomatic \:8868\:8fbe\:5f0f*)


paraInitial=Hold[
(* \:58f0\:660e\:8fd9\:4e9b\:5e38\:91cf\:662f\:6d1b\:4f26\:5179\:6807\:91cf lorentz scalars *)
LScalarQ[\[CapitalLambda]]=True;LScalarQ[mE]=True;
LScalarQ[mm1]=True;LScalarQ[mm2]=True;
LScalarQ[mo1]=True;LScalarQ[mo2]=True;
LScalarQ[md1]=True;LScalarQ[md2]=True;
LScalarQ[Q2]=True;
(* \:521d\:672b\:6001,\:8fd0\:52a8\:5b66\:5173\:7cfb*)
onShell={LDot[p1,p1]->mE^2,LDot[p2,p2]->mE^2,LDot[p1,p2]->Q2/2+mE^2};
(*\:8bbe\:7f6e\:5316\:7b80\:65f6\:95f4\:9650\:5236*)
SetOptions[Simplify,TimeConstraint->1];
SetOptions[Refine,TimeConstraint->1];
Off[Simplify::time];Off[Refine::time];
];


(*\:5e76\:884c\:521d\:59cb\:5316*)
DistributeDefinitions[$inNBook,gitLocalName,fileName,echo,enList,enString,
mfilesDir,
reg,prp,intgd,num,prp1,dprop,cltcom,dgam3,\[CapitalTheta],spDec
];
ReleaseHold@paraInitial
ParallelEvaluate[ReleaseHold@paraInitial];


(* ::Chapter:: *)
(*loop Integral: octet*)


SetAttributes[paraLintSubmit,HoldAll];
paraLintSubmit[spin_,scalar_,fyTag_]:=ParallelSubmit[
(* ParallelSubmit \:5177\:6709 HoldAllComplete \:5c5e\:6027, \:4e0b\:9762\:7684 Block \:5757\:4e0d\:4f1a\:88ab\:8ba1\:7b97 *)
Block[{numer,denom,fyAmp,time0Result,path},
(* \:5408\:6210\:5708\:79ef\:5206\:7684\:5206\:5b50\:548c\:5206\:6bcd *)
numer=Times@@Cases[scalar,num[x_]:>x]*spin;(*\:751f\:6210\:5206\:5b50,*)
denom=Cases[scalar,prp[x_]:>x];(*\:751f\:6210\:5206\:6bcd,\:4e5f\:5c31\:662f\:4f20\:64ad\:5b50*)
(* -----------------\:8fd4\:56de\:503c: \:5c06\:5708\:79ef\:5206,\:5206\:89e3\:5230\:6807\:51c6\:57fa Passarino-Veltman \:51fd\:6570 -------------------------------- *)
echo["loopIntegrate on: ",fyTag];
time0Result=LoopIntegrate[numer,k,Sequence@@denom,Cancel->Automatic,Apart->True]/.onShell//AbsoluteTiming;
fyAmp=<|
"tag"->fyTag,
"time"->First@time0Result,
"expr"->Last@time0Result
|>;
(*\:9009\:5b9a\:5bfc\:51fa\:683c\:5f0f\:ff0c\:5e76\:6253\:5370\:4fdd\:5b58\:6210\:529f*)
path=FileNameJoin[{mfilesDir,"integral.strange."<>StringRiffle[fyTag,"."]<>".wdx"}];
Export[path,fyAmp];echo["Exporting finished: ",path];
(*\:5982\:679c\:5728\:7b14\:8bb0\:672c\:4e2d\:ff0c\:8fd4\:56de\:8ba1\:7b97\:7684\:7ed3\:679c*)
If[$inNBook,fyAmp]]
]


end=4;delta=end/80;(*\:793a\:610f\:56fe\:7684\:5c3a\:5bf8\:521d\:59cb\:5316*)


(* ::Section::Closed:: *)
(*RB,mes,oct*)


(* \:6309\:7167 package-X\:7684\:7ea6\:5b9a, \:4e0d\:5199 1/(2\[Pi])^4 *)
fyTag={"RB","mes","oct"};
fyAmp[fyTag,{"intg","eid"}]=With[{tag=fyTag},
paraLintSubmit[
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:65cb\:91cf\:90e8\:5206*)
Contract[
LTensor[2k+p2-p1,\[Mu]]*Spur[
LDot[DiracG,k+p2-p1],DiracG5,
LDot[DiracG,(p1-k)]+mo1*Dirac1,(* Dirac1 \:662f\:5355\:4f4d\:77e9\:9635*)
LDot[DiracG,k],DiracG5,
#]]&/@{
Projector["F1",\[Mu]][{p1,mE},{p2,mE}],
Projector["F2",\[Mu]][{p1,mE},{p2,mE}]
}/.onShell,
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:6807\:91cf\:90e8\:5206*)
{
(* --------- \:6b63\:89c4\:5b50 --------- *)
reg[p2-p1,\[CapitalLambda]],
reg[p2-p1+k,mm1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
(* --------- \:666e\:901a\:4f20\:64ad\:5b50 --------- *)
prp1[k+p2-p1,mm1],
prp1[k,mm1],
prp1[p1-k,mo1]
}/.intgd->Sequence,
(* \:8d39\:66fc\:79ef\:5206\:7684 Tag *)
tag
]];


(* ::Section::Closed:: *)
(*KR,mes,oct,left*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{end/4,0},{end/4,-end/5}}],*)
(*Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end,-4delta}]*)
(*},ImageSize->Small]*)


(* \:6309\:7167 package-X\:7684\:7ea6\:5b9a, \:4e0d\:5199 1/(2\[Pi])^4 *)
fyTag={"KR","mes","oct","left"};
fyAmp[fyTag,{"intg","eid"}]=With[{tag=fyTag},
paraLintSubmit[
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:65cb\:91cf\:90e8\:5206*)
Contract[
Spur[
LDot[DiracG,k],DiracG5,
LDot[DiracG,(p2-k)]+mo1 Dirac1,(* Dirac1 \:662f\:5355\:4f4d\:77e9\:9635*)
LTensor[DiracG,\[Mu]],DiracG5,
#]]&/@{
Projector["F1",\[Mu]][{p1,mE},{p2,mE}],
Projector["F2",\[Mu]][{p1,mE},{p2,mE}]
}/.onShell,
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:6807\:91cf\:90e8\:5206*)
{
(* --------- \:6b63\:89c4\:5b50 --------- *)
reg[p2-p1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
(* --------- \:666e\:901a\:4f20\:64ad\:5b50 --------- *)
prp1[k,mm1],
prp1[p2-k,mo1]
}/.intgd->Sequence,
(* \:8d39\:66fc\:79ef\:5206\:7684 Tag *)
tag
]];


(* ::Section::Closed:: *)
(*KR,mes,oct,right*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{3/4end,0},{3/4end,-end/5}}],*)
(*Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end+4delta,+4delta}]*)
(*},ImageSize->Small]*)


(* \:6309\:7167 package-X\:7684\:7ea6\:5b9a, \:4e0d\:5199 1/(2\[Pi])^4 *)
fyTag={"KR","mes","oct","right"};
fyAmp[fyTag,{"intg","eid"}]=With[{tag=fyTag},
paraLintSubmit[
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:65cb\:91cf\:90e8\:5206*)
Contract[
Spur[
LTensor[DiracG,\[Mu]],DiracG5,
LDot[DiracG,(p1-k)]+mo1 Dirac1,(* Dirac1 \:662f\:5355\:4f4d\:77e9\:9635*)
LDot[DiracG,k],DiracG5,
#]]&/@{
Projector["F1",\[Mu]][{p1,mE},{p2,mE}],
Projector["F2",\[Mu]][{p1,mE},{p2,mE}]
}/.onShell,
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:6807\:91cf\:90e8\:5206*)
{
(* --------- \:6b63\:89c4\:5b50 --------- *)
reg[p2-p1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
(* --------- \:666e\:901a\:4f20\:64ad\:5b50 --------- *)
prp1[k,mm1],
prp1[p1-k,mo1]
}/.intgd->Sequence,(* \:8d39\:66fc\:79ef\:5206\:7684 Tag *)
(* \:8d39\:66fc\:79ef\:5206\:7684 Tag *)
tag
]];


(* ::Section::Closed:: *)
(*KR,mes,oct,add,left*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{end/4,0},{end/4,-end/5}}],*)
(*Disk[{end/4,0},.1],*)
(*Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end,-4delta}]*)
(*},ImageSize->Small]*)


(* \:6309\:7167 package-X\:7684\:7ea6\:5b9a, \:4e0d\:5199 1/(2\[Pi])^4 *)
fyTag={"KR","mes","oct","add","left"};
fyAmp[fyTag,{"intg","eid"}]=With[{tag=fyTag},
paraLintSubmit[
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:65cb\:91cf\:90e8\:5206*)
Contract[
(* -R^\[Mu](-k,q) \:7684 -(-2k+q)^\[Mu] \:8003\:8651\:5728\:8fd9\:91cc *)
(-1)LTensor[-2k+(p2-p1),\[Mu]]*Spur[
LDot[DiracG,k],DiracG5,
LDot[DiracG,(p2-k)]+mo1 Dirac1,(* Dirac1 \:662f\:5355\:4f4d\:77e9\:9635*)
LDot[DiracG,k-(p2-p1)],DiracG5,
#]]&/@{
Projector["F1",\[Mu]][{p1,mE},{p2,mE}],
Projector["F2",\[Mu]][{p1,mE},{p2,mE}]
}/.onShell,
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:6807\:91cf\:90e8\:5206*)
{
(* --------- \:6b63\:89c4\:5b50 --------- *)
reg[p2-p1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
cltcom[{-k,p2-p1},{mm1,\[CapitalLambda]}],
(* --------- \:666e\:901a\:4f20\:64ad\:5b50 --------- *)
prp1[k,mm1],
prp1[p2-k,mo1]
}/.intgd->Sequence,
(* \:8d39\:66fc\:79ef\:5206\:7684 Tag *)
tag
]];


(* ::Section::Closed:: *)
(*KR,mes,oct,add,right*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{3/4end,0},{3/4end,-end/5}}],*)
(*Disk[{3/4end,0},.1],*)
(*Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end+4delta,+4delta}]*)
(*},ImageSize->Small]*)


(* \:6309\:7167 package-X\:7684\:7ea6\:5b9a, \:4e0d\:5199 1/(2\[Pi])^4 *)
fyTag={"KR","mes","oct","add","right"};
fyAmp[fyTag,{"intg","eid"}]=With[{tag=fyTag},
paraLintSubmit[
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:65cb\:91cf\:90e8\:5206*)
Contract[
(* R^\[Mu](k,q)\:4e2d\:7684 (2k+q)^\[Mu] \:8003\:8651\:5728\:8fd9\:91cc *)
LTensor[2k+(p2-p1),\[Mu]]*Spur[
LDot[DiracG,k+p2-p1],DiracG5,
LDot[DiracG,p1-k]+mo1 Dirac1,(* Dirac1 \:662f\:5355\:4f4d\:77e9\:9635*)
LDot[DiracG,k],DiracG5,
#]]&/@{
Projector["F1",\[Mu]][{p1,mE},{p2,mE}],
Projector["F2",\[Mu]][{p1,mE},{p2,mE}]
}/.onShell,
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:6807\:91cf\:90e8\:5206*)
{
(* --------- \:6b63\:89c4\:5b50 --------- *)
reg[p2-p1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
cltcom[{k,p2-p1},{mm1,\[CapitalLambda]}],
(* --------- \:666e\:901a\:4f20\:64ad\:5b50 --------- *)
prp1[k,mm1],
prp1[p1-k,mo1]
}/.intgd->Sequence,
(* \:8d39\:66fc\:79ef\:5206\:7684 Tag *)
tag
]];


(* ::Section:: *)
(*RB,oct,F1,*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{end/2,-end/8},{end/2,0}}],*)
(*Text["v1",{end/4,-4delta}],Text["v2",{end/2,4delta}],Text["v3",{3/4end,-4delta}]*)
(*},ImageSize->Small]*)


(* \:6309\:7167 package-X\:7684\:7ea6\:5b9a, \:4e0d\:5199 1/(2\[Pi])^4 *)
fyTag={"RB","oct","F1"};
fyAmp[fyTag,{"intg","eid"}]=With[{tag=fyTag},
paraLintSubmit[
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:65cb\:91cf\:90e8\:5206*)
Contract[
Spur[
LDot[DiracG,k],DiracG5,
LDot[DiracG,(p2-k)]+mo2*Dirac1,LTensor[DiracG,\[Mu]],
LDot[DiracG,(p1-k)]+mo1*Dirac1,(*Dirac1 \:662f\:5355\:4f4d\:77e9\:9635*)
LDot[DiracG,k],DiracG5,
#]]&/@{
Projector["F1",\[Mu]][{p1,mE},{p2,mE}],
Projector["F2",\[Mu]][{p1,mE},{p2,mE}]
}/.onShell,
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:6807\:91cf\:90e8\:5206*)
{
(* --------- \:6b63\:89c4\:5b50 --------- *)
reg[p2-p1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
(* --------- \:666e\:901a\:4f20\:64ad\:5b50 --------- *)
prp1[k,mm1],
prp1[p2-k,mo2],
prp1[p1-k,mo1]
}/.intgd->Sequence,
(* \:8d39\:66fc\:79ef\:5206\:7684 Tag *)
tag
]];


(* ::Section::Closed:: *)
(*RB,oct,F2,*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{end/2,-end/8},{end/2,0}}],*)
(*Text["v1",{end/4,-4delta}],Text["v2",{end/2,4delta}],Text["v3",{3/4end,-4delta}]*)
(*},ImageSize->Small]*)


(* \:6309\:7167 package-X\:7684\:7ea6\:5b9a, \:4e0d\:5199 1/(2\[Pi])^4 *)
fyTag={"RB","oct","F2"};
fyAmp[fyTag,{"intg","eid"}]=With[{tag=fyTag},
paraLintSubmit[
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:65cb\:91cf\:90e8\:5206*)
Contract[
(I LTensor[p2-p1,\[Nu]])/(2mE)*Spur[
LDot[DiracG,k],DiracG5,
LDot[DiracG,(p2-k)]+mo2*Dirac1, LTensor[DiracS,\[Mu],\[Nu]], 
LDot[DiracG,(p1-k)]+mo1*Dirac1,(* Dirac1 \:662f\:5355\:4f4d\:77e9\:9635*)
LDot[DiracG,k],DiracG5,
#]]&/@{
Projector["F1",\[Mu]][{p1,mE},{p2,mE}],
Projector["F2",\[Mu]][{p1,mE},{p2,mE}]
}/.onShell,
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:6807\:91cf\:90e8\:5206*)
{
(* --------- \:6b63\:89c4\:5b50 --------- *)
reg[p2-p1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
(* --------- \:666e\:901a\:4f20\:64ad\:5b50 --------- *)
prp1[k,mm1],
prp1[p2-k,mo2],
prp1[p1-k,mo1]
}/.intgd->Sequence,
(* \:8d39\:66fc\:79ef\:5206\:7684 Tag *)
tag
]];


(* ::Section::Closed:: *)
(*tadpole,oct,o2*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Arrow@BezierCurve[{*)
(*{end/2,0},{0,end/2},{end,end/2},{end/2,0}*)
(*}],*)
(*Line[{{end/2,-end/6},{end/2,0}}],*)
(*Text["v1",{end/2,+5delta}]*)
(*},ImageSize->Small]*)


(* \:6309\:7167 package-X\:7684\:7ea6\:5b9a, \:4e0d\:5199 1/(2\[Pi])^4 *)
fyTag={"tad","oct","o2"};
fyAmp[fyTag,{"intg","eid"}]=With[{tag=fyTag},
paraLintSubmit[
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:65cb\:91cf\:90e8\:5206*)
Contract[
Spur[
2 LTensor[DiracG,\[Mu]],(*\[Gamma]^\[Mu] \:77e9\:9635*)
#]]&/@{
Projector["F1",\[Mu]][{p1,mE},{p2,mE}],
Projector["F2",\[Mu]][{p1,mE},{p2,mE}]
}/.onShell,
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:6807\:91cf\:90e8\:5206*)
{
(* --------- \:6b63\:89c4\:5b50 --------- *)
reg[p2-p1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
(* --------- \:666e\:901a\:4f20\:64ad\:5b50 --------- *)
prp1[k,mm1]
}/.intgd->Sequence,
(* \:8d39\:66fc\:79ef\:5206\:7684 Tag *)
tag
]];


(* ::Section::Closed:: *)
(*tadpole,oct,add,o2*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Arrow@BezierCurve[{*)
(*{end/2,0},{0,end/2},{end,end/2},{end/2,0}*)
(*}],*)
(*Line[{{end/2,-end/6},{end/2,0}}],*)
(*Text["v1",{end/2,+5delta}]*)
(*},ImageSize->Small]*)


(* \:6309\:7167 package-X\:7684\:7ea6\:5b9a, \:4e0d\:5199 1/(2\[Pi])^4 *)
fyTag={"tad","oct","add","o2"};
fyAmp[fyTag,{"intg","eid"}]=With[{tag=fyTag},
paraLintSubmit[
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:65cb\:91cf\:90e8\:5206*)
Contract[
LTensor[2k+(p2-p1),\[Mu]]*Spur[
4 LDot[DiracG,k],(*\:79ef\:5206\:5728 k\[Rule]-k \:53d8\:6362\:4e0b\:ff0c\:6d88\:53bb\:5947\:51fd\:6570\:7684\:90e8\:5206,*)
#]]&/@{
Projector["F1",\[Mu]][{p1,mE},{p2,mE}],
Projector["F2",\[Mu]][{p1,mE},{p2,mE}]
}/.onShell,
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:6807\:91cf\:90e8\:5206*)
{
(* --------- \:6b63\:89c4\:5b50 --------- *)
reg[p2-p1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
cltcom[{k,p2-p1},{mm1,\[CapitalLambda]}],
(* --------- \:666e\:901a\:4f20\:64ad\:5b50 --------- *)
prp1[k,mm1]
}/.intgd->Sequence,
(* \:8d39\:66fc\:79ef\:5206\:7684 Tag *)
tag
]];


(* ::Section::Closed:: *)
(*tadpole,oct,mag,o2*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Arrow@BezierCurve[{*)
(*{end/2,0},{0,end/2},{end,end/2},{end/2,0}*)
(*}],*)
(*Line[{{end/2,-end/6},{end/2,0}}],*)
(*Rectangle[{1/2end-3/2delta,-3/2delta},{1/2end+3/2delta,+3/2delta}],*)
(*Text["v1",{end/2,+5delta}]*)
(*},ImageSize->Small]*)


(* \:6309\:7167 package-X\:7684\:7ea6\:5b9a, \:4e0d\:5199 1/(2\[Pi])^4 *)
fyTag={"tad","oct","mag","o2"};
fyAmp[fyTag,{"intg","eid"}]=With[{tag=fyTag},
paraLintSubmit[
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:65cb\:91cf\:90e8\:5206*)
Contract[
(I LTensor[p2-p1,\[Nu]])/(2mE) *Spur[
LTensor[DiracS,\[Mu],\[Nu]],
#]]&/@{
Projector["F1",\[Mu]][{p1,mE},{p2,mE}],
Projector["F2",\[Mu]][{p1,mE},{p2,mE}]
}/.onShell,
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:6807\:91cf\:90e8\:5206*)
{
(* --------- \:6b63\:89c4\:5b50 --------- *)
reg[p2-p1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
(* --------- \:666e\:901a\:4f20\:64ad\:5b50 --------- *)
prp1[k,mm1]
}/.intgd->Sequence,
(* \:8d39\:66fc\:79ef\:5206\:7684 Tag *)
tag
]];


(* ::Section::Closed:: *)
(*bubble,mes,o2*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Arrow@BezierCurve[{*)
(*{end/2,0},{0,end/2},{end,end/2},{end/2,0}*)
(*}],Line[{{end/2,3/8*end},{end/2,end/2}}],*)
(*Text["v1",{end/2,-4delta}],Text["v2",{end/2,end/3-delta}]*)
(*},*)
(*ImageSize->Small]*)


(* \:6309\:7167 package-X\:7684\:7ea6\:5b9a, \:4e0d\:5199 1/(2\[Pi])^4 *)
fyTag={"bub","mes","o2"};
fyAmp[fyTag,{"intg","eid"}]=With[{tag=fyTag},
paraLintSubmit[
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:65cb\:91cf\:90e8\:5206*)
Contract[
LTensor[2k+p2-p1,\[Mu]]*Spur[
LDot[DiracG,(2k+p2-p1)],
#]]&/@{
Projector["F1",\[Mu]][{p1,mE},{p2,mE}],
Projector["F2",\[Mu]][{p1,mE},{p2,mE}]
}/.onShell,
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:6807\:91cf\:90e8\:5206*)
{
(* --------- \:6b63\:89c4\:5b50 --------- *)
reg[p2-p1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
reg[k+p2-p1,mm1,\[CapitalLambda]],
(* --------- \:666e\:901a\:4f20\:64ad\:5b50 --------- *)
prp1[k,mm1],
prp1[k+p2-p1,mm1]
}/.intgd->Sequence,
(* \:8d39\:66fc\:79ef\:5206\:7684 Tag *)
tag
]];


(* ::Section::Closed:: *)
(*bubble,mes,ten,o2*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Arrow@BezierCurve[{*)
(*{end/2,0},{0,end/2},{end,end/2},{end/2,0}*)
(*}],Line[{{end/2,3/8*end},{end/2,end/2}}],*)
(*Text["v1",{end/2,-4delta}],Text["v2",{end/2,end/3-delta}]*)
(*},*)
(*ImageSize->Small]*)


(* \:6309\:7167 package-X\:7684\:7ea6\:5b9a, \:4e0d\:5199 1/(2\[Pi])^4 *)
fyTag={"bub","mes","ten","o2"};
fyAmp[fyTag,{"intg","eid"}]=With[{tag=fyTag},
paraLintSubmit[
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:65cb\:91cf\:90e8\:5206*)
Contract[
LTensor[p2-p1,\[Rho]]*LTensor[k,\[Nu]]*LTensor[2k+p2-p1,\[Mu]]*Spur[
LTensor[DiracS,\[Rho],\[Nu]],
#]]&/@{
Projector["F1",\[Mu]][{p1,mE},{p2,mE}],
Projector["F2",\[Mu]][{p1,mE},{p2,mE}]
}/.onShell,
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:6807\:91cf\:90e8\:5206*)
{
(* --------- \:6b63\:89c4\:5b50 --------- *)
reg[p2-p1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
reg[k+p2-p1,mm1,\[CapitalLambda]],
(* --------- \:666e\:901a\:4f20\:64ad\:5b50 --------- *)
prp1[k,mm1],
prp1[k+p2-p1,mm1]
}/.intgd->Sequence,
(* \:8d39\:66fc\:79ef\:5206\:7684 Tag *)
tag
]];


(* ::Chapter:: *)
(*loop Integral: decuplet*)


(* ::Section::Closed:: *)
(*RB,mes,dec*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Line[{{end/4,delta},{3/4end,delta}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{end/2,end/4},{end/2,end/2.4}}],*)
(*Text["v1",{end/4,-4delta}],Text["v2",{end/2,end/4-4delta}],Text["v3",{3/4end,-4delta}]*)
(*},ImageSize->Small]*)


(* \:6309\:7167 package-X\:7684\:7ea6\:5b9a, \:4e0d\:5199 1/(2\[Pi])^4 *)
fyTag={"RB","mes","dec"};
fyAmp[fyTag,{"intg","eid"}]=With[{tag=fyTag},
paraLintSubmit[
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:65cb\:91cf\:90e8\:5206*)
Contract[
LTensor[k+p2-p1,\[Alpha]]*LTensor[k,\[Nu]]*LTensor[2k+p2-p1,\[Mu]]*Spur[
\[CapitalTheta][\[Alpha],\[Beta]],spDec[{\[Beta],\[Rho]},{p1-k,md1}],\[CapitalTheta][\[Rho],\[Nu]],
#]]&/@{
Projector["F1",\[Mu]][{p1,mE},{p2,mE}],
Projector["F2",\[Mu]][{p1,mE},{p2,mE}]
}/.onShell,
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:6807\:91cf\:90e8\:5206*)
{
(* --------- \:6b63\:89c4\:5b50 --------- *)
reg[p2-p1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
reg[p2-p1+k,mm1,\[CapitalLambda]],
(* --------- \:666e\:901a\:4f20\:64ad\:5b50 --------- *)
prp1[k+p2-p1,mm1],
prp1[k,mm1],
prp1[p1-k,md1]
}/.intgd->Sequence,
(* \:8d39\:66fc\:79ef\:5206\:7684 Tag *)
tag
]];


(* ::Section::Closed:: *)
(*RB,dec,F1*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{end/2,-end/8},{end/2,0}}],*)
(*Line[{{end/4,delta},{3/4end,delta}}],*)
(*Text["v1",{end/4,-4delta}],Text["v2",{end/2,4delta}],Text["v3",{3/4end,-4delta}]*)
(*},ImageSize->Small]*)


(* \:6309\:7167 package-X\:7684\:7ea6\:5b9a, \:4e0d\:5199 1/(2\[Pi])^4 *)
fyTag={"RB","dec","F1"};
fyAmp[fyTag,{"intg","eid"}]=With[{tag=fyTag},
paraLintSubmit[
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:65cb\:91cf\:90e8\:5206*)
Contract[
LTensor[k,\[Alpha]]*LTensor[k,\[Eta]]*Spur[
\[CapitalTheta][\[Alpha],\[Beta]],spDec[{\[Beta],\[Theta]},{p2-k,md1}],
dgam3[\[Theta],\[Nu],\[Mu]],
spDec[{\[Nu],\[Rho]},{p1-k,md1}],\[CapitalTheta][\[Rho],\[Eta]],
#]]&/@{
Projector["F1",\[Mu]][{p1,mE},{p2,mE}],
Projector["F2",\[Mu]][{p1,mE},{p2,mE}]
}/.onShell,
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:6807\:91cf\:90e8\:5206*)
{
(* --------- \:6b63\:89c4\:5b50 --------- *)
reg[p2-p1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
(* --------- \:666e\:901a\:4f20\:64ad\:5b50 --------- *)
prp1[k,mm1],
prp1[p2-k,md1],
prp1[p1-k,md1]
}/.intgd->Sequence,
(* \:8d39\:66fc\:79ef\:5206\:7684 Tag *)
tag
]];


(* ::Section::Closed:: *)
(*RB,dec,F2*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Rectangle[{1/2end-3/2delta,-3/2delta},{1/2end+3/2delta,3/2delta}],*)
(*Line[{{end/2,-end/8},{end/2,0}}],*)
(*Line[{{end/4,delta},{3/4end,delta}}],*)
(*Text["v1",{end/4,-4delta}],Text["v2",{end/2,4delta}],Text["v3",{3/4end,-4delta}]*)
(*},ImageSize->Small]*)


(* \:6309\:7167 package-X\:7684\:7ea6\:5b9a, \:4e0d\:5199 1/(2\[Pi])^4 *)
fyTag={"RB","dec","F2"};
fyAmp[fyTag,{"intg","eid"}]=With[{tag=fyTag},
paraLintSubmit[
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:65cb\:91cf\:90e8\:5206*)
Contract[
LTensor[k,\[Alpha]]*LTensor[k,\[Eta]]*(I LTensor[p2-p1,\[Nu]])/(2md1)*Spur[
\[CapitalTheta][\[Alpha],\[Beta]],spDec[{\[Beta],\[Theta]},{p2-k,md1}],
LTensor[DiracS,\[Mu],\[Nu]],
spDec[{\[Theta],\[Rho]},{p1-k,md1}],\[CapitalTheta][\[Rho],\[Eta]],
#]]&/@{
Projector["F1",\[Mu]][{p1,mE},{p2,mE}],
Projector["F2",\[Mu]][{p1,mE},{p2,mE}]
}/.onShell,
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:6807\:91cf\:90e8\:5206*)
{
(* --------- \:6b63\:89c4\:5b50 --------- *)
reg[p2-p1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
(* --------- \:666e\:901a\:4f20\:64ad\:5b50 --------- *)
prp1[k,mm1],
prp1[p2-k,md1],
prp1[p1-k,md1]
}/.intgd->Sequence,
(* \:8d39\:66fc\:79ef\:5206\:7684 Tag *)
tag
]];


(* ::Section::Closed:: *)
(*RB,trans,left*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Rectangle[{1/2end-3/2delta,-3/2delta},{1/2end+3/2delta,3/2delta}],*)
(*Line[{{end/2,-end/8},{end/2,0}}],*)
(*Line[{{end/4,delta},{2/4end,delta}}],*)
(*Text["v1",{end/4,-4delta}],Text["v2",{end/2,4delta}],Text["v3",{3/4end,-4delta}]*)
(*},ImageSize->Small]*)


(* \:6309\:7167 package-X\:7684\:7ea6\:5b9a, \:4e0d\:5199 1/(2\[Pi])^4 *)
fyTag={"RB","trans","left"};
fyAmp[fyTag,{"intg","eid"}]=With[{tag=fyTag},
paraLintSubmit[
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:65cb\:91cf\:90e8\:5206*)
Contract[
LTensor[k,\[Alpha]]*LTensor[p2-p1,\[Nu]]*Spur[
LDot[DiracG,k],DiracG5,
LDot[DiracG,(p2-k)]+mo1*Dirac1,
DiracMatrix[LTensor[DiracG,\[Nu]],DiracG5,spDec[{\[Mu],\[Beta]},{p1-k,md1}]]-
DiracMatrix[LTensor[DiracG,\[Mu]],DiracG5,spDec[{\[Nu],\[Beta]},{p1-k,md1}]],
\[CapitalTheta][\[Beta],\[Alpha]],
#]]&/@{
Projector["F1",\[Mu]][{p1,mE},{p2,mE}],
Projector["F2",\[Mu]][{p1,mE},{p2,mE}]
}/.onShell,
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:6807\:91cf\:90e8\:5206*)
{
(* --------- \:6b63\:89c4\:5b50 --------- *)
reg[p2-p1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
(* --------- \:666e\:901a\:4f20\:64ad\:5b50 --------- *)
prp1[k,mm1],
prp1[p2-k,mo1],
prp1[p1-k,md1]
}/.intgd->Sequence,
(* \:8d39\:66fc\:79ef\:5206\:7684 Tag *)
tag
]];


(* ::Section::Closed:: *)
(*RB,trans,right*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Rectangle[{1/2end-3/2delta,-3/2delta},{1/2end+3/2delta,3/2delta}],*)
(*Line[{{end/2,-end/8},{end/2,0}}],*)
(*Line[{{2/4end,delta},{3/4end,delta}}],*)
(*Text["v1",{end/4,-4delta}],Text["v2",{end/2,4delta}],Text["v3",{3/4end,-4delta}]*)
(*},ImageSize->Small]*)


(* \:6309\:7167 package-X\:7684\:7ea6\:5b9a, \:4e0d\:5199 1/(2\[Pi])^4 *)
fyTag={"RB","trans","right"};
fyAmp[fyTag,{"intg","eid"}]=With[{tag=fyTag},
paraLintSubmit[
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:65cb\:91cf\:90e8\:5206*)
Contract[
LTensor[k,\[Alpha]]*LTensor[p2-p1,\[Nu]]*Spur[
\[CapitalTheta][\[Alpha],\[Beta]],
DiracMatrix[spDec[{\[Beta],\[Nu]},{p2-k,md1}],LTensor[DiracG,\[Mu]],DiracG5]-
DiracMatrix[spDec[{\[Beta],\[Mu]},{p2-k,md1}],LTensor[DiracG,\[Nu]],DiracG5],
LDot[DiracG,(p1-k)]+mo1*Dirac1,
LDot[DiracG,k],DiracG5,
#]]&/@{
Projector["F1",\[Mu]][{p1,mE},{p2,mE}],
Projector["F2",\[Mu]][{p1,mE},{p2,mE}]
}/.onShell,
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:6807\:91cf\:90e8\:5206*)
{
(* --------- \:6b63\:89c4\:5b50 --------- *)
reg[p2-p1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
(* --------- \:666e\:901a\:4f20\:64ad\:5b50 --------- *)
prp1[k,mm1],
prp1[p2-k,md1],
prp1[p1-k,mo1]
}/.intgd->Sequence,
(* \:8d39\:66fc\:79ef\:5206\:7684 Tag *)
tag
]];


(* ::Section::Closed:: *)
(*KR,mes,dec,left*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{end/4,0},{end/4,-end/5}}],*)
(*Line[{{end/4,1.5delta},{3/4end,1.5delta}}],*)
(*Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end,-4delta}]*)
(*},ImageSize->Small]*)


(* \:6309\:7167 package-X\:7684\:7ea6\:5b9a, \:4e0d\:5199 1/(2\[Pi])^4 *)
fyTag={"KR","mes","dec","left"};
fyAmp[fyTag,{"intg","eid"}]=With[{tag=fyTag},
paraLintSubmit[
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:65cb\:91cf\:90e8\:5206*)
Contract[
LTensor[k,\[Rho]]*Spur[
\[CapitalTheta][\[Rho],\[Beta]],
spDec[{\[Beta],\[Nu]},{p2-k,md1}],
\[CapitalTheta][\[Nu],\[Mu]],
#]]&/@{
Projector["F1",\[Mu]][{p1,mE},{p2,mE}],
Projector["F2",\[Mu]][{p1,mE},{p2,mE}]
}/.onShell,
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:6807\:91cf\:90e8\:5206*)
{
(* --------- \:6b63\:89c4\:5b50 --------- *)
reg[p2-p1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
(* --------- \:666e\:901a\:4f20\:64ad\:5b50 --------- *)
prp1[k,mm1],
prp1[p2-k,md1]
}/.intgd->Sequence,
(* \:8d39\:66fc\:79ef\:5206\:7684 Tag *)
tag
]];


(* ::Section::Closed:: *)
(*KR,mes,dec,right*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{3/4end,0},{3/4end,-end/5}}],*)
(*Line[{{end/4,1.5delta},{3/4end,1.5delta}}],*)
(*Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end+4delta,+4delta}]*)
(*},ImageSize->Small]*)


(* \:6309\:7167 package-X\:7684\:7ea6\:5b9a, \:4e0d\:5199 1/(2\[Pi])^4 *)
fyTag={"KR","mes","dec","right"};
fyAmp[fyTag,{"intg","eid"}]=With[{tag=fyTag},
paraLintSubmit[
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:65cb\:91cf\:90e8\:5206*)
Contract[
LTensor[k,\[Beta]]*Spur[
\[CapitalTheta][\[Mu],\[Nu]],
spDec[{\[Nu],\[Rho]},{p1-k,md1}],
\[CapitalTheta][\[Rho],\[Beta]],
#]]&/@{
Projector["F1",\[Mu]][{p1,mE},{p2,mE}],
Projector["F2",\[Mu]][{p1,mE},{p2,mE}]
}/.onShell,
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:6807\:91cf\:90e8\:5206*)
{
(* --------- \:6b63\:89c4\:5b50 --------- *)
reg[p2-p1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
(* --------- \:666e\:901a\:4f20\:64ad\:5b50 --------- *)
prp1[k,mm1],
prp1[p1-k,md1]
}/.intgd->Sequence,
(* \:8d39\:66fc\:79ef\:5206\:7684 Tag *)
tag
]];


(* ::Section::Closed:: *)
(*KR,mes,dec,add,left*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Disk[{1/4end,0},2delta],*)
(*Line[{{end/4,0},{end/4,-end/5}}],*)
(*Line[{{end/4,1.5delta},{3/4end,1.5delta}}],*)
(*Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end,-4delta}]*)
(*},ImageSize->Small]*)


(* \:6309\:7167 package-X\:7684\:7ea6\:5b9a, \:4e0d\:5199 1/(2\[Pi])^4 *)
fyTag={"KR","mes","dec","add","left"};
fyAmp[fyTag,{"intg","eid"}]=With[{tag=fyTag},
paraLintSubmit[
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:65cb\:91cf\:90e8\:5206*)
Contract[
(* -R^\[Mu](-k,q) \:7684 -(-2k+q)^\[Mu] \:8003\:8651\:5728\:8fd9\:91cc *)
LTensor[k,\[Alpha]]*LTensor[k-(p2-p1),\[Rho]]*(-1)LTensor[-2k+(p2-p1),\[Mu]]*Spur[
\[CapitalTheta][\[Alpha],\[Beta]],
spDec[{\[Beta],\[Nu]},{p2-k,md1}],
\[CapitalTheta][\[Nu],\[Rho]],
#]]&/@{
Projector["F1",\[Mu]][{p1,mE},{p2,mE}],
Projector["F2",\[Mu]][{p1,mE},{p2,mE}]
}/.onShell,
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:6807\:91cf\:90e8\:5206*)
{
(* --------- \:6b63\:89c4\:5b50 --------- *)
reg[p2-p1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
cltcom[{-k,p2-p1},{mm1,\[CapitalLambda]}],
(* --------- \:666e\:901a\:4f20\:64ad\:5b50 --------- *)
prp1[k,mm1],
prp1[p2-k,md1]
}/.intgd->Sequence,
(* \:8d39\:66fc\:79ef\:5206\:7684 Tag *)
tag
]];


(* ::Section:: *)
(*KR,mes,dec,add,right*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Disk[{3/4end,0},2delta],*)
(*Line[{{3/4end,0},{3/4end,-end/5}}],*)
(*Line[{{end/4,1.5delta},{3/4end,1.5delta}}],*)
(*Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end+4delta,+4delta}]*)
(*},ImageSize->Small]*)


(* \:6309\:7167 package-X\:7684\:7ea6\:5b9a, \:4e0d\:5199 1/(2\[Pi])^4 *)
fyTag={"KR","mes","dec","add","right"};
fyAmp[fyTag,{"intg","eid"}]=With[{tag=fyTag},
paraLintSubmit[
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:65cb\:91cf\:90e8\:5206*)
Contract[
(* R^\[Mu](k,q) \:7684 (2k+q)^\[Mu] \:8003\:8651\:5728\:8fd9\:91cc *)
LTensor[k+(p2-p1),\[Rho]]*LTensor[k,\[Beta]]*LTensor[2k+(p2-p1),\[Mu]]*Spur[
\[CapitalTheta][\[Rho],\[Nu]],
spDec[{\[Nu],\[Alpha]},{p1-k,md1}],
\[CapitalTheta][\[Alpha],\[Beta]],
#]]&/@{
Projector["F1",\[Mu]][{p1,mE},{p2,mE}],
Projector["F2",\[Mu]][{p1,mE},{p2,mE}]
}/.onShell,
(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:6807\:91cf\:90e8\:5206*)
{
(* --------- \:6b63\:89c4\:5b50 --------- *)
reg[p2-p1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
cltcom[{k,p2-p1},{mm1,\[CapitalLambda]}],
(* --------- \:666e\:901a\:4f20\:64ad\:5b50 --------- *)
prp1[k,mm1],
prp1[p1-k,md1]
}/.intgd->Sequence,
(* \:8d39\:66fc\:79ef\:5206\:7684 Tag *)
tag
]];


(* ::Chapter:: *)
(*LoopIntegrate parallel*)


(* Set \:5177\:6709 HoldFirst \:5c5e\:6027*)
fyAmpLst=WaitAll[fyAmp[#,{"intg","eid"}]&/@fyAmpTagLst];
