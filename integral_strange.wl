(* ::Package:: *)

(* ::Title:: *)
(*integral_strange.wl*)


(* ::Chapter:: *)
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
ResetDirectory[];,
Throw["I cann't find any init.wl in this project"]
];
recurFind[start];
]


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
delayedNumerator := Contract[Spur[LTensor[\[Gamma], \[Rho]], (p2.\[Gamma] - k.\[Gamma] + m \[DoubleStruckOne]), LTensor[\[Gamma], \[Mu]], (p1.\[Gamma] - k.\[Gamma] + m \[DoubleStruckOne]), LTensor[\[Gamma], \[Nu]], Projector["F2", \[Mu]][{p1, m}, {p2, m}]] LTensor[\[DoubleStruckG], \[Rho], \[Nu]]];
LoopIntegrate[delayedNumerator, k, {k - p2, m}, {k - p1, m}, {k, 0}] /. {p1.p1 -> m^2, p2.p2 -> m^2, p1.p2 -> -q.q/2 + m^2}
*)


(* ::Section:: *)
(*Package-X*)


(* ::Input:: *)
(*SetOptions[EvaluationNotebook[],CommonDefaultFormatTypes->{"Output"->StandardForm}](*\:8bbe\:7f6e\:663e\:5f0f\:683c\:5f0f\:4e3a\:6807\:51c6\:683c\:5f0f*)*)


If[NameQ["\[Sigma]"],echo["please remove the definitions of \[Sigma], \[Sigma] will be used in package-X"];Remove["Global`\[Sigma]"]];(* \[Sigma] \:662f package-X \:7684\:4fdd\:7559\:6807\:8bc6\:7b26,\:9700\:8981\:6e05\:9664*)
<<X`;(*\:5bfc\:5165 Package-X *) 
SetOptions[Simplify,TimeConstraint->2];(*\:8bbe\:7f6e\:5316\:7b80\:65f6\:95f4\:9650\:5236*)
SetOptions[Refine,TimeConstraint->2];
(*\:5bfc\:5165\:4e00\:4e9b\:683c\:5f0f\:5316\:7684\:8bbe\:7f6e\:ff0c\:663e\:793a\:573a\:7684\:5e38\:7528\:5f62\:5f0f*)
Get[FileNameJoin[{gitLocalName,"gen_format.wl"}]];


reg::usage="\:6b63\:89c4\:5b50 F[k]=(\[CapitalLambda]^2-m\[Phi]^2)^2/(k^2-\[CapitalLambda]^2+I*\[CurlyEpsilon])^2, \:5176\:4e2d m\[Phi] \:662f\:4ecb\:5b50\:8d28\:91cf\:ff0c\:5bf9\:4e8e\:5149\:5b50,\:6b64\:8d28\:91cf\:4e3a\:96f6\:3002
\:6b63\:89c4\:5b50\:5f52\:4e00\:5316\:5230 F[m\[Phi]]=1. \:751f\:6210\:5217\:8868\:ff0c\:7b2c\:4e00\:9879\:662f\:5206\:5b50\:ff0c\:7b2c\:4e8c\:9879\:662f\:5206\:6bcd";
reg[q_,\[CapitalLambda]_]:=intgd[num[\[CapitalLambda]^4],prp[{q,\[CapitalLambda],2}]](* \:5bf9\:4e8e\:5149\:5b50\:ff0c\:8d28\:91cf\:4e3a\:96f6 *)
reg[k_,m\[Phi]_,\[CapitalLambda]_]:=intgd[num[(\[CapitalLambda]^2-m\[Phi]^2)^2],prp[{k,\[CapitalLambda],2}]] (*\:5bf9\:4e8e\:4ecb\:5b50\:ff0c\:9700\:8981\:63d0\:4f9b\:8d28\:91cf*)
(* +++++++++++++++++++++++++++++++++++ *)
prp1::usage="\:5206\:5b50\:4e3a1\:7684\:666e\:901a\:4f20\:64ad\:5b50";
prp1[k_,m_]:=intgd[num[1],prp[{k,m}]]


dprop::usage="\:5341\:91cd\:6001\:91cd\:5b50\:4f20\:64ad\:5b50";
dprop[p_,m_,\[Mu]_,\[Nu]_]:=(-LTensor[MetricG,\[Mu],\[Nu]]*Dirac1
+DiracMatrix[LTensor[DiracG, \[Mu]],LTensor[DiracG, \[Nu]]]/3+
(2 LTensor[p, \[Mu]]*LTensor[p, \[Nu]]*Dirac1)/(3 m^2)+
(LTensor[DiracG, \[Mu]]*LTensor[p, \[Nu]]-LTensor[DiracG,\[Nu]]*LTensor[p, \[Mu]])/(3 m)
)


cltcom::usage="\:6b63\:89c4\:5b50\:7684\:7ec4\:5408";
correlatecom[q_,k_,\[CapitalLambda]_]:={
{k,\[CapitalLambda]},
{k,\[CapitalLambda]},
{q+k,\[CapitalLambda]},
{q+k,\[CapitalLambda]}
}
(* \[CapitalLambda]^4 (2 \[CapitalLambda]^2-SP[k]-SP[-k+q]) 
 \[CapitalLambda]^4 (2 \[CapitalLambda]^2-2k . k-2k . (p2-p1)-2mN^2+2p1 . p2) 
factor for every correlatecom .*)


gam3::usage="\:5341\:91cd\:6001F1\:9876\:89d2\:4f3d\:9a6c\:77e9\:9635";
gam3[\[Mu]_,\[Nu]_,\[Rho]_]:=(-I)/2 (
DiracMatrix[LTensor[DiracS,\[Mu],\[Nu]],LTensor[DiracG, \[Rho]]]+
DiracMatrix[LTensor[DiracG,\[Rho]],LTensor[DiracS,\[Mu],\[Nu]]]
)


(* ::Section:: *)
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


(* \:58f0\:660e\:8fd9\:4e9b\:5e38\:91cf\:662f\:6d1b\:4f26\:5179\:6807\:91cf lorentz scalars *)
LScalarQ[\[CapitalLambda]]=True;LScalarQ[mE]=True;
LScalarQ[mm1]=True;LScalarQ[mm2]=True;
LScalarQ[mo1]=True;LScalarQ[mo2]=True;
LScalarQ[md1]=True;LScalarQ[md2]=True;
LScalarQ[Q2]=True;


(* \:521d\:672b\:6001,\:8fd0\:52a8\:5b66\:5173\:7cfb*)
onShell={p1 . p1->mE^2,p2 . p2->mE^2,p1 . p2->Q2/2+mE^2};


(* ::Section:: *)
(*storage name*)


SetDirectory[
FileNameJoin[{
ParentDirectory[NotebookDirectory[]],
"analytic-storage.strange.series-o2"
}]
]


intdirectory=FileNameJoin[{
ParentDirectory[NotebookDirectory[]],
"integral-storage.strange"
}]


fuintf1nm=Table["0",{diord,1,11,1}];
fuintf2nm=Table["0",{diord,1,11,1}];
fuff1nm=Table["0",{diord,1,11,1}];
fuff2nm=Table["0",{diord,1,11,1}];


(* ::Text:: *)
(*Get int expressions*)


Table[
fuintf1nm[[diord]]=FileNameJoin[{intdirectory,"f1."<>"simplify."<>ToString[diord]<>".m"}];
fuintf2nm[[diord]]=FileNameJoin[{intdirectory,"f2."<>"simplify."<>ToString[diord]<>".m"}];
,{diord,1,11,1}];


(* ::Text:: *)
(*analytic names*)


Table[
fuff1nm[[diord]]=FileNameJoin[{Directory[],"f1."<>"analytic."<>ToString[diord]<>".m"}];
fuff2nm[[diord]]=FileNameJoin[{Directory[],"f2."<>"analytic."<>ToString[diord]<>".m"}];
,{diord,1,11,1}];


(* ::Chapter:: *)
(*loop Integral*)


(* ::Section:: *)
(*RB,mes,oct*)


(* \:6309\:7167 package-X\:7684\:7ea6\:5b9a, \:4e0d\:5199 1/(2\[Pi])^4 *)
fyTag={"RB","mes","oct"};
fyAmp[fyTag,"spin"]=Contract[
(LTensor[2k+p2-p1,\[Mu]])*Spur[
(k+p2-p1) . \[Gamma],\[Gamma]5,\[Gamma] . (p1-k)+mo1 \[DoubleStruckOne],\[Gamma] . k,\[Gamma]5,
#1]]&/@{
Projector["F1",\[Mu]][{p1,mE},{p2,mE}],
Projector["F2",\[Mu]][{p1,mE},{p2,mE}]
}/.onShell;


fyAmp[fyTag,"scal"]={
reg[p2-p1+k,mm1,\[CapitalLambda]],
reg[k,mm1,\[CapitalLambda]],
reg[p2-p1,\[CapitalLambda]],
prp1[k+p2-p1,mm1],
prp1[k,mm1],
prp1[p1-k,mo1]
}/.intgd->Sequence;(*\:5708\:79ef\:5206\:7684\:88ab\:79ef\:5f0f\:7684\:6807\:91cf\:90e8\:5206*)
(* ---------------------------------------------------------------------------- *)
fyAmp[fyTag,"num"]=Times@@Cases[fyAmp[fyTag,"scal"],num[x_]:>x]*fyAmp[fyTag,"spin"];(*\:751f\:6210\:5206\:5b50,F1F2*)
fyAmp[fyTag,"prp"]=Cases[fyAmp[fyTag,"scal"],prp[x_]:>x];(*\:751f\:6210\:5206\:6bcd,\:4e5f\:5c31\:662f\:4f20\:64ad\:5b50*)
(* ----------------- \:5c06\:5708\:79ef\:5206,\:5206\:89e3\:5230\:6807\:51c6\:57fa Passarino-Veltman \:51fd\:6570 -------------------------------- *)
fyAmp[fyTag,"intg"]=LoopIntegrate[
fyAmp[fyTag,"num"],k,Sequence@@fyAmp[fyTag,"prp"],
Cancel->Automatic,Apart->True
]/.onShell;//AbsoluteTiming


(* ::Section:: *)
(*LoopRefineSeries*)


(* ::Input:: *)
(*fuaff1=LoopRefineSeries[fuaintf1,{Q2,0,2},Organization->Function];//AbsoluteTiming*)


(* ::Input:: *)
(*fuaff2=LoopRefineSeries[fuaintf2,{Q2,0,2},Organization->Function];//AbsoluteTiming*)


(* ::Input:: *)
(*fuaff1//Dimensions*)
(*fuaff2//Dimensions*)
(*fuaff1//Head*)
(*fuaff2//Head*)


(* ::Input:: *)
(*Put[fuaff1,fuff1nm[[1]]];*)
(*Put[fuaff2,fuff2nm[[1]]];*)
