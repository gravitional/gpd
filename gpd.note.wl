(* ::Package:: *)

(* ::Title:: *)
(*gpd_notation.nb*)


(* ::Chapter:: *)
(*backup 360b29f*)


(* ::Input:: *)
(*(*\:666e\:901a\:6570\:5b57\:7684\:4e8c\:6b21\:591a\:9879\:5f0f\:914d\:9f50-----------------*)*)
(*toSquare[expr_,k_,l_]:=Module[*)
(*{a=Coefficient[expr,k,2],*)
(*b=Coefficient[expr,k,1],*)
(*c=Coefficient[expr,k,0]*)
(*},*)
(*(*\:52a8\:91cf\:914d\:9f50\:9700\:8981\:6ee1\:8db3\:7684\:5173\:7cfb---------*)*)
(*{*)
(*l->k+b/(2a),*)
(*k->l-b/(2a),*)
(*-l^2->b^2/(4a)-c*)
(*}*)
(*]*)


(* ::Chapter:: *)
(*note 369e9bd*)


(* ::Text:: *)
(*\:91cd\:65b0\:8bbe\:5b9a quarkflow \:56fe\:7684\:7c92\:5ea6\:ff0c\:7531\:4e8e\:7b80\:5e76\:7684\:5b58\:5728\:ff0c\:9700\:8981\:7ed9 quarkflow \:52a0\:4e0a chpt \:5f3a\:5b50\:5c42\:6b21\:7684tag\:ff0c\:624d\:80fd\:8fdb\:884c\:533a\:5206\:ff0c*)
(*\:53e6\:4e00\:65b9\:9762\:ff0c\[Pi]0-\[Eta]8 \:7684 quarkflow \:662f\:7b80\:5e76\:7684\:ff0c\:5e94\:8be5\:5408\:5e76\:8003\:8651\:ff1a*)
(*\:5bf9\:4e8e \[CapitalSigma]0,\[CapitalLambda] \:4e4b\:5916\:7684 octet\:ff1a\[Pi]0,\[Eta]8 \:603b\:662f\:540c\:65f6\:8d21\:732e\:5230 quarkflow \:4e2d\:ff0c\:5bf9\:5e94\:7684\:4e2d\:95f4\:6001\:91cd\:5b50\:662f\:76f8\:540c\:7684\:3002\:6240\:4ee5\:603b\:662f\:8003\:8651\:5b83\:4eec\:7684\:548c\:5bf9 quarkflow \:7684\:8d21\:732e\:ff0c\:5982\:679c\:533a\:5206 \[Pi]0,\[Eta]8 \:4f1a\:4ea7\:751f\:591a\:4f59\:7684 quarkflow \:81ea\:7531\:5ea6\:3002*)
(*\:5bf9\:4e8e \[CapitalSigma]0,\[CapitalLambda]\:ff1a \[Pi]0,\[Eta]8 \:5bf9\:5e94\:7684\:4e2d\:95f4\:6001\:91cd\:5b50\:4e0d\:540c\:ff0c\:4e2d\:95f4\:901a\:8fc7\:63d2\:5165 \:5473\:9053\:6ce2\:51fd\:6570\:7cfb\:6570\:ff0c\:4eba\:4e3a\:5730\:628a \[Pi]0-\[Eta]8 \:7684\:8d21\:732e\:5206\:5f00\:3002\[Pi]0 \:53ea\:8d21\:732e\:5230 uubar, ddbar. \:800c \[Eta]8 \:8d21\:732e\:5230 uubar, ddbar, ssbar\:ff0c\:6240\:4ee5\:53ef\:4ee5\:901a\:8fc7 quarkflow \:6570\:76ee\:533a\:5206\:8fd9\:4e24\:79cd\:60c5\:51b5\:3002*)


(* ::Chapter:: *)
(*note 1a68c23*)


(* ::Text:: *)
(*\:53cd\:5e38\:78c1\:77e9\:9876\:70b9 F1,F2 \:4e2d, \:6d89\:53ca\:5230 \[CapitalSigma]-\[CapitalLambda] \:578b\:4e2d\:95f4\:6001\:9876\:70b9\:ff0c\:4e24\:8d39\:7c73\:5b50\:7ebf\:7684\:8d28\:91cf\:4e0d\:76f8\:7b49\:ff0c\:5982\:679c\:4ee3\:5165\:7cbe\:786e\:7684 mo1, mo2, \:8ba1\:7b97\:65f6\:95f4\:8f83\:957f\:3002*)
(*\:5982\:679c\:5c06\:7ed3\:679c\:6309\:7167 (mo2-mo1) \:7684\:7ea7\:6570\:5c55\:5f00\:ff0c\:53ef\:4ee5\:7b80\:5316\:8fd0\:7b97\:ff0c\:56e0\:4e3a\:6d89\:53ca\:5230\:7684\:8d28\:91cf\:662f*)


mo1->1.193, mo2->1.116, mo2-mo1->0.077


(* ::Text:: *)
(*\:7ecf\:8fc7\:6d4b\:8bd5\:ff0c\:8fd9\:4e2a\:5c55\:5f00\:5e26\:6765\:7684\:8bef\:5dee\:662f\:5f88\:5c0f\:7684\:3002*)


(* ::Chapter:: *)
(*note 3accbe8*)


(*\:4f55\:65b9\:6210 nucleon strange quark magnetic moment \:7684\:7ed3\:679c*)
TableForm[
{
{"\[CapitalLambda]==0.9","1 a","2 b","3 c","4 d+e","5 f+g","6 h","7 i" ,"8 j","9 k+l","10 m+n","11 o+p"},
{"fch",\[Minus]0.017,0.005,\[Minus]0.008,\[Minus]0.016,\[Minus]0.030,0.009,\[Minus]0.0004,0.009,\[Minus]0.012,0.001,0.018}
}
]


(* ::Text:: *)
(*\:4e4b\:524d\:8ba1\:7b97\:7684 Nucleon strange quark magnetic \:7684\:7ed3\:679c, \:6570\:503c\:7684\:5dee\:5f02\:53ef\:80fd\:6765\:81ea\:4e8e \:6b63\:89c4\:5b50\:51fd\:6570\:5f62\:5f0f\:7684\:9009\:62e9 \:548c \:8d28\:91cf\:ff0c\:5728 old \:5206\:652f\:4e2d\:ff0c\:4e13\:95e8\:4f7f\:7528\:4e4b\:524d\:7684 \:6b63\:89c4\:5b50\:8fdb\:884c\:8ba1\:7b97.*)


-0.002200491130978127+0.02252016282410953-0.016483728333618483//Simplify


(* ::Chapter:: *)
(*notation*)


(* ::Text:: *)
(*\:4fdd\:7559\:4e00\:4e9b\:8bb0\:53f7\:ff0c\:4ee5\:53ca\:52a8\:91cf\:53c2\:6570\:5316*)


(*\:524d\:9762\:591a\:4e2al\:8868\:793a\:662f\:5149\:9525\:5206\:91cf\:7684\:610f\:601d,\:5404\:52a8\:91cf\:5206\:91cf\:7684\:53c2\:6570\:5316*)
k={(y+\[Xi])/(2Sqrt[2]\[Xi]) Q,km,kt};(*\:4ecb\:5b50\:5708\:52a8\:91cf*)
p1={(1+\[Xi])/(2Sqrt[2]\[Xi]) Q,(1-\[Xi])/(2Sqrt[2]\[Xi]) Q,pt};(*GDP \:4e2d\:7684\:6838\:5b50\:521d\:672b\:6001\:52a8\:91cf\:ff0c\:4ee5\:53ca\:52a8\:91cf\:4e4b\:5dee*)
p2={(1-\[Xi])/(2Sqrt[2]\[Xi]) Q,(1+\[Xi])/(2Sqrt[2]\[Xi]) Q,pt};
q={-1/Sqrt[2] Q,1/Sqrt[2] Q,0};


\[CapitalLambda]=\[CapitalLambda]; (*dipole \:5f62\:72b6\:56e0\:5b50\:53c2\:6570*)
m\[Phi]=Subscript["m","\[Phi]"]; (*\:4ecb\:5b50\:8d28\:91cf*)
\[CapitalLambda]l=OverBar["\[CapitalLambda]"]; (*\:8d28\:91cf\:4e4b\:5dee\:ff0c\[CapitalLambda]-m\[Phi]*)
\[CapitalOmega]\[Phi]=Subscript["\[CapitalOmega]","\[Phi]"];(* \:5b64\:5be1\:79ef\:5206\:7684\:7ed3\:679c kt^2+m\[Phi]^2*)
\[CapitalOmega]\[CapitalLambda]=Subscript["\[CapitalOmega]","\[CapitalLambda]"];(* kt^2+\[CapitalLambda]^2*)


(* ::DisplayFormula:: *)
(*D\[Phi]B=(kt^2+y*MB^2-y*yl*M^2+yl*m\[Phi]^2)/- yl;*)
(*D\[CapitalLambda]B=(kt^2+y*MB^2-y*yl*M^2+yl*\[CapitalLambda]^2)/- yl;*)
(*D\[Phi]\[CapitalLambda]=kt^2+\[CapitalOmega]=kt^2+(1-z)m\[Phi]^2+z*\[CapitalLambda]^2;*)
(*\[CapitalOmega]=(1-z)m\[Phi]^2+z*\[CapitalLambda]^2;*)
(*D\[Phi]B+m\[Phi]^2-\[CapitalDelta]^2=(kt^2+(\[CapitalDelta]+M y)^2)/-yl;*)


\[CapitalOmega]=\[CapitalOmega];(*\:8d39\:66fc\:53c2\:6570\:5316\:5f97\:5230\:7684\:7ec4\:5408*)
z=z;(*\:8d39\:66fc\:53c2\:6570\:5316\:7684\:53c2\:6570*)
\[Delta]=\[Delta]; (*\[Delta]\:51fd\:6570*)
s\[Mu]=s\[Mu]; (*\:53d1\:6563\:79ef\:5206\:4e2d\:7684\:6807\:5ea6*)


(* ::Section:: *)
(*Note fcf14744388648*)


<<X`


(* ::Text:: *)
(*\:6570\:503c\:4e0d\:7a33\:5b9a\:6027\:6765\:81ea\:4e8e\:7c7b\:4f3cScalarC0\:8fd9\:6837\:7684\:51fd\:6570\:ff0c\:4e3a\:4e86\:907f\:514d\:4e0d\:7a33\:5b9a\:6027\:ff0c\:5728\:6240\:6709\:7684\:6570\:5b57\:4e2d\:52a0\:4e0a\:7cbe\:786e\:5ea6\:6807\:8bb0\:5373\:53ef\:3002*)


ScalarC0[1.4232490000000002`,1.4232490000000002`,-1.` Q2,0.9105336951480709`,1.385`,0.1381`]/.Q2->0.01
ScalarC0[1.4232490000000002`,1.4232490000000002`,-1.` Q2,0.1381`,1.385`,0.1381`]/.Q2->0.01
ScalarC0[1.4232490000000002`,1.4232490000000002`,-1.` Q2,0.9105336951480709`,1.385`,0.1381`]/.Q2->0.01


ScalarC0[1.4232490000000002`20,1.4232490000000002`20,-1.`20*Q2,0.9105336951480709`20,1.385`20,0.1381`20]/.Q2->0.000001`20
ScalarC0[1.4232490000000002`20,1.4232490000000002`20,-1.`20*Q2,0.1381`20,1.385`20,0.1381`20]/.Q2->0.00001`20
ScalarC0[1.4232490000000002`20,1.4232490000000002`20,-1.`20*Q2,0.9105336951480709`20,1.385`20,0.1381`20]/.Q2->0.00001`20


SetPrecision[ScalarC0[1.4232490000000002`,1.4232490000000002`,-1.` Q2,0.9105336951480709`,1.385`,0.1381`],10]/.Q2->0.000001`10


(* ::Text:: *)
(*\:6b64\:5916\:ff0cDiscB \:4f1a\:5f15\:5165\:5f88\:5c0f\:7684\:865a\:90e8\:ff0c\:4f8b\:598210^-15i\:ff0c\:9700\:8981\:60f3\:529e\:6cd5 Chop \:6389\:8fd9\:79cd\:7ed3\:679c\:3002*)


ScalarC0[mE^2,mE^2,-Q2,\[CapitalLambda],mo1,mm1]
ScalarC0[s1,s12,s2,m0,m1,m2]


2p1 . p2=-(p1-p2)^2+2mE^2=Q2+2mE^2


p2,p1,q


tea={{-(mE^2-\[CapitalLambda]^2),(Q2/2+mE^2),(Q2/2)},
{(Q2/2+mE^2),-(mE^2-mo1^2),(Q2/2)},
{(Q2/2),(Q2/2),-(-Q2-mm1^2)}
}
Det@tea


ScalarC0[1.4232493`,1.4232492`,-1.` Q2,0.9105336951480709`,1.385`,0.1381`]/.Q2->0.01


ter=(Kallen\[Lambda][mE,mE2,-Q2]//KallenExpand)
Series[ter,{mE2,mE,2}]//Simplify


(* ::Section:: *)
(*Landau singularity*)


tef={mE^2->1.4232490000000002,mm1->0.1381,\[CapitalLambda]->0.9105336951480709,mo1->1.385};


Echo@Simplify[Solve[Det[tea]==0,Q2],
{mE>mm1,\[CapitalLambda]>mm1}
]/.tef


Echo@Simplify[Solve[Det[tea[[{1,3},{1,3}]]]==0,Q2],
{mE>mm1,\[CapitalLambda]>mm1}
]/.tef


Echo@Simplify[Solve[Det[tea[[{1,2},{1,2}]]]==0,Q2],
{mE>mm1,\[CapitalLambda]>mm1}
]/.tef


Echo@Simplify[Solve[Det[tea[[{2,3},{2,3}]]]==0,Q2],
{mE>mm1,\[CapitalLambda]>mm1}
]/.tef


(* ::Section:: *)
(*ScalarC0,DiLog,Kallen\[Lambda]*)


ScalarC0[mE,mE2,-Q2,\[CapitalLambda],mo1,mm1]//C0Expand


(*If*)
Kallen\[Lambda][mE,mE2,-Q2]>0


Series[KallenExpand@Kallen\[Lambda][mE,mE2,-Q2],{mE2,mE,2}]//Simplify
KallenExpand@Kallen\[Lambda][mE,mE2,-Q2]/.{mE2->mE+\[Delta]}//Simplify


(* ::Chapter:: *)
(*back up*)


(* ::Section:: *)
(*num FFs bakup*)


(*\:5224\:65ad\:662f\:5426\:5904\:4e8e fitting \:6a21\:5f0f, \:5904\:7406\:4f20\:5165\:7684 ffsMergedWithRen, \:7ed9\:51fa\:5404\:79cd\:5473\:9053\:7684\:7ed3\:679c,\:8fd9\:91cc\:53ea\:63d0\:53d6\:4e86 GEGM *)
numFFs[<|"fit"->$fittingQ_|>,ffsMergedWithRen_,chopQ2Val_]:=Switch[$fittingQ,
(*\:666e\:901a\:8ba1\:7b97, \:4f7f\:7528\:7684\:6570\:503c\:8868\:8fbe\:5f0f*)
False,
Query[All,<|
tagNum["tr","uds"]->chopQ2Val[#@ffsTreeGEGM/.quaCharge["uds"]],
tagNum["tr","u"]->chopQ2Val[#@ffsTreeGEGM/.quaCharge["u"]],
tagNum["tr","d"]->chopQ2Val[#@ffsTreeGEGM/.quaCharge["d"]],
tagNum["tr","s"]->chopQ2Val[#@ffsTreeGEGM/.quaCharge["s"]],
(*loop*)
tagNum["lo","uds"]->chopQ2Val[#@ffsLoopGEGM/.quaCharge["uds"]],
tagNum["lo","u"]->chopQ2Val[#@ffsLoopGEGM/.quaCharge["u"]],
tagNum["lo","d"]->chopQ2Val[#@ffsLoopGEGM/.quaCharge["d"]],
tagNum["lo","s"]->chopQ2Val[#@ffsLoopGEGM/.quaCharge["s"]],
(*total = tree +(Z-1)*tree+loop*)
tagNum["tr+lo","uds"]->chopQ2Val[
#@recon*#@ffsTreeGEGM+#@ffsLoopGEGM/.quaCharge["uds"]]
|>&
]@ffsMergedWithRen,
(*-------------------- fitting \:65f6,\:4f7f\:7528\:7684\:8868\:8fbe\:5f0f --------------*)
True,
Query[All,<|
(*total = tree +(Z-1)*tree+loop*)
tagNum["tr+lo","uds"]->chopQ2Val[
#@recon*#@ffsTreeGEGM+#@ffsLoopGEGM/.quaCharge["uds"]]
|>&
]@ffsMergedWithRen
];


(*\:6839\:636e\:8ba1\:7b97\:7684\:5c55\:5f00\:9636\:6570, $parOrdStr, \:9009\:62e9\:4e0d\:540c\:7684\:6570\:503c\:5904\:7406\:65b9\:5f0f; numVal \:662f\:9884\:7559\:63a5\:53e3, \:7528\:6765\:63a7\:5236\:6570\:5b57\:683c\:5f0f\:5316*)
numFFs[<|"ord"->$parOrdStr_|>,ffsMergedWithRen_]:=Module[{chopQ2Val},
Switch[$parOrdStr,
(*\:5982\:679c\:8ba1\:7b97 order 0 \:7684\:6570\:636e,\:9009\:62e9 chopQ2, \:5373\:4ee4 Q2\[Rule]0*)
$ord0,
chopQ2Val[x_]:=numVal@chopQ2[x];
numFFs[<|"fit"->$fittingQ|>,ffsMergedWithRen,chopQ2Val],
(*\:5982\:679c\:8ba1\:7b97 order 1 order full \:7684\:6570\:636e, \:9009\:62e9 chop, \:4fdd\:7559 Q2 \:4f9d\:8d56*)
_,
chopQ2Val[x_]:=numVal@chop[x];
numFFs[<|"fit"->$fittingQ|>,ffsMergedWithRen,chopQ2Val]
]];


(*\:751f\:6210\:6700\:7ec8\:7684\:6570\:503c\:8868\:793a*)
numFFs["v"]=Switch[$parOrdStr,
(*order 0 \:7684\:8ba1\:7b97\:7ed3\:679c*)
$ord0,
<|
$ord0->numFFs[<|"ord"->$ord0|>,ffsMerged["WithRen"][[$ord0]]],
Nothing
|>,
(* \:5176\:4ed6 order \:7684\:7ed3\:679c*)
_,
<|
(*\:9644\:52a0 order 0 \:7684\:8ba1\:7b97\:7ed3\:679c*)
$ord0->numFFs[<|"ord"->$ord0|>,ffsMerged["WithRen"][[$ord0]]],
(* order 1, \:6216\:8005 order full \:7684\:8ba1\:7b97\:7ed3\:679c*)
$parOrdStr->numFFs[<|"ord"->$parOrdStr|>,ffsMerged["WithRen"][[$parOrdStr]]]
|>
];


(* ::Section:: *)
(*Lagrangian:b9,b10,b11*)


(*\:5f20\:91cf\:8026\:5408\:9879, 1/2 \[ImaginaryI] b9 Tr[Bbar Subscript[A, \[Mu]]].\[Sigma]^\[Mu]\[Nu].Tr[Subscript[A, \[Nu]].B]+1/2 \[ImaginaryI] b10 Tr[Bbar[Subscript[A, \[Mu]],Subscript[A, \[Nu]]].\[Sigma]^\[Mu]\[Nu].B]+1/2 \[ImaginaryI] b11 Tr[Bbar {Subscript[A, \[Mu]],Subscript[A, \[Nu]]}.\[Sigma]^\[Mu]\[Nu].B] *)
SymIdx[A\[Mu]_,A\[Nu]_]:=Module[{U\[Mu],U\[Nu],L\[Mu],L\[Nu],D\[Mu],D\[Nu]},
U\[Mu]=UpperTriangularize[A\[Mu]];U\[Nu]=UpperTriangularize[A\[Nu]];
L\[Mu]=LowerTriangularize[A\[Mu]];L\[Nu]=LowerTriangularize[A\[Nu]];
D\[Mu]=DiagonalMatrix[Diagonal[A\[Mu]]];D\[Nu]=DiagonalMatrix[Diagonal[A\[Nu]]];
D\[Mu] . D\[Nu]-D\[Mu] . L\[Nu]-D\[Mu] . U\[Nu]+L\[Mu] . L\[Nu]+U\[Mu] . U\[Nu]+L\[Mu] . U\[Nu]+L\[Nu] . D\[Mu]+U\[Nu] . D\[Mu]-U\[Nu] . L\[Mu]
](*\:8fd9\:4e2a\:51fd\:6570\:7528\:6765\:628a\:504f\:5bfc\:6570\:7684\:6307\:6807\:5bf9\:79f0\:5316\:ff0c\:628a\:4ecb\:5b50\:77e9\:9635\:6309\:4e0a\:4e0b\:4e09\:89d2\:548c\:5bf9\:89d2\:7ebf\:5206\:89e3*)


lag["bbb"]=Expand[4*I/2 (
lecs["b9"]*(Tr[mat["Bbar"] . crt["u","\[Mu]","hd"]]*Tr[crt["u","\[Nu]","hd"] . ltz[mat["B"],"tp"->"\[Sigma]","idx"->"\[Mu]\[Nu]"]])
-lecs["b10"]*Tr[mat["Bbar"] . acmt[Dot,2*SymIdx[crt["u","\[Nu]","hd"],crt["u","\[Mu]","hd"]],ltz[mat["B"],"tp"->"\[Sigma]","idx"->"\[Mu]\[Nu]"]]]
-lecs["b11"]*Tr[mat["Bbar"] . cmt[Dot,2*SymIdx[crt["u","\[Nu]","hd"],crt["u","\[Mu]","hd"]],ltz[mat["B"],"tp"->"\[Sigma]","idx"->"\[Mu]\[Nu]"]]]
)
];
(*\:6311\:9009\:51fa\:5176\:4e2d\:67d0\:4e9b\:9879*)
laglkp1[
lag["bbb"],
ContainsAll,{fd[2,3,0],fd[2,3,1](*\:53ea\:67e5\:770b\:6838\:5b50*)}
]
