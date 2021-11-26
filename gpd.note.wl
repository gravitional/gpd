(* ::Package:: *)

(* ::Title:: *)
(*gpd_notation.nb*)


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
