(* ::Package:: *)

(* ::Title:: *)
(*gpd.loop.residue-symmetric.wl*)


(* ::Text:: *)
(*\:4f7f\:7528\:7559\:6570\:5b9a\:7406\:67e5\:770b\:4f20\:64ad\:5b50\:79ef\:5206\:516c\:5f0f\:7684\:5f62\:5f0f*)


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


(* ::Text:: *)
(*\:5982\:679c\:5728\:547d\:4ee4\:884c\:6267\:884c\:ff0c\:5c31\:76f4\:63a5\:63a5\:53d7\:547d\:4ee4\:884c\:53d8\:91cf*)


(* ::Text:: *)
(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c\:ff0c\:5c31\:6a21\:62df\:547d\:4ee4\:884c\:8f93\:5165\:ff0c\:8ba1\:7b97\:6a21\:62df\:53d8\:91cf\:7684\:5b57\:7b26\:4e32\:5f62\:5f0f\:3002\:56e0\:4e3a\:547d\:4ee4\:884c\:4f20\:5165\:7684\:4e00\:822c\:662f\:5b57\:7b26\:4e32\:ff0c\:8fd9\:6837\:53ef\:4ee5\:7edf\:4e00\:5f62\:5f0f\:3002*)


(* ::Section:: *)
(*kinematics*)


(*GDP \:4e2d\:7684\:6838\:5b50\:521d\:672b\:6001\:52a8\:91cf\:ff0c\:4ee5\:53ca\:52a8\:91cf\:4e4b\:5dee*)
p1:={(1+\[Xi])Pp,(mN^2+\[CapitalDelta]t2/4)/(2(1+\[Xi])Pp),\[CapitalDelta]t/2}
p2:={(1-\[Xi])Pp,(mN^2+\[CapitalDelta]t2/4)/(2(1-\[Xi])Pp),-\[CapitalDelta]t/2}
\[CapitalDelta]:={\[Xi]*2*Pp,(\[CapitalDelta]2+\[CapitalDelta]t2)/(4\[Xi]*Pp),\[CapitalDelta]t}
k:={(y+\[Xi])Pp,km,kt+\[CapitalDelta]t/2}(*\:4ecb\:5b50\:5708\:52a8\:91cf*)


(*\:5149\:9525\:6807\:91cf\:79ef:light-cone scalar product\:ff0c1\:8868\:793a+\:5206\:91cf\:ff0c2\:8868\:793a-\:5206\:91cf\:ff0c3\:8868\:793a\[Perpendicular]\:5206\:91cf*)
lConeSP[k_,p_]:=Expand[
k[[1]]*p[[2]]+k[[2]]*p[[1]]-k[[3]]*p[[3]]
]


(*\:516b\:91cd\:6001\:90e8\:5206\:547d\:540d*)
Clear[
mN,(*\:521d\:672b\:6001\:6838\:5b50\:8d28\:91cf*)f(*\:4ecb\:5b50\:8870\:53d8\:5e38\:6570*),
mD(*MB-mN*)
];
mB; (*in\:8868\:793a\:4e2d\:95f4\:ff0c\:5373\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf\:ff0c\:524d\:51e0\:4e2a\:56fe\:662f\:516b\:91cd\:6001*)
mS; (*M+MB\:ff0c\:8d28\:91cf\:4e4b\:548c*)
eQM=Subsuperscript["e","Q","M"]; (*\:8026\:5408\:5e38\:6570*)
CBM=Subsuperscript["C","B","M"];
CMM=Subscript["C","\[Phi]\[Phi]\[ConjugateTranspose]"];
eqB=Subsuperscript["e","q","B"];


(*\:4f20\:64ad\:5b50*)
gator[\[CapitalLambda]]:=lConeSP[k,k]-\[CapitalLambda]^2;(*\:6b63\:89c4\:5b501*)
gator[\[CapitalLambda]\[CapitalDelta]]:=lConeSP[k-\[CapitalDelta],k-\[CapitalDelta]]-\[CapitalLambda]^2;(*\:6b63\:89c4\:5b502*)
gator[m]:=lConeSP[k,k]-mm^2;(*\:4ecb\:5b50\:4f20\:64ad\:5b503*)
gator[m\[CapitalDelta]]:=lConeSP[k-\[CapitalDelta],k-\[CapitalDelta]]-mm^2;(*\:4ecb\:5b50\:4f20\:64ad\:5b504*)
gator[B]:=lConeSP[p1-k,p1-k]-mB^2;(*\:91cd\:5b50\:4f20\:64ad\:5b505*)
gator[B\[CapitalDelta]]:=lConeSP[p2-k,p2-k]-mB^2;(*\:91cd\:5b50\:4f20\:64ad\:5b506*)


(*\:4f20\:64ad\:5b50\:7684\:96c6\:5408*)
propgators:={
gator[\[CapitalLambda]],
gator[\[CapitalLambda]\[CapitalDelta]],
gator[m],
gator[m\[CapitalDelta]],
gator[B],
gator[B\[CapitalDelta]]
}
(*k^-\:5206\:91cf\:524d\:7684\:7cfb\:6570*)
k2Coes:=Simplify[(Coefficient[#1,km,1]&/@propgators)]
(*\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:503c*)
propgPoles:=Simplify[Values[Solve[#1==0,{km}][[1,1]]&/@propgators]]


(* ::Chapter:: *)
(*\:7559\:6570\:5b9a\:7406*)


ruleMomentum:={
\[CapitalDelta]t2->-4mN^2*\[Xi]^2+(1-\[Xi]^2)(-\[CapitalDelta]2),
\[CapitalDelta]t^2->-4mN^2*\[Xi]^2+(1-\[Xi]^2)(-\[CapitalDelta]2)
}


residueSum[int_,km_,poles_]:=Simplify[
Total[
(Residue[int,{km,#1}]&/@poles)/.ruleMomentum
]]


rulePropgator={

-4 kt y \[CapitalDelta]t+2 y \[CapitalLambda]^2+4 kt^2 \[Xi]-4 mN^2 y^2 \[Xi]+y^2 \[CapitalDelta]2 \[Xi]+\[CapitalDelta]t^2 \[Xi]+2 \[CapitalLambda]^2 \[Xi]+4 mN^2 \[Xi]^3-\[CapitalDelta]2 \[Xi]^3->D1,
4 kt yl \[CapitalDelta]t+4 kt^2 (1+\[Xi])+4 mB^2 (y+\[Xi])->D2,

kt^2+\[CapitalLambda]^2->\[CapitalOmega]\[CapitalLambda]^2,
kt^2+mm^2->\[CapitalOmega]m^2,
2kt^2+\[CapitalLambda]^2+mm^2->\[CapitalOmega]\[CapitalLambda]^2+\[CapitalOmega]m^2,

\[CapitalLambda]^2-mm^2->\[CapitalLambda]l^2,mm^2-\[CapitalLambda]^2->-\[CapitalLambda]l^2,
1-y->yl,y-1->-yl,1-\[Zeta]->\[Zeta]l,\[Zeta]-1->-\[Zeta]l,
1-y*\[Zeta]->y\[Zeta]l,y*\[Zeta]-1->-y\[Zeta]l
};


(* ::Section:: *)
(*fig a*)


(* ::Subsection:: *)
(*\:6ee1\:4f20\:64ad\:5b50*)


residueSum[
1/(gator[\[CapitalLambda]]*gator[\[CapitalLambda]\[CapitalDelta]]*gator[m]*gator[m\[CapitalDelta]]*gator[B]),
km,
{propgPoles[[1]],propgPoles[[3]]}
](*/.rulePropgator*)


residueSum[
1/(gator[\[CapitalLambda]]*gator[\[CapitalLambda]\[CapitalDelta]]*gator[m]*gator[mq]*gator[B]),
km,
propgPoles[[{5}]]
]/.rulePropgator


(* ::Subsection:: *)
(*\:65e0\:91cd\:5b50*)


residueSum[
1/(gator[\[CapitalLambda]]*gator[\[CapitalLambda]\[CapitalDelta]]*gator[m]*gator[mq]),km,propgPoles[[{1,3}]]
]/.rulePropgator


(* ::Subsection:: *)
(*a*)


residueSum[
1/(gator[\[CapitalLambda]]*gator[\[CapitalLambda]\[CapitalDelta]]*gator[B]),km,{propgPoles[[1]]}
]/.rulePropgator


residueSum[
1/(gator[m]*gator[mq]*gator[B]),km,{propgPoles[[3]]}
]/.rulePropgator


(* ::Subsection:: *)
(*b*)


residueSum[
1/(gator[\[CapitalLambda]]*gator[\[CapitalLambda]\[CapitalDelta]]),km,{propgPoles[[1]]}
]/.rulePropgator


residueSum[
1/(gator[m]*gator[mq]),km,{propgPoles[[3]]}
]/.rulePropgator


residueSum[
1/gator[m],km,{propgPoles[[3]]}
]/.rulePropgator


(* ::Subsection:: *)
(*c*)


residueSum[
1/(gator[\[CapitalLambda]]*gator[m]*gator[B]),km,{propgPoles[[1]],propgPoles[[3]]}
]/.rulePropgator


(* ::Text:: *)
(*\:53ef\:4ee5\:4f7f\:7528DB[p-k]\:7684pole\:9a8c\:8bc1\:4e00\:4e0b*)


residueSum[
1/(gator[m]*gator[mq]),km,{propgPoles[[3]],propgPoles[[4]]}
]/.rulePropgator


residueSum[
1/(gator[\[CapitalLambda]]*gator[m]*gator[B]),km,{propgPoles[[5]]}
]/.rulePropgator
