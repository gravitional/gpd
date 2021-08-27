(* ::Package:: *)

(* ::Title:: *)
(*gpd_loop_reduction.nb*)


(* ::Text:: *)
(*\:4f7f\:7528\:7559\:6570\:5b9a\:7406\:67e5\:770b\:4f20\:64ad\:5b50\:79ef\:5206\:516c\:5f0f\:7684\:5f62\:5f0f*)


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
"a"
(*\:5728\:8fd9\:91cc\:7ed9\:51fa\:5176\:4ed6\:53c2\:6570\:5728mathematica\:8bed\:6cd5\:4e0b\:7684\:5f62\:5f0f\:ff0c\:5916\:9762\:7684ToString\:4f1a\:81ea\:52a8\:8f6c\:6362*)
]
}
];
echo["the input parameter is:\n"];enString[inputCml]


(* ::Chapter:: *)
(*initial2*)


(* ::Section:: *)
(*kinematics*)


(*\:524d\:9762\:591a\:4e2al\:8868\:793a\:662f\:5149\:9525\:5206\:91cf\:7684\:610f\:601d*)
k:={kp,km,kt}(*\:4ecb\:5b50\:5708\:52a8\:91cf*)
p1:={p1p,p1m,pt}(*GDP \:4e2d\:7684\:6838\:5b50\:521d\:672b\:6001\:52a8\:91cf\:ff0c\:4ee5\:53ca\:52a8\:91cf\:4e4b\:5dee*)
p2:={p2p,p2m,pt}
q:={-p1p/\[Zeta],p1p/\[Zeta],0}


(*\:5149\:9525\:6807\:91cf\:79ef:light-cone scalar product\:ff0c1\:8868\:793a+\:5206\:91cf\:ff0c2\:8868\:793a-\:5206\:91cf\:ff0c3\:8868\:793a\[Perpendicular]\:5206\:91cf*)
lgsp[k_,p_]:=Expand[k[[1]]*p[[2]]+k[[2]]*p[[1]]-k[[3]]*p[[3]]]


(*\:516b\:91cd\:6001\:90e8\:5206\:547d\:540d*)
Clear[
MN,(*\:521d\:672b\:6001\:6838\:5b50\:8d28\:91cf*)f(*\:4ecb\:5b50\:8870\:53d8\:5e38\:6570*),
\[CapitalDelta](*MB-MN*)
];
Mmd=Subscript["M","B"]; (*in\:8868\:793a\:4e2d\:95f4\:ff0c\:5373\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf\:ff0c\:524d\:51e0\:4e2a\:56fe\:662f\:516b\:91cd\:6001*)
Ml=OverBar["M"]; (*M+MB\:ff0c\:8d28\:91cf\:4e4b\:548c*)
eq\[Phi]=Subsuperscript["e","q","\[Phi]"]; (*\:8026\:5408\:5e38\:6570*)
Cmd\[Phi]=Subsuperscript["C","B","\[Phi]"];
C\[Phi]\[Phi]=Subscript["C","\[Phi]\[Phi]\[ConjugateTranspose]"];
eqmd=Subsuperscript["e","q","B"];
(*\:4f20\:64ad\:5b50*)
Dp[\[CapitalLambda]]:=lgsp[k,k]-\[CapitalLambda]^2;(*\:6b63\:89c4\:5b501*)
Dp[\[CapitalLambda]q]:=lgsp[k+q,k+q]-\[CapitalLambda]^2;(*\:6b63\:89c4\:5b502*)
Dp[\[Phi]]:=lgsp[k,k]-m\[Phi]^2;(*\:4ecb\:5b50\:4f20\:64ad\:5b503*)
Dp[\[Phi]q]:=lgsp[k+q,k+q]-m\[Phi]^2;(*\:4ecb\:5b50\:4f20\:64ad\:5b504*)
Dp[B]:=lgsp[p1-k,p1-k]-Mmd^2;(*\:91cd\:5b50\:4f20\:64ad\:5b505*)
Dp[Bq]:=lgsp[p2-k,p2-k]-Mmd^2;(*\:91cd\:5b50\:4f20\:64ad\:5b506*)
Dps:={Dp[\[CapitalLambda]],Dp[\[CapitalLambda]q],Dp[\[Phi]],Dp[\[Phi]q],Dp[B],Dp[Bq]}
(*k^-\:5206\:91cf\:524d\:7684\:7cfb\:6570*)
k2Coes:=Simplify[(Coefficient[#1,km,1]&/@Dps)]
(*\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:503c*)
DpZs:=Simplify[Values[Solve[#1==0,{km}][[1,1]]&/@Dps]]


(* ::Chapter:: *)
(*\:7559\:6570\:5b9a\:7406*)


rl`momentum:={
kp->y*p1p,
p1m->(pt^2+MN^2)/(2p1p)
}


res`sum[int_,km_,poles_]:=Simplify[Total[(Residue[int,{km,#1}]&/@poles)/.rl`momentum]]


rl`ppg={
kt^2+m\[Phi]^2-MN^2*y-m\[Phi]^2*y-2kt*pt*y+MN^2*y^2+pt^2*y^2+y*Mmd^2->(-yl)D\[Phi]B,
kt^2+\[CapitalLambda]^2-MN^2*y-\[CapitalLambda]^2*y-2kt*pt*y+MN^2*y^2+pt^2*y^2+y*Mmd^2->(-yl)D\[CapitalLambda]B,
(2p1p^2*(-1+y)*(-1+y*\[Zeta])+\[Zeta]*(-pt^2*y+kt^2*(-1+\[Zeta])+m\[Phi]^2*\[Zeta]-m\[Phi]^2*y*\[Zeta]+pt^2*y^2*\[Zeta]-2*kt*pt*(-1+y*\[Zeta])+MN^2*(-1+y)(-1+y*\[Zeta]))+\[Zeta]*(-1+y*\[Zeta])Mmd^2) ->D\[Phi]Bt,
(2p1p^2*(-1+y)*(-1+y*\[Zeta])+\[Zeta]*(-pt^2*y+kt^2*(-1+\[Zeta])+pt^2*y^2*\[Zeta]-2*kt*pt*(-1+y*\[Zeta])+MN^2*(-1+y)*(-1+y*\[Zeta])+\[Zeta]*\[CapitalLambda]^2-y*\[Zeta]*\[CapitalLambda]^2)+\[Zeta]*(-1+y*\[Zeta])Mmd^2)->D\[CapitalLambda]Bt,
(2p1p^2*y(y*\[Zeta]-1)+\[Zeta](-kt^2+m\[Phi]^2 (y*\[Zeta]-1)-y*\[Zeta]*\[CapitalLambda]^2))->-\[CapitalOmega]B\[Phi]t2,
(2p1p^2*y(y*\[Zeta]-1)-\[Zeta](\[CapitalLambda]^2+kt^2+y*\[Zeta]*m\[Phi]^2-y*\[Zeta]*\[CapitalLambda]^2))->-\[CapitalOmega]B\[CapitalLambda]t2,
(2p1p^2*y*(y*\[Zeta]-1)-\[Zeta](kt^2+m\[Phi]^2))->-\[CapitalOmega]B\[Phi]t,
(2p1p^2*y*(y*\[Zeta]-1)-\[Zeta](kt^2+\[CapitalLambda]^2))->-\[CapitalOmega]B\[CapitalLambda]t,
4p1p^3*y*(y*\[Zeta]-1)-2p1p*\[Zeta]*(kt^2+m\[Phi]^2)->-2p1p*\[CapitalOmega]B\[Phi]t,
4p1p^3*y*(y*\[Zeta]-1)-2p1p*\[Zeta]*(kt^2+\[CapitalLambda]^2)->-2p1p*\[CapitalOmega]B\[CapitalLambda]t,
kt^2+\[CapitalLambda]^2->\[CapitalOmega]\[CapitalLambda]^2,kt^2+m\[Phi]^2->\[CapitalOmega]\[Phi]^2,2kt^2+\[CapitalLambda]^2+m\[Phi]^2->\[CapitalOmega]\[CapitalLambda]^2+\[CapitalOmega]\[Phi]^2,
\[CapitalLambda]^2-m\[Phi]^2->\[CapitalLambda]l^2,m\[Phi]^2-\[CapitalLambda]^2->-\[CapitalLambda]l^2,
1-y->yl,y-1->-yl,1-\[Zeta]->\[Zeta]l,\[Zeta]-1->-\[Zeta]l,
1-y*\[Zeta]->y\[Zeta]l,y*\[Zeta]-1->-y\[Zeta]l
};


(* ::Section:: *)
(*fig a*)


(* ::Subsection:: *)
(*\:6ee1\:4f20\:64ad\:5b50*)


res`sum[
1/(Dp[\[CapitalLambda]]*Dp[\[CapitalLambda]q]*Dp[\[Phi]]*Dp[\[Phi]q]*Dp[B]),km,{DpZs[[1]],DpZs[[3]]}
]/.rl`ppg


res`sum[
1/(Dp[\[CapitalLambda]]*Dp[\[CapitalLambda]q]*Dp[\[Phi]]*Dp[\[Phi]q]*Dp[B]),km,DpZs[[{5}]]
]/.rl`ppg


(* ::Subsection:: *)
(*\:65e0\:91cd\:5b50*)


res`sum[
1/(Dp[\[CapitalLambda]]*Dp[\[CapitalLambda]q]*Dp[\[Phi]]*Dp[\[Phi]q]),km,DpZs[[{1,3}]]
]/.rl`ppg


(* ::Subsection:: *)
(*a*)


res`sum[
1/(Dp[\[CapitalLambda]]*Dp[\[CapitalLambda]q]*Dp[B]),km,{DpZs[[1]]}
]/.rl`ppg


res`sum[
1/(Dp[\[Phi]]*Dp[\[Phi]q]*Dp[B]),km,{DpZs[[3]]}
]/.rl`ppg


(* ::Subsection:: *)
(*b*)


res`sum[
1/(Dp[\[CapitalLambda]]*Dp[\[CapitalLambda]q]),km,{DpZs[[1]]}
]/.rl`ppg


res`sum[
1/(Dp[\[Phi]]*Dp[\[Phi]q]),km,{DpZs[[3]]}
]/.rl`ppg


res`sum[
1/Dp[\[Phi]],km,{DpZs[[3]]}
]/.rl`ppg


(* ::Subsection:: *)
(*c*)


res`sum[
1/(Dp[\[CapitalLambda]]*Dp[\[Phi]]*Dp[B]),km,{DpZs[[1]],DpZs[[3]]}
]/.rl`ppg


(* ::Text:: *)
(*\:53ef\:4ee5\:4f7f\:7528DB[p-k]\:7684pole\:9a8c\:8bc1\:4e00\:4e0b*)


res`sum[
1/(Dp[\[Phi]]*Dp[\[Phi]q]),km,{DpZs[[3]],DpZs[[4]]}
]/.rl`ppg


res`sum[
1/(Dp[\[CapitalLambda]]*Dp[\[Phi]]*Dp[B]),km,{DpZs[[5]]}
]/.rl`ppg
