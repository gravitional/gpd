(* ::Package:: *)

(* ::Title:: *)
(*pdf_loop_reduction.nb*)


(* ::Text:: *)
(*\:5316\:7b80PDF\:8ba1\:7b97\:4e2d\:7684\:5708\:79ef\:5206*)


(* ::Chapter:: *)
(*initial*)


(*\:672c\:6587\:4ef6\:7684\:540d\:79f0*)
$fileName=If[$Notebooks,NotebookFileName[],$InputFileName];
(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c\:ff0c\:5c31\:5237\:65b0\:7b14\:8bb0\:672c\:7684\:6807\:9898*)
Once@If[!$Notebooks,NotebookWrite[Cells[][[1]],Cell[Last@FileNameSplit[$fileName],"Title"]]];
(*\:67e5\:627e init.wl, \:5bfc\:5165\:6839\:76ee\:5f55\:548c\:51fd\:6570\:5b9a\:4e49.*)
Catch@Module[{recurFind,start=1,depMax},
depMax=FileNameDepth[$fileName];(*\:8def\:5f84\:7684\:6700\:5927\:5c42\:6b21*)
(*-------\:5b9a\:4e49\:9012\:5f52\:51fd\:6570-------*)
recurFind[dep_Integer]:=If[dep<=depMax,
SetDirectory[DirectoryName[$fileName,dep]];(*SetDirectory[]\:8bbe\:7f6e\:5de5\:4f5c\:76ee\:5f55\:4e3a\:5bb6\:76ee\:5f55*)
If[FileExistsQ["init.wl"],Get["init.wl"];Throw["The base directory is : "<>$gitLocalName],recurFind[dep+1](*\:5982\:679c\:8fd9\:4e00\:5c42\:627e\:4e0d\:5230\:ff0c\:5c31\:4e0a\:5347\:4e00\:5c42*)];
ResetDirectory[];,
Throw["I cann't find any init.wl in this project"]
];
recurFind[start];
]


(* ::Text:: *)
(*\:5982\:679c\:5728\:547d\:4ee4\:884c\:6267\:884c\:ff0c\:5c31\:76f4\:63a5\:63a5\:53d7\:547d\:4ee4\:884c\:53d8\:91cf*)


(* ::Text:: *)
(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c\:ff0c\:5c31\:6a21\:62df\:547d\:4ee4\:884c\:8f93\:5165\:ff0c\:8ba1\:7b97\:6a21\:62df\:53d8\:91cf\:7684\:5b57\:7b26\:4e32\:5f62\:5f0f\:3002\:56e0\:4e3a\:547d\:4ee4\:884c\:4f20\:5165\:7684\:4e00\:822c\:662f\:5b57\:7b26\:4e32\:ff0c\:8fd9\:6837\:53ef\:4ee5\:7edf\:4e00\:5f62\:5f0f\:3002*)


If[cmdQ,
(*\:5982\:679c\:5728\:547d\:4ee4\:884c\:6267\:884c*)
input`cml=$ScriptCommandLine,
(*++++++++++++++++++++++++++++++++++++++++*)
(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c*)
input`cml={$fileName,(*\:547d\:4ee4\:884c\:7b2c\:4e00\:4e2a\:53c2\:6570\:6c38\:8fdc\:662f\:6b64\:811a\:672c\:7684\:7edd\:5bf9\:8def\:5f84*)
(*\:5176\:4ed6\:53c2\:6570*)
input`simulation=enString[
(*\:5728\:8fd9\:91cc\:7ed9\:51fa\:5176\:4ed6\:53c2\:6570\:5728mathematica\:8bed\:6cd5\:4e0b\:7684\:5f62\:5f0f\:ff0c\:5916\:9762\:7684ToString\:4f1a\:81ea\:52a8\:8f6c\:6362*)
(*\:5c3d\:91cf\:591a\:4f7f\:7528Association\:7ed3\:6784*)
",a"
(*\:5728\:8fd9\:91cc\:7ed9\:51fa\:5176\:4ed6\:53c2\:6570\:5728mathematica\:8bed\:6cd5\:4e0b\:7684\:5f62\:5f0f\:ff0c\:5916\:9762\:7684ToString\:4f1a\:81ea\:52a8\:8f6c\:6362*)
]
}
];
echo["the input parameter is:\n"];enString[inputCml]


(* ::Chapter:: *)
(*package*)


Once[<<"FeynCalc`"]


fcestd[x_]:=x//FCE//StandardForm;
fcistd[x_]:=x//FCI//StandardForm;


(* ::Chapter:: *)
(*kinematics*)


k=k;(*\:4ecb\:5b50\:5708\:52a8\:91cf*)p=p;(*pdf \:4e2d\:7684\:521d\:672b\:6001\:6838\:5b50\:52a8\:91cf*)
p2=p;p1=p;q=0;(*GDP \:4e2d\:7684\:6838\:5b50\:52a8\:91cf*)
\[Mu]=\[Mu];(*\:81ea\:7531\:6307\:6807*)
pp=FV[p,"+"];kp=FV[k,"+"];qp=FV[q,"+"];(*\:52a8\:91cf\:7684plus\:5206\:91cf*)
y=y;yl=OverBar[y];(*\:52a8\:91cf\:5206\:6570*)
kt=Subscript[k,"\[Perpendicular]"];(*\:6a2a\:5411\:52a8\:91cf*)
tykp=tykp;(*2y*k.p\:ff0c\:4ecb\:5b50\:548c\:6838\:5b50\:7684\:52a8\:91cf\:5185\:79ef*)


m=m;
\[CapitalLambda]=\[CapitalLambda]; (*dipole \:5f62\:72b6\:56e0\:5b50\:53c2\:6570*)
m\[Phi]=Subscript["m","\[Phi]"]; (*\:4ecb\:5b50\:8d28\:91cf*)
\[CapitalLambda]l=OverBar["\[CapitalLambda]"]; (*\:8d28\:91cf\:4e4b\:5dee\:ff0c\[CapitalLambda]-m\[Phi]*)
\[CapitalOmega]\[Phi]=Subscript["\[CapitalOmega]","\[Phi]"];(* \:5b64\:5be1\:79ef\:5206\:7684\:7ed3\:679c kt^2+m\[Phi]^2*)
\[CapitalOmega]\[CapitalLambda]=Subscript["\[CapitalOmega]","\[CapitalLambda]"];(* kt^2+\[CapitalLambda]^2*)


(*\:516b\:91cd\:6001\:90e8\:5206\:547d\:540d*)
M=M; (*\:521d\:672b\:6001\:6838\:5b50\:8d28\:91cf*)f=f;
Mmd=Subscript["M","B"]; (*in\:8868\:793a\:4e2d\:95f4\:ff0c\:5373\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf\:ff0c\:524d\:51e0\:4e2a\:56fe\:662f\:516b\:91cd\:6001*)
\[CapitalDelta]=\[CapitalDelta]; (*MB-M*)
Ml=OverBar["M"]; (*M+MB\:ff0c\:8d28\:91cf\:4e4b\:548c*)
eq\[Phi]=Subsuperscript["e","q","\[Phi]"]; (*\:8026\:5408\:5e38\:6570*)
Cmd\[Phi]=Subsuperscript["C","B","\[Phi]"];
C\[Phi]\[Phi]=Subscript["C","\[Phi]\[Phi]\[ConjugateTranspose]"];
eqmd=Subsuperscript["e","q","B"];
(*\:4f20\:64ad\:5b50*)
prp=prp;
D\[Phi]=Subscript["D","\[Phi]"];
D\[CapitalLambda]=Subscript["D","\[CapitalLambda]"]; 
Dmd=Subscript["D","B"];
(*\:4f20\:64ad\:5b50\:7684\:7ec4\:5408*)
D\[Phi]\[CapitalLambda]=Subscript["D","\[Phi]\[CapitalLambda]"];
D\[Phi]md=Subscript["D","\[Phi]B"]; 
D\[CapitalLambda]md=Subscript["D","\[CapitalLambda]B"];


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
SP[p,p]=M^2; (*\:6838\:5b50\:7684\:5728\:58f3\:5173\:7cfb\:ff0c\:5148\:7ed9\:51fa\:6807\:91cf\:79ef\:ff0c\:53ef\:4ee5\:52a0\:901f feyncalc \:5316\:7b80*)


(* ::Chapter:: *)
(*\:79ef\:5206\:5e93*)


(* ::DisplayFormula:: *)
(*D\[Phi]\[CapitalLambda]=kt^2+\[CapitalOmega]*)


ats[x:__Association|__Rule]:=Sequence@@Normal[Merge[{x},Total]];
ats::usage="\:8f85\:52a9\:51fd\:6570\:ff0c\:5c06\:591a\:4e2a\:5173\:8054\:5408\:5e76,\:91cd\:590d\:7684\:952e\:503c\:6c42\:548c\:ff0c\:5e76\:8f6c\:5316\:4e3a\:89c4\:5219\:7684\:5e8f\:5217";


fad::usage="\:8bb0\:5f55\:4f20\:64ad\:5b50\:7ed3\:6784\:ff0c\:4f5c\:4e3a\:8f93\:5165\:63a5\:53e3";
fad[x:__Rule]:=fad`a[ats[x]]; (*\:5c06\:91cd\:590d\:7684\:4f20\:64ad\:5b50\:5e42\:6b21\:7d2f\:52a0\:8d77\:6765*)


SetAttributes[{(*\:8ba1\:7b97\:89e3\:6790\:7ed3\:679c\:7684\:51fd\:6570\:4e0d\:4f9d\:8d56\:4e8e\:53c2\:6570\:6b21\:5e8f*)
fad`a,fad`b,fad`\[Delta],
fad`anl,fad`anl`\[Delta]
},Orderless];


(* ::Text:: *)
(*\:6574\:7406\:5206\:6bcd\:4e0a\:7684\:4f20\:64ad\:5b50\:ff0c\:5408\:5e76\:76f8\:540c\:7684\:9879*)


fad`a::usage="\:8bb0\:5f55\:4f20\:64ad\:5b50\:7ed3\:6784\:ff0c\:81ea\:52a8\:5408\:5e76\:4e24\:4e2a\:76f8\:4e58\:7684\:4f20\:64ad\:5b50\:51fd\:6570";
fad`a[x:__Rule]*fad`a[y:__Rule]^:=fad`a[ats[x,y]];
Power[fad`a[x:__Rule],n_]^:=fad`a[ats[n*Association[List[x]]]] (*\:5c06\:4f20\:64ad\:5b50\:7684\:5e42\:6b21\:653e\:5230\:53c2\:6570\:4e2d*)


(* ::Text:: *)
(*\:5f53 DB or DT \:51fa\:73b0\:5728\:5206\:5b50\:4e0a\:65f6\:ff0c\:8f6c\:6362\:6210\:53ef\:4ee5\:8ba1\:7b97\:7684\:5f62\:5f0f\:ff0c*)


fad`\[Delta]::usage="fad`\[Delta] \:53ef\:4ee5\:628afad`b \:8f6c\:6362\:6210 fad`\[Delta]";
fad`b::usage="fad`b \:5c06\:5206\:5b50\:4e0a\:7684DB or DT \:8f6c\:6362\:6210\:53ef\:4ee5\:8ba1\:7b97\:7684 kp \:9879";
fad`b[x:__Rule]*fad`b[y:__Rule]^:=fad`b[ats[x,y]] (*\:81ea\:52a8\:5408\:5e76\:4e24\:4e2a\:76f8\:4e58\:7684\:4f20\:64ad\:5b50\:51fd\:6570*)
Power[fad`b[x:__Rule],n_]^:=fad`b[ats[n*Association[List[x]]]] (*\:5c06\:4f20\:64ad\:5b50\:7684\:5e42\:6b21\:653e\:5230\:53c2\:6570\:4e2d*)
fad`\[Delta][x:__Rule]*fad`\[Delta][y:__Rule]^:=fad`\[Delta][ats[x,y]] (*fad`\[Delta] \:540c\:6837\:9700\:8981\:8fd9\:4e9b\:6027\:8d28*)
Power[fad`\[Delta][x:__Rule],n_]^:=fad`\[Delta][ats[n*Association[List[x]]]] 
fad`\[Delta][y:__Rule]*fad`b[x:__Rule]^:=fad`\[Delta][ats[x,y]];(* fad`\[Delta] \:628afad`b \:8f6c\:6362\:6210 fad`\[Delta] *)


(* ::Text:: *)
(*\:5bf9\:4e8e\:5177\:4f53\:7684\:4f20\:64ad\:5b50\:7ed3\:6784\:ff0c\:7ed9\:51fa\:7ed3\:679c*)


fun`def=<||>;fun`i=1; (*\:5b58\:50a8\:8fd9\:4e9b\:5b9a\:4e49\:ff0c\:65b9\:4fbf\:518d\:6b21\:8c03\:7528*)


(*\:5f53\:4e2d\:95f4\:6001\:91cd\:5b50\:5e42\:6b21\:4e3a\:8d1f\:ff0c\:4e5f\:5c31\:662fDB,DT \:51fa\:73b0\:5728\:5206\:5b50\:4e0a\:65f6\:7684\:516c\:5f0f*)
fun`def[1]:=(
fad`b[{k,m\[Phi]}->a_,{k,\[CapitalLambda]}->b_,{-k+p,Mmd}->c_]:=(
(
fad`b[{k,m\[Phi]}->-1,{k,\[CapitalLambda]}->0,{-k+p,Mmd}->0]+(m\[Phi]^2-Mmd^2+M^2)
-1/y*fad`\[Delta][{k,m\[Phi]}->0,{k,\[CapitalLambda]}->0,{-k+p,Mmd}->0,tykp->1](*salamu\:8bb0\:53f7\:4e2d\:8fd9\:91cc\:6709\:4e2a1/2\:ff0c\:4e0e2ykp\:76f8\:6d88*)
)^-c*fad`b[{k,m\[Phi]}->a,{k,\[CapitalLambda]}->b,{-k+p,Mmd}->0]
)/;And[a>=0,b>=0,a+b>=1,c<=-1]
);
(*\:5f53 D\[Phi] \:5e42\:6b21\:4e3a\:8d1f\:6570\:65f6\:7684\:516c\:5f0f*)
fun`def[2]:=(
fad`b[{k,m\[Phi]}->a_,{k,\[CapitalLambda]}->b_,{-k+p,Mmd}->c_]:=(
(fad`b[{k,m\[Phi]}->0,{k,\[CapitalLambda]}->-1,{-k+p,Mmd}->0]+\[CapitalLambda]l^2)^-a*fad`b[{k,m\[Phi]}->0,{k,\[CapitalLambda]}->b,{-k+p,Mmd}->c]
)/;And[a<=-1,b>=1]
)


fad`anl::usage="fad`anl \:5c06\:4f20\:64ad\:5b50\:7ed3\:6784\:5f0f\:8f6c\:6362\:6210\:89e3\:6790\:8868\:8fbe\:5f0f";


(*\:5f53\:4e2d\:95f4\:6001\:91cd\:5b50\:5e42\:6b21\:4e3a0\:65f6\:7684\:516c\:5f0f\:ff0c\:4e5f\:5c31\:662f\:53d1\:6563\:79ef\:5206\:7684\:516c\:5f0f\:ff0c\:7ed3\:679c\:6b63\:6bd4\:4e8e\[Delta][y]*)
fun`def[3]:=(
fad`anl[{k,m\[Phi]}->a_,{k,\[CapitalLambda]}->0,{-k+p,Mmd}->0]:=(((2\[Pi]*I)*\[Delta][y])/Factorial[a-1]*D[Log[(kt^2+\[CapitalOmega]\[Phi])/s\[Mu]^2],{\[CapitalOmega]\[Phi],a-1}])/;And[a>=1]
);
fun`def[4]:=(
fad`anl[{k,m\[Phi]}->0,{k,\[CapitalLambda]}->b_,{-k+p,Mmd}->0]:=(((2\[Pi]*I)*\[Delta][y])/Factorial[b-1]*D[Log[(kt^2+\[CapitalOmega]\[CapitalLambda])/s\[Mu]^2],{\[CapitalOmega]\[CapitalLambda],b-1}])/;And[b>=1]
);
fun`def[5]:=(
fad`anl[{k,m\[Phi]}->a_,{k,\[CapitalLambda]}->b_,{-k+p,Mmd}->0]:=(((1-z)^(a-1) z^(b-1) (2\[Pi]*I)*\[Delta][y])/(Factorial[a-1]*Factorial[b-1])*D[1/D\[Phi]\[CapitalLambda],{D\[Phi]\[CapitalLambda],a+b-2}])/;And[a>=1,b>=1]
);


(*\:7279\:6b8a\:53d1\:6563\:79ef\:5206\:7684\:516c\:5f0f\:ff0c\:5206\:5b50\:4e0a\:4e3a y*k.p],\:7ed3\:679c\:6b63\:6bd4\:4e8e\[Delta][y]*)
fun`def[6]:=(
fad`anl`\[Delta][{k,m\[Phi]}->a_,{k,\[CapitalLambda]}->b_,{-k+p,Mmd}->0,tykp->l_]:=((Factorial[l]*(1-z)^(a-1) z^(b-1) (2\[Pi]*I)*\[Delta][y])/(Factorial[a-1]*Factorial[b-1])*D[Log[D\[Phi]\[CapitalLambda]/s\[Mu]^2],{D\[Phi]\[CapitalLambda],a+b-l-1}])/;And[a>=1,b>=1,l>=1]
);
fun`def[7]:=(
fad`anl`\[Delta][{k,m\[Phi]}->a_,{k,\[CapitalLambda]}->0,{-k+p,Mmd}->0,tykp->l_]:=((Factorial[l]*(2\[Pi]*I)*\[Delta][y])/Factorial[a-1]*D[Log[(kt^2+\[CapitalOmega]\[Phi])/s\[Mu]^2],{\[CapitalOmega]\[Phi],a-l-1}])/;And[a>=1,l>=1];
fad`anl`\[Delta][{k,m\[Phi]}->0,{k,\[CapitalLambda]}->b:_,{-k+p,Mmd}->0,tykp->l_]:=((Factorial[l]*(2\[Pi]*I)*\[Delta][y])/Factorial[b-1]*D[Log[(kt^2+\[CapitalOmega]\[CapitalLambda])/s\[Mu]^2],{\[CapitalOmega]\[CapitalLambda],b-l-1}])/;And[b>=1,l>=1];
);


fun`def[8]:=(
(*\:5f53\:4e2d\:95f4\:6001\:91cd\:5b50\:5e42\:6b21\:4e3a\:4e0d\:4e3a\:96f6\:65f6\:7684\:516c\:5f0f\:ff0c\:4e5f\:5c31\:662f\:6536\:655b\:79ef\:5206\:7684\:516c\:5f0f\:ff0c\:53ea\:8981a+b\[GreaterEqual]1\:5373\:53ef\:ff0c\:5373\:5b58\:5728\:4e00\:4e2ak^2-m\[Phi]^2\:6216k^2-\[CapitalLambda]^2\:5373\:53ef*)
fad`anl[{k,m\[Phi]}->a_,{k,\[CapitalLambda]}->b_,{-k+p,Mmd}->c_]:=Module[{ghost}, 
(
((-2\[Pi]*I)/(yl*Factorial[c-1])*D[1/((D\[Phi]md-y/yl*ghost)^a (D\[CapitalLambda]md-y/yl*ghost)^b),{ghost,c-1}])/.ghost->0
)/;And[a+b>=1,a>=0,b>=0,c>=1]
]
)


fun`def[9]:=(
(*\:4f20\:64ad\:5b50\:7684\:6392\:7248\:ff0c\:5b9a\:4e49\:4e00\:4e2a\:6613\:8bfb\:7684\:6837\:5f0f*)
Format[fad`a[{k,\[CapitalLambda]}->a_,{k,m\[Phi]}->b_,{p-k,mmd_}->c_],TraditionalForm]:=fpp[D\[CapitalLambda]^(-a)*D\[Phi]^(-b)*Dmd^(-c)];
Format[fad`b[{k,\[CapitalLambda]}->a_,{k,m\[Phi]}->b_,{p-k,mmd_}->c_],TraditionalForm]:=fpp[D\[CapitalLambda]^(-a)*D\[Phi]^(-b)*Dmd^(-c)];
Format[fad`\[Delta][tykp->y_,{k,\[CapitalLambda]}->a_,{k,m\[Phi]}->b_,{-k+p,mmd_}->c_],TraditionalForm]:=fpp[tykp^y*D\[CapitalLambda]^(-a)*D\[Phi]^(-b)*Dmd^(-c)];
)


prg::usage="ppr[a:_,b:_,c:_] \:8f93\:5165 D\[CapitalLambda]^a,D\[Phi]^b,DB^c\:7684\:5feb\:6377\:51fd\:6570";
fun`def[10]:=(
prg[a:_,b:_,c:_]:=fad`a[{k, \[CapitalLambda]}->a,{k,m\[Phi]}->b,{-k+p,Mmd}->c];(*\:8f93\:5165\:4f20\:64ad\:5b50\:7684\:5feb\:6377\:51fd\:6570*)
)


Values[fun`def]; (*\:5bf9 octet \:5b9a\:4e49 \:51fd\:6570\:5e93\:89c4\:5219*)


(* ::Chapter:: *)
(*octet splitting function*)


(* ::Section:: *)
(*a*)


(* ::Text:: *)
(*\:5206\:88c2\:51fd\:6570\:4e2d\:7684\:666e\:901a\:6570\:5b57\:90e8\:5206*)


prfactor=\[Pi]/(4M*eq\[Phi]);
splt[{"a","rbw","\[Phi]B"},{"cls"}]=(prfactor*(I*eq\[Phi]*Cmd\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1*M/(kp/y)*
fad[
{k+q,\[CapitalLambda]}->2,{k,\[CapitalLambda]}->2,{k+q,m\[Phi]}->1,{k,m\[Phi]}->1,{p-k,Mmd}->1
]*(2kp+qp)
)//ScalarProductExpand


(* ::Text:: *)
(*\:4f7f\:7528 FeynCalc \:5316\:7b80\:65cb\:91cf\:7ed3\:6784*)


splt[{"a","rbw","\[Phi]B"},{"spr"}]=DiracTrace[
(GS[p]+M) . GS[k] . GA5 . (GS[p1-k]+Mmd) . GS[k] . GA5
]//DiracSimplify


splt[{"a","rbw","\[Phi]B"}]=Collect2[
Expand[
splt[{"a","rbw","\[Phi]B"},{"cls"}]*splt[{"a","rbw","\[Phi]B"},{"spr"}]/.FCI[{
SP[k,k]->fad[{k,m\[Phi]}->-1]+m\[Phi]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*),
SP[k,p]->1/2(fad[{k,m\[Phi]}->-1]-fad[{p-k,Mmd}->-1]+m\[Phi]^2-Mmd^2+M^2),(*\:5bf9k.p\:4f5c\:66ff\:6362*)
pp->kp/y
}]
],
fad`a[__]
]


(* ::Subsection:: *)
(*to analytic*)


jcb=1/2;prfactor=\[Pi]/(4M*eq\[Phi]); (*\:53ea\:662f\:4e3a\:4e86\:63d0\:9192\:524d\:9762\:7528\:8fc7\:8fd9\:4e2a\:56e0\:5b50*)prfactor=(4\[Pi]*f)^2/(Cmd\[Phi]^2*Ml^2);
anl`rule[x_]:=(Expand[x/.{fad`a->fad`b}]/.{fad`b->fad`anl,fad`\[Delta]->fad`anl`\[Delta]}
/.{Condition[Power[y,n_.]*\[Delta][y],n>=-2]:>0}(*\:53bb\:6389\:7b49\:4e8e\:96f6\:7684\:9879,\:5373 y*\[Delta][y]*)
/.{Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y}
);
Collect2[anl`rule[
jcb*prfactor*splt[{"a","rbw","\[Phi]B"}]
],
{D\[CapitalLambda]md,D\[Phi]md,D\[Phi]\[CapitalLambda]}
]


(* ::Section:: *)
(*c*)


(* ::Text:: *)
(*\:5206\:88c2\:51fd\:6570\:4e2d\:7684\:666e\:901a\:6570\:5b57\:90e8\:5206*)


prfactor=\[Pi]/(4M*eq\[Phi]);
splt[{"c","KR","B"},{"cls"}]=prfactor*((-I)*eq\[Phi]*Cmd\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1*M/(kp/y)*
fad[
{k,\[CapitalLambda]}->4,{k,m\[Phi]}->1,{p1-k,Mmd}->1
]//ScalarProductExpand


(* ::Text:: *)
(*\:4f7f\:7528 FeynCalc \:5316\:7b80\:65cb\:91cf\:7ed3\:6784*)


splt[{"c","KR","B"},{"spr"}]=DiracSimplify[
DiracTrace[
(GS[p]+M) . (
GA["+"] . GA5 . (GS[p-k]+Mmd) . GS[k] . GA5+GS[k] . GA5 . (GS[p-k]+Mmd) . GA["+"] . GA5
)
]
]/.{FCI[pp->kp/y]}


splt[{"c","KR","B"}]=Collect2[
Expand[
splt[{"c","KR","B"},{"cls"}]*splt[{"c","KR","B"},{"spr"}]/.FCI[{
SP[k,k]->fad[{k,m\[Phi]}->-1]+m\[Phi]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*),
SP[k,p]->1/2(fad[{k,m\[Phi]}->-1]-fad[{p-k,Mmd}->-1]+m\[Phi]^2-Mmd^2+M^2),(*\:5bf9k.p\:4f5c\:66ff\:6362*)
pp->kp/y
}]
],
fad`a[__]
]


(* ::Subsection:: *)
(*to analytic*)


prfactor=\[Pi]/(4M*eq\[Phi]);(*\:53ea\:662f\:4e3a\:4e86\:63d0\:9192\:524d\:9762\:7528\:8fc7\:8fd9\:4e2a\:56e0\:5b50*)
jcb=1/2;prfactor=(-1) (4\[Pi]*f)^2/(Cmd\[Phi]^2*Ml^2);
anl`rule[x_]:=(Expand[x/.{fad`a->fad`b}]/.{fad`b->fad`anl,fad`\[Delta]->fad`anl`\[Delta]}
/.{Condition[Power[y,n_.]*\[Delta][y],n>=-2]:>0}(*\:53bb\:6389\:7b49\:4e8e\:96f6\:7684\:9879,\:5373 y*\[Delta][y]*)
/.{Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y}
);
Collect2[anl`rule[
jcb*prfactor*splt[{"c","KR","B"}]
],
{D\[CapitalLambda]md,D\[Phi]md,D\[Phi]\[CapitalLambda]}
]


(* ::Section:: *)
(*d*)


(* ::Text:: *)
(*\:5206\:88c2\:51fd\:6570\:4e2d\:7684\:666e\:901a\:6570\:5b57\:90e8\:5206*)


prfactor=\[Pi]/(4M*eq\[Phi]);
splt[{"d","\[Delta]KR","B"},{"cls"}]=prfactor*(I*eq\[Phi]*Cmd\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1*M/(kp/y)*
fad[
{k,\[CapitalLambda]}->5,{k,m\[Phi]}->1,{p1-k,Mmd}->1
]//ScalarProductExpand


(* ::Text:: *)
(*\:4f7f\:7528 FeynCalc \:5316\:7b80\:65cb\:91cf\:7ed3\:6784*)


splt[{"d","\[Delta]KR","B"},{"spr"}]=DiracSimplify[
DiracTrace[
(GS[p]+M) . (8FV[k,"+"]) . GS[k] . GA5 . (GS[p-k]+Mmd) . GS[k] . GA5
]
]/.{FCI[pp->kp/y]}


splt[{"d","\[Delta]KR","B"}]=Collect2[
Expand[
splt[{"d","\[Delta]KR","B"},{"cls"}]*splt[{"d","\[Delta]KR","B"},{"spr"}]/.FCI[{
SP[k,k]->fad[{k,m\[Phi]}->-1]+m\[Phi]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*),
SP[k,p]->1/2(fad[{k,m\[Phi]}->-1]-fad[{p-k,Mmd}->-1]+m\[Phi]^2-Mmd^2+M^2),(*\:5bf9k.p\:4f5c\:66ff\:6362*)
pp->kp/y
}]
],
fad`a[__]
]


(* ::Subsection:: *)
(*to analytic*)


prfactor=\[Pi]/(4M*eq\[Phi]); (*\:53ea\:662f\:4e3a\:4e86\:63d0\:9192\:524d\:9762\:7528\:8fc7\:8fd9\:4e2a\:56e0\:5b50*)
jcb=1/2;prfactor=(-1) (4\[Pi]*f)^2/(Cmd\[Phi]^2*Ml^2);
anl`rule[x_]:=(Expand[x/.{fad`a->fad`b}]/.{fad`b->fad`anl,fad`\[Delta]->fad`anl`\[Delta]}
/.{Condition[Power[y,n_.]*\[Delta][y],n>=-2]:>0}(*\:53bb\:6389\:7b49\:4e8e\:96f6\:7684\:9879,\:5373 y*\[Delta][y]*)
/.{Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y}
);
Collect2[anl`rule[
jcb*prfactor*splt[{"d","\[Delta]KR","B"}]
],
{D\[CapitalLambda]md,D\[Phi]md,D\[Phi]\[CapitalLambda]}
]


(* ::Section:: *)
(*e*)


(* ::Text:: *)
(*\:5206\:88c2\:51fd\:6570\:4e2d\:7684\:666e\:901a\:6570\:5b57\:90e8\:5206*)


prfactor=\[Pi]/(4M*eq\[Phi]);
splt[{"e","tad","\[Phi]"},{"cls"}]=prfactor*((-I)*eq\[Phi]*C\[Phi]\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1*M/(kp/y)*
fad[
{k,\[CapitalLambda]}->4,{k,m\[Phi]}->1,{-k+p,Mmd}->0
]//ScalarProductExpand


(* ::Text:: *)
(*\:4f7f\:7528 FeynCalc \:5316\:7b80\:65cb\:91cf\:7ed3\:6784*)


splt[{"e","tad","\[Phi]"},{"spr"}]=DiracSimplify[
DiracTrace[
(GS[p]+M) . GA["+"]
]
]/.{FCI[pp->kp/y]}


splt[{"e","tad","\[Phi]"}]=Collect2[
Expand[
splt[{"e","tad","\[Phi]"},{"cls"}]*splt[{"e","tad","\[Phi]"},{"spr"}]/.FCI[{
SP[k,k]->fad[{k,m\[Phi]}->-1]+m\[Phi]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*),
SP[k,p]->1/2(fad[{k,m\[Phi]}->-1]-fad[{p-k,Mmd}->-1]+m\[Phi]^2-Mmd^2+M^2),(*\:5bf9k.p\:4f5c\:66ff\:6362*)
pp->kp/y
}]
],
fad`a[__]
]


(* ::Subsection:: *)
(*to analytic*)


prfactor=\[Pi]/(4M*eq\[Phi]); (*\:53ea\:662f\:4e3a\:4e86\:63d0\:9192\:524d\:9762\:7528\:8fc7\:8fd9\:4e2a\:56e0\:5b50*)
jcb=1/2;prfactor=(-1) (4\[Pi]*f)^2/(C\[Phi]\[Phi]^2*Ml^2);
anl`rule[x_]:=(Expand[x/.{fad`a->fad`b}]/.{fad`b->fad`anl,fad`\[Delta]->fad`anl`\[Delta]}
/.{Condition[Power[y,n_.]*\[Delta][y],n>=-2]:>0}(*\:53bb\:6389\:7b49\:4e8e\:96f6\:7684\:9879,\:5373 y*\[Delta][y]*)
/.{Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y}
);
Collect2[anl`rule[
jcb*prfactor*splt[{"e","tad","\[Phi]"}]
],
{D\[CapitalLambda]md,D\[Phi]md,D\[Phi]\[CapitalLambda]}
]


(* ::Section:: *)
(*f*)


(* ::Text:: *)
(*\:5206\:88c2\:51fd\:6570\:4e2d\:7684\:666e\:901a\:6570\:5b57\:90e8\:5206*)


prfactor=\[Pi]/(4M*eq\[Phi]);
splt[{"f","bub","\[Phi]"},{"cls"}]=prfactor*(I*eq\[Phi]*C\[Phi]\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1*M/(kp/y)*
fad[
{k,\[CapitalLambda]}->4,{k,m\[Phi]}->2,{-k+p,Mmd}->0
]//ScalarProductExpand


(* ::Text:: *)
(*\:4f7f\:7528 FeynCalc \:5316\:7b80\:65cb\:91cf\:7ed3\:6784*)


splt[{"f","bub","\[Phi]"},{"spr"}]=DiracSimplify[
DiracTrace[
(GS[p]+M) . (2FV[k,"+"]) . GS[k]
]
]/.{FCI[pp->kp/y]}


splt[{"f","bub","\[Phi]"}]=Collect2[
Expand[
splt[{"f","bub","\[Phi]"},{"cls"}]*splt[{"f","bub","\[Phi]"},{"spr"}]/.FCI[{
SP[k,k]->fad[{k,m\[Phi]}->-1]+m\[Phi]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*),
SP[k,p]->1/2(fad[{k,m\[Phi]}->-1]-fad[{p-k,Mmd}->-1]+m\[Phi]^2-Mmd^2+M^2),(*\:5bf9k.p\:4f5c\:66ff\:6362*)
pp->kp/y
}]
],
fad`a[__]
]


(* ::Subsection:: *)
(*to analytic*)


prfactor=\[Pi]/(4M*eq\[Phi]); (*\:53ea\:662f\:4e3a\:4e86\:63d0\:9192\:524d\:9762\:7528\:8fc7\:8fd9\:4e2a\:56e0\:5b50*)
jcb=1/2;prfactor=(4\[Pi]*f)^2/(C\[Phi]\[Phi]^2*Ml^2);
anl`rule[x_]:=(Expand[x/.{fad`a->fad`b}]/.{fad`b->fad`anl,fad`\[Delta]->fad`anl`\[Delta]}
/.{Condition[Power[y,n_.]*\[Delta][y],n>=-2]:>0}(*\:53bb\:6389\:7b49\:4e8e\:96f6\:7684\:9879,\:5373 y*\[Delta][y]*)
/.{Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y}
);
Collect2[anl`rule[
jcb*prfactor*splt[{"f","bub","\[Phi]"}]
],
{D\[CapitalLambda]md,D\[Phi]md,D\[Phi]\[CapitalLambda]}
]


(* ::Section:: *)
(*g*)


(* ::Text:: *)
(*\:5206\:88c2\:51fd\:6570\:4e2d\:7684\:666e\:901a\:6570\:5b57\:90e8\:5206*)


prfactor=\[Pi]/(4M*eq\[Phi]);
splt[{"g","\[Delta]tad","\[Phi]"},{"cls"}]=prfactor*(I*eq\[Phi]*C\[Phi]\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1*M/(kp/y)*
fad[
{k,\[CapitalLambda]}->5,{k,m\[Phi]}->1,{-k+p,Mmd}->0
]//ScalarProductExpand


(* ::Text:: *)
(*\:4f7f\:7528 FeynCalc \:5316\:7b80\:65cb\:91cf\:7ed3\:6784*)


splt[{"g","\[Delta]tad","\[Phi]"},{"spr"}]=DiracSimplify[
DiracTrace[
(GS[p]+M) . (8FV[k,"+"]) . GS[k]
]
]/.{FCI[pp->kp/y]}


splt[{"g","\[Delta]tad","\[Phi]"}]=Collect2[
Expand[
splt[{"g","\[Delta]tad","\[Phi]"},{"cls"}]*splt[{"g","\[Delta]tad","\[Phi]"},{"spr"}]/.FCI[{
SP[k,k]->fad[{k,m\[Phi]}->-1]+m\[Phi]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*),
SP[k,p]->1/2(fad[{k,m\[Phi]}->-1]-fad[{p-k,Mmd}->-1]+m\[Phi]^2-Mmd^2+M^2),(*\:5bf9k.p\:4f5c\:66ff\:6362*)
pp->kp/y
}]
],
fad`a[__]
]


(* ::Subsection:: *)
(*to analytic*)


prfactor=\[Pi]/(4M*eq\[Phi]); (*\:53ea\:662f\:4e3a\:4e86\:63d0\:9192\:524d\:9762\:7528\:8fc7\:8fd9\:4e2a\:56e0\:5b50*)
jcb=1/2;prfactor=(-1) (4\[Pi]*f)^2/(C\[Phi]\[Phi]^2*Ml^2);
anl`rule[x_]:=(Expand[x/.{fad`a->fad`b}]/.{fad`b->fad`anl,fad`\[Delta]->fad`anl`\[Delta]}
/.{Condition[Power[y,n_.]*\[Delta][y],n>=-2]:>0}(*\:53bb\:6389\:7b49\:4e8e\:96f6\:7684\:9879,\:5373 y*\[Delta][y]*)
/.{Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y}
);
Collect2[anl`rule[
jcb*prfactor*splt[{"g","\[Delta]tad","\[Phi]"}]
],
{D\[CapitalLambda]md,D\[Phi]md,D\[Phi]\[CapitalLambda]}
]


(* ::Section:: *)
(*decuplet initial*)


(* ::Text:: *)
(*\:5237\:65b0\:51fd\:6570\:5e93*)


(*decuplet \:90e8\:5206\:547d\:540d*)
M=M; (*\:521d\:672b\:6001\:6838\:5b50\:8d28\:91cf*)f=f;
Mmd=Subscript["M","T"]; (*in\:8868\:793a\:4e2d\:95f4\:ff0c\:5373\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf\:ff0c\:524d\:51e0\:4e2a\:56fe\:662f\:516b\:91cd\:6001*)
\[CapitalDelta]=Subscript["\[CapitalDelta]","T"]; (*MT-M*)
Ml=Subscript[OverBar["M"],"T"]; (*M+MT\:ff0c\:8d28\:91cf\:4e4b\:548c*)
eq\[Phi]=Subsuperscript["e","q","\[Phi]"]; (*\:8026\:5408\:5e38\:6570*)
Cmd\[Phi]=Subsuperscript["C","T","\[Phi]"]; 
eqmd=Subsuperscript["e","q","T"]; 
(*\:4f20\:64ad\:5b50*)
D\[Phi]=Subscript["D","\[Phi]"];
D\[CapitalLambda]=Subscript["D","\[CapitalLambda]"]; 
Dmd=Subscript["D","T"];
(*\:4f20\:64ad\:5b50\:7684\:7ec4\:5408*)
D\[Phi]\[CapitalLambda]=Subscript["D","\[Phi]\[CapitalLambda]"];
D\[Phi]md=Subscript["D","\[Phi]T"]; 
D\[CapitalLambda]md=Subscript["D","\[CapitalLambda]T"];


Values[fun`def]; (*\:5bf9 decuplet \:8865\:5145\:51fd\:6570\:5e93\:89c4\:5219*)


(* ::Section:: *)
(*h*)


(* ::Text:: *)
(*\:5206\:88c2\:51fd\:6570\:4e2d\:7684\:666e\:901a\:6570\:5b57\:90e8\:5206*)


prfactor=\[Pi]/(4M*eq\[Phi]);
splt[{"h","rbw","\[Phi]T"},{"cls"}]=prfactor*(I*eq\[Phi]*Cmd\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1*M/(kp/y)*
fad[
{k+q,\[CapitalLambda]}->2,{k,\[CapitalLambda]}->2,{k+q,m\[Phi]}->1,{k,m\[Phi]}->1,{p-k,Mmd}->1
]//ScalarProductExpand


(* ::Text:: *)
(*\:4f7f\:7528 FeynCalc \:5316\:7b80\:65cb\:91cf\:7ed3\:6784*)


st[p:_,{\[Alpha]_,\[Beta]_}]:=(-(GS[p]+Mmd)) . (MT[\[Alpha],\[Beta]]-1/3 GA[\[Alpha]] . GA[\[Beta]]-(GA[\[Alpha]] . FV[p,\[Beta]]-GA[\[Beta]] . FV[p,\[Alpha]])/(3Mmd)-(2FV[p,\[Alpha]]*FV[p,\[Beta]])/(3Mmd^2))
\[CapitalTheta][a:_,b:_]:=I*DiracSigma[GA[a],GA[b]]


splt[{"h","rbw","\[Phi]T"},{"spr"}]=Simplify[DiracSimplify[
DiracTrace[
FV[2k+q,"+"] . (GS[p]+M) . FV[k,\[Alpha]] . \[CapitalTheta][\[Alpha],\[Beta]] . st[p-k,{\[Beta],\[Rho]}] . \[CapitalTheta][\[Rho],\[Sigma]] . FV[k,\[Sigma]]
]
]
]


splt[{"h","rbw","\[Phi]T"}]=Collect2[
Expand[
splt[{"h","rbw","\[Phi]T"},{"cls"}]*splt[{"h","rbw","\[Phi]T"},{"spr"}]/.FCI[{
SP[k,k]->fad[{k,m\[Phi]}->-1]+m\[Phi]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*),
SP[k,p]->1/2(fad[{k,m\[Phi]}->-1]-fad[{p-k,Mmd}->-1]+m\[Phi]^2-Mmd^2+M^2),(*\:5bf9k.p\:4f5c\:66ff\:6362*)
pp->kp/y
}]
],
fad`a[__]
]


(* ::Subsection:: *)
(*to analytic*)


anl`rule[x_]:=(Expand[x/.{fad`a->fad`b}]/.{fad`b->fad`anl,fad`\[Delta]->fad`anl`\[Delta]}
/.{Condition[Power[y,n_.]*\[Delta][y],n>=-2]:>0}(*\:53bb\:6389\:7b49\:4e8e\:96f6\:7684\:9879,\:5373 y*\[Delta][y]*)
/.{Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y}
);
prp`rule[x_]:=(Expand[x/.{fad`a->fad`b}]
/.{Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y}
);
jcb=1/2;prfactor=\[Pi]/(4M*eq\[Phi]); (*\:53ea\:662f\:4e3a\:4e86\:63d0\:9192\:524d\:9762\:7528\:8fc7\:8fd9\:4e2a\:56e0\:5b50*)
prfactor=(6Ml^2*Mmd^2)/\[CapitalLambda]l^8 (4\[Pi]*f)^2/(Cmd\[Phi]^2 Ml^2);
Collect2[anl`rule[
jcb*prfactor*splt[{"h","rbw","\[Phi]T"}]
],
{D\[CapitalLambda]md,D\[Phi]md,D\[Phi]\[CapitalLambda]}
]


(* ::Section:: *)
(*i*)


(* ::Text:: *)
(*\:5206\:88c2\:51fd\:6570\:4e2d\:7684\:666e\:901a\:6570\:5b57\:90e8\:5206*)


(* ::DisplayFormula:: *)
(*\[Gamma]^\[Mu]\[Nu]\[Alpha]=1/2 {(1/2)[\[Gamma]\[Mu],\[Gamma]\[Nu]],\[Gamma]\[Alpha]}=-I/2 {\[Sigma]^\[Mu]\[Nu],\[Gamma]\[Alpha]}*)


prfactor=\[Pi]/(4M*eqmd);
splt[{"i","rbw","T\[Phi]"},{"cls"}]=prfactor*(I*eqmd*Cmd\[Phi]^2)/((2\[Pi])^4*f^2)*\[CapitalLambda]l^8/1*M/(kp/y)*
fad[
{k,\[CapitalLambda]}->4,{k,m\[Phi]}->1,{p2-k,Mmd}->1,{p1-k,Mmd}->1
]//ScalarProductExpand


(* ::Text:: *)
(*\:4f7f\:7528 FeynCalc \:5316\:7b80\:65cb\:91cf\:7ed3\:6784*)


st[p:_,{\[Alpha]_,\[Beta]_}]:=(-(GS[p]+Mmd)) . (MT[\[Alpha],\[Beta]]-1/3 GA[\[Alpha]] . GA[\[Beta]]-(GA[\[Alpha]] . FV[p,\[Beta]]-GA[\[Beta]] . FV[p,\[Alpha]])/(3Mmd)-(2FV[p,\[Alpha]]*FV[p,\[Beta]])/(3Mmd^2))
\[CapitalTheta][a:_,b:_]:=I*DiracSigma[GA[a],GA[b]]
\[Gamma][a:_,b:_,c:_]:=(-I/2)*AntiCommutator[DiracSigma[GA[a],GA[b]],GA[c]]


splt[{"i","rbw","T\[Phi]"},{"spr"}]=Simplify[DiracSimplify[
DiracTrace[
(GS[p]+M) . FV[k,\[Alpha]] . \[CapitalTheta][\[Alpha],\[Beta]] . st[p2-k,{\[Beta],\[Theta]}] . \[Gamma][\[Theta],\[Nu],"+"] . st[p1-k,{\[Nu],\[Rho]}] . \[CapitalTheta][\[Rho],\[Sigma]] . FV[k,\[Sigma]]
]
]
]


splt[{"i","rbw","T\[Phi]"}]=Collect2[
Expand[
splt[{"i","rbw","T\[Phi]"},{"cls"}]*splt[{"i","rbw","T\[Phi]"},{"spr"}]/.FCI[{
SP[k,k]->fad[{k,m\[Phi]}->-1]+m\[Phi]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*),
SP[k,p]->1/2(fad[{k,m\[Phi]}->-1]-fad[{p-k,Mmd}->-1]+m\[Phi]^2-Mmd^2+M^2),(*\:5bf9k.p\:4f5c\:66ff\:6362*)
pp->kp/y
}]
],
fad`a[__]
]


(* ::Subsection:: *)
(*compare*)


(*\:548c salamu \:7684\:7ed3\:679c\:4f5c\:5bf9\:6bd4*)
prfactor=(2\[Pi])^4/\[Pi] (6Mmd^2*f^2)/(I*Cmd\[Phi]^2*\[CapitalLambda]l^8);
fylc=Collect2[
prfactor*Expand[
splt[{"i","rbw","T\[Phi]"}
]
]/.{Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y},
fad`a[__]
]


salamu=(yl*(Ml^2-m\[Phi]^2)^2(\[CapitalDelta]^2-m\[Phi]^2)prg[4,1,2]-(Ml^2-m\[Phi]^2)((y-2)Mmd^2-2y*M*Mmd+(y+2)(M^2-m\[Phi]^2))*prg[4,1,1]
+yl((SP[k,k]+m\[Phi]^2)(2Ml^2+\[CapitalDelta]^2-m\[Phi]^2)-Ml^4-2Ml^2*\[CapitalDelta]^2-SP[k,k]^2)*prg[4,0,2]
+((y+2)(2M^2-m\[Phi]^2-SP[k,k])+(y*Mmd+2M)2Mmd)*prg[4,0,1]
+(Ml^2+2y*Ml*M-2y*SP[k,p]+3SP[k,k])*prg[4,1,0]
);
salamu1=Collect2[
Expand[
salamu/.Identity[{
SP[k,k]->fad[{k,m\[Phi]}->-1]+m\[Phi]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*),
SP[k,p]->1/2(fad[{k,m\[Phi]}->-1]-fad[{p-k,Mmd}->-1]+m\[Phi]^2-Mmd^2+M^2)(*\:5bf9k.p\:4f5c\:66ff\:6362*)
}]
]/.{Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y},
fad`a[__]
]
(salamu1-fylc)


(* ::Subsection:: *)
(*to analytic*)


jcb=1/2;prfactor=\[Pi]/(4M*eqmd); (*\:53ea\:662f\:4e3a\:4e86\:63d0\:9192\:524d\:9762\:7528\:8fc7\:8fd9\:4e2a\:56e0\:5b50*)prfactor=(6Ml^2*Mmd^2)/\[CapitalLambda]l^8 (4\[Pi]*f)^2/(Cmd\[Phi]^2 Ml^2);
(*\:8f6c\:6362\:5230\:53ef\:4ee5\:8ba1\:7b97\:7684 integral basis*)
prp`rule[x_]:=(Expand[x/.{fad`a->fad`b}]
/.{Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y}
);
Collect2[prp`rule[
jcb*prfactor*splt[{"i","rbw","T\[Phi]"}]
],
{fad`b[__]}
]


jcb=1/2;prfactor=\[Pi]/(4M*eqmd); (*\:53ea\:662f\:4e3a\:4e86\:63d0\:9192\:524d\:9762\:7528\:8fc7\:8fd9\:4e2a\:56e0\:5b50*)prfactor=(6Ml^2*Mmd^2)/\[CapitalLambda]l^8 (4\[Pi]*f)^2/(Cmd\[Phi]^2 Ml^2);
anl`rule[x_]:=(Expand[x/.{fad`a->fad`b}]/.{fad`b->fad`anl,fad`\[Delta]->fad`anl`\[Delta]}
/.{Condition[Power[y,n_.]*\[Delta][y],n>=-2]:>0}(*\:53bb\:6389\:7b49\:4e8e\:96f6\:7684\:9879,\:5373 y*\[Delta][y]*)
/.{Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y}
);
temp=Collect2[anl`rule[
jcb*prfactor*splt[{"i","rbw","T\[Phi]"}]
],
{D\[CapitalLambda]md,D\[Phi]md,D\[Phi]\[CapitalLambda]}
]


(* ::Subsection:: *)
(*check*)


(* ::Input:: *)
(*(*\:68c0\:67e5\:662f\:5426\:548c salamu \:7684\:7ed3\:679c\:4e00\:81f4\:ff0c\:5e38\:7528\:7684\:5b9a\:4e49\:548c\:53d8\:6362\:89c4\:5219*)*)
(*rule={*)
(*Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y,\[CapitalLambda]^2->\[CapitalLambda]l^2+m\[Phi]^2*)
(*};*)
(*D\[CapitalLambda]T=(kt^2+y*Mmd^2-y*yl*M^2+yl*(\[CapitalLambda]l^2+m\[Phi]^2))/(-yl)/.rule;*)
(*D\[Phi]T=(kt^2+y*Mmd^2-y*yl*M^2+yl*m\[Phi]^2)/(-yl)/.rule;*)


(* ::Input:: *)
(*(* on1,Overscript[y, _]Subsuperscript[D, \[CapitalLambda]T, 4]Subsuperscript[D, \[Phi]T, 2] *)*)
(*feyn=y (Ml^2-m\[Phi]^2)^2(\[CapitalDelta]^2-m\[Phi]^2);*)
(*on1=y (Ml^2-m\[Phi]^2)^2(\[CapitalDelta]^2-m\[Phi]^2);*)
(*feyn-on1*)


(* ::Input:: *)
(*(* on2+off,Overscript[y, _]Subsuperscript[D, \[CapitalLambda]T, 4]Subscript[D, \[Phi]T] *)*)
(*feyn=(Ml^2-m\[Phi]^2)(2\[CapitalDelta]*Ml+y*m\[Phi]^2+2m\[Phi]^2-y \[CapitalDelta]^2);*)
(*on2=y(Ml^2-m\[Phi]^2)(-Ml^2+3m\[Phi]^2-2\[CapitalDelta]^2);*)
(*off=(-2)(Ml^2-m\[Phi]^2)(yl(M^2-m\[Phi]^2)-(1+y)Mmd^2);*)
(*feyn-(on2+off)/.rule//Expand*)


(* ::Input:: *)
(*(*Overscript[y, _]Subsuperscript[D, \[CapitalLambda]T, 4] on`end,off`end,\[Delta]`off,Overscript[y, _]^4Subsuperscript[D, \[CapitalLambda]T, 5]Subscript[D, \[Phi]T]*)(*\:63d0\:53d6\:7ed3\:679c\:4e2d\:4e0d\:540c\:5e42\:6b21\:7684\:7cfb\:6570*)*)
(*coes=Association[CoefficientRules[temp,{D\[CapitalLambda]md^(-1),D\[Phi]md^(-1),D\[Phi]\[CapitalLambda]^(-1)}]];*)


(* ::Input:: *)
(*(*salamu \:6587\:7ae0\:4e2d\:7684\:7ed3\:679c\:ff0c\:8f93\:5165\:7684\:65f6\:5019\:ff0c\:63d0\:53d6\:51fa\:4e86\:4e00\:4e2a Overscript[\[CapitalLambda], _]^8/(6Subsuperscript[M, T, 2]*Subsuperscript[Overscript[M, _], T, 2]) *)*)
(*feyn1=coes[{4,0,0}]*D\[CapitalLambda]T^-4/.rule;*)
(*feyn2=coes[{3,0,0}]*D\[CapitalLambda]T^-3/.rule;*)
(*feyn3=coes[{5,1,0}]*D\[CapitalLambda]T^-5 D\[Phi]T^-1/.rule;*)
(*feyn4=coes[{5,0,0}]*D\[CapitalLambda]T^-5/.rule;*)
(*on`end=(-y)/yl((\[CapitalLambda]l^2-2Ml^2+3m\[Phi]^2-\[CapitalDelta]^2)/(D\[CapitalLambda]T^4)+1/D\[CapitalLambda]T^3)/.rule;*)
(*off`end=(-1)(kt^2+yl^2*M^2+yl(Ml^2-m\[Phi]^2)-Mmd^2)/(yl D\[CapitalLambda]T^4)/.rule;*)
(*\[Delta]`off=y (kt^2+(y M-Ml)^2)^2(kt^2+(y M+\[CapitalDelta])^2)/(yl^4D\[CapitalLambda]T^5 D\[Phi]T)/.rule;*)
(*((1-y)^4 D\[CapitalLambda]T^5 D\[Phi]T)( *)
(*feyn1+feyn2+feyn3+feyn4*)
(*-on`end-(-2)off`end-4\[Delta]`off*)
(*)//Simplify*)


(* ::Section:: *)
(*j*)


(* ::Text:: *)
(*\:5206\:88c2\:51fd\:6570\:4e2d\:7684\:666e\:901a\:6570\:5b57\:90e8\:5206*)


(* ::DisplayFormula:: *)
(*\[Gamma]^\[Mu]\[Nu]\[Alpha]=1/2 {(1/2)[\[Gamma]\[Mu],\[Gamma]\[Nu]],\[Gamma]\[Alpha]}=-I/2 {\[Sigma]^\[Mu]\[Nu],\[Gamma]\[Alpha]}*)


prfactor=\[Pi]/(4M*eq\[Phi]);
splt[{"j","KR","T"},{"cls"}]=prfactor*((-I)*eq\[Phi]*Cmd\[Phi]^2)/((2\[Pi])^4*f^2)*(M*\[CapitalLambda]l^8)/(kp/y)*
fad[
{k,\[CapitalLambda]}->4,{k,m\[Phi]}->1,{p1-k,Mmd}->1
]//ScalarProductExpand


(* ::Text:: *)
(*\:4f7f\:7528 FeynCalc \:5316\:7b80\:65cb\:91cf\:7ed3\:6784*)


st[p:_,{\[Alpha]_,\[Beta]_}]:=(-(GS[p]+Mmd)) . (MT[\[Alpha],\[Beta]]-1/3 GA[\[Alpha]] . GA[\[Beta]]-(GA[\[Alpha]] . FV[p,\[Beta]]-GA[\[Beta]] . FV[p,\[Alpha]])/(3Mmd)-(2FV[p,\[Alpha]]*FV[p,\[Beta]])/(3Mmd^2))
\[CapitalTheta][a:_,b:_]:=I*DiracSigma[GA[a],GA[b]]
\[Gamma][a:_,b:_,c:_]:=(-I/2)*AntiCommutator[DiracSigma[GA[a],GA[b]],GA[c]]


splt[{"j","KR","T"},{"spr"}]=Simplify[DiracSimplify[
DiracTrace[
(GS[p]+M) . \[CapitalTheta]["+",\[Nu]] . st[p1-k,{\[Nu],\[Rho]}] . \[CapitalTheta][\[Rho],\[Sigma]] . FV[k,\[Sigma]]+
(GS[p]+M) . FV[k,\[Rho]] . \[CapitalTheta][\[Rho],\[Sigma]] . st[p1-k,{\[Sigma],\[Nu]}] . \[CapitalTheta][\[Nu],"+"]
]
]
]


splt[{"j","KR","T"}]=Collect2[
Expand[
splt[{"j","KR","T"},{"cls"}]*splt[{"j","KR","T"},{"spr"}]/.FCI[{
SP[k,k]->fad[{k,m\[Phi]}->-1]+m\[Phi]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*),
SP[k,p]->1/2(fad[{k,m\[Phi]}->-1]-fad[{p-k,Mmd}->-1]+m\[Phi]^2-Mmd^2+M^2),(*\:5bf9k.p\:4f5c\:66ff\:6362*)
pp->kp/y
}]
],
fad`a[__]
]


(* ::Subsection:: *)
(*compare*)


(*\:548c salamu \:7684\:7ed3\:679c\:4f5c\:5bf9\:6bd4*)
prfactor=(-1) (2\[Pi])^4/\[Pi] (3Mmd^2*f^2)/((-I)*Cmd\[Phi]^2*\[CapitalLambda]l^8);
fylc=Collect2[
prfactor*Expand[
splt[{"j","KR","T"}]
]/.{Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y},
fad`a[__]
]


salamu=(
((Ml^2-m\[Phi]^2)((1+y)Mmd^2-yl(M^2-m\[Phi]^2)))prg[4,1,1]+
((1-y)SP[k,k]-2(1+y)SP[k,p]+y(2M^2+Ml^2)+Ml^2)prg[4,1,0]+
((-2y)Mmd^2-yl(SP[k,k]+m\[Phi]^2-2M*Ml))prg[4,0,1]
);
salamu1=Collect2[
Expand[
salamu/.FCE[{
SP[k,k]->fad[{k,m\[Phi]}->-1]+m\[Phi]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*),
SP[k,p]->1/2(fad[{k,m\[Phi]}->-1]-fad[{p-k,Mmd}->-1]+m\[Phi]^2-Mmd^2+M^2)(*\:5bf9k.p\:4f5c\:66ff\:6362*)
}]
]/.{Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y},
fad`a[__]
]
(salamu1-fylc)


(* ::Subsection:: *)
(*to analytic*)


jcb=1/2;prfactor=\[Pi]/(4M*eqmd); (*\:53ea\:662f\:4e3a\:4e86\:63d0\:9192\:524d\:9762\:7528\:8fc7\:8fd9\:4e2a\:56e0\:5b50*)prfactor=-\[Pi]/1 (6Ml^2*Mmd^2)/\[CapitalLambda]l^8 (4\[Pi]*f)^2/(Cmd\[Phi]^2 Ml^2);
(*\:8f6c\:6362\:5230\:53ef\:4ee5\:8ba1\:7b97\:7684 integral basis*)
prp`rule[x_]:=(Expand[x/.{fad`a->fad`b}]
/.{Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y}
);
Collect2[prp`rule[
jcb*prfactor*splt[{"j","KR","T"}]
],
{fad`b[__]}
]


jcb=1/2;prfactor=\[Pi]/(4M*eqmd); (*\:53ea\:662f\:4e3a\:4e86\:63d0\:9192\:524d\:9762\:7528\:8fc7\:8fd9\:4e2a\:56e0\:5b50*)prfactor=(-1) (6Ml^2*Mmd^2)/\[CapitalLambda]l^8 (4\[Pi]*f)^2/(Cmd\[Phi]^2 Ml^2);
anl`rule[x_]:=(Expand[x/.{fad`a->fad`b}]/.{fad`b->fad`anl,fad`\[Delta]->fad`anl`\[Delta]}
/.{Condition[Power[y,n_.]*\[Delta][y],n>=-2]:>0}(*\:53bb\:6389\:7b49\:4e8e\:96f6\:7684\:9879,\:5373 y*\[Delta][y]*)
/.{Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y}
);
temp=Collect2[anl`rule[
jcb*prfactor*splt[{"j","KR","T"}]
],
{D\[CapitalLambda]md,D\[Phi]md,D\[Phi]\[CapitalLambda]}
]


(* ::Subsection:: *)
(*check*)


(* ::Input:: *)
(*(*\:68c0\:67e5\:662f\:5426\:548c salamu \:7684\:7ed3\:679c\:4e00\:81f4\:ff0c\:5e38\:7528\:7684\:5b9a\:4e49\:548c\:53d8\:6362\:89c4\:5219*)*)
(*rule={*)
(*Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y,\[CapitalLambda]^2->\[CapitalLambda]l^2+m\[Phi]^2*)
(*};*)
(*D\[CapitalLambda]T=(kt^2+y*Mmd^2-y*yl*M^2+yl*(\[CapitalLambda]l^2+m\[Phi]^2))/(-yl)/.rule;*)
(*D\[Phi]T=(kt^2+y*Mmd^2-y*yl*M^2+yl*m\[Phi]^2)/(-yl)/.rule;*)


(* ::Input:: *)
(*(*Overscript[y, _]Subsuperscript[D, \[CapitalLambda]T, 4] on`end,off`end,\[Delta]`off,Overscript[y, _]^4Subsuperscript[D, \[CapitalLambda]T, 5]Subscript[D, \[Phi]T]*)(*\:63d0\:53d6\:7ed3\:679c\:4e2d\:4e0d\:540c\:5e42\:6b21\:7684\:7cfb\:6570*)*)
(*coes=Association[CoefficientRules[temp,{D\[CapitalLambda]md^(-1),D\[Phi]md^(-1),D\[Phi]\[CapitalLambda]^(-1)}]];*)


(* ::Input:: *)
(*(*salamu \:6587\:7ae0\:4e2d\:7684\:7ed3\:679c\:ff0c\:8f93\:5165\:7684\:65f6\:5019\:ff0c\:63d0\:53d6\:51fa\:4e86\:4e00\:4e2a Overscript[\[CapitalLambda], _]^8/(6Subsuperscript[M, T, 2]*Subsuperscript[Overscript[M, _], T, 2]) *)*)
(*feyn1=coes[{4,0,0}]*D\[CapitalLambda]T^-4/.rule;*)
(*feyn2=coes[{4,1,0}]*D\[CapitalLambda]T^-4 D\[Phi]T^-1/.rule;*)
(*feyn3=coes[{3,0,0}]*D\[CapitalLambda]T^-3/.rule;*)
(*off=((Ml^2-m\[Phi]^2)(yl(M^2-m\[Phi]^2)-(1+y)Mmd^2)/(yl D\[CapitalLambda]T^4D\[Phi]T))/.rule;*)
(*off`end=(-(kt^2+yl^2*M^2+yl(Ml^2-m\[Phi]^2)-Mmd^2)/(yl D\[CapitalLambda]T^4))/.rule;*)
(*(D\[CapitalLambda]T^4 D\[Phi]T D\[Phi]\[CapitalLambda])( *)
(*feyn1+feyn2+feyn3*)
(*-2off-2off`end*)
(*)//Simplify*)


(* ::Section:: *)
(*k*)


(* ::Text:: *)
(*\:5206\:88c2\:51fd\:6570\:4e2d\:7684\:666e\:901a\:6570\:5b57\:90e8\:5206*)


(* ::DisplayFormula:: *)
(*\[Gamma]^\[Mu]\[Nu]\[Alpha]=1/2 {(1/2)[\[Gamma]\[Mu],\[Gamma]\[Nu]],\[Gamma]\[Alpha]}=-I/2 {\[Sigma]^\[Mu]\[Nu],\[Gamma]\[Alpha]}*)


prfactor=\[Pi]/(4M*eq\[Phi]);
splt[{"k","\[Delta]KR","T"},{"cls"}]=ScalarProductExpand[
prfactor*(I*eq\[Phi]*Cmd\[Phi]^2)/((2\[Pi])^4*f^2)*(M*\[CapitalLambda]l^8)/(kp/y)*2*
fad[{k+q,\[CapitalLambda]}->2,{k,\[CapitalLambda]}->3,{k,m\[Phi]}->1,{p1-k,Mmd}->1]
]


(* ::Text:: *)
(*\:4f7f\:7528 FeynCalc \:5316\:7b80\:65cb\:91cf\:7ed3\:6784*)


st[p:_,{\[Alpha]_,\[Beta]_}]:=(-(GS[p]+Mmd)) . (MT[\[Alpha],\[Beta]]-1/3 GA[\[Alpha]] . GA[\[Beta]]-(GA[\[Alpha]] . FV[p,\[Beta]]-GA[\[Beta]] . FV[p,\[Alpha]])/(3Mmd)-(2FV[p,\[Alpha]]*FV[p,\[Beta]])/(3Mmd^2))
\[CapitalTheta][a:_,b:_]:=I*DiracSigma[GA[a],GA[b]]
\[Gamma][a:_,b:_,c:_]:=(-I/2)*AntiCommutator[DiracSigma[GA[a],GA[b]],GA[c]]


splt[{"k","\[Delta]KR","T"},{"spr"}]=2*Simplify[DiracSimplify[
DiracTrace[
(GS[p]+M) . (2FV[k,"+"]) . FV[k,\[Rho]] . \[CapitalTheta][\[Rho],\[Nu]] . st[p1-k,{\[Nu],\[Alpha]}] . \[CapitalTheta][\[Alpha],\[Beta]] . FV[k,\[Beta]]
]
]
]


splt[{"k","\[Delta]KR","T"}]=Collect2[
Expand[
splt[{"k","\[Delta]KR","T"},{"cls"}]*splt[{"k","\[Delta]KR","T"},{"spr"}]/.FCI[{
SP[k,k]->fad[{k,m\[Phi]}->-1]+m\[Phi]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*),
SP[k,p]->1/2(fad[{k,m\[Phi]}->-1]-fad[{p-k,Mmd}->-1]+m\[Phi]^2-Mmd^2+M^2),(*\:5bf9k.p\:4f5c\:66ff\:6362*)
pp->kp/y
}]
],
fad`a[__]
]


(* ::Subsection:: *)
(*compare*)


(*\:548c salamu \:7684\:7ed3\:679c\:4f5c\:5bf9\:6bd4*)
prfactor=(2\[Pi])^4/\[Pi] (-1) (3Mmd^2*f^2)/((-2I)*Cmd\[Phi]^2*\[CapitalLambda]l^8);
fylc=Collect2[
prfactor*Expand[
splt[{"k","\[Delta]KR","T"}]
]/.{Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y},
fad`a[__]
]


salamu=y(
(Ml^2-m\[Phi]^2)^2(\[CapitalDelta]^2-m\[Phi]^2)prg[5,1,1]+
((SP[k,k]+m\[Phi]^2)(2Ml^2+\[CapitalDelta]^2-m\[Phi]^2)-(Ml^2+2\[CapitalDelta]^2)Ml^2-SP[k,k]^2)prg[5,0,1]+
(4SP[k,p]^2+3m\[Phi]^4-(3SP[k,k]+3Ml^2+\[CapitalDelta]^2-Ml*\[CapitalDelta])m\[Phi]^2-(4SP[k,k]-6m\[Phi]^2+2Ml^2)SP[k,p]+SP[k,k]Ml^2+Ml^3*\[CapitalDelta]+SP[k,k]^2)prg[5,1,0]+
(-1)(3Mmd^2+5M^2+4M*Mmd-3m\[Phi]^2-6SP[k,p])prg[5,0,0]
);
salamu1=Collect2[
Expand[
salamu/.FCE[{
SP[k,k]->fad[{k,m\[Phi]}->-1]+m\[Phi]^2(*\:5bf9k.k\:4f5c\:66ff\:6362*),
SP[k,p]->1/2(fad[{k,m\[Phi]}->-1]-fad[{p-k,Mmd}->-1]+m\[Phi]^2-Mmd^2+M^2)(*\:5bf9k.p\:4f5c\:66ff\:6362*)
}]
]/.{Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y},
fad`a[__]
]
(salamu1-fylc)//Simplify


(* ::Subsection:: *)
(*to analytic*)


jcb=1/2;prfactor=\[Pi]/(4M*eqmd); (*\:53ea\:662f\:4e3a\:4e86\:63d0\:9192\:524d\:9762\:7528\:8fc7\:8fd9\:4e2a\:56e0\:5b50*)prfactor=(-1) \[Pi]/1 (3Ml^2*Mmd^2)/\[CapitalLambda]l^8 (4\[Pi]*f)^2/(Cmd\[Phi]^2 Ml^2);
(*\:8f6c\:6362\:5230\:53ef\:4ee5\:8ba1\:7b97\:7684 integral basis*)
prp`rule[x_]:=(Expand[x/.{fad`a->fad`b}]
/.{Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y}
);
Collect2[prp`rule[
jcb*prfactor*splt[{"k","\[Delta]KR","T"}]
],
{fad`b[__]}
]


jcb=1/2;prfactor=\[Pi]/(4M*eqmd); (*\:53ea\:662f\:4e3a\:4e86\:63d0\:9192\:524d\:9762\:7528\:8fc7\:8fd9\:4e2a\:56e0\:5b50*)prfactor=(-1) (6Ml^2*Mmd^2)/\[CapitalLambda]l^8 (4\[Pi]*f)^2/(Cmd\[Phi]^2 Ml^2);
anl`rule[x_]:=(Expand[x/.{fad`a->fad`b}]/.{fad`b->fad`anl,fad`\[Delta]->fad`anl`\[Delta]}
/.{Condition[Power[y,n_.]*\[Delta][y],n>=-2]:>0}(*\:53bb\:6389\:7b49\:4e8e\:96f6\:7684\:9879,\:5373 y*\[Delta][y]*)
/.{Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y}
);
temp=Collect2[anl`rule[
jcb*prfactor*splt[{"k","\[Delta]KR","T"}]
],
{D\[CapitalLambda]md,D\[Phi]md,D\[Phi]\[CapitalLambda]}
]


(* ::Subsection:: *)
(*check*)


(* ::Input:: *)
(*(*\:68c0\:67e5\:662f\:5426\:548c salamu \:7684\:7ed3\:679c\:4e00\:81f4\:ff0c\:5e38\:7528\:7684\:5b9a\:4e49\:548c\:53d8\:6362\:89c4\:5219*)*)
(*rule={*)
(*Mmd->(Ml+\[CapitalDelta])/2,M->(Ml-\[CapitalDelta])/2,yl->1-y,\[CapitalLambda]^2->\[CapitalLambda]l^2+m\[Phi]^2*)
(*};*)
(*D\[CapitalLambda]T=(kt^2+y*Mmd^2-y*yl*M^2+yl*(\[CapitalLambda]l^2+m\[Phi]^2))/(-yl)/.rule;*)
(*D\[Phi]T=(kt^2+y*Mmd^2-y*yl*M^2+yl*m\[Phi]^2)/(-yl)/.rule;*)


(* ::Input:: *)
(*(*\:63d0\:53d6\:7ed3\:679c\:4e2d\:4e0d\:540c\:5e42\:6b21\:7684\:7cfb\:6570*)*)
(*coes=Association[CoefficientRules[temp,{D\[CapitalLambda]md^(-1),D\[Phi]md^(-1),D\[Phi]\[CapitalLambda]^(-1)}]];*)


(* ::Input:: *)
(*(*salamu \:6587\:7ae0\:4e2d\:7684\:7ed3\:679c\:ff0c\:8f93\:5165\:7684\:65f6\:5019\:ff0c\:63d0\:53d6\:51fa\:4e86\:4e00\:4e2a Overscript[\[CapitalLambda], _]^8/(6Subsuperscript[M, T, 2]*Subsuperscript[Overscript[M, _], T, 2]) *)*)
(*feyn1=coes[{4,0,0}]*D\[CapitalLambda]T^-4/.rule;*)
(*feyn2=coes[{5,0,0}]*D\[CapitalLambda]T^-5/.rule;*)
(*feyn3=coes[{5,1,0}]*D\[CapitalLambda]T^-5 D\[Phi]T^-1/.rule;*)
(*feyn4=coes[{3,0,0}]*D\[CapitalLambda]T^-3/.rule;*)
(*\[Delta]`off=(y (kt^2+(y M-Ml)^2)^2(kt^2+(y M+\[CapitalDelta])^2)/(yl^4 D\[CapitalLambda]T^5D\[Phi]T))/.rule;*)
(*(D\[CapitalLambda]T^5 D\[Phi]T)( *)
(*feyn1+feyn2+feyn3+feyn4*)
(*-(-4)\[Delta]`off*)
(*)//Simplify*)
