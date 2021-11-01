(* ::Package:: *)

(* ::Title:: *)
(*gen_chpt_coes.nb*)


(* ::Text:: *)
(*\:8bb0\:5f55\:62c9\:6c0f\:91cf\:5c55\:5f00\:540e\:4ea7\:751f\:7684\:8026\:5408\:7cfb\:6570\:3002*)
(*K0b\:8868\:793a K0 bar, pb \:8868\:793a p bar, \:8d28\:5b50\:7684\:53cd\:7c92\:5b50\:573a\:3002*)


(* ::Text:: *)
(*\:4f7f\:7528\:7535\:5b50\:7535\:8377e\:4f5c\:4e3a\:5355\:4f4d\:ff0c\:8fd9\:6837e\:4e0d\:663e\:5f0f\:51fa\:73b0\:5728\:9876\:70b9\:4e2d. e>0*)


(* ::Chapter:: *)
(*initial*)


(*\:672c\:6587\:4ef6\:7684\:540d\:79f0*)
fileName=If[$Notebooks,NotebookFileName[],$InputFileName];
(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c\:ff0c\:5c31\:5237\:65b0\:7b14\:8bb0\:672c\:7684\:6807\:9898*)
Once@If[!Notebooks,NotebookWrite[Cells[][[1]],Cell[Last@FileNameSplit[fileName],"Title"]]];
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


Get[FileNameJoin[{gitLocalName,"gen.format.wl"}]];(*\:5bfc\:5165\:4e00\:4e9b\:683c\:5f0f\:5316\:7684\:8bbe\:7f6e\:ff0c\:663e\:793a\:573a\:7684\:5e38\:7528\:5f62\:5f0f*)


(* ---------------------------- \:4e00\:4e9b\:8f93\:5165\:63a5\:53e3\:ff0c\:65b9\:4fbf\:8f93\:5165\:548c\:540e\:7eed\:66f4\:6539 ---------------------------- *)
unq::usage="\:5c06\:8f93\:5165\:53c2\:6570\:7ec4\:6210\:5217\:8868\:ff0c\:6216\:8005\:5173\:8054,\:9884\:7559\:7684\:51fd\:6570\:63a5\:53e3";
unq[fiels__]:=Association[fiels]
(* ------------- *)
coeIn::usage="coeIn[type,coes],\:7528\:4e8e\:8f93\:5165\:9876\:70b9\:7cfb\:6570";
mesout::usage="\:533a\:522b\:4ecb\:5b50\:ff0cout\:8868\:793a\:51fa\:5c04";
mesout[x_]:=mes[x,"out"]
(* -------------------------- \:7c92\:5b50\:7c7b\:578b\:63a5\:53e3 --------------------------*)
Get[FileNameJoin[{gitLocalName,"coes.interface.wl"}]];
(*++++++++++++++++++++++++++++++ \:65e0\:6cd5\:786e\:5b9a\:5927\:5c0f\:7684\:91cf +++++++++++++++++++++++++++*)
undefined;
(*--------------------------------------*)
vtxCoeAnti[x_]:=vtxCoe[-x]


(* ::Chapter::Closed:: *)
(*strong interaction*)


(* ::Section::Closed:: *)
(*Strong,BB\[Phi],DF*)


(*Overscript[1, _].\[Gamma]^\[Mu].\[Gamma]^5.p.0+Overscript[p, _].\[Gamma]^\[Mu].\[Gamma]^5.1.0\[Rule](D-F)/(2 Subscript[f, \[Phi]])*)
(*++++++++++++++++++++++++++\:5f3a\:76f8\:4e92\:4f5c\:7528\:7684 BBM, \:8f74\:77e2\:9879 DF+++++++++++++++++++++*)
vtxtp=vtxType["stro","DF","mesIn"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
coeIn[type_,x_]:=type->vtxCoe[x/cc["f"]](*\:7cfb\:6570\:7684\:7c7b\:578b\:ff0c\:4ee5\:53ca\:5e38\:6570\:56e0\:5b50*)
(* ----------------  ---------------- *)
vtx[unq["type"->vtxtp]]={
(*+++ proton +++*)
unq[oct["pb"],oct["p"],mes["\[Pi]0"],coeIn[vtxtp,(cc["D"]+cc["F"])/2]],
unq[oct["pb"],oct["p"],mes["\[Eta]8"],coeIn[vtxtp,-(cc["D"]-3cc["F"])/(2Sqrt[3])]],
unq[oct["pb"],oct["p"],mes["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["pb"],oct["n"],mes["\[Pi]+"],coeIn[vtxtp,(cc["D"]+cc["F"])/Sqrt[2]]],
unq[oct["pb"],oct["\[CapitalSigma]+"],mes["K0"],coeIn[vtxtp,(cc["D"]-cc["F"])/Sqrt[2]]],
unq[oct["pb"],oct["\[CapitalSigma]0"],mes["K+"],coeIn[vtxtp,(cc["D"]-cc["F"])/2]],
unq[oct["pb"],oct["\[CapitalLambda]"],mes["K+"],coeIn[vtxtp,-(cc["D"]+3cc["F"])/(2Sqrt[3])]],
(*+++ neurton +++*)
unq[oct["nb"],oct["n"],mes["\[Pi]0"],coeIn[vtxtp,-(cc["D"]+cc["F"])/2]],
unq[oct["nb"],oct["n"],mes["\[Eta]8"],coeIn[vtxtp,-(cc["D"]-3cc["F"])/(2Sqrt[3])]],
unq[oct["nb"],oct["n"],mes["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["nb"],oct["p"],mes["\[Pi]-"],coeIn[vtxtp,(cc["D"]+cc["F"])/Sqrt[2]]],
unq[oct["nb"],oct["\[CapitalSigma]0"],mes["K0"],coeIn[vtxtp,(-cc["D"]+cc["F"])/2]],
unq[oct["nb"],oct["\[CapitalSigma]-"],mes["K+"],coeIn[vtxtp,(cc["D"]-cc["F"])/Sqrt[2]]],
unq[oct["nb"],oct["\[CapitalLambda]"],mes["K0"],coeIn[vtxtp,-(cc["D"]+3cc["F"])/(2Sqrt[3])]],
(*+++ \[CapitalSigma]+ +++*)
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Pi]0"],coeIn[vtxtp,cc["F"]]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Eta]8"],coeIn[vtxtp,cc["D"]/Sqrt[3]]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalLambda]"],mes["\[Pi]+"],coeIn[vtxtp,cc["D"]/Sqrt[3]]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]0"],mes["\[Pi]+"],coeIn[vtxtp,-cc["F"]]],
unq[oct["\[CapitalSigma]+b"],oct["p"],mes["K0b"],coeIn[vtxtp,(cc["D"]-cc["F"])/Sqrt[2]]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalXi]0"],mes["K+"],coeIn[vtxtp,(cc["D"]+cc["F"])/Sqrt[2]]],
(*+++ \[CapitalSigma]0 +++*)
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalLambda]"],mes["\[Pi]0"],coeIn[vtxtp,cc["D"]/Sqrt[3]]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],mes["\[Eta]8"],coeIn[vtxtp,cc["D"]/Sqrt[3]]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],mes["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]-"],mes["\[Pi]+"],coeIn[vtxtp,cc["F"]]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]+"],mes["\[Pi]-"],coeIn[vtxtp,-cc["F"]]],
unq[oct["\[CapitalSigma]0b"],oct["p"],mes["K-"],coeIn[vtxtp,(cc["D"]-cc["F"])/2]],
unq[oct["\[CapitalSigma]0b"],oct["n"],mes["K0b"],coeIn[vtxtp,(-cc["D"]+cc["F"])/2]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalXi]-"],mes["K+"],coeIn[vtxtp,(cc["D"]+cc["F"])/2]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalXi]0"],mes["K0"],coeIn[vtxtp,-(cc["D"]+cc["F"])/2]],
(*+++ \[CapitalSigma]- +++*)
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Pi]0"],coeIn[vtxtp,-cc["F"]]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Eta]8"],coeIn[vtxtp,cc["D"]/Sqrt[3]]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalLambda]"],mes["\[Pi]-"],coeIn[vtxtp,cc["D"]/Sqrt[3]]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]0"],mes["\[Pi]-"],coeIn[vtxtp,cc["F"]]],
unq[oct["\[CapitalSigma]-b"],oct["n"],mes["K-"],coeIn[vtxtp,(cc["D"]-cc["F"])/Sqrt[2]]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalXi]-"],mes["K0"],coeIn[vtxtp,(cc["D"]+cc["F"])/Sqrt[2]]],
(*+++ \[CapitalXi]0 +++*)
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Pi]0"],coeIn[vtxtp,(-cc["D"]+cc["F"])/2]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Eta]8"],coeIn[vtxtp,-((cc["D"]+3cc["F"])/(2Sqrt[3]))]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalXi]0b"],oct["\[CapitalSigma]0"],mes["K0b"],coeIn[vtxtp,-((cc["D"]+cc["F"])/2)]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]-"],mes["\[Pi]+"],coeIn[vtxtp,(cc["D"]-cc["F"])/Sqrt[2]]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalSigma]+"],mes["K-"],coeIn[vtxtp,(cc["D"]+cc["F"])/Sqrt[2]]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalLambda]"],mes["K0b"],coeIn[vtxtp,-((cc["D"]-3cc["F"])/(2Sqrt[3]))]],
(*+++ \[CapitalXi]- +++*)
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Pi]0"],coeIn[vtxtp,(cc["D"]-cc["F"])/2]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Eta]8"],coeIn[vtxtp,-((cc["D"]+3cc["F"])/(2Sqrt[3]))]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalXi]-b"],oct["\[CapitalSigma]0"],mes["K-"],coeIn[vtxtp,(cc["D"]+cc["F"])/2]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]0"],mes["\[Pi]-"],coeIn[vtxtp,(cc["D"]-cc["F"])/Sqrt[2]]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalSigma]-"],mes["K0b"],coeIn[vtxtp,(cc["D"]+cc["F"])/Sqrt[2]]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalLambda]"],mes["K-"],coeIn[vtxtp,-((cc["D"]-3cc["F"])/(2Sqrt[3]))]],
(*+++ \[CapitalLambda] +++*)
unq[oct["\[CapitalLambda]b"],oct["\[CapitalSigma]0"],mes["\[Pi]0"],coeIn[vtxtp,cc["D"]/Sqrt[3]]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],mes["\[Eta]8"],coeIn[vtxtp,-(cc["D"]/Sqrt[3])]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],mes["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalLambda]b"],oct["\[CapitalSigma]-"],mes["\[Pi]+"],coeIn[vtxtp,-(cc["D"]/Sqrt[3])]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalSigma]+"],mes["\[Pi]-"],coeIn[vtxtp,cc["D"]/Sqrt[3]]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalXi]-"],mes["K+"],coeIn[vtxtp,-((cc["D"]-3cc["F"])/(2Sqrt[3]))]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalXi]0"],mes["K0"],coeIn[vtxtp,-((cc["D"]-3cc["F"])/(2Sqrt[3]))]],
unq[oct["\[CapitalLambda]b"],oct["p"],mes["K-"],coeIn[vtxtp,-((cc["D"]+3cc["F"])/(2Sqrt[3]))]],
unq[oct["\[CapitalLambda]b"],oct["n"],mes["K0b"],coeIn[vtxtp,-((cc["D"]+3cc["F"])/(2Sqrt[3]))]]
};


vtxtp=vtxType["stro","DF","mesIn"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
vtxtpFlip=vtxType["stro","DF","mesOut"];(* \:53e6\:4e00\:534a\:9876\:70b9 *)
coeIn[type_,x_]:=type->vtxCoe[x/cc["f"]](*\:7cfb\:6570\:7684\:7c7b\:578b\:ff0c\:4ee5\:53ca\:5e38\:6570\:56e0\:5b50*)
(* ------------- \:6dfb\:52a0\:53e6\:5916\:534a\:90e8\:5206\:9876\:70b9\:ff0c\:6539\:53d8\:5404\:4e2a\:7c92\:5b50\:7684\:65b9\:5411 -------------*)
vtx[unq["type"->vtxtpFlip]]=Query[All,<|
fdTypeOct->(#@fdTypeOctb/.fd[a_,b_,1]:>fd[a,b,0]),
fdTypeMesOut->(#@fdTypeMes/.fd[a_,b_,0]:>fd[a,b,1]),
fdTypeOctb->(#@fdTypeOct/.fd[a_,b_,0]:>fd[a,b,1]),
vtxtpFlip->(#@vtxtp)
|>&
]@vtx[unq["type"->vtxtp]];


(* ::Section::Closed:: *)
(*Strong, BT\[Phi], C*)


(*Overscript[p, _].\[CapitalTheta]^\[Mu]\[Nu].Subsuperscript[\[CapitalDelta], \[Nu], ++].\!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]
\*SuperscriptBox[\(\[Pi]\), \(-\)]\)-Overscript[Subsuperscript[\[CapitalDelta], \[Nu], ++], _].\[CapitalTheta]^\[Mu]\[Nu].p.\!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]
\*SuperscriptBox[\(\[Pi]\), \(-\)]\)\[Rule]\[ScriptCapitalC]/(Sqrt[2] Subscript[f, \[Phi]])*)
(*++++++++++++++++++++++++++\:5f3a\:76f8\:4e92\:4f5c\:7528\:7684 BMT \:8f74\:77e2\:9879 C+++++++++++++++++++++*)
vtxtp=vtxType["stro","C","mesIn"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b,oct bar\:7c7b\:578b\:7684\:ff0cdecuplet bar \:7c7b\:578b\:7684\:8026\:5408\:5e38\:6570\:5dee\:8d1f\:53f7*)
coeIn[type_,x_]:=type->vtxCoe[cc["C"]/(Sqrt[2]cc["f"])*x](*\:7cfb\:6570\:7684\:7c7b\:578b\:ff0c\:4ee5\:53ca\:5e38\:6570\:56e0\:5b50*)
(* ----------------  ---------------- *)
vtx[unq["type"->vtxtp]]={
(*+++ proton +++*)
unq[oct["pb"],mes["\[Pi]0"],dec["\[CapitalDelta]+"],coeIn[vtxtp,(-1)*Sqrt[2/3]]],
unq[oct["pb"],mes["\[Pi]-"],dec["\[CapitalDelta]++"],coeIn[vtxtp,1/Sqrt[1]]],
unq[oct["pb"],mes["K+"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,(-1)/(Sqrt[6])]],
unq[oct["pb"],mes["\[Pi]+"],dec["\[CapitalDelta]0"],coeIn[vtxtp,-1/Sqrt[3]]],
unq[oct["pb"],mes["K0"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,1/Sqrt[3]]],
(*+++ neurton +++*)
unq[oct["nb"],mes["\[Pi]0"],dec["\[CapitalDelta]0"],coeIn[vtxtp,(-1)*Sqrt[2/3]]],
unq[oct["nb"],mes["\[Pi]+"],dec["\[CapitalDelta]-"],coeIn[vtxtp,-1/Sqrt[1]]],
unq[oct["nb"],mes["K0"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,1/(Sqrt[6])]],
unq[oct["nb"],mes["\[Pi]-"],dec["\[CapitalDelta]+"],coeIn[vtxtp,1/Sqrt[3]]],
unq[oct["nb"],mes["K+"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,-1/Sqrt[3]]],
(*+++ \[CapitalSigma]+ +++*)
unq[oct["\[CapitalSigma]+b"],mes["\[Pi]0"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,1/(Sqrt[6])]],
unq[oct["\[CapitalSigma]+b"],mes["\[Eta]8"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,1/Sqrt[2]]],
unq[oct["\[CapitalSigma]+b"],mes["\[Eta]0"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]+b"],mes["K-"],dec["\[CapitalDelta]++"],coeIn[vtxtp,-1/Sqrt[1]]],
unq[oct["\[CapitalSigma]+b"],mes["\[Pi]+"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,1/(Sqrt[6])]],
unq[oct["\[CapitalSigma]+b"],mes["K+"],dec["\[CapitalXi]*0"],coeIn[vtxtp,1/Sqrt[3]]],
unq[oct["\[CapitalSigma]+b"],mes["K0b"],dec["\[CapitalDelta]+"],coeIn[vtxtp,-1/Sqrt[3]]],
(*+++ \[CapitalSigma]0 +++*)
unq[oct["\[CapitalSigma]0b"],mes["\[Eta]8"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,-1/Sqrt[2]]],
unq[oct["\[CapitalSigma]0b"],mes["\[Eta]0"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]0b"],mes["\[Pi]+"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,-1/(Sqrt[6])]],
unq[oct["\[CapitalSigma]0b"],mes["\[Pi]-"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,-1/(Sqrt[6])]],
unq[oct["\[CapitalSigma]0b"],mes["K+"],dec["\[CapitalXi]*-"],coeIn[vtxtp,-1/(Sqrt[6])]],
unq[oct["\[CapitalSigma]0b"],mes["K-"],dec["\[CapitalDelta]+"],coeIn[vtxtp,Sqrt[2/3]]],
unq[oct["\[CapitalSigma]0b"],mes["K0"],dec["\[CapitalXi]*0"],coeIn[vtxtp,-1/(Sqrt[6])]],
unq[oct["\[CapitalSigma]0b"],mes["K0b"],dec["\[CapitalDelta]0"],coeIn[vtxtp,Sqrt[2/3]]],
(*+++ \[CapitalSigma]- +++*)
unq[oct["\[CapitalSigma]-b"],mes["\[Eta]8"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,-1/Sqrt[2]]],
unq[oct["\[CapitalSigma]-b"],mes["\[Eta]0"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]-b"],mes["K0b"],dec["\[CapitalDelta]-"],coeIn[vtxtp,1/Sqrt[1]]],
unq[oct["\[CapitalSigma]-b"],mes["\[Pi]0"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,1/(Sqrt[6])]],
unq[oct["\[CapitalSigma]-b"],mes["\[Pi]-"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,-1/(Sqrt[6])]],
unq[oct["\[CapitalSigma]-b"],mes["K-"],dec["\[CapitalDelta]0"],coeIn[vtxtp,1/Sqrt[3]]],
unq[oct["\[CapitalSigma]-b"],mes["K0"],dec["\[CapitalXi]*-"],coeIn[vtxtp,-1/Sqrt[3]]],
(*+++ \[CapitalXi]0 +++*)
unq[oct["\[CapitalXi]0b"],mes["\[Eta]8"],dec["\[CapitalXi]*0"],coeIn[vtxtp,1/Sqrt[2]]],
unq[oct["\[CapitalXi]0b"],mes["\[Eta]0"],dec["\[CapitalXi]*0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalXi]0b"],mes["K+"],dec["\[CapitalOmega]-"],coeIn[vtxtp,1/Sqrt[1]]],
unq[oct["\[CapitalXi]0b"],mes["\[Pi]0"],dec["\[CapitalXi]*0"],coeIn[vtxtp,1/(Sqrt[6])]],
unq[oct["\[CapitalXi]0b"],mes["K0b"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,-1/(Sqrt[6])]],
unq[oct["\[CapitalXi]0b"],mes["\[Pi]+"],dec["\[CapitalXi]*-"],coeIn[vtxtp,1/Sqrt[3]]],
unq[oct["\[CapitalXi]0b"],mes["K-"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,-1/Sqrt[3]]],
(*+++ \[CapitalXi]- +++*)
unq[oct["\[CapitalXi]-b"],mes["\[Eta]8"],dec["\[CapitalXi]*-"],coeIn[vtxtp,-1/Sqrt[2]]],
unq[oct["\[CapitalXi]-b"],mes["\[Eta]0"],dec["\[CapitalXi]*-"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalXi]-b"],mes["K0"],dec["\[CapitalOmega]-"],coeIn[vtxtp,-1/Sqrt[1]]],
unq[oct["\[CapitalXi]-b"],mes["\[Pi]0"],dec["\[CapitalXi]*-"],coeIn[vtxtp,1/(Sqrt[6])]],
unq[oct["\[CapitalXi]-b"],mes["K-"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,1/(Sqrt[6])]],
unq[oct["\[CapitalXi]-b"],mes["\[Pi]-"],dec["\[CapitalXi]*0"],coeIn[vtxtp,-1/Sqrt[3]]],
unq[oct["\[CapitalXi]-b"],mes["K0b"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,1/Sqrt[3]]],
(*+++ \[CapitalLambda] +++*)
unq[oct["\[CapitalLambda]b"],mes["\[Pi]+"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,1/Sqrt[2]]],
unq[oct["\[CapitalLambda]b"],mes["\[Pi]0"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,1/Sqrt[2]]],
unq[oct["\[CapitalLambda]b"],mes["\[Pi]-"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,-(1/Sqrt[2])]],
unq[oct["\[CapitalLambda]b"],mes["K+"],dec["\[CapitalXi]*-"],coeIn[vtxtp,1/Sqrt[2]]],
unq[oct["\[CapitalLambda]b"],mes["K0"],dec["\[CapitalXi]*0"],coeIn[vtxtp,-(1/Sqrt[2])]]
};


vtxtp=vtxType["stro","C","mesIn"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b,oct bar\:7c7b\:578b\:7684\:ff0cdecuplet bar \:7c7b\:578b\:7684\:8026\:5408\:5e38\:6570\:5dee\:8d1f\:53f7*)
vtxtpFlip=vtxType["stro","C","mesOut"];
coeIn[type_,x_]:=type->vtxCoe[cc["C"]/(Sqrt[2]cc["f"])*x](*\:7cfb\:6570\:7684\:7c7b\:578b\:ff0c\:4ee5\:53ca\:5e38\:6570\:56e0\:5b50*)
(* ------------- \:6dfb\:52a0\:53e6\:5916\:534a\:90e8\:5206\:9876\:70b9 -------------*)
vtx[unq["type"->vtxtpFlip]]=Query[All,<|
fdTypeOct ->(#@fdTypeOctb/.fd[a_,b_,1]:>fd[a,b,0]),
fdTypeMesOut ->(#@fdTypeMes/.mesAntiRule),
fdTypeDecb ->(#@fdTypeDec/.fd[a_,b_,0]:>fd[a,b,1]),
vtxtpFlip->(#@vtxtp/.vtxCoe->vtxCoeAnti)
|>&
]@vtx[unq["type"->vtxtp]];


(* ::Section::Closed:: *)
(*Strong, TT\[Phi], H*)


(*\!\(
\*SubsuperscriptBox[\(T\), \(\[Nu]\), \(ijl\)] . 
\*SubsuperscriptBox[\(u\), \(\[Alpha]\), \(kl\)] . 
\*SubsuperscriptBox[
OverscriptBox[\(T\), \(_\)], \(\[Mu]\), \(ijk\)] . 
\*SuperscriptBox[\(\[Gamma]\), \(\[Mu]\[Nu]\[Alpha]\)] . 
\*SuperscriptBox[\(\[Gamma]\), \(5\)]\)*)
(*++++++++++++++++++++++++++\:5f3a\:76f8\:4e92\:4f5c\:7528\:7684 BMT \:8f74\:77e2\:9879 C+++++++++++++++++++++*)
vtxtp=vtxType["stro","H"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
coeIn[type_,x_]:=type->vtxCoe[cc["H"]/cc["f"] x](*\:7cfb\:6570\:7684\:7c7b\:578b\:ff0c\:4ee5\:53ca\:5e38\:6570\:56e0\:5b50*)
(* ----------------  ---------------- *)
vtx[unq["type"->vtxtp]]={
(*+++ \[CapitalDelta]++ +++*)
unq[dec["\[CapitalDelta]++b"],dec["\[CapitalDelta]++"],mes["\[Pi]0"],coeIn[vtxtp,-1/2]],
unq[dec["\[CapitalDelta]++b"],dec["\[CapitalDelta]++"],mes["\[Eta]8"],coeIn[vtxtp,-1/(2Sqrt[3])]],
unq[dec["\[CapitalDelta]++b"],dec["\[CapitalDelta]++"],mes["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalDelta]++b"],dec["\[CapitalDelta]+"],mes["\[Pi]+"],coeIn[vtxtp,-1/Sqrt[6]]],
unq[dec["\[CapitalDelta]++b"],dec["\[CapitalSigma]*+"],mes["K+"],coeIn[vtxtp,-1/Sqrt[6]]],
(*+++ \[CapitalDelta]+ +++*)
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalDelta]+"],mes["\[Pi]0"],coeIn[vtxtp,-1/6]],
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalDelta]+"],mes["\[Eta]8"],coeIn[vtxtp,-1/(2Sqrt[3])]],
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalDelta]+"],mes["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalSigma]*0"],mes["K+"],coeIn[vtxtp,-1/3]],
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalDelta]0"],mes["\[Pi]+"],coeIn[vtxtp,-Sqrt[2]/3]],
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalSigma]*+"],mes["K0"],coeIn[vtxtp,-1/(3Sqrt[2])]],
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalDelta]++"],mes["\[Pi]-"],coeIn[vtxtp,-1/Sqrt[6]]],
(*+++ \[CapitalDelta]0 +++*)
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalDelta]0"],mes["\[Pi]0"],coeIn[vtxtp,1/6]],
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalDelta]0"],mes["\[Eta]8"],coeIn[vtxtp,-1/(2Sqrt[3])]],
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalDelta]0"],mes["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalSigma]*0"],mes["K0"],coeIn[vtxtp,-1/3]],
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalDelta]+"],mes["\[Pi]-"],coeIn[vtxtp,-Sqrt[2]/3]],
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalSigma]*-"],mes["K+"],coeIn[vtxtp,-1/(3Sqrt[2])]],
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalDelta]-"],mes["\[Pi]+"],coeIn[vtxtp,-1/Sqrt[6]]],
(*+++ \[CapitalDelta]- +++*)
unq[dec["\[CapitalDelta]-b"],dec["\[CapitalDelta]-"],mes["\[Pi]0"],coeIn[vtxtp,1/2]],
unq[dec["\[CapitalDelta]-b"],dec["\[CapitalDelta]-"],mes["\[Eta]8"],coeIn[vtxtp,-1/(2Sqrt[3])]],
unq[dec["\[CapitalDelta]-b"],dec["\[CapitalDelta]-"],mes["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalDelta]-b"],dec["\[CapitalDelta]0"],mes["\[Pi]-"],coeIn[vtxtp,-1/Sqrt[6]]],
unq[dec["\[CapitalDelta]-b"],dec["\[CapitalSigma]*-"],mes["K0"],coeIn[vtxtp,-1/Sqrt[6]]],
(*+++ \[CapitalSigma]*+ +++*)
unq[dec["\[CapitalSigma]*+b"],dec["\[CapitalSigma]*+"],mes["\[Pi]0"],coeIn[vtxtp,-1/3]],
unq[dec["\[CapitalSigma]*+b"],dec["\[CapitalSigma]*0"],mes["\[Pi]+"],coeIn[vtxtp,-1/3]],
unq[dec["\[CapitalSigma]*+b"],dec["\[CapitalXi]*0"],mes["K+"],coeIn[vtxtp,-Sqrt[2]/3]],
unq[dec["\[CapitalSigma]*+b"],dec["\[CapitalDelta]+"],mes["K0b"],coeIn[vtxtp,-1/(3Sqrt[2])]],
unq[dec["\[CapitalSigma]*+b"],dec["\[CapitalDelta]++"],mes["K-"],coeIn[vtxtp,-1/Sqrt[6]]],
(*+++ \[CapitalSigma]*0 +++*)
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalSigma]*-"],mes["\[Pi]+"],coeIn[vtxtp,-1/3]],
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalSigma]*+"],mes["\[Pi]-"],coeIn[vtxtp,-1/3]],
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalXi]*-"],mes["K+"],coeIn[vtxtp,-1/3]],
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalDelta]+"],mes["K-"],coeIn[vtxtp,-1/3]],
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalXi]*0"],mes["K0"],coeIn[vtxtp,-1/3]],
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalDelta]0"],mes["K0b"],coeIn[vtxtp,-1/3]],
(*+++ \[CapitalSigma]*- +++*)
unq[dec["\[CapitalSigma]*-b"],dec["\[CapitalSigma]*-"],mes["\[Pi]0"],coeIn[vtxtp,1/3]],
unq[dec["\[CapitalSigma]*-b"],dec["\[CapitalSigma]*0"],mes["\[Pi]-"],coeIn[vtxtp,-1/3]],
unq[dec["\[CapitalSigma]*-b"],dec["\[CapitalDelta]0"],mes["K-"],coeIn[vtxtp,-1/(3Sqrt[2])]],
unq[dec["\[CapitalSigma]*-b"],dec["\[CapitalXi]*-"],mes["K0"],coeIn[vtxtp,-Sqrt[2]/3]],
unq[dec["\[CapitalSigma]*-b"],dec["\[CapitalDelta]-"],mes["K0b"],coeIn[vtxtp,-1/Sqrt[6]]],
(*+++ \[CapitalXi]*0 +++*)
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalXi]*0"],mes["\[Pi]0"],coeIn[vtxtp,-1/6]],
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalXi]*0"],mes["\[Eta]8"],coeIn[vtxtp,1/(2Sqrt[3])]],
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalXi]*0"],mes["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalSigma]*0"],mes["K0b"],coeIn[vtxtp,-1/3]],
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalXi]*-"],mes["\[Pi]+"],coeIn[vtxtp,-1/(3Sqrt[2])]],
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalSigma]*+"],mes["K-"],coeIn[vtxtp,-Sqrt[2]/3]],
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalOmega]-"],mes["K+"],coeIn[vtxtp,-1/Sqrt[6]]],
(*+++ \[CapitalXi]*- +++*)
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalXi]*-"],mes["\[Pi]0"],coeIn[vtxtp,1/6]],
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalXi]*-"],mes["\[Eta]8"],coeIn[vtxtp,1/(2Sqrt[3])]],
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalXi]*-"],mes["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalSigma]*0"],mes["K-"],coeIn[vtxtp,-1/3]],
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalXi]*0"],mes["\[Pi]-"],coeIn[vtxtp,-1/(3Sqrt[2])]],
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalSigma]*-"],mes["K0b"],coeIn[vtxtp,-Sqrt[2]/3]],
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalOmega]-"],mes["K0"],coeIn[vtxtp,-1/Sqrt[6]]],
(*+++ \[CapitalOmega]- +++*)
unq[dec["\[CapitalOmega]-b"],dec["\[CapitalOmega]-"],mes["\[Eta]8"],coeIn[vtxtp,1/Sqrt[3]]],
unq[dec["\[CapitalOmega]-b"],dec["\[CapitalOmega]-"],mes["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalOmega]-b"],dec["\[CapitalXi]*0"],mes["K-"],coeIn[vtxtp,-1/Sqrt[6]]],
unq[dec["\[CapitalOmega]-b"],dec["\[CapitalXi]*-"],mes["K0b"],coeIn[vtxtp,-1/Sqrt[6]]]
};


(* ::Section::Closed:: *)
(*Strong ,BB\[Phi]\[Phi]*)


(* Overscript[p, _].\[Gamma]^\[Mu].p.(-\[Pi]^-.0+\[Pi]^+.0)\[Rule]\[ImaginaryI]/(4 Subsuperscript[f, \[Phi], 2]) *)
(*++++++++++++++++++++++++++\:5f3a\:76f8\:4e92\:4f5c\:7528\:7684 BB\[Phi]\[Phi] \:9876\:70b9 +++++++++++++++++++++*)
vtxtp=vtxType["stro","BB\[Phi]\[Phi]"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
coeIn[type_,x_]:=type->vtxCoe[x/cc["f"]^2](*\:7cfb\:6570\:7684\:7c7b\:578b\:ff0c\:4ee5\:53ca\:5e38\:6570\:56e0\:5b50*)
(* ----------------  ---------------- *)
vtx[unq["type"->vtxtp]]={
(*+++ proton +++*)
unq[oct["pb"],oct["p"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,I/4]],
unq[oct["pb"],oct["p"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,-I/4]],
unq[oct["pb"],oct["p"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["pb"],oct["p"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,I/(4Sqrt[3])]],
unq[oct["pb"],oct["p"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,I/4]],
unq[oct["pb"],oct["p"],mes["K+"],mesout["K-"],coeIn[vtxtp,I/2]],
unq[oct["pb"],oct["p"],mes["K0"],mesout["K0b"],coeIn[vtxtp,-I/4]],
(*+++ neutron +++*)
unq[oct["nb"],oct["n"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,I/4]],
unq[oct["nb"],oct["n"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,-I/4]],
unq[oct["nb"],oct["n"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["nb"],oct["n"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,-I/(4Sqrt[3])]],
unq[oct["nb"],oct["n"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,-I/4]],
unq[oct["nb"],oct["n"],mes["K+"],mesout["K-"],coeIn[vtxtp,I/4]],
unq[oct["nb"],oct["n"],mes["K0"],mesout["K0b"],coeIn[vtxtp,-I/2]],
(*+++ Sigma+ +++*)
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,0]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,0]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,I/(2Sqrt[3])]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,I/2]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["K+"],mesout["K-"],coeIn[vtxtp,I/4]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["K0"],mesout["K0b"],coeIn[vtxtp,I/4]],
(*+++ Sigma0 +++*)
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,0]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,0]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,0]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,0]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],mes["K+"],mesout["K-"],coeIn[vtxtp,0]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],mes["K0"],mesout["K0b"],coeIn[vtxtp,0]],
(*+++ Sigma- +++*)
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]-"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,0]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]-"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,0]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]-"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]-"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,-I/(2Sqrt[3])]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]-"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,-I/2]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]-"],mes["K+"],mesout["K-"],coeIn[vtxtp,-I/4]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]-"],mes["K0"],mesout["K0b"],coeIn[vtxtp,-I/4]],
(*+++ Xi0 +++*)
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,-I/4]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,I/4]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,I/(4Sqrt[3])]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,I/4]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["K+"],mesout["K-"],coeIn[vtxtp,-I/4]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["K0"],mesout["K0b"],coeIn[vtxtp,I/2]],
(*+++ Xi- +++*)
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,-I/4]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,I/4]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,-I/(4Sqrt[3])]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,-I/4]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["K+"],mesout["K-"],coeIn[vtxtp,-I/2]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["K0"],mesout["K0b"],coeIn[vtxtp,I/2]],
(*+++ Lambda +++*)
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,0]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,0]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,0]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,0]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],mes["K+"],mesout["K-"],coeIn[vtxtp,0]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],mes["K0"],mesout["K0b"],coeIn[vtxtp,0]]
};


(* ::Section::Closed:: *)
(*Strong,BB\[Phi]\[Phi],tensor coupling*)


(* Overscript[p, _].\[Sigma]^\[Mu]\[Nu].p.(\!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]
\*SuperscriptBox[\(\[Pi]\), \(+\)]\).\!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Nu]\)]
\*SuperscriptBox[\(\[Pi]\), \(-\)]\)-\!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]
\*SuperscriptBox[\(\[Pi]\), \(-\)]\).\!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Nu]\)]
\*SuperscriptBox[\(\[Pi]\), \(+\)]\))\[Rule]I/Subsuperscript[f, \[Phi], 2](Subscript[b, 10]+Subscript[b, 11]*)
(*++++++++++++++++++++++++++\:5f3a\:76f8\:4e92\:4f5c\:7528\:7684BBMM,\:5f20\:91cf\:8026\:5408\:9879 +++++++++++++++++++++*)
vtxtp=vtxType["stro","ten"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
coeIn[type_,x_]:=type->vtxCoe[x/cc["f"]^2](*\:7cfb\:6570\:7684\:7c7b\:578b\:ff0c\:4ee5\:53ca\:5e38\:6570\:56e0\:5b50*)
(* ----------------  ---------------- *)
vtx[unq["type"->vtxtp]]={
(*+++ proton +++*)
unq[oct["pb"],oct["p"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,-I(5cc["b10"]-3cc["b11"])]],
unq[oct["pb"],oct["p"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["pb"],oct["p"],mes["K0"],mesout["K0b"],coeIn[vtxtp,-2I(cc["b10"]-cc["b11"])]],
unq[oct["pb"],oct["p"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,-3I(cc["b10"]+cc["b11"])]],
unq[oct["pb"],oct["p"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,2I(cc["b10"]+cc["b11"])]],
unq[oct["pb"],oct["p"],mes["\[Eta]8"],mesout["\[Pi]0"],coeIn[vtxtp,-I Sqrt[3](cc["b10"]+cc["b11"])]],
unq[oct["pb"],oct["p"],mes["K+"],mesout["K-"],coeIn[vtxtp,I(4cc["b11"]+cc["b9"])]],
(*+++ neurton +++*)
unq[oct["nb"],oct["n"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,-I(5cc["b10"]-3cc["b11"])]],
unq[oct["nb"],oct["n"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["nb"],oct["n"],mes["K+"],mesout["K-"],coeIn[vtxtp,-2I(cc["b10"]-cc["b11"])]],
unq[oct["nb"],oct["n"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,-3I(cc["b10"]+cc["b11"])]],
unq[oct["nb"],oct["n"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,-2I(cc["b10"]+cc["b11"])]],
unq[oct["nb"],oct["n"],mes["\[Eta]8"],mesout["\[Pi]0"],coeIn[vtxtp,I Sqrt[3](cc["b10"]+cc["b11"])]],
unq[oct["nb"],oct["n"],mes["K0"],mesout["K0b"],coeIn[vtxtp,I(4cc["b11"]+cc["b9"])]],
(*+++ \[CapitalSigma]+ +++*)
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["K0"],mesout["K0b"],coeIn[vtxtp,2I(cc["b10"]-cc["b11"])]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["K+"],mesout["K-"],coeIn[vtxtp,2I(cc["b10"]+cc["b11"])]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,I(4cc["b11"]+cc["b9"])]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,-6I(cc["b10"])]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,-2I(cc["b10"])]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Eta]8"],mesout["\[Pi]0"],coeIn[vtxtp,-2I Sqrt[3](cc["b11"])]],
(*+++ \[CapitalSigma]0 +++*)
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,-I(6cc["b10"]-cc["b9"])]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],mes["K+"],mesout["K-"],coeIn[vtxtp,2I(cc["b10"])]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],mes["K0"],mesout["K0b"],coeIn[vtxtp,2I(cc["b10"])]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,-2I(cc["b10"])]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
(*+++ \[CapitalSigma]- +++*)
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["K+"],mesout["K-"],coeIn[vtxtp,2I(cc["b10"]-cc["b11"])]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["K0"],mesout["K0b"],coeIn[vtxtp,2I(cc["b10"]+cc["b11"])]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,-6I(cc["b10"])]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,-2I(cc["b10"])]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Eta]8"],mesout["\[Pi]0"],coeIn[vtxtp,2I Sqrt[3](cc["b11"])]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,I(cc["b9"]-4cc["b11"])]],
(*+++ \[CapitalXi]0 +++*)
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,-3I(cc["b10"]-cc["b11"])]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,-2I(cc["b10"]-cc["b11"])]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["K+"],mesout["K-"],coeIn[vtxtp,-2I(cc["b10"]+cc["b11"])]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,-I(5cc["b10"]+3cc["b11"])]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Eta]8"],mesout["\[Pi]0"],coeIn[vtxtp,I Sqrt[3](cc["b10"]-cc["b11"])]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["K0"],mesout["K0b"],coeIn[vtxtp,I(cc["b9"]-4cc["b11"])]],
(*+++ \[CapitalXi]- +++*)
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,-3I(cc["b10"]-cc["b11"])]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,2I(cc["b10"]-cc["b11"])]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["K0"],mesout["K0b"],coeIn[vtxtp,-2I(cc["b10"]+cc["b11"])]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,-I(5cc["b10"]+3cc["b11"])]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Eta]8"],mesout["\[Pi]0"],coeIn[vtxtp,-I Sqrt[3](cc["b10"]-cc["b11"])]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["K+"],mesout["K-"],coeIn[vtxtp,I(cc["b9"]-4cc["b11"])]],
(*+++ \[CapitalLambda] +++*)
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,-I(6cc["b10"]-cc["b9"])]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,-2I(cc["b10"])]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],mes["K+"],mesout["K-"],coeIn[vtxtp,-2I(cc["b10"])]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],mes["K0"],mesout["K0b"],coeIn[vtxtp,-2I(cc["b10"])]]
};


(* ::Chapter::Closed:: *)
(*Mass filed, electric magnetic current*)


(* ::Section::Closed:: *)
(*F1,meson,\[Phi]\[Phi]A*)


(*Subscript[e\[ScriptCapitalA], \[Mu]](SuperMinus[\[Pi]] . \!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\(\(.\)
\*SuperscriptBox[\(\[Pi]\), \(+\)]\)\)-SuperPlus[\[Pi]] . \!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\(\(.\)
\*SuperscriptBox[\(\[Pi]\), \(-\)]\)\))->i(Subscript[Q, u]-Subscript[Q, d])*)
(*++++++++++++++++++++++++++ \:7535\:78c1\:5b88\:6052\:6d41\:ff0c\:4ecb\:5b50 +++++++++++++++++++++*)
vtxtp=vtxType["F1","\[Phi]\[Phi]A"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
coeIn[type_,x_]:=type->vtxCoe[x](*\:7cfb\:6570\:7684\:7c7b\:578b\:ff0c\:4ee5\:53ca\:5e38\:6570\:56e0\:5b50*)
(* ----------------  ---------------- *)
vtx[unq["type"->vtxtp]]={
unq[mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,I(ch["u"]-ch["d"])]],
unq[mes["K+"],mesout["K-"],coeIn[vtxtp,I(ch["u"]-ch["s"])]],
unq[mes["K0"],mesout["K0b"],coeIn[vtxtp,I(ch["d"]-ch["s"])]],
unq[mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,0]],
unq[mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,0]],
unq[mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
(*\:4ea4\:6362\:5165\:5c04\:548c\:51fa\:5c04\:7c92\:5b50*)
unq[mes["\[Pi]-"],mesout["\[Pi]+"],coeIn[vtxtp,-I(ch["u"]-ch["d"])]],
unq[mes["K-"],mesout["K+"],coeIn[vtxtp,-I(ch["u"]-ch["s"])]],
unq[mes["K0b"],mesout["K0"],coeIn[vtxtp,-I(ch["d"]-ch["s"])]]
};


(* ::Section::Closed:: *)
(*F1,oder1,BBA,*)


(*Overscript[p, _].\[Gamma]^\[Mu].p.Subscript[\[ScriptCapitalA], \[Mu]]->2u+d*)
(*++++++++++++++++++++++++++ \:7535\:78c1\:5b88\:6052\:6d41 BB\[Phi]\[Phi]A +++++++++++++++++++++*)
vtxtp=vtxType["F1","oct"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
coeIn[type_,x_]:=type->vtxCoe[x](*\:7cfb\:6570\:7684\:7c7b\:578b\:ff0c\:4ee5\:53ca\:5e38\:6570\:56e0\:5b50*)
(* ----------------  ---------------- *)
vtx[unq["type"->vtxtp]]={
(*+++ proton +++*)
unq[oct["pb"],oct["p"],coeIn[vtxtp,2ch["u"]+ch["d"]]],
(*+++ neurton +++*)
unq[oct["nb"],oct["n"],coeIn[vtxtp,ch["u"]+2ch["d"]]],
(*+++ \[CapitalSigma]+ +++*)
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],coeIn[vtxtp,2ch["u"]+ch["s"]]],
(*+++ \[CapitalSigma]0 +++*)
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],coeIn[vtxtp,ch["u"]+ch["d"]+ch["s"]]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalLambda]"],coeIn[vtxtp,0]],(*\:6dfb\:52a0\:7684\:989d\:5916\:9879,\:5bf9\:5e94\:53cd\:5e38\:78c1\:77e9*)
(*+++ \[CapitalSigma]- +++*)
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],coeIn[vtxtp,2ch["d"]+ch["s"]]],
(*+++ \[CapitalXi]0 +++*)
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],coeIn[vtxtp,2ch["d"]+ch["s"]]],
(*+++ \[CapitalXi]- +++*)
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],coeIn[vtxtp,ch["d"]+2ch["s"]]],
(*+++ \[CapitalLambda] +++*)
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],coeIn[vtxtp,ch["u"]+ch["d"]+ch["s"]]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalSigma]0"],coeIn[vtxtp,0]](*\:6dfb\:52a0\:7684\:989d\:5916\:9879,\:5bf9\:5e94\:53cd\:5e38\:78c1\:77e9*)
};


(* ::Section::Closed:: *)
(*F1,BB\[Phi]A, DF*)


(* e.Subscript[\[ScriptCapitalA], \[Mu]].(Overscript[n, _].\[Gamma]^\[Mu].\[Gamma]^5.p.\[Pi]^--Overscript[p, _].\[Gamma]^\[Mu].\[Gamma]^5.n.\[Pi]^+)\[Rule](i(D+F)(Subscript[Q, u]-Subscript[Q, d]))/(Sqrt[2] Subscript[f, \[Phi]]) *)
(*++++++++++++++++++++++++++\:7535\:78c1\:5b88\:6052\:6d41 BB\[Phi]A, \:8f74\:77e2\:9879 DF+++++++++++++++++++++*)
vtxtp=vtxType["F1","DF","mesIn"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
vtxtpStr=vtxType["F1","DF","mesIn","stro"];(*\:5206\:79bb\:51fa\:5f3a\:4f5c\:7528\:90e8\:5206\:7cfb\:6570*) 
coeIn[type_,x_]:=type->vtxCoe[x/cc["f"]](*\:7cfb\:6570\:7684\:7c7b\:578b\:ff0c\:4ee5\:53ca\:5e38\:6570\:56e0\:5b50*)
(* ----------------  ---------------- *)
vtx[unq["type"->vtxtp]]={
(*+++ proton +++*)
(*unq[oct["p"],oct["pb"],mes["\[Pi]0"],coeIn[vtxtp,0],coeIn[vtxtpStr,(cc["D"]+cc["F"])/2]],
unq[oct["p"],oct["pb"],mes["\[Eta]8"],coeIn[vtxtp,0],coeIn[vtxtpStr,-(cc["D"]-3cc["F"])/(2Sqrt[3])]],
unq[oct["p"],oct["pb"],mes["\[Eta]0"],coeIn[vtxtp,0],coeIn[vtxtpStr,0]],*)
(* --------------  -------------- *)
unq[oct["p"],oct["nb"],mes["\[Pi]-"],coeIn[vtxtp,(I(cc["D"]+cc["F"])(ch["u"]-ch["d"]))/Sqrt[2]],
coeIn[vtxtpStr,(cc["D"]+cc["F"])/Sqrt[2]]],
unq[oct["p"],oct["\[CapitalSigma]0b"],mes["K-"],coeIn[vtxtp,(I(cc["D"]-cc["F"])(ch["u"]-ch["s"]))/2],
coeIn[vtxtpStr,(cc["D"]-cc["F"])/2]],
unq[oct["p"],oct["\[CapitalSigma]+b"],mes["K0b"],coeIn[vtxtp,(I(cc["D"]-cc["F"])(ch["d"]-ch["s"]))/Sqrt[2]],
coeIn[vtxtpStr,(cc["D"]-cc["F"])/Sqrt[2]]],
unq[oct["p"],oct["\[CapitalLambda]b"],mes["K-"],coeIn[vtxtp,(-I(cc["D"]+3cc["F"])(ch["u"]-ch["s"]))/(2Sqrt[3])],
coeIn[vtxtpStr,-(cc["D"]+3cc["F"])/(2Sqrt[3])]],
(*+++ neurton +++*)
(*unq[oct["n"],oct["nb"],mes["\[Pi]0"],coeIn[vtxtp,0],coeIn[vtxtpStr,-(cc["D"]+cc["F"])/2]],
unq[oct["n"],oct["nb"],mes["\[Eta]8"],coeIn[vtxtp,0],coeIn[vtxtpStr,-(cc["D"]-3cc["F"])/(2Sqrt[3])]],
unq[oct["n"],oct["nb"],mes["\[Eta]0"],coeIn[vtxtp,0],coeIn[vtxtpStr,0]],*)
(* --------------  -------------- *)
unq[oct["n"],oct["pb"],mes["\[Pi]+"],coeIn[vtxtp,(-I(cc["D"]+cc["F"])(ch["u"]-ch["d"]))/Sqrt[2]],
coeIn[vtxtpStr,(cc["D"]+cc["F"])/Sqrt[2]]],
unq[oct["n"],oct["\[CapitalSigma]0b"],mes["K0b"],coeIn[vtxtp,(-I(cc["D"]-cc["F"])(ch["d"]-ch["s"]))/2],
coeIn[vtxtpStr,(-cc["D"]+cc["F"])/2]],
unq[oct["n"],oct["\[CapitalSigma]-b"],mes["K-"],coeIn[vtxtp,(I(cc["D"]-cc["F"])(ch["u"]-ch["s"]))/Sqrt[2]],
coeIn[vtxtpStr,(cc["D"]-cc["F"])/Sqrt[2]]],
unq[oct["n"],oct["\[CapitalLambda]b"],mes["K0b"],coeIn[vtxtp,(-I(cc["D"]+3cc["F"])(ch["d"]-ch["s"]))/(2Sqrt[3])],
coeIn[vtxtpStr,(cc["D"]+3cc["F"])/(-2 Sqrt[3])]],
(*+++ \[CapitalSigma]+ +++*)
(*unq[oct["\[CapitalSigma]+"],oct["\[CapitalSigma]+b"],mes["\[Pi]0"],coeIn[vtxtp,0],coeIn[vtxtpStr,(cc["F"])/1]],
unq[oct["\[CapitalSigma]+"],oct["\[CapitalSigma]+b"],mes["\[Eta]8"],coeIn[vtxtp,0],coeIn[vtxtpStr,(cc["D"])/Sqrt[3]]],
unq[oct["\[CapitalSigma]+"],oct["\[CapitalSigma]+b"],mes["\[Eta]0"],coeIn[vtxtp,0],coeIn[vtxtpStr,0]],*)
(* --------------  -------------- *)
unq[oct["\[CapitalSigma]+"],oct["\[CapitalLambda]b"],mes["\[Pi]-"],coeIn[vtxtp,(I(cc["D"])(ch["u"]-ch["d"]))/Sqrt[3]],
coeIn[vtxtpStr,(cc["D"])/Sqrt[3]]],
unq[oct["\[CapitalSigma]+"],oct["\[CapitalSigma]0b"],mes["\[Pi]-"],coeIn[vtxtp,(-I(cc["F"])(ch["u"]-ch["d"]))/1],
coeIn[vtxtpStr,-(cc["F"])/Sqrt[1]]],
unq[oct["\[CapitalSigma]+"],oct["\[CapitalXi]0b"],mes["K-"],coeIn[vtxtp,(I(cc["D"]+cc["F"])(ch["u"]-ch["s"]))/Sqrt[2]],
coeIn[vtxtpStr,(cc["D"]+cc["F"])/Sqrt[2]]],
unq[oct["\[CapitalSigma]+"],oct["pb"],mes["K0"],coeIn[vtxtp,(-I(cc["D"]-cc["F"])(ch["d"]-ch["s"]))/Sqrt[2]],
coeIn[vtxtpStr,(cc["D"]-cc["F"])/Sqrt[2]]],
(*+++ \[CapitalSigma]0 +++*)
unq[oct["\[CapitalSigma]0"],oct["\[CapitalLambda]b"],mes["\[Pi]0"],coeIn[vtxtp,0],
coeIn[vtxtpStr,(cc["D"])/Sqrt[3]]],
unq[oct["\[CapitalSigma]0"],oct["\[CapitalSigma]0b"],mes["\[Eta]8"],coeIn[vtxtp,0],
coeIn[vtxtpStr,(cc["D"])/Sqrt[3]]],
unq[oct["\[CapitalSigma]0"],oct["\[CapitalSigma]0b"],mes["\[Eta]0"],coeIn[vtxtp,0],
coeIn[vtxtpStr,undefined]],
(* --------------  -------------- *)
unq[oct["\[CapitalSigma]0"],oct["\[CapitalSigma]+b"],mes["\[Pi]+"],coeIn[vtxtp,(I(cc["F"])(ch["u"]-ch["d"]))/1],
coeIn[vtxtpStr,-(cc["F"])/1]],
unq[oct["\[CapitalSigma]0"],oct["\[CapitalSigma]-b"],mes["\[Pi]-"],coeIn[vtxtp,(I(cc["F"])(ch["u"]-ch["d"]))/1],
coeIn[vtxtpStr,(cc["F"])/1]],
unq[oct["\[CapitalSigma]0"],oct["\[CapitalXi]-b"],mes["K-"],coeIn[vtxtp,(I(cc["D"]+cc["F"])(ch["u"]-ch["s"]))/2],
coeIn[vtxtpStr,(cc["D"]+cc["F"])/2]],
unq[oct["\[CapitalSigma]0"],oct["\[CapitalXi]0b"],mes["K0b"],coeIn[vtxtp,(-I(cc["D"]+cc["F"])(ch["d"]-ch["s"]))/2],
coeIn[vtxtpStr,(cc["D"]+cc["F"])/-2]],
unq[oct["\[CapitalSigma]0"],oct["pb"],mes["K+"],coeIn[vtxtp,(-I(cc["D"]-cc["F"])(ch["u"]-ch["s"]))/2],
coeIn[vtxtpStr,(cc["D"]-cc["F"])/2]],
unq[oct["\[CapitalSigma]0"],oct["nb"],mes["K0"],coeIn[vtxtp,(I(cc["D"]-cc["F"])(ch["d"]-ch["s"]))/2],
coeIn[vtxtpStr,(-cc["D"]+cc["F"])/2]],
(*+++ \[CapitalSigma]- +++*)
(*unq[oct["\[CapitalSigma]-"],oct["\[CapitalSigma]-b"],mes["\[Pi]0"],coeIn[vtxtp,0],coeIn[vtxtpStr,-(cc["F"])/1]],
unq[oct["\[CapitalSigma]-"],oct["\[CapitalSigma]-b"],mes["\[Eta]8"],coeIn[vtxtp,0],coeIn[vtxtpStr,(cc["D"])/Sqrt[3]]],
unq[oct["\[CapitalSigma]-"],oct["\[CapitalSigma]-b"],mes["\[Eta]0"],coeIn[vtxtp,0],coeIn[vtxtpStr,0]],*)
(* --------------  -------------- *)
unq[oct["\[CapitalSigma]-"],oct["\[CapitalLambda]b"],mes["\[Pi]+"],coeIn[vtxtp,(-I(cc["D"])(ch["u"]-ch["d"]))/Sqrt[3]],
coeIn[vtxtpStr,(cc["D"])/Sqrt[3]]],
unq[oct["\[CapitalSigma]-"],oct["\[CapitalSigma]0b"],mes["\[Pi]+"],coeIn[vtxtp,(-I(cc["F"])(ch["u"]-ch["d"]))/1],
coeIn[vtxtpStr,(cc["F"])/1]],
unq[oct["\[CapitalSigma]-"],oct["\[CapitalXi]-b"],mes["K0b"],coeIn[vtxtp,(I(cc["D"]+cc["F"])(ch["d"]-ch["s"]))/Sqrt[2]],
coeIn[vtxtpStr,(cc["D"]+cc["F"])/Sqrt[2]]],
unq[oct["\[CapitalSigma]-"],oct["nb"],mes["K+"],coeIn[vtxtp,(-I(cc["D"]-cc["F"])(ch["u"]-ch["s"]))/Sqrt[2]],
coeIn[vtxtpStr,(cc["D"]-cc["F"])/Sqrt[2]]],
(*+++ \[CapitalXi]0 +++*)
(*unq[oct["\[CapitalXi]0"],oct["\[CapitalXi]0b"],mes["\[Pi]0"],coeIn[vtxtp,0],coeIn[vtxtpStr,(-cc["D"]+cc["F"])/2]],
unq[oct["\[CapitalXi]0"],oct["\[CapitalXi]0b"],mes["\[Eta]8"],coeIn[vtxtp,0],coeIn[vtxtpStr,(cc["D"]+3cc["F"])/(-2Sqrt[3])]],
unq[oct["\[CapitalXi]0"],oct["\[CapitalXi]0b"],mes["\[Eta]0"],coeIn[vtxtp,0],coeIn[vtxtpStr,0]],*)
(* --------------  -------------- *)
unq[oct["\[CapitalXi]0"],oct["\[CapitalXi]-b"],mes["\[Pi]-"],coeIn[vtxtp,(I(cc["D"]-cc["F"])(ch["u"]-ch["d"]))/Sqrt[2]],
coeIn[vtxtpStr,(cc["D"]-cc["F"])/Sqrt[2]]],
unq[oct["\[CapitalXi]0"],oct["\[CapitalSigma]0b"],mes["K0"],coeIn[vtxtp,(I(cc["D"]+cc["F"])(ch["d"]-ch["s"]))/2],
coeIn[vtxtpStr,(cc["D"]+cc["F"])/-2]],
unq[oct["\[CapitalXi]0"],oct["\[CapitalSigma]+b"],mes["K+"],coeIn[vtxtp,(-I(cc["D"]+cc["F"])(ch["u"]-ch["s"]))/Sqrt[2]],
coeIn[vtxtpStr,(cc["D"]+cc["F"])/Sqrt[2]]],
unq[oct["\[CapitalXi]0"],oct["\[CapitalLambda]b"],mes["K0"],coeIn[vtxtp,(I(cc["D"]-3cc["F"])(ch["d"]-ch["s"]))/(2Sqrt[3])],
coeIn[vtxtpStr,(cc["D"]-3cc["F"])/(-2 Sqrt[3])]],
(*+++ \[CapitalXi]- +++*)
(*unq[oct["\[CapitalXi]-"],oct["\[CapitalXi]-b"],mes["\[Pi]0"],coeIn[vtxtp,0],coeIn[vtxtpStr,(cc["D"]-cc["F"])/2]],
unq[oct["\[CapitalXi]-"],oct["\[CapitalXi]-b"],mes["\[Eta]8"],coeIn[vtxtp,0],coeIn[vtxtpStr,(cc["D"]+3cc["F"])/(-2Sqrt[3])]],
unq[oct["\[CapitalXi]-"],oct["\[CapitalXi]-b"],mes["\[Eta]0"],coeIn[vtxtp,0],coeIn[vtxtpStr,0]],*)
(* --------------  -------------- *)
unq[oct["\[CapitalXi]-"],oct["\[CapitalXi]0b"],mes["\[Pi]+"],coeIn[vtxtp,(-I(cc["D"]-cc["F"])(ch["u"]-ch["d"]))/Sqrt[2]],
coeIn[vtxtpStr,(cc["D"]-cc["F"])/Sqrt[2]]],
unq[oct["\[CapitalXi]-"],oct["\[CapitalSigma]0b"],mes["K+"],coeIn[vtxtp,(-I(cc["D"]+cc["F"])(ch["u"]-ch["s"]))/2],
coeIn[vtxtpStr,(cc["D"]+cc["F"])/2]],
unq[oct["\[CapitalXi]-"],oct["\[CapitalSigma]-b"],mes["K0"],coeIn[vtxtp,(-I(cc["D"]+cc["F"])(ch["d"]-ch["s"]))/Sqrt[2]],
coeIn[vtxtpStr,(cc["D"]+cc["F"])/Sqrt[2]]],
unq[oct["\[CapitalXi]-"],oct["\[CapitalLambda]b"],mes["K+"],coeIn[vtxtp,(I(cc["D"]-3cc["F"])(ch["u"]-ch["s"]))/(2Sqrt[3])],
coeIn[vtxtpStr,(cc["D"]-3cc["F"])/(-2 Sqrt[3])]],
(*+++ \[CapitalLambda] +++*)
unq[oct["\[CapitalLambda]"],oct["\[CapitalSigma]0b"],mes["\[Pi]0"],coeIn[vtxtp,0],coeIn[vtxtpStr,(cc["D"])/Sqrt[3]]],
unq[oct["\[CapitalLambda]"],oct["\[CapitalLambda]b"],mes["\[Eta]8"],coeIn[vtxtp,0],coeIn[vtxtpStr,-(cc["D"])/Sqrt[3]]],
unq[oct["\[CapitalLambda]"],oct["\[CapitalLambda]b"],mes["\[Eta]0"],coeIn[vtxtp,0],coeIn[vtxtpStr,undefined]],
(* --------------  -------------- *)
unq[oct["\[CapitalLambda]"],oct["\[CapitalSigma]+b"],mes["\[Pi]+"],coeIn[vtxtp,(-I(cc["D"])(ch["u"]-ch["d"]))/Sqrt[3]],
coeIn[vtxtpStr,(cc["D"])/Sqrt[3]]],
unq[oct["\[CapitalLambda]"],oct["\[CapitalSigma]-b"],mes["\[Pi]-"],coeIn[vtxtp,(I(cc["D"])(ch["u"]-ch["d"]))/Sqrt[3]],
coeIn[vtxtpStr,(cc["D"])/Sqrt[3]]],
unq[oct["\[CapitalLambda]"],oct["pb"],mes["K+"],coeIn[vtxtp,(I(cc["D"]+3cc["F"])(ch["u"]-ch["s"]))/(2Sqrt[3])],
coeIn[vtxtpStr,(cc["D"]+3cc["F"])/(-2 Sqrt[3])]],
unq[oct["\[CapitalLambda]"],oct["nb"],mes["K0"],coeIn[vtxtp,(I(cc["D"]+3cc["F"])(ch["d"]-ch["s"]))/(2Sqrt[3])],
coeIn[vtxtpStr,(cc["D"]+3cc["F"])/(-2 Sqrt[3])]],
unq[oct["\[CapitalLambda]"],oct["\[CapitalXi]-b"],mes["K-"],coeIn[vtxtp,(-I(cc["D"]-3cc["F"])(ch["u"]-ch["s"]))/(2Sqrt[3])],
coeIn[vtxtpStr,(cc["D"]-3cc["F"])/(-2 Sqrt[3])]],
unq[oct["\[CapitalLambda]"],oct["\[CapitalXi]0b"],mes["K0b"],coeIn[vtxtp,(-I(cc["D"]-3cc["F"])(ch["d"]-ch["s"]))/(2Sqrt[3])],
coeIn[vtxtpStr,(cc["D"]-3cc["F"])/(-2 Sqrt[3])]]
};


vtxtp=vtxType["F1","DF","mesIn"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
vtxtpStr=vtxType["F1","DF","mesIn","stro"];(*\:5206\:79bb\:51fa\:5f3a\:4f5c\:7528\:90e8\:5206\:7cfb\:6570*)
vtxtpEM=vtxType["F1","DF","mesIn","EM"];(*\:5206\:79bb\:51fa\:5f3a\:4f5c\:7528\:90e8\:5206\:7cfb\:6570*)
coeIn[type_,x_]:=type->vtxCoe[x/cc["f"]](*\:7cfb\:6570\:7684\:7c7b\:578b\:ff0c\:4ee5\:53ca\:5e38\:6570\:56e0\:5b50*)
(* ------------- \:6dfb\:52a0 \:7535\:78c1\:6d41\:7cfb\:6570 -------------*)
vtx[unq["type"->vtxtp]]=Query[All,Append[#,
vtxtpEM->vtxCoe[Identity@@#@vtxtp/Identity@@#@vtxtpStr]
]&
]@vtx[unq["type"->vtxtp]];
(* ------------- \:6dfb\:52a0\:53e6\:5916\:534a\:90e8\:5206\:9876\:70b9\:ff0c\:6539\:53d8\:5404\:4e2a\:7c92\:5b50\:7684\:65b9\:5411 -------------*)
vtxtpFlip=vtxType["F1","DF","mesOut"];(* \:53e6\:4e00\:534a\:9876\:70b9 *)
vtxtpStrFlip=vtxType["F1","DF","mesOut","stro"];(*\:5206\:79bb\:51fa\:5f3a\:4f5c\:7528\:90e8\:5206\:7cfb\:6570*)
vtxtpEMFlip=vtxType["F1","DF","mesOut","EM"];(*\:5206\:79bb\:51fa\:5f3a\:4f5c\:7528\:90e8\:5206\:7cfb\:6570*)
(*--------------------------------------*)
vtx[unq["type"->vtxtpFlip]]=Query[All,<|
fdTypeOct->(#@fdTypeOctb/.fd[a_,b_,1]:>fd[a,b,0]),
fdTypeOctb->(#@fdTypeOct/.fd[a_,b_,0]:>fd[a,b,1]),
fdTypeMesOut->(#@fdTypeMes/.mesAntiRule),
vtxtpFlip->(#@vtxtp/.vtxCoe->vtxCoeAnti),
vtxtpStrFlip->(#@vtxtpStr),
vtxtpEMFlip->(#@vtxtpEM/.vtxCoe->vtxCoeAnti)
|>&
]@vtx[unq["type"->vtxtp]];


(* ::Section::Closed:: *)
(*F1,BT\[Phi]A, C*)


(*e.Subscript[\[ScriptCapitalA], \[Mu]] . (Overscript[p, _] . \[CapitalTheta]^\[Mu]\[Nu] . Subsuperscript[\[CapitalDelta], \[Nu], ++] . SuperMinus[\[Pi]]+Overscript[Subsuperscript[\[CapitalDelta], \[Nu], ++], _] . \[CapitalTheta]^\[Mu]\[Nu] . p . SuperPlus[\[Pi]])->(i(Subscript[Q, u]-Subscript[Q, d]))/Sqrt[2] \[ScriptCapitalC]/Subscript[f, \[Phi]]*)
(*++++++++++++++++++++++++++ \:7535\:78c1\:5b88\:6052\:6d41 BT\[Phi]A, \:8f74\:77e2\:9879 C +++++++++++++++++++++*)
vtxtp=vtxType["F1","C","mesIn"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
vtxtpStr=vtxType["F1","C","mesIn","stro"];(*\:5206\:79bb\:51fa\:5f3a\:4f5c\:7528\:90e8\:5206\:7cfb\:6570*)
coeIn[type_,x_]:=type->vtxCoe[cc["C"]/(Sqrt[2]cc["f"])*x](*\:7cfb\:6570\:7684\:7c7b\:578b\:ff0c\:4ee5\:53ca\:5e38\:6570\:56e0\:5b50*)
(* ----------------  ---------------- *)
vtx[unq["type"->vtxtp]]={
(*+++ proton +++*)
unq[oct["pb"],mes["\[Pi]-"],dec["\[CapitalDelta]++"],coeIn[vtxtp,(I(ch["u"]-ch["d"]))/Sqrt[1]],
coeIn[vtxtpStr,1/Sqrt[1]]],
unq[oct["pb"],mes["\[Pi]+"],dec["\[CapitalDelta]0"],coeIn[vtxtp,(I(ch["u"]-ch["d"]))/Sqrt[3]],
coeIn[vtxtpStr,-1/Sqrt[3]]],
unq[oct["pb"],mes["K+"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,(I(ch["u"]-ch["s"]))/Sqrt[6]],
coeIn[vtxtpStr,(-1)/(Sqrt[6])]],
unq[oct["pb"],mes["K0"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,(-I(ch["d"]-ch["s"]))/Sqrt[3]],
coeIn[vtxtpStr,1/Sqrt[3]]],
(*+++ neurton +++*)
unq[oct["nb"],mes["\[Pi]+"],dec["\[CapitalDelta]-"],coeIn[vtxtp,(I(ch["u"]-ch["d"]))/Sqrt[1]],
coeIn[vtxtpStr,-1/Sqrt[1]]],
unq[oct["nb"],mes["\[Pi]-"],dec["\[CapitalDelta]+"],coeIn[vtxtp,(I(ch["u"]-ch["d"]))/Sqrt[3]],
coeIn[vtxtpStr,1/Sqrt[3]]],
unq[oct["nb"],mes["K0"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,(-I(ch["d"]-ch["s"]))/Sqrt[6]],
coeIn[vtxtpStr,1/(Sqrt[6])]],
unq[oct["nb"],mes["K+"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,(I(ch["u"]-ch["s"]))/Sqrt[3]],
coeIn[vtxtpStr,-1/Sqrt[3]]],
(*+++ \[CapitalSigma]+ +++*)
unq[oct["\[CapitalSigma]+b"],mes["\[Pi]+"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,(-I(ch["u"]-ch["d"]))/Sqrt[6]],
coeIn[vtxtpStr,1/(Sqrt[6])]],
unq[oct["\[CapitalSigma]+b"],mes["K-"],dec["\[CapitalDelta]++"],coeIn[vtxtp,(-I(ch["u"]-ch["s"]))/Sqrt[1]],
coeIn[vtxtpStr,-1/Sqrt[1]]],
unq[oct["\[CapitalSigma]+b"],mes["K+"],dec["\[CapitalXi]*0"],coeIn[vtxtp,(-I(ch["u"]-ch["s"]))/Sqrt[3]],
coeIn[vtxtpStr,1/Sqrt[3]]],
unq[oct["\[CapitalSigma]+b"],mes["K0b"],dec["\[CapitalDelta]+"],coeIn[vtxtp,(-I(ch["d"]-ch["s"]))/Sqrt[3]],
coeIn[vtxtpStr,-1/Sqrt[3]]],
(*+++ \[CapitalSigma]0 +++*)
unq[oct["\[CapitalSigma]0b"],mes["\[Pi]-"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,(-I(ch["u"]-ch["d"]))/Sqrt[6]],
coeIn[vtxtpStr,-1/(Sqrt[6])]],
unq[oct["\[CapitalSigma]0b"],mes["\[Pi]+"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,(I(ch["u"]-ch["d"]))/Sqrt[6]],
coeIn[vtxtpStr,-1/(Sqrt[6])]],
unq[oct["\[CapitalSigma]0b"],mes["K-"],dec["\[CapitalDelta]+"],coeIn[vtxtp,(I(ch["u"]-ch["s"]))*Sqrt[2/3]],
coeIn[vtxtpStr,Sqrt[2/3]]],
unq[oct["\[CapitalSigma]0b"],mes["K+"],dec["\[CapitalXi]*-"],coeIn[vtxtp,(I(ch["u"]-ch["s"]))/Sqrt[6]],
coeIn[vtxtpStr,-1/(Sqrt[6])]],
unq[oct["\[CapitalSigma]0b"],mes["K0b"],dec["\[CapitalDelta]0"],coeIn[vtxtp,(I(ch["d"]-ch["s"]))*Sqrt[2/3]],
coeIn[vtxtpStr,Sqrt[2/3]]],
unq[oct["\[CapitalSigma]0b"],mes["K0"],dec["\[CapitalXi]*0"],coeIn[vtxtp,(I(ch["d"]-ch["s"]))/Sqrt[6]],
coeIn[vtxtpStr,-1/(Sqrt[6])]],
(*+++ \[CapitalSigma]- +++*)
unq[oct["\[CapitalSigma]-b"],mes["\[Pi]-"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,(-I(ch["u"]-ch["d"]))/Sqrt[6]],
coeIn[vtxtpStr,-1/(Sqrt[6])]],
unq[oct["\[CapitalSigma]-b"],mes["K0b"],dec["\[CapitalDelta]-"],coeIn[vtxtp,(I(ch["d"]-ch["s"]))/Sqrt[1]],
coeIn[vtxtpStr,1/Sqrt[1]]],
unq[oct["\[CapitalSigma]-b"],mes["K-"],dec["\[CapitalDelta]0"],coeIn[vtxtp,(I(ch["u"]-ch["s"]))/Sqrt[3]],
coeIn[vtxtpStr,1/Sqrt[3]]],
unq[oct["\[CapitalSigma]-b"],mes["K0"],dec["\[CapitalXi]*-"],coeIn[vtxtp,(I(ch["d"]-ch["s"]))/Sqrt[3]],
coeIn[vtxtpStr,-1/Sqrt[3]]],
(*+++ \[CapitalXi]0 +++*)
unq[oct["\[CapitalXi]0b"],mes["\[Pi]+"],dec["\[CapitalXi]*-"],coeIn[vtxtp,(-I(ch["u"]-ch["d"]))/Sqrt[3]],
coeIn[vtxtpStr,1/Sqrt[3]]],
unq[oct["\[CapitalXi]0b"],mes["K+"],dec["\[CapitalOmega]-"],coeIn[vtxtp,(-I(ch["u"]-ch["s"]))/Sqrt[1]],
coeIn[vtxtpStr,1/Sqrt[1]]],
unq[oct["\[CapitalXi]0b"],mes["K0b"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,(-I(ch["d"]-ch["s"]))/Sqrt[6]],
coeIn[vtxtpStr,-1/(Sqrt[6])]],
unq[oct["\[CapitalXi]0b"],mes["K-"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,(-I(ch["u"]-ch["s"]))/Sqrt[3]],
coeIn[vtxtpStr,-1/Sqrt[3]]],
(*+++ \[CapitalXi]- +++*)
unq[oct["\[CapitalXi]-b"],mes["\[Pi]-"],dec["\[CapitalXi]*0"],coeIn[vtxtp,(-I(ch["u"]-ch["d"]))/Sqrt[3]],
coeIn[vtxtpStr,-1/Sqrt[3]]],
unq[oct["\[CapitalXi]-b"],mes["K0"],dec["\[CapitalOmega]-"],coeIn[vtxtp,(I(ch["d"]-ch["s"]))/Sqrt[1]],
coeIn[vtxtpStr,-1/Sqrt[1]]],
unq[oct["\[CapitalXi]-b"],mes["K-"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,(I(ch["u"]-ch["s"]))/Sqrt[6]],
coeIn[vtxtpStr,1/(Sqrt[6])]],
unq[oct["\[CapitalXi]-b"],mes["K0b"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,(I(ch["d"]-ch["s"]))/Sqrt[3]],
coeIn[vtxtpStr,1/Sqrt[3]]],
(*+++ \[CapitalLambda] +++*)
unq[oct["\[CapitalLambda]b"],mes["\[Pi]-"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,(-I(ch["u"]-ch["d"]))/Sqrt[2]],
coeIn[vtxtpStr,-(1/Sqrt[2])]],
unq[oct["\[CapitalLambda]b"],mes["\[Pi]+"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,(-I(ch["u"]-ch["d"]))/Sqrt[2]],
coeIn[vtxtpStr,1/Sqrt[2]]],
unq[oct["\[CapitalLambda]b"],mes["K+"],dec["\[CapitalXi]*-"],coeIn[vtxtp,(-I(ch["u"]-ch["s"]))/Sqrt[2]],
coeIn[vtxtpStr,1/Sqrt[2]]],
unq[oct["\[CapitalLambda]b"],mes["K0"],dec["\[CapitalXi]*0"],coeIn[vtxtp,(I(ch["d"]-ch["s"]))/Sqrt[2]],
coeIn[vtxtpStr,-(1/Sqrt[2])]]
};


vtxtp=vtxType["F1","C","mesIn"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
vtxtpStr=vtxType["F1","C","mesIn","stro"];(*\:5206\:79bb\:51fa\:5f3a\:4f5c\:7528\:90e8\:5206\:7cfb\:6570*)
coeIn[type_,x_]:=type->vtxCoe[cc["C"]/(Sqrt[2]cc["f"])*x](*\:7cfb\:6570\:7684\:7c7b\:578b\:ff0c\:4ee5\:53ca\:5e38\:6570\:56e0\:5b50*)
vtxtpEM=vtxType["F1","C","mesIn","EM"];(*\:5206\:79bb\:51fa\:5f3a\:4f5c\:7528\:90e8\:5206\:7cfb\:6570*)
(* ------------- \:6dfb\:52a0 \:7535\:78c1\:6d41\:7cfb\:6570 -------------*)
vtx[unq["type"->vtxtp]]=Query[All,Append[#,
vtxtpEM->vtxCoe[Identity@@#@vtxtp/Identity@@#@vtxtpStr]
]&
]@vtx[unq["type"->vtxtp]];
(* ------------- \:6dfb\:52a0\:53e6\:5916\:534a\:90e8\:5206\:9876\:70b9,\:6539\:53d8\:5404\:4e2a\:7c92\:5b50\:7684\:65b9\:5411 -------------*)
vtxtpFlip=vtxType["F1","C","mesOut"];(* \:53e6\:4e00\:534a\:9876\:70b9 *)
vtxtpStrFlip=vtxType["F1","C","mesOut","stro"];(*\:5206\:79bb\:51fa\:5f3a\:4f5c\:7528\:90e8\:5206\:7cfb\:6570*) 
vtxtpEMFlip=vtxType["F1","C","mesOut","EM"];(*\:5206\:79bb\:51fa\:5f3a\:4f5c\:7528\:90e8\:5206\:7cfb\:6570*)
(*----------------------------------------------*)
vtx[unq["type"->vtxtpFlip]]=Query[All,
<|
fdTypeOct->(#@fdTypeOctb/.fd[a_,b_,1]:>fd[a,b,0]),
fdTypeDecb->(#@fdTypeDec/.fd[a_,b_,0]:>fd[a,b,1]),
fdTypeMesOut->(#@fdTypeMes/.mesAntiRule),
vtxtpFlip->(#@vtxtp),
vtxtpStrFlip->(#@vtxtpStr),
vtxtpEMFlip->(#@vtxtpEM)
|>&
]@vtx[unq["type"->vtxtp]];


(* ::Section::Closed:: *)
(*F1,oder2,BB\[Phi]\[Phi]A*)


(*Overscript[p, _].\[Gamma]^\[Mu].p.\[Pi]^+.\[Pi]^-.Subscript[\[ScriptCapitalA], \[Mu]]\[Rule]1/(2Subsuperscript[f, \[Phi], 2])Subscript[Q, d]*)
(*++++++++++++++++++++++++++ \:7535\:78c1\:5b88\:6052\:6d41 BB\[Phi]\[Phi]A +++++++++++++++++++++*)
vtxtp=vtxType["F1","oct","o2"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
vtxtpStr=vtxType["F1","oct","o2","stro"];(*\:5206\:79bb\:51fa\:5f3a\:4f5c\:7528\:90e8\:5206\:7cfb\:6570*)
coeIn[type_,x_]:=type->vtxCoe[x/cc["f"]^2](*\:7cfb\:6570\:7684\:7c7b\:578b\:ff0c\:4ee5\:53ca\:5e38\:6570\:56e0\:5b50*)
(* ----------------  ---------------- *)
vtx[unq["type"->vtxtp]]={
(*+++ proton +++*)
unq[oct["pb"],oct["p"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,ch["u"]/4],
coeIn[vtxtpStr,I/4]],
unq[oct["pb"],oct["p"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["u"]-4ch["s"])/12],
coeIn[vtxtpStr,-I/4]],
unq[oct["pb"],oct["p"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0],
coeIn[vtxtpStr,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["pb"],oct["p"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,ch["u"]/(2Sqrt[3])],
coeIn[vtxtpStr,I/(4Sqrt[3])]],
unq[oct["pb"],oct["p"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,ch["d"]/2],
coeIn[vtxtpStr,I/4]],
unq[oct["pb"],oct["p"],mes["K0"],mesout["K0b"],coeIn[vtxtp,-(ch["d"]/2)],
coeIn[vtxtpStr,-I/4]],
unq[oct["pb"],oct["p"],mes["K+"],mesout["K-"],coeIn[vtxtp,(-ch["u"]+ch["s"])/2],
coeIn[vtxtpStr,I/2]],
(*+++ neurton +++*)
unq[oct["nb"],oct["n"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,ch["d"]/4],
coeIn[vtxtpStr,I/4]],
unq[oct["nb"],oct["n"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["d"]-4ch["s"])/12],
coeIn[vtxtpStr,-I/4]],
unq[oct["nb"],oct["n"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0],
coeIn[vtxtpStr,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["nb"],oct["n"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,-ch["d"]/(2Sqrt[3])],
coeIn[vtxtpStr,-I/(4Sqrt[3])]],
unq[oct["nb"],oct["n"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,ch["u"]/2],
coeIn[vtxtpStr,-I/4]],
unq[oct["nb"],oct["n"],mes["K0"],mesout["K0b"],coeIn[vtxtp,(-ch["d"]+ch["s"])/2],
coeIn[vtxtpStr,-I/2]],
unq[oct["nb"],oct["n"],mes["K+"],mesout["K-"],coeIn[vtxtp,-ch["u"]/2],
coeIn[vtxtpStr,I/4]],
(*+++ \[CapitalSigma]+ +++*)
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(ch["u"]-ch["d"])/4],
coeIn[vtxtpStr,0]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["u"]-ch["d"])/12],
coeIn[vtxtpStr,0]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0],
coeIn[vtxtpStr,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["u"]+ch["d"])/(2Sqrt[3])],
coeIn[vtxtpStr,I/(2Sqrt[3])]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,(-ch["u"]+ch["d"])/2],
coeIn[vtxtpStr,I/2]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["K0"],mesout["K0b"],coeIn[vtxtp,-ch["s"]/2],
coeIn[vtxtpStr,I/4]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["K+"],mesout["K-"],coeIn[vtxtp,ch["s"]/2],
coeIn[vtxtpStr,I/4]],
(*+++ \[CapitalSigma]0 +++*)

(*+++ \[CapitalSigma]- +++*)
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(-ch["u"]+ch["d"])/4],
coeIn[vtxtpStr,0]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(-ch["u"]+ch["d"])/12],
coeIn[vtxtpStr,0]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0],
coeIn[vtxtpStr,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(-ch["u"]-ch["d"])/(2Sqrt[3])],
coeIn[vtxtpStr,-I/(2Sqrt[3])]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,(ch["u"]-ch["d"])/2],
coeIn[vtxtpStr,-I/2]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["K0"],mesout["K0b"],coeIn[vtxtp,ch["s"]/2],
coeIn[vtxtpStr,-I/4]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["K+"],mesout["K-"],coeIn[vtxtp,-ch["s"]/2],
coeIn[vtxtpStr,-I/4]],
(*+++ \[CapitalXi]0 +++*)
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(-ch["u"]+ch["d"])/4],
coeIn[vtxtpStr,-I/4]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(-ch["u"]+ch["d"])/12],
coeIn[vtxtpStr,I/4]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0],
coeIn[vtxtpStr,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(-ch["u"]-ch["d"])/(2Sqrt[3])],
coeIn[vtxtpStr,I/(4Sqrt[3])]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,(ch["u"]-ch["d"])/2],
coeIn[vtxtpStr,I/4]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["K0"],mesout["K0b"],coeIn[vtxtp,ch["s"]/2],
coeIn[vtxtpStr,I/2]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["K+"],mesout["K-"],coeIn[vtxtp,-ch["s"]/2],
coeIn[vtxtpStr,-I/4]],
(*+++ \[CapitalXi]- +++*)
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,-ch["u"]/4],
coeIn[vtxtpStr,-I/4]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,-(ch["u"]-4ch["s"])/12],
coeIn[vtxtpStr,I/4]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0],
coeIn[vtxtpStr,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,-ch["u"]/(2Sqrt[3])],
coeIn[vtxtpStr,-I/(4Sqrt[3])]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,-ch["d"]/2],
coeIn[vtxtpStr,-I/4]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["K0"],mesout["K0b"],coeIn[vtxtp,ch["d"]/2],
coeIn[vtxtpStr,I/2]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["K+"],mesout["K-"],coeIn[vtxtp,(ch["u"]-ch["s"])/2],
coeIn[vtxtpStr,-I/2]]
(*+++ \[CapitalLambda] +++*)

};


(* ::Section::Closed:: *)
(*F1,TTA,order 1*)


(*e . Subscript[\[ScriptCapitalA], \[Mu]] . Overscript[Subsuperscript[\[CapitalDelta], \[Nu], ++], _] . \[Gamma]^\[Nu]\[Alpha]\[Mu] . Subsuperscript[\[CapitalDelta], \[Alpha], ++] . SuperPlus[\[Pi]] . SuperMinus[\[Pi]]->(3Subscript[Q, d])/(2Subsuperscript[f, \[Phi], 2])*)
(*++++++++++++++++++++++++++ \:7535\:78c1\:5b88\:6052\:6d41 TTA, TTMMA +++++++++++++++++++++*)
vtxtp=vtxType["F1","dec"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
coeIn[type_,x_]:=type->vtxCoe[x](*\:7cfb\:6570\:7684\:7c7b\:578b\:ff0c\:4ee5\:53ca\:5e38\:6570\:56e0\:5b50*)
(* ----------------  ---------------- *)
vtx[unq["type"->vtxtp]]={
(*+++ \[CapitalDelta]++ +++*)
unq[dec["\[CapitalDelta]++b"],dec["\[CapitalDelta]++"],coeIn[vtxtp,3ch["u"]]],
(*+++ \[CapitalDelta]+ +++*)
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalDelta]+"],coeIn[vtxtp,2ch["u"]+ch["d"]]],
(*+++ \[CapitalDelta]0 +++*)
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalDelta]0"],coeIn[vtxtp,ch["u"]+2ch["d"]]],
(*+++ \[CapitalDelta]- +++*)
unq[dec["\[CapitalDelta]-b"],dec["\[CapitalDelta]-"],coeIn[vtxtp,3ch["d"]]],
(*+++ \[CapitalSigma]*+ +++*)
unq[dec["\[CapitalSigma]*+b"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,2ch["u"]+ch["s"]]],
(*+++ \[CapitalSigma]*0 +++*)
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,ch["u"]+ch["d"]+ch["s"]]],
(*+++ \[CapitalSigma]*- +++*)
unq[dec["\[CapitalSigma]*-b"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,2ch["d"]+ch["s"]]],
(*+++ \[CapitalXi]*0 +++*)
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalXi]*0"],coeIn[vtxtp,ch["u"]+2ch["s"]]],
(*+++ \[CapitalXi]*- +++*)
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalXi]*-"],coeIn[vtxtp,ch["d"]+2ch["s"]]],
(*+++ \[CapitalOmega]- +++*)
unq[dec["\[CapitalOmega]-b"],dec["\[CapitalOmega]-"],coeIn[vtxtp,3ch["s"]]]
};


(* ::Section::Closed:: *)
(*F1,TT\[Phi]\[Phi]A, order 2*)


(*e . Subscript[\[ScriptCapitalA], \[Mu]] . Overscript[Subsuperscript[\[CapitalDelta], \[Nu], ++], _] . \[Gamma]^\[Nu]\[Alpha]\[Mu] . Subsuperscript[\[CapitalDelta], \[Alpha], ++] . SuperPlus[\[Pi]] . SuperMinus[\[Pi]]->(3Subscript[Q, d])/(2Subsuperscript[f, \[Phi], 2])*)
(*++++++++++++++++++++++++++ \:7535\:78c1\:5b88\:6052\:6d41 TTA, TTMMA +++++++++++++++++++++*)
vtxtp=vtxType["F1","dec","o2"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
coeIn[type_,x_]:=type->vtxCoe[x/cc["f"]^2](*\:7cfb\:6570\:7684\:7c7b\:578b\:ff0c\:4ee5\:53ca\:5e38\:6570\:56e0\:5b50*)
(* ----------------  ---------------- *)
vtx[unq["type"->vtxtp]]={
(*+++ \[CapitalDelta]++ +++*)
unq[dec["\[CapitalDelta]++b"],dec["\[CapitalDelta]++"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(3ch["u"])/4]],
unq[dec["\[CapitalDelta]++b"],dec["\[CapitalDelta]++"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,ch["u"]/4]],
unq[dec["\[CapitalDelta]++b"],dec["\[CapitalDelta]++"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalDelta]++b"],dec["\[CapitalDelta]++"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(Sqrt[3]ch["u"])/2]],
unq[dec["\[CapitalDelta]++b"],dec["\[CapitalDelta]++"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,(3ch["d"])/2]],
unq[dec["\[CapitalDelta]++b"],dec["\[CapitalDelta]++"],mes["K+"],mesout["K-"],coeIn[vtxtp,(3ch["s"])/2]],
(*+++ \[CapitalDelta]+ +++*)
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalDelta]+"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(2ch["u"]+ch["d"])/4]],
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalDelta]+"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(2ch["u"]+ch["d"])/12]],
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalDelta]+"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalDelta]+"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(2ch["u"]-ch["d"])/(2Sqrt[3])]],
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalDelta]+"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,(ch["u"]+2ch["d"])/2]],
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalDelta]+"],mes["K+"],mesout["K-"],coeIn[vtxtp,ch["s"]]],
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalDelta]+"],mes["K0"],mesout["K0b"],coeIn[vtxtp,ch["s"]/2]],
(*+++ \[CapitalDelta]0 +++*)
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalDelta]0"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(ch["u"]+2ch["d"])/4]],
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalDelta]0"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["u"]+2ch["d"])/12]],
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalDelta]0"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalDelta]0"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["u"]-2ch["d"])/(2Sqrt[3])]],
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalDelta]0"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,(2ch["u"]+ch["d"])/2]],
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalDelta]0"],mes["K+"],mesout["K-"],coeIn[vtxtp,ch["s"]/2]],
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalDelta]0"],mes["K0"],mesout["K0b"],coeIn[vtxtp,ch["s"]]],
(*+++ \[CapitalDelta]- +++*)
unq[dec["\[CapitalDelta]-b"],dec["\[CapitalDelta]-"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(3ch["d"])/4]],
unq[dec["\[CapitalDelta]-b"],dec["\[CapitalDelta]-"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,ch["d"]/4]],
unq[dec["\[CapitalDelta]-b"],dec["\[CapitalDelta]-"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalDelta]-b"],dec["\[CapitalDelta]-"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(-Sqrt[3]ch["d"])/2]],
unq[dec["\[CapitalDelta]-b"],dec["\[CapitalDelta]-"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,(3ch["u"])/2]],
unq[dec["\[CapitalDelta]-b"],dec["\[CapitalDelta]-"],mes["K0"],mesout["K0b"],coeIn[vtxtp,(3ch["s"])/2]],
(*+++ \[CapitalSigma]*+ +++*)
unq[dec["\[CapitalSigma]*+b"],dec["\[CapitalSigma]*+"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,ch["u"]/2]],
unq[dec["\[CapitalSigma]*+b"],dec["\[CapitalSigma]*+"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["u"]+2ch["s"])/6]],
unq[dec["\[CapitalSigma]*+b"],dec["\[CapitalSigma]*+"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalSigma]*+b"],dec["\[CapitalSigma]*+"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,ch["u"]/Sqrt[3]]],
unq[dec["\[CapitalSigma]*+b"],dec["\[CapitalSigma]*+"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,ch["d"]]],
unq[dec["\[CapitalSigma]*+b"],dec["\[CapitalSigma]*+"],mes["K+"],mesout["K-"],coeIn[vtxtp,(ch["u"]+2ch["s"])/2]],
unq[dec["\[CapitalSigma]*+b"],dec["\[CapitalSigma]*+"],mes["K0"],mesout["K0b"],coeIn[vtxtp,ch["d"]/2]],
(*+++ \[CapitalSigma]*0 +++*)
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalSigma]*0"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(ch["u"]+ch["d"])/4]],
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalSigma]*0"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["u"]+ch["d"]+4ch["s"])/12]],
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalSigma]*0"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalSigma]*0"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["u"]-ch["d"])/(2Sqrt[3])]],
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalSigma]*0"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,(ch["u"]+ch["d"])/2]],
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalSigma]*0"],mes["K+"],mesout["K-"],coeIn[vtxtp,(ch["u"]+2ch["s"])/2]],
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalSigma]*0"],mes["K0"],mesout["K0b"],coeIn[vtxtp,(ch["d"]+2ch["s"])/2]],
(*+++ \[CapitalSigma]*- +++*)
unq[dec["\[CapitalSigma]*-b"],dec["\[CapitalSigma]*-"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,ch["d"]/2]],
unq[dec["\[CapitalSigma]*-b"],dec["\[CapitalSigma]*-"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["d"]+2ch["s"])/6]],
unq[dec["\[CapitalSigma]*-b"],dec["\[CapitalSigma]*-"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalSigma]*-b"],dec["\[CapitalSigma]*-"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,-ch["d"]/Sqrt[3]]],
unq[dec["\[CapitalSigma]*-b"],dec["\[CapitalSigma]*-"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,ch["u"]]],
unq[dec["\[CapitalSigma]*-b"],dec["\[CapitalSigma]*-"],mes["K+"],mesout["K-"],coeIn[vtxtp,ch["u"]/2]],
unq[dec["\[CapitalSigma]*-b"],dec["\[CapitalSigma]*-"],mes["K0"],mesout["K0b"],coeIn[vtxtp,(ch["d"]+2ch["s"])/2]],
(*+++ \[CapitalXi]*0 +++*)
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalXi]*0"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,ch["u"]/4]],
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalXi]*0"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["u"]+8ch["s"])/12]],
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalXi]*0"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalXi]*0"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,ch["u"]/(2Sqrt[3])]],
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalXi]*0"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,ch["d"]/2]],
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalXi]*0"],mes["K+"],mesout["K-"],coeIn[vtxtp,(2ch["u"]+ch["s"])/2]],
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalXi]*0"],mes["K0"],mesout["K0b"],coeIn[vtxtp,ch["d"]]],
(*+++ \[CapitalXi]*- +++*)
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalXi]*-"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,ch["d"]/4]],
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalXi]*-"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["d"]+8ch["s"])/12]],
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalXi]*-"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalXi]*-"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,-ch["d"]/(2Sqrt[3])]],
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalXi]*-"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,ch["u"]/2]],
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalXi]*-"],mes["K+"],mesout["K-"],coeIn[vtxtp,ch["u"]]],
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalXi]*-"],mes["K0"],mesout["K0b"],coeIn[vtxtp,(2ch["d"]+ch["s"])/2]],
(*+++ \[CapitalOmega]- +++*)
unq[dec["\[CapitalOmega]-b"],dec["\[CapitalOmega]-"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,ch["s"]]],
unq[dec["\[CapitalOmega]-b"],dec["\[CapitalOmega]-"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalOmega]-b"],dec["\[CapitalOmega]-"],mes["K+"],mesout["K-"],coeIn[vtxtp,(3ch["u"])/2]],
unq[dec["\[CapitalOmega]-b"],dec["\[CapitalOmega]-"],mes["K0"],mesout["K0b"],coeIn[vtxtp,(3ch["d"])/3]]
};


(* ::Chapter::Closed:: *)
(*anomalous magnetic moment*)


(* ::Section::Closed:: *)
(*octet,order1,BBA*)


(*e/(4Subscript[M, N]) \[ScriptCapitalF]^\[Mu]\[Nu] . Overscript[p, _] . Subscript[\[Sigma], \[Mu]\[Nu]] . p->Subscript[c, 2](Subscript[Q, d]+2Subscript[Q, u])-Subscript[c, 1] Subscript[Q, d]*)
(*++++++++++++++++++++++++++ \:53cd\:5e38\:78c1\:77e9,\:516b\:91cd\:6001\:91cd\:5b50 +++++++++++++++++++++*)
vtxtp=vtxType["F2","oct"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
coeIn[type_,x_]:=type->vtxCoe[x](*\:7cfb\:6570\:7684\:7c7b\:578b,\:4ee5\:53ca\:5e38\:6570\:56e0\:5b50*)
(* ----------------  ---------------- *)
vtx[unq["type"->vtxtp]]={
unq[oct["pb"],oct["p"],coeIn[vtxtp,-cc["c1"]*ch["d"]+cc["c2"](2ch["u"]+ch["d"])]],
unq[oct["nb"],oct["n"],coeIn[vtxtp,-cc["c1"]*ch["u"]+cc["c2"](ch["u"]+2ch["d"])]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],coeIn[vtxtp,-cc["c1"]*ch["s"]+cc["c2"](2ch["u"]+ch["s"])]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],coeIn[vtxtp,-cc["c1"]*ch["s"]+cc["c2"](ch["u"]+ch["d"]+ch["s"])]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],coeIn[vtxtp,-cc["c1"]*ch["s"]+cc["c2"](2ch["d"]+ch["s"])]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],coeIn[vtxtp,-cc["c1"]*ch["u"]+cc["c2"](ch["u"]+2ch["s"])]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],coeIn[vtxtp,-cc["c1"]*ch["d"]+cc["c2"](ch["d"]+2ch["s"])]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],coeIn[vtxtp,(cc["c1"](-2ch["u"]-2ch["d"]+ch["s"])+3cc["c2"](ch["u"]+ch["d"]+ch["s"]))/3]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalSigma]0"],coeIn[vtxtp,(cc["c1"](ch["u"]-ch["d"]))/Sqrt[3]]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalLambda]"],coeIn[vtxtp,(cc["c1"](ch["u"]-ch["d"]))/Sqrt[3]]]
};


(* ::Section::Closed:: *)
(*octet,order2,BB\[Phi]\[Phi]A*)


(*(e \[ScriptCapitalF]^\[Mu]\[Nu].Overscript[p, _].Subscript[\[Sigma], \[Mu]\[Nu]].p.\[Pi]^+.\[Pi]^-)/(8 Subsuperscript[f, \[Phi], 2] Subscript[M, N])\[Rule]-Subscript[c, 1] Subscript[Q, u]+Subscript[c, 2] (2 Subscript[Q, d]+Subscript[Q, u])*)
(*++++++++++++++++++++++++++ \:53cd\:5e38\:78c1\:77e9\:4e8c\:9636,\:516b\:91cd\:6001\:91cd\:5b50 +++++++++++++++++++++*)
vtxtp=vtxType["F2","oct","o2"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
coeIn[type_,x_]:=type->vtxCoe[x/(2cc["f"]^2)](*\:7cfb\:6570\:7684\:7c7b\:578b,\:4ee5\:53ca\:5e38\:6570\:56e0\:5b50*)
(* ----------------  ---------------- *)
vtx[unq["type"->vtxtp]]={
(*+++ proton +++*)
unq[oct["pb"],oct["p"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(ch["d"](cc["c2"]-cc["c1"])+2cc["c2"]*ch["u"])/2]],
unq[oct["pb"],oct["p"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["d"](cc["c2"]-cc["c1"])+2cc["c2"]*ch["u"])/6]],
unq[oct["pb"],oct["p"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["pb"],oct["p"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(-ch["d"](cc["c2"]-cc["c1"])+2cc["c2"]*ch["u"])/Sqrt[3]]],
unq[oct["pb"],oct["p"],mes["K0"],mesout["K0b"],coeIn[vtxtp,ch["s"](cc["c2"]-cc["c1"])]],
unq[oct["pb"],oct["p"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,ch["u"](cc["c2"]-cc["c1"])+2ch["d"]*cc["c2"]]],
unq[oct["pb"],oct["p"],mes["K+"],mesout["K-"],coeIn[vtxtp,2cc["c2"]*ch["s"]]],
(*+++ neutron +++*)
unq[oct["nb"],oct["n"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(ch["u"](cc["c2"]-cc["c1"])+2cc["c2"]*ch["d"])/2]],
unq[oct["nb"],oct["n"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["u"](cc["c2"]-cc["c1"])+2cc["c2"]*ch["d"])/6]],
unq[oct["nb"],oct["n"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["nb"],oct["n"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["u"](cc["c2"]-cc["c1"])-2cc["c2"]*ch["d"])/Sqrt[3]]],
unq[oct["nb"],oct["n"],mes["K0"],mesout["K0b"],coeIn[vtxtp,2cc["c2"]*ch["s"]]],
unq[oct["nb"],oct["n"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,ch["d"](cc["c2"]-cc["c1"])+2cc["c2"]*ch["u"]]],
unq[oct["nb"],oct["n"],mes["K+"],mesout["K-"],coeIn[vtxtp,ch["s"](cc["c2"]-cc["c1"])]],
(*+++ \[CapitalSigma]+ +++*)
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,cc["c2"]*ch["u"]]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(cc["c2"]*ch["u"]+2ch["s"](cc["c2"]-cc["c1"]))/3]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(2cc["c2"]*ch["u"])/Sqrt[3]]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["K0"],mesout["K0b"],coeIn[vtxtp,ch["d"](cc["c2"]-cc["c1"])]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,2cc["c2"]*ch["d"]]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["K+"],mesout["K-"],coeIn[vtxtp,ch["u"](cc["c2"]-cc["c1"])+2cc["c2"]*ch["s"]]],
(*+++ \[CapitalSigma]0 +++*)
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(cc["c2"](ch["u"]+ch["d"]))/2]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(cc["c2"](ch["u"]+ch["d"])+4ch["s"](cc["c2"]-cc["c1"]))/6]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(cc["c2"](ch["u"]-ch["d"]))/Sqrt[3]]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],mes["K0"],mesout["K0b"],coeIn[vtxtp,ch["d"](cc["c2"]-cc["c1"])+cc["c2"]*ch["s"]]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,cc["c2"](ch["u"]+ch["d"])]],
unq[oct["\[CapitalSigma]0b"],oct["\[CapitalSigma]0"],mes["K+"],mesout["K-"],coeIn[vtxtp,ch["u"](cc["c2"]-cc["c1"])+cc["c2"]*ch["s"]]],
(*+++ \[CapitalSigma]- +++*)
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,cc["c2"]*ch["d"]]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(cc["c2"]*ch["d"]+2ch["s"](cc["c2"]-cc["c1"]))/3]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(-2cc["c2"]*ch["d"])/Sqrt[3]]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["K0"],mesout["K0b"],coeIn[vtxtp,ch["d"](cc["c2"]-cc["c1"])+2cc["c2"]*ch["s"]]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,2cc["c2"]*ch["u"]]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["K+"],mesout["K-"],coeIn[vtxtp,ch["u"](cc["c2"]-cc["c1"])]],
(*+++ \[CapitalXi]0 +++*)
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(ch["u"](cc["c2"]-cc["c1"]))/2]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["u"](cc["c2"]-cc["c1"])+8cc["c2"]*ch["s"])/6]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["u"](cc["c2"]-cc["c1"]))/Sqrt[3]]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["K0"],mesout["K0b"],coeIn[vtxtp,2cc["c2"]*ch["d"]]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,ch["d"](cc["c2"]-cc["c1"])]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["K+"],mesout["K-"],coeIn[vtxtp,ch["s"](cc["c2"]-cc["c1"])+2cc["c2"]*ch["u"]]],
(*+++ \[CapitalXi]- +++*)
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(ch["d"](cc["c2"]-cc["c1"]))/2]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["d"](cc["c2"]-cc["c1"])+8cc["c2"]*ch["s"])/6]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(-ch["d"](cc["c2"]-cc["c1"]))/Sqrt[3]]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["K0"],mesout["K0b"],coeIn[vtxtp,ch["s"](cc["c2"]-cc["c1"])+2cc["c2"]*ch["d"]]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,ch["u"](cc["c2"]-cc["c1"])]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["K+"],mesout["K-"],coeIn[vtxtp,2cc["c2"]*ch["u"]]],
(*+++ \[CapitalLambda] +++*)
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(-(2cc["c1"]-3cc["c2"])(ch["u"]+ch["d"]))/6]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["d"](3cc["c2"]-2cc["c1"])+4ch["s"](cc["c1"]+3cc["c2"])+ch["u"](3cc["c2"]-2cc["c1"]))/18]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,((2cc["c1"]-3cc["c2"])(ch["d"]-ch["u"]))/(3Sqrt[3])]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],mes["K0"],mesout["K0b"],coeIn[vtxtp,(ch["d"](cc["c1"]+3cc["c2"])+ch["s"](3cc["c2"]-2cc["c1"]))/3]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,(-(2cc["c1"]-3cc["c2"])(ch["u"]+ch["d"]))/3]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalLambda]"],mes["K+"],mesout["K-"],coeIn[vtxtp,(ch["s"](3cc["c2"]-2cc["c1"])+ch["u"](cc["c1"]+3cc["c2"]))/3]],
(*+++ Overscript[\[CapitalLambda], _]-\[CapitalSigma]0 +++*)
unq[oct["\[CapitalLambda]b"],oct["\[CapitalSigma]0"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(cc["c1"](ch["u"]-ch["d"]))/(2Sqrt[3])]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalSigma]0"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(cc["c1"](ch["u"]-ch["d"]))/(6Sqrt[3])]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalSigma]0"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalLambda]b"],oct["\[CapitalSigma]0"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(cc["c1"](ch["u"]+ch["d"]))/3]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalSigma]0"],mes["K0"],mesout["K0b"],coeIn[vtxtp,(-cc["c1"]*ch["s"])/Sqrt[3]]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalSigma]0"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,(cc["c1"](ch["d"]-ch["u"]))/Sqrt[3]]],
unq[oct["\[CapitalLambda]b"],oct["\[CapitalSigma]0"],mes["K+"],mesout["K-"],coeIn[vtxtp,(cc["c1"]*ch["s"])/Sqrt[3]]]
};


(* ::Section::Closed:: *)
(*decuplet,order1,TTA*)


(*e/(4Subscript[M, T]) \[ScriptCapitalF]^\[Mu]\[Nu] . OverscriptBox[SubsuperscriptBox[\[CapitalDelta],\[Alpha],++],_] . Subscript[\[Sigma], \[Mu]\[Nu]] . \[CapitalDelta]^++\[Alpha]->Subscript[Q, u]3Subscript[c, T],Subscript[c, T]=1/2 (3Subscript[c, 2]+1)*)
(*++++++++++++++++++++++++++ \:53cd\:5e38\:78c1\:77e9,\:5341\:91cd\:6001\:91cd\:5b50 +++++++++++++++++++++*)
vtxtp=vtxType["F2","dec"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
coeIn[type_,x_]:=type->vtxCoe[cc["cT"]*x](*\:7cfb\:6570\:7684\:7c7b\:578b,\:4ee5\:53ca\:5e38\:6570\:56e0\:5b50*)
(* ----------------  ---------------- *)
vtx[unq["type"->vtxtp]]={
unq[dec["\[CapitalDelta]++b"],dec["\[CapitalDelta]++"],coeIn[vtxtp,3ch["u"]]],
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalDelta]+"],coeIn[vtxtp,2ch["u"]+ch["d"]]],
unq[dec["\[CapitalDelta]-b"],dec["\[CapitalDelta]-"],coeIn[vtxtp,3ch["d"]]],
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalDelta]0"],coeIn[vtxtp,ch["u"]+2ch["d"]]],
unq[dec["\[CapitalSigma]*+b"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,2ch["u"]+ch["s"]]],
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,ch["u"]+ch["d"]+ch["s"]]],
unq[dec["\[CapitalSigma]*-b"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,2ch["d"]+ch["s"]]],
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalXi]*0"],coeIn[vtxtp,ch["u"]+2ch["s"]]],
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalXi]*-"],coeIn[vtxtp,ch["d"]+2ch["s"]]],
unq[dec["\[CapitalOmega]-b"],dec["\[CapitalOmega]-"],coeIn[vtxtp,3ch["s"]]]
};


(* ::Section::Closed:: *)
(*trans magnetic,order1,BTA*)


(*(i e)/Subscript[M, N].\[ScriptCapitalF]^\[Mu]\[Nu].(Overscript[p, _].\[Gamma]^\[Mu].\[Gamma]^5.\[CapitalDelta]^+\[Nu]-Overscript[\[CapitalDelta]^+\[Nu], _].\[Gamma]^\[Mu].\[Gamma]^5.p)\[Rule]Subscript[c, 4].(Subscript[Q, u]-Subscript[Q, d]),Subscript[c, 4]=Subscript[c, 1]/Sqrt[3]*)
(*++++++++++++++++++++++++++ \:53cd\:5e38\:78c1\:77e9,\:8f6c\:79fb\:78c1\:77e9 +++++++++++++++++++++*)
vtxtp=vtxType["F2","tran","octOut"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
coeIn[type_,x_]:=type->vtxCoe[cc["c4"]/MassOct1 x](*\:7cfb\:6570\:7684\:7c7b\:578b,\:4ee5\:53ca\:5e38\:6570\:56e0\:5b50*)
(* ----------------  ---------------- *)
vtx[unq["type"->vtxtp]]={
unq[oct["pb"],dec["\[CapitalDelta]+"],coeIn[vtxtp,I(ch["u"]-ch["d"])]],
unq[oct["nb"],dec["\[CapitalDelta]0"],coeIn[vtxtp,I(ch["u"]-ch["d"])]],
unq[oct["\[CapitalSigma]+b"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,-I(ch["u"]-ch["s"])]],
unq[oct["\[CapitalSigma]0b"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,(I(ch["u"]+ch["d"]-2ch["s"]))/2]],
unq[oct["\[CapitalSigma]-b"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,I(ch["d"]-ch["s"])]],
unq[oct["\[CapitalXi]0b"],dec["\[CapitalXi]*0"],coeIn[vtxtp,-I(ch["u"]-ch["s"])]],
unq[oct["\[CapitalXi]-b"],dec["\[CapitalXi]*-"],coeIn[vtxtp,I(ch["d"]-ch["s"])]],
unq[oct["\[CapitalLambda]b"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,(-Sqrt[3]I(ch["u"]-ch["d"]))/2]]
};


vtxtp=vtxType["F2","tran","octOut"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
coeIn[type_,x_]:=type->vtxCoe[cc["c4"]/MassOct1 x](*\:7cfb\:6570\:7684\:7c7b\:578b,\:4ee5\:53ca\:5e38\:6570\:56e0\:5b50*)
vtxtpFlip=vtxType["F2","tran","octIn"];(* \:53e6\:4e00\:534a\:9876\:70b9 *)
(* ------------- \:6dfb\:52a0\:53e6\:5916\:534a\:90e8\:5206\:9876\:70b9,\:6539\:53d8\:5404\:4e2a\:7c92\:5b50\:7684\:65b9\:5411 -------------*)
vtx[unq["type"->vtxtpFlip]]=Query[All,<|
fdTypeOct->(#@fdTypeOctb/.fd[a_,b_,1]:>fd[a,b,0]),
fdTypeDecb->(#@fdTypeDec/.fd[a_,b_,0]:>fd[a,b,1]),
vtxtpFlip->(#@vtxtp/.vtxCoe->vtxCoeAnti)
|>&
]@vtx[unq["type"->vtxtp]];


(* ::Chapter:: *)
(*anomalous magnetic, to nonlocal*)


(* ::Section:: *)
(*octet,order 1*)


(*++++++++++++++++++++++++++ \:53cd\:5e38\:78c1\:77e9,\:975e\:5b9a\:57df\:5316 +++++++++++++++++++++*)
vtxtp=vtxType["F1F2","oct","nloc"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
vtxType1=vtxType["F1","oct"];vtx1=vtx[unq["type"->vtxType1]];
vtxType2=vtxType["F2","oct"];vtx2=vtx[unq["type"->vtxType2]];
vtxF1=vtxType["F1","oct","nloc"];vtxF2=vtxType["F2","oct","nloc"];
vtxGE=vtxType["GE","oct","nloc"];vtxGM=vtxType["GM","oct","nloc"];
massKey;\[CapitalLambda];Q2;(*\:8fd0\:52a8\:5b66\:91cf*)
(* --------------------------------- \:6dfb\:52a0\:5404\:79cd tag --------------------------------- *)
vtx[unq["type"->vtxtp]]=(Query[All,
(*+++++++++++++++++++++++++ \:52a0\:4e0a\:7c92\:5b50\:8d28\:91cftag +++++++++++++++++++++++++*)
(*\:590d\:5408\:7b97\:7b26:/*, \:88ab\:8fde\:63a5\:7684\:51fd\:6570\:4ece\:5de6\:5230\:53f3\:4f9d\:6b21\:4f5c\:7528\:5230\:5173\:8054\:4e0a *)
(Append[#,massKey->
(#@fdTypeOct/.fd[a_,b_,0]:>massV@fd[a,b,2])*
(#@fdTypeOctb/.fd[c_,d_,1]:>massV@fd[c,d,2])
]&)/*
(*------------------------- \:7ed9\:51faGE,GM\:7684\:975e\:5b9a\:57df\:5316\:5f62\:5f0f, \:6dfb\:52a0 GE GMtag -------------------------*)
(Append[#,{
vtxGE->#@vtxType1*\[CapitalLambda]^4/(Q2+\[CapitalLambda]^2)^2,
vtxGM->\[CapitalLambda]^4/(Q2+\[CapitalLambda]^2)^2 (#@vtxType1 + #@vtxType2)
}]&)/*
(*------------------------- \:7528\:65b9\:7a0b\:53cd\:89e3\:51fa\:975e\:5b9a\:57df F1(Q2),F2(Q2), \:6dfb\:52a0 F1,F2 tag -------------------------*)
(Append[#,First@Solve[{(* Solve \:8fd4\:56de Rules *)
(*\:8fd9\:91cc\:76f4\:63a5\:628a massKey \:8f6c\:6362\:6210\:4e86\:5e95\:5c42\:7684\:8d28\:91cf\:5f62\:5f0f,\:4f8b\:5982 massV@fd[2,1,2]*)
vtxF1-Q2/(4*#@massKey)*vtxF2==#@vtxGE, 
vtxF1+vtxF2==#@vtxGM
},{vtxF1,vtxF2}]]&)/*
(*------------------------- \:5220\:6389\:5197\:4f59\:7684\:5b57\:6bb5 -------------------------*)
KeyDrop[{vtxType1,vtxType2,massKey}]
]@
(*+++++++++++++++++++++++++ \:5c06\:53cd\:5e38\:78c1\:77e9\:7684F1,F2\:8fde\:63a5\:8d77\:6765 +++++++++++++++++++++++++*)
JoinAcross[vtx1,vtx2,{Key@fdTypeOctb,Key@fdTypeOct}]);


(* ::Section:: *)
(*octet,order 2*)


(*++++++++++++++++++++++++++ \:53cd\:5e38\:78c1\:77e9,\:975e\:5b9a\:57df\:5316 +++++++++++++++++++++*)
(*
\:5c06\:51fa\:73b0\:7684\:4ecb\:5b50\:5bf9\:79f0\:6027\:5316,\:4f8b\:5982:
\[LeftAssociation]{"octb"}\[Rule]Overscript["p", _],{"oct"}\[Rule]"p",{"mes"}\[Rule]"\[Pi]+",{"mes","out"}\[Rule]"\[Pi]+",{"F1","oct","o2"}\[Rule]{("d")/2}\[RightAssociation],
\[LeftAssociation]{"octb"}\[Rule]Overscript["p", _],{"oct"}\[Rule]"p",{"mes"}\[Rule]"\[Pi]-",{"mes","out"}\[Rule]"\[Pi]-",{"F1","oct","o2"}\[Rule]{("d")/2}\[RightAssociation]
*)
mesSym[vtx_]:=Union[vtx,vtx/.mesAntiRule];
(*++++++++++++++++++++++++++ \:8f93\:5165\:53c2\:91cf +++++++++++++++++++++*)
vtxtp=vtxType["F1F2","oct","o2","nloc"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
vtxType1=vtxType["F1","oct","o2"];vtx1=vtx[unq["type"->vtxType1]];
vtxType2=vtxType["F2","oct","o2"];vtx2=vtx[unq["type"->vtxType2]];
vtxF1=vtxType["F1","oct","o2","nloc"];vtxF2=vtxType["F2","oct","o2","nloc"];
vtxGE=vtxType["GE","oct","o2","nloc"];vtxGM=vtxType["GM","oct","o2","nloc"];
massKey;\[CapitalLambda];Q2;(*\:8fd0\:52a8\:5b66\:91cf*)
(* --------------------------------- \:6dfb\:52a0\:5404\:79cd tag --------------------------------- *)
vtx[unq["type"->vtxtp]]=(Query[All,
(*+++++++++++++++++++++++++ \:52a0\:4e0a\:7c92\:5b50\:8d28\:91cftag +++++++++++++++++++++++++*)
(Append[#,massKey->
(#@fdTypeOct/.fd[a_,b_,0]:>massV@fd[a,b,2])*
(#@fdTypeOctb/.fd[c_,d_,1]:>massV@fd[c,d,2])
]&)/*
(*\:7ed9\:51faGE,GM\:7684\:975e\:5b9a\:57df\:5316\:5f62\:5f0f,\:7528\:65b9\:7a0b\:53cd\:89e3\:51fa F1(Q2),F2(Q2) *)
(Append[#,{
vtxGE->#@vtxType1*\[CapitalLambda]^4/(Q2+\[CapitalLambda]^2)^2,
vtxGM->\[CapitalLambda]^4/(Q2+\[CapitalLambda]^2)^2 (#@vtxType1 + #@vtxType2)
}]&)/*
(*------------------------- \:7528\:65b9\:7a0b\:53cd\:89e3\:51fa\:975e\:5b9a\:57df F1(Q2),F2(Q2), \:6dfb\:52a0 F1,F2 tag -------------------------*)
(Append[#,First@Solve[{(* Solve \:8fd4\:56de Rules *)
(*\:8fd9\:91cc\:76f4\:63a5\:628a massKey \:8f6c\:6362\:6210\:4e86\:5e95\:5c42\:7684\:8d28\:91cf\:5f62\:5f0f,\:4f8b\:5982 fd[2,1,2]*)
vtxF1-Q2/(4*#@massKey)*vtxF2==#@vtxGE, 
vtxF1+vtxF2==#@vtxGM}
,{vtxF1,vtxF2}]]&)/*
(*------------------------- \:5220\:6389\:5197\:4f59\:7684\:952e -------------------------*)
KeyDrop[{vtxType1,vtxType2,massKey}]
]@
(*+++++++++++++++++++++++++ \:5c06\:53cd\:5e38\:78c1\:77e9\:7684F1,F2\:8fde\:63a5\:8d77\:6765 +++++++++++++++++++++++++*)
JoinAcross[vtx1,vtx2,{Key@fdTypeOctb,Key@fdTypeOct,Key@fdTypeMes,Key@fdTypeMesOut}]);


(* ::Section:: *)
(*decuplet, order 1*)


(*++++++++++++++++++++++++++ \:53cd\:5e38\:78c1\:77e9,\:975e\:5b9a\:57df\:5316 +++++++++++++++++++++*)
vtxtp=vtxType["F1F2","dec","nloc"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
vtxType1=vtxType["F1","dec"];vtx1=vtx[unq["type"->vtxType1]];
vtxType2=vtxType["F2","dec"];vtx2=vtx[unq["type"->vtxType2]];
vtxF1=vtxType["F1","dec","nloc"];vtxF2=vtxType["F2","dec","nloc"];
vtxGE=vtxType["GE","dec","nloc"];vtxGM=vtxType["GM","dec","nloc"];
massKey;\[CapitalLambda];Q2;(*\:8fd0\:52a8\:5b66\:91cf*)
(* --------------------------------- \:6dfb\:52a0\:5404\:79cd tag --------------------------------- *)
vtx[unq["type"->vtxtp]]=(Query[All,
(*+++++++++++++++++++++++++ \:52a0\:4e0a\:7c92\:5b50\:8d28\:91cftag +++++++++++++++++++++++++*)
(Append[#,massKey->
(#@fdTypeDec/.fd[a_,b_,0]:>massV@fd[a,b,2])*
(#@fdTypeDecb/.fd[c_,d_,1]:>massV@fd[c,d,2])
]&)/*
(*------------------------- \:7ed9\:51faGE,GM\:7684\:975e\:5b9a\:57df\:5316\:5f62\:5f0f, \:6dfb\:52a0 GE GMtag -------------------------*)
(Append[#,{
vtxGE->#@vtxType1*\[CapitalLambda]^4/(Q2+\[CapitalLambda]^2)^2,
vtxGM->\[CapitalLambda]^4/(Q2+\[CapitalLambda]^2)^2 (#@vtxType1 + #@vtxType2)
}]&)/*
(*------------------------- \:7528\:65b9\:7a0b\:53cd\:89e3\:51fa\:975e\:5b9a\:57df F1(Q2),F2(Q2), \:6dfb\:52a0 F1,F2 tag -------------------------*)
(Append[#,First@Solve[{(* Solve \:8fd4\:56de Rules *)
(*\:8fd9\:91cc\:76f4\:63a5\:628a massKey \:8f6c\:6362\:6210\:4e86\:5e95\:5c42\:7684\:8d28\:91cf\:5f62\:5f0f,\:4f8b\:5982 fd[2,1,2]*)
vtxF1-Q2/(4*#@massKey)*vtxF2==#@vtxGE, 
vtxF1+vtxF2==#@vtxGM}
,{vtxF1,vtxF2}]]&)/*
(*------------------------- \:5220\:6389\:5197\:4f59\:7684\:5b57\:6bb5 -------------------------*)
KeyDrop[{vtxType1,vtxType2,massKey}]
]@
(*\:5c06\:53cd\:5e38\:78c1\:77e9\:7684F1,F2\:8fde\:63a5\:8d77\:6765*)
JoinAcross[vtx1,vtx2,{Key@fdTypeDecb,Key@fdTypeDec}]);


(* ::Chapter:: *)
(*saveas*)


(* ::Input:: *)
(*If[FileExtension@NotebookFileName[]==="nb",*)
(*FrontEndExecute[FrontEndToken[FrontEnd`EvaluationNotebook[], "Save", {StringTrim[NotebookFileName[],".nb"~~EndOfString]<>".wl", "Package"}]]]*)
