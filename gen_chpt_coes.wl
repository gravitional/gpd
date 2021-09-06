(* ::Package:: *)

(* ::Title:: *)
(*gen_chpt_coes.nb*)


(* ::Text:: *)
(*\:8bb0\:5f55\:62c9\:6c0f\:91cf\:5c55\:5f00\:540e\:4ea7\:751f\:7684\:8026\:5408\:7cfb\:6570\:3002*)
(*K0b\:8868\:793a K0 bar, pb \:8868\:793a p bar, \:8d28\:5b50\:7684\:53cd\:7c92\:5b50\:573a\:3002*)


(* ::Chapter:: *)
(*initial*)


(*\:672c\:6587\:4ef6\:7684\:540d\:79f0*)
fileName=If[$Notebooks,NotebookFileName[],$InputFileName];
(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c\:ff0c\:5c31\:5237\:65b0\:7b14\:8bb0\:672c\:7684\:6807\:9898*)
Once@If[!$Notebooks,NotebookWrite[Cells[][[1]],Cell[Last@FileNameSplit[fileName],"Title"]]];
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


Get[FileNameJoin[{gitLocalName,"gen_format.wl"}]];(*\:5bfc\:5165\:4e00\:4e9b\:683c\:5f0f\:5316\:7684\:8bbe\:7f6e\:ff0c\:663e\:793a\:573a\:7684\:5e38\:7528\:5f62\:5f0f*)


(* ::Chapter:: *)
(*\:5f3a\:76f8\:4e92\:4f5c\:7528*)


unq::usage="\:5c06\:8f93\:5165\:53c2\:6570\:7ec4\:6210\:5217\:8868\:ff0c\:6216\:8005\:5173\:8054,\:9884\:7559\:7684\:51fd\:6570\:63a5\:53e3";
unq[fiels__]:=Association[fiels]
(* ------------- *)
coeIn::usage="coeIn[type,coes],\:7528\:4e8e\:8f93\:5165\:9876\:70b9\:7cfb\:6570";
coeIn[type_,x_]:=type->vtxCoe[x]
mesout::usage="\:533a\:522b\:4ecb\:5b50\:ff0cout\:8868\:793a\:51fa\:5c04";
mesout[x_]:=mes[x,"out"]


(* ::Section:: *)
(*Strong,BB\[Phi]\[Phi]*)


(* Overscript[p, _].\[Gamma]^\[Mu].p.(-\[Pi]^-.0+\[Pi]^+.0)\[Rule]\[ImaginaryI]/(4 Subsuperscript[f, \[Phi], 2]) *)
(*++++++++++++++++++++++++++\:5f3a\:76f8\:4e92\:4f5c\:7528\:7684 BB\[Phi]\[Phi] \:9876\:70b9 +++++++++++++++++++++*)
vtxtp=vtxType["stro","BB\[Phi]\[Phi]"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
vtx[unq["type"->vtxtp]]={
(*+++ proton +++*)
unq[oct["p"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,I/4]],
unq[oct["p"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,-I/4]],
unq[oct["p"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["p"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,I/(4Sqrt[3])]],
unq[oct["p"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,I/4]],
unq[oct["p"],mes["K+"],mesout["K-"],coeIn[vtxtp,I/2]],
unq[oct["p"],mes["K0"],mesout["K0b"],coeIn[vtxtp,-I/4]],
(*+++ neutron +++*)
unq[oct["n"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,I/4]],
unq[oct["n"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,-I/4]],
unq[oct["n"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["n"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,-I/(4Sqrt[3])]],
unq[oct["n"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,-I/4]],
unq[oct["n"],mes["K+"],mesout["K-"],coeIn[vtxtp,I/4]],
unq[oct["n"],mes["K0"],mesout["K0b"],coeIn[vtxtp,-I/2]],
(*+++ Sigma+ +++*)
unq[oct["\[CapitalSigma]+"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,0]],
unq[oct["\[CapitalSigma]+"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,0]],
unq[oct["\[CapitalSigma]+"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]+"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,I/(2Sqrt[3])]],
unq[oct["\[CapitalSigma]+"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,I/2]],
unq[oct["\[CapitalSigma]+"],mes["K+"],mesout["K-"],coeIn[vtxtp,I/4]],
unq[oct["\[CapitalSigma]+"],mes["K0"],mesout["K0b"],coeIn[vtxtp,I/4]],
(*+++ Sigma0 +++*)
unq[oct["\[CapitalSigma]0"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,0]],
unq[oct["\[CapitalSigma]0"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,0]],
unq[oct["\[CapitalSigma]0"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]0"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,0]],
unq[oct["\[CapitalSigma]0"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,0]],
unq[oct["\[CapitalSigma]0"],mes["K+"],mesout["K-"],coeIn[vtxtp,0]],
unq[oct["\[CapitalSigma]0"],mes["K0"],mesout["K0b"],coeIn[vtxtp,0]],
(*+++ Sigma- +++*)
unq[oct["\[CapitalSigma]-"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,0]],
unq[oct["\[CapitalSigma]-"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,0]],
unq[oct["\[CapitalSigma]-"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]-"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,-I/(2Sqrt[3])]],
unq[oct["\[CapitalSigma]-"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,-I/2]],
unq[oct["\[CapitalSigma]-"],mes["K+"],mesout["K-"],coeIn[vtxtp,-I/4]],
unq[oct["\[CapitalSigma]-"],mes["K0"],mesout["K0b"],coeIn[vtxtp,-I/4]],
(*+++ Xi0 +++*)
unq[oct["\[CapitalXi]0"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,-I/4]],
unq[oct["\[CapitalXi]0"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,I/4]],
unq[oct["\[CapitalXi]0"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalXi]0"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,I/(4Sqrt[3])]],
unq[oct["\[CapitalXi]0"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,I/4]],
unq[oct["\[CapitalXi]0"],mes["K+"],mesout["K-"],coeIn[vtxtp,-I/4]],
unq[oct["\[CapitalXi]0"],mes["K0"],mesout["K0b"],coeIn[vtxtp,I/2]],
(*+++ Xi- +++*)
unq[oct["\[CapitalXi]-"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,-I/4]],
unq[oct["\[CapitalXi]-"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,I/4]],
unq[oct["\[CapitalXi]-"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalXi]-"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,-I/(4Sqrt[3])]],
unq[oct["\[CapitalXi]-"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,-I/4]],
unq[oct["\[CapitalXi]-"],mes["K+"],mesout["K-"],coeIn[vtxtp,-I/2]],
unq[oct["\[CapitalXi]-"],mes["K0"],mesout["K0b"],coeIn[vtxtp,I/2]],
(*+++ Lambda +++*)
unq[oct["\[CapitalLambda]"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,0]],
unq[oct["\[CapitalLambda]"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,0]],
unq[oct["\[CapitalLambda]"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalLambda]"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,0]],
unq[oct["\[CapitalLambda]"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,0]],
unq[oct["\[CapitalLambda]"],mes["K+"],mesout["K-"],coeIn[vtxtp,0]],
unq[oct["\[CapitalLambda]"],mes["K0"],mesout["K0b"],coeIn[vtxtp,0]]
};


(* ::Section:: *)
(*Strong,BB\[Phi],DF*)


(*Overscript[1, _].\[Gamma]^\[Mu].\[Gamma]^5.p.0+Overscript[p, _].\[Gamma]^\[Mu].\[Gamma]^5.1.0\[Rule](D-F)/(2 Subscript[f, \[Phi]])*)
(*++++++++++++++++++++++++++\:5f3a\:76f8\:4e92\:4f5c\:7528\:7684 BBM, \:8f74\:77e2\:9879 DF+++++++++++++++++++++*)
vtxtp=vtxType["stro","DF"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
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


(* ::Section:: *)
(*Strong, BMT, C*)


(*Overscript[p, _].\[CapitalTheta]^\[Mu]\[Nu].Subsuperscript[\[CapitalDelta], \[Nu], ++].\!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]
\*SuperscriptBox[\(\[Pi]\), \(-\)]\)-Overscript[Subsuperscript[\[CapitalDelta], \[Nu], ++], _].\[CapitalTheta]^\[Mu]\[Nu].p.\!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]
\*SuperscriptBox[\(\[Pi]\), \(-\)]\)\[Rule]\[ScriptCapitalC]/(Sqrt[2] Subscript[f, \[Phi]])*)
(*++++++++++++++++++++++++++\:5f3a\:76f8\:4e92\:4f5c\:7528\:7684 BMT \:8f74\:77e2\:9879 C+++++++++++++++++++++*)
vtxtp=vtxType["stro","C"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
vtx[unq["type"->vtxtp]]={
(*+++ proton +++*)
unq[oct["pb"],mes["\[Pi]0"],dec["\[CapitalDelta]+"],coeIn[vtxtp,-1/Sqrt[3]]],
unq[oct["pb"],mes["\[Pi]-"],dec["\[CapitalDelta]++"],coeIn[vtxtp,1/Sqrt[2]]],
unq[oct["pb"],mes["K+"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,-1/(2Sqrt[3])]],
unq[oct["pb"],mes["\[Pi]+"],dec["\[CapitalDelta]0"],coeIn[vtxtp,-1/Sqrt[6]]],
unq[oct["pb"],mes["K0"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,1/Sqrt[6]]],
(*+++ neurton +++*)
unq[oct["nb"],mes["\[Pi]0"],dec["\[CapitalDelta]0"],coeIn[vtxtp,-1/Sqrt[3]]],
unq[oct["nb"],mes["\[Pi]+"],dec["\[CapitalDelta]-"],coeIn[vtxtp,-1/Sqrt[2]]],
unq[oct["nb"],mes["K0"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,1/(2Sqrt[3])]],
unq[oct["nb"],mes["\[Pi]-"],dec["\[CapitalDelta]+"],coeIn[vtxtp,1/Sqrt[6]]],
unq[oct["nb"],mes["K+"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,-1/Sqrt[6]]],
(*+++ \[CapitalSigma]+ +++*)
unq[oct["\[CapitalSigma]+b"],mes["\[Pi]0"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,1/(2Sqrt[3])]],
unq[oct["\[CapitalSigma]+b"],mes["\[Eta]8"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,1/2]],
unq[oct["\[CapitalSigma]+b"],mes["\[Eta]0"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]+b"],mes["K-"],dec["\[CapitalDelta]++"],coeIn[vtxtp,-1/Sqrt[2]]],
unq[oct["\[CapitalSigma]+b"],mes["\[Pi]+"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,1/(2Sqrt[3])]],
unq[oct["\[CapitalSigma]+b"],mes["K+"],dec["\[CapitalXi]*0"],coeIn[vtxtp,1/Sqrt[6]]],
unq[oct["\[CapitalSigma]+b"],mes["K0b"],dec["\[CapitalDelta]+"],coeIn[vtxtp,-1/Sqrt[6]]],
(*+++ \[CapitalSigma]0 +++*)
unq[oct["\[CapitalSigma]0b"],mes["\[Eta]8"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,-1/2]],
unq[oct["\[CapitalSigma]0b"],mes["\[Eta]0"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]0b"],mes["\[Pi]+"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,-1/(2Sqrt[3])]],
unq[oct["\[CapitalSigma]0b"],mes["\[Pi]-"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,-1/(2Sqrt[3])]],
unq[oct["\[CapitalSigma]0b"],mes["K+"],dec["\[CapitalXi]*-"],coeIn[vtxtp,-1/(2Sqrt[3])]],
unq[oct["\[CapitalSigma]0b"],mes["K-"],dec["\[CapitalDelta]+"],coeIn[vtxtp,1/Sqrt[3]]],
unq[oct["\[CapitalSigma]0b"],mes["K0"],dec["\[CapitalXi]*0"],coeIn[vtxtp,-1/(2Sqrt[3])]],
unq[oct["\[CapitalSigma]0b"],mes["K0b"],dec["\[CapitalDelta]0"],coeIn[vtxtp,1/Sqrt[3]]],
(*+++ \[CapitalSigma]- +++*)
unq[oct["\[CapitalSigma]-b"],mes["\[Eta]8"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,-1/2]],
unq[oct["\[CapitalSigma]-b"],mes["\[Eta]0"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]-b"],mes["K0b"],dec["\[CapitalDelta]-"],coeIn[vtxtp,1/Sqrt[2]]],
unq[oct["\[CapitalSigma]-b"],mes["\[Pi]0"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,1/(2Sqrt[3])]],
unq[oct["\[CapitalSigma]-b"],mes["\[Pi]-"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,-1/(2Sqrt[3])]],
unq[oct["\[CapitalSigma]-b"],mes["K-"],dec["\[CapitalDelta]0"],coeIn[vtxtp,1/Sqrt[6]]],
unq[oct["\[CapitalSigma]-b"],mes["K0"],dec["\[CapitalXi]*-"],coeIn[vtxtp,-1/Sqrt[6]]],
(*+++ \[CapitalXi]0 +++*)
unq[oct["\[CapitalXi]0b"],mes["\[Eta]8"],dec["\[CapitalXi]*0"],coeIn[vtxtp,1/2]],
unq[oct["\[CapitalXi]0b"],mes["\[Eta]0"],dec["\[CapitalXi]*0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalXi]0b"],mes["K+"],dec["\[CapitalOmega]-"],coeIn[vtxtp,1/Sqrt[2]]],
unq[oct["\[CapitalXi]0b"],mes["\[Pi]0"],dec["\[CapitalXi]*0"],coeIn[vtxtp,1/(2Sqrt[3])]],
unq[oct["\[CapitalXi]0b"],mes["K0b"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,-1/(2Sqrt[3])]],
unq[oct["\[CapitalXi]0b"],mes["\[Pi]+"],dec["\[CapitalXi]*-"],coeIn[vtxtp,1/Sqrt[6]]],
unq[oct["\[CapitalXi]0b"],mes["K-"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,-1/Sqrt[6]]],
(*+++ \[CapitalXi]- +++*)
unq[oct["\[CapitalXi]-b"],mes["\[Eta]8"],dec["\[CapitalXi]*-"],coeIn[vtxtp,-1/2]],
unq[oct["\[CapitalXi]-b"],mes["\[Eta]0"],dec["\[CapitalXi]*-"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalXi]-b"],mes["K0"],dec["\[CapitalOmega]-"],coeIn[vtxtp,-1/Sqrt[2]]],
unq[oct["\[CapitalXi]-b"],mes["\[Pi]0"],dec["\[CapitalXi]*-"],coeIn[vtxtp,1/(2Sqrt[3])]],
unq[oct["\[CapitalXi]-b"],mes["K-"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,1/(2Sqrt[3])]],
unq[oct["\[CapitalXi]-b"],mes["\[Pi]-"],dec["\[CapitalXi]*0"],coeIn[vtxtp,-1/Sqrt[6]]],
unq[oct["\[CapitalXi]-b"],mes["K0b"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,1/Sqrt[6]]],
(*+++ \[CapitalLambda] +++*)
unq[oct["\[CapitalLambda]b"],mes["\[Pi]+"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,1/2]],
unq[oct["\[CapitalLambda]b"],mes["\[Pi]0"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,1/2]],
unq[oct["\[CapitalLambda]b"],mes["\[Pi]-"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,-(1/2)]],
unq[oct["\[CapitalLambda]b"],mes["K+"],dec["\[CapitalXi]*-"],coeIn[vtxtp,1/2]],
unq[oct["\[CapitalLambda]b"],mes["K0"],dec["\[CapitalXi]*0"],coeIn[vtxtp,-(1/2)]]
};


(* ::Section:: *)
(*Strong, T-M, H*)


(*\!\(
\*SubsuperscriptBox[\(T\), \(\[Nu]\), \(ijl\)] . 
\*SubsuperscriptBox[\(u\), \(\[Alpha]\), \(kl\)] . 
\*SubsuperscriptBox[
OverscriptBox[\(T\), \(_\)], \(\[Mu]\), \(ijk\)] . 
\*SuperscriptBox[\(\[Gamma]\), \(\[Mu]\[Nu]\[Alpha]\)] . 
\*SuperscriptBox[\(\[Gamma]\), \(5\)]\)*)
(*++++++++++++++++++++++++++\:5f3a\:76f8\:4e92\:4f5c\:7528\:7684 BMT \:8f74\:77e2\:9879 C+++++++++++++++++++++*)
vtxtp=vtxType["stro","H"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
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


(* ::Section:: *)
(*Strong,BBMM,\:5f20\:91cf\:8026\:5408\:9879*)


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
vtxtp=vtxType["stro","bbb"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
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


(* ::Chapter:: *)
(*\:7269\:8d28\:573a\:7535\:78c1\:6d41*)


(* ::Section:: *)
(*\:7535\:78c1\:6d41,oder1,BBA,*)


(*Overscript[p, _].\[Gamma]^\[Mu].p.Subscript[\[ScriptCapitalA], \[Mu]]->2u+d*)
(*++++++++++++++++++++++++++ \:7535\:78c1\:5b88\:6052\:6d41 BB\[Phi]\[Phi]A +++++++++++++++++++++*)
vtxtp=vtxType["F1","oct"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
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


(* ::Section:: *)
(*ElectricMagnetic current,oder2,BB\[Phi]\[Phi]A*)


(*Overscript[p, _].\[Gamma]^\[Mu].p.\[Pi]^+.\[Pi]^-.Subscript[\[ScriptCapitalA], \[Mu]]\[Rule]1/(2Subsuperscript[f, \[Phi], 2])Subscript[Q, d]*)
(*++++++++++++++++++++++++++ \:7535\:78c1\:5b88\:6052\:6d41 BB\[Phi]\[Phi]A +++++++++++++++++++++*)
vtxtp=vtxType["F1","oct","o2"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
vtx[unq["type"->vtxtp]]={
(*+++ proton +++*)
unq[oct["pb"],oct["p"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,ch["u"]/4]],
unq[oct["pb"],oct["p"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["u"]-4ch["s"])/12]],
unq[oct["pb"],oct["p"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["pb"],oct["p"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,ch["u"]/(2Sqrt[3])]],
unq[oct["pb"],oct["p"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,ch["d"]/2]],
unq[oct["pb"],oct["p"],mes["K0"],mesout["K0b"],coeIn[vtxtp,-(ch["d"]/2)]],
unq[oct["pb"],oct["p"],mes["K+"],mesout["K-"],coeIn[vtxtp,(-ch["u"]+ch["s"])/2]],
(*+++ neurton +++*)
unq[oct["nb"],oct["n"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,ch["d"]/4]],
unq[oct["nb"],oct["n"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["d"]-4ch["s"])/12]],
unq[oct["nb"],oct["n"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["nb"],oct["n"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,-ch["d"]/(2Sqrt[3])]],
unq[oct["nb"],oct["n"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,ch["u"]/2]],
unq[oct["nb"],oct["n"],mes["K0"],mesout["K0b"],coeIn[vtxtp,(-ch["d"]+ch["s"])/2]],
unq[oct["nb"],oct["n"],mes["K+"],mesout["K-"],coeIn[vtxtp,-ch["u"]/2]],
(*+++ \[CapitalSigma]+ +++*)
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(ch["u"]-ch["d"])/4]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["u"]-ch["d"])/12]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["u"]+ch["d"])/(2Sqrt[3])]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,(-ch["u"]+ch["d"])/2]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["K0"],mesout["K0b"],coeIn[vtxtp,-ch["s"]/2]],
unq[oct["\[CapitalSigma]+b"],oct["\[CapitalSigma]+"],mes["K+"],mesout["K-"],coeIn[vtxtp,ch["s"]/2]],
(*+++ \[CapitalSigma]0 +++*)

(*+++ \[CapitalSigma]- +++*)
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(-ch["u"]+ch["d"])/4]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(-ch["u"]+ch["d"])/12]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(-ch["u"]-ch["d"])/(2Sqrt[3])]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,(ch["u"]-ch["d"])/2]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["K0"],mesout["K0b"],coeIn[vtxtp,ch["s"]/2]],
unq[oct["\[CapitalSigma]-b"],oct["\[CapitalSigma]-"],mes["K+"],mesout["K-"],coeIn[vtxtp,-ch["s"]/2]],
(*+++ \[CapitalXi]0 +++*)
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(-ch["u"]+ch["d"])/4]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(-ch["u"]+ch["d"])/12]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(-ch["u"]-ch["d"])/(2Sqrt[3])]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,(ch["u"]-ch["d"])/2]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["K0"],mesout["K0b"],coeIn[vtxtp,ch["s"]/2]],
unq[oct["\[CapitalXi]0b"],oct["\[CapitalXi]0"],mes["K+"],mesout["K-"],coeIn[vtxtp,-ch["s"]/2]],
(*+++ \[CapitalXi]- +++*)
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,-ch["u"]/4]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,-(ch["u"]-4ch["s"])/12]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,-ch["u"]/(2Sqrt[3])]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,-ch["d"]/2]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["K0"],mesout["K0b"],coeIn[vtxtp,ch["d"]/2]],
unq[oct["\[CapitalXi]-b"],oct["\[CapitalXi]-"],mes["K+"],mesout["K-"],coeIn[vtxtp,(ch["u"]-ch["s"])/2]]
(*+++ \[CapitalLambda] +++*)

};


(* ::Section:: *)
(*electric magnetic curent, BB\[Phi]A, DF*)


(* ::DisplayFormula:: *)
(*e . Subscript[\[ScriptCapitalA], \[Mu]] . (\!\(\*OverscriptBox[\(n\), \(_\)]\) . \[Gamma]^\[Mu] . \[Gamma]^5 . p . SuperMinus[\[Pi]]-\!\(\*OverscriptBox[\(p\), \(_\)]\) . \[Gamma]^\[Mu] . \[Gamma]^5 . n . SuperPlus[\[Pi]])->(i(D+F)(Subscript[Q, u]-Subscript[Q, d]))/(Sqrt[2] Subscript[f, \[Phi]])*)


(* e.Subscript[\[ScriptCapitalA], \[Mu]].(Overscript[n, _].\[Gamma]^\[Mu].\[Gamma]^5.p.\[Pi]^--Overscript[p, _].\[Gamma]^\[Mu].\[Gamma]^5.n.\[Pi]^+)\[Rule](i(D+F)(Subscript[Q, u]-Subscript[Q, d]))/(Sqrt[2] Subscript[f, \[Phi]]) *)
(*++++++++++++++++++++++++++\:7535\:78c1\:5b88\:6052\:6d41 BB\[Phi]A, \:8f74\:77e2\:9879 DF+++++++++++++++++++++*)
vtxtp=vtxType["F1","DF"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
vtx[unq["type"->vtxtp]]={
(*+++ proton +++*)
unq[oct["p"],oct["nb"],mes["\[Pi]-"],coeIn[vtxtp,(I(cc["D"]+cc["F"])(ch["u"]-ch["d"]))/Sqrt[2]]],
unq[oct["p"],oct["\[CapitalSigma]0b"],mes["K-"],coeIn[vtxtp,(I(cc["D"]-cc["F"])(ch["u"]-ch["s"]))/2]],
unq[oct["p"],oct["\[CapitalSigma]+b"],mes["K0b"],coeIn[vtxtp,(I(cc["D"]-cc["F"])(ch["d"]-ch["s"]))/Sqrt[2]]],
unq[oct["p"],oct["\[CapitalLambda]b"],mes["K-"],coeIn[vtxtp,(-I(cc["D"]+3cc["F"])(ch["u"]-ch["s"]))/(2Sqrt[3])]],
(*+++ neurton +++*)
unq[oct["n"],oct["pb"],mes["\[Pi]+"],coeIn[vtxtp,(-I(cc["D"]+cc["F"])(ch["u"]-ch["d"]))/Sqrt[2]]],
unq[oct["n"],oct["\[CapitalSigma]0b"],mes["K0b"],coeIn[vtxtp,(-I(cc["D"]-cc["F"])(ch["d"]-ch["s"]))/2]],
unq[oct["n"],oct["\[CapitalSigma]-b"],mes["K-"],coeIn[vtxtp,(I(cc["D"]-cc["F"])(ch["u"]-ch["s"]))/Sqrt[2]]],
unq[oct["n"],oct["\[CapitalLambda]b"],mes["K0b"],coeIn[vtxtp,(-I(cc["D"]+3cc["F"])(ch["d"]-ch["s"]))/(2Sqrt[3])]],
(*+++ \[CapitalSigma]+ +++*)
unq[oct["\[CapitalSigma]+"],oct["\[CapitalLambda]b"],mes["\[Pi]-"],coeIn[vtxtp,(I(cc["D"])(ch["u"]-ch["d"]))/Sqrt[3]]],
unq[oct["\[CapitalSigma]+"],oct["\[CapitalSigma]0b"],mes["\[Pi]-"],coeIn[vtxtp,(-I(cc["F"])(ch["u"]-ch["d"]))/1]],
unq[oct["\[CapitalSigma]+"],oct["\[CapitalXi]0b"],mes["K-"],coeIn[vtxtp,(I(cc["D"]+cc["F"])(ch["u"]-ch["s"]))/Sqrt[2]]],
unq[oct["\[CapitalSigma]+"],oct["pb"],mes["K0"],coeIn[vtxtp,(-I(cc["D"]-cc["F"])(ch["d"]-ch["s"]))/Sqrt[2]]],
(*+++ \[CapitalSigma]0 +++*)
unq[oct["\[CapitalSigma]0"],oct["\[CapitalSigma]+b"],mes["\[Pi]+"],coeIn[vtxtp,(I(cc["F"])(ch["u"]-ch["d"]))/1]],
unq[oct["\[CapitalSigma]0"],oct["\[CapitalSigma]-b"],mes["\[Pi]-"],coeIn[vtxtp,(I(cc["F"])(ch["u"]-ch["d"]))/1]],
unq[oct["\[CapitalSigma]0"],oct["\[CapitalXi]-b"],mes["K-"],coeIn[vtxtp,(I(cc["D"]+cc["F"])(ch["u"]-ch["s"]))/2]],
unq[oct["\[CapitalSigma]0"],oct["\[CapitalXi]0b"],mes["K0b"],coeIn[vtxtp,(-I(cc["D"]+cc["F"])(ch["d"]-ch["s"]))/2]],
unq[oct["\[CapitalSigma]0"],oct["pb"],mes["K+"],coeIn[vtxtp,(-I(cc["D"]-cc["F"])(ch["u"]-ch["s"]))/2]],
unq[oct["\[CapitalSigma]0"],oct["nb"],mes["K0"],coeIn[vtxtp,(I(cc["D"]-cc["F"])(ch["d"]-ch["s"]))/2]],
(*+++ \[CapitalSigma]- +++*)
unq[oct["\[CapitalSigma]-"],oct["\[CapitalLambda]b"],mes["\[Pi]+"],coeIn[vtxtp,(-I(cc["D"])(ch["u"]-ch["d"]))/Sqrt[3]]],
unq[oct["\[CapitalSigma]-"],oct["\[CapitalSigma]0b"],mes["\[Pi]+"],coeIn[vtxtp,(-I(cc["F"])(ch["u"]-ch["d"]))/1]],
unq[oct["\[CapitalSigma]-"],oct["\[CapitalXi]-b"],mes["K0b"],coeIn[vtxtp,(I(cc["D"]+cc["F"])(ch["d"]-ch["s"]))/Sqrt[2]]],
unq[oct["\[CapitalSigma]-"],oct["nb"],mes["K+"],coeIn[vtxtp,(-I(cc["D"]-cc["F"])(ch["u"]-ch["s"]))/Sqrt[2]]],
(*+++ \[CapitalXi]0 +++*)
unq[oct["\[CapitalXi]0"],oct["\[CapitalXi]-b"],mes["\[Pi]-"],coeIn[vtxtp,(I(cc["D"]-cc["F"])(ch["u"]-ch["d"]))/Sqrt[2]]],
unq[oct["\[CapitalXi]0"],oct["\[CapitalSigma]0b"],mes["K0"],coeIn[vtxtp,(I(cc["D"]+cc["F"])(ch["d"]-ch["s"]))/2]],
unq[oct["\[CapitalXi]0"],oct["\[CapitalXi]+b"],mes["K+"],coeIn[vtxtp,(-I(cc["D"]+cc["F"])(ch["u"]-ch["s"]))/Sqrt[2]]],
unq[oct["\[CapitalXi]0"],oct["\[CapitalLambda]b"],mes["K0"],coeIn[vtxtp,(I(cc["D"]-3cc["F"])(ch["d"]-ch["s"]))/(2Sqrt[3])]],
(*+++ \[CapitalXi]- +++*)
unq[oct["\[CapitalXi]-"],oct["\[CapitalXi]0b"],mes["\[Pi]+"],coeIn[vtxtp,(-I(cc["D"]-cc["F"])(ch["u"]-ch["d"]))/Sqrt[2]]],
unq[oct["\[CapitalXi]-"],oct["\[CapitalSigma]0b"],mes["K+"],coeIn[vtxtp,(-I(cc["D"]+cc["F"])(ch["u"]-ch["s"]))/2]],
unq[oct["\[CapitalXi]-"],oct["\[CapitalSigma]-b"],mes["K0"],coeIn[vtxtp,(-I(cc["D"]+cc["F"])(ch["d"]-ch["s"]))/Sqrt[2]]],
unq[oct["\[CapitalXi]-"],oct["\[CapitalLambda]b"],mes["K+"],coeIn[vtxtp,(I(cc["D"]-3cc["F"])(ch["u"]-ch["s"]))/(2Sqrt[3])]],
(*+++ \[CapitalLambda] +++*)
unq[oct["\[CapitalLambda]"],oct["\[CapitalSigma]+b"],mes["\[Pi]+"],coeIn[vtxtp,(-I(cc["D"])(ch["u"]-ch["d"]))/Sqrt[3]]],
unq[oct["\[CapitalLambda]"],oct["\[CapitalSigma]-b"],mes["\[Pi]-"],coeIn[vtxtp,(I(cc["D"])(ch["u"]-ch["d"]))/Sqrt[3]]],
unq[oct["\[CapitalLambda]"],oct["pb"],mes["K+"],coeIn[vtxtp,(I(cc["D"]+3cc["F"])(ch["u"]-ch["s"]))/(2Sqrt[3])]],
unq[oct["\[CapitalLambda]"],oct["nb"],mes["K0"],coeIn[vtxtp,(I(cc["D"]+3cc["F"])(ch["d"]-ch["s"]))/(2Sqrt[3])]],
unq[oct["\[CapitalLambda]"],oct["\[CapitalXi]-b"],mes["K-"],coeIn[vtxtp,(-I(cc["D"]-3cc["F"])(ch["u"]-ch["s"]))/(2Sqrt[3])]],
unq[oct["\[CapitalLambda]"],oct["\[CapitalXi]0b"],mes["K0b"],coeIn[vtxtp,(-I(cc["D"]-3cc["F"])(ch["d"]-ch["s"]))/(2Sqrt[3])]]
};


(* ::Section:: *)
(*electric magnetic current, TTA, TT\[Phi]\[Phi]A*)


(*e . Subscript[\[ScriptCapitalA], \[Mu]] . Overscript[Subsuperscript[\[CapitalDelta], \[Nu], ++], _] . \[Gamma]^\[Nu]\[Alpha]\[Mu] . Subsuperscript[\[CapitalDelta], \[Alpha], ++] . SuperPlus[\[Pi]] . SuperMinus[\[Pi]]->(3Subscript[Q, d])/(2Subsuperscript[f, \[Phi], 2])*)
(*++++++++++++++++++++++++++ \:7535\:78c1\:5b88\:6052\:6d41 TTA, TTMMA +++++++++++++++++++++*)
vtxtp=vtxType["F1","dec"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
vtx[unq["type"->vtxtp]]={
(*+++ \[CapitalDelta]++ +++*)
unq[dec["\[CapitalDelta]++b"],dec["\[CapitalDelta]++"],coeIn[vtxtp,3ch["u"]]],
unq[dec["\[CapitalDelta]++b"],dec["\[CapitalDelta]++"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(3ch["u"])/4]],
unq[dec["\[CapitalDelta]++b"],dec["\[CapitalDelta]++"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,ch["u"]/4]],
unq[dec["\[CapitalDelta]++b"],dec["\[CapitalDelta]++"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalDelta]++b"],dec["\[CapitalDelta]++"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(Sqrt[3]ch["u"])/2]],
unq[dec["\[CapitalDelta]++b"],dec["\[CapitalDelta]++"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,(3ch["d"])/2]],
unq[dec["\[CapitalDelta]++b"],dec["\[CapitalDelta]++"],mes["K+"],mesout["K-"],coeIn[vtxtp,(3ch["s"])/2]],
(*+++ \[CapitalDelta]+ +++*)
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalDelta]+"],coeIn[vtxtp,2ch["u"]+ch["d"]]],
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalDelta]+"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(2ch["u"]+ch["d"])/4]],
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalDelta]+"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(2ch["u"]+ch["d"])/12]],
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalDelta]+"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalDelta]+"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(2ch["u"]-ch["d"])/(2Sqrt[3])]],
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalDelta]+"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,(ch["u"]+2ch["d"])/2]],
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalDelta]+"],mes["K+"],mesout["K-"],coeIn[vtxtp,ch["s"]]],
unq[dec["\[CapitalDelta]+b"],dec["\[CapitalDelta]+"],mes["K0"],mesout["K0b"],coeIn[vtxtp,ch["s"]/2]],
(*+++ \[CapitalDelta]0 +++*)
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalDelta]0"],coeIn[vtxtp,ch["u"]+2ch["d"]]],
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalDelta]0"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(ch["u"]+2ch["d"])/4]],
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalDelta]0"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["u"]+2ch["d"])/12]],
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalDelta]0"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalDelta]0"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["u"]-2ch["d"])/(2Sqrt[3])]],
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalDelta]0"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,(2ch["u"]+ch["d"])/2]],
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalDelta]0"],mes["K+"],mesout["K-"],coeIn[vtxtp,ch["s"]/2]],
unq[dec["\[CapitalDelta]0b"],dec["\[CapitalDelta]0"],mes["K0"],mesout["K0b"],coeIn[vtxtp,ch["s"]]],
(*+++ \[CapitalDelta]- +++*)
unq[dec["\[CapitalDelta]-b"],dec["\[CapitalDelta]-"],coeIn[vtxtp,3ch["d"]]],
unq[dec["\[CapitalDelta]-b"],dec["\[CapitalDelta]-"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(3ch["d"])/4]],
unq[dec["\[CapitalDelta]-b"],dec["\[CapitalDelta]-"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,ch["d"]/4]],
unq[dec["\[CapitalDelta]-b"],dec["\[CapitalDelta]-"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalDelta]-b"],dec["\[CapitalDelta]-"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(-Sqrt[3]ch["d"])/2]],
unq[dec["\[CapitalDelta]-b"],dec["\[CapitalDelta]-"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,(3ch["u"])/2]],
unq[dec["\[CapitalDelta]-b"],dec["\[CapitalDelta]-"],mes["K0"],mesout["K0b"],coeIn[vtxtp,(3ch["s"])/2]],
(*+++ \[CapitalSigma]*+ +++*)
unq[dec["\[CapitalSigma]*+b"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,2ch["u"]+ch["s"]]],
unq[dec["\[CapitalSigma]*+b"],dec["\[CapitalSigma]*+"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,ch["u"]/2]],
unq[dec["\[CapitalSigma]*+b"],dec["\[CapitalSigma]*+"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["u"]+2ch["s"])/6]],
unq[dec["\[CapitalSigma]*+b"],dec["\[CapitalSigma]*+"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalSigma]*+b"],dec["\[CapitalSigma]*+"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,ch["u"]/Sqrt[3]]],
unq[dec["\[CapitalSigma]*+b"],dec["\[CapitalSigma]*+"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,ch["d"]]],
unq[dec["\[CapitalSigma]*+b"],dec["\[CapitalSigma]*+"],mes["K+"],mesout["K-"],coeIn[vtxtp,(ch["u"]+2ch["s"])/2]],
unq[dec["\[CapitalSigma]*+b"],dec["\[CapitalSigma]*+"],mes["K0"],mesout["K0b"],coeIn[vtxtp,ch["d"]/2]],
(*+++ \[CapitalSigma]*0 +++*)
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,ch["u"]+ch["d"]+ch["s"]]],
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalSigma]*0"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,(ch["u"]+ch["d"])/4]],
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalSigma]*0"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["u"]+ch["d"]+4ch["s"])/12]],
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalSigma]*0"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalSigma]*0"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["u"]-ch["d"])/(2Sqrt[3])]],
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalSigma]*0"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,(ch["u"]+ch["d"])/2]],
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalSigma]*0"],mes["K+"],mesout["K-"],coeIn[vtxtp,(ch["u"]+2ch["s"])/2]],
unq[dec["\[CapitalSigma]*0b"],dec["\[CapitalSigma]*0"],mes["K0"],mesout["K0b"],coeIn[vtxtp,(ch["d"]+2ch["s"])/2]],
(*+++ \[CapitalSigma]*- +++*)
unq[dec["\[CapitalSigma]*-b"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,2ch["d"]+ch["s"]]],
unq[dec["\[CapitalSigma]*-b"],dec["\[CapitalSigma]*-"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,ch["d"]/2]],
unq[dec["\[CapitalSigma]*-b"],dec["\[CapitalSigma]*-"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["d"]+2ch["s"])/6]],
unq[dec["\[CapitalSigma]*-b"],dec["\[CapitalSigma]*-"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalSigma]*-b"],dec["\[CapitalSigma]*-"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,-ch["d"]/Sqrt[3]]],
unq[dec["\[CapitalSigma]*-b"],dec["\[CapitalSigma]*-"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,ch["u"]]],
unq[dec["\[CapitalSigma]*-b"],dec["\[CapitalSigma]*-"],mes["K+"],mesout["K-"],coeIn[vtxtp,ch["u"]/2]],
unq[dec["\[CapitalSigma]*-b"],dec["\[CapitalSigma]*-"],mes["K0"],mesout["K0b"],coeIn[vtxtp,(ch["d"]+2ch["s"])/2]],
(*+++ \[CapitalXi]*0 +++*)
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalXi]*0"],coeIn[vtxtp,ch["u"]+2ch["s"]]],
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalXi]*0"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,ch["u"]/4]],
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalXi]*0"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["u"]+8ch["s"])/12]],
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalXi]*0"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalXi]*0"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,ch["u"]/(2Sqrt[3])]],
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalXi]*0"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,ch["d"]/2]],
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalXi]*0"],mes["K+"],mesout["K-"],coeIn[vtxtp,(2ch["u"]+ch["s"])/2]],
unq[dec["\[CapitalXi]*0b"],dec["\[CapitalXi]*0"],mes["K0"],mesout["K0b"],coeIn[vtxtp,ch["d"]]],
(*+++ \[CapitalXi]*- +++*)
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalXi]*-"],coeIn[vtxtp,ch["d"]+2ch["s"]]],
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalXi]*-"],mes["\[Pi]0"],mesout["\[Pi]0"],coeIn[vtxtp,ch["d"]/4]],
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalXi]*-"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,(ch["d"]+8ch["s"])/12]],
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalXi]*-"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalXi]*-"],mes["\[Pi]0"],mesout["\[Eta]8"],coeIn[vtxtp,-ch["d"]/(2Sqrt[3])]],
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalXi]*-"],mes["\[Pi]+"],mesout["\[Pi]-"],coeIn[vtxtp,ch["u"]/2]],
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalXi]*-"],mes["K+"],mesout["K-"],coeIn[vtxtp,ch["u"]]],
unq[dec["\[CapitalXi]*-b"],dec["\[CapitalXi]*-"],mes["K0"],mesout["K0b"],coeIn[vtxtp,(2ch["d"]+ch["s"])/2]],
(*+++ \[CapitalOmega]- +++*)
unq[dec["\[CapitalOmega]-b"],dec["\[CapitalOmega]-"],coeIn[vtxtp,3ch["s"]]],
unq[dec["\[CapitalOmega]-b"],dec["\[CapitalOmega]-"],mes["\[Eta]8"],mesout["\[Eta]8"],coeIn[vtxtp,ch["s"]]],
unq[dec["\[CapitalOmega]-b"],dec["\[CapitalOmega]-"],mes["\[Eta]0"],mesout["\[Eta]0"],coeIn[vtxtp,0]],(*\:989d\:5916\:6dfb\:52a0\[Eta]0*)
unq[dec["\[CapitalOmega]-b"],dec["\[CapitalOmega]-"],mes["K+"],mesout["K-"],coeIn[vtxtp,(3ch["u"])/2]],
unq[dec["\[CapitalOmega]-b"],dec["\[CapitalOmega]-"],mes["K0"],mesout["K0b"],coeIn[vtxtp,(3ch["d"])/3]]
};


(* ::Section:: *)
(*electric magnetic current, BT\[Phi]A, C*)


(*e.Subscript[\[ScriptCapitalA], \[Mu]] . (Overscript[p, _] . \[CapitalTheta]^\[Mu]\[Nu] . Subsuperscript[\[CapitalDelta], \[Nu], ++] . SuperMinus[\[Pi]]+Overscript[Subsuperscript[\[CapitalDelta], \[Nu], ++], _] . \[CapitalTheta]^\[Mu]\[Nu] . p . SuperPlus[\[Pi]])->(i(Subscript[Q, u]-Subscript[Q, d]))/Sqrt[2] \[ScriptCapitalC]/Subscript[f, \[Phi]]*)
(*++++++++++++++++++++++++++ \:7535\:78c1\:5b88\:6052\:6d41 BT\[Phi]A, \:8f74\:77e2\:9879 C +++++++++++++++++++++*)
vtxtp=vtxType["F1","C"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
vtx[unq["type"->vtxtp]]={
(*+++ proton +++*)
unq[oct["pb"],mes["\[Pi]-"],dec["\[CapitalDelta]++"],coeIn[vtxtp,(I(ch["u"]-ch["d"]))/Sqrt[2]]],
unq[oct["pb"],mes["\[Pi]+"],dec["\[CapitalDelta]0"],coeIn[vtxtp,(I(ch["u"]-ch["d"]))/Sqrt[6]]],
unq[oct["pb"],mes["K+"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,(I(ch["u"]-ch["s"]))/(2Sqrt[3])]],
unq[oct["pb"],mes["K0"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,(-I(ch["d"]-ch["s"]))/Sqrt[6]]],
(*+++ neurton +++*)
unq[oct["nb"],mes["\[Pi]+"],dec["\[CapitalDelta]-"],coeIn[vtxtp,(I(ch["u"]-ch["d"]))/Sqrt[2]]],
unq[oct["nb"],mes["\[Pi]-"],dec["\[CapitalDelta]+"],coeIn[vtxtp,(I(ch["u"]-ch["d"]))/Sqrt[6]]],
unq[oct["nb"],mes["K0"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,(-I(ch["d"]-ch["s"]))/(2Sqrt[3])]],
unq[oct["nb"],mes["K+"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,(I(ch["u"]-ch["s"]))/Sqrt[6]]],
(*+++ \[CapitalSigma]+ +++*)
unq[oct["\[CapitalSigma]+b"],mes["\[Pi]+"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,(-I(ch["u"]-ch["d"]))/(2Sqrt[3])]],
unq[oct["\[CapitalSigma]+b"],mes["K-"],dec["\[CapitalDelta]++"],coeIn[vtxtp,(-I(ch["u"]-ch["s"]))/Sqrt[2]]],
unq[oct["\[CapitalSigma]+b"],mes["K+"],dec["\[CapitalXi]*0"],coeIn[vtxtp,(-I(ch["u"]-ch["s"]))/Sqrt[6]]],
unq[oct["\[CapitalSigma]+b"],mes["K0b"],dec["\[CapitalDelta]+"],coeIn[vtxtp,(-I(ch["d"]-ch["s"]))/Sqrt[6]]],
(*+++ \[CapitalSigma]0 +++*)
unq[oct["\[CapitalSigma]0b"],mes["\[Pi]-"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,(-I(ch["u"]-ch["d"]))/(2Sqrt[3])]],
unq[oct["\[CapitalSigma]0b"],mes["\[Pi]+"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,(I(ch["u"]-ch["d"]))/(2Sqrt[3])]],
unq[oct["\[CapitalSigma]0b"],mes["K-"],dec["\[CapitalDelta]+"],coeIn[vtxtp,(I(ch["u"]-ch["s"]))/Sqrt[3]]],
unq[oct["\[CapitalSigma]0b"],mes["K+"],dec["\[CapitalXi]*-"],coeIn[vtxtp,(I(ch["u"]-ch["s"]))/(2Sqrt[3])]],
unq[oct["\[CapitalSigma]0b"],mes["K0b"],dec["\[CapitalDelta]0"],coeIn[vtxtp,(I(ch["d"]-ch["s"]))/Sqrt[3]]],
unq[oct["\[CapitalSigma]0b"],mes["K0"],dec["\[CapitalXi]*0"],coeIn[vtxtp,(I(ch["d"]-ch["s"]))/(2Sqrt[3])]],
(*+++ \[CapitalSigma]- +++*)
unq[oct["\[CapitalSigma]-b"],mes["\[Pi]-"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,(-I(ch["u"]-ch["d"]))/(2Sqrt[3])]],
unq[oct["\[CapitalSigma]-b"],mes["K0b"],dec["\[CapitalDelta]-"],coeIn[vtxtp,(I(ch["d"]-ch["s"]))/Sqrt[2]]],
unq[oct["\[CapitalSigma]-b"],mes["K-"],dec["\[CapitalDelta]0"],coeIn[vtxtp,(I(ch["u"]-ch["s"]))/Sqrt[6]]],
unq[oct["\[CapitalSigma]-b"],mes["K0"],dec["\[CapitalXi]*-"],coeIn[vtxtp,(I(ch["d"]-ch["s"]))/Sqrt[6]]],
(*+++ \[CapitalXi]0 +++*)
unq[oct["\[CapitalXi]0b"],mes["\[Pi]+"],dec["\[CapitalXi]*-"],coeIn[vtxtp,(-I(ch["u"]-ch["d"]))/Sqrt[6]]],
unq[oct["\[CapitalXi]0b"],mes["K+"],dec["\[CapitalOmega]-"],coeIn[vtxtp,(-I(ch["u"]-ch["s"]))/Sqrt[2]]],
unq[oct["\[CapitalXi]0b"],mes["K0b"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,(-I(ch["d"]-ch["s"]))/(2Sqrt[3])]],
unq[oct["\[CapitalXi]0b"],mes["K-"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,(-I(ch["u"]-ch["s"]))/Sqrt[6]]],
(*+++ \[CapitalXi]- +++*)
unq[oct["\[CapitalXi]-b"],mes["\[Pi]-"],dec["\[CapitalXi]*0"],coeIn[vtxtp,(-I(ch["u"]-ch["d"]))/Sqrt[6]]],
unq[oct["\[CapitalXi]-b"],mes["K0"],dec["\[CapitalOmega]-"],coeIn[vtxtp,(I(ch["d"]-ch["s"]))/Sqrt[2]]],
unq[oct["\[CapitalXi]-b"],mes["K-"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,(I(ch["u"]-ch["s"]))/(2Sqrt[3])]],
unq[oct["\[CapitalXi]-b"],mes["K0b"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,(I(ch["d"]-ch["s"]))/Sqrt[6]]],
(*+++ \[CapitalLambda] +++*)
unq[oct["\[CapitalLambda]b"],mes["\[Pi]-"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,(-I(ch["u"]-ch["d"]))/2]],
unq[oct["\[CapitalLambda]b"],mes["\[Pi]+"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,(-I(ch["u"]-ch["d"]))/2]],
unq[oct["\[CapitalLambda]b"],mes["K+"],dec["\[CapitalXi]*-"],coeIn[vtxtp,(-I(ch["u"]-ch["s"]))/2]],
unq[oct["\[CapitalLambda]b"],mes["K0"],dec["\[CapitalXi]*0"],coeIn[vtxtp,(I(ch["d"]-ch["s"]))/2]]
};


(* ::Section:: *)
(*meson, \[Phi]\[Phi]A*)


(*Subscript[e\[ScriptCapitalA], \[Mu]](SuperMinus[\[Pi]] . \!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\(\(.\)
\*SuperscriptBox[\(\[Pi]\), \(+\)]\)\)-SuperPlus[\[Pi]] . \!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\(\(.\)
\*SuperscriptBox[\(\[Pi]\), \(-\)]\)\))->i(Subscript[Q, u]-Subscript[Q, d])*)
(*++++++++++++++++++++++++++ \:7535\:78c1\:5b88\:6052\:6d41\:ff0c\:4ecb\:5b50 +++++++++++++++++++++*)
vtxtp=vtxType["F1","\[Phi]\[Phi]A"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
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


(* ::Chapter:: *)
(*anomalous magnetic moment*)


(* ::Section:: *)
(*octet,order1,BBA*)


(*e/(4Subscript[M, N]) \[ScriptCapitalF]^\[Mu]\[Nu] . Overscript[p, _] . Subscript[\[Sigma], \[Mu]\[Nu]] . p->Subscript[c, 2](Subscript[Q, d]+2Subscript[Q, u])-Subscript[c, 1] Subscript[Q, d]*)
(*++++++++++++++++++++++++++ \:53cd\:5e38\:78c1\:77e9\:ff0c\:516b\:91cd\:6001\:91cd\:5b50 +++++++++++++++++++++*)
vtxtp=vtxType["F2","oct"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
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


(* ::Section:: *)
(*octet,order2,BB\[Phi]\[Phi]A*)


(*(e \[ScriptCapitalF]^\[Mu]\[Nu].Overscript[p, _].Subscript[\[Sigma], \[Mu]\[Nu]].p.\[Pi]^+.\[Pi]^-)/(8 Subsuperscript[f, \[Phi], 2] Subscript[M, N])\[Rule]-Subscript[c, 1] Subscript[Q, u]+Subscript[c, 2] (2 Subscript[Q, d]+Subscript[Q, u])*)
(*++++++++++++++++++++++++++ \:53cd\:5e38\:78c1\:77e9\:4e8c\:9636\:ff0c\:516b\:91cd\:6001\:91cd\:5b50 +++++++++++++++++++++*)
vtxtp=vtxType["F2","oct","o2"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
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


(* ::Section:: *)
(*decuplet*)


(*e/(4Subscript[M, T]) \[ScriptCapitalF]^\[Mu]\[Nu] . OverscriptBox[SubsuperscriptBox[\[CapitalDelta],\[Alpha],++],_] . Subscript[\[Sigma], \[Mu]\[Nu]] . \[CapitalDelta]^++\[Alpha]->Subscript[Q, u]3Subscript[c, T],Subscript[c, T]=1/2 (3Subscript[c, 2]+1)*)
(*++++++++++++++++++++++++++ \:53cd\:5e38\:78c1\:77e9\:ff0c\:5341\:91cd\:6001\:91cd\:5b50 +++++++++++++++++++++*)
vtxtp=vtxType["F2","dec"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
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


(* ::Section:: *)
(*trans magnetic*)


(*(i e)/Subscript[M, N].\[ScriptCapitalF]^\[Mu]\[Nu].(Overscript[p, _].\[Gamma]^\[Mu].\[Gamma]^5.\[CapitalDelta]^+\[Nu]-Overscript[\[CapitalDelta]^+\[Nu], _].\[Gamma]^\[Mu].\[Gamma]^5.p)\[Rule]Subscript[c, 4].(Subscript[Q, u]-Subscript[Q, d]),Subscript[c, 4]=Subscript[c, 1]/Sqrt[3]*)
(*++++++++++++++++++++++++++ \:53cd\:5e38\:78c1\:77e9\:ff0c\:8f6c\:79fb\:78c1\:77e9 +++++++++++++++++++++*)
vtxtp=vtxType["F2","tran"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
vtx[unq["type"->vtxtp]]={
unq[oct["pb"],dec["\[CapitalDelta]+"],coeIn[vtxtp,cc["c4"](ch["u"]-ch["d"])]],
unq[oct["nb"],dec["\[CapitalDelta]0"],coeIn[vtxtp,cc["c4"](ch["u"]-ch["d"])]],
unq[oct["\[CapitalSigma]+b"],dec["\[CapitalSigma]*+"],coeIn[vtxtp,-cc["c4"](ch["u"]-ch["s"])]],
unq[oct["\[CapitalSigma]0b"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,(cc["c4"](ch["u"]+ch["d"]-2ch["s"]))/2]],
unq[oct["\[CapitalSigma]-b"],dec["\[CapitalSigma]*-"],coeIn[vtxtp,cc["c4"](ch["d"]-ch["s"])]],
unq[oct["\[CapitalXi]0b"],dec["\[CapitalXi]*0"],coeIn[vtxtp,-cc["c4"](ch["u"]-ch["s"])]],
unq[oct["\[CapitalXi]-b"],dec["\[CapitalXi]*-"],coeIn[vtxtp,cc["c4"](ch["d"]-ch["s"])]],
unq[oct["\[CapitalLambda]b"],dec["\[CapitalSigma]*0"],coeIn[vtxtp,(-Sqrt[3]cc["c4"](ch["u"]-ch["d"]))/2]]
};


(* ::Chapter:: *)
(*\:53cd\:5e38\:78c1\:77e9\:975e\:5b9a\:57df\:5316*)


(* ::Section:: *)
(*order 1*)


(*++++++++++++++++++++++++++ \:53cd\:5e38\:78c1\:77e9\:ff0c\:975e\:5b9a\:57df\:5316 +++++++++++++++++++++*)
vtxtp=vtxType["F1F2","nloc"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
vtx[unq["type"->vtxtp]]=Query[All,
KeyDrop[{vtxType["F1","oct"],vtxType["F2","oct"]}]
]@Query[All,(*\:7ed9\:51faGE,GM\:7684\:975e\:5b9a\:57df\:5316\:5f62\:5f0f\:ff0c\:7528\:65b9\:7a0b\:53cd\:89e3\:51fa F1(Q2),F2(Q2) *)
Append[#,First@Solve[{vtxType["F1","oct","nloc"]-kin["Q"]^2/(4#[MassKey["vtx","oct"]]^2) vtxType["F2","oct","nloc"]==#[vtxType["F1","oct"]]*kin["\[CapitalLambda]0"]^4/(kin["Q"]^2+kin["\[CapitalLambda]0"]^2)^2,
vtxType["F1","oct","nloc"]+vtxType["F2","oct","nloc"]==kin["\[CapitalLambda]0"]^4/(kin["Q"]^2+kin["\[CapitalLambda]0"]^2)^2 (#[vtxType["F1","oct"]]+ #[vtxType["F2","oct"]])}
,{vtxType["F1","oct","nloc"],vtxType["F2","oct","nloc"]}]]&
]@Query[All,(*\:52a0\:4e0a\:7c92\:5b50\:8d28\:91cftag*)
Append[#,MassKey["vtx","oct"]->(#[fdType["oct"]]/.fd[a_,b_,0]:>fd[a,b,2])]&
]@JoinAcross[(*\:5c06\:53cd\:5e38\:78c1\:77e9\:7684F1,F2\:8fde\:63a5\:8d77\:6765*)
vtx[unq["type"->vtxType["F1","oct"]]],
vtx[unq["type"->vtxType["F2","oct"]]],
{Key[fdType["octb"]],Key[fdType["oct"]]}
];


(* ::Section:: *)
(*order 2*)


(*
\:5c06\:51fa\:73b0\:7684\:4ecb\:5b50\:5bf9\:79f0\:6027\:5316\:ff0c\:4f8b\:5982:
\[LeftAssociation]{"octb"}\[Rule]Overscript["p", _],{"oct"}\[Rule]"p",{"mes"}\[Rule]"\[Pi]+",{"mes","out"}\[Rule]"\[Pi]+",{"F1","oct","o2"}\[Rule]{("d")/2}\[RightAssociation],
\[LeftAssociation]{"octb"}\[Rule]Overscript["p", _],{"oct"}\[Rule]"p",{"mes"}\[Rule]"\[Pi]-",{"mes","out"}\[Rule]"\[Pi]-",{"F1","oct","o2"}\[Rule]{("d")/2}\[RightAssociation]
*)
mesSym[vtx_]:=Union[vtx,vtx/.mesAntiRule];


(*++++++++++++++++++++++++++ \:53cd\:5e38\:78c1\:77e9\:ff0c\:975e\:5b9a\:57df\:5316 +++++++++++++++++++++*)
vtxtp=vtxType["F1F2","o2","nloc"];(*\:66f4\:65b0\:6b64\:9876\:70b9\:7684\:7c7b\:578b*)
(*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
vtx[unq["type"->vtxtp]]=Query[All,
KeyDrop[{vtxType["F1","oct","o2"],vtxType["F2","oct","o2"]}]
]@Query[All,(*\:7ed9\:51faGE,GM\:7684\:975e\:5b9a\:57df\:5316\:5f62\:5f0f\:ff0c\:7528\:65b9\:7a0b\:53cd\:89e3\:51fa F1(Q2),F2(Q2) *)
Append[#,First@Solve[{
vtxType["F1","oct","o2","nloc"]-kin["Q"]^2/(4#[MassKey["vtx","oct"]]^2) vtxType["F2","oct","o2","nloc"]==#[vtxType["F1","oct","o2"]]*kin["\[CapitalLambda]0"]^4/(kin["Q"]^2+kin["\[CapitalLambda]0"]^2)^2,
vtxType["F1","oct","o2","nloc"]+vtxType["F2","oct","o2","nloc"]==kin["\[CapitalLambda]0"]^4/(kin["Q"]^2+kin["\[CapitalLambda]0"]^2)^2 (#[vtxType["F1","oct","o2"]]+ #[vtxType["F2","oct","o2"]])
}
,{vtxType["F1","oct","o2","nloc"],vtxType["F2","oct","o2","nloc"]}
]]&
]@Query[All,(*\:52a0\:4e0a\:7c92\:5b50\:8d28\:91cftag*)
Append[#,MassKey["vtx","oct"]->(#[fdType["oct"]]/.fd[a_,b_,0]:>fd[a,b,2])]&
]@JoinAcross[(*\:5c06\:53cd\:5e38\:78c1\:77e9\:7684F1,F2\:8fde\:63a5\:8d77\:6765*)
vtx[unq["type"->vtxType["F1","oct","o2"]]]//mesSym,
vtx[unq["type"->vtxType["F2","oct","o2"]]]//mesSym,
{Key@fdType["octb"],Key@fdType["oct"],Key@fdType["mes"],Key@fdType["mes","out"]}
];


(* ::Chapter:: *)
(*saveas*)


(* ::Input:: *)
(*If[FileExtension@NotebookFileName[]==="nb",*)
(*FrontEndExecute[FrontEndToken[FrontEnd`EvaluationNotebook[], "Save", {StringTrim[NotebookFileName[],".nb"~~EndOfString]<>".wl", "Package"}]]]*)
