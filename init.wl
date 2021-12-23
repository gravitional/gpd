(* ::Package:: *)

(* ::Title:: *)
(*init.m*)


(*\:5b9a\:4e49\:4e00\:4e9b\:5e38\:7528\:7684\:51fd\:6570*)
enList[x__]:=Replace[{x},{{y__}}:>{y},{0}](*\:5b9a\:4e49\:4e00\:4e2a\:786e\:4fdd\:5217\:8868\:7684\:51fd\:6570*)
enString[x__]:=StringJoin[ToString/@enList[x]](*\:5b9a\:4e49\:4e00\:4e2a\:786e\:4fdd\:5b57\:7b26\:4e32\:7684\:51fd\:6570*)
If[$Notebooks,
echo[x__]:=Print[x],(*\:5b9a\:4e49\:7b14\:8bb0\:672c\:7684\:6253\:5370\:51fd\:6570*)
echo[x__]:=Print["----------------------------","\n\033[1;44m\033[1;37m",enString[x],"\033[0;0m\n","----------------------------"](*\:5b9a\:4e49\:7ec8\:7aef\:7684\:6253\:5370\:51fd\:6570*)
]
(*\:5b9a\:4e49\:672c\:5730git\:76ee\:5f55\:ff0c\:4e5f\:5c31\:662f\:7a0b\:5e8f\:7684\:6839\:76ee\:5f55*)
$srcRoot=AbsoluteFileName[DirectoryName[If[$Notebooks,NotebookFileName[],$InputFileName],1]]


(*\:786e\:4fdd dir \:5b58\:5728, \:5982\:679c\:4e0d\:5b58\:5728\:5219\:521b\:5efa\:76ee\:5f55*)
enDir[dir_]:=If[!DirectoryQ[dir],CreateDirectory[dir];echo["Create a new directory: ",dir]];


(*\:8bb0\:5f55\:7b26\:53f7\:5b9a\:4e49\:6240\:5728\:7684\:6587\:4ef6*)
locationMessage[x_Symbol]:=x::loc=$InputFileName;


(* ::Section:: *)
(*\:4fdd\:5b58\:76f8\:5173*)


(* ::Input:: *)
(*(* \:4f1a\:8bdd\:7684\:8fd0\:884c\:65f6\:95f4 *)*)
(*$sessionTime:=StringRiffle[{Quotient[#,3600],"h:",Quotient[Mod[#,3600],60],"m:",Mod[#,60],"s"}]&@SessionTime[]*)


(* ::Input:: *)
(*(*\:5c06 WL \:6587\:4ef6\:8f6c\:6362\:6210 nb \:6587\:4ef6*)*)
(*wlToNB[wl_]:=Module[{wlObj=NotebookOpen[wl,Visible->False]},*)
(*FrontEndExecute[FrontEndToken[wlObj,"Save",{StringTrim[wl,".wl"~~EndOfString]<>".nb","Notebook"}]];*)
(*NotebookClose[wlObj]*)
(*]*)
(*(*\:5c06 notebook \:8f6c\:6362\:6210 WL \:6587\:4ef6*)*)
(*nbToWL[nb_]:=Module[{nbObj=NotebookOpen[nb,Visible->False]},*)
(*FrontEndExecute[FrontEndToken[nbObj,"Save",{StringTrim[nb,".nb"~~EndOfString]<>".wl","Package"}]];*)
(*NotebookClose[nbObj]*)
(*]*)


(* ::Input:: *)
(*(*\:5c06\:6839\:76ee\:5f55\:4e0b\:7684\:6240\:6709wl\:8f6c\:6362\:6210nb, \:4f46\:4e0d\:5305\:62ec\:6b64\:6587\:4ef6, \:5728\:6bcf\:6b21 git pull \:4e4b\:540e\:8fd0\:884c*)*)
(*wlToNB/@DeleteCases[FileNames["*.wl",$srcRoot],NotebookFileName[]];*)


(* ::Input:: *)
(*(*\:5c06\:6839\:76ee\:5f55\:4e0b\:7684\:6240\:6709 nb \:8f6c\:6362\:6210 Wl, \:4f46\:4e0d\:5305\:62ec\:6b64\:6587\:4ef6, \:5728\:6bcf\:6b21 git commit \:4e4b\:524d\:8fd0\:884c*)*)
(*nbToWL/@DeleteCases[FileNames["*.nb",$srcRoot],NotebookFileName[]];*)
