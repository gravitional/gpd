(* ::Package:: *)

(* ::Section:: *)
(*parse1*)


parse[plist_List]:=AssociationThread@@Transpose[PadRight[#,2,True]&/@StringSplit[StringSplit[StringJoin@Riffle[plist,"\036"],"-"],"\036"]]
parse@{"-a","1","-b","2","-c","-d","-f","ss"}


(* ::Section:: *)
(*parse2*)


parse[plist_List]:=Which[
(*----------------*)
Length@plist>=2&&StringStartsQ[plist[[1]],"-"]&&!StringStartsQ[plist[[2]],"-"],
{StringTrim[plist[[1]],StartOfString~~"-"]->plist[[2]]}~Join~parse[plist[[3;;]]],
(*----------------*)
Length@plist>=1&&StringStartsQ[plist[[1]],"-"],
{StringTrim[plist[[1]],StartOfString~~"-"]->True}~Join~parse[Rest@plist],
(*----------------*)
Length@plist>=1,
{StringTrim[plist[[1]],StartOfString~~"-"]->True}~Join~parse[Rest@plist],
True,{}
]
parse@{"-a","1","-b","2","-c","-d","-f","ss"}


(* ::Chapter:: *)
(*parseGnu*)


parseGnu[plist_List]:=Which[
(*----------------*)
Length@plist>=2&&StringStartsQ[First@plist,"-"]&&!StringStartsQ[plist[[2]],"-"]&&StringLength[First@plist]==2||
Length@plist>=2&&StringStartsQ[First@plist,"--"]&&!StringStartsQ[plist[[2]],"-"]&&StringLength[First@plist]>=4,
{StringTrim[plist[[1]],StartOfString~~"-"..]->plist[[2]]}~Join~parseGnu[plist[[3;;]]],
(*----------------*)
Length@plist>=1&&StringStartsQ[First@plist,"-"~~Except["-"]]&&StringLength[First@plist]>2,
(Rule[#,True]&/@Characters@StringTrim[plist[[1]],StartOfString~~"-"])~Join~parseGnu[Rest@plist],
(*----------------*)
Length@plist>=1&&StringStartsQ[First@plist,"--"],
{StringTrim[plist[[1]],StartOfString~~"--"]->True}~Join~parseGnu[Rest@plist],
(*----------------*)
Length@plist>=1,
{First@plist->True}~Join~parseGnu[Rest@plist],
True,{}
]


parseGnu@{"-asq","1","--boj","2","-cfd","--dddf","--fff","ss"}


(* ::Input:: *)
(*StringTrim["--boj",StartOfString~~"-"..]*)
