(* ::Package:: *)

(*\:5b9a\:4e49\:4e00\:4e9b\:5e38\:7528\:7684\:51fd\:6570*)
enList[x__]:=Replace[{x},{{y__}}:>{y},{0}](*\:5b9a\:4e49\:4e00\:4e2a\:786e\:4fdd\:5217\:8868\:7684\:51fd\:6570*)
enString[x__]:=StringJoin[ToString/@enList[x]](*\:5b9a\:4e49\:4e00\:4e2a\:786e\:4fdd\:5b57\:7b26\:4e32\:7684\:51fd\:6570*)


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
(*CmdParse*)


CmdParse["ToRuleLst"][plist_List]:=Which[
(*\:957f\:5ea6\[GreaterEqual]1, -- val.., \:540e\:9762\:7684\:90fd\:5f53\:6210\:4f4d\:7f6e\:53c2\:6570 ----------------*)
Length@plist>=1&&StringMatchQ[First@plist,"--"],
Rule[#,True]&/@Rest@plist,
(*\:957f\:5ea6\[GreaterEqual]2, -o val, \:6216\:8005 --opt, val \:7684\:6a21\:5f0f----------------*)
Length@plist>=2&&StringStartsQ[First@plist,"-"]&&!StringStartsQ[plist[[2]],"-"]&&StringLength[First@plist]==2||
Length@plist>=2&&StringStartsQ[First@plist,"--"]&&!StringStartsQ[plist[[2]],"-"]&&StringLength[First@plist]>=4,
{plist[[1]]->plist[[2]]}~Join~CmdParse["ToRuleLst"][plist[[3;;]]],
(*\:957f\:5ea6\[GreaterEqual]1, -abc, \:77ed\:9009\:9879\:7684\:805a\:5408 ----------*)
Length@plist>=1&&StringStartsQ[First@plist,"-"~~Except["-"]]&&StringLength[First@plist]>2,
(Rule["-"<>#,True]&/@Characters@StringTrim[plist[[1]],StartOfString~~"-"])~Join~CmdParse["ToRuleLst"][Rest@plist],
(*\:957f\:5ea6\[GreaterEqual]1, --opt \:6216\:8005 -o, \:5f00\:5173 ---------*)
Length@plist>=1&&StringStartsQ[First@plist,"--"|"-"],
{plist[[1]]->True}~Join~CmdParse["ToRuleLst"][Rest@plist],
(*\:5176\:4ed6\:60c5\:51b5, \:90fd\:5f53\:6210\:4f4d\:7f6e\:53c2\:6570----------------*)
Length@plist>=1,
{First@plist->True}~Join~CmdParse["ToRuleLst"][Rest@plist],
(*\:5982\:679c\:4f20\:5165\:7a7a\:5217\:8868--------------*)
True,{}
]


CmdParse["ToAssoc"][plist_List]:=Module[{
(*\:52a0\:4e0a\:9ed8\:8ba4\:9009\:9879 help*)
assoc=Merge[Join@@#&]@{
<|True->{"--help"->False},False->{}|>,
GroupBy[CmdParse["ToRuleLst"]@plist,StringStartsQ[Keys@#,"-"]&]
}},
<|
(*trim \:6389\:5f00\:5934\:7684 - \:6216 --*)(*\:6539\:6210\:5173\:8054\:ff0c\:5bf9\:4e8e\:91cd\:590d\:7684\:9009\:9879, \:6700\:540e\:4e00\:4e2a\:751f\:6548*)
"opt"->KeyMap[StringTrim[#,StartOfString~~"-"..]&]@Association@assoc[[Key@True]],
(*\:4f4d\:7f6e\:53c2\:6570\:7684\:5217\:8868*)
"pos"->Keys@assoc[[Key@False]]
|>]


(* ::Section:: *)
(*dasf*)


CmdParse["tmp"]=<|
"opt"-><|
{"abs","a"}->{1,"the abs feature"},
{"big","b"}->{True,"the big feature"},
"l1"->"2",
"l2"->True,
"l3"->2
|>,
"pos"->{
{1,"the 1st position arguments"},
2
}
|>;


CmdParse["parse-tmp"]:=Module[{optValue,optHelp,posValue,posHelp,pair},
optValue[key_,val_]:=Rule[#,First@enList[val]]&/@enList[key];
posValue[pos_]:=First@enList@pos;
optHelp[key_,val_]:=key->enList[val][[2;;]];
posHelp[pos_]:=(pair=enList@pos;First@pair->pair[[2;;]]);
(* asdfdsafa af as*)
<|
"opt"->Association@KeyValueMap[optValue]@CmdParse["tmp"]@"opt",
"pos"->posValue/@CmdParse["tmp"]@"pos",
"help"->Association@Join[
KeyValueMap[optHelp]@CmdParse["tmp"]@"opt",
posHelp/@CmdParse["tmp"]@"pos"
]
|>
]


CmdParse["GetOpt"]:=Merge[Join@@#&]@{
CmdParse["parse-tmp"],
CmdParse["ToAssoc"]@$scriptCommandLine}


$scriptCommandLine={"--boj","2","--help"};
CmdParse["GetOpt"]


(* ::Section:: *)
(*fsdg*)


CmdParse["help"]:=StringRiffle[
KeyValueMap[{StringRiffle[enList@#1,", "],StringRiffle[enList@#2]}&]@
CmdParse["GetOpt"]@"help"
,"\n",": "]


If[CmdParse["GetOpt"]["opt"]["help"],
CmdParse["help"]]
