(* ::Package:: *)

(* ::Title:: *)
(*CmdParser*)


(*\:8bbe\:7f6e CmdParser` \:4e3a\:5f53\:524d\:4e0a\:4e0b\:6587, \:5e76\:4e14\:628a System` \:653e\:8fdb $ContextPath*)
BeginPackage["CmdParser`"]


(*\:4ecb\:7ecd\:6253\:7b97\:8981\:5bfc\:51fa\:7684\:5bf9\:8c61, \:4e0d\:5305\:62ec\:5176\:4ed6\:5bf9\:8c61, \:51fd\:6570\:540d\:5728\:8fd9\:91cc\:5efa\:7acb\:540e, \:5b83\:7684\:4e0a\:4e0b\:6587\:4f1a\:662fPackage`, \:53ef\:4ee5\:88ab\:5916\:90e8\:4f7f\:7528*)
CmdParser::usage="CmdParser[\"template\"] \:7ed9\:51fa\:547d\:4ee4\:884c\:53c2\:6570\:7684\:6a21\:677f. \:6307\:5b9a\:9009\:9879\:7684\:957f\:540d\:79f0\:ff0c\:77ed\:540d\:79f0; \:4f4d\:7f6e\:53c2\:6570; \:9ed8\:8ba4\:503c; Example:
CmdParser[\"template\"]=<|
\"opt\"\[Rule]<|
{\"abs\",\"a\"}\[Rule]{1,\"the abs feature\"},
\"l1\"\[Rule]\"2\"|>,
\"pos\"\[Rule]{
{1,\"the 1st position arguments\"},
2}|>
],FormatType\[Rule]InputForm];

CmdParser[\"init\"] CmdParser[\"init\"] \:5f15\:7528\:547d\:4ee4\:884c\:53c2\:6570 $ScriptCommandLine, \:7ed3\:5408\:53c2\:6570\:6a21\:677f\:ff0c\:5b8c\:6210\:89e3\:6790;

CmdParser[\"pseudo\"] Mock \:547d\:4ee4\:884c\:8f93\:5165, \:5728\:7b14\:8bb0\:672c\:754c\:9762\:8c03\:8bd5\:4f7f\:7528;";


(*\:8bbe\:7f6e\:5f53\:524d\:4e0a\:4e0b\:6587\:4e3a  Package`Private`*)
Begin["`Private`"]


(* ::Section:: *)
(*auxlinary*)


(*\:5b9a\:4e49\:4e00\:4e9b\:5e38\:7528\:7684\:51fd\:6570*)
enList[x__]:=Replace[{x},{{y__}}:>{y},{0}](*\:786e\:4fdd\:5217\:8868\:7684\:51fd\:6570*)
enString[x__]:=StringJoin[ToString/@enList[x]](*\:786e\:4fdd\:5b57\:7b26\:4e32\:7684\:51fd\:6570*)


If[$Notebooks,
$inputCml:=CmdParser["pseudo"];,
(*\:7531\:7528\:6237\:7ed9\:51fa\:7684 \:547d\:4ee4\:884c\:53c2\:6570 \:6a21\:677f\:ff0c\:6307\:5b9a\:53c2\:6570\:957f\:540d\:79f0\:ff0c\:77ed\:540d\:79f0\:ff0c\:53c2\:6570\:8bf4\:660e\:ff0c\:4ee5\:53ca\:9ed8\:8ba4\:503c,
\:65e0\:9ed8\:8ba4\:503c\:7684\:53c2\:6570\:7528 Undefined \:7ed9\:51fa *)
(*--------------------*)
$inputCml:=$ScriptCommandLine;
]


(* ::Section:: *)
(*GNU CommandLine Parser*)


(*\:5c06 \:547d\:4ee4\:884c\:53c2\:6570 \:89e3\:6790\:6210 \:89c4\:5219\:5217\:8868 \:683c\:5f0f*)
Parser["ToRuleLst"][plist_List]:=Which[
(*\:957f\:5ea6\[GreaterEqual]1, -- val.., \:540e\:9762\:7684\:90fd\:5f53\:6210\:4f4d\:7f6e\:53c2\:6570 ----------------*)
Length@plist>=1&&StringMatchQ[First@plist,"--"],
Rule[#,True]&/@Rest@plist,
(*\:957f\:5ea6\[GreaterEqual]2, -o val, \:6216\:8005 --opt, val \:7684\:6a21\:5f0f----------------*)
Length@plist>=2&&StringStartsQ[First@plist,"-"]&&!StringStartsQ[plist[[2]],"-"]&&StringLength[First@plist]==2||
Length@plist>=2&&StringStartsQ[First@plist,"--"]&&!StringStartsQ[plist[[2]],"-"]&&StringLength[First@plist]>=4,
{plist[[1]]->plist[[2]]}~Join~Parser["ToRuleLst"][plist[[3;;]]],
(*\:957f\:5ea6\[GreaterEqual]1, -abc, \:77ed\:9009\:9879\:7684\:805a\:5408 ----------*)
Length@plist>=1&&StringStartsQ[First@plist,"-"~~Except["-"]]&&StringLength[First@plist]>2,
(Rule["-"<>#,True]&/@Characters@StringTrim[plist[[1]],StartOfString~~"-"])~Join~Parser["ToRuleLst"][Rest@plist],
(*\:957f\:5ea6\[GreaterEqual]1, --opt \:6216\:8005 -o, \:5f00\:5173 ---------*)
Length@plist>=1&&StringStartsQ[First@plist,"--"|"-"],
{plist[[1]]->True}~Join~Parser["ToRuleLst"][Rest@plist],
(*\:5176\:4ed6\:60c5\:51b5, \:90fd\:5f53\:6210\:4f4d\:7f6e\:53c2\:6570----------------*)
Length@plist>=1,
{First@plist->True}~Join~Parser["ToRuleLst"][Rest@plist],
(*\:5982\:679c\:4f20\:5165\:7a7a\:5217\:8868--------------*)
True,{}
]


(*\:5c06 \:89c4\:5219\:5217\:8868 \:89e3\:6790\:6210\:9009\:9879 option \:548c \:4f4d\:7f6e\:53c2\:6570 pos, \:5b58\:50a8\:5728\:5173\:8054\:4e2d*)
Parser["ToAssoc"][plist_List]:=Module[{
(*\:52a0\:4e0a\:9ed8\:8ba4\:9009\:9879 help*)
assoc=Merge[Join@@#&]@{
<|True->{"--help"->False},False->{}|>,
GroupBy[Parser["ToRuleLst"]@plist,StringStartsQ[Keys@#,"-"]&]
}},
<|
(*trim \:6389\:5f00\:5934\:7684 - \:6216 --*)(*\:6539\:6210\:5173\:8054\:ff0c\:5bf9\:4e8e\:91cd\:590d\:7684\:9009\:9879, \:6700\:540e\:4e00\:4e2a\:751f\:6548*)
"opt"->KeyMap[StringTrim[#,StartOfString~~"-"..]&]@Association@assoc[[Key@True]],
(*\:4f4d\:7f6e\:53c2\:6570\:7684\:5217\:8868*)
"pos"->Keys@assoc[[Key@False]]
|>]


(* ::Section:: *)
(*argument template*)


Parser::NeedTemplate="Please give the cmdline argument template, for example:\n
CmdParser[\"template\"]=<|
\"opt\"-><|
{\"abs\",\"a\"}->{1,\"Argument description: the abs feature.\"},
\"bar\"->{2, \"Description can leave out with blank.\"}
|>,
\"pos\"->{
{1,\"The 1st position argument: file name\"},
{2,\"The 2st position argument: file author\"}
}
|>";


(*\:63d0\:53d6\:51fa \:9009\:9879\:53c2\:6570 key-value pair*)
optValue[key_,val_]:=Rule[#,First@enList[val]]&/@enList[key];
(*\:63d0\:53d6\:51fa\:4f4d\:7f6e\:53c2\:6570\:7684 value *)
posValue[pos_]:=First@enList@pos;
(*\:9009\:9879\:53c2\:6570 \:7684\:5e2e\:52a9\:6587\:6863, {longName,shortName}\[Rule]{Description} *)
optHelp[key_,val_]:=enList@key->enList[val][[2;;]];
(*\:4f4d\:7f6e\:53c2\:6570 \:7684\:5e2e\:52a9\:6587\:6863, {order}\[Rule]{Description}*)
posHelp[pos_]:=#[[{1}]]->#[[2;;]]&[ToString/@enList@pos];


(*\:89e3\:6790\:7528\:6237\:7ed9\:51fa\:7684 \:547d\:4ee4\:884c\:53c2\:6570 \:6a21\:677f, \:751f\:6210 option, position, \:4ee5\:53ca\:5e2e\:52a9\:4fe1\:606f*)
Parser["parser-template"]:=If[
Head@CmdParser["template"]=!=Association,
(*\:5982\:679c\:8fd8\:672a\:5b9a\:4e49\:6a21\:677f\:ff0c\:5219\:63d0\:9192\:5b9a\:4e49\:6a21\:677f--------------------------*)
Message[Parser::NeedTemplate];Abort[],
(*\:4ece\:6a21\:677f\:4e2d\:89e3\:6790\:51fa\:53c2\:6570\:548c\:9ed8\:8ba4\:503c---------------------------*)
<|
"opt"->Association@KeyValueMap[optValue]@CmdParser["template"]@"opt",
"pos"->posValue/@CmdParser["template"]@"pos",
"HelpAll"->Association@Join[
(*\:89e3\:6790 option\:53c2\:6570 \:7684\:5e2e\:52a9*)
KeyValueMap[optHelp]@CmdParser["template"]@"opt",
(*\:89e3\:6790 position\:53c2\:6570 \:7684\:5e2e\:52a9*)
posHelp/@CmdParser["template"]@"pos"
]
|>
]


(* ::Section:: *)
(*help message*)


(*\:6253\:5370\:5e2e\:52a9\:4fe1\:606f*)
Parser["echo-help"][HelpMessage_]:=Print[
"\033[1;44m\033[1;37m"<>"Help Message:"<>"\033[0;0m\n",
StringRiffle[
KeyValueMap[{StringRiffle[#1,", "],StringRiffle[#2]}&]@
HelpMessage
,"\n",":\t"]
,"\n"]


Parser["need-help?"]:=Module[{
(*\:547d\:4ee4\:884c\:67e5\:8be2\:7684\:5177\:4f53\:5e2e\:52a9 {optName} \:6216\:8005 {order}, \:4f4d\:7f6e\:53c2\:6570\:7684\:987a\:5e8f *)
HelpNeed=enList@Parser["session"]["opt"]["help"],
HelpAll=Parser["session"]["HelpAll"],
helpSearch
},
helpSearch=KeySelect[ContainsAny[#,HelpNeed]&]@HelpAll;

Switch[HelpNeed,
(*\:5982\:679c\:662f help->True, \:8f93\:51fa\:6240\:6709\:5e2e\:52a9*)
{True},Parser["echo-help"]@HelpAll,
(*\:5982\:679c\:662f help\[Rule]False, \:4e0d\:8fdb\:884c\:64cd\:4f5c*)
{False},Null,
(*\:5982\:679c\:662f help\[Rule]HelpNeed, \:8f93\:51fa\:7279\:5b9a\:6761\:76ee*)
_,If[helpSearch===<||>,
Print["I can't find this term in help document"];Abort[],
Parser["echo-help"]@helpSearch
]]]


(* ::Section:: *)
(*parser initiate*)


CmdParser["init"]:=(
Parser["session"]=Merge[Join@@#&]@{
Parser["parser-template"],
Parser["ToAssoc"]@$inputCml
};
Parser["need-help?"];
Parser["session"]
)


(* ::Chapter:: *)
(*EOF*)


(*\:6062\:590d\:5230\:4e4b\:524d\:7684\:4e0a\:4e0b\:6587, \:6b64\:5904\:4e3a Package` *)
End[]
(*\:7ed3\:675f\:5305, \:628aPackage`\:653e\:5230\:4e0a\:4e0b\:6587\:641c\:7d22\:8def\:5f84\:4e2d*)
EndPackage[]


(* ::Chapter:: *)
(*Other*)


(* ::Section::Closed:: *)
(*simple parser*)


(* ::Input:: *)
(*parser[plist_List]:=AssociationThread@@Transpose[PadRight[#,2,True]&/@StringSplit[StringSplit[StringJoin@Riffle[plist,"\036"],"-"],"\036"]]*)
(*parser@{"-a","1","-b","2","-c","-d","-f","ss"}*)


(* ::Section::Closed:: *)
(*simple parser 2*)


(* ::Input:: *)
(*parser[plist_List]:=Which[*)
(*(*----------------*)*)
(*Length@plist>=2&&StringStartsQ[plist[[1]],"-"]&&!StringStartsQ[plist[[2]],"-"],*)
(*{StringTrim[plist[[1]],StartOfString~~"-"]->plist[[2]]}~Join~parser[plist[[3;;]]],*)
(*(*----------------*)*)
(*Length@plist>=1&&StringStartsQ[plist[[1]],"-"],*)
(*{StringTrim[plist[[1]],StartOfString~~"-"]->True}~Join~parser[Rest@plist],*)
(*(*----------------*)*)
(*Length@plist>=1,*)
(*{StringTrim[plist[[1]],StartOfString~~"-"]->True}~Join~parser[Rest@plist],*)
(*True,{}*)
(*]*)
(*parser@{"-a","1","-b","2","-c","-d","-f","ss"}*)
