(* ::Package:: *)

(* ::Chapter:: *)
(*fields in human-readable form*)


recordLocationInMessage[ff,fd,fdDisp]


(* ::Section:: *)
(*meson*)


(*\:516b\:91cd\:6001\:4ecb\:5b50\:7684\:8f93\:5165\:63a5\:53e3 ++++++++++++++++++++++++++++++++*)
Block[{type},
type="mes";
fdStr[type]={(*\:4ecb\:5b50\:573a\:7684\:5b57\:7b26\:4e32\:8868\:793a*)
"\[Eta]0",
"\[Pi]+","\[Pi]0","\[Pi]-",
"K+","K-","K0","K0b",
"\[Eta]8"
};
(* ++++++++ \:5b9a\:4e49\:4ecb\:5b50\:8f93\:5165\:63a5\:53e3, ff["\[Pi]+"]=fd[1,1,0] ++++++++ *)
Activate@Inactive[Set][
Inactive[ff]/@fdStr[type],Array[fd[1,#,0]&,9,{0,8}]
];
Activate@Inactive[Set][
Inactive[ff]@"\[Pi]\[Eta]",fd[1,{0,2,8},0]
];(*\[Eta]0,\[Pi]0,\[Eta]8,\:7684\:7b80\:5e76\:8868\:793a*)
(*------------ \:8bbe\:7f6e\:573a\:7684 Display \:5185\:5bb9 ------------*)
Activate@Inactive[Set][
Array[Inactive[fdDisp][1,#,0]&,9,{0,8}],
fdStr[type]
];
(* \:4ecb\:5b50Key-Value, \:4f8b\:5982 mes["\[Pi]+"]={"mes"}\[Rule]fd[1,1,0] ----------- *)
Activate@Inactive[Set][
Inactive[mes]/@fdStr[type],
(*Array \:751f\:6210 \:4ecb\:5b50\:7684raw\:8868\:793a*)
fdType[type]->#& /@ Array[fd[1,#,0]&,9,{0,8}]
];
(*\:4ecb\:5b50\:53cd\:573a\:7684raw\:8868\:793a*)
Activate@Inactive[Set][
Inactive[mes][#,"out"]&/@fdStr[type],
fdType[type,"out"]->#&/@Array[fd[1,#,0]&,9,{0,8}]
];
(*Block end*)];


(* ::Section:: *)
(*octet*)


(*\:516b\:91cd\:6001\:91cd\:5b50\:7684\:8f93\:5165\:63a5\:53e3 ++++++++++++++++++++++++++++++++*)
Block[{type},
type="oct";
fdStr[type]={
"p","n",
"\[CapitalSigma]+","\[CapitalSigma]0","\[CapitalSigma]-",
"\[CapitalXi]0","\[CapitalXi]-",
"\[CapitalLambda]"};
(* ----- \:5b9a\:4e49\:8f93\:5165\:63a5\:53e3,  ff["p"]=fd[2,1,0] ----- *)
Activate@Inactive[Set][
Inactive[ff]/@fdStr[type],
Array[fd[2,#,0]&,8]
];
Activate@Inactive[Set][
Inactive[ff]@"\[CapitalSigma]0\[CapitalLambda]",fd[2,{4,8},0]
];(*\[CapitalSigma]0,\[CapitalLambda],\:7684\:7b80\:5e76\:8868\:793a*)
(*------------ \:8bbe\:7f6e\:573a\:7684 Display \:5185\:5bb9 ------------*)
Activate@Inactive[Set][
Array[Inactive[fdDisp][2,#,0]&,8],
fdStr[type]
];
(* oct Key-Value, \:4f8b\:5982 oct["p"]={"oct"}\[Rule]fd[2,1,0] ------ *)
Activate@Inactive[Set][
Inactive[oct]/@fdStr[type],
fdType[type]->#& /@ Array[fd[2,#,0]&,8]
];
(*++++++++++++++++++++++++ \:516b\:91cd\:6001\:91cd\:5b50 anti field, \:5e26 bar \:573a ++++++++++++++++++++++++ *)
type="octb";
fdStr[type]={
"pb","nb",
"\[CapitalSigma]+b","\[CapitalSigma]0b","\[CapitalSigma]-b",
"\[CapitalXi]0b","\[CapitalXi]-b",
"\[CapitalLambda]b"};
(* ------ \:5b9a\:4e49\:8f93\:5165\:63a5\:53e3,  ff["p"]=fd[2,1,0] ------ *)
Activate@Inactive[Set][
Inactive[ff]/@fdStr[type],
Array[fd[2,#,1]&,8]
];
Activate@Inactive[Set][
Inactive[ff]@"\[CapitalSigma]0b\[CapitalLambda]b",
fd[2,{4,8},1](*\[CapitalSigma]0b,\[CapitalLambda]b \:7684\:7b80\:5e76\:8868\:793a*)
];
(*------------ \:8bbe\:7f6e\:573a\:7684 Display \:5185\:5bb9 ------------*)
Activate@Inactive[Set][
Array[Inactive[fdDisp][2,#,1]&,8],
fdStr[type]
];
(* oct Key-Value, \:4f8b\:5982 oct["pb"]={"octb"}\[Rule]fd[2,1,1] ------ *)
Activate@Inactive[Set][
Inactive[oct]/@fdStr[type],
fdType[type]->#& /@ Array[fd[2,#,1]&,8]
];
(*block end*)];


(*\:66ff\:6362\:89c4\:5219\:ff0c\[Psi]bar\[Rule]\[Psi]*)
octetRule["bar->reg"]=MapThread[Rule,{
Array[fd[2,#,1]&,8]
,Array[fd[2,#,0]&,8]
}];
(*\:66ff\:6362\:89c4\:5219\:ff0c\[Psi]\[Rule]\[Psi]bar*)
octetRule["reg->bar"]=MapThread[Rule,{
Array[fd[2,#,0]&,8]
,Array[fd[2,#,1]&,8]
}];


(* ::Section:: *)
(*decuplet*)


(*++++++++++++++++++++++++ \:5341\:91cd\:6001\:91cd\:5b50\:7684\:8f93\:5165\:63a5\:53e3 ++++++++++++++++++++++++*)
Block[{type},
type="dec";
fdStr[type]={
"\[CapitalDelta]++","\[CapitalDelta]+","\[CapitalDelta]0","\[CapitalDelta]-",
"\[CapitalSigma]*+","\[CapitalSigma]*0","\[CapitalSigma]*-",
"\[CapitalXi]*0","\[CapitalXi]*-",
"\[CapitalOmega]-"
};
(* ++++++++ \:5b9a\:4e49\:8f93\:5165\:63a5\:53e3, ff["\[CapitalDelta]++"]=fd[3,1,0] ++++++++ *)
Activate@Inactive[Set][
Inactive[ff]/@fdStr[type],
Array[fd[3,#,0]&,10]
];
(*------------ \:8bbe\:7f6e\:573a\:7684 Display \:5185\:5bb9 ------------*)
Activate@Inactive[Set][
Array[Inactive[fdDisp][3,#,0]&,10],
fdStr[type]
];
(*\:8d39\:66fc\:9876\:70b9\:8f93\:5165\:63a5\:53e3 --------------------------*)
Activate@Inactive[Set][
Inactive[dec]/@fdStr[type],
fdType[type]->#& /@ Array[fd[3,#,0]&,10]
];
(*\:5341\:91cd\:6001\:91cd\:5b50 anti field, \:5e26 bar \:573a----------------------*)
type="decb";
fdStr[type]={
"\[CapitalDelta]++b","\[CapitalDelta]+b","\[CapitalDelta]0b","\[CapitalDelta]-b",
"\[CapitalSigma]*+b","\[CapitalSigma]*0b","\[CapitalSigma]*-b",
"\[CapitalXi]*0b","\[CapitalXi]*-b",
"\[CapitalOmega]-b"
};
Activate@Inactive[Set][
Inactive[ff]/@fdStr[type],
Array[fd[3,#,1]&,10]
];
(*------------ \:8bbe\:7f6e\:573a\:7684 Display \:5185\:5bb9 ------------*)
Activate@Inactive[Set][
Array[Inactive[fdDisp][3,#,1]&,10],
fdStr[type]
];
(*\:8d39\:66fc\:9876\:70b9\:8f93\:5165\:63a5\:53e3 --------------------------*)
Activate@Inactive[Set][
Inactive[dec]/@fdStr[type],
fdType[type]->#& /@ Array[fd[3,#,1]&,10]
];
(*block end*)];


(*\:66ff\:6362\:89c4\:5219\:ff0c\[Psi]\[Rule]\[Psi]bar*)
decupletRule["reg->bar"]=MapThread[Rule,{
Array[fd[3,#,0]&,10]
,Array[fd[3,#,1]&,10]
}];
(*\:66ff\:6362\:89c4\:5219\:ff0c\[Psi]bar\[Rule]\[Psi]*)
decupletRule["bar->reg"]=MapThread[Rule,{
Array[fd[3,#,1]&,10]
,Array[fd[3,#,0]&,10]
}];


(* ::Section:: *)
(*quark*)


(*\:66ff\:6362\:89c4\:5219:\:5938\:514b\[Rule]\:53cd\:5938\:514b*)
quarkRule["reg->bar"]={fd[4,1,0]->fd[4,1,1],fd[4,2,0]->fd[4,2,1],fd[4,3,0]->fd[4,3,1]};
(*\:66ff\:6362\:89c4\:5219:\:53cd\:5938\:514b\[Rule]\:5938\:514b*)
quarkRule["bar->reg"]={fd[4,1,1]->fd[4,1,0],fd[4,2,1]->fd[4,2,0],fd[4,3,1]->fd[4,3,0]};
(*\:66ff\:6362\:89c4\:5219:\:6b63\:53cd\:5938\:514b\:4e92\:6362*)
quarkRule["reverse"]=quarkRule["reg->bar"]~Join~quarkRule["bar->reg"];


(*++++++++++++++++++++++++ quark \:7684\:8f93\:5165\:63a5\:53e3 ++++++++++++++++++++++++*)
Block[{type},
type="qua";
fdStr[type]={"u","d","s"};
(* ++++++++ \:5b9a\:4e49\:8f93\:5165\:63a5\:53e3, quark human-readable, ff["u"]=fd[4,1,0] ++++++++ *)
Activate@Inactive[Set][
Inactive[ff]/@fdStr[type],
Array[fd[4,#,0]&,3]
];
(*------------ \:8bbe\:7f6e\:573a\:7684 Display \:5185\:5bb9 ------------*)
Activate@Inactive[Set][
Array[Inactive[fdDisp][4,#,0]&,3],
fdStr[type]
];
(*\:8d39\:66fc\:9876\:70b9\:8f93\:5165\:63a5\:53e3 --------------------------*)
Activate@Inactive[Set][
Inactive[qua]/@fdStr[type],
fdType[type]->#& /@ Array[fd[4,#,0]&,3]
];
(* -----------\:53cd\:5938\:514b\:7684\:8f93\:5165\:63a5\:53e3 ----------- *)
type="quab";
fdStr[type]={"ub","db","sb"};
Activate@Inactive[Set][
Inactive[ff]/@fdStr[type],
Array[fd[4,#,1]&,3]
];
(*------------ \:8bbe\:7f6e\:573a\:7684 Display \:5185\:5bb9 ------------*)
Activate@Inactive[Set][
Array[Inactive[fdDisp][4,#,1]&,3],
fdStr[type]
];
(*\:8d39\:66fc\:9876\:70b9\:8f93\:5165\:63a5\:53e3 --------------------------*)
Activate@Inactive[Set][
Inactive[qua]/@fdStr[type],
fdType[type]->#& /@ Array[fd[4,#,1]&,3]
];
(*block end*)];
