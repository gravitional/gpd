(* ::Package:: *)

(* ::Section:: *)
(*cmd args*)


(*\:5904\:7406\:547d\:4ee4\:884c\:53c2\:6570\:7684\:5305*)
Get["gen.parse.wl"];


(*\:547d\:4ee4\:884c\:53c2\:6570\:6a21\:677f*)
CmdParser["template"]=<|
"opt"-><|
{"fit"}->{"False","\:662f\:5426\:5904\:4e8e fitting \:53c2\:6570\:6a21\:5f0f"},
{"update"}->{"True","\:662f\:5426\:91cd\:65b0\:8ba1\:7b97 ffsMerged,\:8d39\:66fc\:56fe\:90e8\:5206\:6570\:503c\:7684\:7ed3\:679c"},
{"para-coupl"}->{"False","\:4ee3\:5165\:8026\:5408\:5e38\:6570\:6570\:503c\:65f6,\:662f\:5426\:8fd0\:884c\:5e76\:884c\:5185\:6838."},
{"interp"}->{"True","\:662f\:5426\:8fd0\:884c\:5bf9 full order \:7684\:63d2\:503c\:7a0b\:5e8f."},
{"para-interp"}->{"True","\:8ba1\:7b97 order full \:63d2\:503c\:51fd\:6570\:65f6,\:662f\:5426\:8fd0\:884c\:5e76\:884c\:5185\:6838"},
{"ord"}->{"$ordFull","\:5708\:79ef\:5206\:7684\:7ea7\:6570 order: \:6709 ord0, ord1, ordFull"},
{"lbd-num"}->{"0.80","\:6570\:503c\:8ba1\:7b97\:4e2d Lambda \:7684\:53d6\:503c: 0.80,0.90,1.00"},
{"lbd-fit"}->{"Undefined","\:5f15\:7528\:7684 fitting \:57fa\:4e8e\:7684 Lambda, \:800c\:4e0d\:662f\:6570\:503c\:8ba1\:7b97\:4e2d\:4f7f\:7528\:7684 Lambda: 0.80,0.90,1.00"},
{"fit-scheme"}->{"Automatic","\:62df\:5408\:65b9\:6848\:7684\:8bbe\:7f6e"}
|>,
"pos"->{}
|>;


(* ::Section:: *)
(*read arguments*)


parseCml[]:=Module[{paras,options},
(*\:8ba1\:7b97 cmd \:8f93\:5165, \:6216\:7b14\:8bb0\:672c\:6a21\:62df\:8f93\:5165-----------*)
paras=Query[
(*<|opt,pos|>*)All,
(*<|opt1->val1|>*)All,
(*val*)If[SyntaxQ@#,ToExpression@#,#]&
]@CmdParser["get"];
(*\:63d0\:53d6\:51fa\:9009\:9879\:90e8\:5206*)
options=paras@"opt"//EchoFunction[InputForm];
(*\:6839\:636e\:53c2\:6570\:ff0c\:8fdb\:884c\:76f8\:5e94\:7684\:8bbe\:7f6e------------------------------*)
(*\:5982\:679c\:53ea\:662f\:6253\:5370\:5e2e\:52a9\:4fe1\:606f\:ff0c\:5219\:505c\:6b62\:8ba1\:7b97*)
Switch[options@"help",
False,Null,
_,Abort[]];
(*\:662f\:5426\:5904\:4e8e fitting \:53c2\:6570\:6a21\:5f0f*)
$fittingQ=options@"fit";
(*\:662f\:5426\:91cd\:65b0\:8ba1\:7b97 ffsMerged -------------------*)
$updateFFsMergedQ=options@"update";
(*\:8ba1\:7b97\:6570\:503c\:65f6,\:662f\:5426\:8fd0\:884c\:5e76\:884c\:5185\:6838*)
$parallel$couplsQ=options@"para-coupl";
(*\:662f\:5426\:8fd0\:884c\:5bf9 full order \:7684\:63d2\:503c\:7a0b\:5e8f --------------------*)
$interpolateQ=options@"interp";
(*\:8ba1\:7b97 order full \:63d2\:503c\:51fd\:6570\:65f6,\:662f\:5426\:8fd0\:884c\:5e76\:884c\:5185\:6838-------------*)
$parallel$interpoQ=options@"para-interp"&&$interpolateQ;
(*\:5708\:56fe\:5c55\:5f00\:7684\:9636\:6570-------------------*)
$parOrdStr=options@"ord";
(*\:6570\:503c\:8ba1\:7b97\:4f7f\:7528\:7684 Lambda value ----------------*)
$LambdaNum=options@"lbd-num";
$LambdaNumStr=enString@NumberForm[$LambdaNum,{3,2}];
(* fitScheme \:5b9a\:4e49\:89c1: $fittingScheme*)
(*$fitScheme={"\[CapitalSigma]+-","\[CapitalSigma]","\[CapitalSigma]-p","\[CapitalSigma]N","\[CapitalSigma]-\[CapitalXi]-","N","p\[CapitalXi]-","\[CapitalXi]","charged","many","most","all"};*)
$fitScheme=Switch[options@"fit-scheme",
Automatic,{"\[CapitalSigma]+-","\[CapitalSigma]","\[CapitalSigma]N","N","p\[CapitalXi]-","charged","many","most","all"},
1,{"\[CapitalSigma]+-","N","charged","most"},
2,{"\[CapitalSigma]+-","N","most"},
3,{"N","most"},
_,{"N","most"}
];
(*\:5f15\:7528\:7684 fit \:7ed3\:679c\:57fa\:4e8e\:7684 Lambda, \:800c\:4e0d\:662f\:6570\:503c\:8ba1\:7b97\:4e2d\:4f7f\:7528\:7684 Lambda *)
$LambdaFit=Switch[options@"lbd-fit",
Undefined,$LambdaNum,
_,options@"lbd-fit"
];
(*\:76f8\:5e94\:7684\:5b57\:7b26\:5f62\:5f0f*)
$LambdaFitStr=enString@NumberForm[$LambdaFit,{3,2}];
]


(* ::Section:: *)
(*arguments check*)


parseCml["check"]:=echo["The configur is: ",$LambdaNum];
