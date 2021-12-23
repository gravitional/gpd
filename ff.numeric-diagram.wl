(* ::Package:: *)

(* ::Title:: *)
(*ff.nume*)


loopResults["v"]=Import@localCachePath["loop-result"];


ffsLoop["diagram"]=Query[(*order \:5c42*)$ordFull,(*loop Channel \:5c42*)kLoopChanSum,
(*octet*)All,(*diagram*)All,(*F1F2,GEGM*)All
]@loopResults["v"];


(* \:62df\:5408\:51fa\:7684 c1,c2 \:503c\:88ab\:653e\:5728 ccfitted$Err \:4e2d, \:4f7f\:7528 Query \:67e5\:8be2; \:8003\:8651 \:53c2\:6570\:4e4b\:95f4\:7684\:9650\:5236\:ff0c\:7ed9\:51fa\:6240\:6709\:53c2\:6570\:7684\:6570\:503c*)
(*\:5404\:79cd C,c1,c2 \:914d\:7f6e\:4e0b\:ff0c\:8ba1\:7b97\:51fa\:7684\:6570\:503c\:7ed3\:679c\:ff0c\:5938\:514b\:7535\:8377\:548c Q2 \:672a\:4ee3\:5165\:5177\:4f53\:6570\:503c*)
Module[{ccNumStr},
ffsLoop["confs"]=Association@Table[
(*\:521d\:59cb\:5316 C \:5b57\:7b26\:5f62\:5f0f*)
ccNumStr=enString@NumberForm[ccNum,{3,2}];
(*\:4ee3\:5165\:62df\:5408\:786e\:5b9a\:7684\:6570\:503c*)
Query[{Key@cc["C",ccNumStr]},All(*\:8ba1\:7b97\:6240\:6709\:62df\:5408\:65b9\:6848*),
(* \:5e94\:7528\:5230 ccfitted$Err \:6570\:636e\:5c42\:7684\:51fd\:6570*)
apply$cc$numeric[ccNum][(*\:9996\:53c2\:6570\:662f\:51fd\:6570\:ff0c\:6d88\:8d39\:4ea7\:751f \:53c2\:6570\:89c4\:5219*)
Query[(*octet*)All,(*diagram*)All,(*FFactors*)All,
ReplaceAll[#]]@ffsLoop["diagram"]&,
(*ccfitted \:7684\:9996\:5143\:7d20\:662f\:8bef\:5dee\:ff0c\:672b\:5143\:7d20\:662f c \:7684\:66ff\:6362\:89c4\:5219 *)
Last@#]&
]@ccfitted$Err
(* C\:7684\:8fed\:4ee3\:5217\:8868 *)
,{ccNum,{1.0,1.1,1.2,1.3,1.4,1.5}} 
]];
