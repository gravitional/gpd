(* ::Package:: *)

(* ::Title:: *)
(*coe_quarkflow.nb*)


(* ::Chapter:: *)
(*initial*)


(*\:8ba1\:7b97\:73af\:5883\:53c2\:91cf\:ff0c\:6bd4\:5982\:8def\:5f84*)
gitRemoteName="gpd";(*\:7ed9\:51fa\:8fdc\:7a0bgit\:4ed3\:5e93\:7684\:540d\:5b57*)
cmdQ=Not[$Notebooks];(*\:811a\:672c\:7684\:8fd0\:884c\:6a21\:5f0f\:5224\:65ad\:ff0cTrue\:4ee3\:8868\:547d\:4ee4\:884c\:ff0cFalse\:4ee3\:8868\:524d\:7aef*)
fileName=If[Not[cmdQ],NotebookFileName[],$InputFileName](*\:7ed9\:51fa\:7b14\:8bb0\:672c\:7684\:7edd\:5bf9\:8def\:5f84*)
(*\:5b9a\:4e49\:4e00\:4e9b\:5e38\:7528\:7684\:51fd\:6570*)
enList[x__]:=Replace[{x},{{y__}}:>{y},{0}](*\:5b9a\:4e49\:4e00\:4e2a\:786e\:4fdd\:5217\:8868\:7684\:51fd\:6570*)
enString[x__]:=StringJoin[ToString/@enList[x]](*\:5b9a\:4e49\:4e00\:4e2a\:786e\:4fdd\:5b57\:7b26\:4e32\:7684\:51fd\:6570*)
If[cmdQ,
echo[x__]:=Print["----------------------------","\n\033[1;44m\033[1;37m",enString[x],"\033[0;0m\n","----------------------------"],(*\:5b9a\:4e49\:7ec8\:7aef\:7684\:6253\:5370\:51fd\:6570*)
echo[x__]:=Print[x](*\:5b9a\:4e49\:7b14\:8bb0\:672c\:7684\:6253\:5370\:51fd\:6570*)
]
(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c\:ff0c\:5c31\:5237\:65b0\:7b14\:8bb0\:672c\:7684\:540d\:5b57*)
Once[If[
(* if $ScriptCommandLine==={}, the environment is frontend*)
Not[cmdQ],
(*if execute in the frontend mode, refresh the title name*)
CompoundExpression[
cellTitle=(Cells[][[1]]),(*\:5355\:5143\:5bf9\:8c61,\:7b2c\:4e00\:4e2a\:5355\:5143*)
NotebookWrite[cellTitle,Cell[FileNameSplit[fileName][[-1]],"Title"]](*\:5237\:65b0\:7b2c\:4e00\:4e2a\:5355\:5143\:7684\:540d\:5b57*)
]
]];
If[cmdQ,echo["Ready to execute this script"]](*\:5982\:679c\:5728\:547d\:4ee4\:884c\:6267\:884c\:ff0c\:5c31\:6253\:5370\:63d0\:793a\:4fe1\:606f*)
(*\:5b9a\:4e49\:672c\:5730git\:76ee\:5f55\:ff0c\:4e5f\:5c31\:662f\:7a0b\:5e8f\:7684\:6839\:76ee\:5f55*)
echo["the gitLocalName is"];
gitLocalName=FileNameJoin[Append[TakeWhile[FileNameSplit[ExpandFileName[fileName]],UnsameQ[#1,gitRemoteName]&],gitRemoteName]]


(* ::Chapter:: *)
(*chpt coe read*)


Get[FileNameJoin[{gitLocalName,"coe_channel.wl"}]];(*\:8bfb\:5165\:50a8\:5b58\:9876\:70b9\:7cfb\:6570\:7684\:6587\:4ef6*)


(* ::Chapter:: *)
(*quarkflow*)


(* ::Section:: *)
(*quark components*)


(*\:8d39\:66fc\:56fe\:4e2d\:5165\:5c04\:7684\:5938\:514b\:7ec4\:6210,mes,quench*)
tofqdKey["qch"][x_]:=fqdKey["qch",x]
tofqd["qch"][x__]:=fqd[x]
fqdData["qch","mes"]=Association@KeyValueMap[(#1/.qwKey->tofqdKey["qch"])->(#2/.qwave->tofqd["qch"])&]@qwData["mes"];
(*\:8d39\:66fc\:56fe\:4e2d\:5165\:5c04\:7684\:5938\:514b\:7ec4\:6210,mes,sea*)
tofqdKey["sea"][x_]:=fqdKey["sea",x]
tofqd["sea"][x__]:=fqd[x]
fqdData["sea","mes"]=Association@KeyValueMap[(#1/.qwKey->tofqdKey["sea"])->(#2/.qwave->tofqd["sea"])&]@qwData["mes"];


(*\:8d39\:66fc\:56fe\:4e2d\:5165\:5c04\:7684\:5938\:514b\:7ec4\:6210,oct,quench*)
tofqdKey["qch"][x_]:=fqdKey["qch",x]
tofqd["qch"][x__]:=fqd[x]
fqdData["qch","oct"]=Association@KeyValueMap[(#1/.qwKey->tofqdKey["qch"])->(#2/.qwave->tofqd["qch"])&]@qwData["oct"];
(*\:8d39\:66fc\:56fe\:4e2d\:5165\:5c04\:7684\:5938\:514b\:7ec4\:6210,oct,sea*)
tofqdKey["sea"][x_]:=fqdKey["sea",x]
tofqd["sea"][x_,y_,z_]:=fqd[x,Sequence@@Sort[{y,z}]]
fqdData["sea","oct"]=Association@KeyValueMap[(#1/.qwKey->tofqdKey["sea"])->DeleteDuplicates[#2/.qwave->tofqd["sea"]]&]@qwData["oct"];


(* ::Section:: *)
(*RB mes,sea and quench*)


(*\:751f\:6210\:5938\:514b\:56fe\:4e2d\:6240\:6709\:53ef\:80fd\:7684\:5938\:514b\:914d\:7f6e, sea*)
fyTag={"RB","mes"};qchTp1="sea";qchTp2="qch";
tmpoct=fdTypeOct;(*\:5165\:5c04oct\:7684Key*)
tmpmes=fyVtx2@fdTypeMes;(*\:4e2d\:95f4\:4ecb\:5b50\:7684Key*)
(*+++++++++++++++++++++++++++*)
coeJoin[fyTag,{qchTp1,"all"}]=Query[All,Append[#,
fqdKey[fyTag,qchTp1]-><|
fqdpos[1,2,3]->fqdData[qchTp1,"oct"]@fqdKey[qchTp1,#@tmpoct](*oct\:6240\:6709\:7684\:5938\:514b\:7ec4\:5408*),
fqdpos[4,5]->fqdData[qchTp1,"mes"]@fqdKey[qchTp1,#@tmpmes](*mes\:6240\:6709\:7684\:5938\:514b\:7ec4\:5408*)
|>
]&
]@coeJoin[fyTag];
(*++++++++++++++++++++++++++++++++++++++++++++*)
(*\:7b5b\:9009\:51fa\:53ef\:80fd\:7684\:5938\:514b\:914d\:7f6e\:7684\:51fd\:6570,sea*)
connect[qchTp1][x_]:=Module[{qua123,qua45,qua123vld,chOct,chOctb,chMes,quarkTag},
qua123=x[fqdKey[fyTag,qchTp1]][fqdpos[1,2,3]];(*\:63d0\:53d6\:51fa123\:4f4d\:7f6e\:4e0a\:7684\:5938\:514b, fqdList*)
qua45=x[fqdKey[fyTag,qchTp1]][fqdpos[4,5]];(*\:63d0\:53d6\:51fa45\:4f4d\:7f6e\:4e0a\:7684\:5938\:514b\:ff0cfqdList*)
chOct=x@fdTypeOct;chOctb=x@fdTypeOctb;chMes=x@fyVtx2@fdTypeMes;(*\:5b57\:6bb5\:4e2d\:7684 oct,octb, mes *)
(*Sequence@@*)DeleteCases[
<|
(*\:8fd9\:91cc, \:67d0\:79cd\:4ecb\:5b50\:5bf9\:5e94\:7684quarkflow\:56fe,\:5938\:514b4\:662f\:786e\:5b9a\:7684\:ff0c\:53ea\:5bf9\:5e94\:4e00\:79cd\:5938\:514b123\:ff0c\:56e0\:6b64\:4fdd\:8bc1\:4e86\:7a0b\:5e8f\:4e0d\:51fa\:9519*)
qua123vld=First[Cases[qua123,fqd[First[#],fd__]],fqd["mis","mis","mis"]];(*\:6311\:9009\:51fa\:5339\:914d\:5938\:514b4\:7684\:5938\:514b123*)
quarkTag=fqdTag[qua123vld,#(*\:5938\:514b4,5*),qchTp1,{chMes,#}];(*\:4f4d\:7f6e1..5\:7684\:5938\:514b\:914d\:7f6e,\:4ecb\:5b50tag\:53ef\:4ee5\:533a\:5206 \[Pi]0,\[Eta]8\:7684\:60c5\:5f62, \:52a0\:4e0a\:5938\:514b45tag\:65b9\:4fbf\:5206\:914d\:7b80\:5e76\:7cfb\:6570*)
chTagKey["in"]->chTag[chOct], (*chpt \:5165\:5c04\:7c92\:5b50,\:65b9\:4fbf\:5bf9quarkflow\:5206\:7ec4*)
chTagKey[fyTag]->chTag[chOct,chOctb,chMes],(*chpt\:56fe\:7684tag\:ff0c\:65b9\:4fbf\:5bf9quarkflow\:5206\:7ec4*)
fqdpos[1,5]->quarkTag,(*\:4f4d\:7f6e1..5\:7684\:5938\:514b*)
fyCoeKeycStr->x@fyCoeKeycStr,(*\:5f3a\:76f8\:4e92\:4f5c\:7528\:7cfb\:6570*)
fqdpos[1,2,3]->fqdTag[qua123vld,qchTp1],(*\:4f4d\:7f6e123\:7684\:5938\:514b\:914d\:7f6e,\:65b9\:4fbf\:8054\:7acb sea \:56fe\:7684\:76f8\:7b49\:5173\:7cfb, \:4e0d\:9700\:8981 mes Tag*)
fqdpos[6,7,8]->fqdTag[fqd@@Flatten[{#[[2]]/.ToQuark(*\:5938\:514bbar\[Rule]\:5938\:514b*),qua123vld[[2;;3]]},Infinity,fqd],qchTp1,chMes](*\:538b\:5e73\:5d4c\:5957\:7684fqd*)
|>&/@qua45,(*Map \:4ecb\:5b50\:53ef\:80fd\:7684\:5938\:514b\:7ec4\:5408 *)
<|__,fqdpos[1,5]->fqdTag[fqd["mis","mis","mis"],__],__|>(*\:5220\:9664 Miss[] \:7c7b\:578b\:ff0c\:4e5f\:5c31\:662f123\:548c45\:4e0d\:5339\:914d\:7684\:60c5\:51b5*)
]/.fqdList->Sequence
]
(*+\:7b5b\:9009\:51fa\:53ef\:80fd\:7684\:5938\:514b\:6d41\:914d\:7f6e++++++++++++++++++*)
coeJoin[fyTag,{qchTp1,"poss"}]=Query[Sort,KeyDrop[fqdpos[6,7,8]](*\:53bb\:6389\:5197\:4f59\:7684678\:5938\:514b*)
]@Query[All,connect[qchTp1]]@coeJoin[fyTag,{qchTp1,"all"}];
(*\:6784\:9020\:7684\:5176\:4ed6\:7ed3\:6784
coeJoin[fyTag,{qchTp1,"poss"}]=Query[All,<|
fdTypeOct\[Rule]Key@fdTypeOct,fdTypeOctb\[Rule]Key@fdTypeOctb,
fdTypeMes\[Rule]Key@fyVtx2@fdTypeMes,
fyCoeKey["cStr"]\[Rule]Key@fyCoeKey["cStr"],
fqdKey[fyTag,qchTp1]\[Rule]connect[qchTp1]|>
]@coeJoin[fyTag,{qchTp1,"all"}];
*)


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*fyTag={"RB","mes"};qchTp1="sea";qchTp2="qch";*)
(*Query[Cases[KeyValuePattern[chTagKey["in"]->chTag[fd[2, 1, 0]]]]]@coeJoin[fyTag,{qchTp1,"poss"}]//dsetFmt*)


(*\:7ee7\:7eed\:6dfb\:52a0\:5938\:514b\:56fe\:4e2d\:6240\:6709\:53ef\:80fd\:7684\:5938\:514b\:914d\:7f6e, quench\:56fe*)
fyTag={"RB","mes"};qchTp1="sea";qchTp2="qch";
tmpoct=fdTypeOct;(*\:5165\:5c04oct\:7684Key*)
tmpmes=fyVtx2@fdTypeMes;(*\:4e2d\:95f4\:4ecb\:5b50\:7684Key*)
(*+++++++++++++++++++++++++++*)
coeJoin[fyTag,{qchTp2,"all"}]=Query[All,Append[#,
fqdKey[fyTag,qchTp2]-><|
fqdpos[1,2,3]->fqdData[qchTp2,"oct"][fqdKey[qchTp2,#[tmpoct]]](*oct\:6240\:6709\:7684\:5938\:514b\:7ec4\:5408*),
fqdpos[4,5]->fqdData[qchTp2,"mes"][fqdKey[qchTp2,#[tmpmes]]](*mes\:6240\:6709\:7684\:5938\:514b\:7ec4\:5408*)
|>
]&
]@coeJoin[fyTag];
(*++++++++++++++++++++++++++++++++++++++++++++*)
(*\:7b5b\:9009\:51fa\:6240\:6709\:53ef\:80fd\:7684\:914d\:7f6e\:7684\:51fd\:6570, qch*)
(*quaoctToMes[fd[x__,0],fd[y__,0]]:=fqd[fd[x,0],fd[y,1]](*\:5c06\:5938\:514b2\:53d8\:6210\:53cd\:5938\:514b*)*)
quaMesToOct[fd[x__,0],fd[y__,1]]:=fqd[fd[x,0],fd[y,0],fd_](*\:5c06 mes \:53d8\:6210 oct, \:5373\:5c06\:5938\:514b2\:4ece\:53cd\:53d8\:5230\:6b63, \:5e76\:8ffd\:52a0 fd_ \:6a21\:5f0f*)
connect[qchTp2][x_]:=Module[{qua123,qua45,qua123vld,chOct,chOctb,chMes,quarkTag},
qua123=x[fqdKey[fyTag,qchTp2]][fqdpos[1,2,3]];(*\:63d0\:53d6\:51fa123\:4f4d\:7f6e\:4e0a\:7684\:5938\:514b, fqdList*)
qua45=x[fqdKey[fyTag,qchTp2]][fqdpos[4,5]];(*\:63d0\:53d6\:51fa45\:4f4d\:7f6e\:4e0a\:7684\:5938\:514b\:ff0cfqdList*)
chOct=x@fdTypeOct;chOctb=x@fdTypeOctb;chMes=x@fyVtx2@fdTypeMes;(*\:5b57\:6bb5\:4e2d\:7684 oct,octb, mes *)
DeleteCases[
<|
qua123vld=First[Cases[(*\:6311\:9009\:51fa\:5339\:914d\:5938\:514b4\:7684\:5938\:514b123*)
DeleteCases[qua123,fqd[a_fd,b_fd,b_fd]],(* \:5220\:9664\:4e0d\:5b58\:5728\:7684 qqq \:4e2d\:95f4\:6001,\:8fd9\:91cc\:5e94\:8be5\:4e0d\:4f1a\:5269\:4e0b\:7a7a\:96c6*)
#/.fqd->quaMesToOct
],
fqd["mis","mis","mis"]
];
quarkTag=fqdTag[qua123vld,#(*\:5938\:514b4,5*),qchTp2,{chMes,#}];(*\:4f4d\:7f6e1..5\:7684\:5938\:514b\:914d\:7f6e,\:4ecb\:5b50tag \:53ef\:4ee5\:533a\:5206 \[Pi]0,\[Eta]8\:7684\:60c5\:5f62*)
chTagKey["in"]->chTag[chOct],(*chpt \:5165\:5c04\:7c92\:5b50,\:65b9\:4fbf\:5bf9quarkflow\:5206\:7ec4*)
chTagKey[fyTag]->chTag[chOct,chOctb,chMes],(*chpt\:56fe\:7684tag\:ff0c\:65b9\:4fbf\:5bf9quarkflow\:5206\:7ec4*)
fqdpos[1,5]->quarkTag,(*\:4f4d\:7f6e1..5\:7684\:5938\:514b*)
fyCoeKeycStr->x@fyCoeKeycStr,(*\:5f3a\:76f8\:4e92\:4f5c\:7528\:7cfb\:6570*)
fqdpos[1,2,3]->fqdTag[qua123vld,qchTp2],(*\:4f4d\:7f6e123\:7684\:5938\:514b\:914d\:7f6e*)
fqdpos[6,7,8]->fqdTag[qua123vld[[{2,2,3}]],qchTp2,chMes]
|>&/@qua45,(*Map \:4ecb\:5b50\:53ef\:80fd\:7684\:5938\:514b\:7ec4\:5408 *)
<|__,fqdpos[1,5]->fqdTag[fqd["mis","mis","mis"],__],__|>(*\:5220\:9664 Miss[] \:7c7b\:578b\:ff0c\:4e5f\:5c31\:662f123\:548c45\:4e0d\:5339\:914d\:7684\:60c5\:51b5*)
]/.fqdList->Sequence
]
(*++++++++++++++++++\:7b5b\:9009\:51fa\:53ef\:80fd\:7684\:5938\:514b\:6d41\:914d\:7f6e++++++++++++++++++*)
coeJoin[fyTag,{qchTp2,"poss"}]=Query[Sort,KeyDrop[fqdpos[6,7,8]](*\:53bb\:6389\:5197\:4f59\:7684678\:5938\:514b*)
]@Query[All,connect[qchTp2]]@coeJoin[fyTag,{qchTp2,"all"}];
(*++++++++++++++++++\:6c47\:603b seq quench \:4e24\:8005\:60c5\:51b5\:7684\:5938\:514b\:56fe++++++++++++++++++*)
coeJoin[fyTag,{qchTp1,qchTp2,"poss"}]=Query[Sort]@Join[coeJoin[fyTag,{qchTp1,"poss"}],coeJoin[fyTag,{qchTp2,"poss"}]];


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*fyTag={"RB","mes"};qchTp1="sea";qchTp2="qch";*)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*fyTag={"RB","mes"};qchTp1="sea";qchTp2="qch";*)
(*Query[Cases[KeyValuePattern[chTagKey["in"]->chTag[fd[2,1,0]]]]]@coeJoin[fyTag,{qchTp2,"poss"}]//dsetFmt*)


(*quarkflow \:6c42\:548c\:5173\:7cfb,sea+quench \[Equal] chpt *)
fyTag={"RB","mes"};qchTp1="sea";qchTp2="qch";sumeq={qchTp1,qchTp2,"sumeq"};
(*-----------------------------*)
coeJoin[fyTag,sumeq]=Query[
GroupBy[#@chTagKey["in"]&],(*GroupBy\:662f\:4e0b\:964d\:7b97\:7b26*)
GroupBy[#@chTagKey[fyTag]&],
{Key@fyCoeKey["cStr"],Key@fqdpos[1, 5]}@*Merge[Union](*\:5408\:5e76\:76f8\:540c\:7684\:952e\:ff0c\:628a\:540c\:7c7b\:7684\:5938\:514b\:56fe\:805a\:96c6\:5728\:4e00\:8d77*)
]@coeJoin[fyTag,{qchTp1,qchTp2,"poss"}];
(*+++++++++++++++++++++++++++++++++++++++++\:4e3a\:7b80\:5e76\:7684\:5938\:514b\:56fe\:51c6\:5907\:7684\:7cfb\:6570,\:53bb\:7b80\:5e76+++++++++++++++++++++++++++++++++++++++++*)
(*---------------\[Pi]0---------------*)
fqdTagDD[x_fqd,y_fqd,z_,{fd[1,2,0],fqd[fd[4,1,0],fd[4,1,1]]}]:=1/2 fqdTagSum[x,y,z]
fqdTagDD[x_fqd,y_fqd,z_,{fd[1,2,0],fqd[fd[4,2,0],fd[4,2,1]]}]:=1/2 fqdTagSum[x,y,z]
(*---------------\[Eta]8---------------*)
fqdTagDD[x_fqd,y_fqd,z_,{fd[1,8,0],fqd[fd[4,1,0],fd[4,1,1]]}]:=1/6 fqdTagSum[x,y,z]
fqdTagDD[x_fqd,y_fqd,z_,{fd[1,8,0],fqd[fd[4,2,0],fd[4,2,1]]}]:=1/6 fqdTagSum[x,y,z]
fqdTagDD[x_fqd,y_fqd,z_,{fd[1,8,0],fqd[fd[4,3,0],fd[4,3,1]]}]:=2/3 fqdTagSum[x,y,z]
(*---------------\[Eta]0---------------*)
fqdTagDD[x_fqd,y_fqd,z_,{fd[1,0,0],fqd[fd[4,1,0],fd[4,1,1]]}]:=1/3 fqdTagSum[x,y,z]
fqdTagDD[x_fqd,y_fqd,z_,{fd[1,0,0],fqd[fd[4,2,0],fd[4,2,1]]}]:=1/3 fqdTagSum[x,y,z]
fqdTagDD[x_fqd,y_fqd,z_,{fd[1,0,0],fqd[fd[4,3,0],fd[4,3,1]]}]:=2/3 fqdTagSum[x,y,z]
(*--------------\:4e00\:822c\:7684\:60c5\:51b5\:ff0c\:4e00\:822c\:89c4\:5219\:5728\:7279\:6b8a\:89c4\:5219\:4e4b\:540e\:4f7f\:7528------------*)
fqdTagDD[x_fqd,y_fqd,z_,w_]:=fqdTagSum[x,y,z]


(*quarkflow sea \:56fe\:7684\:76f8\:7b49\:65b9\:7a0b*)
Query[
GroupBy[#@chTagKey["in"]&],(*GroupBy\:662f\:4e0b\:964d\:7b97\:7b26*)
GroupBy[#@fqdpos[1,2,3]&],
Merge[Union]
]@coeJoin[fyTag,{qchTp1,"poss"}];


(* ::Chapter:: *)
(*saveas*)


(* ::Input:: *)
(*FrontEndExecute[FrontEndToken[FrontEnd`EvaluationNotebook[], "Save", {StringTrim[NotebookFileName[],".nb"~~EndOfString]<>".wl", "Package"}]]*)
