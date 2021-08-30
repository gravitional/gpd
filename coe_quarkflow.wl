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
(*RB mes,sea,quench*)


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
connect[qchTp1][x_]:=Module[{qua123,qua45,sel123,chOct},
qua123=x[fqdKey[fyTag,qchTp1]][fqdpos[1,2,3]];(*\:63d0\:53d6\:51fa123\:4f4d\:7f6e\:4e0a\:7684\:5938\:514b, fqdList*)
qua45=x[fqdKey[fyTag,qchTp1]][fqdpos[4,5]];(*\:63d0\:53d6\:51fa456\:4f4d\:7f6e\:4e0a\:7684\:5938\:514b\:ff0cfqdList*)
(*Sequence@@*)DeleteCases[
<|
(*\:8fd9\:91cc\:4e00\:79cd\:4ecb\:5b50\:5bf9\:5e94\:7684\:4f4d\:7f6e4\:7684\:5938\:514b\:662f\:786e\:5b9a\:7684\:ff0c\:5bf9\:5e94\:7684123\:4f4d\:7f6e\:53ea\:6709\:4e00\:79cd\:72ec\:7acb\:7684\:5938\:514b\:914d\:7f6e\:ff0c\:4fdd\:8bc1\:4e86\:7a0b\:5e8f\:4e0d\:51fa\:9519*)
chOct=x@fdTypeOct;
sel123=First[Cases[qua123,fqd[First[#],fd__]],fqd["mis","mis","mis"]];(*\:7528\:4f4d\:7f6e1\:4e0a\:7684\:5938\:514b\:5339\:914d\:4f4d\:7f6e4\:4e0a\:7684\:5938\:514b*)
chTagKey["in"]->chTag[chOct],
chTagKey[fyTag]->chTag[chOct,x@fdTypeOctb,x@fyVtx2@fdTypeMes],
fyCoeKey["cStr"]->x@fyCoeKey["cStr"],
fqdpos[1,2,3]->fqdTag[sel123,qchTp1,chOct],
fqdpos[1,5]->fqdTag[sel123,#(*\:5938\:514b4,5*),qchTp1,chOct],
fqdpos[6,7,8]->fqdTag[fqd@@Flatten[{#[[2]]/.ToQuark(*\:5938\:514bbar\[Rule]\:5938\:514b*),sel123[[2;;3]]},Infinity,fqd],qchTp1,chOct](*\:538b\:5e73\:5d4c\:5957\:7684fqd*)
|>&/@qua45,(*Map \:4ecb\:5b50\:53ef\:80fd\:7684\:5938\:514b\:7ec4\:5408 *)
(*\:5220\:9664 Miss[] \:7c7b\:578b\:ff0c\:4e5f\:5c31\:662f123\:548c45\:4e0d\:5339\:914d\:7684\:60c5\:51b5*)
<|__,fqdpos[1,5]->fqdTag[fqd["mis","mis","mis"],__],__|>
]/.fqdList->Sequence
]
(*+\:7b5b\:9009\:51fa\:53ef\:80fd\:7684\:5938\:514b\:6d41\:914d\:7f6e++++++++++++++++++*)
coeJoin[fyTag,{qchTp1,"poss"}]=Query[All,KeyDrop[fqdpos[6,7,8]](*\:53bb\:6389\:5197\:4f59\:7684678\:5938\:514b*)
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
quaAnti2[fd[x__,0],fd[y__,0]]:=fqd[fd[x,0],fd[y,1]](*\:5c06\:4f4d\:7f6e2\:4e0a\:7684\:5938\:514b\:53d8\:6210\:53cd\:5938\:514b*)
connect[qchTp2][x_]:=Module[{qua123,sel123,chOct},
qua123=DeleteCases[(*\:63d0\:53d6\:51fa123\:4f4d\:7f6e\:4e0a\:7684\:5938\:514b, \:5220\:9664\:516b\:91cd\:6001\:4e2d\:4e0d\:53ef\:80fd\:5b58\:5728\:7684 bbb \:4e2d\:95f4\:6001,\:8fd9\:91cc\:5e94\:8be5\:4e0d\:4f1a\:5269\:4e0b\:7a7a\:96c6*)
x[fqdKey[fyTag,qchTp2]][fqdpos[1,2,3]],fqd[a_fd,b_fd,b_fd]
];
(*qench \:60c5\:5f62\:5938\:514b\:56fe\:7531123\:4f4d\:7f6e\:7684\:5938\:514b\:5b8c\:5168\:51b3\:5b9a*)
(<|
chOct=x@fdTypeOct;
chTagKey["in"]->chTag[chOct],
chTagKey[fyTag]->chTag[x@fdTypeOct,x@fdTypeOctb,x@fyVtx2@fdTypeMes],
fyCoeKey["cStr"]->x@fyCoeKey["cStr"],
fqdpos[1,5]->fqdTag[#,#[[1;;2]]/.fqd->quaAnti2,(*4,5\:4f4d\:7f6e\:7684\:5938\:514b*)qchTp2,chOct],
fqdpos[6,7,8]->fqdTag[#[[{2,2,3}]],qchTp2,chOct]
|>&/@qua123)/.fqdList->Sequence
]
(*+\:7b5b\:9009\:51fa\:53ef\:80fd\:7684\:5938\:514b\:6d41\:914d\:7f6e++++++++++++++++++*)
coeJoin[fyTag,{qchTp2,"poss"}]=Query[All,KeyDrop[fqdpos[6,7,8]](*\:53bb\:6389\:5197\:4f59\:7684678\:5938\:514b*)
]@Query[All,connect[qchTp2]]@coeJoin[fyTag,{qchTp2,"all"}];


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*fyTag={"RB","mes"};qchTp1="sea";qchTp2="qch";*)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*fyTag={"RB","mes"};qchTp1="sea";qchTp2="qch";*)
(*Query[Cases[KeyValuePattern[chTagKey["in"]->chTag[fd[2,1,0]]]]]@coeJoin[fyTag,{qchTp2,"poss"}]//dsetFmt*)


fyTag={"RB","mes"};qchTp1="sea";qchTp2="qch";
coeJoin[fyTag,{qchTp1,qchTp2,"poss"}]=Join[(*\:6c47\:603b seq quench \:4e24\:8005\:60c5\:51b5\:7684\:5938\:514b\:56fe*)
coeJoin[fyTag,{qchTp1,"poss"}],coeJoin[fyTag,{qchTp2,"poss"}]
];


(* ::Section:: *)
(*GroupBy*)


(* ::Input:: *)
(*fyTag={"RB","mes"};qchTp1="sea";qchTp2="qch";*)
(*(* sea+quench \[Equal] total *)*)
(*tea=Query[Values,Values,All,Key@fqdpos[1,5]*)
(*]@Query[*)
(*GroupBy[#@chTagKey["in"]&],(*GroupBy\:662f\:4e0b\:964d\:7b97\:7b26*)*)
(*GroupBy[#@chTagKey[fyTag]&]*)
(*]@coeJoin[fyTag,{qchTp1,qchTp2,"poss"}];*)
(*fqdDepth=FirstPosition[tea,_fqd]//Length;*)
(*teb=Apply[Plus,tea,{fqdDepth-2}];*)


(* ::Input:: *)
(*(*sea \:7684\:76f8\:7b49\:65b9\:7a0b*)*)
(*Query[All,All,eqAll,Key@fqdpos[1,5]*)
(*]@Query[*)
(*GroupBy[#@chTagKey["in"]&],(*GroupBy\:662f\:4e0b\:964d\:7b97\:7b26*)*)
(*GroupBy[#@fqdpos[1, 2, 3]&]*)
(*]@coeJoin[fyTag,{qchTp1,"poss"}]*)


(* ::Chapter:: *)
(*Saveas*)


(* ::Input:: *)
(*FrontEndExecute[{FrontEndToken[FrontEnd`EvaluationNotebook[],"SaveRename"]}];*)
