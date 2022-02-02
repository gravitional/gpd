(* ::Package:: *)

(* ::Text:: *)
(*Rainbow \:7c7b\:578b\:56fe\:7684 quark flow \:8ba1\:7b97\:811a\:672c*)


(*\[Pi]0,\[Eta]0,\[Eta]8 \:4ecb\:5b50 \:7684 chpt \:56fe\:5bf9\:5e94\:76f8\:540c\:7684 quarkflow \:56fe*)
mesNeutRule={ff["\[Eta]0"]->ff["\[Pi]\[Eta]"],ff["\[Pi]0"]->ff["\[Pi]\[Eta]"],ff["\[Eta]8"]->ff["\[Pi]\[Eta]"]};
(*\[CapitalSigma]0b,\[CapitalLambda]b \:91cd\:5b50 chpt \:56fe\:5bf9\:5e94\:76f8\:540c\:7684 quarkflow \:56fe*)
octbNeutRule={ff["\[CapitalSigma]0b"]->ff["\[CapitalSigma]0b\[CapitalLambda]b"],ff["\[CapitalLambda]b"]->ff["\[CapitalSigma]0b\[CapitalLambda]b"]};


(*+++++++++++++++++++++++++++ Key \:521d\:59cb\:5316+++++++++++++++++++++++++++*)
tmpoct=inOct;(*\:9876\:70b91,oct,\:5165\:5c04\:573a*)
tmpmes=medMes1;(*\:9876\:70b92,mes,\:5165\:5c04\:573a*)
(*\:751f\:6210\:5938\:514b\:56fe\:4e2d\:6240\:6709\:53ef\:80fd\:7684\:5938\:514b\:914d\:7f6e, sea ++++++++++++++++++++++++++++++++*)
coeJoin[{fyTag,qchTp1,"all"}]=Query[(*{channels}*)All
(*<channel details>*),Append[#,
fqdKey[fyTag,qchTp1]-><|
fqdpos[1,2,3]->fqdData[qchTp1,"oct"]@fqdKey[qchTp1,#@tmpoct](*oct\:6240\:6709\:7684\:5938\:514b\:7ec4\:5408*),
fqdpos[4,5]->fqdData[qchTp1,"mes"]@fqdKey[qchTp1,#@tmpmes](*mes\:6240\:6709\:7684\:5938\:514b\:7ec4\:5408*)
|>
]&
]@coeJoin[fyTag];
(*++++++++++++++++++++++++++++++++++++++++++++*)
(*\:7b5b\:9009\:51fa\:53ef\:80fd\:7684\:5938\:514b\:914d\:7f6e\:7684\:51fd\:6570,sea*)
connect[qchTp1][x_]:=Module[{qua123,qua45,qua123vld,chOct,chOctb,chMes,chptTag},
qua123=x[fqdKey[fyTag,qchTp1]][fqdpos[1,2,3]];(*\:63d0\:53d6\:51fa123\:4f4d\:7f6e\:4e0a\:7684\:5938\:514b, fqdList*)
qua45=x[fqdKey[fyTag,qchTp1]][fqdpos[4,5]];(*\:63d0\:53d6\:51fa45\:4f4d\:7f6e\:4e0a\:7684\:5938\:514b\:ff0cfqdList*)
(* --------------- \:5b57\:6bb5\:4e2d\:7684 oct,octb, mes --------------- *)
chOct=x@inOct;(*\:5165\:5c04 oct*)
chOctb=x@medOct1;(*\:4e2d\:95f4 octb*)
(*\:4e2d\:95f4 mes; \:5c06\[Pi]0,\[Eta]0,\[Eta]8,\:6620\:5c04\:5230\:540c\:6837\:7684\:7c92\:5b50 ----------------- *)
chMes=x@medMes1/.mesNeutRule;
(*chpt\:56fe\:7684tag\:ff0c\:65b9\:4fbf\:5bf9quarkflow\:5206\:7ec4;--------------------*)
chptTag=chTag[chOct,chMes,chOctb];
(*\:4ea7\:751f\:5408\:9002\:7684\:6570\:636e\:96c6 ----------------------*)
DeleteCases[
<|
(*\:8fd9\:91cc, \:67d0\:79cd\:4ecb\:5b50\:5bf9\:5e94\:7684quarkflow\:56fe,\:5938\:514b4\:662f\:786e\:5b9a\:7684\:ff0c\:53ea\:5bf9\:5e94\:4e00\:79cd\:5938\:514b123\:ff0c\:56e0\:6b64\:4fdd\:8bc1\:4e86\:7a0b\:5e8f\:4e0d\:51fa\:9519*)
(*\:6311\:9009\:51fa\:5339\:914d\:5938\:514b4\:7684\:5938\:514b123*)
qua123vld=First[Cases[qua123,fqd[First@#,fd__]],fqd["mis","mis","mis"]];
(*chpt \:5165\:5c04\:7c92\:5b50,\:65b9\:4fbf\:5bf9quarkflow\:5206\:7ec4*)
chTagKey["in"]->chTag[chOct]
(*chpt\:56fe\:7684tag\:ff0c\:65b9\:4fbf\:5bf9quarkflow\:5206\:7ec4,\:628a \[Pi]0,\[Eta]0,\[Eta]8 \:5206\:5230\:540c\:4e00\:7ec4, \[CapitalSigma]0b,\[CapitalLambda]b\:5206\:5230\:540c\:4e00\:7ec4*)
,chTagKey[fyTag]->chptTag
(*\:4f4d\:7f6e1..5\:7684\:5938\:514b; # \:662f \:5938\:514b4,5 ; qua123vld \:662f \:5938\:514b123;\:5938\:514b 4,5\:653e\:5728\:524d\:9762\:662f\:4e3a\:4e86\:65b9\:4fbf\:540e\:9762\:7684\:6392\:5e8f, \:5904\:7406 \[CapitalSigma]0, \[CapitalLambda] \:7b80\:5e76\:7684\:60c5\:5f62*)
,fqdpos[1,5]->fqdTag[qchTp1][#,qua123vld]
(*\:4e3a\:533a\:5206\:7b80\:5e76\:5230\:76f8\:540c quarkflw \:4e2d\:7684 chpt \:56fe*)
,fqdChpt[1,5,chpt]->fqdTag[qchTp1][#,qua123vld,chptTag]
(*\:5f3a\:76f8\:4e92\:4f5c\:7528\:7cfb\:6570, chpt Tag \:4fdd\:7559\:4e0d\:540c\:4ecb\:5b50\:7684\:8d21\:732e,\:540c\:65f6\:65b9\:4fbf\:6392\:5e8f*)
,fyCoeKeycStr-><|chptTag->x@fyCoeKeycStr|>
(*\:4f4d\:7f6e123\:7684\:5938\:514b\:914d\:7f6e,\:65b9\:4fbf\:8054\:7acb sea \:56fe\:7684\:76f8\:7b49\:5173\:7cfb, \:4e0d\:9700\:8981 mes Tag*)
,fqdpos[1,2,3]->fqdTag[qua123vld,qchTp1]
,fqdpos[6,7,8]->fqdTag[fqd@@Flatten[{#[[2]]/.quarkRule["bar->reg"](*\:5938\:514bbar\[Rule]\:5938\:514b*),qua123vld[[2;;3]]},Infinity,fqd],qchTp1,chOct](*\:538b\:5e73\:5d4c\:5957\:7684fqd*)
|>&/@qua45,(*Map \:4ecb\:5b50\:53ef\:80fd\:7684\:5938\:514b\:7ec4\:5408 *)
KeyValuePattern[fqdpos[1,5]->fqdTag[qchTp1][ __,fqd["mis","mis","mis"] ]](*\:5220\:9664 Miss \:7c7b\:578b\:ff0c\:4e5f\:5c31\:662f123\:548c45\:4e0d\:5339\:914d\:7684\:60c5\:51b5*)
]/.fqdList->Sequence
]
(*+\:7b5b\:9009\:51fa\:53ef\:80fd\:7684\:5938\:514b\:6d41\:914d\:7f6e++++++++++++++++++*)
coeJoin[{fyTag,qchTp1,"poss"}]=Query[Sort,KeyDrop[fqdpos[6,7,8]](*\:53bb\:6389\:5197\:4f59\:7684678\:5938\:514b*)
]@Query[All,connect[qchTp1]]@coeJoin[{fyTag,qchTp1,"all"}];


(*\:7ee7\:7eed\:6dfb\:52a0\:5938\:514b\:56fe\:4e2d\:6240\:6709\:53ef\:80fd\:7684\:5938\:514b\:914d\:7f6e, quench\:56fe*)
tmpoct=inOct;(*\:9876\:70b91,oct,\:5165\:5c04\:573a*)
tmpmes=medMes1;(*\:9876\:70b92,mes,\:5165\:5c04\:573a*)
(*+++++++++++++++++++++++++++*)
coeJoin[{fyTag,qchTp2,"all"}]=Query[(*{channels}*)All
(*<channel details>*),Append[#,
fqdKey[fyTag,qchTp2]-><|
fqdpos[1,2,3]->fqdData[qchTp2,"oct"]@fqdKey[qchTp2,#@tmpoct](*oct\:6240\:6709\:7684\:5938\:514b\:7ec4\:5408*),
fqdpos[4,5]->fqdData[qchTp2,"mes"]@fqdKey[qchTp2,#@tmpmes](*mes\:6240\:6709\:7684\:5938\:514b\:7ec4\:5408*)
|>
]&
]@coeJoin[fyTag];
(*++++++++++++++++++++++++++++++++++++++++++++*)
(*\:7b5b\:9009\:51fa\:6240\:6709\:53ef\:80fd\:7684\:914d\:7f6e\:7684\:51fd\:6570, qch*)
quaMesToOct[fd[x__,0],fd[y__,1]]:=fqd[fd[x,0],fd[y,0],fd_](*\:5c06 mes \:53d8\:6210 oct, \:5373\:5c06\:5938\:514b2\:4ece\:53cd\:53d8\:5230\:6b63, \:5e76\:8ffd\:52a0\:6a21\:5f0f fd_ *)
(* ----- ----- *)
connect[qchTp2][x_]:=Module[{qua123,qua45,qua123vld,chOct,chOctb,chMes,chptTag},
qua123=x[fqdKey[fyTag,qchTp2]]@fqdpos[1,2,3];(*\:63d0\:53d6\:51fa123\:4f4d\:7f6e\:4e0a\:7684\:5938\:514b, fqdList*)
qua45=x[fqdKey[fyTag,qchTp2]]@fqdpos[4,5];(*\:63d0\:53d6\:51fa45\:4f4d\:7f6e\:4e0a\:7684\:5938\:514b\:ff0cfqdList*)
(* --------------- \:5b57\:6bb5\:4e2d\:7684 oct,octb, mes --------------- *)
chOct=x@inOct;(*\:5165\:5c04 oct*)
chOctb=x@medOct1;(*\:4e2d\:95f4 octb*)
(*\:4e2d\:95f4 mes, \:5c06\[Pi]0,\[Eta]0,\[Eta]8,\:6620\:5c04\:5230\:540c\:6837\:7684\:7c92\:5b50 --------------- *)
chMes=x@medMes1/.mesNeutRule;
(*chpt\:56fe\:7684tag\:ff0c\:65b9\:4fbf\:5bf9quarkflow\:5206\:7ec4,\:628a \[Pi]0,\[Eta]0,\[Eta]8 \:5206\:5230\:540c\:4e00\:7ec4, \[CapitalSigma]0b,\[CapitalLambda]b\:5206\:5230\:540c\:4e00\:7ec4*)
chptTag=chTag[chOct,chMes,chOctb];
(*----------- \:4ea7\:751f\:5408\:9002\:7684\:6570\:636e\:96c6 --------------------*)
DeleteCases[<|
(*\:6311\:9009\:51fa\:5339\:914d\:5938\:514b4\:7684\:5938\:514b123;\:5220\:9664\:4e0d\:5b58\:5728\:7684 qqq \:4e2d\:95f4\:6001,\:8fd9\:91cc\:5e94\:8be5\:4e0d\:4f1a\:5269\:4e0b\:7a7a\:96c6*)
qua123vld=First[Cases[
DeleteCases[qua123,fqd[a_fd,b_fd,b_fd]],
#/.fqd->quaMesToOct],fqd["mis","mis","mis"]];
(*chpt \:5165\:5c04\:7c92\:5b50,\:65b9\:4fbf\:5bf9quarkflow\:5206\:7ec4----------------*)
chTagKey["in"]->chTag[chOct]
(*chpt \:5c42\:6b21\:7684 tag-------------------------*)
,chTagKey[fyTag]->chptTag
(*\:4f4d\:7f6e1..5\:7684\:5938\:514b; # \:662f \:5938\:514b4,5 ; qua123vld \:662f \:5938\:514b123;
\:5938\:514b 4,5\:653e\:5728\:524d\:9762\:662f\:4e3a\:4e86\:65b9\:4fbf\:540e\:9762\:7684\:6392\:5e8f, \:5904\:7406 \[CapitalSigma]0, \[CapitalLambda] \:7b80\:5e76\:7684\:60c5\:5f62*)
,fqdpos[1,5]->fqdTag[qchTp2][#,qua123vld]
(*\:4e3a\:533a\:5206\:7b80\:5e76\:5230\:76f8\:540c quarkflw \:4e2d\:7684 chpt \:56fe------------*)
,fqdChpt[1,5,chpt]->fqdTag[qchTp2][#,qua123vld,chptTag]
(*\:5f3a\:76f8\:4e92\:4f5c\:7528\:7cfb\:6570, chpt Tag \:4fdd\:7559\:4e0d\:540c\:4ecb\:5b50\:7684\:8d21\:732e,\:540c\:65f6\:65b9\:4fbf\:6392\:5e8f----------------*)
,fyCoeKeycStr-><|chptTag->x@fyCoeKeycStr|>
(*\:4f4d\:7f6e123\:7684\:5938\:514b\:914d\:7f6e*)
,fqdpos[1,2,3]->fqdTag[qua123vld,qchTp2]
,fqdpos[6,7,8]->fqdTag[qua123vld[[{2,2,3}]],qchTp2,chOct]
|>&/@qua45,(*Map \:4ecb\:5b50\:53ef\:80fd\:7684\:5938\:514b\:7ec4\:5408 *)
KeyValuePattern[fqdpos[1,5]->fqdTag[qchTp2][ __,fqd["mis","mis","mis"] ]](*\:5220\:9664 Miss \:7c7b\:578b\:ff0c\:4e5f\:5c31\:662f123\:548c45\:4e0d\:5339\:914d\:7684\:60c5\:51b5*)
]/.fqdList->Sequence
]
(*++++++++++++++++++\:7b5b\:9009\:51fa\:53ef\:80fd\:7684\:5938\:514b\:6d41\:914d\:7f6e++++++++++++++++++*)
coeJoin[{fyTag,qchTp2,"poss"}]=Query[Sort,KeyDrop[fqdpos[6,7,8]](*\:53bb\:6389\:5197\:4f59\:7684678\:5938\:514b*)
]@Query[All,connect[qchTp2]]@coeJoin[{fyTag,qchTp2,"all"}];
(*++++++++++++++++++\:6c47\:603b seq quench \:4e24\:8005\:60c5\:51b5\:7684\:5938\:514b\:56fe++++++++++++++++++*)
coeJoin[{fyTag,qchPoss}]=Query[Sort]@Join[coeJoin[{fyTag,qchTp1,"poss"}],coeJoin[{fyTag,qchTp2,"poss"}]];


(* ::Section:: *)
(*sum equations*)


(*quarkflow \:6c42\:548c\:5173\:7cfb, sea+quench \[Equal] chpt *)
(*++++ \:5904\:7406\:6536\:96c6\:8d77\:6765\:7684 quarkflow channel \:548c\:7cfb\:6570, \:5938\:514b\:9053\:53d6 Union,
Union \:5220\:9664\:91cd\:590d\:7cfb\:6570\:5e76\:6392\:5e8f, \:6ce8\:610f Union \:53ea\:4f5c\:7528\:4e8e\:5173\:8054\:7684 Value, \:6240\:4ee5\:8fd9\:91cc\:4e0d\:7528 Union +++*)
fqdTot2[quark_List,chpt_List]:=fqdTot3[Union[quark],
Values@KeySort@(*\:6309\:4ecb\:5b50\:51fa\:73b0\:987a\:5e8f\:6392\:5e8f*)
Merge[DeleteDuplicates@chpt,Total]];
(*\:5bf9\:4e8e \[CapitalSigma]0-\[CapitalLambda] \:4f7f\:7528\:9000\:7b80\:5e76\:6761\:4ef6\:ff0c\:5148\:4f7f\:7528\:7279\:6b8a\:89c4\:5219, \:5728 fqdTot2 \:4e2d, quarkflow \:5217\:8868\:5df2\:88ab\:6392\:5e8f *)
fqdTot3[qua:{
Repeated[fqdTag["sea"][__,chTag[ff["\[CapitalSigma]0"]|ff["\[CapitalLambda]"],ff["\[Pi]\[Eta]"],__]],{3}]},
chpt_List
]:=eqList["qf"]@@MapThread[fqdTot,{qua,{1/6,1/6,2/3}*First@chpt}]

fqdTot3[qua:{
Repeated[fqdTag["sea"][__,chTag[ff["\[CapitalSigma]0"]|ff["\[CapitalLambda]"],ff["\[Pi]\[Eta]"],__]],{2}]},
chpt_List
]:=eqList["qf"]@@MapThread[fqdTot,{qua,{1/2,1/2}*First@chpt}]

(* --------\:518d\:4f7f\:7528\:4e00\:822c\:89c4\:5219------------- *)
fqdTot3[qua_List,chpt_List]:=eqList["qf"][fqdTot[Total@qua,Total@chpt]]
(*------------quarkflow \:53cd\:5e94\:9053\:4e0e chpt \:7cfb\:6570\:7684\:6c42\:548c\:65b9\:5f0f------------*)
fqdTot[qua_,chpt_]:=qua-chpt==0


(* ++++++++++++++++++++++++++++++++++++ \:5efa\:7acb\:65b9\:7a0b\:7ec4,sea+quench \[Equal] chpt ++++++++++++++++++++++++++++++++++++ *)
coeJoin[{fyTag,totEq}]=Query[
(*{<channels>..};GroupBy \:662f\:4e0b\:964d\:7b97\:7b26, \:6309\:7167\:5165\:5c04\:7c92\:5b50\:5206\:7ec4*)
GroupBy[#@chTagKey["in"]&],
(*{<channels>..}; \:6309\:7167 chpt \:56fe\:7684 tag \:5206\:7ec4*)
GroupBy[#@chTagKey[fyTag]&],
(*{<channels>..}; ------------------------------------------*)
(*\:5408\:5e76\:5355\:4e2a chpt \:4e0b\:6240\:6709\:53ef\:80fd\:7684 quarkflow*)
Merge[Join]/*
(*\:53d6\:51fa chpt,quarkflow tag, \:8054\:7acb\:65b9\:7a0b*)
(fqdTot2[#@fqdChpt[1,5,chpt],#@fyCoeKeycStr]&)
]@coeJoin[{fyTag,qchPoss}];


(* ::Section:: *)
(*equivalent equations*)


(*quarkflow \:5bf9\:79f0\:6027\:5173\:7cfb\:ff0c\:76f8\:540c\:4ef7\:5938\:514b\:914d\:7f6e\:7684\:6d77\:5938\:514b\:56fe\:76f8\:7b49--------*)
(*quarkflow \:53cd\:5e94\:9053 \:7684\:5bf9\:79f0\:6027\:5173\:7cfb -------------------*)
fqdSym[{x_}]:=Nothing(*\:5220\:9664\:5355\:4e2a\:5143\:7d20\:81ea\:76f8\:7b49\:7684\:6052\:7b49\:5f0f*)
(*\:5c06\:6c42\:548c\:7684 quarkflow-chpt \:7528 \[Equal] \:8fde\:63a5\:8d77\:6765*)
fqdSym[{x__}]:=eqList["qf"][Equal[x]]


(* --------------------- \:5efa\:7acb symmetry \:65b9\:7a0b\:7ec4 --------------------- *)
coeJoin[{fyTag,symEq}]=Query[
(*{<channels>..};GroupBy \:662f\:4e0b\:964d\:7b97\:7b26, \:6309\:7167\:5165\:5c04\:7c92\:5b50\:5206\:7ec4*)
GroupBy[#@chTagKey["in"]&]
(*{<channels>..}; \:6309\:4f4d\:7f6e123\:7684 quark \:5206\:7ec4*)
,GroupBy[#@fqdpos[1,2,3]&]
(*{<channels>..}; \:6309\:4f4d\:7f6e 12345 \:7684 quark \:5206\:7ec4*)
,GroupBy[#@fqdpos[1,5]&]/*(*\:53d6 Group \:7684\:7ed3\:679c*)Values/*
(*\:5c06\:4e0b\:5c42\:6c42\:548c\:8fc7\:7684 quarkflow-chpt \:5217\:6210\:7b49\:5f0f*)fqdSym
(*{<channels>..}-----------------------------------------*)
(*\:5408\:5e76\:ff0cUnion \:5c06\:8fdb\:884c\:6392\:5e8f,\:5e76\:5220\:9664\:91cd\:590d\:5143\:7d20*)
,Merge[Union]/*
(*\:53d6\:51fa\:5e26 chpt-tag \:7684 quarkflow*)(Key@fqdChpt[1,5,chpt])/*
(*\:5c06\:5c5e\:4e8e\:540c\:4e00\:4e2a quarkflow \:7684 quarkflow-chpt \:6c42\:548c*)Total
]@coeJoin[{fyTag,qchTp1,"poss"}];


(* ++++++++++++++++++++++++++++++++++++ \:5408\:5e76\:4e24\:79cd\:65b9\:7a0b\:7ec4 ++++++++++++++++++++++++++++++++++++ *)
coeJoin[{fyTag,qfEqs}]=Merge[{
Query[All,Values]@coeJoin[{fyTag,symEq}],
Query[All,Values]@coeJoin[{fyTag,totEq}]
},
Catenate
];


(* --------------------- \:6c42\:89e3\:8054\:7acb\:7684\:65b9\:7a0b\:7ec4 --------------------- *)
coeJoin[{fyTag,quaFlow}]=Query[(*<oct>*)All
(*{equs}*)
,First@Solve[#/.eqList["qf"]->Sequence,
DeleteDuplicates@Cases[#,Blank@fqdTag["sea"]|Blank@fqdTag["qch"],
Infinity]]&
]@coeJoin[{fyTag,qfEqs}];


(* ::Section:: *)
(*check degree of freedom*)


(* ::Input:: *)
(*(*\:67e5\:770b sum \:65b9\:7a0b*)*)
(*Query[(*<oct>*){1},*)
(*Normal/*TableForm*)
(*]@coeJoin[{fyTag,totEq}]*)


(* ::Input:: *)
(*(*\:67e5\:770b equivalent \:65b9\:7a0b*)*)
(*Query[(*<oct>*){1},*)
(*Normal/*TableForm*)
(*]@coeJoin[{fyTag,symEq}]*)


(* ::Input:: *)
(*(*\:67e5\:770b\:72ec\:7acb\:7684 quarkflow \:6570\:76ee*)*)
(*Query[(*<oct>*){1}*)
(*(*{equs}*)*)
(*,(DeleteDuplicates@Cases[#,Blank@fqdTag["sea"]|Blank@fqdTag["qch"],*)
(*Infinity]&)/*TableForm*)
(*]@coeJoin[{fyTag,qfEqs}]*)


(* ::Input:: *)
(*(*\:67e5\:770b quarkflow \:6c42\:89e3\:7ed3\:679c*)*)
(*Query[(*<oct>*){4},*)
(*Normal/*TableForm*)
(*]@coeJoin[{fyTag,quaFlow}]*)
