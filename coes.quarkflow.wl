(* ::Package:: *)

(* ::Title:: *)
(*coes.quarkflow.wl*)


(* ::Chapter:: *)
(*initial*)


(*\:672c\:6587\:4ef6\:7684\:540d\:79f0*)
$fileName=If[$Notebooks,NotebookFileName[],$InputFileName];
(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c\:ff0c\:5c31\:5237\:65b0\:7b14\:8bb0\:672c\:7684\:6807\:9898*)
Once@If[$Notebooks,NotebookWrite[Cells[][[1]],Cell[Last@FileNameSplit[$fileName],"Title"]]];
(*\:67e5\:627e init.wl, \:5bfc\:5165\:6839\:76ee\:5f55\:548c\:51fd\:6570\:5b9a\:4e49.*)
Once@Catch@Module[{recurFind,start=1,depMax},
depMax=FileNameDepth[$fileName];(*\:8def\:5f84\:7684\:6700\:5927\:5c42\:6b21*)
(*-------\:5b9a\:4e49\:9012\:5f52\:51fd\:6570-------*)
recurFind[dep_Integer]:=If[dep<=depMax,
SetDirectory[DirectoryName[$fileName,dep]];(*SetDirectory[]:\:8bbe\:7f6e\:5de5\:4f5c\:76ee\:5f55\:4e3a\:5bb6\:76ee\:5f55*)
(*\:5982\:679c\:5728\:5f53\:524d\:5c42\:80fd\:627e\:5230 init.wl,\:5c31\:8fd0\:884c\:5b83,\:5e76\:628a\:6839\:76ee\:5f55\:6dfb\:52a0\:5230\:641c\:7d22\:8def\:5f84*)
If[FileExistsQ["init.wl"],
Get["init.wl"];PrependTo[$Path,$srcRoot];
Throw["The base directory is : "<>$srcRoot];,
(*\:5982\:679c\:8fd9\:4e00\:5c42\:627e\:4e0d\:5230\:ff0c\:5c31\:4e0a\:5347\:4e00\:5c42*)
recurFind[dep+1]];
ResetDirectory[];(*\:91cd\:8bbe\:4e3a\:4e4b\:524d\:7684\:76ee\:5f55*),
Throw["I cann't find any init.wl in this project"]
];
recurFind[start];
]
(* \:8bb0\:5f55 master Kernel \:7684\:8fd0\:884c\:6a21\:5f0f, \:53ef\:5728\:5e76\:884c\:8ba1\:7b97\:4e2d\:4f7f\:7528 *)
$inNBook=$Notebooks;echo[DateString[]," <<",$fileName];


(* ::Section:: *)
(*<< modules*)


(*\:5bfc\:5165\:4e00\:4e9b\:683c\:5f0f\:5316\:7684\:8bbe\:7f6e\:ff0c\:663e\:793a\:573a\:7684\:5e38\:7528\:5f62\:5f0f*)
Get["gen.format.wl"];
Get["gen.integral-TagList.wl"];
(*\:7c92\:5b50\:7c7b\:578b\:63a5\:53e3 --------------------------*)
Get["coes.interface.wl"];


coeJoin=Import@localPath["coes"]["coes.chpt.wdx"];


(* ::Section:: *)
(*format*)


(*\:5b9a\:4e49\:4e00\:4e2a\:786e\:4fdd\:5b57\:7b26\:4e32\:7684\:51fd\:6570,\:7528\:9017\:53f7\:9694\:5f00\:8f93\:5165*)
enStrRiff[x__]:=StringRiffle[ToString/@enList[x],","]
(*\:7528\:6765\:5c06 dataset \:4e2d\:7b2c\:4e8c\:5c42\:ff0c\:5373\:5173\:8054\:7684key\:5f3a\:5236\:6392\:7248\:4e3a\:5b57\:7b26\:4e32\:ff0c\:53ef\:4ee5\:8f83\:597d\:7684\:663e\:793a.*)
dsetFmt[x_]:=Dataset[x/.Association->assocTemp/.{
fdType->enStrRiff,vtxType->enStrRiff,
chTagKey->enStrRiff,fyCoeKey->enStrRiff,
fyVtx->enStrRiff,vtxCoe->Identity,fyCoe->Times,massV->Identity,
fqdKey->enStrRiff,fqdpos->enStrRiff,fqdChpt->enStrRiff,
inOct->"in",outOct->"out",MassIn->"m.In",MassOut->"m.out",
medOct1->"medOct1",medOct2->"medOct2",
medMes1->"medMes1",medMes2->"medMes2",
medDec1->"medDec1",medDec2->"medDec2",
mo1->"mo1",mo2->"mo2",
mm1->"mm1",mm2->"mm2",
md1->"md1",md2->"md2"
}/.assocTemp->Association]
(*\:67e5\:770b\:5217\:8868\:7684\:524d\:51e0\:9879*)
testFmt[n_]:=EchoFunction[InputForm]@#[[n]]&


(* ::Section:: *)
(*quark composition*)


(*\:8d39\:66fc\:56fe\:4e2d\:5165\:5c04 mes \:7684\:5938\:514b\:7ec4\:6210,quench*)
tofqdKey["qch"][x_]:=fqdKey["qch",x]
tofqd["qch"][x__]:=fqd[x]
fqdData["qch","mes"]=Association@KeyValueMap[(#1/.qwKey->tofqdKey["qch"])->(#2/.qwave->tofqd["qch"])&]@qwData["mes"];
(*\:8d39\:66fc\:56fe\:4e2d\:5165\:5c04 mes \:7684\:5938\:514b\:7ec4\:6210,sea*)
tofqdKey["sea"][x_]:=fqdKey["sea",x]
tofqd["sea"][x__]:=fqd[x]
fqdData["sea","mes"]=Association@KeyValueMap[(#1/.qwKey->tofqdKey["sea"])->(#2/.qwave->tofqd["sea"])&]@qwData["mes"];


(*\:8d39\:66fc\:56fe\:4e2d\:5165\:5c04 oct \:7684\:5938\:514b\:7ec4\:6210,quench, quench \:914d\:7f6e 1,2,3 \:4f4d\:7f6e\:7684\:5938\:514b\:90fd\:4e0d\:7b49\:4ef7. *)
tofqdKey["qch"][x_]:=fqdKey["qch",x]
tofqd["qch"][x__]:=fqd[x]
fqdData["qch","oct"]=Association@KeyValueMap[(#1/.qwKey->tofqdKey["qch"])->(#2/.qwave->tofqd["qch"])&]@qwData["oct"];
(*\:8d39\:66fc\:56fe\:4e2d\:5165\:5c04 oct \:7684\:5938\:514b\:7ec4\:6210,sea, sea \:914d\:7f6e2,3\:4f4d\:7f6e\:7684\:5938\:514b\:662f\:7b49\:4ef7\:7684.*)
tofqdKey["sea"][x_]:=fqdKey["sea",x]
tofqd["sea"][x_,y_,z_]:=fqd[x,Sequence@@Sort[{y,z}]]
fqdData["sea","oct"]=Association@KeyValueMap[(#1/.qwKey->tofqdKey["sea"])->DeleteDuplicates[#2/.qwave->tofqd["sea"]]&]@qwData["oct"];


(* ::Chapter:: *)
(*quarkflow*)


(* ::Section:: *)
(*RainBow,A-meson,octet*)


(* ::Input:: *)
(*diagIllus@chTag@{"RB","mes","oct"}*)


(*+++++++++++++++++++++++++++Tag \:521d\:59cb\:5316 sea+++++++++++++++++++++++++++*)
fyTag={"RB","mes","oct"};qchTp1="sea";qchTp2="qch";
qchPoss={qchTp1,qchTp2,"poss"};
totEq={qchTp1,qchTp2,"totEq"};
symEq={qchTp1,qchTp2,"symEq"};
qfEqs={qchTp1,qchTp2,"qfEqs"};quaFlow="quaFlow";(* \:8bbe\:7f6etag *)
(*--------\:8ba1\:7b97 quarkflow \:5206\:89e3------------*)
Get["coes.quarkflow-rainbow.wl"];


(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)
fyTag={"RB","mes","oct"};qchTp1="sea";
If[$inNBook,
Query[Cases@KeyValuePattern[
chTagKey["in"]->chTag@ff["\[CapitalSigma]0"]
]]@coeJoin[{fyTag,qchTp1,"poss"}]//dsetFmt]


(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)
fyTag={"RB","mes","oct"};qchTp1="sea";qchTp2="qch";
If[$inNBook,
Query[Cases@KeyValuePattern[
chTagKey["in"]->chTag@fd[2,1,0]
]]@coeJoin[{fyTag,qchTp2,"poss"}]//dsetFmt]


If[$inNBook,
coeJoin[{fyTag,quaFlow}]//testFmt[1]
]


(* ::Section::Closed:: *)
(*Kroll-Ruderman ,A-meson,octet,left*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*end=4;delta=0.05;*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{end/4,0},{end/4,-end/5}}],*)
(*Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end,-4delta}]*)
(*},ImageSize->Small]*)


(*+++++++++++++++++++++++++++Tag \:521d\:59cb\:5316 sea+++++++++++++++++++++++++++*)
fyTag={"KR","mes","oct","left"};qchTp1="sea";qchTp2="qch";
qchPoss={qchTp1,qchTp2,"poss"};
totEq={qchTp1,qchTp2,"totEq"};
symEq={qchTp1,qchTp2,"symEq"};
qfEqs={qchTp1,qchTp2,"qfEqs"};quaFlow="quaFlow";(* \:8bbe\:7f6etag *)
(*--------\:8ba1\:7b97 quarkflow \:5206\:89e3------------*)
Get["coes.quarkflow-rainbow.wl"];


(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)
fyTag={"KR","mes","oct","left"};qchTp1="sea";
If[$inNBook,
Query[Cases@KeyValuePattern[
chTagKey["in"]->chTag@ff["p"]
]]@coeJoin[{fyTag,qchTp1,"poss"}]//dsetFmt]


(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)
fyTag={"KR","mes","oct","left"};qchTp1="sea";qchTp2="qch";
If[$inNBook,
Query[Cases@KeyValuePattern[
chTagKey["in"]->chTag@fd[2,1,0]
]]@coeJoin[{fyTag,qchTp2,"poss"}]//dsetFmt]


If[$inNBook,coeJoin[{fyTag,quaFlow}]//testFmt[1]]


(* ::Section::Closed:: *)
(*Kroll-Ruderman ,A-meson,octet,right*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*end=4;delta=0.05;*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{3/4end,0},{3/4end,-end/5}}],*)
(*Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end+4delta,+4delta}]*)
(*},ImageSize->Small]*)


(*+++++++++++++++++++++++++++Tag \:521d\:59cb\:5316 sea+++++++++++++++++++++++++++*)
fyTag={"KR","mes","oct","right"};qchTp1="sea";qchTp2="qch";
qchPoss={qchTp1,qchTp2,"poss"};
totEq={qchTp1,qchTp2,"totEq"};
symEq={qchTp1,qchTp2,"symEq"};
qfEqs={qchTp1,qchTp2,"qfEqs"};quaFlow="quaFlow";(* \:8bbe\:7f6etag *)
(*--------\:8ba1\:7b97 quarkflow \:5206\:89e3------------*)
Get["coes.quarkflow-rainbow.wl"];


(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)
fyTag={"KR","mes","oct","right"};qchTp1="sea";qchTp2="qch";
If[$inNBook,
Query[Cases@KeyValuePattern[
chTagKey["in"]->chTag@fd[2,1,0]
]]@coeJoin[{fyTag,qchTp1,"poss"}]//dsetFmt]


(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)
fyTag={"KR","mes","oct","right"};qchTp1="sea";qchTp2="qch";
If[$inNBook,
Query[Cases@KeyValuePattern[
chTagKey["in"]->chTag@fd[2,1,0]
]]@coeJoin[{fyTag,qchTp2,"poss"}]//dsetFmt]


If[$inNBook,coeJoin[{fyTag,quaFlow}]//testFmt[1]]


(* ::Section:: *)
(*RainBow,A-octet,F1F2,nonlocal*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*end=4;delta=0.05;*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{end/2,-end/8},{end/2,0}}],*)
(*Text["v1",{end/4,-4delta}],Text["v2",{end/2,4delta}],Text["v3",{3/4end,-4delta}]*)
(*},ImageSize->Small]*)


(*+++++++++++++++++++++++++++Tag \:521d\:59cb\:5316 sea+++++++++++++++++++++++++++*)
fyTag={"RB","oct","F1F2"};qchTp1="sea";qchTp2="qch";
qchPoss={qchTp1,qchTp2,"poss"};
totEq={qchTp1,qchTp2,"totEq"};
symEq={qchTp1,qchTp2,"symEq"};
qfEqs={qchTp1,qchTp2,"qfEqs"};quaFlow="quaFlow";(* \:8bbe\:7f6etag *)
Get["coes.quarkflow-oct-f1f2.wl"];


(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)
fyTag={"RB","oct","F1F2"};qchTp1="sea";qchTp2="qch";
If[$inNBook,
Query[Cases@KeyValuePattern[
chTagKey["in"]->chTag@ff["p"]
]]@coeJoin[{fyTag,qchTp1,"poss"}]//dsetFmt]


(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)
fyTag={"RB","oct","F1F2"};qchTp1="sea";qchTp2="qch";
If[$inNBook,
Query[Cases@KeyValuePattern[
chTagKey["in"]->chTag@ff["p"]
]]@coeJoin[{fyTag,qchTp2,"poss"}]//dsetFmt]


If[$inNBook,
coeJoin[{fyTag,quaFlow}]//testFmt[4]
]


(* ::Section:: *)
(*tadpole, A - octet, F1F2 - order2, nonlocal*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*end=4;delta=0.05;*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Arrow@BezierCurve[{*)
(*{end/2,0},{0,end/2},{end,end/2},{end/2,0}*)
(*}],*)
(*Line[{{end/2,-end/6},{end/2,0}}],*)
(*Text["v1",{end/2,+5delta}]*)
(*},ImageSize->Small]*)


(*+++++++++++++++++++++++++++Tag \:521d\:59cb\:5316 sea+++++++++++++++++++++++++++*)
fyTag={"tad","oct","F1F2"};qchTp1="sea";qchTp2="qch";
(*+++++++++++++++++++++++++++ Key \:521d\:59cb\:5316+++++++++++++++++++++++++++*)
tmpoct=inOct;(*\:9876\:70b91,oct,\:5165\:5c04\:573a*)
tmpmes=medMes1;(*\:9876\:70b92,mes,\:5165\:5c04\:573a*)
(*+++++++++++++++++++++++++++\:751f\:6210\:5938\:514b\:56fe\:4e2d\:6240\:6709\:53ef\:80fd\:7684\:5938\:514b\:914d\:7f6e, sea+++++++++++++++++++++++++++*)
coeJoin[{fyTag,qchTp1,"all"}]=Query[All,Append[#,
fqdKey[fyTag,qchTp1]-><|
fqdpos[1,2,3]->fqdData[qchTp1,"oct"]@fqdKey[qchTp1,#@tmpoct](*oct\:6240\:6709\:7684\:5938\:514b\:7ec4\:5408*),
fqdpos[4,5]->fqdData[qchTp1,"mes"]@fqdKey[qchTp1,#@tmpmes](*mes\:6240\:6709\:7684\:5938\:514b\:7ec4\:5408*)
|>
]&
]@coeJoin[fyTag];
(*++++++++++++++++++++++++++++++++++++++++++++*)
(*\:7b5b\:9009\:51fa\:53ef\:80fd\:7684\:5938\:514b\:914d\:7f6e\:7684\:51fd\:6570,sea*)
connect[qchTp1][x_]:=Module[{qua123,qua45,qua123vld,chOct,chOctb,chMes,chMesMod,chOctbMod,quarkTag},
qua123=x[fqdKey[fyTag,qchTp1]][fqdpos[1,2,3]];(*\:63d0\:53d6\:51fa123\:4f4d\:7f6e\:4e0a\:7684\:5938\:514b, fqdList*)
qua45=x[fqdKey[fyTag,qchTp1]][fqdpos[4,5]];(*\:63d0\:53d6\:51fa45\:4f4d\:7f6e\:4e0a\:7684\:5938\:514b\:ff0cfqdList*)
(* --------------- \:5b57\:6bb5\:4e2d\:7684 oct,octb, mes --------------- *)
chOct=x@inOct;(*\:5165\:5c04 oct *)
chOctb=x@inOct;(*\:4e2d\:95f4 oct *)
chMes=x@medMes1;(*\:4e2d\:95f4 mes *)
(* --------------- \:5c06\[Pi]0,\[Eta]0,\[Eta]8,\:6620\:5c04\:5230\:540c\:6837\:7684\:7c92\:5b50 --------------- *)
chMesMod=chMes/.mesNeutRule;
chOctbMod=chOctb/.octbNeutRule;(*\:5c06\[CapitalSigma]0b,\[CapitalLambda]b,\:6620\:5c04\:5230\:540c\:6837\:7684\:7c92\:5b50*)
(*----------- \:4ea7\:751f\:5408\:9002\:7684\:6570\:636e\:96c6 --------------------*)
DeleteCases[
<|
(*\:8fd9\:91cc, \:67d0\:79cd\:4ecb\:5b50\:5bf9\:5e94\:7684quarkflow\:56fe,\:5938\:514b4\:662f\:786e\:5b9a\:7684\:ff0c\:53ea\:5bf9\:5e94\:4e00\:79cd\:5938\:514b123\:ff0c\:56e0\:6b64\:4fdd\:8bc1\:4e86\:7a0b\:5e8f\:4e0d\:51fa\:9519*)
qua123vld=First[Cases[qua123,fqd[First@#,fd__]],fqd["mis","mis","mis"]];(*\:6311\:9009\:51fa\:5339\:914d\:5938\:514b4\:7684\:5938\:514b123*)
quarkTag=fqdTag[qchTp1][#(*\:5938\:514b4,5*),qua123vld(*\:5938\:514b123*)];(*\:5938\:514b 4,5\:653e\:5728\:524d\:9762\:662f\:4e3a\:4e86\:65b9\:4fbf\:540e\:9762\:7684\:6392\:5e8f, \:5904\:7406 \[CapitalSigma]0, \[CapitalLambda] \:7b80\:5e76\:7684\:60c5\:5f62*)
chTagKey["in"]->chTag[chOct], (*chpt \:5165\:5c04\:7c92\:5b50,\:65b9\:4fbf\:5bf9quarkflow\:5206\:7ec4*)
chTagKey[fyTag]->chTag[chOct,chMesMod,chOctbMod],(*chpt\:56fe\:7684tag\:ff0c\:65b9\:4fbf\:5bf9quarkflow\:5206\:7ec4,\:628a \[Pi]0,\[Eta]0,\[Eta]8 \:5206\:5230\:540c\:4e00\:7ec4, \[CapitalSigma]0b,\[CapitalLambda]b\:5206\:5230\:540c\:4e00\:7ec4*)
fqdpos[1,5]->quarkTag,(*\:4f4d\:7f6e1..5\:7684\:5938\:514b*)
(*\:5f3a\:76f8\:4e92\:4f5c\:7528\:7cfb\:6570, chpt Tag \:4fdd\:7559\:4e0d\:540c\:4ecb\:5b50\:7684\:8d21\:732e,\:540c\:65f6\:65b9\:4fbf\:6392\:5e8f*)
fyCoeKeycStrF1-><|fyCoeTag[chMes]->fyCoe[chOctb][x@fyCoeKeycStrF1,x@fyCoeKeycEMF1]|>,
fyCoeKeycStrF2-><|fyCoeTag[chMes]->fyCoe[chOctb][x@fyCoeKeycStrF2,x@fyCoeKeycEMF2]|>,
fqdpos[1,2,3]->fqdTag[qua123vld,qchTp1],(*\:4f4d\:7f6e123\:7684\:5938\:514b\:914d\:7f6e,\:65b9\:4fbf\:8054\:7acb sea \:56fe\:7684\:76f8\:7b49\:5173\:7cfb, \:4e0d\:9700\:8981 mes Tag*)
fqdpos[6,7,8]->fqdTag[fqd@@Flatten[{#[[2]]/.quarkRule["bar->reg"](*\:5938\:514bbar\[Rule]\:5938\:514b*),qua123vld[[2;;3]]},Infinity,fqd],qchTp1,chOct](*\:538b\:5e73\:5d4c\:5957\:7684fqd*)
|>&/@qua45,(*Map \:4ecb\:5b50\:53ef\:80fd\:7684\:5938\:514b\:7ec4\:5408 *)
KeyValuePattern[fqdpos[1,5]->fqdTag[qchTp1][ __,fqd["mis","mis","mis"] ]](*\:5220\:9664 Miss \:7c7b\:578b\:ff0c\:4e5f\:5c31\:662f123\:548c45\:4e0d\:5339\:914d\:7684\:60c5\:51b5*)
]/.fqdList->Sequence
]
(*+\:7b5b\:9009\:51fa\:53ef\:80fd\:7684\:5938\:514b\:6d41\:914d\:7f6e++++++++++++++++++*)
coeJoin[{fyTag,qchTp1,"poss"}]=Query[Sort,KeyDrop[fqdpos[6,7,8]](*\:53bb\:6389\:5197\:4f59\:7684678\:5938\:514b*)
]@Query[All,connect[qchTp1]]@coeJoin[{fyTag,qchTp1,"all"}];


(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)
If[$inNBook,
fyTag={"tad","oct","F1F2"};qchTp1="sea";qchTp2="qch";
Query[Cases@KeyValuePattern[
chTagKey["in"]->chTag@fd[2,1,0]
]]@coeJoin[{fyTag,qchTp1,"poss"}]//dsetFmt]


(*\:7ee7\:7eed\:6dfb\:52a0\:5938\:514b\:56fe\:4e2d\:6240\:6709\:53ef\:80fd\:7684\:5938\:514b\:914d\:7f6e, quench\:56fe*)
fyTag={"tad","oct","F1F2"};qchTp1="sea";qchTp2="qch";
qchPoss={qchTp1,qchTp2,"poss"};
tmpoct=inOct;(*\:9876\:70b91,oct,\:5165\:5c04\:573a*)
tmpmes=medMes1;(*\:9876\:70b92,mes,\:5165\:5c04\:573a*)
(*+++++++++++++++++++++++++++*)
coeJoin[{fyTag,qchTp2,"all"}]=Query[All,Append[#,
fqdKey[fyTag,qchTp2]-><|
fqdpos[1,2,3]->fqdData[qchTp2,"oct"]@fqdKey[qchTp2,#@tmpoct](*oct\:6240\:6709\:7684\:5938\:514b\:7ec4\:5408*),
fqdpos[4,5]->fqdData[qchTp2,"mes"]@fqdKey[qchTp2,#@tmpmes](*mes\:6240\:6709\:7684\:5938\:514b\:7ec4\:5408*)
|>
]&
]@coeJoin[fyTag];
(*++++++++++++++++++++++++++++++++++++++++++++*)
(*\:7b5b\:9009\:51fa\:6240\:6709\:53ef\:80fd\:7684\:914d\:7f6e\:7684\:51fd\:6570, qch*)
quaMesToOct[fd[x__,0],fd[y__,1]]:=fqd[fd[x,0],fd[y,0],fd_](*\:5c06 mes \:53d8\:6210 oct, \:5373\:5c06\:5938\:514b2\:4ece\:53cd\:53d8\:5230\:6b63, \:5e76\:8ffd\:52a0 fd_ \:6a21\:5f0f*)
(* ----- ----- *)
connect[qchTp2][x_]:=Module[{qua123,qua45,qua123vld,chOct,chOctb,chMes,chOctbMod,chMesMod,quarkTag},
qua123=x[fqdKey[fyTag,qchTp2]]@fqdpos[1,2,3];(*\:63d0\:53d6\:51fa123\:4f4d\:7f6e\:4e0a\:7684\:5938\:514b, fqdList*)
qua45=x[fqdKey[fyTag,qchTp2]]@fqdpos[4,5];(*\:63d0\:53d6\:51fa45\:4f4d\:7f6e\:4e0a\:7684\:5938\:514b\:ff0cfqdList*)
(* --------------- \:5b57\:6bb5\:4e2d\:7684 oct,octb, mes --------------- *)
chOct=x@inOct;(* \:5165\:5c04 oct *)
chOctb=x@inOct;(* \:4e2d\:95f4 oct *)
chMes=x@medMes1;(* \:4e2d\:95f4 mes *)
(* --------------- \:5c06\[Pi]0,\[Eta]0,\[Eta]8,\:6620\:5c04\:5230\:540c\:6837\:7684\:7c92\:5b50 --------------- *)
chMesMod=chMes/.mesNeutRule;
chOctbMod=chOctb/.octbNeutRule;(*\:5c06\[CapitalSigma]0b,\[CapitalLambda]b \:6620\:5c04\:5230\:540c\:6837\:7684\:7c92\:5b50*)
(*----------- \:4ea7\:751f\:5408\:9002\:7684\:6570\:636e\:96c6 --------------------*)
DeleteCases[<|
qua123vld=First[(*\:6311\:9009\:51fa\:5339\:914d\:5938\:514b4\:7684\:5938\:514b123*)
Cases[(* \:5220\:9664\:4e0d\:5b58\:5728\:7684 qqq \:4e2d\:95f4\:6001,\:8fd9\:91cc\:5e94\:8be5\:4e0d\:4f1a\:5269\:4e0b\:7a7a\:96c6*)
DeleteCases[qua123,fqd[a_fd,b_fd,b_fd]],#/.fqd->quaMesToOct],fqd["mis","mis","mis"]];
quarkTag=fqdTag[qchTp2][#(*\:5938\:514b4,5*),qua123vld(*\:5938\:514b123*)];(*\:5938\:514b 4,5\:653e\:5728\:524d\:9762\:662f\:4e3a\:4e86\:65b9\:4fbf\:540e\:9762\:7684\:6392\:5e8f, \:5904\:7406 \[CapitalSigma]0, \[CapitalLambda] \:7b80\:5e76\:7684\:60c5\:5f62*)
chTagKey["in"]->chTag[chOct],(*chpt \:5165\:5c04\:7c92\:5b50,\:65b9\:4fbf\:5bf9quarkflow\:5206\:7ec4*)
chTagKey[fyTag]->chTag[chOct,chMesMod,chOctbMod],(*chpt\:56fe\:7684tag\:ff0c\:65b9\:4fbf\:5bf9quarkflow\:5206\:7ec4,\:628a \[Pi]0,\[Eta]0,\[Eta]8 \:5206\:5230\:540c\:4e00\:7ec4, \[CapitalSigma]0b,\[CapitalLambda]b\:5206\:5230\:540c\:4e00\:7ec4*)
fqdpos[1,5]->quarkTag,(*\:4f4d\:7f6e1..5\:7684\:5938\:514b*)
(*\:5f3a\:76f8\:4e92\:4f5c\:7528\:7cfb\:6570, chpt Tag \:4fdd\:7559\:4e0d\:540c\:4ecb\:5b50\:7684\:8d21\:732e,\:540c\:65f6\:65b9\:4fbf\:6392\:5e8f*)
fyCoeKeycStrF1-><|fyCoeTag[chMes]->fyCoe[chOctb][x@fyCoeKeycStrF1,x@fyCoeKeycEMF1]|>,
fyCoeKeycStrF2-><|fyCoeTag[chMes]->fyCoe[chOctb][x@fyCoeKeycStrF2,x@fyCoeKeycEMF2]|>,
(*\:4f4d\:7f6e123\:7684\:5938\:514b\:914d\:7f6e,\:65b9\:4fbf\:8054\:7acb sea \:56fe\:7684\:76f8\:7b49\:5173\:7cfb, \:4e0d\:9700\:8981 mes Tag*)
fqdpos[1,2,3]->fqdTag[qua123vld,qchTp2],
fqdpos[6,7,8]->fqdTag[qua123vld[[{2,2,3}]],qchTp2,chOct]
|>&/@qua45,(*Map \:4ecb\:5b50\:53ef\:80fd\:7684\:5938\:514b\:7ec4\:5408 *)
KeyValuePattern[fqdpos[1,5]->fqdTag[qchTp2][ __,fqd["mis","mis","mis"] ]](*\:5220\:9664 Miss \:7c7b\:578b\:ff0c\:4e5f\:5c31\:662f123\:548c45\:4e0d\:5339\:914d\:7684\:60c5\:51b5*)
]/.fqdList->Sequence
]
(*++++++++++++++++++\:7b5b\:9009\:51fa\:53ef\:80fd\:7684\:5938\:514b\:6d41\:914d\:7f6e++++++++++++++++++*)
coeJoin[{fyTag,qchTp2,"poss"}]=Query[Sort,KeyDrop[fqdpos[6,7,8]](*\:53bb\:6389\:5197\:4f59\:7684678\:5938\:514b*)
]@Query[All,connect[qchTp2]]@coeJoin[{fyTag,qchTp2,"all"}];
(*++++++++++++++++++\:6c47\:603b seq quench \:4e24\:8005\:60c5\:51b5\:7684\:5938\:514b\:56fe++++++++++++++++++*)
coeJoin[{fyTag,qchPoss}]=Query[Sort]@Join[coeJoin[{fyTag,qchTp1,"poss"}],coeJoin[{fyTag,qchTp2,"poss"}]];


(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)
If[$inNBook,
fyTag={"tad","oct","F1F2"};qchTp1="sea";qchTp2="qch";
Query[Cases@KeyValuePattern[
chTagKey["in"]->chTag@fd[2,1,0]
]]@coeJoin[{fyTag,qchTp2,"poss"}]//dsetFmt]


(* ::Subsection:: *)
(*equations*)


(*quarkflow \:6c42\:548c\:5173\:7cfb, sea+quench \[Equal] chpt *)
fyTag={"tad","oct","F1F2"};qchTp1="sea";qchTp2="qch";
qchPoss={qchTp1,qchTp2,"poss"};
totEq={qchTp1,qchTp2,"totEq"};
(* ++++++++++++++++++++++++++++++++++++ \:5efa\:7acb\:65b9\:7a0b\:7ec4,sea+quench \[Equal] chpt ++++++++++++++++++++++++++++++++++++ *)
coeJoin[{fyTag,totEq}]=Query[
GroupBy[#@chTagKey["in"]&],(*GroupBy\:662f\:4e0b\:964d\:7b97\:7b26*)
GroupBy[#@chTagKey[fyTag]&],
fqdTot2[#@fqdpos[1,5],#@fyCoeKeycStr]&@*Merge[Join]
]@coeJoin[{fyTag,qchPoss}];
(*quarkflow \:5bf9\:79f0\:6027\:5173\:7cfb\:ff0c\:76f8\:540c\:4ef7\:5938\:514b\:914d\:7f6e\:7684\:6d77\:5938\:514b\:56fe\:76f8\:7b49---------------*)
qchPoss={qchTp1,qchTp2,"poss"};
symEq={qchTp1,qchTp2,"symEq"};
(*------------quarkflow \:53cd\:5e94\:9053 \:7684\:5bf9\:79f0\:6027\:5173\:7cfb ------------*)
fqdSym[{x_}]:=Nothing(*\:5220\:9664\:5355\:4e2a\:5143\:7d20\:81ea\:76f8\:7b49\:7684\:6052\:7b49\:5f0f*)
fqdSym[{x__}]:=eqList["qf"][Equal[x]]
(* --------------------- \:5efa\:7acb symmetry \:65b9\:7a0b\:7ec4 --------------------- *)
coeJoin[{fyTag,symEq}]=Query[
GroupBy[#@chTagKey["in"]&],
GroupBy[#@fqdpos[1,2,3]&],
fqdSym@*(Key@fqdpos[1,5])@*Merge[Union]
]@coeJoin[{fyTag,qchTp1,"poss"}];
(* ++++++++++++++++++++++++++++++++++++ \:5408\:5e76\:4e24\:79cd\:65b9\:7a0b\:7ec4 ++++++++++++++++++++++++++++++++++++ *)
qfEqs={qchTp1,qchTp2,"qfEqs"};quaFlow="quaFlow";(* \:8bbe\:7f6etag *)
coeJoin[{fyTag,qfEqs}]=Merge[{
Query[All,Values]@coeJoin[{fyTag,symEq}],
Query[All,Values]@coeJoin[{fyTag,totEq}]
},
Catenate
];
(* --------------------- \:6c42\:89e3\:8054\:7acb\:7684\:65b9\:7a0b\:7ec4 --------------------- *)
coeJoin[{fyTag,quaFlow}]=Query[All,
First@Solve[#/.eqList["qf"]->Sequence,DeleteDuplicates@Cases[#,Blank@fqdTag["sea"]|Blank@fqdTag["qch"],Infinity ]]&
]@coeJoin[{fyTag,qfEqs}];


If[$inNBook,
coeJoin[{fyTag,quaFlow}]//testFmt[1]]
