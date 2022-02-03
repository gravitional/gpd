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
fqdData["sea","oct"]=Association@KeyValueMap[
(#1/.qwKey->tofqdKey["sea"])->DeleteDuplicates[#2/.qwave->tofqd["sea"]]&
]@qwData["oct"];


(* ::Chapter:: *)
(*quarkflow*)


(* ::Section::Closed:: *)
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
chTagKey["in"]->chTag@ff["p"]
]]@coeJoin[{fyTag,qchTp2,"poss"}]//dsetFmt]


If[$inNBook,
coeJoin[{fyTag,quaFlow}]//testFmt[1]
]


(* ::Section::Closed:: *)
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
fyTag={"tad","oct","F1F2"};
qchTp1="sea";qchTp2="qch";
qchPoss={qchTp1,qchTp2,"poss"};
totEq={qchTp1,qchTp2,"totEq"};
symEq={qchTp1,qchTp2,"symEq"};


(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)
If[$inNBook,
fyTag={"tad","oct","F1F2"};qchTp1="sea";qchTp2="qch";
Query[Cases@KeyValuePattern[
chTagKey["in"]->chTag@fd[2,1,0]
]]@coeJoin[{fyTag,qchTp1,"poss"}]//dsetFmt]


(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)
If[$inNBook,
fyTag={"tad","oct","F1F2"};qchTp1="sea";qchTp2="qch";
Query[Cases@KeyValuePattern[
chTagKey["in"]->chTag@fd[2,1,0]
]]@coeJoin[{fyTag,qchTp2,"poss"}]//dsetFmt]


If[$inNBook,
coeJoin[{fyTag,quaFlow}]//testFmt[1]]
