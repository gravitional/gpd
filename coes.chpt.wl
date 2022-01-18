(* ::Package:: *)

(* ::Title:: *)
(*coes.chpt.wl*)


(* ::Text:: *)
(*\:4ece\:62c9\:6c0f\:91cf\:5c55\:5f00\:540e\:4ea7\:751f\:7684\:8026\:5408\:7cfb\:6570, \:63a8\:5bfc\:4e0d\:540c\:53cd\:5e94\:9053\:7684\:524d\:7f6e\:7cfb\:6570\:ff0c\:5373\:628a\:7cfb\:6570\:7ec4\:5408\:8d77\:6765.*)
(*K0b\:8868\:793a K0 bar, pb \:8868\:793a p bar, \:5373\:8d28\:5b50\:7684\:53cd\:7c92\:5b50\:573a\:3002*)


(* ::Text:: *)
(*v1,v2,v3 \:8868\:793a\:7279\:5b9a\:4f4d\:7f6e\:7684\:9876\:70b9,\:9876\:70b9\:4ece\:5de6\:5230\:53f3\:6392\:5e8f.  p1,p2 \:8868\:793a\:7279\:5b9a\:4f4d\:7f6e\:7684\:4e2d\:95f4\:6001\:7c92\:5b50, \:4ece\:5de6\:5230\:53f3.*)


(* ::Text:: *)
(*\:4f7f\:7528\:7535\:5b50\:7535\:8377e\:4f5c\:4e3a\:5355\:4f4d\:ff0c\:8fd9\:6837e\:4e0d\:663e\:5f0f\:51fa\:73b0\:5728\:9876\:70b9\:4e2d. e>0*)


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
(*<<modules*)


(*\:8bfb\:5165\:50a8\:5b58\:9876\:70b9\:7cfb\:6570\:7684\:6587\:4ef6*)
Enclose@Confirm@Get["gen.chpt.coes.wl"];


(*\:7ed9\:51fa\:672c\:5730\:7f13\:5b58\:6587\:4ef6\:7684\:8def\:5f84,\:7ed9\:51fa\:6587\:4ef6\:548c\:62d3\:5c55\:540d*)
localPath[Directory_][filename_]:=FileNameJoin[{
Directory,StringRiffle[enList@filename,"-"]}];
(*io \:51fd\:6570, \:4fdd\:5b58\:7ed3\:679c\:5230\:672c\:5730\:6587\:4ef6*)
serialize[Directory_][filename_,result_]:=With[
{path=localPath[Directory][filename]},
Export[path,result];echo["Exporting finished: ", path];]


(*\:4fdd\:5b58\:7cfb\:6570\:5230\:672c\:5730\:6587\:4ef6*)
(*serializeCoe[fyTag_,coeJoin_]:=Block[{path},
path=FileNameJoin[{coesDir,"coe.chpt."<>StringRiffle[fyTag,"."]<>".wdx"}];
Export[path,coeJoin[fyTag]];
echo["Exporting finished: ", path];
]*)


(*\:5b9a\:4e49\:4e00\:4e2a\:786e\:4fdd\:5b57\:7b26\:4e32\:7684\:51fd\:6570,\:7528\:9017\:53f7\:9694\:5f00\:8f93\:5165*)
enStrRiff[x__]:=StringRiffle[ToString/@enList[x],","]
(*\:7528\:6765\:5c06 dataset \:4e2d\:7b2c\:4e8c\:5c42\:ff0c\:5373\:5173\:8054\:7684key\:5f3a\:5236\:6392\:7248\:4e3a\:5b57\:7b26\:4e32\:ff0c\:53ef\:4ee5\:8f83\:597d\:7684\:663e\:793a.*)
dsetFmt[x_]:=Dataset[x/.Association->assocTemp/.{
fdType->enStrRiff,vtxType->enStrRiff,
chTagKey->enStrRiff,fyCoeKey->enStrRiff,
fyVtx->enStrRiff,vtxCoe->Identity,fyCoe->Times,massV->Identity,
fqdKey->enStrRiff,fqdpos->enStrRiff,
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


(* \:56fe\:5f62\:8868\:793a\:521d\:59cb\:5316 *)
end=4;delta=0.05;
(*\:521d\:59cb\:5316\:7cfb\:6570\:7684\:54c8\:5e0c*)
coeJoin=<||>;


(* ::Chapter:: *)
(*channels*)


(* ::Section:: *)
(*RainBow,A-meson,octet*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*end=4;delta=0.05;*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{end/2,end/4},{end/2,end/4+end/8}}],*)
(*Text["v1",{end/4,-4delta}],Text["v2",{end/2,end/4-4delta}],Text["v3",{3/4end,-4delta}]*)
(*},ImageSize->Small]*)


(* \:8d39\:66fc\:56fe\:7684chpt Tag\:ff0c\:4ee5\:53ca\:7528\:5230\:7684\:9876\:70b9 v1,v2,v3, \:4ece\:5de6\:5230\:53f3\:6392\:5217 *)
fyTag={"RB","mes","oct"};
(*\:7ed9\:9876\:70b9\:6253\:4e0atag,\:9632\:6b62 JoinAcross \:64cd\:4f5c\:65f6 Key \:51b2\:7a81*)
vtxType1=vtxType["str","DF","mesOut"];  vtx1=Query[All,KeyMap@fyVtx1]@vtx[unq["type"->vtxType1]];
vtxType2=vtxType["F1","\[Phi]\[Phi]A"];            vtx2=Query[All,KeyMap@fyVtx2]@vtx[unq["type"->vtxType2]];
vtxType3=vtxType["str","DF","mesIn"];   vtx3=Query[All,KeyMap@fyVtx3]@vtx[unq["type"->vtxType3]];
(*++++++++++++++++++++++++++++++++++++++++++++++++++*)
vtxJoin1[fyTag]=Query[All,(*\:4fee\:9970\:9876\:70b91*)
Append[#,{
(*\:9876\:70b91\:7684\:51fa\:5c04\:573a -- mesOut\:ff0c\:7b49\:4e8e\:9876\:70b92\:7684\:5165\:5c04\:573a -- mes *)
fyVtx2@fdTypeMes->(#@fyVtx1@fdTypeMesOut/.mesRule["reverse"]),
(*\:9876\:70b91\:7684\:51fa\:5c04\:573a -- octb\:ff0c\:7b49\:4e8e\:9876\:70b93\:7684\:5165\:5c04\:573a -- oct*)
fyVtx3@fdTypeOct->(#@fyVtx1@fdTypeOctb/.octetRule["bar->reg"]),
(*\:5bf9\:4e8e\[CapitalLambda]-\[CapitalSigma]0\:7c92\:5b50\:ff0c\:8fd8\:9700\:8981\:9650\:5b9a\:51fa\:5c04\:6001\:548c\:5165\:5c04\:6001\:76f8\:540c*)
fyVtx3@fdTypeOctb->(#@fyVtx1@fdTypeOct/.octetRule["reg->bar"])
}
]&
]@vtx1;
(*\:9876\:70b92\:51fa\:5c04\:573a -- mesOut, \:7b49\:4e8e\:9876\:70b93\:7684\:5165\:5c04\:573a -- mes *)
vtxJoin2[fyTag]=Query[All,
Append[#,fyVtx3@fdTypeMes->(#@fyVtx2@fdTypeMesOut/.mesRule["reverse"])]&
]@vtx2;(*\:9876\:70b92\:ff0c\:53cd\:5e38\:78c1\:77e9\:975e\:5b9a\:57df\:5316*)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91\:ff0c2*)
vtxJoinTmp1[fyTag]=JoinAcross[
vtxJoin1@fyTag,(*\:9876\:70b91,\:4fee\:9970\:8fc7\:7684\:5f3a\:76f8\:4e92\:4f5c\:7528\:9876\:70b9\:ff0c\:52a0\:5165\:4e86\:9876\:70b92\:51fa\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
vtxJoin2@fyTag,(*\:9876\:70b92\:ff0c\:4fee\:9970\:8fc7\:7684\:4ecb\:5b50\:7535\:78c1\:6d41,\:52a0\:5165\:4e86\:9876\:70b93\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
{Key@fyVtx2@fdTypeMes}(*\:7c98\:8fde\:9876\:70b91\:ff0c2.\:6761\:4ef6\:662f\:ff1a\:4ece\:9876\:70b91\:63a8\:5bfc\:7684 mes \[Equal] \:9876\:70b92\:7684 mes *)
,"Inner"
];
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91,2\:4e0e\:9876\:70b93*)
vtxJoinTmp2[fyTag]=JoinAcross[(*\:7c98\:8fde\:9876\:70b9(1,2)\:ff0c3 *)
vtxJoinTmp1@fyTag, (*\:4fee\:9970\:597d\:7684\:9876\:70b91,2*)
vtx3,(*\:9876\:70b93\:ff0c\:5f3a\:76f8\:4e92\:4f5c\:7528*)
{
Key@fyVtx3@fdTypeOct,(*\:9876\:70b91\:7684oct\:6b63\:573a==\:9876\:70b93\:7684oct\:573a*)
Key@fyVtx3@fdTypeOctb,(*\:9876\:70b93\:7684\:51fa\:5c04\:573a\:ff0c\:8981\:6c42\:548c\:5165\:5c04\:573a\:4e00\:81f4*)
Key@fyVtx3@fdTypeMes(*\:7531\:9876\:70b92\:63a8\:5bfc\:7684mes\:6b63\:573a==\:9876\:70b93\:7684mes\:6b63\:573a*)
}
,"Inner"
];
(*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:9009\:53d6\:9700\:8981\:7684\:5b57\:6bb5\:ff0c\:5e76\:6309\:7167\:987a\:5e8f\:6392\:5217*)
vtxJoin[fyTag]=Query[All,
<|
inOct->#@fyVtx1@fdTypeOct,(*\:9876\:70b91,oct,\:5165\:5c04\:573a*)
outOct->#@fyVtx3@fdTypeOctb,(*\:9876\:70b93,oct,\:51fa\:5c04\:573a*)
medOct1->#@fyVtx1@fdTypeOctb,(*\:9876\:70b91,oct,\:51fa\:5c04\:573a*)
medMes1->#@fyVtx2@fdTypeMes,(*\:9876\:70b92,mes,\:5165\:5c04\:573a*)
(*+++++++++++++++++++++++++ \:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef +++++++++++++++++++++++++*)
fyCoeKeycAll->fyCoe[(*\:6240\:6709\:8026\:5408\:7cfb\:6570\:4e58\:79ef*)
#@fyVtx1@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType3,(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxType2(*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycStr->fyCoe[(*\:5f3a\:76f8\:4e92\:4f5c\:7528\:8026\:5408\:7cfb\:6570\:7684\:4e58\:79ef*)
#@fyVtx1@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType3(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycEM->fyCoe[(* \:7535\:78c1\:6d41\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxType2(*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
]
|>&
]@vtxJoinTmp2[fyTag];


fyTag={"RB","mes","oct"};
coeJoin[fyTag]=Join[
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[chTagKey["chTag"]->chTag[fyTag]]]@vtxJoin[fyTag]
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
,Query[All,
<|
MassIn->(#@inOct/.fd[a_,b_,0]:>massV@fd[a,b,2])(* \:5165\:5c04\:91cd\:5b50\:8d28\:91cf*)
,MassOct1->(#@medOct1/.fd[a_,b_,1]:>massV@fd[a,b,2])(*\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf*)
,MassMes1->(#@medMes1/.fd[a_,b_,0]:>massV@fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag]
,2(*\:8fde\:63a5\:7b2c2\:5c42*)
];
(*serializeCoe[fyTag,coeJoin]*)


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*If[$inNBook,*)
(*fyTag={"RB","mes","oct"};*)
(*Query[Cases@KeyValuePattern[*)
(*inOct->fd[2,4,0]*)
(*]]@coeJoin[fyTag]//dsetFmt]*)


(* ::Section::Closed:: *)
(*Kroll-Ruderman,A-meson,octet,left*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{end/4,0},{end/4,-end/5}}],*)
(*Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end,-4delta}]*)
(*},ImageSize->Small]*)


(* \:8d39\:66fc\:56fe\:7684chpt Tag\:ff0c\:4ee5\:53ca\:7528\:5230\:7684\:9876\:70b9 v1,v2,v3, \:4ece\:5de6\:5230\:53f3\:6392\:5217 *)
fyTag={"KR","mes","oct","left"};
(*\:7ed9\:9876\:70b9\:6253\:4e0atag,\:9632\:6b62 JoinAcross \:64cd\:4f5c\:65f6 Key \:51b2\:7a81*)
vtxType1=vtxType["F1","DF","mesOut"];   vtx1=Query[All,KeyMap@fyVtx1]@vtx[unq["type"->vtxType1]];
vtxType1Str=vtxType["F1","DF","mesOut","str"]; vtxType1EM=vtxType["F1","DF","mesOut","EM"];
vtxType2=vtxType["str","DF","mesIn"];  vtx2=Query[All,KeyMap@fyVtx2]@vtx[unq["type"->vtxType2]];
(*++++++++++++++++++++++++++++++++++++++++++++++++++*)
vtxJoin1[fyTag]=Query[All,(*\:4fee\:9970\:9876\:70b91*)
Append[#,{
(*\:9876\:70b91\:7684\:51fa\:5c04\:573a -- mesOut\:ff0c\:5b83\:7684\:53cd\:573a==\:9876\:70b92\:7684\:5165\:5c04\:573a -- mes *)
fyVtx2@fdTypeMes->(#@fyVtx1@fdTypeMesOut/.mesRule["reverse"]),
(*\:9876\:70b91\:7684\:51fa\:5c04\:573a -- octb\:ff0c\:7b49\:4e8e\:9876\:70b92\:7684\:5165\:5c04\:573a -- oct*)
fyVtx2@fdTypeOct->(#@fyVtx1@fdTypeOctb/.fd[a_,b_,1]:>fd[a,b,0]),
(*\:5bf9\:4e8e\[CapitalLambda]-\[CapitalSigma]0\:7c92\:5b50\:ff0c\:8fd8\:9700\:8981\:9650\:5b9a\:51fa\:5c04\:6001\:548c\:5165\:5c04\:6001\:76f8\:540c*)
fyVtx2@fdTypeOctb->(#@fyVtx1@fdTypeOct/.fd[a_,b_,0]:>fd[a,b,1])
}
]&
]@vtx1;
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91\:ff0c2*)
vtxJoinTmp1[fyTag]=JoinAcross[
vtxJoin1@fyTag,(*\:9876\:70b91,\:4fee\:9970\:8fc7\:7684\:5f3a\:76f8\:4e92\:4f5c\:7528\:9876\:70b9\:ff0c\:52a0\:5165\:4e86\:9876\:70b92\:51fa\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
vtx2,(*\:9876\:70b92\:ff0c\:4fee\:9970\:8fc7\:7684\:4ecb\:5b50\:7535\:78c1\:6d41,\:52a0\:5165\:4e86\:9876\:70b93\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
{Key@fyVtx2@fdTypeMes,Key@fyVtx2@fdTypeOct,Key@fyVtx2@fdTypeOctb}(*\:7c98\:8fde\:9876\:70b91\:ff0c2.\:6761\:4ef6\:662f\:ff1a\:4ece\:9876\:70b91\:63a8\:5bfc\:7684\:51fa\:5c04mes out\[Equal]\:4ecb\:5b50\:7535\:78c1\:6d41\:7684\:51fa\:5c04mesb*)
,"Inner"
];
(*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:9009\:53d6\:9700\:8981\:7684\:5b57\:6bb5\:ff0c\:5e76\:6309\:7167\:987a\:5e8f\:6392\:5217*)
vtxJoin[fyTag]=Query[All,
<|
inOct->#@fyVtx1@fdTypeOct,(*\:9876\:70b91,oct\:5165\:5c04\:573a*)
outOct->#@fyVtx2@fdTypeOctb,(*\:9876\:70b92,oct\:51fa\:5c04\:573a*)
medOct1->#@fyVtx1@fdTypeOctb,(*\:9876\:70b91,oct\:51fa\:5c04\:573a*)
medMes1->#@fyVtx2@fdTypeMes,(*\:9876\:70b92,mes,\:5165\:5c04\:573a*)
(*\:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef *)
fyCoeKeycAll->fyCoe[(*\:6240\:6709\:8026\:5408\:7cfb\:6570\:4e58\:79ef*)
#@fyVtx1@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxType2(*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycStr->fyCoe[(*\:5f3a\:76f8\:4e92\:4f5c\:7528\:8026\:5408\:7cfb\:6570\:7684\:4e58\:79ef*)
#@fyVtx1@vtxType1Str,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxType2(*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycEM->fyCoe[(* \:7535\:78c1\:6d41\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx1@vtxType1EM
]
|>&
]@vtxJoinTmp1[fyTag];


fyTag={"KR","mes","oct","left"};
coeJoin[fyTag]=Join[
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[chTagKey["chTag"]->chTag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassIn->(#@inOct/.fd[a_,b_,0]:>massV@fd[a,b,2]),(* \:5165\:5c04\:91cd\:5b50\:8d28\:91cf*)
MassOct1->(#@medOct1/.fd[a_,b_,1]:>massV@fd[a,b,2]),(*\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf*)
MassMes1->(#@medMes1/.fd[a_,b_,0]:>massV@fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag],
2(*\:8fde\:63a5\:7b2c2\:5c42*)
];
(*serializeCoe[fyTag,coeJoin]*)


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*fyTag={"KR","mes","oct","left"};*)
(*Query[Cases@KeyValuePattern[*)
(*inOct->fd[2,2,0]*)
(*]]@coeJoin[fyTag]//dsetFmt*)


(* ::Section::Closed:: *)
(*Kroll-Ruderman,A-meson,octet,right*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{3/4end,0},{3/4end,-end/5}}],*)
(*Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end+4delta,+4delta}]*)
(*},ImageSize->Small]*)


(* \:8d39\:66fc\:56fe\:7684chpt Tag\:ff0c\:4ee5\:53ca\:7528\:5230\:7684\:9876\:70b9 v1,v2 \:4ece\:5de6\:5230\:53f3\:6392\:5217 *)
fyTag={"KR","mes","oct","right"};
(*\:7ed9\:9876\:70b9\:6253\:4e0atag,\:9632\:6b62 JoinAcross \:64cd\:4f5c\:65f6 Key \:51b2\:7a81*)
vtxType1=vtxType["str","DF","mesOut"];  vtx1=Query[All,KeyMap@fyVtx1]@vtx[unq["type"->vtxType1]];
vtxType2=vtxType["F1","DF","mesIn"];     vtx2=Query[All,KeyMap@fyVtx2]@vtx[unq["type"->vtxType2]];
vtxType2Str=vtxType["F1","DF","mesIn","str"]; vtxType2EM=vtxType["F1","DF","mesIn","EM"];
(*++++++++++++++++++++++++++++++++++++++++++++++++++*)
vtxJoin1[fyTag]=Query[All,(*\:4fee\:9970\:9876\:70b91*)
Append[#,{
(*\:9876\:70b91\:7684\:51fa\:5c04\:573a -- mesOut\:ff0c\:5b83\:7684\:53cd\:573a==\:9876\:70b92\:7684\:5165\:5c04\:573a -- mes *)
fyVtx2@fdTypeMes->(#@fyVtx1@fdTypeMesOut/.mesRule["reverse"]),
(*\:9876\:70b91\:7684\:51fa\:5c04\:573a -- octb\:ff0c\:7b49\:4e8e\:9876\:70b92\:7684\:5165\:5c04\:573a -- oct*)
fyVtx2@fdTypeOct->(#@fyVtx1@fdTypeOctb/.fd[a_,b_,1]:>fd[a,b,0]),
(*\:5bf9\:4e8e\[CapitalLambda]-\[CapitalSigma]0\:7c92\:5b50\:ff0c\:8fd8\:9700\:8981\:9650\:5b9a\:51fa\:5c04\:6001\:548c\:5165\:5c04\:6001\:76f8\:540c*)
fyVtx2@fdTypeOctb->(#@fyVtx1@fdTypeOct/.fd[a_,b_,0]:>fd[a,b,1])
}
]&
]@vtx1;
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91\:ff0c2*)
vtxJoinTmp1[fyTag]=JoinAcross[
vtxJoin1@fyTag,(*\:9876\:70b91,\:4fee\:9970\:8fc7\:7684\:5f3a\:76f8\:4e92\:4f5c\:7528\:9876\:70b9\:ff0c\:52a0\:5165\:4e86\:9876\:70b92\:51fa\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
vtx2,(*\:9876\:70b92\:ff0c\:4fee\:9970\:8fc7\:7684\:4ecb\:5b50\:7535\:78c1\:6d41,\:52a0\:5165\:4e86\:9876\:70b93\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
{Key@fyVtx2@fdTypeMes,Key@fyVtx2@fdTypeOct,Key@fyVtx2@fdTypeOctb}(*\:7c98\:8fde\:9876\:70b91\:ff0c2.\:6761\:4ef6\:662f\:ff1a\:4ece\:9876\:70b91\:63a8\:5bfc\:7684\:51fa\:5c04mes out\[Equal]\:4ecb\:5b50\:7535\:78c1\:6d41\:7684\:51fa\:5c04mesb*)
,"Inner"
];
(*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:9009\:53d6\:9700\:8981\:7684\:5b57\:6bb5\:ff0c\:5e76\:6309\:7167\:987a\:5e8f\:6392\:5217*)
vtxJoin[fyTag]=Query[All,
<|
inOct->#@fyVtx1@fdTypeOct,(*\:9876\:70b91,oct\:5165\:5c04\:573a*)
outOct->#@fyVtx2@fdTypeOctb,(*\:9876\:70b92,oct\:51fa\:5c04\:573a*)
medOct1->#@fyVtx1@fdTypeOctb,(*\:9876\:70b91,oct\:51fa\:5c04\:573a*)
medMes1->#@fyVtx2@fdTypeMes,(*\:9876\:70b92,mes,\:5165\:5c04\:573a*)
(*\:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef *)
fyCoeKeycAll->fyCoe[(*\:6240\:6709\:8026\:5408\:7cfb\:6570\:4e58\:79ef*)
#@fyVtx1@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxType2(*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycStr->fyCoe[(*\:5f3a\:76f8\:4e92\:4f5c\:7528\:8026\:5408\:7cfb\:6570\:7684\:4e58\:79ef*)
#@fyVtx1@vtxType1,(*\:9876\:70b91,\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxType2Str(*\:9876\:70b92,\:8026\:5408\:7cfb\:6570,\:5f3a\:4f5c\:7528\:90e8\:5206*)
],
fyCoeKeycEM->fyCoe[(* \:7535\:78c1\:6d41\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxType2EM
]
|>&
]@vtxJoinTmp1[fyTag];


fyTag={"KR","mes","oct","right"};
coeJoin[fyTag]=Join[
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[chTagKey["chTag"]->chTag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassIn->(#@inOct/.fd[a_,b_,0]:>massV@fd[a,b,2]),(* \:5165\:5c04\:91cd\:5b50\:8d28\:91cf*)
MassOct1->(#@medOct1/.fd[a_,b_,1]:>massV@fd[a,b,2]),(*\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf*)
MassMes1->(#@medMes1/.fd[a_,b_,0]:>massV@fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag],
2(*\:8fde\:63a5\:7b2c2\:5c42*)
];
(*serializeCoe[fyTag,coeJoin]*)


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*fyTag={"KR","mes","oct","right"};*)
(*Query[Cases@KeyValuePattern[*)
(*inOct->fd[2,2,0]*)
(*]]@coeJoin[fyTag]//dsetFmt*)


(* ::Section::Closed:: *)
(*Kroll-Ruderman,A-meson,octet,addition,left*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{end/4,0},{end/4,-end/5}}],*)
(*Disk[{end/4,0},.1],*)
(*Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end,-4delta}]*)
(*},ImageSize->Small]*)


(* \:989d\:5916 KR \:56fe\:7684\:7cfb\:6570\:548c \:666e\:901a KR \:56fe\:7684\:632f\:5e45\:662f\:76f8\:540c\:7684*)
fyTag={"KR","mes","oct","left"};
fyTagTmp={"KR","mes","oct","add","left"};
coeJoin[fyTagTmp]=Query[All,Append[chTagKey["chTag"]->chTag[fyTagTmp]]
]@coeJoin[fyTag];
(*serializeCoe[fyTagTmp,coeJoin]*)


(* ::Input:: *)
(*fyTag={"KR","mes","oct","left"};*)
(*Query[Cases@KeyValuePattern[*)
(*inOct->fd[2,1,0]*)
(*]]@coeJoin[fyTag]//dsetFmt*)


(* ::Section::Closed:: *)
(*Kroll-Ruderman, A-meson, octet,addition,right*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{3/4end,0},{3/4end,-end/5}}],*)
(*Disk[{3/4end,0},.1],*)
(*Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end+4delta,+4delta}]*)
(*},ImageSize->Small]*)


(* \:989d\:5916 KR \:56fe\:7684\:7cfb\:6570\:548c \:666e\:901a KR \:56fe\:7684\:7cfb\:6570\:76f8\:540c*)
fyTag={"KR","mes","oct","right"};
fyTagTmp={"KR","mes","oct","add","right"};
coeJoin[fyTagTmp]=Query[All,Append[chTagKey["chTag"]->chTag[fyTagTmp]]
]@coeJoin[fyTag];
(*serializeCoe[fyTagTmp,coeJoin]*)


fyTag={"KR","mes","oct","right"};
If[$inNBook,
Query[Cases@KeyValuePattern[
inOct->fd[2,1,0]
]]@coeJoin[fyTag]//dsetFmt]


(* ::Section::Closed:: *)
(*RainBow,A-octet,F1F2,nonlocal*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{end/2,-end/8},{end/2,0}}],*)
(*Text["v1",{end/4,-4delta}],Text["v2",{end/2,4delta}],Text["v3",{3/4end,-4delta}]*)
(*},ImageSize->Small]*)


(*\:8d39\:66fc\:56fe\:7684 chpt Tag\:ff0c\:4ee5\:53ca\:7528\:5230\:7684\:9876\:70b9v1,v2,v3, \:8fd9\:91cc\:987a\:4fbf\:8ba1\:7b97\:4e86 F2 *)
fyTag={"RB","oct","F1F2"};
vtxType1=vtxType["str","DF","mesOut"];       vtx1=Query[All,KeyMap@fyVtx1]@vtx[unq["type"->vtxType1]];
vtxType2=vtxType["F1F2","oct","nloc"];       vtx2=Query[All,KeyMap@fyVtx2]@vtx[unq["type"->vtxType2]];
vtxType3=vtxType["str","DF","mesIn"];        vtx3=Query[All,KeyMap@fyVtx3]@vtx[unq["type"->vtxType3]];
vtxF1=vtxType["F1","oct","nloc"];            vtxF2=vtxType["F2","oct","nloc"];
(*++++++++++++++++++++++\:8fde\:63a5\:5404\:4e2a\:9876\:70b9++++++++++++++++++++++*)
vtxJoin1[fyTag]=Query[All,(*\:4fee\:9970\:9876\:70b91*)
Append[#,{
(*\:9876\:70b91\:7684\:51fa\:5c04\:7c92\:5b50 -- octb\:ff0c\:7b49\:4e8e\:9876\:70b92\:7684\:5165\:5c04\:7c92\:5b50 -- oct. \:8fd9\:91cc\:6839\:636e\:9876\:70b91\:7684\:53cd\:7c92\:5b50\:751f\:6210\:9876\:70b92\:7684\:6b63\:7c92\:5b50*)
fyVtx2@fdTypeOct->(#@fyVtx1@fdTypeOctb/.fd[a_,b_,1]:>fd[a,b,0]),
(*\:9876\:70b91\:7684\:51fa\:5c04\:7c92\:5b50 -- mesOut\:ff0c\:7b49\:4e8e\:9876\:70b93\:7684\:5165\:5c04\:7c92\:5b50 -- mes *)
fyVtx3@fdTypeMes->(#@fyVtx1@fdTypeMesOut/.mesRule["reverse"]),
(*\:5bf9\:4e8e\[CapitalLambda]-\[CapitalSigma]0\:7c92\:5b50\:ff0c\:8fd8\:9700\:8981\:9650\:5b9a\:51fa\:5c04\:6001\:548c\:5165\:5c04\:6001\:76f8\:540c*)
fyVtx3@fdTypeOctb->(#@fyVtx1@fdTypeOct/.fd[a_,b_,0]:>fd[a,b,1])
}
]&
]@vtx1;
(*\:9876\:70b93\:7684\:5165\:5c04\:573a -- oct\:ff0c\:7b49\:4e8e\:9876\:70b92\:7684\:51fa\:5c04\:573a -- octb. \:8fd9\:91cc\:6839\:636e\:9876\:70b92\:7684\:53cd\:7c92\:5b50\:751f\:6210\:9876\:70b93\:7684\:6b63\:7c92\:5b50*)
vtxJoin2[fyTag]=Query[All,
Append[#,fyVtx3@fdTypeOct->(#@fyVtx2@fdTypeOctb/.fd[a_,b_,1]:>fd[a,b,0])]&
]@vtx2;(*\:9876\:70b92\:ff0c\:53cd\:5e38\:78c1\:77e9f1f2,\:975e\:5b9a\:57df\:5316*)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91\:ff0c2*)
vtxJoinTmp1[fyTag]=JoinAcross[
vtxJoin1@fyTag,(*\:9876\:70b91,\:4fee\:9970\:8fc7\:7684\:5f3a\:76f8\:4e92\:4f5c\:7528\:9876\:70b9\:ff0c\:52a0\:5165\:4e86\:9876\:70b92\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
vtxJoin2@fyTag,(*\:9876\:70b92\:ff0c\:4fee\:9970\:8fc7\:7684\:53cd\:5e38\:78c1\:77e9,\:52a0\:5165\:4e86\:9876\:70b93\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
{Key@fyVtx2@fdTypeOct}(*\:7c98\:8fde\:9876\:70b91\:ff0c2\:3002\:7c98\:8fde\:6761\:4ef6\:662f\:ff1a\:4ece\:9876\:70b91\:63a8\:5bfc\:7684\:5165\:5c04\:573a\[Equal]\:53cd\:5e38\:78c1\:77e9\:5b57\:6bb5\:4e2d\:7684\:5165\:5c04\:573a*)
,"Inner"
];
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91,2\:4e0e\:9876\:70b93*)
vtxJoinTmp2[fyTag]=JoinAcross[(*\:7c98\:8fde\:9876\:70b9(1,2)\:ff0c3 *)
vtxJoinTmp1@fyTag, (*\:4fee\:9970\:597d\:7684\:9876\:70b91,2*)
vtx3,(*\:9876\:70b93\:ff0c\:5f3a\:76f8\:4e92\:4f5c\:7528*)
{
Key@fyVtx3@fdTypeMes,(*\:9876\:70b93\:7684\:4ecb\:5b50\:573a\:ff0c\:7531\:9876\:70b91\:63a8\:5bfc\:51fa*)
Key@fyVtx3@fdTypeOctb,(*\:9876\:70b93\:7684\:51fa\:5c04\:573a\:ff0c\:8981\:6c42\:548c\:5165\:5c04\:573a\:4e00\:81f4*)
Key@fyVtx3@fdTypeOct(*\:7531\:9876\:70b92\:63a8\:5bfc\:7684oct\:6b63\:573a\:ff0c\:5339\:914d\:9876\:70b93\:7684oct \:6b63\:573a*)
}
,"Inner"
];
(*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:9009\:53d6\:9700\:8981\:7684\:5b57\:6bb5\:ff0c\:5e76\:6309\:7167\:987a\:5e8f\:6392\:5217*)
vtxJoin[fyTag]=Query[All,
<|
inOct->#@fyVtx1@fdTypeOct,(*\:9876\:70b91\:5165\:5c04\:573a*)
outOct->#@fyVtx3@fdTypeOctb,(*\:9876\:70b93 oct \:51fa\:5c04\:573a*)
medMes1->#@fyVtx3@fdTypeMes,(*\:9876\:70b93,mes,\:5165\:5c04\:573a*)
medOct1->#@fyVtx2@fdTypeOct,(*\:9876\:70b92 oct \:5165\:5c04\:573a*)
medOct2->#@fyVtx2@fdTypeOctb,(*\:9876\:70b92 oct \:51fa\:5c04\:573a*)
(*\:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef *)
fyCoeKeycAllF1->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx1@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType3,(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxF1(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570,F1 *)
],
fyCoeKeycAllF2->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx1@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType3,(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxF2(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570,F2 *)
],
fyCoeKeycStrF1->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx1@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType3(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycStrF2->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx1@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType3(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycEMF1->fyCoe[(* \:7535\:78c1\:6d41\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxF1(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570,F1 *)
],
fyCoeKeycEMF2->fyCoe[(* \:7535\:78c1\:6d41\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxF2(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570,F2 *)
]
|>&
]@vtxJoinTmp2[fyTag];


fyTag={"RB","oct","F1F2"};
coeJoin[fyTag]=Join[
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[chTagKey["chTag"]->chTag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassIn->(#@inOct/.fd[a_,b_,0]:>massV@fd[a,b,2]),(* \:5165\:5c04\:91cd\:5b50\:8d28\:91cf*)
MassOct1->(#@medOct1/.fd[a_,b_,0]:>massV@fd[a,b,2]),(*\:751f\:6210\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf\:9879*)
MassOct2->(#@medOct2/.fd[a_,b_,1]:>massV@fd[a,b,2]),
MassMes1->(#@medMes1/.fd[a_,b_,0]:>massV@fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag],
2(*\:8fde\:63a5\:7b2c2\:5c42*)
];
(*---------------------------- \:751f\:6210 F1 \:5bf9\:5e94\:7684\:7cfb\:6570 ----------------------------*)
fyTagTmp={"RB","oct","F1"};
coeJoin[fyTagTmp]=Query[All,
(*\:6dfb\:52a0 F1 chpt Tag *)
(Append[#,{
chTagKey["chTag"]->chTag[fyTagTmp],
fyCoeKeycAll->#@fyCoeKeycAllF1,
fyCoeKeycStr->#@fyCoeKeycStrF1,
fyCoeKeycEM->#@fyCoeKeycEMF1
}]&)/*
(*\:5220\:9664\:5197\:4f59\:7684\:8026\:5408\:7cfb\:6570\:4fe1\:606f*)
KeyDrop[{
fyCoeKeycAllF1,fyCoeKeycAllF2,
fyCoeKeycStrF1,fyCoeKeycStrF2,
fyCoeKeycEMF1,fyCoeKeycEMF2
}]
]@coeJoin[fyTag];
(*serializeCoe[fyTagTmp,coeJoin];*)
(*---------------------------- \:751f\:6210 F2 \:5bf9\:5e94\:7684\:7cfb\:6570 ----------------------------*)
fyTagTmp={"RB","oct","F2"};
coeJoin[fyTagTmp]=Query[All,
(Append[#,{
chTagKey["chTag"]->chTag[fyTagTmp],
fyCoeKeycAll->#@fyCoeKeycAllF2,
fyCoeKeycStr->#@fyCoeKeycStrF2,
fyCoeKeycEM->#@fyCoeKeycEMF2
}]&)/*
(*\:5220\:9664\:5197\:4f59\:7684\:8026\:5408\:7cfb\:6570\:4fe1\:606f*)
KeyDrop[{
fyCoeKeycAllF1,fyCoeKeycAllF2,
fyCoeKeycStrF1,fyCoeKeycStrF2,
fyCoeKeycEMF1,fyCoeKeycEMF2
}]
]@coeJoin[fyTag];
(*serializeCoe[fyTagTmp,coeJoin];*)


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*If[$inNBook,*)
(*fyTag={"RB","oct","F1F2"};*)
(*Query[Cases@KeyValuePattern[*)
(*inOct->fd[2,4,0]*)
(*]]@coeJoin[fyTag]//dsetFmt]*)


(* ::Section::Closed:: *)
(*tree-level,A-octet,F1F2,nonlocal*)


(* ::Input:: *)
(*(* \:6811\:56fe\:9636\:7684\:8d21\:732e,\:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Line[{{end/2,-end/8},{end/2,0}}],*)
(*Text["v2",{end/2,4delta}]*)
(*},ImageSize->Small]*)


(*\:8d39\:66fc\:56fe\:7684 chpt Tag,\:4ee5\:53ca\:7528\:5230\:7684\:9876\:70b9v1*)
fyTag={"tree","oct","F1F2"};
vtxType2=vtxType["F1F2","oct","nloc"];        vtx2=Query[All,KeyMap@fyVtx2]@vtx[unq["type"->vtxType2]];
vtxF1=vtxType["F1","oct","nloc"];             vtxF2=vtxType["F2","oct","nloc"];
vtxGE=vtxType["GE","oct","nloc"];             vtxGM=vtxType["GM","oct","nloc"];
(*++++++++++++++++++++++ \:9009\:53d6\:9700\:8981\:7684\:5b57\:6bb5,\:5e76\:6309\:7167\:987a\:5e8f\:6392\:5217 ++++++++++++++++++++++++++++++++++*)
vtxJoin[fyTag]=Query[All,
<|
inOct->#@fyVtx2@fdTypeOct,(*\:9876\:70b92 oct \:5165\:5c04\:573a*)
outOct->#@fyVtx2@fdTypeOctb,(*\:9876\:70b92 oct \:51fa\:5c04\:573a*)
(*--------------- \:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef ---------------*)
fyCoeKeycAllF1->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx2@vtxF1(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570,F1 *)
],
fyCoeKeycAllF2->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx2@vtxF2(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570,F2 *)
],
fyCoeKeyGE->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx2@vtxGE(* \:7535\:78c1\:9876\:70b9\:7684 GE *)
],
fyCoeKeyGM->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx2@vtxGM(* \:7535\:78c1\:9876\:70b9\:7684 GM *)
]
|>&
]@vtx2;
(*+++++++++++++++++++++++++++ \:6dfb\:52a0\:5176\:4ed6 tag ++++++++++++++++++++++++++++++++++++++++++*)
coeJoin[fyTag]=Join[
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[chTagKey["chTag"]->chTag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassIn->(#@inOct/.fd[a_,b_,0]:>massV@fd[a,b,2])(* \:5165\:5c04\:91cd\:5b50\:8d28\:91cf*)
|>&
]@vtxJoin[fyTag],
2(*\:8fde\:63a5\:7b2c2\:5c42*)
];
(*serializeCoe[fyTag,coeJoin];*)
(*---------------------------- \:751f\:6210 F1 \:5bf9\:5e94\:7684\:7cfb\:6570 ----------------------------*)
fyTagTmp={"tree","oct","F1"};
coeJoin[fyTagTmp]=Query[All,
(*\:6dfb\:52a0 F1 chpt Tag *)
(Append[#,{
chTagKey["chTag"]->chTag[fyTagTmp],
fyCoeKeycAll->#@fyCoeKeycAllF1
}]&)/*
(*\:5220\:9664\:5197\:4f59\:7684\:8026\:5408\:7cfb\:6570\:4fe1\:606f*)
KeyDrop[{
fyCoeKeycAllF1,fyCoeKeycAllF2,fyCoeKeyGM
}]]@coeJoin[fyTag];
(*serialize["coes"][coeJoin[fyTagTmp]<>".wdx",coeJoin[fyTagTmp]]*)
(*---------------------------- \:751f\:6210 F2 \:5bf9\:5e94\:7684\:7cfb\:6570 ----------------------------*)
fyTagTmp={"tree","oct","F2"};
coeJoin[fyTagTmp]=Query[All,
(Append[#,{
chTagKey["chTag"]->chTag[fyTagTmp],
fyCoeKeycAll->#@fyCoeKeycAllF2
}]&)/*
(*\:5220\:9664\:5197\:4f59\:7684\:8026\:5408\:7cfb\:6570\:4fe1\:606f*)
KeyDrop[{
fyCoeKeycAllF1,fyCoeKeycAllF2,fyCoeKeyGE
}]]@coeJoin[fyTag];
(*serialize["coes"][coeJoin[fyTagTmp]<>".wdx",coeJoin[fyTagTmp]]*)


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*If[$inNBook,*)
(*fyTag={"tree","oct","F1F2"};*)
(*coeJoin[fyTag]//dsetFmt]*)


(* ::Section:: *)
(*tadpole,A-octet,F1F2,nonlocal*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*If[$inNBook,Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Arrow@BezierCurve[{*)
(*{end/2,0},{0,end/2},{end,end/2},{end/2,0}*)
(*}],*)
(*Line[{{end/2,-end/6},{end/2,0}}],*)
(*Text["v1",{end/2,+5delta}]*)
(*},ImageSize->Small]]*)


(*\:8d39\:66fc\:56fe\:7684chpt tag,\:4ee5\:53ca\:7528\:5230\:7684\:9876\:70b9*)
fyTag={"tad","oct","F1F2"};
vtxType1=vtxType["F1F2","oct","o2","nloc"];  vtx1=Query[All,KeyMap@fyVtx1]@vtx[unq["type"->vtxType1]];
vtxF1=vtxType["F1","oct","o2","nloc"];       vtxF2=vtxType["F2","oct","o2","nloc"];
vtxF1Str=vtxType["F1","oct","o2","str"];     vtxF2Str=vtxType["F2","oct","o2","str"];
vtxF1EM=vtxType["F1","oct","o2","EM"];       vtxF2EM=vtxType["F2","oct","o2","EM"];
(* \:68c0\:9a8c\:9876\:70b9\:53d1\:51fa\:7684\:4e24\:4e2a\:4ecb\:5b50\:662f\:5426\:4e92\:4e3a\:6b63\:53cd\:7c92\:5b50, && \:521d\:672b\:6001\:91cd\:5b50\:662f\:5426\:76f8\:540c *)
sameLinesQ[rec_]:=SameQ[{rec@fyVtx1@fdTypeOct,rec@fyVtx1@fdTypeMes},
{rec@fyVtx1@fdTypeOctb/.fd[2,b_,1]:>fd[2,b,0],rec@fyVtx1@fdTypeMesOut/.mesRule["reverse"]}];
(*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:9009\:53d6\:9700\:8981\:7684\:5b57\:6bb5,\:5e76\:6309\:7167\:987a\:5e8f\:6392\:5217*)
vtxJoin[fyTag]=Query[Select@sameLinesQ,(*\:9009\:51fa\:76f8\:540c\:590d\:5408\:6761\:4ef6\:7684 record *)
<|
inOct->#@fyVtx1@fdTypeOct,(*\:9876\:70b91 oct \:5165\:5c04\:573a*)
outOct->#@fyVtx1@fdTypeOctb,(*\:9876\:70b91 oct \:51fa\:5c04\:573a*)
medMes1->#@fyVtx1@fdTypeMes,(*\:9876\:70b91,mes,\:5165\:5c04\:573a*)
medMes2->#@fyVtx1@fdTypeMesOut,
(*+++++++++++++++++++++++++++++ \:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef +++++++++++++++++++++++++++++*)
fyCoeKeycAllF1->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx1@vtxF1(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570,F1 *)
]
,fyCoeKeycAllF2->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx1@vtxF2(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570,F2 *)
]
,fyCoeKeycStrF1->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx1@vtxF1Str(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570,F1 *)
]
,fyCoeKeycStrF2->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx1@vtxF2Str(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570,F2 *)
]
,fyCoeKeycEMF1->fyCoe[(* \:7535\:78c1\:6d41\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx1@vtxF1/#@fyVtx1@vtxF1Str(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570,F1 *)
]
,fyCoeKeycEMF2->fyCoe[(* \:7535\:78c1\:6d41\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx1@vtxF2/#@fyVtx1@vtxF2Str(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570,F2 *)
]
|>&
]@vtx1;


(*++++++++++++++++++++++++++ \:63d0\:53d6\:7cfb\:6570  ++++++++++++++++++++++++++++++++++++++++++++++++*)
fyTag={"tad","oct","F1F2"};
coeJoin[fyTag]=Join[
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[chTagKey["chTag"]->chTag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassIn->(#@inOct/.fd[a_,b_,0]:>massV@fd[a,b,2]),(* \:5165\:5c04\:91cd\:5b50\:8d28\:91cf*)
MassMes1->(#@medMes1/.fd[a_,b_,0]:>massV@fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag],
2(*\:8fde\:63a5\:7b2c2\:5c42*)
];
(* \:751f\:6210 F1 \:5bf9\:5e94\:7684\:7cfb\:6570 ----------------------------*)
fyTagTmp={"tad","oct","F1"};
coeJoin[fyTagTmp]=Query[All,
(*\:6dfb\:52a0 F1 chpt Tag *)
(Append[#,{
chTagKey["chTag"]->chTag[fyTagTmp],
fyCoeKeycAll->#@fyCoeKeycAllF1,
fyCoeKeycStr->#@fyCoeKeycStrF1,
fyCoeKeycEM->#@fyCoeKeycEMF1
}]&)/*
(*\:5220\:9664\:5197\:4f59\:7684\:8026\:5408\:7cfb\:6570\:4fe1\:606f*)
KeyDrop[{
fyCoeKeycAllF1,fyCoeKeycAllF2,
fyCoeKeycStrF1,fyCoeKeycStrF2,
fyCoeKeycEMF1,fyCoeKeycEMF2
}]
]@coeJoin[fyTag];
(*serializeCoe[fyTagTmp,coeJoin];*)
(* \:751f\:6210 F2 \:5bf9\:5e94\:7684\:7cfb\:6570 -------------------------------*)
fyTagTmp={"tad","oct","F2"};
coeJoin[fyTagTmp]=Query[All,
(Append[#,{
chTagKey["chTag"]->chTag[fyTagTmp],
fyCoeKeycAll->#@fyCoeKeycAllF2,
fyCoeKeycStr->#@fyCoeKeycStrF2,
fyCoeKeycEM->#@fyCoeKeycEMF2
}]&)/*
(*\:5220\:9664\:5197\:4f59\:7684\:8026\:5408\:7cfb\:6570\:4fe1\:606f*)
KeyDrop[{
fyCoeKeycAllF1,fyCoeKeycAllF2,
fyCoeKeycStrF1,fyCoeKeycStrF2,
fyCoeKeycEMF1,fyCoeKeycEMF2
}]
]@coeJoin[fyTag];
(*serializeCoe[fyTagTmp,coeJoin];*)


(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)
If[$inNBook,
fyTag={"tad","oct","F1"};
Query[Cases@KeyValuePattern[
inOct->fd[2,1,0]
]]@coeJoin[fyTag]//dsetFmt]


(* ::Section:: *)
(*tadpole,A-octet,addition,o2,nonlocal*)


(* \:56fe\:5f62\:8868\:793a *)
If[$inNBook,
Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Arrow@BezierCurve[{
{end/2,0},{0,end/2},{end,end/2},{end/2,0}
}],
Line[{{end/2,-end/6},{end/2,0}}],
Disk[{end/2,0},0.1],
Text["v1",{end/2,+5delta}]
},ImageSize->Small]]


(* \:989d\:5916 tadpole \:56fe\:7684\:7cfb\:6570\:548c \:666e\:901a tadpole \:56fe\:7684\:7cfb\:6570\:76f8\:540c*)
fyTag={"tad","oct","F1"};
fyTagTmp={"tad","oct","F1","add"};
coeJoin[fyTagTmp]=Query[All,Append[chTagKey["chTag"]->chTag[fyTagTmp]]
]@coeJoin[fyTag];
(*serializeCoe[fyTagTmp,coeJoin]*)


(* ::Section:: *)
(*bubble, A-meson,order2*)


(* \:56fe\:5f62\:8868\:793a *)
If[$inNBook,
Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Arrow@BezierCurve[{
{end/2,0},{0,end/2},{end,end/2},{end/2,0}
}],Line[{{end/2,3/8*end},{end/2,end/2}}],
Text["v1",{end/2,-4delta}],Text["v2",{end/2,end/3-delta}]
},
ImageSize->Small]]


(*\:8d39\:66fc\:56fe\:7684 chpt Tag,\:4ee5\:53ca\:7528\:5230\:7684\:9876\:70b9v1,v2*)
fyTag={"bub","mes","o2"};
vtxType1=vtxType["str","BB\[Phi]\[Phi]"];   vtx1=Query[All,KeyMap@fyVtx1]@vtx[unq["type"->vtxType1]];
(*BB\[Phi]\[Phi]\:9876\:70b9\:4e2d\:6ca1\:6709\:4e0d\:540c\:7684\:4e24\:4e2a\:91cd\:5b50\:8026\:5408\:7684*)
vtxType2=vtxType["F1","\[Phi]\[Phi]A"];      vtx2=Query[All,KeyMap@fyVtx2]@vtx[unq["type"->vtxType2]];
(* \:68c0\:9a8c\:9876\:70b9\:53d1\:51fa\:7684\:4e24\:4e2a\:4ecb\:5b50\:662f\:5426\:4e92\:4e3a\:6b63\:53cd\:7c92\:5b50, && \:521d\:672b\:6001\:91cd\:5b50\:662f\:5426\:76f8\:540c--------------------*)
sameLinesQ[rec_]:=SameQ[{rec@fyVtx1@fdTypeOct,rec@fyVtx1@fdTypeMes},
{rec@fyVtx1@fdTypeOctb/.fd[2,b_,1]:>fd[2,b,0],rec@fyVtx1@fdTypeMesOut/.mesRule["reverse"]}];
(*++++++++++++++++++++++\:8fde\:63a5\:5404\:4e2a\:9876\:70b9++++++++++++++++++++++*)
vtxJoin1[fyTag]=Query[Select[sameLinesQ],(*\:4fee\:9970\:9876\:70b91*)
Append[#,{
(*\:9876\:70b91\:7684\:51fa\:5c04\:7c92\:5b50 -- mesout,\:7b49\:4e8e\:9876\:70b92\:7684\:5165\:5c04\:7c92\:5b50 -- mes. \:8fd9\:91cc\:6839\:636e\:9876\:70b91\:7684\:53cd\:7c92\:5b50\:751f\:6210\:9876\:70b92\:7684\:6b63\:7c92\:5b50*)
fyVtx2@fdTypeMes->(#@fyVtx1@fdTypeMesOut/.mesRule["reverse"])
}
]&
]@vtx1;
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91,2*)
vtxJoinTmp1[fyTag]=JoinAcross[
vtxJoin1[fyTag],(*\:9876\:70b91,\:4fee\:9970\:8fc7\:7684\:5f3a\:76f8\:4e92\:4f5c\:7528\:9876\:70b9,\:52a0\:5165\:4e86\:9876\:70b92\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
vtx2,(*\:9876\:70b92,\:4ecb\:5b50\:7535\:78c1\:6d41\:76f8\:4e92\:4f5c\:7528*)
{Key@fyVtx2@fdTypeMes}(*\:7c98\:8fde\:9876\:70b91,2\:3002\:7c98\:8fde\:6761\:4ef6\:662f\:ff1a\:4ece\:9876\:70b91\:63a8\:5bfc\:7684\:5165\:5c04\:573a\[Equal]\:53cd\:5e38\:78c1\:77e9\:5b57\:6bb5\:4e2d\:7684\:5165\:5c04\:573a*)
,"Inner"];
(*++++++++++++++++++++++ \:9009\:53d6\:9700\:8981\:7684\:5b57\:6bb5,\:5e76\:6309\:7167\:987a\:5e8f\:6392\:5217  ++++++++++++++++++++++*)
vtxJoin[fyTag]=Query[All,
<|
inOct->#@fyVtx1@fdTypeOct,(*\:9876\:70b91, oct,\:5165\:5c04\:573a*)
outOct->#@fyVtx1@fdTypeOctb,(*\:9876\:70b91,oct,\:51fa\:5c04\:573a*)
medMes1->#@fyVtx2@fdTypeMes,(*\:9876\:70b92,mes,\:5165\:5c04\:573a*)
medMes2->#@fyVtx2@fdTypeMesOut,(*\:9876\:70b91,mes,\:51fa\:5c04\:573a*)
(*\:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef *)
fyCoeKeycAll->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx1@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxType2(*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycStr->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx1@vtxType1(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycEM->fyCoe[(* \:7535\:78c1\:6d41\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxType2(*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
]
|>&
]@vtxJoinTmp1[fyTag];


fyTag={"bub","mes","o2"};
coeJoin[fyTag]=Join[
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[chTagKey["chTag"]->chTag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassIn->(#@inOct/.fd[a_,b_,0]:>massV@fd[a,b,2]),(* \:5165\:5c04\:91cd\:5b50\:8d28\:91cf*)
MassMes1->(#@medMes1/.fd[a_,b_,0]:>massV@fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag],
2(*\:8fde\:63a5\:7b2c2\:5c42*)
];
(*serializeCoe[fyTag,coeJoin]*)


(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)
If[$inNBook,
fyTag={"bub","mes","o2"};
Query[Cases@KeyValuePattern[
inOct->fd[2,4,0]]
]@coeJoin[fyTag]//dsetFmt]


(* ::Section:: *)
(*bubble,A-meson,tensor,order 2*)


(* \:56fe\:5f62\:8868\:793a *)
If[$inNBook,
Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Arrow@BezierCurve[{
{end/2,0},{0,end/2},{end,end/2},{end/2,0}
}],Line[{{end/2,3/8*end},{end/2,end/2}}],
Rectangle[{end/2-1.5delta,-1.5delta},{end/2+1.5delta,1.5delta}],
Text["v1",{end/2,-4delta}],Text["v2",{end/2,end/3-delta}]
},
ImageSize->Small]]


(*\:8d39\:66fc\:56fe\:7684 chpt Tag,\:4ee5\:53ca\:7528\:5230\:7684\:9876\:70b9v1,v2*)
fyTag={"bub","mes","ten","o2"};
vtxType1=vtxType["str","ten"];  vtx1=Query[All,KeyMap@fyVtx1]@vtx[unq["type"->vtxType1]];
(*BB\[Phi]\[Phi]\:9876\:70b9\:4e2d\:6ca1\:6709\:4e0d\:540c\:7684\:4e24\:4e2a\:91cd\:5b50\:8026\:5408\:7684*)
vtxType2=vtxType["F1","\[Phi]\[Phi]A"];    vtx2=Query[All,KeyMap@fyVtx2]@vtx[unq["type"->vtxType2]];
(* \:68c0\:9a8c\:9876\:70b9\:53d1\:51fa\:7684\:4e24\:4e2a\:4ecb\:5b50\:662f\:5426\:4e92\:4e3a\:6b63\:53cd\:7c92\:5b50, && \:521d\:672b\:6001\:91cd\:5b50\:662f\:5426\:76f8\:540c *)
sameLinesQ[rec_]:=SameQ[{rec@fyVtx1@fdTypeOct,rec@fyVtx1@fdTypeMes},
{rec@fyVtx1@fdTypeOctb/.fd[2,b_,1]:>fd[2,b,0],rec@fyVtx1@fdTypeMesOut/.mesRule["reverse"]}];
(*++++++++++++++++++++++\:8fde\:63a5\:5404\:4e2a\:9876\:70b9++++++++++++++++++++++*)
vtxJoin1[fyTag]=Query[Select[sameLinesQ],(*\:4fee\:9970\:9876\:70b91*)
Append[#,{
(* \:9876\:70b91\:7684\:51fa\:5c04\:7c92\:5b50 -- mesOut,\:7b49\:4e8e\:9876\:70b92\:7684\:5165\:5c04\:7c92\:5b50 -- mes. *)
fyVtx2@fdTypeMes->(#@fyVtx1@fdTypeMesOut/.mesRule["reverse"]),
(* \:7c7b\:4f3c\:5730,\:9650\:5236\:53e6\:4e00\:4e2a\:9876\:70b9. *)
fyVtx2@fdTypeMesOut->(#@fyVtx1@fdTypeMes/.mesRule["reverse"])
}
]&
]@vtx1;
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91,2*)
vtxJoinTmp1[fyTag]=JoinAcross[
vtxJoin1[fyTag],(*\:9876\:70b91,\:4fee\:9970\:8fc7\:7684\:5f3a\:76f8\:4e92\:4f5c\:7528\:9876\:70b9,\:52a0\:5165\:4e86\:9876\:70b92\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
vtx2,(*\:9876\:70b92,\:4ecb\:5b50\:7535\:78c1\:6d41\:76f8\:4e92\:4f5c\:7528*)
{Key@fyVtx2@fdTypeMes,Key@fyVtx2@fdTypeMesOut}(*\:7c98\:8fde\:9876\:70b91,2\:3002\:7c98\:8fde\:6761\:4ef6\:662f\:ff1a\:4ece\:9876\:70b91\:63a8\:5bfc\:7684\:5165\:5c04\:573a\[Equal]\:53cd\:5e38\:78c1\:77e9\:5b57\:6bb5\:4e2d\:7684\:5165\:5c04\:573a*)
,"Inner"];
(*+++++++++++++++++++++++++++++++++ \:9009\:53d6\:9700\:8981\:7684\:5b57\:6bb5,\:5e76\:6309\:7167\:987a\:5e8f\:6392\:5217 ++++++++++++++++++++++++++++++++*)
vtxJoin[fyTag]=Query[All,
<|
inOct->#@fyVtx1@fdTypeOct,(*\:9876\:70b91,oct,\:5165\:5c04\:573a*)
outOct->#@fyVtx1@fdTypeOctb,(*\:9876\:70b91,oct,\:51fa\:5c04\:573a*)
medMes1->#@fyVtx2@fdTypeMes,(*\:9876\:70b92,mes,\:5165\:5c04\:573a*)
medMes2->#@fyVtx2@fdTypeMesOut,
(*\:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef *)
fyCoeKeycAll->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx1@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxType2(*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycStr->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx1@vtxType1(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycEM->fyCoe[(* \:7535\:78c1\:6d41\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxType2(*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
]
|>&
]@vtxJoinTmp1[fyTag];


fyTag={"bub","mes","ten","o2"};
coeJoin[fyTag]=Join[
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[chTagKey["chTag"]->chTag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassIn->(#@inOct/.fd[a_,b_,0]:>massV@fd[a,b,2]),(* \:5165\:5c04\:91cd\:5b50\:8d28\:91cf*)
MassMes1->(#@medMes1/.fd[a_,b_,0]:>massV@fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag],
2(*\:8fde\:63a5\:7b2c2\:5c42*)
];
(*serializeCoe[fyTag,coeJoin]*)


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*If[$inNBook,*)
(*fyTag={"bub","mes","ten","o2"};*)
(*Query[Cases@KeyValuePattern[*)
(*inOct->fd[2,4,0]*)
(*]]@coeJoin[fyTag]//dsetFmt]*)


(* ::Section:: *)
(*RainBow,A-meson,decuplet mediate*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Line[{{end/4,delta},{3/4end,delta}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{end/2,end/4},{end/2,end/2.4}}],*)
(*Text["v1",{end/4,-4delta}],Text["v2",{end/2,end/4-4delta}],Text["v3",{3/4end,-4delta}]*)
(*},ImageSize->Small]*)


(* \:8d39\:66fc\:56fe\:7684chpt Tag,\:4ee5\:53ca\:7528\:5230\:7684\:9876\:70b9 v1,v2,v3, \:4ece\:5de6\:5230\:53f3\:6392\:5217 *)
fyTag={"RB","mes","dec"};
vtxType1=vtxType["str","C","mesOut"];    vtx1=Query[All,KeyMap@fyVtx1]@vtx[unq["type"->vtxType1]];
vtxType2=vtxType["F1","\[Phi]\[Phi]A"];             vtx2=Query[All,KeyMap@fyVtx2]@vtx[unq["type"->vtxType2]];
vtxType3=vtxType["str","C","mesIn"];     vtx3=Query[All,KeyMap@fyVtx3]@vtx[unq["type"->vtxType3]];
(*++++++++++++++++++++++++++++++++++++++++++++++++++*)
vtxJoin1[fyTag]=Query[All,(*\:4fee\:9970\:9876\:70b91*)
Append[#,{
(*\:9876\:70b91\:7684\:51fa\:5c04\:573a -- mesOut,\:7b49\:4e8e\:9876\:70b92\:7684\:5165\:5c04\:573a -- mes *)
fyVtx2@fdTypeMes->(#@fyVtx1@fdTypeMesOut/.mesRule["reverse"]),
(*\:9876\:70b91\:7684\:51fa\:5c04\:573a -- decb,\:7b49\:4e8e\:9876\:70b93\:7684\:5165\:5c04\:573a -- dec *)
fyVtx3@fdTypeDec->(#@fyVtx1@fdTypeDecb/.fd[a_,b_,1]:>fd[a,b,0]),
(*\:5bf9\:4e8e \[CapitalLambda]-\[CapitalSigma]0 \:7c92\:5b50,\:8fd8\:9700\:8981\:9650\:5b9a\:51fa\:5c04\:6001\:548c\:5165\:5c04\:6001\:76f8\:540c*)
fyVtx3@fdTypeOctb->(#@fyVtx1@fdTypeOct/.fd[a_,b_,0]:>fd[a,b,1])
}
]&
]@vtx1;
(*\:9876\:70b92\:51fa\:5c04\:573a -- mesOut, \:7b49\:4e8e\:9876\:70b93\:7684\:5165\:5c04\:573a -- mes *)
vtxJoin2[fyTag]=Query[All,
Append[#,fyVtx3@fdTypeMes->(#@fyVtx2@fdTypeMesOut/.mesRule["reverse"])]&
]@vtx2;(*\:9876\:70b92,\:53cd\:5e38\:78c1\:77e9\:975e\:5b9a\:57df\:5316*)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91,2*)
vtxJoinTmp1[fyTag]=JoinAcross[
vtxJoin1@fyTag,(*\:9876\:70b91,\:4fee\:9970\:8fc7\:7684\:5f3a\:76f8\:4e92\:4f5c\:7528\:9876\:70b9,\:52a0\:5165\:4e86\:9876\:70b92\:51fa\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
vtxJoin2@fyTag,(*\:9876\:70b92,\:4fee\:9970\:8fc7\:7684\:4ecb\:5b50\:7535\:78c1\:6d41,\:52a0\:5165\:4e86\:9876\:70b93\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
{Key@fyVtx2@fdTypeMes}(*\:7c98\:8fde\:9876\:70b91,2.\:6761\:4ef6\:662f\:ff1a\:4ece\:9876\:70b91\:63a8\:5bfc\:7684\:51fa\:5c04mes out\[Equal]\:4ecb\:5b50\:7535\:78c1\:6d41\:7684\:51fa\:5c04mesb*)
,"Inner"];
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91,2\:4e0e\:9876\:70b93*)
vtxJoinTmp2[fyTag]=JoinAcross[(*\:7c98\:8fde\:9876\:70b9(1,2),3 *)
vtxJoinTmp1[fyTag], (*\:4fee\:9970\:597d\:7684\:9876\:70b91,2*)
vtx3,(*\:9876\:70b93,\:5f3a\:76f8\:4e92\:4f5c\:7528*)
{
Key@fyVtx3@fdTypeDec,(*\:9876\:70b91\:7684oct\:6b63\:573a==\:9876\:70b93\:7684oct\:573a*)
Key@fyVtx3@fdTypeOctb,(*\:9876\:70b93\:7684\:51fa\:5c04\:573a,\:8981\:6c42\:548c\:5165\:5c04\:573a\:4e00\:81f4*)
Key@fyVtx3@fdTypeMes(*\:7531\:9876\:70b92\:63a8\:5bfc\:7684mes\:6b63\:573a==\:9876\:70b93\:7684mes\:6b63\:573a*)
}
,"Inner"];
(*++++++++++++++++++++++++++ \:9009\:53d6\:9700\:8981\:7684\:5b57\:6bb5,\:5e76\:6309\:7167\:987a\:5e8f\:6392\:5217 ++++++++++++++++++++++++++++++++++*)
vtxJoin[fyTag]=Query[All,
<|
inOct->#@fyVtx1@fdTypeOct,(*\:9876\:70b91,oct,\:5165\:5c04\:573a*)
outOct->#@fyVtx3@fdTypeOctb,(*\:9876\:70b93,oct,\:51fa\:5c04\:573a*)
medDec1->#@fyVtx1@fdTypeDecb,(*\:9876\:70b91,dec,\:51fa\:5c04\:573a*)
medMes1->#@fyVtx2@fdTypeMes,(*\:9876\:70b92,mes,\:5165\:5c04\:573a*)
(*\:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef *)
fyCoeKeycAll->fyCoe[(*\:6240\:6709\:8026\:5408\:7cfb\:6570\:4e58\:79ef*)
#@fyVtx1@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType3,(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxType2(*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycStr->fyCoe[(*\:5f3a\:76f8\:4e92\:4f5c\:7528\:8026\:5408\:7cfb\:6570\:7684\:4e58\:79ef*)
#@fyVtx1@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType3(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycEM->fyCoe[(* \:7535\:78c1\:6d41\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxType2(*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
]
|>&
]@vtxJoinTmp2[fyTag];


fyTag={"RB","mes","dec"};
coeJoin[fyTag]=Join[
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[chTagKey["chTag"]->chTag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassIn->(#@inOct/.fd[a_,b_,0]:>massV@fd[a,b,2]),(* \:5165\:5c04\:91cd\:5b50\:8d28\:91cf*)
MassDec1->(#@medDec1/.fd[a_,b_,1]:>massV@fd[a,b,2]),(*\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf*)
MassMes1->(#@medMes1/.fd[a_,b_,0]:>massV@fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag],
2(*\:8fde\:63a5\:7b2c2\:5c42*)
];
(*serializeCoe[fyTag,coeJoin]*)


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*If[$inNBook,*)
(*fyTag={"RB","mes","dec"};*)
(*Query[Cases@KeyValuePattern[*)
(*inOct->fd[2,1,0]*)
(*]]@coeJoin[fyTag]//dsetFmt]*)


(* ::Section:: *)
(*RainBow,A-decuplet,F1F2,nonlocal*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{end/2,-end/8},{end/2,0}}],*)
(*Line[{{end/4,delta},{3/4end,delta}}],*)
(*Text["v1",{end/4,-4delta}],Text["v2",{end/2,4delta}],Text["v3",{3/4end,-4delta}]*)
(*},ImageSize->Small]*)


(*\:8d39\:66fc\:56fe\:7684 chpt Tag,\:4ee5\:53ca\:7528\:5230\:7684\:9876\:70b9v1,v2,v3*)
fyTag={"RB","dec","F1F2"};
vtxType1=vtxType["str","C","mesOut"];   vtx1=Query[All,KeyMap@fyVtx1]@vtx[unq["type"->vtxType1]];
vtxType2=vtxType["F1F2","dec","nloc"];   vtx2=Query[All,KeyMap@fyVtx2]@vtx[unq["type"->vtxType2]];(*\:53cd\:5e38\:78c1\:77e9*)
vtxType3=vtxType["str","C","mesIn"];    vtx3=Query[All,KeyMap@fyVtx3]@vtx[unq["type"->vtxType3]];
vtxF1=vtxType["F1","dec","nloc"];        vtxF2=vtxType["F2","dec","nloc"];
(*++++++++++++++++++++++\:8fde\:63a5\:5404\:4e2a\:9876\:70b9++++++++++++++++++++++*)
vtxJoin1[fyTag]=Query[All,(*\:4fee\:9970\:9876\:70b91*)
Append[#,{
(*\:9876\:70b93\:7684\:5165\:5c04\:7c92\:5b50 -- mes, \:7b49\:4e8e\:9876\:70b91\:7684\:51fa\:5c04\:7c92\:5b50 mesOut *)
fyVtx3@fdTypeMes->(#@fyVtx1@fdTypeMesOut/.mesRule["reverse"]),
(*\:9876\:70b91\:7684\:51fa\:5c04\:7c92\:5b50 -- decb,\:7b49\:4e8e\:9876\:70b92\:7684\:5165\:5c04\:7c92\:5b50 -- dec. \:8fd9\:91cc\:6839\:636e\:9876\:70b91\:7684\:53cd\:7c92\:5b50\:751f\:6210\:9876\:70b92\:7684\:6b63\:7c92\:5b50*)
fyVtx2@fdTypeDec->(#@fyVtx1@fdTypeDecb/.fd[a_,b_,1]:>fd[a,b,0]),
(*\:5bf9\:4e8e\[CapitalLambda]-\[CapitalSigma]0 \:7c7b\:578b\:8026\:5408,\:8fd8\:9700\:8981\:9650\:5b9a\:51fa\:5c04\:6001\:548c\:5165\:5c04\:6001\:76f8\:540c*)
fyVtx3@fdTypeOctb->(#@fyVtx1@fdTypeOct/.fd[a_,b_,0]:>fd[a,b,1])
}
]&
]@vtx1;
(*\:9876\:70b93\:7684\:5165\:5c04\:7c92\:5b50 -- dec, \:7b49\:4e8e\:9876\:70b92\:7684\:51fa\:5c04\:7c92\:5b50 -- decb. *)
vtxJoin2[fyTag]=Query[All,
Append[#,fyVtx3@fdTypeDec->(#@fyVtx2@fdTypeDecb/.fd[a_,b_,1]:>fd[a,b,0])]&
]@vtx2;(*\:9876\:70b92,\:53cd\:5e38\:78c1\:77e9f1f2,\:975e\:5b9a\:57df\:5316*)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91,2*)
vtxJoinTmp1[fyTag]=JoinAcross[
vtxJoin1[fyTag],(*\:9876\:70b91,\:4fee\:9970\:8fc7\:7684\:5f3a\:76f8\:4e92\:4f5c\:7528\:9876\:70b9,\:52a0\:5165\:4e86\:9876\:70b92\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
vtxJoin2[fyTag],(*\:9876\:70b92,\:4fee\:9970\:8fc7\:7684\:53cd\:5e38\:78c1\:77e9,\:52a0\:5165\:4e86\:9876\:70b93\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
{Key@fyVtx2@fdTypeDec}(*\:7c98\:8fde\:9876\:70b91,2. \:7c98\:8fde\:6761\:4ef6\:662f\:ff1a\:4ece\:9876\:70b91\:63a8\:5bfc\:7684\:5165\:5c04\:573a\[Equal]\:53cd\:5e38\:78c1\:77e9\:5b57\:6bb5\:4e2d\:7684\:5165\:5c04\:573a*)
,"Inner"(*\:4e0d\:9700\:8981 KeyCollisionFunction \:51fd\:6570*)
];
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91,2\:4e0e\:9876\:70b93*)
vtxJoinTmp2[fyTag]=JoinAcross[(*\:7c98\:8fde\:9876\:70b9(1,2),3 *)
vtxJoinTmp1[fyTag], (*\:4fee\:9970\:597d\:7684\:9876\:70b91,2*)
vtx3,(*\:9876\:70b93,\:5f3a\:76f8\:4e92\:4f5c\:7528*)
{
Key@fyVtx3@fdTypeMes,(*\:9876\:70b91\:7684\:4ecb\:5b50\:53cd\:573a,\:5339\:914d\:9876\:70b93\:7684\:4ecb\:5b50\:6b63\:573a*)
Key@fyVtx3@fdTypeDec,(*\:7531\:9876\:70b92\:63a8\:5bfc\:7684Dec,\:5339\:914d\:9876\:70b93\:7684Dec*)
Key@fyVtx3@fdTypeOctb(*\:9876\:70b93\:7684\:51fa\:5c04\:573a,\:8981\:6c42\:548c\:5165\:5c04\:573a\:4e00\:81f4*)
}
,"Inner"
];
(*++++++++++++++++++ \:9009\:53d6\:9700\:8981\:7684\:5b57\:6bb5,\:5e76\:6309\:7167\:987a\:5e8f\:6392\:5217 ++++++++++++++++++++++++++++++++++++++++*)
vtxJoin[fyTag]=Query[All,
<|
inOct->#@fyVtx1@fdTypeOct,(*\:9876\:70b91,oct,\:5165\:5c04\:573a*)
outOct->#@fyVtx3@fdTypeOctb,(*\:9876\:70b93,oct,\:51fa\:5c04\:573a*)
medMes1->#@fyVtx3@fdTypeMes,(*\:9876\:70b93,mes,\:5165\:5c04\:573a*)
medDec1->#@fyVtx2@fdTypeDec,(*\:9876\:70b92,dec,\:5165\:5c04\:573a*)
(*\:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef *)
fyCoeKeycAllF1->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx1@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType3,(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxF1(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570,F1 *)
],
fyCoeKeycAllF2->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx1@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType3,(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxF2(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570,F2 *)
],
fyCoeKeycStrF1->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx1@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType3(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycStrF2->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx1@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType3(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycEMF1->fyCoe[(* \:7535\:78c1\:6d41\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxF1(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570,F1 *)
],
fyCoeKeycEMF2->fyCoe[(* \:7535\:78c1\:6d41\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxF2(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570,F2 *)
]
|>&
]@vtxJoinTmp2[fyTag];


fyTag={"RB","dec","F1F2"};
coeJoin[fyTag]=Join[
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[chTagKey["chTag"]->chTag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassIn->(#@inOct/.fd[a_,b_,0]:>massV@fd[a,b,2]),(* \:5165\:5c04\:91cd\:5b50\:8d28\:91cf*)
MassDec1->(#@medDec1/.fd[a_,b_,0]:>massV@fd[a,b,2]),(*\:751f\:6210\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf\:9879*)
MassMes1->(#@medMes1/.fd[a_,b_,0]:>massV@fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag],
2(*\:8fde\:63a5\:7b2c2\:5c42*)
];
(*---------------------------- \:751f\:6210 F1 \:5bf9\:5e94\:7684\:7cfb\:6570 ----------------------------*)
fyTagTmp={"RB","dec","F1"};
coeJoin[fyTagTmp]=Query[All,
(*\:6dfb\:52a0 F1 chpt Tag *)
(Append[#,{
chTagKey["chTag"]->chTag[fyTagTmp],
fyCoeKeycAll->#@fyCoeKeycAllF1,
fyCoeKeycStr->#@fyCoeKeycStrF1,
fyCoeKeycEM->#@fyCoeKeycEMF1
}]&)/*
(*\:5220\:9664\:5197\:4f59\:7684\:8026\:5408\:7cfb\:6570\:4fe1\:606f*)
KeyDrop[{
fyCoeKeycAllF1,fyCoeKeycAllF2,
fyCoeKeycStrF1,fyCoeKeycStrF2,
fyCoeKeycEMF1,fyCoeKeycEMF2
}]
]@coeJoin[fyTag];
(*serializeCoe[fyTagTmp,coeJoin];*)
(*---------------------------- \:751f\:6210 F2 \:5bf9\:5e94\:7684\:7cfb\:6570 ----------------------------*)
fyTagTmp={"RB","dec","F2"};
coeJoin[fyTagTmp]=Query[All,
(Append[#,{
chTagKey["chTag"]->chTag[fyTagTmp],
fyCoeKeycAll->#@fyCoeKeycAllF2,
fyCoeKeycStr->#@fyCoeKeycStrF2,
fyCoeKeycEM->#@fyCoeKeycEMF2
}]&)/*
(*\:5220\:9664\:5197\:4f59\:7684\:8026\:5408\:7cfb\:6570\:4fe1\:606f*)
KeyDrop[{
fyCoeKeycAllF1,fyCoeKeycAllF2,
fyCoeKeycStrF1,fyCoeKeycStrF2,
fyCoeKeycEMF1,fyCoeKeycEMF2
}]
]@coeJoin[fyTag];
(*serializeCoe[fyTagTmp,coeJoin];*)


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*fyTag={"RB","dec","F1F2"};*)
(*Query[Cases@KeyValuePattern[*)
(*inOct->fd[2,1,0]*)
(*]]@coeJoin[fyTag]//dsetFmt*)


(* ::Section:: *)
(*RainBow,A-decuplet,trans,left*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{end/2,-end/8},{end/2,0}}],*)
(*Line[{{end/4,delta},{1/2end,delta}}],*)
(*Text["v1",{end/4,-4delta}],Text["v2",{end/2,4delta}],Text["v3",{3/4end,-4delta}]*)
(*},ImageSize->Small]*)


(*\:8d39\:66fc\:56fe\:7684 chpt Tag,\:4ee5\:53ca\:7528\:5230\:7684\:9876\:70b9v1,v2,v3*)
fyTag={"RB","trans","left"};
vtxType1=vtxType["str","C","mesOut"];   vtx1=Query[All,KeyMap@fyVtx1]@vtx[unq["type"->vtxType1]];
vtxType2=vtxType["F2","tran","octOut"];   vtx2=Query[All,KeyMap@fyVtx2]@vtx[unq["type"->vtxType2]];(*\:8f6c\:79fb\:78c1\:77e9*)
vtxType3=vtxType["str","DF","mesIn"];   vtx3=Query[All,KeyMap@fyVtx3]@vtx[unq["type"->vtxType3]];
(*++++++++++++++++++++++\:8fde\:63a5\:5404\:4e2a\:9876\:70b9++++++++++++++++++++++*)
vtxJoin1[fyTag]=Query[All,(*\:4fee\:9970\:9876\:70b91*)
Append[#,{
(*\:9876\:70b93\:7684\:5165\:5c04\:7c92\:5b50 -- mes, \:7b49\:4e8e\:9876\:70b91\:7684\:51fa\:5c04\:7c92\:5b50 mesOut *)
fyVtx3@fdTypeMes->(#@fyVtx1@fdTypeMesOut/.mesRule["reverse"]),
(*\:9876\:70b91\:7684\:51fa\:5c04\:7c92\:5b50 -- decb,\:7b49\:4e8e\:9876\:70b92\:7684\:5165\:5c04\:7c92\:5b50 -- dec. \:8fd9\:91cc\:6839\:636e\:9876\:70b91\:7684\:53cd\:7c92\:5b50\:751f\:6210\:9876\:70b92\:7684\:6b63\:7c92\:5b50*)
fyVtx2@fdTypeDec->(#@fyVtx1@fdTypeDecb/.fd[a_,b_,1]:>fd[a,b,0]),
(*\:5bf9\:4e8e\[CapitalLambda]-\[CapitalSigma]0 \:7c7b\:578b\:8026\:5408,\:8fd8\:9700\:8981\:9650\:5b9a\:51fa\:5c04\:6001\:548c\:5165\:5c04\:6001\:76f8\:540c*)
fyVtx3@fdTypeOctb->(#@fyVtx1@fdTypeOct/.fd[a_,b_,0]:>fd[a,b,1])
}
]&
]@vtx1;
(*\:9876\:70b93\:7684\:5165\:5c04\:7c92\:5b50 -- dec, \:7b49\:4e8e\:9876\:70b92\:7684\:51fa\:5c04\:7c92\:5b50 -- decb . *)
vtxJoin2[fyTag]=Query[All,
Append[#,fyVtx3@fdTypeOct->(#@fyVtx2@fdTypeOctb/.fd[a_,b_,1]:>fd[a,b,0])]&
]@vtx2;(*\:9876\:70b92,\:53cd\:5e38\:78c1\:77e9f1f2,\:975e\:5b9a\:57df\:5316*)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91,2*)
vtxJoinTmp1[fyTag]=JoinAcross[
vtxJoin1[fyTag],(*\:9876\:70b91,\:4fee\:9970\:8fc7\:7684\:5f3a\:76f8\:4e92\:4f5c\:7528\:9876\:70b9,\:52a0\:5165\:4e86\:9876\:70b92\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
vtxJoin2[fyTag],(*\:9876\:70b92,\:4fee\:9970\:8fc7\:7684\:53cd\:5e38\:78c1\:77e9,\:52a0\:5165\:4e86\:9876\:70b93\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
{Key@fyVtx2@fdTypeDec}(*\:7c98\:8fde\:9876\:70b91,2. \:7c98\:8fde\:6761\:4ef6\:662f\:ff1a\:4ece\:9876\:70b91\:63a8\:5bfc\:7684\:5165\:5c04\:573a\[Equal]\:53cd\:5e38\:78c1\:77e9\:5b57\:6bb5\:4e2d\:7684\:5165\:5c04\:573a*)
,"Inner"(*\:4e0d\:9700\:8981 KeyCollisionFunction \:51fd\:6570*)
];
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91,2\:4e0e\:9876\:70b93*)
vtxJoinTmp2[fyTag]=JoinAcross[(*\:7c98\:8fde\:9876\:70b9(1,2),3 *)
vtxJoinTmp1[fyTag], (*\:4fee\:9970\:597d\:7684\:9876\:70b91,2*)
vtx3,(*\:9876\:70b93,\:5f3a\:76f8\:4e92\:4f5c\:7528*)
{
Key@fyVtx3@fdTypeMes,(*\:9876\:70b91\:7684\:4ecb\:5b50\:53cd\:573a,\:5339\:914d\:9876\:70b93\:7684\:4ecb\:5b50\:6b63\:573a*)
Key@fyVtx3@fdTypeOctb,(*\:9876\:70b93\:7684\:51fa\:5c04\:573a,\:8981\:6c42\:548c\:5165\:5c04\:573a\:4e00\:81f4*)
Key@fyVtx3@fdTypeOct(*\:7531\:9876\:70b92\:63a8\:5bfc\:7684Dec,\:5339\:914d\:9876\:70b93\:7684Dec*)
}
,"Inner"
];
(*+++++++++++++++++++++++++ \:9009\:53d6\:9700\:8981\:7684\:5b57\:6bb5,\:5e76\:6309\:7167\:987a\:5e8f\:6392\:5217 ++++++++++++++++++++++++++++++++++++*)
vtxJoin[fyTag]=Query[All,
<|
inOct->#@fyVtx1@fdTypeOct,(*\:9876\:70b91,oct,\:5165\:5c04\:573a*)
outOct->#@fyVtx3@fdTypeOctb,(*\:9876\:70b93,oct,\:51fa\:5c04\:573a*)
medMes1->#@fyVtx3@fdTypeMes,(*\:9876\:70b93,mes,\:5165\:5c04\:573a*)
medDec1->#@fyVtx2@fdTypeDec,(*\:9876\:70b92,dec,\:5165\:5c04\:573a*)
medOct1->#@fyVtx2@fdTypeOctb,(*\:9876\:70b92,oct,\:51fa\:5c04\:573a*)
(*\:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef *)
fyCoeKeycAll->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx1@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType3,(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxType2(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570, \:53cd\:5e38\:78c1\:77e9 *)
],
fyCoeKeycStr->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx1@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType3(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycEM->fyCoe[(* \:7535\:78c1\:6d41\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxType2(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570, \:53cd\:5e38\:78c1\:77e9 *)
]
|>&
]@vtxJoinTmp2[fyTag];


fyTag={"RB","trans","left"};
coeJoin[fyTag]=Query[All,KeyDrop[(*\:5220\:9664\:5197\:4f59\:7684\:8026\:5408\:7cfb\:6570\:4fe1\:606f*)
{fyVtx1@vtxType1,fyVtx2@vtxType2,fyVtx3@vtxType3}
]
]@Join[
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[chTagKey["chTag"]->chTag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassIn->(#@inOct/.fd[a_,b_,0]:>massV@fd[a,b,2]),(* \:5165\:5c04\:91cd\:5b50\:8d28\:91cf*)
MassDec1->(#@medDec1/.fd[a_,b_,0]:>massV@fd[a,b,2]),(*\:751f\:6210\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf\:9879*)
MassOct1->(#@medOct1/.fd[a_,b_,1]:>massV@fd[a,b,2]),(*\:751f\:6210\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf\:9879*)
MassMes1->(#@medMes1/.fd[a_,b_,0]:>massV@fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag],
2(*\:8fde\:63a5\:7b2c2\:5c42*)
];
(*serializeCoe[fyTag,coeJoin]*)


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*fyTag={"RB","trans","left"};*)
(*Query[Cases@KeyValuePattern[*)
(*inOct->fd[2,1,0]*)
(*]]@coeJoin[fyTag]//dsetFmt*)


(* ::Section:: *)
(*RainBow,A-decuplet,trans,right*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{end/2,-end/8},{end/2,0}}],*)
(*Line[{{end/2,delta},{3/4end,delta}}],*)
(*Text["v1",{end/4,-4delta}],Text["v2",{end/2,4delta}],Text["v3",{3/4end,-4delta}]*)
(*},ImageSize->Small]*)


(*\:8d39\:66fc\:56fe\:7684 chpt Tag,\:4ee5\:53ca\:7528\:5230\:7684\:9876\:70b9v1,v2,v3*)
fyTag={"RB","trans","right"};
vtxType1=vtxType["str","DF","mesOut"];  vtx1=Query[All,KeyMap@fyVtx1]@vtx[unq["type"->vtxType1]];
vtxType2=vtxType["F2","tran","octIn"];   vtx2=Query[All,KeyMap@fyVtx2]@vtx[unq["type"->vtxType2]];(*\:8f6c\:79fb\:78c1\:77e9*)
vtxType3=vtxType["str","C","mesIn"];    vtx3=Query[All,KeyMap@fyVtx3]@vtx[unq["type"->vtxType3]];
(*++++++++++++++++++++++\:8fde\:63a5\:5404\:4e2a\:9876\:70b9++++++++++++++++++++++*)
vtxJoin1[fyTag]=Query[All,(*\:4fee\:9970\:9876\:70b91*)
Append[#,{
(*\:9876\:70b93\:7684\:5165\:5c04\:7c92\:5b50 -- mes, \:7b49\:4e8e\:9876\:70b91\:7684\:51fa\:5c04\:7c92\:5b50 mesOut *)
fyVtx3@fdTypeMes->(#@fyVtx1@fdTypeMesOut/.mesRule["reverse"]),
(*\:9876\:70b91\:7684\:51fa\:5c04\:7c92\:5b50 -- decb,\:7b49\:4e8e\:9876\:70b92\:7684\:5165\:5c04\:7c92\:5b50 -- dec. \:8fd9\:91cc\:6839\:636e\:9876\:70b91\:7684\:53cd\:7c92\:5b50\:751f\:6210\:9876\:70b92\:7684\:6b63\:7c92\:5b50*)
fyVtx2@fdTypeOct->(#@fyVtx1@fdTypeOctb/.fd[a_,b_,1]:>fd[a,b,0]),
(*\:5bf9\:4e8e\[CapitalLambda]-\[CapitalSigma]0 \:7c7b\:578b\:8026\:5408,\:8fd8\:9700\:8981\:9650\:5b9a\:51fa\:5c04\:6001\:548c\:5165\:5c04\:6001\:76f8\:540c*)
fyVtx3@fdTypeOctb->(#@fyVtx1@fdTypeOct/.fd[a_,b_,0]:>fd[a,b,1])
}
]&
]@vtx1;
(*\:9876\:70b93\:7684\:5165\:5c04\:7c92\:5b50 -- dec, \:7b49\:4e8e\:9876\:70b92\:7684\:51fa\:5c04\:7c92\:5b50 -- decb . *)
vtxJoin2[fyTag]=Query[All,
Append[#,fyVtx3@fdTypeDec->(#@fyVtx2@fdTypeDecb/.fd[a_,b_,1]:>fd[a,b,0])]&
]@vtx2;(*\:9876\:70b92,\:53cd\:5e38\:78c1\:77e9f1f2,\:975e\:5b9a\:57df\:5316*)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91,2*)
vtxJoinTmp1[fyTag]=JoinAcross[
vtxJoin1[fyTag],(*\:9876\:70b91,\:4fee\:9970\:8fc7\:7684\:5f3a\:76f8\:4e92\:4f5c\:7528\:9876\:70b9,\:52a0\:5165\:4e86\:9876\:70b92\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
vtxJoin2[fyTag],(*\:9876\:70b92,\:4fee\:9970\:8fc7\:7684\:53cd\:5e38\:78c1\:77e9,\:52a0\:5165\:4e86\:9876\:70b93\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
{Key@fyVtx2@fdTypeOct}(*\:7c98\:8fde\:9876\:70b91,2. \:7c98\:8fde\:6761\:4ef6\:662f\:ff1a\:4ece\:9876\:70b91\:63a8\:5bfc\:7684\:5165\:5c04\:573a\[Equal]\:53cd\:5e38\:78c1\:77e9\:5b57\:6bb5\:4e2d\:7684\:5165\:5c04\:573a*)
,"Inner"(*\:4e0d\:9700\:8981 KeyCollisionFunction \:51fd\:6570*)
];
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91,2\:4e0e\:9876\:70b93*)
vtxJoinTmp2[fyTag]=JoinAcross[(*\:7c98\:8fde\:9876\:70b9(1,2),3 *)
vtxJoinTmp1[fyTag], (*\:4fee\:9970\:597d\:7684\:9876\:70b91,2*)
vtx3,(*\:9876\:70b93,\:5f3a\:76f8\:4e92\:4f5c\:7528*)
{
Key@fyVtx3@fdTypeMes,(*\:9876\:70b91\:7684\:4ecb\:5b50\:53cd\:573a,\:5339\:914d\:9876\:70b93\:7684\:4ecb\:5b50\:6b63\:573a*)
Key@fyVtx3@fdTypeOctb,(*\:9876\:70b93\:7684\:51fa\:5c04\:573a,\:8981\:6c42\:548c\:5165\:5c04\:573a\:4e00\:81f4*)
Key@fyVtx3@fdTypeDec(*\:7531\:9876\:70b92\:63a8\:5bfc\:7684Dec,\:5339\:914d\:9876\:70b93\:7684Dec*)
}
,"Inner"
];
(*+++++++++++++++++++++++++ \:9009\:53d6\:9700\:8981\:7684\:5b57\:6bb5,\:5e76\:6309\:7167\:987a\:5e8f\:6392\:5217 +++++++++++++++++++++++++++++++++++++*)
vtxJoin[fyTag]=Query[All,
<|
inOct->#@fyVtx1@fdTypeOct,(*\:9876\:70b91,oct,\:5165\:5c04\:573a*)
outOct->#@fyVtx3@fdTypeOctb,(*\:9876\:70b93,oct,\:51fa\:5c04\:573a*)
medMes1->#@fyVtx3@fdTypeMes,(*\:9876\:70b93,mes,\:5165\:5c04\:573a*)
medOct1->#@fyVtx2@fdTypeOct,(*\:9876\:70b92,oct,\:5165\:5c04\:573a*)
medDec1->#@fyVtx2@fdTypeDecb,(*\:9876\:70b92,dec,\:51fa\:5c04\:573a*)
(*\:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef *)
fyCoeKeycAll->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx1@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType3,(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxType2(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570, \:53cd\:5e38\:78c1\:77e9 *)
],
fyCoeKeycStr->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@fyVtx1@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType3(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycEM->fyCoe[(* \:7535\:78c1\:6d41\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxType2(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570, \:53cd\:5e38\:78c1\:77e9 *)
]
|>&
]@vtxJoinTmp2[fyTag];


fyTag={"RB","trans","right"};
coeJoin[fyTag]=Query[All,KeyDrop[(*\:5220\:9664\:5197\:4f59\:7684\:8026\:5408\:7cfb\:6570\:4fe1\:606f*)
{fyVtx1@vtxType1,fyVtx2@vtxType2,fyVtx3@vtxType3}
]
]@Join[
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[chTagKey["chTag"]->chTag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassIn->(#@inOct/.fd[a_,b_,0]:>massV@fd[a,b,2]),(* \:5165\:5c04\:91cd\:5b50\:8d28\:91cf*)
MassOct1->(#@medOct1/.fd[a_,b_,0]:>massV@fd[a,b,2]),(*\:751f\:6210\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf\:9879*)
MassDec1->(#@medDec1/.fd[a_,b_,1]:>massV@fd[a,b,2]),(*\:751f\:6210\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf\:9879*)
MassMes1->(#@medMes1/.fd[a_,b_,0]:>massV@fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag],
2(*\:8fde\:63a5\:7b2c2\:5c42*)
];
(*serializeCoe[fyTag,coeJoin]*)


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*fyTag={"RB","trans","right"};*)
(*Query[Cases@KeyValuePattern[*)
(*inOct->fd[2,1,0]*)
(*]]@coeJoin[fyTag]//dsetFmt*)


(* ::Section:: *)
(*Kroll-Ruderman, A-meson,decuplet,left*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{end/4,0},{end/4,-end/5}}],*)
(*Line[{{end/4,1.5delta},{3/4end,1.5delta}}],*)
(*Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end,-4delta}]*)
(*},ImageSize->Small]*)


(* \:8d39\:66fc\:56fe\:7684chpt Tag,\:4ee5\:53ca\:7528\:5230\:7684\:9876\:70b9 v1,v2,v3, \:4ece\:5de6\:5230\:53f3\:6392\:5217 *)
fyTag={"KR","mes","dec","left"};
(*\:7ed9\:9876\:70b9\:6253\:4e0atag,\:9632\:6b62 JoinAcross \:64cd\:4f5c\:65f6 Key \:51b2\:7a81*)
vtxType1=vtxType["F1","C","mesOut"];    vtx1=Query[All,KeyMap@fyVtx1]@vtx[unq["type"->vtxType1]];
vtxType1Str=vtxType["F1","C","mesOut","str"]; vtxType1EM=vtxType["F1","C","mesOut","EM"];
vtxType2=vtxType["str","C","mesIn"];   vtx2=Query[All,KeyMap@fyVtx2]@vtx[unq["type"->vtxType2]];
(*++++++++++++++++++++++++++++++++++++++++++++++++++*)
vtxJoin1[fyTag]=Query[All,(*\:4fee\:9970\:9876\:70b91*)
Append[#,{
(*\:9876\:70b91\:7684\:51fa\:5c04\:573a -- mesOut,\:5b83\:7684\:53cd\:573a==\:9876\:70b92\:7684\:5165\:5c04\:573a -- mes *)
fyVtx2@fdTypeMes->(#@fyVtx1@fdTypeMesOut/.mesRule["reverse"]),
(*\:9876\:70b91\:7684\:51fa\:5c04\:573a -- decb,\:7b49\:4e8e\:9876\:70b92\:7684\:5165\:5c04\:573a -- dec*)
fyVtx2@fdTypeDec->(#@fyVtx1@fdTypeDecb/.fd[a_,b_,1]:>fd[a,b,0]),
(*\:5bf9\:4e8e\[CapitalLambda]-\[CapitalSigma]0\:7c92\:5b50,\:8fd8\:9700\:8981\:9650\:5b9a\:51fa\:5c04\:6001\:548c\:5165\:5c04\:6001\:76f8\:540c*)
fyVtx2@fdTypeOctb->(#@fyVtx1@fdTypeOct/.fd[a_,b_,0]:>fd[a,b,1])
}
]&
]@vtx1;
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91,2*)
vtxJoinTmp1[fyTag]=JoinAcross[
vtxJoin1@fyTag,(*\:9876\:70b91,\:4fee\:9970\:8fc7\:7684\:5f3a\:76f8\:4e92\:4f5c\:7528\:9876\:70b9,\:52a0\:5165\:4e86\:9876\:70b92\:51fa\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
vtx2,(*\:9876\:70b92,\:4fee\:9970\:8fc7\:7684\:4ecb\:5b50\:7535\:78c1\:6d41,\:52a0\:5165\:4e86\:9876\:70b93\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
{Key@fyVtx2@fdTypeMes,Key@fyVtx2@fdTypeDec,Key@fyVtx2@fdTypeOctb}(*\:7c98\:8fde\:9876\:70b91,2.\:6761\:4ef6\:662f\:ff1a\:4ece\:9876\:70b91\:63a8\:5bfc\:7684\:51fa\:5c04mes out\[Equal]\:4ecb\:5b50\:7535\:78c1\:6d41\:7684\:51fa\:5c04mesb*)
,"Inner"
];
(*+++++++++++++++++++++++++ \:9009\:53d6\:9700\:8981\:7684\:5b57\:6bb5,\:5e76\:6309\:7167\:987a\:5e8f\:6392\:5217 ++++++++++++++++++++++++++++++++++++*)
vtxJoin[fyTag]=Query[All,
<|
inOct->#@fyVtx1@fdTypeOct,(*\:9876\:70b91,oct,\:5165\:5c04\:573a*)
outOct->#@fyVtx2@fdTypeOctb,(*\:9876\:70b92,oct,\:51fa\:5c04\:573a*)
medDec1->#@fyVtx1@fdTypeDecb,(*\:9876\:70b91,dec,\:51fa\:5c04\:573a*)
medMes1->#@fyVtx2@fdTypeMes,(*\:9876\:70b92,mes,\:5165\:5c04\:573a*)
(*\:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef*)
fyCoeKeycAll->fyCoe[(*\:6240\:6709\:8026\:5408\:7cfb\:6570\:4e58\:79ef*)
#@fyVtx1@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxType2(*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycStr->fyCoe[(*\:5f3a\:76f8\:4e92\:4f5c\:7528\:8026\:5408\:7cfb\:6570\:7684\:4e58\:79ef*)
#@fyVtx1@vtxType1Str, (*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxType2(*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycEM->fyCoe[(* \:7535\:78c1\:6d41\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx1@vtxType1EM(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
]
|>&
]@vtxJoinTmp1[fyTag];


fyTag={"KR","mes","dec","left"};
coeJoin[fyTag]=Query[All,
KeyDrop[{fyVtx1@vtxType1,fyVtx2@vtxType2,fyVtx3@vtxType3}](*\:5220\:9664\:5197\:4f59\:7684\:8026\:5408\:7cfb\:6570\:4fe1\:606f*)
]@Join[
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[chTagKey["chTag"]->chTag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassIn->(#@inOct/.fd[a_,b_,0]:>massV@fd[a,b,2]),(* \:5165\:5c04\:91cd\:5b50\:8d28\:91cf*)
MassDec1->(#@medDec1/.fd[a_,b_,1]:>massV@fd[a,b,2]),(*\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf*)
MassMes1->(#@medMes1/.fd[a_,b_,0]:>massV@fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag],
2(*\:8fde\:63a5\:7b2c2\:5c42*)
];
(*serializeCoe[fyTag,coeJoin]*)


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*fyTag={"KR","mes","dec","left"};*)
(*Query[Cases@KeyValuePattern[*)
(*inOct->fd[2,1,0]*)
(*]]@coeJoin[fyTag]//dsetFmt*)


(* ::Section:: *)
(*Kroll-Ruderman, A-meson,decuplet,right*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Circle[{end/2,0},end/4,{0,\[Pi]}],*)
(*Line[{{3/4end,0},{3/4end,-end/5}}],*)
(*Line[{{end/4,1.5delta},{3/4end,1.5delta}}],*)
(*Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end+4delta,+4delta}]*)
(*},ImageSize->Small]*)


(* \:8d39\:66fc\:56fe\:7684chpt Tag,\:4ee5\:53ca\:7528\:5230\:7684\:9876\:70b9 v1,v2,v3, \:4ece\:5de6\:5230\:53f3\:6392\:5217 *)
fyTag={"KR","mes","dec","right"};
(*\:7ed9\:9876\:70b9\:6253\:4e0atag,\:9632\:6b62 JoinAcross \:64cd\:4f5c\:65f6 Key \:51b2\:7a81*)
vtxType1=vtxType["str","C","mesOut"];  vtx1=Query[All,KeyMap@fyVtx1]@vtx[unq["type"->vtxType1]];
vtxType2=vtxType["F1","C","mesIn"];     vtx2=Query[All,KeyMap@fyVtx2]@vtx[unq["type"->vtxType2]];
vtxType2Str=vtxType["F1","C","mesIn","str"]; vtxType2EM=vtxType["F1","C","mesIn","EM"];
(*++++++++++++++++++++++++++++++++++++++++++++++++++*)
vtxJoin1[fyTag]=Query[All,(*\:4fee\:9970\:9876\:70b91*)
Append[#,{
(*\:9876\:70b91\:7684\:51fa\:5c04\:573a -- mesOut,\:5b83\:7684\:53cd\:573a==\:9876\:70b92\:7684\:5165\:5c04\:573a -- mes *)
fyVtx2@fdTypeMes->(#@fyVtx1@fdTypeMesOut/.mesRule["reverse"]),
(*\:9876\:70b91\:7684\:51fa\:5c04\:573a -- octb,\:7b49\:4e8e\:9876\:70b93\:7684\:5165\:5c04\:573a -- oct*)
fyVtx2@fdTypeDec->(#@fyVtx1@fdTypeDecb/.fd[a_,b_,1]:>fd[a,b,0]),
(*\:5bf9\:4e8e\[CapitalLambda]-\[CapitalSigma]0\:7c92\:5b50,\:8fd8\:9700\:8981\:9650\:5b9a\:51fa\:5c04\:6001\:548c\:5165\:5c04\:6001\:76f8\:540c*)
fyVtx2@fdTypeOctb->(#@fyVtx1@fdTypeOct/.fd[a_,b_,0]:>fd[a,b,1])
}
]&
]@vtx1;
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91,2*)
vtxJoinTmp1[fyTag]=JoinAcross[
vtxJoin1@fyTag,(*\:9876\:70b91,\:4fee\:9970\:8fc7\:7684\:5f3a\:76f8\:4e92\:4f5c\:7528\:9876\:70b9,\:52a0\:5165\:4e86\:9876\:70b92\:51fa\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
vtx2,(*\:9876\:70b92,\:4fee\:9970\:8fc7\:7684\:4ecb\:5b50\:7535\:78c1\:6d41,\:52a0\:5165\:4e86\:9876\:70b93\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
{Key@fyVtx2@fdTypeMes,Key@fyVtx2@fdTypeDec,Key@fyVtx2@fdTypeOctb}(*\:7c98\:8fde\:9876\:70b91,2.\:6761\:4ef6\:662f\:ff1a\:4ece\:9876\:70b91\:63a8\:5bfc\:7684\:51fa\:5c04mes out\[Equal]\:4ecb\:5b50\:7535\:78c1\:6d41\:7684\:51fa\:5c04mesb*)
,"Inner"
];
(*+++++++++++++++++++++ \:9009\:53d6\:9700\:8981\:7684\:5b57\:6bb5,\:5e76\:6309\:7167\:987a\:5e8f\:6392\:5217 ++++++++++++++++++++++++++++++++++*)
vtxJoin[fyTag]=Query[All,
<|
inOct->#@fyVtx1@fdTypeOct,(*\:9876\:70b91,oct,\:5165\:5c04\:573a*)
outOct->#@fyVtx2@fdTypeOctb,(*\:9876\:70b92,oct,\:51fa\:5c04\:573a*)
medDec1->#@fyVtx1@fdTypeDecb,(*\:9876\:70b91,dec,\:51fa\:5c04\:573a*)
medMes1->#@fyVtx2@fdTypeMes,(*\:9876\:70b92,mes,\:5165\:5c04\:573a*)
(*\:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef *)
fyCoeKeycAll->fyCoe[(*\:6240\:6709\:8026\:5408\:7cfb\:6570\:4e58\:79ef*)
#@fyVtx1@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxType2(*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycStr->fyCoe[(*\:5f3a\:76f8\:4e92\:4f5c\:7528\:8026\:5408\:7cfb\:6570\:7684\:4e58\:79ef*)
#@fyVtx1@vtxType1,(*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxType2Str(*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycEM->fyCoe[(* \:7535\:78c1\:6d41\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx2@vtxType2EM(*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
]
|>&
]@vtxJoinTmp1[fyTag];


fyTag={"KR","mes","dec","right"};
coeJoin[fyTag]=Query[All,
KeyDrop[{fyVtx1@vtxType1,fyVtx2@vtxType2,fyVtx3@vtxType3}](*\:5220\:9664\:5197\:4f59\:7684\:8026\:5408\:7cfb\:6570\:4fe1\:606f*)
]@Join[
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[chTagKey["chTag"]->chTag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassIn->(#@inOct/.fd[a_,b_,0]:>massV@fd[a,b,2]),(* \:5165\:5c04\:91cd\:5b50\:8d28\:91cf*)
MassDec1->(#@medDec1/.fd[a_,b_,1]:>massV@fd[a,b,2]),(*\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf*)
MassMes1->(#@medMes1/.fd[a_,b_,0]:>massV@fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag],
2(*\:8fde\:63a5\:7b2c2\:5c42*)
];
(*serializeCoe[fyTag,coeJoin]*)


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*fyTag={"KR","mes","dec","right"};*)
(*Query[Cases@KeyValuePattern[*)
(*inOct->fd[2,1,0]*)
(*]]@coeJoin[fyTag]//dsetFmt*)


(* ::Section::Closed:: *)
(*Kroll-Ruderman, A-meson,decuplet,addition,left*)


(* \:989d\:5916 KR \:56fe\:7684\:7cfb\:6570\:548c \:666e\:901a KR \:56fe\:7684\:7cfb\:6570\:76f8\:540c*)
fyTagTmp={"KR","mes","dec","add","left"};
fyTag={"KR","mes","dec","left"};
coeJoin[fyTagTmp]=Query[All,Append[chTagKey["chTag"]->chTag[fyTagTmp]]
]@coeJoin[fyTag];
(*serializeCoe[fyTagTmp,coeJoin]*)


(* ::Section::Closed:: *)
(*Kroll-Ruderman, A-meson,decuplet,addition,right*)


(* \:989d\:5916 KR \:56fe\:7684\:7cfb\:6570\:548c \:666e\:901a KR \:56fe\:7684\:7cfb\:6570\:76f8\:540c*)
fyTagTmp={"KR","mes","dec","add","right"};
fyTag={"KR","mes","dec","right"};
coeJoin[fyTagTmp]=Query[All,Append[chTagKey["chTag"]->chTag[fyTagTmp]]
]@coeJoin[fyTag];
(*serializeCoe[fyTagTmp,coeJoin]*)


(* ::Section:: *)
(*export*)


(*DumpSave[localPath["coes"]["coes.chpt.mx"],coeJoin];*)
(*\:4fdd\:5b58\:6570\:636e*)
serialize["coes"]["coes.chpt.wdx",coeJoin]
