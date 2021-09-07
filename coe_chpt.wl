(* ::Package:: *)

(* ::Title:: *)
(*coe_channel.nb*)


(* ::Text:: *)
(*\:4ece\:62c9\:6c0f\:91cf\:5c55\:5f00\:540e\:4ea7\:751f\:7684\:8026\:5408\:7cfb\:6570, \:63a8\:5bfc\:4e0d\:540c\:53cd\:5e94\:9053\:7684\:524d\:7f6e\:7cfb\:6570\:ff0c\:5373\:628a\:7cfb\:6570\:7ec4\:5408\:8d77\:6765.*)
(*K0b\:8868\:793a K0 bar, pb \:8868\:793a p bar, \:5373\:8d28\:5b50\:7684\:53cd\:7c92\:5b50\:573a\:3002*)


(* ::Text:: *)
(*v1,v2,v3 \:8868\:793a\:7279\:5b9a\:4f4d\:7f6e\:7684\:9876\:70b9,\:9876\:70b9\:4ece\:5de6\:5230\:53f3\:6392\:5e8f.  p1,p2 \:8868\:793a\:7279\:5b9a\:4f4d\:7f6e\:7684\:4e2d\:95f4\:6001\:7c92\:5b50, \:4ece\:5de6\:5230\:53f3.*)


(* ::Chapter:: *)
(*initial*)


(*\:672c\:6587\:4ef6\:7684\:540d\:79f0*)
fileName=If[$Notebooks,NotebookFileName[],$InputFileName];
(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c\:ff0c\:5c31\:5237\:65b0\:7b14\:8bb0\:672c\:7684\:6807\:9898*)
Once@If[!$Notebooks,NotebookWrite[Cells[][[1]],Cell[Last@FileNameSplit[fileName],"Title"]]];
(*\:67e5\:627e init.wl, \:5bfc\:5165\:6839\:76ee\:5f55\:548c\:51fd\:6570\:5b9a\:4e49.*)
Catch@Module[{recurFind,start=1,depMax},
depMax=FileNameDepth[fileName];(*\:8def\:5f84\:7684\:6700\:5927\:5c42\:6b21*)
(*-------\:5b9a\:4e49\:9012\:5f52\:51fd\:6570-------*)
recurFind[dep_Integer]:=If[dep<=depMax,
SetDirectory[DirectoryName[fileName,dep]];(*SetDirectory[]\:8bbe\:7f6e\:5de5\:4f5c\:76ee\:5f55\:4e3a\:5bb6\:76ee\:5f55*)
If[FileExistsQ["init.wl"],Get["init.wl"];Throw["The base directory is : "<>gitLocalName],recurFind[dep+1](*\:5982\:679c\:8fd9\:4e00\:5c42\:627e\:4e0d\:5230\:ff0c\:5c31\:4e0a\:5347\:4e00\:5c42*)];
ResetDirectory[];,
Throw["I cann't find any init.wl in this project"]
];
recurFind[start];
]


Get[FileNameJoin[{gitLocalName,"gen_chpt_coes.wl"}]];(*\:8bfb\:5165\:50a8\:5b58\:9876\:70b9\:7cfb\:6570\:7684\:6587\:4ef6*)


(*\:5b9a\:4e49\:4e00\:4e2a\:786e\:4fdd\:5b57\:7b26\:4e32\:7684\:51fd\:6570,\:7528\:9017\:53f7\:9694\:5f00\:8f93\:5165*)
enStrRiff[x__]:=StringRiffle[ToString/@enList[x],","]
(*\:7528\:6765\:5c06 dataset \:4e2d\:7b2c\:4e8c\:5c42\:ff0c\:5373\:5173\:8054\:7684key\:5f3a\:5236\:6392\:7248\:4e3a\:5b57\:7b26\:4e32\:ff0c\:53ef\:4ee5\:8f83\:597d\:7684\:663e\:793a.*)
dsetFmt[x_]:=Dataset[x/.Association->assocTemp/.{
fdType->enStrRiff,vtxType->enStrRiff,
chTagKey->enStrRiff,
MassKey->enStrRiff,fyCoeKey->enStrRiff,
fyVtx->enStrRiff,vtxCoe->Identity,fyCoe->Times,
fqdKey->enStrRiff,fqdpos->enStrRiff
}/.assocTemp->Association]
testFmt:=EchoFunction[InputForm]
testFmt1[x_]:=x[[{1}]]//EchoFunction[InputForm]


(* ::Chapter:: *)
(*channels*)


(* ::Section:: *)
(*interface*)


(*\:5b9a\:4e49\:4e00\:4e9b\:8f93\:5165\:7684\:63a5\:53e3*)
fdTypeOct=fdType["oct"];
fdTypeOctb=fdType["octb"];
fdTypeMes=fdType["mes"];
fdTypeMesOut=fdType["mes","out"];
(*\:8d39\:66fc\:56fe\:9876\:70b9tag\:7684\:63a5\:53e3*)
fyVtx1[x_]:=fyVtx[x,"v1"]
fyVtx2[x_]:=fyVtx[x,"v2"]
fyVtx3[x_]:=fyVtx[x,"v3"]
(*\:9876\:70b9\:4fee\:9970\:7684\:63a5\:53e3*)
vtxJoin1[x_]:=fyVtx[x,"v1"]
vtxJoin2[x_]:=fyVtx[x,"v2"]
vtxJointTmp1[x_]:=fyVtx[x,"tmp1"]
vtxJointTmp2[x_]:=fyVtx[x,"tmp2"]
(*\:8026\:5408\:5e38\:6570\:4e58\:79ef\:7684\:63a5\:53e3*)
fyCoeKeycAll=fyCoeKey["cAll"];
fyCoeKeycStr=fyCoeKey["cStr"];
(*---------*)
fyCoeKeycAllF1=fyCoeKey["cAll","F1"];(*\:8ddf\:53cd\:573a\:78c1\:77e9\:6709\:5173\:7684\:7c7b\:578b F1 *)
fyCoeKeycAllF2=fyCoeKey["cAll","F2"];
fyCoeKeycStrF1=fyCoeKey["cStr","F1"];(*\:8ddf\:53cd\:573a\:78c1\:77e9\:6709\:5173\:7684\:7c7b\:578b F1 *)
fyCoeKeycStrF2=fyCoeKey["cStr","F2"];
(*\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:7684\:63a5\:53e3*)
MassOct1=MassKey["oct","p1"];(*\:4e2d\:95f4\:91cd\:5b501*)
MassOct2=MassKey["oct","p2"];(*\:4e2d\:95f4\:91cd\:5b502*)
MassMes1=MassKey["mes","p1"];


(* ::Section:: *)
(*RainBow,meson*)


(* \:8d39\:66fc\:56fe\:7684chpt Tag\:ff0c\:4ee5\:53ca\:7528\:5230\:7684\:9876\:70b9 v1,v2,v3, \:4ece\:5de6\:5230\:53f3\:6392\:5217 *)
fyTag={"RB","mes"};
vtxType1=vtxType["stro","DF"];vtx1=vtx[unq["type"->vtxType1]];
vtxType2=vtxType["F1","\[Phi]\[Phi]A"];vtx2=vtx[unq["type"->vtxType2]];
vtxType3=vtxType["stro","DF"];vtx3=vtx[unq["type"->vtxType3]];
(*++++++++++++++++++++++++++++++++++++++++++++++++++*)
vtxJoin1[fyTag]=Query[All,(*\:4fee\:9970\:9876\:70b91*)
Append[#,{
(*\:9876\:70b91\:7684\:5165\:5c04mes--\:6b63\:573a\:ff0c\:5b83\:7684\:53cd\:573a==\:9876\:70b92\:7684\:51fa\:5c04mesb--\:53cd\:573a*)
fyVtx2@fdTypeMesOut->(#[fdTypeMes]/.fd[a_,b_,0]:>fd[a,b,1]),
(*\:9876\:70b91\:7684\:51fa\:5c04oct--\:53cd\:573a\:ff0c\:7b49\:4e8e\:9876\:70b93\:7684\:5165\:5c04oct--\:6b63\:573a*)
fyVtx3@fdTypeOct->(#[fdTypeOctb]/.fd[a_,b_,1]:>fd[a,b,0]),
(*\:5bf9\:4e8e\[CapitalLambda]-\[CapitalSigma]0\:7c92\:5b50\:ff0c\:8fd8\:9700\:8981\:9650\:5b9a\:51fa\:5c04\:6001\:548c\:5165\:5c04\:6001\:76f8\:540c*)
fyVtx3@fdTypeOctb->(#@fdTypeOct/.fd[a_,b_,0]:>fd[a,b,1])
}
]&
]@vtx1;
(*\:9876\:70b92\:51fa\:5c04mesb==\:9876\:70b93\:7684\:5165\:5c04\:7c92\:5b50--\:6b63\:573a*)
vtxJoin2[fyTag]=Query[All,
Append[#,fyVtx3@fdTypeMes->(#[fdTypeMesOut])]&
]@vtx2;(*\:9876\:70b92\:ff0c\:53cd\:5e38\:78c1\:77e9\:975e\:5b9a\:57df\:5316*)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91\:ff0c2*)
vtxJoinTmp1[fyTag]=JoinAcross[
vtxJoin1[fyTag],(*\:9876\:70b91,\:4fee\:9970\:8fc7\:7684\:5f3a\:76f8\:4e92\:4f5c\:7528\:9876\:70b9\:ff0c\:52a0\:5165\:4e86\:9876\:70b92\:51fa\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
vtxJoin2[fyTag],(*\:9876\:70b92\:ff0c\:4fee\:9970\:8fc7\:7684\:4ecb\:5b50\:7535\:78c1\:6d41,\:52a0\:5165\:4e86\:9876\:70b93\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
{Key@fyVtx2@fdTypeMesOut->Key@fdTypeMesOut}(*\:7c98\:8fde\:9876\:70b91\:ff0c2.\:6761\:4ef6\:662f\:ff1a\:4ece\:9876\:70b91\:63a8\:5bfc\:7684\:51fa\:5c04mes out\[Equal]\:4ecb\:5b50\:7535\:78c1\:6d41\:7684\:51fa\:5c04mesb*)
,"Inner"
,KeyCollisionFunction->Function[x,{fyVtx1@x,fyVtx2@x}](*v1 \:8868\:793a\:9876\:70b91\:ff0cv2\:8868\:793a\:9876\:70b92*)
];
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91,2\:4e0e\:9876\:70b93*)
vtxJoinTmp2[fyTag]=JoinAcross[(*\:7c98\:8fde\:9876\:70b9(1,2)\:ff0c3 *)
vtxJoinTmp1[fyTag], (*\:4fee\:9970\:597d\:7684\:9876\:70b91,2*)
vtx3,(*\:9876\:70b93\:ff0c\:5f3a\:76f8\:4e92\:4f5c\:7528*)
{
Key@fyVtx3@fdTypeOct->Key@fdTypeOct,(*\:9876\:70b91\:7684oct\:6b63\:573a==\:9876\:70b93\:7684oct\:573a*)
Key@fyVtx3@fdTypeMes->Key[fdTypeMes],(*\:7531\:9876\:70b92\:63a8\:5bfc\:7684mes\:6b63\:573a==\:9876\:70b93\:7684mes\:6b63\:573a*)
Key@fyVtx3@fdTypeOctb->Key@fdTypeOctb(*\:9876\:70b93\:7684\:51fa\:5c04\:573a\:ff0c\:8981\:6c42\:548c\:5165\:5c04\:573a\:4e00\:81f4*)
}
,"Inner"
,KeyCollisionFunction->Function[x,{x,fyVtx3@x}](* \:5c06\:9876\:70b93\:91cd\:590d\:51fa\:73b0\:7684\:952e\:6253\:4e0atag*)
];
(*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:9009\:53d6\:9700\:8981\:7684\:5b57\:6bb5\:ff0c\:5e76\:6309\:7167\:987a\:5e8f\:6392\:5217*)
vtxJoin[fyTag]=Query[All,
{
Key@fdTypeOct,(*\:9876\:70b91,oct\:5165\:5c04\:573a*)
Key@fyVtx3@fdTypeOctb,(*\:9876\:70b93,oct\:51fa\:5c04\:573a*)
Key@fdTypeOctb,(*\:9876\:70b91,oct\:51fa\:5c04\:573a*)
Key@fyVtx2@fdTypeMes,(*\:9876\:70b92,mes\:5165\:5c04\:573a*)
Key@vtxType1,(*\:9876\:70b91\:8026\:5408\:7cfb\:6570*)
Key@fyVtx3@vtxType3,(*\:9876\:70b93\:8026\:5408\:7cfb\:6570*)
Key@vtxType2(*\:9876\:70b92\:8026\:5408\:7cfb\:6570*)
}
]@vtxJoinTmp2[fyTag];


fyTag={"RB","mes"};
coeJoin[fyTag]=Query[All,
KeyDrop[{vtxType1,fyVtx3@vtxType3,vtxType2}](*\:5220\:9664\:5197\:4f59\:7684\:8026\:5408\:7cfb\:6570\:4fe1\:606f*)
]@Join[
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[chTagKey["chTag"]->chTag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassOct1->(#[fdTypeOctb]/.fd[a_,b_,1]:>fd[a,b,2]),(*\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf*)
MassMes1->(#[fyVtx2@fdTypeMes]/.fd[a_,b_,0]:>fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag],
(*\:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef *)
Query[All,<|
fyCoeKeycAll->fyCoe[(*\:6240\:6709\:8026\:5408\:7cfb\:6570\:4e58\:79ef*)
#@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType3,(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
#@vtxType2(*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycStr->fyCoe[(*\:5f3a\:76f8\:4e92\:4f5c\:7528\:8026\:5408\:7cfb\:6570\:7684\:4e58\:79ef*)
#@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType3(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
]
|>&
]@vtxJoin[fyTag],
(*\:8fde\:63a5\:7b2c2\:5c42*)
2
];


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*fyTag={"RB","mes"};*)
(*Query[Cases[KeyValuePattern[fdTypeOct->fd[2,7,0]]]]@coeJoin[fyTag]//dsetFmt*)


(* ::Section:: *)
(*RainBow,octet,nonlocal*)


(*\:8d39\:66fc\:56fe\:7684 chpt Tag\:ff0c\:4ee5\:53ca\:7528\:5230\:7684\:9876\:70b9v1,v2,v3*)
fyTag={"RB","oct"};
vtxType1=vtxType["stro","DF"];vtx1=vtx[unq["type"->vtxType1]];
vtxType2=vtxType["F1F2","nloc"];vtx2=vtx[unq["type"->vtxType2]];
vtxF1=vtxType["F1","oct","nloc"];vtxF2=vtxType["F2","oct","nloc"];
vtxType3=vtxType["stro","DF"];vtx3=vtx[unq["type"->vtxType3]];
(*++++++++++++++++++++++\:8fde\:63a5\:5404\:4e2a\:9876\:70b9++++++++++++++++++++++*)
vtxJoin1[fyTag]=Query[All,(*\:4fee\:9970\:9876\:70b91*)
Append[#,{
(*\:9876\:70b91\:7684\:51fa\:5c04\:7c92\:5b50 -- octb\:ff0c\:7b49\:4e8e\:9876\:70b92\:7684\:5165\:5c04\:7c92\:5b50 -- oct. \:8fd9\:91cc\:6839\:636e\:9876\:70b91\:7684\:53cd\:7c92\:5b50\:751f\:6210\:9876\:70b92\:7684\:6b63\:7c92\:5b50*)
fyVtx2@fdTypeOct->(#@fdTypeOctb/.fd[a_,b_,1]:>fd[a,b,0]),
(*\:9876\:70b91\:7684\:5165\:5c04mes--\:6b63\:7c92\:5b50\:ff0c\:7b49\:4e8e\:9876\:70b93\:7684\:51fa\:5c04mes--\:53cd\:7c92\:5b50*)
fyVtx3@fdTypeMesOut->(#@fdTypeMes/.fd[a_,b_,0]:>fd[a,b,1]),
(*\:5bf9\:4e8e\[CapitalLambda]-\[CapitalSigma]0\:7c92\:5b50\:ff0c\:8fd8\:9700\:8981\:9650\:5b9a\:51fa\:5c04\:6001\:548c\:5165\:5c04\:6001\:76f8\:540c*)
fyVtx3@fdTypeOctb->(#@fdTypeOct/.fd[a_,b_,0]:>fd[a,b,1])
}
]&
]@vtx1;
(*\:9876\:70b92\:51fa\:5c04oct--\:53cd\:7c92\:5b50\:ff0c\:6b63\:597d\:662f\:9876\:70b93\:7684\:5165\:5c04\:7c92\:5b50--\:6b63\:7c92\:5b50\:3002 \:8fd9\:91cc\:6839\:636e\:9876\:70b92\:7684\:53cd\:7c92\:5b50\:751f\:6210\:9876\:70b93\:7684\:6b63\:7c92\:5b50*)
vtxJoin2[fyTag]=Query[All,
Append[#,fyVtx3@fdTypeOct->(#[fdTypeOctb]/.fd[a_,b_,1]:>fd[a,b,0])]&
]@vtx2;(*\:9876\:70b92\:ff0c\:53cd\:5e38\:78c1\:77e9f1f2,\:975e\:5b9a\:57df\:5316*)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91\:ff0c2*)
vtxJoinTmp1[fyTag]=JoinAcross[
vtxJoin1[fyTag],(*\:9876\:70b91,\:4fee\:9970\:8fc7\:7684\:5f3a\:76f8\:4e92\:4f5c\:7528\:9876\:70b9\:ff0c\:52a0\:5165\:4e86\:9876\:70b92\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
vtxJoin2[fyTag],(*\:9876\:70b92\:ff0c\:4fee\:9970\:8fc7\:7684\:53cd\:5e38\:78c1\:77e9,\:52a0\:5165\:4e86\:9876\:70b93\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
{Key@fyVtx2@fdTypeOct->Key[fdTypeOct]}(*\:7c98\:8fde\:9876\:70b91\:ff0c2\:3002\:7c98\:8fde\:6761\:4ef6\:662f\:ff1a\:4ece\:9876\:70b91\:63a8\:5bfc\:7684\:5165\:5c04\:573a\[Equal]\:53cd\:5e38\:78c1\:77e9\:5b57\:6bb5\:4e2d\:7684\:5165\:5c04\:573a*)
,"Inner"
,KeyCollisionFunction->Function[x,{fyVtx1@x,fyVtx2@x}](*v1 \:8868\:793a\:9876\:70b91\:ff0cv2\:8868\:793a\:9876\:70b92*)
];
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91,2\:4e0e\:9876\:70b93*)
vtxJoinTmp2[fyTag]=JoinAcross[(*\:7c98\:8fde\:9876\:70b9(1,2)\:ff0c3 *)
vtxJoinTmp1[fyTag], (*\:4fee\:9970\:597d\:7684\:9876\:70b91,2*)
vtx3,(*\:9876\:70b93\:ff0c\:5f3a\:76f8\:4e92\:4f5c\:7528*)
{
Key@fyVtx3@fdTypeMesOut->Key@fdTypeMes,(*\:9876\:70b91\:7684\:4ecb\:5b50\:53cd\:573a\:ff0c\:5339\:914d\:9876\:70b93\:7684\:4ecb\:5b50\:6b63\:573a*)
Key@fyVtx3@fdTypeOct->Key@fdTypeOct,(*\:7531\:9876\:70b92\:63a8\:5bfc\:7684oct\:6b63\:573a\:ff0c\:5339\:914d\:9876\:70b93\:7684oct \:6b63\:573a*)
Key@fyVtx3@fdTypeOctb->Key@fdTypeOctb(*\:9876\:70b93\:7684\:51fa\:5c04\:573a\:ff0c\:8981\:6c42\:548c\:5165\:5c04\:573a\:4e00\:81f4*)
}
,"Inner"
,KeyCollisionFunction->Function[x,{x,fyVtx3@x}](* \:5c06\:9876\:70b93\:91cd\:590d\:51fa\:73b0\:7684\:952e\:6253\:4e0atag*)
];
(*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:9009\:53d6\:9700\:8981\:7684\:5b57\:6bb5\:ff0c\:5e76\:6309\:7167\:987a\:5e8f\:6392\:5217*)
vtxJoin[fyTag]=Query[All,
{
Key@fyVtx1@fdTypeOct,(*\:9876\:70b91\:5165\:5c04\:573a*)
Key@fdTypeOctb,(*\:9876\:70b93\:51fa\:5c04\:573a*)
Key@fyVtx3@fdTypeMes,(*\:9876\:70b93\:4ecb\:5b50\:573a*)
Key@fyVtx2@fdTypeOct,(*\:9876\:70b92\:5165\:5c04\:573a*)
Key@fyVtx2@fdTypeOctb,(*\:9876\:70b92\:51fa\:5c04\:573a*)
Key@vtxType1,(*\:9876\:70b91\:8026\:5408\:7cfb\:6570*)
Key@fyVtx3@vtxType3,(*\:9876\:70b93\:8026\:5408\:7cfb\:6570*)
Key@vtxF1,(*\:53cd\:5e38\:78c1\:77e9\:975e\:5b9a\:57df\:7cfb\:6570,F1*)
Key@vtxF2(*\:53cd\:5e38\:78c1\:77e9\:975e\:5b9a\:57df\:7cfb\:6570,F2*)
}
]@vtxJoinTmp2[fyTag];
(* \:4e5f\:53ef\:4ee5\:53cd\:5411\:64cd\:4f5c\:ff0c\:5220\:9664\:4e0d\:9700\:8981\:7684\:5b57\:6bb5
KeyDrop[
{
fyVtx1@fdTypeOctb,(*\:53bb\:6389\:9876\:70b91\:7684\:51fa\:5c04\:573a*)
fyVtx3@fdTypeOct,(*\:63a8\:5bfc\:7684\:ff0c\:9876\:70b93\:7684\:5165\:5c04\:573a*)
fyVtx3@fdTypeMes,(*\:9876\:70b93\:7684\:4ecb\:5b50\:573a*)
fdTypeOct(*\:5f3a\:76f8\:4e92\:4f5c\:7528\:8bb0\:5f55\:4e2d\:7684\:5165\:5c04\:573a*)
}
]*)


fyTag={"RB","oct"};
coeJoin[fyTag]=Query[All,KeyDrop[(*\:5220\:9664\:5197\:4f59\:7684\:8026\:5408\:7cfb\:6570\:4fe1\:606f*)
{vtxType1,fyVtx3@vtxType3,vtxF1,vtxF2}
]
]@Join[
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[chTagKey["Diag"]->chTag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassOct1->(#@fyVtx2@fdTypeOct/.fd[a_,b_,0]:>fd[a,b,2]),(*\:751f\:6210\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf\:9879*)
MassOct2->(#@fyVtx2@fdTypeOctb/.fd[a_,b_,1]:>fd[a,b,2]),
MassMes1->(#@fyVtx3@fdTypeMes/.fd[a_,b_,0]:>fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag],
(*\:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef *)
Query[All,<|
fyCoeKeycAllF1->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType3,(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
#@vtxF1(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570,F1 *)
],
fyCoeKeycAllF2->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType3,(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
#@vtxF2(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570,F2 *)
],
fyCoeKeycStrF1->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType3(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycStrF2->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType3(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
]
|>&
]@vtxJoin[fyTag],
(*\:8fde\:63a5\:7b2c2\:5c42*)
2
];


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*fyTag={"RB","oct"};*)
(*Query[Cases[KeyValuePattern[fyVtx1@fdTypeOct->fd[2,1,0]]]]@coeJoin[fyTag]//dsetFmt*)


(* ::Section:: *)
(*tadpole,octet,order2,nonlocal*)


(*\:8d39\:66fc\:56fe\:7684chpt tag\:ff0c\:4ee5\:53ca\:7528\:5230\:7684\:9876\:70b9*)
fyTag={"tad","oct","o2"};
vtxType2=vtxType["F1F2","o2","nloc"];vtx2=vtx[unq["type"->vtxType2]];
vtxF1=vtxType["F1","oct","o2","nloc"];vtxF2=vtxType["F2","oct","o2","nloc"];
(*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:9009\:53d6\:9700\:8981\:7684\:5b57\:6bb5\:ff0c\:5e76\:6309\:7167\:987a\:5e8f\:6392\:5217*)
vtxJoin[fyTag]=Query[All,
{
Key@fdTypeOct,(*\:9876\:70b92\:5165\:5c04\:573a*)
Key@fdTypeOctb,(*\:9876\:70b92\:51fa\:5c04\:573a*)
Key@fdTypeMes,(*\:9876\:70b92\:4ecb\:5b50\:573a*)
Key@vtxF1,(* \:9876\:70b92\:8026\:5408\:7cfb\:6570 F1 *)
Key@vtxF2(* \:9876\:70b92\:8026\:5408\:7cfb\:6570 F2 *)
}
]@vtx2;


fyTag={"tad","oct","o2"};
coeJoin[fyTag]=Query[All,KeyDrop[(*\:5220\:9664\:5197\:4f59\:7684\:8026\:5408\:7cfb\:6570\:4fe1\:606f*)
{vtxF1,vtxF2}
]
]@Join[
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[chTagKey["Diag"]->chTag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassOct1->(#@fdTypeOct/.fd[a_,b_,0]:>fd[a,b,2]),(*\:751f\:6210\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf\:9879*)
MassMes1->(#@fdTypeMes/.fd[a_,b_,0]:>fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag],
(*\:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef *)
Query[All,<|
fyCoeKeycAllF1->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@vtxF1(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570,F1 *)
],
fyCoeKeycAllF2->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@vtxF2(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570,F2 *)
](*
,fyCoeKeycStrF1->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@vtxF1(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570,F1 *)
],
fyCoeKeycStrF2->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@vtxF2(* \:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570,F2 *)
]*)
|>&
]@vtxJoin[fyTag],
(*\:8fde\:63a5\:7b2c2\:5c42*)
2
];


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*fyTag={"RB","oct","o2"};*)
(*Query[Cases[KeyValuePattern[fdTypeOct->fd[2,1,0]]]]@coeJoin[fyTag]//dsetFmt*)


(* ::Section:: *)
(*bubble, nonlocal*)


(* ::Input:: *)
(*(* \:56fe\:5f62\:8868\:793a *)*)
(*end=4;*)
(*Graphics[{*)
(*Black,Line[{{0,0},{end,0}}],*)
(*Arrowheads[{{Automatic,.53}}],*)
(*Arrow@BezierCurve[{*)
(*{end/2,0},{0,end/2},{end,end/2},{end/2,0}*)
(*}],*)
(*Line[{{end/2,3/8*end},{end/2,end/2}}]*)
(*},*)
(*ImageSize->Small]*)


(*\:8d39\:66fc\:56fe\:7684 chpt Tag\:ff0c\:4ee5\:53ca\:7528\:5230\:7684\:9876\:70b9v1,v2*)
fyTag={"bubble","oct"};
vtxType1=vtxType["stro","BB\[Phi]\[Phi]"];vtx1=vtx[unq["type"->vtxType1]];
(*BB\[Phi]\[Phi]\:9876\:70b9\:4e2d\:6ca1\:6709\:4e0d\:540c\:7684\:4e24\:4e2a\:91cd\:5b50\:8026\:5408\:7684*)
vtxType2=vtxType["F1","\[Phi]\[Phi]A"];vtx2=vtx[unq["type"->vtxType2]];
(*++++++++++++++++++++++\:8fde\:63a5\:5404\:4e2a\:9876\:70b9++++++++++++++++++++++*)
vtxJoin1[fyTag]=Query[All,(*\:4fee\:9970\:9876\:70b91*)
Append[#,{
(*\:9876\:70b91\:7684\:51fa\:5c04\:7c92\:5b50 -- mesout\:ff0c\:7b49\:4e8e\:9876\:70b92\:7684\:5165\:5c04\:7c92\:5b50 -- mes. \:8fd9\:91cc\:6839\:636e\:9876\:70b91\:7684\:53cd\:7c92\:5b50\:751f\:6210\:9876\:70b92\:7684\:6b63\:7c92\:5b50*)
fyVtx2@fdTypeMes->(#@fdTypeMesOut/.mesAntiRule)
}
]&
]@vtx1;
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91\:ff0c2*)
vtxJoinTmp1[fyTag]=JoinAcross[
vtxJoin1[fyTag],(*\:9876\:70b91,\:4fee\:9970\:8fc7\:7684\:5f3a\:76f8\:4e92\:4f5c\:7528\:9876\:70b9\:ff0c\:52a0\:5165\:4e86\:9876\:70b92\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
vtx2,(*\:9876\:70b92\:ff0c\:4ecb\:5b50\:7535\:78c1\:6d41\:76f8\:4e92\:4f5c\:7528*)
{Key@fyVtx2@fdTypeMes->Key@fdTypeMes}(*\:7c98\:8fde\:9876\:70b91\:ff0c2\:3002\:7c98\:8fde\:6761\:4ef6\:662f\:ff1a\:4ece\:9876\:70b91\:63a8\:5bfc\:7684\:5165\:5c04\:573a\[Equal]\:53cd\:5e38\:78c1\:77e9\:5b57\:6bb5\:4e2d\:7684\:5165\:5c04\:573a*)
,"Inner"
,KeyCollisionFunction->Function[x,{fyVtx1@x,fyVtx2@x}](*v1 \:8868\:793a\:9876\:70b91\:ff0cv2\:8868\:793a\:9876\:70b92*)];
(*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:9009\:53d6\:9700\:8981\:7684\:5b57\:6bb5\:ff0c\:5e76\:6309\:7167\:987a\:5e8f\:6392\:5217*)
vtxJoin[fyTag]=Query[All,{
Key@fdTypeOct,(*\:9876\:70b91\:5165\:5c04\:573a*)
Key@fdTypeOctb,(*\:9876\:70b91\:51fa\:5c04\:573a*)
Key@fyVtx1@fdTypeMesOut,(*\:9876\:70b91\:51fa\:5c04\:4ecb\:5b50\:573a*)
Key@vtxType1,(*\:9876\:70b91\:8026\:5408\:7cfb\:6570*)
Key@vtxType2(*\:9876\:70b92\:8026\:5408\:7cfb\:6570*)
}]@vtxJoinTmp1[fyTag];


fyTag={"bubble","oct"};
coeJoin[fyTag]=Query[All,KeyDrop[(*\:5220\:9664\:5197\:4f59\:7684\:8026\:5408\:7cfb\:6570\:4fe1\:606f*)
{vtxType1,vtxType2}
]
]@Join[
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[chTagKey["Diag"]->chTag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassMes1->(#@fyVtx1@fdTypeMesOut/.fd[a_,b_,0]:>fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag],
(*\:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef *)
Query[All,<|
fyCoeKeycAll->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@vtxType1,(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@vtxType2(*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKeycStr->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@vtxType1(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
]
|>&
]@vtxJoin[fyTag],
(*\:8fde\:63a5\:7b2c2\:5c42*)
2
];


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*fyTag={"bubble","oct"};*)
(*Query[Cases[*)
(*KeyValuePattern[fdTypeOct->fd[2,1,0]]*)
(*]]@coeJoin[fyTag]//dsetFmt*)


(* ::Chapter:: *)
(*Saveas*)


(* ::Input:: *)
(*If[FileExtension@NotebookFileName[]==="nb",*)
(*FrontEndExecute[FrontEndToken[FrontEnd`EvaluationNotebook[], "Save", {StringTrim[NotebookFileName[],".nb"~~EndOfString]<>".wl", "Package"}]]]*)
