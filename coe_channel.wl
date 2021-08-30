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


(* ::Section:: *)
(*initial parameters*)


If[cmdQ,
(*\:5982\:679c\:5728\:547d\:4ee4\:884c\:6267\:884c*)
inputCml=$ScriptCommandLine,
(*++++++++++++++++++++++++++++++++++++++++*)
(*\:5982\:679c\:5728\:524d\:7aef\:6267\:884c*)
inputCml={fileName,(*\:547d\:4ee4\:884c\:7b2c\:4e00\:4e2a\:53c2\:6570\:6c38\:8fdc\:662f\:6b64\:811a\:672c\:7684\:7edd\:5bf9\:8def\:5f84*)
(*\:5176\:4ed6\:53c2\:6570*)
inputSim=enString[
(*\:5728\:8fd9\:91cc\:7ed9\:51fa\:5176\:4ed6\:53c2\:6570\:5728mathematica\:8bed\:6cd5\:4e0b\:7684\:5f62\:5f0f\:ff0c\:5916\:9762\:7684 enString \:4f1a\:81ea\:52a8\:8f6c\:6362*)
(*\:5c3d\:91cf\:591a\:4f7f\:7528Association\:7ed3\:6784*)
",a"
(*\:5728\:8fd9\:91cc\:7ed9\:51fa\:5176\:4ed6\:53c2\:6570\:5728mathematica\:8bed\:6cd5\:4e0b\:7684\:5f62\:5f0f\:ff0c\:5916\:9762\:7684 enString \:4f1a\:81ea\:52a8\:8f6c\:6362*)
]
}
];
echo["the input parameter is:\n"];enString[inputCml]


(* ::Chapter:: *)
(*vtx read*)


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
testFmt[x_]:=x[[{1}]]//EchoFunction[InputForm]


(* ::Chapter:: *)
(*channels*)


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


(* ::Section:: *)
(*RainBow*)


fyTag={"RB","mes"};
vtxJoin1[fyTag]=Query[All,(*\:4fee\:9970\:9876\:70b91*)
Append[#,{
(*\:9876\:70b91\:7684\:5165\:5c04mes--\:6b63\:573a\:ff0c\:5b83\:7684\:53cd\:573a==\:9876\:70b92\:7684\:51fa\:5c04mesb--\:53cd\:573a*)
fyVtx2@fdTypeMesOut->(#[fdTypeMes]/.fd[a_,b_,0]:>fd[a,b,1]),
(*\:9876\:70b91\:7684\:51fa\:5c04oct--\:53cd\:573a\:ff0c\:7b49\:4e8e\:9876\:70b93\:7684\:5165\:5c04oct--\:6b63\:573a*)
fyVtx3@fdTypeOct->(#[fdTypeOctb]/.fd[a_,b_,1]:>fd[a,b,0])
}
]&
]@vtx[unq["type"->vtxType["stro","DF"]]];
(*\:9876\:70b92\:51fa\:5c04mesb==\:9876\:70b93\:7684\:5165\:5c04\:7c92\:5b50--\:6b63\:573a*)
vtxJoin2[fyTag]=Query[All,
Append[#,fyVtx3@fdTypeMes->(#[fdTypeMesOut])]&
]@vtx[unq["type"->vtxType["F1","\[Phi]\[Phi]A"]]];(*\:9876\:70b92\:ff0c\:53cd\:5e38\:78c1\:77e9\:975e\:5b9a\:57df\:5316*)
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
vtx[unq["type"->vtxType["stro","DF"]]],(*\:9876\:70b93\:ff0c\:5f3a\:76f8\:4e92\:4f5c\:7528*)
{
Key@fyVtx3@fdTypeOct->Key@fdTypeOct,(*\:9876\:70b91\:7684oct\:6b63\:573a==\:9876\:70b93\:7684oct\:573a*)
Key@fyVtx3@fdTypeMes->Key[fdTypeMes](*\:7531\:9876\:70b92\:63a8\:5bfc\:7684mes\:6b63\:573a==\:9876\:70b93\:7684mes\:6b63\:573a*)
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
Key@vtxType["stro","DF"],(*\:9876\:70b91\:8026\:5408\:7cfb\:6570*)
Key@fyVtx3@vtxType["stro","DF"],(*\:9876\:70b93\:8026\:5408\:7cfb\:6570*)
Key@vtxType["F1", "\[Phi]\[Phi]A"](*\:9876\:70b92\:8026\:5408\:7cfb\:6570*)
}
]@vtxJoinTmp2[fyTag];


fyTag={"RB","mes"};
coeJoin[fyTag]=Query[All,KeyDrop[(*\:5220\:9664\:5197\:4f59\:7684\:8026\:5408\:7cfb\:6570\:4fe1\:606f*)
{vtxType["stro", "DF"],fyVtx3@vtxType["stro", "DF"],vtxType["F1", "\[Phi]\[Phi]A"]}
]
]@Join[
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[chTagKey["chTag"]->chTag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassKey["p1","oct"]->(#[fdTypeOctb]/.fd[a_,b_,1]:>fd[a,b,2]),(*\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf*)
MassKey["p1","mes"]->(#[fyVtx2@fdTypeMes]/.fd[a_,b_,0]:>fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag],
(*\:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef *)
Query[All,<|
fyCoeKey["cAll"]->fyCoe[(*\:6240\:6709\:8026\:5408\:7cfb\:6570\:4e58\:79ef*)
#[vtxType["stro","DF"]],(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#[fyVtx3@vtxType["stro","DF"]],(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
#[vtxType["F1", "\[Phi]\[Phi]A"]](*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKey["cStr"]->fyCoe[(*\:5f3a\:76f8\:4e92\:4f5c\:7528\:8026\:5408\:7cfb\:6570\:7684\:4e58\:79ef*)
#[vtxType["stro","DF"]],(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#[fyVtx3@vtxType["stro","DF"]](*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
]
|>&
]@vtxJoin[fyTag],
(*\:8fde\:63a5\:7b2c2\:5c42*)
2
];


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*fyTag={"RB","mes"};*)
(*Query[Cases[KeyValuePattern[fdTypeOct->fd[2,8,0]]]]@coeJoin[fyTag]//dsetFmt*)


(* ::Section:: *)
(*RainBow,octet*)


fyTag={"RB","oct"};
vtxJoin1[fyTag]=Query[All,(*\:4fee\:9970\:9876\:70b91*)
Append[#,{
(*\:9876\:70b91\:7684\:51fa\:5c04oct--\:53cd\:7c92\:5b50\:ff0c\:7b49\:4e8e\:9876\:70b92\:7684\:5165\:5c04oct--\:6b63\:7c92\:5b50\:3002\:8fd9\:91cc\:6839\:636e\:9876\:70b91\:7684\:53cd\:7c92\:5b50\:751f\:6210\:9876\:70b92\:7684\:6b63\:7c92\:5b50*)
fyVtx2@fdTypeOct->(#[fdTypeOctb]/.fd[a_,b_,1]:>fd[a,b,0]),
(*\:9876\:70b91\:7684\:5165\:5c04mes--\:6b63\:7c92\:5b50\:ff0c\:7b49\:4e8e\:9876\:70b93\:7684\:51fa\:5c04mes--\:53cd\:7c92\:5b50*)
fyVtx3@fdTypeMesOut->(#[fdTypeMes]/.fd[a_,b_,0]:>fd[a,b,1])
}
]&
]@vtx[unq["type"->vtxType["stro","DF"]]];
(*\:9876\:70b92\:51fa\:5c04oct--\:53cd\:7c92\:5b50\:ff0c\:6b63\:597d\:662f\:9876\:70b93\:7684\:5165\:5c04\:7c92\:5b50--\:6b63\:7c92\:5b50\:3002 \:8fd9\:91cc\:6839\:636e\:9876\:70b92\:7684\:53cd\:7c92\:5b50\:751f\:6210\:9876\:70b93\:7684\:6b63\:7c92\:5b50*)
vtxJoin2[fyTag]=Query[All,
Append[#,fyVtx3@fdTypeOct->(#[fdTypeOctb]/.fd[a_,b_,1]:>fd[a,b,0])]&
]@vtx[unq["type"->vtxType["F1F2","nloc"]]];(*\:9876\:70b92\:ff0c\:53cd\:5e38\:78c1\:77e9f1f2,\:975e\:5b9a\:57df\:5316*)
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
vtx[unq["type"->vtxType["stro","DF"]]],(*\:9876\:70b93\:ff0c\:5f3a\:76f8\:4e92\:4f5c\:7528*)
{
Key@fyVtx3@fdTypeMesOut->Key@fdTypeMes,(*\:9876\:70b91\:7684\:4ecb\:5b50\:53cd\:573a\:ff0c\:5339\:914d\:9876\:70b93\:7684\:4ecb\:5b50\:6b63\:573a*)
Key@fyVtx3@fdTypeOct->Key@fdTypeOct(*\:7531\:9876\:70b92\:63a8\:5bfc\:7684oct\:6b63\:573a\:ff0c\:5339\:914d\:9876\:70b93\:7684oct \:6b63\:573a*)
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
Key@vtxType["stro", "DF"],(*\:9876\:70b91\:8026\:5408\:7cfb\:6570*)
Key@fyVtx3@vtxType["stro","DF"],(*\:9876\:70b93\:8026\:5408\:7cfb\:6570*)
Key@vtxType["F1","oct","nloc"],(*\:53cd\:5e38\:78c1\:77e9\:975e\:5b9a\:57df\:7cfb\:6570,F1*)
Key@vtxType["F2","oct","nloc"](*\:53cd\:5e38\:78c1\:77e9\:975e\:5b9a\:57df\:7cfb\:6570,F2*)
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
coeJoin[fyTag]=Join[
(*\:53bb\:6389\:5355\:72ec\:8026\:5408\:9876\:70b9\:7684\:4fe1\:606f, \:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
(*Query[All,Prepend[chTagKey["Diag"]\[Rule]chTag[1]]@*KeyDrop[{vtxType["stro", "DF"],vtxType["F1","\[Phi]\[Phi]A"]}]]@vtxJoin["f1"],*)
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[chTagKey["Diag"]->chTag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassKey["p1","oct"]->(#[fyVtx2@fdTypeOct]/.fd[a_,b_,0]:>fd[a,b,2]),(*\:751f\:6210\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf\:9879*)
MassKey["p2","oct"]->(#[fyVtx2@fdTypeOctb]/.fd[a_,b_,1]:>fd[a,b,2]),
MassKey["p1","mes"]->(#[fyVtx3@fdTypeMes]/.fd[a_,b_,0]:>fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag],
(*\:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef *)
Query[All,<|
fyCoeKey["cc*","F1"]->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#[vtxType["stro", "DF"]],(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#[fyVtx3@vtxType["stro", "DF"]],(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
#[vtxType["F1","oct","nloc"]](*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKey["cc*","F2"]->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#[vtxType["stro", "DF"]],(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#[fyVtx3@vtxType["stro", "DF"]],(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
#[vtxType["F2","oct","nloc"]](*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
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
(*RainBow,order2,octet*)


fyTag={"RB","oct","o2"};
vtxJoin1[fyTag]=Query[All,(*\:4fee\:9970\:9876\:70b91*)
Append[#,{
(*\:9876\:70b91\:7684\:51fa\:5c04oct--\:53cd\:7c92\:5b50\:ff0c\:7b49\:4e8e\:9876\:70b92\:7684\:5165\:5c04oct--\:6b63\:7c92\:5b50\:3002\:8fd9\:91cc\:6839\:636e\:9876\:70b91\:7684\:53cd\:7c92\:5b50\:751f\:6210\:9876\:70b92\:7684\:6b63\:7c92\:5b50*)
fyVtx2@fdTypeOct->(#[fdTypeOctb]/.fd[a_,b_,1]:>fd[a,b,0]),
(*\:9876\:70b91\:7684\:5165\:5c04mes--\:6b63\:7c92\:5b50\:ff0c\:7b49\:4e8e\:9876\:70b93\:7684\:51fa\:5c04mes--\:53cd\:7c92\:5b50*)
fyVtx3@fdTypeMesOut->(#[fdTypeMes]/.fd[a_,b_,0]:>fd[a,b,1])
}
]&
]@vtx[unq["type"->vtxType["stro","DF"]]];
(*\:9876\:70b92\:51fa\:5c04oct--\:53cd\:7c92\:5b50\:ff0c\:6b63\:597d\:662f\:9876\:70b93\:7684\:5165\:5c04\:7c92\:5b50--\:6b63\:7c92\:5b50\:3002 \:8fd9\:91cc\:6839\:636e\:9876\:70b92\:7684\:53cd\:7c92\:5b50\:751f\:6210\:9876\:70b93\:7684\:6b63\:7c92\:5b50*)
vtxJoin2[fyTag]=Query[All,
Append[#,fyVtx3@fdTypeOct->(#[fdTypeOctb]/.fd[a_,b_,1]:>fd[a,b,0])]&
]@vtx[unq["type"->vtxType["F1F2","nloc"]]];(*\:9876\:70b92\:ff0c\:53cd\:5e38\:78c1\:77e9\:975e\:5b9a\:57df\:5316*)
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
vtx[unq["type"->vtxType["stro","DF"]]],(*\:9876\:70b93\:ff0c\:5f3a\:76f8\:4e92\:4f5c\:7528*)
{
Key@fyVtx3@fdTypeMesOut->Key[fdTypeMes],(*\:9876\:70b91\:7684\:4ecb\:5b50\:53cd\:573a\:ff0c\:5339\:914d\:9876\:70b93\:7684\:4ecb\:5b50\:6b63\:573a*)
Key@fyVtx3@fdTypeOct->Key[fdTypeOct](*\:7531\:9876\:70b92\:63a8\:5bfc\:7684oct\:6b63\:573a\:ff0c\:5339\:914d\:9876\:70b93\:7684oct \:6b63\:573a*)
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
Key@vtxType["stro", "DF"],(*\:9876\:70b91\:8026\:5408\:7cfb\:6570*)
Key@fyVtx3@vtxType["stro", "DF"],(*\:9876\:70b93\:8026\:5408\:7cfb\:6570*)
Key@vtxType["F2","oct","nloc"](*\:9876\:70b92\:8026\:5408\:7cfb\:6570*)
}
]@vtxJoinTmp2[fyTag];


fyTag={"RB","oct","o2"};
coeJoin[fyTag]=Join[
(*\:53bb\:6389\:5355\:72ec\:8026\:5408\:9876\:70b9\:7684\:4fe1\:606f, \:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
(*Query[All,Prepend[chTagKey["Diag"]\[Rule]chTag[1]]@*KeyDrop[{vtxType["stro", "DF"],vtxType["F1","\[Phi]\[Phi]A"]}]]@vtxJoin["f1"],*)
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[chTagKey["Diag"]->chTag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassKey["p1","oct"]->(#@fyVtx2@fdTypeOct/.fd[a_,b_,0]:>fd[a,b,2]),(*\:751f\:6210\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf\:9879*)
MassKey["p2","oct"]->(#@fyVtx2@fdTypeOctb/.fd[a_,b_,1]:>fd[a,b,2]),
MassKey["p1","mes"]->(#@fyVtx3@fdTypeMes/.fd[a_,b_,0]:>fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag],
(*\:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef *)
Query[All,<|
fyCoeKey["cc*"]->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#@vtxType["stro", "DF"],(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#@fyVtx3@vtxType["stro", "DF"],(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
#@vtxType["F2","oct","nloc"](*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
]
|>&
]@vtxJoin[fyTag],
(*\:8fde\:63a5\:7b2c2\:5c42*)
2
];


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*fyTag={"RB","oct","o2"};*)
(*Query[Cases[KeyValuePattern[fyVtx1@fdTypeOct->fd[2,1,0]]]]@coeJoin[fyTag]//dsetFmt*)


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