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


Get[FileNameJoin[{gitLocalName,"gen_chpt_coes.m"}]];(*\:8bfb\:5165\:50a8\:5b58\:9876\:70b9\:7cfb\:6570\:7684\:6587\:4ef6*)


(*\:5b9a\:4e49\:4e00\:4e2a\:786e\:4fdd\:5b57\:7b26\:4e32\:7684\:51fd\:6570,\:7528\:9017\:53f7\:9694\:5f00\:8f93\:5165*)
enStrRiff[x__]:=StringRiffle[ToString/@enList[x],","]
(*\:7528\:6765\:5c06 dataset \:4e2d\:7b2c\:4e8c\:5c42\:ff0c\:5373\:5173\:8054\:7684key\:5f3a\:5236\:6392\:7248\:4e3a\:5b57\:7b26\:4e32\:ff0c\:53ef\:4ee5\:8f83\:597d\:7684\:663e\:793a.*)
dsetFmt[x_]:=Dataset[x/.Association->assocTemp/.{
fdType->enStrRiff,vtxType->enStrRiff,
fyDiagKey->enStrRiff,fyDiag->enStrRiff,
MassKey->enStrRiff,fyCoeKey->enStrRiff,
fyVtx->enStrRiff,vtxCoe->Identity,fyCoe->Times,
fqdKey->enStrRiff
}/.assocTemp->Association]
testFmt[x_]:=(Query[{1}]@x)//InputForm


(* ::Chapter:: *)
(*channels*)


(* ::Section:: *)
(*RainBow*)


fyTag={"RB","mes"};
vtxJoin[fyTag,"v1"]=Query[All,(*\:4fee\:9970\:9876\:70b91*)
Append[#,{
(*\:9876\:70b91\:7684\:5165\:5c04mes--\:6b63\:573a\:ff0c\:5b83\:7684\:53cd\:573a==\:9876\:70b92\:7684\:51fa\:5c04mesb--\:53cd\:573a*)
fyVtx[fdType["mes","out"],"v2"]->(#[fdType["mes"]]/.fd[a_,b_,0]:>fd[a,b,1]),
(*\:9876\:70b91\:7684\:51fa\:5c04oct--\:53cd\:573a\:ff0c\:7b49\:4e8e\:9876\:70b93\:7684\:5165\:5c04oct--\:6b63\:573a*)
fyVtx[fdType["oct"],"v3"]->(#[fdType["octb"]]/.fd[a_,b_,1]:>fd[a,b,0])
}
]&
]@vtx[unq["type"->vtxType["stro","DF"]]];
(*\:9876\:70b92\:51fa\:5c04mesb==\:9876\:70b93\:7684\:5165\:5c04\:7c92\:5b50--\:6b63\:573a*)
vtxJoin[fyTag,"v2"]=Query[All,
Append[#,fyVtx[fdType["mes"],"v3"]->(#[fdType["mes","out"]])]&
]@vtx[unq["type"->vtxType["F1","\[Phi]\[Phi]A"]]];(*\:9876\:70b92\:ff0c\:53cd\:5e38\:78c1\:77e9\:975e\:5b9a\:57df\:5316*)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91\:ff0c2*)
vtxJoin[fyTag,"tmp1"]=JoinAcross[
vtxJoin[fyTag,"v1"],(*\:9876\:70b91,\:4fee\:9970\:8fc7\:7684\:5f3a\:76f8\:4e92\:4f5c\:7528\:9876\:70b9\:ff0c\:52a0\:5165\:4e86\:9876\:70b92\:51fa\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
vtxJoin[fyTag,"v2"],(*\:9876\:70b92\:ff0c\:4fee\:9970\:8fc7\:7684\:4ecb\:5b50\:7535\:78c1\:6d41,\:52a0\:5165\:4e86\:9876\:70b93\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
{Key[fyVtx[fdType["mes","out"],"v2"]]->Key[fdType["mes","out"]]}(*\:7c98\:8fde\:9876\:70b91\:ff0c2.\:6761\:4ef6\:662f\:ff1a\:4ece\:9876\:70b91\:63a8\:5bfc\:7684\:51fa\:5c04mes out\[Equal]\:4ecb\:5b50\:7535\:78c1\:6d41\:7684\:51fa\:5c04mesb*)
,"Inner"
,KeyCollisionFunction->Function[x,{fyVtx[x,"v1"],fyVtx[x,"v2"]}](*v1 \:8868\:793a\:9876\:70b91\:ff0cv2\:8868\:793a\:9876\:70b92*)
];
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91,2\:4e0e\:9876\:70b93*)
vtxJoin[fyTag,"tmp2"]=JoinAcross[(*\:7c98\:8fde\:9876\:70b9(1,2)\:ff0c3 *)
vtxJoin[fyTag,"tmp1"], (*\:4fee\:9970\:597d\:7684\:9876\:70b91,2*)
vtx[unq["type"->vtxType["stro","DF"]]],(*\:9876\:70b93\:ff0c\:5f3a\:76f8\:4e92\:4f5c\:7528*)
{
Key[fyVtx[fdType["oct"],"v3"]]->Key[fdType["oct"]],(*\:9876\:70b91\:7684oct\:6b63\:573a==\:9876\:70b93\:7684oct\:573a*)
Key[fyVtx[fdType["mes"],"v3"]]->Key[fdType["mes"]](*\:7531\:9876\:70b92\:63a8\:5bfc\:7684mes\:6b63\:573a==\:9876\:70b93\:7684mes\:6b63\:573a*)
}
,"Inner"
,KeyCollisionFunction->Function[x,{x,fyVtx[x,"v3"]}](* \:5c06\:9876\:70b93\:91cd\:590d\:51fa\:73b0\:7684\:952e\:6253\:4e0atag*)
];
(*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:9009\:53d6\:9700\:8981\:7684\:5b57\:6bb5\:ff0c\:5e76\:6309\:7167\:987a\:5e8f\:6392\:5217*)
vtxJoin[fyTag]=Query[All,
{
Key@fdType["oct"],(*\:9876\:70b91,oct\:5165\:5c04\:573a*)
Key@fyVtx[fdType["octb"],"v3"],(*\:9876\:70b93,oct\:51fa\:5c04\:573a*)
Key@fdType["octb"],(*\:9876\:70b91,oct\:51fa\:5c04\:573a*)
Key@fyVtx[fdType["mes"],"v2"],(*\:9876\:70b92,mes\:5165\:5c04\:573a*)
Key@vtxType["stro","DF"],(*\:9876\:70b91\:8026\:5408\:7cfb\:6570*)
Key@fyVtx[vtxType["stro","DF"],"v3"],(*\:9876\:70b93\:8026\:5408\:7cfb\:6570*)
Key@vtxType["F1", "\[Phi]\[Phi]A"](*\:9876\:70b92\:8026\:5408\:7cfb\:6570*)
}
]@vtxJoin[fyTag,"tmp2"];


fyTag={"RB","mes"};
coeJoin[fyTag]=Join[
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[fyDiagKey["Diag"]->fyDiag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassKey["p1","oct"]->(#[fdType["octb"]]/.fd[a_,b_,1]:>fd[a,b,2]),(*\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf*)
MassKey["p1","mes"]->(#[fyVtx[fdType["mes"],"v2"]]/.fd[a_,b_,0]:>fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag],
(*\:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef *)
Query[All,<|
fyCoeKey["cc*"]->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#[vtxType["stro","DF"]],(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#[fyVtx[vtxType["stro","DF"],"v3"]],(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
#[vtxType["F1", "\[Phi]\[Phi]A"]](*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
]
|>&
]@vtxJoin[fyTag],
(*\:8fde\:63a5\:7b2c2\:5c42*)
2
];


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*fyTag={"RB","mes"};*)
(*Query[Cases[KeyValuePattern[fdType["oct"]->fd[2,1,0]]]]@coeJoin[fyTag]//dsetFmt*)


(* ::Section:: *)
(*RainBow,octet*)


fyTag={"RB","oct"};
vtxJoin[fyTag,"v1"]=Query[All,(*\:4fee\:9970\:9876\:70b91*)
Append[#,{
(*\:9876\:70b91\:7684\:51fa\:5c04oct--\:53cd\:7c92\:5b50\:ff0c\:7b49\:4e8e\:9876\:70b92\:7684\:5165\:5c04oct--\:6b63\:7c92\:5b50\:3002\:8fd9\:91cc\:6839\:636e\:9876\:70b91\:7684\:53cd\:7c92\:5b50\:751f\:6210\:9876\:70b92\:7684\:6b63\:7c92\:5b50*)
fyVtx[fdType["oct"],"v2"]->(#[fdType["octb"]]/.fd[a_,b_,1]:>fd[a,b,0]),
(*\:9876\:70b91\:7684\:5165\:5c04mes--\:6b63\:7c92\:5b50\:ff0c\:7b49\:4e8e\:9876\:70b93\:7684\:51fa\:5c04mes--\:53cd\:7c92\:5b50*)
fyVtx[fdType["mes","out"],"v3"]->(#[fdType["mes"]]/.fd[a_,b_,0]:>fd[a,b,1])
}
]&
]@vtx[unq["type"->vtxType["stro","DF"]]];
(*\:9876\:70b92\:51fa\:5c04oct--\:53cd\:7c92\:5b50\:ff0c\:6b63\:597d\:662f\:9876\:70b93\:7684\:5165\:5c04\:7c92\:5b50--\:6b63\:7c92\:5b50\:3002 \:8fd9\:91cc\:6839\:636e\:9876\:70b92\:7684\:53cd\:7c92\:5b50\:751f\:6210\:9876\:70b93\:7684\:6b63\:7c92\:5b50*)
vtxJoin[fyTag,"v2"]=Query[All,
Append[#,fyVtx[fdType["oct"],"v3"]->(#[fdType["octb"]]/.fd[a_,b_,1]:>fd[a,b,0])]&
]@vtx[unq["type"->vtxType["F1F2","nloc"]]];(*\:9876\:70b92\:ff0c\:53cd\:5e38\:78c1\:77e9f1f2,\:975e\:5b9a\:57df\:5316*)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91\:ff0c2*)
vtxJoin[fyTag,"tmp1"]=JoinAcross[
vtxJoin[fyTag,"v1"],(*\:9876\:70b91,\:4fee\:9970\:8fc7\:7684\:5f3a\:76f8\:4e92\:4f5c\:7528\:9876\:70b9\:ff0c\:52a0\:5165\:4e86\:9876\:70b92\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
vtxJoin[fyTag,"v2"],(*\:9876\:70b92\:ff0c\:4fee\:9970\:8fc7\:7684\:53cd\:5e38\:78c1\:77e9,\:52a0\:5165\:4e86\:9876\:70b93\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
{Key[fyVtx[fdType["oct"],"v2"]]->Key[fdType["oct"]]}(*\:7c98\:8fde\:9876\:70b91\:ff0c2\:3002\:7c98\:8fde\:6761\:4ef6\:662f\:ff1a\:4ece\:9876\:70b91\:63a8\:5bfc\:7684\:5165\:5c04\:573a\[Equal]\:53cd\:5e38\:78c1\:77e9\:5b57\:6bb5\:4e2d\:7684\:5165\:5c04\:573a*)
,"Inner"
,KeyCollisionFunction->Function[x,{fyVtx[x,"v1"],fyVtx[x,"v2"]}](*v1 \:8868\:793a\:9876\:70b91\:ff0cv2\:8868\:793a\:9876\:70b92*)
];
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91,2\:4e0e\:9876\:70b93*)
vtxJoin[fyTag,"tmp2"]=JoinAcross[(*\:7c98\:8fde\:9876\:70b9(1,2)\:ff0c3 *)
vtxJoin[fyTag,"tmp1"], (*\:4fee\:9970\:597d\:7684\:9876\:70b91,2*)
vtx[unq["type"->vtxType["stro","DF"]]],(*\:9876\:70b93\:ff0c\:5f3a\:76f8\:4e92\:4f5c\:7528*)
{
Key[fyVtx[fdType["mes","out"],"v3"]]->Key[fdType["mes"]],(*\:9876\:70b91\:7684\:4ecb\:5b50\:53cd\:573a\:ff0c\:5339\:914d\:9876\:70b93\:7684\:4ecb\:5b50\:6b63\:573a*)
Key[fyVtx[fdType["oct"],"v3"]]->Key[fdType["oct"]](*\:7531\:9876\:70b92\:63a8\:5bfc\:7684oct\:6b63\:573a\:ff0c\:5339\:914d\:9876\:70b93\:7684oct \:6b63\:573a*)
}
,"Inner"
,KeyCollisionFunction->Function[x,{x,fyVtx[x,"v3"]}](* \:5c06\:9876\:70b93\:91cd\:590d\:51fa\:73b0\:7684\:952e\:6253\:4e0atag*)
];
(*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:9009\:53d6\:9700\:8981\:7684\:5b57\:6bb5\:ff0c\:5e76\:6309\:7167\:987a\:5e8f\:6392\:5217*)
vtxJoin[fyTag]=Query[All,
{
Key@fyVtx[fdType["oct"], "v1"],(*\:9876\:70b91\:5165\:5c04\:573a*)
Key@fdType["octb"],(*\:9876\:70b93\:51fa\:5c04\:573a*)
Key@fyVtx[fdType["mes"], "v3"],(*\:9876\:70b93\:4ecb\:5b50\:573a*)
Key@fyVtx[fdType["oct"], "v2"],(*\:9876\:70b92\:5165\:5c04\:573a*)
Key@fyVtx[fdType["octb"], "v2"],(*\:9876\:70b92\:51fa\:5c04\:573a*)
Key@vtxType["stro", "DF"],(*\:9876\:70b91\:8026\:5408\:7cfb\:6570*)
Key@fyVtx[vtxType["stro", "DF"], "v3"],(*\:9876\:70b93\:8026\:5408\:7cfb\:6570*)
Key@vtxType["F1","oct","nloc"],(*\:53cd\:5e38\:78c1\:77e9\:975e\:5b9a\:57df\:7cfb\:6570,F1*)
Key@vtxType["F2","oct","nloc"](*\:53cd\:5e38\:78c1\:77e9\:975e\:5b9a\:57df\:7cfb\:6570,F2*)
}
]@vtxJoin[fyTag,"tmp2"];
(* \:4e5f\:53ef\:4ee5\:53cd\:5411\:64cd\:4f5c\:ff0c\:5220\:9664\:4e0d\:9700\:8981\:7684\:5b57\:6bb5
KeyDrop[
{
fyVtx[fdType["octb"],"v1"],(*\:53bb\:6389\:9876\:70b91\:7684\:51fa\:5c04\:573a*)
fyVtx[fdType["oct"],"v3"],(*\:63a8\:5bfc\:7684\:ff0c\:9876\:70b93\:7684\:5165\:5c04\:573a*)
fyVtx[fdType["mes"],"v3"],(*\:9876\:70b93\:7684\:4ecb\:5b50\:573a*)
fdType["oct"](*\:5f3a\:76f8\:4e92\:4f5c\:7528\:8bb0\:5f55\:4e2d\:7684\:5165\:5c04\:573a*)
}
]*)


fyTag={"RB","oct"};
coeJoin[fyTag]=Join[
(*\:53bb\:6389\:5355\:72ec\:8026\:5408\:9876\:70b9\:7684\:4fe1\:606f, \:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
(*Query[All,Prepend[fyDiagKey["Diag"]\[Rule]fyDiag[1]]@*KeyDrop[{vtxType["stro", "DF"],vtxType["F1","\[Phi]\[Phi]A"]}]]@vtxJoin["f1"],*)
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[fyDiagKey["Diag"]->fyDiag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassKey["p1","oct"]->(#[fyVtx[fdType["oct"], "v2"]]/.fd[a_,b_,0]:>fd[a,b,2]),(*\:751f\:6210\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf\:9879*)
MassKey["p2","oct"]->(#[fyVtx[fdType["octb"], "v2"]]/.fd[a_,b_,1]:>fd[a,b,2]),
MassKey["p1","mes"]->(#[fyVtx[fdType["mes"], "v3"]]/.fd[a_,b_,0]:>fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag],
(*\:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef *)
Query[All,<|
fyCoeKey["cc*","F1"]->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#[vtxType["stro", "DF"]],(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#[fyVtx[vtxType["stro", "DF"], "v3"]],(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
#[vtxType["F1","oct","nloc"]](*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
],
fyCoeKey["cc*","F2"]->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#[vtxType["stro", "DF"]],(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#[fyVtx[vtxType["stro", "DF"], "v3"]],(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
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
(*Query[Cases[KeyValuePattern[fyVtx[fdType["oct"],"v1"]->fd[2,1,0]]]]@coeJoin[fyTag]//dsetFmt*)


(* ::Section:: *)
(*RainBow,order2,octet*)


fyTag={"RB","oct","o2"};
vtxJoin[fyTag,"v1"]=Query[All,(*\:4fee\:9970\:9876\:70b91*)
Append[#,{
(*\:9876\:70b91\:7684\:51fa\:5c04oct--\:53cd\:7c92\:5b50\:ff0c\:7b49\:4e8e\:9876\:70b92\:7684\:5165\:5c04oct--\:6b63\:7c92\:5b50\:3002\:8fd9\:91cc\:6839\:636e\:9876\:70b91\:7684\:53cd\:7c92\:5b50\:751f\:6210\:9876\:70b92\:7684\:6b63\:7c92\:5b50*)
fyVtx[fdType["oct"],"v2"]->(#[fdType["octb"]]/.fd[a_,b_,1]:>fd[a,b,0]),
(*\:9876\:70b91\:7684\:5165\:5c04mes--\:6b63\:7c92\:5b50\:ff0c\:7b49\:4e8e\:9876\:70b93\:7684\:51fa\:5c04mes--\:53cd\:7c92\:5b50*)
fyVtx[fdType["mes","out"],"v3"]->(#[fdType["mes"]]/.fd[a_,b_,0]:>fd[a,b,1])
}
]&
]@vtx[unq["type"->vtxType["stro","DF"]]];
(*\:9876\:70b92\:51fa\:5c04oct--\:53cd\:7c92\:5b50\:ff0c\:6b63\:597d\:662f\:9876\:70b93\:7684\:5165\:5c04\:7c92\:5b50--\:6b63\:7c92\:5b50\:3002 \:8fd9\:91cc\:6839\:636e\:9876\:70b92\:7684\:53cd\:7c92\:5b50\:751f\:6210\:9876\:70b93\:7684\:6b63\:7c92\:5b50*)
vtxJoin[fyTag,"v2"]=Query[All,
Append[#,fyVtx[fdType["oct"],"v3"]->(#[fdType["octb"]]/.fd[a_,b_,1]:>fd[a,b,0])]&
]@vtx[unq["type"->vtxType["F1F2","nloc"]]];(*\:9876\:70b92\:ff0c\:53cd\:5e38\:78c1\:77e9\:975e\:5b9a\:57df\:5316*)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91\:ff0c2*)
vtxJoin[fyTag,"tmp1"]=JoinAcross[
vtxJoin[fyTag,"v1"],(*\:9876\:70b91,\:4fee\:9970\:8fc7\:7684\:5f3a\:76f8\:4e92\:4f5c\:7528\:9876\:70b9\:ff0c\:52a0\:5165\:4e86\:9876\:70b92\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
vtxJoin[fyTag,"v2"],(*\:9876\:70b92\:ff0c\:4fee\:9970\:8fc7\:7684\:53cd\:5e38\:78c1\:77e9,\:52a0\:5165\:4e86\:9876\:70b93\:5165\:5c04\:7c92\:5b50\:7684\:5b57\:6bb5*)
{Key[fyVtx[fdType["oct"],"v2"]]->Key[fdType["oct"]]}(*\:7c98\:8fde\:9876\:70b91\:ff0c2\:3002\:7c98\:8fde\:6761\:4ef6\:662f\:ff1a\:4ece\:9876\:70b91\:63a8\:5bfc\:7684\:5165\:5c04\:573a\[Equal]\:53cd\:5e38\:78c1\:77e9\:5b57\:6bb5\:4e2d\:7684\:5165\:5c04\:573a*)
,"Inner"
,KeyCollisionFunction->Function[x,{fyVtx[x,"v1"],fyVtx[x,"v2"]}](*v1 \:8868\:793a\:9876\:70b91\:ff0cv2\:8868\:793a\:9876\:70b92*)
];
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:8fde\:63a5\:9876\:70b91,2\:4e0e\:9876\:70b93*)
vtxJoin[fyTag,"tmp2"]=JoinAcross[(*\:7c98\:8fde\:9876\:70b9(1,2)\:ff0c3 *)
vtxJoin[fyTag,"tmp1"], (*\:4fee\:9970\:597d\:7684\:9876\:70b91,2*)
vtx[unq["type"->vtxType["stro","DF"]]],(*\:9876\:70b93\:ff0c\:5f3a\:76f8\:4e92\:4f5c\:7528*)
{
Key[fyVtx[fdType["mes","out"],"v3"]]->Key[fdType["mes"]],(*\:9876\:70b91\:7684\:4ecb\:5b50\:53cd\:573a\:ff0c\:5339\:914d\:9876\:70b93\:7684\:4ecb\:5b50\:6b63\:573a*)
Key[fyVtx[fdType["oct"],"v3"]]->Key[fdType["oct"]](*\:7531\:9876\:70b92\:63a8\:5bfc\:7684oct\:6b63\:573a\:ff0c\:5339\:914d\:9876\:70b93\:7684oct \:6b63\:573a*)
}
,"Inner"
,KeyCollisionFunction->Function[x,{x,fyVtx[x,"v3"]}](* \:5c06\:9876\:70b93\:91cd\:590d\:51fa\:73b0\:7684\:952e\:6253\:4e0atag*)
];
(*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(*\:9009\:53d6\:9700\:8981\:7684\:5b57\:6bb5\:ff0c\:5e76\:6309\:7167\:987a\:5e8f\:6392\:5217*)
vtxJoin[fyTag]=Query[All,
{
Key@fyVtx[fdType["oct"], "v1"],(*\:9876\:70b91\:5165\:5c04\:573a*)
Key@fdType["octb"],(*\:9876\:70b93\:51fa\:5c04\:573a*)
Key@fyVtx[fdType["mes"], "v3"],(*\:9876\:70b93\:4ecb\:5b50\:573a*)
Key@fyVtx[fdType["oct"], "v2"],(*\:9876\:70b92\:5165\:5c04\:573a*)
Key@fyVtx[fdType["octb"], "v2"],(*\:9876\:70b92\:51fa\:5c04\:573a*)
Key@vtxType["stro", "DF"],(*\:9876\:70b91\:8026\:5408\:7cfb\:6570*)
Key@fyVtx[vtxType["stro", "DF"], "v3"],(*\:9876\:70b93\:8026\:5408\:7cfb\:6570*)
Key@vtxType["F2","oct","nloc"](*\:9876\:70b92\:8026\:5408\:7cfb\:6570*)
}
]@vtxJoin[fyTag,"tmp2"];


fyTag={"RB","oct","o2"};
coeJoin[fyTag]=Join[
(*\:53bb\:6389\:5355\:72ec\:8026\:5408\:9876\:70b9\:7684\:4fe1\:606f, \:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
(*Query[All,Prepend[fyDiagKey["Diag"]\[Rule]fyDiag[1]]@*KeyDrop[{vtxType["stro", "DF"],vtxType["F1","\[Phi]\[Phi]A"]}]]@vtxJoin["f1"],*)
(*\:52a0\:4e0a\:8d39\:66fc\:56fe\:7684\:7f16\:53f7*)
Query[All,Prepend[fyDiagKey["Diag"]->fyDiag[fyTag]]]@vtxJoin[fyTag],
(*\:751f\:6210\:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:5b57\:6bb5*)
Query[All,
<|
MassKey["p1","oct"]->(#[fyVtx[fdType["oct"], "v2"]]/.fd[a_,b_,0]:>fd[a,b,2]),(*\:751f\:6210\:4e2d\:95f4\:91cd\:5b50\:8d28\:91cf\:9879*)
MassKey["p2","oct"]->(#[fyVtx[fdType["octb"], "v2"]]/.fd[a_,b_,1]:>fd[a,b,2]),
MassKey["p1","mes"]->(#[fyVtx[fdType["mes"], "v3"]]/.fd[a_,b_,0]:>fd[a,b,2])(*\:751f\:6210\:4e2d\:95f4\:4ecb\:5b50\:8d28\:91cf\:9879*)
|>&
]@vtxJoin[fyTag],
(*\:751f\:6210\:8026\:5408\:7cfb\:6570\:4e58\:79ef *)
Query[All,<|
fyCoeKey["cc*"]->fyCoe[(*\:8026\:5408\:7cfb\:6570\:4e58\:79ef\:7684\:5934\:90e8*)
#[vtxType["stro", "DF"]],(*\:9876\:70b91\:7684\:8026\:5408\:7cfb\:6570*)
#[fyVtx[vtxType["stro", "DF"], "v3"]],(*\:9876\:70b93\:7684\:8026\:5408\:7cfb\:6570*)
#[vtxType["F2","oct","nloc"]](*\:9876\:70b92\:7684\:8026\:5408\:7cfb\:6570*)
]
|>&
]@vtxJoin[fyTag],
(*\:8fde\:63a5\:7b2c2\:5c42*)
2
];


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*fyTag={"RB","oct","o2"};*)
(*Query[Cases[KeyValuePattern[fyVtx[fdType["oct"],"v1"]->fd[2,1,0]]]]@coeJoin[fyTag]//dsetFmt*)


(* ::Chapter:: *)
(*quarkflow*)


(*\:8d39\:66fc\:56fe\:4e2d\:5165\:5c04\:7684\:5938\:514b\:7ec4\:6210,mes*)
tofqdKey["qch"][x_]:=fqdKey["qch",x]
tofqd["qch"][x__]:=fqd[x]
fqdData["qch","mes"]=Association@KeyValueMap[(#1/.qwKey->tofqdKey["qch"])->(#2/.qwave->tofqd["qch"])&]@qwData["mes"];
(*\:8d39\:66fc\:56fe\:4e2d\:5165\:5c04oct\:7684\:5938\:514b\:7ec4\:6210,sea*)
tofqdKey["sea"][x_]:=fqdKey["sea",x]
tofqd["sea"][x__]:=fqd[x]
fqdData["sea","mes"]=Association@KeyValueMap[(#1/.qwKey->tofqdKey["sea"])->(#2/.qwave->tofqd["sea"])&]@qwData["mes"];


(*\:8d39\:66fc\:56fe\:4e2d\:5165\:5c04\:7684\:5938\:514b\:7ec4\:6210,oct,quench*)
tofqdKey["qch"][x_]:=fqdKey["qch",x]
tofqd["qch"][x__]:=fqd[x]
fqdData["qch","oct"]=Association@KeyValueMap[(#1/.qwKey->tofqdKey["qch"])->(#2/.qwave->tofqd["qch"])&]@qwData["oct"];
(*\:8d39\:66fc\:56fe\:4e2d\:5165\:5c04oct\:7684\:5938\:514b\:7ec4\:6210,sea*)
tofqdKey["sea"][x_]:=fqdKey["sea",x]
tofqd["sea"][x_,y_,z_]:=fqd[x,Sequence@@Sort[{y,z}]]
fqdData["sea","oct"]=Association@KeyValueMap[(#1/.qwKey->tofqdKey["sea"])->DeleteDuplicates[#2/.qwave->tofqd["sea"]]&]@qwData["oct"];


(* ::Section:: *)
(*RB,mes,sea*)


(*\:751f\:6210\:5938\:514b\:56fe\:4e2d\:6240\:6709\:53ef\:80fd\:7684\:5938\:514b\:914d\:7f6e*)
fyTag={"RB","mes"};
qchTp="sea";(*\:6d77\:5938\:514b\:56fe*)
tmpoct=fdType["oct"];(*\:5165\:5c04oct\:7684Key*)
tmpmes=fyVtx[fdType["mes"],"v2"];(*\:4e2d\:95f4\:4ecb\:5b50\:7684Key*)
(*+++++++++++++++++++++++++++*)
coeJoin[fyTag,qchTp,"all"]=Query[All,Append[#,
fqdKey["qfl"]-><|
fqdpos[1,2,3]->fqdData[qchTp,"oct"][fqdKey[qchTp,#[tmpoct]]],
fqdpos[4,5]->fqdData[qchTp,"mes"][fqdKey[qchTp,#[tmpmes]]]
|>
]&
]@coeJoin[fyTag];
(*\:7b5b\:9009\:51fa\:6240\:6709\:53ef\:80fd\:7684\:914d\:7f6e\:7684\:51fd\:6570*)
connect[qchTp][x_]:=Module[{qua123List,qua4List},
qua123List=x[fqdKey["qfl"]][fqdpos[1,2,3]];(*\:63d0\:53d6\:51fa123\:4f4d\:7f6e\:4e0a\:7684fqdList*)
qua4List=x[fqdKey["qfl"]][fqdpos[4,5]]/.{fqdList->List};(*\:63d0\:53d6\:51fa45\:4f4d\:7f6e\:4e0a\:7684fqdList\:ff0c\:5e76\:5c06\:5934\:90e8\:66ff\:6362\:6210\:5217\:8868*)
fqdList2@@DeleteCases[
<|
"tp"->{fyTag,qchTp},
fqdpos[1,2,3]->fqdList@@Cases[qua123List,fqd[First[#],fd__]],
fqdpos[4,5]->#
|>&/@qua4List,
<|fqdpos[1,2,3]->fqdList[],fqdpos[4,5]->_|>
]
]
(*+\:7b5b\:9009\:51fa\:53ef\:80fd\:7684\:5938\:514b\:6d41\:914d\:7f6e++++++++++++++++++*)
coeJoin[fyTag,qchTp,"poss"]=Query[All,
Append[#,fqdKey["qfl"]->connect[qchTp][#]]&
]@coeJoin[fyTag,qchTp,"all"];


(* ::Input:: *)
(*(*\:67e5\:8be2\:5c5e\:4e8e\:7279\:5b9a\:7c92\:5b50\:7684\:53cd\:5e94\:9053*)*)
(*fyTag={"RB","mes"};*)
(*Query[Cases[KeyValuePattern[fdType["oct"]->fd[2,1,0]]]]@coeJoin[fyTag,qchTp,"poss"]//dsetFmt*)