(* ::Package:: *)

(* ::Section:: *)
(*Feynman diagram tags*)


(*+++++++++++++++++++++++++++++++ \:8d39\:66fc\:56fe\:7684\:5708\:56fe tag \:5217\:8868,\:6240\:6709\:7684\:56fe +++++++++++++++++++++++++++++++*)
fyAmpLoopLstAll={
(* \:516b\:91cd\:6001 *)
(*1*){"RB","mes","oct"},
(*2*){"KR","mes","oct","left"},
(*3*){"KR","mes","oct","right"},
(*4*){"KR","mes","oct","add","left"},
(*5*){"KR","mes","oct","add","right"},
(*6*){"RB","oct","F1"},
(*7*){"RB","oct","F2"},
(*8*){"tad","oct","F1"},
(*9*){"tad","oct","F1","add"},
(*10*){"tad","oct","F2"},
(*11*){"bub","mes","o2"},
(*12*){"bub","mes","ten","o2"},
(* \:5341\:91cd\:6001 *)
(*13*){"RB","mes","dec"},
(*14*){"RB","dec","F1"},
(*15*){"RB","dec","F2"},
(*16*){"RB","trans","left"},
(*17*){"RB","trans","right"},
(*18*){"KR","mes","dec","left"},
(*19*){"KR","mes","dec","right"},
(*20*){"KR","mes","dec","add","left"},
(*21*){"KR","mes","dec","add","right"}
};


(*+++++++++++++++++++++++++++++++ \:8d39\:66fc\:56fe\:7684\:5708\:56fe tag \:5217\:8868\:ff0c\:53bb\:6389\:5728\:65f6\:95f4\:53cd\:6f14\:4e0b\:7ed3\:679c\:91cd\:590d\:7684\:56fe +++++++++++++++++++++++++++++++*)
fyAmpLoopLst={
(* \:516b\:91cd\:6001 *)
(*1*){"RB","mes","oct"},
(*2*){"KR","mes","oct","left"},
(*3*){"KR","mes","oct","add","left"},
(*4*){"RB","oct","F1"},
(*5*){"RB","oct","F2"},
(*6*){"tad","oct","F1"},
(*7*){"tad","oct","F1","add"},
(*8*){"tad","oct","F2"},
(*9*){"bub","mes","o2"},
(*10*){"bub","mes","ten","o2"},
(* \:5341\:91cd\:6001 *)
(*11*){"RB","mes","dec"},
(*12*){"RB","dec","F1"},
(*13*){"RB","dec","F2"},
(*14*){"RB","trans","left"},
(*15*){"KR","mes","dec","left"},
(*16*){"KR","mes","dec","add","left"}
};


(*+++++++++++++++++++++++ \:6811\:56fe Tag +++++++++++++++++++++++++++++*)
fyAmpTree={"tree","oct","F1F2"};
fyAmpTreeLst={
(*1*){"tree","oct","F1"},
(*2*){"tree","oct","F2"}
};


(* ::Input:: *)
(*(*\:6765\:81ea\:9876\:70b9\:4e4b\:5916\:7684\:7cfb\:6570\:ff0c\:5982\:4f20\:64ad\:5b50\:5e26\:6765\:7684 I *)*)
(*otherCoesAll=<|*)
(*(* \:516b\:91cd\:6001\:4e2d\:95f4\:6001 *)*)
(*chTag@{"RB","mes","oct"}->fyCoe[1],*)
(*chTag@{"KR","mes","oct","left"}->fyCoe[1],*)
(*chTag@{"KR","mes","oct","right"}->fyCoe[1],*)
(*chTag@{"KR","mes","oct","add","left"}->fyCoe[1],*)
(*chTag@{"KR","mes","oct","add","right"}->fyCoe[1],*)
(**)
(*chTag@{"RB","oct","F1"}->fyCoe[I],*)
(*chTag@{"RB","oct","F2"}->fyCoe[I],*)
(**)
(*chTag@{"tree","oct","F1F2"}->fyCoe[1],*)
(**)
(*chTag@{"tad","oct","F1"}->fyCoe[I],*)
(*chTag@{"tad","oct","F1","add"}->fyCoe[-I],*)
(**)
(*chTag@{"tad","oct","F2"}->fyCoe[I],*)
(**)
(*chTag@{"bub","mes","o2"}->fyCoe[-I],*)
(*chTag@{"bub","mes","ten","o2"}->fyCoe[-1],*)
(*(* \:5341\:91cd\:6001\:4e2d\:95f4\:6001 *)*)
(*chTag@{"RB","mes","dec"}->fyCoe[1],*)
(**)
(*chTag@{"RB","dec","F1"}->fyCoe[I],*)
(*chTag@{"RB","dec","F2"}->fyCoe[I],*)
(**)
(*chTag@{"RB","trans","left"}->fyCoe[1],*)
(*chTag@{"RB","trans","right"}->fyCoe[-1],*)
(**)
(*chTag@{"KR","mes","dec","left"}->fyCoe[1],*)
(*chTag@{"KR","mes","dec","right"}->fyCoe[1],*)
(**)
(*chTag@{"KR","mes","dec","add","left"}->fyCoe[1],*)
(*chTag@{"KR","mes","dec","add","right"}->fyCoe[1]*)
(*|>;*)


(*\:6765\:81ea\:9876\:70b9\:4e4b\:5916\:7684\:7cfb\:6570\:ff0c\:5982\:4f20\:64ad\:5b50\:5e26\:6765\:7684 I\:ff0c\:5fae\:6270\:5c55\:5f00\:4e2d\:7684 I,
\:5e76\:8003\:8651\:65f6\:95f4\:53cd\:6f14\:5bf9\:79f0\:6027\:ff0c\:5c06 left \:56fe\:7684\:7ed3\:679c*2 *)
fyCoesOther=<|
(* \:516b\:91cd\:6001\:4e2d\:95f4\:6001 *)
chTag@{"RB","mes","oct"}->fyCoe[1],
chTag@{"KR","mes","oct","left"}->fyCoe[2],
chTag@{"KR","mes","oct","add","left"}->fyCoe[2],
chTag@{"RB","oct","F1"}->fyCoe[I],
chTag@{"RB","oct","F2"}->fyCoe[I],

chTag@{"tree","oct","F1F2"}->fyCoe[1],

(* bubble and tadpole*)
chTag@{"tad","oct","F1"}->fyCoe[I],
chTag@{"tad","oct","F1","add"}->fyCoe[I],
chTag@{"tad","oct","F2"}->fyCoe[I],
chTag@{"bub","mes","o2"}->fyCoe[-I],
chTag@{"bub","mes","ten","o2"}->fyCoe[-1],

(* \:5341\:91cd\:6001\:4e2d\:95f4\:6001 *)
chTag@{"RB","mes","dec"}->fyCoe[1],
chTag@{"RB","dec","F1"}->fyCoe[I],
chTag@{"RB","dec","F2"}->fyCoe[-I],(*\:6b64\:5904\:6709\:8d1f\:53f7*)
chTag@{"RB","trans","left"}->fyCoe[-2],(*\:6b64\:5904\:6709\:8d1f\:53f7*)
chTag@{"KR","mes","dec","left"}->fyCoe[2],
chTag@{"KR","mes","dec","add","left"}->fyCoe[2]
|>;


(* ::Section:: *)
(*bubble and no bubble*)


(*\:65b9\:6848\:540d\:79f0--------------------------*)
coesAdjBub="bub";
coesAdjNoBub="nobub";
(*\:624b\:52a8\:7ed9\:51fa \:6c42\:548c \:6240\:5305\:62ec\:7684\:56fe---------------------------*)
fyCoesAdjust=<|
(*\:8003\:8651 bubble tadpole \:7684\:8d21\:732e*)
coesAdjBub-><|
(* \:516b\:91cd\:6001\:4e2d\:95f4\:6001 *)
chTag@{"RB","mes","oct"}->fyCoe[1]
,chTag@{"KR","mes","oct","left"}->fyCoe[1]
,chTag@{"KR","mes","oct","add","left"}->fyCoe[1]
,chTag@{"RB","oct","F1"}->fyCoe[1]
,chTag@{"RB","oct","F2"}->fyCoe[1]
,chTag@{"tree","oct","F1F2"}->fyCoe[1]

,chTag@{"tad","oct","F1"}->fyCoe[1]
,chTag@{"tad","oct","F1","add"}->fyCoe[1]
,chTag@{"tad","oct","F2"}->fyCoe[1]
,chTag@{"bub","mes","o2"}->fyCoe[1]
,chTag@{"bub","mes","ten","o2"}->fyCoe[1]

(* \:5341\:91cd\:6001\:4e2d\:95f4\:6001 *)
,chTag@{"RB","mes","dec"}->fyCoe[1]
,chTag@{"RB","dec","F1"}->fyCoe[1]
,chTag@{"RB","dec","F2"}->fyCoe[1]
,chTag@{"RB","trans","left"}->fyCoe[1]
,chTag@{"KR","mes","dec","left"}->fyCoe[1]
,chTag@{"KR","mes","dec","add","left"}->fyCoe[1]
|>
(*\:4e0d\:8003\:8651 bubble tadpole \:7684\:8d21\:732e*)
,coesAdjNoBub-><|
(* \:516b\:91cd\:6001\:4e2d\:95f4\:6001 *)
chTag@{"RB","mes","oct"}->fyCoe[1]
,chTag@{"KR","mes","oct","left"}->fyCoe[1]
,chTag@{"KR","mes","oct","add","left"}->fyCoe[1]
,chTag@{"RB","oct","F1"}->fyCoe[1]
,chTag@{"RB","oct","F2"}->fyCoe[1]
,chTag@{"tree","oct","F1F2"}->fyCoe[1]

,chTag@{"tad","oct","F1"}->fyCoe[0]
,chTag@{"tad","oct","F1","add"}->fyCoe[0]
,chTag@{"tad","oct","F2"}->fyCoe[0]
,chTag@{"bub","mes","o2"}->fyCoe[0]
,chTag@{"bub","mes","ten","o2"}->fyCoe[0]

(* \:5341\:91cd\:6001\:4e2d\:95f4\:6001 *)
,chTag@{"RB","mes","dec"}->fyCoe[1]
,chTag@{"RB","dec","F1"}->fyCoe[1]
,chTag@{"RB","dec","F2"}->fyCoe[1]
,chTag@{"RB","trans","left"}->fyCoe[1]
,chTag@{"KR","mes","dec","left"}->fyCoe[1]
,chTag@{"KR","mes","dec","add","left"}->fyCoe[1]
|>
|>;


(* ::Section:: *)
(*diagram illustration*)


recordLocationInMessage@diagIllus


Block[{end=4,delta=end/80(*\:793a\:610f\:56fe\:7684\:5c3a\:5bf8\:521d\:59cb\:5316*)},
diagIllus=<|

chTag@{"RB","mes","oct"}->
Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Circle[{end/2,0},end/4,{0,\[Pi]}],
Line[{{end/2,end/4},{end/2,end/4+end/8}}],
FontSize->Small,
Text["v1",{end/4,-4delta}],Text["v2",{end/2,end/4-4delta}],Text["v3",{3/4end,-4delta}]
},ImageSize->Small]

,chTag@{"KR","mes","oct","left"}->Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Circle[{end/2,0},end/4,{0,\[Pi]}],
Line[{{end/4,0},{end/4,-end/5}}],
FontSize->Small,
Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end,-4delta}]
},ImageSize->Small]

,chTag@{"KR","mes","oct","right"}->
Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Circle[{end/2,0},end/4,{0,\[Pi]}],
Line[{{3/4end,0},{3/4end,-end/5}}],
FontSize->Small,
Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end+4delta,+4delta}]
},ImageSize->Small]

,chTag@{"KR","mes","oct","add","left"}->
Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Circle[{end/2,0},end/4,{0,\[Pi]}],
Line[{{end/4,0},{end/4,-end/5}}],
Disk[{end/4,0},.1],
FontSize->Small,
Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end,-4delta}]
},ImageSize->Small]

,chTag@{"KR","mes","oct","add","right"}->
Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Circle[{end/2,0},end/4,{0,\[Pi]}],
Line[{{3/4end,0},{3/4end,-end/5}}],
Disk[{3/4end,0},.1],
FontSize->Small,
Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end+4delta,+4delta}]
},ImageSize->Small]

,chTag@{"RB","oct","F1"}->
Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Circle[{end/2,0},end/4,{0,\[Pi]}],
Line[{{end/2,-end/8},{end/2,0}}],
FontSize->Small,
Text["v1",{end/4,-4delta}],Text["v2",{end/2,4delta}],Text["v3",{3/4end,-4delta}]
},ImageSize->Small]

,chTag@{"RB","oct","F2"}->
Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Circle[{end/2,0},end/4,{0,\[Pi]}],
Line[{{end/2,-end/8},{end/2,0}}],
FontSize->Small,
Text["v1",{end/4,-4delta}],Text["v2",{end/2,4delta}],Text["v3",{3/4end,-4delta}]
},ImageSize->Small]

,chTag@{"tad","oct","F1"}->
Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Arrow@BezierCurve[{
{end/2,0},{0,end/2},{end,end/2},{end/2,0}
}],
Line[{{end/2,-end/6},{end/2,0}}],
FontSize->Small,
Text["v1",{end/2,+5delta}]
},ImageSize->Small]

,chTag@{"tad","oct","F1","add"}->
Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Arrow@BezierCurve[{
{end/2,0},{0,end/2},{end,end/2},{end/2,0}
}],
Line[{{end/2,-end/6},{end/2,0}}],
Disk[{end/2,0},0.1],
FontSize->Small,
Text["v1",{end/2,+5delta}]
},ImageSize->Small]

,chTag@{"tad","oct","F2"}->
Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Arrow@BezierCurve[{
{end/2,0},{0,end/2},{end,end/2},{end/2,0}
}],
Line[{{end/2,-end/6},{end/2,0}}],
Rectangle[{1/2end-3/2delta,-3/2delta},{1/2end+3/2delta,+3/2delta}],
FontSize->Small,
Text["v1",{end/2,+5delta}]
},ImageSize->Small]

,chTag@{"bub","mes","o2"}->
Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Arrow@BezierCurve[{
{end/2,0},{0,end/2},{end,end/2},{end/2,0}
}]
,Line[{{end/2,3/8*end},{end/2,end/2}}]
,FontSize->Small
,Text["v1",{end/2,-4delta}],Text["v2",{end/2,end/3-delta}]
},
ImageSize->Small]

,chTag@{"bub","mes","ten","o2"}->
Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Arrow@BezierCurve[{
{end/2,0},{0,end/2},{end,end/2},{end/2,0}
}]
,Line[{{end/2,3/8*end},{end/2,end/2}}]
,Rectangle[{1/2end-3/2delta,-3/2delta},{1/2end+3/2delta,+3/2delta}]
,FontSize->Small
,Text["v1",{end/2,-5delta}],Text["v2",{end/2,end/3-delta}]
},
ImageSize->Small]

(* \:5341\:91cd\:6001\:4e2d\:95f4\:6001 *)
,chTag@{"RB","mes","dec"}->
Graphics[{
Black,Line[{{0,0},{end,0}}],
Line[{{end/4,delta},{3/4end,delta}}],
Arrowheads[{{Automatic,.53}}],
Circle[{end/2,0},end/4,{0,\[Pi]}],
Line[{{end/2,end/4},{end/2,end/2.4}}],
FontSize->Small,
Text["v1",{end/4,-4delta}],Text["v2",{end/2,end/4-4delta}],Text["v3",{3/4end,-4delta}]
},ImageSize->Small]

,chTag@{"RB","dec","F1"}->
Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Circle[{end/2,0},end/4,{0,\[Pi]}],
Line[{{end/2,-end/8},{end/2,0}}],
Line[{{end/4,delta},{3/4end,delta}}],
FontSize->Small,
Text["v1",{end/4,-4delta}],Text["v2",{end/2,4delta}],Text["v3",{3/4end,-4delta}]
},ImageSize->Small]

,chTag@{"RB","dec","F2"}->
Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Circle[{end/2,0},end/4,{0,\[Pi]}],
Rectangle[{1/2end-3/2delta,-3/2delta},{1/2end+3/2delta,3/2delta}],
Line[{{end/2,-end/8},{end/2,0}}],
Line[{{end/4,delta},{3/4end,delta}}],
FontSize->Small,
Text["v1",{end/4,-4delta}],Text["v2",{end/2,4delta}],Text["v3",{3/4end,-4delta}]
},ImageSize->Small]

,chTag@{"RB","trans","left"}->
Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Circle[{end/2,0},end/4,{0,\[Pi]}],
Rectangle[{1/2end-3/2delta,-3/2delta},{1/2end+3/2delta,3/2delta}],
Line[{{end/2,-end/8},{end/2,0}}],
Line[{{end/4,delta},{2/4end,delta}}],
FontSize->Small,
Text["v1",{end/4,-4delta}],Text["v2",{end/2,4delta}],Text["v3",{3/4end,-4delta}]
},ImageSize->Small]

,chTag@{"RB","trans","right"}->
Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Circle[{end/2,0},end/4,{0,\[Pi]}],
Rectangle[{1/2end-3/2delta,-3/2delta},{1/2end+3/2delta,3/2delta}],
Line[{{end/2,-end/8},{end/2,0}}],
Line[{{2/4end,delta},{3/4end,delta}}],
FontSize->Small,
Text["v1",{end/4,-4delta}],Text["v2",{end/2,4delta}],Text["v3",{3/4end,-4delta}]
},ImageSize->Small]

,chTag@{"KR","mes","dec","left"}->
Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Circle[{end/2,0},end/4,{0,\[Pi]}],
Line[{{end/4,0},{end/4,-end/5}}],
Line[{{end/4,1.5delta},{3/4end,1.5delta}}],
FontSize->Small,
Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end,-4delta}]
},ImageSize->Small]

,chTag@{"KR","mes","dec","right"}->
Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Circle[{end/2,0},end/4,{0,\[Pi]}],
Line[{{3/4end,0},{3/4end,-end/5}}],
Line[{{end/4,1.5delta},{3/4end,1.5delta}}],
FontSize->Small,
Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end+4delta,+4delta}]
},ImageSize->Small]

,chTag@{"KR","mes","dec","add","left"}->
Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Circle[{end/2,0},end/4,{0,\[Pi]}],
Disk[{1/4end,0},2delta],
Line[{{end/4,0},{end/4,-end/5}}],
Line[{{end/4,1.5delta},{3/4end,1.5delta}}],
FontSize->Small,
Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end,-4delta}]
},ImageSize->Small]

,chTag@{"KR","mes","dec","add","right"}->
Graphics[{
Black,Line[{{0,0},{end,0}}],
Arrowheads[{{Automatic,.53}}],
Circle[{end/2,0},end/4,{0,\[Pi]}],
Disk[{3/4end,0},2delta],
Line[{{3/4end,0},{3/4end,-end/5}}],
Line[{{end/4,1.5delta},{3/4end,1.5delta}}],
FontSize->Small,
Text["v1",{end/4-4delta,+4delta}],Text["v2",{3/4end+4delta,+4delta}]
},ImageSize->Small]

|>];
