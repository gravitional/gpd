(* ::Package:: *)

(* ::Input:: *)
(*(*+++++++++++++++++++++++++++++++ \:8d39\:66fc\:56fe\:7684\:5708\:56fe tag \:5217\:8868,\:6240\:6709\:7684\:56fe +++++++++++++++++++++++++++++++*)*)
(*fyAmpLoopLstAll={*)
(*(* \:516b\:91cd\:6001 *)*)
(*(*1*){"RB","mes","oct"},*)
(*(*2*){"KR","mes","oct","left"},*)
(*(*3*){"KR","mes","oct","right"},*)
(*(*4*){"KR","mes","oct","add","left"},*)
(*(*5*){"KR","mes","oct","add","right"},*)
(*(*6*){"RB","oct","F1"},*)
(*(*7*){"RB","oct","F2"},*)
(*(*8*){"tad","oct","F1"},*)
(*(*9*){"tad","oct","F1","add"},*)
(*(*10*){"tad","oct","F2"},*)
(*(*11*){"bub","mes","o2"},*)
(*(*12*){"bub","mes","ten","o2"},*)
(*(* \:5341\:91cd\:6001 *)*)
(*(*13*){"RB","mes","dec"},*)
(*(*14*){"RB","dec","F1"},*)
(*(*15*){"RB","dec","F2"},*)
(*(*16*){"RB","trans","left"},*)
(*(*17*){"RB","trans","right"},*)
(*(*18*){"KR","mes","dec","left"},*)
(*(*19*){"KR","mes","dec","right"},*)
(*(*20*){"KR","mes","dec","add","left"},*)
(*(*21*){"KR","mes","dec","add","right"}*)
(*};*)


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
(*chTag@{"tad","oct","F1"}->fyCoe[-I],*)
(*chTag@{"tad","oct","F2"}->fyCoe[I],*)
(*chTag@{"tad","oct","F1","add"}->fyCoe[-I],*)
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


(*\:6765\:81ea\:9876\:70b9\:4e4b\:5916\:7684\:7cfb\:6570\:ff0c\:5982\:4f20\:64ad\:5b50\:5e26\:6765\:7684 I\:ff0c\:8003\:8651\:65f6\:95f4\:53cd\:6f14\:5bf9\:79f0\:6027\:ff0c\:5c06 left \:56fe\:7684\:7ed3\:679c*2 *)
otherCoes=<|
(* \:516b\:91cd\:6001\:4e2d\:95f4\:6001 *)
chTag@{"RB","mes","oct"}->fyCoe[1],

chTag@{"KR","mes","oct","left"}->fyCoe[2],
chTag@{"KR","mes","oct","add","left"}->fyCoe[2],

chTag@{"RB","oct","F1"}->fyCoe[I],
chTag@{"RB","oct","F2"}->fyCoe[I],

chTag@{"tree","oct","F1F2"}->fyCoe[1],

chTag@{"tad","oct","F1"}->fyCoe[-I],
chTag@{"tad","oct","F2"}->fyCoe[I],
chTag@{"tad","oct","F1","add"}->fyCoe[-I],

chTag@{"bub","mes","o2"}->fyCoe[-I],
chTag@{"bub","mes","ten","o2"}->fyCoe[-1],
(* \:5341\:91cd\:6001\:4e2d\:95f4\:6001 *)
chTag@{"RB","mes","dec"}->fyCoe[1],

chTag@{"RB","dec","F1"}->fyCoe[I],
chTag@{"RB","dec","F2"}->fyCoe[I],

chTag@{"RB","trans","left"}->fyCoe[2],

chTag@{"KR","mes","dec","left"}->fyCoe[2],
chTag@{"KR","mes","dec","add","left"}->fyCoe[2]
|>;
