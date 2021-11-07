(* ::Package:: *)

(* ::Section:: *)
(*interface*)


(*\:5b9a\:4e49\:4e00\:4e9b\:8f93\:5165\:7684\:63a5\:53e3*)
fdTypeOct=fdType["oct"];fdTypeOctb=fdType["octb"];
fdTypeDec=fdType["dec"];fdTypeDecb=fdType["decb"];
fdTypeMes=fdType["mes"];fdTypeMesOut=fdType["mes","out"];
(* --------- \:8d39\:66fc\:56fe\:9876\:70b9tag\:7684\:63a5\:53e3 --------- *)
fyVtx1[x_]:=fyVtx[x,"v1"];
fyVtx2[x_]:=fyVtx[x,"v2"];
fyVtx3[x_]:=fyVtx[x,"v3"];
(* --------- \:9876\:70b9\:4fee\:9970\:7684\:63a5\:53e3 --------- *)
vtxJoin1[x_]:=fyVtx[x,"v1"]
vtxJoin2[x_]:=fyVtx[x,"v2"]
vtxJointTmp1[x_]:=fyVtx[x,"tmp1"]
vtxJointTmp2[x_]:=fyVtx[x,"tmp2"]
(* --------- \:8026\:5408\:5e38\:6570\:4e58\:79ef\:7684\:63a5\:53e3 --------- *)
fyCoeKeycAll=fyCoeKey["cAll"];
fyCoeKeycEM=fyCoeKey["cEM"];
fyCoeKeycStr=fyCoeKey["cStr"];
(* --------- \:53cd\:5e38\:78c1\:77e9 --------- *)
fyCoeKeycAllF1=fyCoeKey["cAll","F1"];(*\:8ddf\:53cd\:573a\:78c1\:77e9\:6709\:5173\:7684\:7c7b\:578b F1 *)
fyCoeKeycAllF2=fyCoeKey["cAll","F2"];
fyCoeKeycStrF1=fyCoeKey["cStr","F1"];(*\:8ddf\:53cd\:573a\:78c1\:77e9\:6709\:5173\:7684\:7c7b\:578b F1 *)
fyCoeKeycStrF2=fyCoeKey["cStr","F2"];
fyCoeKeycEMF1=fyCoeKey["cEM","F1"];(* EM \:7cfb\:6570*)
fyCoeKeycEMF2=fyCoeKey["cEM","F2"];
(*----------- GE,GM tag -----------*)
fyCoeKeyGE=fyCoeKey["cEM","GE"];
fyCoeKeyGM=fyCoeKey["cEM","GM"];
(* --------- \:6807\:8bb0\:5165\:5c04\:ff0c\:4e2d\:95f4\:6001\:4fe1\:606f --------- *)
inOct;outOct;
medOct1;medOct2;
medMes1;medMes2;
medDec1;medDec2;


(* ::Input:: *)
(*(* --------- \:4e2d\:95f4\:7c92\:5b50\:8d28\:91cf\:7684\:63a5\:53e3, previous --------- *)*)
(*MassOct1=MassKey["oct","p1"];MassOct2=MassKey["oct","p2"];(*\:4e2d\:95f4 oct \:91cd\:5b501,2*)*)
(*MassMes1=MassKey["mes","p1"];*)
(*MassDec1=MassKey["dec","p1"];MassDec2=MassKey["dec","p2"];(*\:4e2d\:95f4 dec \:91cd\:5b501,2*)*)


(* --------- \:4e3a\:4e86\:65b9\:4fbf\:5339\:914d \:5708\:79ef\:5206\:8868\:8fbe\:5f0f\:4e2d\:7684\:540d\:79f0, \:8d28\:91cf\:6539\:5199\:4e3a --------- *)
MassIn=mE;MassOut;
MassOct1=mo1;MassOct2=mo2;(*\:4e2d\:95f4 oct \:91cd\:5b50 1,2*)
MassMes1=mm1;MassMes2=mm2;(*\:4e2d\:95f4 oct \:4ecb\:5b50 1,2*)
MassDec1=md1;MassDec2=md2;(*\:4e2d\:95f4 dec \:91cd\:5b501,2*)


(* ::Input:: *)
(*ffsF1F2=formFactor["F1F2"];*)
(*ffsGEGM=formFactor["GEGM"];*)


ffsF1F2="F1F2";
ffsGEGM="GEGM";
ffsTreeF1F2="TreeF1F2";ffsLoopF1F2="LoopF1F2";
ffsTreeGEGM="TreeGEGM";ffsLoopGEGM="LoopGEGM";
