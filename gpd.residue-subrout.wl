(* ::Package:: *)

(* ::Chapter:: *)
(*regular part*)


(* ::Section:: *)
(*poles of regular part*)


(*\:8ba1\:7b97\:51fa\:6b64\:8d39\:66fc\:56fe\:4f20\:64ad\:5b50\:7684 poles \:5206\:5e03 *)
pickPoles=AssociationThread[
{"itv1","itv2"},
Query[All,KeyTake[{"propa","pow","kmCoe"}]
]@{
(*\:5728\:533a\:95f4 -\[Xi]<y<\[Xi], \[Xi]<y<1 \:4e0a\:ff0c\:53d6\:6781\:70b9\:5728 x\:8f74\:4e0a\:65b9\:7684 \:4f20\:64ad\:5b50\:ff0c*)
Select[#,#[["itv1","up"]]&],
Select[#,#[["itv2","up"]]&]
}
]&@propaPoles@splt[{fyTag,"clas"}];
(*\:5728\:533a\:95f4 -\[Xi]<y<\[Xi], \[Xi]<y<1 \:4e0a\:ff0c\:6781\:70b9\:5728x\:8f74\:4e0a\:65b9\:7684\:4f20\:64ad\:5b50\:ff0c\:5177\:4f53\:7684\:8bb0\:53f7 --------*)
regularPoles=Query[All,All,"propa"]@pickPoles;


(* ::Section:: *)
(*reduce: numerator -> propagators*)


(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
spltIntTmp=Expand[
splt[{fyTag,"clas"}]*
(splt[{fyTag,"FAFB","spin"}]/.ruleScalar)
];
(*\:72ec\:7acb\:79ef\:5206\:5f0f\[Rule]\:7cfb\:6570 \:7684\:5173\:8054*)
spltIntAssoc=findIntegrate/@spltIntTmp;
(*\:5c06\:4e24\:90e8\:5206\:7ed3\:679c\:76f8\:4e58\:ff0c\:5e76\:6574\:7406*)
spltIntScalar=Collect[spltIntTmp,fadTmp1[__],Simplify];


(*\:5708\:79ef\:5206\:7684\:5149\:9525\:5f62\:5f0f*)
spltIntLCone=Query[(*{FA,FB}*)All
(*FA,FB*),ReplaceAll[fadTmp1->fadTmp2]
]@spltIntScalar;


(* ::Section:: *)
(*residue of poles*)


(*\:5708\:79ef\:5206\:5728\:6d89\:53ca\:5230\:7684 poles \:4e0a\:7684\:7559\:6570;
\:8fd4\:56de\:7684\:7ed3\:679c\:662f <pole->{FAFB}..>*)
spltIntResidue=Query[
(*<propa-poles\[Rule]expr>;\:5728\:8fd9\:4e9b\:4f20\:64ad\:5b50\:7684\:96f6\:70b9\:5904\:6c42\:7559\:6570-------------------*)
Key/@DeleteDuplicates@Values@Map[Splice]@regularPoles
(*{FA,FB};\:5e94\:7528\:6c42\:7559\:6570\:7684\:51fd\:6570---------------*)
,FAFBResidue[#,spltIntLCone]&
]@gatorZeros;


(* ::Section:: *)
(*FAFB -> F1F2*)


(*\:4ece FAFB \:7ec4\:5408\:5230 F1F2, <pole->{FAFB}..>*)
spltF1F2Res=projToF1F2/@spltIntResidue;


(*\:5bf9\:6bcf\:4e2a\:5206\:6bb5\:533a\:95f4\:5185\:ff0c\:6d89\:53ca\:7684\:5404\:4e2a\:7559\:6570\:6c42\:548c--------*)
spltF1F2=Query[(*<itvl\[Rule]v..>*)All
,(*{F1,F2}*)Total
(*<poles\[Rule]vals>*)
]@Query[
(*<propa-poles\[Rule]{FAFB}>; \:5404\:4f20\:64ad\:5b50\:96f6\:70b9\:7684\:7559\:6570*)All
(*\:53d6\:51fa\:533a\:95f4\:5206\:6bb5\:4e0a,\:5bf9\:5e94\:7684\:7559\:6570,*)
,(KeyTake[spltF1F2Res,#]&)
]@regularPoles;


(*\:8f6c\:6362\:5d4c\:5957\:683c\:5f0f\:ff0c\:6700\:5916\:5c42\:662f {f1,f2}, \:4e4b\:540e\:662f\:5206\:6bb5\:51fd\:6570 Piecewise*)
splt[{fyTag,"F1F2","regul"}]=Table[
Piecewise[
{(*val,condition*)
spltF1F2[[#,f1f2]],gpdRegion@#}&/@Keys@gpdRegion
]
,{f1f2,1,2}];


(* ::Chapter:: *)
(*Singular part*)


(* ::Section:: *)
(*singular*)


(* ::Text:: *)
(*\:5947\:5f02\:7684\:90e8\:5206\:ff0c\:4f7f\:7528\:5f85\:5b9a\:7cfb\:6570\:6cd5\:ff0c\:540c\:65f6\:4e5f\:5b8c\:6210\:4e86\:5bf9  kT \:6a2a\:52a8\:91cf \:7684\:79ef\:5206*)


(*\:4ece \:5708\:79ef\:5206\[Rule] \:7a00\:758f\:7684 Association \:4e2d\:ff0c\:63d0\:53d6\:51fa\:5b58\:5728\:7684\:53d1\:6563\:7ed3\:6784, \:4f8b\:5982\:6b63\:6bd4\:4e8e \[Delta][x] \:7684\:9879*)
splitIntDelta=picDeltaContrib/@spltIntAssoc;


(*\:5c06\:81ea\:5b9a\:4e49\:79ef\:5206\:7b26\:53f7\:8868\:793a fad \:8f6c\:6362\:6210 FeynCalc \:7684\:8868\:793a*)
fadToFAD[propa__Rule]:=FAD@@KeyValueMap[Append[#1,-#2]&]@Association[propa]


(*\:5e94\:7528 \:7b26\:53f7\:8f6c\:6362 \:51fd\:6570*)
splitIntDeltaTmp1=Query[(*{F1,F2}*)All
(*{<Integral records>..}*),All
(*<fields of integral>*)
,ReplaceAll[fadTmp1->fadToFAD]
]@splitIntDelta;


paveEvaluate[scalarInt_,k_]:=Module[
{decomposed=TID[scalarInt,k]},
(*----*)
PaXEvaluate[decomposed, k, 
 PaXImplicitPrefactor->1/(2Pi)^(4-2Epsilon)]
]


splt[{fyTag,"F1F2","delta"}]=Query[
(*{F1,F2}*)All
(*{<Integral records>..}*),All
,Append[#,"int"->paveEvaluate[#@"int",k]]&
]@splitIntDeltaTmp1;
