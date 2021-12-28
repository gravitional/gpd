(* ::Package:: *)

(* ::Section:: *)
(*\:5904\:7406\:811a\:672c\:53c2\:6570*)


(*++++++++++++++++++++++++++++++++++++++++ \:63a5\:6536\:53c2\:6570, \:4fdd\:5b58\:5230\:53d8\:91cf, \:6216\:8005\:8fdb\:884c\:8fdb\:4e00\:6b65\:5904\:7406 ++++++++++++++++++++++++++++++++++++++++*)
{$parOrdStr,$LambdaNumStr,$parCStr,$fitScheme,$LambdaFit}={
enString@$inputCml[[2]],
enString@NumberForm[$inputCml[[3]],{3,2}],
enString@NumberForm[$inputCml[[4]],{3,2}],
enString@$inputCml[[5]],
enString@$inputCml[[6]]
}
(*++++++++++++++++++++++++++++++++++++++++ \:68c0\:67e5\:8f93\:5165\:7684\:53c2\:6570\:662f\:5426\:5408\:6cd5 ++++++++++++++++++++++++++++++++++++++++*)
If[Nand[
StringMatchQ[$fitScheme,{"Sigma1","Sigma2","Nucleon","Cascade","Baryons"}],
StringMatchQ[$LambdaFit,{"notbar","L-"~~NumberString~~".ci-"~~NumberString}] (*eg."L_0.90_ci_1.50"*)
],
echo["Please check the input parameters"];Abort[]
]
(* \:5904\:7406\:6570\:5b57 *)
$parC=SetPrecision[ToExpression@$parCStr,20]
$LambdaNum=SetPrecision[ToExpression@$LambdaNumStr,20]


(*++++++++++++++++++++++++++++++++ \:8bfb\:53d6c1,c2\:7684\:53d6\:503c ++++++++++++++++++++++++++++++++*)
(*c3=c2-c1;*)
echo["c1,c2 configuration"]
If[$LambdaFit==="notbar",
(*++++++++++++++++++++++++++++++++ \:5982\:679c\:4e0d\:662f\:4e3a\:4e86\:8ba1\:7b97Error\:ff0c\:5c31\:4f7f\:7528\:5339\:914d\:7684c1c2\:8fdb\:884c\:8ba1\:7b97 ++++++++++++++++++++++++++++++++*)
echo[$fitSchemePath=FileNameJoin[{fittingsDir,"c1c2-magfit.L-"<>$LambdaNumStr<>".ci-"<>$parCStr<>".wdx"}]];
(*++++++++++++++++++++++++++++++++ \:4f7f\:7528\:7b2c\:4e8c\:79cd\:91cd\:6574\:5316\:65b9\:6848,Z*tree+loop ++++++++++++++++++++++++++++++++*)
echo[numCCC=Import[$fitSchemePath][$fitScheme][[2,2]]];,
(*++++++++++++++++++++++++++++++++ \:5982\:679c\:662f\:4e3a\:4e86\:8ba1\:7b97Error\:ff0c\:5c31\:4f7f\:7528\:6307\:5b9a \[CapitalLambda],ci \:5bf9\:5e94\:7684c1c2\:8fdb\:884c\:8ba1\:7b97 ++++++++++++++++++++++++++++++++*)
echo[$fitSchemePath=FileNameJoin[{fittingsDir,"c1c2-magfit."<>$LambdaFit<>".wdx"}]];
echo[numCCC=Import[$fitSchemePath][$fitScheme][[2,2]]];
]
