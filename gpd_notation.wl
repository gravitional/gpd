(* ::Package:: *)

(* ::Title:: *)
(*gpd_notation.nb*)


(* ::Text:: *)
(*\:4fdd\:7559\:4e00\:4e9b\:8bb0\:53f7\:ff0c\:4ee5\:53ca\:52a8\:91cf\:53c2\:6570\:5316*)


(*\:524d\:9762\:591a\:4e2al\:8868\:793a\:662f\:5149\:9525\:5206\:91cf\:7684\:610f\:601d,\:5404\:52a8\:91cf\:5206\:91cf\:7684\:53c2\:6570\:5316*)
k={(y+\[Xi])/(2Sqrt[2]\[Xi]) Q,km,kt};(*\:4ecb\:5b50\:5708\:52a8\:91cf*)
p1={(1+\[Xi])/(2Sqrt[2]\[Xi]) Q,(1-\[Xi])/(2Sqrt[2]\[Xi]) Q,pt};(*GDP \:4e2d\:7684\:6838\:5b50\:521d\:672b\:6001\:52a8\:91cf\:ff0c\:4ee5\:53ca\:52a8\:91cf\:4e4b\:5dee*)
p2={(1-\[Xi])/(2Sqrt[2]\[Xi]) Q,(1+\[Xi])/(2Sqrt[2]\[Xi]) Q,pt};
q={-1/Sqrt[2] Q,1/Sqrt[2] Q,0};


\[CapitalLambda]=\[CapitalLambda]; (*dipole \:5f62\:72b6\:56e0\:5b50\:53c2\:6570*)
m\[Phi]=Subscript["m","\[Phi]"]; (*\:4ecb\:5b50\:8d28\:91cf*)
\[CapitalLambda]l=OverBar["\[CapitalLambda]"]; (*\:8d28\:91cf\:4e4b\:5dee\:ff0c\[CapitalLambda]-m\[Phi]*)
\[CapitalOmega]\[Phi]=Subscript["\[CapitalOmega]","\[Phi]"];(* \:5b64\:5be1\:79ef\:5206\:7684\:7ed3\:679c kt^2+m\[Phi]^2*)
\[CapitalOmega]\[CapitalLambda]=Subscript["\[CapitalOmega]","\[CapitalLambda]"];(* kt^2+\[CapitalLambda]^2*)


(* ::DisplayFormula:: *)
(*D\[Phi]B=(kt^2+y*MB^2-y*yl*M^2+yl*m\[Phi]^2)/- yl;*)
(*D\[CapitalLambda]B=(kt^2+y*MB^2-y*yl*M^2+yl*\[CapitalLambda]^2)/- yl;*)
(*D\[Phi]\[CapitalLambda]=kt^2+\[CapitalOmega]=kt^2+(1-z)m\[Phi]^2+z*\[CapitalLambda]^2;*)
(*\[CapitalOmega]=(1-z)m\[Phi]^2+z*\[CapitalLambda]^2;*)
(*D\[Phi]B+m\[Phi]^2-\[CapitalDelta]^2=(kt^2+(\[CapitalDelta]+M y)^2)/-yl;*)


\[CapitalOmega]=\[CapitalOmega];(*\:8d39\:66fc\:53c2\:6570\:5316\:5f97\:5230\:7684\:7ec4\:5408*)
z=z;(*\:8d39\:66fc\:53c2\:6570\:5316\:7684\:53c2\:6570*)
\[Delta]=\[Delta]; (*\[Delta]\:51fd\:6570*)
s\[Mu]=s\[Mu]; (*\:53d1\:6563\:79ef\:5206\:4e2d\:7684\:6807\:5ea6*)
