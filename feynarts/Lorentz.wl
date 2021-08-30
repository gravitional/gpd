(* ::Package:: *)

(* Patched for use with FeynCalc *)
(*
	Lorentz . gen
		\:6d1b\:4f26\:5179\:6846\:67b6\:7684\:901a\:7528\:6a21\:578b\:6587\:4ef6
		\:4f5c\:8005\:ff1aA . Denner, H . Eck, O . Hahn, S . Kueblbeck 1995
		\:6700\:540e\:7531Thomas Hahn\:4e8e13\:5e743\:670825\:65e5\:4fee\:6539
		
\:8be5\:6587\:4ef6\:5b9a\:4e49\:4e86\:4e00\:822c\:7684\:89e3\:6790\:4f20\:64ad\:5b50\:548c\:8026\:5408\:5e38\:6570\:3002
\:8fd9\:91cc\:63cf\:8ff0\:7684\:6a21\:578b\:662f\:6807\:51c6\:7684\:53ef\:91cd\:6b63\:5316\:7684\:901a\:7528\:6a21\:578b
\:573a\:7406\:8bba\:7684\:6807\:51c6\:901a\:7528\:6a21\:578b\:ff0811\:4e2a\:4e00\:822c\:8026\:5408\:5e38\:6570\:ff09\:ff0c\:4ee5\:53ca3\:4e2a\:989d\:5916\:76842--\:9876\:89d2\:3002

Reference:
	A. Denner, "Techniques for the calculation of electroweak
	radiative corrections at the one-loop level and results for
	W-physics at LEP200", Fortschr. d. Physik 41 (1993) 4
*)

	(*\:8fd0\:52a8\:5b66\:6307\:6807 \:662f\:6cbf\:7740\:4f20\:64ad\:5b50 "\:4f20\:8f93" \:7684\:3002
	  KinematicIndices[X] = {Name} \:610f\:5473\:7740generic\:573a X
	  \:5c06\:6cbf\:7740\:4f20\:64ad\:5b50\:643a\:5e26\:4e00\:4e2a Index[Name, i]\:3002  
	 X[ n, {m..}, p, {Index[Name, i]} -> {Index[Name, i + 1]} ] *)

KinematicIndices[ F ] = {};
KinematicIndices[ V ] = {Lorentz};
KinematicIndices[ S ] = {};
KinematicIndices[ SV ] = {Lorentz};
KinematicIndices[ U ] = {}

$FermionLines = True

P$NonCommuting = F | U

Attributes[ FAMetricTensor ] = Attributes[ FAScalarProduct ] = {Orderless}


FAFourVector/: -FAFourVector[ mom_, mu___ ] := FAFourVector[Expand[-mom], mu]

FAFourVector[ 0, ___ ] = 0


SpinorType[j_Integer, ___] := MajoranaSpinor /; SelfConjugate[F[j]]

SpinorType[_Integer, __] = FADiracSpinor


M$GenericPropagators = {

	(* \:4e00\:822c\:8d39\:7c73\:5b50\:4f20\:64ad\:5b50: *)

  AnalyticalPropagator[External][ s1 F[j1, mom] ] == (*j1 \:8868\:793a\:81ea\:65cb\:ff0cmom \:8868\:793a\:52a8\:91cf*)
    FANonCommutative[ SpinorType[j1][-mom, Mass[F[j1]]] ],
(*
\:5907\:6ce8:
	   \:8d39\:7c73\:5b50\:4f20\:64ad\:5b50\:ff08\:548c\:5176\:4ed6\:6240\:6709\:4f20\:64ad\:5b50\:4e00\:6837\:ff09\:ff0c\:5176\:52a8\:91cf\:4ece\:5de6\:81f3\:53f3. 
	   fermion flow\:ff08\:5bf9\:4e8e\:72c4\:62c9\:514b\:8d39\:7c73\:5b50\:ff1afermion number flow\:ff09\:4ece\:53f3\:5230\:5de6.
	   \:5982\:679c\:4f20\:64ad\:5b50\:5185\:90e8\:7684\:8d39\:7c73\:5b50\:65e0\:7b26\:53f7 (\:5373\:8d39\:7c73\:5b50\:6570\:6d41\:4e0e\:8d39\:7c73\:5b50\:6d41\:76f8\:53cd\:ff0c\:6216\:8005\:8d39\:7c73\:5b50\:662f\:81ea\:5171\:8f6d\:7684), \:6211\:4eec\:4f7f\:7528\:5185\:90e8\:4f20\:64ad\:5b50 S(-p) \:5373\:53ef.
	   \:5982\:679c\:8d39\:7c73\:5b50\:6709\:4e0d\:540c\:7684\:7b26\:53f7\:ff0c\:6839\:636e\:9a6c\:7ea6\:62c9\:7eb3\:7684\:8bba\:6587, \:6211\:4eec\:5fc5\:987b\:4f7f\:7528\:8d39\:66fc\:89c4\:5219 S(p).
	   \:7136\:800c\:ff0c\:8fd9\:4e2a\:89c4\:5219\:662f\:9488\:5bf9\:4e8e\:ff1a\:8d39\:7c73\:5b50\:6d41\:4e0e\:52a8\:91cf\:6d41\:76f8\:53cd\:7684\:60c5\:5f62\:7ed9\:51fa\:7684\:ff0c\:6240\:4ee5\:ff0c\:6211\:4eec\:518d\:6b21\:5f97\:5230 S(-p).
*)

  AnalyticalPropagator[Internal][ s1 F[j1, mom] ] ==
    FANonCommutative[ FADiracSlash[-mom] + Mass[F[j1]] ] *
      I FAPropagatorDenominator[mom, Mass[F[j1]]], 

	(* \:4e00\:822c \:77e2\:91cf \:73bb\:8272\:5b50 \:4f20\:64ad\:5b50: *)

  AnalyticalPropagator[External][ s1 V[j1, mom, {li2}] ] == (* j1 \:662f\:77e2\:91cf\:573a\:7684\:79cd\:7c7b\:ff0cmom \:52a8\:91cf, li2 \:662f\:5916\:90e8\:6307\:6807 *)
    FAPolarizationVector[V[j1], mom, li2],

  AnalyticalPropagator[Internal][ s1 V[j1, mom, {li1} -> {li2}] ] == (* j1 \:662f\:77e2\:91cf\:573a\:7684\:79cd\:7c7b\:ff0c{li1} -> {li2} \:8868\:793a\:4ece li1 \:5230 li2 *)
    -I FAPropagatorDenominator[mom, Mass[V[j1]]]*(
    FAMetricTensor[li1, li2] - (1 - FAGaugeXi[V[j1]]) *
         FAFourVector[mom, li1] FAFourVector[mom, li2] *
         FAPropagatorDenominator[mom, Sqrt[FAGaugeXi[V[j1]]] Mass[V[j1]]]
         ),

	(* general mixing scalar-vector propagator: *)

  AnalyticalPropagator[Internal][ s1 SV[j1, mom, {li1} -> {li2}] ] == 
    I Mass[SV[j1]] FAPropagatorDenominator[mom, Mass[SV[j1]]] *
      FAFourVector[mom, If[s1 == 1 || s1 == -2, li1, li2]],

	(* general scalar propagator: *)

  AnalyticalPropagator[External][ s1 S[j1, mom] ] == 1,

  AnalyticalPropagator[Internal][ s1 S[j1, mom] ] ==
    I FAPropagatorDenominator[mom, Sqrt[FAGaugeXi[S[j1]]] Mass[S[j1]]],

	(* general Fadeev-Popov ghost propagator: *)

  AnalyticalPropagator[External][ s1 U[j1, mom] ] == 1,

  AnalyticalPropagator[Internal][ s1 U[j1, mom] ] ==
    I FAPropagatorDenominator[mom, Sqrt[FAGaugeXi[U[j1]]] Mass[U[j1]]]
}

	(* Definition of the generic couplings.
	   The couplings must be defined as a Dot product of the (generic)
	   coupling vector G[+/-][ field1, field2, .. ] and the
	   kinematical vector Gamma = {Gamma1, Gamma2, ...}.
	   The kinematical vector must have the following properties:
	   a) the entries of Gamma must close under permutation of the
	      fields, i.e. under permutation of the momenta and
	      kinematical indices. One exception is allowed: if the
	      elements of Gamma only change their signs under certain
	      permutations (e.g. Gamma1 = mom1 - mom2), a coupling vector
	      G[-] can be used.
	      This leads to the following behaviour during the
	      construction of the classes couplings: if a permuted
	      coupling was found and the corresponding permutation doesn't
	      resolve the coupling vector entry, then the program tries
	      the negative expression of the corresponding Gamma and
	      multiplies the coupling with (-1).
	   b) the entries of the kinematical vector have to be closed
	      under application of the M$FlippingRules, i.e. fermionic
	      couplings have to be written such that the flipped couplings
	      are present in the generic coupling. Again, it is possible
	      to define flippings that change the sign of Gamma and to
	      take care for those signs by using a G[-]. *)

M$GenericCouplings = {

	(* V-V: *)

  AnalyticalCoupling[ s1 V[j1, mom1, {li1}], s2 V[j2, mom2, {li2}] ] ==
    G[1][s1 V[j1], s2 V[j2]] .
      { FAMetricTensor[li1, li2] FAScalarProduct[mom1, mom2],
        FAMetricTensor[li1, li2],
        FAFourVector[mom1, li2] FAFourVector[mom2, li1] },

	(* S-V: *)

  AnalyticalCoupling[ s1 S[j1, mom1], s2 V[j2, mom2, {li2}] ] ==
    G[1][s1 S[j1], s2 V[j2]] .
      { FAFourVector[mom1, li2],
        FAFourVector[mom2, li2] },

	(* S-S: *)

  AnalyticalCoupling[ s1 S[j1, mom1], s2 S[j2, mom2] ] ==
    G[1][s1 S[j1], s2 S[j2]] .
      { FAScalarProduct[mom1, mom2],
        1 },

	(* U-U: *)

  AnalyticalCoupling[ s1 U[j1, mom1], s2 U[j2, mom2] ] ==
    G[-1][s1 U[j1], s2 U[j2]] .
      { FAScalarProduct[mom1, mom2],
        1 },

	(* F-F: *)

  AnalyticalCoupling[ s1 F[j1, mom1], s2 F[j2, mom2] ] ==
    G[-1][s1 F[j1], s2 F[j2]] .
      { FANonCommutative[FADiracSlash[mom1], FAChiralityProjector[-1]],
        FANonCommutative[FADiracSlash[mom2], FAChiralityProjector[+1]],
        FANonCommutative[FAChiralityProjector[-1]],
        FANonCommutative[FAChiralityProjector[+1]] },

	(* V-V-V-V: *)

  AnalyticalCoupling[ s1 V[j1, mom1, {li1}], s2 V[j2, mom2, {li2}], 
      s3 V[j3, mom3, {li3}], s4 V[j4, mom4, {li4}] ] ==
    G[1][s1 V[j1], s2 V[j2], s3 V[j3], s4 V[j4]] .
      { FAMetricTensor[li1, li2] FAMetricTensor[li3, li4],
        FAMetricTensor[li1, li3] FAMetricTensor[li2, li4],
        FAMetricTensor[li1, li4] FAMetricTensor[li3, li2] },

	(* V-V-V: *)

  AnalyticalCoupling[ s1 V[j1, mom1, {li1}], s2 V[j2, mom2, {li2}], 
      s3 V[j3, mom3, {li3}] ] ==
    G[-1][s1 V[j1], s2 V[j2], s3 V[j3]] .
      { FAMetricTensor[li1, li2] FAFourVector[mom2 - mom1, li3] +
          FAMetricTensor[li2, li3] FAFourVector[mom3 - mom2, li1] +
          FAMetricTensor[li3, li1] FAFourVector[mom1 - mom3, li2] },

	(* S-S-S-S: *)

  AnalyticalCoupling[ s1 S[j1, mom1], s2 S[j2, mom2],
      s3 S[j3, mom3], s4 S[j4, mom4] ] ==
    G[1][s1 S[j1], s2 S[j2], s3 S[j3], s4 S[j4]] .
      { 1 },

	(* S-S-S: *)

  AnalyticalCoupling[ s1 S[j1, mom1], s2 S[j2, mom2], s3 S[j3, mom3] ] ==
    G[1][s1 S[j1], s2 S[j2], s3 S[j3]] .
      { 1 },

	(* S-S-V-V: *)

  AnalyticalCoupling[ s1 S[j1, mom1], s2 S[j2, mom2],
      s3 V[j3, mom3, {li3}], s4 V[j4, mom4, {li4}] ] ==
    G[1][s1 S[j1], s2 S[j2], s3 V[j3], s4 V[j4]] .
      { FAMetricTensor[li3, li4] },

	(* S-S-V: *)

  AnalyticalCoupling[ s1 S[j1, mom1], s2 S[j2, mom2],
      s3 V[j3, mom3, {li3}] ] == 
    G[-1][s1 S[j1], s2 S[j2], s3 V[j3]] .
      { FAFourVector[mom1 - mom2, li3] },

	(* S-V-V: *)

  AnalyticalCoupling[ s1 S[j1, mom1], s2 V[j2, mom2, {li2}], 
      s3 V[j3, mom3, {li3}] ] ==
    G[1][s1 S[j1], s2 V[j2], s3 V[j3]] .
      { FAMetricTensor[li2, li3] },

	(* F-F-V: *)

  AnalyticalCoupling[ s1 F[j1, mom1], s2 F[j2, mom2],
      s3 V[j3, mom3, {li3}] ] ==
    G[-1][s1 F[j1], s2 F[j2], s3 V[j3]] .
      { FANonCommutative[FADiracMatrix[li3], FAChiralityProjector[-1]], 
        FANonCommutative[FADiracMatrix[li3], FAChiralityProjector[+1]] },
 
	(* F-F-S: *)

  AnalyticalCoupling[ s1 F[j1, mom1], s2 F[j2, mom2], s3 S[j3, mom3] ] ==
    G[1][s1 F[j1], s2 F[j2], s3 S[j3]] . 
      { FANonCommutative[FAChiralityProjector[-1]],  
        FANonCommutative[FAChiralityProjector[+1]] },

	(* U-U-V: *)

  AnalyticalCoupling[ s1 U[j1, mom1], s2 U[j2, mom2],
      s3 V[j3, mom3, {li3}] ] ==
    G[1][s1 U[j1], s2 U[j2], s3 V[j3]] .
      { FAFourVector[mom1, li3],
        FAFourVector[mom2, li3] },

	(* S-U-U: *)

  AnalyticalCoupling[ s1 S[j1, mom1], s2 U[j2, mom2], s3 U[j3, mom3] ] ==
    G[1][s1 S[j1], s2 U[j2], s3 U[j3]] .
      { 1 }
}

	(* FlippingRules: the flipping rules determines how Dirac
	   objects change when the order of fermion fields in the
	   coupling is reversed. In other words, it defines how the
	   coupling C[F, -F, ...] is derived from C[-F, F, ...].
	   Of the elements of the Dirac algebra we need to consider
	   only gamma_mu omega_pm since the others are either
	   unchanged or not used (sigma_{mu,nu}).
	   See Denner, Eck, Hahn, Kueblbeck, NPB 387 (1992) 467. *)

M$FlippingRules =
  FANonCommutative[dm:_FADiracMatrix | _FADiracSlash, FAChiralityProjector[pm_]] ->
    -FANonCommutative[dm, FAChiralityProjector[-pm]]

	(* TruncationRules: rule for omitting the wave functions of
	   external Propagators defined in this file. *)

M$TruncationRules = {
  _FAPolarizationVector -> 1,
  _FADiracSpinor -> 1,
  _MajoranaSpinor -> 1 
}

	(* LastGenericRules: the very last rules that are applied to an
	   amplitude before it is returned by CreateFeynAmp. *)

M$LastGenericRules = {
  FAPolarizationVector[p_, _. mom:FourMomentum[Outgoing, _], li_] :>
    Conjugate[FAPolarizationVector][p, mom, li]
}


	(* cosmetics: *)

	(*  left spinor in chain + mom incoming -> \bar v
	    left spinor in chain + mom outgoing -> \bar u
	   right spinor in chain + mom incoming -> u
	   right spinor in chain + mom outgoing -> v *)
Format[
  FermionChain[
    FANonCommutative[_[s1_. mom1_, mass1_]],
    r___,
    FANonCommutative[_[s2_. mom2_, mass2_]]] ] :=
  Overscript[If[FreeQ[mom1, Incoming], "u", "v"], "_"][mom1, mass1] .
    r . If[FreeQ[mom2, Outgoing], "u", "v"][mom2, mass2]

Format[ FADiracSlash ] = "gs"

Format[ FADiracMatrix ] = "ga"

Format[ FAChiralityProjector[1] ] = Subscript["om", "+"]

Format[ FAChiralityProjector[-1] ] = Subscript["om", "-"]

Format[ FAGaugeXi[a_] ] := Subscript["xi", a]

Format[ FAPolarizationVector ] = "ep"

Unprotect[Conjugate];
Format[ Conjugate[a_] ] = Superscript[a, "*"];
Protect[Conjugate]

Format[ FAMetricTensor ] = "g"

Format[ FAScalarProduct[a__] ] := Dot[a]

Format[ FAFourVector[a_, b_] ] := a[b]

Format[ FAFourVector[a_] ] := a

