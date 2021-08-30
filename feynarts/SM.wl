(* ::Package:: *)

(* Patched for use with FeynCalc *)
(*
	SM .mod
		\:6807\:51c6\:6a21\:578b\:7684\:7c7b\:6a21\:578b\:6587\:4ef6
		\:4f5c\:8005\:ff1aHagen Eck\:548cSepp Kueblbeck 1995\:5e74
		\:6700\:540e\:7531Thomas Hahn\:4e8e19\:5e743\:67085\:65e5\:4fee\:6539
		
\:8fd9\:4e2a\:6587\:4ef6\:5305\:542bFeynArts\:7684Classes\:6a21\:578b\:7684\:5b9a\:4e49\:3002
\:5b83\:9700\:8981\:901a\:7528\:6a21\:578b\:6587\:4ef6Lorentz .gen\:3002

\:5f53\:4f60\:6539\:53d8\:4e8b\:7269\:65f6\:ff0c\:8bf7\:8bb0\:4f4f\:3002

-- \:6240\:6709\:7684\:7c92\:5b50\:90fd\:662f\:6309\:7c7b\:6392\:5217\:7684\:3002\:5bf9\:4e8e\:5355\:7c92\:5b50\:6a21\:578b\:7684\:5b9a\:4e49\:ff0c\:6bcf\:4e2a\:7c92\:5b50\:90fd\:751f\:6d3b\:5728\:5b83\:81ea\:5df1\:7684\:7c7b\:4e2d\:3002

-- \:5bf9\:4e8e\:6bcf\:4e2a\:7c7b\:6765\:8bf4\:ff0c\:516c\:5171\:7684 SelfConjugate \:884c\:4e3a\:548c IndexRange \:5fc5\:987b\:5b58\:5728\:4e8e\:5b9a\:4e49\:4e2d\:3002

-- \:91cd\:8981\:7684\:662f\:ff1acoupling matrices \:5fc5\:987b\:6309\:7167\:4e0e Generic coupling \:5728\:76f8\:540c\:7684\:9636\:58f0\:660e\:3002

Reference:
	Ansgar Denner, "Techniques for the calculation of electroweak
	radiative corrections at one-loop level and results for
	W-physics at LEP200", Fortschr. d. Physik, 41 (1993) 4.

Oct 95: one-loop counter terms added by Stefan Bauberger:
	Some corrections and addition of all one-loop counter terms
	according to A. Denner.  The gauge-fixing terms are assumed not
	to be renormalized.  The Denner conventions are extended to
	include field renormalization of the Goldstone bosons.
	The counter terms associated with quark mixing are not well
	tested yet.

Apr 99: Christian Schappacher added colour indices for the quarks.

Apr 99:	Terms for ghost sector updated by Ayres Freitas.
	The gauge-fixing terms are still assumed not to be renormalized
	but the renormalized gauge parameters follow the R_xi-gauge.
	In addition, renormalization for the ghost fields is included.
	The 2-loop counter terms for vector-boson selfenergies and for
	the W-nu-l vertex have been added.
	Old versions of the changes of sbau are removed!

Apr 01:	Thomas Hahn added the definitions of the renormalization
	constants a la A. Denner.

May 13: Christian Schappacher added ColorCharge.

Mar 14: Cyril Pietsch corrected some ghost couplings and RCs.


\:8be5\:6587\:4ef6\:5f15\:5165\:4e86\:4ee5\:4e0b\:7b26\:53f7:

	\:8026\:5408\:5e38\:6570\:548c\:8d28\:91cf:
	------------------------------
	FCGV["EL"]:		\:7535\:5b50\:7535\:8377 (Thomson limit)
	FCGV["CW"], FCGV["SW"]:		\:6e29\:4f2f\:683c\:89d2\:7684\:4f59\:5f26\:548c\:6b63\:5f26

	FCGV["MW"], FCGV["MZ"], FCGV["MH"]:	W, Z, Higgs masses

	MLE:		\:8f7b\:5b50\:8d28\:91cf
	FCGV["ME"], FCGV["MM"], FCGV["ML"]:	\:8f7b\:5b50\:8d28\:91cf (e, mu, tau)

	MQU:		u-type \:5938\:514b \:7c7b \:8d28\:91cf
	FCGV["MU"], FCGV["MC"], FCGV["MT"]:	u-type quark masses (up, charm, top)

	MQD:		d-type \:5938\:514b \:7c7b \:8d28\:91cf
	FCGV["MD"], FCGV["MS"], FCGV["MB"]:	d-type quark masses (down, strange, bottom)

	CKM:		quark \:6df7\:5408 \:77e9\:9635
			(\:5bf9\:4e8e\:6ca1\:6709\:5938\:514b\:6df7\:5408\:7684\:60c5\:5f62\:ff0c\:4ee4 CKM = IndexDelta )

	FAGaugeXi[A, W, Z]: photon, W, Z \:89c4\:8303\:53c2\:6570


	\:5355\:5708 \:91cd\:6b63\:5316 \:5e38\:6570 (RCs): delta(\:91cd\:6b63\:5316)Z(\:5e38\:6570)e(\:7535\:8377)1(\:7f16\:53f71)
	-----------------------------------------
	dZe1:		\:7535\:78c1 \:8377 RC
	dSW1, dCW1:	\:6e29\:4f2f\:683c\:89d2 \:6b63\:5f26/\:4f59\:5f26 RC

	dZH1, dMHsq1:	Higgs \:573a \:548c \:8d28\:91cf RC
	dZW1, dMWsq1:	W \:573a \:548c \:8d28\:91cf RC
	dMZsq1:		Z \:8d28\:91cf RC
	dZZZ1, dZZA1,
	dZAZ1, dZAA1:	Z \:548c \:5149\:5b50 \:573a RCs

	dMf1:		\:8d39\:7c73\:5b50 \:8d28\:91cf RCs
	dZfL1, dZfR1:	\:8d39\:7c73\:5b50 \:573a RCs

	dCKM1:		\:5938\:514b \:6df7\:5408 \:77e9\:9635 RCs

	dZG01, dZGp1:	\:975e\:7269\:7406 \:6807\:91cf\:573a\:7684 \:573a RC
	dUZZ1, dUZA1,
	dUAZ1, dUAA1:	\:573a RCs for \:5149\:5b50 and Z \:9b3c\:573a
	dUW1:		\:573a RC for +/- ghosts

	\:53cc\:5708 \:91cd\:6b63\:5316 \:5e38\:6570:
	-----------------------------------
	dZe2:           \:7535\:78c1 \:8377 RC
	dSW2:           \:5f31 \:6df7\:5408\:89d2 \:6b63\:5f26/\:4f59\:5f26 RC

	dZW2, dMWsq2:	W \:573a \:548c \:8d28\:91cf RC
	dMZsq2:		Z \:8d28\:91cf RC
	dZZZ2, dZZA2,
	dZAZ2, dZAA2:	Z \:548c \:5149\:5b50 \:573a RCs

	dZfL2:		\:8d39\:7c73\:5b50 \:573a RCs
*)

$CKM = $CKM === True;

FAPrint[1, ""];
FAPrint[1, Definition[$CKM]];
FAPrint[1, ""];

If[ !$CKM, CKM = IndexDelta; _dCKM1 = 0 ]

IndexRange[ Index[Generation] ] = Range[3]

IndexRange[ Index[Colour] ] = NoUnfold[Range[3]]

IndexStyle[ Index[Generation, i_Integer] ] := Alph[i + 8]

MaxGenerationIndex = 3


ViolatesQ[ q__ ] := Plus[q] =!= 0


mdZfLR1[ type_, j1_, j2_ ] :=
  Mass[F[type, {j1}]]/2 dZfL1[type, j1, j2] +
    Mass[F[type, {j2}]]/2 Conjugate[dZfR1[type, j2, j1]]

mdZfRL1[ type_, j1_, j2_ ] :=
  Mass[F[type, {j1}]]/2 dZfR1[type, j1, j2] +
    Mass[F[type, {j2}]]/2 Conjugate[dZfL1[type, j2, j1]]


(* \:8f7b\:5b50 \:573a\:5f3a \:91cd\:6b63\:5316 RCs \:662f\:5bf9\:89d2\:7684: *)

dZfL1[ type:1 | 2, j1_, j2_ ] :=
  IndexDelta[j1, j2] dZfL1[type, j1, j1] /; j1 =!= j2

dZfR1[ type:1 | 2, j1_, j2_ ] :=
  IndexDelta[j1, j2] dZfR1[type, j1, j1] /; j1 =!= j2


(* some short-hands for fermionic couplings: *)

FermionCharge[1] = 0;
FermionCharge[2] = -1;
FermionCharge[3] = 2/3;
FermionCharge[4] = -1/3

gR[ type_ ] :=
  -FCGV["SW"]/FCGV["CW"] FermionCharge[type];
gL[ type_ ] :=
  (If[ OddQ[type], 1/2, -1/2 ] - FCGV["SW"]^2 FermionCharge[type])/(FCGV["SW"] FCGV["CW"]);
dgR[ type_ ] :=
  gR[type] (dZe1 + 1/(FCGV["CW"]^2 FCGV["SW"]) dSW1);
dgL[ type_ ] :=
  If[ OddQ[type], 1/2, -1/2 ]/(FCGV["SW"] FCGV["CW"]) *
    (dZe1 + (FCGV["SW"]^2 - FCGV["CW"]^2)/(FCGV["CW"]^2 FCGV["SW"]) dSW1) + dgR[type]


M$ClassesDescription = {

	(* Leptons (neutrino): I_3 = +1/2, Q = 0 *)
  F[1] == {
	SelfConjugate -> False,
	Indices -> {Index[Generation]},
	Mass -> 0,
	QuantumNumbers -> {0 Charge, LeptonNumber},
	PropagatorLabel -> ComposedChar["\\nu", Index[Generation]],
	PropagatorType -> Straight,
	PropagatorArrow -> Forward },

	(* Leptons (electron): I_3 = -1/2, Q = -1 *)
  F[2] == {
	SelfConjugate -> False,
	Indices -> {Index[Generation]},
	Mass -> MLE,
	QuantumNumbers -> {-1 Charge, LeptonNumber},
	PropagatorLabel -> ComposedChar["e", Index[Generation]],
	PropagatorType -> Straight,
	PropagatorArrow -> Forward },

	(* Quarks (u): I_3 = +1/2, Q = +2/3 *)
  F[3] == {
	SelfConjugate -> False,
	Indices -> {Index[Generation], Index[Colour]},
	Mass -> MQU,
	QuantumNumbers -> {2/3 Charge, Sqrt[4/3] ColorCharge},
	PropagatorLabel -> ComposedChar["u", Index[Generation]],
	PropagatorType -> Straight,
	PropagatorArrow -> Forward },

	(* Quarks (d): I_3 = -1/2, Q = -1/3 *) 
  F[4] == {
	SelfConjugate -> False,
	Indices -> {Index[Generation], Index[Colour]},
	Mass -> MQD,
	QuantumNumbers -> {-1/3 Charge, Sqrt[4/3] ColorCharge},
	PropagatorLabel -> ComposedChar["d", Index[Generation]],
	PropagatorType -> Straight, 
	PropagatorArrow -> Forward },

	(* Gauge bosons: Q = 0 *)
  V[1] == {
	SelfConjugate -> True,
	Indices -> {},
	Mass -> 0,
	PropagatorLabel -> "\\gamma",
	PropagatorType -> Sine,
	PropagatorArrow -> None },

  V[2] == {
	SelfConjugate -> True, 
	Indices -> {},
	Mass -> FCGV["MZ"],
	PropagatorLabel -> "Z",
	PropagatorType -> Sine,
	PropagatorArrow -> None },

	(* Gauge bosons: Q = -1 *)
  V[3] == {
	SelfConjugate -> False,
	Indices -> {},
	Mass -> FCGV["MW"],
	QuantumNumbers -> -Charge,
	PropagatorLabel -> "W",
	PropagatorType -> Sine,
	PropagatorArrow -> Forward },

(*
  Mix[V,V][4] == {
	SelfConjugate -> True,
	Indices -> {},
	Mass -> MAZ,
	MixingPartners -> {V[1], V[2]},
	PropagatorLabel -> {"\\gamma", "Z"},
	PropagatorType -> Sine,
	PropagatorArrow -> None },
*)

	(* mixing Higgs gauge bosons: Q = 0 *) 
  Mix[S,V][2] == {
	SelfConjugate -> True,
	Indices -> {},
	Mass -> FCGV["MZ"],
	MixingPartners -> {S[2], V[2]} },

	(* mixing Higgs gauge bosons: charged *) 
  Mix[S,V][3] == {
	SelfConjugate -> False,
	Indices -> {},
	Mass -> FCGV["MW"],
	QuantumNumbers -> -Charge,
	MixingPartners -> {S[3], V[3]} },

	(* physical Higgs: Q = 0 *) 
  S[1] == {
	SelfConjugate -> True,
	Indices -> {},
	Mass -> FCGV["MH"],
	PropagatorLabel -> "H",
	PropagatorType -> ScalarDash,
	PropagatorArrow -> None },

	(* unphysical Higgs: neutral *) 
  S[2] == {
	SelfConjugate -> True,
	Indices -> {},
	Mass -> FCGV["MZ"],
	PropagatorLabel -> ComposedChar["G", Null, "0"],
	PropagatorType -> ScalarDash,
	PropagatorArrow -> None },

	(* unphysical Higgs: Q = -1 *)  
  S[3] == {
	SelfConjugate -> False,
	Indices -> {},
	Mass -> FCGV["MW"],
	QuantumNumbers -> -Charge,
	PropagatorLabel -> "G",
	PropagatorType -> ScalarDash,
	PropagatorArrow -> Forward },

	(* Ghosts: neutral *) 
  U[1] == {
	SelfConjugate -> False,
	Indices -> {},
	Mass -> 0,
	QuantumNumbers -> GhostNumber,
	PropagatorLabel -> ComposedChar["u", "\\gamma"],
	PropagatorType -> GhostDash,
	PropagatorArrow -> Forward },

  U[2] == {
	SelfConjugate -> False,
	Indices -> {},
	Mass -> FCGV["MZ"],
	QuantumNumbers -> GhostNumber,
	PropagatorLabel -> ComposedChar["u", "Z"],
	PropagatorType -> GhostDash,
	PropagatorArrow -> Forward },

	(* Ghosts: charged *) 
  U[3] == {
	SelfConjugate -> False,
	Indices -> {},
	Mass -> FCGV["MW"],
	QuantumNumbers -> {-1 Charge, GhostNumber},
	PropagatorLabel -> ComposedChar["u", "-"],
	PropagatorType -> GhostDash,
	PropagatorArrow -> Forward },

  U[4] == {
	SelfConjugate -> False,
	Indices -> {},
	Mass -> FCGV["MW"],
	QuantumNumbers -> {1 Charge, GhostNumber},
	PropagatorLabel -> ComposedChar["u", "+"],
	PropagatorType -> GhostDash,
	PropagatorArrow -> Forward }
}

MLE[1] = FCGV["ME"];
MLE[2] = FCGV["MM"];
MLE[3] = FCGV["ML"];
MQU[1] = FCGV["MU"];
MQU[2] = FCGV["MC"];
MQU[3] = FCGV["MT"];
MQD[1] = FCGV["MD"];
MQD[2] = FCGV["MS"];
MQD[3] = FCGV["MB"];
MQU[gen_, _] := MQU[gen];
MQD[gen_, _] := MQD[gen]

TheLabel[ F[1, {1}] ] = ComposedChar["\\nu", "e"]; 
TheLabel[ F[1, {2}] ] = ComposedChar["\\nu", "\\mu"]; 
TheLabel[ F[1, {3}] ] = ComposedChar["\\nu", "\\tau"]; 
TheLabel[ F[2, {1}] ] = "e"; 
TheLabel[ F[2, {2}] ] = "\\mu"; 
TheLabel[ F[2, {3}] ] = "\\tau";
TheLabel[ F[3, {1, ___}] ] = "u"; 
TheLabel[ F[3, {2, ___}] ] = "c";
TheLabel[ F[3, {3, ___}] ] = "t";
TheLabel[ F[4, {1, ___}] ] = "d"; 
TheLabel[ F[4, {2, ___}] ] = "s";
TheLabel[ F[4, {3, ___}] ] = "b"

FAGaugeXi[ V[1] ] = FAGaugeXi[A];
FAGaugeXi[ V[2] ] = FAGaugeXi[Z];
FAGaugeXi[ V[3] ] = FAGaugeXi[W];
FAGaugeXi[ S[1] ] = 1;
FAGaugeXi[ S[2] ] = FAGaugeXi[Z];
FAGaugeXi[ S[3] ] = FAGaugeXi[W];
FAGaugeXi[ U[1] ] = FAGaugeXi[A];
FAGaugeXi[ U[2] ] = FAGaugeXi[Z];
FAGaugeXi[ U[3] ] = FAGaugeXi[W];
FAGaugeXi[ U[4] ] = FAGaugeXi[W]


M$CouplingMatrices = {

	(* V-V:  G(+) . { -g[mu, nu] mom^2, g[mu, nu], -mom[mu] mom[nu] } *)

  C[ -V[3], V[3] ] == I *
    { {0, dZW1, dZW2},
      {0, FCGV["MW"]^2 dZW1 + dMWsq1, FCGV["MW"]^2 dZW2 + dMWsq2 + dMWsq1 dZW1},
      {0, -dZW1, -dZW2} },

  C[ V[2], V[2] ] == I *
    { {0, dZZZ1, dZZZ2 + 1/4 dZAZ1^2},
      {0, FCGV["MZ"]^2 dZZZ1 + dMZsq1, FCGV["MZ"]^2 dZZZ2 + dMZsq2 + dMZsq1 dZZZ1},
      {0, -dZZZ1, -dZZZ2 - 1/4 dZAZ1^2} },

  C[ V[1], V[1] ] == I *
    { {0, dZAA1, dZAA2 + 1/4 dZZA1^2},
      {0, 0, 1/4 FCGV["MZ"]^2 dZZA1^2},
      {0, -dZAA1, -dZAA2 - 1/4 dZZA1^2} },

  C[ V[1], V[2] ] == I *
    { {0, dZAZ1/2 + dZZA1/2,
    	  (dZAZ2 + dZZA2 + 1/2 dZZA1 dZZZ1 + 1/2 dZAZ1 dZAA1)/2},
      {0, FCGV["MZ"]^2 dZZA1/2,
          (FCGV["MZ"]^2 dZZA2 + 1/2 FCGV["MZ"]^2 dZZZ1 dZZA1 + dMZsq1 dZZA1)/2},
      {0, -dZAZ1/2 - dZZA1/2,
          -(dZAZ2 + dZZA2 + 1/2 dZZA1 dZZZ1 + 1/2 dZAZ1 dZAA1)/2} },

	(* S-V:  G(+) . { mom1[mu], mom2[mu] } *)

  C[ S[3], -V[3] ] == I FCGV["MW"]/4 *
    { {0, -dZW1 - dZGp1 - dMWsq1/FCGV["MW"]^2},
      {0, dZW1 + dZGp1 + dMWsq1/FCGV["MW"]^2} },

  C[ -S[3], V[3] ] == I FCGV["MW"]/4 *
    { {0, dZW1 + dZGp1 + dMWsq1/FCGV["MW"]^2},
      {0, -dZW1 - dZGp1 - dMWsq1/FCGV["MW"]^2} },

  C[ S[2], V[2] ] == FCGV["MZ"]/4 *
    { {0, dZZZ1 + dZG01 + dMZsq1/FCGV["MZ"]^2},
      {0, -dZZZ1 - dZG01 - dMZsq1/FCGV["MZ"]^2} },

  C[ S[2], V[1] ] == FCGV["MZ"]/4 *
    { {0, dZZA1},
      {0, -dZZA1} },

	(* S-S:  G(+) . { -mom^2, 1 } *)

  C[ S[1], S[1] ] == -I *
    { {0, dZH1},
      {0, dMHsq1 + FCGV["MH"]^2 dZH1} },

  C[ S[2], S[2] ] == -I *
    { {0, dZG01},
      {0, -FCGV["EL"]/(2 FCGV["MW"] FCGV["SW"]) dTH1} },

  C[ S[3], -S[3] ] == -I *
    { {0, dZGp1},
      {0, -FCGV["EL"]/(2 FCGV["MW"] FCGV["SW"]) dTH1} },

	(* U-U:  G(+) . { -mom^2, 1 } *)

  C[ U[1], -U[1] ] == -I *
    { {0, -dZAA1/2 + dUAA1},
      {0, 0} },
  
  C[ U[2], -U[2] ] == -I *
    { {0, -dZZZ1/2 + dUZZ1},
      {0, FAGaugeXi[Z] (FCGV["MZ"]^2 (-dZG01/2 + dUZZ1) + dMZsq1/2) } },
  
  C[ U[2], -U[1] ] == -I *
    { {0, -dZAZ1/2 + dUAZ1},
      {0, 0} },
  
  C[ U[1], -U[2] ] == -I *
    { {0, -dZZA1/2 + dUZA1},
      {0, FAGaugeXi[Z] FCGV["MZ"]^2 dUZA1} },
  
  C[ U[3], -U[3] ] == -I *
    { {0, -dZW1/2 + dUW1},
      {0, FAGaugeXi[W] (FCGV["MW"]^2 (-dZGp1/2 + dUW1) + dMWsq1/2) } },

  C[ U[4], -U[4] ] == -I *
    { {0, -dZW1/2 + dUW1},
      {0, FAGaugeXi[W] (FCGV["MW"]^2 (-dZGp1/2 + dUW1) + dMWsq1/2) } },

	(* F-F:  G(+) . { slash[mom1] omega[-], slash[mom2] omega[+],
	                  omega[-], omega[+] } *)

  C[ -F[1, {j1}], F[1, {j2}] ] == I *
    { {0, -AddHC[dZfL1[1, j1, j2]]},
      {0, AddHC[dZfR1[1, j1, j2]]},
      {0, 0},
      {0, 0} },

  C[ -F[2, {j1}], F[2, {j2}] ] == I *
    { {0, -AddHC[dZfL1[2, j1, j2]]},
      {0, AddHC[dZfR1[2, j1, j2]]},
      {0, -mdZfLR1[2, j1, j2] - IndexDelta[j1, j2] dMf1[2, j1]},
      {0, -mdZfRL1[2, j1, j2] - IndexDelta[j1, j2] dMf1[2, j1]} },

  C[ -F[3, {j1, o1}], F[3, {j2, o2}] ] == I IndexDelta[o1, o2] *
    { {0, -AddHC[dZfL1[3, j1, j2]]},
      {0, AddHC[dZfR1[3, j1, j2]]},
      {0, -mdZfLR1[3, j1, j2] - IndexDelta[j1, j2] dMf1[3, j1]},
      {0, -mdZfRL1[3, j1, j2] - IndexDelta[j1, j2] dMf1[3, j1]} },

  C[ -F[4, {j1, o1}], F[4, {j2, o2}] ] == I IndexDelta[o1, o2] *
    { {0, -AddHC[dZfL1[4, j1, j2]]},
      {0, AddHC[dZfR1[4, j1, j2]]},
      {0, -mdZfLR1[4, j1, j2] - IndexDelta[j1, j2] dMf1[4, j1]},
      {0, -mdZfRL1[4, j1, j2] - IndexDelta[j1, j2] dMf1[4, j1]} },

	(* V-V-V-V:  G(+) . { g[mu1, mu2] g[mu3, mu4],
	                      g[mu1, mu4] g[mu2, mu3],
	                      g[mu1, mu3] g[mu2, mu4] } *)

  C[ -V[3], -V[3], V[3], V[3] ] == I FCGV["EL"]^2/FCGV["SW"]^2 *
    { {2, 4 dZe1 - 4 dSW1/FCGV["SW"] + 4 dZW1}, 
      {-1, -2 dZe1 + 2 dSW1/FCGV["SW"] - 2 dZW1},
      {-1, -2 dZe1 + 2 dSW1/FCGV["SW"] - 2*dZW1} },

  C[ -V[3], V[3], V[2], V[2] ] == -I FCGV["EL"]^2 FCGV["CW"]^2/FCGV["SW"]^2 *
    { {2, 4 dZe1 - 4 dSW1/(FCGV["SW"] FCGV["CW"]^2) + 2 dZW1 + 2 dZZZ1 - 2 dZAZ1 FCGV["SW"]/FCGV["CW"]}, 
      {-1, -2 dZe1 + 2 dSW1/(FCGV["SW"] FCGV["CW"]^2) - dZW1 - dZZZ1 + dZAZ1 FCGV["SW"]/FCGV["CW"]},
      {-1, -2 dZe1 + 2 dSW1/(FCGV["SW"] FCGV["CW"]^2) - dZW1 - dZZZ1 + dZAZ1 FCGV["SW"]/FCGV["CW"]} },

  C[ -V[3], V[3], V[1], V[2] ] == I FCGV["EL"]^2 FCGV["CW"]/FCGV["SW"] *
    { {2, 4 dZe1 - 2 dSW1/(FCGV["SW"] FCGV["CW"]^2) + 2 dZW1 +
            dZZZ1 + dZAA1 - FCGV["SW"]/FCGV["CW"] dZAZ1 - FCGV["CW"]/FCGV["SW"] dZZA1},
      {-1, -2 dZe1 + dSW1/(FCGV["SW"] FCGV["CW"]^2) - dZW1 -
            dZZZ1/2 - dZAA1/2 + FCGV["SW"]/FCGV["CW"] dZAZ1/2 + FCGV["CW"]/FCGV["SW"] dZZA1/2},
      {-1, -2 dZe1 + dSW1/(FCGV["SW"] FCGV["CW"]^2) - dZW1 -
            dZZZ1/2 - dZAA1/2 + FCGV["SW"]/FCGV["CW"] dZAZ1/2 + FCGV["CW"]/FCGV["SW"] dZZA1/2} },

  C[ -V[3], V[3], V[1], V[1] ] == -I FCGV["EL"]^2 *
    { {2, 4 dZe1 + 2 dZW1 + 2 dZAA1 - 2 FCGV["CW"]/FCGV["SW"] dZZA1}, 
      {-1, -2 dZe1 - dZW1 - dZAA1 + FCGV["CW"]/FCGV["SW"] dZZA1},
      {-1, -2 dZe1 - dZW1 - dZAA1 + FCGV["CW"]/FCGV["SW"] dZZA1} },

	(* V-V-V:  G(-) . (g[mu1, mu2] (p2 - p1)_mu3 +
	                   g[mu2, mu3] (p3 - p2)_mu1 +
	                   g[mu3, mu1] (p1 - p3)_mu2) *)

  C[ V[1], -V[3], V[3] ] == -I FCGV["EL"] *
    { {1, dZe1 + dZW1 + dZAA1/2 - FCGV["CW"]/FCGV["SW"] dZZA1/2} },

  C[ V[2], -V[3], V[3] ] == I FCGV["EL"] FCGV["CW"]/FCGV["SW"] *
    { {1, dZe1 - dSW1/(FCGV["SW"] FCGV["CW"]^2) + dZW1 + dZZZ1/2 - FCGV["SW"]/FCGV["CW"] dZAZ1/2} },

	(* S-S-S-S:  G(+) . 1 *)

  C[ S[1], S[1], S[1], S[1] ] == -3 I FCGV["EL"]^2 FCGV["MH"]^2/(4 FCGV["SW"]^2 FCGV["MW"]^2) *
    { {1, 2 dZe1 - 2 dSW1/FCGV["SW"] + dMHsq1/FCGV["MH"]^2 + FCGV["EL"]/(2 FCGV["SW"] FCGV["MW"] FCGV["MH"]^2) dTH1 -
            dMWsq1/FCGV["MW"]^2 + 2 dZH1} },

  C[ S[1], S[1], S[2], S[2] ] == -I FCGV["EL"]^2 FCGV["MH"]^2/(4 FCGV["SW"]^2 FCGV["MW"]^2) *
    { {1, 2 dZe1 - 2 dSW1/FCGV["SW"] + dMHsq1/FCGV["MH"]^2 + FCGV["EL"]/(2 FCGV["SW"] FCGV["MW"] FCGV["MH"]^2) dTH1 -
            dMWsq1/FCGV["MW"]^2 + dZH1 + dZG01} },

  C[ S[1], S[1], S[3], -S[3] ] == -I FCGV["EL"]^2 FCGV["MH"]^2/(4 FCGV["SW"]^2 FCGV["MW"]^2) *
    { {1, 2 dZe1 - 2 dSW1/FCGV["SW"] + dMHsq1/FCGV["MH"]^2 + FCGV["EL"]/(2 FCGV["SW"] FCGV["MW"] FCGV["MH"]^2) dTH1 -
            dMWsq1/FCGV["MW"]^2 + dZH1 + dZGp1} },

  C[ S[2], S[2], S[2], S[2] ] == -3 I FCGV["EL"]^2 FCGV["MH"]^2/(4 FCGV["SW"]^2 FCGV["MW"]^2) *
    { {1, 2 dZe1 - 2 dSW1/FCGV["SW"] + dMHsq1/FCGV["MH"]^2 + FCGV["EL"]/(2 FCGV["SW"] FCGV["MW"] FCGV["MH"]^2) dTH1 -
            dMWsq1/FCGV["MW"]^2 + 2 dZG01} },

  C[ S[2], S[2], S[3], -S[3] ] == -I FCGV["EL"]^2 FCGV["MH"]^2/(4 FCGV["SW"]^2 FCGV["MW"]^2) *
    { {1, 2 dZe1 - 2 dSW1/FCGV["SW"] + dMHsq1/FCGV["MH"]^2 + FCGV["EL"]/(2 FCGV["SW"] FCGV["MW"] FCGV["MH"]^2) dTH1 -
            dMWsq1/FCGV["MW"]^2 + dZG01 + dZGp1} },

  C[ S[3], S[3], -S[3], -S[3] ] == -I FCGV["EL"]^2 FCGV["MH"]^2/(2 FCGV["SW"]^2 FCGV["MW"]^2) *
    { {1, 2 dZe1 - 2 dSW1/FCGV["SW"] + dMHsq1/FCGV["MH"]^2 + FCGV["EL"]/(2 FCGV["SW"] FCGV["MW"] FCGV["MH"]^2) dTH1 -
            dMWsq1/FCGV["MW"]^2 + 2 dZGp1} },

	(* S-S-S:  G(+) . 1 *)

  C[ S[1], S[1], S[1] ] == -3 I FCGV["EL"] FCGV["MH"]^2/(2 FCGV["SW"] FCGV["MW"]) *
    { {1, dZe1 - dSW1/FCGV["SW"] + dMHsq1/FCGV["MH"]^2 + FCGV["EL"]/(2 FCGV["SW"] FCGV["MW"] FCGV["MH"]^2) dTH1 -
            dMWsq1/(2 FCGV["MW"]^2) + 3/2 dZH1} },
 
  C[ S[1], S[2], S[2] ] == -I FCGV["EL"] FCGV["MH"]^2/(2 FCGV["SW"] FCGV["MW"]) *
    { {1, dZe1 - dSW1/FCGV["SW"] + dMHsq1/FCGV["MH"]^2 + FCGV["EL"]/(2 FCGV["SW"] FCGV["MW"] FCGV["MH"]^2) dTH1 -
            dMWsq1/(2 FCGV["MW"]^2) + dZH1/2 + dZG01} },

  C[ S[3], S[1], -S[3] ] == -I FCGV["EL"] FCGV["MH"]^2/(2 FCGV["SW"] FCGV["MW"]) *
    { {1, dZe1 - dSW1/FCGV["SW"] + dMHsq1/FCGV["MH"]^2 + FCGV["EL"]/(2 FCGV["SW"] FCGV["MW"] FCGV["MH"]^2) dTH1 -
            dMWsq1/(2 FCGV["MW"]^2) + dZH1/2 + dZGp1} },

	(* S-S-V-V:  G(+) . g[mu3, mu4] *)

  C[ S[1], S[1], V[3], -V[3] ] == I FCGV["EL"]^2/(2 FCGV["SW"]^2) *
    { {1, 2 dZe1 - 2 dSW1/FCGV["SW"] + dZW1 + dZH1} },

  C[ S[2], S[2], V[3], -V[3] ] == I FCGV["EL"]^2/(2 FCGV["SW"]^2) *
    { {1, 2 dZe1 - 2 dSW1/FCGV["SW"] + dZW1 + dZG01} },

  C[ S[3], -S[3], V[3], -V[3] ] == I FCGV["EL"]^2/(2 FCGV["SW"]^2) *
    { {1, 2 dZe1 - 2 dSW1/FCGV["SW"] + dZW1 + dZGp1} },

  C[ S[3], -S[3], V[2], V[2] ] == I FCGV["EL"]^2 (FCGV["SW"]^2 - FCGV["CW"]^2)^2/(2 FCGV["CW"]^2 FCGV["SW"]^2) *
    { {1, 2 dZe1 + 2/(FCGV["SW"] FCGV["CW"]^2 (FCGV["SW"]^2 - FCGV["CW"]^2)) dSW1 + dZZZ1 + dZGp1 +
            2 FCGV["SW"] FCGV["CW"]/(FCGV["SW"]^2 - FCGV["CW"]^2) dZAZ1} },

  C[ S[3], -S[3], V[1], V[2] ] == I FCGV["EL"]^2 (FCGV["SW"]^2 - FCGV["CW"]^2)/(FCGV["CW"] FCGV["SW"]) *
    { {1, 2 dZe1 + dSW1/(FCGV["SW"] FCGV["CW"]^2 (FCGV["SW"]^2 - FCGV["CW"]^2)) + dZZZ1/2 + dZAA1/2 +
            dZGp1 + (FCGV["SW"]^2 - FCGV["CW"]^2)/(4 FCGV["SW"] FCGV["CW"]) dZZA1 +
            FCGV["SW"] FCGV["CW"]/(FCGV["SW"]^2 - FCGV["CW"]^2) dZAZ1} },

  C[ S[3], -S[3], V[1], V[1] ] == 2 I FCGV["EL"]^2 *
    { {1, 2 dZe1 + dZAA1 + dZGp1 + (FCGV["SW"]^2 - FCGV["CW"]^2)/(2 FCGV["SW"] FCGV["CW"]) dZZA1} },

  C[ S[1], S[1], V[2], V[2] ] == I FCGV["EL"]^2/(2 FCGV["CW"]^2 FCGV["SW"]^2) *
    { {1, 2 dZe1 + 2 (FCGV["SW"]^2 - FCGV["CW"]^2)/(FCGV["SW"] FCGV["CW"]^2) dSW1 + dZZZ1 + dZH1} },

  C[ S[2], S[2], V[2],  V[2] ] == I FCGV["EL"]^2/(2 FCGV["CW"]^2 FCGV["SW"]^2) *
    { {1, 2 dZe1 + 2 (FCGV["SW"]^2 - FCGV["CW"]^2)/(FCGV["SW"] FCGV["CW"]^2) dSW1 + dZZZ1 + dZG01} },

  C[ S[1], S[1], V[1], V[2] ] == I FCGV["EL"]^2/(2 FCGV["CW"]^2 FCGV["SW"]^2) *
    { {0, dZZA1/2} },

  C[ S[2], S[2], V[1], V[2] ] == I FCGV["EL"]^2/(2 FCGV["CW"]^2 FCGV["SW"]^2) *
    { {0, dZZA1/2} },

  C[ S[1], -S[3], V[3], V[2] ] == -I FCGV["EL"]^2/(2 FCGV["CW"]) *
    { {1, 2 dZe1 - dCW1/FCGV["CW"] + dZW1/2 + dZH1/2 + dZGp1/2 +
            dZZZ1/2 + FCGV["CW"]/FCGV["SW"] dZAZ1/2} },

  C[ S[1], S[3], -V[3], V[2] ] == -I FCGV["EL"]^2/(2 FCGV["CW"]) *
    { {1, 2 dZe1 - dCW1/FCGV["CW"] + dZW1/2 + dZH1/2 + dZGp1/2 +
            dZZZ1/2 + FCGV["CW"]/FCGV["SW"] dZAZ1/2} },

  C[ S[1], S[3], -V[3], V[1] ] == -I FCGV["EL"]^2/(2 FCGV["SW"]) *
    { {1, 2 dZe1 - dSW1/FCGV["SW"] + dZW1/2 + dZH1/2 + dZGp1/2 +
            dZAA1/2 + FCGV["SW"]/FCGV["CW"] dZZA1/2} },

  C[ S[1], -S[3], V[3], V[1] ] == -I FCGV["EL"]^2/(2 FCGV["SW"]) *
    { {1, 2 dZe1 - dSW1/FCGV["SW"] + dZW1/2 + dZH1/2 + dZGp1/2 +
            dZAA1/2 + FCGV["SW"]/FCGV["CW"] dZZA1/2} },

  C[ S[3], S[2], V[2], -V[3] ] == FCGV["EL"]^2/(2 FCGV["CW"]) *
    { {1, 2 dZe1 - dCW1/FCGV["CW"] + dZW1/2 + dZZZ1/2 + dZGp1/2 + dZG01/2 +
            FCGV["CW"]/FCGV["SW"] dZAZ1/2} },

  C[ -S[3], S[2], V[2], V[3] ] == -FCGV["EL"]^2/(2 FCGV["CW"]) *
    { {1, 2 dZe1 - dCW1/FCGV["CW"] + dZW1/2 + dZZZ1/2 + dZGp1/2 + dZG01/2 +
            FCGV["CW"]/FCGV["SW"] dZAZ1/2} },

  C[ S[3], S[2], V[1], -V[3] ] == FCGV["EL"]^2/(2 FCGV["SW"]) *
    { {1, 2 dZe1 - dSW1/FCGV["SW"] + dZW1/2 + dZAA1/2 + dZGp1/2 + dZG01/2 +
            FCGV["SW"]/FCGV["CW"] dZZA1/2} },

  C[ -S[3], S[2], V[1], V[3] ] == -FCGV["EL"]^2/(2 FCGV["SW"]) *
    { {1, 2 dZe1 - dSW1/FCGV["SW"] + dZW1/2 + dZAA1/2 + dZGp1/2 + dZG01/2 +
            FCGV["SW"]/FCGV["CW"] dZZA1/2} },

	(* S-S-V:  G(-) . (p1 - p2)_mu3 *)

  C[ S[2], S[1], V[1] ] == FCGV["EL"]/(2 FCGV["CW"] FCGV["SW"]) *
    { {0, dZZA1/2} },

  C[ S[2], S[1], V[2] ] == FCGV["EL"]/(2 FCGV["CW"] FCGV["SW"]) *
    { {1, dZe1 + (FCGV["SW"]^2 - FCGV["CW"]^2)/(FCGV["CW"]^2 FCGV["SW"]) dSW1 + dZH1/2 + dZZZ1/2 +
            dZG01/2} },

  C[ -S[3], S[3], V[1] ] == -I FCGV["EL"] *
    { {1, dZe1 + dZAA1/2 + dZGp1 + (FCGV["SW"]^2 - FCGV["CW"]^2)/(2 FCGV["SW"] FCGV["CW"]) dZZA1/2} },

  C[ -S[3], S[3], V[2] ] == -I FCGV["EL"] (FCGV["SW"]^2 - FCGV["CW"]^2)/(2 FCGV["CW"] FCGV["SW"]) *
    { {1, dZe1 + dSW1/((FCGV["SW"]^2 - FCGV["CW"]^2) FCGV["CW"]^2 FCGV["SW"]) + dZZZ1/2 + dZGp1 +
            2 FCGV["SW"] FCGV["CW"]/(FCGV["SW"]^2 - FCGV["CW"]^2) dZAZ1/2} },

  C[ S[3], S[1], -V[3] ] == -I FCGV["EL"]/(2 FCGV["SW"]) *
    { {1, dZe1 - dSW1/FCGV["SW"] + dZW1/2 + dZH1/2 + dZGp1/2} },

  C[ -S[3], S[1], V[3] ] == I FCGV["EL"]/(2 FCGV["SW"]) *
    { {1, dZe1 - dSW1/FCGV["SW"] + dZW1/2 + dZH1/2 + dZGp1/2} },

  C[ S[3], S[2], -V[3] ] == FCGV["EL"]/(2 FCGV["SW"]) *
    { {1, dZe1 - dSW1/FCGV["SW"] + dZW1/2 + dZGp1/2 + dZG01/2} },

  C[ -S[3], S[2], V[3] ] == FCGV["EL"]/(2 FCGV["SW"]) *
    { {1, dZe1 - dSW1/FCGV["SW"] + dZW1/2 + dZGp1/2 + dZG01/2} },

	(* S-V-V:  G(+) . g[mu2, mu3] *)

  C[ S[1], -V[3], V[3] ] == I FCGV["EL"] FCGV["MW"]/FCGV["SW"] *
    { {1, dZe1 - dSW1/FCGV["SW"] + dMWsq1/(2 FCGV["MW"]^2) + dZH1/2 + dZW1} },

  C[ S[1], V[2], V[2] ] == I FCGV["EL"] FCGV["MW"]/(FCGV["SW"] FCGV["CW"]^2) *
    { {1, dZe1 + (2 FCGV["SW"]^2 - FCGV["CW"]^2)/(FCGV["CW"]^2 FCGV["SW"]) dSW1 + dMWsq1/(2 FCGV["MW"]^2) +
            dZH1/2 + dZZZ1} },

  C[ S[1], V[2], V[1] ] == I FCGV["EL"] FCGV["MW"]/(FCGV["SW"] FCGV["CW"]^2) *
    { {0, dZZA1/2} },

  C[ -S[3], V[3], V[2] ] == -I FCGV["EL"] FCGV["MW"] FCGV["SW"]/FCGV["CW"] *
    { {1, dZe1 + dSW1/(FCGV["CW"]^2 FCGV["SW"]) + dMWsq1/(2 FCGV["MW"]^2) + dZW1/2 + dZZZ1/2 +
            dZGp1/2 + FCGV["CW"]/FCGV["SW"] dZAZ1/2} },

  C[ S[3], -V[3], V[2] ] == -I FCGV["EL"] FCGV["MW"] FCGV["SW"]/FCGV["CW"] *
    { {1, dZe1 + dSW1/(FCGV["CW"]^2 FCGV["SW"]) + dMWsq1/(2 FCGV["MW"]^2) + dZW1/2 + dZZZ1/2 +
            dZGp1/2 + FCGV["CW"]/FCGV["SW"] dZAZ1/2} },

  C[ -S[3], V[3], V[1] ] == -I FCGV["EL"] FCGV["MW"] *
    { {1, dZe1 + dMWsq1/(2 FCGV["MW"]^2) + dZW1/2 + dZAA1/2 + dZGp1/2 +
            FCGV["SW"]/FCGV["CW"] dZZA1/2} },

  C[ S[3], -V[3], V[1] ] == -I FCGV["EL"] FCGV["MW"] *
    { {1, dZe1 + dMWsq1/(2 FCGV["MW"]^2) + dZW1/2 + dZAA1/2 + dZGp1/2 +
            FCGV["SW"]/FCGV["CW"] dZZA1/2} },

	(* F-F-V:  G(-) . { gamma[mu3] omega[-], gamma[mu3] omega[+] } *)

  C[ -F[1, {j1}], F[1, {j2}], V[1] ] == I FCGV["EL"] *
    { {0, gL[1] IndexDelta[j1, j2] dZZA1/2},
      {0, 0} },

  C[ -F[2, {j1}], F[2, {j2}], V[1] ] == I FCGV["EL"] *
    { {-FermionCharge[2] IndexDelta[j1, j2],
        -FermionCharge[2] *
          (IndexDelta[j1, j2] (dZe1 + dZAA1/2) + AddHC[dZfL1[2, j1, j2]]) +
          gL[2] IndexDelta[j1, j2] dZZA1/2},
      {-FermionCharge[2] IndexDelta[j1, j2],
        -FermionCharge[2] *
          (IndexDelta[j1, j2] (dZe1 + dZAA1/2) + AddHC[dZfR1[2, j1, j2]]) +
          gR[2] IndexDelta[j1, j2] dZZA1/2} },

  C[ -F[3, {j1, o1}], F[3, {j2, o2}], V[1] ] == I FCGV["EL"] IndexDelta[o1, o2] *
    { {-FermionCharge[3] IndexDelta[j1, j2],
        -FermionCharge[3] *
          (IndexDelta[j1, j2] (dZe1 + dZAA1/2) + AddHC[dZfL1[3, j1, j2]]) +
          gL[3] IndexDelta[j1, j2] dZZA1/2},
      {-FermionCharge[3] IndexDelta[j1, j2],
        -FermionCharge[3] *
          (IndexDelta[j1, j2] (dZe1 + dZAA1/2) + AddHC[dZfR1[3, j1, j2]]) +
          gR[3] IndexDelta[j1, j2] dZZA1/2} },

  C[ -F[4, {j1, o1}], F[4, {j2, o2}], V[1] ] == I FCGV["EL"] IndexDelta[o1, o2] *
    { {-FermionCharge[4] IndexDelta[j1, j2],
        -FermionCharge[4] *
          (IndexDelta[j1, j2] (dZe1 + dZAA1/2) + AddHC[dZfL1[4, j1, j2]]) +
          gL[4] IndexDelta[j1, j2] dZZA1/2},
      {-FermionCharge[4] IndexDelta[j1, j2],
        -FermionCharge[4] *
          (IndexDelta[j1, j2] (dZe1 + dZAA1/2) + AddHC[dZfR1[4, j1, j2]]) +
          gR[4] IndexDelta[j1, j2] dZZA1/2} },

  C[ -F[1, {j1}], F[1, {j2}], V[2] ] == I FCGV["EL"] *
    { {gL[1] IndexDelta[j1, j2],
        IndexDelta[j1, j2] (gL[1] dZZZ1/2 + dgL[1]) +
        gL[1] AddHC[dZfL1[1, j1, j2]]},
      {0, 0} },

  C[ -F[2, {j1}], F[2, {j2}], V[2] ] == I FCGV["EL"] *
    { {gL[2] IndexDelta[j1, j2],
        IndexDelta[j1, j2] *
          (gL[2] dZZZ1/2 + dgL[2] - FermionCharge[2] dZAZ1/2) +
          gL[2] AddHC[dZfL1[2, j1, j2]]},
      {gR[2] IndexDelta[j1, j2],
        IndexDelta[j1, j2] *
          (gR[2] dZZZ1/2 + dgR[2] - FermionCharge[2] dZAZ1/2) +
          gR[2] AddHC[dZfR1[2, j1, j2]]} },

  C[ -F[3, {j1, o1}], F[3, {j2, o2}], V[2] ] == I FCGV["EL"] IndexDelta[o1, o2] *
    { {gL[3] IndexDelta[j1, j2],
        IndexDelta[j1, j2] *
          (gL[3] dZZZ1/2 + dgL[3] - FermionCharge[3] dZAZ1/2) +
          gL[3] AddHC[dZfL1[3, j1, j2]]},
      {gR[3] IndexDelta[j1, j2],
        IndexDelta[j1, j2] *
          (gR[3] dZZZ1/2 + dgR[3] - FermionCharge[3] dZAZ1/2) +
          gR[3] AddHC[dZfR1[3, j1, j2]]} },

  C[ -F[4, {j1, o1}], F[4, {j2, o2}], V[2] ] == I FCGV["EL"] IndexDelta[o1, o2] *
    { {gL[4] IndexDelta[j1, j2],
        IndexDelta[j1, j2] *
          (gL[4] dZZZ1/2 + dgL[4] - FermionCharge[4] dZAZ1/2) +
          gL[4] AddHC[dZfL1[4, j1, j2]]},
      {gR[4] IndexDelta[j1, j2],
        IndexDelta[j1, j2] *
          (gR[4] dZZZ1/2 + dgR[4] - FermionCharge[4] dZAZ1/2) +
          gR[4] AddHC[dZfR1[4, j1, j2]]} },

  C[ -F[1, {j1}], F[2, {j2}], -V[3] ] ==
    I FCGV["EL"]/(Sqrt[2] FCGV["SW"]) IndexDelta[j1, j2] *
    { {1, dZe1 - dSW1/FCGV["SW"] + dZW1/2 +
            Conjugate[dZfL1[1, j1, j1]]/2 + dZfL1[2, j1, j1]/2,
          dZe2 - dSW2/FCGV["SW"] +
            1/2 (dZW2 + Conjugate[dZfL2[1, j1, j1]] + dZfL2[2, j1, j1]) +
            (dSW1/FCGV["SW"])^2 - dSW1/FCGV["SW"] dZe1 -
            1/8 (dZW1^2 + Conjugate[dZfL1[1, j1, j1]]^2 +
              dZfL1[2, j1, j1]^2) +
            (dZe1 - dSW1/FCGV["SW"]) *
              1/2 (dZW1 + Conjugate[dZfL1[1, j1, j1]] + dZfL1[2, j1, j1]) +
            1/4 (dZW1 dZfL1[2, j1, j1] + dZW1 Conjugate[dZfL1[1, j1, j1]] +
                   Conjugate[dZfL1[1, j1, j1]] dZfL1[2, j1, j1]) },
      {0, 0, 0} },

  C[ -F[2, {j1}], F[1, {j2}], V[3] ] ==
    I FCGV["EL"]/(Sqrt[2] FCGV["SW"]) IndexDelta[j1, j2] *
    { {1, dZe1 - dSW1/FCGV["SW"] + dZW1/2 +
            dZfL1[1, j1, j1]/2 + Conjugate[dZfL1[2, j1, j1]]/2,
          dZe2 - dSW2/FCGV["SW"] +
            1/2 (dZW2 + dZfL2[1, j1, j1] + Conjugate[dZfL2[2, j1, j1]]) +
            (dSW1/FCGV["SW"])^2 - dSW1/FCGV["SW"] dZe1 -
            1/8 (dZW1^2 + dZfL1[1, j1, j1]^2 +
              Conjugate[dZfL1[2, j1, j1]]^2) +
            (dZe1 - dSW1/FCGV["SW"]) *
              1/2 (dZW1 + dZfL1[1, j1, j1] + Conjugate[dZfL1[2, j1, j1]]) +
	    1/4 (dZW1 Conjugate[dZfL1[2, j1, j1]] + dZW1 dZfL1[1, j1, j1] +
              dZfL1[1, j1, j1] Conjugate[dZfL1[2, j1, j1]]) },
      {0, 0, 0} },

  C[ -F[3, {j1, o1}], F[4, {j2, o2}], -V[3] ] ==
    I FCGV["EL"]/(Sqrt[2] FCGV["SW"]) IndexDelta[o1, o2] *
    { {CKM[j1, j2],
        CKM[j1, j2] (dZe1 - dSW1/FCGV["SW"] + dZW1/2) + dCKM1[j1, j2] +
        1/2 IndexSum[
          Conjugate[dZfL1[3, gn, j1]] CKM[gn, j2] +
          CKM[j1, gn] dZfL1[4, gn, j2],
        {gn, MaxGenerationIndex}]},
      {0, 0} },

  C[ -F[4, {j2, o2}], F[3, {j1, o1}], V[3] ] ==
    I FCGV["EL"]/(Sqrt[2] FCGV["SW"]) IndexDelta[o1, o2] *
    { {Conjugate[CKM[j1, j2]],
        Conjugate[CKM[j1, j2]] (dZe1 - dSW1/FCGV["SW"] + dZW1/2) +
          Conjugate[dCKM1[j1, j2]] +
          1/2 IndexSum[
            Conjugate[dZfL1[4, gn, j2]] Conjugate[CKM[j1, gn]] +
            Conjugate[CKM[gn, j2]] dZfL1[3, gn, j1],
          {gn, MaxGenerationIndex}]},
      {0, 0} },

	(* F-F-S:  G(+) . { omega[-], omega[+] } *)

  C[ -F[2, {j1}], F[2, {j2}], S[1] ] == -I FCGV["EL"]/(2 FCGV["SW"] FCGV["MW"]) *
    { {Mass[F[2, {j1}]] IndexDelta[j1, j2],
        Mass[F[2, {j1}]] IndexDelta[j1, j2] (dZe1 - dSW1/FCGV["SW"] +
          dMf1[2, j1]/Mass[F[2, {j1}]] - dMWsq1/(2 FCGV["MW"]^2) + dZH1/2) +
          mdZfLR1[2, j1, j2]},
      {Mass[F[2, {j1}]] IndexDelta[j1, j2],
        Mass[F[2, {j1}]] IndexDelta[j1, j2] (dZe1 - dSW1/FCGV["SW"] +
          dMf1[2, j1]/Mass[F[2, {j1}]] - dMWsq1/(2 FCGV["MW"]^2) + dZH1/2) +
          mdZfRL1[2, j1, j2]} },

  C[ -F[3, {j1, o1}], F[3, {j2, o2}], S[1] ] ==
    -I FCGV["EL"]/(2 FCGV["SW"] FCGV["MW"]) IndexDelta[o1, o2] *
    { {Mass[F[3, {j1}]] IndexDelta[j1, j2],
        Mass[F[3, {j1}]] IndexDelta[j1, j2] (dZe1 - dSW1/FCGV["SW"] +
          dMf1[3, j1]/Mass[F[3, {j1}]] - dMWsq1/(2 FCGV["MW"]^2) + dZH1/2) +
          mdZfLR1[3, j1, j2]},
      {Mass[F[3, {j1}]] IndexDelta[j1, j2],
        Mass[F[3, {j1}]] IndexDelta[j1, j2] (dZe1 - dSW1/FCGV["SW"] +
          dMf1[3, j1]/Mass[F[3, {j1}]] - dMWsq1/(2 FCGV["MW"]^2) + dZH1/2) +
          mdZfRL1[3, j1, j2]} },

  C[ -F[4, {j1, o1}], F[4, {j2, o2}], S[1] ] ==
    -I FCGV["EL"]/(2 FCGV["SW"] FCGV["MW"]) IndexDelta[o1, o2] *
    { {Mass[F[4, {j1}]] IndexDelta[j1, j2],
        Mass[F[4, {j1}]] IndexDelta[j1, j2] (dZe1 - dSW1/FCGV["SW"] +
          dMf1[4, j1]/Mass[F[4, {j1}]] - dMWsq1/(2 FCGV["MW"]^2) + dZH1/2) +
          mdZfLR1[4, j1, j2]},
      {Mass[F[4, {j1}]] IndexDelta[j1, j2],
        Mass[F[4, {j1}]] IndexDelta[j1, j2] (dZe1 - dSW1/FCGV["SW"] +
          dMf1[4, j1]/Mass[F[4, {j1}]] - dMWsq1/(2 FCGV["MW"]^2) + dZH1/2) +
          mdZfRL1[4, j1, j2]} },

  C[ -F[2, {j1}], F[2, {j2}], S[2] ] == -FCGV["EL"]/(2 FCGV["SW"] FCGV["MW"]) *
    { {Mass[F[2, {j1}]] IndexDelta[j1, j2],
        Mass[F[2, {j1}]] IndexDelta[j1, j2] (dZe1 - dSW1/FCGV["SW"] +
          dMf1[2, j1]/Mass[F[2, {j1}]] - dMWsq1/(2 FCGV["MW"]^2) + dZG01/2) +
          mdZfLR1[2, j1, j2]},
      {-Mass[F[2, {j1}]] IndexDelta[j1, j2],
        -Mass[F[2, {j1}]] IndexDelta[j1, j2] (dZe1 - dSW1/FCGV["SW"] +
          dMf1[2, j1]/Mass[F[2, {j1}]] - dMWsq1/(2 FCGV["MW"]^2) + dZG01/2) -
          mdZfRL1[2, j1, j2]} },

  C[ -F[3, {j1, o1}], F[3, {j2, o2}], S[2] ] ==
    FCGV["EL"]/(2 FCGV["SW"] FCGV["MW"]) IndexDelta[o1, o2] *
    { {Mass[F[3, {j1}]] IndexDelta[j1, j2],
        Mass[F[3, {j1}]] IndexDelta[j1, j2] (dZe1 - dSW1/FCGV["SW"] +
          dMf1[3, j1]/Mass[F[3, {j1}]] - dMWsq1/(2 FCGV["MW"]^2) + dZG01/2) +
          mdZfLR1[3, j1, j2]},
      {-Mass[F[3, {j1}]] IndexDelta[j1, j2],
        -Mass[F[3, {j1}]] IndexDelta[j1, j2] (dZe1 - dSW1/FCGV["SW"] +
          dMf1[3, j1]/Mass[F[3, {j1}]] - dMWsq1/(2 FCGV["MW"]^2) + dZG01/2) -
          mdZfRL1[3, j1, j2]} },

  C[ -F[4, {j1, o1}], F[4, {j2, o2}], S[2] ] ==
    -FCGV["EL"]/(2 FCGV["SW"] FCGV["MW"]) IndexDelta[o1, o2] *
    { {Mass[F[4, {j1}]] IndexDelta[j1, j2],
        Mass[F[4, {j1}]] IndexDelta[j1, j2] (dZe1 - dSW1/FCGV["SW"] +
          dMf1[4, j1]/Mass[F[4, {j1}]] - dMWsq1/(2 FCGV["MW"]^2) + dZG01/2) +
          mdZfLR1[4, j1, j2]},
      {-Mass[F[4, {j1}]] IndexDelta[j1, j2],
        -Mass[F[4, {j1}]] IndexDelta[j1, j2] (dZe1 - dSW1/FCGV["SW"] +
          dMf1[4, j1]/Mass[F[4, {j1}]] - dMWsq1/(2 FCGV["MW"]^2) + dZG01/2) -
          mdZfRL1[4, j1, j2]} },

  C[ -F[3, {j1, o1}], F[4, {j2, o2}], -S[3] ] ==
    I FCGV["EL"]/(Sqrt[2] FCGV["SW"] FCGV["MW"]) IndexDelta[o1, o2] *
    { {Mass[F[3, {j1}]] CKM[j1, j2],
        Mass[F[3, {j1}]] *
          (CKM[j1, j2] (dZe1 - dSW1/FCGV["SW"] + dMf1[3, j1]/Mass[F[3, {j1}]] -
            dMWsq1/(2 FCGV["MW"]^2) + dZGp1/2) + dCKM1[j1, j2]) +
          1/2 IndexSum[
            Mass[F[3, {gn}]] Conjugate[dZfR1[3, gn, j1]] CKM[gn, j2] +
            Mass[F[3, {j1}]] CKM[j1, gn] dZfL1[4, gn, j2],
          {gn, MaxGenerationIndex}]},
      {-Mass[F[4, {j2}]] CKM[j1, j2],
        -Mass[F[4, {j2}]] *
          (CKM[j1, j2] (dZe1 - dSW1/FCGV["SW"] + dMf1[4, j2]/Mass[F[4, {j2}]] -
            dMWsq1/(2 FCGV["MW"]^2) + dZGp1/2) + dCKM1[j1, j2]) -
          1/2 IndexSum[
            Mass[F[4, {j2}]] Conjugate[dZfL1[3, gn, j1]] CKM[gn, j2] +
            Mass[F[4, {gn}]] CKM[j1, gn] dZfR1[4, gn, j2],
          {gn, MaxGenerationIndex}]} },

  C[ -F[4, {j2, o2}], F[3, {j1, o1}], S[3] ] ==
    -I FCGV["EL"]/(Sqrt[2] FCGV["SW"] FCGV["MW"]) IndexDelta[o1, o2] *
    { {Mass[F[4, {j2}]] Conjugate[CKM[j1, j2]],
        Mass[F[4, {j2}]] (
            Conjugate[CKM[j1, j2]] (dZe1 - dSW1/FCGV["SW"] +
              dMf1[4, j2]/Mass[F[4, {j2}]] - dMWsq1/(2 FCGV["MW"]^2) + dZGp1/2) +
            Conjugate[dCKM1[j1, j2]] ) +
          1/2 IndexSum[
            Mass[F[4, {gn}]] Conjugate[dZfR1[4, gn, j2]] *
              Conjugate[CKM[j1, gn]] +
            Mass[F[4, {j2}]] Conjugate[CKM[gn, j2]] dZfL1[3, gn, j1],
          {gn, MaxGenerationIndex}]},
      {-Mass[F[3, {j1}]] Conjugate[CKM[j1, j2]],
        -Mass[F[3, {j1}]] (
            Conjugate[CKM[j1, j2]] (dZe1 - dSW1/FCGV["SW"] +
              dMf1[3, j2]/Mass[F[3, {j2}]] - dMWsq1/(2 FCGV["MW"]^2) + dZGp1/2) +
            Conjugate[dCKM1[j1, j2]] ) -
          1/2 IndexSum[
            Mass[F[3, {j1}]] Conjugate[dZfL1[4, gn, j2]] *
              Conjugate[CKM[j1, gn]] +
            Mass[F[3, {gn}]] Conjugate[CKM[gn, j2]] *
              dZfR1[3, gn, j1],
          {gn, MaxGenerationIndex}]} },

  C[ -F[1, {j1}], F[2, {j2}], -S[3] ] ==
    -I FCGV["EL"] Mass[F[2, {j1}]]/(Sqrt[2] FCGV["SW"] FCGV["MW"]) IndexDelta[j1, j2] *
    { {0, 0},
      {1, dZe1 - dSW1/FCGV["SW"] + dMf1[2, j1]/Mass[F[2, {j1}]] -
            dMWsq1/(2 FCGV["MW"]^2) + dZGp1/2 +
            Conjugate[dZfL1[1, j1, j1]]/2 + dZfR1[2, j1, j1]/2} },

  C[ -F[2, {j1}], F[1, {j2}], S[3] ] ==
    -I FCGV["EL"] Mass[F[2, {j1}]]/(Sqrt[2] FCGV["SW"] FCGV["MW"]) IndexDelta[j1, j2] *
    { {1, dZe1 - dSW1/FCGV["SW"] + dMf1[2, j1]/Mass[F[2, {j1}]] -
            dMWsq1/(2 FCGV["MW"]^2) + dZGp1/2 +
            dZfL1[1, j1, j1]/2 + Conjugate[dZfR1[2, j1, j1]]/2},
      {0, 0} },

	(* U-U-V:  G(+) . { p1_mu3, p2_mu3 } *)

  C[ -U[3], U[3], V[1] ] == -I FCGV["EL"] *
    { {1, dZe1 + dZAA1/2 - dZW1/2 + dUW1 - FCGV["CW"]/FCGV["SW"] dZZA1/2},
      {0, 0} },

  C[ -U[4], U[4], V[1] ] == I FCGV["EL"] *
    { {1, dZe1 + dZAA1/2 - dZW1/2 + dUW1 - FCGV["CW"]/FCGV["SW"] dZZA1/2},
      {0, 0} },

  C[ -U[3], U[3], V[2] ] == I FCGV["EL"] FCGV["CW"]/FCGV["SW"] *
    { {1, dZe1 - 1/(FCGV["CW"]^2 FCGV["SW"]) dSW1 + dZZZ1/2 - dZW1/2 + dUW1 - FCGV["SW"]/FCGV["CW"] dZAZ1/2},
      {0, 0} },

  C[ -U[4], U[4], V[2] ] == -I FCGV["EL"] FCGV["CW"]/FCGV["SW"] *
    { {1, dZe1 - 1/(FCGV["CW"]^2 FCGV["SW"]) dSW1 + dZZZ1/2 - dZW1/2 + dUW1 - FCGV["SW"]/FCGV["CW"] dZAZ1/2},
      {0, 0} },

  C[ -U[3], U[2], V[3] ] == -I FCGV["EL"] FCGV["CW"]/FCGV["SW"] *
    { {1, dZe1 - 1/(FCGV["CW"]^2 FCGV["SW"]) dSW1 + dUZZ1 - FCGV["SW"]/FCGV["CW"] dUAZ1},
      {0, 0} },

  C[ -U[2], U[3], -V[3] ] == -I FCGV["EL"] *
    { {FCGV["CW"]/FCGV["SW"],
       FCGV["CW"]/FCGV["SW"] (dZe1 - 1/(FCGV["CW"]^2 FCGV["SW"]) dSW1 + dZW1/2 - dZZZ1/2 + dUW1) + dZZA1/2},
      {0, 0} }, 

  C[ -U[4], U[2], -V[3] ] == I FCGV["EL"] FCGV["CW"]/FCGV["SW"] *
    { {1, dZe1 - 1/(FCGV["CW"]^2 FCGV["SW"]) dSW1 + dUZZ1 - FCGV["SW"]/FCGV["CW"] dUAZ1},
      {0, 0} },

  C[ -U[2], U[4], V[3] ] == I FCGV["EL"] *
    { {FCGV["CW"]/FCGV["SW"],
       FCGV["CW"]/FCGV["SW"] (dZe1 - 1/(FCGV["CW"]^2 FCGV["SW"]) dSW1 + dZW1/2 - dZZZ1/2 + dUW1) + dZZA1/2},
      {0, 0} },

  C[ -U[3], U[1], V[3] ] == I FCGV["EL"] *
    { {1, dZe1 + dUAA1 - FCGV["CW"]/FCGV["SW"] dUZA1},
      {0, 0} },

  C[ -U[1], U[3], -V[3] ] == I FCGV["EL"] *
    { {1, dZe1 + dZW1/2 - dZAA1/2 + dUW1 + FCGV["CW"]/FCGV["SW"] dZAZ1/2},
      {0, 0} },

  C[ -U[4], U[1], -V[3] ] == -I FCGV["EL"] *
    { {1, dZe1 + dUAA1 - FCGV["CW"]/FCGV["SW"] dUZA1},
      {0, 0} },

  C[ -U[1], U[4], V[3] ] == -I FCGV["EL"] *
    { {1, dZe1 + dZW1/2 - dZAA1/2 + dUW1 + FCGV["CW"]/FCGV["SW"] dZAZ1/2},
      {0, 0} },

	(* S-U-U:  G(+) . 1 *)

  C[ S[1], -U[2], U[1] ] == -I FCGV["EL"] FCGV["MZ"] FAGaugeXi[Z]/(2 FCGV["SW"] FCGV["CW"]) *
    { {0, dUZA1} },

  C[ S[1], -U[2], U[2] ] == -I FCGV["EL"] FCGV["MZ"] FAGaugeXi[Z]/(2 FCGV["SW"] FCGV["CW"]) *
    { {1, dZe1 + (FCGV["SW"]^2 - FCGV["CW"]^2)/(FCGV["CW"]^2 FCGV["SW"]) dSW1 + dZH1/2 - dZG01/2 + dUZZ1} },

  C[ S[1], -U[3], U[3] ] == -I FCGV["EL"] FCGV["MW"] FAGaugeXi[W]/(2 FCGV["SW"]) *
    { {1, dZe1 - dSW1/FCGV["SW"] + dZH1/2 - dZGp1/2 + dUW1} },

  C[ S[1], -U[4], U[4] ] == -I FCGV["EL"] FCGV["MW"] FAGaugeXi[W]/(2 FCGV["SW"]) *
    { {1, dZe1 - dSW1/FCGV["SW"] + dZH1/2 - dZGp1/2 + dUW1} },

  C[ S[2], -U[4], U[4] ] == FCGV["EL"] FCGV["MW"] FAGaugeXi[W]/(2 FCGV["SW"]) *
    { {1, dZe1 - dSW1/FCGV["SW"] + dZG01/2 - dZGp1/2 + dUW1} },

  C[ S[2], -U[3], U[3] ] == -FCGV["EL"] FCGV["MW"] FAGaugeXi[W]/(2 FCGV["SW"]) *
    { {1, dZe1 - dSW1/FCGV["SW"] + dZG01/2 - dZGp1/2 + dUW1} },

  C[ -S[3], -U[2], U[3] ] == I FCGV["EL"] FCGV["MZ"] FAGaugeXi[Z]/(2 FCGV["SW"]) *
    { {1, dZe1 - dSW1/FCGV["SW"] + dZGp1/2 - dZG01/2 + dUW1} },

  C[ S[3], -U[2], U[4] ] == I FCGV["EL"] FCGV["MZ"] FAGaugeXi[Z]/(2 FCGV["SW"]) *
    { {1, dZe1 - dSW1/FCGV["SW"] + dZGp1/2 - dZG01/2 + dUW1} },

  C[ -S[3], -U[4], U[2] ] == I FCGV["EL"] (FCGV["SW"]^2 - FCGV["CW"]^2) FCGV["MW"] FAGaugeXi[W]/(2 FCGV["CW"] FCGV["SW"]) *
    { {1, dZe1 + dSW1/((FCGV["SW"]^2 - FCGV["CW"]^2) FCGV["CW"]^2 FCGV["SW"]) + dUZZ1 +
        (2 FCGV["SW"] FCGV["CW"])/(FCGV["SW"]^2 - FCGV["CW"]^2) dUAZ1} },

  C[ S[3], -U[3], U[2] ] == I FCGV["EL"] (FCGV["SW"]^2 - FCGV["CW"]^2) FCGV["MW"] FAGaugeXi[W]/(2 FCGV["CW"] FCGV["SW"]) *
    { {1, dZe1 + dSW1/((FCGV["SW"]^2 - FCGV["CW"]^2) FCGV["CW"]^2 FCGV["SW"]) + dUZZ1 +
        (2 FCGV["SW"] FCGV["CW"])/(FCGV["SW"]^2 - FCGV["CW"]^2) dUAZ1} },

  C[ -S[3], -U[4], U[1] ] == I FCGV["EL"] FCGV["MW"] FAGaugeXi[W] *
    { {1, dZe1 + dUAA1 + (FCGV["SW"]^2 - FCGV["CW"]^2)/(2 FCGV["SW"] FCGV["CW"]) dUZA1} },

  C[ S[3], -U[3], U[1] ] == I FCGV["EL"] FCGV["MW"] FAGaugeXi[W] *
    { {1, dZe1 + dUAA1 + (FCGV["SW"]^2 - FCGV["CW"]^2)/(2 FCGV["SW"] FCGV["CW"]) dUZA1} }
}


M$LastModelRules = {}


(* \:4e00\:4e9b\:7528\:4e8e\:6392\:9664 \:7c92\:5b50\:7c7b \:7684\:7b80\:6377\:65b9\:6cd5 *)

QEDOnly = ExcludeParticles -> {F[1], V[2], V[3], S, SV, U[2], U[3], U[4]}

NoGeneration1 = ExcludeParticles -> F[_, {1, ___}]

NoGeneration2 = ExcludeParticles -> F[_, {2, ___}]

NoGeneration3 = ExcludeParticles -> F[_, {3, ___}]

NoElectronHCoupling =
  ExcludeFieldPoints -> {
    FieldPoint[_][-F[2, {1}], F[2, {1}], S],
    FieldPoint[_][-F[2, {1}], F[1, {1}], S] }

NoLightFHCoupling =
  ExcludeFieldPoints -> {
    FieldPoint[_][-F[2], F[2], S],
    FieldPoint[_][-F[2], F[1], S],
    FieldPoint[_][-F[3, {1, ___}], F[3, {1, ___}], S],
    FieldPoint[_][-F[3, {2, ___}], F[3, {2, ___}], S],
    FieldPoint[_][-F[4], F[4], S],
    FieldPoint[_][-F[4], F[3, {1, ___}], S],
    FieldPoint[_][-F[4], F[3, {2, ___}], S] }

NoQuarkMixing =
  ExcludeFieldPoints -> {
    FieldPoint[_][-F[4, {1, ___}], F[3, {2, ___}], S[3]],
    FieldPoint[_][-F[4, {1, ___}], F[3, {2, ___}], V[3]],
    FieldPoint[_][-F[4, {1, ___}], F[3, {3, ___}], S[3]],
    FieldPoint[_][-F[4, {1, ___}], F[3, {3, ___}], V[3]],
    FieldPoint[_][-F[4, {2, ___}], F[3, {1, ___}], S[3]],
    FieldPoint[_][-F[4, {2, ___}], F[3, {1, ___}], V[3]],
    FieldPoint[_][-F[4, {2, ___}], F[3, {3, ___}], S[3]],
    FieldPoint[_][-F[4, {2, ___}], F[3, {3, ___}], V[3]],
    FieldPoint[_][-F[4, {3, ___}], F[3, {1, ___}], S[3]],
    FieldPoint[_][-F[4, {3, ___}], F[3, {1, ___}], V[3]],
    FieldPoint[_][-F[4, {3, ___}], F[3, {2, ___}], S[3]],
    FieldPoint[_][-F[4, {3, ___}], F[3, {2, ___}], V[3]] }


(* The following definitions of renormalization constants
   are for the on-shell renormalization of the Standard Model in
   the scheme of A. Denner, Fortschr. d. Physik, 41 (1993) 4.

   The renormalization constants are not directly used by
   FeynArts, and hence do not restrict the generation of diagrams
   and amplitudes in any way. *)

Clear[RenConst]

RenConst[ dMf1[type_, j1_] ] := MassRC[F[type, {j1}]]

RenConst[ dZfL1[type_, j1_, j2_] ] :=
  FieldRC[F[type, {j1}], F[type, {j2}]][[1]]

RenConst[ dZfR1[type_, j1_, j2_] ] :=
  FieldRC[F[type, {j1}], F[type, {j2}]][[2]]

If[ dCKM1[] =!= 0,
RenConst[dCKM1[j1_, j2_]] := 1/4 Sum[
  (dZfL1[3, j1, gn] - Conjugate[dZfL1[3, gn, j1]]) CKM[gn, j2] -
  CKM[j1, gn] (dZfL1[4, gn, j2] - Conjugate[dZfL1[4, j2, gn]]), {gn, 3} ]
]

RenConst[ dMZsq1 ] := MassRC[V[2]]

RenConst[ dMWsq1 ] := MassRC[V[3]]

RenConst[ dMHsq1 ] := MassRC[S[1]]

RenConst[ dZAA1 ] := FieldRC[V[1]]

RenConst[ dZAZ1 ] := FieldRC[V[1], V[2]]

RenConst[ dZZA1 ] := FieldRC[V[2], V[1]]

RenConst[ dZZZ1 ] := FieldRC[V[2]]

RenConst[ dZG01 ] := FieldRC[S[2]]

RenConst[ dZW1 ] := FieldRC[V[3]]

RenConst[ dZGp1 ] := FieldRC[S[3]]

RenConst[ dZH1 ] := FieldRC[S[1]]

RenConst[ dTH1 ] := TadpoleRC[S[1]]

RenConst[ dSW1 ] := FCGV["CW"]^2/FCGV["SW"]/2 (dMZsq1/FCGV["MZ"]^2 - dMWsq1/FCGV["MW"]^2)

RenConst[ dZe1 ] := -1/2 (dZAA1 + FCGV["SW"]/FCGV["CW"] dZZA1)

RenConst[ dUW1 ] := FieldRC[U[3]] + dZW1/2

RenConst[ dUAA1 ] := FieldRC[U[1]] + dZAA1/2

RenConst[ dUAZ1 ] := FieldRC[U[1], U[2]]/2 + dZAZ1/2

RenConst[ dUZA1 ] := FieldRC[U[2], U[1]]/2

RenConst[ dUZZ1 ] := FieldRC[U[2]] + dZZZ1/2
