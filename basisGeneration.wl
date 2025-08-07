(* ::Package:: *)

(* Basis Generation Functions *)
BeginPackage["BasisGeneration`"];

bVecs::usage = "compute reciprocal lattice vectors given lattice vectors."
kpVec::usage = "compute k(p) vector."
kp::usage = "compute 2-norm ||k(p)||_2 manually." 
kpFast::usage = "compute 2-norm ||k(p)||_2."
kp1norm::usage = "compute 1-norm ||k(p)||_1."
cellVol::usage = "cellVol[aVecs_] computes simulation volume given real-space lattice vectors (in AU)"
basisGeneration::usage = "basisGeneration[pmaxVec_,dimFrac_,removeZero_] generates the full discrete basis if dimFrac=1
and a partial basis from dimFrac>1."
constructBasis::usage = "constructBasis[aVecs_,cutoff_,basisToggle_,fullBasis_] constructs full plane-wave basis,
- aVecs: list of real-space lattice vectors,
- cutoff: energy cutoff in Hartree.,
- If fullBasis = True, compute full basis, else compute information about simulation cell without all basis elements.,
- If basisToggle = True, compute the integer number of qubits with the given cutoff, 
then recompute the true basis size, integer pmax, and true cutoff. If false, we keep float values for pmax."
viewBasisInfo::usage = "viewBasisInfo[assoc_] provides a concise display of the basis info."

Begin["`Private`"];

(* Exact computation requires full basis generation and sums over the basis *)
(* Define simulation cell *)
cellVol[aVecs_]:=Abs[Dot[aVecs[[1]],Cross[aVecs[[2]],aVecs[[3]]]]];
b1Vec[aVecs_]:=(2\[Pi])/cellVol[aVecs] Cross[aVecs[[2]],aVecs[[3]]];
b2Vec[aVecs_]:=(2\[Pi])/cellVol[aVecs] Cross[aVecs[[3]],aVecs[[1]]];
b3Vec[aVecs_]:=(2\[Pi])/cellVol[aVecs] Cross[aVecs[[1]],aVecs[[2]]];
bVecs[aVecs_]:={b1Vec[aVecs],b2Vec[aVecs],b3Vec[aVecs]};

(* Generate max p values - isotropic per-component momentum cutoff *)
bVecNorms[bVecs_]:=Map[Norm,bVecs];
pmaxVals[bVecs_, cutoff_]:=cutoff/bVecNorms[bVecs];
numQbits[pmax_]:=Log2[2pmax+2]; (* Kept as float for now *)
basisSizeVec[nqubits_]:=2^nqubits-1;

(* Compute kp vector and norms *)
kpVec[pVec_,bVecs_]:=Sum[pVec[[i]]bVecs[[i]],{i,1,3}] (* Cartesian components *)
kp[pVec_,bVecs_]:=Sqrt[Dot[kpVec[pVec,bVecs],kpVec[pVec,bVecs]]] (* Basis independent, 2-norm *)
kpFast[pVec_,bVecs_]:=Norm[kpVec[pVec,bVecs],2];
kp1norm[pVec_,bVecs_]:=Norm[kpVec[pVec,bVecs],1] (* Cartesian 1-norm *)

(* Compute max |momentum| in basis - 2 versions *)
maxAbsMom[basisAssoc_]:=Module[{allSigns,allpmaxVecs},
allSigns=Tuples[{-1,1},3];
allpmaxVecs=Table[basisAssoc["pmax"]*allSigns[[i]],{i,1,Length@allSigns}];
Return[Max[Map[kpFast[#,basisAssoc["bVecs"]] &,allpmaxVecs]]];
]

maxAbsMomExplicit[pmaxVec_,bVecs_]:=Module[{allSigns,allpmaxVecs},
allSigns=Tuples[{-1,1},3];
allpmaxVecs=Table[pmaxVec*allSigns[[i]],{i,1,Length@allSigns}];
Return[Max[Map[kpFast[#,bVecs] &,allpmaxVecs]]];
]

(* Compute kVals over basis - 2 versions. First version is if all p vectors are stored in "basis".
 Second version is explicit computation of k when provided the basis directly. *)
computeKValsOnBasis[basisAssoc_]:=Module[{func,basis},
func[pVec_]:=kpFast[pVec,basisAssoc["bVecs"]];
basis=basisAssoc["basis"];
Return[ParallelMap[func,basis]];
]

computeKValsOnBasisExplicit[bVecs_,basis_]:=Module[{func},
func[pVec_]:=kpFast[pVec,bVecs];
Return[ParallelMap[func,basis]];
]

(* Generates the full discrete basis if dimFrac=1 and a partial basis from dimFrac>1. *)
basisGeneration[pmaxVec_,dimFrac_,removeZero_]:=
Module[{pmaxVecFrac,basis},
pmaxVecFrac=Ceiling[pmaxVec/dimFrac];

(* Original version *)
basis=Tuples[{Table[p1,{p1,-pmaxVecFrac[[1]],pmaxVecFrac[[1]]}],Table[p2,{p2,-pmaxVecFrac[[2]],pmaxVecFrac[[2]]}],
Table[p3,{p3,-pmaxVecFrac[[3]],pmaxVecFrac[[3]]}]}];
If[removeZero,basis=DeleteCases[basis,{0,0,0}]];
Return[basis]
]

(* Construct Full Basis,
- aVecs: list of real-space lattice vectors,
- cutoff: NEW = energy cutoff in Hartree. OLD = per-direction preferred minimum cutoff,
- If basisToggle = True, compute the integer number of qubits with the given cutoff (generally leave this True), 
- If fullBasis = True, compute full basis, else compute information about simulation cell without all basis elements.,
then recompute the true basis size, integer pmax, and true cutoff. If false, we keep float values for pmax.
*)
constructBasis[aVecs_,cutoff_,basisToggle_,fullBasis_]:=Module[{momentumComponentCutoff,bVecList,pmaxVec,nqubits,
basisSizeVector,trueMomentumComponentCutoff,basis,Kmax,Qmax,maxCheck,
momentumComponentCutoff\[CapitalLambda]bar,momentumComponentCutoff\[CapitalLambda]bartrunc,pmaxVec\[CapitalLambda]bar,nqubits\[CapitalLambda]bar,basisSizeVector\[CapitalLambda]bar,
trueMomentumComponentCutoff\[CapitalLambda]bar,pmaxVec\[CapitalLambda]bartrunc,basistrunc,Kbarmax,
Qbarmax,Kbartruncmax,Qbartruncmax,trueMomentumComponentCutoff\[CapitalLambda]bartrunc},

(* cutoff is a list of 3 elements in Ha. First is for G,G0. Second is for Gbar. Third is for G0bartrunc. *)
momentumComponentCutoff=Sqrt[2cutoff[[1]]]; (* \[CapitalLambda] for G,G0 *)
momentumComponentCutoff\[CapitalLambda]bar=Sqrt[2cutoff[[2]]]; (* \[CapitalLambda]bar for Gbar *)
momentumComponentCutoff\[CapitalLambda]bartrunc=Sqrt[2cutoff[[3]]];  (* \[CapitalLambda]bartrunc for G0bartrunc *)

(* For \[CapitalLambda] giving G,G0 *)
(* Computed desired pmax and corresponding floating point number of qubits *) 
bVecList=bVecs[aVecs] //N;
pmaxVec=pmaxVals[bVecList,momentumComponentCutoff] //N;
nqubits=numQbits[pmaxVec];
basisSizeVector=basisSizeVec[nqubits];
(* Recompute basis size with integer qubits and the associated true cutoffs *)
If [basisToggle,
Module[{x},
nqubits=Ceiling[nqubits];
basisSizeVector=basisSizeVec[nqubits];
pmaxVec=2^(nqubits-1)-1;
trueMomentumComponentCutoff=pmaxVec Table[Norm[bVecList[[\[Alpha]]]],{\[Alpha],1,3}]; 
],
trueMomentumComponentCutoff={momentumComponentCutoff,momentumComponentCutoff,momentumComponentCutoff};
];

(* For \[CapitalLambda]bar giving Gbar *)
pmaxVec\[CapitalLambda]bar=pmaxVals[bVecList,momentumComponentCutoff\[CapitalLambda]bar] //N;
nqubits\[CapitalLambda]bar=numQbits[pmaxVec\[CapitalLambda]bar];
basisSizeVector\[CapitalLambda]bar=basisSizeVec[nqubits\[CapitalLambda]bar];
If [basisToggle,
Module[{x},
nqubits\[CapitalLambda]bar=Ceiling[nqubits\[CapitalLambda]bar];
basisSizeVector\[CapitalLambda]bar=basisSizeVec[nqubits\[CapitalLambda]bar];
pmaxVec\[CapitalLambda]bar=2^(nqubits\[CapitalLambda]bar-1)-1;
trueMomentumComponentCutoff\[CapitalLambda]bar=pmaxVec\[CapitalLambda]bar Table[Norm[bVecList[[\[Alpha]]]],{\[Alpha],1,3}];
],
trueMomentumComponentCutoff\[CapitalLambda]bar={momentumComponentCutoff\[CapitalLambda]bar,momentumComponentCutoff\[CapitalLambda]bar,momentumComponentCutoff\[CapitalLambda]bar};
];

(* For \[CapitalLambda]bartrunc giving G0bartrunc. Note \[CapitalLambda]bartrunc<=\[CapitalLambda]bar required. *)
pmaxVec\[CapitalLambda]bartrunc=pmaxVals[bVecList,momentumComponentCutoff\[CapitalLambda]bartrunc] //N;
pmaxVec\[CapitalLambda]bartrunc=Ceiling[pmaxVec\[CapitalLambda]bartrunc];
trueMomentumComponentCutoff\[CapitalLambda]bartrunc=pmaxVec\[CapitalLambda]bartrunc Table[Norm[bVecList[[\[Alpha]]]],{\[Alpha],1,3}];

(* Original method that keeps the actual p values for the full basis. 
This isn't needed since all computations involve k (or q) only. 
Hence, we just compute and store those values tossing out the storage of the actual basis itself. *)
(*If[fullBasis,basis=basisGeneration[pmaxVec,1,removeZero],basis={}];*)

If[fullBasis,
Module[{Gbasis,G0basis,kVals,qVals,Gbartruncbasis,G0bartruncbasis,kValstrunc,qValstrunc},
Gbasis=basisGeneration[pmaxVec,1,True]; (* should technically keep (0,0,0) in G but thrown out for numerical ease *) 
kVals=computeKValsOnBasisExplicit[bVecList,Gbasis];
G0basis=basisGeneration[2pmaxVec,1,True]; (* Has twice the range in each dimension and removes (0,0,0) element *)
qVals=computeKValsOnBasisExplicit[bVecList,G0basis];
basis={kVals,qVals};

(* For \[CapitalLambda]bartrunc *)
Gbartruncbasis=basisGeneration[pmaxVec\[CapitalLambda]bartrunc,1,True]; 
kValstrunc=computeKValsOnBasisExplicit[bVecList,Gbartruncbasis];
G0bartruncbasis=basisGeneration[2pmaxVec\[CapitalLambda]bartrunc,1,True]; 
qValstrunc=computeKValsOnBasisExplicit[bVecList,G0bartruncbasis];
basistrunc={kValstrunc,qValstrunc};

],Module[{x},basis={}; basistrunc={};]];

(* Compute max k and q values without directly looking at basis *)
Kmax=maxAbsMomExplicit[pmaxVec,bVecList];
Qmax=maxAbsMomExplicit[2pmaxVec,bVecList];

(* For \[CapitalLambda]bar *)
Kbarmax=maxAbsMomExplicit[pmaxVec\[CapitalLambda]bar,bVecList];
Qbarmax=maxAbsMomExplicit[2pmaxVec\[CapitalLambda]bar,bVecList];

(* For \[CapitalLambda]bartrunc *)
Kbartruncmax=maxAbsMomExplicit[pmaxVec\[CapitalLambda]bartrunc,bVecList];
Qbartruncmax=maxAbsMomExplicit[2pmaxVec\[CapitalLambda]bartrunc,bVecList];

(* If full basis exists, then check that the max function works correctly *) 
maxCheck=If[fullBasis,{Max[basis[[1]]]==Kmax,Max[basis[[2]]]==Qmax},{}];

Return[<|
"nqubits"->nqubits,
"nqubits\[CapitalLambda]bar"->nqubits\[CapitalLambda]bar,
"pmax"->pmaxVec,
"pmax\[CapitalLambda]bar"->pmaxVec\[CapitalLambda]bar,
"pmax\[CapitalLambda]bartrunc"->pmaxVec\[CapitalLambda]bartrunc,
"cutoff"->{trueMomentumComponentCutoff,Norm[trueMomentumComponentCutoff],1/2 Norm[trueMomentumComponentCutoff]^2,1/2 Min[trueMomentumComponentCutoff]^2},
"cutoff\[CapitalLambda]bar"->{trueMomentumComponentCutoff\[CapitalLambda]bar,Norm[trueMomentumComponentCutoff\[CapitalLambda]bar],1/2 Norm[trueMomentumComponentCutoff\[CapitalLambda]bar]^2,1/2 Min[trueMomentumComponentCutoff\[CapitalLambda]bar]^2},
"cutoff\[CapitalLambda]bartrunc"->{trueMomentumComponentCutoff\[CapitalLambda]bartrunc,Norm[trueMomentumComponentCutoff\[CapitalLambda]bartrunc],1/2 Norm[trueMomentumComponentCutoff\[CapitalLambda]bartrunc]^2,1/2 Min[trueMomentumComponentCutoff\[CapitalLambda]bartrunc]^2},
"Kmax"->Kmax,
"Qmax"->Qmax,
"Kbarmax"->Kbarmax,
"Qbarmax"->Qbarmax,
"Kbartruncmax"->Kbartruncmax,
"Qbartruncmax"->Qbartruncmax,
"maxCheck"->maxCheck,
"aVecs"->aVecs,
"bVecs"->bVecList,
"basisSize"->Times@@basisSizeVector,
"basis"->basis,
"basistruncSize"->If[fullBasis,Length@basistrunc[[1]],{}],
"basistrunc"->basistrunc|>]
]

viewBasisInfo[assoc_]:=Print[KeyDrop[assoc,{"basis","basistrunc"}]];

End[];
EndPackage[];
