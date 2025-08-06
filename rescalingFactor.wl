(* ::Package:: *)

(* Rescaling factor computations *)
BeginPackage["RescalingFactor`"];

all\[Lambda]Bounds::usage = "all\[Lambda]Bounds[instance_] computes bounds on rescaling factors given instance."
all\[Lambda]Exact::usage = "all\[Lambda]Exact[instance_,basisAssoc_] computes exact rescaling factors given instance."

coulombSumNormalized::usage = "for internal function testing only."
locSumNormalized::usage = "for internal function testing only."
NLSumNormalized::usage = "for internal function testing only."

Needs["Preprocessing`"];
Needs["BasisGeneration`"];

Begin["`Private`"];




(* ------------------------------------------------------ *)
(* Per-particle/pair Rescaling functions *)
(* ------------------------------------------------------ *)

(* Bounds are simple and only require max momentum K, max momentum exchange Q, 
and the computed c coeffs for local term, and D, Ctilde coeffs for non-local term. Fast estimation *)
\[Lambda]CoulombPerPairBound[Q_]:=Q/\[Pi];
\[Lambda]LocPerPairBound[clist_]:=Dot[Abs[clist],{1,1,3,15,105}];
\[Lambda]NLPerPairBound[Dlist_,Ctildelist_]:=Sum[(2l+1)Abs[Dlist[[l+1,\[Alpha]]]]Ctildelist[[l+1,\[Alpha]]],{l,0,2},{\[Alpha],1,3}]

(* Key sums that have been approximately bounded by integrals - utilities for exact computation *)
coulombSumNormalized[qVals_,aVecs_]:=Total[1/cellVol[aVecs] ParallelMap[#^-2&,qVals]];
locSumNormalized[qVals_,aVecs_,element_,s_]:=Module[{qValsNonDim,rloc},
rloc=Preprocessing`rlocAssoc[element];
qValsNonDim=qVals rloc;
Return[Total[rloc^3/BasisGeneration`cellVol[aVecs] ParallelMap[Exp[-(1/2) #^2]#^(2s)&,qValsNonDim] ]]
];

NLSumNormalized[kVals_,aVecs_,element_,l_,\[Alpha]_]:=Module[{kValsNonDim,GfuncSqCompiled,coeffs,rl,
sumTotal,sumCompiled,chunkSumCompiled,chunks},
rl=Preprocessing`rlAssoc[element][[l+1]];
kValsNonDim=kVals rl;
coeffs=Preprocessing`XFullAssoc[element][[l+1]][[\[Alpha],2]] ;(* ion, l, \[Alpha], eigenvalue = 1, eigenvector = 2*)

(* Faster version - single core *)
GfuncSqCompiled=Compile[{{k,_Real,1}},Gfunc[l,coeffs,k]^2]; (* compiled, vectorized since just linear sum of poly*exp *)
sumTotal=Total[GfuncSqCompiled[kValsNonDim]];
Return[rl^3/BasisGeneration`cellVol[aVecs] sumTotal];
];

(* Exact pair-wise rescaling factors *)
\[Lambda]KineticPerParticle[Kmax_]:=1/2 Kmax^2;
\[Lambda]CoulombExactPerPair[qVals_,aVecs_]:=2\[Pi] coulombSumNormalized[qVals,aVecs];
\[Lambda]LocExactPerPair[qVals_,aVecs_,element_,ccoeffs_]:=Module[{sums},
sums=Table[locSumNormalized[qVals,aVecs,element,s],{s,-1,3}];
Return[4\[Pi] Sqrt[\[Pi]/2] Dot[Abs[ccoeffs],sums]]
];
\[Lambda]NLExactPerPair[kVals_,aVecs_,element_,Dlist_]:=Module[{sums},
sums=Table[NLSumNormalized[kVals,aVecs,element,l,\[Alpha]],{l,0,2},{\[Alpha],1,3}];
Return[Sum[4\[Pi](2l+1)Abs[Dlist[[l+1,\[Alpha]]]]sums[[l+1,\[Alpha]]],{l,0,2},{\[Alpha],1,3}]];
];

(* ------------------------------------------------------ *)
(* Total rescaling factors *)
(* ------------------------------------------------------ *)

(* Indirect meaning per-particle/pair rescaling must be given manually. Used with exact expressions or bounds. *)
\[Lambda]ElKineticIndirect[\[Eta]e_,\[Lambda]PerParticle_]:=(\[Eta]e)\[Lambda]PerParticle;
\[Lambda]IonKineticIndirect[num\[Zeta]_,Mlist_,\[Lambda]PerParticle_]:=(Sum[num\[Zeta][[\[Zeta]]] 1/Mlist[[\[Zeta]]],{\[Zeta],1,Length@num\[Zeta]}])\[Lambda]PerParticle;
\[Lambda]ElCoulombIndirect[\[Eta]e_,\[Lambda]PerPair_]:=(\[Eta]e(\[Eta]e-1)) \[Lambda]PerPair;
\[Lambda]IonCoulombIndirect[num\[Zeta]_,ZPIlist_,\[Lambda]PerPair_]:=(Sum[num\[Zeta][[\[Zeta]]]ZPIlist[[\[Zeta]]],{\[Zeta],1,Length@num\[Zeta]}]^2-Sum[num\[Zeta][[\[Zeta]]]ZPIlist[[\[Zeta]]]^2,{\[Zeta],1,Length@num\[Zeta]}]) \[Lambda]PerPair;
\[Lambda]PIIndirect[\[Eta]e_,num\[Zeta]_,\[Lambda]PerPairlist_]:=\[Eta]e Sum[num\[Zeta][[\[Zeta]]] \[Lambda]PerPairlist[[\[Zeta]]],{\[Zeta],1,Length@num\[Zeta]}];

(* Compute rescaling factor approximate bounds *)
(*num\[Zeta] is the multiplicity of each pseudoion type. The local variables Mlist, ZPIlist, allclists, allDlists, allCtildelists 
are required to be Length@num\[Zeta] in the first dimension. *)
all\[Lambda]Bounds[instance_]:=Module[{num\[Zeta],Mlist,ZPIlist,allclists,allDlists,allCtildelists,\[Eta]el,Kmax,Qmax,\[Lambda]PerPair\[Zeta]List,
\[Lambda]Vec,elements,\[Lambda]PerPair\[Zeta]Assoc,\[Lambda]VecAssoc,Kbarmax,Qbarmax,Kbartruncmax,Qbartruncmax},

(* Unpack instance *)
elements=KeyValueMap[#1&,instance[[1]]];
num\[Zeta]=KeyValueMap[#2[[1]]&,instance[[1]]];
Mlist=KeyValueMap[#2[[2]]&,instance[[1]]];
ZPIlist=KeyValueMap[#2[[3]]&,instance[[1]]];
allclists=KeyValueMap[#2[[4]]&,instance[[1]]];
allDlists=KeyValueMap[#2[[5]]&,instance[[1]]];
allCtildelists=KeyValueMap[#2[[6]]&,instance[[1]]];
Kmax=instance[[2]]["Kmax"];
Qmax=instance[[2]]["Qmax"];
Kbarmax=instance[[2]]["Kbarmax"];
Qbarmax=instance[[2]]["Qbarmax"];
Kbartruncmax=instance[[2]]["Kbartruncmax"];
Qbartruncmax=instance[[2]]["Qbartruncmax"];
\[Eta]el=instance[[3]]["\[Eta]el"];

(* Provide clist, Dlist, Ctildelist for each \[Zeta] and compute per-pair rescaling factor bounds as a list over \[Zeta] in the first dimension. *)
\[Lambda]PerPair\[Zeta]List=Table[
{\[Lambda]KineticPerParticle[Kmax],
\[Lambda]KineticPerParticle[Kbarmax],
\[Lambda]CoulombPerPairBound[Qmax],
\[Lambda]CoulombPerPairBound[Qbartruncmax],
\[Lambda]LocPerPairBound[allclists[[\[Zeta]]]],
\[Lambda]NLPerPairBound[allDlists[[\[Zeta]]],allCtildelists[[\[Zeta]]]]},
{\[Zeta],1,Length@num\[Zeta]}];
\[Lambda]PerPair\[Zeta]Assoc=AssociationThread[elements,\[Lambda]PerPair\[Zeta]List];

(* Compute total rescaling factors. Note that per-particle \[Lambda]Kin,\[Lambda]Coul are uniform over \[Zeta] so we just take first element 
for them since all kin/coul functions accept constant \[Lambda]PerParticle. Only for \[Lambda]PI do you need \[Zeta]-dependent \[Lambda]PerParticle. *);
\[Lambda]Vec=
{\[Lambda]ElKineticIndirect[\[Eta]el,\[Lambda]PerPair\[Zeta]List[[1,1]]],
\[Lambda]IonKineticIndirect[num\[Zeta],Mlist,\[Lambda]PerPair\[Zeta]List[[1,2]]],
\[Lambda]ElCoulombIndirect[\[Eta]el,\[Lambda]PerPair\[Zeta]List[[1,3]]],
\[Lambda]IonCoulombIndirect[num\[Zeta],ZPIlist,\[Lambda]PerPair\[Zeta]List[[1,4]]],
\[Lambda]PIIndirect[\[Eta]el,num\[Zeta],\[Lambda]PerPair\[Zeta]List[[;;,5]]],
\[Lambda]PIIndirect[\[Eta]el,num\[Zeta],\[Lambda]PerPair\[Zeta]List[[;;,6]]]};
\[Lambda]VecAssoc=AssociationThread[{"Tel","Tion","Vel","Vion","Loc","NL"},\[Lambda]Vec];

Return[<|"\[Lambda]PerPairBound"->\[Lambda]PerPair\[Zeta]Assoc,"\[Lambda]TermBound"->\[Lambda]VecAssoc,"\[Lambda]Bound"->Total@\[Lambda]Vec|>]
]


(* Compute total rescaling factors exact, 
- instance: same input as for all\[Lambda]Bounds. The second part of instance (and Ctilde in first part) is ignored for exact computations.,
- basisAssoc: contains the association of all simulation cell data including the full basis.
*)
all\[Lambda]Exact[instance_,basisAssoc_]:=Module[{num\[Zeta],Mlist,ZPIlist,allclists,allDlists,\[Eta]el,\[Lambda]PerPair\[Zeta]List,\[Lambda]Vec,kVals,qVals,
elements,\[Lambda]PerPair\[Zeta]Assoc,\[Lambda]VecAssoc,kValstrunc,qValstrunc},

(* Unpack instance *)
elements=KeyValueMap[#1&,instance[[1]]];
num\[Zeta]=KeyValueMap[#2[[1]]&,instance[[1]]];
Mlist=KeyValueMap[#2[[2]]&,instance[[1]]];
ZPIlist=KeyValueMap[#2[[3]]&,instance[[1]]];
allclists=KeyValueMap[#2[[4]]&,instance[[1]]];
allDlists=KeyValueMap[#2[[5]]&,instance[[1]]];
\[Eta]el=instance[[3]]["\[Eta]el"];

(*(* Compute k values over basis *)
kVals=computeKValsOnBasis[basisAssoc];*)

(* Extract k and q values *) 
kVals=basisAssoc["basis"][[1]];
qVals=basisAssoc["basis"][[2]];

kValstrunc=basisAssoc["basistrunc"][[1]];
qValstrunc=basisAssoc["basistrunc"][[2]];

(* Provide clist, Dlist, for each \[Zeta] and compute per-pair rescaling factor bounds as a list over \[Zeta] in the first dimension. *)
\[Lambda]PerPair\[Zeta]List=Table[
{\[Lambda]KineticPerParticle[basisAssoc["Kmax"]],
\[Lambda]KineticPerParticle[basisAssoc["Kbarmax"]],
\[Lambda]CoulombExactPerPair[qVals,basisAssoc["aVecs"]],
\[Lambda]CoulombExactPerPair[qValstrunc,basisAssoc["aVecs"]], 
\[Lambda]LocExactPerPair[qVals,basisAssoc["aVecs"],elements[[\[Zeta]]],allclists[[\[Zeta]]]],
\[Lambda]NLExactPerPair[kVals,basisAssoc["aVecs"],elements[[\[Zeta]]],allDlists[[\[Zeta]]]]},
{\[Zeta],1,Length@num\[Zeta]}];
\[Lambda]PerPair\[Zeta]Assoc=AssociationThread[elements,\[Lambda]PerPair\[Zeta]List];

(* Compute total rescaling factors. Note that per-particle \[Lambda]Kin,\[Lambda]Coul are uniform over \[Zeta] so we just take 
first element for them since all kin/coul functions accept constant \[Lambda]PerParticle. Only for \[Lambda]PI do you need \[Zeta]-dependent \[Lambda]PerParticle. *);
\[Lambda]Vec=
{\[Lambda]ElKineticIndirect[\[Eta]el,\[Lambda]PerPair\[Zeta]List[[1,1]]],
\[Lambda]IonKineticIndirect[num\[Zeta],Mlist,\[Lambda]PerPair\[Zeta]List[[1,2]]],  
\[Lambda]ElCoulombIndirect[\[Eta]el,\[Lambda]PerPair\[Zeta]List[[1,3]]],
\[Lambda]IonCoulombIndirect[num\[Zeta],ZPIlist,\[Lambda]PerPair\[Zeta]List[[1,4]]],
\[Lambda]PIIndirect[\[Eta]el,num\[Zeta],\[Lambda]PerPair\[Zeta]List[[;;,5]]],
\[Lambda]PIIndirect[\[Eta]el,num\[Zeta],\[Lambda]PerPair\[Zeta]List[[;;,6]]]};
\[Lambda]VecAssoc=AssociationThread[{"Tel","Tion","Vel","Vion","Loc","NL"},\[Lambda]Vec];

Return[<|"\[Lambda]PerPairExact"->\[Lambda]PerPair\[Zeta]Assoc,"\[Lambda]TermExact"->\[Lambda]VecAssoc,"\[Lambda]Exact"->Total@\[Lambda]Vec|>]
]

End[];
EndPackage[];
