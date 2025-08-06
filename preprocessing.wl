(* ::Package:: *)

(* HGH data pre-processing *)
BeginPackage["Preprocessing`"];

allGcoeffs::usage="all B matrix eigenvectors to construct G-functions from g-functions."
XFullAssoc::usage="X matrix (eigenvalues/vectors of B) assoc"
rlocAssoc::usage = "rloc parameter assoc"
rlAssoc::usage = "rl parameter assoc"
ionChargeAssoc::usage = "ion Charge parameter assoc."
ionMassAssoc::usage = "ion Mass parameter assoc."
DValsNLAssoc::usage = "Eigenvalues of B matrix for NL term."
CtildeValsNLAssoc::usage = "Ctilde coeffs for NL bounds."
cCoeffsLocAssoc::usage = "c coeffs (polynomially-ordered local term coeffs)."
Gfunc::usage = "G-function (eigenvector reweighted g-functions)."
GfuncAbs::usage = "|G|-function (eigenvector reweighted g-functions)."


Needs["HGHData`"]; (* will use unqualified lookup of HGHData variables below for ease *)

Begin["`Private`"];
(* ------------------------------------------------------ *)
(* Nonlocal Parameters B matrix analysis *)
(* ------------------------------------------------------ *)

(* Methods to construct B matrix and diagonalize to get X matrix *)
B11[Bdiag_,l_]:=Part[Bdiag[l],1]
B22[Bdiag_,l_]:=Part[Bdiag[l],2]
B33[Bdiag_,l_]:=Part[Bdiag[l],3]
B12[Bdiag_,l_]:=\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{
RowBox[{"-", 
RowBox[{"(", 
RowBox[{"1", "/", "2"}], ")"}]}], 
RowBox[{"Sqrt", "[", 
RowBox[{"3", "/", "5"}], "]"}], 
RowBox[{"B22", "[", 
RowBox[{"Bdiag", ",", "l"}], "]"}]}], 
RowBox[{"l", "==", "0"}]},
{
RowBox[{
RowBox[{"-", 
RowBox[{"(", 
RowBox[{"1", "/", "2"}], ")"}]}], 
RowBox[{"Sqrt", "[", 
RowBox[{"5", "/", "7"}], "]"}], 
RowBox[{"B22", "[", 
RowBox[{"Bdiag", ",", "l"}], "]"}]}], 
RowBox[{"l", "==", "1"}]},
{
RowBox[{
RowBox[{"-", 
RowBox[{"(", 
RowBox[{"1", "/", "2"}], ")"}]}], 
RowBox[{"Sqrt", "[", 
RowBox[{"7", "/", "9"}], "]"}], 
RowBox[{"B22", "[", 
RowBox[{"Bdiag", ",", "l"}], "]"}]}], 
RowBox[{"l", "==", "2"}]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)
B13[Bdiag_,l_]:=\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{
RowBox[{"(", 
RowBox[{"1", "/", "2"}], ")"}], 
RowBox[{"Sqrt", "[", 
RowBox[{"5", "/", "21"}], "]"}], 
RowBox[{"B33", "[", 
RowBox[{"Bdiag", ",", "l"}], "]"}]}], 
RowBox[{"l", "==", "0"}]},
{
RowBox[{
RowBox[{"(", 
RowBox[{"1", "/", "6"}], ")"}], 
RowBox[{"Sqrt", "[", 
RowBox[{"35", "/", "11"}], "]"}], 
RowBox[{"B33", "[", 
RowBox[{"Bdiag", ",", "l"}], "]"}]}], 
RowBox[{"l", "==", "1"}]},
{
RowBox[{
RowBox[{"(", 
RowBox[{"1", "/", "2"}], ")"}], 
RowBox[{"Sqrt", "[", 
RowBox[{"63", "/", "143"}], "]"}], 
RowBox[{"B33", "[", 
RowBox[{"Bdiag", ",", "l"}], "]"}]}], 
RowBox[{"l", "==", "2"}]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)
B23[Bdiag_,l_]:=\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{
RowBox[{"-", 
RowBox[{"(", 
RowBox[{"1", "/", "2"}], ")"}]}], 
RowBox[{"(", 
RowBox[{"10", "/", 
RowBox[{"Sqrt", "[", "63", "]"}]}], ")"}], 
RowBox[{"B33", "[", 
RowBox[{"Bdiag", ",", "l"}], "]"}]}], 
RowBox[{"l", "==", "0"}]},
{
RowBox[{
RowBox[{"-", 
RowBox[{"(", 
RowBox[{"1", "/", "6"}], ")"}]}], 
RowBox[{"(", 
RowBox[{"14", "/", 
RowBox[{"Sqrt", "[", "11", "]"}]}], ")"}], 
RowBox[{"B33", "[", 
RowBox[{"Bdiag", ",", "l"}], "]"}]}], 
RowBox[{"l", "==", "1"}]},
{
RowBox[{
RowBox[{"-", 
RowBox[{"(", 
RowBox[{"1", "/", "2"}], ")"}]}], 
RowBox[{"(", 
RowBox[{"18", "/", 
RowBox[{"Sqrt", "[", "143", "]"}]}], ")"}], 
RowBox[{"B33", "[", 
RowBox[{"Bdiag", ",", "l"}], "]"}]}], 
RowBox[{"l", "==", "2"}]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)
B21[Bdiag_,l_]:=B12[Bdiag,l]
B31[Bdiag_,l_]:=B13[Bdiag,l]
B32[Bdiag_,l_]:=B23[Bdiag,l]
Bmat[Bdiag_,l_]:={{B11[Bdiag,l],B12[Bdiag,l],B13[Bdiag,l]},{B21[Bdiag,l],B22[Bdiag,l],B23[Bdiag,l]},
{B31[Bdiag,l],B32[Bdiag,l],B33[Bdiag,l]}}

computeXmats[Bm_]:=Table[Transpose[Eigensystem[Bm[[l+1]]] [[2]]] ,{l,0,2}]

(* Compute all B,X matrices for l=0,1,2 *)
allBmats=Table[Table[Bmat[Symbol["Bdiag"<>name],l],{l,0,2}],{name,allElementsNames}];
allXmats=Table[computeXmats[allBmats[[i]]],{i,1,Length[allBmats]}];

numElements=Length@allElementsNames;

LabeledBmats=AssociationThread[allElementsNames,allBmats];
LabeledXmats=AssociationThread[allElementsNames,allXmats];

(* Organize all eigenvector coeffs for all cases *)
allcoeffsl0=Flatten[Table[Transpose[allXmats[[i]][[0+1]]],{i,1,Length[allXmats]}],1];
allcoeffsl1=Flatten[Table[Transpose[allXmats[[i]][[1+1]]],{i,1,Length[allXmats]}],1];
allcoeffsl2=Flatten[Table[Transpose[allXmats[[i]][[2+1]]],{i,1,Length[allXmats]}],1];
allGcoeffs = {allcoeffsl0,allcoeffsl1,allcoeffsl2};

(* Compute both eigenvalues and eigenvectors of the B matrices - redundant to allXmats but the need to keep eigenvalues 
came after so easier to repeat and not break legacy code. Also, it has nicer structure for processing. *)
computeXFull[Bm_]:=Table[Transpose[Eigensystem[Bm[[l+1]]]] ,{l,0,2}]
allXFull=Table[computeXFull[allBmats[[i]]],{i,1,Length[allBmats]}];
Dimensions@allXFull; (*allXFull[[ion,l,\[Alpha],eigval/eigvec=1,2]]*)
XFullAssoc=AssociationThread[allElementsNames,allXFull];

(* Create index -> element name map and inverse *)
indexGroups = Table[{3i+1,3i+2,3i+3},{i,0,numElements-1}];
nameIndexMap = Association[];
Table[Table[AssociateTo[nameIndexMap,indexGroups[[i]][[j]]->allElementsNames[[i]]],{j,1,3}],{i,1,Length@indexGroups}];
nameIndexMapInverse=GroupBy[Keys[nameIndexMap],nameIndexMap[#]&];
(* Find index for element *)
ionIndex[elementName_]:=(Flatten@Position[allElementsNames,elementName])[[1]]

(* ------------------------------------------------------ *)
(* Pre-processing rl (nonlocal radius), local parameters, ion charge, and mass *)
(* ------------------------------------------------------ *)

allrlParams=Symbol["rlParams"<>#]&/@allElementsNames;
rlAssoc=AssociationThread[allElementsNames,allrlParams];

allLocParams=Symbol["LocParams"<>#]&/@allElementsNames;
rlocAssoc=AssociationThread[allElementsNames,allLocParams[[;;,2]]];

ionChargeAssoc=AssociationThread[allElementsNames,allLocParams[[;;,1]]];

allMassParams=Symbol["Mass"<>#]&/@allElementsNames;
ionMassAssoc= AssociationThread[allElementsNames,allMassParams];

(* ------------------------------------------------------ *)
(* Compute c coeffs (local parameters in polynomial order) *)
(* ------------------------------------------------------ *)

computeccoeffsLoc[LocParams_]:=Module[{cm1,c0,c1,c2,c3,C1,C2,C3,C4},
C1=LocParams[[3]];
C2=LocParams[[4]];
C3=LocParams[[5]];
C4=LocParams[[6]];

cm1=-Sqrt[2/\[Pi]] LocParams[[1]]/LocParams[[2]];
c0=C1+3C2+15C3+105C4;
c1=-C2-10C3-105C4;
c2=C3+21C4;
c3=-C4;

Return[{cm1,c0,c1,c2,c3}]
];
cCoeffsLocAssoc=AssociationThread[allElementsNames,Map[computeccoeffsLoc,allLocParams]];

(* ------------------------------------------------------ *)
(* Define g-functions and G-functions (reweighted by eigenvectors of B) *)
(* ------------------------------------------------------ *)

(* Define non-local functions: g and G (linear combinations of g) *)
gfunc[l_,a_,k_]:=(Sqrt[\[Pi]]2^(a-1) Factorial[a-1]/Sqrt[Gamma[l+2a-1/2]])(Exp[-k^2/2]LaguerreL[a-1,l+1/2,k^2/2])\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{"1", 
RowBox[{"l", "==", "0"}]},
{
SuperscriptBox["k", "l"], 
RowBox[{"l", ">=", "1"}]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)
gfuncAbs[l_,a_,k_]:=Abs[gfunc[l,a,k]]
Gfunc[l_,coeffs_,k_]:=Sum[coeffs[[a]]gfunc[l,a,k],{a,1,3}];
GfuncAbs[l_,coeffs_,k_]:=Abs[Gfunc[l,coeffs,k]];

(* ------------------------------------------------------ *)
(* Compute Ctilde coeffs (for nonlocal bounds) *)
(* ------------------------------------------------------ *)

(* computing G^2 integrals *)
(* allGcoeffs[[l,index,components]] l, index=ion*eigenstate by nameIndexMap, components *)

computeGSqRadialIntegral[l_,index_,limit_]:=NIntegrate[k^2 Gfunc[l+1,allGcoeffs[[l+1,index]],k]^2,{k,0,limit}]
CtildeCoeffsForBound=2/\[Pi] Table[computeGSqRadialIntegral[l,i,Infinity],{i,1,Length@nameIndexMap},{l,0,2}] ;

(* Reshape to (ion,l,\[Alpha]) *)
CtildeCoeffs=Transpose[Table[Partition[Transpose[CtildeCoeffsForBound][[l+1]],3],{l,0,2}],{2,1,3}]; 
CtildeValsNLAssoc=AssociationThread[allElementsNames,CtildeCoeffs];

(* ------------------------------------------------------ *)
(* Compute D coeffs *)
(* ------------------------------------------------------ *)

(* Test to see if all eigenvectors are in the same order from the calculations since done twice, G^2 
and Ctilde is with allGCoeffs from allXMats and D with allXFull *)
(* allXFull[[ion,l,\[Alpha]]] is list {eigenvalue, eigenvector} *)
(* allGcoeffs[[l,index]] = eigenvector *)

testCalculationsMatchOrder[element_]:=Module[{ionI,ionComboInds,check},
ionI=ionIndex[element];(* find index in ion name array for element *)
ionComboInds=nameIndexMapInverse[[element]]; (* find flattened indices for all \[Alpha] cases for the element *)

(*allGcoeffs[[1+l]][[ionComboInds]] ; (* all eigenvectors for give l *);
allXFull[[ionInd]][[1+l]] ; (* eigensystem for (ion,l) case *);
allXFull[[ionInd]][[1+l]] [[;;,1]] ; (* eigenvalues for (ion,l) case *);
allXFull[[ionInd]][[1+l]] [[;;,2]] ;(* eigenvectors for (ion,l) case *);*)

(* Test eigenvectors are equal *)
check=Table[allGcoeffs[[l+1]][[ionComboInds]] ==allXFull[[ionI]][[l+1]] [[;;,2]] ,{l,0,2}];
Return[check];
]
AllTrue[Flatten@Table[testCalculationsMatchOrder[allElementsNames[[i]]],{i,1,Length@allElementsNames}],TrueQ]

(* Compute all D coeffs in the same shape as CtildeCoeffs *)
computeDlist[element_]:=Module[{Dlist},
ionInd=(Flatten@Position[allElementsNames,element])[[1]]; (* find index in ion name array for element *)
(* eigenvalues for (ion,l) case *)
Return[Table[allXFull[[ionInd]][[l+1]] [[;;,1]] ,{l,0,2}]]; (* (l,\[Alpha]) *)
]
allDLists=Table[computeDlist[allElementsNames[[i]]],{i,1,Length@allElementsNames}];
DValsNLAssoc=AssociationThread[allElementsNames,allDLists];

End[];
EndPackage[];
