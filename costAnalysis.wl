(* ::Package:: *)

(* Instance construction and cost analysis methods *)

BeginPackage["CostAnalysis`"];

constructInput::usage = "constructInput[ionInput_,basis_] constructs minimal input from 
association of Key = ion type, Value = number of ions, 
with minimal info taken from basis to construct bounds only."
constructInstance::usage = "constructInstance[inputAssoc_] constructs instance which has 3 assocs. 
The first augments the input with all requisite information: Key = ion, and 
Value = either constants or have [[s]] or [[l,\[Alpha]]] indexing depending on the data.
The second is inputAssoc[[2]], the minimal basis info coming from the input for 
simple computation of bounds if full basis calculation is not desired. The third details particle counts."
fullBlockEncodingCost::usage = "fullBlockEncodingCost[input_,basis_,exactToggle_] gives the block-encoding 
Toffoli gate cost analysis and system qubit cost (no ancilla) for the problem instance"
totalCost::usage = "totalCost[fullResults_,t_,\[Epsilon]_,exactToggle_] gives the time-evolution Toffoli gate cost and system qubit cost analysis 
for the problem instance with fullResults = output of fullBlockEncodingCost[...]"
spatialCost::usage = "spatialCost[inputAssoc_,instanceBasis_] gives the system qubit cost (no ancilla) for the problem instance"
viewAllResults::usage = "viewAllResults[fullResults_,t_,\[Epsilon]_] is a nice display of the fullResults = output of fullBlockEncodingCost[...]"


Needs["Preprocessing`"];
Needs["RescalingFactor`"];
Needs["CompilationCost`"];


Begin["`Private`"];
(* ------------------------------------------------------ *)
(* Input and Instance Construction *)
(* ------------------------------------------------------ *)

(* Constructs minimal input from association of Key = ion type, Value = number of ions, 
with minimal info taken from basis to construct bounds only. *)
constructInput[ionInput_,basis_]:={ionInput,KeyTake[basis,{"Kmax","Qmax","Kbarmax","Qbarmax","Kbartruncmax","Qbartruncmax"}]};

(* Constructs instance which has 3 assocs. The first augments the input with all requisite information: Key = ion, and 
Value = either constants or have [[s]] or [[l,\[Alpha]]] indexing depending on the data.
The second is inputAssoc[[2]], the minimal basis info coming from the input for 
simple computation of bounds if full basis calculation is not desired. The third details particle counts. *)
constructInstance[inputAssoc_]:=Module[{\[Eta]el,\[Eta]ion,particuleNumAssoc,allDataAssoc},
\[Eta]el=Dot[Preprocessing`ionChargeAssoc/@Keys[inputAssoc[[1]]],Values[inputAssoc[[1]]]];
\[Eta]ion=Total@Values[inputAssoc[[1]]];
particuleNumAssoc=<|"\[Eta]el"->\[Eta]el,"\[Eta]ion"->\[Eta]ion,"\[Eta]"->\[Eta]el+\[Eta]ion|>;
allDataAssoc=Association[KeyValueMap[#1->{#2,Preprocessing`ionMassAssoc[#1],Preprocessing`ionChargeAssoc[#1],
Preprocessing`cCoeffsLocAssoc[#1],Preprocessing`DValsNLAssoc[#1],Preprocessing`CtildeValsNLAssoc[#1]} &,inputAssoc[[1]]]];
Return[{allDataAssoc,inputAssoc[[2]],particuleNumAssoc}]
]

(* ------------------------------------------------------ *)
(* Cost Analysis *)
(* ------------------------------------------------------ *)

spatialCost[inputAssoc_,instanceBasis_]:=Module[{\[Eta]el,\[Eta]ion,systemCost},
\[Eta]el=Dot[Preprocessing`ionChargeAssoc/@Keys[inputAssoc[[1]]],Values[inputAssoc[[1]]]];
\[Eta]ion=Total@Values[inputAssoc[[1]]];
systemCost=\[Eta]el*Total[instanceBasis["nqubits"]]+\[Eta]ion*Total[instanceBasis["nqubits\[CapitalLambda]bar"]];
Return[systemCost]
]

computeCompilingCost[instance_,basis_]:=CompilationCost`TotalCost[instance[[3]]["\[Eta]el"], instance[[3]]["\[Eta]ion"],Length[instance[[1]]],
basis["nqubits"][[1]],basis["nqubits"][[2]],basis["nqubits"][[3]],basis["nqubits\[CapitalLambda]bar"][[1]],basis["nqubits\[CapitalLambda]bar"][[2]],basis["nqubits\[CapitalLambda]bar"][[3]]]

(* Computes rescaling factors, compiling costs, and system qubit cost *)
fullBlockEncodingCost[input_,basis_,exactToggle_]:=Module[{instance,compilationCost,\[Lambda]Bound,\[Lambda]Exact,qubitCost},
instance=constructInstance[input];
compilationCost=computeCompilingCost[instance,basis];
qubitCost=spatialCost[input,basis];
\[Lambda]Bound=RescalingFactor`all\[Lambda]Bounds[instance];
If[exactToggle==True,\[Lambda]Exact=RescalingFactor`all\[Lambda]Exact[instance,basis],\[Lambda]Exact={}];
Return[<|"particle"->instance[[3]],"bound"->\[Lambda]Bound,"exact"->\[Lambda]Exact,"compilationCost"->compilationCost,"qubitCost"->qubitCost|>]
]

(* Assembles rescaling costs and compiling costs for the BE to give total cost of time-evolution for a given time and error *)
timeEvoCost[compCost_,\[Lambda]_,t_,\[Epsilon]_]:=Module[{c,queryCost,totalCost},
c=4/(Sqrt[2\[Pi]]Exp[1/13]) //N; (* ~1.47762 *)
queryCost=Ceiling[Exp[1]/2 \[Lambda] t +Log[(2c)/\[Epsilon]]]+2;
totalCost=queryCost compCost; (* flagging that this should be slightly modified for true exact count *)
Return[<|"query"->queryCost,"compilation"->compCost,"total"->totalCost|>]
]

totalCost[fullResults_,t_,\[Epsilon]_,exactToggle_]:=Module[{x},
If[exactToggle==True,
Return[
<|"exact"->timeEvoCost[Last@fullResults["compilationCost"],fullResults["exact"]["\[Lambda]Exact"],t,\[Epsilon]],
"bound"->timeEvoCost[Last@fullResults["compilationCost"],fullResults["bound"]["\[Lambda]Bound"],t,\[Epsilon]],
"qubitCost"->fullResults["qubitCost"]|>],
Return[
<|"exact"->{},"bound"->timeEvoCost[Last@fullResults["compilationCost"],fullResults["bound"]["\[Lambda]Bound"],t,\[Epsilon]],
"qubitCost"->fullResults["qubitCost"]|>]
]
];

(* ------------------------------------------------------ *)
(* Cost analysis display methods *)
(* ------------------------------------------------------ *)

formatData[data_]:=Map[Association@*KeyValueMap[Function[{key,value},key->If[IntegerQ[value],ScientificForm[1.0*value,5],value]]],data];

viewExactBoundComparison[exactData_,boundsData_]:={{Dataset[exactData["\[Lambda]PerPairExact"]],Dataset[boundsData["\[Lambda]PerPairBound"]]},
{Dataset[exactData["\[Lambda]TermExact"]],Dataset[boundsData["\[Lambda]TermBound"]]},
{exactData["\[Lambda]Exact"],boundsData["\[Lambda]Bound"]}}

viewExactBoundComparisonLabeled[exactData_,boundsData_]:=Module[{labelData,dataset1Pair,dataset2Pair,combinedDataPair,
totalDataPieChart,ratioAssoc,exactWithTotal,boundWithTotal},
labelData[data_]:=Module[{termLabels,labeledData},
(*Define labels*)
termLabels={"Tel","Tion","Vel","Vion","Loc","NL"};
(*Restructure the data for Dataset*)
labeledData=KeyValueMap[Function[{key,values},Prepend[AssociationThread[termLabels,values],"Ion"->key]],data];
Return[labeledData]
];

dataset1Pair=Dataset@labelData[exactData["\[Lambda]PerPairExact"]];
dataset2Pair=Dataset@labelData[boundsData["\[Lambda]PerPairBound"]];

combinedDataPair=MapThread[Function[{row1,row2},Association[KeyValueMap[Function[{label,value1},
If[label==="Key",label->value1,(*Preserve "Key" column*)label->Row[{value1,Style[row2[label],Gray]},"\n"] (*Stack values*)]],row1]]],
{Normal[dataset1Pair],Normal[dataset2Pair]} (*Process rows in parallel*)];

(* Adding total to exact and bound tables for ease. Append is non-destructive. *)
exactWithTotal=Append[exactData["\[Lambda]TermExact"],"Total"->exactData["\[Lambda]Exact"]];
boundWithTotal=Append[boundsData["\[Lambda]TermBound"],"Total"->boundsData["\[Lambda]Bound"]];
ratioAssoc=boundWithTotal/exactWithTotal;

Return[{Dataset@combinedDataPair,Dataset@{exactWithTotal,boundWithTotal,ratioAssoc}}];

];

viewAllResults[fullResults_,t_,\[Epsilon]_]:=Module[{x},
Print["Particle Count:",fullResults["particle"]];
Print["Rescaling Factors:",viewExactBoundComparisonLabeled[fullResults["exact"],fullResults["bound"]]];
Print["Compilation Cost:",fullResults["compilationCost"]];
Print["Qubit Cost:",fullResults["qubitCost"]];
Print["Time Evolution Cost:",Dataset@formatData@KeyDrop[totalCost[fullResults,t,\[Epsilon],True],"qubitCost"]];
]

End[];
EndPackage[];
