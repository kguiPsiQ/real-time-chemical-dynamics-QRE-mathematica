(* ::Package:: *)

BeginPackage["SimulationCell`"];

convertAngstromtoAUCellSpec::usage = "converts Angstrom spec fo AU"
aVecsRectCuboid::usage = "Rectangular cuboid."
aVecsInPlane60deg::usage = "Hexagonal 2D + z direction. 60 deg convention."
aVecsInPlane120deg::usage = "Hexagonal 2D + z direction. 120 deg convention."

Begin["`Private`"];

convertAngstromtoAUCellSpec[angstromCell_]:=angstromCell*1.89;

aVecsRectCuboid[aList_]:={aList[[1]]{1,0,0},aList[[2]]{0,1,0},aList[[3]]{0,0,1}};
aVecsInPlane60deg[aList_]:={aList[[1]]{1,0,0},aList[[2]]{1/2,Sqrt[3]/2,0},aList[[3]]{0,0,1}};
aVecsInPlane120deg[aList_]:={aList[[1]]{1,0,0},aList[[2]]{-(1/2),Sqrt[3]/2,0},aList[[3]]{0,0,1}};

End[];
EndPackage[];
