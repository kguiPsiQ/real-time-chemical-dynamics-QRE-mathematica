(* ::Package:: *)

BeginPackage["HGHData`"]

(* NOTE: To add new HGH pseudopotentials/pseudoions, append the new names to the end of allElementNames
and also write the parameters as per the formats below *)

allElementsNames={"C4","N5","O6","Al3","Si4","Fe8","Fe16","Ni10","Ni18","Cu1","Cu11","W6","W14","Ir9","Ir17",
"Pt10","Pt18","H1","B3","F7","Pd10","Pd18"};

BdiagC4[l_]:=Piecewise[{{{9.522842,0,0},l==0},{{0,0,0},l==1},{{0,0,0},l==2}}]
BdiagN5[l_]:=Piecewise[{{{13.552243,0,0},l==0},{{0,0,0},l==1},{{0,0,0},l==2}}]
BdiagO6[l_]:=Piecewise[{{{18.266917,0,0},l==0},{{0,0,0},l==1},{{0,0,0},l==2}}]
BdiagAl3[l_]:=Piecewise[{{{5.088340,2.679700,0},l==0},{{2.193438,0,0},l==1},{{0,0,0},l==2}}]
BdiagSi4[l_]:=Piecewise[{{{5.906928,3.258196,0},l==0},{{2.727013,0,0},l==1},{{0,0,0},l==2}}]
BdiagFe8[l_]:=Piecewise[{{{3.016640,2.583038,3.257635},l==0},{{1.499642,0.326874,0},l==1},{{-9.145354,0,0},l==2}}]
BdiagFe16[l_]:=Piecewise[{{{10.193723,-6.834982,0},l==0},{{0.145613,-5.234954,0},l==1},{{-12.026941,0,0},l==2}}]
BdiagNi10[l_]:=Piecewise[{{{3.619651,3.088965,3.058598},l==0},{{1.742220,0.386341,0},l==1},{{-11.608428,0,0},l==2}}]
BdiagNi18[l_]:=Piecewise[{{{3.610311,0.449638,0},l==0},{{12.161131,-9.078929,0},l==1},{{0.269572,-0.143442,0},l==2}}]
BdiagCu1[l_]:=Piecewise[{{{0.975787,-0.822070,-0.133237},l==0},{{0.024580,-0.249001,0},l==1},{{-0.065292,0,0},l==2}}]
BdiagCu11[l_]:=Piecewise[{{{3.888050,3.276584,2.290091},l==0},{{1.751272,0.374943,0},l==1},{{-12.676957,0,0},l==2}}]
BdiagW6[l_]:=Piecewise[{{{2.161166,2.741500,0},l==0},{{0.600973,1.299943,0},l==1},{{-2.517063,-0.789137,0},l==2}}]
BdiagW14[l_]:=Piecewise[{{{2.692204,-6.022637,1.218316},l==0},{{-0.702084,-2.451680,0},l==1},{{1.177436,-5.553621,0},l==2}}]
BdiagIr9[l_]:=Piecewise[{{{2.445999,2.811037,0},l==0},{{0.461792,1.304726,0},l==1},{{-4.545484,-1.635428,0},l==2}}]
BdiagIr17[l_]:=Piecewise[{{{3.243278,-7.315509,2.956978},l==0},{{-0.380574,-3.504403,0},l==1},{{0.754315,-5.875580,0},l==2}}]
BdiagPt10[l_]:=Piecewise[{{{2.447430,2.640360,0},l==0},{{0.408453,1.647716,0},l==1},{{-4.552295,-2.102396,0},l==2}}]
BdiagPt18[l_]:=Piecewise[{{{2.994366,-7.448772,4.243095},l==0},{{-0.225181,-3.776974,0},l==1},{{0.0632067,-5.755431,0},l==2}}]
BdiagH1[l_]:=Piecewise[{{{0,0,0},l==0},{{0,0,0},l==1},{{0,0,0},l==2}}]
BdiagB3[l_]:=Piecewise[{{{6.233928,0,0},l==0},{{0,0,0},l==1},{{0,0,0},l==2}}]
BdiagF7[l_]:=Piecewise[{{{23.584942,0,0},l==0},{{0,0,0},l==1},{{0,0,0},l==2}}]
BdiagPd10[l_]:=Piecewise[{{{2.411076,2.318920,0},l==0},{{1.227253,0.758021,0},l==1},{{-4.377131,0.413271,0},l==2}}]
BdiagPd18[l_]:=Piecewise[{{{5.177686,-5.852819,0},l==0},{{-0.372561,-3.258728,0},l==1},{{-1.608273,1.446609,0},l==2}}]

rlParamsC4={0.304553,0.232677,0};
rlParamsN5={0.256605,0.270134,0};
rlParamsO6={0.221786,0.256829,0};
rlParamsAl3={0.46014,0.536744,0};
rlParamsSi4={0.422738,0.484278,0};
rlParamsFe8={0.454482,0.638903,0.308732};
rlParamsFe16={0.269268,0.247686,0.223021};
rlParamsNi10={0.425399,0.584081,0.278113};
rlParamsNi18={0.245105,0.234741,0.214950};
rlParamsCu1={0.843283,1.089543,1.291602};
rlParamsCu11={0.423734,0.572177,0.266143};
rlParamsW6={0.582463,0.742307,0.534959};
rlParamsW14={0.418570,0.449555,0.399602};
rlParamsIr9={0.509960,0.684971,0.471745};
rlParamsIr17={0.404469,0.411426,0.376428};
rlParamsPt10={0.520132,0.658976,0.451243};
rlParamsPt18={0.409942,0.398652,0.367964};
rlParamsH1={0,0,0};
rlParamsB3={0.373843,0.360393,0};
rlParamsF7={0.195567,0.174268,0};
rlParamsPd10={0.582204,0.688787,0.442835};
rlParamsPd18={0.342151,0.343111,0.494916};

LocParamsC4={4,0.348830,-8.513771,1.228432,0,0};
LocParamsN5={5,0.289179,-12.234820,1.766407,0,0};
LocParamsO6={6,0.247621,-16.580318,2.395701,0,0};
LocParamsAl3={3,0.45,-8.491351,0,0,0};
LocParamsSi4={4,0.44,-7.336103,0,0,0};
LocParamsFe8={8,0.61,0,0,0,0};
LocParamsFe16={16,0.36,5.392507,-0.030066,0,0};
LocParamsNi10={10,0.56,0,0,0,0};
LocParamsNi18={18,0.35,3.610311,0.449638,0,0};
LocParamsCu1={1,0.58,0,0,0,0};
LocParamsCu11={11,0.53,0,0,0,0};
LocParamsW6={6,0.719,4.05845,0,0,0};
LocParamsW14={14,0.54,4.800251,0.901544,0,0};
LocParamsIr9={9,0.641,10.720016,0,0,0};
LocParamsIr17={17,0.51,4.904509,1.313786,0,0};
LocParamsPt10={10,0.61600,11.027417,0,0,0};
LocParamsPt18={18,0.5,5.445832,1.156382,0,0};
LocParamsH1={1,0.2,-4.180237,0.725075,0,0};
LocParamsB3={3,0.433930,-5.578642,0.804251,0,0};
LocParamsF7={7,0.218525,-21.307361,3.072869,0,0};
LocParamsPd10={10,0.596,5.209665,0,0,0};
LocParamsPd18={18,0.41,15.720259,0.140765,0,0};

ionMass[Mamu_,Zval_]:=1823 Mamu-Zval
MassC4=ionMass[12.011,LocParamsC4[[1]]];
MassN5=ionMass[14.007,LocParamsN5[[1]]];
MassO6=ionMass[15.999,LocParamsO6[[1]]];
MassAl3=ionMass[26.982,LocParamsAl3[[1]]];
MassSi4=ionMass[28.086,LocParamsSi4[[1]]];
MassFe8=ionMass[55.845,LocParamsFe8[[1]]];
MassFe16=ionMass[55.845,LocParamsFe16[[1]]];
MassNi10=ionMass[58.693,LocParamsNi10[[1]]];
MassNi18=ionMass[58.693,LocParamsNi18[[1]]];
MassCu1=ionMass[63.546,LocParamsCu1[[1]]];
MassCu11=ionMass[63.546,LocParamsCu11[[1]]];
MassW6=ionMass[183.84,LocParamsW6[[1]]];
MassW14=ionMass[183.84,LocParamsW14[[1]]];
MassIr9=ionMass[192.22,LocParamsIr9[[1]]];
MassIr17=ionMass[192.22,LocParamsIr17[[1]]];
MassPt10=ionMass[195.08,LocParamsPt10[[1]]];
MassPt18=ionMass[195.08,LocParamsPt18[[1]]];
MassH1=ionMass[1.00784,LocParamsH1[[1]]];
MassB3=ionMass[10.811,LocParamsB3[[1]]];
MassF7=ionMass[18.9984,LocParamsF7[[1]]];
MassPd10=ionMass[106.42,LocParamsPd10[[1]]];
MassPd18=ionMass[106.42,LocParamsPd18[[1]]];

EndPackage[];
