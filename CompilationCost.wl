(* ::Package:: *)

(* Compilation Costs *)
BeginPackage["CompilationCost`"];

TotalCost::usage = "TotalCost[...] gives the compilation cost for block-encoding the full Hamiltonian"

Begin["`Private`"];

(* Set precision bits and global variables *)
bP := 7; (* USP: precision for rotations in term selection prep. Google uses b=7. *)
bk := 7; (* USP: precision for ref state for |k|^2 in kinetic term. Google uses b=7. *)
bT := 7; (* USP: precision for prep in kinetic term. Google uses b=7. *)
b\[Kappa] :=7; (* USP: precision for prep of \[Phi]V in Coulomb term. Google uses b=7. *)
bg := 7 (* USP: precision for prep of \[Phi]Vq in Coulomb term. Google uses b=7. *)
bZ := 7(* USP: precision for USP in ion input state prep for local and NL terms. Google uses b=7. *)
b\[Alpha]l := 7;(* USP: precision for alias sampler input. Google uses b=7. *)
bM := 7; (* USP: precision for QRS in NL term. Google uses b=7. *)
bconst := 7; (* USP: precision for USP for const part of reference state. Google uses b=7. *)
bkeep := 20; (* Size of inequality test in alias sampler *)
bcoeff := 20; (* Bits of precision for LKS state prep for the coeff state in the local term. *)
bz := 20; (* Bits of precision for the ionic state prep in the local term. *)
bI := 7; (* USP: precision for USP in the ionic prep in the local term. Google uses b=7. *);
brot := 20; (* Bits of precision for rotating weight between const and exp in reference state. *)
bexp := 20; (* Bits of precision for the rotations in the exponential part. *)
bpl := 20; (* Bits of precision for the rotations in the power law part. *)
b := 20; (* Number of arithmetic bits for electrons. Google uses b=20. *)
barb := 23; (* Number of arithmetic bits for ions. Must be >= b.*)
MT := 1024; (* Number of amplitudes prepared by USP for the inequality test in kinetic term. *)
M := 1024;(* Number of amplitudes prepared by USP for the inequality test in QRS. *)
kl := {1.4,1.8,2.2}; (* Transition points for the reference states. *)
AAReps := 2; (* Number of amplitude amplification repetitions needed for QRS in the NL term. Good numerical evidence that this should be <= 2. *)

(* Cost Expressions *)

sharedCost[\[Eta]e_, \[Eta]I_,nx_, ny_, nz_,Nx_,Ny_,Nz_] := Module[{n=nx+ny+nz,N=Nx+Ny+Nz, prepTerms, SwupE, SwupI, prepTermsdag},
prepTerms = 3bP + 3;
SwupE = 4\[Eta]e n + 4 \[Eta]e - 8;
SwupI = 4\[Eta]I N + 4 \[Eta]I - 8;
prepTermsdag = 3bP+3;
{{prepTerms, SwupE, SwupI, prepTermsdag},prepTerms+SwupE + SwupI+prepTermsdag }
];

TCost[\[Eta]e_, \[Eta]I_, Nx_, Ny_, Nz_]:= Module[{\[Eta] = \[Eta]e+\[Eta]I,N=Nx+Ny+Nz, N2=Nx^2+Ny^2+Nz^2,maxN=Max[Nx,Ny,Nz], phiT, phiTdag,ineqtest,refState, refStatedag, compkP, compkPdag},
phiT = 6Ceiling[Log2[MT]]+13Ceiling[Log2[\[Eta]]]+8bT-30;
refState = Ceiling[Log2[\[Eta]]] + 7 barb + 7b + 4bk -12;
compkP=5/2 N2+2N^2+4 barb N - 2 maxN(maxN +barb);
ineqtest = b+barb;
compkPdag=5/2 N2+2N^2+4 barb N - 2 maxN(maxN +barb);
refStatedag = Ceiling[Log2[\[Eta]]] + 7 barb + 7b + 4bk -12;
phiTdag = 6Ceiling[Log2[MT]]+13Ceiling[Log2[\[Eta]]]+8bT-30;
{{phiT, refState,compkP,ineqtest,compkPdag,refStatedag , phiTdag},phiT+refState+compkP+ineqtest+compkPdag+refStatedag+phiTdag}];

VCost[\[Eta]e_, \[Eta]I_, nx_,ny_,nz_, Nx_, Ny_, Nz_]:= Module[{n = nx+ny+nz,n2= nx^2+ny^2+nz^2,N=Nx+Ny+Nz,N2=Nx^2+Ny^2+Nz^2,prep1,prep2el,prep2ion, selVel,selVion,prep2eldag,prep2iondag, prep1dag},
prep1 = 6(\[Eta]e+5Ceiling[Log2[\[Eta]e+\[Eta]I]]+2Ceiling[Log2[2\[Eta]e]]+2b\[Kappa]-8);
prep2el  = Ceiling[5n2 + 4n^2+8n bg ];
prep2ion = Ceiling[5N2+4N^2+8N bg];
selVel = 8n;
selVion= 8N;
prep2iondag = Ceiling[1/2 (5N2+4N^2+8N bg)];
prep2eldag = Ceiling[1/2 (5n2 + 4n^2+8n bg)];
prep1dag = 2(\[Eta]e+5Ceiling[Log2[\[Eta]e+\[Eta]I]]+2Ceiling[Log2[2\[Eta]e]]+2b\[Kappa]-8);
{{prep1,prep2el,prep2ion,selVel,selVion,prep2iondag,prep2eldag,prep1dag}, prep1+prep2el+prep2ion+selVel+selVion+prep2iondag,prep2eldag+ prep1dag}];

locCost[\[Eta]e_,\[Eta]I_,Z_, nx_,ny_,nz_, Nx_, Ny_, Nz_] :=
Module[{n=nx+ny+nz,N=Nx+Ny+Nz,prepcoeff, prepion, prepel, sel, prepeldag,prepiondag, prepcoeffdag},
prepel = 7Ceiling[Log2[\[Eta]e]]+2bZ -6;
prepion = 6Z+ Ceiling[Log2[Z]](bz-3)+7Ceiling[Log2[\[Eta]I]]+2bI - 6;
prepcoeff=Z(2 bcoeff +bkeep+25);
sel = 8N;
prepcoeffdag = Z(2 bcoeff +bkeep+25);
prepiondag = 6Z+ Ceiling[Log2[Z]](bz-3)+7Ceiling[Log2[\[Eta]I]]+2bI - 6;
prepeldag = 7Ceiling[Log2[\[Eta]e]]+2bZ -6;
{{prepcoeff, prepion, prepel, sel, prepeldag, prepiondag, prepcoeffdag},prepcoeff+prepion+prepel+sel+prepeldag+prepiondag+prepcoeffdag }];

nlCost[\[Eta]I_,Z_, nx_,ny_,nz_, Nx_,Ny_,Nz_] := Module[{n=nx+ny+nz,n2 = nx^2+ny^2+nz^2,N=Nx+Ny+Nz,maxn=Max[nx,ny,nz], prepel, prepion,loadspecies,prep1,prep2, prepG, USPG, IneqG, USPP, prepP, IneqP, prepPdag, USPPdag, nuclmomentum,IneqGdag, USPGdag, prepGdag, prep2dag, prep1dag, prepiondag, prepeldag},
prepel=0;
prepion = 4Z+Ceiling[Log2[Z]](bZ-3)-2;
loadspecies = \[Eta]I;
prep1 = 11Z + 3*Ceiling[Log2[9Z]]+2b\[Alpha]l +bkeep -8;
prep2 = (1+2 AAReps)*(12n2 + 74n+4n^2+6n bpl + 6n bexp + 3 brot + 8); 
prepG = (1+ AAReps)*(4n2 + 2n^2+7b n -2maxn(maxn+b)+ (51/4)b^2+32b+116);
USPG = 0;
IneqG = (1+AAReps)*(b+Ceiling[Log2[M]]);
USPP = 0;
prepP = Ceiling[5n2 + 5n^2+8b n + (21/4)b^2+(13/2)b-6-2maxn(maxn+b)];
IneqP = 2 (Max[b, Ceiling[Log2[M]]])^2+Max[b, Ceiling[Log2[M]]];
prepPdag = Ceiling[5n2 + 5n^2+8b n + (21/4)b^2+(13/2)b-6-2maxn(maxn+b)];
USPPdag=0;
nuclmomentum = 2N;
IneqGdag = (1+AAReps)*(b+Ceiling[Log2[M]]);
USPGdag=0;
prepGdag = (1+ AAReps)*(4n2 + 2n^2+7b n -2maxn(maxn+b)+ (51/4)b^2+32b+116);
prep2dag = (1+2 AAReps)*(12n2 + 74n+4n^2+6n bpl + 6n bexp + 3 brot + 8); 
prep1dag = 11Z + 3*Ceiling[Log2[9Z]]+2b\[Alpha]l +bkeep -8;
prepeldag=0;
prepiondag= 4Z+Ceiling[Log2[Z]](bZ-3)-2;
{{prepel, prepion, loadspecies, prep1, prep2, prepG, USPG, IneqG, USPP, prepP, IneqP, prepPdag, USPPdag, nuclmomentum,IneqGdag, USPGdag,prepGdag,prep2dag,prep1dag, prepiondag, prepeldag}, prepel+prepion+loadspecies+prep1+ prep2+prepG+USPG+IneqG+USPP+prepP+IneqP+prepPdag+USPPdag+nuclmomentum+IneqGdag+USPGdag+prepGdag+prep2dag+prep1dag+prepiondag+prepeldag}
];

TotalCost[\[Eta]e_, \[Eta]I_,Z_,nx_, ny_, nz_, Nx_, Ny_, Nz_]:= {{sharedCost[\[Eta]e,\[Eta]I,nx,ny,nz, Nx, Ny, Nz][[2]],
TCost[\[Eta]e,\[Eta]I, Nx, Ny, Nz][[2]],VCost[\[Eta]e,\[Eta]I,nx,ny,nz, Nx, Ny, Nz][[2]],locCost[\[Eta]e,\[Eta]I,Z,nx,ny,nz, Nx, Ny, Nz][[2]],nlCost[\[Eta]I,Z,nx,ny,nz, Nx, Ny, Nz][[2]]},
sharedCost[\[Eta]e,\[Eta]I,nx,ny,nz, Nx, Ny, Nz][[2]]+TCost[\[Eta]e,\[Eta]I, Nx, Ny, Nz][[2]]+VCost[\[Eta]e,\[Eta]I,nx,ny,nz, Nx,Ny, Nz][[2]]
+locCost[\[Eta]e,\[Eta]I,Z,nx,ny,nz, Nx, Ny, Nz][[2]]+nlCost[\[Eta]I,Z,nx,ny,nz, Nx, Ny, Nz][[2]]};

End[];
EndPackage[];
