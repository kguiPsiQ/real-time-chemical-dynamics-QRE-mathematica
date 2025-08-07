# real-time-chemical-dynamics-QRE-mathematica


This folder contains all necessary Mathematica files to obtain all the number in Table 1, and generate Fig. 2, Fig. 6, and Fig 7 in the paper.

<img width="570" height="383" alt="Screenshot 2025-08-07 at 11 02 08 AM" src="https://github.com/user-attachments/assets/5fb90d1c-6155-470e-8759-a110e5fe1cfc" />



The files are organized as follows:

The root level notebook, `paper_instance_analysis.nb`, first do all the necessary steps for getting the required numbers for table 1 and all the figures. Then it generates the 3 figures.

`paper_instance_analysis.nb` calls on `basisGeneration.wl` which handles all the plane wave generations, `costAnalysis.wl` which computes all the quantum resource costs given a chemical instance, and `simulationCell.wl` which sets the simulation cell sizes.

`costAnalysis.wl` then calls on `preprocessing.wl` which preprocess some of the required precalculation need for the block encoding, `rescalingFactor.wl` which computes the rescaling factor alpha of different Hamiltonian instances, and `compilationCost.wl` which computes the Toffoli gates need for block encoding H.

Lastly, `preprocessing.wl` calls on `HGHdata.wl` to write the pseudopotentials.
