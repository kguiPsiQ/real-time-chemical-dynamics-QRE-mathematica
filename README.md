# real-time-chemical-dynamics-QRE-mathematica


This folder contains all necessary Mathematica files to obtain all the number in Table 1, and generate Fig. 2, Fig. 6, and Fig 7 in the paper.

If you want to reproduce all these results, clone this entire repo, navigate to the `paper_instance_analysis.nb` file, and open it using Mathematica, which should look something like this:

<img width="535" height="365" alt="Screenshot 2025-08-07 at 11 29 32 AM" src="https://github.com/user-attachments/assets/d254515b-7c34-44a2-b20a-6bbaea223824" />

Then you can trying running all the Mathematica blocks in sequence. We first set up some kernels to help you run things in parallel, then load all the needed packages as illustrated below, execute all the quantum resource estimation for different chemical instances, and lastly plot all the QRE results together accross all the instances. We also offer a detailed walkthrough with important comments in `paper_instance_analysis.nb`.

-----------------------------------------------------

The files are organized as follows:

<img width="570" height="383" alt="Screenshot 2025-08-07 at 11 02 08 AM" src="https://github.com/user-attachments/assets/5fb90d1c-6155-470e-8759-a110e5fe1cfc" />

The root level notebook, `paper_instance_analysis.nb`, first do all the necessary steps for getting the required numbers for table 1 and all the figures. Then it generates the 3 figures.

`paper_instance_analysis.nb` calls on `basisGeneration.wl` which handles all the plane wave generations, `costAnalysis.wl` which computes all the quantum resource costs given a chemical instance, and `simulationCell.wl` which sets the simulation cell sizes.

`costAnalysis.wl` then calls on `preprocessing.wl` which preprocess some of the required precalculation need for the block encoding, `rescalingFactor.wl` which computes the rescaling factor alpha of different Hamiltonian instances, and `compilationCost.wl` which computes the Toffoli gates need for block encoding H.

Lastly, `preprocessing.wl` calls on `HGHdata.wl` to write the pseudopotentials.
