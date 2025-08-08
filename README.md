# real-time-chemical-dynamics-QRE-mathematica

## Summary
The main goal of this Mathematica code base is to easily (in a few lines) setup and perform fault-tolerant quantum resource estimates (QREs)
for a user-defined instance of an atomic-scale chemical dynamics problem as described in the manuscript [A comprehensive framework to simulate real-time chemical dynamcis on a fault-tolerant quantum computer](https://arxiv.org/abs/2504.06348). The target users are presumed to have basic chemistry/materials physics knowledge but importantly require *no knowledge of quantum algorithms* to perform QREs. Crucially, this code is meant to be chemistry-user-friendly and allow easy access to the numerical resource estimates in the manuscript without understanding the substantial algorithmic detail contained within. The `paper_instance_analysis.nb` notebook contains the setup and QRE of all problem instances analyzed in the manuscript. As a result, this notebook itself serves as a user-guide for how to setup and run QREs.

**Disclaimer**: While great care has been taken by the authors to ensure correctness, this is *research code* and the style/structure of the packages should be viewed in that light. We welcome comments and discussion.

## Structure

The files have dependency graph

<img width="512" height="430" alt="image" src="https://github.com/user-attachments/assets/cefdf3e0-1574-4465-adbe-c0addf1542ad" />


and perform the following functions:
- The root level notebook, `paper_instance_analysis.nb`, instantiates and performs QREs for all 7 instances (as per Table 1) in the manuscript, followed by generation of Figures 2, 6, 7 that combine all instance results together for easy visualization. This notebook serves as the user-guide.
- `HGHdata.wl` lists parameters for the [HGH pseudopotentials](https://journals.aps.org/prb/abstract/10.1103/PhysRevB.58.3641). Only a small subset of all HGH pseudopotentials are included in the file. However, additional pseudopotentials can be straightforwardly added following the format in the package.
- `preprocessing.wl` reads in the HGH parameters and compute numerical quantities that are relevant for the quantum algorithm construction.
- `rescalingFactor.wl` contains methods that compute rescaling factors for the Hamiltonian and its parts using preprocessed HGH information.
- `compilationCost.wl` contains methods that enumerate the Toffoli costs of the block-encoding circuit.
- `costAnalysis.wl` combines methods from `rescalingFactor.wl` and `compilationCost.wl`, along with information from `preprocessing.wl` to perform the full time-evolution cost analysis for a given a chemical instance.
- `simulationCell.wl` enumerates a set of methods to easily define common simulation cells (e.g. a rectangular cuboidal cell).
- `basisGeneration.wl` contains methods that construct the plane wave basis for a given simulation cell.

## Instructions
1. Clone the repo, navigate to the `paper_instance_analysis.nb` file, and open it using Mathematica. 

2. Run all the Mathematica notebook cells in sequence. A detailed walkthrough with important comments is provided within the notebook.


