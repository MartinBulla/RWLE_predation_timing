## Timing of predation in red-wattled lapwings *Vanellus indicus*

by [Martin Sládeček](slava.laguna.os@volny.cz) and [Martin Bulla](bulla.mar@gmail.com)

### **Overview**

Data, codes and outputs of the analyses in [Diel timing of nest predation changes across breeding season in a subtropical shorebird](https://doi.org/10.22541/au.160991969.96235686/v1)

### **Folders and files**

[Data](Data/): all data used in the analyses
- READ_ME.txt - column definitions for each of the four datasets
- nests_data.txt - information on each nest used in the paper
- logger_data.txt - information about each logger placed on each nest
- temperatures.txt - ground temperature measurements based on all ground temperature recordings (next to the nests) from the given hour in the whole study area.
- potential_predators_ebird.txt - bird observations in the Dubai area acquired from [eBird 2020](https://ebird.org)

[R](R/): all r-scripts used in the analyses
- tools.R loads functions and packages used in the other R-scripts (needs to be always loaded before running the other scripts)
- prepare_data.R prepares the main datasets (but only after tools.R is loaded)
- prepare_logger_data_intersections.R generates periods, for which each nest was continuously recorded with one or more devices (to run, need tools.R loaded)
- Out_Text.R script generates written outputs (e.g sample sizes, estimates) for the text - Abstract, Introduction, Method, and Results
- Fig scripts generate Figures
- Table scripts generate Tables

[Output](Output/): contains outputs of the Fig and Table scripts (i.e. Figs and Tables for the main text, as well as plots of model assumptions for each model from this manuscript) and DPR_sim.Rdata file containing simulation that generate the exact results for the main DPR finding presented within the text of the manuscript

RWLA_predation_timing.sublime-project - sublime project file
