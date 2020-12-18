## Timing of predation in red-wattled lapwings *Vanellus indicus*

by Martin Sládeček and Martin Bulla

### **Overview**

Data, codes and outputs of the analyses.  

### **Folders and files**

[Data](Data/): 
- READ_ME.txt - column definitions for each of the three datasets
- nests_data.txt - information on each nest used in the paper
- logger_data.txt - information about each logger placed on each nest
- temperatures.txt - ground temperature measurements based on all ground temperature recordings (next to the nests) from the given hour in the whole study area.

[R](R/): all r-scripts used in the analyses
- tools.R loads functions and packages used in the other R-scripts (needs to be always loaded before running the other scripts)
- prepare_data.R prepares the main datasets (but only after tools.R is loaded)
- prepare_logger_overlaps.R generates periods, for which each nest was continuously recorded with one or more devices 
- Out_Text scripts generates written outputs (e.g sample sizes, estimates) for the text - Abstract, Introduction, Method, and Results
- Fig scripts generate Figures
- Table scripts generate Tables

[Output](Output/): contains outputs of the Fig and Table scripts (i.e. Figs and Tables for the main text, as well as plots of model assumptions for each model from this manuscript).