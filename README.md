**Example Usage**:

devtools::install_github("mywijaya/ACTpackage")

library(Rpack)

your_wd <- "D:/odesk/upwork 2023/6. Islah Project - GSK ACT/Final R code" # directory of ADAM and CRF files 

func_tfl_act(directory = your_wd,
             output_file = "TFL.docx",
             params = list(param1 = "value1"))
