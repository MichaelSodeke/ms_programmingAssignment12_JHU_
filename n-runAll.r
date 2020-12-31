# [n-runAll.r]

## -------------------------- activate script ---------------------------------------------------------
# run 'source(dir()[6])' or source("n-runAll.r")

message("\n\n | running all scripts")
message(" -----------------------------------------------------------------")
##-create a function that pauses R script till key is pressed                                                                                                                                                                                                                                                                                                            
Mstr.pause <- function()
{                                                                                                                                                                                                                                                                                                            
	message("\n | <<< current file: [n-runAll.r]")
	line <- readline(prompt = "\n\t Press [enter] to continue...")                                                                                                                                                                                                                                                                  
}

# ------------------------------------------- PART-0 --------------------------------------------------
# [0-loadPackages.r]
source("0-loadPackages.r")
Mstr.pause()

# ------------------------------------------- PART-1 --------------------------------------------------
# [1-C1_dataEng.r]
source("1-C1_dataEng.r")
Mstr.pause()

# ------------------------------------------- PART-2 --------------------------------------------------
# [2-C1_descriptiveStats.r]
source("2-C1_descriptiveStats.r")
Mstr.pause()

# ------------------------------------------- PART-3 --------------------------------------------------
# [3-C1_predStudyDesign.r]
source("3-C1_predStudyDesign.r")
Mstr.pause()

# ------------------------------------------- PART-4 --------------------------------------------------
# [4-C2_prediction.r]
source("4-C2_prediction.r")
Mstr.pause()

# ------------------------------------------- PART-n --------------------------------------------------
message("\n\n\n -----------------------------------------------------------")
message(" Task complete...")
message(" Removing R objects from memory...")
message(" -----------------------------------------------------------")
#clear_stash()
rm(list = ls())