# ======================================================================
# Eworx S.A.- 2012
# Author kp@eworx.gr
# ======================================================================
# Load available visualization functions

visualization.functions.location <- "visualizationFunctions"

# ======================================================================
# visualization functions depedencies


# ======================================================================

visualization.functions.files <- list.files(path = paste(project.location, visualization.functions.location, sep = ""), pattern = "*.R")

for(visualization.functions.file in visualization.functions.files){
  print(paste("    -", visualization.functions.file))
  source(paste(project.location, visualization.functions.location, "/", visualization.functions.file, sep = ""))
}

# ======================================================================

rm(visualization.functions.location)
rm(visualization.functions.files)
rm(visualization.functions.file)

# ======================================================================
