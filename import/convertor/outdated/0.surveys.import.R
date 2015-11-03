# ======================================================================
# Eworx S.A.- 2012
# Author kp@eworx.gr
# ======================================================================

# rm(list=ls()) # clear memory
# source("/web-pub/foundation/html/DVS/import/convertor/0.surveys.import.R") # load this script
# loads all defined sources

# ======================================================================

dvs.folder <- "/web-pub/foundation/html/DVS/"
converter.base.path <- paste(dvs.folder, "import/surveys/", sep = "")
converter.scripts.path <- paste(dvs.folder, "import/convertor/", sep = "")
RRML.validation.layer <- paste(dvs.folder, "render/VL/", sep = "")
DVT.folder <- paste(dvs.folder, "DVT/", sep = "")
DVT.model.folder <- paste(DVT.folder, "model/", sep = "")

# ======================================================================

source.blocks <- c(
  "1.properties.R", "Loading  - Properties",
  "2.libraries.R", "Loading  - R Libraries",
  "3.base.functions.R", "Loading  - Base Functions",
  "4.step.one.functions.R", "Loading  - Step one functions",
  "5.step.two.functions.R", "Loading  - Step two functions",
  "6.refined.data.to.statistics.R", "Loading  - Survey To Statistics functions",
  "7.step.three.localization.functions.R", "Loading - Step three localization functions",
  "8.step.three.translations.R", "Loading - Step three translations functions",
  "9.update.dvs.validation.layer.R", "Loading - DVS validation layer update functions",
  "10.update.dvt.R", "Loading - DVT survey model update functions"
)

print("------------------------------------------------------------")

for(source.index in seq(1, length(source.blocks), 2)){
    sourceFile <- source.blocks[source.index]
    print(paste(" |", source.blocks[source.index + 1], ":", "(source:", sourceFile, ")"))
    source.block.starting.time <- proc.time()[3]
    source(paste(converter.scripts.path, sourceFile, sep = ""))
    print(paste(" |  - Time Needed :", proc.time()[3] - source.block.starting.time, "seconds"))
    print("------------------------------------------------------------")
}

rm(source.index)
rm(sourceFile)
rm(source.blocks)
rm(source.block.starting.time)

# ======================================================================
