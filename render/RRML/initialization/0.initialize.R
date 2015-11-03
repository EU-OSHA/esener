# ======================================================================
# Eworx S.A.- 2012
# Author kp@eworx.gr
# ======================================================================
# Initialize an R instance
# source("/web-pub/foundation/html/DVS/render/RRML/initialization/0.initialize.R")

#r.instance.screen.name = "r.instance.1" # To be suplied dynamically

print("------------------------------------------------------------")
print(paste("Initializing R instance:", r.instance.screen.name))
print("------------------------------------------------------------")
starting.time <- proc.time()[3]

# ======================================================================

dvs.folder <- "/web-pub/foundation/html/DVS/"
converter.base.path <- paste(dvs.folder, "import/surveys/", sep = "")

project.location	<- paste(dvs.folder, "render/", sep ="")
RRML.location		<- paste(project.location, "RRML/", sep ="")
RRML.location.svg.o <- paste(RRML.location, "SVGOptimizer/", sep ="")
RRML.db.location	<- paste(RRML.location, "db/", sep ="")
RRML.db.api.php		<- "RRML.php"
scripts.location	<- paste(RRML.location, "initialization/", sep ="")

# ======================================================================

source.blocks <- c(

	"1.properties.R", "Loading  - Properties",
	"2.libraries.R", "Loading - Libraries",
	"3.base.functions.R", "Loading - Base Functions and Translator API",
	"4.visualizations.themes.R", "Loading Visualizations Themes",
	"5.visualizations.load.R", "Loading - Visualization Functions",
	"6.reload.survey.R", "Loading Reload survey statistics data Functions",
	"7.db.status.change.R", "Loading Database statuses change functions"

)

# ======================================================================

for(source.index in seq(1, length(source.blocks), 2)){
	sourceFile <- source.blocks[source.index]
	print(paste(" |", source.blocks[source.index + 1], ":", "(source:", sourceFile, ")"))
	source.block.starting.time <- proc.time()[3]
	source(paste(scripts.location, sourceFile, sep = ""))
	print(paste(" |  - Time Needed :", proc.time()[3] - source.block.starting.time, "seconds"))
	print("------------------------------------------------------------")
}

print("------------------------------------------------------------")
print("Initialization : completed")
print(paste(" |  - Total Time Needed :", proc.time()[3] - starting.time, "seconds"))
print("------------------------------------------------------------")

# ======================================================================

rm(source.blocks)
rm(source.block.starting.time)
rm(starting.time)
rm(source.index)
#rm(scripts.location)
rm(sourceFile)

print("------------------------------------------------------------")
print("Available variables :")
print("------------------------------------------------------------")
print(ls())
print("------------------------------------------------------------")

# ======================================================================

