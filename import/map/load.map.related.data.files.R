# ======================================================================
# Eworx S.A.- 2012
# Author kp@eworx.gr
# ======================================================================
# Loads the Map related data files into the session
# This is initialized once and variables are saved to the workspace
# ======================================================================
# source("/web-pub/foundation/html/DVS/import/map/load.map.related.data.files.R")

location <- "/web-pub/foundation/html/DVS/import/map/"

# ======================================================================

eu.map <- read.csv(paste(location, "european.map.v2.xls", sep = ""), sep = "\t")
eu.mask.map <- read.csv(paste(location, "european.mask.map.v2.xls", sep = ""), sep = "\t")

countries.centers <- read.csv(paste(location, "eu30.country.centers.xls", sep = ""), sep = "\t")
countries.centers.non.eu <- read.csv(paste(location, "non.eu30.country.centers.xls", sep = ""), sep = "\t")

# ======================================================================

rm(location)

# ======================================================================
