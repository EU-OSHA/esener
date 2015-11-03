# ======================================================================
# Eworx S.A.- 2012
# Author kp@eworx.gr
# ======================================================================
# Shared Properties

plots.location <- paste(project.location, "plots/", sep = "")
plots.csv.location <- paste(project.location, "plots/xls/", sep = "")
surveys.location <-  paste(project.location, "../import/surveys/", sep = "")
countries.code.name.status.file.name <- "defaultData/countries.code.name.xls"

#php.command <- "/opt/lampp/bin/php"
#php.command <- "/usr/lib/cgi-bin/php5 "
php.command <- "/usr/bin/php"

shouldOptimizePNG <- TRUE

# ======================================================================
# shared properties with the import convertor process

step1.input.file.name  <- "analysed.columns.xls"
step3.values.input.file.name.pattern <- "translated.values.*.xls"
step3.questions.input.file.name.pattern <- "translated.questions.*.xls"
time.series.input.file.name <- "timeSeries.xls"

# ======================================================================
