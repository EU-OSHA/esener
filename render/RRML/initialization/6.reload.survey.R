# ======================================================================
# Eworx S.A.- 2012
# Author kp@eworx.gr
# ======================================================================
# Load survey data
# ======================================================================

loadDataFrame <- function(file){
  load(file)
  return(data)
}

# ======================================================================

loadSurveyDataIntoWorkspace <- function(survey){
	survey.data.location <- paste(surveys.location, survey, "/Survey", survey, ".RData" , sep = "")
	if( file.exists(survey.data.location) ){
		  r.statement <- paste(
		  	"Survey", survey ," <- ",
		  	"loadDataFrame(paste(surveys.location, \"", survey, "\", \"/Survey\", \"", survey, "\", \".RData\" , sep = \"\"))"
		  	, sep = "")
		  eval(parse(text = r.statement), envir = parent.frame())
		  save.image()
	}else{
		print("Survey does not exist")
	}
}

#loadSurveyDataIntoWorkspace("3RDEQLS")

