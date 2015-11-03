# ======================================================================
# Eworx S.A. - 2012 / 2013
# Author kp@eworx.gr
# ======================================================================
# Contains the functions to list and validate available localizations
# ======================================================================

listAvailableTranslations <- function(survey){

  result <- isAvailableTranslationsValid(survey)

  if(result != "Yes"){
    print(paste("Please note:", result))
  }

  survey.folder <- getSurveyFolder(survey)
  values.localizations.pattern <- paste(survey.folder, survey, ".", step3.values.input.file.name.pattern, sep = "" )
  values.localizations.files <- Sys.glob(values.localizations.pattern)
  values.localizations <- retainWildCharPart(values.localizations.files, values.localizations.pattern)

  questions.localizations.pattern <- paste(survey.folder, survey, ".", step3.questions.input.file.name.pattern, sep = "" )
  questions.localizations.files <- Sys.glob(values.localizations.pattern)
  questions.localizations <- retainWildCharPart(questions.localizations.files, values.localizations.pattern)

  return(intersect(values.localizations, questions.localizations))
}

#listAvailableTranslations("3RDEQLS")

# ======================================================================

isAvailableTranslationsValid <- function(survey){
  # validates all provided localization files if they are valid
  #
  # Args:
  #   survey : the survey Id
  #
  # Returns:
  #   Yes or the problem

  result <- "Yes"
  survey.folder <- getSurveyFolder(survey)
  values.localizations.pattern <- paste(survey.folder, survey, ".", step3.values.input.file.name.pattern, sep = "" )
  values.localizations.files <- Sys.glob(values.localizations.pattern)

  result <- isValuesLocalizationfilesValid(values.localizations.files)

  if(result != "Yes"){
    return(result)
  }

  questions.localizations.pattern <- paste(survey.folder, survey, ".", step3.questions.input.file.name.pattern, sep = "" )
  questions.localizations.files <- Sys.glob(questions.localizations.pattern)

  result <- isQuestionsLocalizationfilesValid(questions.localizations.files)
  if(result != "Yes"){
    return(result)
  }

  return("Yes")
}

#isAvailableTranslationsValid("3RDEQLS")

# ======================================================================
# ======================================================================

isValuesLocalizationfilesValid <- function(localization.files){
  # validates all provided localization files if they are valid
  #
  # Args:
  #   localization.files: the Values localization files
  #
  # Returns:
  #   Yes or the problem

  result <- "Yes"
  for(localization.file in localization.files){
    result <- isValuesLocalizationfileValid(localization.file)
    if(result != "Yes"){
      return(paste(localization.file, "Invalid! - ", result))
    }
  }
  return(result)
}

# ------------------------------------------------------------------------

isValuesLocalizationfileValid <- function(localization.file){
  # validates the provided localization file if it is valid
  #
  # Args:
  #   localization.file: the Values localization file
  #
  # Returns:
  #   Yes or the problem

   values <- readXls(localization.file)
   values.columns <- names(values)

   intersection <- intersect(values.columns, valid.values.localization.columns)

   # are the provided column types valid?
   if(length(intersection) != length(valid.values.localization.columns)){
     return (
       paste("Detected a Column anomaly: ",
             paste(setdiff(values.columns, intersection), collapse = ", ")
       )
     )
   }
   #######
   # here to improove the functionality we should read the EN local, read the XX local and the XX local should have all entries as the EN locale.
   #######

   #for(column in values.columns ){
   #  if(! (length(which(values[[column]] == "" & )) == 0) ){
   #    return(paste("Column", column, "contains at least one empty string. Please check row : ", which(values[[column]] == "")))
   #  }
   #}

   return("Yes")
}


crossValidateXLSRows <- function(file.A, file.B, columns = c("EntryType","Column", "Value")){
# generic function to evaluate that all entries of A exist for B for a desired number of columns

	A <- readXls(file.A)
	B <- readXls(file.B)

	A <- A[, c(columns)]
	B <- B[, c(columns)]

	A[,"ID"] <- do.call("paste", c(A[columns], sep = "/"))
	B[,"ID"] <- do.call("paste", c(B[columns], sep = "/"))

	A[,"AAA"] <- 1:nrow(A)
	B[,"AAB"] <- 1:nrow(B)

	A <- A[, c("ID", "AAA")]
	B <- B[, c("ID", "AAB")]

	result <- sqldf("select * from 'A' left outer join 'B' on 'A'.ID='B'.ID where 'B'.ID is NULL")
	print(paste(nrow(result) ,": results out of sync ") )
	result <- result[,"ID"]
	print(result)

}

if(FALSE)
	crossValidateXLSRows("/web-pub/foundation/html/DVS/import/surveys/3RDEQLS/3RDEQLS.translated.values.EN.xls", "/web-pub/foundation/html/DVS/import/surveys/3RDEQLS/3RDEQLS.translated.values.EL.xls")

# ======================================================================

isQuestionsLocalizationfilesValid <- function(localization.files){
  # validates all provided localization files if they are valid
  #
  # Args:
  #   localization.files: the Questions localization files
  #
  # Returns:
  #   Yes or the problem

  result <- "Yes"
  for(localization.file in localization.files){
    result <- isQuestionsLocalizationfileValid(localization.file)
    if(result != "Yes"){
      return(paste(localization.file, "Invalid! - ", result))
    }
  }
  return(result)
}

# isQuestionsLocalizationfilesValid("/web-pub/foundation/html/DVS/import/surveys/data/3RDEQLS/3RDEQLS.translated.questions.EN.xls")

# ------------------------------------------------------------------------

isQuestionsLocalizationfileValid <- function(localization.file){
  # validates the provided localization file if it is valid
  #
  # Args:
  #   localization.file: the Questions localization file
  #
  # Returns:
  #   Yes or the problem

  values <- readXls(localization.file)
  values.columns <- names(values)

  intersection <- intersect(values.columns, valid.questions.localization.columns)

  # are the provided column types valid?
  if(length(intersection) != length(valid.questions.localization.columns)){
    return (
      paste("Detected a Column anomaly: ",
            paste(setdiff(values.columns, intersection), collapse = ", ")
      )
    )
  }

  for(column in values.columns ){
    if(!(column %in% c("Note", "FullTranslation"))){
      if(! (length(which(values[[column]] == "")) == 0) ){
        return(paste("Column", column, "contains at least one empty string. Please check row :", which(values[[column]] == "")))
      }
    }
  }

  return("Yes")
}

# ======================================================================

sortQuestionsLocalizationfile <- function(localization.file){
  # sorts the values of a localization file
  #
  # Args:
  #   localization.file: the Questions localization file
  #
  # Returns:
  #   Yes or the problem

  #--------------------------------------
  print(paste(" | sortQuestionsLocalizationfile : -  Sort Questions Localization file"))
  function.block.starting.time <- proc.time()[3]
  #--------------------------------------

  if(isQuestionsLocalizationfileValid(localization.file)!="Yes")
  	return("Not valid Data");

  values <- readXls(localization.file)
	values <- smartSoundOrderingBy(values, c("Category", "QuestionCode"))

	writeXls(values, localization.file)

	#--------------------------------------
	print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

}

#sortQuestionsLocalizationfile("/web-pub/foundation/html/DVS/import/surveys/3RDEQLS/3RDEQLS.translated.questions.EN.xls")

sortValuesLocalizationfile <- function(localization.file){
  # sorts the values of a localization file
  #
  # Args:
  #   localization.file: the Questions localization file
  #
  # Returns:
  #   Yes or the problem

  #--------------------------------------
  print(paste(" | sortValuesLocalizationfile : -  Sort Values Localization file"))
  function.block.starting.time <- proc.time()[3]
  #--------------------------------------


  values <- readXls(localization.file)
  values <- smartSoundOrderingBy(values, c("EntryType", "Column", "Value",  "TranslatedValue"))

  writeXls(values, localization.file)

  #--------------------------------------
  print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
  #--------------------------------------

}

# ======================================================================

retainWildCharPart <- function(vector.to.filter, pattern.with.wildchar){
  # retains the wildchar part from every vector element given the pattern.with.wildchar
  #
  # Args:
  #   vector.to.filter: the vector to filter
  #   pattern.with.wildchar: the pattern with a wildchar
  #
  # Returns:
  #   the filter vector

  library(stringr)

  pattern.with.wildchar <- gsub(pattern = "\\.", replacement = "\\\\.",  pattern.with.wildchar)
  values.to.remove <- unlist(strsplit(pattern.with.wildchar, split = "*", fixed = TRUE))

  for(vector.to.filter.index in 1:length(vector.to.filter)){
    temp <- vector.to.filter[vector.to.filter.index]
    for(value.to.remove in values.to.remove){
      temp <- gsub(value.to.remove, "", temp)
    }
    vector.to.filter[vector.to.filter.index] <- temp
  }
  return(vector.to.filter)
}

# ======================================================================
