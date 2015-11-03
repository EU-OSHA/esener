# ======================================================================
# Eworx S.A.- 2012
# Author kp@eworx.gr
# ======================================================================
# Contains the functions used by step three localization step of the process
# ======================================================================

generateValuesTranslations <- function(survey, fileNameToAppend = step2.values.output.file.name){

  # Creates an xls containing all distinct values per answers, subsets and subset values
  #
  # Args:
  #   survey: the id name of the survey  c("EntryType", "Column", "Value", "TranslatedValue")
  #

  print("------------------------------------------")
  print(paste(" * generateValuesTranslations :", survey, " - STARTED"))
  function.starting.time <- proc.time()[3]
  print("------------------------------------------")

  output.file <- paste(getSurveyFolder(survey), survey, ".", fileNameToAppend, sep = "")

  if(file.exists(paste(getSurveyFolder(survey), survey.refined.data.file.name, sep = ""))){

    #--------------------------------------
    print(paste(" | generateValuesTranslations :", survey, " - Loading required information"))
    function.block.starting.time <- proc.time()[3]
    #--------------------------------------

    survey.refined.data <- loadRefinedSurvey(survey)
    columns <- names(survey.refined.data)

    column.types <- readXls(paste(getSurveyFolder(survey), survey, ".", step1.input.file.name, sep = ""))
    column.types <- sqldf("select * from 'column.types' where Type in ('Question', 'Subset', 'QuestionMean')")

    survey.data.columns <- names(survey.refined.data)

    column.values.to.translate <- data.frame();

    #--------------------------------------
    print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
    #--------------------------------------
    #--------------------------------------
    print(paste(" | generateValuesTranslations :", survey, " - List translatable values (Questions Answers)"))
    function.block.starting.time <- proc.time()[3]
    #--------------------------------------

    column.type <- "Question"
    columns <- sqldf(paste( "select * from 'column.types' where Type == '", column.type, "' order by Column", sep = ""))
    columns <- columns$Column

    for(column in columns){
      distinct.column.values <- as.vector(unique(survey.refined.data[[column]]))
      distinct.column.values <- distinct.column.values[!distinct.column.values == "DELETE"]

      temp.table <- data.frame(distinct.column.values)
      colnames(temp.table)[1] <- "Value"

      temp.table[, "TranslatedValue"] <- temp.table$Value
      temp.table[, "EntryType"] <- "Answer"
      temp.table[, "Column"] <- column

      temp.table <- temp.table[valid.values.localization.columns]

      if(length(column.values.to.translate[1,]) == 0){
        column.values.to.translate <- temp.table
      }else{
        column.values.to.translate <- merge(temp.table, column.values.to.translate, all = TRUE)
      }
    }

    #--------------------------------------
    print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
    #--------------------------------------


    #--------------------------------------
    print(paste(" | generateValuesTranslations :", survey, " - List Mean value for Questions Mean to be translated"))
    function.block.starting.time <- proc.time()[3]
    #--------------------------------------


    column.type <- "QuestionMean"
    columns <- sqldf(paste( "select * from 'column.types' where Type == '", column.type, "' order by Column", sep = ""))
    columns <- columns$Column

    for(column in columns){
      distinct.column.values <- c("Mean")
      temp.table <- data.frame(distinct.column.values)
      colnames(temp.table)[1] <- "Value"

      temp.table[, "TranslatedValue"] <- temp.table$Value
      temp.table[, "EntryType"] <- "Answer"
      temp.table[, "Column"] <- column

      temp.table <- temp.table[valid.values.localization.columns]

      if(length(column.values.to.translate[1,]) == 0){
        column.values.to.translate <- temp.table
      }else{
        column.values.to.translate <- merge(temp.table, column.values.to.translate, all = TRUE)
      }
    }

    #--------------------------------------
    print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
    #--------------------------------------

    #--------------------------------------
    print(paste(" | generateValuesTranslations :", survey, " - List translatable values (Subset Values)"))
    function.block.starting.time <- proc.time()[3]
    #--------------------------------------

    column.type <- "Subset"
    columns <- sqldf(paste( "select * from 'column.types' where Type == '", column.type, "' order by Column" , sep = ""))
    columns <- columns$Column

    for(column in columns){
      distinct.column.values <- as.vector(unique(survey.refined.data[[column]]))
      distinct.column.values <- distinct.column.values[!distinct.column.values == "DELETE"]

      temp.table <- data.frame(distinct.column.values)
      colnames(temp.table)[1] <- "Value"

      temp.table[, "TranslatedValue"] <- temp.table$Value
      temp.table[, "EntryType"] <- "SubsetValue"
      temp.table[, "Column"] <- column

      temp.table <- temp.table[valid.values.localization.columns]
      temp.table <- sqldf(paste("select 'SubsetValue' EntryType, '", column, "' Column, 'All' Value, 'All' TranslatedValue union all select * from 'temp.table'", sep  = ""))

      column.values.to.translate <- merge(temp.table, column.values.to.translate, all = TRUE)
    }

    #--------------------------------------
    print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
    #--------------------------------------

    #--------------------------------------
    print(paste(" | generateValuesTranslations :", survey, " - List translatable values (Subsets)"))
    function.block.starting.time <- proc.time()[3]
    #--------------------------------------

    temp.table <- data.frame(columns)
    colnames(temp.table)[1] <- "Value"

    temp.table[, "TranslatedValue"] <- temp.table$Value
    temp.table[, "EntryType"] <- "Subset"
    temp.table[, "Column"] <- temp.table$Value
    temp.table <- temp.table[valid.values.localization.columns]

    column.values.to.translate <- merge(temp.table, column.values.to.translate, all = TRUE)

    column.values.to.translate <- sqldf("select * from 'column.values.to.translate' order by EntryType, Column, CASE WHEN Value = 'All' THEN 0 END desc")
    #--------------------------------------
    print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
    #--------------------------------------

    #--------------------------------------
    print(paste(" | generateValuesTranslations :", survey, " - Add DVT Country Code translatable keys and default values to be translated"))
    function.block.starting.time <- proc.time()[3]
    #--------------------------------------

		countries.key.localizations <- sqldf("select '_Selector' EntryType, 'country' Column, CountryCode Value, CountryName TranslatedValue from  'countries.code.name' where translatedValue <>'UK'")
		column.values.to.translate <- merge(column.values.to.translate, countries.key.localizations, all = TRUE)

		print(paste(" | generateValuesTranslations :", survey, " - Add DVT Survey key value to be translated"))
		column.values.to.translate <- merge(column.values.to.translate, sqldf(paste("select '_DVT' EntryType, 'survey' Column, '", survey, "' Value, '", survey, "' TranslatedValue", sep = "")), all = TRUE)
		
		print(paste(" | generateValuesTranslations :", survey, " - Add DVT related keys and default values to be translated"))
		column.values.to.translate <- merge(column.values.to.translate, loadDVTvaluesToTranslate(), all = TRUE)

		print(paste(" | generateValuesTranslations :", survey, " - Add DVT Calculate and set EU Maps visualizations count"))
		column.values.to.translate <- merge(column.values.to.translate, sqldf(paste("select '_DVT' EntryType, 'visualizationsCount' Column, 'heatMap' Value, '", heatMapCountVisualizations(survey) , "' TranslatedValue", sep = "")), all = TRUE)

		print(paste(" | generateValuesTranslations :", survey, " - Add DVT Calculate and set EU Bars visualizations count"))
		column.values.to.translate <- merge(column.values.to.translate, sqldf(paste("select '_DVT' EntryType, 'visualizationsCount' Column, 'euBars' Value, '", euBarsCountVisualizations(survey) , "' TranslatedValue", sep = "")), all = TRUE)

		print(paste(" | generateValuesTranslations :", survey, " - Add DVT Calculate and set In Country visualizations count"))
		column.values.to.translate <- merge(column.values.to.translate, sqldf(paste("select '_DVT' EntryType, 'visualizationsCount' Column, 'inCountry' Value, '", inCountryCountVisualizations(survey) , "' TranslatedValue", sep = "")), all = TRUE)

		print(paste(" | generateValuesTranslations :", survey, " - Add DVT Calculate and set Cross country visualizations count"))
		column.values.to.translate <- merge(column.values.to.translate, sqldf(paste("select '_DVT' EntryType, 'visualizationsCount' Column, 'crossCountry' Value, '", crossCountryCountVisualizations(survey) , "' TranslatedValue", sep = "")), all = TRUE)

		print(paste(" | generateValuesTranslations :", survey, " - Add DVS related keys and default values to be translated"))
		column.values.to.translate <- merge(column.values.to.translate, loadDVSvaluesToTranslate(), all = TRUE) 

    #--------------------------------------
    print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
    #--------------------------------------
    #--------------------------------------
    print(paste(" | generateValuesTranslations :", survey, " - Save translatable values : ", output.file))
    function.block.starting.time <- proc.time()[3]
    #--------------------------------------

    writeXls(column.values.to.translate, output.file)

    #--------------------------------------
    print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
    #--------------------------------------

  }else{
    print("Refined dataset does not exist")
  }

  #--------------------------------------
  print("------------------------------------------")
  print(paste(" * generateValuesTranslations :", survey, " - | DONE"))
  print(paste(" *  Time Needed :", (proc.time()[3] - function.starting.time), "seconds"))
  print("------------------------------------------")
  #--------------------------------------

}

#generateValuesTranslations("3RDEQLS")

generateQuestionsTranslations <- function(survey, fileNameToAppend = step2.questions.output.file.name){

  # Creates an xls containing all question column names to be translated. Web version, Full version, note
  #
  # Args:
  #   survey: the id name of the survey
  #

  print("------------------------------------------")
  print(paste(" * generateQuestionsTranslations :", survey, " - STARTED"))
  function.starting.time <- proc.time()[3]
  print("------------------------------------------")

  ouput.file <- paste(getSurveyFolder(survey), survey, ".", fileNameToAppend, sep = "")

  if(file.exists(paste(getSurveyFolder(survey), survey.refined.data.file.name, sep = ""))){

    #--------------------------------------
    print(paste(" | generateQuestionsTranslations :", survey, " - Loading required information"))
    function.block.starting.time <- proc.time()[3]
    #--------------------------------------

    column.types <- readXls(paste(getSurveyFolder(survey), survey, ".", step1.input.file.name, sep = ""))
    column.types <- sqldf("select * from 'column.types' where Type in ('Question', 'QuestionMean') order by Column")
    column.types <-  as.vector(column.types$Column)

    #--------------------------------------
    print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
    #--------------------------------------
    #--------------------------------------
    print(paste(" | generateQuestionsTranslations :", survey, " - List questions codes to translate and Save : ", ouput.file))
    function.block.starting.time <- proc.time()[3]
    #--------------------------------------

    questions.to.translate <- data.frame(column.types);
    colnames(questions.to.translate)[1] <- "QuestionCode"
    questions.to.translate[, "Category"] <- "Unknown"
    questions.to.translate[, "WebTranslation"] <- questions.to.translate$QuestionCode
    questions.to.translate[, "FullTranslation"] <- questions.to.translate$QuestionCode
    questions.to.translate[, "Note"] <- ""

    writeXls(questions.to.translate, ouput.file)

    #--------------------------------------
    print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
    #--------------------------------------

  }else{
    print("Refined dataset does not exist")
  }
  #--------------------------------------
  print("------------------------------------------")
  print(paste(" * generateQuestionsTranslations :", survey, " - | DONE"))
  print(paste(" *  Time Needed :", (proc.time()[3] - function.starting.time), "seconds"))
  print("------------------------------------------")
  #--------------------------------------
}

#generateQuestionsTranslations("3RDEQLS")
