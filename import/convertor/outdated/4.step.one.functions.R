# ======================================================================
# Eworx S.A.- 2012
# Author kp@eworx.gr
# ======================================================================
# Contains the functions used by step one of the process
# ======================================================================


#identifySPSSColumns <- function(survey){

  # Creates an xls containing all columns as rows so as to specify their type
  #
  # Args:
  #   survey: the id name of the survey  
  #
  # Returns:
  #   the column names
  
#  columns <- names(loadSPSSSurvey(survey))
#  columns <- data.frame(columns)
#  colnames(columns) <- c("Column")
#  columns[, "Type"] <- paste("", paste(valid.column.types, collapse ="|") )
#  writeXls(columns, paste(getSurveyFolder(survey), survey, ".", "identify.columns.xls", sep = "")) 
#  return(columns)
#}

# ======================================================================

#test <- identifySPSSColumns("3RDEQLS")

analyseSPSSColumns <- function(survey, fileNameToAppend = step0.output.file.name){
  
  # Creates an xls containing all columns as rows so as to specify their type and a text overview related to their distinct values to aiding a user. As this output is more user friendly it is used instead of identifySPSSColumns
  #
  # Args:
  #   survey: the id name of the survey  
  #

  print("------------------------------------------")
  print(paste(" * analyseSPSSColumns :", survey, " - STARTED"))  
  function.starting.time <- proc.time()[3]
  print("------------------------------------------")
  
  output.file.name <- paste(getSurveyFolder(survey), survey, ".", fileNameToAppend, sep = "")
  
  #--------------------------------------
  print(paste(" | analyseSPSSColumns :", survey, " - Loading SPSS data"))
  function.block.starting.time <- proc.time()[3]          
  #--------------------------------------
  
  survey.data <- loadSPSSSurvey(survey)
  columns <- names(survey.data)
  columns <- data.frame(columns)
  colnames(columns) <- c("Column")
  
  columns[, "Overview"] <- "number"
  
  #--------------------------------------
  print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
  #--------------------------------------
  
  #--------------------------------------
  print(paste(" | analyseSPSSColumns :", survey, " - Writing output xls file",  output.file.name))
  function.block.starting.time <- proc.time()[3]          
  #--------------------------------------
  
  for(column.name in columns$Column ){
    
    column.rows <- survey.data[[column.name]]
    uniqueValues <- unique(column.rows);
    uniqueValuesLength <- length(uniqueValues)
    
    column.analysis <- ""
    
    if(uniqueValuesLength < 13){
      column.analysis <- paste(uniqueValues, collapse = "|")
      column.analysis <- trimIfMoreThan(clearWhiteSpaces(column.analysis))
      column.analysis <- paste(uniqueValuesLength, "values :",  column.analysis)
    }else{
      first3Values <- paste(uniqueValues[1], uniqueValues[2], uniqueValues[3], sep ="|")
      first3Values <- trimIfMoreThan(clearWhiteSpaces(first3Values))
      column.analysis <- paste("> 13 values, count:", uniqueValuesLength, "class: ", class(column.rows), ". First three distinct values : ", first3Values)
    }
    
    columns[["Overview"]][which(columns["Column"] == column.name)] <- column.analysis
    
  }
  
  columns[, "Type"] <- paste("", paste(valid.column.types, collapse ="|") )
  
  
  writeXls(columns, output.file.name)
  
  #--------------------------------------
  print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
  #--------------------------------------
  
  #--------------------------------------
  print("------------------------------------------")
  print(paste(" * analyseSPSSColumns :", survey, " - | DONE"))  
  print(paste(" *  Time Needed :", (proc.time()[3] - function.starting.time), "seconds"))
  print("------------------------------------------")
  #--------------------------------------
}

# analyseSPSSColumns("3RDEQLS")

# ======================================================================

isIdentifiedSPSSColumnsValid <- function(survey){
  
  # Validation of the input file. Validates for: Valid Column Types, Question exists, Subset exists, Country_code or Country_Name exists, Country_code or Country_Name values are known.
  #
  # Args:
  #   survey: the id name of the survey  
  #
  # Returns:
  #   Yes, if it validates, or an error message.
    
  print("------------------------------------------")
  print(paste(" * isIdentifiedSPSSColumnsValid :", survey, " - STARTED"))  
  function.starting.time <- proc.time()[3]
  print("------------------------------------------")  

  #--------------------------------------
  print(paste(" | isIdentifiedSPSSColumnsValid : - Loading required information"))
  function.block.starting.time <- proc.time()[3]          
  #--------------------------------------
  
  identified.spss.column.types <- readXls(paste(getSurveyFolder(survey), survey, ".", step1.input.file.name, sep = ""))
  expected.columns = c("Column", "Type")
  identified.spss.column.types <- identified.spss.column.types[expected.columns]
  result <- "Yes"
  
  #--------------------------------------
  print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
  #--------------------------------------
  
  #--------------------------------------
  print(paste(" | isIdentifiedSPSSColumnsValid : - Validating Provided Identified Columns"))  
  function.block.starting.time <- proc.time()[3]          
  #--------------------------------------
  
  uniqueValues <- unique(identified.spss.column.types[["Type"]]);
  intersection <- intersect(uniqueValues, valid.column.types)
  
  # are the provided column types valid?
  if(length(intersection) != length(uniqueValues)){
    return (
      paste("Detected Uknown provided Column Types. Those are :", 
            paste(setdiff(uniqueValues, intersection), collapse = ", ")
      )
    )
  }
    
  # find the count per columns types  
  types.count <- sqldf("select type, count(type) as count from 'identified.spss.column.types' group by type")
  print(types.count);
  
  print(paste(" | isIdentifiedSPSSColumnsValid : -  At least one Question or QuestionMean"))
  
  # evaluate that the expected columns are valid
  # - Question exists
  test <- sqldf("select * from 'types.count' where Type = 'Question' or Type = 'QuestionMean' and count > 0")  
  if(length(test[["Type"]]) == 0){
    return("You have to define at least one column of type Question")
  }
  
  print(paste(" | isIdentifiedSPSSColumnsValid : -  At least one Question Subset"))
  
  # - Subset exists
  test <- sqldf("select * from 'types.count' where Type = 'Subset' and count > 0")
  if(length(test[["Type"]]) == 0){
    return("You have to define at least one column of type Subset")
  }
  
  print(paste(" | isIdentifiedSPSSColumnsValid : -  At least a CountryCode or a CountryName "))
  
  # - CountryCode or CountryName exists
  test <- sqldf("select * from 'types.count' where Type in ('CountryCode', 'CountryName') and count = 1")
  
  if(length(test[["Type"]]) == 0){
    result <- ("You have to define at least one column of type CountryCode or CountryName")
  }
  
  test <- as.character(test[["Type"]])
  
  #--------------------------------------
  print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
  #--------------------------------------
  
  #--------------------------------------
  print(paste(" | isIdentifiedSPSSColumnsValid : - Validating for unknown provided country related values"))
  function.block.starting.time <- proc.time()[3]          
  #--------------------------------------
  
  # validate country related values
  print(paste(" Loading SPSS Data... ", test[1], "column values."))
  survey.data <- loadSPSSSurvey(survey)
  
  if(test[1] == "CountryName"){
    # test if All countryNames are known    
    CountryName.column.name <- as.character(identified.spss.column.types[which(identified.spss.column.types[2] == "CountryName"), 1])
    uniqueValues <- unique( survey.data[[CountryName.column.name]] );    
    intersection <- intersect(uniqueValues, countries.code.name$CountryName)
    if(length(intersection) != length(uniqueValues)){
      return (
        paste("Detected Uknown Country Names. Those are :", 
              paste(setdiff(uniqueValues, intersection), collapse = ", ")
        )
      )
    }
  }else{
    # test if All CountryCode are known
    CountryName.column.name <- as.character(identified.spss.column.types[which(identified.spss.column.types[2] == "CountryCode"), 1])      
    uniqueValues <- unique( survey.data[[CountryName.column.name]] );    
    intersection <- intersect(uniqueValues, countries.code.name$CountryName)
    if(length(intersection) != length(uniqueValues)){
      return (
        paste("Detected Uknown Country Codes. Those are :", 
              paste(setdiff(uniqueValues, intersection), collapse = ", ")
        )
      )
    }
  }
  
  #--------------------------------------
  print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
  #--------------------------------------
  
  #--------------------------------------
  print("------------------------------------------")
  print(paste(" * isIdentifiedSPSSColumnsValid :", survey, " - | DONE"))  
  print(paste(" *  Time Needed :", (proc.time()[3] - function.starting.time), "seconds"))
  print("------------------------------------------")
  #--------------------------------------
  
  return(result);
}

#print(isIdentifiedSPSSColumnsValid("3RDEQLS"))

clearWhiteSpaces <- function(text){
  text <- gsub('\\s+', ' ', text)
  text <- gsub('\\s+', ' ', text)
  text <- gsub('\\s+', ' ', text)
  text <- gsub('\\t', ' ', text)
  text <- gsub('\n', '', text)
  return(text)
}

trimIfMoreThan <- function(text, maxSize = 45, toAppend = "..."){
  result <- text
  if(nchar(result) > maxSize){
    result <- substr(result, 0, maxSize - nchar(toAppend))
    result <- paste(result, toAppend, sep = "")
  }
  return(result)
}
# ======================================================================
