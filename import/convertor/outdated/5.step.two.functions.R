# ======================================================================
# Eworx S.A.- 2012
# Author kp@eworx.gr
# ======================================================================
# Contains the functions used by step two of the process
# ======================================================================

generateSubstitutableColumnValues <- function(survey, fileNameToAppend = step1.output.file.name){

	# Creates an xls containing all distinct values per substitutable columns
	#
	# Args:
	#	 survey: the id name of the survey
	#
	# Returns:
	#	 the column names


	print("------------------------------------------")
	print(paste(" * generateSubstitutableColumnValues :", survey, " - STARTED"))
	function.starting.time <- proc.time()[3]
	print("------------------------------------------")

	output.file.name <- paste(getSurveyFolder(survey), survey, ".", fileNameToAppend, sep = "")

	if(isIdentifiedSPSSColumnsValid(survey) == "Yes"){

	#--------------------------------------
	print(paste(" | generateSubstitutableColumnValues :", survey, " - Survey is valid. Loading required information"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	identified.spss.column.types <- readXls(paste(getSurveyFolder(survey), survey, ".", step1.input.file.name, sep = ""));
	identified.spss.column.types <- identified.spss.column.types[c("Column", "Type")]

	survey.data <- loadSPSSSurvey(survey, TRUE)

	survey.data.columns <- names(survey.data)

	column.values.to.substitute <- data.frame();

	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------
	#--------------------------------------
	print(paste(" | generateSubstitutableColumnValues :", survey, " -	Listing Distinct Values per Substitutable Columns types"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	for(substitutable.column.type in substitutable.column.types){

		for(column.index in 1:length(survey.data.columns)){

		column.name <- survey.data.columns[column.index]
		column.type <- as.character(identified.spss.column.types[which(identified.spss.column.types[1] == column.name ), 2])

		if(column.type == substitutable.column.type){

			uniqueValues <- unique(survey.data[column.name]);

			temp.table <- data.frame(uniqueValues)
			temp.table[, "Value"] <- uniqueValues
			temp.table[, "Column"] <- column.name
			temp.table[, "ColumnType"] <- column.type

			temp.table <- temp.table[c("Column", "ColumnType", "Value")]

			if(length(column.values.to.substitute[1,]) == 0){
			column.values.to.substitute <- temp.table
			}else{
			column.values.to.substitute <- merge(temp.table, column.values.to.substitute, all = TRUE)
			}
		}
		}
	}

	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	print(paste(" | generateSubstitutableColumnValues :", survey, " -	Do a Sound Ordering of results, add New Value column "))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	#note the trick multiplying a string with a number to perform a sound ordering
	column.values.to.substitute <- sqldf(
		"SELECT	* from 'column.values.to.substitute' order by ColumnType desc, Column, Value * 314"
	)

	# add the column to specify the values substitutions

	column.values.to.substitute[, "Importance"] <- 0
	column.values.to.substitute[, "NewValue"] <- column.values.to.substitute["Value"]

	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	print(paste(" | generateSubstitutableColumnValues :", survey, " -	Null Values to Delete Values "))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	# illustrate that all Null values will be Deleted
	levels(column.values.to.substitute[, "NewValue"]) <- c(levels(column.values.to.substitute[, "NewValue"]), value.delete)
	column.values.to.substitute["NewValue"] [is.na(column.values.to.substitute[, "NewValue"]), "NewValue"]	<- value.delete

	column.values.to.substitute <- sqldf(paste ("select * from 'column.values.to.substitute' where	NewValue <> '", value.delete, "'", sep = ""))

	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------


	#--------------------------------------
	print(paste(" | generateSubstitutableColumnValues :", survey, "Writing USER output xls file",	output.file.name))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	column.user.values.to.substitute <- sqldf(paste("select * from 'column.values.to.substitute' where	ColumnType in (",
		paste("'",paste(user.substitutable.column.types, collapse="', '"),"'",sep="")
	,") ", sep = ""))


	writeXls(column.user.values.to.substitute, output.file.name)

	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	output.file.name <- paste(getSurveyFolder(survey), survey, ".", step1.means.output.file.name, sep = "")
	
	#--------------------------------------
	print(paste(" | generateSubstitutableColumnValues :", survey, "Writing System, Questions Mean, output xls file",	output.file.name))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	column.means.values.to.substitute <- column.values.to.substitute

	column.means.values.to.substitute <- sqldf(paste("select * from 'column.values.to.substitute' where	ColumnType ='",
		 mean.substitutable.column.types
	,"' ", sep = ""))

	writeXls(column.means.values.to.substitute, output.file.name)

	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	}else{
		print("Survey is NOT valid.")
	}

	#--------------------------------------
	print("------------------------------------------")
	print(paste(" * generateSubstitutableColumnValues :", survey, " - | DONE"))
	print(paste(" *	Time Needed :", (proc.time()[3] - function.starting.time), "seconds"))
	print("------------------------------------------")
	#--------------------------------------
}

# generateSubstitutableColumnValues("3RDEQLS")

#--------------------------------------------------------------------


refineSurveyData <- function(survey){

	print("------------------------------------------")
	print(paste(" * refineSurveyData :", survey, " - STARTED"))
	function.starting.time <- proc.time()[3]
	print("------------------------------------------")

	#--------------------------------------
	print(paste(" | refineSurveyData :", survey, " - Loading user defined new values"))
	print(paste(" | refineSurveyData :", survey, " - Loading and converting into numerical data all QuestionMeans values"))
	print(paste(" | refineSurveyData :", survey, " - merging new values sets"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------
	substitute.columns.values <- readXls(paste(getSurveyFolder(survey), survey, ".", step2.input.file.name, sep = ""));
	substitutable.columns.values <- readXls(paste(getSurveyFolder(survey), survey, ".", step1.output.file.name, sep = ""));

	if(nrow(substitute.columns.values) != nrow(substitutable.columns.values)){
		msg <- "Subtitubale values are less than the should be. To specify a value to be deleted, you indicate it by entering DELETE and not actually delete the row."
		return(msg);
	}


	#remove Value entries corresponding to Null values as those have been already handled as they are not just values
	substitute.columns.values <- sqldf("select * from 'substitute.columns.values' where Value <> '' ")

	#### Add the importance infront of the new value
	substitute.columns.values$NewValue <- paste(substitute.columns.values$Importance, ". ", substitute.columns.values$NewValue, sep = "")
	substitute.columns.values$NewValue[which(substitute.columns.values$NewValue == "0. DELETE")] <- "DELETE"
	
	
	### All mean values must be converted into numbers and the merged with the user's substitution of values
	substitute.means.columns.values <- readXls(paste(getSurveyFolder(survey), survey, ".", step1.means.output.file.name, sep = ""))
	substitute.means.columns.values$NewValue <- gsub( '[^0-9]+', '', substitute.means.columns.values$NewValue)	
	substitute.columns.values <- merge(substitute.columns.values, substitute.means.columns.values, all = TRUE)

	###----------------------------------------------------------
	
	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	print(paste(" | refineSurveyData :", survey, " - Loading identified desired columns"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------
	#retain just the desired columns from the spss data
	identified.spss.column.types <- readXls(paste(getSurveyFolder(survey), survey, ".", step1.input.file.name, sep = ""));
	identified.spss.column.types <- sqldf("select * from 'identified.spss.column.types' where Type <> 'Delete' order by Type, Column ")
	desired.columns <- as.vector(identified.spss.column.types$Column)
	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	print(paste(" | refineSurveyData :", survey, " - Loading SPSS Data"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------
	survey.refined.data <- loadSPSSSurvey(survey, TRUE)
	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	print(paste(" | refineSurveyData :", survey, " - Retain desired columns"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------
	survey.refined.data <- survey.refined.data[desired.columns] # now are data contain just the desired columns
	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	print(paste(" | refineSurveyData :", survey, " - Substitute all NULL values with the DELETE value"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	# substitute all NULL values with the DELETE value
	for(column in names(survey.refined.data)){
	if(!(value.delete %in% levels(survey.refined.data[[column]]))){
		levels(survey.refined.data[[column]]) <- c(levels(survey.refined.data[[column]]), value.delete)
	}
	survey.refined.data[is.na(survey.refined.data[[column]]), column] <- value.delete
	}
	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	print(paste(" | refineSurveyData :", survey, " - Substitute all values with the new desired values (New value | DELETE)"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------
	# substitute all column values with the new desired ones.

	for(row in 1:nrow(substitute.columns.values)){

	column				 <- as.character( substitute.columns.values[row, "Column"] )
	column.current.value <- as.character( substitute.columns.values[row, "Value"] )
	column.new.value	 <- as.character( substitute.columns.values[row, "NewValue"] )

	if(column.current.value != column.new.value){
		if(!(column.new.value %in% levels(survey.refined.data[[column]]))){
		levels(survey.refined.data[[column]]) <- c(levels(survey.refined.data[[column]]), column.new.value)
		}
		survey.refined.data[survey.refined.data[[column]] == column.current.value, column] <- column.new.value
	}

	}
	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	# if there is a column of type CountryName replace it with a CountryCode
	if("CountryName" %in% levels(identified.spss.column.types$Type) ){
	#--------------------------------------
	print(paste(" | refineSurveyData :", survey, " - CountryCode does not exist. Replace Country names with Country Codes"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	column.CountryName <- (sqldf("select Column from 'identified.spss.column.types' where Type = 'CountryName' "))
	column.CountryName <- as.character(column.CountryName$Column)
	column <- column.CountryName

	for(row in 1:nrow(countries.code.name)){
		country.code <- as.character(countries.code.name$CountryCode[row])
		country.name <- as.character(countries.code.name$CountryName[row])
		#if the country exists in the data
		if(!(country.code %in% levels(survey.refined.data[[column]]))){
		levels(survey.refined.data[[column]]) <- c(levels(survey.refined.data[[column]]), country.code)
		}
		survey.refined.data[survey.refined.data[[column]] == country.name, column] <- country.code
	}
	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------
	}

	#--------------------------------------
	print(paste(" | refineSurveyData :", survey, " - Add EU Country Status in the survey.refined.data (CountryEUStatus)"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	column.CountryCode <- (sqldf("select Column from 'identified.spss.column.types' where Type = 'CountryName' or Type = 'CountryCode' "))
	column.CountryCode <- as.character(column.CountryCode$Column)

	# join survey data with countries EU statuses
	sql <- "SELECT "
	sql <- paste(sql, paste(names(survey.refined.data), collapse = ", ") )
	# place CountryEUStatus before the country column
	sql <- sub(paste(column.CountryCode,",",sep=""), paste("CountryEUStatus,", column.CountryCode, ",", sep = ""), sql)
	sql <- paste(sql, " from 'survey.refined.data' inner join 'countries.code.name' on CountryCode=", column.CountryCode)
	survey.refined.data <- sqldf(sql)

	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	print(paste(" | refineSurveyData :", survey, " - Set arbitrary Country Column name to fixed CountryCode"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------
	#set a fixed Country column name -> set to CountryCode
	column.Country <- (sqldf("select Column from 'identified.spss.column.types' where Type = 'CountryName' or Type = 'CountryCode' "))
	column.Country <- as.character(column.Country$Column)
	colnames(survey.refined.data)[which(names(survey.refined.data) == column.Country)] <- "CountryCode"
	column.Country <- "CountryCode"
	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	print(paste(" | refineSurveyData :", survey, " - ReIndex/Relevel Table"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------
	# recalculate all levels for every column in the data
	survey.refined.data <- drop.levels(survey.refined.data)
	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	print(paste(" | refineSurveyData :", survey, " - Save survey.refined.data:", paste(getSurveyFolder(survey), survey.refined.data.file.name, sep = "")))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------
	saveRefinedSurvey(survey, survey.refined.data)
	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	print(paste(" | refineSurveyData :", survey, " - Test - Load saved survey.refined.data"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------
	loadRefinedSurvey(survey)
	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	print("------------------------------------------")
	print(paste(" * refineSurveyData :", survey, " - SURVEY REFINED"))
	print(paste(" *	Time Needed :", proc.time()[3] - function.starting.time, "seconds"))
	print("------------------------------------------")
	#--------------------------------------
}

#refineSurveyData("3RDEQLS")
#test <- loadRefinedSurvey("3RDEQLS")
#test <- loadRefinedSurvey("3RDEQLS")
#unique(test$Y11_Q24)
