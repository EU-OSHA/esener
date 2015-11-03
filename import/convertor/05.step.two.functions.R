# ======================================================================
# Eworx S.A. - 2012 / 2013
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

	survey.data <- loadSPSSSurvey(survey, TRUE)

	if(isIdentifiedSPSSColumnsValid(survey, survey.data) == "Yes"){

	#--------------------------------------
	print(paste(" | generateSubstitutableColumnValues :", survey, " - Survey is valid. Loading required information"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	identified.spss.column.types <- readXls(paste(getSurveyFolder(survey), survey, ".", step1.input.file.name, sep = ""));

	identified.spss.column.types[is.na(identified.spss.column.types$ColumnAs), "ColumnAs" ] <- ""


	identified.question.countryName.spss.columns <- sqldf("select Column from 'identified.spss.column.types' where Type='Question' and ColumnAs='CountryName'")
	identified.non.question.countryName.spss.columns <- sqldf("select Column from 'identified.spss.column.types' where Type='Question' and ColumnAs<>'CountryName'")

	identified.question.countryName.spss.columns.values <- as.vector(unique(identified.question.countryName.spss.columns$Column))


	identified.spss.column.types <- identified.spss.column.types[c("Column", "Type")]


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

			column.type <- as.character(
				identified.spss.column.types[which(identified.spss.column.types[1] == column.name ), 2]
			)

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

	#- - - - - - - - - - - - - - - - - - -
	# FRWBII-245  Multiple Response Questions - new survey question type implementation
	# Columns of QuestionMR which are answers, should be included in the result file substitutable.columns.values.xls of substitutableColumnValues().

	survey.labels <- getSPSSLabels(survey.data)

	multiple.question.answers <- readXls(paste(getSurveyFolder(survey), survey, ".", step1.input.file.name, sep = ""));
	multiple.question.answers <- subset(multiple.question.answers, (Type == column.type.QuestionMR))

	if(nrow(multiple.question.answers)>0){
		multiple.question.answers <- sqldf("select ColumnAs Column, Type ColumnType, Column Value, '0' Importance from 'multiple.question.answers'")
		multiple.question.answers <- merge(multiple.question.answers, survey.labels, by = "Value")
		multiple.question.answers <- multiple.question.answers[c("Column", "ColumnType", "Value", "Importance", "NewValue") ]
	}

	#- - - - - - - - - - - - - - - - - - -

	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	print(paste(" | generateSubstitutableColumnValues :", survey, " -	Add NewValue column."))
	print(paste(" | generateSubstitutableColumnValues :", survey, " -	Smart Sound Ordering of results"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	column.values.to.substitute <- smartSoundOrderingBy(column.values.to.substitute, c("ColumnType", "Column", "Value"))

	# add the column to specify the values substitutions

	column.values.to.substitute[, "Importance"] <- 0


	#- - - - - - - - - - - - - - - - - - -
	#- - - - - - - - - - - - - - - - - - -

	print(paste(" | generateSubstitutableColumnValues :", survey, " -	Replace Question ColumnAs CountryName to CountryCode Values. NA values to Delete Value"))

	column.values.to.substitute[, "NewValue"] <- replaceEquote(clearWhiteSpaces(replaceSingleQuote(column.values.to.substitute[, "Value"])))
	column.values.to.substitute.full <- column.values.to.substitute

	column.values.to.substitute.subsets <- sqldf(paste("select * from 'column.values.to.substitute' where ColumnType = '", column.type.Subset, "'", sep = ""))

	#Questions ColumnAs = CountryName values should be replaced with CountryCode values | non existing countries should marked for deletion

	column.question.country.name.values.to.substitute <- merge(column.values.to.substitute, identified.question.countryName.spss.columns, by = "Column")
	column.question.country.name.values.to.substitute$NewValue <- NULL

	countries.code.names.to.replace <- countries.code.name
	countries.code.names.to.replace[, "Value"] <- countries.code.names.to.replace$CountryName
	countries.code.names.to.replace[, "NewValue"] <- countries.code.names.to.replace$CountryCode
	countries.code.names.to.replace <- countries.code.names.to.replace[, c("Value", "NewValue")]

	if(length(identified.question.countryName.spss.columns.values)>0){
		temp <- merge(countries.code.names.to.replace, column.question.country.name.values.to.substitute, by = "Value", all.y = TRUE)

		sql <- paste("update temp set NewValue = 'Delete' where NewValue is NULL")
		temp <- sqldf(c(sql,  "select * from main.temp"))
		sql <- paste("delete from temp where Value is NULL")
		temp <- sqldf(c(sql,  "select * from main.temp"))
		temp<-temp[, c("Column", "ColumnType", "Value", "Importance", "NewValue")]

		column.question.country.name.values.to.substitute <- temp

		column.values.to.substitute <- merge(column.question.country.name.values.to.substitute, column.non.question.country.name.values.to.substitute, all = TRUE)
	}
	column.non.question.country.name.values.to.substitute <- merge(column.values.to.substitute, identified.non.question.countryName.spss.columns, by = "Column")

	column.values.to.substitute <- merge(column.values.to.substitute, column.values.to.substitute.subsets, all = TRUE)
	#- - - - - - - - - - - - - - - - - - -

	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	print(paste(" | generateSubstitutableColumnValues :", survey, " -	Null Values to Delete Values "))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	# illustrate that all Null values will be Deleted
	# levels(column.values.to.substitute[, "NewValue"]) <- c(levels(column.values.to.substitute[, "NewValue"]), value.delete)
	# column.values.to.substitute["NewValue"] [is.na(column.values.to.substitute[, "NewValue"]), "NewValue"]	<- value.delete


	column.values.to.substitute <- sqldf("select * from 'column.values.to.substitute' where Value is not NULL")

	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------


	#--------------------------------------
	print(paste(" | generateSubstitutableColumnValues :", survey, "Writing default Subset, Question output xls file",	output.file.name))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	column.user.values.to.substitute <- sqldf(paste("select * from 'column.values.to.substitute' where	ColumnType in (",
		paste("'",paste(user.substitutable.column.types, collapse="', '"), "'", sep="")
	,") ", sep = ""))


	column.user.values.to.substitute.subsets <- subset(column.user.values.to.substitute, (ColumnType == "Subset"))
	column.user.values.to.substitute.questions <- subset(column.user.values.to.substitute, (ColumnType != "Subset"))
	if(nrow(multiple.question.answers)>0)
		column.user.values.to.substitute.questions <- rbind(column.user.values.to.substitute.questions, multiple.question.answers)

	column.user.values.to.substitute.questions <- sqldf("SELECT	* from 'column.user.values.to.substitute.questions'")	#order by Column, Value
	column.user.values.to.substitute <- rbind(column.user.values.to.substitute.subsets, column.user.values.to.substitute.questions)

	column.user.values.to.substitute <- smartSoundOrderingBy(column.user.values.to.substitute, c("ColumnType", "Column", "Value" ))

	writeXls(column.user.values.to.substitute, output.file.name)

	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	output.file.name <- paste(getSurveyFolder(survey), survey, ".", step1.means.output.file.name, sep = "")

	#--------------------------------------
	print(paste(" | generateSubstitutableColumnValues :", survey, "Writing default Questions Mean, output xls file",	output.file.name))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	column.means.values.to.substitute <- column.values.to.substitute.full

	column.means.values.to.substitute <- sqldf(paste("select * from 'column.means.values.to.substitute' where	ColumnType ='", mean.substitutable.column.types ,"' ", sep = ""))

	writeXls(column.means.values.to.substitute, output.file.name)

	#--------------------------------------

	if(FALSE){

		# block is commented as we can take for granted that mrq's may only have 1 or 0 equivelant values
		# block has not been deleted yet as we may want to see the unique values contained in the mrqs

		# output.file.name <- paste(getSurveyFolder(survey), survey, ".", step1.multiple.output.file.name, sep = "")

		#--------------------------------------
		print(paste(" | generateSubstitutableColumnValues :", survey, "Writing default, Multiple response questions, output xls file",	output.file.name))
		function.block.starting.time <- proc.time()[3]
		#--------------------------------------

		identified.spss.column.types <- readXls(paste(getSurveyFolder(survey), survey, ".", step1.input.file.name, sep = ""));
		identified.spss.column.types[is.na(identified.spss.column.types$ColumnAs), "ColumnAs" ] <- ""
		identified.spss.column.multiple <- identified.spss.column.types[c("Column", "Type", "ColumnAs" )]
		identified.spss.column.multiple <- subset(identified.spss.column.multiple, (Type == column.type.QuestionMR))[c("Column", "ColumnAs" )]

		column.multiple.values.to.substitute <- column.values.to.substitute
		column.multiple.values.to.substitute <- sqldf(paste("select * from 'column.values.to.substitute' where ColumnType ='", column.type.QuestionMR ,"' ", sep = ""))

		column.multiple.values.to.substitute <- merge(
			column.multiple.values.to.substitute,
			identified.spss.column.multiple,
			by = "Column"
		)[c("ColumnAs", "Column", "ColumnType", "Value",	"NewValue" )]

		column.multiple.values.to.substitute[, "NewValue"] <- "Delete"

		levels(column.multiple.values.to.substitute[["NewValue"]]) <- c(levels(column.multiple.values.to.substitute[["NewValue"]]), "1")
		levels(column.multiple.values.to.substitute[["NewValue"]]) <- c(levels(column.multiple.values.to.substitute[["NewValue"]]), "0")

		for(row in 1:nrow(column.multiple.values.to.substitute)){
			column.current.value <- as.character( column.multiple.values.to.substitute[row, "Value"] )
			if(column.current.value == multiple.response.true){
				column.multiple.values.to.substitute[row, "NewValue"] <- "1"
			}else
			if(column.current.value == multiple.response.false){
				column.multiple.values.to.substitute[row, "NewValue"] <- "0"
			}
		}

		writeXls(column.multiple.values.to.substitute, output.file.name)
	}
	#--------------------------------------

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
#--------------------------------------------------------------------
#--------------------------------------------------------------------

standarizeSubstituteColumnsValues <- function(survey){
	output.file.name <- paste(getSurveyFolder(survey), survey, ".", step2.input.file.name, sep = "")
	data <- standarizeXLSColumnValues(output.file.name, "NewValue")

	print(paste(" - standarizeSubstituteColumnsValues  -  Delete values should have 0 Importance"))

	column <- "Importance"
	levels(data[[column]]) <- c(levels(data[[column]]), 0)

	data[data[["NewValue"]] == "Delete", column] <- 0
	writeXls(data, output.file.name)

	print(paste(" - standarizeSubstituteColumnsValues  -  Input file updated"))
	print(paste(" - standarizeSubstituteColumnsValues  - Finished"))
}

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

	#standarizeSubstituteColumnsValues(survey)
	#this has been used to "standardized" user values.
	#the e9 character though gets degraded so it is removed from the standard process


	identified.spss.column.types <- readXls(paste(getSurveyFolder(survey), survey, ".", step1.input.file.name, sep = ""));
	identified.spss.column.types[is.na(identified.spss.column.types$ColumnAs), "ColumnAs" ] <- ""
	identified.spss.column.types <- sqldf("select * from 'identified.spss.column.types' where Type <> 'Delete'")
	identified.spss.column.types <- smartSoundOrderingBy(identified.spss.column.types, c("Type", "Column"))

	identified.question.countryName.spss.columns <- sqldf("select Column from 'identified.spss.column.types' where Type='Question' and ColumnAs='CountryName'")
	identified.question.countryName.spss.columns.values <- as.vector(unique(identified.question.countryName.spss.columns$Column))


	substitute.columns.values <- readXls(paste(getSurveyFolder(survey), survey, ".", step2.input.file.name, sep = ""));
	substitutable.columns.values <- readXls(paste(getSurveyFolder(survey), survey, ".", step1.output.file.name, sep = ""));

	if(nrow(substitutable.columns.values) != nrow(substitute.columns.values)){
		msg <- paste("Substitutable values ", nrow(substitutable.columns.values), " are different in size than the values to substitute", nrow(substitute.columns.values))
		return(msg);
	}

	#### Add the importance infront of the new value
	###########################################################
	#remove Value entries corresponding to Null values as those have been already handled by a previous step
	#substitute.columns.values <- readXls(paste(getSurveyFolder(survey), survey, ".", step2.input.file.name, sep = ""));

	substitute.columns.values <- sqldf(paste("select * from 'substitute.columns.values' where Value <> '' and ColumnType <> '", column.type.QuestionMR, "'", sep = ""))
	answersMaxCount <- sqldf("select count(*) answersCount from 'substitute.columns.values' where NewValue <> 'Delete' and ColumnType == 'Question' group by Column")
	answersMaxCount <- max(answersMaxCount$answersCount)
	print(paste(" | refineSurveyData :", survey, " - If number of answers < 10 (", answersMaxCount, "). do not prepend zero"))

	if(answersMaxCount>9)
		substitute.columns.values$NewValue <- paste(prePendZeroInNumber(substitute.columns.values$Importance), ". ", substitute.columns.values$NewValue, sep = "")
	else
		substitute.columns.values$NewValue <- paste((substitute.columns.values$Importance), ". ", substitute.columns.values$NewValue, sep = "")

	levels(substitute.columns.values[["NewValue"]]) <- c(levels(substitute.columns.values[["NewValue"]]), value.delete)
	substitute.columns.values$NewValue[which(substitute.columns.values$NewValue == paste("0. ", value.delete, sep = ""))] <- value.delete

	###

	### All mean values must be converted into numbers otherwise to delete values and then merged with the user's substitution of values
	substitute.means.columns.values <- readXls(paste(getSurveyFolder(survey), survey, ".", step2.means.input.file.name, sep = ""))
	substitute.means.columns.values$NewValue <- gsub( '^$', value.delete, gsub( '[^0-9\\.]+', '', substitute.means.columns.values$NewValue))

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


	#- - - - - - - - - - - - - - - - - - -
	# FRWBII-245  Multiple Response Questions - new survey question type implementation
	# the dataset must be separated into two refined datasets
	# the two datasets are to be handled differently and at the statitics calculation step, their results should be merged again as then, their encoding will be the same.

	# The two datasets must have in common the transaction identifier keys; these should be all the selected columns except the question related columns.
	# as such, using as a convenssion the first key as a transaction identifier, will allow reusing the processed data performed for Questions, Questions Means.

	#c(column.type.CountryCode, column.type.Subset, column.type.QuestionMR, column.type.Weight)

	desired.columns.mrq <- as.vector(subset(identified.spss.column.types, Type == column.type.QuestionMR)$Column)
	desired.columns.mrq.full <- as.vector(subset(identified.spss.column.types, Type == column.type.CountryCode | Type == column.type.Subset |   Type == column.type.QuestionMR  |  Type == column.type.Weight )$Column)
	desired.columns <- as.vector(subset(identified.spss.column.types, Type != column.type.QuestionMR)$Column)


	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	print(paste(" | refineSurveyData :", survey, " - Loading SPSS Data as data frame"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------
	survey.refined.data <- loadSPSSSurvey(survey, TRUE)
	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	print(paste(" | refineSurveyData :", survey, " - Retain desired columns except MRQ"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	survey.refined.data.a <- survey.refined.data[c(
		#names(survey.refined.data)[1], # the first column was considered to be the weight# however it is already included in the desired.columns # so it is commented
		desired.columns)] ### now the data contain just the desired columns

	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	print(paste(" | refineSurveyData :", survey, " - Retain desired MRQ columns"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	survey.refined.data.mrq <- survey.refined.data[c(names(survey.refined.data)[1], desired.columns.mrq) ] ### now the data contain just the desired columns

	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	survey.refined.data <- survey.refined.data.a

	#--------------------------------------
	print(paste(" | refineSurveyData :", survey, " - Substitute all NULL values with the Delete value"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	# substitute all NULL values with the Delete value
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
	print(paste(" | refineSurveyData :", survey, " - Substitute all values with the new desired values (New value)"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------
	# substitute all column values with the new desired ones.

	for(row in 1:nrow(substitute.columns.values)){

		column				 <- as.character( substitute.columns.values[row, "Column"] )
		column.current.value <- as.character( substitute.columns.values[row, "Value"] )
		column.new.value	 <- as.character( substitute.columns.values[row, "NewValue"] )

		if(column %in% identified.question.countryName.spss.columns.values){
 			column.new.value <- clearIndex(column.new.value)
 		}

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
		print(paste(" | refineSurveyData :", survey, " - CountryCode does not exist. Replacing country names with country codes"))
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
	print(paste(" | refineSurveyData :", survey, " - Merging identifier columns, Save 1) survey.refined.data:", paste(getSurveyFolder(survey), survey.refined.data.file.name, sep = "")))
	if(length(names(survey.refined.data.mrq))>1){
		print(paste(" | refineSurveyData :", survey, " - Merging identifier columns, Save 2) survey.refined.data:", paste(getSurveyFolder(survey), survey.refined.mrq.data.file.name, sep = "")))
	}

	function.block.starting.time <- proc.time()[3]

	if(length(names(survey.refined.data.mrq))>1){
		print(paste(" | refineSurveyData : ", survey, " - Refining MRQ values (NULL, NON TRUE/FALSE) to -1, TRUE to 1, FALSE to 0", sep = ""))

		survey.refined.data.mrq <- merge(survey.refined.data, survey.refined.data.mrq, by = names(survey.refined.data)[1])

		for(mrqColum in desired.columns.mrq){
			distinctValues <- as.vector(unique(survey.refined.data.mrq[, mrqColum]))

			levels(survey.refined.data.mrq[[mrqColum]]) <- c(levels(survey.refined.data.mrq[[mrqColum]]), 1)
			levels(survey.refined.data.mrq[[mrqColum]]) <- c(levels(survey.refined.data.mrq[[mrqColum]]), -1)
			levels(survey.refined.data.mrq[[mrqColum]]) <- c(levels(survey.refined.data.mrq[[mrqColum]]), 0)

			for(distinctValue in distinctValues){
				#print(paste(mrqColum, "-", distinctValue))
				survey.refined.data[is.na(survey.refined.data[[column]]), column] <- value.delete

				if(is.na(distinctValue)){
					survey.refined.data.mrq[is.na(survey.refined.data.mrq[[mrqColum]]), mrqColum] <- -1
				}
				else
				if(distinctValue == multiple.response.true)
					survey.refined.data.mrq[survey.refined.data.mrq[[mrqColum]] == distinctValue, mrqColum] <- 1
				else
				if(distinctValue == multiple.response.false)
					survey.refined.data.mrq[survey.refined.data.mrq[[mrqColum]] == distinctValue, mrqColum] <- 0
				else
					survey.refined.data.mrq[survey.refined.data.mrq[[mrqColum]] == distinctValue, mrqColum] <- -1
			}
		}

		survey.refined.data.mrq <- drop.levels(survey.refined.data.mrq)

		survey.refined.data.mrq <- survey.refined.data.mrq[c("CountryEUStatus", "CountryCode", desired.columns.mrq.full)]

		print(paste(" | refineSurveyData :", survey, " - MRQ values Refined", sep = ""))
	}
	print(paste(" | refineSurveyData :", survey, " - Saving", sep = ""))

	#mrq <- loadRefinedMRQSurvey("3RDEQLS")

	#--------------------------------------
	#survey.refined.data[1] <- NULL
	saveRefinedSurvey(survey, survey.refined.data)

	if(length(names(survey.refined.data.mrq))>1){
		saveRefinedMRQSurvey(survey, survey.refined.data.mrq)
	}
	#--------------------------------------
	print(paste(" |	- Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	print(paste(" | refineSurveyData :", survey, " - Test - Load saved survey.refined.data"))
	if(length(names(survey.refined.data.mrq))>1){
		print(paste(" | refineSurveyData :", survey, " - Test - Load saved survey.refined.mrq.data"))
	}
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------
	loadRefinedSurvey(survey)
	if(length(names(survey.refined.data.mrq))>1){
		loadRefinedMRQSurvey(survey)
	}
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
