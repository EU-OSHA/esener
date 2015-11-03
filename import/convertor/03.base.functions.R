# ======================================================================
# Eworx S.A. - 2012/2013
# Author kp@eworx.gr
# ======================================================================
# Contains common functions used by the process
# ======================================================================

getSurveyFolder <- function(survey){
  return(paste(converter.base.path, survey, "/", sep = ""))
}

# ======================================================================

readXls <- function(file){
  return(read.csv(file = file, head = TRUE, encoding = "UTF-8", sep = "\t"))
}

writeXls <- function(data, file){
  write.table(data, file, sep = "\t", na = "", row.names = F)
}

# ======================================================================
# uses the foreign lib
# expects the spss to br in a file named spss.sav in a separate folder based on the Survey id name

loadSPSSSurvey <- function(survey, to.data.frame = FALSE){
	result <- read.spss(paste(getSurveyFolder(survey), step0.input.file.name, sep = ""), trim.factor.names = TRUE, to.data.frame = TRUE)
	if(survey == "3RDEQLS"){
		print("!!! Applying hardcoded weight fixing.")
		#print("count w5_EU28 == 0 ")
		#print(nrow(subset(result, w5_EU28 == 0 )))
		print("where w5_EU28 is null make it a 0")
		result$w5_EU28[is.na(result$w5_EU28)]<-0
		print(nrow(subset(result, w5_EU28 == 0 )))
		print("Set w4 where w5_EU28 is 0")
		result[which(result$w5_EU28 == 0), "w5_EU28"]<- result[which(result$w5_EU28 == 0), "w4"]
		print("Print count where w4 == w5_EU28")
		print(nrow(subset(result, w5_EU28==w4)))
		print("ok")
	}

	if(survey == "1STEQLS" || survey == "2NDEQLS"){
		print("!!! Applying hardcoded weight fixing. set w4 where w5_EU27 is null")
		print(nrow(result))
		print("where w5_EU27 is null make it a 0")
		result$w5_EU27[is.na(result$w5_EU27)]<-0
		print(nrow(subset(result, w5_EU27 == 0 )))
		print("Set w4 where w5_EU27 is 0")
		result[which(result$w5_EU27 == 0), "w5_EU27"]<- result[which(result$w5_EU27 == 0), "w4"]
		print("Print count where w4 == w5_EU27")
		print(nrow(subset(result, w5_EU27==w4)))
		print("ok")
	}
	if(survey == "3RDECS"){

		print("!!! Applying hardcoded rules for the 3RDECS survey")
		print(nrow(result))
		result <- subset(result, priv_act %in% c("private activities"))
		#result <- subset(result, priv_eu28 %in% c("private activities in EU28"))
		#result <- subset(result, ER_priv %in% c("private activities establishment where ER interview was completed"))
		#result <- subset(result, ER_priv_EU28 %in% c("EU28 private activities establishment where ER interview was completed"))
		print(nrow(result))


	}

	if(!to.data.frame){
		result <- as.list(result)
	}
	return(result)
}

getSPSSLabels <- function(survey){

	if(class(survey) == "character")
		survey.data <- loadSPSSSurvey(survey, TRUE)
	else
		survey.data <- survey

	survey.labels <- attr(survey.data, "variable.labels")
	survey.labels.columns <- as.vector(names(survey.labels ))

	survey.labels.values <- c();

	for(survey.labels.column in survey.labels.columns){
		survey.labels.values <- c(survey.labels.values, as.character(strsplit(as.character(survey.labels[survey.labels.column]), "\n")))
	}

	survey.labels <- data.frame(Value =  survey.labels.columns, NewValue = survey.labels.values)

	return(survey.labels)
}

# ======================================================================

saveDataFrame <- function(data, file){
  save(data, file = file)
}

loadDataFrame <- function(file){
  load(file)
  return(data)
}

saveRefinedSurvey <- function(survey, data){
  saveDataFrame(data, paste(getSurveyFolder(survey), survey.refined.data.file.name, sep = ""))
}

saveRefinedMRQSurvey <- function(survey, data){
  saveDataFrame(data, paste(getSurveyFolder(survey), survey.refined.mrq.data.file.name, sep = ""))
}

loadRefinedSurvey <- function(survey){
  return(loadDataFrame(paste(getSurveyFolder(survey), survey.refined.data.file.name, sep = "")))
}

loadRefinedMRQSurvey <- function(survey){
  return(loadDataFrame(paste(getSurveyFolder(survey), survey.refined.mrq.data.file.name, sep = "")))
}


loadSurveyStatistics <- function(survey){
	survey.data.location <- paste(survey.folder <- getSurveyFolder(survey), "Survey", survey, ".RData" , sep = "")
	if( file.exists(survey.data.location) ){
		return(loadDataFrame(survey.data.location))
	}else{
		print("Survey does not exist")
	}
}

loadMRQSurveyStatistics <- function(survey){
	survey.data.location <- paste(survey.folder <- getSurveyFolder(survey), "SurveyMRQ", survey, ".RData" , sep = "")
	if( file.exists(survey.data.location) ){
		return(loadDataFrame(survey.data.location))
	}else{
		print("Survey does not exist")
	}
}

# ======================================================================

countries.code.name <- readXls(paste(converter.scripts.path, countries.code.name.status.file.name, sep = ""))

loadDVTvaluesToTranslate <- function(){
  return(readXls(paste(converter.scripts.path, DVT.key.localized.values.file.name, sep = "")))
}

loadDVSvaluesToTranslate <- function(){
  return(readXls(paste(converter.scripts.path, DVS.key.localized.values.file.name, sep = "")))
}


# ======================================================================
# ======================================================================
# ======================================================================


recordStart <- function(location = "/web-pub/foundation/html/DVS/import/workspace/output.txt" ){
	sink(location)
}

recordStop <-function(){
	sink()
}


 
getColumnType <- function(survey, question.column.name){
		question.type <- subset( getIdentifiedColumnTypes(survey), (Column ==  question.column.name ))
		if(nrow(question.type) == 0 )
			return("Unknown")
		return(as.character(factor(question.type[1,2])))
}


getIdentifiedColumnTypes <- function(survey){
	identified.spss.column.types <- readXls(paste(getSurveyFolder(survey), survey, ".", step1.input.file.name, sep = ""))
	identified.spss.column.types <- identified.spss.column.types[c("Column", "Type")]
	return(identified.spss.column.types)
}

isQuestionMean <- function(survey, question.column.name){
		question.type <- subset( getIdentifiedColumnTypes(survey), (Column ==  question.column.name ))
		if(nrow(question.type) == 0 )
			return(FALSE)
		return(as.character(factor(question.type[1,2])) == "QuestionMean")
}

getSurveyTranslatedValues <- function(survey, locale){
	pattern.with.wildchar <- paste(getSurveyFolder(survey), survey, ".", step3.values.input.file.name.pattern, sep = "" )
	local.file <- gsub(pattern = "\\*", replacement = locale,  pattern.with.wildchar)
	return(readXls(local.file))
}

getSurveyTranslatedQuestions <- function(survey, locale){
	pattern.with.wildchar <- paste(getSurveyFolder(survey), survey, ".", step3.questions.input.file.name.pattern, sep = "" )
	local.file <- gsub(pattern = "\\*", replacement = locale,  pattern.with.wildchar)
	return(readXls(local.file))
}

#one time only to get the word all
getSurveyAllTranslatedValue <- function(survey, locale){
	translated.values <- getSurveyTranslatedValues(survey, locale)
	result <- sqldf("select TranslatedValue from 'translated.values' where column='word' and Value = 'all' ");
	return(as.character(factor(result[1,1])))
}
# getSurveyAllTranslatedValue("3RDEQLS", "EN")

# ======================================================================

heatMapCountVisualizations <- function (survey){

	data <- loadSurveyStatistics(survey)

	question.codes.type <- readXls(paste(getSurveyFolder(survey), survey, ".", step1.input.file.name, sep = ""));
	question.codes.type <- question.codes.type[c("Column", "Type")]
	question.codes.type <- sqldf("select * from 'question.codes.type' where Type in ('Question', 'QuestionMean')")
	unique.countries <- c()

	result.count <- 0;

	for(subset in names(data)){

		subsetData <- data[[subset]]
		subsetData <- subsetData [c("CountryCode", "subset", "question_code", "answer")]

		unique.countries <- unique(subsetData$CountryCode)

		#subsetData <- subsetData [c("subset", "question_code", "answer")]

		questions <- sqldf(
			"select subset, question_code, answer from 'subsetData'
				inner join 'question.codes.type' qTypes on qTypes.Column = 'subsetData'.question_code and Type = 'Question'
				group by subset, question_code, answer
			"
		)

		questionsMeans <- sqldf(
			"
			select subset, question_code, answer from 'subsetData'
				inner join 'question.codes.type' qTypes on qTypes.Column = 'subsetData'.question_code  and Type = 'QuestionMean'
				group by subset, question_code
			"
		)
		questionsMeans$answer <- "Mean"
		subsetData <- merge(questions, questionsMeans, all = TRUE)

		result.count <- result.count + nrow(subsetData)
	}
	return (result.count);

}

euBarsCountVisualizations <- function (survey){

	data <- loadSurveyStatistics(survey)

	question.codes.type <- readXls(paste(getSurveyFolder(survey), survey, ".", step1.input.file.name, sep = ""));
	question.codes.type <- question.codes.type[c("Column", "Type")]
	question.codes.type <- sqldf("select * from 'question.codes.type' where Type in ('Question', 'QuestionMean')")
	unique.countries <- c()

	result.count <- 0;

	for(subset in names(data)){

		subsetData <- data[[subset]]
		subsetData <- subsetData [c("CountryCode", "subset", "question_code", "answer")]

		unique.countries <- unique(subsetData$CountryCode)

		#subsetData <- subsetData [c("subset", "question_code", "answer")]

		questions <- sqldf(
			"select subset, question_code, answer from 'subsetData'
				inner join 'question.codes.type' qTypes on qTypes.Column = 'subsetData'.question_code and Type = 'Question'
				group by subset, question_code
			"
		)

		questionsMeans <- sqldf(
			"
			select subset, question_code, answer from 'subsetData'
				inner join 'question.codes.type' qTypes on qTypes.Column = 'subsetData'.question_code  and Type = 'QuestionMean'
				group by subset, question_code
			"
		)
		questionsMeans$answer <- "Mean"
		subsetData <- merge(questions, questionsMeans, all = TRUE)

		result.count <- result.count + nrow(subsetData)
	}
	return (result.count);

}


inCountryCountVisualizations <- function (survey){

	data <- loadSurveyStatistics(survey)

	question.codes.type <- readXls(paste(getSurveyFolder(survey), survey, ".", step1.input.file.name, sep = ""));
	question.codes.type <- question.codes.type[c("Column", "Type")]
	question.codes.type <- sqldf("select * from 'question.codes.type' where Type in ('Question', 'QuestionMean')")
	unique.countries <- c()

	result.count <- 0;

	for(subset in names(data)){

		subsetData <- data[[subset]]
		subsetData <- subsetData [c("CountryCode", "subset", "question_code", "answer")]

		unique.countries <- unique(subsetData$CountryCode)

		#subsetData <- subsetData [c("subset", "question_code", "answer")]

		questions <- sqldf(
			"select CountryCode, question_code, answer from 'subsetData'
				inner join 'question.codes.type' qTypes on qTypes.Column = 'subsetData'.question_code and Type = 'Question'
				group by CountryCode, question_code
			"
		)

		questionsMeans <- sqldf(
			"
			select CountryCode, question_code, answer from 'subsetData'
				inner join 'question.codes.type' qTypes on qTypes.Column = 'subsetData'.question_code  and Type = 'QuestionMean'
				group by CountryCode, question_code
			"
		)
		questionsMeans$answer <- "Mean"
		subsetData <- merge(questions, questionsMeans, all = TRUE)

		result.count <- result.count + nrow(subsetData)
	}
	return (result.count);

}


crossCountryCountVisualizations <- function (survey){

	data <- loadSurveyStatistics(survey)

	question.codes.type <- readXls(paste(getSurveyFolder(survey), survey, ".", step1.input.file.name, sep = ""));
	question.codes.type <- question.codes.type[c("Column", "Type")]
	question.codes.type <- sqldf("select * from 'question.codes.type' where Type in ('Question', 'QuestionMean')")
	unique.countries <- c()

	result.count <- 0;

	for(subset in names(data)){

		subsetData <- data[[subset]]
		subsetData <- subsetData [c("CountryCode", "subset", "question_code", "answer")]

		unique.countries <- unique(subsetData$CountryCode)

		#subsetData <- subsetData [c("subset", "question_code", "answer")]

		questions <- sqldf(
			"select CountryCode,subset, question_code, answer from 'subsetData'
				inner join 'question.codes.type' qTypes on qTypes.Column = 'subsetData'.question_code and Type = 'Question'
				group by CountryCode, subset, question_code
			"
		)

		questionsMeans <- sqldf(
			"
			select CountryCode, subset, question_code, answer from 'subsetData'
				inner join 'question.codes.type' qTypes on qTypes.Column = 'subsetData'.question_code  and Type = 'QuestionMean'
				group by CountryCode, subset, question_code
			"
		)
		questionsMeans$answer <- "Mean"
		subsetData <- merge(questions, questionsMeans, all = TRUE)

		result.count <- result.count + nrow(subsetData)
	}

	return (result.count * length(unique.countries));

}


clearIndex <- function(value){
	return(gsub("^[0-9]+\\. ", "", value))
}

toUrlValue <- function(value){
	return(gsub("[^a-zA-Z0-9]", "-", value));
}

clearWhiteSpaces <- function(text){
  text <- gsub('\\s+', ' ', text)
  text <- gsub('\\s+', ' ', text)
  text <- gsub('\\s+', ' ', text)
  text <- gsub('\\t', ' ', text)
  text <- gsub('\n', '', text)
  text <- gsub('[[:space:]]$', '', text)
  text <- gsub('^[[:space:]]', '', text)
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

replaceSingleQuote<-function(result){
	return(gsub(pattern = "\\'", replacement = "`",  result))
}

replaceEquote<-function(result){
	return(result)#(gsub(pattern = "e", replacement = "e",  result))
}

standarizeXLSColumnValues <- function(output.file.name, columnToStandarize){
	print("standarizeXLSColumnValues SHOULD BE AVOIDED FOR UTF-8 CONTENT")

	print(paste(" - standardizeXLSColumnValues  - STARTED"))
	print(paste(" - standardizeXLSColumnValues  -  File:", output.file.name))
	print(paste(" - standardizeXLSColumnValues  -   Column:", columnToStandarize, " replaceEquote, replaceSingleQuote, clearWhiteSpaces, trim "))
	dataFrame <- readXls(output.file.name);
	dataFrame[, columnToStandarize] <- replaceEquote(clearWhiteSpaces(replaceSingleQuote(dataFrame[, columnToStandarize])))
	writeXls(dataFrame, output.file.name)
	print(paste(" - standarizeXLSColumnValues  - Finished"))
	return(dataFrame)
}

createDVTSurveyFolders <- function(survey){
	mainDir <- "/web-pub/foundation/html/DVS/DVT/model/"
	dir.create(file.path(mainDir, survey))
	mainDir <- paste(mainDir, survey, sep = "")
	dir.create(file.path(mainDir, "js"))
	dir.create(file.path(mainDir, "php"))
}

########################################

smartSoundOrderingBy <- function(data, columns, sql = "select * from 'data' "){
  if(nrow(data)==0)return(data);
  print(paste(" |  smartSoundOrderingBy : - ", "Ordering by: ", paste(columns, collapse = ", "), " total rows ", nrow(data)))
  order.by <- " order by "
  original.column.names <- names(data)

  for(column in columns){
		data[, paste(column, "Enhanced", sep = "")]	<- changeOrderOfEnhancedColumns(toAlphaSmartNumerical(data[, column]))
		#gsub(pattern = "([[:alpha:][:punct:]])?([1-9]){1}", replacement = "\\10\\2",   data[, column])
  }

  sql <- paste(paste(sql, order.by, paste(columns, collapse = "Enhanced, ")), "Enhanced", sep = "")
  #print(sql)
  data <- sqldf(sql)

  return (data[, c(original.column.names)])
}

changeOrderOfEnhancedColumns <- function(data){
	return(
		gsub(pattern = "EuropeanUnion", replacement = "ZZZZZ2",
			gsub(pattern = "EUCC", replacement = "ZZZZZ1",
				gsub(pattern = "All", replacement = "0",   data)
			)
		)
	)
}

toAlphaSmartNumerical <- function(data){
	#\\10 is \\1 the first capturing group followed by a 0
 	return(gsub(pattern = "([[:alpha:][:punct:]]{1,})([1-9]){1}([[:alpha:][:punct:]])", replacement = "\\10\\2\\3",
 			gsub(pattern = "([[:alpha:][:punct:]]{1,})([1-9]){1}$", replacement = "\\10\\2",
 				gsub(pattern = "^([1-9]){1}([[:alpha:][:punct:]])", replacement = "0\\1\\2",   data )
 			)
 		)
 	)
}

prePendZeroInNumber <- function(data){
 	return(gsub(pattern = "^([1-9]){1}$", replacement = "0\\1\\2",   data ))
}

prePendZeroInNumberDot <- function(data){
 	return(gsub(pattern = "^([1-9]){1}\\.", replacement = "0\\1\\2\\.",   data ))
}

prePendZeroInColumnXLSUpdate <- function(file.name, column){
 data <- readXls(file.name)

 data <- prependZeroInDataframeColumn(data, column)

 writeXls(data, paste(file.name, "", sep = ""))
}


prePendZeroInNumberDotColumnXLSUpdate <- function(file.name, column){
## to update translation
 data <- readXls(file.name)
 data[, column] <- prePendZeroInNumberDot(data[, column])
 writeXls(data, paste(file.name, "", sep = ""))
}

prependZeroInDataframeColumn <- function(data, column){
	 columns <- names(data)
	 enhancedColumn <- paste(column, "Enhanced", sep = "")
	 data[, enhancedColumn] <- prePendZeroInNumber(data[, column])
	 data[, column] <- NULL
	 names(data)[names(data) == enhancedColumn ] <- column
	 data <- data[, c(columns)]
	 return(data)
}

smartSoundOrderingByXLSUpdate <- function(file.name, columns){
 data <- readXls(file.name)
 data <- smartSoundOrderingBy(data, columns)
 writeXls(data, paste(file.name, "", sep = ""))
}

smartSoundOrderingByAllFiles <- function(){
	surveyFolder <- "/web-pub/foundation/html/DVS/import/surveys/3RDEQLS/"

	prePendZeroInColumnXLSUpdate("/web-pub/foundation/html/DVS/import/surveys/3RDEQLS/3RDEQLS.substitute.columns.values.xls", "Importance")

	readXls("/web-pub/foundation/html/DVS/import/surveys/3RDEQLS/3RDEQLS.substitute.columns.values.xls")$Importance

	smartSoundOrderingByXLSUpdate( paste(surveyFolder, "3RDEQLS.substitutable.columns.values.xls", sep = ""), c("ColumnType", "Column", "Value"))

	smartSoundOrderingByXLSUpdate( paste(surveyFolder, "3RDEQLS.substitute.columns.values.xls", sep = "") , c("ColumnType", "Column", "Value"))

	smartSoundOrderingByXLSUpdate( paste(surveyFolder, "3RDEQLS.translate.values.XX.xls", sep = "") , c("EntryType",	"Column", "Value"))
	smartSoundOrderingByXLSUpdate( paste(surveyFolder, "3RDEQLS.translated.values.EN.xls", sep = "") , c("EntryType",	"Column", "Value"))

	smartSoundOrderingByXLSUpdate( paste(surveyFolder, "", sep = "3RDEQLS.translate.questions.XX.xls") , c("QuestionCode"))
	smartSoundOrderingByXLSUpdate( paste(surveyFolder, "3RDEQLS.translated.questions.EN.xls", sep = ""), c("QuestionCode"))

	#a <- as.vector(readXls(paste(surveyFolder, "", sep = "3RDEQLS.translate.questions.XX.xls"))$QuestionCode)
	#b <- as.vector(readXls(paste(surveyFolder, "", sep = "3RDEQLS.translated.questions.EN.xls"))$QuestionCode)

	#print(setdiff(a,b))
	return("ok")


	data <- readXls("/web-pub/foundation/html/DVS/import/surveys/3RDEQLS/3RDEQLS.substitute.columns.values.xls")

	data[, "NewValue2"] <- paste(prePendZeroInNumber(data[, "Importance"]), ". ", data[, "NewValue"], sep = "")
	data[, "NewValue"] <- data[, "NewValue2"]
	data<- data[, c("Column", "ColumnType", "Value", "NewValue")]


	prePendZeroInNumberDotColumnXLSUpdate("/web-pub/foundation/html/DVS/import/surveys/3RDEQLS/3RDEQLS.translated.values.EN.xls", "Value")
	prePendZeroInNumberDotColumnXLSUpdate("/web-pub/foundation/html/DVS/import/surveys/3RDEQLS/3RDEQLS.translated.values.EN.xls", "TranslatedValue")
}

##### these functions are provided by the import process

readXls <- function(file) {
	return(read.csv(file = file, head = TRUE, sep = "\t"))
}

writeDataInCSV <- function(data, file) {
	write.table(data, file, sep = "\t", na = "", dec = ",", row.names = F)
}

#  syncCSVby("/web-pub/foundation/html/DVS/import/surveys/2NDEQLS/2NDEQLS.analyse.columns.xls", 	"/web-pub/foundation/html/DVS/import/surveys/3RDEQLS/3RDEQLS.analysed.columns.xls",	c("Column"),	"/web-pub/foundation/html/DVS/import/surveys/2NDEQLS/2NDEQLS.analysed.columns.xls")

syncCSVby <- function(sourceA, sourceB, columnKeys = c(), outputFile) {
	#syncs two csv files by the columnKeys.
	#for every entry that exists in a and b the values if b are used
	#for every entry that exists only in a the values of a are used

	if (!file.exists(sourceA))
		print("First source file does not exist in the directory") else if (!file.exists(sourceB))
		print("Second source file does not exist in the directory") else if (length(columnKeys) == 0)
		print("No key columns are given") else {

		data1 <- readXls(sourceA)
		data2 <- readXls(sourceB)

		data1$key <- ""
		for (j in 1:length(columnKeys)) {
			data1$key <- paste(data1$key, data1[, columnKeys[j]], sep = "")
		}


		data2$key <- ""
		for (j in 1:length(columnKeys)) {
			data2$key <- paste(data2$key, data2[, columnKeys[j]], sep = "")
		}

		domains.names <- names(data2)
		select.columns.2ndarray <- ""
		for (i in 1:length(domains.names)) {
			column.temp <- paste("data2.", domains.names[i], sep = "")
			if (i > 1) {
				select.columns.2ndarray <- paste(select.columns.2ndarray, column.temp,
					sep = ",")
			} else if (i == 1) {
				select.columns.2ndarray <- paste(select.columns.2ndarray, column.temp,
					sep = "")
			}
		}

		domains.names <- names(data1)
		select.columns.1starray <- ""
		for (i in 1:length(domains.names)) {
			column.temp <- paste("data1.", domains.names[i], sep = "")
			if (i > 1) {
				select.columns.1starray <- paste(select.columns.1starray, column.temp,
					sep = ",")
			} else if (i == 1) {
				select.columns.1starray <- paste(select.columns.1starray, column.temp,
					sep = "")
			}
		}

		sql.query <- paste("select ", select.columns.1starray, " from data1 left outer join data2 on data1.key = data2.key")

		data.1ST.rest <- sqldf(sql.query)

		sql.query <- paste("select ", select.columns.2ndarray, "from data1 inner join data2 on data1.key = data2.key")

		data.in.both <- sqldf(sql.query)

		if (sourceA == "1STEQLS.translate.values.XX.xls") {
			#changing the TranslatedValue in the xls files
			data.1ST.rest[data.1ST.rest$Column == "survey", ][, 4] <- gsub("2012",
				"2003", as.character(data2[data2$Column == "survey", ][, 4]))
		}
		else if (sourceA == "2NDEQLS.translate.values.XX.xls"){
			#changing the TranslatedValue in the xls files
			data.1ST.rest[data.1ST.rest$Column == "survey", ][, 4] <- gsub("2012",
				"2007", as.character(data2[data2$Column == "survey", ][, 4]))
		}

		if (nrow(data.1ST.rest) != nrow(data.in.both)) {
			error.message <- paste("There are ", as.character(nrow(data.1ST.rest) -
				nrow(data.in.both)), " rows that existed in the \n", sourceA, " file but not in the \n",
				sourceB)
			print(error.message)
			print("The rows that existed only in the old file are the below:")
			rows.only.old <- sqldf("select * from 'data.1ST.rest' as old where old.key not in (select key from 'data.in.both') ")
			rows.only.old$key <- NULL

			for (i in 1:nrow(rows.only.old)) {
				row.all.columns <- ""
				for (j in 1:ncol(rows.only.old)) {
					if (j > 1) {
					row.all.columns <- paste(row.all.columns, rows.only.old[i, j],
						sep = " , ")
					} else if (j == 1) {
					row.all.columns <- paste(row.all.columns, rows.only.old[i, j],
						sep = "")
					}
				}
				print(row.all.columns)
			}

			data.in.both$key <- NULL	#dropping the useless anymore column
			data.final <- rbind(data.in.both, rows.only.old)	#adding the rows that existed only in the old file

			if (!file.exists(outputFile))
				file.create(outputFile, showWarnings = TRUE)

			writeDataInCSV(data.final, outputFile)

		} else {
			data.in.both$key <- NULL	#dropping the useless anymore column
			if (!file.exists(outputFile))
				file.create(outputFile, showWarnings = TRUE)
			writeDataInCSV(data.in.both, outputFile)
		}
	}

}
