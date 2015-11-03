# ======================================================================
# Eworx S.A.- 2012
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
  return(read.spss(paste(getSurveyFolder(survey), step0.input.file.name, sep = ""), trim.factor.names = TRUE,  to.data.frame =  to.data.frame))
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

loadRefinedSurvey <- function(survey){
  return(loadDataFrame(paste(getSurveyFolder(survey), survey.refined.data.file.name, sep = "")))
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
	return(gsub("^[0-9]\\. ", "", value))
}

toUrlValue <- function(value){
	return(gsub("[^a-zA-Z0-9]", "-", value));
}