# ======================================================================
# Eworx S.A. - 2012 / 2013
# Author kp@eworx.gr
# ======================================================================
# Contains the functions to update the validation layer
# ======================================================================

updateValidationLayer <- function(survey){

	print("------------------------------------------")
	print(paste(" * DVS Validation Layer Population. :", survey, " - STARTED"))
	function.starting.time <- proc.time()[3]
	print("------------------------------------------")

	result <- surveyToPHPValidationCode(survey)

	con <- file(paste(RRML.validation.layer, survey,".", valid.parameters.php, sep = ""))
	writeLines(c(result), con)
	close(con)
	print(" - New validation layer Published")

	#--------------------------------------
	print("------------------------------------------")
	print(paste(" * DVS Validation Layer update. :", survey, " - COMPLETED"))
	print(paste(" *  Time Needed :", (proc.time()[3] - function.starting.time) , "seconds"))
	print("------------------------------------------")
	#--------------------------------------

}

#updateValidationLayer("3RDEQLS")
# ======================================================================

questionsAnswersToIndexedPHPArray <- function(valid.combinations){
	result <- ""
	questions <- as.vector(factor(unique(valid.combinations$question_code)))

	for(question in questions){
		questions.answers <- paste("'", question,"' => array(", questionAnswersToList(question, valid.combinations), ")", sep = "")
		if(question == questions[1]){
			result <- questions.answers
		}else{
			result <- paste(result, ",\n\t\t\t", questions.answers, sep = "")
		}
	}

	result <- paste("\n\t\t'questions' => array(\n\t\t\t", result , "\n\t\t)", sep = "")
	return(result)
}

# ======================================================================

questionAnswersToList <- function(question, valid.combinations){
	result <- sqldf(paste("select answer from 'valid.combinations' where question_code = '", question ,"'", sep = ""))
	result <- as.vector(result$answer)

	result <- paste(result, collapse = "SEPARATOR")
	result <- replaceSingleQuote(result)

	result <- gsub(pattern = "SEPARATOR", replacement = "', '",  result)

	result <- paste("'", result, "'", sep = "")

	return(result)
}

# ======================================================================

subsetsToIndexedPHPArray <- function(valid.combinations){
	result <- ""
	subsets <- as.vector(factor(unique(valid.combinations$Subset)))
	for(subset in subsets){
		subsets.values <- paste("'", subset,"' => array(", subsetsSubsetValuesToList(subset, valid.combinations), ")", sep = "")
		if(subset == subsets[1]){
			result <- subsets.values
		}else{
			result <- paste(result, ",\n\t\t\t", subsets.values, sep = "")
		}
	}
	result <- paste("\n\t\t'subsets' => array(\n\t\t\t", result , "\n\t\t)", sep = "")
	return(result)
}

# ======================================================================

subsetsSubsetValuesToList <- function(subset, valid.combinations){
	result <- sqldf(paste("select SubsetValue from 'valid.combinations' where Subset = '", subset ,"'", sep = ""))
	result <- as.vector(result$SubsetValue)
	result <- paste(result, collapse = ",")
	result <- gsub(pattern = ",", replacement = "', '",  result)
	result <- paste("'", result, "'", sep = "")
	return(result)
}

# ======================================================================

countriesToIndexedPHPArray <- function(countries){
	result <- paste(countries, collapse = ",")
	result <- gsub(pattern = ",", replacement = "', '",  result)
	result <- paste("'", result, "'", sep = "")
	result <- paste("\n\t\t'countries' => array(\n\t\t\t", result , "\n\t\t)", sep = "")
	return(result)
}

# ======================================================================

surveyToPHPValidationCode  <- function(survey){

	print("------------------------------------------")
	print(paste(" * DVS Validation Layer PHP model generation. :", survey, " - STARTED"))
	function.starting.time <- proc.time()[3]
	print("------------------------------------------")

	#--------------------------------------
	print(paste(" | surveyToPHPValidationCode :", survey, " - Loading required information"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	data <- loadSurveyStatistics(survey)

	question.codes.type <- readXls(paste(getSurveyFolder(survey), survey, ".", step1.input.file.name, sep = ""));
	question.codes.type[is.na(question.codes.type$ColumnAs), "ColumnAs" ] <- ""
	#question.codes.type <- question.codes.type[c("Column", "Type")]

	question.codes.type.std <- sqldf("select Column, Type, ColumnAs from 'question.codes.type' where Type in ('Question', 'QuestionMean')")	
	question.codes.type.mrq <- sqldf("select ColumnAs Column, Type, ColumnAs from 'question.codes.type' where Type = 'QuestionMR'")
	question.codes.type <- merge(question.codes.type.std, question.codes.type.mrq, all = TRUE)

	#--------------------------------------
	print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------
	#--------------------------------------
	print(paste(" | surveyToPHPValidationCode :", survey, " - Find distinct Subset - SubsetValue combinations from statistical data"))
	print(paste(" | surveyToPHPValidationCode :", survey, " - Find distinct countries "))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------
	# gather Subset - SubsetValue
	# gather countries

	subsets.values <- c()
	countries <- c()

	for(subset in names(data)){

		subsetData <- data[[subset]]
		subsetData <- subsetData [c("CountryCode", "subset", "question_code", "answer")]

		if(subset == names(data)[1]){
			countries <- as.vector(factor(unique(subsetData$CountryCode)))
		}

		subsetData <- subsetData [c("subset", "question_code", "answer")]
		subset.values <- sqldf("select distinct subset SubsetValue from 'subsetData'");
		subset.values[,"Subset"] <- subset
		subset.values <- subset.values[c("Subset","SubsetValue")]

		if(subset == names(data)[1]){
			subsets.values <- subset.values;
		}else{
			subsets.values  <- merge(subsets.values , subset.values , all = TRUE)
		}

	}

	#subsets.values <- sqldf("select * from 'subsets.values'"); # order by Subset * 1, CASE WHEN subsetValue = 'All' THEN 0 END DESC

	subsets.values <- smartSoundOrderingBy(subsets.values, c("Subset", "SubsetValue"))

	#--------------------------------------
	print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------
	#--------------------------------------
	print(paste(" | surveyToPHPValidationCode :", survey, " - Find distinct questions - answers combinations from statistical data"))
	print(paste(" | surveyToPHPValidationCode :", survey, " -  Questions, QuestionMeans, Questions CountryMean"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	# gather all questions - answers combinations
	# all subsets should have the same question answers combinations
	# this has been verified per subsetData
	# use the first

	questionsAnswers <- c()

	{
		subset <- names(data)[1]

		subsetData <- data[[subset]]
		subsetData <- subsetData [c("question_code", "answer")]

		#gather questions - answers
		questions <- sqldf(
			"select question_code, answer from 'subsetData'
				inner join 'question.codes.type' qTypes on qTypes.Column = 'subsetData'.question_code and Type in ('Question', 'QuestionMR')
				where ColumnAs <> 'CountryName'
				group by question_code, answer
			"
		)

		questionsMeans <- sqldf(
			"
			select question_code, answer from 'subsetData'
				inner join 'question.codes.type' qTypes on qTypes.Column = 'subsetData'.question_code  and Type = 'QuestionMean'
				group by question_code
			"
		)
		
		if(nrow(questionsMeans)>0){
			questionsMeans$answer <- "Mean"
			questionsAnswers <- merge(questions, questionsMeans, all = TRUE)
		}else{
			questionsAnswers <- questions
		}

		#gather questions - answers
		questionsCountryName <- sqldf(
			"select distinct question_code, 'CountryMean' answer from 'subsetData'
				inner join 'question.codes.type' qTypes on qTypes.Column = 'subsetData'.question_code and Type = 'Question' and ColumnAs = 'CountryName'
				group by question_code, answer
			"
		)
		if(nrow(questionsCountryName)>0){
			questionsCountryName$answer <- "CountryMean"
			questionsAnswers <- merge(questionsAnswers, questionsCountryName, all = TRUE)
		}
	}
	#--------------------------------------
	print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	print(paste(" | surveyToPHPValidationCode :", survey, " - generate gathered data to php"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	result <- paste(
		"<?php",
		"\n//Auto Generated file",
		"\n\t$validParameterCombinations = array( ",
		questionsAnswersToIndexedPHPArray(questionsAnswers), ",",
		subsetsToIndexedPHPArray(subsets.values), ",",
		countriesToIndexedPHPArray(countries),
		"\n\t);",
	sep = "")

	#--------------------------------------
	print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	print("------------------------------------------")
	print(paste(" * DVS Validation Layer PHP model generation. :", survey, " - COMPLETED"))
	print(paste(" *  Time Needed :", (proc.time()[3] - function.starting.time) , "seconds"))
	print("------------------------------------------")
	#--------------------------------------

	return(result)
}

#surveyToPHPValidationCode("3RDEQLS")
#survey = "3RDEQLS"

