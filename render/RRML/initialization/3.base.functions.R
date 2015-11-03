# ======================================================================
# Eworx S.A.- 2012
# Author kp@eworx.gr
# ======================================================================
# Shared functions


##if columns.to.be.selected and columns.new.names are both c(), then function is obvisouly not working
inner.join.function <- function(columns.to.be.selected,A,B,columns.to.join.from.A,columns.to.join.from.B,columns.new.names = c()){
# 'A','B' are the selected dataframes to be joined 
    result <- merge(A,B,by.x = columns.to.join.from.A,by.y = columns.to.join.from.B,sort = FALSE)
    if(length(columns.to.be.selected != 0))   result <- as.data.frame(result[,columns.to.be.selected])
    
    if(length(columns.new.names) != 0)	names(result) = columns.new.names 
    else if(length(columns.to.be.selected) == 1)	names(result) = columns.to.be.selected

    return(result)
}

##if columns.to.be.selected and new.names are both c(), then function is not working
left.outer.join.function <- function(columns.to.be.selected,A,B,columns.to.join.from.A,columns.to.join.from.B,columns.new.names = c()){
# 'A','B' are the selected dataframes to be joined 
    result <- merge(A,B,by.x = columns.to.join.from.A,by.y = columns.to.join.from.B,sort = FALSE,all.x = TRUE,incomparables = NA)
    if(length(columns.to.be.selected != 0))   result <- as.data.frame(result[,columns.to.be.selected])
    else	result <- as.data.frame(result[,])
    
    if(length(columns.new.names) != 0)	names(result) = columns.new.names 
    else if(length(columns.to.be.selected) == 1)	names(result) = columns.to.be.selected

    return(result)
}

##columns.names and columns.values must have the same length,or be both empty
simple.select.function <- function(columns.to.be.selected,array,columns.names,columns.values){
# 'array' is the dataframe, from which we want to filter
	result <- array
	if( (length(columns.names) != 0) || (length(columns.values) != 0) ){
		for(i in (1:length(columns.names))){
			col <- result[,columns.names[i]]
			result <- result[col == columns.values[i],]
		}
	}
	if(length(columns.to.be.selected) != 0){
		result <- as.data.frame(result[,columns.to.be.selected])
		names(result) <- columns.to.be.selected
	}
    else	result <- as.data.frame(result[,])
    
	return(result)
}


readXls <- function(file){
  return(read.csv(file = file, head = TRUE, sep = "\t" ))#, encoding = "UTF-8"))
}

writeDataInCSV <- function(data, file){
  write.table(data, file, sep = "\t", na = "", dec = ",", row.names = F)
}

getSurveyFolder <- function(survey){
  return(paste(converter.base.path, survey, "/", sep = ""))
}

countries.code.name <- readXls(paste(converter.base.path, "../convertor/", countries.code.name.status.file.name, sep = ""))


# ======================================================================

timeSeriesExists <- function(survey){
	return(file.exists(paste(getSurveyFolder(survey), survey, ".", time.series.input.file.name, sep = "")))
}

getTimeSeriesSurveys <- function(survey){	
	year.survey <- readXls(paste(getSurveyFolder(survey), survey, ".", time.series.input.file.name, sep = ""))
	return(year.survey)
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

covertQuestionMeanStatsToMeans <- function(stat.data){
		stat.data.question.mean <- stat.data
		stat.data.question.mean[,"numericAnswerTimesAnswer"] <- as.numeric(stat.data.question.mean$answer) * stat.data.question.mean$answers
		stat.data.question.mean <- sqldf(" select CountryEUStatus, CountryCode, question_code, subset, 'Mean' answer, answers, subset_answers, (sum(numericAnswerTimesAnswer) / subset_answers) percentage from 'stat.data.question.mean' group by CountryEUStatus, CountryCode, question_code, subset");
		return(stat.data.question.mean)
}

# ======================================================================

getSurveyTranslatedValues <- function(survey, locale){
	pattern.with.wildchar <- paste(getSurveyFolder(survey), survey, ".", step3.values.input.file.name.pattern, sep = "" )
	local.file <- gsub(pattern = "\\*", replacement = locale,  pattern.with.wildchar)
	return(readXls(local.file))
}

# --------------------------------------------

getSurveyQuestionTranslatedAnswers <- function(data, question){
	result <- data
	#previous slower
	#result <- sqldf(paste("select Value, TranslatedValue from result where EntryType = 'Answer' and Column ='", question, "'", sep = ""));
	#optimized code#
	result <- simple.select.function(c("Value","TranslatedValue"),result,c("EntryType","Column"),c("Answer",question));
	#optimized code#
	return(result)
}

# getSurveyQuestionTranslatedAnswers(getSurveyTranslatedValues("3RDEQLS", "EN"), "Y11_Q12a" )

# --------------------------------------------

getSurveyQuestionTranslatedAnswer <- function(data, question, answer){
	result <- data
	#previous slower
	#result <- sqldf(paste("select TranslatedValue from result where EntryType = 'Answer' and Column ='", question, "' and Value = '", answer, "'", sep = ""));
	#optimized code#
	result <- simple.select.function(c("TranslatedValue"),result,c("EntryType","Column","Value"),c("Answer",question,answer));
	#optimized code#
	return(as.character(factor(result[1, 1])))
}

#getSurveyQuestionTranslatedAnswer(getSurveyTranslatedValues("3RDEQLS", "EN"), "Y11_Q23a", "1. Yes" )

# --------------------------------------------

getSurveyPlotDescriptorTemplate <- function(data, plot, isQuestionMean_param = FALSE){
	result <- data
	#previous slower 
	#result <- sqldf(paste("select TranslatedValue from result where EntryType = '_DVS' and Column = '", plot, "' and Value = '", ifelse(isQuestionMean_param, "mean", "percentage"), "'", sep = ""));
	#optimized code#
	if(isQuestionMean_param){
		result <- simple.select.function(c("TranslatedValue"),result,c("EntryType","Column","Value"),c("_DVS",plot,"mean"));
	}
	else{
		result <- simple.select.function(c("TranslatedValue"),result,c("EntryType","Column","Value"),c("_DVS",plot,"percentage"));
	}
	#optimized code#
	return(as.character(factor(result[1, 1])))
}

# getSurveyPlotDescriptorTemplate(getSurveyTranslatedValues("3RDEQLS", "EN"), "heatMap" )

# --------------------------------------------

getSurveyWord <- function(data, word ){
	result <- data
	#previous slower
	#result <- sqldf(paste("select TranslatedValue from result where EntryType = '_DVS' and Column = 'word' and Value = '", word, "'", sep = ""));
	#optimized code#
	result <- simple.select.function(c("TranslatedValue"),result,c("EntryType","Column","Value"),c("_DVS","word",word));
	#optimized code#
	return(as.character(factor(result[1, 1])))
}

# getSurveyWord(getSurveyTranslatedValues("3RDEQLS", "EN"), "above" )

# --------------------------------------------

getSurveyCountriesTranslations <- function(data){
	result <- data
	#previous slower
	#result <- sqldf(paste("select Value, TranslatedValue from result where EntryType = '_Selector' and Column = 'country'", sep = ""));
	#optimized code#
	result <- simple.select.function(c("Value","TranslatedValue"),result,c("EntryType","Column"),c("_Selector","country"));
	#optimized code#
	return(result)
}

#getSurveyCountriesTranslations(getSurveyTranslatedValues("3RDEQLS", "EN"))

# --------------------------------------------

getSurveyCountryTranslation <- function(data, country){
	result <- data
	#previous slower
	#result <- sqldf(paste("select TranslatedValue from result where EntryType = '_Selector' and Column = 'country' and Value ='", country, "'", sep = ""));
	#optimized code#
	result <- simple.select.function(c("TranslatedValue"),result,c("EntryType","Column","Value"),c("_Selector","country",country));
	#optimized code#
	return(as.character(factor(result[1, 1])))
}

#getSurveyCountryTranslation(getSurveyTranslatedValues("3RDEQLS", "EN"), "EL")

# --------------------------------------------

getSurveyLabel <- function(data, word ){
	result <- data
	#previous slower
	#result <- sqldf(paste("select TranslatedValue from result where EntryType = '_DVS' and Column = 'word' and Value = '", word, "'", sep = ""));
	#optimized code#
	result <- simple.select.function(c("TranslatedValue"),result,c("EntryType","Column","Value"),c("_DVS","word",word));
	#optimized code#
	return(as.character(factor(result[1, 1])))
}


# --------------------------------------------

getSurveyTranslatedValue <- function(data){
	result <- data
	#previous slower
	#result <- sqldf(paste("select TranslatedValue from result where EntryType = '_DVT' and Column = 'survey'", sep = ""));
	#optimized code#
	result <- simple.select.function(c("TranslatedValue"),result,c("EntryType","Column"),c("_DVT","survey"));
	#optimized code#
	return(as.character(factor(result[1, 1])))
}

#getSurveyTranslatedValue(getSurveyTranslatedValues("3RDEQLS", "EN"))

# --------------------------------------------

getSurveyLabel <- function(data, label ){
	result <- data
	#previous slower
	#result <- sqldf(paste("select TranslatedValue from result where EntryType = '_DVT' and Column = 'Label' and Value = '", label, "'", sep = ""));
	#optimized code#
	result <- simple.select.function(c("TranslatedValue"),result,c("EntryType","Column","Value"),c("_DVT","Label",label));
	#optimized code#
	return(as.character(factor(result[1, 1])))
}

# getSurveyLabel(getSurveyTranslatedValues("3RDEQLS", "EN"), "country" )

# --------------------------------------------

getSubsetTranslation <- function(data, subset ){
	result <- data
	#previous slower
	#result <- sqldf(paste("select TranslatedValue from result where EntryType = 'Subset' and Column  = '", subset, "' and Value = '", subset, "'", sep = ""));
	#optimized code#
	result <- simple.select.function(c("TranslatedValue"),result,c("EntryType","Column","Value"),c("Subset",subset,subset));
	#optimized code#
	return(as.character(factor(result[1, 1])))
}

#getSubsetTranslation(getSurveyTranslatedValues("3RDEQLS", "EN"), "Y11_Agecategory" )

# --------------------------------------------

getSubsetValuesTranslation <- function(data, subset){
	result <- data
	#previous slower
	#result <- sqldf(paste("select Value, TranslatedValue from result where EntryType = 'SubsetValue' and Column  = '", subset, "'", sep = ""));
	#optimized code#
	result <- simple.select.function(c("Value","TranslatedValue"),result,c("EntryType","Column"),c("SubsetValue",subset));
	#optimized code#
	return(result)
}

# getSubsetValuesTranslation(getSurveyTranslatedValues("3RDEQLS", "EN"), "Y11_Agecategory" )

getSubsetValueTranslation <- function(data, subset, subsetValue ){
	result <- data
	#previous slower
	#result <- sqldf(paste("select TranslatedValue from result where EntryType = 'SubsetValue' and Column  = '", subset, "' and Value='",subsetValue ,"'", sep = ""));
	#optimized code#
	result <- simple.select.function(c("TranslatedValue"),result,c("EntryType","Column","Value"),c("SubsetValue",subset,subsetValue));
	#optimized code#
	return(as.character(factor(result[1, 1])))
}

# getSubsetValueTranslation(getSurveyTranslatedValues("3RDEQLS", "EN"), "Y11_Agecategory" )


# --------------------------------------------

getSurveyTranslatedQuestions <- function(survey, locale){
	pattern.with.wildchar <- paste(getSurveyFolder(survey), survey, ".", step3.questions.input.file.name.pattern, sep = "" )
	local.file <- gsub(pattern = "\\*", replacement = locale,  pattern.with.wildchar)
	return(readXls(local.file))
}

# --------------------------------------------

getSurveyFullTranslatedQuestion <- function(survey, locale, question_code){
	result <- getSurveyTranslatedQuestions(survey, locale)
	#previous slower
	#result <- sqldf(paste("select FullTranslation from result where QuestionCode  = '", question_code, "'", sep = ""));
	#optimized code#
	result <- simple.select.function(c("FullTranslation"),result,c("QuestionCode"),c(question_code));
	#optimized code#
	return(as.character(factor(result[1, 1])))
}

#getSurveyFullTranslatedQuestion("3RDEQLS", "EN", "Y11_Q38" )

# --------------------------------------------

getSurveyTranslatedQuestion <- function(survey, locale, question_code){
	result <- getSurveyTranslatedQuestions(survey, locale)
	#previous slower
	#result <- sqldf(paste("select WebTranslation from result where QuestionCode  = '", question_code, "'", sep = ""));
	#optimized code#
	result <- simple.select.function(c("WebTranslation"),result,c("QuestionCode"),c(question_code));
	#optimized code#
	return(as.character(factor(result[1, 1])))
}

#getSurveyTranslatedQuestion("3RDEQLS", "EN", "Y11_Q38" )

getSurveyTranslatedQuestionTopic <- function(survey, locale, question_code){
	result <- getSurveyTranslatedQuestions(survey, locale)
	#previous slower
	#result <- sqldf(paste("select Category from result where QuestionCode  = '", question_code, "'", sep = ""));
	#optimized code#
	result <- simple.select.function(c("Category"),result,c("QuestionCode"),c(question_code));
	#optimized code#
	return(as.character(factor(result[1, 1])))
}

# ======================================================================

fillDescription <- function(description, desciption.values){
	count <- 0;
	for(desciption.value in desciption.values){
		count <- count + 1
		description <- gsub(paste("%", count, sep = ""), desciption.value, description)
	}
	return(description)
}

#fillDescription("In above figure, we see the percentage of the people that answered %1 for the %2 %3 when questioned %4 per country.  The first countries with the highest value are %5 and the ones with the lowest are %6.", c("blah", "222","3333","444" , "555","66666") )

# ======================================================================

replaceValuesInDataframe <- function(data, column, value.translated ){
		for(row in 1:nrow(value.translated)){
			column.current.value <- as.character( value.translated[row, "Value"] )
			column.new.value <- as.character( value.translated[row, "TranslatedValue"] )
			if(column.current.value != column.new.value){
				if(!(column.new.value %in% levels(data[[column]]))){
					levels(data[[column]]) <- c(levels(data[[column]]), column.new.value)
				}
				data[data[[column]] == column.current.value, column] <- column.new.value
			}
	}
	return(data)
}

# replaceValuesInDataframe(data, column, value.translated )

# ======================================================================


translateStatDataValues <- function(stat.data, translated.values , subset, param_question_code){
	for(stat.data.column in names(stat.data)){
		if(stat.data.column == "CountryEUStatus"){
			#no need to change anything
		}else
		if(stat.data.column == "CountryCode"){
			#for every country code, change the code to the translation
			stat.data <- replaceValuesInDataframe(stat.data, "CountryCode", getSurveyCountriesTranslations(translated.values) )
		}else
		if(stat.data.column == "question_code"){
			#no need to change anything
		}else
		if(stat.data.column == "subset"){
			stat.data <- replaceValuesInDataframe(stat.data, "subset", getSubsetValuesTranslation(translated.values, subset ))
		}else
		if(stat.data.column == "answer"){
			stat.data <- replaceValuesInDataframe(stat.data, "answer", getSurveyQuestionTranslatedAnswers(translated.values, param_question_code ))
		}
	}
	return(stat.data)
}

# ======================================================================

translateStatAllSubsetsDataValues <- function(stat.data, translated.values , subset, param_question_code){
	for(stat.data.column in names(stat.data)){
		if(stat.data.column == "CountryCode"){
			stat.data <- replaceValuesInDataframe(stat.data, "CountryCode", getSurveyCountriesTranslations(translated.values) )
		}else
		if(stat.data.column == "subset"){

			all.subset <- data.frame(c(1), c(1))
			all.subset[,"Value"] <- "All"
			all.subset[,"TranslatedValue"] <- "0. All"
			all.subset <- all.subset[c("Value", "TranslatedValue")]

			stat.data <- replaceValuesInDataframe(stat.data, "subset", all.subset)

			subsetValues.values <- getSubsetValuesTranslation(translated.values, subset)
			subsetValues.values[, "combined"] <- paste("", getSubsetTranslation(translated.values, subset), ", ", subsetValues.values$TranslatedValue, sep = "")
			subsetValues.values <- subsetValues.values[c("Value", "combined")]
			names(subsetValues.values)[names(subsetValues.values) == "combined"] <- "TranslatedValue"
			stat.data <- replaceValuesInDataframe(stat.data, "subset", subsetValues.values )

		}else
		if(stat.data.column == "answer"){
			stat.data <- replaceValuesInDataframe(stat.data, "answer", getSurveyQuestionTranslatedAnswers(translated.values, param_question_code ))
		}
	}
	return(stat.data)
}


# ---------------------------------------------------------

clearIndex <- function(value){
	return(gsub("^[0-9]\\. ", "", value))
}

clearIndexInString <- function(value){
	return(gsub("[0-9]\\. ", "", value))
}

clearIndexInColumnStrings <- function(stat.data, columnToRelabel = "subset"){
	stat.data[, columnToRelabel] <- factor(
		stat.data [, columnToRelabel],  
		levels = levels(factor(stat.data [, columnToRelabel]) ),
		labels = clearIndexInString( levels(factor(stat.data [, columnToRelabel]) ) )
	)
	return(stat.data)
}

toUrlValue <- function(value){
	return(gsub("[^a-zA-Z0-9]", "-", value));
}

# ---------------------------------------------------------

toPHPArray <- function(data, variable.name, wrap.before = " = array(", wrap.after = ");", before = "\n\t\tarray(", after = ")"){

	result <- ""

	for(row.index in 1:nrow(data)){
		row = data[row.index,]
		result.temp <- c()

		for(row.column.index in 1:length(names(row))){
			result.temp <- c(result.temp, as.character(factor(row[1, row.column.index])))
		}

		result.temp <- paste(result.temp , collapse = ",,,")
		result.temp <- gsub(pattern = "'", replacement = "",  result.temp )
		result.temp <- gsub(pattern = ",,,", replacement = "', '",  result.temp )
		result.temp <- paste(before, "'", result.temp, "'" , after, sep = "")
		result <- paste(result, result.temp, ifelse(row.index<nrow(data),",", ""), sep = "")
	}

	result <- paste(variable.name, wrap.before, result, wrap.after, sep = "\n\t")

	return(result)
}

#toPHPArray(categories.localized, "categories")

toJavascriptArray <- function(data, variable.name){
	return(toPHPArray(data, variable.name, wrap.before = " = [", wrap.after = "];", before = "\n\t\t[", after = "]"))
}

setValueInCell <- function(data, column, row, value){
	if(!(value %in% levels(data[, column])))
		levels(data[, column]) <- c(levels(data[, column]), value)
	data[row, column] <- value
	return(data)
}

#http://stackoverflow.com/questions/2547402/standard-library-function-in-r-for-finding-the-mode

estimate_mode <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]
}

clusterResults <- function(data, clustersCount = 3, method = "complete"){
	return(cutree(hclust(dist(data), method = method), clustersCount))

	#http://svn.r-project.org/R/trunk/src/library/stats/R/hclust.R
	# "ward", "single", "complete", "average", "mcquitty", "median" or "centroid".
	## 1. Ward's minimum variance or error sum of squares method.
	## 2. single linkage or nearest neighbor method.
	## 3. complete linkage or diameter.
	## 4. average linkage, group average, or UPGMA method.
	## 5. McQuitty's or WPGMA method.
	## 6. median, Gower's or WPGMC method.
	## 7. centroid or UPGMC method (7).

	#http://en.wikipedia.org/wiki/Ward%27s_method
	#http://en.wikipedia.org/wiki/Single-linkage_clustering
	#http://en.wikipedia.org/wiki/Complete-linkage_clustering
	#average http://www.statistics.com/index.php?page=glossary&term_id=714
	#mcquitty 1960

	#http://v8doc.sas.com/sashtml/stat/chap23/sect12.htm
}

prePendZeroInNumber <- function(data){
 	return(gsub(pattern = "^([1-9])$", replacement = "0\\1",  gsub(pattern = "^([0-9]){1}\\.", replacement = "0\\1.\\2",   data )))
}


#--------------------------- collective wage bargaining base/util functions

covertTableVariablesIntoRows <- function(stat.data){
	# stat.data expected as a data.frame with at least two columns
	# "multiply" all columns into rows by the first Column

	result = data.frame()
	columnNames = names(stat.data)

	columnNoData <- 0
	for(columnIndex in 2:length(columnNames)){		
		temp.table <- stat.data[, c(columnNames[1], columnNames[columnIndex])]		
		temp.table <- temp.table[complete.cases(temp.table),]
		if(nrow(temp.table) == 0){
			columnNoData <- columnNoData +1
			next
		}
		
		names(temp.table)[2] <- "Value"
		temp.table[, "Variable"] <- columnNames[columnIndex]
		if((columnIndex - columnNoData) == 2)
			result = temp.table
		else
			result = merge(result, temp.table, all = TRUE)
	}

	return(result)
}