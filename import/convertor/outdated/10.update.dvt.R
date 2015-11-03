# ======================================================================
# Eworx S.A.- 2012
# Author kp@eworx.gr
# ======================================================================
# Contains the functions to update the DVT
# ======================================================================


# ======================================================================

surveyUpdateDVT <- function(survey, locale){

	print("------------------------------------------")
	print(paste(" * DVT Model population for", survey, locale, " - STARTED"))
	function.starting.time <- proc.time()[3]
	print("------------------------------------------")

	#--------------------------------------
	print(paste(" | surveyUpdateDVT :", survey, " - Loading required information"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------
	print(paste(" | surveyUpdateDVT :", survey, locale, " - Loading localizations"))
	print(paste(" | surveyUpdateDVT :", survey, " -  Loading Answers, Subsets, SubsetValues"))

	survey.folder <- getSurveyFolder(survey)
	pattern.with.wildchar <- paste(survey.folder, survey, ".", step3.values.input.file.name.pattern, sep = "" )
	local.file <- gsub(pattern = "\\*", replacement = locale,  pattern.with.wildchar)

	if(!file.exists(local.file) ){
		print(paste("Localization file does not exist :", local.file))
		return("");
	}else{
		print(paste(" |      -", local.file))
	}

	values.localization <- readXls(local.file)

	print(paste(" | surveyUpdateDVT :", survey, " -  Loading Questions"))

	pattern.with.wildchar <- paste(survey.folder, survey, ".", step3.questions.input.file.name.pattern, sep = "" )
	local.file <- gsub(pattern = "\\*", replacement = locale,  pattern.with.wildchar)

	if(!file.exists(local.file) ){
		print(paste("Localization file does not exist :", local.file))
		return("");
	}else{
		print(paste(" |      -", local.file))
	}

	questions.localization <- readXls(local.file)

	#--------------------------------------
	print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------
	#--------------------------------------
	print(paste(" | surveyUpdateDVT :", survey, " - Loading statistical data and Questions and Question Means columns "))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	data <- loadSurveyStatistics(survey)

	question.codes.type <- readXls(paste(getSurveyFolder(survey), survey, ".", step1.input.file.name, sep = ""));
	question.codes.type <- question.codes.type[c("Column", "Type")]
	question.codes.type <- sqldf("select * from 'question.codes.type' where Type in ('Question', 'QuestionMean')")

	#--------------------------------------
	print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------
	#--------------------------------------
	print(paste(" | surveyUpdateDVT :", survey, " - Find distinct Subset - SubsetValue combinations from statistical data"))
	print(paste(" | surveyUpdateDVT :", survey, " - Find distinct countries "))
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
			countries <-  unique(subsetData$CountryCode)
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

	subsets.values <- sqldf("select * from 'subsets.values' order by Subset, CASE WHEN subsetValue = 'All' THEN 0 END DESC");

	#--------------------------------------
	print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------
	#--------------------------------------
	print(paste(" | surveyUpdateDVT :", survey, " - Find distinct questions - answers combinations from statistical data"))
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
				inner join 'question.codes.type' qTypes on qTypes.Column = 'subsetData'.question_code and Type = 'Question'
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

		questionsMeans$answer <- "Mean"

		questionsAnswers <- merge(questions, questionsMeans, all = TRUE)
	}
	#--------------------------------------
	print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	print(paste(" | surveyUpdateDVT :", survey, locale, " - Cross join Keys with Translations to DVT Model data"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	question.answers.localized <- sqldf("select question_code QuestionCode, answer, TranslatedValue  from questionsAnswers qa inner join 'values.localization' vl on (vl.Column = qa.question_code and vl.Value = qa.answer)")
	# REMOVE INDEX FROM TRANSLATED VALUE
	question.answers.localized[, "NoIndex"] <- clearIndex(question.answers.localized$TranslatedValue)
	question.answers.localized <- question.answers.localized[c("QuestionCode", "answer", "NoIndex" )]
	names(question.answers.localized)[names(question.answers.localized) == "NoIndex"] <- "TranslatedValue"
	# ANSWER KEYS to CLEAN URL KEYS
	question.answers.localized[, "CLEANEDKEY"] <- toUrlValue(question.answers.localized$answer)
	question.answers.localized <- question.answers.localized[c("QuestionCode", "TranslatedValue", "CLEANEDKEY" )]
	names(question.answers.localized)[names(question.answers.localized) == "CLEANEDKEY"] <- "answer"
	question.answers.localized <- question.answers.localized[c("QuestionCode", "answer", "TranslatedValue" )]


	questions.localized <- sqldf("select QuestionCode, WebTranslation TranslatedValue from 'questions.localization'")
	questions.meta <- sqldf("select QuestionCode, FullTranslation, Note  from 'questions.localization'")

	subsets.localized <- sqldf("select distinct Subset from 'subsets.values' ")
	subsets.localized <- sqldf("select Subset, TranslatedValue  from 'subsets.localized' s inner join 'values.localization' vl on (vl.Column = s.Subset and vl.Value = s.Subset)")

	subsets.values.localized <- sqldf("select Subset, SubsetValue, TranslatedValue  from 'subsets.values' s inner join 'values.localization' vl on (vl.Column = s.Subset and vl.Value = s.SubsetValue)")

	# REMOVE INDEX FROM TRANSLATED VALUE
	subsets.values.localized[, "NoIndex"] <- clearIndex(subsets.values.localized$TranslatedValue)
	subsets.values.localized <- subsets.values.localized[c("Subset", "SubsetValue", "NoIndex" )]
	names(subsets.values.localized)[names(subsets.values.localized) == "NoIndex"] <- "TranslatedValue"
	# SubsetValue KEYS to CLEAN URL KEYS
	subsets.values.localized[, "CLEANEDKEY"] <- toUrlValue(subsets.values.localized$SubsetValue)
	subsets.values.localized <- subsets.values.localized[c("Subset", "TranslatedValue", "CLEANEDKEY" )]
	names(subsets.values.localized)[names(subsets.values.localized) == "CLEANEDKEY"] <- "SubsetValue"
	subsets.values.localized <- subsets.values.localized[c("Subset", "SubsetValue", "TranslatedValue" )]

	#--------------------------------------
	print(paste(" | surveyUpdateDVT :", survey, locale," - Populate Question Topic (Category) Sellector and add All (count questions) option"))
	#--------------------------------------


	categories.questions <- sqldf("select Category, QuestionCode, WebTranslation TranslatedValue from 'questions.localization'")
	####

	categories.all.questions.count <- sqldf("select count(distinct(QuestionCode)) Count from 'categories.questions'")

	categories.localized <- sqldf("select category, count(*) Count from 'categories.questions' group by category")
	categories.localized[, "TranslatedValue"] <- paste(categories.localized$Category, " (", categories.localized$Count, ")", sep = "")
	categories.localized <- categories.localized[c("Category", "TranslatedValue")]
	####

	categories.all <- paste(getSurveyAllTranslatedValue(survey, locale)," (", as.character(factor(categories.all.questions.count[1,1])),")", sep = "" )

	categories.localized <- sqldf(
		paste("select '' Category, '", categories.all, "' TraslatedValue union  select * from 'categories.localized'", sep = "")
	)

	countries <- data.frame(countries)

	#------------------ dvt
	print(paste(" | surveyUpdateDVT :", survey, locale, " - USE Countries Localizations"))

	countries.translated <- getSurveyTranslatedValues(survey, locale)
	countries.translated <- sqldf("select Value CountryCode, 	TranslatedValue CountryName from 'countries.translated' ct where ct.Column = 'country' ")

	countries.localized <- sqldf("select  CountryCode, CountryName TranslatedValue  from 'countries.translated' c inner join countries on c.CountryCode = countries.countries " )


	#--------------------
	print(paste(" | surveyUpdateDVT :", survey, locale, " - Populate DVT Titles"))

	DVT.translations <- getSurveyTranslatedValues(survey, locale)
	DVT.translations <- sqldf("select * from 'DVT.translations' where EntryType = '_DVT'")
	dvt.titles <- sqldf("select 'survey' Value, TranslatedValue from 'DVT.translations' where Column = 'survey'")

	dvt.titles   <- merge(dvt.titles , sqldf("select Value, TranslatedValue from 'DVT.translations' where Column = 'Title'") , all = TRUE)


	print(paste(" | surveyUpdateDVT :", survey, locale, " - Populate DVT Link Titles"))
	dvt.link.titles <- sqldf("select Value, TranslatedValue from 'DVT.translations' where Column == 'Link' ")

	print(paste(" | surveyUpdateDVT :", survey, locale, " - Populate DVT Fields Labels"))
	dvt.labels <- sqldf("select Value, TranslatedValue from 'DVT.translations' where Column == 'Label' ")

	print(paste(" | surveyUpdateDVT :", survey, locale, " - Populate DVT Tootips"))
	dvt.tooltips <- sqldf("select Value, TranslatedValue from 'DVT.translations' where Column == 'Tooltip' ")

	print(paste(" | surveyUpdateDVT :", survey, locale, " - Populate DVT Visualizations Count per visualization function"))
	visualizationsCount <- sqldf("select Value, TranslatedValue from 'DVT.translations' where Column == 'visualizationsCount' ")

	#---------
	print(paste(" | surveyUpdateDVT :", survey, locale, " - Populate DVT Visualizations Functions key - values"))
	DVT.selectors <- getSurveyTranslatedValues(survey, locale)
	DVT.selectors.plots <- sqldf("select  Value, TranslatedValue from 'DVT.selectors' where EntryType = '_Selector' and Column = 'plot' order by TranslatedValue")
	# REMOVE INDEX FROM TRANSLATED VALUE
	DVT.selectors.plots[, "NoIndex"] <- clearIndex(DVT.selectors.plots$TranslatedValue)
	DVT.selectors.plots <- DVT.selectors.plots[c("Value", "NoIndex" )]
	names(DVT.selectors.plots)[names(DVT.selectors.plots) == "NoIndex"] <- "TranslatedValue"

	#---------
	DVT.selectors.view.data <- sqldf("select  Value, TranslatedValue from 'DVT.selectors' where EntryType = '_Selector' and Column = 'exportType' and value = 'csv'")

 	print(paste(" | surveyUpdateDVT :", survey, locale, " -  -> subsets.localized"))
	print(paste(" | surveyUpdateDVT :", survey, locale, " -  -> subsets.values.localized"))
	print(paste(" | surveyUpdateDVT :", survey, locale, " -  -> categories.localized"))
	print(paste(" | surveyUpdateDVT :", survey, locale, " -  -> categories.questions"))
	print(paste(" | surveyUpdateDVT :", survey, locale, " -  -> questions.localized"))
	print(paste(" | surveyUpdateDVT :", survey, locale, " -  -> question.answers.localized"))
	print(paste(" | surveyUpdateDVT :", survey, locale, " -  -> countries.localized"))
	print(paste(" | surveyUpdateDVT :", survey, locale, " -  -> questions.meta"))

	print(paste(" | surveyUpdateDVT :", survey, locale, " -  -> dvt.titles"))
	print(paste(" | surveyUpdateDVT :", survey, locale, " -  -> dvt.link.titles"))
	print(paste(" | surveyUpdateDVT :", survey, locale, " -  -> dvt.labels"))
	print(paste(" | surveyUpdateDVT :", survey, locale, " -  -> dvt.tooltips"))
	print(paste(" | surveyUpdateDVT :", survey, locale, " -  -> visualizationsCount"))

	print(paste(" | surveyUpdateDVT :", survey, locale, " -  -> DVT.selectors.plots"))
	print(paste(" | surveyUpdateDVT :", survey, locale, " -  -> DVT.selectors.view.data"))


	#--------------------------------------
	print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------


	#--------------------------------------
	print(paste(" | surveyUpdateDVT :", survey, locale," - translate data to php"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	phpResult <- paste(
		"<?php",
		"\n//Auto Generated file",
			# not needed for current version
			# will be neded for a totaly no js version if it will be requested
			# commented for now of optimization reasons - we use the js version
			#toPHPArray(subsets.localized, "$subset_"),
			#toPHPArray(subsets.values.localized, "$subset_subsetValue"),
			#toPHPArray(categories.localized, "$topic_"),
			#toPHPArray(categories.questions, "$topic_question"),
			#toPHPArray(questions.localized, "$question_"),
			#toPHPArray(question.answers.localized, "$question_answer"),
			toPHPArray(countries.localized, "$country_"),
			#toPHPArray(questions.meta, "$question_meta_"),

			toPHPArray(dvt.titles, "$titles_"),
			toPHPArray(dvt.link.titles, "$link_titles"),
			toPHPArray(dvt.labels, "$labels_"),
			toPHPArray(dvt.tooltips, "$tooltips_"),
			toPHPArray(visualizationsCount, "$visualizationsCount_"),

			toPHPArray(DVT.selectors.plots, "$plots_"),
			toPHPArray( DVT.selectors.view.data, "$dataViews_")

	,sep = "\n")

	#-------

	out.file <- paste(DVT.model.folder, survey, "/php/", locale,".php", sep = "")
	con <- file(out.file)
	writeLines(c(phpResult), con)
	close(con)
	print(paste(" | surveyUpdateDVT :", survey, locale," - New DVT PHP model Published :", out.file  ))

	#--------------------------------------
	print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	out.file <- paste(RRML.validation.layer, survey,".", valid.survey.locales.php, sep = "")
	print(paste(" | surveyUpdateDVT :", survey, locale, " - Updating Validation Layer Survey Locales : ", out.file))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	PHPVLresult <- paste(listAvailableTranslations(survey), collapse = ",")
	PHPVLresult <- gsub(pattern = ",", replacement = "', '",  PHPVLresult)
	PHPVLresult <- paste("'", PHPVLresult, "'", sep = "")
	PHPVLresult <- paste("\n$surveyLocales = array(\n\t\t\t", PHPVLresult , "\n\t\t);", sep = "")

	PHPVLresult <- paste(
		"<?php",
		"\n//Auto Generated file",
		PHPVLresult
	,sep = "\n")


	con <- file(out.file)
	writeLines(c(PHPVLresult), con)
	close(con)
	print(" - New validation layer Published")

	#--------------------------------------
	print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------


	#--------------------------------------
	print(paste(" | surveyUpdateDVT :", survey, locale, " - translate data to javascript"))
	function.block.starting.time <- proc.time()[3]
	#--------------------------------------

	jsResult <- paste(
		"//Auto Generated file",
			toJavascriptArray(subsets.localized, "var subset_"),
			toJavascriptArray(subsets.values.localized, "var subset_subsetValue"),
			toJavascriptArray(categories.localized, "var topic_"),
			toJavascriptArray(categories.questions, "var topic_question"),
			toJavascriptArray(questions.localized, "var question_"),
			toJavascriptArray(question.answers.localized, "var question_answer"),
			toJavascriptArray(countries.localized, "var country_"),
			toJavascriptArray(questions.meta, "var question_meta"),
	sep = "\n")

	#-------

	out.file <- paste(DVT.model.folder, survey, "/js/", locale,".js", sep = "")
	con <- file(out.file)
	writeLines(c(jsResult), con)
	close(con)
	print(paste(" | surveyUpdateDVT :", survey, locale," - New DVT JS model Published :", out.file  ))


	#--------------------------------------
	print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
	#--------------------------------------

	#--------------------------------------
	print("------------------------------------------")
	print(paste(" * DVT Model population for", survey, locale, " - COMPLETED"))
	print(paste(" *  Time Needed :", (proc.time()[3] - function.starting.time) , "seconds"))
	print("------------------------------------------")
	#--------------------------------------

}

#surveyUpdateDVT("3RDEQLS","EN")

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

#toJavascriptArray(question.answers.localized, "categories")


