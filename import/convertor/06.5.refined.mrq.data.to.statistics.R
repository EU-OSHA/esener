# ======================================================================
# Eworx S.A. - 2012 / 2013
# Author kp@eworx.gr
# ======================================================================
# Contains the functions that convert the refined.survey.data to statistics
# and the refined.survey.mrq.data to statistics
# ======================================================================


#test <- loadRefinedSurvey("3RDEQLS")
#Survey.3RD-EQLS <- list(refinedData = test, statisticsData = test )

convertMRQSurveyToStatistics <- function(survey){

  if(
    file.exists(paste(getSurveyFolder(survey), survey.refined.mrq.data.file.name, sep = "")) &&
    file.exists(paste(getSurveyFolder(survey), survey, ".", step1.input.file.name, sep = ""))
  ){

    print("------------------------------------------")
    print("------------------------------------------")
    print(paste(" * convertMRQSurveyToStatistics :", survey, " - STARTED"))
    function.starting.time <- proc.time()[3]
    print("------------------------------------------")
    print("------------------------------------------")

    #--------------------------------------
    print(paste(" | convertMRQSurveyToStatistics :", survey, " - Loading required information"))
    function.block.starting.time <- proc.time()[3]
    #--------------------------------------

    survey.refined.data <- loadRefinedMRQSurvey(survey)
    columns <- names(survey.refined.data)

    ### ###  ###  ###  ###  ###  ###  ###  ### ###

    column.types <- readXls(paste(getSurveyFolder(survey), survey, ".", step1.input.file.name, sep = ""))
    column.types[is.na(column.types$ColumnAs), "ColumnAs" ] <- ""
    column.types <- sqldf("select * from 'column.types' where Type <> 'Delete' and Type <> 'CountryCode' and Type <> 'CountryName' and Type <> 'Question' and Type <> 'QuestionMean'") ### order by Type, Column * 1
    column.types <- smartSoundOrderingBy(column.types , c("Type", "Column"))

    columns.mrq  <- sqldf("select ColumnAs Question, Column answerColumn  from 'column.types' where Type = 'QuestionMR' ") ###order by Column * 1

    columns.mrq <- smartSoundOrderingBy(columns.mrq , c("Question", "answerColumn"))
    columns.mrq.mr  <- unique(columns.mrq$answerColumn)



    ### ###  ###  ###  ###  ###  ###  ###  ### ###

    column.CountryCode <- column.type.CountryCode

    column.weight <- sqldf("select Column from 'column.types' where Type = 'Weight' ")
    column.weight <- as.character(column.weight$Column)
    column.weight.exists <- (length(column.weight) > 0)

    columns.subsets <- sqldf("select Column from 'column.types' where Type = 'Subset' ")
    columns.subsets <- columns.subsets$Column
    columns.subsets <- as.vector(columns.subsets)

    ## Answers are treated as questions

    columns.questions <- sqldf("select Column from 'column.types' where Type = 'QuestionMR' ")
    columns.questions <- columns.questions$Column
    columns.questions <- as.vector(columns.questions)
    ### ###  ###  ###  ###  ###  ###  ###  ### ###


    countries.eu.statuses <- sqldf("select distinct CountryEUStatus from 'survey.refined.data' ");
    countries.eu.statuses <- countries.eu.statuses$CountryEUStatus
    countries.eu.statuses <- as.vector(countries.eu.statuses)

    countries.eu.statuses <- c("EuropeanUnion") # perform statistics only for EuropeanUnion. otherwise stated

    #--------------------------------------
    print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
    #--------------------------------------

    #--------------------------------------
    print(paste(" | convertMRQSurveyToStatistics :", survey, " - Generating statistics for all subsets, subset values X all questions, answers X all EU Statuses"))
    #--------------------------------------

		survey.refined.data.full <- survey.refined.data

    # for every subset
    for(subset in columns.subsets){
			#--------------------------------------
      print(paste(" | convertMRQSurveyToStatistics :", survey, " - Generating statistics for subset: ", subset))
      function.block.starting.time <- proc.time()[3]
      #--------------------------------------

			## remove non relevant subset columns - optimization 1
    	survey.refined.data <- survey.refined.data.full[,c(setdiff(names(survey.refined.data.full), columns.subsets), subset)]
			survey.refined.data.forSubset <- survey.refined.data

			#select the columns for the question, data for subset and the question.
			###############################



      questions.by.subset.stats <- data.frame()

			# NOTE question here, is actually an answer to a multiple response question, but its properties are as a yes no question.

      # for every question  per subset
      for(question in columns.questions){
      	## remove non relevant question mr codes - optimization 2
				survey.refined.data <- survey.refined.data.forSubset[, c(setdiff(names(survey.refined.data.forSubset), columns.mrq.mr), question)]
				## retain 1, retain 0.

        #--------------------------------------
        print(paste(" | convertMRQSurveyToStatistics :", survey, " -  Generating statistics for subset", subset, "for Question", question))
        subset.question.starting.time <- proc.time()[3]
        #--------------------------------------

        sqlA.join.sqlB <- paste(
          "
          Select sqlA.CountryEUStatus, sqlA.CountryCode, sqlA.question_code, sqlA.subset, sqlA.answer, sqlA.answers, sqlB.subset_answers, 0 as percentage

            from
              (
                Select CountryEUStatus, CountryCode, '", question ,"' as question_code, ", subset," as subset, ", question, " as answer, ",

                      ifelse(column.weight.exists, paste("sum(", column.weight, ")", sep = ""), "count(*)"), " as answers

                  from 'survey.refined.data' where ", question, " = 1 and ", subset, " <> 'Delete'

                  group by CountryCode, ", subset, ",", question, "
              )

              sqlA

            inner join

              (
                Select CountryCode, '", question, "' as question_code, ", subset, " as subset, ",

                       ifelse(column.weight.exists, paste("sum(", column.weight, ")", sep = ""), "count(*)"), " as subset_answers

                from  'survey.refined.data' where ", question, " > -1 and ", subset, " <> 'Delete'

                group by CountryCode, ", subset,"
              )

              sqlB

              on sqlA.CountryCode = sqlB.CountryCode  and  sqlA.question_code = sqlB.question_code  and  sqlA.subset = sqlB.subset
          "
          , sep = ""
        )

        sqlC.join.sqlD <-paste(
          "
          Select sqlC.CountryEUStatus, sqlC.CountryCode, sqlC.question_code, sqlC.subset, sqlC.answer, sqlC.answers, sqlD.subset_answers, 0 as percentage

          from
            (
              Select CountryEUStatus, CountryCode, '", question, "' as question_code, 'All' as subset,", question, " as answer, ",

                     ifelse(column.weight.exists, paste("sum(", column.weight, ")", sep = ""), "count(*)"), " as answers

                from 'survey.refined.data' where ", question, " = 1 and ", subset, " <> 'Delete'

                group by CountryCode, ", question, "
            )

            sqlC

          inner join
            (
              Select CountryCode, ",

                     ifelse(column.weight.exists, paste("sum(", column.weight, ")", sep = ""), "count(*)"), " as subset_answers

                from 'survey.refined.data' where ", question, " > -1 and ", subset, " <> 'Delete'

                group by CountryCode
            )

            sqlD

          on  sqlC.CountryCode = sqlD.CountryCode

          "
          , sep = ""
        )

        question.by.subset.stats.sql <- paste(
          sqlA.join.sqlB,
          "union all",
          sqlC.join.sqlD
        )

        question.by.subset.stats <- sqldf(question.by.subset.stats.sql)

        #--------------------------------------
        print(paste(" |  - Time Needed :", proc.time()[3] - subset.question.starting.time, "seconds"))
        #--------------------------------------
        #--------------------------------------
        print(paste(" | convertMRQSurveyToStatistics :", survey, " -  Merging question stats"))
        subset.question.starting.time <- proc.time()[3]
        #--------------------------------------
        if(question == columns.questions[1]){
          questions.by.subset.stats <- question.by.subset.stats
        }else{
          questions.by.subset.stats <- merge(questions.by.subset.stats, question.by.subset.stats, all = TRUE)
        }
        #--------------------------------------
        print(paste(" |  - Time Needed :", proc.time()[3] - subset.question.starting.time, "seconds"))
        #--------------------------------------
      }

      print("Primary Statistics for Subset completed")
      #--------------------------------------
      print(paste(" | convertMRQSurveyToStatistics :", survey, " -  Order Primary Subset Statistics"))
      subset.question.starting.time <- proc.time()[3]
      #--------------------------------------
      questions.by.subset.stats <- sqldf("select * from 'questions.by.subset.stats' ") #order by CountryEUStatus desc, CountryCode, question_code * 1, answer * 1, subset * 1
      questions.by.subset.stats <- smartSoundOrderingBy(questions.by.subset.stats, c("CountryEUStatus", "CountryCode", "question_code", "answer", "subset" ))
      #--------------------------------------
      print(paste(" |  - Time Needed :", proc.time()[3] - subset.question.starting.time, "seconds"))
      #--------------------------------------

      print("Calculate Subset Statistics for every Country EU Status using the computed Primary Subset Questions Statistics")

      eu.statuses.means.statistics <- data.frame()

      for(eu.status in countries.eu.statuses){

        #--------------------------------------
        print(paste(" | convertMRQSurveyToStatistics :", survey, " -  Calculating", eu.status, "means for subset", subset ))
        subset.question.starting.time <- proc.time()[3]
        #--------------------------------------
        eu.status.means.statistics <-
        sqldf(
          paste(
            "
              Select
              '", eu.status,"' as CountryEUStatus, '", eu.status,"' as CountryCode, question_code, subset, answer, sum(answers) as answers, sum(subset_answers) as subset_answers, 0 as percentage
              from 'questions.by.subset.stats'
              /*where CountryEUStatus = '", eu.status,"'*/
              group by question_code, subset, answer
              /*order by question_code * 1, answer * 1, subset * 1*/
            "
            , sep = ""
          )
        )

        eu.status.means.statistics <- smartSoundOrderingBy(eu.status.means.statistics, c("question_code", "answer", "subset"))

        if(eu.status == countries.eu.statuses[1]){
          eu.statuses.means.statistics <- eu.status.means.statistics
        }else{
          eu.statuses.means.statistics <- merge(eu.statuses.means.statistics, eu.status.means.statistics, all = TRUE)
        }

        #--------------------------------------
        print(paste(" |  - Time Needed :", proc.time()[3] - subset.question.starting.time, "seconds"))
        #--------------------------------------

        #--------------------------------------
        print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
        #--------------------------------------
      }

      #if more than one EU Country Status is detected calculate the EUALL status for all countries in the set
      if(FALSE)
      if(length(countries.eu.statuses)>1){
        # calculate stats for non existing status the EUALL status
        eu.status <- "EUALL"

        #--------------------------------------
        print(paste(" | convertMRQSurveyToStatistics :", survey, " -  Calculating", eu.status, "means for subset", subset ))
        subset.question.starting.time <- proc.time()[3]
        #--------------------------------------
        eu.status.means.statistics <-
          sqldf(
            paste(
              "
                Select
              '", eu.status,"' as CountryEUStatus, '", eu.status,"' as CountryCode, question_code, subset, answer, sum(answers) as answers, sum(subset_answers) as subset_answers, 0 as percentage
              from 'questions.by.subset.stats'
              group by question_code, subset, answer
              /*order by question_code * 1, answer * 1, subset * 1*/
              "
              , sep = ""
            )
          )

				eu.status.means.statistics <- smartSoundOrderingBy(eu.status.means.statistics , c("question_code", "answer", "subset"))

        eu.statuses.means.statistics <- merge(eu.statuses.means.statistics, eu.status.means.statistics, all = TRUE)

        #--------------------------------------
        print(paste(" |  - Time Needed :", proc.time()[3] - subset.question.starting.time, "seconds"))
        #--------------------------------------
      }else{
        print("Only one EU country status is detected in the set. Ommit the calculation of EUALL that would contain the means of all countries in the corpus")
      }
      #--------------------------------------
      print(paste(" | convertMRQSurveyToStatistics :", survey, " -  Merging subset Primary stats with available EU means stats"))
      subset.question.starting.time <- proc.time()[3]
      #--------------------------------------
      questions.by.subset.stats <- merge(questions.by.subset.stats, eu.statuses.means.statistics, all = TRUE)
      #--------------------------------------
      print(paste(" |  - Time Needed :", proc.time()[3] - subset.question.starting.time, "seconds"))
      #--------------------------------------

      #--------------------------------------
      print(paste(" | convertMRQSurveyToStatistics :", survey, " -  Last step calculation step for subset - calculate percentages"))
      subset.question.starting.time <- proc.time()[3]
      #--------------------------------------

      questions.by.subset.stats$percentage <- (questions.by.subset.stats$answers  * 100) / questions.by.subset.stats$subset_answers
      #--------------------------------------
      print(paste(" |  - Time Needed :", proc.time()[3] - subset.question.starting.time, "seconds"))
      #--------------------------------------

      r.statement <- paste("Subset", subset ," <- questions.by.subset.stats", sep = "")
      eval(parse(text = r.statement))

    		# here we have to see the created object.
				# if everything is ok then an inner join with the non existing mrqs  qestion codes,
				# current questions are renamed as answers, Check if Not mentioned exists in the results. Mentioned not mentioned will no longer exist.
				# and thats it.
				# ....
			 eval(parse(text = r.statement))
    }

    #wrap stats objects per subset into one survey list object
    #--------------------------------------
    print(paste(" | convertMRQSurveyToStatistics :", survey, " -  Wrapping stats per subset into a survey object and save to file"))
    subset.question.starting.time <- proc.time()[3]
    #--------------------------------------

    r.statement <- ""
    for(subset in columns.subsets){
      r.statement <- paste(
        r.statement,
        subset, " = ", "Subset", subset , ifelse(subset == columns.subsets[length(columns.subsets)], "", ",")
        , sep = ""
      )
    }

    r.statement <- paste("Survey", survey ," <- list(",r.statement, ")" , sep = "")
    eval(parse(text = r.statement))
    survey.folder <- getSurveyFolder(survey)


    r.statement <- paste("saveDataFrame(Survey", survey, ",'", survey.folder, "SurveyMRQ", survey, ".RData')", sep = "")
    eval(parse(text = r.statement))

    #--------------------------------------
    print(paste(" |  - Time Needed :", proc.time()[3] - subset.question.starting.time, "seconds"))
    #--------------------------------------

    #--------------------------------------
    print("------------------------------------------")
    print(paste(" * convertMRQSurveyToStatistics :", survey, " - STATISTICS OBJECT CREATED"))
    print(paste(" *  Time Needed :", (proc.time()[3] - function.starting.time) / 60, "minutes"))
    print("------------------------------------------")
    #--------------------------------------
		standardiseMRQStatisticsAndMergeWithStandardStats(survey)
  }else{
    print("Prior Steps have not been completed")
    if(!file.exists(paste(getSurveyFolder(survey), survey.refined.data.file.name, sep = ""))){
      print("Refined dataset does not exist")
    }
    if(!file.exists(paste(getSurveyFolder(survey), step1.input.file.name, sep = ""))){
      print("Survey Analysed Column file does not exist")
    }
  }


}

#convertMRQSurveyToStatistics("3RDEQLS")
#test<-loadSurveyStatistics("3RDEQLS")
#test<-standardiseMRQStatisticsAndMergeWithStandardStats("3RDEQLS")

standardiseMRQStatisticsAndMergeWithStandardStats <- function(survey){

    print("------------------------------------------")
    print(paste(" * standardiseMRQStatisticsAndMergeWithStandardStats :", survey, " - STARTED"))
    function.starting.time <- proc.time()[3]
    print("------------------------------------------")

		print(paste(" * standardiseMRQStatisticsAndMergeWithStandardStats :", survey, " --  Loading definitions "))

		column.types <- readXls(paste(getSurveyFolder(survey), survey, ".", step1.input.file.name, sep = ""))
		column.types[is.na(column.types$ColumnAs), "ColumnAs" ] <- ""
		column.types <- sqldf("select * from 'column.types' where Type <> 'Delete' and Type <> 'CountryCode' and Type <> 'CountryName' and Type <> 'Question' and Type <> 'QuestionMean'") ### order by Type, Column * 1

		columns.mrq.qmr  <- sqldf("select ColumnAs main_question, Column question_code  from 'column.types' where Type = 'QuestionMR'") ###
		columns.mrq.qmr  <- smartSoundOrderingBy(columns.mrq.qmr, c("main_question", "question_code"))

		substitute.columns.values <- readXls(paste(getSurveyFolder(survey), survey, ".", step2.input.file.name, sep = ""));
		mrq.answers <- sqldf(paste( "select Value question_code, Importance, NewValue answeraman from 'substitute.columns.values' where ColumnType ='",column.type.QuestionMR,"'", sep=""))
		mrq.answers[, "answer"] <- paste(prePendZeroInNumber(mrq.answers$Importance), ". ", mrq.answers$answeraman , sep = "")
		mrq.answers <- mrq.answers[, c("question_code", "answer" )]
		mrq.answers  <- smartSoundOrderingBy(mrq.answers, c("question_code", "answer"))

		print(paste(" * standardiseMRQStatisticsAndMergeWithStandardStats :", survey, " --  Loading Standard statistics object"))
			stats.mrq <- loadMRQSurveyStatistics(survey)
		print(paste(" * standardiseMRQStatisticsAndMergeWithStandardStats :", survey, " --  Loading MRQ statistics object"))
			stats.standard <- loadSurveyStatistics(survey)
		print(paste(" * standardiseMRQStatisticsAndMergeWithStandardStats :", survey, " ---------------------------------------------  "))

		print(paste(" * standardiseMRQStatisticsAndMergeWithStandardStats :", survey, " -- for every subset statistics:"))
		print(paste(" * standardiseMRQStatisticsAndMergeWithStandardStats :", survey, " --- convert MRQ questions to answers "))
		print(paste(" * standardiseMRQStatisticsAndMergeWithStandardStats :", survey, " --- merge main question codes "))
		print(paste(" * standardiseMRQStatisticsAndMergeWithStandardStats :", survey, " --- replace answers codes with answer values "))
		print(paste(" * standardiseMRQStatisticsAndMergeWithStandardStats :", survey, " --- merge MRQ standardised statistics with standard statistics"))

		i<-0;
		for(stats.mrq.subset in stats.mrq){
			i <- i+1
			#print(i)
			stats.mrq.subset<-merge(stats.mrq.subset, columns.mrq.qmr, by = "question_code")

			names(stats.mrq.subset)[names(stats.mrq.subset) == "answer"] <- "delete"
			stats.mrq.subset$delete <- NULL
			stats.mrq.subset <- merge(stats.mrq.subset, mrq.answers, by = "question_code")
			stats.mrq.subset$question_code <- NULL
			names(stats.mrq.subset)[names(stats.mrq.subset) == "main_question"] <- "question_code"
			stats.mrq[[i]] <- stats.mrq.subset[ ,c("CountryEUStatus", "CountryCode", "question_code",   "subset",  "answer",  "answers", "subset_answers", "percentage")]
			stats.standard[[i]] <- merge(stats.standard[[i]], stats.mrq[[i]], all = TRUE)
		}

		print(paste(" * standardiseMRQStatisticsAndMergeWithStandardStats :", survey, " - Done Saving unified statistics object"))


		survey.folder <- getSurveyFolder(survey)
		result.file.name<-paste(survey.folder, "Survey", survey, ".RData", sep = "")
		saveDataFrame(stats.standard, result.file.name)
		print(paste(" * standardiseMRQStatisticsAndMergeWithStandardStats :", survey, " - File overwriten: ", result.file.name))

	  #--------------------------------------
    print(paste(" |  - Time Needed :", proc.time()[3] - function.starting.time, "seconds"))
    #--------------------------------------

}
