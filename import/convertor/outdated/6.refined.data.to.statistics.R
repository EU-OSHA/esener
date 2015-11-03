# ======================================================================
# Eworx S.A.- 2012
# Author kp@eworx.gr
# ======================================================================
# Contains the functions that convert the refined.survey.data to statics
# ======================================================================
 

#test <- loadRefinedSurvey("3RDEQLS")
#Survey.3RD-EQLS <- list(refinedData = test, statisticsData = test )

convertSurveyToStatistics <- function(survey){
  
  if(
    file.exists(paste(getSurveyFolder(survey), survey.refined.data.file.name, sep = "")) &&
    file.exists(paste(getSurveyFolder(survey), survey, ".", step1.input.file.name, sep = ""))
  ){
    
    print("------------------------------------------")
    print("------------------------------------------")
    print(paste(" * convertSurveyToStatistics :", survey, " - STARTED"))  
    function.starting.time <- proc.time()[3]
    print("------------------------------------------")
    print("------------------------------------------")
    
    #--------------------------------------
    print(paste(" | convertSurveyToStatistics :", survey, " - Loading required information"))
    function.block.starting.time <- proc.time()[3]          
    #--------------------------------------      
    
    survey.refined.data <- loadRefinedSurvey(survey)
    columns <- names(survey.refined.data)
    
    column.types <- readXls(paste(getSurveyFolder(survey), survey, ".", step1.input.file.name, sep = ""))
    column.types <- sqldf("select * from 'column.types' where Type <> 'Delete' and Type <> 'CountryCode' and Type <> 'CountryName' order by Type, Column")
        
    column.CountryCode <- "CountryCode"    
    
    column.weight <- sqldf("select Column from 'column.types' where Type = 'Weight' ")
    column.weight <- as.character(column.weight$Column)
    column.weight.exists <- (length(column.weight) > 0)
    
    columns.subsets <- sqldf("select Column from 'column.types' where Type = 'Subset' ")
    columns.subsets <- columns.subsets$Column
    columns.subsets <- as.vector(columns.subsets)
    
    columns.questions <- sqldf("select Column from 'column.types' where Type in ('Question', 'QuestionMean') ")
    columns.questions <- columns.questions$Column
    columns.questions <- as.vector(columns.questions)
    
    countries.eu.statuses <- sqldf("select distinct CountryEUStatus from 'survey.refined.data' ");
    countries.eu.statuses <- countries.eu.statuses$CountryEUStatus
    countries.eu.statuses <- as.vector(countries.eu.statuses)
    
    #--------------------------------------
    print(paste(" |  - Time Needed :", proc.time()[3] - function.block.starting.time, "seconds"))
    #--------------------------------------
    
    #--------------------------------------
    print(paste(" | convertSurveyToStatistics :", survey, " - Generating statistics for all subsets, subset values X all questions, answers X all EU Statuses"))
    #--------------------------------------
    
    # for every subset
    for(subset in columns.subsets){
      #--------------------------------------
      print(paste(" | convertSurveyToStatistics :", survey, " - Generating statistics for subset: ", subset))
      function.block.starting.time <- proc.time()[3]
      #--------------------------------------
      
      questions.by.subset.stats <- data.frame()
      
      # for every question  per subset
      for(question in columns.questions){
        
        #--------------------------------------
        print(paste(" | convertSurveyToStatistics :", survey, " -  Generating statistics for subset", subset, "for Question", question))
        subset.question.starting.time <- proc.time()[3]
        #--------------------------------------
        
        sqlA.join.sqlB <- paste(
          "
          Select sqlA.CountryEUStatus, sqlA.CountryCode, sqlA.question_code, sqlA.subset, sqlA.answer, sqlA.answers, sqlB.subset_answers, 0 as percentage 

            from 
              (
                Select CountryEUStatus, CountryCode, '", question ,"' as question_code, ", subset," as subset, ", question, " as answer, ",
          
                      ifelse(column.weight.exists, paste("sum(", column.weight, ")", sep = ""), "count(*)"), " as answers 

                  from 'survey.refined.data' where ", question, " <> 'DELETE' and ", subset, " <> 'DELETE'
                  group by CountryCode, ", subset, ",", question, "
              ) 
              sqlA 

            inner join
              (
                Select CountryCode, '", question, "' as question_code, ", subset, " as subset, ",
                       ifelse(column.weight.exists, paste("sum(", column.weight, ")", sep = ""), "count(*)"), " as subset_answers 
                from  'survey.refined.data' where ", question, " <> 'DELETE' and ", subset, " <> 'DELETE'
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
                from 'survey.refined.data' where ", question, " <> 'DELETE' and ", subset, " <> 'DELETE'
                group by CountryCode, ", question, "
            )
            sqlC

          inner join
            (
              Select CountryCode, ", 
                     ifelse(column.weight.exists, paste("sum(", column.weight, ")", sep = ""), "count(*)"), " as subset_answers  
                from 'survey.refined.data' where ", question, " <> 'DELETE' and ", subset, " <> 'DELETE'
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
        print(paste(" | convertSurveyToStatistics :", survey, " -  Merging question stats"))
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
      print(paste(" | convertSurveyToStatistics :", survey, " -  Order Primary Subset Statistics"))
      subset.question.starting.time <- proc.time()[3]
      #--------------------------------------      
      questions.by.subset.stats <- sqldf("select * from 'questions.by.subset.stats' order by CountryEUStatus desc, CountryCode, question_code, answer, subset")
      #--------------------------------------
      print(paste(" |  - Time Needed :", proc.time()[3] - subset.question.starting.time, "seconds"))
      #--------------------------------------
      
      print("Calculate Subset Statistics for every Country EU Status using the computed Primary Subset Questions Statistics")
      
      eu.statuses.means.statistics <- data.frame()
      
      for(eu.status in countries.eu.statuses){
        
        #--------------------------------------
        print(paste(" | convertSurveyToStatistics :", survey, " -  Calculating", eu.status, "means for subset", subset ))
        subset.question.starting.time <- proc.time()[3]
        #--------------------------------------        
        eu.status.means.statistics <-
        sqldf(
          paste(
            "
              Select 
              '", eu.status,"' as CountryEUStatus, '", eu.status,"' as CountryCode, question_code, subset, answer, sum(answers) as answers, sum(subset_answers) as subset_answers, 0 as percentage
              from 'questions.by.subset.stats' 
              where CountryEUStatus = '", eu.status,"' 
              group by question_code, subset, answer
              order by question_code, answer, subset
            "
            , sep = ""
          )
        )
        
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
      
      #if more than one EU Country Status is detected calculate the EUALL status for all coountries in the set
      if(length(countries.eu.statuses)>1){
        # calculate stats for non existing status the EUALL status 
        eu.status <- "EUALL"
        
        #--------------------------------------
        print(paste(" | convertSurveyToStatistics :", survey, " -  Calculating", eu.status, "means for subset", subset ))
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
              order by question_code, answer, subset
              "
              , sep = ""
            )
          )
  
        eu.statuses.means.statistics <- merge(eu.statuses.means.statistics, eu.status.means.statistics, all = TRUE)
  
        #--------------------------------------
        print(paste(" |  - Time Needed :", proc.time()[3] - subset.question.starting.time, "seconds"))
        #--------------------------------------
      }else{
        print("Only one EU country status is detected in the set. Ommit the calculation of EUALL that would contain the means of all countries in the corpus")        
      }
      #--------------------------------------
      print(paste(" | convertSurveyToStatistics :", survey, " -  Merging subset Primary stats with available EU means stats"))
      subset.question.starting.time <- proc.time()[3]
      #--------------------------------------
      questions.by.subset.stats <- merge(questions.by.subset.stats, eu.statuses.means.statistics, all = TRUE)
      #--------------------------------------
      print(paste(" |  - Time Needed :", proc.time()[3] - subset.question.starting.time, "seconds"))
      #--------------------------------------
      
      #--------------------------------------
      print(paste(" | convertSurveyToStatistics :", survey, " -  Last step calculation step for subset - calculate percentages"))
      subset.question.starting.time <- proc.time()[3]
      #--------------------------------------            
      questions.by.subset.stats$percentage <- (questions.by.subset.stats$answers  * 100) / questions.by.subset.stats$subset_answers
      #--------------------------------------
      print(paste(" |  - Time Needed :", proc.time()[3] - subset.question.starting.time, "seconds"))
      #--------------------------------------
     
      r.statement <- paste("Subset", subset ," <- questions.by.subset.stats", sep = "")
      eval(parse(text = r.statement))
    }
    
    #wrap stats objects per subset into one survey list object
    #--------------------------------------
    print(paste(" | convertSurveyToStatistics :", survey, " -  Wrapping stats per subset into a survey object and save to file"))
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
    
    r.statement <- paste("saveDataFrame(Survey", survey, ",'", survey.folder, "Survey", survey, ".RData')", sep = "")
    eval(parse(text = r.statement))
    
    #--------------------------------------
    print(paste(" |  - Time Needed :", proc.time()[3] - subset.question.starting.time, "seconds"))
    #--------------------------------------
    
    #--------------------------------------
    print("------------------------------------------")
    print(paste(" * convertSurveyToStatistics :", survey, " - STATISTICS OBJECT CREATED"))  
    print(paste(" *  Time Needed :", (proc.time()[3] - function.starting.time) / 60, "minutes"))
    print("------------------------------------------")
    #--------------------------------------
    
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

#convertSurveyToStatistics("3RDEQLS")

# ======================================================================

loadDataFrame <- function(file){
  load(file)
  return(data)
}

# ======================================================================

loadSurveyStatistics <- function(survey){
	survey.data.location <- paste(survey.folder <- getSurveyFolder(survey), "Survey", survey, ".RData" , sep = "")
	if( file.exists(survey.data.location) ){
		return(loadDataFrame(survey.data.location))
	}else{
		print("Survey does not exist")
	}
}

#test<-loadSurveyStatistics("3RDEQLS")

# ======================================================================

