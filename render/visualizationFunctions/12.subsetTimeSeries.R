# ======================================================================
# Eworx S.A. - 2012/2013
# Author kp@eworx.gr
# ======================================================================
# defines the map related Visualization functions
# ======================================================================



#subsetTimeSeries("3RDEQLS","Y11_Agecategory","Y11_Q12a", "1. Several times a week", "AT","EN","png",740, TRUE);

#subsetTimeSeries "3RDEQLS", ""

subsetTimeSeries <- function(
	survey,
	subset,
	param_question_code,
	param_question_answer,
	param_country,
	locale = "EN",
	media = "png",
	width = standard.width,
	entitle = FALSE,
	height = standard.height.map, #690,
	savePath = plots.location
){
# The visualization illustrates in a map the percentages of answers, or the mean answer value of a question per European Country.

	dynamicHeight = ifelse(entitle, standard.height.map, standard.height.map - (standard.height.map * 0.115))

	if(width != standard.width){
		height = ( width * dynamicHeight ) / standard.width
	}

	#-------------------------------------------------

	visualization.file.body.name <- paste("subsetTimeSeries-", survey, "-", subset, "-", param_question_code, "-", toUrlValue(param_question_answer), "-", param_country, "-", locale, sep = "" )

########################################################################################################################################3
########################################################################################################################################3
########################################################################################################################################3

	#-----------------------------------------------
	# Data manipulation
	#-----------------------------------------------

	stat.data <- data.frame()

	question.is.mean <- param_question_answer == "Mean"

	if(timeSeriesExists(survey)){
		year.survey <- getTimeSeriesSurveys(survey)

		for(time in 1:nrow(year.survey)){

			surveyInTime <-  as.character(year.survey$Survey[time])
			yearInTime <-  as.numeric(year.survey$Year[time])

			temp.stat.data <- getSurveyDataSubsetTimeSeries(surveyInTime, subset, param_question_code, param_question_answer, param_country)

			if(nrow(temp.stat.data)>0){

				temp.stat.data[, "Year"] <- yearInTime
				temp.stat.data[, "inFocus"] <- surveyInTime == survey

				if(nrow(stat.data) == 0)
					stat.data <- temp.stat.data
				else
					stat.data <- merge(stat.data, temp.stat.data, all = TRUE)
			} 

	 
		}
	}else{
		return("No time series data. Please set the time.series.input.file.name variable or respective file correctly");
	}


	#-----------------------------------------------


########################################################################################################################################3
########################################################################################################################################3
########################################################################################################################################3

	csv.data <- stat.data[c("Year", "CountryCode", "question_code", "subset", "answer", ifelse(question.is.mean, "Mean", "percentage"))]

	if(question.is.mean){
		csv.data$Mean <- round(csv.data$Mean, 1)
	}else{
		csv.data$percentage <- round(csv.data$percentage, 1)
	}

	writeDataInCSV(
		csv.data,
		paste(plots.csv.location, visualization.file.body.name, "", ".xls", sep = "")
	)

	#---------------------------
	# AUTO DESCRIPTOR
	#---------------------------
	translated.values <- getSurveyTranslatedValues(survey, locale)

	countries.translatation <- getSurveyCountriesTranslations(translated.values)


#	max.countries <- sqldf(paste(
#		"select TranslatedValue Country from 'stat.data' sd inner join 'countries.translatation' ct on ct.Value=sd.CountryCode order by ",
#			ifelse(question.is.mean, "Mean", "percentage"),
#		" desc limit 3", sep = "")
#	)
#	max.countries <- as.vector(max.countries$Country)
#	max.countries <- paste(max.countries, collapse = ", ")

#	min.countries <- sqldf(paste(
#		"select TranslatedValue Country from 'stat.data' sd inner join 'countries.translatation' ct on ct.Value=sd.CountryCode order by ",
#		ifelse(question.is.mean, "Mean", "percentage"),
#		" asc limit 3", sep = "")
#	)

#	min.countries <- as.vector(min.countries$Country)
#	min.countries <- paste(min.countries, collapse = ", ")

#	description <- getSurveyPlotDescriptorTemplate(translated.values , "heatMap", question.is.mean )

#	if(param_question_answer == "Mean"){
#		description.values <- c(
#		getSubsetTranslation(translated.values, subset),
#		clearIndex(getSurveyFullTranslatedQuestion(survey, locale, param_question_code )),
#		max.countries, min.countries
#		)
#	}else{
#		description.values <- c(
#		clearIndex(getSurveyQuestionTranslatedAnswer(translated.values, param_question_code, param_question_answer)),
#		getSubsetTranslation(translated.values, subset),
#		getSurveyFullTranslatedQuestion(survey, locale, param_question_code ),
#		max.countries, min.countries
#		)
#	}

#	description <- fillDescription(description, description.values)

subsetTimeSeriesTemplatePart1 <- "In the figure, we see the time series of the people per %1, answering '%2' when asked '%3'."
subsetTimeSeriesMeanTemplatePart1 <- "In the figure, we see the time series of the people per %1 mean value."
increased <- "increased"
decreased <- "decreased"
subsetTimeSeriesTemplatePart2 <- "From %1 to %2 the answer's average %3 by %4 percent scoring %5%" 
subsetTimeSeriesMeanTemplatePart2 <- "From %1 to %2 the answer's average %3 by %4 points scoring %5." 

subsetTimeSeriesTemplatePart3 <- "- The most popular subset for %1 is the %2 and the least the %3."
subsetTimeSeriesTemplatePart4 <- "- The largest increase is observed for the %1 %2 by %3"
subsetTimeSeriesTemplatePart5 <- "- The largest decrease is observed for the %1 %2 by %3"
subsetTimeSeriesTemplatePart6 <- "- There was no increase among the data."
subsetTimeSeriesTemplatePart7 <- "- There was no decrease among the data."
subsetTimeSeriesTemplatePart8 <- "- The least change is observed for the %1 %2 by %3"

subsetTimeSeriesNoData <- "There are no time series information available for the selected combination."


sortedUniqueYears = sort(unique(stat.data$Year))

	templateDescriptionFirst <- subsetTimeSeriesTemplatePart1 #subsetTimeSeriesTemplatePart1 <- "In the figure, we see the time series of the people per %1, answering '%2' when asked '%3'."
	
	if(question.is.mean){
		templateDescriptionFirst <- subsetTimeSeriesMeanTemplatePart1 #subsetTimeSeriesMeanTemplatePart1 <- "In the figure, we see the time series of the people per %1 mean value."
		templateDescriptionFirst <- fillDescription(templateDescriptionFirst, c(
				getSubsetTranslation(translated.values, subset),				
				getSurveyFullTranslatedQuestion(survey, locale, param_question_code )				
			)
		)
	}else{
		templateDescriptionFirst <- fillDescription(templateDescriptionFirst, c(
				getSubsetTranslation(translated.values, subset),
				clearIndex(getSurveyQuestionTranslatedAnswer(translated.values, param_question_code, param_question_answer)),
				getSurveyFullTranslatedQuestion(survey, locale, param_question_code )				
			)
		)
}

 description<- templateDescriptionFirst




if(length(sortedUniqueYears) > 1){

	toYearIndex<-length(sortedUniqueYears)
	toYearIndex<-toYearIndex -1

	for(yearIndex in 1:toYearIndex ){

		fromYear <- sortedUniqueYears[yearIndex]
		toYear <- sortedUniqueYears[yearIndex+1]
 
		comonstat.data <- stat.data

		if(question.is.mean)
			colnames(comonstat.data)[6] <- "percentage"
 

		fromYearData <- subset(comonstat.data, Year == fromYear)
		toYearData <- subset(comonstat.data, Year == toYear)

		# perform an inner join ensuring that we have one to one mappings thus the derivatives tobe computed will be valid 

		periodData <- merge(fromYearData, toYearData, by = c("subset"))[c(
				"CountryEUStatus.x", "CountryCode.x",
			 	"question_code.x", "subset", "answer.x",
			  	"percentage.x", "Year.x", "inFocus.x",
			  	"percentage.y", "Year.y", "inFocus.y"
			  )
		]

		colnames(periodData)[1] <- "CountryEUStatus"
		colnames(periodData)[2] <- "CountryCode"
		colnames(periodData)[3] <- "question_code"
		colnames(periodData)[4] <- "subset"
		colnames(periodData)[5] <- "answer"
		colnames(periodData)[6] <- "percentage"
		colnames(periodData)[7] <- "Year"
		colnames(periodData)[8] <- "inFocus"
		colnames(periodData)[9] <- "percentage_b"
		colnames(periodData)[10] <- "Year_b"
		colnames(periodData)[11] <- "inFocus_b"

		periodData[, "diff"] <- periodData$percentage_b-periodData$percentage
				
		periodData$percentage <- round(periodData$percentage, 1)
		periodData$percentage_b <- round(periodData$percentage_b, 1)
		periodData$diff <- round(periodData$diff, 1)

		#print(periodData[c("subset","percentage", "percentage_b", "diff")])
		
 
		average <- subset(periodData, subset == "All") #select average 
		periodData <- subset(periodData, subset != "All") #exclude average 

		templateDescriptionSecond <-  fillDescription(ifelse(question.is.mean, 
					subsetTimeSeriesMeanTemplatePart2, #subsetTimeSeriesTemplatePart2 <- "From %1 to %2 the answer's average %3 by %4 percent scoring %5%" 
					subsetTimeSeriesTemplatePart2), #subsetTimeSeriesMeanTemplatePart2 <- "From %1 to %2 the answer's average %3 by %4 points scoring %5." 
			c(
				fromYear,
				toYear,				
				ifelse(average$diff[1] > 0, increased, decreased),
				gsub("\\.", "·", as.character(average$diff[1])),
				gsub("\\.", "·", as.character(average$percentage_b[1]))
			)) 

 
		if(yearIndex > 1)
			description <- paste(description, templateDescriptionSecond)
		else
			description <- paste(templateDescriptionFirst, templateDescriptionSecond)

 
 		


		templateDescriptionSecond <- fillDescription(subsetTimeSeriesTemplatePart3, c( #subsetTimeSeriesTemplatePart3 <- "- The most popular subset for %1 is the %2 and the least the %3."
				toYear,
				clearIndex(getSubsetValueTranslation(translated.values, subset, as.character(sqldf("select subset from toYearData order by percentage desc limit 1")[1, 1]) )),
				clearIndex(getSubsetValueTranslation(translated.values, subset, as.character(sqldf("select subset from toYearData order by percentage asc limit 1")[1, 1]) ))				
			)
		)

  		description <- paste(description, templateDescriptionSecond)
		



		if(nrow(subset(periodData, diff>0))){
			periodMaxDiff <- sqldf("select subset, diff from periodData order by diff desc limit 1 ")
			description <- paste(description, fillDescription(subsetTimeSeriesTemplatePart4, c( #subsetTimeSeriesTemplatePart4 <- "- The largest increase is observed for the %1 %2 by %3"
					getSubsetTranslation(translated.values, subset),
					clearIndex(getSubsetValueTranslation(translated.values, subset, as.character(periodMaxDiff[1, 1]) )),
					gsub("\\.", "·", as.character(periodMaxDiff[1, 2]))		
				)
			), ifelse(question.is.mean, "points.", "percent.") )
		}else{
			 description <- paste(description, subsetTimeSeriesTemplatePart6)#subsetTimeSeriesTemplatePart6 <- "- There was no increase among the data."
		}
 
		if(nrow(subset(periodData, diff<0))){
			periodMinDiff <- sqldf("select subset, diff from periodData order by diff asc limit 1 ")

			description <- paste(description, fillDescription(subsetTimeSeriesTemplatePart5, c(#subsetTimeSeriesTemplatePart5 <- "- The largest decrease is observed for the %1 %2 by %3"
					getSubsetTranslation(translated.values, subset),
					clearIndex(getSubsetValueTranslation(translated.values, subset, as.character(periodMinDiff[1, 1]) )),
					gsub("\\.", "·", as.character(abs(periodMinDiff[1, 2])))
				)
			), ifelse(question.is.mean, "points.", "percent.") )
  		}else{
			 description <- paste(description, subsetTimeSeriesTemplatePart7)#subsetTimeSeriesTemplatePart7 <- "- There was no decrease among the data."
		}

			periodMinABSDiff <- periodData
			periodMinABSDiff[, "absdiff"] <- abs(periodMinABSDiff$diff)
 

			periodMinABSDiff <- sqldf("select subset, diff, absdiff from periodMinABSDiff order by absdiff asc limit 1")
 		 

			description <- paste(description, fillDescription(subsetTimeSeriesTemplatePart8, c( #subsetTimeSeriesTemplatePart8 <- "- The least change is observed for the %1 %2 by %3"
					getSubsetTranslation(translated.values, subset),
					clearIndex(getSubsetValueTranslation(translated.values, subset, as.character(periodMinABSDiff[1, 1]) )),
					gsub("\\.", "·", as.character(periodMinABSDiff[1, 2]))		
				)
			), ifelse(question.is.mean, "points.", "percent.") )
 		  
  

	}
	
}else{
	description <- subsetTimeSeriesNoData #subsetTimeSeriesNoData <- "There are no time series information available for the selected combination."
}


 
 

# increased 
 # there were declines of over 20 % in the levels of optimism and hap
	out.file <- paste(savePath, "/txt/", visualization.file.body.name, ".htm", sep = "")
	con <- file(out.file)
	writeLines(c(description), con)
	close(con)
 
	################

	#-----------------------------------------------
	#-----------------------------------------------

	file.visualization <- "";
	if(media == "png"){
		file.visualization <- paste(savePath, visualization.file.body.name, "-", width, ifelse(shouldOptimizePNG, "_", ""), ".png", sep = "")
		Cairo(file = file.visualization , type = "png", width = width, height = height, bg = "white", antialias = "subpixel");
	}else
	if(media == "pdf"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width, ".", media, sep = "")
		cairo_pdf(filename = visualization.file.body.name, width =  13 * (width/standard.width), height = 13 *(height/standard.height.map), pointsize = 12, onefile = TRUE, bg = "white", family = "Open Sans", antialias = c("default", "none", "gray", "subpixel"))
	}else
	if(media == "eps"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width, ".", media, sep = "")
		postscript(visualization.file.body.name, paper="special", width =  13 * (width/standard.width), height = 13 *(height/standard.height.map), horizontal = FALSE)
	}else
	if(media == "svg"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width, sep = "")
		file.visualization <- paste(visualization.file.body.name, ".svg", sep = "")
		if(file.exists(file.visualization) || file.exists(paste(visualization.file.body.name, ".svg.gz", sep = "")) )
			return("");
		Cairo(file = visualization.file.body.name, type = "svg", width =  12.77 * (width/standard.width), height = 12.77 * (height/standard.height.map), bg = "white", pointsize = 12, units = "in");
	}else
	if(media == "xls"){
		return("");
	}

	#-----------------------------------------------
	# Visualization
	#-----------------------------------------------

#####################################

	uniqueItems <- unique(stat.data$subset)
	years.count <-length(unique(stat.data$Year))
	subsets.count <-length(unique(stat.data$subset))


	valueLimits <- c()

	if(question.is.mean)
		valueLimits <- c(min(stat.data$Mean), max(stat.data$Mean))
	else
		valueLimits <- c(min(stat.data$percentage), max(stat.data$percentage))

	dataMean <- (valueLimits[1] + valueLimits[2]) / 2

	stat.data [, "surveyDate"] <- as.Date(paste("1 1 ", stat.data$Year), "%d %m %Y")




	#-----------------------------------------------

	stat.data <- clearIndexInColumnStrings(stat.data, "subset")


	#stat.data [, "subset"] <- clearIndex(stat.data [, "subset"] )

	#-----------------------------------------------

#####################################

	if(question.is.mean){
		print(colnames(stat.data)[6]);
		colnames(stat.data)[6] <- "percentage"
	}

	incomplete.stat.data <- data.frame()
	singular.stat.data <- data.frame()
	complete.stat.data <- stat.data

	case <- 0;
 

	if(years.count == 1 )
		case <- 1
	else if(years.count * subsets.count != nrow(stat.data))
		case <- 2
	else if(years.count * subsets.count == nrow(stat.data))
		case <- 3


	if(case == 2){
		#separate the data into two sets one containign the complete while the second the incomplete data
		observationsPerSubsetAnswer <- sqldf(paste("select count(*) cases, subset, answer from 'stat.data' group by answer, subset having count(*) < ", years.count))

		for(toRemoveIndex in 1:nrow(observationsPerSubsetAnswer)){
			subsetToRemove <- as.character( observationsPerSubsetAnswer$subset[toRemoveIndex])

			if(observationsPerSubsetAnswer$cases[toRemoveIndex] == 1){

				if(nrow(incomplete.stat.data) == 0){
					singular.stat.data <- subset(stat.data, subset == subsetToRemove )
				}else{
					singular.stat.data <- merge(singular.stat.data, subset(stat.data, subset == subsetToRemove ), all = TRUE )
				}
			}else{
				if(nrow(incomplete.stat.data) == 0){
					incomplete.stat.data <- subset(stat.data, subset == subsetToRemove )
				}else{
					incomplete.stat.data <- merge(incomplete.stat.data, subset(stat.data, subset == subsetToRemove ), all = TRUE )
				}
			}

			complete.stat.data <- subset(complete.stat.data, subset != subsetToRemove )

		}

	}

#subsetTimeSeries("3RDEQLS","Y11_Agecategory","Y11_Q12a","1. Several times a week","AT","EN","svg",740);mark.as.available();

 #################################

 	legendText = getSubsetTranslation(translated.values, subset)


	map <- ggplot(stat.data, aes(x = surveyDate, y = percentage, group = subset, color = subset))


	if(case != 1 ){
		rounding <- 0
		if(length(unique(round(unique(stat.data$percentage), 0)))<5)
			rounding <- 1

		map <- map + scale_y_continuous(breaks = unique(round(unique(stat.data$percentage), rounding)) )
	}


	map <- map + scale_x_date("Year", labels = date_format("%Y"), breaks = unique(stat.data$surveyDate)) +

	scale_colour_manual(legendText, values = pallet.clusters) +
	geom_point(aes(shape = subset), size = 6) +
	scale_shape_manual(legendText, values = c(15: (15 + length(uniqueItems))) ) +

	geom_point(colour = "white", size = 1.5) +

	ylab(clearIndex(getSurveyQuestionTranslatedAnswer(translated.values, param_question_code, param_question_answer))) +

	theme_ef_survey_time_series()

	print("case:")
	print(case)
	if(case == 1 ){
		print(names(stat.data))
		map <- map + geom_hline(aes( yintercept = percentage, color = subset), size = 0.8) #geom_line(size = 0.8)
	}else
	if(case == 2){

		map <- map + geom_line(data = incomplete.stat.data, size = 0.8)
		map <- map + geom_smooth(data = complete.stat.data, size = 0.8)#, legend = FALSE, method="loess")

		if(nrow(singular.stat.data)>0){
			map <- map + geom_hline(data = singular.stat.data, aes( yintercept = percentage, color = subset), size = 0.8)
		}
	}else
    if(case == 3){
    	#map <- map + geom_hline(aes( yintercept = percentage, color = subset), size = 0.8) #geom_line(size = 0.8)
    	#print(complete.stat.data)

    	if(years.count == 2)
 			map <- map + geom_line(data=complete.stat.data, size = 0.8) 
		else 		
 		 	map <- map + geom_smooth(data=complete.stat.data, size = 0.8)#, legend = FALSE , method="loess" )
	}

###################################

	if(entitle){

		grid.arrange(
			getTitlesPlot(text=
				c(
					getSurveyTranslatedQuestionTopic(survey, locale, param_question_code),
					clearIndex(getSurveyTranslatedQuestion(survey, locale, param_question_code )),
					paste(
						ifelse( question.is.mean, "", paste(getSurveyLabel(translated.values, "answer"), ": ",
							clearIndex(getSurveyQuestionTranslatedAnswer(translated.values, param_question_code, param_question_answer)), ", ",  sep = "" )),
						getSurveyLabel(translated.values, "subset"), ": ",
						clearIndex(getSubsetTranslation(translated.values, subset)), ", ",
						sep = ""
					)
				), width
			),

			map,
			getBottomPlot(
				paste(
					standard.copyright,
					getSurveyTranslatedValue(translated.values),
					sep = " - "
				)
			),
			ncol = 1,
			heights = c(0.115, 0.737 + 0.09 + 0.04, 0.045)
			#heights = c(0.14, 0.82, 0.04)
		)

	}else{

		grid.arrange(
				map,
				ncol = 1,
				heights = c(0.115 + 0.737 + 0.045, 0.09 , 0.04 )
				#heights = c(0.115, 0.737, 0.09, 0.04, 0.045)
				#heights = c(0.14, 0.82, 0.04)
		)
	}



	if(entitle){
		pushViewport(viewport( x = 0.413, y = -0.31, width = 0.1, just = c("left", "bottom")))
	}else{
		pushViewport(viewport( x = 0.413, y = -0.35, width = 0.1, just = c("left", "bottom")))
	}

	upViewport()

	if(entitle)
		superImposeLogo(x = 0.86, y = 0.31)
	else
		superImposeLogo(x = 0.86, y = 0.39)

	#print(map)
	#addEFFootnote()
	dev.off()

	if(media == "png"  && shouldOptimizePNG)
		optimizePNG(file.visualization)

	if(media == "svg")
		optimizeSVG(file.visualization, width, height)
}


# ======================================================================

getSurveyDataSubsetTimeSeries <- function(survey, subset, param_question_code, param_question_answer  = "*", param_country = "*"){
	

	question.is.mean <- FALSE

	eval(parse(text = paste("stat.data <- Survey", survey, "$", subset, sep = "")))

	stat.data <- subset(stat.data, (CountryCode %in% param_country ))

	if(param_question_answer == "Mean") {
		question.is.mean <- TRUE
		stat.data <- subset(stat.data, (question_code == param_question_code))
		
		if(nrow(stat.data)>0){
			stat.data <- covertQuestionMeanStatsToMeans(stat.data)[c("CountryEUStatus", "CountryCode", "question_code", "subset", "answer", "percentage")]
			colnames(stat.data)[6] <- "Mean"
		}
	}else{
		stat.data<- stat.data[c("CountryEUStatus", "CountryCode", "question_code", "subset", "answer", "percentage")]
		if(param_country != "*")
			stat.data <- subset(stat.data,(question_code == param_question_code))

		if(param_question_answer  != "*")
			stat.data <- subset(stat.data, ( answer == param_question_answer ))
	}

	return(stat.data)
}