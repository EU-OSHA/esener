# ======================================================================
# Eworx S.A. - 2012/2013
# Author kp@eworx.gr
# ======================================================================
# defines the crossCountry Visualization function
# ======================================================================

crossCountry <- function(

	survey = "3RDEQLS",
	subset = "Y11_Agecategory",
	param_subset_value = "All",
	param_question_code = "Y11_Partnerprefhours",
	param_country = "EL",
	param_countryB = "EuropeanUnion",
	locale = "EN",
	media = "png",
	width = standard.width,
	entitle = FALSE,
	height = standard.height,
	savePath = plots.location

){

# The visualization illustrates the percentages of answers, or the mean answer value for a particular group of people between two countries or the European mean for a question.

	dynamicHeight = ifelse(entitle, standard.height, standard.height - (standard.height * (0.115)) )

	if(width != standard.width){
		height = ( width * dynamicHeight ) / standard.width
	}

	visualization.file.body.name <- paste("crossCountry-", survey, "-", subset, "-", toUrlValue(param_subset_value), "-", param_question_code, "-", param_country, "-", param_countryB, "-", locale, sep = "" )

	eval(parse(text =
		paste(
			"stat.data <- Survey", survey, "$", subset
			,sep = "")
		)
	)

	#-----------------------------------------------
	# Data manipulation
	#-----------------------------------------------

	countryData <- subset(
		stat.data,
		(
			question_code == param_question_code &
			subset == param_subset_value &
			(CountryCode == param_country | CountryCode == param_countryB)
		)
	)

	#-----------------------------------------------
	# Question mean handling
	#-----------------------------------------------

	question.is.mean = FALSE;
	if(isQuestionMean(survey, param_question_code)){
		countryData <- covertQuestionMeanStatsToMeans(countryData)
		question.is.mean = TRUE;
	}

	countryData <- countryData[c("CountryCode", "question_code", "subset", "answer", "percentage")]

	csv.data <- countryData[c("CountryCode", "question_code", "subset", "answer", "percentage")]
	
	csv.data$percentage <- round(csv.data$percentage, 1)
 
	writeDataInCSV(csv.data, paste(plots.csv.location, visualization.file.body.name, ".xls", sep = "" ))

	#---------------------------
	# AUTO DESCRIPTOR
	#---------------------------
	translated.values <- getSurveyTranslatedValues(survey, locale)
	description <- getSurveyPlotDescriptorTemplate(translated.values , "crossCountry", question.is.mean )

	#percentage	In the above figure, we see a comparison between %1 and %2 for the people with "%3", "%4" when questioned "%5".	[For the "%1" answer, %2 is %3 %4.]
	#mean	In the above figure, we see a comparison between %1 and %2 for the people with "%3", "%4" when questioned “%5”. %1 with %6 mean value is %7 %2 with %8 mean value.

	description.group <- gsub("\\]","",gsub(".*\\[","",description))
	description.groups <- ""
	description <- gsub("\\[.*\\]","", description)

	if(question.is.mean){
		#In the above figure, we see a comparison between %1 and %2 for the people with "%3", "%4" when questioned “%5”. %1 with %6 mean value is %7 %2 with %8 mean value.

		height <- height / 2.8

		#previous slower#
		#valueA <- sqldf(paste("select percentage from 'countryData' where CountryCode = '",param_country,"'", sep = ""))
		#optimized code#
		valueA <- as.data.frame(countryData[countryData$CountryCode == param_country,c("percentage")])
		names(valueA) = "percentage"
		#optimized code#
		valueA <- sub('\\.','·', round(as.numeric(as.character(factor(valueA[1,1]))), 2))
		#previous slower#
		#valueB <- sqldf(paste("select percentage from 'countryData' where CountryCode = '", param_countryB, "'", sep = ""))
		#optimized code#
		valueB <- as.data.frame(countryData[countryData$CountryCode == param_countryB,c("percentage")])
		names(valueB) = "percentage"
		#optimized code#
		valueB <- sub('\\.','·', round(as.numeric(as.character(factor(valueB[1,1]))), 2))

		description.values <- c(
			getSurveyCountryTranslation(translated.values, param_country),
			getSurveyCountryTranslation(translated.values, param_countryB),

			getSubsetTranslation(translated.values, subset),
			clearIndex(getSubsetValueTranslation(translated.values, subset, param_subset_value)),
			clearIndex(getSurveyFullTranslatedQuestion(survey, locale, param_question_code )),

			valueA,
			getSurveyWord(translated.values, ifelse(valueA > valueB, "above", "below")),
			valueB
		)

		description <- fillDescription(description, description.values)

	}else{
		#percentage	In the above figure, we see a comparison between %1 and %2 for the people with "%3", "%4" when questioned "%5".

		substract <- (5 - length(unique(countryData$answer)))
		if(substract > 0)
			height <- height - substract * (height / 6)

		description.values <- c(
			getSurveyCountryTranslation(translated.values, param_country),
			getSurveyCountryTranslation(translated.values, param_countryB),

			getSubsetTranslation(translated.values, subset),
			clearIndex(getSubsetValueTranslation(translated.values, subset, param_subset_value)),
			clearIndex(getSurveyFullTranslatedQuestion(survey, locale, param_question_code ))
		)

		description <- fillDescription(description, description.values)

		#[For the "%1" answer, %2 is %3 %4.]
		#previous slower#
		#answers <- sqldf("select distinct(answer) answer from 'countryData' ")
		answers <- data.frame(answer = unique(countryData[,"answer"]))
		for(answer in as.vector(answers$answer)){

			answerValue<-answer
			valueA <- (subset(countryData, answer == answerValue & CountryCode == param_country)$percentage) 
			valueB <- (subset(countryData, answer == answerValue & CountryCode == param_countryB)$percentage)

			description.groups <- paste(description.groups, fillDescription(description.group,
				c(
				clearIndex(getSurveyQuestionTranslatedAnswer(translated.values, param_question_code, answer)),
				getSurveyCountryTranslation(translated.values, param_country),
				getSurveyWord(translated.values, ifelse(valueA > valueB, "above", "below")),
				getSurveyCountryTranslation(translated.values, param_countryB) ) ))
		}

		description <- paste(description, description.groups)
	}


	out.file <- paste(savePath, "/txt/", visualization.file.body.name,".htm", sep = "")
	con <- file(out.file)
	writeLines(c(description), con)
	close(con)

	#---------------------------
	#---------------------------


	#---------------------------
	# AUTO TRANSLATION / LOCALIZATION OF STAT DATA
	#---------------------------
	countryData[,"CC"] <- countryData[,"CountryCode"]
	countryData <- translateStatDataValues(countryData, translated.values, subset, param_question_code)

	countryData[,"coloring"] <- countryData$answer
	## do a custom coloring the comparing country bar to always have the same color
	countryData[which(countryData$CountryCode == getSurveyCountryTranslation(translated.values, param_countryB) ), "coloring"] <- "same"
	pallet.dynamic <- pallet.clusters
	my.factors = unique(factor(countryData$coloring))
	pallet.dynamic <- append(pallet.dynamic, "#e8e8e8", after = which(my.factors == "same")-1)
	###

	countryData[, "PercentageRounded"] <- paste(round(countryData$percentage, 1), ifelse(question.is.mean,"","%"))

	countryData$CountryCode <- factor(
		countryData$CountryCode,
		levels = c(getSurveyCountryTranslation(translated.values, param_countryB), getSurveyCountryTranslation(translated.values, param_country) ), #rev(sort(unique(countryData$CountryCode))),
		ordered = TRUE
	)

	#-----------------------------------------------
	#-----------------------------------------------
	file.visualization <- "";

	if(media == "png"){
		file.visualization <- paste(savePath, visualization.file.body.name, "-", width, ifelse(shouldOptimizePNG, "_", ""), ".png", sep = "")
		Cairo(file = file.visualization, type = "png", width = width, height= height, bg = "white");
		#png(filename = paste(savePath, visualization.file.body.name, "-", width, ".png", sep = ""), width, height, units = "px", pointsize = 12, antialias = "subpixel" )
	}else
	if(media == "pdf"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width, ".", media, sep = "")
		cairo_pdf(filename = visualization.file.body.name, width =  13 * (width/standard.width), height = 13 *(height/standard.height), pointsize = 12, onefile = TRUE, bg = "white", antialias = c("default", "none", "gray", "subpixel"))
	}else
	if(media == "eps"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width, ".", media, sep = "")
		postscript(visualization.file.body.name, paper = "special", width =  13 * (width/standard.width), height = 13 *(height/standard.height), horizontal = FALSE)
	}else
	if(media == "svg"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width, sep = "")
		file.visualization <- paste(visualization.file.body.name, ".svg", sep = "")	
		if(file.exists(file.visualization) || file.exists(paste(visualization.file.body.name, ".svg.gz", sep = "")) )
			return("");		
		Cairo(file = visualization.file.body.name, type = "svg", width =  12.77  * (width/standard.width), height = 12.77  *(height/standard.height), bg = "white", pointsize = 12, units = "in");
	}else
	if(media == "xls"){
		return("");
	}

	#-----------------------------------------------
	# Visualization
	#-----------------------------------------------

	chart <- ""
	heights = c(0.115, 0.84, 0.045)
	#heights = c(0.115, 0.2,0.2,0.2,0.2, 0.045)

	#------------------------------------------------------------------
	subPlotValues = unique(countryData$answer)
	#------------------------------------------------------------------

	if(question.is.mean){
		
		isFirstSelected <- as.character(countryData[1,"CC"]) == param_country
		
		if(!isFirstSelected){
			temp <- pallet.dynamic[1]
			pallet.dynamic[1] <- pallet.dynamic[2]
			pallet.dynamic[2] <- temp
		}

		yLim = c(0, max(countryData$percentage) * 1.2);
		
		print(countryData)
		
		chart <- ggplot(
			countryData,
			aes(x = CountryCode, y = percentage, fill = coloring )
		) + theme_ef_incountry() +
			geom_bar(stat="identity") +
			coord_cartesian() +
			#coord_polar(theta = "y") +
			scale_fill_manual(values = pallet.dynamic) +
			coord_flip(ylim = yLim) +
			geom_text(aes(label = PercentageRounded, hjust = -0.3), color = "grey70")

		heights = c(0.35, 0.5, 0.15)

	}else{

		originalCountryData <- countryData
		originalCountryData[, "percentageTextValue"] <- originalCountryData$percentage + 1

		yLim = c(max(originalCountryData$percentage) * -0.04, max(originalCountryData$percentage) + 7);#?
		twoquestionHeigths <-  c(0.225, 0.68, 0.095)

		if(length(subPlotValues) < 5){
			diff <- 5 - length(subPlotValues)
			heights <- heights + ((twoquestionHeigths - heights ) / 4) * diff
		}

		#------------------------------------------------------------------
		plotIndex = 0;

		for(subPlotValue in (subPlotValues)){
			plotIndex = plotIndex+1
			pallet.dynamic <- c(pallet.clusters[plotIndex], "#e8e8e8")
			eval(parse(text = paste("pallet.dynamic" , plotIndex , " <- pallet.dynamic", sep = "")))

			countryData <- subset(originalCountryData,(answer == subPlotValue))

			chart <- ggplot(
				countryData,
				aes(x = CountryCode, y = percentage, fill = coloring )
			) +
				geom_bar(stat="identity") +
				coord_cartesian() +
				coord_flip(ylim = yLim) +
				geom_text(aes(y = percentageTextValue, label = PercentageRounded, hjust = 0), color = "grey70")

			chart <- chart + scale_fill_manual(values = pallet.dynamic)
			eval(parse(text = paste("chart <- chart + scale_fill_manual(values = pallet.dynamic", plotIndex, ")", sep = "")))

			if(plotIndex == length(subPlotValues)){
				chart <- chart + theme_ef_incountry()
				if(!question.is.mean)
					chart = chart +	scale_y_continuous(labels = percentageLabels)
			}else{
				chart <- chart + theme_ef_incountry_no_axis()
			}

			eval(parse(text = paste("chart" , plotIndex , " <- chart", sep = "")))

		}
		#------------------------------------------------------------------

	}

	bottomPlot <- getBottomPlot( paste( standard.copyright, getSurveyTranslatedValue(translated.values), sep = " - " ))

	# Titles Anotation
	titlesPlots <-  getTitlesPlot(text=
			c(
				getSurveyTranslatedQuestionTopic(survey, locale, param_question_code),
				clearIndex(getSurveyTranslatedQuestion(survey, locale, param_question_code )),
				paste(
					getSurveyLabel(translated.values, "country"), ": ",
					getSurveyCountryTranslation(translated.values, param_country), ", ",
					getSurveyLabel(translated.values, "countryB"), ": ",
					getSurveyCountryTranslation(translated.values, param_countryB), ", ",

					getSurveyLabel(translated.values, "subset"), ": ",
					clearIndex(getSubsetTranslation(translated.values, subset)), ", ",
					clearIndex(getSubsetValueTranslation(translated.values, subset, param_subset_value)),

				sep = "")
			), width
		)

	subHeigths = c(heights[2])
	subPlotsParameters <- "chart,"

	if(!question.is.mean){
		subHeigths= c()
		subPlotsParameters <- ""
		subHeigthTitle = 3/10
		subHeigthPlot = 7/10
		subPlotMainHeigth = (heights[2]/length(subPlotValues))

		for(subPlotValue in 1:length(subPlotValues)){
			subHeigths = c(subHeigths, subPlotMainHeigth * subHeigthTitle, subPlotMainHeigth * subHeigthPlot  )
			subPlotsParameters <- paste(subPlotsParameters, "getTitleSubPlot(subPlotValues[",subPlotValue,"]), chart", subPlotValue,",", sep = "")
		}
	}

	if(entitle){
		heights = c(heights[1], subHeigths, heights[3])
		compileText <- paste("grid.arrange(titlesPlots,",subPlotsParameters, "bottomPlot, ncol = 1, heights = heights )",sep="")
	}else{
		heights = c( subHeigths )
		compileText <- paste("grid.arrange(",subPlotsParameters, " ncol = 1, heights = heights )", sep="")
	}

	eval(parse(text = compileText))


	dev.off()

	if(shouldOptimizePNG && media == "png")
		optimizePNG(file.visualization);
	if(media == "svg")
		optimizeSVG(file.visualization, width, height)

}


#-----------------------------------------------

