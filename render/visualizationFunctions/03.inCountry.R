# ======================================================================
# Eworx S.A. - 2012/2013
# Author kp@eworx.gr
# ======================================================================
# defines the inCountry / nation Visualization function
# ======================================================================

inCountry <-function(

	survey,
	subset,
	param_question_code,
	param_country,
	locale = "EN",
	media = "png",
	width = standard.width,
	entitle = FALSE,
	height = standard.height,
	savePath = plots.location

){

# The visualization illustrates the percentages of answers, or the mean answer values for a question per group of people for a European Country.

	dynamicHeight = ifelse(entitle, standard.height, standard.height - (standard.height * (0.115)) )

	if(width != standard.width){
		height = ( width * dynamicHeight ) / standard.width
	}

	visualization.file.body.name <- paste("inCountry-", survey, "-", subset, "-", param_question_code, "-", param_country, "-", locale, sep = "" )

	eval(parse(text = paste("stat.data <- Survey", survey, "$", subset, sep = "")))

	#-----------------------------------------------
	# Data manipulation
	#-----------------------------------------------

	countryData <- subset(stat.data, (CountryCode == param_country & question_code == param_question_code))

	#-----------------------------------------------
	# Question mean handling
	#-----------------------------------------------

	question.is.mean = FALSE;
	if(isQuestionMean(survey, param_question_code)){
		question.is.mean = TRUE;
		countryData <- covertQuestionMeanStatsToMeans(countryData)
		countryData <- countryData[c("CountryEUStatus", "CountryCode", "question_code", "subset", "answer", "percentage")]
		colnames(countryData)[6] <- "Mean"
		countryData [, "MeanRounded"] <- paste(round(countryData$Mean, 1), sep = "")
	}

	#-----------------------------------------------
	
	csv.data <- countryData[c("CountryCode", "question_code", "subset", "answer", ifelse(question.is.mean, "Mean", "percentage"))]
	
	if(question.is.mean)
		csv.data$Mean <- round(csv.data$Mean, 1)
	else
		csv.data$percentage <- round(csv.data$percentage, 1)
 
	writeDataInCSV(csv.data, paste(plots.csv.location, visualization.file.body.name, ".xls", sep = "" ))

	#---------------------------
	# AUTO DESCRIPTOR
	#---------------------------
	translated.values <- getSurveyTranslatedValues(survey, locale)
	description <- getSurveyPlotDescriptorTemplate(translated.values , "inCountry", question.is.mean )

	#percentage	test<- "In the above figure, we see the percentages of the answers of %1 by \"%2\" when questioned \"%3\". [For the \"%1\" answer the \"%2\" group has the highest value.]"
	#mean	In the above figure, we see the mean value for %1 by "%2" when questioned "%3". ordered ...: [For the "%1" group the mean value is %2.]

	description.group <- gsub("\\]","", gsub(".*\\[","",description))
	description.groups <- ""
	description <- gsub("\\[.*\\]","",description)

	description.values <- c(
		getSurveyCountryTranslation(translated.values, param_country),
		clearIndex(getSubsetTranslation(translated.values, subset)),
		getSurveyFullTranslatedQuestion(survey, locale, param_question_code )
	)

	description <- fillDescription(description, description.values)



	totalItems <- length(unique(countryData$subset))
	totalAnswers <- 1
	if(question.is.mean){

		##previous slower#
		#subsetsMean <- sqldf("select subset, MeanRounded from countryData order by Mean desc")
		#optimized code#
		Mean.order <- order(countryData$Mean,decreasing=TRUE)
		subsetMean <- countryData[Mean.order,]
		subsetsMean <- subsetMean[,c("subset","MeanRounded")]
		#optimized code#

		for(i in 1:nrow(subsetsMean)) {
			row <- subsetsMean[i,]
			description.groups <- paste(
			description.groups,
			fillDescription(description.group, c(clearIndex(getSubsetValueTranslation(translated.values, subset, as.character(factor(row$subset)))),
			sub('\\.','·', 
				as.character(factor(row$MeanRounded))
			)
			)))
		}

	}else{

			#previous slower#
			#answers <- sqldf("select distinct(answer) answer from countryData ")
			#optimized code#
			answers <- data.frame(answer = unique(countryData[,"answer"]))
			#optimized code#
			totalAnswers <- length(unique(countryData$answer))
			totalItems <- totalItems * length(unique(countryData$answer))

			for(answer in as.vector(answers$answer)){
				#previous slower#
				#qresult <- sqldf(paste("select subset from countryData where answer='", answer,"' order by percentage desc limit 1", sep = ""))
				#optimized code
				answer.temp = answer
				qresult <- as.data.frame(subset(countryData, answer == answer.temp)[,c("subset","percentage")])
				percentage.order <- order(qresult$percentage,decreasing = TRUE)
				qresult <- qresult[percentage.order,]
				qresult <- qresult[1,c("subset")]
				#optimized code
				

				description.groups <- paste(
					description.groups,
					fillDescription(description.group, c(clearIndex(getSurveyQuestionTranslatedAnswer(translated.values, param_question_code, answer)),
					clearIndex(getSubsetValueTranslation(translated.values, subset, as.character((qresult)))) ) ))
			}

			#Dynamic height calculation
			#----------------------------------

			if(width < 740){
				reduceFactor <- 20; optimoum <- 7
				diffFromOptimoum <- (totalItems - optimoum)
				if(diffFromOptimoum > 0){
					scaleFactor <- ( 1 + (diffFromOptimoum / reduceFactor) )
					height <- height * scaleFactor
				}
			}else
			if(width < 920){
				reduceFactor <- 35;optimoum <- 10
				diffFromOptimoum <- (totalItems - optimoum)
				if(diffFromOptimoum > 0){
					scaleFactor <- ( 1 + (diffFromOptimoum / reduceFactor) )
					height <- height * scaleFactor
				}
			}
		}




	description <- paste(description, description.groups)

	out.file <- paste(savePath, "/txt/", visualization.file.body.name,".htm", sep = "")
	con <- file(out.file)
	writeLines(c(description), con)
	close(con)

	#---------------------------
	#---------------------------


	#---------------------------
	# AUTO TRANSLATION / LOCALIZATION OF STAT DATA
	#---------------------------

	countryData <- translateStatDataValues(countryData, translated.values, subset, param_question_code)

	countryData[, "PercentageRounded"] <- 1;
	if(question.is.mean)
		countryData[, "PercentageRounded"] <- round(   countryData$Mean , 1)
	else
		countryData[, "PercentageRounded"] <- paste(round(  countryData$percentage , 1),  "%")

	#-----------------------------------------------

	countryData$answer <- factor(
		countryData$answer,
		levels = rev(sort(unique(countryData$answer))),
		ordered = TRUE
	)

	#-----------------------------------------------
	#-----------------------------------------------
	file.visualization <- "";
	if(media == "png"){
		file.visualization <- paste(savePath, visualization.file.body.name, "-", width, ifelse(shouldOptimizePNG, "_", ""), ".png", sep = "")
		Cairo(file = file.visualization, type = "png", width = width, height = height, bg = "white");
		#png(filename = paste(savePath, visualization.file.body.name, "-", width, ".png", sep = ""), width, height, units = "px", pointsize = 12, antialias = "subpixel" )
	}else
	if(media == "pdf"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width, ".", media, sep = "")
		cairo_pdf(visualization.file.body.name , width =  13 * (width/standard.width), height = 13 *(height/standard.height), pointsize = 12, onefile = TRUE, bg = "white", antialias = c("default", "none", "gray", "subpixel"))
	}else
	if(media == "eps"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width, ".", media, sep = "")
		postscript(visualization.file.body.name, paper="special", width =  13 * (width/standard.width), height = 13 *(height/standard.height), horizontal = FALSE)
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

	#------------------------------------------------------------------
	subPlotValues = unique(countryData$subset)
	#------------------------------------------------------------------

	chart <- ""
	if(question.is.mean){
	
		valueLimits <- c(min(countryData$Mean), max(countryData$Mean))
		dataMean <- (valueLimits[1] + valueLimits[2]) / 2

		yLim = c( min(countryData$Mean) * 0.95, max(countryData$Mean) * 1.05);
		chart <- ggplot(
			countryData,
			aes(x = subset, y = Mean, fill = Mean)
		) + theme_ef_incountry() +
			geom_bar(stat="identity") +
			coord_cartesian() +

			#scale_fill_gradient2(low = pallet.map[2], high = pallet.map[1], guide = "colorbar") +
			scale_fill_gradient2(
				low = pallet.intensity[1],
				mid = pallet.intensity[6],
				high = pallet.intensity[12],
				midpoint = dataMean,
				na.value = pallet.map.grid,
				limits = valueLimits
			) +
						
			geom_text(aes(label = MeanRounded, hjust=-0.3), color="grey70") +

			##facet_grid(subset ~ .) +
			coord_flip(ylim = yLim)

	}else{
		###############################################################

		originalCountryData <- countryData

		


		originalCountryData[, "percentageTextValue"] <- originalCountryData$percentage + 1

		yLim = c(0, max(originalCountryData$percentage) * ifelse(width >= 920, 1.1, ifelse( width >= 740, 1.2, 1.3)));
		twoquestionHeigths <-  c(0.225, 0.68, 0.095)

		if(length(subPlotValues) < 5){
			diff <- 5 - length(subPlotValues)
			heights <- heights + ((twoquestionHeigths - heights ) / 4) * diff
		}

		#------------------------------------------------------------------
		plotIndex = 0;

		for(subPlotValue in (subPlotValues)){
			plotIndex = plotIndex+1
			countryData <- subset(originalCountryData, (subset == subPlotValue))

			##countryData <- clearIndexInColumnStrings(countryData, "subset")
			
# countryData <- clearIndexInColumnStrings(countryData, "answer")
 
			#### check if there is a NA answer
			groupAnswers <- as.vector(unique(countryData$answer))
			originalGroupAnswers <- as.vector(unique(originalCountryData$answer))

			if(length(groupAnswers) != length(originalGroupAnswers)){

				for(answer in originalGroupAnswers){
					if(!(answer %in% groupAnswers)){
						indexToadd=length(countryData[,1])+1
						countryData[indexToadd,]<-countryData[1,]
						countryData<-setValueInCell(countryData, "answer", indexToadd, answer)
						countryData<-setValueInCell(countryData, "percentage", indexToadd, 0)
						countryData<-setValueInCell(countryData, "PercentageRounded", indexToadd, "")
						countryData<-setValueInCell(countryData, "percentageTextValue", indexToadd, 0)
					}
				}
			}
			####
countryData <- clearIndexInColumnStrings(countryData, "answer")

			pallet.custom <- rev(pallet.clusters[1:totalAnswers])

			chart <- ggplot(
				countryData,
				aes(x = answer, y = percentage, fill = answer)
			) +
				geom_bar(stat="identity") +
				coord_cartesian() +
				scale_fill_manual(values = pallet.custom ) +
				geom_text(aes(y = percentageTextValue, label = PercentageRounded, hjust = 0), color = "grey70") +
				coord_flip(ylim = yLim)

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
	titlesPlots <-  getTitlesPlot(
			text=
			c(
				getSurveyTranslatedQuestionTopic(survey, locale, param_question_code),
				clearIndex(getSurveyTranslatedQuestion(survey, locale, param_question_code )),
				paste(
					getSurveyLabel(translated.values, "country"), ": ",
					getSurveyCountryTranslation(translated.values, param_country), ", ",
					getSurveyLabel(translated.values, "subset"), ": ",
					clearIndex(getSubsetTranslation(translated.values, subset)),
					sep = ""
				)
			), width
		)

	subHeigths = c(heights[2])
	subPlotsParameters <- "chart,"

	if(!question.is.mean){
		subHeigths = c()
		subPlotsParameters <- ""
		reduce <- 1

		if(width < 740){
			 reduce <- 3
			 if(totalItems>25)
			 	reduce <- 4
			 else
			 if(totalItems>15)
			 	reduce <- 2.3
		}else
		if(width < 920){
			 reduce <- 2
			 if(totalItems>20)
			 	reduce <- 2.7
			 else
			 if(totalItems>11)
			 	reduce <- 2.5


		}else
			if(totalItems>30)
				reduce <- 2.6
			else
			if(totalItems>15)
				reduce <- 2.2
			else
			if(totalItems>9)
				reduce <- 1.9


		subHeigthTitle = reduce/10
		subHeigthPlot = (10 - reduce)/10

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


# ======================================================================
#inCountry("3RDEQLS","Y11_HH2a","Y11_Q13a","AT");
#?plot=inCountry&dataSource=3RDEQLS&subset=Y11_HH2a&question=Y11_Q50a&country=EL

# ======================================================================

