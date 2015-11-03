# ======================================================================
# Eworx S.A. - 2012/2013
# Author kp@eworx.gr
# ======================================================================
# defines the euMatrix Visualization function
# ======================================================================

euMatrix <- function(
	survey = "3RDEQLS",
	param_question_code = "Y11_Q39c", # "Y11_Q28c",
	locale = "EN",
	media = "png",
	width = standard.width,
	euOnly = 0,
	entitle = FALSE,
	height = standard.height,
	savePath = plots.location
){

	visualization.file.body.name <- paste("euMatrix-", survey, "-", param_question_code, "-", locale, sep = "" )

	dynamicHeight = ifelse(entitle, standard.height, standard.height - (standard.height * (0.115)) )

	if(width != standard.width){
		height = ( width * dynamicHeight ) / standard.width
	}
 

	eval(parse(text =
		paste(
			"SurveyData <- Survey", survey
			,sep = "")
		)
	)

	translated.values <- getSurveyTranslatedValues(survey, locale)
	stat.data.all <- data.frame()

	for(subset in names(SurveyData)){

		eval(parse(text =paste("stat.data <- Survey", survey, "$", subset,sep = "")))

		stat.data <- subset(stat.data,	(question_code == param_question_code))

		if(subset == names(SurveyData[1]))
			stat.data.all <- stat.data
		else
			stat.data.all <- merge(stat.data, stat.data.all, all = TRUE)
	}

	if(euOnly)
		stat.data.all <- subset(stat.data.all, (CountryEUStatus == "EuropeanUnion" | CountryEUStatus == "EU3"))

	question.is.mean = FALSE;
	if(isQuestionMean(survey, param_question_code)){
		stat.data.all <- covertQuestionMeanStatsToMeans(stat.data.all)
		question.is.mean = TRUE;
		stat.data.all.csv <- stat.data.all[c("CountryCode", "question_code", "subset", "answer", "percentage")]		
		stat.data.all.csv$percentage <- round(stat.data.all.csv$percentage, 1)
		colnames(stat.data.all.csv)[5] <- "Mean"		
		writeDataInCSV(stat.data.all.csv, paste(plots.csv.location, visualization.file.body.name, ifelse(euOnly,"-EU",""),".xls", sep = "" ))
	}else{
		csv.data <- stat.data.all[c("CountryCode", "question_code", "subset", "answer", "percentage")]
		csv.data$percentage <- round(csv.data$percentage, 1)
		writeDataInCSV(csv.data, paste(plots.csv.location, visualization.file.body.name, ifelse(euOnly,"-EU",""), ".xls", sep = "" ))
	}

	for(subset in names(SurveyData)){
		# TRANSLATION / LOCALIZATION OF STAT DATA
		stat.data.all <- translateStatAllSubsetsDataValues(stat.data.all, translated.values, subset, param_question_code)
	}


	#Dynamic height calculation
	#----------------------------------
	totalItems = length(unique(stat.data.all$subset)) * length(unique(stat.data.all$answer))
	print(totalItems)
	if(width < 740){
		reduceFactor <- 25; optimoum <- 25
		diffFromOptimoum <- (totalItems - optimoum)
		if(diffFromOptimoum > 0){
			scaleFactor <- ( 1 + (diffFromOptimoum / reduceFactor) )
			height <- height * scaleFactor
		}
	}else
	if(width < 920){
		reduceFactor <- 35;optimoum <- 30
		diffFromOptimoum <- (totalItems - optimoum)
		if(diffFromOptimoum > 0){
			scaleFactor <- ( 1 + (diffFromOptimoum / reduceFactor) )
			height <- height * scaleFactor
		}
	}else{
		reduceFactor <- 35;optimoum <- 40
		if(totalItems < 15){
			reduceFactor <- 35;optimoum <- 20
		}
		diffFromOptimoum <- (totalItems - optimoum)

		scaleFactor <- ( 1 + (diffFromOptimoum / reduceFactor) )
		height <- height * scaleFactor

	}
	#----------------------------------

	translated.values <- getSurveyTranslatedValues(survey, locale)
	description <- getSurveyPlotDescriptorTemplate(translated.values , "euMatrix", TRUE )
	out.file <- paste(savePath, "/txt/", visualization.file.body.name,".htm", sep = "")
	con <- file(out.file)
	writeLines(c(description), con)
	close(con)

	file.visualization <- "";
	if(media == "png"){
		file.visualization <- paste(savePath, visualization.file.body.name, "-", width, ifelse(euOnly,"-EU",""), ifelse(shouldOptimizePNG, "_", ""), ".png", sep = "")
		Cairo(file = file.visualization, type = "png", width = width, height = height, bg = "white");
		#png(filename = paste(savePath, visualization.file.body.name, "-", width, ".png", sep = ""), width, height, units = "px", pointsize = 12, antialias = "subpixel" )
	}else
	if(media == "pdf"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width,ifelse(euOnly,"-EU",""), ".", media, sep = "")
		cairo_pdf(filename = visualization.file.body.name , width =  13 * (width/standard.width), height = 13 *(height/standard.height), pointsize = 12, onefile = TRUE, bg = "white", antialias = c("default", "none", "gray", "subpixel"))
	}else
	if(media == "eps"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width, ifelse(euOnly,"-EU",""), ".", media, sep = "")
		postscript(visualization.file.body.name, paper="special", width =  13 * (width/standard.width), height = 13 *(height/standard.height), horizontal = FALSE)
	}else
	if(media == "svg"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width, ifelse(euOnly,"-EU",""), sep = "")
		file.visualization <- paste(visualization.file.body.name, ".svg", sep = "")
		if(file.exists(file.visualization) || file.exists(paste(visualization.file.body.name, ".svg.gz", sep = "")) )
			return("");
		Cairo(file = visualization.file.body.name, type = "svg", width =  12.77  * (width/standard.width), height = 12.77  *(height/standard.height), bg = "white", pointsize = 12, units = "in");
	}else
	if(media == "xls"){
		return("");
	}

	stat.data.all$subset <- factor(
		stat.data.all$subset,
		levels = rev(sort(unique(stat.data.all$subset))),
		ordered = TRUE
	)

	stat.data.all <- clearIndexInColumnStrings(stat.data.all, "subset")
	stat.data.all <- clearIndexInColumnStrings(stat.data.all, "answer")

	stat.data.all$percentage <- round(stat.data.all$percentage, 2)

	valueLimits <- c()

	valueLimits <- c(min(stat.data.all$percentage), max(stat.data.all$percentage))

	dataMean <- (valueLimits[1] + valueLimits[2]) / 2

	levels(stat.data.all$CountryEUStatus) <- c(levels(stat.data.all$CountryEUStatus), "0", "1", "2")
	stat.data.all[which(stat.data.all$CountryCode == "EuropeanUnion"), "CountryEUStatus"] <- "0"
	stat.data.all[which(stat.data.all$CountryEUStatus == "EuropeanUnion"), "CountryEUStatus"] <- "1"
	stat.data.all[which(stat.data.all$CountryEUStatus == "EUCC"), "CountryEUStatus"] <- "2"

	map <- ggplot(stat.data.all, aes(y = factor(CountryCode), x = factor(subset), fill = percentage)) + geom_tile() + theme_ef_grid()  +

	#scale_fill_gradient2(low = pallet.map[2], high = pallet.map[1], guide = "colorbar") +


	scale_fill_gradient2(
		low = pallet.intensity[1],
		mid = pallet.intensity[6],
		high = pallet.intensity[12],
		midpoint = dataMean,
		na.value = "white",
		limits = valueLimits
	) +

	coord_flip() +
	facet_grid(answer ~ .)
	#facet_wrap(~ answer, ncol = 1)
	
	if(entitle){
		grid.arrange(
			getTitlesPlot(
				text =
				c(
					getSurveyTranslatedQuestionTopic(survey, locale, param_question_code),
					clearIndex(getSurveyTranslatedQuestion(survey, locale, param_question_code )),
					""
				), width, 0.45
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
			heights = c(0.065, 0.89, 0.045)
		)
		superImposeLogo(x = 0.021, y = ifelse(totalItems < 40, -0.38, -0.42))
	}else{
		grid.arrange(			
			map,
			ncol = 1,
			heights = c(0.065 + 0.89 + 0.045)
		)
		superImposeLogo(x = 0.021, y = ifelse(totalItems < 40, -0.43, -0.47))
	}

	#print(map)
	#addEFFootnote()
	
	dev.off()

	if(shouldOptimizePNG && media == "png")
		optimizePNG(file.visualization);
	if(media == "svg")
		optimizeSVG(file.visualization, width, height)
}
