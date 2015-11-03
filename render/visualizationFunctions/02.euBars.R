# ======================================================================
# defines the euBars Visualization function
# ======================================================================

euBars <- function(
	survey = "3RDEQLS",
	subset = "Y11_Agecategory",
	param_subset_value = "All",
	param_question_code = "Y11_Partnerprefhours",
	locale = "EN",
	media = "png",
	width = standard.width,
	euOnly = 0,
	entitle = FALSE,
	height = standard.height,
	savePath = plots.location

){

# The visualization illustrates the percentages of answers, or the mean answer value per European Country and across all European Countries for a question.

	dynamicHeight = ifelse(entitle, standard.height, standard.height - (standard.height * (0.115)) )

	if(width != standard.width){
		height = ( width * dynamicHeight ) / standard.width
	}
 

	visualization.file.body.name <- paste("euBars-", survey, "-", subset, "-", toUrlValue(param_subset_value), "-", param_question_code, "-", locale, sep = "" )

	eval(parse(text =
		paste(
			"stat.data <- Survey", survey, "$", subset
			,sep = "")
		)
	)
	

	#-----------------------------------------------
	# Data manipulation
	#-----------------------------------------------

	stat.data <- subset(
		stat.data,
		(
			question_code == param_question_code &
			subset == param_subset_value
		)
	)
	
	if(euOnly)
		stat.data <- subset(stat.data, (CountryEUStatus == "EuropeanUnion" | CountryEUStatus == "EU3"))
	#-----------------------------------------------
	# Question mean handling
	#-----------------------------------------------

	question.is.mean = FALSE;
	if(isQuestionMean(survey, param_question_code)){
		question.is.mean = TRUE;
		#print(stat.data )
		stat.data <- covertQuestionMeanStatsToMeans(stat.data)
		#print(stat.data )
		stat.data <- stat.data[c("CountryEUStatus", "CountryCode", "question_code", "subset", "answer", "percentage")]

		colnames(stat.data)[6] <- "Mean"
		stat.data[, "MeanRounded"] <- paste(round(stat.data$Mean, 1), sep = "")
	}else{
		stat.data <- stat.data[c("CountryEUStatus", "CountryCode", "question_code", "subset", "answer", "percentage")]
	}

	#-----------------------------------------------

	csv.data <- stat.data[c("CountryCode", "question_code", "subset", "answer", ifelse(question.is.mean, "Mean", "percentage"))]

	if(question.is.mean)
		csv.data$Mean <- round(csv.data$Mean, 1)
	else
		csv.data$percentage <- round(csv.data$percentage, 1)
	
	writeDataInCSV(csv.data, paste(plots.csv.location, visualization.file.body.name, ifelse(euOnly,"-EU",""), ".xls", sep = "" ))

	#-----------------------------------------------

	#---------------------------
	# AUTO DESCRIPTOR
	#---------------------------
	translated.values <- getSurveyTranslatedValues(survey, locale)
	description <- getSurveyPlotDescriptorTemplate(translated.values , "euBars", question.is.mean )

	description.group <- gsub("\\]","",gsub(".*\\[","",description))
	description.groups <- ""
	description <- gsub("\\[.*\\]","",description)

	if(question.is.mean){

		countries.translatation <- getSurveyCountriesTranslations(translated.values)

		#previous slower#
		#max.countries <- sqldf(paste("select TranslatedValue Country from 'stat.data' sd inner join 'countries.translatation' ct on ct.Value=sd.CountryCode order by Mean desc limit 3", sep = ""))
		#optimized code#
		max.countries <- inner.join.function(c("TranslatedValue","Mean"),stat.data,countries.translatation,"CountryCode","Value",c("Country","Mean"))
		sorting.order <- order(max.countries$Mean,decreasing = TRUE)
		max.countries <- max.countries[sorting.order,]
		max.countries <- as.data.frame(max.countries[(1:3),])
		#optimized code#
		max.countries <- as.vector(max.countries$Country)
		max.countries <- paste(max.countries, collapse = ", ")

		#previous slower#
		#min.countries <- sqldf(paste("select TranslatedValue Country from 'stat.data' sd inner join 'countries.translatation' ct on ct.Value=sd.CountryCode order by Mean asc limit 3", sep = ""))
		#optimized code#
		min.countries <- inner.join.function(c("TranslatedValue","Mean"),stat.data,countries.translatation,"CountryCode","Value",c("Country","Mean"))
		sorting.order <- order(min.countries$Mean,decreasing = TRUE)
		min.countries <- min.countries[sorting.order,]
		min.countries <- as.data.frame(min.countries[(1:3),])
		#optimized code#
		min.countries <- as.vector(min.countries$Country)
		min.countries <- paste(min.countries, collapse = ", ")

		description.values <- c(
			clearIndex(getSubsetTranslation(translated.values, subset)),
			clearIndex(getSubsetValueTranslation(translated.values, subset, param_subset_value)),
			clearIndex(getSurveyFullTranslatedQuestion(survey, locale, param_question_code )),
			max.countries,
			min.countries
		)

		description <- fillDescription(description, description.values)

	}else{

		description.values <- c(
			getSubsetTranslation(translated.values, subset),
			clearIndex(getSubsetValueTranslation(translated.values, subset, param_subset_value)),
			getSurveyFullTranslatedQuestion(survey, locale, param_question_code )
		)

		description <- fillDescription(description, description.values)

		#[For answer '%1', %2 has the highest percentage and %3 the lowest.]
		#previous slower#
		#answers <- sqldf("select distinct(answer) answer from 'stat.data' ")
		answers <- data.frame(answer = unique(stat.data[,"answer"]))
		for(answer in as.vector(answers$answer)){
			#previous slower#
			#qresult <- sqldf(paste("select CountryCode from 'stat.data' where answer='", answer,"' order by percentage desc limit 1", sep = ""))
			#optimized code#
			percentage.order <- order(stat.data$percentage,decreasing=TRUE)
			qresult <- stat.data[percentage.order,]
			qresult <- as.data.frame(qresult[qresult$answer == answer,c("CountryCode")])
			qresult <- as.data.frame(qresult[1,])
			names(qresult) = "CountryCode"
			#optimized code#
			
			#previous slower#
			#mresult <- sqldf(paste("select CountryCode from 'stat.data' where answer='", answer,"' order by percentage asc limit 1", sep = ""))
			#optimized code#
			percentage.order <- order(stat.data$percentage)
			mresult <- stat.data[percentage.order,]
			mresult <- as.data.frame(mresult[mresult$answer == answer,c("CountryCode")])
			mresult <- as.data.frame(mresult[1,])
			names(mresult) = "CountryCode"
			#optimized code#

			description.groups <- paste(
				description.groups,
				fillDescription(description.group, c(clearIndex(getSurveyQuestionTranslatedAnswer(translated.values, param_question_code, answer)),
				getSurveyCountryTranslation(translated.values, as.character(factor(qresult[1,1]))),
				getSurveyCountryTranslation(translated.values, as.character(factor(mresult[1,1]))) ) ))
		}

		description <- paste(description, description.groups)
	}


	out.file <- paste(savePath, "/txt/", visualization.file.body.name,".htm", sep = "")
	con <- file(out.file)
	writeLines(c(description), con)
	close(con)

	#---------------------------
	#---------------------------

	#-----------------------------------------------
	# groups - countries and region - like the EU 27
	levels(stat.data$CountryEUStatus) <- c(levels(stat.data$CountryEUStatus), "0", "1", "2")
	stat.data[which(stat.data$CountryCode == "EuropeanUnion"), "CountryEUStatus"] <- "0"
	stat.data[which(stat.data$CountryEUStatus == "EuropeanUnion"), "CountryEUStatus"] <- "1"
	stat.data[which(stat.data$CountryEUStatus == "EUCC"), "CountryEUStatus"] <- "2"

	#-----------------------------------------------

	#---------------------------
	# AUTO TRANSLATION / LOCALIZATION OF STAT DATA
	#---------------------------

	stat.data <- translateStatDataValues(stat.data, translated.values, subset, param_question_code)

	#-----------------------------------------------
	file.visualization <- "";
	if(media == "png"){
		file.visualization <- paste(savePath, visualization.file.body.name, "-", width, ifelse(euOnly,"-EU",""), ifelse(shouldOptimizePNG, "_", ""), ".png", sep = "")
		Cairo(file = file.visualization, type = "png", width = width, height = height, bg = "white");
		#png(filename = paste(savePath, visualization.file.body.name, "-", width, ".png", sep = ""), width, height, units = "px", pointsize = 12, antialias = "subpixel" )
	}else
	if(media == "pdf"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width, ifelse(euOnly,"-EU",""), ".", media, sep = "")
		cairo_pdf(filename = visualization.file.body.name, width =  13 * (width/standard.width), height = 13 *(height/standard.height), pointsize = 12, onefile = TRUE, bg = "white", antialias = c("default", "none", "gray", "subpixel"))
	}else
	if(media == "eps"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width, ifelse(euOnly,"-EU",""), ".", media, sep = "")
		postscript(visualization.file.body.name, paper="special",width =  13 * (width/standard.width), height = 13 *(height/standard.height), horizontal = FALSE)
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


	#-----------------------------------------------
	#-----------------------------------------------
	#-----------------------------------------------

	# Stacked bar charts per country should add up to 100%
	# This is applied for MRQs and
	# for standard questions as well due to the value to integer conversion (zero decimal place precission), sums do not add up to exactly 100%
	# The appliance should not affect question means as there we don't have stacked charts and country questions

	#-----------------------------------------------


	if(!question.is.mean){

		stat.data[, "normalized"] <- stat.data[, "percentage"]

		stat.data.normalized <- data.frame()

		byColumn = "CountryCode"
		byColumnDistinctValues <- unique(stat.data[, byColumn])

		for(byColumnDistinctValue in byColumnDistinctValues){

			#previous slower#
			#dataNormalized <- sqldf(paste("select * from 'stat.data' where ", byColumn, " = '", byColumnDistinctValue, "'", sep = ""))
			#optimized code#
			command <- paste("stat.data[stat.data$",byColumn," == '",byColumnDistinctValue,"',]",sep="")
			dataNormalized <- eval(parse(text = command))
			#optimized code#

			sumData <- sum(dataNormalized$normalized)
			dataNormalized$normalized <- dataNormalized$normalized / sumData
			dataNormalized$normalized <- dataNormalized$normalized * 100

			if(byColumnDistinctValue == byColumnDistinctValues[1])
				stat.data.normalized <- dataNormalized
			else
				stat.data.normalized <- rbind(stat.data.normalized, dataNormalized)
		}
		stat.data <- stat.data.normalized
		stat.data[, "normalizedLabels"] <- round(stat.data$normalized, 0)
		stat.data[, "normalizedLabels2"]	<- ifelse(stat.data[, "normalizedLabels"]<4,"", stat.data[, "normalizedLabels"])

		#print(stat.data)
		#print(names(stat.data))
	}


	#-----------------------------------------------
	# Visualization
	#-----------------------------------------------
	chart <- ""

	valueLimits <- c(0)
	if(question.is.mean)
		valueLimits <- c(min(stat.data$Mean) , max(stat.data$Mean))
	else
		valueLimits <- c(valueLimits , 100)

	dataMean <- (valueLimits[1] + valueLimits[2]) / 2

	if(question.is.mean){
		distance = max(stat.data$Mean) - min(stat.data$Mean);
		distance.marging = distance * 1/20

		yLim = c( min(stat.data$Mean)- distance.marging, max(stat.data$Mean) + distance.marging * ifelse(height>740, 2, ifelse(height>500, 4, 10)));

		chart <- ggplot(
			stat.data,
			aes(x = CountryCode, y = Mean, fill = Mean), axes = FALSE
		) + coord_cartesian(ylim = yLim ) +

			#scale_fill_gradient2(low = pallet.map[2], high = pallet.map[1], guide = "colorbar") +

			scale_fill_gradient2(
				low = pallet.intensity[1],
				mid = pallet.intensity[6],
				high = pallet.intensity[12],
				midpoint = dataMean,
				na.value = pallet.map.grid,
				limits = valueLimits
			) +

			geom_text(aes(label = MeanRounded), vjust = ifelse(height > 740, 0.5, 0.5), hjust = ifelse(height > 740, -0.3, -0.3), color="grey70", angle = ifelse(height>740, 90, 90)) +

			theme_ef_stacked() +
			geom_bar(stat="identity") +
			facet_grid(. ~ CountryEUStatus, scales = "free", space = "free")

	}else{
		stat.data <- clearIndexInColumnStrings(stat.data, "answer")
		chart <- ggplot(
			stat.data,
			aes(x = CountryCode, y = percentage, fill = answer), axes = FALSE
		) + coord_cartesian(ylim = c(0, 100)) +
		 	scale_fill_manual(values = pallet.clusters) +
			theme_ef_stacked() +
			geom_bar(stat="identity") +
			geom_text(
				aes(label = normalizedLabels2),
				size = 3, hjust = 0.5, vjust = 2, position = "stack", colour = "white"
			) +
			facet_grid(. ~ CountryEUStatus, scales = "free", space = "free")

	}

	if(entitle){
		grid.arrange(
			getTitlesPlot(text=
				c(
					getSurveyTranslatedQuestionTopic(survey, locale, param_question_code),
					clearIndex(getSurveyTranslatedQuestion(survey, locale, param_question_code )),
					paste(
						getSurveyLabel(translated.values, "subset"), ": ",
						clearIndex(getSubsetTranslation(translated.values, subset)), ", ",
						clearIndex(getSubsetValueTranslation(translated.values, subset, param_subset_value)),
						sep = ""
					)
				), width
			),
			chart,
			getBottomPlot(
				paste(
					standard.copyright,
					getSurveyTranslatedValue(translated.values),
					sep = " - "
				)
			),
			ncol = 1,
			heights = c(0.115, 0.84, 0.045)
		)

		#superImposeLogo(x = 0.8, y = -0.39)
	}else{
		grid.arrange( chart, ncol = 1, heights = c(0.115 + 0.84 + 0.045))
		#superImposeLogo(x = 0.8, y = -0.42)
	}

	dev.off()
	if(shouldOptimizePNG && media == "png")
		optimizePNG(file.visualization);
	if(media == "svg")
		optimizeSVG(file.visualization, width, height)
}


#euBars("3RDEQLS","Y11_HH2a","Y11_Q13a");
#?plot=chartD&dataSource=3RDEQLS&subset=Y11_HH2a&question=Y11_Q50a&country=EL

#---------------------------------------------
