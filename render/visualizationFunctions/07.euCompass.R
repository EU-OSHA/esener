# ======================================================================
# Eworx S.A. - 2012/2013
# Author kp@eworx.gr
# ======================================================================
# defines the EU Compass Visualization function
# ======================================================================

euCompass <- function(
	survey,
	subset,
	param_subset_value,
	param_question_code,
	param_question_answer,
	locale = "EN",
	media = "png",
	width = standard.width,
	entitle = FALSE,
	height = standard.height,
	savePath = plots.location
){

# The visualization illustrates in a map the percentages of answers, or the mean answer value of a question per European Country.


	dynamicHeight = ifelse(entitle, standard.height, standard.height - (standard.height * (0.115)) )

	if(width != standard.width){
		height = ( width * dynamicHeight ) / standard.width
	}
 

	#-------------------------------------------------

	visualization.file.body.name <- paste("euCompass-", survey, "-", subset, "-", toUrlValue(param_subset_value), "-", param_question_code, "-", toUrlValue(param_question_answer), "-", locale, sep = "" )

	eval(parse(text =
		paste(
			"stat.data <- Survey", survey, "$", subset
			,sep = "")
		)
	)

	stat.data <- subset(stat.data,(
	 	CountryEUStatus== "EuropeanUnion" &
		CountryCode != "EuropeanUnion" & CountryCode != "EUCC"
	))


	#-----------------------------------------------
	# Data manipulation
	#-----------------------------------------------

	question.is.mean <- FALSE
	js.stat.data <- data.frame()

	if(param_question_answer == "Mean") {

		#-----------------------------------------------
		# Question mean handling
		#-----------------------------------------------

		question.is.mean <- TRUE

		stat.data <- subset(
			stat.data,
			(
				question_code == param_question_code &
				subset == param_subset_value
			)
		)

		stat.data <- covertQuestionMeanStatsToMeans(stat.data)[c("CountryEUStatus", "CountryCode", "question_code", "subset", "answer", "percentage")]

		js.stat.data <- stat.data
		js.stat.data[, "CC"] <- stat.data[, "CountryCode"]
		colnames(stat.data)[6] <- "Mean"

	}else{
		stat.data <- subset(
			stat.data,
			(
				question_code == param_question_code &
				subset == param_subset_value

			)
		)	[c("CountryEUStatus", "CountryCode", "question_code", "subset", "answer", "percentage")]

		js.stat.data <- stat.data
		js.stat.data[,"CC"] <- stat.data[,"CountryCode"]

		stat.data <- subset(stat.data, ( answer == param_question_answer ))
	}

	#-----------------------------------------------


	csv.data <- stat.data[c("CountryCode", "question_code", "subset", "answer", ifelse(question.is.mean, "Mean", "percentage"))]
	#previous slower#
	#csv.data  <- sqldf(paste(
	#	"select * from 'csv.data' order by ",ifelse(question.is.mean, "Mean", "percentage"), " desc ", sep = "")
	#)
	#optimized code#
	if(question.is.mean){
		term.order <- order(csv.data$Mean,decreasing=TRUE)
	}
	else{
		term.order <- order(csv.data$percentage,decreasing=TRUE)
	}
	csv.data <- csv.data[term.order,]
	#optimized code#
	
	if(question.is.mean){
		csv.data$Mean <- round(csv.data$Mean, 1)
	}else{
		csv.data$percentage <- round(csv.data$percentage, 1)
	}
	
	writeDataInCSV(
		csv.data,
		paste(plots.csv.location, visualization.file.body.name, ".xls", sep = "" )
	)

	#---------------------------
	# AUTO DESCRIPTOR
	#---------------------------
	translated.values <- getSurveyTranslatedValues(survey, locale)
	countries.translatation <- getSurveyCountriesTranslations(translated.values)

	
	#previous slower#
	#dat <- sqldf(paste(
	#	"select TranslatedValue Country, CountryEUStatus, CountryCode, question_code, subset, answer, ", ifelse(question.is.mean, "Mean", "percentage"), " percentage from 'stat.data' sd left outer join 'countries.translatation' ct on ct.Value=sd.CountryCode order by percentage desc ", sep = "")
	#)
	#optimized code (slight problem occuring in eval : line 43 object "Country" does not exist ) #
	if(question.is.mean){
		dat <- left.outer.join.function(c( "TranslatedValue", "CountryEUStatus", "CountryCode", "question_code", "subset", "answer","Mean"),stat.data,countries.translatation,"CountryCode","Value",c( "Country", "CountryEUStatus", "CountryCode", "question_code", "subset", "answer","percentage"))	
		sorting.order <- order(dat$percentage,decreasing = TRUE)
	}
	else{ 
		dat <- left.outer.join.function(c( "TranslatedValue", "CountryEUStatus", "CountryCode", "question_code", "subset", "answer","percentage"),stat.data,countries.translatation,"CountryCode","Value",c( "Country", "CountryEUStatus", "CountryCode", "question_code", "subset", "answer","percentage"))	
		sorting.order <- order(dat$percentage,decreasing = TRUE)
	}
	dat <- dat[sorting.order,]
	#optimized code#
	
	distribution <- dist(dat$percentage)
	hierarchicalClustering <- hclust(distribution)
	clusters4 = cutree(hierarchicalClustering, 3)
	dat[, "Level"] <- clusters4
	

	levels.count <- sqldf(paste("select Level, count(*) Count from 'dat' group by Level order by Level ", sep = ""))

	levels.count <- as.vector(levels.count$Count)

	description <- getSurveyPlotDescriptorTemplate(translated.values , "euCompass", question.is.mean )

	if(question.is.mean){
		description.values <- c(
		clearIndex(getSubsetTranslation(translated.values, subset)),
		clearIndex(getSubsetValueTranslation(translated.values, subset, param_subset_value)),
		clearIndex(getSurveyFullTranslatedQuestion(survey, locale, param_question_code )),
		#max.countries,
		levels.count
		)
	}else{
		description.values <- c(
		clearIndex(getSurveyQuestionTranslatedAnswer(translated.values, param_question_code, param_question_answer)),
		clearIndex(getSubsetTranslation(translated.values, subset)),
		clearIndex(getSubsetValueTranslation(translated.values, subset, param_subset_value)),
		getSurveyFullTranslatedQuestion(survey, locale, param_question_code ),
		levels.count
		)
	}

	description <- fillDescription(description, description.values)

	out.file <- paste(savePath, "/txt/", visualization.file.body.name,".htm", sep = "")
	con <- file(out.file)
	writeLines(c(description), con)
	close(con)

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
		cairo_pdf(filename = visualization.file.body.name , width =  13 * (width/standard.width), height = 13 *(height/standard.height), pointsize = 12, onefile = TRUE, bg = "white", antialias = c("default", "none", "gray", "subpixel"))
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

	#####################

	#stat.data[,"percentage"] <- stat.data[, ifelse(question.is.mean, "Mean", "percentage")]
	stat.data <- dat

	stat.data <- stat.data [with(stat.data , order(percentage)), ]

	stat.data[, "group"] <- clusterResults(stat.data$percentage, 3)
	stat.data$group <- paste("Group", stat.data$group)
	#stat.data[, "percentage"] <- round(stat.data[,"percentage"], 2)

	stat.data$fraction = 1 / nrow(stat.data) #stade
	stat.data$ymax = cumsum(stat.data$fraction) #stade 
	stat.data$ymin = c(0, head(stat.data$ymax, n = -1)) #stade

	stat.data$ymaxLabel <- stat.data$ymax - stat.data$fraction / 2
	
	stat.data[, "labelRotation"] <- stat.data$ymaxLabel * -360 + 90
	stat.data$score <- paste(round(stat.data$percentage, ifelse(question.is.mean, 1, 0)), ifelse(question.is.mean, "", "%"), sep = "")
	
	stat.data[, "orientation"] <- ifelse(stat.data$labelRotation < -90, 1, 0)
	stat.data$labelRotation <- ifelse(stat.data$orientation, stat.data$labelRotation + 180, stat.data$labelRotation)
	
	expand <- 0
	if(width < "741")
		expand <- expand + 0.3
	if(width < "501")
		expand <- expand + 0.3

	stat.data[, "scoreX"] <- 4.2 
	stat.data[, "countryX"] <- ifelse(question.is.mean, 4.7, 5) + expand
	

	map =  ggplot(stat.data, aes(fill = group, ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
	geom_rect() + xlim(c(0, 7)) +
	coord_polar(theta = "y") +
	geom_text( aes( x = scoreX, y = ymaxLabel, label = score, fontface = "bold", angle = labelRotation, colour = group, vjust = 0.5, hjust = orientation), size = 5) +
	geom_text( aes( x = countryX, y = ymaxLabel, label = Country, angle = labelRotation, colour = group, vjust = 0.5, hjust = orientation), size = 5) + 
	scale_fill_manual(values = pallet.clusters.groups.fills) +
	scale_colour_manual(values = pallet.clusters.groups.colors) +
	theme_ef_map()

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
						clearIndex(getSubsetValueTranslation(translated.values, subset, param_subset_value)),
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
			heights = c(0.115, 0.84, 0.045)
			#heights = c(0.14, 0.82, 0.04)
		)
		superImposeLogo(x = 0.85, y = -0.35)
	}else{
		grid.arrange(
			map,
			ncol = 1,
			heights = c(0.115 + 0.84 + 0.045)
			#heights = c(0.14, 0.82, 0.04)
		)
		superImposeLogo(x = 0.85, y = -0.39)

	}
	#print(map)

	dev.off()

	if(shouldOptimizePNG && media == "png")
		optimizePNG(file.visualization);

	if(media == "svg")
		optimizeSVG(file.visualization, width, height)

}

