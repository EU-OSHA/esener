# ======================================================================
# Eworx S.A. - 2012/2013
# Author kp@eworx.gr
# ======================================================================
# defines the word cloud map related Visualization functions
# ======================================================================

wordMap <- function(
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

	visualization.file.body.name <- paste("wordMap-", survey, "-", subset, "-", toUrlValue(param_subset_value), "-", param_question_code, "-", toUrlValue(param_question_answer), "-", locale, sep = "" )

	eval(parse(text =
		paste(
			"stat.data <- Survey", survey, "$", subset
			,sep = "")
		)
	)

	#-----------------------------------------------
	# Data manipulation
	#-----------------------------------------------
	stat.data <- subset(stat.data,(
	 	CountryEUStatus== "EuropeanUnion" &
		CountryCode != "EuropeanUnion" & CountryCode != "EUCC"
	))

	

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
				subset == param_subset_value &
				CountryEUStatus == "EuropeanUnion"
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
	js.stat.data <- translateStatDataValues(js.stat.data, translated.values, subset, param_question_code)
	countries.translatation <- getSurveyCountriesTranslations(translated.values)

	#previous slower#
	#max.countries <- sqldf(paste(
	#	"select TranslatedValue Country from 'stat.data' sd inner join 'countries.translatation' ct on ct.Value=sd.CountryCode order by ",
	#		ifelse(question.is.mean, "Mean", "percentage"),
	#	" desc limit 3", sep = "")
	#)
	#optimized code#
	if(question.is.mean){
		max.countries <- inner.join.function(c("TranslatedValue","Mean"),stat.data,countries.translatation,"CountryCode","Value",c("Country","Mean"))	
		sorting.order <- order(max.countries$Mean,decreasing = TRUE)
	}
	else{ 
		max.countries <- inner.join.function(c("TranslatedValue","percentage"),stat.data,countries.translatation,"CountryCode","Value",c("Country","percentage"))	
		sorting.order <- order(max.countries$percentage,decreasing = TRUE)
	}
	max.countries <- max.countries[sorting.order,]
	max.countries <- as.data.frame(max.countries[(1:3),])
	#optimized code#
	max.countries <- as.vector(max.countries$Country)
	max.countries <- paste(max.countries, collapse = ", ")

	#previous slower#
	#min.countries <- sqldf(paste(
	#	"select TranslatedValue Country from 'stat.data' sd inner join 'countries.translatation' ct on ct.Value=sd.CountryCode order by ",
	#	ifelse(question.is.mean, "Mean", "percentage"),
	#	" asc limit 3", sep = "")
	#)
	#optimized code#
	if(question.is.mean){
		min.countries <- inner.join.function(c("TranslatedValue","Mean"),stat.data,countries.translatation,"CountryCode","Value",c("Country","Mean"))	
		sorting.order <- order(min.countries$Mean)
	}
	else{ 
		min.countries <- inner.join.function(c("TranslatedValue","percentage"),stat.data,countries.translatation,"CountryCode","Value",c("Country","percentage"))	
		sorting.order <- order(min.countries$percentage)
	}
	min.countries <- min.countries[sorting.order,]
	min.countries <- as.data.frame(min.countries[(1:3),])
	#optimized code#
	min.countries <- as.vector(min.countries$Country)
	min.countries <- paste(min.countries, collapse = ", ")

	description <- getSurveyPlotDescriptorTemplate(translated.values , "heatMap", question.is.mean )

	if(param_question_answer == "Mean"){
		description.values <- c(
		getSubsetTranslation(translated.values, subset),
		clearIndex(getSubsetValueTranslation(translated.values, subset, param_subset_value)),
		clearIndex(getSurveyFullTranslatedQuestion(survey, locale, param_question_code )),
		max.countries, min.countries
		)
	}else{
		description.values <- c(
		clearIndex(getSurveyQuestionTranslatedAnswer(translated.values, param_question_code, param_question_answer)),
		getSubsetTranslation(translated.values, subset),
		clearIndex(getSubsetValueTranslation(translated.values, subset, param_subset_value)),
		getSurveyFullTranslatedQuestion(survey, locale, param_question_code ),
		max.countries, min.countries
		)
	}

	description <- fillDescription(description, description.values)

	out.file <- paste(savePath, "/txt/", visualization.file.body.name,".htm", sep = "")
	con <- file(out.file)
	writeLines(c(description), con)
	close(con)

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

	#library("wordcloud")
	#pal <- brewer.pal(9,"Blues") #Blues  | BuGn
	#pal <- pal[ -(1:4)]
	#pal <- c("#E2E7F0", "#1B4389", "#A7BD43", "#7FADBA", "#DCAB5D")
	
	stat.data <- sqldf(paste(
		"select TranslatedValue Country, CountryEUStatus, CountryCode, question_code, subset, answer, ", ifelse(question.is.mean, "Mean", "percentage"), " percentage from 'stat.data' sd left outer join 'countries.translatation' ct on ct.Value=sd.CountryCode order by percentage desc ", sep = "")
	)
	
	#stat.data[,"percentage"] <- stat.data[, ifelse(question.is.mean, "Mean", "percentage")]
	stat.data <- stat.data [with(stat.data , order(percentage)), ]
	stat.data[, "cluster"] <- clusterResults(stat.data$percentage, 3)
	stat.data[,"percentage"] <- round(stat.data[,"percentage"], 2)
	
	stat.data[, "index"] <- 1:nrow(stat.data)
	stat.data[, "index"] <- prePendZeroInNumber(stat.data[, "index"])	
	#stat.data[, "score"] <- round(stat.data[,"percentage"], 1)
	#stat.data[, "score"] <- paste(stat.data[, "index"], " (", stat.data[, "score"], ifelse(question.is.mean, "", "%"), ")")
	stat.data[, "score"] <- paste("#", stat.data[, "index"], sep = "")
	
	#stat.data[, "score"] <- paste(prePendZeroInNumber(stat.data[, "score"] ), stat.data[, "Country"] )
	
	map = ggplot(stat.data, aes(x = percentage, y = paste(score))) + 
	
	geom_smooth(aes(group = cluster, colour = factor(cluster) ), method = "lm" , se = FALSE ) + 
	scale_colour_manual(values = pallet.clusters.groups.light) +
	geom_text(aes(label = CountryCode)) + theme_ef_scatter() # + coord_polar(theta = "y") +	
	
	if(!question.is.mean)
		map = map +	scale_x_continuous(labels = percentageLabels)


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

