#==============================================
# defines the map related Visualization functions
# ======================================================================

heatMap <- function(
	survey,
	subset,
	param_subset_value,
	param_question_code,
	param_question_answer,
	locale = "EN",
	media = "png",
	width = standard.width,
	euOnly = 0,
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

	visualization.file.body.name <- paste("heatMap-", survey, "-", subset, "-", toUrlValue(param_subset_value), "-", param_question_code, "-", toUrlValue(param_question_answer), "-", locale, sep = "" )

	eval(parse(text =
		paste(
			"stat.data <- Survey", survey, "$", subset
			,sep = "")
		)
	)

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

		stat.data <- subset(stat.data,(
			question_code == param_question_code &
			subset == param_subset_value
		))

		stat.data <- subset(stat.data,(
			CountryCode != "EuropeanUnion" & CountryCode != "EUCC" 
		))

		if(euOnly)
			stat.data <- subset(stat.data, (CountryEUStatus == "EuropeanUnion"))


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

		stat.data <- subset(stat.data,(
			CountryCode != "EuropeanUnion" & CountryCode != "EUCC" 
		))
		if(euOnly)
			stat.data <- subset(stat.data, (CountryEUStatus == "EuropeanUnion" ))

		js.stat.data <- stat.data
		js.stat.data[, "CC"] <- stat.data[, "CountryCode"]

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
		paste(plots.csv.location, visualization.file.body.name, ifelse(euOnly,"-EU",""), ".xls", sep = "" )
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

	out.file <- paste(savePath, "/txt/", visualization.file.body.name, ifelse(euOnly,"-EU",""),".htm", sep = "")
	con <- file(out.file)
	writeLines(c(description), con)
	close(con)

	#---------------------------

	js.stat.data <- js.stat.data[c("CC","CountryCode","answer", "percentage")]
	js.stat.data[,"percentage"] <- round(js.stat.data[,"percentage"], 1)

	js.stat.data[, "NoIndex"] <- clearIndex(js.stat.data$answer)
	js.stat.data <- js.stat.data[c("CC", "CountryCode", "percentage","NoIndex" )]
	names(js.stat.data)[names(js.stat.data) == "NoIndex"] <- "answer"
	js.stat.data <- js.stat.data[c("CC","CountryCode","answer", "percentage")]

	out.file <- paste(savePath, "/js/", visualization.file.body.name,ifelse(euOnly,"-EU",""),".js", sep = "")
	con <- file(out.file)
	writeLines(c(toJavascriptArray(js.stat.data, "var heatMapTooltips")), con)
	close(con)

	#---------------------------

	################
	################
	################
	location <- "/web-pub/foundation/html/DVS/import/map/"
	eu.map <- read.csv(paste(location, "european.map.v2.xls", sep = ""), sep = "\t")
	
	heat.map <- sqldf( paste(
	"
		select
			 V1, V2, eumap.country country, boundaryId, countryType, CountryCode, question_code,", ifelse(question.is.mean, "Mean", "percentage"), "
		from
			'eu.map' eumap left outer join
			'stat.data' statdata
				on eumap.country = statdata.CountryCode
	"
	))

	#-----------------------------------------------
	#-----------------------------------------------

	file.visualization <- "";
	if(media == "png"){
		file.visualization <- paste(savePath, visualization.file.body.name, "-", width, ifelse(euOnly,"-EU",""), ifelse(shouldOptimizePNG, "_", ""), ".png", sep = "")
		Cairo(file = file.visualization , type = "png", width = width, height = height, bg = "white", antialias = "subpixel");
	}else
	if(media == "pdf"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width, ifelse(euOnly,"-EU",""), ".", media, sep = "")
		cairo_pdf(filename = visualization.file.body.name, width =  13 * (width/standard.width), height = 13 *(height/standard.height.map), pointsize = 12, onefile = TRUE, bg = "white", family = "Open Sans", antialias = c("default", "none", "gray", "subpixel"))
	}else
	if(media == "eps"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width, ifelse(euOnly,"-EU",""), ".", media, sep = "")
		postscript(visualization.file.body.name, paper="special", width =  13 * (width/standard.width), height = 13 *(height/standard.height.map), horizontal = FALSE)
	}else
	if(media == "svg"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width, ifelse(euOnly,"-EU",""), sep = "")
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

	dataDist <- stat.data

	map <- ggplot(heat.map, aes(V1, V2, group = country ))

	map <- map + coord_map(project = "globular", xlim = c(-9, 44), ylim = c(35, 70))

	if(question.is.mean){
		map <- map + geom_polygon(aes(fill = Mean))
		dataDist<-data.frame(value = dataDist$Mean)
	}else{
		map <- map + geom_polygon(aes(fill = percentage))
		dataDist<-data.frame(value = dataDist$percentage)
	}

	map <- map + geom_path(aes(V1, V2), data = heat.map, colour = "white", size = 0.1)


	uniqueCountries <- data.frame(CountryCode = unique(stat.data$CountryCode))
	#previous slower#
	#uniqueCountries.centers <- sqldf("select * from 'countries.centers' cc inner join uniqueCountries uc on cc.country = uc.CountryCode ")
	uniqueCountries.centers <- inner.join.function(c(),countries.centers,uniqueCountries,"country","CountryCode")

	map <- map + geom_text(aes(x = V1, y = V2, label = country, fontface = "bold"), colour = pallet.intensity[4], data = uniqueCountries.centers)

	map <- map + geom_text(aes(x = V1, y = V2, label = country), colour = "white", data = countries.centers)
	map <- map + geom_text(aes(x = V1, y = V2, label = country), colour = "white", data = countries.centers.non.eu)

	#map <- map + scale_fill_gradient2(low = pallet.map[2], high = pallet.map[1], guide = "colorbar",
	#		name = getSurveyWord(translated.values, ifelse(question.is.mean, "mean", "percentage"))
	#)


	valueLimits <- c()

	if(question.is.mean)
		valueLimits <- c(min(stat.data$Mean), max(stat.data$Mean))
	else
		valueLimits <- c(min(stat.data$percentage), max(stat.data$percentage))

	dataMean <- (valueLimits[1] + valueLimits[2]) / 2

	map <- map + scale_fill_gradient2(
		limits = valueLimits,
		midpoint = dataMean,
		low = pallet.intensity[1],
		mid = pallet.intensity[6],
		high = pallet.intensity[12],
		na.value = pallet.map.grid
	)

	MT<- subset(countries.centers, country=="MT")

	map <- map +  annotate("text", label = "MT", x = MT$V1, y = MT$V2, colour = "#a6a6a6") + theme_ef_map()

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
			getValuesDistributionPlot(dataDist),
			getGradientPlot(valueLimits[1], dataMean, valueLimits[2], question.is.mean),
			getBottomPlot(
				paste(
					standard.copyright,
					getSurveyTranslatedValue(translated.values),
					sep = " - "
				)
			),
			ncol = 1,
			heights = c(0.115, 0.737, 0.09, 0.04, 0.045)
			#heights = c(0.14, 0.82, 0.04)
		)

	}else{

		grid.arrange(
				map,
				getValuesDistributionPlot(dataDist),
				getGradientPlot(valueLimits[1], dataMean, valueLimits[2], question.is.mean),
				ncol = 1,
				heights = c(0.115 + 0.737 + 0.045, 0.09 , 0.04 )
				#heights = c(0.115, 0.737, 0.09, 0.04, 0.045)
				#heights = c(0.14, 0.82, 0.04)
		)
	}


	#superImposeShape()#superimposes a transparent shape on top of Malta

	if(entitle){
		pushViewport(viewport( x = 0.413, y = -0.31, width = 0.1, just = c("left", "bottom")))
	}else{
		pushViewport(viewport( x = 0.413, y = -0.35, width = 0.1, just = c("left", "bottom")))
	}

	grid.circle(r = 0.2, gp = gpar(fill = rgb(0.1, 0.1, 0.1, 0.1), alpha=0.1, lwd = 0))

	upViewport()
	#superImposeLogo(x = 0.79, y = 0.38)
	#print(map)
	#addEFFootnote()
	dev.off()

	if(media == "png"  && shouldOptimizePNG)
		optimizePNG(file.visualization)

	if(media == "svg")
		optimizeSVG(file.visualization, width, height)
}

#heatMap("3RDEQLS","Y11_HH2a","Male","Y11_Q13a","Yes");
#?plot=heatMap&dataSource=3RDEQLS&subset=Y11_HH2a&subsetValue=Male&question=Y11_Q50a&answer=No problems

# ======================================================================


maskMap <- function(
	media = "png",
	width = standard.width,
	height = standard.height.map,
	savePath = plots.location
){

	if(width != standard.width){
		height = ( width * standard.height.map ) / standard.width
	}

	visualization.file.body.name <- paste("maskMap-", width, sep = "");

	file.visualization <- paste(savePath, visualization.file.body.name, ifelse(shouldOptimizePNG, "_", ""), ".png", sep = "")
	Cairo(file = file.visualization, type = "png", width = width, height= height, bg = "white");

	#png(filename = paste(savePath, visualization.file.body.name, ".png", sep = ""), width, height, units = "px", pointsize = 12, antialias = "subpixel")

	#-----------------------------------------------
	# Visualization
	#-----------------------------------------------

	dataDist <- data.frame(value=1:10)

	map <- ggplot(eu.mask.map, aes(V1, V2, group = country ))
	map <- map + coord_map(project = "globular", xlim = c(-9, 44), ylim = c(35, 70))
	map <- map + geom_polygon(aes(fill = country))
	#map <- map + opts(title = "European countries")
	map <- map + theme_ef_mask_map()

	grid.arrange(
#		getTitlesPlot(text=
#			c("","",""), width
#		),
		map,
		getValuesDistributionPlot(dataDist),
		getGradientPlot(0, 50, 100),
#		getBottomPlot(),
		ncol = 1,
		heights = c(0.115 + 0.737 + 0.045, 0.09 , 0.04 )
	)

	#print(map)
	dev.off()

	if(shouldOptimizePNG && media == "png")
		optimizePNG(file.visualization);
}

theme_ef_gradient  <- function (base_size = 12, base_family = base_font){
	theme_clean(base_size = base_size, base_family = base_family) %+replace%
	theme(
		legend.position = "none"
	)
}

getValuesDistributionPlot <- function(data){
	return(
		ggplot(data, aes(x=value, fill = value)) +
			geom_density() +
			theme_ef_gradient() #+	geom_vline(data=cdf, aes(xintercept=rating.mean,  colour=cond), linetype="dashed", size=1)
	)
}

getGradientPlot <- function(dataMin = 0, dataMid = 50, dataMax = 100, question.is.mean = FALSE, lowColor = pallet.intensity[1], midColor = pallet.intensity[6], highColor = pallet.intensity[12] ){

	dataRange <- dataMax - dataMin
	colorRange <- data.frame(x = c((dataMin + (dataRange / 7) * 0:7)), y = c(0:7))
	colorRange$xLabel <- round(colorRange$x, 1)

	if(!question.is.mean)
		colorRange$xLabel <- paste(colorRange$xLabel, "%", sep = "")
	colorRange$y <- 1

	return(
		qplot(x, y, data = colorRange, fill = x, geom = "raster", label = xLabel) +
		theme_ef_gradient() +
		scale_fill_gradient2( low = lowColor, mid = midColor, high = highColor, midpoint = dataMid) +
		geom_text( fontface = "bold", colour = pallet.intensity[4] ) +
		geom_text( colour = "white")
		)
}

