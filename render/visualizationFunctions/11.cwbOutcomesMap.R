# ======================================================================
# Eworx S.A. - 2013
# Author kp@eworx.gr
# ======================================================================
# defines the CWB time series related visualization functions
# ======================================================================


#----------------------------------------------------------------------

cwbOutcomesMapStatDataFolder <- paste(dvs.folder, "import/cwb/outcomes-map/", sep = "")
standard.height.cwbOutcomesMap <- standard.height.map - (standard.height.map * 0.115)

pallet.outcomes = c("#2a483c", "#749d8c",  "#c4d8cf") #c("darkred", "orange",  "darkgreen") #("#537E00", "orange",  "#B75B2E")
cwbOutcomesMapValueLimit = 4

#----------------------------------------------------------------------

cwbOutcomesMap <- function(
	queryString,
	media = "png",
	width = standard.width,
	entitle = FALSE,
	height = standard.height.cwbOutcomesMap,
	savePath = plots.location
){

# The visualization illustrates in a map the percentages of answers, or the mean answer value of a question per European Country.

	dynamicHeight = ifelse(entitle, standard.height.map, standard.height.map - (standard.height.map * 0.115))

	if(width != standard.width){
		height = ( width * dynamicHeight ) / standard.width
	}

	#-------------------------------------------------

	visualization.file.body.name <- paste("cwbOutcomesMap-", queryString, sep = "" )
	stat.data.source <- paste(cwbOutcomesMapStatDataFolder, queryString, ".csv", sep = "")
	#stat.data.options.source <- paste(cwbOutcomesMapStatDataFolder, queryString, "_options.csv", sep = "")

	#-----------------------------------------------
	# Read data
	#-----------------------------------------------

	if(!file.exists(stat.data.source))
		return("File does not exist");

	stat.data <- readXls(stat.data.source)

	
	names(stat.data)[3]<-"Value"

	if(!("Value" %in% names(stat.data))){
		return("Column Value does not exist")
	}


	#-----------------------------------------------
	# Read options
	#-----------------------------------------------

	#vOptions <- readXls(stat.data.options.source)
	#axis_title_y <- as.character(vOptions[vOptions$property == "axis.title.y", "value"])

	#-----------------------------------------------
	# Data manipulation
	#-----------------------------------------------

	stat.data <-stat.data[complete.cases(stat.data),]

	if(nrow(stat.data)<5){ # requested explicitely
		return("No infomation available")
	}

	stat.data <- stat.data[with(stat.data, order(Country)), ]

	#replace Country with CountryCode

		column <- "Country"

		for(row in 1:nrow(countries.code.name)){
			country.code <- as.character(countries.code.name$CountryCode[row])
			country.name <- as.character(countries.code.name$CountryName[row])
			#if the country exists in the data
			if(!(country.code %in% levels(stat.data[[column]]))){
				levels(stat.data[[column]]) <- c(levels(stat.data[[column]]), country.code)
			}
			stat.data[stat.data[[column]] == country.name, column] <- country.code
		}

	stat.data <-stat.data[complete.cases(stat.data),]

	if(nrow(stat.data)<5){
		return("No infomation available")
	}

	#-----------------------------------------------

	for(rowIndex in 1:nrow(stat.data)){
		value <- stat.data[rowIndex, "Value"]
		stat.data[rowIndex, "Value"] <- ifelse(value > cwbOutcomesMapValueLimit, cwbOutcomesMapValueLimit, value)
	}


	dataDist <- stat.data
	dataDist<-data.frame(value = dataDist$Value)

	#-----------------------------------------------
	# Media devices
	#-----------------------------------------------

	file.visualization <- "";

	if(media == "png"){
		file.visualization <- paste(savePath, visualization.file.body.name, "-", width, ifelse(shouldOptimizePNG, "_", ""), ".png", sep = "")
		Cairo(file = file.visualization, type = "png", width = width, height = height, bg = "white");
		#png(filename = paste(savePath, visualization.file.body.name, "-", width, ".png", sep = ""), width, height, units = "px", pointsize = 12, antialias = "subpixel" )
	}else
	if(media == "pdf"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width, ".", media, sep = "")
		cairo_pdf(filename = visualization.file.body.name , width =  12.77 * (width/standard.width), height = 12.77 * (height/standard.height.map), pointsize = 12, onefile = TRUE, bg = "white", antialias = c("default", "none", "gray", "subpixel"))
	}else
	if(media == "eps"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width, ".", media, sep = "")
		postscript(visualization.file.body.name, paper="special", width =  12.77 * (width/standard.width), height = 12.77 * (height/standard.height.map), horizontal = FALSE)
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

 	heat.map <- sqldf( paste(
	"
		select
			 V1, V2, eumap.country country, boundaryId, countryType, eumap.country Country, Value
		from
			'eu.map' eumap left outer join
			'stat.data' statdata
				on eumap.country = statdata.Country
			
	" #where eumap.country <> \"IS\"
	))

	#-----------------------------------------------

	valueLimits <- c(min(stat.data$Value), max(stat.data$Value))
	dataMean <- (valueLimits[1] + valueLimits[2]) / 2
	greenHigh <- FALSE

	map <- ggplot(data = heat.map, aes(x = V1, y = V2, group = country))
	map <- map + coord_map(project = "globular", xlim = c(-9, 44), ylim = c(35, 70))
	map <- map + geom_polygon(aes(fill = Value))

	map <- map + geom_path(aes(V1, V2), data = heat.map, colour = "white", size = 0.1)
	
 	uniqueCountries <- data.frame(CountryCode = unique(stat.data$Country))
	uniqueCountries.centers <- sqldf("select * from 'countries.centers' cc inner join uniqueCountries uc on cc.country = uc.CountryCode ")

	map <- map + geom_text(aes(x = V1, y = V2, label = country, fontface = "bold"), colour = "#8eb0a2", data = uniqueCountries.centers)


	map <- map + geom_text(aes(x = V1, y = V2, label = country), colour = "white", data = countries.centers)	
	map <- map + geom_text(aes(x = V1, y = V2, label = country), colour = "white", data = countries.centers.non.eu)

	MT<- subset(countries.centers, country == "MT")
	map <- map +  annotate("text", label = "MT", x = MT$V1, y = MT$V2, colour = "#a6a6a6")

	#map <- map + scale_fill_manual(values = dynamicPallet, 	na.value = pallet.map.grid) #"#cccccc")
	map <- map + scale_fill_gradient2(
		limits = valueLimits,
		midpoint = dataMean,
		low = pallet.outcomes[3],
		mid = pallet.outcomes[2],
		high = pallet.outcomes[1],
		na.value = pallet.map.grid
	)

	map <- map + theme_ef_cwb_outcomes_map()

	grid.arrange(
		map,
		getValuesDistributionPlot(dataDist),
		getCWBOutcomesGradientPlot(valueLimits[1], dataMean, valueLimits[2]),
		ncol = 1,		
		heights = c(0.737 + 0.115 + 0.045, 0.09, 0.04)
	)

	if(FALSE){
		pushViewport(viewport( x = 0.413, y = -0.31, width = 0.1, just = c("left", "bottom")))		
	}else{
		pushViewport(viewport( x = 0.413, y = -0.35, width = 0.1, just = c("left", "bottom")))		
	}
	
	grid.circle(r = 0.2, gp = gpar(fill = rgb(0.1, 0.1, 0.1, 0.1), alpha=0.1, lwd = 0))
	upViewport()
 
	#superImposeLegend()
	#upViewport(1)
	superImposeLogo(x = 0.79, y = 0.38) #y = 0.31)
	dev.off()

	if(shouldOptimizePNG && media == "png")
		optimizePNG(file.visualization);

	if(media == "svg")
		optimizeSVG(file.visualization, width, height)

}


getCWBOutcomesGradientPlot <- function(dataMin = 0, dataMid = 50, dataMax = 100, lowColor = pallet.outcomes[3], midColor = pallet.outcomes[2], highColor = pallet.outcomes[1], limit = cwbOutcomesMapValueLimit ){

	dataRange <- dataMax - dataMin
	colorRange <- data.frame(x = c((dataMin + (dataRange / 6) * 0:6)), y = c(0:6))
	
	colorRange$xLabel <- round(colorRange$x, 1)
	colorRange$xLabel <- paste(colorRange$xLabel, "%", sep = "")
	
	maxLimit <- paste(">",colorRange[nrow(colorRange), "xLabel"])

	colorRange[nrow(colorRange), "xLabel"] <- maxLimit
	colorRange$xLabel <- as.factor(colorRange$xLabel)
	
	colorRange$y <- 1

	return(
		qplot(x, y, data = colorRange, fill = x, geom = "raster", label = xLabel) +
		theme_ef_gradient() +
		scale_fill_gradient2( low = lowColor, mid = midColor, high = highColor, midpoint = dataMid) + 
			geom_text( fontface = "bold", colour = "#8eb0a2" )+
			geom_text( colour = "white") 
		)
}
