# ======================================================================
# Eworx S.A. - 2013
# Author kp@eworx.gr
# ======================================================================
# defines the CWB time series related visualization functions
# ======================================================================


#----------------------------------------------------------------------

cwbContextMapStatDataFolder <- paste(dvs.folder, "import/cwb/context-map/", sep = "")

commonFields <- c("SeriesID", "Year")
pallet3X3 <- c(
				"#286DA9", "#73A0C7", "#A9C5DD",
				#"#ee8d00","#f5bb66","#faddb2", #orange
				 "#B75B2E", "#D09477", "#E2BDAB",  				
				 "#537E00", "#8FAB59", "#BACB99"
			 )

values.to.colors <- data.frame(
	Value = c(
		"Centralised highly coordinated",
		"Centralised medium level of coordination",
		"Centralised low level of coordination",
		"Decentralised highly coordinated",
		"Decentralised medium level of coordination", "Decentralised medium level of coordination ",
		"Decentralised low level of coordination",
		"Intermediate highly coordinated", "Intermediate highly coordinated ",
		"Intermediate medium level of coordination", "Intermediate medium level of coordination ",
		"Intermediate low level of coordination"
	),
	Color = c(
		pallet3X3[1],
		pallet3X3[2],
		pallet3X3[3],
		pallet3X3[4],
		pallet3X3[5], pallet3X3[5],
		pallet3X3[6],
		pallet3X3[7], pallet3X3[7],
		pallet3X3[8], pallet3X3[8],
		pallet3X3[9]
	)
)

cwbContextMap_legend <- readPicture(paste(scripts.location, "cwbContextMap_legend.ps.xml", sep = ""))

superImposeLegend <- function(x = 0, y = 0.34, size = "0.33", RGML = cwbContextMap_legend){
	pushViewport(viewport( x = x, y = y, width = size, just = c("left","bottom")))
	grid.picture(RGML)
}

standard.height.cwbContextMap <- standard.height.map - (standard.height.map * 0.115)

#----------------------------------------------------------------------

cwbContextMap <- function(
	queryString,
	media = "png",
	width = standard.width,
	height = standard.height.cwbContextMap,
	savePath = plots.location
){

# The visualization illustrates in a map the percentages of answers, or the mean answer value of a question per European Country.

	if(width != standard.width){
		height = ( width * standard.height.cwbContextMap ) / standard.width
	}

	#-------------------------------------------------

	visualization.file.body.name <- paste("cwbContextMap-", queryString, sep = "" )
	stat.data.source <- paste(cwbContextMapStatDataFolder, queryString, ".csv", sep = "")
	#stat.data.options.source <- paste(cwbContextMapStatDataFolder, queryString, "_options.csv", sep = "")

	#-----------------------------------------------
	# Read data
	#-----------------------------------------------

	if(!file.exists(stat.data.source))
		return("File does not exist");

	stat.data <- readXls(stat.data.source)

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

	#-----------------------------------------------

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
		cairo_pdf(filename = visualization.file.body.name ,width =  12.77 * (width/standard.width), height = 12.77 * (height/standard.height.map), pointsize = 12, onefile = TRUE, bg = "white", antialias = c("default", "none", "gray", "subpixel"))
	}else
	if(media == "eps"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width, ".", media, sep = "")
		postscript(visualization.file.body.name, paper="special", width =  12.77 * (width/standard.width), height = 12.77 * (height/standard.height.map), horizontal = FALSE)
	}else
	if(media == "svg"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width, sep = "")
		file.visualization <- paste(visualization.file.body.name, ".svg", sep = "")
		if(file.exists(file.visualization) || file.exists(paste(visualization.file.body.name, ".svg.gz", sep = "")) )
			return("svg exists");
		Cairo(file = visualization.file.body.name, type = "svg", width =  12.77 * (width/standard.width), height = (12.77 * (height/standard.height.map) ) * (1- (0.115 + 0.0115) ), bg = "white", pointsize = 12, units = "in");
	}else
	if(media == "xls"){
		return("");
	}

	#-----------------------------------------------
	# Visualization
	#-----------------------------------------------


  uniqueValues <- unique(stat.data$Value)
  uniqueValues <- uniqueValues[order(uniqueValues)]
  dynamicPallet <- c()

  for(uniqueValue in uniqueValues){
  	color.value <- as.character(values.to.colors[values.to.colors$Value == uniqueValue, "Color"])
  	print(paste(uniqueValue, " - ",color.value))
  	dynamicPallet <- c(dynamicPallet, color.value )
  }


 	heat.map <- sqldf( paste(
	"
		select
			 V1, V2, eumap.country country, boundaryId, countryType, eumap.country Country, Value
		from
			'eu.map' eumap left outer join
			'stat.data' statdata
				on eumap.country = statdata.Country
			where eumap.country <> \"IS\"
	"
	))

	#-----------------------------------------------

	dataDist <- stat.data
	dataDist<-data.frame(value = dataDist$Value)

	map <- ggplot(data = heat.map, aes(x = V1, y = V2, group = country))
	map <- map + coord_map(project = "globular", xlim = c(-9, 44), ylim = c(35, 70))
	map <- map + geom_polygon(aes(fill = Value))

	map <- map + geom_path(aes(V1, V2), data = heat.map, colour = "white", size = 0.1)



	uniqueCountries.centers <- sqldf("select * from 'stat.data' where Value like 'Centralised%' ")
 	uniqueCountries <- data.frame(CountryCode = unique(uniqueCountries.centers$Country))
	uniqueCountries.centers <- sqldf("select * from 'countries.centers' cc inner join uniqueCountries uc on cc.country = uc.CountryCode ")
	map <- map + geom_text(aes(x = V1, y = V2, label = country, fontface = "bold"), colour = "#73A0C7", data = uniqueCountries.centers)

	uniqueCountries.centers <- sqldf("select * from 'stat.data' where Value like 'Decentralised%' ")
 	uniqueCountries <- data.frame(CountryCode = unique(uniqueCountries.centers$Country))
	uniqueCountries.centers <- sqldf("select * from 'countries.centers' cc inner join uniqueCountries uc on cc.country = uc.CountryCode ")
	map <- map + geom_text(aes(x = V1, y = V2, label = country, fontface = "bold"), colour = "#D09477", data = uniqueCountries.centers)		

	uniqueCountries.centers <- sqldf("select * from 'stat.data' where Value like 'Intermediate%' ")
 	uniqueCountries <- data.frame(CountryCode = unique(uniqueCountries.centers$Country))
	uniqueCountries.centers <- sqldf("select * from 'countries.centers' cc inner join uniqueCountries uc on cc.country = uc.CountryCode ")
	map <- map + geom_text(aes(x = V1, y = V2, label = country, fontface = "bold"), colour = "#8FAB59", data = uniqueCountries.centers)

	map <- map + geom_text(aes(x = V1, y = V2, label = country), colour = "white", data = countries.centers)
	map <- map + geom_text(aes(x = V1, y = V2, label = country), colour = "white", data = countries.centers.non.eu)

	MT<- subset(countries.centers, country=="MT")
	map <- map +  annotate("text", label = "MT", x = MT$V1, y = MT$V2, colour = "#a6a6a6")

	map <- map + scale_fill_manual(values = dynamicPallet, 	na.value = pallet.map.grid) #"#cccccc")

	map <- map + theme_ef_cwb_map()
 
 	grid.arrange(map)
 
 	if(FALSE){
		pushViewport(viewport( x = 0.413, y = -0.31, width = 0.1, just = c("left", "bottom")))		
	}else{
		pushViewport(viewport( x = 0.413, y = -0.472, width = 0.1, just = c("left", "bottom")))		
	} 
	
	grid.circle(r = 0.2, gp = gpar(fill = rgb(0.1, 0.1, 0.1, 0.1), alpha=0.1, lwd = 0))
	upViewport(1)

	superImposeLegend()
	upViewport(1)
	superImposeLogo()
	dev.off()

	if(shouldOptimizePNG && media == "png")
		optimizePNG(file.visualization);

	if(media == "svg")
		optimizeSVG(file.visualization, width, height)


}

getCWBContextValuesDistributionPlot <- function(data){
	print(data)
	return(
		ggplot(data, aes(x = Year, fill = factor(Value))) +
			geom_density() +
			theme_ef_gradient() #+	geom_vline(data=cdf, aes(xintercept=rating.mean,  colour=cond), linetype="dashed", size=1)
	)

}

getCWBContextGradientPlot <- function(dataValues, dynamicPallet ){
 
print("---------	")
	print(dataValues)
	question.is.mean = FALSE
	colorRange <- data.frame(x = dataValues, y = dataValues )
	colorRange$y <- 1
	return(
			qplot(x, y, data = colorRange, fill = x, geom = "raster") +
			theme_ef_gradient() +
			scale_fill_manual(values = dynamicPallet, na.value = pallet.map.grid) 
		)
}
