# ======================================================================
# Eworx S.A. - 2013
# Author kp@eworx.gr
# ======================================================================
# defines the CWB time series related visualization functions
# ======================================================================

cwbTimeSeriesStatDataFolder <- paste(dvs.folder, "import/cwb/time-series/", sep = "")
commonFields <- c("SeriesID", "Year")

standard.height.timeSeries = standard.height * 0.82;

cwbTimeSeries <- function(
	queryString,
	media = "png",
	width = standard.width,
	height = standard.height.timeSeries,
	savePath = plots.location
){

# The visualization illustrates in a map the percentages of answers, or the mean answer value of a question per European Country.

	if(width != standard.width){
		height = ( width * standard.height.timeSeries ) / standard.width
	}

	#-------------------------------------------------

	visualization.file.body.name <- paste("cwbTimeSeries-", queryString, sep = "" )
	stat.data.source <- paste(cwbTimeSeriesStatDataFolder, queryString, ".csv", sep = "")
	stat.data.options.source <- paste(cwbTimeSeriesStatDataFolder, queryString, "_options.csv", sep = "")

	#-----------------------------------------------
	# Read data
	#-----------------------------------------------

	if(!file.exists(stat.data.source))
		return("File does not exist");

	stat.data <- readXls(stat.data.source)



	#-----------------------------------------------
	# Read options
	#-----------------------------------------------

	vOptions <- readXls(stat.data.options.source)
	axis_title_y <- as.character(vOptions[vOptions$property == "axis.title.y", "value"])

	#-----------------------------------------------
	# Data manipulation
	#-----------------------------------------------

	stat.data <- covertTableVariablesIntoRows(stat.data)
	stat.data <- stat.data[complete.cases(stat.data),]

	if(nrow(stat.data)==0)
		return("File contains no data for the provided variable(s)");

	if(nrow(stat.data)<3)
		return("Less than 3 observations. Please select different Series ID");

	stat.data <- stat.data[with(stat.data, order(Variable, Year)), ]

print(stat.data)

	#-----------------------------------------------

	#-----------------------------------------------
	# Media devices
	#-----------------------------------------------

	file.visualization <- "";

	if(media == "png"){
		file.visualization <- paste(savePath, visualization.file.body.name, "-", width, ifelse(shouldOptimizePNG, "_", ""), ".png", sep = "")
		Cairo(file = file.visualization, type = "png", width = width, height = height, bg = timeSeriesBackground);
		#png(filename = paste(savePath, visualization.file.body.name, "-", width, ".png", sep = ""), width, height, units = "px", pointsize = 12, antialias = "subpixel" )
	}else
	if(media == "pdf"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width, ".", media, sep = "")
		cairo_pdf(filename = visualization.file.body.name , width =  12.77 * (width/standard.width), height = 12.77 *(height/standard.height) * 0.82, pointsize = 12, onefile = TRUE, bg = timeSeriesBackground, antialias = c("default", "none", "gray", "subpixel"))
	}else
	if(media == "eps"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width, ".", media, sep = "")
		postscript(visualization.file.body.name, paper="special", width =  12.77 * (width/standard.width), height = 12.77 *(height/standard.height)* 0.82, horizontal = FALSE)
	}else
	if(media == "svg"){
		visualization.file.body.name <- paste(savePath, "/", media, "/", visualization.file.body.name, "-", width, sep = "")
		file.visualization <- paste(visualization.file.body.name, ".svg", sep = "")
		if(file.exists(file.visualization) || file.exists(paste(visualization.file.body.name, ".svg.gz", sep = "")) )
			return("");
		Cairo(file = visualization.file.body.name, type = "svg", width =  12.77 * (width/standard.width), height = 12.77 * (height/standard.height) , bg = timeSeriesBackground, pointsize = 12, units = "in");
	}else
	if(media == "xls"){
		return("");
	}

	#-----------------------------------------------
	# Visualization
	#-----------------------------------------------

	variablesCount <- length(unique(stat.data$Variable))

	yearRangeJitter <- (max(stat.data$Year) - min(stat.data$Year)) * 0.03
	stat.data[, "yearRangeJitter"] <- yearRangeJitter
	#valueRange <- max(stat.data$Value) - min(stat.data$Value)

	#valueRangeJitter <- valueRange * 0.04
	stat.data[, "valueRangeJitter"] <- 0

	# auto compute labels positions
#	for(rowIndex in 2:nrow(stat.data)){
#		distance <- stat.data$Value[rowIndex] - stat.data$Value[rowIndex-1]
#		threshold =  valueRange * 0.1

#		if(distance  < threshold ){
#			stat.data$yearRangeJitter[rowIndex] = stat.data$yearRangeJitter[rowIndex] * -0.44;
#			stat.data$valueRangeJitter[rowIndex] = valueRangeJitter;
#		}
#	}

	#-----------------------------------------------

	 map = ggplot(stat.data, aes(x = Year, y = Value, group = Variable, color = Variable)) +
	 
	 geom_line(size = 0.8) + #, arrow = arrow(ends = "both")) +

     geom_smooth(size = 0, show_guide = FALSE, aes(fill = factor(Variable)), alpha = 0.2, method="loess")+
	 scale_fill_manual(values = pallet.clusters) +

	 scale_shape(solid = TRUE) +	 
	 scale_colour_manual(values = pallet.clusters) +
	 geom_point(aes(shape = Variable), size = 6) + scale_shape_manual(values=c(15,16,17,18)) +

	 geom_point(colour = "white", size = 1.5) +
	 ylab(axis_title_y) +

#geom_text( aes( x = Year - yearRangeJitter, y = Value + valueRangeJitter, fontface = "bold", angle = 0, label = Value, colour = Variable, vjust = 0.45, hjust = 1, position = "jitter"), size = 4) +

	 theme_ef_time_series()

	if(variablesCount  == 1){
		map = map + geom_segment(data = stat.data, aes(group = Variable, xend=c(tail(Year, n=-1), NA), yend=c(tail(Value, n=-1), NA)), arrow=arrow(length=unit(0.8,"cm")))
	}

	#superImposeLogo()
	print(map)

	dev.off()


	if(shouldOptimizePNG && media == "png")
		optimizePNG(file.visualization);

	if(media == "svg")
		optimizeSVG(file.visualization, width, height)

}

