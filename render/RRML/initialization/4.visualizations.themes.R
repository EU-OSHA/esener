# ======================================================================
# THEMES | pallets | Common styles | Visualizations functions dependencies
# ======================================================================

pallet.clusters <- c(

	
	"#F6A400", "#C7E2E3", "#449FA2", "#FBDB99", "#003399", "#58585A", "#B1B3B4", "#00982E", # original solution 8 colors
	"#948A71", "#566965", "#CE6A51", "#AB7A59", "#705455", "#9DBC9D", "#659D90", # extended solution 2 - up to 15 colors

	"#AB9A84","#D9AA8D", "#4F7D80", "#B6D685", "#91CFA8", "#F6AA83", "#87B799", # extended solution 1 - up to 15 colors - too / yellow orange
	"#F6A400", "#C7E2E3", "#449FA2", "#FBDB99", "#003399", "#58585A", "#B1B3B4", "#00982E", # repeat first 8
	"#BFB789", "#A0B57A", "#B68FBB", "#62AFB9", "#84A4AF", "#7CB777", # some more
	"#F6A400", "#C7E2E3", "#449FA2", "#FBDB99", "#003399", "#58585A", "#B1B3B4", "#00982E"  # repeat first 8

) # removed 5th color "#003399" grey value - reserved for cross country compare to country

timeSeriesBackground <- "white" #"#f9f9f9"
pallet.clusters.four <- c("#F6A400", "#C7E2E3", "#449FA2", "#FBDB99" )

pallet.clusters.groups <- c("#c81616", "#0c0f6d", "#1a9c15")
						 #c("#c81616", "#161ac8", "#13c40a")
						 #c("#B1B3B4", "#58585A", "#F6A400")

pallet.clusters.groups.colors <- c("#c81616", "#0c0f6d", "#1a9c15")
pallet.clusters.groups.fills  <- c("#b73930", "#5866a5", "#5e9a42")

pallet.clusters.groups.light <- c("#b73930", "#1553ba", "#5e9a42") #c("#c81616", "#1553ba", "#1a9c15")


pallet.map <- c("#1B4389", "#E2E7F0")

pallet.map.grid <- "#F2F2F2"

pallet.intensity <- c(
	"#E2E7F0","#D0D8E7","#BEC9DD","#ACBAD4", "#9AABCB",  "#889CC1", "#758EB8", "#637FAE","#5170A5","#3F619C","#2D5292","#1a305b" #"#122443" # Substituted last color with a darker one providing higher dynamic range."#1B4389"
) # pallet.map # http://meyerweb.com/eric/tools/color-blend/

pallet.intensity.high.point <- "#1B4389"

standard.width <- 920
standard.height <- 920 #815#690
standard.height.map <- 950

# -------------------

standard.copyright <- "© OSHA - All rights reserved"
logoRGML <- readPicture(paste(scripts.location, "logo.ps.xml", sep = "")) #install.packages("grImport");PostScriptTrace("logo.ps")
#circleRGML <- readPicture(paste(scripts.location, "circle.ps.xml", sep = ""))

base_font = "Open Sans";

optimizePNG <- function(filename){

  #filename : expects an underscore (xxx_.png) to be removed in the output file

  # pngquant] had the lowest installation footprint, accurate colour representation, result size, and speed.
  # 1st attempt, GD, php. very low output size, but innacurate colours, we resulted that it uses a fixed indexed pallet.
  # 2nd attempt, image magick, good results, maximoum flexibility, relatively slow, large installation footprint
  # 3rd attempt, optipng, relatively large files.
  # 4th attempt, had the lowest installation footprint, accurate colour representation, result size, and speed.

  #system(paste("cd ", project.location, ";", php.command, " -f optimizePNG.php ", filename, sep = ""))
  #system(paste("cd ", project.location, ";", "convert ", filename, " -colors 256 ", gsub("_.png", ".png", filename), sep = ""))
  #winner http://optipng.sourceforge.net/optipng-0.7.4.man.pdf
  system(paste("cd ", project.location, ";", "pngquant 256 ", filename, ";mv ", gsub("_.png", "_-fs8.png", filename), " ",  gsub("_.png", ".png", filename), ";rm ", filename, sep = ""))

}

optimizeSVG <- function(filename, width, height){
	# should open up the filename and append the width and height values in some's element attributes.
	# the width and height are in "pt" and the svg has by default values in "in".
	#width = width * 1.02
	if(!file.exists(filename))
		return("")

	svgText = readLines(filename, -1, encoding = "UTF-8")
	#svgText[2] <- paste("<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' width='", width ,"px' height='", height,"px' viewBox='0 0 ", width," ", height,"' version='1.1'>", sep="")
	svgText[2] <- gsub("pt", "px", svgText[2])
	svgText[2] <- gsub("<svg", "<svg preserveAspectRatio=\"xMinYMin meet\"", svgText[2])

	if(regexpr("euMatrix", filename) != -1){
		crosshair <- " <line id=\"horizontalHair\" x1=\"0\" y1=\"0\" x2=\"0\" y2=\"0\" ></line><line id=\"verticalHair\" x1=\"0\" y1=\"0\" x2=\"0\" y2=\"0\" ></line>"
		svgText[length(svgText)] = paste(crosshair,svgText[length(svgText)])
	}

	

	writeLines(svgText,filename)

	if(TRUE){ # USE PYTHON SVG OPTIMIZER
		exec.command <- paste("cd ", RRML.location.svg.o, ";", "python  SVGOptimize.py ", filename, sep = "")
		system(exec.command)
	} 
	#if(TRUE){ # Patch for outdated Cairo libraries
	#	svgText<- gsub("<symbol", "<symbol overflow=\"visible\"", svgText)
	#}


	system(paste("cd ", project.location, ";", "gzip ", filename, sep = ""))

}

# important without this R cannot determine system fonts
CairoFonts(
	regular = "Open Sans:style=Regular",
	bold = "Open Sans:style=Bold",
	italic = "Open Sans:style=Oblique",
	bolditalic = "Open Sans:style=BoldOblique"
)

theme_ef <- function (base_size = 12, base_family = base_font){
	theme(

		plot.background = element_rect(colour = "white"),
		plot.title = element_text(size = rel(1.5), hjust = 0, vjust = 0),
		#plot.title = element_text(family = base_family, face = "bold", colour = "black", size = 18, hjust = 0, vjust = 1, angle = 0, lineheight = 0.9),
		#plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
		plot.margin = unit(c(0, 0, 0, 0), "lines"),

		panel.background = element_rect(fill = "white", colour = NA),
		#panel.background = element_rect(fill = "grey90", colour = NA),
		panel.border = element_blank(),
		#panel.border = element_rect(fill = NA, colour = "grey50"),
		panel.grid.major = element_line(colour = "white"),
		#panel.grid.major = element_line(colour = "grey90", size = 0.2),
		panel.grid.minor = element_line(colour = "grey95", size = 0.25),
		#panel.grid.minor = element_line(colour = "grey98", size = 0.5),
		panel.margin = unit(0.25, "lines"),
		text = element_text(family = base_family, face = "plain", colour = "black", size = base_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
		line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
		rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),

		axis.text = element_text(size = rel(1), colour = "grey50"),
		axis.line = element_blank(),
		axis.text.x = element_text(vjust = 1),
		axis.text.y = element_text(hjust = 1),

		axis.title.x = element_text(),
		axis.title.y = element_text(angle = 90),

		axis.ticks = element_line(colour = "grey50"),
		#axis.ticks = element_line(colour = "black"),
		axis.ticks.length = unit(0.15, "cm"),
		axis.ticks.margin = unit(0.1, "cm"),

		legend.key = element_rect(colour = "white"),
		#legend.key = element_rect(fill = "grey95", colour = "white"),
		legend.background = element_rect(colour = NA),

		legend.margin = unit(0.2, "cm"),
		legend.key.size = unit(1.2, "lines"),
		legend.key.height = NULL,
		legend.key.width = NULL,

		legend.text = element_text(size = rel(0.95)),
		legend.text.align = NULL,
		legend.title = element_text(size = rel(1), face = "bold", hjust = 0),
		legend.title.align = NULL,

		legend.position = "right",
		legend.box = NULL,

		strip.text = element_text(size = rel(1)),
		strip.background = element_rect(fill = "grey80", colour = "grey50"),
		#strip.background = element_rect(fill = "grey80", colour = NA),
		strip.text.x = element_text(),
		strip.text.y = element_text(angle = -90),

		complete = TRUE
	)
}

theme_ef_map  <- function (base_size = 12, base_family = base_font){
	theme_ef(base_size = base_size, base_family = base_family) %+replace%
	theme(
		panel.grid.major = element_line(colour = pallet.map.grid, size = 0.4),

		axis.text = element_blank(),
		axis.title.x = element_blank(),
		axis.title.y = element_blank(),
		axis.ticks = element_line(colour = "white"),
		axis.ticks.length = unit(0, "cm"),

#		legend.margin = unit(0.2, "cm"),
#		legend.position = c(0.95, 1),
#		legend.justification = c(1, 1),
#		legend.key.size = unit(2, "cm"),
#		legend.key.width = unit(0.8, "cm"),
#		#legend.title = element_text(size = rel(1.2), face = "bold", hjust = 0),
#		legend.title = element_blank()

		legend.position = "none"
	)
}

theme_ef_cwb_map  <- function (base_size = 12, base_family = base_font){
	theme_ef(base_size = base_size, base_family = base_family) %+replace%
	theme(
		panel.grid.major = element_line(colour = pallet.map.grid, size = 0.4),

		axis.text = element_blank(),
		axis.title.x = element_blank(),
		axis.title.y = element_blank(),
		axis.ticks = element_line(colour = "white"),
		axis.ticks.length = unit(0, "cm"),

		#legend.margin = unit(0.2, "cm"),
		#legend.position = c(0.95, 1),
		#legend.justification = c(1, 1),
		#legend.key.size = unit(2, "cm"),
		#legend.key.width = unit(0.8, "cm"),
		
		legend.position = "none"
		#legend.direction = "vertical",
		#legend.title = element_blank()
		
#		#legend.title = element_text(size = rel(1.2), face = "bold", hjust = 0),
		

		
	)
}


theme_ef_cwb_outcomes_map  <- function (base_size = 12, base_family = base_font){
	theme_ef(base_size = base_size, base_family = base_family) %+replace%
	theme(
		panel.grid.major = element_line(colour = pallet.map.grid, size = 0.4),

		axis.text = element_blank(),
		axis.title.x = element_blank(),
		axis.title.y = element_blank(),
		axis.ticks = element_line(colour = "white"),
		axis.ticks.length = unit(0, "cm"),
		legend.position = "none"
	)
}

theme_ef_mask_map  <- function (base_size = 12, base_family = base_font){
	theme_ef_map(base_size = base_size, base_family = base_family) %+replace%
	theme(
		legend.position = "none"
	)
}

theme_ef_stacked  <- function (base_size = 12, base_family = base_font){
	theme_ef(base_size = base_size, base_family = base_family) %+replace%
	theme(
		strip.background = element_blank(),
		strip.text.x = element_blank(),
		strip.text.y = element_blank(),
		axis.text.x = element_text(angle = -90, hjust = 0),
		axis.title.x = element_blank(),
		axis.title.y = element_blank(),
		legend.title = element_blank(),

		legend.margin = unit(0.2, "cm"),
		legend.position = "bottom",
		#legend.direction = "horizontal"
		legend.direction = "vertical"
	)
}



theme_ef_survey_time_series <- function (base_size = 12, base_family = base_font){
	theme_ef(base_size = base_size, base_family = base_family) %+replace%
	theme(

		plot.margin = unit(c(0, 1, 0, 0), "lines"),
		
		plot.background = element_rect(fill = timeSeriesBackground, colour = timeSeriesBackground),
		panel.background = element_rect(fill = timeSeriesBackground, colour = timeSeriesBackground),
		legend.background = element_rect(fill = timeSeriesBackground, colour = timeSeriesBackground),
		strip.background = element_rect(fill = timeSeriesBackground, colour = timeSeriesBackground),
		legend.key = element_rect(fill = timeSeriesBackground, colour = timeSeriesBackground),
 
		

		axis.title.x = element_text(face = "bold", angle = 0, hjust = 0.95),
		axis.title.y = element_text(face = "bold", angle = 90, hjust = 0.95, vjust = 0.5),

		axis.text.x = element_text(angle = 0, hjust = 0.5),
 
		panel.grid.major = element_line(linetype = 5, colour = "#7e7e7e", size = 0.2),
		panel.grid.minor = element_line(linetype = 2, colour = "#cccccc", size = 0.2),
 

		legend.key.size = unit(2, "lines"),
		axis.ticks.length = unit(0.3, "cm")
	)
}


theme_ef_time_series <- function (base_size = 12, base_family = base_font){
	theme_ef(base_size = base_size, base_family = base_family) %+replace%
	theme(

		plot.margin = unit(c(0, 1, 0, 0), "lines"),
		plot.background = element_rect(fill = timeSeriesBackground, colour = timeSeriesBackground),
		panel.background = element_rect(fill = timeSeriesBackground, colour = timeSeriesBackground),
		legend.background = element_rect(fill = timeSeriesBackground, colour = timeSeriesBackground),
		strip.background = element_rect(fill = timeSeriesBackground, colour = timeSeriesBackground),
		legend.key = element_rect(fill = timeSeriesBackground, colour = timeSeriesBackground),

		#strip.background = element_blank(),
		
		axis.title.x = element_text(face = "bold", angle = 0, hjust = 0.95),
		axis.title.y = element_text(face = "bold", angle = 90, hjust = 0.95, vjust = 0.5),
		
		#strip.text.x = element_blank(),
		#strip.text.y = element_blank(),
		axis.text.x = element_text(angle = 0, hjust = 0.5),
		#axis.line = element_line(colour = "#7f7f7f", size = 0.3),
		panel.grid.major = element_line(linetype = 5, colour = "#7e7e7e", size = 0.2),
		panel.grid.minor = element_line(linetype = 2, colour = "#b3b3b3", size = 0.2),

		#legend.title = element_blank(),

		legend.key.size = unit(4, "lines"),
		axis.ticks.length = unit(0.3, "cm"),
		#legend.margin = unit(1, "cm"),
		legend.position = "bottom",
		#legend.justification = c(-0.5,0), does not seem to work
		legend.direction = "horizontal"
	)
}

percentageLabels <-  function (x, ...){
	return(paste(x, "%"))
}

theme_ef_scatter <- function (base_size = 12, base_family = base_font){
	theme_ef(base_size = base_size, base_family = base_family) %+replace%
	theme(
		strip.background = element_blank(),
		strip.text.x = element_blank(),
		strip.text.y = element_blank(),
		axis.title.x = element_blank(),
		axis.title.y = element_blank(),
		axis.text.y = element_blank(), ######
		legend.title = element_blank(),
		#legend.margin = unit(0.2, "cm"),
		legend.position = "none"#,
		#legend.direction = "horizontal"

	)
}

#--------------------------------------

theme_ef_incountry  <- function (base_size = 12, base_family = base_font){

	theme_ef(base_size = base_size, base_family = base_family) %+replace%
	theme(
		legend.position = "none",
		strip.text.x = element_text(size = 14, face = "bold", hjust = 0, colour = "grey30") ,
		strip.text.y = element_text(angle = 0 ),
		strip.background = element_rect(colour = "white", fill = "white"),
		axis.title.x = element_blank(),
		axis.title.y = element_blank(),
		axis.ticks.x = element_line(colour = "grey50"),
		axis.ticks.y = element_line(colour = "white")
		#axis.ticks.length = unit(1, "cm"),
		#axis.ticks.margin = unit(0.1, "cm")
	)
}

theme_ef_incountry_no_axis  <- function (base_size = 12, base_family = base_font){

	theme_ef_incountry(base_size = base_size, base_family = base_family) %+replace%
	theme(
		axis.ticks.x = element_line(colour = "white"),
		axis.text.x = element_text(size = 14, face = "bold", hjust = 0, colour = "white")
	)
}

#--------------------------------------

theme_ef_grid  <- function (base_size = 12, base_family = base_font){
	theme_ef_incountry(base_size = base_size, base_family = base_family) %+replace%
	theme(
		strip.text.y = element_text(angle = 90 ),
		axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0 ),
		axis.ticks.length = unit(0.1, "cm"),
		axis.ticks.margin = unit(0.1, "cm")
	)
}


theme_clean <- function (base_size = 12, base_family = base_font){
	theme(
		plot.background = element_rect(colour = "white"),
		plot.title = element_blank(),
		plot.margin = unit(c(0, 0, 0, 0), "lines"),
		panel.background = element_rect(fill = "white", colour = NA),
		panel.border = element_blank(),
		panel.grid.major = element_line(colour = "white"),
		panel.grid.minor = element_line(colour = "white"),
		panel.margin = unit(0.25, "lines"),
		text = element_text(family = base_family, face = "plain", colour = "black", size = base_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
		line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
		rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
		axis.text = element_blank(),
		axis.line = element_blank(),
		axis.text.x = element_blank(),
		axis.text.y = element_blank(),
		axis.title.x =element_blank(),
		axis.title.y = element_blank(),
		axis.ticks = element_line(colour = "white"),
		axis.ticks.length = unit(0.01, "cm"),
		axis.ticks.margin = unit(0.01, "cm"),
		legend.key = element_rect(colour = "white"),
		legend.background = element_rect(colour = NA),
		legend.margin = unit(0.02, "cm"),
		legend.key.size = unit(1.2, "lines"),
		legend.key.height = NULL,
		legend.key.width = NULL,
		legend.text = element_text(size = rel(1)),
		legend.text.align = NULL,
		legend.title = element_text(size = rel(1), face = "bold", hjust = 0),
		legend.title.align = NULL,
		legend.position = "right",
		legend.direction = NULL,
		legend.justification = "center",
		legend.box = NULL,
		strip.text = element_blank(),
		strip.background = element_rect(fill = "white", colour = "white"),
		strip.text.x = element_blank(),
		strip.text.y = element_blank(),
		complete = TRUE
	)
}

getTitlesPlotDataFrame <- function(text = c("1","2","3"), width = 500){
	titlesData <- data.frame(
		y = c(1.7, 1.15, 0.5),
		x = c(0, 0, 0),
		size = c(0.6, ifelse((width > 500), 0.85, 0.7), 0.6),
		alpha = c(0.6, 1, 0.6),
		face = c("plain", "bold", "plain"),
		text = text
	)
	if(width == standard.width){
		titlesData$y[1] <- 1.6
	}

	if(nchar(text[2]) > 72){

		titlesData$size[2] <- titlesData$size[2] * 0.9

		if(nchar(text[2]) > 115)
			titlesData$size[2] <- titlesData$size[2] * 0.8

		if(width < 750){
			titlesData$face[2] <- "plain"
			titlesData$size[2] <- titlesData$size[2] * 0.9
		}
		if(width < 501)
			titlesData$size[2] <- titlesData$size[2] * 0.8
	}

	return(titlesData)
}

getTitlesPlot <- function(
		text = c(
			"European Quality of Life Surveys 2012.",
			"Chronix physical or mental health problem or disability ?",
			"Country : Austria, By: Gender, All Genders"
		), width = 920, yMin = -0.2
	){

	titlesData <- getTitlesPlotDataFrame(text, width)

	p <- ggplot(data = titlesData, mapping = aes(x = x, y = y, label = text)) +
		geom_text(hjust = 0, vjust = 1, alpha =  titlesData$alpha, size = (titlesData$size * 7), fontface = titlesData$face ) +
		coord_cartesian(xlim = c(0,5), ylim = c(yMin, 2)) +
		geom_abline(intercept = ifelse((width > 500), 0.65, 0.62), slope = 0, alpha = 0.3) +
		theme_clean()

	return(p)
}

getTitleSubPlot <- function(text = "Yes I agree very much", width = 920, yMin = -0.2){

	titlesData <- data.frame(
		y = c(0.6),
		x = c(0, 0),
		size = c(ifelse(width > 500, ifelse(width > 740, 1, 0.7), 0.3)),
		face = c("bold"),
		alpha = c(0.6),
		text = c(clearIndex(text))
	)

	p <- ggplot(data = titlesData, mapping = aes(x = x, y = y, label = text)) +
		geom_text(hjust = 0, vjust = 1, alpha =  titlesData$alpha, size = (titlesData$size * 5), fontface = titlesData$face ) +
		coord_cartesian(xlim = c(0,5), ylim = c(yMin, 1)) +
		theme_clean()

	return(p)
}

getTitlesPlotBasic <- function(
		text = c(
			"European Quality of Life Surveys 2012.",
			"Chronix physical or mental health problem or disability ?",
			"Country : Austria, By: Gender, All Genders"
		), width = 920, yMin = -0.2
	){

	titlesData <- getTitlesPlotDataFrame(text, width)

	plot(0, 0, xlim = c( 0, 5 ), ylim = c( -0.2, 2), type = "n", axes = F, xaxt = "n", yaxt = 'n', xaxs = 'i', yaxs = 'i', ann = F)
	lines(c(0,5), c(ifelse((width > 500), 0.65, 0.62)+.15, ifelse((width > 500), 0.65, 0.62)+.15), col = "gray80")
	text(
		x= titlesData$x, y = titlesData$y, labels = titlesData$text,
		font = ifelse(titlesData$face == "plain", 1, 2),
		adj = 0,
		cex = (titlesData$size * 1.7),
		col = ifelse(titlesData$alpha == 1, "black", "gray30")
	)
}

# getTitlesPlot()

getBottomPlot <- function(footText = standard.copyright){

	bottomData <- data.frame(
		y = c(0.75),
		x = c(0),
		size = c(1.2),
		face = c("plain"),
		text = c(footText)
	)

	p <- ggplot(data = bottomData, mapping = aes(x = x, y = y, label = text)) +
		geom_text(hjust = 0, vjust = 1, alpha =  0.9, size = (bottomData$size * 3), fontface = bottomData$face ) +
		coord_cartesian(xlim = c(0,5), ylim = c(0, 1)) +
		theme_clean()

	return(p)
}

getBottomPlotBasic <- function(footText = standard.copyright){

	bottomData <- data.frame(
		y = c(0.75),
		x = c(0),
		size = c(1.2),
		face = c("plain"),
		text = c(footText)
	)

	plot(0, 0, xlim = c( 0, 5 ), ylim = c( -0.2, 2), type = "n", axes = F, xaxt = "n", yaxt = 'n', xaxs = 'i', yaxs = 'i', ann = F)

	text(
		x= bottomData$x, y = bottomData$y, labels = bottomData$text,
		font = ifelse(bottomData$face == "plain", 1, 2),
		adj = 0,
		cex = (bottomData$size * 0.7),
		col = "gray10"
	)
}

superImposeLogo <- function(x = 0.79, y = 0.31, size = 0.1, RGML = logoRGML){
	pushViewport(viewport( x = x, y = y, width = size, just = c("left","bottom")))
	grid.picture(RGML)
}

# ---------------------------------------------------------
#############################DELETE##############3

addEFFootnote <- function(footnoteText = "Eurofound - All rights reserved", size= .7, color = grey(.5)){
	require(grid)
	pushViewport(viewport())
	grid.text(
		label = footnoteText ,
		#x = unit(1, "npc") - unit(2, "mm"),
		#y = unit(2, "mm"),
		x = unit(0, "npc"),
		y = unit(0, "npc"),
		just = c("left", "bottom"),
		gp = gpar(cex = size, col = color)
	)
	popViewport()
}
#############################DELETE##############3
