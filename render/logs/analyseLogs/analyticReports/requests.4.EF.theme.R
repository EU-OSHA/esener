# ======================================================================
# Eworx S.A. - 2012/2013
# Author kp@eworx.gr
# ======================================================================
# THEMES | pallets | Common styles | Visualizations functions dependencies
# ======================================================================


options(gsubfn.engine = "R")
library("Cairo")
library("sqldf")
library("grid")
library(gridExtra)
library("ggplot2")



pallet.clusters <- c("#A7BD43", "#7FADBA", "#DCAB5D", "#E5DCBB", "#C5C8BF", "#92A8B4", "#CF9173", "#E4D6A3") # removed 5th color "#C5C8BF" grey value - reserved for cross country compare to country
pallet.clusters.four <- c("#A7BD43", "#92A8B4", "#DCAB5D", "#CF9173" )
pallet.map <- c("#1B4389", "#E2E7F0")
pallet.map.grid <- "#F2F2F2"
pallet.intensity <- c("#E2E7F0","#D0D8E7","#BEC9DD","#ACBAD4", "#9AABCB",  "#889CC1", "#758EB8", "#637FAE","#5170A5","#3F619C","#2D5292","#1B4389"  ) # pallet.map # http://meyerweb.com/eric/tools/color-blend/

standard.width <- 920
standard.height <- 920 #815#690
standard.copyright <- "© EUROFOUND - All rights reserved"

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

		legend.margin = unit(0.2, "cm"),
		legend.position = c(1, 1),
		legend.justification = c(1, 1),
		legend.title = element_text(size = rel(1.2), face = "bold", hjust = 0)
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
		axis.text.x = element_text(angle = -35,	hjust = 0),
		axis.title.x = element_blank(),
		axis.title.y = element_blank(),
		legend.title = element_blank(),

		legend.margin = unit(0.2, "cm"),
		legend.position = "bottom",
		#legend.direction = "horizontal"
		legend.direction = "vertical"
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
