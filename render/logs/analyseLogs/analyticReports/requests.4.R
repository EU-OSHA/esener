
# ======================================================================
# Eworx S.A. - 2013
# Author kp@eworx.gr
# ======================================================================
# Analyses logs and presents insight overview
# ======================================================================
# source("/web-pub/foundation/html/DVS/render/logs/analyseLogs/requests.4.R")

readXls <- function(file){
  return(read.csv(file = file, head = TRUE, encoding = "UTF-8", sep = "\t"))
}



#-----------------------
width = 740
height = 740
base.folder <- "/web-pub/foundation/html/DVS/render/logs/"
#-----------------------

#IP"	"currentTimeMil"	"url"	"served"	"time"
requests <- readXls(paste(base.folder, "analyseRequests", sep=""))


#IP"	"currentTimeMil"	"url"	"served"	"time"

 
#requests <- readXls("http://eurofound.europa.eu/DVS/render/logs/requests")
nrow(requests)
requests[,"Count"]<-1:nrow(requests)

#-----------------------

#-----------------------
file.png <- "A1.png"
Cairo(file = paste(base.folder,file.png,sep=""), type = "png", width = width, height = height, bg = "white");
qplot(served, data=requests, geom="histogram") +  theme_ef() + opts(title = "Requests served through caching or through rendering") + xlab("") + ylab("Requests count")  
dev.off()
#--------
sqldf("select served, count(*) Count from requests group by served order by Count desc")

############################
#    served Count
#1   CACHED 29334
#2 RENDERED  7195
############################
#-----------------------


#1
file.png <- "B1.png"
Cairo(file = paste(base.folder,file.png,sep=""), type = "png", width = width, height = height, bg = "white");
qplot(Count, time, data = requests) + theme_ef() +  opts(title = "Detecting lags") + ylab("Response time (milliseconds)") + xlab("Request id") 
dev.off()
#-----------------------

#2, 2b
file.png <- "B2.png"
Cairo(file = paste(base.folder,file.png,sep=""), type = "png", width = width, height = height, bg = "white");
requests <- subset(requests,(time < 50000))
qplot(Count, time, data = requests) + theme_ef() +  opts(title = "Detecting lags - peaks removed (zoom in < 50000)") + ylab("Response time (milliseconds)") + xlab("Request id") 
dev.off()
#-----------------------

#3
file.png <- "B3.png"
Cairo(file = paste(base.folder,file.png,sep=""), type = "png", width = width, height = height, bg = "white");
requests <- subset(requests,(time < 5000))
qplot(Count, time, data = requests) + theme_ef() + opts(title = "Detecting lags - peaks removed (zoom in < 5000)") + ylab("Response time (milliseconds)") + xlab("Request id")  
dev.off()
#-----------------------

#file.png <- "B4.png"
#Cairo(file = paste(base.folder,file.png,sep=""), type = "png", width = width, height = height, bg = "white");
#4 0 response time indicates cached media
qplot(time, data=requests, geom="histogram") +  theme_ef() + opts(title = "Requests per response time") + xlab("Response time (milliseconds)") + ylab("Requests count") 
#dev.off()
#-----------------------

#5, (mirrored - to grayscale inserted into 3)
file.png <- "B4.png"
Cairo(file = paste(base.folder,file.png,sep=""), type = "png", width = width, height = height, bg = "white");
requests <- subset(requests,(served == "RENDERED"))
qplot(time, data=requests, geom="histogram")+  geom_histogram(aes(fill = ..count..)) +  theme_ef() + opts(title = "Rendered (new) requests per response time ") + xlab("Response time (milliseconds)") + ylab("Requests count") 
dev.off()

#-----------------------

# 7
file.png <- "C1.png"
Cairo(file = paste(base.folder,file.png,sep=""), type = "png", width = width, height = height, bg = "white");



 
#IP"	"currentTimeMil"	"url"	"served"	"time"
requests <- readXls(paste(base.folder, "analyseRequests", sep=""))

requests <- subset(requests,(time < 5000))
requests <- subset(requests,(served == "RENDERED"))
requests[,"plot"] <- getParameterInColumn("plot", requests$url)

plotMeans <- data.frame(
	plot = c("heatMap", "maskMap", "crossCountry", "euBars", "inCountry", "euMatrix" ),
	enplot = c("European map", "EU mask map", "National comparisons", "EU bar chart", " National bar chart", "Data Matrix"),
	plotMean = c(
		mean(subset(requests,(plot == "heatMap"))$time),
		mean(subset(requests,(plot == "maskMap"))$time),
		mean(subset(requests,(plot == "crossCountry"))$time),
		mean(subset(requests,(plot == "euBars"))$time),
		mean(subset(requests,(plot == "inCountry"))$time),
		mean(subset(requests,(plot == "euMatrix"))$time)
	),
	xroma = c(pallet.clusters[1],pallet.clusters[2],pallet.clusters[3],pallet.clusters[4],pallet.clusters[5],pallet.clusters[6])
)

plotMeans$plotMean= round(plotMeans$plotMean, 1)
plotMeans <- sqldf("select * from plotMeans order by plotMean asc")

requests <- merge(requests, plotMeans)
requests[,"print"] <- paste(" \n ", requests[,"plotMean"], " milliseconds mean time  - ",  requests$enplot, "(",requests$plot,") " )

vis <- ggplot(requests, aes(x = time)) + 
	# geom_density() +
  stat_density(aes(ymax = ..density..,  ymin = -..density..),
  fill = "grey50", colour = "grey50",
  geom = "ribbon", position = "identity") +
 	
	facet_wrap(~ print, ncol = 1)+ theme_ef_incountry()    + opts(title = "Density per response time per visualisation - 0.6 , 1.2, 1.6 sec mean times") + ylab("Density") + xlab("Response time (milliseconds)")
 
 
vis <- vis + geom_vline(xintercept = 679, colour = as.character(plotMeans$xroma[2]))
vis <- vis + geom_vline(xintercept = 1200, colour = as.character(plotMeans$xroma[3]))
vis <- vis + geom_vline(xintercept = 1600, colour = as.character(plotMeans$xroma[4]))

vis
dev.off()

plotMeans
#          plot plotMean
#1      maskMap    679.3
#2    inCountry   1233.9
#3     euMatrix   1237.1
#4 crossCountry   1427.5
#5       euBars   1535.7
#6      heatMap   1634.2

mean(plotMeans$plotMean) 

#1291.283


#-----------------------

#8 NOT NECESSARY
requests <- readXls(paste(base.folder, "analyseRequests", sep="")
library("sqldf")
sqldf("select IP, count(*) Count from requests_all group by IP order by Count desc")
#qplot(IP, data=requests_all, geom="histogram")+  geom_histogram(aes(fill = ..count..))
 
#-----------------------
#-----------------------

getParameterInColumn <- function(column, data){
	return(gsub("&.*", "", gsub(paste("^.*", column , "=", sep = "") , "", data)))
}

#6
requests <- readXls(paste(base.folder, "analyseRequests", sep="")
file.png <- "D1.png"
Cairo(file = paste(base.folder,file.png,sep=""), type = "png", width = width, height = height, bg = "white");
requests[,"plot"] <- getParameterInColumn("plot", requests$url)
qplot(plot, data=requests, geom="histogram")+  geom_histogram(aes(fill = ..count..)) +      theme_ef_stacked() +scale_fill_gradient2(low = pallet.map[2], high = pallet.map[1], guide = "colorbar") + opts(title = "Requests per visualisation function ")  +  theme_ef() + xlab("Visualisation code") + ylab("Requests count") 
dev.off()

sqldf("select plot, count(*) Count from requests group by plot order by Count desc")

#          plot Count
#1      heatMap 17996
#2      maskMap  6094
#3 crossCountry  5058
#4       euBars  3259
#5    inCountry  2917
#6     euMatrix  1228


#-----------------------
#-----------------------
#-----------------------
#TABLE 

#10
 
requests[,"media"] <- getParameterInColumn("media", requests$url)
sqldf("select media, count(*) Count from requests group by media order by Count desc")
# not necessary qplot(media, data=requests, geom="histogram")+  geom_histogram(aes(fill = ..count..))
#just the result

#  media Count
#1   png 34911
#2   csv  1518
#3   eps   123

#-----------------------
#TABLE

#11
 
requests <- readXls(paste(base.folder, "analyseRequests", sep="")
requests[,"locale"] <- getParameterInColumn("locale", requests$url)
requests <- subset(requests, locale != "/DVS/render/?plot=maskMap" )
sqldf("select locale, count(*) Count from requests group by locale order by Count desc")

#			Locale Count
#1     EN 27605
#2     ES  1064
#3     FR   854
#4     DE   786
#5     NL   149

#-----------------------

#12
requests <- readXls(paste(base.folder, "analyseRequests", sep="")
file.png <- "E1.png"
 
requests[,"question"] <- getParameterInColumn("question", requests$url)
requests <- subset(requests, question != "/DVS/render/?plot=maskMap" )
qplot(question, data=requests, geom="histogram")+  geom_histogram(aes(fill = ..count..))+     theme_ef()  + opts(title = "Requests per question")  +  theme_ef() + xlab("Question code") + ylab("Requests count")  + theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1)) 
dev.off()


sqldf("select question, count(*) Count from requests group by question order by Count desc limit 20")
#          	question Count
#1       Y11_Partnerprefhours  6040
#2  Y11_PreferredWorkingHours  2876
#3                   Y11_Q29a  1066
#4    Y11_Strainbasedconflict   748
#5                    Y11_Q41   628
#6                    Y11_Q57   592
#7                    Y11_Q24   573
#8                    Y11_Q15   510
#9                   Y11_Q33a   502
#10                   Y11_Q58   480
#11                   Y11_Q42   469
#12                   Y11_Q18   454
#13                  Y11_Q21a   422
#14            Y11_Disability   408
#15                  Y11_Q28e   400
#16                  Y11_Q12a   392
#17                  Y11_Q53a   392
#18                  Y11_Q29e   366
#19                  Y11_Q12b   334
#20                   Y11_Q30   317

#-----------------------

#13

requests <- readXls(paste(base.folder, "analyseRequests", sep="")
requests[,"question"] <- getParameterInColumn("question", requests$url)
requests <- subset(requests, question != "/DVS/render/?plot=maskMap" )
replaceValuesInDataframe <- function(data, column, value.translated ){
		for(row in 1:nrow(value.translated)){
			column.current.value <- as.character( value.translated[row, "Value"] )
			column.new.value <- as.character( value.translated[row, "TranslatedValue"] )
			if(column.current.value != column.new.value){
				if(!(column.new.value %in% levels(data[[column]]))){
					levels(data[[column]]) <- c(levels(data[[column]]), column.new.value)
				}
				data[data[[column]] == column.current.value, column] <- column.new.value
			}
	}
	return(data)
}

values.newValues = data.frame(
	Value = c(
			"/DVS/render/?plot=maskMap", "/DVS/render/?locale=FR", "/DVS/render/?locale=EN","/DVS/render/?locale=NL","/DVS/render/?locale=ES", "/DVS/render/?locale=DE", "Y11_Incomequartiles_percapita", "Y11_HH2a", "Y11_Agecategory"
	),
	TranslatedValue = c(
		"Not Applicable", "Not Applicable",  "Not Applicable", "Not Applicable","Not Applicable", "Not Applicable", "Income", "Gender", "Age"
	)
)

requests <- readXls(paste(base.folder, "analyseRequests", sep="")
requests[,"subset"] <- getParameterInColumn("subset", requests$url)
requests <- replaceValuesInDataframe(requests,6, values.newValues)

requests <- readXls(paste(base.folder, "analyseRequests", sep="")
file.png <- "E2.png"
# A
qplot(subset, data=requests, geom="histogram")+  geom_histogram(aes(fill = ..count..)) + coord_flip() + theme_ef_incountry()  +scale_fill_gradient2(low = pallet.map[2], high = pallet.map[1], guide = "colorbar")  + opts(title = "Requests per subset") + ylab("Requests count")
dev.off()
#-----------------------

# B
requests <- readXls(paste(base.folder, "analyseRequests", sep="")
file.png <- "E3.png"
requests <- subset(requests, subset != "Not Applicable" )
requests[,"subsetValue"] <- getParameterInColumn("subsetValue", requests$url)
requests <- replaceValuesInDataframe(requests, 7, values.newValues)
requests <- subset(requests, subsetValue != "Not Applicable" )
requests <- subset(requests, subsetValue != "All" )

requests[,"subsetFull"] <- paste( "[",requests[,"subset"],"] - [", requests[,"subsetValue"],"]")

qplot(subsetFull, data=requests, geom="histogram") + geom_histogram(aes(fill = ..count..)) + coord_flip() + theme_ef_incountry()  +scale_fill_gradient2(low = pallet.map[2], high = pallet.map[1], guide = "colorbar")  + opts(title = "Requests per subset - subset value (where this is applicable)") + ylab("Requests count")
dev.off()
#-----------------------


#  14
requests <- readXls(paste(base.folder, "analyseRequests", sep="")
file.png <- "E4.png"
 
requests[,"plot"] <- getParameterInColumn("plot", requests$url)
requests <- subset(requests, plot == "inCountry" | plot == "crossCountry")
requests[,"country"] <- getParameterInColumn("country", requests$url)

qplot(country, data=requests, geom="histogram") + geom_histogram(aes(fill = ..count..)) + theme_ef_incountry()  +scale_fill_gradient2(low = pallet.map[2], high = pallet.map[1], guide = "colorbar")  + opts(title = "Requests per selected country - (where this is applicable)") + ylab("Requests count")
sqldf("select country, count(*) countby from requests group by country order by countby desc")
dev.off()

#Country countby
#1       AT    2220
#2       EL    1032
#3       ES     763
#4       DE     621
#5       IE     509
#6       MT     319
#7       RO     212
#8       PT     191
#9       SE     163
#10      IT     159
#11      DK     153
#12      NL     152
#13      FR     145
#14      BG     136
#15      LV     126
#16      UK     123
#17      HU     118
#18    EU27     114
#19      FI     111
#20      BE     110
#21      PL     102
#22      LU      78
#23      EE      67
#24      SK      65
#25      CZ      61
#26      LT      52
#27      SI      52
#28      CY      21


-----------------------

