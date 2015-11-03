
# ======================================================================
# Eworx S.A. - 2013
# Author kp@eworx.gr
# ======================================================================
# Analyses logs to find the links that represent most, the requests already performed
# ======================================================================
# source("/web-pub/foundation/html/DVS/render/logs/analyseLogs/adaptiveCaching/linkRequestsFrequencies3.R")

options(gsubfn.engine = "R")
library("sqldf")


readXls <- function(file){
  return(read.csv(file = file, head = TRUE, encoding = "UTF-8", sep = "\t"))
}



#-----------------------
width = 740
height = 740
base.folder <- "/web-pub/foundation/html/DVS/render/logs/"
#-----------------------

#"IP"	"currentTimeMil"	"url"	"served"	"time"

requests.file<-paste(base.folder, "analyseRequests", sep="")
print(paste("Analysing log file :", requests.file))

requests <- readXls(requests.file)
print(paste("Columns:",names(requests)))
print(paste("number of rows: ", nrow(requests)))


library(gclus)
#library(ape)


findWorthyNumberOfLinks <- function(requestsFrequencies, cluster = 1, verbose = 0){

	library(gclus)
#	library(ape)
	nrows <- nrow(requestsFrequencies)

	if(nrows<3)
	return(" please provide more data")
	totalRequestsSum <- sum(requestsFrequencies$TotalRequests)
	#print(paste("Total Requests:", totalRequestsSum))

	requestedClusters<-requestsFrequencies[c("NumberOfLinks")]
	dis <- dist(requestedClusters)
	hc <- hclust(dis)
	clus5 = cutree(hc, ifelse(nrows>10,10, nrows))
	requestedClusters[,"NumberOfLinksCluster"] <- clus5

	worthyMaxNumberOfLinksRequests <- sqldf(paste("select max(NumberOfLinks) toSelect from requestedClusters where NumberOfLinksCluster <  ", (cluster+1)))
	worthyMaxNumberOfLinksRequests <- max(worthyMaxNumberOfLinksRequests$toSelect)
	#print(paste("Worthy Max number of requests Requests:", worthyMaxNumberOfLinksRequests))

	worthyRequestsFrequencies <- sqldf(paste("select * from requestsFrequencies where NumberOfLinks <= ", worthyMaxNumberOfLinksRequests ))
	worthyRequestsFrequencies[, "Represents"] <- (worthyRequestsFrequencies[, "TotalRequests"] / totalRequestsSum) * 100

	result <- c(round(cluster,2), round(worthyMaxNumberOfLinksRequests,2), round(sum(worthyRequestsFrequencies$NumberOfLinks)),round(sum(worthyRequestsFrequencies$Represents)), totalRequestsSum  )
		if(verbose ){
			print(paste(
				"Up to the first", result[1], "clusters => "
			)
		)
		print(paste(
				"   with ",
				result[3],
				" links you cover ",
				result[4],
				"% of the total requests :",
				result[5]
			)
		)
	}

	return(result)
}

analyseResults <- function (requestsFrequencies){
	Locales <- unique(requestsFrequencies$Locale)

	for(aLocale in Locales){
		Plots <- unique(requestsFrequencies$Plot)
		for(aPlot in Plots){
			print("");print("");print(paste("Plot:", aPlot));print("------------------------");print("")
			requestsFrequenciesInPlot <- subset(requestsFrequencies, Plot == aPlot & Locale == aLocale)
			print(requestsFrequenciesInPlot )
			print("");print("------------------------")
			nrows <- length(unique(requestsFrequenciesInPlot[,"NumberOfLinks"]))


			for(index in 1:ifelse(nrows>10,10, nrows)){
					print(findWorthyNumberOfLinks(requestsFrequenciesInPlot, index))
			}
			print("------------------------")
			print(findWorthyNumberOfLinks(requestsFrequenciesInPlot, 1, 1))
			print(findWorthyNumberOfLinks(requestsFrequenciesInPlot, 5, 1))
			print("------------------------")
		}

	}
}

analyseResults <- function (requestsFrequencies){
	Locales <- unique(requestsFrequencies$Locale)

	for(aLocale in Locales){
		Plots <- unique(requestsFrequencies$Plot)
		for(aPlot in Plots){
			print("");print("");print(paste("Plot:", aPlot));print("------------------------");print("")
			requestsFrequenciesInPlot <- subset(requestsFrequencies, Plot == aPlot & Locale == aLocale)
			print(requestsFrequenciesInPlot )
			print("");print("------------------------")
			nrows <- length(unique(requestsFrequenciesInPlot[,"NumberOfLinks"]))


			for(index in 1:ifelse(nrows>10,10, nrows)){
					print(findWorthyNumberOfLinks(requestsFrequenciesInPlot, index))
			}
			print("------------------------")
			print(findWorthyNumberOfLinks(requestsFrequenciesInPlot, 1, 1))
			print(findWorthyNumberOfLinks(requestsFrequenciesInPlot, 5, 1))
			print("------------------------")
		}

	}
}


retainWorthyLinksUpToNthCluster <- function(requestsCount, requestsFrequencies, cluster = 1, verbose = 0){

 	NumberOfLinks <- findWorthyNumberOfLinks(requestsFrequencies, cluster, 1)[2]
 	RequestedAtLeast <- sqldf(paste("select Requested toSelect from requestsFrequencies where NumberOfLinks >=  ", NumberOfLinks))
	RequestedAtLeast <- max(RequestedAtLeast$toSelect)
 	result <- sqldf(paste("select * from requestsCount where Count >", RequestedAtLeast, "order by Count desc"))
	print(paste("Links requested from ", min(result$Count), "-", max(result$Count), "times"))
 	return(result)

}


writeTxt<- function(data, file, append = FALSE){
  write.table(data, file, sep = "\t", append = append, na = "", row.names = FALSE, col.names = FALSE, quote = FALSE)
}
writeXls<- function(data, file, append = FALSE){
  write.table(data, file, sep = "\t", append = append, na = "", row.names = FALSE)
}
getParameterInColumn <- function(column, data){
	return(gsub("&.*", "", gsub(paste("^.*", column , "=", sep = "") , "", data)))
}

#------------------------------------------------------

if(TRUE){

	requests[,"Survey"] <- getParameterInColumn("survey", requests$url)
	requests[,"Locale"] <- getParameterInColumn("locale", requests$url)
	requests[,"Plot"] <- getParameterInColumn("plot", requests$url)
	
	requests[,"Locale"] <- "all"
	requests[,"Plot"] <- "all"
	
	
	requestsCount <- sqldf("select Locale, Plot, url, count(*) Count from requests group by Locale, Plot, url order by Count")
		
	
	requestsFrequencies <- sqldf("select Locale, Plot, count(*) NumberOfLinks, Count Requested  from requestsCount group by Locale, Plot, Requested order by Locale, Plot, Requested desc")
	requestsFrequencies[,"TotalRequests"] <- requestsFrequencies[,"NumberOfLinks"] *  requestsFrequencies[,"Requested"]
	
	frequencies.file <- paste(base.folder, "analyseLogs/linkFrequencies.xls", sep="")
	writeXls(requestsFrequencies, frequencies.file)		
	print(requestsFrequencies)	
	print(paste("File in:", frequencies.file ))
	
	#analyseResults (requestsFrequencies)
	
	links <- retainWorthyLinksUpToNthCluster(requestsCount, requestsFrequencies, 7)
	
	links.file <- paste(base.folder, "analyseLogs/newLinks.txt", sep="")
	writeTxt(links$url, links.file)
	print(links.file)
}
