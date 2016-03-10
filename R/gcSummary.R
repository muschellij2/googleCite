#' @name gcSummary 
#' @title Google Citation summary
#' @description Gets a summary from google citations page
#' @param alldata googleCite output
#' @return Printout of outcomes
#' @export
gcSummary <-
function(alldata){
  citations = as.numeric(alldata$Citations)
  citations[is.na(citations)] = 0
  nauthors = as.numeric(alldata$"N Authors")
  n = dim(alldata)[1]
  nF = sum(alldata$Is_First)
  nL = sum(alldata$Is_Last)
  nFL = sum(alldata$Is_Last | alldata$Is_First)
  nFS = sum(alldata$Is_First | alldata$Is_Second)

  totalPapers = dim(alldata)[1]
  totalCites = sum(citations,na.rm=T)
  medianCites = median(citations,na.rm=T)
  medianAuthorCites = median(citations/nauthors,na.rm=T)
  
  hindex = sum(citations > 1:n,na.rm=T)
  hindexF = sum(citations[alldata$Is_First]> 1:nF,na.rm=T)
  hindexL = sum(citations[alldata$Is_Last] > 1:nL,na.rm=T)
  hindexFL = sum(citations[alldata$Is_Last | alldata$Is_First] > 1:nFL,na.rm=T)
  hindexFS = sum(citations[alldata$Is_First | alldata$Is_Second] > 1:nFL,na.rm=T)
  
  tmp = cumsum(citations)
  
  gindex = sum(tmp >= (1:n)^2)

  nyears =  as.numeric(format(Sys.time(), "%Y")) - min(as.numeric(alldata$Year),na.rm=T)
  mindex = hindex/nyears
  
  cat("Total papers = ")
  cat(totalPapers)
  cat("\n")
  cat("Median citations per paper = ")
  cat(medianCites)
  cat("\n")
  cat("Median (citations / # of authors) per paper = ")
  cat(medianAuthorCites)
  cat("\n")
  cat("H-index = ")
  cat(hindex)
  cat("\n")
  cat("G-index = ")
  cat(gindex)
  cat("\n")
  cat("M-index = ")
  cat(mindex)
  cat("\n")
  cat("First author H-index = ")
  cat(hindexF)
  cat("\n")
  cat("Last author H-index = ")
  cat(hindexL)
  cat("\n")
  cat("First or last author H-index = ")
  cat(hindexFL)
  cat("\n")
  cat("First or second author H-index = ")
  cat(hindexFS)
  cat("\n")
  
}
