#' @name getCitePerYear
#' @title Get citation and h indices per year
#' @param theurl url from Google Citation or htmlTreeParse if (url=FALSE)
#' @param year year to start from (default NULL)
#' @param url (logical) is theurl a url or htmlTree
#' @param this.year The end year (default this year)
#' @param pagegraph Use this for graphs of an article
#' @export


getCitePerYear <-
function(theurl, year=NULL, url=TRUE, 
         this.year=as.numeric(format(Sys.time(), "%Y")),
         pagegraph = FALSE) {
	## if you've already parsed the url, then just pass that htmltree in
	if (url) {
    y = htmlTreeParse(theurl, isURL=TRUE, useInternalNodes=TRUE, 
                             addFinalizer=TRUE)
  }	else {
    y <- theurl
  }
  
	fillin = function(x){
	  if (length(x) == 0) return(NA)
	  return(x)
	}  
  adder = ""
  if (pagegraph) adder = "raph"
  topstr = paste0('//div[@id="gsc_g', adder, '"]')
  if (pagegraph) topstr = ""
  top_x = paste0(topstr, paste0('//div[@id = "gsc_g', adder , '_x"]'))
  
	graph_x = xpathApply(y, paste0(top_x, '//span[@class="gsc_g_t"]'), xmlValue)
  graph_x = as.numeric(sapply(graph_x, fillin))
  
	top_y = paste0(topstr, paste0('//div[@id = "gsc_g', adder, '_bars"]'))
	graph_y = xpathApply(y, paste0(top_y, '//span[@class="gsc_g_al"]'), xmlValue)
	graph_y = as.numeric(sapply(graph_y, fillin))
#   
# 	src = xpathSApply(y, "//td//img", xmlGetAttr, "src")
# 	if (length(src) > 1) src <- src[grepl(pattern="chart?", x=src, fixed=TRUE)]
# 	if (length(src) > 1) stop("Citation code has changed")
# 	if (length(src) == 0) {
# 		years <- year:this.year
# 		nyears <- length(years)
# 		return(matrix(cbind(Year=years, Citations=rep(0, nyears)), 
#                   nrow=nyears, ncol=2))
# 	}
# 	tags <- strsplit(src, split="&")[[1]]
# 	chxr <- tags[grepl(tags, pattern="chxr")] 
# 	chd <- tags[grepl(tags, pattern="chd")]
# 	chxl <- tags[grepl(tags, pattern="chxl")]
# 	chd <- sub(pattern="chd=t:", replacement = "", x=chd, fixed=TRUE)
# 	chxl <- sub(pattern="chxl=0:|", replacement = "", x=chxl, fixed=TRUE)
# 	## chxr goes 1 (axis), 0 (start), max_num of citatations (end), 
#   ## and how many units per tick (usually max numb)
# 	chxr <- sub(pattern="chxr=1,0,", replacement = "", x=chxr, fixed=TRUE)
# 	chd <- as.numeric(strsplit(chd, split=",")[[1]])
# 	chxl <- as.numeric(strsplit(chxl, split="|", fixed=TRUE)[[1]])
# 	chxl <- min(chxl, na.rm=TRUE):max(chxl, na.rm=TRUE)
# 	chxr <- as.numeric(strsplit(chxr, split=",")[[1]][1])
# 	chd <- chd / 100 * chxr
# 	####
# 	hind <- cbind(Year = chxl, Citations=round(chd))
  hind = cbind(Year = graph_x, Citations = graph_y)
	return (hind)
}
