#' @name getPapers
#' @title Grab the papers from the Google Citations page
#' @param theurl URL to be parsed
#' @export

getPapers <-
function(theurl) {
	y = htmlTreeParse(theurl, isURL=T, useInternalNodes=T, addFinalizer=T)
	src = xpathSApply(y, "//tr//a[@href]", xmlGetAttr, "href")
	out= src[grep("citation_for_view",src)] # you would need to collect these across multiple pages
	return(out)
}
