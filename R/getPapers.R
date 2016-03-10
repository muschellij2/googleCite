#' @name getPapers
#' @title Grab the papers from the Google Citations page
#' @description Exract papers from a Google Citations Page
#' @param theurl URL to be parsed
#' @export
getPapers <-
function(theurl) {
	y = htmlTreeParse(theurl, isURL=TRUE, useInternalNodes=TRUE, addFinalizer=TRUE)
	src = xpathSApply(y, "//tr//a[@href]", xmlGetAttr, "href")
	out= src[grep("citation_for_view",src)] # you would need to collect these across multiple pages
	return(out)
}
