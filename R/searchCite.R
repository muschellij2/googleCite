#' @name searchCite
#' @title Google Citation Searcher
#' @description Search Google Citation for an author
#' @param Author Author name separated by function
#' @param gCite (logical) Return Google Cite output of choice
#' @param ... additional parameters passed to \code{googleCite}
#' @return googleCite output if gCite is TRUE, else return vector of 
#' author list
#' @examples
#' searchCite("John Muschelli")
#' @export

searchCite <- function(Author, gCite=TRUE, verbose=TRUE, ask=TRUE, ...){
  auth.names <- strsplit(Author, " ")[[1]]
  auth.names <- paste(auth.names[1:length(auth.names)], sep="", collapse="+")
  
  ### search the page
  search.page <- paste("http://scholar.google.com/citations?hl=en&view_op=search_authors&mauthors=", auth.names, sep="")
  thepage <- url(search.page)
  x <- readLines(thepage)
  x <- x[[1]]  
	y = htmlTreeParse(x, isURL=F, useInternalNodes=T, 
                    addFinalizer=T, asText = TRUE)
	names = xpathSApply(y, "//tr//a[@href]", xmlValue)
	
	z <- getNodeSet(y, "//tr//td[@valign='top']", 
                  addFinalizer=T)
	insts <- sapply(z, function(x) xmlChildren(x)$text)
	insts <- sapply(insts, xmlValue)
	hrefs <- sapply(z, function(x) xmlChildren(x)$a)
	hr.attr <- sapply(hrefs, function(x) 
    ifelse(is.null(x), NA, xmlAttrs(x)))
  
  ### getting the author names if multiple
	keepers <- hr.attr %in% "cit-dark-large-link"
	hrefs <- hrefs[keepers]
	insts <- insts[keepers]
	names <- sapply(hrefs, xmlValue)
	names <- sapply(hrefs, xmlValue)
	src <- sapply(hrefs, xmlGetAttr, "href")
	if (sum(is.null(src))> 0) stop("Src'es are off")
	if (length(names) != length(src))
		stop("XML Parsing Error - use google Cite with webpage")
	keep <- rep(TRUE, length(names))
	keep[names %in% c("My Citations", "")] <- FALSE

	insts <- sub(pattern="<e2><80><a6>", replacement = "", 
               fixed=TRUE, x=insts)
	dat <- data.frame(cbind(names=names, insts=insts, 
                          src=src), stringsAsFactors = FALSE)
	dat <- dat[keep,]
	dat$fullnames <- paste(dat$names, dat$insts, sep=": ")
  if (nrow(dat) >= 1){
    ### if they have someone for a hit
    ##grab the first hit
    if (nrow(dat) == 1) {
    	choice <- 1
    	if (verbose) print(dat[1, c("fullnames", "src")])
   	} else {
   		if (ask) {
         choice <- menu(dat$fullnames, 
                      title="More than One Author, Please Choose")
   		} else {
         print(dat$fullnames)
         stop("Multiple choices")
   		}
   	}
    if (choice == 0) return("No Choice given, skipped")
    theurl <- paste("http://scholar.google.com", 
                    dat$src[choice], sep="")
    dat$url <- theurl 
    dat <- dat[choice, ]
    # print(theurl)
    if (gCite) return(googleCite(theurl, ...))
    else return(dat)
  } else return("No Author found")
  close(thepage)
}
