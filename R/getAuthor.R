#' @name getAuthor 
#' @title Get the author that the googleCite is based on
#' @description Return the author from a specific Google Citation webpage
#' @param webpage Google Citation Webpage
#' @return name of authors
#' @export
getAuthor <-
function(webpage) {
	# old.locale <- Sys.getlocale()
	Sys.setlocale(category = "LC_ALL", locale = "C")
	old.warn = options()$warn
	options(warn = -1)	
	on.exit({
	  Sys.setlocale(locale = "")
	  options(warn = old.warn)
	})

	y = htmlTreeParse(webpage, isURL=TRUE, 
                    useInternalNodes=TRUE, addFinalizer=TRUE)
	names = xpathSApply(y, "//title", xmlValue)
	names <- strsplit(names, "-")[[1]][1]
	names <- str_trim(names)
	for (icol in 1:ncol(googleCite::hexdata)) {
	  switcher <- googleCite::hexdata[,icol]
	  names <- gsub(pattern=switcher[1], replacement=switcher[2], 
                x=names, fixed=TRUE)
	}  
	out <- names
  
  return(out)
  
}
