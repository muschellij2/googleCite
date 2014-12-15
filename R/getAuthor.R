#' @name getAuthor 
#' @title Get the author that the googleCite is based on
#' @param webpage Google Citation Webpage
#' @return name of authors
#' @export

getAuthor <-
function(webpage) {
	old.locale <- Sys.getlocale()
	Sys.setlocale(locale="C")
	options(warn = -1)	

	y = htmlTreeParse(webpage, isURL=TRUE, 
                    useInternalNodes=TRUE, addFinalizer=TRUE)
	names = xpathSApply(y, "//title", xmlValue)
	names <- strsplit(names, "-")[[1]][1]
	names <- str_trim(names)
	for (icol in 1:ncol(hexdata)) {
	  switcher <- hexdata[,icol]
	  names <- gsub(pattern=switcher[1], replacement=switcher[2], 
                x=names, fixed=TRUE)
	}  
	out <- names
  
  return(out)
  
}
