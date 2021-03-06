#' @name makeAuthorCloud
#' @title Author cloud maker
#' @description Make word cloud of papers
#' @param tab Table generated by googleCite
#' @param splitter split author names (default " ")
#' @param auth_pat Authors to drop
#' @param addastopwords words to delete from wordcloud
#' @param verbose Diagnostic message
#' @param ... Not currently used
#' @return NULL (Plot rendered)
#' @import RColorBrewer
#' @import tm
#' @import wordcloud
#' @import stringr
#' 
#' @export
makeAuthorCloud <- function(tab, splitter = ", ", auth_pat=NULL, 
                            addastopwords = NULL, verbose=TRUE, ...) {
  if (verbose) print(paste("Author Pattern:", auth_pat))
  stopifnot(!is.null(auth_pat))
  auth_pat <- tolower(auth_pat)
  colIndex = which(names(tab) == "Authors")
  
  auths <- as.character(tab[,colIndex])
  tmp = strsplit(auths, splitter)
  tmp <- lapply(tmp, function(x) tolower(str_trim(x)))
  tmp <- lapply(tmp, function(x) gsub(",", "", x))
  tmp <- lapply(tmp, function(x) return(x[!grepl(x, pattern=auth_pat)]))
  #out <- sapply(tmp, function(x) return(x[length(x)]))
	out = sapply(tmp, function(x) {
		x = strsplit(x, " ")
		x = sapply(x, function(x) x[length(x)])
		return(x)
	})
  #out <- tmp
  # out = sapply(tmp, function(x) {
	# x = strsplit(x, " ")
	# x = sapply(x, function(x) x[1])
	# x <- gsub(pattern=",", replacement="", x=x, fixed=TRUE)
	# x = tolower(x)
	# return(x)})
  out = unlist(out)
  tmp2 = table(out)
  tmp2 = tmp2[!(names(tmp2) == "...")]
  tmp2 = tmp2[!(names(tmp2) %in% tolower(addastopwords))]
  d = data.frame(word = names(tmp2), freq = tmp2, row.names = NULL)
  d = d[order(d$freq, decreasing = TRUE),]
  # d = d[-1,]
  
  pal = brewer.pal(9, "BuGn") 
  pal <- pal[-(1:4)]
  
  wordcloud(words = d$word, freq = d$freq, 
            min.freq = 1, max.words = Inf,
            random.order = FALSE, 
			colors = pal,vfont=c("sans serif","plain"))
}
