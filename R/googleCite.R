#' @name googleCite
#' @title Workhorse function to extract information from google
#' citations page
#' @param theurl URL of the google Citation page to be analyzed
#' @param citations (logical) Obsolete
#' @param plotIt Plot the Author/Paper wordcloud
#' @param pdfname (NULL) filename of plot (if plotIt = TRUE)
#' @param dlpdfs (logical) Beta - download pdfs
#' @param dlfolder (file path) folder to download pdf (default getwd())
#' @param ... parameters to be passed to makeAuthorCloud, makePaperCloud
#' @export

googleCite <-
function(theurl, citations=FALSE, plotIt = FALSE,
         pdfname=NULL, dlpdfs=FALSE, dlfolder=NULL, ...) {

  theurl = strsplit(theurl,"&hl")[[1]][1]
  bibpage <- paste(theurl, "&view_op=export_citations", sep="")

  alldata = list()
  author = getAuthor(paste(theurl,"&view_op=list_works&pagesize=100&cstart=",0,sep=""))
	user.cpy <- getCitePerYear(theurl)
  for (ipage in 0:100){
	checker <- ipage * 100
	page = paste(theurl, "&view_op=list_works&pagesize=100&cstart=", checker, sep="")
	alldata[[ipage+1]]  = getPapers(page)
	# cpy  = getCitePerYear(page)
	
	if(length(alldata[[ipage+1]]) == 0) break
	}
	allPapers = unlist(alldata)
	
	thePath = "http://scholar.google.com"


	out=list()

	cols <- c("Title","Authors","Publication date", "Journal name", "Volume","Issue","Total citations")
	npap <- length(allPapers)
	allCitations <- matrix(NA, nrow=npap, ncol=length(cols))
	paper.cpy <- vector("list", length=npap)
	for (irow in 1:npap){
		zz <- allPapers[irow]

		cit = paste(thePath,zz, sep="/")
		
		x <- htmlTreeParse(cit, isURL=T, useInternalNodes=T, addFinalizer=T)
		#print(zz)
		tmp_all<- xpathSApply(x, "//div[@class='cit-dd']")
		pname <- sapply(tmp_all, function(x)xmlChildren(x)$div)
		pname <- pname[sapply(pname, function(x) !is.null(x))]
		pname <- sapply(pname, function(x) xmlChildren(x)$a)
		pname <- pname[sapply(pname, function(x) !is.null(x))]
		pname <- sapply(pname, xmlGetAttr, "href")


		tmp <- xpathSApply(x, "//div[@class='cit-dd']", xmlValue)

		
		tmp2<- xpathSApply(x, "//div[@class='cit-dt']", xmlValue)
		if (length(tmp) != length(tmp2)) stop("Something off about parsing")
		names(tmp) = tmp2
		### use which for ordering
		tmp = tmp[names(tmp) %in% cols]
		this.year <- as.numeric(format(Sys.time(), "%Y"))
		year <- as.numeric(strsplit(tmp["Publication date"], split="/", fixed=TRUE)[[1]][1])
		if (!is.na(year)) {
      cpy <- getCitePerYear(x, year=year, url=FALSE, this.year=this.year)
		} else {
      cpy <- cbind(Year=this.year, Citations=0)
		}
		colnames(cpy) <- c("Year", "Citations")
		
		paper.cpy[[irow]] <- cpy
		
		#### match so there are a known number of columns
		allCitations[irow,] <- tmp[match(cols, names(tmp))]
		# ret <- as.data.frame(ret, stringsAsFactors=FALSE)
	}
	# })

	x=paper.cpy
	
	for(i in seq(along=x)) x[[i]] = data.frame(x[[i]])
	# x2 <- x
	# for(i in seq(along=x2)) x2[[i]]$ID <- i
	# xx = do.call("rbind",x2)
	# x2 <- reshape(xx, direction="wide", timevar="Year", idvar="ID")
	# colnames(x2) <- gsub(x=colnames(x2), pattern="Citations.", replacement="", fixed=TRUE)
		
	xx = do.call("rbind",x)
	
	y = data.frame(matrix(ncol = length(unique(xx$Year)),
	nrow = length(x)))
	names(y) = min(xx$Year):max(xx$Year) 
	
	minYear = min(xx$Year)
	
	
	for(i in seq(along=x)) {
		ind = x[[i]]$Year-minYear+1
		y[i,ind] = x[[i]]$Citations
	}
	
	for(i in 1:nrow(y)) {
		ind = which(!is.na(y[i,]))
		ind= max(ind)
		if(ind < ncol(y)) y[i,(ind+1):ncol(y)] = rep(0)
	}

	
	
	# rownames(allCitations) <- 1:nrow(allCitations)
	
	### tried to make a function to download pdfs
	if (dlpdfs){
		allPdfs <- colnames(allCitations)
		allPdfs = sapply(allPapers, function(zz) {
	
			cit = paste(thePath,zz, sep="/")
			x <- htmlTreeParse(cit, isURL=T, useInternalNodes=T, addFinalizer=T)
			tmp_all<- xpathSApply(x, "//div[@class='cit-dd']")
			pname <- sapply(tmp_all, function(x)xmlChildren(x)$div)
			pname <- pname[sapply(pname, function(x) !is.null(x))]
			pname <- sapply(pname, function(x) xmlChildren(x)$a)
			pname <- pname[sapply(pname, function(x) !is.null(x))]
			pname <- sapply(pname, xmlGetAttr, "href")
	
		return(pname)
		})
		names(allPdfs) <- NULL
		old_dir <- getwd()
    outdir <- old_dir
		if (!is.null(dlfolder)) outdir <- dlfolder
		setwd(outdir)
    
		for (iname in 1:length(allPdfs)) system(sprintf('wget %s -A "*.pdf"', allPdfs[iname]))
		setwd(old_dir)
	}
	
		
#	allCitations = do.call("rbind",allCitations)
	#allCitations = t(allCitations)
	rownames(allCitations) = 1:nrow(allCitations)
	allCitations = data.frame(allCitations, stringsAsFactors= F)
	names(allCitations) <- cols
	names(allCitations)[7] = "Citations"
	allCitations$Citations = ss(allCitations$Citations, " by ", 2)
	# y$Title <- allCitations$Title

  	splitter <- ", "

  alldata <- allCitations
  
  alldata$Entry <- NULL
  
  alldata$"First Author" <- NA
  alldata$"Second Author" <- NA
  alldata$"Last Author" <- NA
  alldata$"N Authors" <- NA
  
  alldata$Author <- str_trim(alldata$Author)
  
  for(irow in 1:nrow(alldata)){
    tmp = strsplit(alldata$Author[irow], splitter)[[1]]
    if (length(tmp) ==  0) next;
    if (tmp == "" | is.na(tmp)) next;
    alldata$"First Author"[irow] <- tmp[1]
    alldata$"Second Author"[irow] <- ifelse(length(tmp) > 1, tmp[2], NA)
    alldata$"Last Author"[irow] <- tmp[length(tmp)]
    alldata$"N Authors"[irow] <- length(tmp)
    
  }
  ln <- strsplit(author, " ")[[1]]
  print(ln[length(ln)])
  ln <- gsub("\\.", "", ln)
  ln <- paste(ln[2:length(ln)], collapse=" ")
  ## may be add out 2013Sep3
#   ln <- ln[length(ln)]
  print(ln)
 	auth_pat <- tolower(paste(substr(author, 1,1), ".* ", ln, sep=""))
  alldata$Is_First <- grepl(tolower(alldata$"First Author"), pattern=auth_pat)
  alldata$Is_Second <- grepl(tolower(alldata$"Second Author"),pattern=auth_pat)
  alldata$Is_Last <- grepl(tolower(alldata$"Last Author"),pattern=auth_pat)

  #alldata$"First Author" <- NULL
  #alldata$"Second Author" <- NULL
  #alldata$"Last Author" <- NULL
  
  alldata$Author <- alldata$Authors
  
  
  if(plotIt) {
    if (!is.null(pdfname)) pdf(pdfname, height = 6, width = 12)
    
    par(mfrow = c(1,2))
    makeAuthorCloud(alldata, splitter, auth_pat, ...)
    makePaperCloud(alldata, ...)
    
    if (!is.null(pdfname)) dev.off()
  }
  
  alldata$Author <- NULL
	alldata$Year <- as.numeric(sapply(strsplit(alldata[, "Publication date"], split="/", fixed=TRUE), function(x) return(x[1])))
 
  alldata <- cbind(alldata, y)
  return(alldata)
}
