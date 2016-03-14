#' @name getCitePerYear
#' @title Get citation and h indices per year
#' @description Making citation indices for each year
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
  nodes = getNodeSet(y, path = topstr)
  
  if (length(nodes) == 0) {
      years <- seq(year, to=this.year)
      nyears <- length(years)
      return(matrix(cbind(Year=years, 
                          Citations=rep(NA, nyears)), 
                    nrow=nyears, ncol=2))
    
  } 
  node = nodes[[1]]
  node = xpathApply(node, '//div[@id="gsc_graph_bars"]')[[1]]
  graph_y = getNodeSet(node, '//a[@class="gsc_g_a"]')
  yearvals = lapply(graph_y, function(r){
    href = xmlGetAttr(r, "href")
    href = strsplit(href, "&")[[1]]
    tryer = "lo"
    get_year = function(tryer, href){
      href = grep(paste0("as_y", tryer, "="), href, value = TRUE)
      href = strsplit(href, "=")
      href = sapply(href, function(x) {
        if (length(x) > 1) {
          return(as.numeric(x[2]))
        } else {
          return(NA)
        }
      })
    }
    year = get_year(tryer = "lo", href)
    year = year[!is.na(year)]
    if (length(year) == 0) {
      year = get_year(tryer = "hi", href)
      year = year[!is.na(year)]      
    }
    if (length(year) == 0) {
      year = NA
    }
    value = fillin(xmlValue(r))
    
    c(Year = year, Citations = as.numeric(value))
  })
  hind = do.call("rbind", yearvals)
  #   xstring = "x"
#   if (pagegraph) xstring = "bars"
#   top_x = paste0(topstr, paste0('//div[@id = "gsc_g', adder , '_', xstring, '"]'))
#   
# 	graph_x = xpathApply(y, paste0(top_x, '//span[@class="gsc_g_t"]'), xmlValue)
#   graph_x = as.numeric(sapply(graph_x, fillin))
#   
# 	top_y = paste0(topstr, paste0('//div[@id = "gsc_g', adder, '_bars"]'))
# 	graph_y = xpathApply(y, paste0(top_y, '//span[@class="gsc_g_al"]'), xmlValue)
# 	graph_y = as.numeric(sapply(graph_y, fillin))
#   

# 	####
# 	hind <- cbind(Year = chxl, Citations=round(chd))
  # hind = cbind(Year = graph_x, Citations = graph_y)
  if (nrow(hind) == 0) {
		years <- seq(year, to=this.year)
		nyears <- length(years)
		return(matrix(cbind(Year=years, Citations=rep(0, nyears)), 
                  nrow=nyears, ncol=2))
	}
	return (hind)
}
