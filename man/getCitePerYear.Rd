\name{getCitePerYear}
\alias{getCitePerYear}
\title{Get citation and h indices per year}
\usage{
  getCitePerYear(theurl, year = NULL, url = TRUE,
    this.year = as.numeric(format(Sys.time(), "\%Y")))
}
\arguments{
  \item{theurl}{url from Google Citation or htmlTreeParse
  if (url=FALSE)}

  \item{year}{year to start from (default NULL)}

  \item{url}{(logical) is theurl a url or htmlTree}

  \item{this.year}{The end year (default this year)}
}
\description{
  Get citation and h indices per year
}

