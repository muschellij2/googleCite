\name{searchCite}
\alias{searchCite}
\title{Google Citation Searcher}
\usage{
  searchCite(Author, gCite = TRUE, verbose = TRUE,
    ask = TRUE, addcrit = NULL, ...)
}
\arguments{
  \item{Author}{Author name separated by function}

  \item{gCite}{(logical) Return Google Cite output of
  choice}

  \item{...}{additional parameters passed to
  \code{googleCite}}
}
\value{
  googleCite output if gCite is TRUE, else return vector of
  author list
}
\description{
  Search Google Citation for an author
}
\examples{
searchCite("John Muschelli")
}

