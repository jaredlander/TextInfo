% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PairExtraction.r
\name{queryAndExtract.OrientDB}
\alias{queryAndExtract.OrientDB}
\title{queryAndExtract.OrientDB}
\usage{
queryAndExtract.OrientDB(ID, db, nerModel, ...)
}
\arguments{
\item{ID}{Record ID to query}

\item{db}{An OrientDB connection}

\item{nerModel}{A ner model supplied by MITIE}

\item{\dots}{Further arguments}
}
\value{
A tbl listing entity cooccurences along with the ID and the sentence number.
}
\description{
Extracts entities from an OrientDB entry
}
\details{
Queries the database and runs the result through \code{extractPairs.tbl}
}
\author{
Jared P. Lander
}

