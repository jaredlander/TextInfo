% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TextExtraction.r
\name{extractTextInfo}
\alias{extractTextInfo}
\title{extractTextInfo}
\usage{
extractTextInfo(text, nerModel, requiredTerms = NULL, ignore.case = TRUE)
}
\arguments{
\item{text}{A vector of text}

\item{nerModel}{A ner model supplied by MITIE}

\item{requiredTerms}{A vector of terms that must be extracted if they exist}

\item{ignore.case}{Logical indicating if requiredTerms is not case sensitive}
}
\value{
A tbl of entity pairs demarcated by the sentence they are paired in.  Could result in multiple combinations.
}
\description{
Given a vector of text, detect sentence-wise entity pairings.
}
\details{
Given a vector of text, detect sentence-wise entity pairings.  Works even if senetences are broken up over multiple elements of the vector of text.
}
\examples{
\dontrun{
ner_model_path <- "tests/data/ner_model.dat"
ner <- NamedEntityExtractor$new(ner_model_path)
thisText <- c("A network of new super PACs said Wednesday that it had raised $31 million to support Senator Ted Cruz's presidential campaign, a sum that could upend expectations in the race for the Republican nomination and rewrite the political rule book for outside spending.",
"The groups, four super PACs sharing variations of the name Keep the Promise, were established and secured commitments with virtually no warning over the course of several days beginning Monday.",
"Dathan Voelter, an Austin, Tex., lawyer and friend of Mr. Cruz who is serving as treasurer for three of the super PACs, said the four organizations would operate in tandem, all seeking to help elect the Texas senator as president. Most of the contributions have already arrived, he said, and the remainder will be collected by the four groups by the end of the week.",
"The dollar figures could not be independently verified, and none of the groups will need to file campaign disclosures with the Federal Election Commission until July. But an outside spending campaign of that size, combined with Mr. Cruz's demonstrated ability to pull in dollars from small donors, would substantially offset Mr. Cruz's difficulties in building a traditional network of regular large donors and volunteer fund-raisers, known as bundlers.",
"The size of the contributions is likely to force backers of other candidates to rethink their budgets for the primary season; other super PACs lining up behind Republican candidates had planned to raise $20 million to $30 million over the course of the entire primary campaign.")
extractTextInfo(thisText, nerModel=ner)
extractTextInfo(thisText, nerModel=ner, requiredTerms=c('dollar'))
}

}
\author{
Jared P. Lander
}

