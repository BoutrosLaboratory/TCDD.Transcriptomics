\name{load.dataset.summary}
\alias{load.dataset.summary}
\title{Load summary of all TCDD datasets included within this package}
\description{Returns a dataframe containing name of datasets, microarray type, time point, TCDD dose used and GEO accession for further reference}
\usage{
load.dataset.summary();
}
\value{Returns a dataframe containing essential experimental information}
\author{Katie Houlahan}
\seealso{\code{\link[TCDD.Transcriptomics]{load.TCDD.expression}}, \code{\link[TCDD.Transcriptomics]{load.TCDD.results}}}
\examples{
datasets <- load.dataset.summary();
}
\keyword{IO}

