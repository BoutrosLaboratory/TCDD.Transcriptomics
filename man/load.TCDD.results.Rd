\name{load.TCDD.results}
\alias{load.TCDD.results}
\title{Load TCDD analysis results datasets}
\description{Returns a list of lists containing log2 coefficients, p-values, and FDR-adjusted p-values from TCDD analyses}
\usage{
load.TCDD.results(datasets.to.load = 'all', verbose = FALSE);
}
\arguments{
  \item{datasets.to.load}{A list of datasets to be loaded. Use \code{\link[TCDD.Transcriptomics]{load.dataset.summary}} to get a dataframe summarizing all available datasets. If 'all', then all available datasets are loaded.}
  \item{verbose}{Indicates whether or not status messages should be given}
}
\value{Returns a list of results datasets requested (<variable>$results)}
\author{Katie Houlahan}
\seealso{\code{\link[TCDD.Transcriptomics]{load.TCDD.expression}}, \code{\link[TCDD.Transcriptomics]{create.significance.covariates}}}
\examples{
results.data <- load.TCDD.results(datasets.to.load = 'rat.HW.liver.19h.100');
all.results.data <- load.TCDD.results(datasets.to.load = 'all');
}
\keyword{IO}
