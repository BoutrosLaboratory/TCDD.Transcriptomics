\name{load.TCDD.expression}
\alias{load.TCDD.expression}
\title{Load TCDD analyses expression datasets}
\description{Returns a list of lists containing normalized expression values from requested TCDD analyses and corresponding phenodata}
\usage{
load.TCDD.expression(datasets.to.load = 'all', verbose = FALSE);
}
\arguments{
  \item{datasets.to.load}{A list of datasets to be loaded. Use \code{\link[TCDD.Transcriptomics]{load.dataset.summary}} to generate list of datasets to be loaded. If 'all', then all available datasets are loaded.}
  \item{verbose}{Indicates whether or not status messages should be given}
}
\value{Returns a list of expression datasets requested (<variable>$expression) and corresponding phenodata (<variable>$phenodata)}
\author{Katie Houlahan}
\seealso{\code{\link[TCDD.Transcriptomics]{load.dataset.summary}}, \code{\link[TCDD.Transcriptomics]{load.TCDD.results}}}
\examples{
# load normalized array data from H/W rat liver, 19hr exposure to 0 and 100 ug/kg TCDD
tmp.data <- load.TCDD.expression(datasets.to.load = 'rat.HW.liver.19h.100');
normalized.data <- tmp.data$expression;
phenodata <- tmp.data$phenodata;

# load all available datasets
all.data <- load.TCDD.expression(datasets.to.load = 'all');
}
\keyword{IO}

