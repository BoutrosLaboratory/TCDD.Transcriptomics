\name{create.significance.covariates}
\alias{create.significance.covariates}
\title{Create plot covariates specifying significance of change in mRNA abundance across experiments}
\description{Determines if genes to be plotted are significantly differentially abundant in previously analyzed mRNA datasets. Creates vectors with arguments to make graphical covariates and corresponding legend that can be added to a plot.}
\usage{
create.significance.covariates(data.variable, genes.to.plot = c(),
    gene.id.type = "Symbol", covariates.to.create = NULL,
    significance.threshold = 0.05, significant.colours = NULL
    );
}
\arguments{
  \item{data.variable}{Variable name containing data read in by the load.TCDD.results function}
  \item{genes.to.plot}{List of genes to be plotted; can be gene symbol or EntrezGeneID}
  \item{gene.id.type}{Type of gene ID provided in 'genes.to.plot'. Can be either "Symbol" or "GeneID"; defaults to "Symbol"}
  \item{covariates.to.create}{Datasets to be used to create covariates. If NULL, uses all datasets in data variable, otherwise specify a vector of dataset names (must be present in data variable)}
  \item{significance.threshold}{Sets q-value threshold of significance. Default is 0.05}
  \item{significant.colours}{Specifies covariate colour when significant; not significant is white while grey represents missing data. Defaults to BoutrosLab.plotting.general::default.colours(number.of.colours = length(covariates.to.create)}
}
\value{Returns a list containing initial data matrix with added covariate information (covariate.data) and arguments to create covariates (covariate.colours) and corresponding legend (covariate.legends)}
\author{Katie Houlahan}
\seealso{\code{\link[TCDD.Transcriptomics]{load.TCDD.results}}}
\examples{
# load in all available data using load.datasets.TCDD function
TCDD.data <- load.TCDD.results();

# get subset of genes to plot
core.genes <- c('Ahrr','Aldh3a1','Cyp1a1','Cyp1a2','Fmo1','Inmt','Nqo1','Tiparp','Nfe2l2');
analysis.results <- TCDD.data$results$mouse.ahrko.kidney.19h.1000;
rownames(analysis.results) <- analysis.results$Symbol;

# get covariate data
covariate <- create.significance.covariates(
	data.variable = TCDD.data, 
	genes.to.plot = core.genes,
	gene.id.type = 'Symbol',
	covariates.to.create = c(
		'mouse.C57.liver.24h.500.male',
		'mouse.C57.liver.144h.500.male',
		'mouse.C57.liver.24h.500.female',
		'mouse.C57.liver.144h.500.female'
		) 
	);

# create and store legends
cov.grob <- BoutrosLab.plotting.general::covariates.grob(
        covariates = covariate$covariate.colours,
        ord = c(length(core.genes):1),
        side = 'right'
        );

leg.grob <- BoutrosLab.plotting.general::legend.grob(
        legends = covariate$covariate.legends,
	layout = c(2,2)
        );

# indicate dot size/colour functions
spot.size.function <- function(x) { 1 + (0.5 * abs(x)); }
spot.colour.function <- function(x) {
        colours <- rep('white', length(x));
        colours[sign(x) == -1] <- BoutrosLab.plotting.general::default.colours(
									palette.type = 'dotmap'
									)[2];
        colours[sign(x) ==  1] <- BoutrosLab.plotting.general::default.colours(
									palette.type = 'dotmap'
									)[1];
        return(colours);
        }

BoutrosLab.plotting.general::create.dotmap(
	x = analysis.results[core.genes,c('M.AHR','M.TCDD','M.Interaction')],
	filename = 'dotmap_with_covariates.tiff',
	resolution = 500,
	bg.data = -log10(analysis.results[core.genes,
					c('q.value.AHR','q.value.TCDD','q.value.Interaction')
					]),
	colour.scheme = c('white','black'),
	bg.alpha = 1,
	key = NULL,
	legend = list(
		right = list(
			fun = cov.grob
			),
		top = list(
			fun = leg.grob
			)
		),
	yaxis.lab = analysis.results[core.genes,]$Symbol,
	xaxis.lab = c('AHR','TCDD','Interaction'),
	spot.size.function = spot.size.function,
	spot.colour.function = spot.colour.function,
	pch = 21,
	key.top = 1.5,
	right.padding = 2,
	yaxis.tck = c(0.5,0),
	xaxis.tck = c(0.5,0),
	height = 10,
	width = 9
	);
}
\keyword{IO}

