# The TCDD.Transcriptomics package is copyright (c) 2013 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

create.significance.covariates <- function(data.variable, genes.to.plot = c(), gene.id.type = 'Symbol', covariates.to.create = NULL, significance.threshold = 0.05, significant.colours = NULL) {

	# make list of covariate to load 
	if (is.null(covariates.to.create)) {
		covariates.to.create <- names(data.variable$results);
		} 
	else {
		covariates.to.create <- covariates.to.create;
		}

        # ensure covariates are valid
        if (!all(covariates.to.create %in% names(data.variable$results))) {
                stop('Invalid covariate names');
	        }

	# make list of colours
	if (is.null(significant.colours)) {
		col <- BoutrosLab.plotting.general::default.colours(length(covariates.to.create));
		} 
	else {
		col <- significant.colours;
		}

        # initialize objects
        data <- matrix(nrow = length(genes.to.plot), ncol = length(covariates.to.create));
	colnames(data) <- covariates.to.create;
	rownames(data) <- genes.to.plot;
	colour <- list();
	legend <- list();
	
	# loop over datasets and create each covariate
	for (covariate in covariates.to.create) {
	
		# create temporary variables for dataset
		tmp <- data.variable$results[[covariate]];

		# create character vector of all genes from load function	
		all.genes <- tmp[,gene.id.type];
	
		# create vector of significant genes
		sig.genes <- tmp[tmp$q.value < significance.threshold,gene.id.type];

		# determine genes expressed in data subset that are also significant in each of the loaded datasets
		# and whether they were tested in those datasets  
		all.genes.tested <- genes.to.plot %in% all.genes;
		sig.genes.tested <- genes.to.plot %in% sig.genes;
		genes.covariate  <- all.genes.tested + sig.genes.tested;
		
		# where 0 = missing; 1 = not significant; 2 = significant
		# save final object
		data[,covariate] <- genes.covariate;

		}

	# fill in covariate data
	for (i in 1:length(covariates.to.create)) {
		 
		# Covariate colours: 
		# Significant	colour
		# Not Sig	white
		# Not Tested	grey
		covariate.colours <- data[,covariates.to.create[i]];
		covariate.colours[covariate.colours == 2] <- col[i];
		covariate.colours[covariate.colours == 1] <- 'white';
		covariate.colours[covariate.colours == 0] <- 'gray50';

		# create list containing arguments to make graphical covariate
		colour <- c(
			colour,
			list(
		        	rect = list(
		                	col = 'black',
		                	fill = covariate.colours,
		                	lwd = 1.5
		                	)
				)
			);
		
		# create list containing arguments to make covariate legend
		legend <- c(
			legend,
			list(
	        		legend = list(
	                		colours = c(col[i], 'white', 'gray50'),
	                		labels = c(
						paste(
							covariates.to.create[i],
							'Yes',
							sep = ':'
							),
						paste(
							covariates.to.create[i],
							'No',
							sep = ':'
							),
						'Missing from Database'
						)
	               			)
				)
			);
		}
	
	# return final lists
	return(
		list(
			covariate.data = data,
			covariate.colours = colour,
			covariate.legends = legend
			)
		);
	}
