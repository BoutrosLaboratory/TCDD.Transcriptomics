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

load.TCDD.results <- function(datasets.to.load = 'all', verbose = FALSE) {

	# make a list of potential locations for the datasets file
        data.directory = './';
	data.directories <- paste(.libPaths(), '/TCDD.Transcriptomics/datasets/', sep = '');
        data.directories <- c(data.directory, data.directories);

        # then search all locations
	datasets.file <- 'datasets.txt';
        file.checks <- file.exists( paste(data.directories, datasets.file, sep = '/') );

        # check to see if the file was actually found
        if (any(file.checks)) {
                data.directory <- data.directories[ order(file.checks, decreasing = TRUE)[1] ];
                } else {
                stop("Unable to find database file");
                }

        # read in the listing of all datasets
        datasets <- read.delim(
                file = paste(data.directory, datasets.file, sep = '/'),
                header = TRUE,
                sep = "\t",
                row.names = NULL,
                as.is = TRUE
                );

        # loop through requested datasets
        # handle the user requesting all datasets
        if ('all' %in% datasets.to.load) { datasets.to.load <- datasets$Dataset; }

        # make sure all requested datasets exist
        if (!all(datasets.to.load %in% datasets$Dataset)) { stop('Non-existant dataset requested'); }

        # initialize data object
        results <- list();

	# load all requested datasets one-by-one
        for (dataset in datasets.to.load) {

                # give the user some status info if requested
                if (verbose) { cat('\nreading annotations for dataset: ', dataset, '\n'); }

                # set up the path to the dataset's files
                dataset.directory <- paste(data.directory, dataset, '/', sep = '');

                results.data <- read.delim(
                        paste(
                                dataset.directory,
                                datasets$Results.file[datasets$Dataset == dataset],
                                sep = ''
                                ),
                        header = TRUE,
                        sep = '\t'
                        );

                # save final objects
                results[[dataset]] <- results.data;
                }

        return(
                list(
                        results = results
                        )
                );
        }

