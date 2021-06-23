<!-- README.md is generated from README.Rmd. Please edit that file -->

FishGraph 2.1
=============

FishGraph is a set of `R` functions that generates diagnostics and data
visualizations from stock assessment model output. These functions are
designed to process model output from the **Beaufort Assessment Model
(BAM)**. Further information on the details of the *BAM* can be found in
([Williams and Shertzer
2015](http://docs.lib.noaa.gov/noaa_documents/NMFS/SEFSC/TM_NMFS_SEFSC/NMFS_SEFSC_TM_671.pdf))

Installation
------------

The package can be installed using the `install_github()` function from
the `devtools` library.

    # To install the devtools package
    install.packages("devtools")

    # Load library and install directly from the repo.
    library(devtools)
    devtools::install_github("rcheshire/FishGraph")

Alternatively, the package can be installed locally from the repository
source files.

    install.packages("C://Location of files//FishGraph_2.1.tar.gz, 
                     repos = NULL, type="source")

Application
-----------

Once installed the package offers a series of functions for model
diagnostics and evaluations. A full list of available functions can be
obtained using:

    library(FishGraph)
    ls(pos = "package:FishGraph")

`FishGraph 2.1` includes updates to several `FishGraph 2.0` functions.  
\* Comp.plots() + combined annual compositions now weighted by effective
sample size (neff). If effective sample size object is not present then
samples size (n) is used for weighting annual compositions. + a new
option was added to create boxplots of residualsby length/age and year,
the default is b.plot = FALSE + added residual type to y-axis label \*
Comp.plots.yearly() + background shading for years where effective
sample size negative (turned off) \* StockRec.plots() + option added for
no spawner-recruit relationship, rec.model=“mean” \* Index.plots() +
diagnostic plots now default to subset of previous diagnostics, default
is resid.tests=“runs”, “multiple” gives all diagnostics

`FishGraph 2.0` includes several updated functions. Additionally,
version 2.0 includes four new functions: `Cohort.plots`, `Phase.plots`,
`Parm.plots`, and `Bound.vec.plots`. All functions can be tested using
the included data set `data(gag)` which is distributed with the package.

A comprehensive description of all included functions and examples of
their graphical output is available in [**NOAA Technical Memorandum
NMFS-SEFSC-684**](https://repository.library.noaa.gov/view/noaa/5248)

Notice of nonendorsement
------------------------

This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.
