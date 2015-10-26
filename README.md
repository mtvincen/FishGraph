# FishGraph 2.0 

FishGraph is a set of `R` functions that generates diagnostics and data visualizations from stock assessment model output.  These functions are designed to process model output from the **Beaufort Assessment Model (BAM)**.  Further information on the details of the *BAM* can be found in ([Williams and Shertzer 2015](http://docs.lib.noaa.gov/noaa_documents/NMFS/SEFSC/TM_NMFS_SEFSC/NMFS_SEFSC_TM_671.pdf))



## Installation

The package can be installed using the `install_github()` function from the `devtools` library.  

```
# To install the devtools package
install.packages("devtools")

# Load library and install directly from the repo.
library(devtools)
install_github("rcheshire/FishGraph")
```

Alternatively, the package can be installed locally from the repository source files.

```
install.packages("C://Location of files//FishGraph_2.0.tar.gz, 
                 repos = NULL, type="source")
```

## Application

Once installed the package offers a series of functions for model diagnostics and evaluations.  A full list of available functions can be obtained using:

```
library(FishGraph)
ls(pos = "package:FishGraph")
```
For previous users, `FishGraph 2.0` includes two updated functions: `Comp.plots()` and `Index.plots` legacy functions are available using the suffix `.archive()`.  Additionally, version 2.0 includes two new functions: `Comp.plots.color.choort` and `Parm.bounds.plots`.  All functions can be tested using the included data set `data(gag)` which is distributed with the package.  

A comprehensive description of all included functions and examples of their graphical output is available in [**NOAA Technical Memorandum NMFS-SEFSC-XXXXX**](http://www.link2file.com).



