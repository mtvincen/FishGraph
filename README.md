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

A full description of all included functions and examples of their graphical output is available in [**NOAA Technical Memorandum NMFS-SEFSC-XXXXX**](http://www.link2file.com).



