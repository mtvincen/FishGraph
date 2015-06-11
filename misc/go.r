# An example script for displaying the uses of the FishGraph package.
# 2015-03-26
# KM Purcell

library(FishGraph)

# The package includes an example data set capable of executing all
# included function.  The data is set to load automatically when the package
# is loaded.  But the structure of the example data can be examined via:
str(gag)

# Of course new data sets will have to be extracted from the .RData output 
# from BAM to a data list object.
#
# spp  <- dget("Link to RData object")


# Set a working directory
# setwd("C:/Users/Kevin.Purcell/Desktop")


# Open a graphics device & delete plot history 
graphics.off()
.SavedPlots <- NULL

# A windows() command must be used based on original savePlot() programming.
windows(width = 8, height = 10, record = TRUE, xpos = 10, ypos = 10)

# set up default colors, etc.
FGSetDefaults()             


# Call the functions

# Diagnostics plots:
Comp.plots(gag, graphics.type = "png", draft = TRUE)

Comp.yearly.plots(gag, graphics.type = "png", compact = FALSE)

BSR.time.plots(gag, start.drop = 4, graphics.type = "png")

Index.plots(gag, DataName = "gag", graphics.type = "png", log.resid = FALSE)

# Data plots:

CLD.total.plots(gag, graphics.type = c("png", "png"), first.year = 1960,
    plot.proportion = TRUE)

Landings.plots(gag, graphics.type = "png")

Growth.plots(gag, graphics.type = "png", plot.all = TRUE)

# Results plots:

F.time.plots(gag, graphics.type = c("png", "png"), start.drop=23, F.references =
   list(a = "F01", b = c("F30", "F40", "F50", "F60")))

PerRec.plots(gag, graphics.type = "png", use.color = TRUE,
    user.PR = list("spr.biomass", "E.spr"))

EqRec.plots(gag, graphics.type = "png", use.color = TRUE,
    user.Eq = list("spr", "E.eq"))

Selectivity.plots(gag, graphics.type = "png")

StockRec.plots(gag, graphics.type = "png", draw.lowess = TRUE, use.color = TRUE,
    start.drop = 17, rec.model = NULL)


##### Uncomment the following lines to open a second graphics window and illustrate
##### the "compact" option of Comp.yearly.plots().
windows(width = 7.3, height = 10, record = TRUE)
Comp.yearly.plots(gag, graphics.type = "png", compact = TRUE, print.angle=TRUE)

##### Save the data oject into an ASCII file (for spreadsheet users, etc.)
rdat2ascii(gag)

