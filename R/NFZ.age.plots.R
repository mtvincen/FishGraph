#' At-age matrix plots
#'
#' The function \code{NFZ.age.plots} generates barplots of estimated abundance and
#' mortality at age over time and bubble plots of number and biomass at age.  Bubble
#' areas are scaled to the largest value within each plot.
#'
#' @param x an R list with output from the assessment models.
#' @param DataName string used in plot titles.  Defaults to argument \code{x}.
#' @param draft modifies plots for use in a report.  When \code{FALSE} main titles
#' are omitted.
#' @param start.drop Number of years at the start of the data to be omitted from
#' plots, as when a model includes an initialization period.
#' @param graphics.type a vector of graphics file types to which graphics are saved.
#' When \code{NULL}, no plots are saved.
#' @param use.color plots are made in grayscale when \code{FALSE}
#' @param units.naa A text string (e.g. \code{"million fish"}) used in labeling
#' the plot of numbers at age.
#' @param units.biomass A text string (e.g. \code{"t"}) used in labeling the plot
#' of biomass at age.
#' @param max.bub A numerical scalar for the maximum bubble size (e.g. \code{"4.0"})
#' in bubble plots of numbers at age and biomass at age.
#' @param user.plots A vector of text strings with the names of additional matrix
#' components of \code{x} to be plotted.
#' @param plot.CLD When \code{TRUE}, each matrix in \code{x$CLD.est.mats} is plotted,
#' in addition to the plots described here.
#' @param drop.last.NB boolean for whether to drop the last year of the abundance and biomass at age, when \code{TRUE} doesn't plot the last year of abundance, default \code{FALSE}.
#'
#' @return Graphics
#'
#' @author M Prager
#' @author E Williams
#' @author K Shertzer
#' @author R Cheshire
#' @author K Purcell
#'
#' @examples \donttest{
#' NFZ.age.plots(gag)
#' }
#' @export
NFZ.age.plots <-
function(x, DataName = deparse(substitute(x)), draft = TRUE,
   start.drop = 0, graphics.type = NULL, use.color = TRUE,
   units.naa = x$info$units.naa, units.biomass = x$info$units.biomass,
   units.ssb = x$info$units.ssb,  max.bub = 4.0,user.plots = NULL, plot.CLD = FALSE,
   drop.last.NB=FALSE)
########################################################################################
{   ### Set up plotting-related stuff:
    savepar <- FGSetPar(draft)

    PlotTitle <- ""
    if (! is.null(graphics.type))
    {   write.graphs <- TRUE
        GraphicsDirName <- paste(DataName, "-figs/NFZ", sep="")
    }
    else
    {   write.graphs <- FALSE
    }
#---------------------------------------------------------------------------------------
#  End of internal function definitions.  Now make plots.
#---------------------------------------------------------------------------------------
    ### PLOTS OF NUMBER AT AGE
    if ("N.age" %in% names(x))
    {   if (start.drop == 0 ){ dataset <- x$N.age
        } else dataset <- x$N.age[-(1:start.drop),]
        if (drop.last.NB) dataset = head(dataset,-1)
        # Plot of N (unnormalized)
        if (draft) PlotTitle <- FGMakeTitle("N at age", DataName)
        lab.y = FGMakeLabel("Numbers", units.naa)
        FGBarplot(dataset, lab.y = lab.y, use.color, PlotTitle, leg.title = "Age")
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "N.age", graphics.type)
        # Plot of N (normalized)
        if (draft) PlotTitle <- FGMakeTitle("Proportion N at age", DataName)
        FGBarplot(dataset, lab.y = "Proportion of N", use.color, PlotTitle,
            leg.title = "Age", proportion = TRUE)
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "N.age.prop", graphics.type)
    }
#---------------------------------------------------------------------------------------
    ### PLOTS OF BIOMASS AT AGE
    if ("B.age" %in% names(x))
    {   if (start.drop == 0 ) { dataset <- x$B.age
        } else dataset <- x$B.age[-(1:start.drop),]
        if (drop.last.NB) dataset = head(dataset,-1)
        # Plot of B (unnormalized)
        if (draft) PlotTitle <- FGMakeTitle("Biomass at age", DataName)
        lab.y <- FGMakeLabel("Biomass", units.biomass)
        FGBarplot(dataset, lab.y= lab.y, use.color, PlotTitle, leg.title = "Age")
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "B.age", graphics.type)
        # Plot of B (normalized)
        if (draft) PlotTitle <- FGMakeTitle("Proportion B at age", DataName)
        FGBarplot(dataset, lab.y = "Proportion of B", use.color, PlotTitle,
            leg.title = "Age", proportion = TRUE)
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "B.age.prop", graphics.type)
    }
#---------------------------------------------------------------------------------------
    ### PLOTS OF F AT AGE:
    if ("F.age" %in% names(x))
    {   if (start.drop == 0 ) {dataset <- x$F.age
        } else dataset <- x$F.age[-(1:start.drop),]
#         # Plot of F (unnormalized)
#         if (draft) PlotTitle <- FGMakeTitle("Cumulative F", DataName)
#         FGBarplot(dataset, lab.y = "Cumulative F at age", use.color, PlotTitle,
#             leg.title = "Age")
#         if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
#             GraphName = "F.age", graphics.type)
        # Plot of F (normalized)
        if (draft) PlotTitle <- FGMakeTitle("Proportion F at age", DataName)
        FGBarplot(dataset, lab.y = "Proportion of F", use.color, PlotTitle,
            leg.title = "Age", proportion = TRUE)
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "F.age.prop", graphics.type)
    }
#---------------------------------------------------------------------------------------
    ### PLOTS OF Z AT AGE:
    if ("Z.age" %in% names(x))
    {   if (start.drop == 0 ){ dataset <- x$Z.age
        } else dataset <- x$Z.age[-(1:start.drop),]
#         # Plot of Z (unnormalized)
#         if (draft) PlotTitle <- FGMakeTitle("Cumulative Z", DataName)
#         FGBarplot(dataset, lab.y = "Cumulative Z at age", use.color, PlotTitle,
#             leg.title = "Age")
#         if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
#             GraphName = "Z.age", graphics.type)
        # Plot of Z (normalized)
        if (draft) PlotTitle <- FGMakeTitle("Proportion Z at age", DataName)
        FGBarplot(dataset, lab.y = "Proportion of Z", use.color, PlotTitle,
            leg.title = "Age", proportion = TRUE)
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "Z.age.prop", graphics.type)
    }
##---------------------------------------------------------------------------------------
    ### PLOTS OF NUMBER  Spawners AT AGE
    if ("N.age.spawn" %in% names(x))
    {   if (start.drop == 0 ){ dataset <- x$N.age.spawn
        } else dataset <- x$N.age.spawn[-(1:start.drop),]
        if (drop.last.NB) dataset = head(dataset,-1)
        # Plot of N (unnormalized)
        if (draft) PlotTitle <- FGMakeTitle("N spawners at age", DataName)
        lab.y = FGMakeLabel("Numbers", units.naa)
        FGBarplot(dataset, lab.y = lab.y, use.color, PlotTitle, leg.title = "Age")
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "N.age.spawners", graphics.type)
        # Plot of N (normalized)
        if (draft) PlotTitle <- FGMakeTitle("Proportion N spawners at age", DataName)
        FGBarplot(dataset, lab.y = "Proportion of N spawners", use.color, PlotTitle,
            leg.title = "Age", proportion = TRUE)
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "N.age.spawners.prop", graphics.type)
    }
##---------------------------------------------------------------------------------------
    ### PLOTS OF SSB AT AGE
    if ("SSB.age" %in% names(x))
    {   if (start.drop == 0 ){ dataset <- x$SSB.age
        } else dataset <- x$SSB.age[-(1:start.drop),]
        if (drop.last.NB) dataset = head(dataset,-1)
        # Plot of N (unnormalized)
        if (draft) PlotTitle <- FGMakeTitle("N spawners at age", DataName)
        lab.y = FGMakeLabel("SSB", units.ssb)
        FGBarplot(dataset, lab.y = lab.y, use.color, PlotTitle, leg.title = "Age")
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "SSB.age", graphics.type)
        # Plot of N (normalized)
        if (draft) PlotTitle <- FGMakeTitle("Proportion SSB at age", DataName)
        FGBarplot(dataset, lab.y = "Proportion of SSB", use.color, PlotTitle,
            leg.title = "Age", proportion = TRUE)
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "SSB.prop", graphics.type)
    }
#---------------------------------------------------------------------------------------
#  Plots of user quantities
#---------------------------------------------------------------------------------------
    if (! is.null(user.plots))
    {   for (datname in user.plots)
        {   if (datname %in% names(x))
            {   dataset <- x[[datname]]
                if (start.drop > 0) dataset <- dataset[-(1:start.drop),]
                if (draft) PlotTitle <- FGMakeTitle(datname, DataName)
                FGBarplot(dataset, lab.y = paste(datname, "at age"),
                    use.color, PlotTitle, leg.title = "Age")
                if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                    GraphName = paste("usr.", datname, sep = ""), graphics.type)
                FGBarplot(dataset, lab.y = paste("Proportion of", datname),
                    use.color, PlotTitle, leg.title = "Age", proportion = TRUE)
                if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                    GraphName = paste("usr", datname, "prop", sep = "."),
                    graphics.type)
            }
        }
    }
#---------------------------------------------------------------------------------------
#  Plots of CLD.est.mats
#---------------------------------------------------------------------------------------
    if (plot.CLD)
    {   for (datname in names(x$CLD.est.mats))
        {   dataset <- x$CLD.est.mats[[datname]]
            if (start.drop > 0 && start.drop < nrow(dataset))
                dataset <- dataset[-(1:start.drop),]
            if (draft) PlotTitle <- FGMakeTitle(datname, DataName)
            FGBarplot(dataset, lab.y = paste(datname, "at age"),
                use.color, PlotTitle, leg.title = "Age")
            if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                GraphName = paste("byage.CLD", datname, sep = "."), graphics.type)
            FGBarplot(dataset, lab.y = paste("Proportion of", datname),
                use.color, PlotTitle, leg.title = "Age", proportion = TRUE)
            if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                GraphName = paste("byage.CLD", datname,"prop", sep = "."), graphics.type)
        }
    }
#---------------------------------------------------------------------------------------
# BUBBLE PLOTS OF NUMBER AT AGE
#---------------------------------------------------------------------------------------
par(savepar)
#par(mfrow=c(1,1))
plot.options = FGGetOptions()
par <- FGSetPar(draft)

if ("N.age" %in% names(x))
{   if (start.drop == 0 ){ dataset <- x$N.age
    } else dataset <- x$N.age[-(1:start.drop),]
    if (drop.last.NB) dataset = head(dataset,-1)
    if (draft) PlotTitle <- FGMakeTitle("N at age", DataName)
irn <- as.integer(rownames(dataset))                        # year names
x1 <- as.integer(rep(irn, ncol(dataset)))                   # year names
y1 <- sort(rep(as.numeric(colnames(dataset)), nrow(dataset)))    # age- or length-class names

### Get size of the bubbles:
wt.m.age=sweep(dataset,MARGIN=2,as.numeric(colnames(dataset)),"*")
m.age=rowSums(wt.m.age)/rowSums(dataset)
bub.size <- max.bub*(sqrt(abs(dataset))/sqrt(max(abs(dataset))))  ###plots area of bubble
# This prevents plotting fractional years (e.g. 1995.5)
xmin = irn[1]-1
xmax = max(max(irn), xmin + 4)
ymin=min(y1)
ymax=max(y1)
#par(cex = 1, cex.main = 1, cex.axis = 0.85)

#### Draw the main (bubble) plot:
if (draft) PlotTitle <- FGMakeTitle("N at age", DataName)
ifelse (use.color, bubble.col <- plot.options$color$clr.line, bubble.col <- plot.options$bw$clr.pred)
lab.y = "Age"

plot(x1, y1, xlab = "Year", ylab = lab.y, main=PlotTitle, type = "n", las = 1,
     xlim = c(xmin, xmax),ylim=c(ymin,ymax))
grid(col = "lightgray", lty = 1)
points(x1, y1, cex = bub.size, col = bubble.col,  pch = 21)
lines(as.numeric(rownames(dataset)),m.age,lwd=2,lty=1)
if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                             GraphName = "N.age.bubble", graphics.type)
}
#---------------------------------------------------------------------------------------
### BUBBLE PLOTS OF BIOMASS AT AGE
#---------------------------------------------------------------------------------------

if ("B.age" %in% names(x))
{   if (start.drop == 0 ){ dataset <- x$B.age
} else dataset <- x$B.age[-(1:start.drop),]
if (drop.last.NB) dataset = head(dataset,-1)
if (draft) PlotTitle <- FGMakeTitle("B at age", DataName)
irn <- as.integer(rownames(dataset))                        # year names
x1 <- as.integer(rep(irn, ncol(dataset)))                   # year names repeated to fill matrix
y1 <- sort(rep(as.numeric(colnames(dataset)), nrow(dataset)))    # age- or length-class names

### Get size of the bubbles:
wt.m.age=sweep(dataset,MARGIN=2,as.numeric(colnames(dataset)),"*")
m.age=rowSums(wt.m.age)/rowSums(dataset)
bub.size <- max.bub*(sqrt(abs(dataset))/sqrt(max(abs(dataset))))  ###plots area of bubble
# This prevents plotting fractional years (e.g. 1995.5)
xmin = irn[1]-1
xmax = max(max(irn), xmin + 4)
#par(cex = 1, cex.main = 1, cex.axis = 0.85)

#### Draw the main (bubble) plot:
if (draft) PlotTitle <- FGMakeTitle("B at age", DataName)
ifelse (use.color, bubble.col <- plot.options$color$clr.line, bubble.col <- plot.options$bw$clr.pred)
lab.y = "Age"

plot(x1, y1, xlab = "Year", ylab = lab.y, main=PlotTitle, type = "n", las = 1,
     xlim = c(xmin, xmax))
grid(col = "lightgray", lty = 1)
points(x1, y1, cex = bub.size, col = bubble.col,  pch = 21)
lines(as.numeric(rownames(dataset)),m.age,lwd=2,lty=1)
if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                             GraphName = "B.age.bubble", graphics.type)
}

#---------------------------------------------------------------------------------------
### BUBBLE PLOTS OF SSB AT AGE
#---------------------------------------------------------------------------------------

if ("SSB.age" %in% names(x))
{   if (start.drop == 0 ){ dataset <- x$SSB.age
} else dataset <- x$SSB.age[-(1:start.drop),]
if (drop.last.NB) dataset = head(dataset,-1)
if (draft) PlotTitle <- FGMakeTitle("SSB at age", DataName)
irn <- as.integer(rownames(dataset))                        # year names
x1 <- as.integer(rep(irn, ncol(dataset)))                   # year names repeated to fill matrix
y1 <- sort(rep(as.numeric(colnames(dataset)), nrow(dataset)))    # age- or length-class names

### Get size of the bubbles:
wt.m.age=sweep(dataset,MARGIN=2,as.numeric(colnames(dataset)),"*")
m.age=rowSums(wt.m.age)/rowSums(dataset)
bub.size <- max.bub*(sqrt(abs(dataset))/sqrt(max(abs(dataset))))  ###plots area of bubble
# This prevents plotting fractional years (e.g. 1995.5)
xmin = irn[1]-1
xmax = max(max(irn), xmin + 4)
#par(cex = 1, cex.main = 1, cex.axis = 0.85)

#### Draw the main (bubble) plot:
if (draft) PlotTitle <- FGMakeTitle("SSB at age", DataName)
ifelse (use.color, bubble.col <- plot.options$color$clr.line, bubble.col <- plot.options$bw$clr.pred)
lab.y = "Age"

plot(x1, y1, xlab = "Year", ylab = lab.y, main=PlotTitle, type = "n", las = 1,
     xlim = c(xmin, xmax))
grid(col = "lightgray", lty = 1)
points(x1, y1, cex = bub.size, col = bubble.col,  pch = 21)
lines(as.numeric(rownames(dataset)),m.age,lwd=2,lty=1)
if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                             GraphName = "SSB.age.bubble", graphics.type)
}


#---------------------------------------------------------------------------------------
### CLD BUBBLE PLOTS
#---------------------------------------------------------------------------------------

if (plot.CLD)
{ # Get total landings and discards at age:
  total.cld.index <-grep("total", names(x$CLD.est.mats))
  for (cldi in 1:length(total.cld.index))
{   datname=names(x$CLD.est.mats)[total.cld.index[cldi]]
    dataset <- x$CLD.est.mats[[datname]]
if (start.drop > 0 && start.drop < nrow(dataset))
  dataset <- dataset[-(1:start.drop),]
if (draft) PlotTitle <- FGMakeTitle(datname, DataName)
    irn <- as.integer(rownames(dataset))                        # year names
    x1 <- as.integer(rep(irn, ncol(dataset)))                   # year names repeated to fill matrix
    y1 <- sort(rep(as.numeric(colnames(dataset)), nrow(dataset)))    # age- or length-class names

    ### Get size of the bubbles:
    wt.m.age=sweep(dataset,MARGIN=2,as.numeric(colnames(dataset)),"*")
    m.age=rowSums(wt.m.age)/rowSums(dataset)
    bub.size <- max.bub*(sqrt(abs(dataset))/sqrt(max(abs(dataset))))  ###plots area of bubble
    # This prevents plotting fractional years (e.g. 1995.5)
    xmin = irn[1]-1
    xmax = max(max(irn), xmin + 4)
    #par(cex = 1, cex.main = 1, cex.axis = 0.85)

    #### Draw the main (bubble) plot:
    if (draft) PlotTitle <- FGMakeTitle(datname, DataName)
    ifelse (use.color, bubble.col <- plot.options$color$clr.line, bubble.col <- plot.options$bw$clr.pred)
    lab.y = "Age"

    plot(x1, y1, xlab = "Year", ylab = lab.y, main=PlotTitle, type = "n", las = 1,
         xlim = c(xmin, xmax))
    grid(col = "lightgray", lty = 1)
    points(x1, y1, cex = bub.size, col = bubble.col,  pch = 21)
    lines(as.numeric(rownames(dataset)),m.age,lwd=2,lty=1)
  }
    if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                                 GraphName = paste("byage.CLD", datname,"bub", sep = "."), graphics.type='png')

  }
# Clean up and return
#---------------------------------------------------------------------------------------
par(savepar)    # reset graphics device
return(invisible(par(savepar)))
}   # end of function
