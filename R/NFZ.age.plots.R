#' At-age matrix plots
#' 
#' The function \code{NFZ.age.plots} generates barplots of estimated abundance and 
#' mortality at age over time.
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
#' @param user.plots A vector of text strings with the names of additional matrix 
#' components of \code{x} to be plotted.
#' @param plot.CLD When \code{TRUE}, each matrix in \code{x$CLD.est.mats} is plotted,
#' in addition to the plots described here.
#' 
#' @return Graphics
#' 
#' @author M. Prager
#' @author Erik H. Williams
#' @author Andi Stephens
#' @author Kyle W. Shertzer
#' 
#' @examples \donttest{
#' NFZ.age.plots(gag)
#' }
#' 
NFZ.age.plots <-
function(x, DataName = deparse(substitute(x)), draft = TRUE,
   start.drop = 0, graphics.type = NULL, use.color = TRUE,
   units.naa = x$info$units.naa, units.biomass = x$info$units.biomass,
   user.plots = NULL, plot.CLD = FALSE)
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
    {   if (start.drop == 0 ) dataset <- x$N.age
        else dataset <- x$N.age[-(1:start.drop),]
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
    {   if (start.drop == 0 ) dataset <- x$B.age
        else dataset <- x$B.age[-(1:start.drop),]
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
    {   if (start.drop == 0 ) dataset <- x$F.age
        else dataset <- x$F.age[-(1:start.drop),]
        # Plot of F (unnormalized)
        if (draft) PlotTitle <- FGMakeTitle("Cumulative F", DataName)
        FGBarplot(dataset, lab.y = "Cumulative F at age", use.color, PlotTitle,
            leg.title = "Age")
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "F.age", graphics.type)
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
    {   if (start.drop == 0 ) dataset <- x$Z.age
        else dataset <- x$Z.age[-(1:start.drop),]
        # Plot of Z (unnormalized)
        if (draft) PlotTitle <- FGMakeTitle("Cumulative Z", DataName)
        FGBarplot(dataset, lab.y = "Cumulative Z at age", use.color, PlotTitle,
            leg.title = "Age")
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "Z.age", graphics.type)
        # Plot of Z (normalized)
        if (draft) PlotTitle <- FGMakeTitle("Proportion Z at age", DataName)
        FGBarplot(dataset, lab.y = "Proportion of Z", use.color, PlotTitle,
            leg.title = "Age", proportion = TRUE)
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "Z.age.prop", graphics.type)
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
# Clean up and return
#---------------------------------------------------------------------------------------
    par(savepar)
}   # end of function

