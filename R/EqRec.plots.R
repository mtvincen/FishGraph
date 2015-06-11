#' Equilibrium-recruitment plots
#' 
#' The function \code{EqRec.plots} generates plots of quantities (calculated by the 
#' user’s model) at equilibrium as a function of fishing mortality rate \emph{F}.
#' This function’s operation is almost identical to that of function \code{PerRec.plots}
#' . It exists to allow the analyst’s model to examine, in two potentially 
#' different resolutions, quantities that vary with \emph{F}.
#' 
#' @param x an R list with output from the assessment models.
#' @param DataName string used in plot titles.  Defaults to argument \code{x}.
#' @param draft modifies plots for use in a report.  When \code{FALSE} main titles 
#' are omitted.
#' @param graphics.type a vector of graphics file types to which graphics are saved.  
#' When \code{NULL}, no plots are saved.
#' @param use.color plots are made in grayscale when \code{FALSE}
#' @param units.L A character string (e.g. \code{"pounds"} for labeling the plot
#' of equilibrium landings.
#' @param units.SSB A character string for labeling the plot of equilibrium 
#' spawning biomass.
#' @param units.B A character string used in labeling the plot of equilibrium biomass.
#' @param units.D A character string used in labeling the plot of equilibrium discards.
#' @param units.Y A character string used in labeling the plot of equilibrium yield.
#' @param units.R A character string used in labeling the plot of equilibrium recruitment.
#' @param units.Eq A list whose elements are names of additional columns of 
#' \code{x$eq.series} to be plotted against \emph{F}.
#' 
#' 
#' @return Graphics
#' 
#' @author M. Prager
#' @author Erik H. Williams
#' @author Andi Stephens
#' @author Kyle W. Shertzer
#' 
#' @examples \donttest{
#' BSR.time.plots(gag)
#' }
#' 
EqRec.plots <-
function(x, DataName = deparse(substitute(x)), draft = TRUE,
    graphics.type = NULL, use.color = TRUE, units.L = x$info$units.landings[1],
    units.SSB = x$info$units.ssb[1], units.B = x$info$units.biomass[1],
    units.D = x$info$units.discards[1], units.Y = x$info$units.yield[1],
    units.R = x$info$units.rec[1],
    user.Eq = NULL)
{   ### Check for required data:
    if (! ("eq.series" %in% names(x)))
    {   Errmsg <- paste("Data frame 'eq.series' not found in data object:",
            deparse(substitute(x)))
        warning(Errmsg, immediate. = TRUE)
        return(invisible(-1))
    }
    eq.df <- x$eq.series

    # Set up plotting-related stuff:
    PlotTitle <- ""
    savepar <- FGSetPar(draft)
    if (! is.null(graphics.type))
    {   write.graphs <- TRUE
        GraphicsDirName <- paste(DataName, "-figs/EQ", sep="")
    }
    else
    {   write.graphs <- FALSE }

    ### Plot of Equilibrium Spawning Biomass vs. F:
    if ("SSB" %in% names(eq.df))
    {   if(draft) PlotTitle <- FGMakeTitle("EqSSB", DataName)
        FGTimePlot(x = eq.df$F.eq, y = eq.df$SSB, y2 = NULL,
            lab.x = "Fishing mortality rate",
            lab.y = FGMakeLabel("Equilibrium spawning biomass", units.SSB),
            FGtype = "linepointnodots",
            main = PlotTitle, use.color = use.color,
            ylim = c(0, max(eq.df$SSB, na.rm = TRUE)))
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "SSB.eq", graphics.type)
    }
    ### Plot of Equilibrium Biomass vs. F:
    if ("B" %in% names(eq.df))
    {   if(draft) PlotTitle <- FGMakeTitle("EqB", DataName)
        FGTimePlot(x = eq.df$F.eq, y = eq.df$B, y2 = NULL,
            lab.x = "Fishing mortality rate",
            lab.y = FGMakeLabel("Equilibrium stock biomass", units.B),
            FGtype = "linepointnodots",
            main = PlotTitle, use.color = use.color,
            ylim = c(0, max(eq.df$B, na.rm = TRUE)))
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "B.eq", graphics.type)
    }
    ### Plot of Equilibrium Recruitment vs. F:
    if ("R" %in% names(eq.df))
    {   if(draft) PlotTitle <- FGMakeTitle("EqRec", DataName)
        FGTimePlot(x = eq.df$F.eq, y = eq.df$R, y2 = NULL,
            lab.x = "Fishing mortality rate",
            lab.y = FGMakeLabel("Equilibrium recruitment", units.R),
            FGtype = "linepointnodots",
            main = PlotTitle, use.color = use.color,
            ylim = c(0, max(eq.df$R, na.rm = TRUE)))
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "R.eq", graphics.type)
    }
    ### Plot of Equilibrium Landings vs. F:
    if ("L" %in% names(eq.df))
    {   if(draft) PlotTitle <- FGMakeTitle("EqL", DataName)
        FGTimePlot(x = eq.df$F.eq, y = eq.df$L, y2 = NULL,
            lab.x = "Fishing mortality rate",
            lab.y = FGMakeLabel("Equilibrium landings", units.L),
            FGtype = "linepointnodots", main = PlotTitle,
            use.color = use.color,
            ylim = c(0, 1.2 * max(eq.df$L, na.rm = TRUE)))
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "L.eq", graphics.type)
    }
    ### Plot of Equilibrium Yield vs. F:
    if ("Y" %in% names(eq.df))
    {   if(draft) PlotTitle <- FGMakeTitle("EqY", DataName)
        FGTimePlot(x = eq.df$F.eq, y = eq.df$Y, y2 = NULL,
            lab.x = "Fishing mortality rate",
            lab.y = FGMakeLabel("Equilibrium yield", units.Y),
            FGtype = "linepointnodots", main = PlotTitle,
            use.color = use.color,
            ylim = c(0, 1.2 * max(eq.df$Y, na.rm = TRUE)))
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "Y.eq", graphics.type)
    }
    ### Plot of Equilibrium Discards vs. F:
    if ("D" %in% names(eq.df))
    {   if(draft) PlotTitle <- FGMakeTitle("EqD", DataName)
        FGTimePlot(x = eq.df$F.eq, y = eq.df$D, y2 = NULL,
            lab.x = "Fishing mortality rate",
            lab.y = FGMakeLabel("Equilibrium discards", units.D),
            FGtype = "linepointnodots", main = PlotTitle, use.color = use.color,
            ylim = c(0, 1.2 * max(eq.df$D, na.rm = TRUE)))
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "D.eq", graphics.type)
    }
    ### Plots of user per-recruit quantities:
    if (is.list(user.Eq))
    {   nplots = length(user.Eq)
        for (iplot in 1:nplots)
        {   eqname  <- user.Eq[[iplot]]
            if(draft) PlotTitle <- FGMakeTitle(eqname, DataName)
            # Get the index of the item in x$pr.series:
            eqindex <- which(names(eq.df) == eqname)
            FGTimePlot(x = eq.df$F.eq, y = eq.df[, eqindex], y2 = NULL,
                lab.x = "Fishing mortality rate", lab.y = eqname,
                FGtype = "linepointnodots", main = PlotTitle,
                use.color = use.color, ylim = c(0, 1.2 * max(eq.df[, eqindex],
                na.rm = TRUE)))
            if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                GraphName = paste("EQ.", eqname, sep = ""), graphics.type)
        }   # END for (iplot in 1:nplots)
    }       # END if (is.list ...)
#===============================================================================
   par(savepar)    # reset graphics device
   return(invisible(0))
} # end function definition

