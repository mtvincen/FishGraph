#' Per-recruit plots
#'
#' The function \code{PerRec.plots} generates plots of quantities
#' on a per-recruit basis as a function of fishing mortality
#' rate \emph{F}.
#'
#' @param x an R list with output from the assessment models.
#' @param DataName string used in plot titles.  Defaults to argument \code{x}.
#' @param draft modifies plots for use in a report.  When \code{FALSE} main titles
#' are omitted.
#' @param graphics.type a vector of graphics file types to which graphics are saved.
#' When \code{NULL}, no plots are saved.
#' @param use.color plots are made in grayscale when \code{FALSE}
#' @param units.ypr A character string (e.g. \code{"pounds"}) used in labeling
#' the plot of yield per recruit.
#' @param user.PR A list whose elements are names of additional columns of
#' \code{x$pr.series} to be plotted against \emph{F}.
#' @param legend.pos A text string compatible with the \code{legend} function of \code{R}.
#' Defines the position of the legend (ex. "bottomright", "bottom", etc.)
#' @param F.references A list of character-string vectors to specify reference
#' points to appear on \emph{F} plots.

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
#' PerRec.plots(gag)
#' }
#' @export
PerRec.plots <-
function(x, DataName = deparse(substitute(x)), draft = TRUE,
    graphics.type = NULL, use.color = TRUE, units.ypr = x$info$units.ypr,
    user.PR = NULL, legend.pos = "topright", F.references = NULL)
{   ### Check for required data:
    if (! ("pr.series" %in% names(x)))
    {   Errmsg <- paste("Data frame 'pr.series' not found in data object:",
            deparse(substitute(x)))
        warning(Errmsg, immediate. = TRUE)
        return(invisible(-1))
    }
    pr.df <- x$pr.series

    # Set up plotting-related stuff:
    ltyvec <- c("dashed", "dotdash", "twodash", "431313", "dotted", "22848222")

    vref<-vrefindex <-vrefnames <- NULL
    if (is.list(F.references))
    { ref.check=F.references %in% names(x$parms)
      if (!all(ref.check)){warning("Missing F.reference in parms", immediate.=TRUE); return(invisible(-1))}
      nrefs = length(F.references)
       for (iplot in 1:nrefs)
       {
          vrefnames <- c(vrefnames,F.references[[iplot]])
          vrefindex <- c(vrefindex, which(names(x$parms) == F.references[[iplot]]))
       }
      vref <- unlist(x$parms[vrefindex])
    }
    xmax <- max(pr.df$F.spr, 0.01, vref, na.rm = TRUE)


    PlotTitle <- ""
    savepar <- FGSetPar(draft)
    if (! is.null(graphics.type))
    {   write.graphs <- TRUE
        GraphicsDirName <- paste(DataName, "-figs/PR", sep="")
    }
    else
    {   write.graphs <- FALSE }

    ### Plot of Spawning potential ratio (%SPR) vs. F:
    if ("spr.prop" %in% names(pr.df))
    {   if(draft) PlotTitle <- FGMakeTitle("SPR", DataName)
        FGTimePlot(x = pr.df$F.spr, y = pr.df$spr.prop, y2 = NULL,
            lab.x = "Fishing mortality rate",
            lab.y = expression(plain("Spawning potential ratio")~~Psi),
            FGtype = "linepointnodots",
            main = PlotTitle, use.color = use.color,
            ylim = c(0, max(pr.df$spr.prop)), xlim=c(0,xmax))
        if (is.list(F.references))
        {
          abline(v=vref, lty=ltyvec, lwd=2)
          legend(legend.pos, legend=vrefnames, lty=ltyvec, lwd=2, bg="white")
        }
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "PR.spr", graphics.type)
    }
    ### Plot of YPR vs. F:
    if ("ypr" %in% names(pr.df))
    {   if(draft) PlotTitle <- FGMakeTitle("YPR", DataName)
        FGTimePlot(x = pr.df$F.spr, y = pr.df$ypr, y2 = NULL,
            lab.x = "Fishing mortality rate",
            lab.y = FGMakeLabel("Yield per recruit", units.ypr),
            FGtype = "linepointnodots", main = PlotTitle, use.color = use.color,
            ylim = c(0, 1.2 * max(pr.df$ypr, na.rm = TRUE)), xlim=c(0,xmax))
        if (is.list(F.references))
        {
          abline(v=vref, lty=ltyvec, lwd=2)
          legend(legend.pos, legend=vrefnames, lty=ltyvec, lwd=2, bg="white")
        }
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "PR.ypr", graphics.type)
    }
    ### Plots of user per-recruit quantities:
    if (is.list(user.PR))
    {   nplots = length(user.PR)
        for (iplot in 1:nplots)
        {   prname  <- user.PR[[iplot]]
            if(draft) PlotTitle <- FGMakeTitle(prname, DataName)
            # Get the index of the item in x$pr.series:
            prindex <- which(names(pr.df) == prname)
            FGTimePlot(x = pr.df$F.spr, y = pr.df[, prindex], y2 = NULL,
                lab.x = "Fishing mortality rate", lab.y = prname,
                FGtype = "linepointnodots", main = PlotTitle,
                use.color = use.color, ylim = c(0, 1.2 * max(pr.df[, prindex],
                na.rm = TRUE)), xlim=c(0,xmax))
            if (is.list(F.references))
            {
              abline(v=vref, lty=ltyvec, lwd=2)
              legend(legend.pos, legend=vrefnames, lty=ltyvec, lwd=2, bg="white")
            }
            if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                GraphName = paste("PR.", prname, sep=""), graphics.type)
        }   # END for (iplot in 1:nplots)
    }       # END if (is.list ...)
#==================================================================================
   par(savepar)    # reset graphics device
   return(invisible(0))
} # end function definition

