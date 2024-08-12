#' Biomass, spawner, and recruitmenet trajectories
#'
#' The function \code{BSR.time.plots} generates time-series plots of stock biomass,
#' spawning stock (as biomass or egg production), and recruitment. Plots are made
#' on absolute scales and also scaled to reference points. Plots may include
#' reference lines at user-specified reference points.
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
#' @param units.b A text string (e.g. \code{"tons"} for labeling plots of stock biomass.
#' @param units.ssb A text string for labeling plots of spawning-stock biomass.
#' @param units.r a text string (e.g. \code{"million fish"}) for labeling plots of recruitment
#' @param legend.pos A text string compatible with the \code{legend} function of \code{R}.
#' Defines the position of the legend (ex. "bottomright", "bottom", etc.)
#' @param from.zero When \code{TRUE}, the Y-axis of each plot
#' (except recruitment deviations) starts at zero.
#' @param BSR.references A list of three character-string names to specify reference
#' points to appear on BSR plots, in order of biomass, spawning biomass, and recruits.
#' Input NULL for none, e.g., BSR.references=list(NULL,"SSBmsy", NULL).
#'
#'
#' @return Graphics
#'
#' @author M. Prager
#' @author E. Williams
#' @author K. Shertzer
#' @author R. Cheshire
#' @author K. Purcell
#'
#' @examples \donttest{
#' BSR.time.plots(gag)
#' }
#'
#' @import stats
#' @import graphics
#' @import grDevices
#' @import nortest
#' @import utils
#' @import lmtest
#' @import tseries
#' @import compResidual
#'
#' @export
BSR.time.plots <-
function(x, DataName = deparse(substitute(x)), draft = TRUE,
   start.drop = 0, graphics.type = NULL, use.color = TRUE,
   units.b = x$info$units.biomass, units.ssb = x$info$units.ssb,
   units.r = x$info$units.rec, legend.pos = "topright", from.zero = TRUE,
   BSR.references = list("Bmsy", "SSBmsy", "Rmsy"))
########################################################################################
#  ARGUMENTS:
#  x - an R list with output from the assessment models
#     The list x must have a component x$t.series that is a data frame
#     The data frame must have components
#        x$t.series$year (integer) with years of analysis
#        x$t.series$SSB (real) with spawning-stock size
#     The list x must have a component x$parms that is a list
#     The list x$parms must have a (vector) component x$parms$SSBmsy that has
#        a single value of spawning-stock size at MSY.
#  DataName - a string representation an identifier for the data (run) in use.
#  draft - TRUE if the figure has a main title
#  graphics.type - a character vector with graphics-file types
#  start.drop - integer, number of years to drop from start of file
#  use.color - TRUE of graphs are in color
# BSR.references - A list of three character-string names to specify reference
# points to appear on BSR plots, in order of Biomass, Spawning biomass, and Recruits.
# Input NULL for none, e.g., BSR.references=list("Bmsy","SSBmsy",NULL).
########################################################################################
###  PLOTS MADE BY THIS FUNCTION
###  1. SSB vs. year with reference line at BSR.references[[2]]
###  2. SSB/BSR.references[[2]] with reference line at 1.0
###  3. SSB/SSB0 with reference line at BSR.references[[2]]/SSB0
###  4. B with reference line at BSR.references[[1]]
###  5. B/BSR.references[[1]] with reference line at 1.0
###  6. B/B0 by year with reference line at BSR.references[[1]]/B0
###  7. Recruits by year with ref. line at BSR.references[[3]]
###  8. Recruits/Rmsy by year with ref. line at 1.0
###  9. Recruits/R0 by year with ref. line at 1.0
### 10. Recruitment deviations by year with loess smooth.
########################################################################################
{  # Start of function
    ### Check for needed data components
    if (! ("t.series" %in% names(x)))
    {   Errstring = (paste("Component ", deparse(substitute(x)), "$t.series not found.",
            sep = ""))
        warning(Errstring, immediate. = TRUE)
        return(invisible(-1))
    }
    if (! ("parms" %in% names(x)))
    {   Errstring = (paste("Component ", deparse(substitute(x)), "$parms not found.",
            sep = ""))
        warning(Errstring, immediate. = TRUE)
        return(invisible(-1))
    }

    h.ref=ifelse (is.list(BSR.references), TRUE, FALSE)
    ref.parms=rep(FALSE, length=3)
    if (h.ref){
      ref.parms=BSR.references %in% names(x$parms)
      for (i in 1:3) {
        if ((!is.null(BSR.references[[i]])) && (!ref.parms[i])) {
          Errstring = (paste("BSR.reference element ", BSR.references[[i]], " not found.", sep = ""))
          warning(Errstring, immediate. = TRUE)
          return(invisible(-1))
        }
      }
    }

    plot.options = FGGetOptions()
    ### Make local copies of data components:
    ts    <- x$t.series[(start.drop + 1):nrow(x$t.series),]
    parms <- x$parms
    ### Set vars global to function:
    lab.x <- "Year"

    ### Set graphics parameters
    savepar <- FGSetPar(draft)

    ### Set main title default for plots
    PlotTitle = ""

    ### If writing graphics files, make sure there is a directory for them:
    if (length(graphics.type > 0))
    {   write.graphs <- TRUE
        GraphicsDirName <- paste(DataName, "-figs/BSR", sep="")
    }
    else
    {   write.graphs <- FALSE
    }
###############  Plot B with reference line at Bref ###############
    if ("B" %in% names(ts))
    {   lab.y <- FGMakeLabel("Total biomass", units.b)
        ylim <- range(ts$B, na.rm = TRUE)
        if (from.zero) ylim <- range(ylim, 0)
        # Plot reference line?
        if ((!is.null(BSR.references[[1]])) && h.ref)
        {  hrefstring <- BSR.references[[1]]
           hrefindex <- which(names(x$parms) == BSR.references[[1]])
           href <- unlist(x$parms[hrefindex])
           hrefnames <- BSR.references[[1]]
           ylim <- range(ylim, href)
        }
        else { href <- NULL ; hrefnames <- NULL }
        if(draft) PlotTitle <- FGMakeTitle("Biomass", DataName)
        FGTimePlot(ts$year, ts$B, lab.x = lab.x, lab.y = lab.y,
            href = href, hrefnames = hrefnames, use.color = use.color,
            ylim = ylim, main = PlotTitle)
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "B.total", graphics.type)
    }
###############  Plot B/Bmsy with reference line at 1.0 ###############
    if ("B" %in% names(ts) && ref.parms[1])
    {
        hrefstring <- BSR.references[[1]]
        hrefindex <- which(names(x$parms) == BSR.references[[1]])
        Bref <- unlist(x$parms[hrefindex])
        ylim <- range(ts$B/Bref, 1.1, na.rm = TRUE)
        lab.y <- paste("B / ", hrefstring,sep="")
        if (from.zero) ylim <- range(ylim, 0)
        if(draft) PlotTitle <- FGMakeTitle(lab.y, DataName)
        href <- if (h.ref) { 1.0 } else { NULL }
        FGTimePlot(ts$year, ts$B/Bref, lab.x = lab.x,
            lab.y = lab.y, href = href, hrefnames = NULL, use.color = use.color,
            ylim = ylim, main = PlotTitle)
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "B.Bref", graphics.type)
    }
#########  Plot B/B0 with reference line at Bref/B0 ###############
    if ("B" %in% names(ts) && "B0" %in% names(parms))
    {   lab.y <- "B / B0"

        if (ref.parms[1] && h.ref)
            {
              href <- Bref/parms$B0
              hrefnames <- paste(BSR.references[[1]]," / B0", sep="")
              ylim <- range(ts$B / parms$B0, Bref / parms$B0, 1, na.rm = TRUE)

            }
        else  { href <- NULL ; hrefnames <- NULL
                ylim <- range(ts$B / parms$B0, 1, na.rm = TRUE)
        }
        if (from.zero) ylim <- range(ylim, 0)
        if(draft) PlotTitle <- FGMakeTitle("B/B0", DataName)
        FGTimePlot(ts$year, ts$B / parms$B0, lab.x = lab.x, lab.y = lab.y,
            href = href, hrefnames = hrefnames, use.color = use.color,
            ylim = ylim, main = PlotTitle)
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "B.B0", graphics.type)
    }
###############  Plot SSB vs. year with ref lines Sref, MSST  ####################
    if ("SSB" %in% names(ts))
    {   lab.y <- FGMakeLabel("Spawning stock", units.ssb)
        ylim  <- range(ts$SSB, na.rm = TRUE)
        if (from.zero) ylim <- range(ylim, 0)
        href <- as.numeric(c(NA, NA))
        hrefnames <- as.character(c(NA, NA))

        if ((!is.null(BSR.references[[2]])) && h.ref)
        {  hrefstring <- BSR.references[[2]]
           hrefindex <- which(names(x$parms) == BSR.references[[2]])
           href[1] <- unlist(x$parms[hrefindex])
           hrefnames[1] <- BSR.references[[2]]
           ylim <- range(ylim, href[1])

           if ("msst" %in% names(parms))
           {   href[2] <- parms$msst
               hrefnames[2] <- "M*S*S*T"
               ylim <- range(ylim, href[2])
           }

           href <- href[! is.na(href)]
           # Note: we had to store the names as character so we could see if they
           # are NA or not.  After we select the ones that are not NA, we convert
           # them to expressions (to get math effect) with parse():
           hrefnames <- parse(text = hrefnames[! is.na(hrefnames)])

        }
        else { href <- NULL ; hrefnames <- NULL }

        if(draft) PlotTitle <- FGMakeTitle("Spawning biomass", DataName)
        FGTimePlot(ts$year, ts$SSB, lab.x = lab.x, lab.y = lab.y,
            href = href, hrefnames = hrefnames, legend.pos = legend.pos,
            ylim = ylim, use.color = use.color, main = PlotTitle)
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName, GraphName="SSB",
            graphics.type)
    }
###############  Plot SSB/Sref with reference line at 1.0 #################
    if ("SSB" %in% names(ts) && ref.parms[2])
    {
      hrefstring <- BSR.references[[2]]
      hrefindex <- which(names(x$parms) == BSR.references[[2]])
      Sref <- unlist(x$parms[hrefindex])
      ylim <- range(ts$SSB/Sref, 1.2, na.rm = TRUE)
      lab.y <- paste("SSB / ", hrefstring, sep="")
      if (from.zero) ylim <- range(ylim, 0)
      if(draft) PlotTitle <- FGMakeTitle(lab.y, DataName)
      href <- if (h.ref) { 1.0 } else { NULL }
      FGTimePlot(ts$year, ts$SSB/Sref, lab.x = lab.x,
                 lab.y = lab.y, href = href, hrefnames = NULL, use.color = use.color,
                 ylim = ylim, main = PlotTitle)
      if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                                   GraphName = "SSB.SSBref", graphics.type)
    }
###############  Plot SSB/SSB0 with reference line at SSBmsy/SSB0 ################
    if ("SSB" %in% names(ts) && "SSB0" %in% names(parms))
    {   lab.y <- "SSB / SSB0"

        if (ref.parms[2] && h.ref)
        {
          href <- Sref/parms$SSB0
          hrefnames <- paste(BSR.references[[2]]," / SSB0", sep="")
          ylim <- range(ts$SSB / parms$SSB0, Sref / parms$SSB0, 1, na.rm = TRUE)

        }
        else  { href <- NULL ; hrefnames <- NULL
                ylim <- range(ts$SSB / parms$SSB0, 1, na.rm = TRUE)
        }
        if (from.zero) ylim <- range(ylim, 0)
        if(draft) PlotTitle <- FGMakeTitle("SSB/SSB0", DataName)
        FGTimePlot(ts$year, ts$SSB / parms$SSB0, lab.x = lab.x, lab.y = lab.y,
                   href = href, hrefnames = hrefnames, use.color = use.color,
                   ylim = ylim, main = PlotTitle)
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                                     GraphName = "SSB.SSB0", graphics.type)
    }
###############  Plot Recruits vs. year with ref. line at Rref ###############
    if ("recruits" %in% names(ts))
    {   lab.y <- FGMakeLabel("Recruitment", units.r)
        ylim <- range(ts$recruits, na.rm = TRUE)
        if (from.zero) ylim <- range(ylim, 0)
        # Plot reference line?
        if ((!is.null(BSR.references[[3]])) && h.ref)
        {  hrefstring <- BSR.references[[3]]
           hrefindex <- which(names(x$parms) == BSR.references[[3]])
           href <- unlist(x$parms[hrefindex])
           hrefnames <- BSR.references[[3]]
           ylim <- range(ylim, href)
        }
        else { href <- NULL ; hrefnames <- NULL }
        if(draft) PlotTitle <- FGMakeTitle("Recruitment", DataName)
        FGTimePlot(ts$year, ts$recruits, lab.x = lab.x, lab.y = lab.y,
                   href = href, hrefnames = hrefnames, use.color = use.color,
                   ylim = ylim, main = PlotTitle)
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                                     GraphName = "R", graphics.type)
    }

###############  Plot Recruits/Rref by year with ref. line at 1.0 ###############
    if ("recruits" %in% names(ts) && ref.parms[3])
    {
      hrefstring <- BSR.references[[3]]
      hrefindex <- which(names(x$parms) == BSR.references[[3]])
      Rref <- unlist(x$parms[hrefindex])
      ylim <- range(ts$recruits/Rref, 1.2, na.rm = TRUE)
      lab.y <- paste("Recruits / ", hrefstring, sep="")
      if (from.zero) ylim <- range(ylim, 0)
      if(draft) PlotTitle <- FGMakeTitle(lab.y, DataName)
      href <- if (h.ref) { 1.0 } else { NULL }
      FGTimePlot(ts$year, ts$recruits/Rref, lab.x = lab.x,
                 lab.y = lab.y, href = href, hrefnames = NULL, use.color = use.color,
                 ylim = ylim, main = PlotTitle)
      if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                                   GraphName = "R.Rref", graphics.type)
    }
###############  Plot Recruits/R0 by year with ref. line at Rref/R0 ###############
    if ("recruits" %in% names(ts) && "R0" %in% names(parms))
    {   lab.y <- "Recruits / R0"

        if (ref.parms[3] && h.ref)
        {
          href <- Rref/parms$R0
          hrefnames <- paste(BSR.references[[3]]," / R0", sep="")
          ylim <- range(ts$recruits / parms$R0, Rref / parms$R0, 1, na.rm = TRUE)

        }
        else  { href <- NULL ; hrefnames <- NULL
                ylim <- range(ts$recruits / parms$R0, 1, na.rm = TRUE)
        }
        if (from.zero) ylim <- range(ylim, 0)
        if(draft) PlotTitle <- FGMakeTitle("Recruits/R0", DataName)
        FGTimePlot(ts$year, ts$recruits / parms$R0, lab.x = lab.x, lab.y = lab.y,
                   href = href, hrefnames = hrefnames, use.color = use.color,
                   ylim = ylim, main = PlotTitle)
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                                     GraphName = "R.R0", graphics.type)
    }
###############  Plot Recruitment deviations  ###############
    if ("logR.dev" %in% names(ts))
    {   # Get smoothed line for plot
        a <- loess(logR.dev ~ year, data = ts, span = 0.6)
        ts$lRd.smooth <- rep(NA, nrow(ts))
        ts$lRd.smooth[! is.na(ts$logR.dev)] <- a$fitted
        ylim <- c(-1, 1) * max(abs(c(ts$logR.dev, ts$lRd.smooth)), na.rm = TRUE)
        # Set color scheme to use:
        if(use.color) FGO <- plot.options$color else FGO <- plot.options$bw
        href <- if (h.ref) { 0.0 } else { NULL }
        if(draft) PlotTitle <- FGMakeTitle("Recruitment deviations", DataName)
        FGTimePlot(x = ts$year, y2 = ts$logR.dev, y = ts$lRd.smooth,
            lab.x = "Year", Y1Col = FGO$clr.lightline, Y2Col = FGO$clr.line,
            href = href, use.color = use.color,
            lab.y = "log Recruitment deviations + loess", hrefnames = NULL,
            main = PlotTitle, FGtype = "linepointnodots", ylim = ylim)
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "R.logRdev", graphics.type)
    }
#########################################################################
    return(invisible(NULL))

}  # End of function BSR.time.plots()

