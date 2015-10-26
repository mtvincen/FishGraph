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
#' @param graphics.type a vector of graphics file types to which graphics are saved.  
#' When \code{NULL}, no plots are saved.
#' @param use.color plots are made in grayscale when \code{FALSE}
#' @param connect.obsd In plots of observed and predicted data, observed points 
#' are joined by a line when \code{TRUE}.
#' @param start.drop Number of years at the start of the data to be omitted from
#' plots, as when a model includes an initialization period.
#' @param units.b A text string (e.g. \code{"tons"} for labeling plots of stock biomass.
#' @param units.ssb A text string for labeling plots of spawning-stock biomass.
#' @param units.r a text string (e.g. \code{"million fish"}) for labeling plots of recruitment
#' @param legend.pos A text string compatible with the \code{legend} function of \code{R}.
#' Defines the position of the legend (ex. "bottomright", "bottom", etc.)
#' @param from.zero When \code{TRUE}, the Y-axis of each plot 
#' (except recruitment deviations) starts at zero.
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
#' 
BSR.time.plots <-
function(x, DataName = deparse(substitute(x)), draft = TRUE,
   start.drop = 0, graphics.type = NULL, use.color = TRUE,
   units.b = x$info$units.biomass, units.ssb = x$info$units.ssb,
   units.r = x$info$units.rec, legend.pos = "topright", from.zero = TRUE,
   h.ref = TRUE, plot.options = FGGetOptions())
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
#  h.ref - TRUE if horizontal reference lines/labels are added
########################################################################################
###  PLOTS MADE BY THIS FUNCTION
###  1. SSB vs. year with reference line at SSBmsy
###  2. SSB/SSBmsy with reference line at 1.0
###  3. SSB/SSB0 with reference line at SSBmsy/SSB0
###  4. B with reference line at Bmsy
###  5. B/Bmsy with reference line at 1.0
###  6. B/B0 by year with reference line at Bmsy/B0
###  7. Recruits by year with ref. line at Rmsy
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
###############  Plot B with reference line at Bmsy ###############
    if ("B" %in% names(ts))
    {   lab.y <- FGMakeLabel("Total biomass", units.b)
        ylim <- range(ts$B, na.rm = TRUE)
        if (from.zero) ylim <- range(ylim, 0)
        # Plot reference line?
        if (("Bmsy" %in% names(parms)) && h.ref)
        {   href <- parms$Bmsy
            hrefnames <- expression(italic(B)[MSY])
            ylim <- range(ylim, parms$Bmsy)
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
    if ("B" %in% names(ts) && "Bmsy" %in% names(parms))
    {   lab.y <- expression(italic(B) / italic(B)[MSY])
        ylim <- range(ts$B/parms$Bmsy, 1.1, na.rm = TRUE)
        if (from.zero) ylim <- range(ylim, 0)
        if(draft) PlotTitle <- FGMakeTitle("B/Bmsy", DataName)
        href <- if (h.ref) { 1.0 } else { NULL }
        FGTimePlot(ts$year, ts$B/parms$Bmsy, lab.x = lab.x,
            lab.y = lab.y, href = href, hrefnames = NULL, use.color = use.color,
            ylim = ylim, main = PlotTitle)
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "B.Bmsy", graphics.type)
    }
#########  Plot B/B0 with reference line at Bmsy/B0 ###############
    if ("B" %in% names(ts) && "B0" %in% names(parms))
    {   lab.y <- expression(italic(B) / italic(B)[0])
        ylim <- range(ts$B / parms$B0, parms$Bmsy / parms$B0, 1, na.rm = TRUE)
        if (from.zero) ylim <- range(ylim, 0)
        if (("Bmsy" %in% names(parms)) && h.ref)
            {   href <- parms$Bmsy/parms$B0
                hrefnames <- expression(italic(B)[MSY] / italic(B)[0])
            }
        else  { href <- NULL ; hrefnames <- NULL }
        if(draft) PlotTitle <- FGMakeTitle("B/B0", DataName)
        FGTimePlot(ts$year, ts$B / parms$B0, lab.x = lab.x, lab.y = lab.y,
            href = href, hrefnames = hrefnames, use.color = use.color,
            ylim = ylim, main = PlotTitle)
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "B.B0", graphics.type)
    }
###############  Plot SSB vs. year with ref lines SSBmsy, MSST  ####################
    if ("SSB" %in% names(ts))
    {   lab.y <- FGMakeLabel("Spawning stock", units.ssb)
        ylim  <- range(ts$SSB, na.rm = TRUE)
        if (from.zero) ylim <- range(ylim, 0)
        # Plot reference line?
        href <- as.numeric(c(NA, NA))
        hrefnames <- as.character(c(NA, NA))
        if ("SSBmsy" %in% names(parms))
        {   href[1] <- parms$SSBmsy
            hrefnames[1] <- "SSB[MSY]"
            ylim <- range(ylim, href[1])
        }
        if ("msst" %in% names(parms))
        {   href[2] <- parms$msst
            hrefnames[2] <- "M*S*S*T"
            ylim <- range(ylim, href[2])
        }
        if (h.ref) {
          href <- href[! is.na(href)]
          # Note: we had to store the names as character so we could see if they
          # are NA or not.  After we select the ones that are not NA, we convert
          # them to expressions (to get math effect) with parse():
          hrefnames <- parse(text = hrefnames[! is.na(hrefnames)])
        }
        else { href <- NULL; hrefnames <- NULL }
        if(draft) PlotTitle <- FGMakeTitle("Spawning biomass", DataName)
        FGTimePlot(ts$year, ts$SSB, lab.x = lab.x, lab.y = lab.y,
            href = href, hrefnames = hrefnames, legend.pos = legend.pos,
            ylim = ylim, use.color = use.color, main = PlotTitle)
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName, GraphName="SSB",
            graphics.type)
    }
###############  Plot SSB/SSBmsy with reference line at 1.0 #################
    if ("SSB" %in% names(ts) && "SSBmsy" %in% names(parms))
    {  lab.y <- expression(SSB / SSB[MSY])
        ylim <- range(ts$SSB/parms$SSBmsy, 1.2, na.rm = TRUE)
        if (from.zero) ylim <- range(ylim, 0)
        href <- if (h.ref) { 1.0 } else { NULL }
        if(draft) PlotTitle <- FGMakeTitle("SSB/SSBmsy", DataName)
        FGTimePlot(ts$year, ts$SSB/parms$SSBmsy, lab.x = lab.x,
            lab.y = lab.y, href = href, hrefnames = NULL, use.color = use.color,
            ylim = ylim, main = PlotTitle)

        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "SSB.SSBmsy", graphics.type)
    }
###############  Plot SSB/SSB0 with reference line at SSBmsy/SSB0 ################
    if ("SSB" %in% names(ts) && "SSB0" %in% names(parms))
    {   lab.y <- expression(SSB / SSB[0])
        ylim <- range(ts$SSB/parms$SSB0, 1.2, na.rm=T)
        if (from.zero) ylim <- range(ylim, 0)
        if (("SSBmsy" %in% names(parms)) && h.ref)
        {   href <- parms$SSBmsy/parms$SSB0
            hrefnames <- expression(SSB[MSY] / SSB[0])
        }
        else  { href <- NULL ; hrefnames <- NULL }
        if(draft) PlotTitle <- FGMakeTitle("SSB/SSB0", DataName)
        FGTimePlot(ts$year, ts$SSB/parms$SSB0, lab.x = lab.x,
            lab.y = lab.y, href = href, hrefnames = hrefnames,
            use.color = use.color, ylim = ylim, main = PlotTitle)
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "SSB.SSB0", graphics.type)
    }
###############  Plot Recruits vs. year with ref. line at Rmsy ###############
    if ("recruits" %in% names(ts))
    {   lab.y <- FGMakeLabel("Recruitment", units.r)
        ylim <- range(ts$recruits, parms$Rmsy, na.rm = TRUE)
        if (from.zero) ylim <- range(ylim, 0)
        if(("Rmsy" %in% names(parms)) && h.ref)
            { href <- parms$Rmsy ; hrefnames <- expression(italic(R)[MSY])}
        else
            { href <- NULL       ; hrefnames <- NULL}
        if(draft) PlotTitle <- FGMakeTitle("Recruitment", DataName)
        FGTimePlot(ts$year, ts$recruits, lab.x = lab.x, lab.y = lab.y,
            href = href, hrefnames = hrefnames, use.color = use.color,
            ylim = ylim, main = PlotTitle)
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName="R", graphics.type)
    }
###############  Plot Recruits/Rmsy by year with ref. line at 1.0 ###############
    if ("recruits" %in% names(ts) && "Rmsy" %in% names(parms))
    {   lab.y <- expression(Recruits / italic(R)[MSY])
        ylim <- range(ts$recruits/parms$Rmsy, 1, na.rm=TRUE)
        if (from.zero) ylim <- range(ylim, 0)
        href <- if (h.ref) { 1.0 } else { NULL }
        if(draft) PlotTitle <- FGMakeTitle("R/Rmsy", DataName)
        FGTimePlot(ts$year, ts$recruits / parms$Rmsy, lab.x = lab.x,
            lab.y = lab.y, href = href, hrefnames = NULL,
            use.color = use.color, ylim = ylim, main = PlotTitle)
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "R.Rmsy", graphics.type)
    }
###############  Plot Recruits/R0 by year with ref. line at Rmsy/R0 ###############
   if ("recruits" %in% names(ts) && "R0" %in% names(parms))
    {   lab.y <- expression(Recruits / italic(R)[0])
        ylim <- range(ts$recruits / parms$R0, na.rm = TRUE)
        if (from.zero) ylim <- range(ylim, 0)
        if (("Rmsy" %in% names(parms)) && h.ref)
        {   href <- parms$Rmsy/parms$R0
            hrefnames <- expression(italic(R)[MSY] / italic(R)[0])
        }
        else
        {  href <- NULL ; hrefnames <- NULL }
        if(draft) PlotTitle <- FGMakeTitle("R/R0", DataName)
        FGTimePlot(ts$year, ts$recruits / parms$R0, lab.x = lab.x,
            lab.y = lab.y, href = href, hrefnames = hrefnames,
            use.color = use.color, ylim = ylim, main = PlotTitle)
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

