#' Stock-recruitment plots
#' 
#' The function \code{StockRec.plots} generates plots of stock vs. recruitment in 
#' linear and logarithmic scales.
#' 
#' @param x an R list with output from the assessment models.
#' @param DataName string used in plot titles.  Defaults to argument \code{x}.
#' @param draft modifies plots for use in a report.  When \code{FALSE} main titles 
#' are omitted.
#' @param graphics.type a vector of graphics file types to which graphics are saved.  
#' When \code{NULL}, no plots are saved.
#' @param use.color plots are made in grayscale when \code{FALSE}
#' @param start.drop Number of years at the start of the data to be omitted from
#' plots, as when a model includes an initialization period.
#' @param units.ssb A text string (e.g. \code{"tons"} or \code{"10^9 eggs"}) used
#' in labeling plots of spawning stock.
#' @param units.rec A text string (e.g. \code{"million fish"}) for labeling plots 
#' of recruitment
#' @param rec.model Specifies the type of recruitment function to draw.  Valid 
#' values are: \code{"BH", "BH-steep", "Ricker", "Ricker-steep"}.
#' @param draw.model If \code{TRUE}, a function curve is drawn on plots of stock
#' and recruitment.
#' @param draw.lowess If \code{TRUE}, a lowess smooth is drawn on plots of stock
#' and recruitment.
#' 
#' @return Graphics
#' 
#' @author M. Prager
#' @author Erik H. Williams
#' @author Andi Stephens
#' @author Kyle W. Shertzer
#' 
#' @examples \donttest{
#' StockRec.plots(gag)
#' }
#' 
StockRec.plots <- function(x, DataName = deparse(substitute(x)), draft = TRUE,
    graphics.type = NULL, use.color = TRUE, start.drop = 0,
    units.ssb = x$info$units.ssb, units.rec = x$info$units.rec,
    rec.model = x$info$rec.model, draw.model = TRUE, draw.lowess = FALSE,
    plot.options = FGGetOptions())
#=================================================================================
{   ### Check for required data:
    if (! ("t.series" %in% names(x)))
    {   Errmsg <- paste("Data frame 't.series' not found in data object:",
            deparse(substitute(x)))
        warning(Errmsg, immediate. = TRUE)
        return(invisible(-1))
    }
    if (! ("parms" %in% names(x)))
    {   Errmsg <- paste("List 'parms' not found in data object:", deparse(substitute(x)))
        warning(Errmsg, immediate. = TRUE)
        return(invisible(-1))
    }
    if (! ("rec.lag" %in% names(x$parms)))
    {   Errmsg <- paste("Value 'parms$rec.lag' not found in data object:",
            deparse(substitute(x)))
        warning(Errmsg, immediate. = TRUE)
        return(invisible(-1))
    }
#=================================================================================
    ### Get data, limits, titles, labels, etc.
#=================================================================================
    ts <- x$t.series
    rec.lag <- x$parms$rec.lag
    draw.bias <- FALSE

    ### Get starting and stopping indices for S and R in S-R plots
    sndx = as.integer(rep(0,2))  # initialize
    sndx[1] <- start.drop + 1
    sndx[2] <- nrow(ts) - rec.lag
    if (sndx[1] >= sndx[2])
    {   Errmsg <- "Length of S-R data series is <= 1"
        warning(Errmsg, immediate. = TRUE)
        return(invisible(-1))
    }
    # Expand index variables to full-length
    sndx <- sndx[1]:sndx[2]
    rndx <- sndx + x$parms$rec.lag

    ### Set up plotting-related stuff:
    PlotTitle <- ""
    savepar <- FGSetPar(draft)
    if (! is.null(graphics.type))
    {   write.graphs <- TRUE
        GraphicsDirName <- paste(DataName, "-figs/SR", sep="")
    }
    else write.graphs <- FALSE

    # Get colors for graphical objects:
    if (use.color) parlist <- plot.options$color
    else parlist <- plot.options$bw
    clr.points <- parlist$clr.line
    clr.lowess <- parlist$clr.lightline

    ### Set plotting limits, etc., for first plot:
    ssb.max <- max(ts$SSB[sndx], na.rm = TRUE)
    r.max   <- max(ts$recruits[rndx], na.rm = TRUE)
    lab.x   <- FGMakeLabel("Spawning stock", units.ssb)
    lab.y   <- FGMakeLabel("Recruitment", units.rec)
    rec.sim <- as.numeric(NA)   # Needs a definition for use in log plot
    lim.x = range(0, 1.1 * ssb.max)
    lim.y = range(0, 1.1 * r.max)
#=================================================================================
    ### If recruitment curve will be drawn, generate simulated
    ### data & adjust Y limits of plot:
#=================================================================================
    if (is.null(rec.model))
    {   draw.model <- FALSE
        rec.model <- "none"
        curve.OK <- TRUE
    }
    # Make sure curve type is recognized, and if so, get parameters:
    if (draw.model)
    {   stock.sim <- seq(from = 0.001 * ssb.max, to = 1.1 * ssb.max, length = 200)
        curve.OK <- FALSE
        # Beverton-Holt curve (standard)
        if (rec.model == "BH")
        {   alpha <- x$parms$BH.alpha
            beta <-  x$parms$BH.beta
            bias.corr <- x$parms$BH.biascorr
            rec.sim <- alpha * stock.sim / (1.0 + beta * stock.sim)
            curve.OK <- TRUE
        }
        # Beverton-Holt curve (steepness)
        if (rec.model == "BH-steep")
        {   R0 <- x$parms$BH.R0
            h <- x$parms$BH.steep
            phi0 <- x$parms$BH.Phi0
            bias.corr <- x$parms$BH.biascorr
            rec.sim <- (0.8 * R0 * h * stock.sim) / (0.2 * phi0 * R0 * (1.0 - h) +
                (h - 0.2) * stock.sim)
            curve.OK <- TRUE
        }
        # Ricker curve (standard)
        if (rec.model == "Ricker")
        {   alpha <- x$parms$Ricker.alpha
            beta <-  x$parms$Ricker.beta
            bias.corr <- x$parms$Ricker.biascorr
            rec.sim <- alpha * stock.sim * exp(-beta * stock.sim)
            curve.OK <- TRUE
        }
        # Ricker curve (steepness)
        if (rec.model == "Ricker-steep")
        {   R0 <- x$parms$Ricker.R0
            h <- x$parms$Ricker.steep
            phi0 <- x$parms$Ricker.Phi0
            bias.corr <- x$parms$Ricker.biascorr
            rec.sim <-  stock.sim / phi0 * exp(h * (1.0 - stock.sim / (phi0 * R0)))
            curve.OK <- TRUE
        }
        if (! curve.OK)
        {   warning("Bad value of argument rec.model\n")
            draw.model <- FALSE
            rec.model <- "none"
            bias.corr <- NULL
        }
    }
    if (draw.model)
    {   if (is.numeric(bias.corr) && bias.corr != 1.0) draw.bias <- TRUE
        # Adjust plotting limits to include the curves:
        {   if (draw.bias) lim.y <- range(0, max(bias.corr * rec.sim, ts$recruits[rndx],
                na.rm = TRUE))
            else lim.y <- range(0, max(rec.sim, ts$recruits[rndx], na.rm = TRUE))
        }
    }
#=================================================================================
    ### Now make the plots:
#=================================================================================
    ### Plot of stock vs. recruitment -- arithmetic scale
    if(draft) PlotTitle <- FGMakeTitle("Stock-recruitment (linear R)", DataName)
    FGTimePlot(x = ts$SSB[sndx], y = ts$recruits[rndx], y2 = NULL,
        lab.x = lab.x, lab.y = lab.y, FGtype = "circles", main = PlotTitle,
        use.color = use.color, xlim = lim.x, ylim = lim.y)
    redraw <- FALSE
    if(draw.lowess)
    {   lines(lowess(ts$SSB[sndx], ts$recruits[rndx],f = 0.55),
            col = clr.lowess, lwd = 2)
        redraw <- TRUE
    }
    if (draw.model && curve.OK)
    {   lines(stock.sim, rec.sim, lwd = 2)
        if (draw.bias)
        {   lines(stock.sim, bias.corr * rec.sim, lwd = 2, lty = "dashed")
            leg.text <- c(rec.model, "Bias corrected")
            legend("topright", legend = leg.text, lty = c("solid", "dashed"),
                lwd = 2, inset = 0.01, bg = "white")
            redraw <- TRUE
        }
    }
    # Replot to reveal any points under legend:
    if (redraw) points(ts$SSB[sndx], ts$recruits[rndx], col = clr.points)
    # Save plots to file:
    if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
        GraphName = paste("SR.",rec.model, ".lin", sep = ""), graphics.type)
    #==================================================================
    ##### Plot of stock & recruitment with function -- log scale
    if(draft) PlotTitle <- FGMakeTitle("Stock-recruitment (log R)", DataName)
    {   if (draw.bias)
            lim.y <- range(ts$recruits[rndx], bias.corr * rec.sim, na.rm = TRUE)
        else
            lim.y <- range(ts$recruits[rndx], rec.sim, na.rm = TRUE)
    }
    lim.y[1] = max(lim.y[1], min(ts$recruits[rndx], na.rm = TRUE) / 5.0)
    FGTimePlot(x = ts$SSB[sndx], y = ts$recruits[rndx],
        y2 = NULL, lab.x = lab.x, lab.y = lab.y, FGtype = "circles",
        main = PlotTitle, use.color = use.color, xlim = lim.x,  ylim = lim.y,
        log = "y")
    if(draw.lowess)
    {   lines(lowess(ts$SSB[sndx], ts$recruits[rndx],f = 0.60),
            col = clr.lowess, lwd = 2)
    }
    if (draw.model && curve.OK)
    {   lines(stock.sim, rec.sim, lwd = 2)
        if (draw.bias)
        {   lines(stock.sim, bias.corr * rec.sim, lwd = 2, lty = "dashed")
            legend("bottomright", legend = leg.text, lty = c("solid", "dashed"),
                lwd = 2, inset = 0.01, bg = "white")
        }
    }
    # Replot to reveal any points under legend:
    if (redraw) points(ts$SSB[sndx], ts$recruits[rndx], col = clr.points)
    # Save plots to file:
    if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
        GraphName = paste("SR.", rec.model, ".log", sep = ""), graphics.type)
#==================================================================================
   par(savepar)    # reset graphics device
   return(invisible(0))
} # end function definition

