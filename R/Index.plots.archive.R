#' Plots of fits to abundance indices
#' 
#' The routine \code{Index.plots} provides graphs for a visual evaluation of fits to 
#' abundance indices. This includes graphs of observed and predicted indices 
#' and graphs of residuals.
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
#' @param from.zero When \code{TRUE}, the Y-axis of each plot 
#' (except recruitment deviations) starts at zero.
#' @param two.panel When \code{TRUE}, the observed-predicted plot and residual 
#' time plot are drawn (and saved) as two panels of a single figure.  When \code{FALSE},
#' that are drawn and saved independently.
#' @param log.resid When \code{TRUE} residuals are computed as \eqn{R = log(\frac{U}{\hat{U}})}.
#' @param draw.barplot When \code{TRUE}, a stacked barplot of standardized 
#' residulas is drawn.
#' @param draw.pairs When \code{TRUE}, a scatterplot matrix of observed abundance
#' indices is drawn.
#' 
#' @return Graphics
#' 
#' @author M. Prager
#' @author Erik H. Williams
#' @author Andi Stephens
#' @author Kyle W. Shertzer
#' 
#' @examples \donttest{
#' Index.plots(gag)
#' }
#' 
Index.plots.archive <-
function(x, DataName = deparse(substitute(x)), draft = TRUE,
   graphics.type = NULL, use.color = TRUE, connect.obsd = FALSE, from.zero = TRUE,
   two.panel = TRUE, log.resid = TRUE, draw.barplot = TRUE, draw.pairs = TRUE, plot.options = FGGetOptions())
#######################################################################################
#  ARGUMENTS:
#  x - an R list with output from the assessment models
#     The list x must have a component x$t.series that is a data frame
#     The data frame must have components
#        x$t.series$year with (integer) years of the analysis
#        x$t.series$U.*.ob with observed index values
#        x$t.series$U.*.pr with predicted index values
#        IN ALL CASES, THE U.*.ob MUST PRECEDE THE CORRESPONDING U.*.pr.
#  DataName - a string representation an identifier for the data (run) in use.
#  draft - TRUE if the figures are to have main titles
#  graphics.type - a character vector with graphics-file types, e.g., c("png", "eps")
#  use.color - TRUE of graphs are in color
#  connect.obsd - TRUE to connect the observed points
#  from.zero - TRUE to start the Y-axis of each plot from zero
#  two.panel - TRUE to plot residuals together with fits as a two-panel figure
#  draw.barplot - TRUE to make barplot of stacked residuals
#  log.resid - TRUE to plot log(resid) rather than resid / mean(abs(resid))
#######################################################################################
{
    ### Check for x$t.series data frame:
    if (! ("t.series" %in% names(x)))
    {   ErrText = (paste("Component ", deparse(substitute(x)),
            "$t.series not found.", sep = ""))
        warning(ErrText, immediate. = TRUE)
        return(invisible(-1))
    }
    ### Local copy of data:
    ts <- x$t.series

    ### Check for other needed data:
    if (! ("year" %in% names(x$t.series)))
    {   warning("Year variable not found in Index.plots!", immediate. = TRUE)
        return(invisible(-1))
    }
    # Get indices of columns beginning with "U" (columns with CPUE):
    Ucols <-  grep("^U", names(ts))
    if (length(Ucols) == 0)
    {   warning("No data columns found that start with'U'.", immediate. = TRUE)
        return(invisible(-1))
    }
    # Use modulo operator to see if # of columns is odd:
    if (( length(Ucols) %% 2) != 0 )
    {   warning("Odd number of index columns found in Index.plots!", immediate. = TRUE)
        return(invisible(-1))
    }
    nplots <- length(Ucols) / 2                   # Number of distinct indices
    ### Set graphics parameters
    savepar <- FGSetPar(draft)
    PlotTitle <- ""
    # Define matrix to split screen later:
    smatrix <- rbind(c(0.0, 1.0, 0.40, 1.0), c(0.0, 1.0, 0.0, 0.40))
    colvec <- FGGetPal(nplots, use.color)
    ### If writing graphics files, make sure there is a directory for them:
    if (! is.null(graphics.type))
    {   write.graphs <- TRUE
        GraphicsDirName <- paste(DataName, "-figs/index", sep="")
    }
    else
    {   write.graphs <- FALSE }

    if (connect.obsd) FGtype <- "2line" else FGtype <- "linepoint"

    #   Find the indices of the years to be plotted -- same for all plots.
    #   Start: the first year with a non-missing value of any index.
    #   Stop: the last year in the t.series data frame.
    temp   <- is.na(ts[Ucols])          # Array of TRUE (missing) or FALSE (not missing)
    temp   <- apply(temp, 1, all)       # Vector with TRUE if all cols are TRUE (missing)
    first  <- which.min(temp)           # First year with any column not NA
    yrndx  <- first:nrow(ts)            # These years (rows) of the d.f. will be plotted
    nyear  <- length(yrndx)
    ts     <- ts[yrndx,]                # Drop years that won't be used

    resids <- matrix(NA, nplots, nyear) # empty matrix to save residuals
    bar.names <- rep("", nplots)        # empty vector of bar names
#----------------------------------------------------------------------------------
# Make the obsd-pred plots:
#----------------------------------------------------------------------------------
    if (two.panel) localpar <- par(no.readonly = TRUE)
    for (iplot in 1:nplots)
    {   # Get indices for plotted columns:
        i2 <- Ucols[2*iplot]
        i1 <- i2 - 1
        #
        cpue.obs <- ts[, Ucols[2*iplot - 1]]
        cpue.pre <- ts[, Ucols[2*iplot]]

        # Save residuals for barplot
        if (log.resid) resids[iplot,] <- log((cpue.obs + 1e-20) / (cpue.pre + 1e-20))
        else resids[iplot,] <- cpue.obs - cpue.pre

        # Get name of index and make plot title:
        IndexName  <- FGTrimName(names(ts[i1]), removePrefix = 1, removeSuffix = 1)
        bar.names[iplot] <- IndexName

        if (draft) PlotTitle <- FGMakeTitle(paste("Index:", IndexName), DataName)
        else PlotTitle <- ""

        # Scale Y-axis:
        if (from.zero)
            {   yrange = c(0, max(cpue.obs, cpue.pre, na.rm = TRUE)) }
        else
            {   yrange <- range(cpue.obs, cpue.pre, na.rm = TRUE) }

        # Plot of obs and predicted:
        lab.x = "Year"
        if (two.panel)
        {   split.screen(smatrix)
            screen(1)
            par(mar = localpar$mar + c(-3, 0, 0, 0), cex = 1, cex.axis = 0.85,
                cex.main = 1)
            lab.x = ""
        }
        FGTimePlot(x = ts$year, y = cpue.pre, y2 = cpue.obs, lab.x = lab.x,
            lab.y = "Relative abundance (CPUE)", href = NULL, hrefnames = NULL,
            use.color = use.color, FGtype = FGtype,
            ylim = yrange, main = PlotTitle)

        ### Write plot to file
        if (! two.panel && write.graphs)
            {   FGSavePlot(GraphicsDirName, DataName,
                GraphName = paste("U", IndexName, sep="."), graphics.type)
            }

        # Plot of residuals:
        if (two.panel)
        {   screen(2)
            PlotTitle = ""
            if (draft) par(mar = localpar$mar + c(0, 0, -1, 0))
            else par(mar = localpar$mar)
            par(cex = 1, cex.main = 1, cex.axis = 0.85, lab = c(5, 4, 7))
        }

        # Scaled residuals if not in log space:
        if (log.resid)
        {   scaled.resids <- resids[iplot,]
            lab.y <- "Log residual"
        }
        else
        {   meany <-mean(abs(resids[iplot,]), na.rm = TRUE)
            scaled.resids <- (resids[iplot,] + 1e-20) / (meany + 1e-20)
            lab.y <- "Scaled residual"
        }
        maxy <- max(abs(scaled.resids), na.rm = TRUE)
        ylim = maxy * 1.05 * c(-1, 1)

        FGTimePlot(x = ts$year, y = scaled.resids, lab.x = "Year",
            lab.y = lab.y, href = NULL, hrefnames = NULL, use.color = use.color,
            FGtype = "stick", ylim = ylim,  main = PlotTitle)
        abline(h = 0)

        ### Write plot to file(s)
        if (two.panel) GraphName <- paste("U", IndexName, sep=".")
        else GraphName = paste("U", IndexName, "resid", sep=".")
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName, GraphName, graphics.type)
        if (two.panel) close.screen(all = TRUE)

    } # end for iplot
    if (two.panel) par(localpar)
#----------------------------------------------------------------------------------
# Make the barplot of residuals:
#----------------------------------------------------------------------------------
    if (draw.barplot)
    {   if (log.resid)
        {   ylab = "Log residual"
        }
        else
        {   # Standardize residuals:  subtract mean, divide by sd.
            meanresids <- mean(resids, na.rm = TRUE)
            sdresids <- sd(as.vector(resids), na.rm = TRUE)
            resids <- (resids - meanresids)
            resids <- resids/sdresids
            ylab = "Standardized residual"
        }
        plotresids <- resids                         # Copy for plotting
        plotresids[is.na(resids)] <- 0.0             # Change NA to zero in copy only

        # Set up plot
        par(mfrow = c(2,1))  # Divide plot region for plots of + and - residuals
        if(draft) PlotTitle <- FGMakeTitle("Residuals in abundance indices.", DataName)
        else PlotTitle <- ""

        # Plot positive residuals:
        xresids <- ifelse(plotresids > 0.0, plotresids, 0.0)  #Get positive resids only
        # Get height of tallest bar, to place legend:
        ymax <- max(apply(xresids, 2, sum, na.rm = TRUE))
        par(mar=c(1, 4.5, 2, 0) + 0.2)
        barplot(xresids, beside = FALSE, names.arg = ts$year, cex.names = 0.9,
            axis.lty = 1, col = colvec, ylab = "", main = "", las = 1)
        legend(x = 1, y = ymax, xjust = 0, yjust = 1, fill = colvec, legend = bar.names)
        mtext(PlotTitle, side = 3, line = 1, adj = 0.5, font = 2)

        ### Plot negative residuals:
        xresids <- ifelse(plotresids < 0.0, plotresids, 0.0)
        par(mar=c(1, 4.5, 1.3, 0) + 0.2)
        mp <- barplot(xresids, beside = FALSE, names.arg = ts$year, axis.lty = 1,
            col = colvec, ylab = "", main = "", xlab = "",las = 1, axisnames = FALSE)
        axis(side = 3, tick = TRUE, at = mp, labels = rep("", nyear), tcl = -0.25)
        par(mfrow = c(1,1))
        mtext(ylab, side = 2, line = 3.5, adj = 0.5, font = 2)

        if (write.graphs) { FGSavePlot(GraphicsDirName, DataName,
            GraphName = "U.resids", graphics.type) }
    }  # End if (barplot)
#----------------------------------------------------------------------------------
# Make the pairs plot of residuals:
#----------------------------------------------------------------------------------
    if (draw.pairs && nplots > 1)
    {   ts <- ts[,Ucols]    # Only CPUE series
        # Only odd (obs) CPUE series
        ts <- ts[, seq(from = 1, to = ncol(ts) - 1, by = 2)]
        tsnames <- names(ts)
        # Take ".ob" from end of names:
        for (i in 1:ncol(ts))
        {    tsnames[i] <- FGTrimName(tsnames[i], removeSuffix = 1)
        }
        names(ts) <- tsnames
        clr <- "black"
        if (use.color) clr <- plot.options$color$clr.line
        PlotTitle = ""
        if (draft) PlotTitle <- FGMakeTitle("Comparison of abundance indices", DataName)
        pairs(ts, main = PlotTitle, lwd = 2, cex.labels = 1.6, cex = 1.5, col = clr,
            cex.axis = 1.2)
        if (write.graphs) { FGSavePlot(GraphicsDirName, DataName,
            GraphName = "U.pairs", graphics.type) }
    }
#----------------------------------------------------------------------------------

    # Add names to the matrix of residuals before returning it to caller
    rownames(resids) <- bar.names
    colnames(resids) <- ts$year
    # Restore graphics parameters and return:
    par(savepar)
    return(invisible(t(resids)))
}   # END OF FUNCTION

