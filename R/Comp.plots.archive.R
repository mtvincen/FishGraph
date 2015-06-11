#' Plots of age- and length-composition fits
#' 
#' The function \code{Comp.plots} generates bubble plots of residuals of age- 
#' and length-composition fits for the entire time frame of the assessment. 
#' Bubbles are scaled to the largest residual in each plot. So that the absolute
#' scale of deviations can be judged, a small inset plot displays the angular 
#' deviation between observed and predicted values each year.
#' 
#' @param x an R list with output from the assessment models.
#' @param DataName string used in plot titles.  Defaults to argument \code{x}.
#' @param draft modifies plots for use in a report.  When \code{FALSE} main titles 
#' are omitted.
#' @param graphics.type a vector of graphics file types to which graphics are saved.  
#' When \code{NULL}, no plots are saved.
#' @param use.color plots are made in grayscale when \code{FALSE}
#' @param units a character string (e.g. \code{"cm"}) for labeling the X-axis of
#' length-composition plots.  
#' 
#' @return Graphics
#' 
#' @author M. Prager
#' @author Erik H. Williams
#' @author Andi Stephens
#' @author Kyle W. Shertzer
#' 
#' @examples \donttest{
#' Comp.plots(gag)
#' }
#'
Comp.plots.archive <-
function(x, DataName = deparse(substitute(x)), draft = TRUE,
   graphics.type = NULL, use.color = TRUE, units = x$info$units.length,
   plot.options = FGGetOptions())
#######################################################################################
#  ARGUMENTS:
#  x - an R list with output from the assessment models
#     The list x must have a component x$comp.mats that is a list of matrices
#     These matrices must be found in pairs.  First, xxxx.ob, then xxxx.pr.
#  DataName - a string representation an identifier for the data (run) in use.
#  draft - TRUE if the figure has a main title.
#  graphics.type - a character vector with graphics-file types
#  use.color - TRUE of graphs are in color
#  units - character string with length units for Y-axis of length-comp plots
#######################################################################################
{  Errstring = ("No composition data found.  Terminating Comp.plots");
   if (! ("comp.mats" %in% names(x))) stop(Errstring)

   ### Make local copy of needed data components
   cm <- x$comp.mats

   # Is number of columns odd?  This is a problem -- they should be in pairs!
   if ( (length(cm) %% 2) != 0 ) stop("Odd number of matrices found in Comp.plots!\n")

   ### Set graphics parameters
   savepar <- FGSetPar(draft)

   ### If writing graphics files, make sure there is a directory for them:
   if (! is.null(graphics.type))
   {  write.graphs <- TRUE
      GraphicsDirName <- paste(DataName, "-figs/comp", sep="")
   }
   else
   {  write.graphs <- FALSE
   }

   ### Set the dimensions for splitting the graphics screen into two plotting regions:
   smatrix <- rbind(c(0.0, 1.0, 0.25, 1.0), c(0.0, 1.0, 0.0, 0.25))

   nplots <- length(cm) %/% 2               # integer division operator
   if (use.color)
   {  clr.pos <- plot.options$color$clr.pos    # color for positive residuals
      clr.neg <- plot.options$color$clr.neg    # color for negative residuals
      clr.ang <- plot.options$color$clr.ang    # color for angle plot
   }  else
   {  clr.pos <- plot.options$bw$clr.pos    # color for positive residuals
      clr.neg <- plot.options$bw$clr.neg    # color for negative residuals
      clr.ang <- plot.options$bw$clr.ang    # color for angle plot
   }

   for (iplot in 1:nplots)
   {  split.screen(smatrix)
      m1 <- cm[[iplot*2-1]]         # Matrix of observed
      m2 <- cm[[iplot*2]]           # matrix of predicted

      ### Get various string representations of data series:
      #  gfileroot is name for the graphics file(s):
      gfileroot <- FGTrimName(names(cm)[iplot * 2], removePrefix = 0, removeSuffix = 1)
      # titleroot is used as part of the plot title:
      titleroot <- paste("Fishery: ", gfileroot)

      ### Set Y-axis title according to data type:
      if(substr(gfileroot, 1, 1)  == "l") title.y <- FGMakeLabel("Length bin", units)
      else title.y <- "Age class"

      ## Get coordinates of bubbles:
      irn <- as.integer(rownames(m1))                        # year names
      x1 <- as.integer(rep(irn, ncol(m1)))                   # year names
      y1 <- sort(rep(as.numeric(colnames(m1)), nrow(m1)))    # age- or length-class names

      ### Get size and color of the bubbles:
      z1 <- c(m1 - m2)             # Residuals
      z2 <- 8.0*(sqrt(abs(z1))/sqrt(max(abs(z1))))
      colvec <- ifelse(z1 < 0.0, clr.neg, clr.pos)

      ### Compute angular deviation for each year using the formula:
      # angle - arccos (dotprod(a,b)/(sqrt(dotprod(a,a) * dotprot(b,b)))
      m12dp <- apply(m1*m2, 1, sum)   # This returns a vector of length nyear
      m11dp <- apply(m1*m1, 1, sum)   # Ditto
      m22dp <- apply(m2*m2, 1, sum)   # Ditto
      angdev = acos(m12dp/sqrt(m11dp*m22dp)) * 180.0 / pi   # Converts to degrees

      # Note: Set x limits so axis range is >= 4
      # This prevents plotting fractional years (e.g. 1995.5)
      xmin = irn[1]
      xmax = max(max(irn), xmin + 4)

      ### Draw the main (bubble) plot:
      screen(1)
      par(cex = 1, cex.main = 1, cex.axis = 0.85)
      {  if (draft) par(mar = c(1, 4, 3, 1 ))
         else par(mar = c(1, 4, 1, 1))
      }
      plot(x1, y1, xlab = "", ylab = title.y, type = "n", las = 1, xaxt = "n",
         xlim = c(xmin, xmax))
      grid(col = "lightgray", lty = 1)
      points(x1, y1, cex = z2, col = 1, bg = colvec, pch = 21)
      axis(side = 1, lab = FALSE)

      ### Draw the angular-deviation plot below the points:
      screen(2)
      par(cex = 1, cex.main = 1, cex.axis = 0.85, mar = c(1, 4, 2, 1))
      plot(irn, angdev, xlab = "Year", xaxt = "n", ylab = "Error, deg.",
         ylim = c(0, 90), axes = FALSE, frame.plot = TRUE, type = "n",
         xlim = c(xmin,xmax))
      axis(side = 1, labels = FALSE, tck = 1, col = "lightgray")
      axis(side = 2, at = seq(from = 0, to = 90, by = 10), las = 1,
        tck = 1, col = "gray75", lty = "dotted", labels = FALSE)
      axis(side = 2, at = c(20), tck = 1, col = "gray55", labels = FALSE)
      axis(side = 2, at = c(0, 30, 60, 90), las = 1, labels = TRUE, cex.axis = 0.75 )
      axis(side = 3, lab = TRUE, at = pretty(xmin:xmax))
      box()
      points(irn, angdev, pch = 18, col = clr.ang, cex = 1.3)

      ### Add main title:
      screen(1)
      if (draft)
      {  if (use.color)
           title(main = FGMakeTitle(paste(titleroot, "    Pink: underestimate"),
              DataName))
         else
           title(main = FGMakeTitle(paste(titleroot, "    Light: underestimate"),
              DataName))
      }  # end if draft

      if (write.graphs) FGSavePlot(GraphicsDirName, DataName, GraphName = gfileroot,
         graphics.type)

      close.screen(all = TRUE)  # Reset graphics window to one screen

   }     # end (for ....)
   par(savepar)
   return(invisible(NULL))
}     # End function definition

