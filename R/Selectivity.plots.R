#' Estimated selectivity curves
#' 
#' The function \code{Selectivity.plots} generates plots of selectivity curves 
#' at age and length.
#' 
#' @param x an R list with output from the assessment models.
#' @param DataName string used in plot titles.  Defaults to argument \code{x}.
#' @param draft modifies plots for use in a report.  When \code{FALSE} main titles 
#' are omitted.
#' @param graphics.type a vector of graphics file types to which graphics are saved.  
#' When \code{NULL}, no plots are saved.
#' @param use.color plots are made in grayscale when \code{FALSE}
#' @param plot.points A logical values; if \code{TRUE}, points will be plotted on curves.
#' @param units.length A text string (e.g. \code{"mm"}) used in labeling plots
#' of selectivity at length.
#' @param units.age A text string (e.g. \code{"years"}) used in labeling plots 
#' of selectivity at age.
#' @param compact When \code{TRUE}, time-varying selectivity curves are placed on a single plot per fishery.
#' @param legend.pos A text string compatible with the \code{legend} function of \code{R}.
#' Defines the position of the legend (ex. "bottomright", "bottom", etc.). Applies only if 
#' \code{compact = TRUE}.
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
#' Selectivity.plots(gag)
#' }
#' 
Selectivity.plots <- function(x, DataName = deparse(substitute(x)),
   draft = TRUE, graphics.type = NULL, use.color = TRUE, plot.points = TRUE,
   units.length = x$info$units.length, units.age = x$info$units.age,
   compact=FALSE, legend.pos = "right")
#=================================================================================
{
    ### Set up plotting-related stuff:
    savepar <- FGSetPar(draft)
    PlotTitle <- ""
    if (! is.null(graphics.type))
    {   write.graphs <- TRUE
        GraphicsDirName <- paste(DataName, "-figs/sel", sep="")
    }
    else
    {   write.graphs <- FALSE }
    # Plot just curves or add lines?
    if (plot.points) {GType <- "linepoint"} else {GType <- "linepointnodots"}


    ### Plotting follows:
    series.names <- c("sel.age", "sel.length")
    for (this.name in series.names)
    {   if (! (this.name %in% names(x)))
        {   message("Information: Subobject ",this.name," not found in object ",
                deparse(substitute(x)))
        }
        else
        {   my.text <- paste("sel.list = x$", this.name, sep="")
            eval(parse(text=my.text)) # This assigns the list in turn to "sel.list"
            selnames <- names(sel.list)
            measure <- substr(this.name, 5, 12)  # "age" or "length"
            Umeasure <- paste(toupper(substr(measure, 1,1)),
                substr(measure,2,11), sep="")  # "Age" or "Length"
            lab.y <- paste("Selectivity at", measure)
            lab.x <- FGMakeLabel(Umeasure,
                eval(parse(text = paste("units", measure, sep="."))))

            ### Make plots of selectivity VECTORS (no change over time):
            Vndx <- grep("^sel.v", names(sel.list))  # Indices of vectors in list
            n.vectors <- length(Vndx)
            for (iy in Vndx)
            {   age <- as.numeric(names(sel.list[[iy]]))
                # Make plot title:
                gearname = sub("sel\\.v\\.", "", selnames[iy])
                if (draft) PlotTitle <- paste("Fishery: ", gearname, "      Data: ",
                    DataName)
                # Make plot:
                FGTimePlot(x = age, y = sel.list[[iy]], lab.x = lab.x,
                    lab.y = lab.y, FGtype = GType, main = PlotTitle,
                    use.color = use.color,  ylim = c(0,1))
                # Save plot to file:
                if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                    GraphName = paste("sel", measure, gearname, sep = "."),
                    graphics.type)
            } # End for (iy in Vndx)

            ### Make plots of selectivity MATRICES (changing selectivity over time):
            Mndx <- grep("^sel.m", names(sel.list))  # indices of matrices in list
            n.matrices <- length(Mndx)
            #ifelse(plot.points, leg.pch<-16, leg.pch<-1) #used in legend if compact = TRUE
            for (imat in Mndx)
            {   smat     <- sel.list[[imat]]   # matrix being plotted
                gearname = sub("sel\\.m\\.", "", selnames[imat])
                yrlabvec <- as.character(rownames(smat))  # Char vectr of years
                xvec <- as.numeric(colnames(smat))   # Num vectr of age/len bins
                # Go through matrix by row (year). Plot each row if it differs
                # from the preceding row (which is always true for first row).
                oldvec <- rep(-1.0, ncol(smat))   # Initialze for comparison
                if (!compact) {
                    for (yrlab in yrlabvec)
                    {   # See if row is unequal to row before:
                        if (any(smat[yrlab,] != oldvec)) plot.it <- TRUE else plot.it <- FALSE
                        oldvec <- smat[yrlab,]          # save for next comparison
                        if (! plot.it) next             # Go to top of "for" loop
                        # Construct plot title:
                        if(draft) PlotTitle <- paste("Selectivity in: ", gearname,
                            "      Data: ", DataName, "      Year: ", yrlab)
                        # Make plot:
                        FGTimePlot(x = xvec, y = smat[yrlab,], lab.x = lab.x,
                            lab.y = lab.y, FGtype = GType, main = PlotTitle,
                            use.color = use.color,  ylim = c(0,1))
                        # Save plot to file:
                        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                            GraphName = paste("sel", measure, gearname, yrlab, sep = "."),
                            graphics.type)
                    }   # End for (yrlab in yrlabvec)
                } # End !compact
                if (compact) {
                  block.counter = 1 #Used if compact = TRUE
                  leg.lty=1
                  ifelse (use.color, leg.col<-"royalblue4", leg.col<-"black")
                  leg.lab=NULL
                  
                  for (yrlab in yrlabvec)
                  {   # See if row is unequal to row before:
                    if (any(smat[yrlab,] != oldvec)) plot.it <- TRUE else plot.it <- FALSE
                    oldvec <- smat[yrlab,]          # save for next comparison
                    
                    if (! plot.it) next             # Go to top of "for" loop
                    # Construct plot legend
                    leg.lab=c(leg.lab, as.character(yrlab))
                    # Make plot:
                    if (block.counter==1) {
                      if(draft) PlotTitle <- paste("Selectivity in: ", gearname,
                                                   "      Data: ", DataName)
                      FGTimePlot(x = xvec, y = smat[yrlab,], lab.x = lab.x,
                                 lab.y = lab.y, FGtype = GType, main = PlotTitle,
                                 use.color = use.color,  ylim = c(0,1))
                      block.counter=block.counter+1
                      
                    } else {
                      
                      if (use.color){
                        lines(xvec,smat[yrlab,], col=block.counter, lty=1, lwd=2)
                        if (plot.points){points(xvec,smat[yrlab,], col=block.counter, pch=16)}
                        leg.lty=c(leg.lty,1); leg.col=c(leg.col, block.counter); 
                      } else {
                        lines(xvec,smat[yrlab,], col="black", lty=block.counter, lwd=2)
                        if (plot.points){points(xvec,smat[yrlab,], col="black", pch=16)}          
                        leg.lty=c(leg.lty, block.counter); leg.col="black"; 
                      }
                      block.counter=block.counter+1
                    }
                  }   # End for (yrlab in yrlabvec)
                  # Save plot to file:
                  legend(legend.pos, legend=leg.lab, lwd=2, lty=leg.lty, col=leg.col, bg="white") 
                  if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                                               GraphName = paste("sel", measure, gearname, sep = "."),
                                               graphics.type)
                  
                } # End compact
                
                
            }   # End for (imat in Mndx)
            if(n.matrices + n.vectors == 0) {message(paste("Warning:", Umeasure, "selectivity data not found but containing object present. Naming error?"))}
        }   # End else ....
    }   # End for (this.name ...)
   par(savepar)
   return(invisible(0))
}  # End function

