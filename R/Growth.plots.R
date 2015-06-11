#' Plots of fish size (and other quantities) at age
#' 
#' The function Growth.plots provides plots of length, weight, and other 
#' quantities at age. Length is also plotted with confidence intervals if the 
#' CV of length at age is found. There is support for models with more than one 
#' growth curve, such as models that describe growth as varying by sex.
#' 
#' @param x an R list with output from the assessment models.
#' @param DataName string used in plot titles.  Defaults to argument \code{x}.
#' @param draft modifies plots for use in a report.  When \code{FALSE} main titles 
#' are omitted.
#' @param graphics.type a vector of graphics file types to which graphics are saved.  
#' When \code{NULL}, no plots are saved.
#' @param use.color plots are made in grayscale when \code{FALSE}.
#' @param units.length Character string giving the units of measure associated
#' with the lengths at age found in \code{x$a.series$length}.
#' @param units.weight  Character string giving the units of measure associated
#' with weights at age found in \code{x$a.series$weight}.
#' @param plot.all When \code{TRUE}, all columns in \code{x$a.series} are plotted
#' against \code{x$a.series$age}.
#' #' 
#' @return Graphics
#' 
#' @author M. Prager
#' @author Erik H. Williams
#' @author Andi Stephens
#' @author Kyle W. Shertzer
#' 
#' @examples \donttest{
#' Growth.plots(gag)
#' }
#' 
Growth.plots <-
function(x, DataName = deparse(substitute(x)), draft = TRUE,
    graphics.type = NULL, use.color = TRUE, units.length = x$info$units.length,
    units.weight = x$info$units.weight, plot.all = FALSE)
{
    ### Check for needed data:
    if (! ("a.series" %in% names(x)))
    {   Errmsg <- paste("Data frame a.series not found in", deparse(substitute(x)))
        warning(Errmsg, immediate. = TRUE)
        return(invisible(-1))
    }
    if (! ("age"  %in% names(x$a.series)))
    {   Errmsg <- paste("Variable 'age' found in ",
            deparse(substitute(x)), "$a.series.", sep = "")
        warning(Errmsg, immediate. = TRUE)
        return(invisible(-1))
    }
    ### Setup graphics directory:
    if (! is.null(graphics.type))
    {   write.graphs <- TRUE
        GraphicsDirName <- paste(DataName, "-figs/growth", sep="")
    }
    else
    {   write.graphs <- FALSE
    }
    ### Set graphics parameters, constants, data structures:
    savepar <- FGSetPar(draft)
    age <- x$a.series$age
    #===========================================================================
    # Plot length at age, and if CV found, make plots of that:
    if ("length" %in% names(x$a.series))
    {   # Plot of length at age:
        length <- x$a.series$length
        PlotTitle <- ifelse(draft, paste("Length at age      Data:", DataName), "")
        yrange <- range(length, x$parms$vb.li)
        lab.y <- FGMakeLabel("Length", units.length)
        href <- x$parms$vb.li
        hrefnames = expression(italic(L)[infinity])
        FGTimePlot(age, length, lab.x = "Age", lab.y = lab.y, use.color = use.color,
             ylim = yrange, main = PlotTitle, FGtype = "1line", href = href,
             hrefnames = hrefnames)
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "growth.len", graphics.type)
        # Additional plots when CV(length) is found
        if ("length.cv" %in% names(x$a.series))
        {   lencv = x$a.series$length.cv
            # Plot of length at age with 95% CI
            PlotTitle <- ifelse(draft, paste("Length with 95% CI      Data:",
                DataName), "")
            lenlower <- length - (1.96 * length * lencv)
            lenupper <- length + (1.96 * length * lencv)
            yrange <- range(lenlower, lenupper)
            lab.y <- FGMakeLabel("Length", units.length)
            FGTimePlot(age, length, lab.x = "Age", lab.y = lab.y,
                 use.color = use.color, ylim = yrange, main = PlotTitle,
                 FGtype = "1line", href = href, hrefnames = hrefnames)
            lines(age, lenlower, type = 'l', lwd = 1.3, col = "gray35")
            lines(age, lenupper, type = 'l', lwd = 1.3, col = "gray35")
            if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                 GraphName = "growth.lenCI", graphics.type)
            # Plot of length CV at age:
            PlotTitle <- ifelse(draft, paste("Length CV      Data:", DataName), "")
            FGTimePlot(age, lencv, lab.x = "Age", lab.y = "CV of length at age",
                use.color = use.color, ylim = range(lencv), main = PlotTitle,
                FGtype = "stick")
            if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                GraphName = "growth.lenCV", graphics.type)
        } # end of plots when CV(length) found
    }
    #===========================================================================
    # Plots of lengths in alternative units (or other sex):
    for (i in 0:9)
    {   leni <- paste("length", i, sep = "")    #length0, length1, ...
        if (leni %in% names(x$a.series))
        {   length <- x$a.series[,leni]
            units.length <- x$info[paste("units.", leni, sep = "")]
            yrange <- range(length)
            lab.y <- FGMakeLabel("Length", units.length)
            linfi <- paste("vb.li", i, sep = "")    # Name of L-infinity variable
            if (linfi %in% names(x$parms)) hrefi <- x$parms[[linfi]] else hrefi <- NULL
            #
            if (draft) PlotTitle <- paste("Length", i, "at age      Data:", DataName)
            FGTimePlot(age, length, lab.x = "Age", lab.y = lab.y, use.color = use.color,
                ylim = yrange, main = PlotTitle, FGtype = "1line",
                href = hrefi, hrefnames = expression(italic(L)[infinity]))
            if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                GraphName = paste("growth.", leni, sep = ""), graphics.type)
        }  # end "if leni %in% ..."
    }      # end for loop
    #===========================================================================
    # Plot weight at age
    if ("weight" %in% names(x$a.series))
    {   weight <- x$a.series$weight
        PlotTitle <- ifelse(draft, paste("Weight at age      Data:", DataName), "")
        yrange <- range(weight, x$parms$vb.wi)
        lab.y <- FGMakeLabel("Weight", units.weight)
        FGTimePlot(age, weight, lab.x = "Age", lab.y = lab.y, use.color = use.color,
            ylim = yrange, main = PlotTitle, FGtype = "1line",
            href = x$parms$vb.wi, hrefnames = expression(italic(W)[infinity]) )
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "growth.wgt", graphics.type)
    }
    #===========================================================================
    # Plots of weights in alternative units (or other sex):
    for (i in 0:9)
    {   wti <- paste("weight", i, sep = "")  # weight1, weight2, ...
        if (wti %in% names(x$a.series))
        {   weight <- x$a.series[,wti]
            units.weight <- x$info[paste("units.", wti, sep = "")]
            yrange <- range(weight)
            lab.y <- FGMakeLabel("Weight", units.weight)
            winfi <- paste("vb.wi", i, sep = "")    # Name of W-infinity variable
            if (winfi %in% names(x$parms)) hrefi <- x$parms[[winfi]] else hrefi <- NULL
            #
            if (draft) PlotTitle <- paste("Weight", i, "at age      Data:", DataName)
            FGTimePlot(age, weight, lab.x = "Age", lab.y = lab.y, use.color = use.color,
                ylim = yrange, main = PlotTitle, FGtype = "1line",
                href = hrefi, hrefnames = expression(italic(W)[infinity]))
            if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                GraphName = paste("growth.", wti, sep = ""), graphics.type)
        }  # end "if wti %in% ..."
    }      # end for loop
    #===========================================================================
    # Plot other columns of data frame:
    if (plot.all)
    {   # Get list of all names and remove those already plotted:
        allnames <- names(x$a.series)
        allnames <- allnames[-which(allnames == "age")]
        allnames <- allnames[-which(allnames == "length")]
        allnames <- allnames[-which(allnames == "weight")]
        # Remove length0 - length9 and weight0 - weight9:
        allnames <- allnames[-grep("^length[[:digit:]]", allnames)]
        allnames <- allnames[-grep("^weight[[:digit:]]", allnames)]

        # Make the plots:
        for (this.name in allnames)
        {   PlotTitle <- ifelse(draft, paste(this.name,"      Data:", DataName), "")
            FGTimePlot(age, x$a.series[,this.name], lab.x = "Age",
                lab.y = this.name, use.color = use.color,
                main = PlotTitle, FGtype = "1line",
                href = NULL,  hrefnames = NULL)
            if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                GraphName = paste("age", this.name, sep="."), graphics.type)
        }
    }
   #===========================================================================
   par(savepar)    # reset graphics device
   return(invisible(0))
} # End of function definition

