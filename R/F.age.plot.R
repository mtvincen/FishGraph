#'
#' The routine \code{F.age.plots} provides a time series plot of fishing mortality at age for all ages. Plots are of absolute estimated F at age.
#'

#' @param x an R list with output from the assessment model
#' @param DataName string used in plot titles. Defaults to argument \code{x}.
#' @param draft modifies plots for use in a report. When \code{FALSE} main titles are omitted.
#' @param graphics.type a vector of graphics file types to which graphics are saved. When \code{NULL}, no plots are saved.
#' @param use.color binary to determine if use default R colors (FALSE) or create using rainbow and number of ages (TRUE). default=TRUE
#' @param legend.pos a text string compatible with the \code{legend} function of \code{R}. Defines the position of the legend (ex. "bottomright","bottom",etc.)
#'
#' @return Graphics
#'
#' @author M Vincent
#'
#' @examples \donttest{
#' F.age.plot(gag)
#' }
#' @export

F.age.plot <- function(x, DataName = deparse(substitute(x)), draft = TRUE,
   graphics.type = NULL, use.color = TRUE, legend.pos = "topleft") {
    ## Check for needed data components
    if (! ("F.age" %in% names(x)))
    {  Errmsg = (paste("Component ", deparse(substitute(x)), "$F.age not found.",
                       sep = ""))
                       warning(Errmsg, immediate. = TRUE)
                       return(invisible(-1))
    }
    ## Get years from row names of matrix of F at age
    years=as.numeric(row.names(x$F.age))
    ## Get ages from column names of matrix of F at age
    ages=as.numeric(colnames(x$F.age))
    ## Make local copy of data
    Fage=x$F.age



    ## If writing graphics files, make sure there is a directory for them:
    if (length(graphics.type > 0)){
        write.graphs <- TRUE
        GraphicsDirName <- paste(DataName, "-figs/F", sep="")
    } else { write.graphs <- FALSE}
    ## Set graphics parameters, constants, data structures:
    plot.options = FGGetOptions()
    savepar <- FGSetPar(draft)
    PlotTitle <- ""
    colvec <- FGGetPal(length(ages), use.color)
    if(use.color){ Y1Col <-colvec[1]
    } else Y1Col <- plot.options$bw$clr.line

    ## Make plot of F at age series:
    lab.x <- "Year"
    lab.y <- "Fishing mortality rate at age"
    if(draft){ PlotTitle <- FGMakeTitle("Fishing mortality at age", DataName)}
    ymin <- 0.0
    ymax <- max(Fage, 0.01, na.rm = TRUE)  # (0.01) --> very small Fs only
    ## Make plot with first age
    FGTimePlot(years, Fage[,1], lab.x = lab.x, lab.y = lab.y, use.color = use.color, ylim = c(ymin, ymax), main = PlotTitle, FGtype = "1line", Y1Col = Y1Col)
    ## Loop over the remaining ages
    for (a in 2:length(ages)){
        lines(years,Fage[,a],col=colvec[a],lty=a,lwd=2)
        points(years,Fage[,a],col=colvec[a],pch=a)
    }
    ## Add in legend
    legend(legend.pos,legend=paste("Age",ages),col=colvec,lty=1:length(ages),pch=c(16,1:length(ages)),bty='n',inset = 0.03 / par()$pin,lwd=2,bg='transparent')
    if (write.graphs) FGSavePlot(GraphicsDirName, DataName, GraphName = "F.age", graphics.type)

    par(savepar)
    return(invisible(NULL))
}  # End of function definition



#create 3D plot of F at age                                       #
#persp(z=Fage,xlab="Time",ylab="Ages",theta=-130,phi=50,shade=-.2)
