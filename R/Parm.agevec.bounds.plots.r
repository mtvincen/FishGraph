#' Age vector parameter and bounds plots for stock-assessment models
#' 
#' Graphically displays age vector parameter and bound values for a stock-assessment model runs.
#' 
#' @param x an R list with output from the assessment models.
#' @param DataName string used in plot titles.  Defaults to argument \code{x}.
#' @param draft modifies plots for use in a report.  When \code{FALSE} main titles 
#' are omitted.
#' @param graphics.type a vector of graphics file types to which graphics are saved.  
#' When \code{NULL}, no plots are saved.
#' @param use.color plots are made in grayscale when \code{FALSE}

#' 
#' @return Graphics
#' 
#' @author Erik H. Williams
#' @author Kyle W. Shertzer
#' 
#' @examples \donttest{
#' Param.agevec.bounds.plots(gag)
#' }
#' 
#######################################################################################
#  Plots of parameters and bounds from fish stock-assessment models
#  Language: R
#  Author: E. H. Williams -- March, 2013
#  Updated 5/28/2013 KWS
#######################################################################################
Parm.agevec.bounds.plots <- function(x, DataName = deparse(substitute(x)),
                              draft = TRUE, graphics.type = NULL, use.color = TRUE)
#======================================================================================
{
  ### Set up plotting-related stuff:
  savepar <- FGSetPar(draft)
  PlotTitle <- ""
  if (! is.null(graphics.type))
  {   write.graphs <- TRUE
      GraphicsDirName <- paste(DataName, "-figs/sel", sep="")
      FGCheckGraphDir(GraphicsDirName)
  }
  else
  {   write.graphs <- FALSE }
  
  
  ##### plot bounds, estimate, and initial guess of dev vectors #######################
  ##### get number of vectors to plot #################################################
  par(mfrow=c(1,1),mai=c(1.5,1,1.5,0.5))
  
  namevec=names(x$parm.avec)[-1]
  numvecs=ncol(x$parm.avec)-1
  nameveccons=names(x$parm.avec.cons)
  numveccons=ncol(x$parm.avec.cons)
  
  for(p in 1:numvecs)
  {
    name=namevec[p]
    colnum=sum(as.numeric(name==nameveccons)*c(1:numveccons))
    xdat=x$parm.avec[,1]
    ydat=x$parm.avec[,1+p]
    ymin=min(na.omit(c(x$parm.avec.cons[,colnum][1],ydat)))
    ymax=max(na.omit(c(x$parm.avec.cons[,colnum][2],ydat)))
    plot(xdat,ydat,ylim=c(ymin,ymax),xlab="Age",ylab="Dev",
         main=paste("Vector:",name),cex=1,lwd=2,pch=16)
    grid()
    abline(h=ymin,col=2,lwd=2)
    abline(h=ymax,col=2,lwd=2)  
  } # end numvecs loop
  
  par(savepar)
  return(invisible(0))
  
} # close plotting function










