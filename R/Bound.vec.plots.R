#' Plots time and age vectors and their bounds for stock-assessment models
#' 
#' Graphically displays estimated vectors and bound values for a stock-assessment model run.
#' 
#' @param x an R list with output from the assessment models.
#' @param DataName string used in plot titles.  Defaults to argument \code{x}.
#' @param draft modifies plots for use in a report.  When \code{FALSE} main titles 
#' are omitted.
#' @param graphics.type a vector of graphics file types to which graphics are saved.  
#' When \code{NULL}, no plots are saved.

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
#' Bound.vec.plots(gag)
#' }
#' 
Bound.vec.plots <- function(x, DataName = deparse(substitute(x)), draft=TRUE, graphics.type = NULL)
#=================================================================================
{

  ### Check for data presence:
  tvec.check="parm.tvec" %in% names(x) #time vectors
  avec.check="parm.avec" %in% names(x) #age vectors
  Errstring = ("No bounded time or age vectors found in the data set.  Terminating Bounded.vec.plots");
  if (! (tvec.check || avec.check )) stop(Errstring)

  if (tvec.check) {
    if (! ("year"  %in% names(x$parm.tvec)))
    {   Errmsg <- paste("Variable 'year' not found in ",
                        deparse(substitute(x)), "$parm.tvec", sep = "")
        warning(Errmsg, immediate. = TRUE)
        return(invisible(-1))
    }
  }
  
  if (avec.check) {
    if (! ("age"  %in% names(x$parm.avec)))
    {   Errmsg <- paste("Variable 'age' not found in ",
                        deparse(substitute(x)), "$parm.avec", sep = "")
        warning(Errmsg, immediate. = TRUE)
        return(invisible(-1))
    }
  }
  
  ### Set up plotting-related stuff:
  savepar <- FGSetPar(draft)
  PlotTitle <- ""
  if (! is.null(graphics.type))
  {   write.graphs <- TRUE
      GraphicsDirName <- paste(DataName, "-figs/boundvecs", sep="")
      FGCheckGraphDir(GraphicsDirName)
  } else {write.graphs <- FALSE}
  
  
  
  ##### plot bounds and estimate of time vectors ###################################
  if (tvec.check) {
    namevec=names(x$parm.tvec)[-1]
    numvecs=ncol(x$parm.tvec)-1
    nameveccons=names(x$parm.tvec.cons)
    numveccons=ncol(x$parm.tvec.cons)
    
    for(p in 1:numvecs)
    {
      name=namevec[p]
      colnum=sum(as.numeric(name==nameveccons)*c(1:numveccons))
      xdat=x$parm.tvec[,1]
      ydat=x$parm.tvec[,1+p]
      ymin=min(na.omit(c(x$parm.tvec.cons[,colnum][1],ydat)))
      ymax=max(na.omit(c(x$parm.tvec.cons[,colnum][2],ydat)))
      phase=x$parm.tvec.cons[3,colnum]
      par(las=FGSetLas(c(ymin,ydat,ymax)))
      PlotTitle <- ifelse(draft, paste("Vector:", name), "")
      plot(xdat,ydat,ylim=c(ymin,ymax),xlab="Year",ylab="Value",main=PlotTitle,cex=1,lwd=2,pch=16)
      grid()
      points(xdat,ydat, pch=16)
      abline(h=ymin,col=2,lwd=2)
      abline(h=ymax,col=2,lwd=2)  
      if (phase<0) {text(grconvertX(0.50,"npc"),grconvertY(0.1,"npc"), "Fixed Values", cex=1.)}
      
      if (write.graphs) FGSavePlot(GraphicsDirName, DataName, 
                                   GraphName = paste("tvec.",name,sep=""),
                                   graphics.type)
      
    } # end numvecs loop  
  } #end tvec.check
 

  ##### plot bounds and estimate of time vectors ###################################  
  if (avec.check) {
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
      phase=x$parm.avec.cons[3,colnum]
      par(las=FGSetLas(c(ymin,ydat,ymax)))
      PlotTitle <- ifelse(draft, paste("Vector:", name), "")
      plot(xdat,ydat,ylim=c(ymin,ymax),xlab="Age",ylab="Value",main=PlotTitle,cex=1,lwd=2,pch=16)
      grid()
      points(xdat,ydat, pch=16)
      abline(h=ymin,col=2,lwd=2)
      abline(h=ymax,col=2,lwd=2)  
      if (phase<0) {text(grconvertX(0.50,"npc"),grconvertY(0.1,"npc"), "Fixed Values", cex=1.)}
      
      if (write.graphs) FGSavePlot(GraphicsDirName, DataName, 
                                   GraphName = paste("avec.",name,sep=""),
                                   graphics.type)
      
    } # end numvecs loop  
  } #end avec.check
  
  par(savepar)
  return(invisible(0))
  
} # close plotting function










