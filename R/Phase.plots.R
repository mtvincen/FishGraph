#' Plots of fishing and spawning biomass status
#' 
#' The function Phase.plots provides plots of fishing status versus spawning biomass status.
#' 
#' @param x an R list with output from the assessment models.
#' @param DataName string used in plot titles.  Defaults to argument \code{x}.
#' @param draft modifies plots for use in a report.  When \code{FALSE} main titles 
#' are omitted.
#' @param graphics.type a vector of graphics file types to which graphics are saved.  
#' When \code{NULL}, no plots are saved.
#' @param use.color plots are made in grayscale when \code{FALSE}.
#' @param start.drop Number of years at the start of the data to be omitted from
#' plots, as when a model includes an initialization period.
#' @param end.drop Number of years at the end of data to be omitted from
#' plots, as when a model includes a projection period.
#' @param year.pos An integer (= 1, 2, 3, or 4) defining the position of text relative to points. 
#' The text indicates first and last years in the time series. A value of year.pos=0 
#' turns off the text feature.
#' @param from.zero When \code{TRUE}, the Y-axis of each plot 
#' starts at zero.
#' @param legend.pos A text string compatible with the \code{legend} function of \code{R}.
#' Defines the position of the legend (ex. "bottomright", "bottom", etc.). The default
#' NULL places text on the plot itself.
#' @param F.series Character string specifying the name of the F metric to be plotted
#' @param B.series Character string specifying the name of the B (or SSB) metric to be plotted
#' @param F.B.references a list of two character-sting names, the first specifying the
#' fishing reference point and the second the biomass reference point. Insert 
#' a value of NULL if reference points are not desired for plotting.
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
#' Phase.plots(gag)
#' }
#' 
Phase.plots <-
function(x, DataName = deparse(substitute(x)), draft = TRUE,
    graphics.type = NULL, use.color = TRUE, start.drop = 0, end.drop=0, 
    year.pos=1, from.zero=TRUE, legend.pos=NULL,
    F.series="F.full", B.series="SSB", F.B.references=list("Fmsy","msst"))
{
  if (! ("t.series" %in% names(x)))
  {  Errmsg = (paste("Component ", deparse(substitute(x)), "$t.series not found.",
                     sep = ""))
     warning(Errmsg, immediate. = TRUE)
     return(invisible(-1))
  }
  if (! ("parms" %in% names(x)))
  {   Errstring = (paste("Component ", deparse(substitute(x)), "$parms not found.",
                         sep = ""))
      warning(Errstring, immediate. = TRUE)
      return(invisible(-1))
  }
  
  # Find column labeled 'year':
  yrcol <- grep("year",names(x$t.series))
  if (length(yrcol) != 1)
  {  warning("No year column found in x$t.series", immediate. = TRUE)
     return(invisible(-1))
  }
  # Find column labeled F.series:
  if (!(F.series %in% names(x$t.series)))    
  { Errmsg=paste("No ",F.series, " column found in x$t.series", sep="") 
    warning(Errmsg, immediate. = TRUE)
     return(invisible(-1))
  }
  if (!(B.series %in% names(x$t.series)))    
  { Errmsg=paste("No ",B.series, " column found in x$t.series", sep="") 
    warning(Errmsg, immediate. = TRUE)
    return(invisible(-1))
  }
  if (!is.null(F.B.references))    
  { if (!is.null(F.B.references[[1]]) && !(F.B.references[[1]] %in% names(x$parms)))
    {
      Errmsg=paste(F.B.references[[1]], " not found in x$parms", sep="") 
      warning(Errmsg, immediate. = TRUE)
      return(invisible(-1))
    } else {
      hrefstring <- F.B.references[[1]]
      hrefindex <- which(names(x$parms) == F.B.references[[1]])
      Fref <- unlist(x$parms[hrefindex])        
      
    }
    if (!is.null(F.B.references[[2]]) && !(F.B.references[[2]] %in% names(x$parms)))
    {
      Errmsg=paste(F.B.references[[2]], " not found in x$parms", sep="") 
      warning(Errmsg, immediate. = TRUE)
      return(invisible(-1))
    } else {
      hrefstring <- F.B.references[[2]]
      hrefindex <- which(names(x$parms) == F.B.references[[2]])
      Bref <- unlist(x$parms[hrefindex])        
      
    }
  } else {
    Fref<-NULL
    Bref<-NULL
  }
  if ((nrow(x$t.series)-end.drop)<=(start.drop + 1))    
  { Errmsg=paste("Time series requires at least one datum. Check arguments start.drop and end.drop.", sep="") 
    warning(Errmsg, immediate. = TRUE)
    return(invisible(-1))
  }
  
  
  ### Make local copies of data:
  yrndx <- (start.drop + 1):(nrow(x$t.series)-end.drop)
  Fcol=which(names(x$t.series)==F.series)
  Bcol=which(names(x$t.series)==B.series)
  F.B.dat <- x$t.series[yrndx, c(Fcol,Bcol), drop = FALSE]
  remove.na = !(is.na(F.B.dat[,1])|is.na(F.B.dat[,2]))
  yrndx=yrndx[remove.na]
  year <- x$t.series$year[yrndx]
  F.B.dat <- x$t.series[yrndx, c(Fcol,Bcol), drop = FALSE]
  
  # Get colors for graphical objects:
  plot.options = FGGetOptions()
  if (use.color) parlist <- plot.options$color
  else parlist <- plot.options$bw
  clr.points <- parlist$clr.line
  
  ### Setup graphics directory:
    if (! is.null(graphics.type))
    {   write.graphs <- TRUE
        GraphicsDirName <- paste(DataName, "-figs/phase", sep="")
    } else {   
       write.graphs <- FALSE
    }
    ### Set graphics parameters, constants, data structures:
    savepar <- FGSetPar(draft)
    #===========================================================================
    # Plot F v B
        PlotTitle <- ifelse(draft, paste("F v B      Data:", DataName), "")
        if (!is.null(Bref)) {ylim <- range(F.B.dat[,2], Bref)
        } else {ylim <- range(F.B.dat[,2])}  
        if (from.zero) ylim <- range(ylim, 0)
        lab.y <- B.series
        if (!is.null(Fref)) {xlim <- range(F.B.dat[,1], Fref, 0)
        } else {xlim <- range(F.B.dat[,1],0)}  
        par(las=FGSetLas(F.B.dat[,2])) 
        FGTimePlot(F.B.dat[,1], F.B.dat[,2], lab.x = "F", lab.y = lab.y, 
                   use.color = use.color, xlim=xlim,
                   ylim = ylim, main = PlotTitle, FGtype = "circles",
                   href = NULL, hrefnames = NULL )        
        if (!is.null(Fref)) {abline(v=Fref, lwd=2, lty=2, col="gray55")}
        if (!is.null(Bref)) {abline(h=Bref, lwd=2, lty=2, col="gray55")}
        points(F.B.dat[,1], F.B.dat[,2], col = clr.points)
        lines(F.B.dat[,1], F.B.dat[,2], col = clr.points, lty=1)
        points(F.B.dat[length(year),1], F.B.dat[length(year),2], col = clr.points, pch=16)
        
        if (!is.null(Bref)) 
          {ifelse(F.B.references[2]=="msst", Blabel<-"MSST", Blabel<-F.B.references[2])} 
        
          if (is.null(legend.pos)) {
            if (!is.null(Fref))   
            {
              ifelse(xlim[2]==Fref, tpos<-2, tpos<-4)
              text(Fref, ylim[1],labels=F.B.references[1], pos=tpos, cex=0.85, offset=0.35, col="gray55")
            }
            if (!is.null(Bref)) 
            { 
              ifelse(ylim[2]==Bref, tpos<-1, tpos<-3)
              ifelse(nchar(Blabel)<6, x.shift<-0.01, x.shift<-0.08)
              text(xlim[1]+x.shift,Bref,labels=Blabel, pos=tpos, cex=0.85, offset=0.35, col="gray55")
            }
          } else { 
            if (!is.null(F.B.references)) {
              ifelse (is.null(Fref), Fstring<-"", Fstring<-paste("F reference: ",F.B.references[1],sep=""))
              ifelse (is.null(Bref), Bstring<-"", Bstring<-paste("B reference: ", Blabel, sep=""))
              if (Fstring=="") {legend.text<-Bstring
              } else if (Bstring=="") {legend.text<-Fstring
              } else {legend.text=c(Fstring,Bstring)}
              legend(legend.pos, legend=legend.text,bg="white", text.col="gray55")
          }                    
        }
        if (year.pos>0){
            text (x=F.B.dat[1,1], y=F.B.dat[1,2],
                  labels=year[1], pos=year.pos, cex=0.85, offset=0.35)
            text (x=F.B.dat[length(year),1], y=F.B.dat[length(year),2],
                  labels=year[length(year)], pos=year.pos, cex=0.85, offset=0.35)
         }
  
        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
            GraphName = "phase.FvB", graphics.type)
    
#===========================================================================
  # Plot Relative F v B
  if (!is.null(Fref) && !is.null(Bref)) 
    { 
      PlotTitle <- ifelse(draft, paste("Relative F v B      Data:", DataName), "")    
      ylim <- range(F.B.dat[,2]/Bref, 1)
      if (from.zero) ylim <- range(ylim, 0)      
      lab.y <- paste(B.series, " / ", Blabel, sep="")
      xlim <- range(F.B.dat[,1]/Fref, 1, 0) 
      lab.x <- paste("F / ", F.B.references[1], sep="") 
      par(las=FGSetLas(F.B.dat[,2]/Bref)) 
      FGTimePlot(F.B.dat[,1]/Fref, F.B.dat[,2]/Bref, lab.x = lab.x, lab.y = lab.y, 
                 use.color = use.color, xlim=xlim,
                 ylim = ylim, main = PlotTitle, FGtype = "circles",
                 href = NULL, hrefnames = NULL )
      abline(v=1, lwd=2, lty=2, col="gray55")
      abline(h=1, lwd=2, lty=2, col="gray55")
      points(F.B.dat[,1]/Fref, F.B.dat[,2]/Bref, col = clr.points)
      lines(F.B.dat[,1]/Fref, F.B.dat[,2]/Bref, col = clr.points, lty=1)
      points(F.B.dat[length(year),1]/Fref, F.B.dat[length(year),2]/Bref, col = clr.points, pch=16)
      
      if (year.pos>0){
        text (x=F.B.dat[1,1]/Fref, y=F.B.dat[1,2]/Bref,
              labels=year[1], pos=year.pos, cex=0.85, offset=0.35)
        text (x=F.B.dat[length(year),1]/Fref, y=F.B.dat[length(year),2]/Bref,
              labels=year[length(year)], pos=year.pos, cex=0.85, offset=0.35)
      }
      
      if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                                   GraphName = "phase.FvB.rel", graphics.type)
  }
    #===========================================================================
   par(savepar)    # reset graphics device
   return(invisible(0))
} # End of function definition

