#' Plots of age- and length-composition fits
#' test this
#' The function \code{Comp.plots} generates bubble plots of residuals of age- 
#' and length-composition fits for the entire time frame of the assessment. 
#' Bubble areas are scaled to the largest residual within each plot. Optionally,
#' a small inset plot displays the correlation or angular 
#' deviation between observed and predicted values each year. In addition,
#' \code{Comp.plots} generates plots of predicted and observed mean compositions
#' pooled across years.  
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
#' @param p.resid when FALSE, scaled to area of residuals, otherwise scaled to area of the 
#'          Pearson residuals (multinomial).  
#' @param corr when \code{FALSE}, angular deviation is displayed in the inset plot; otherwise, 
#' correlations.
#' @param c.min lower bound on the y-axis range of the inset plot, applies only when corr is \code{TRUE}
#' @param max.bub cex value for maximum bubble size, default is 8.0.
#'
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
#' Comp.plots(gag)
#' }
#'
###########################################################################################
#  R function to make bubble plots of age- or length-composition matrices
#  from fish stock-assessment models.
#  Part of FishGraph.
#  REQUIRES source("fgsupport.r") before running this.
#  E. H. Williams, December 2002
#  Revised by M. H. Prager, December, 2005
#  Revised by A. Stephens, May, 2006
#  Major revision by M. H. Prager, November 2006
#  Revised by R. Cheshire, November 2012
#  Revised by K. Shertzer, September 2015
#  Revised by R. Cheshire, June, 2021																			
#######################################################################################
Comp.plots <- function(x, DataName = deparse(substitute(x)), draft = TRUE,
                       graphics.type = NULL, use.color = TRUE, units = x$info$units.length, 
                       p.resid = FALSE, corr = TRUE, c.min = 0.25, max.bub = 8.0, b.plot = FALSE)
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
#  p.resid - when FALSE, bubbles areas scaled to the largest residuals, otherwise scaled to the 
#          Pearson residuals.
#  corr - TRUE plots correlation coefficients in bottom pane,  FALSE plots
#        angular deviation
#  c.min - lower y axis value for correlation plots, default is 0.25.  All correlations
#          below this value are plotted at the minimum as a different symbol and color.
#  max.bub - cex value for maximum bubble size, default is 8.0
#  b.plot - create boxplot of composition residuals, default is FALSE
#######################################################################################
{  Errstring = ("No composition data found.  Terminating Comp.plots");
if (! ("comp.mats" %in% names(x))) stop(Errstring)

### Make local copy of needed data components
cm <- x$comp.mats
ts=x$t.series
# Is effective sample size present if p.resid=TRUE
if(p.resid==TRUE){
  if(length(grep('neff',names(ts)))==0) stop("p.resid = TRUE requires effective sample size 'neff' components in x$t.series.")
}
# Is number of columns odd?  This is a problem -- they should be in pairs!
if ( (length(cm) %% 2) != 0 ) stop("Odd number of matrices found in Comp.plots!\n")

### Set graphics parameters
plot.options = FGGetOptions()
savepar <- FGSetPar(draft)

### If writing graphics files, make sure there is a directory for them:
if (! is.null(graphics.type))
{  write.graphs <- TRUE
GraphicsDirName <- paste(DataName, "-figs/comp", sep="")
FGCheckGraphDir(GraphicsDirName)
}
else
{  write.graphs <- FALSE
}

### Set the dimensions for splitting the graphics screen into two plotting regions:
smatrix <- rbind(c(0.0, 1.0, 0.25, 1.0), c(0.0, 1.0, 0.0, 0.25))

nplots <- length(cm) %/% 2               # integer division operator
# Added this as a short cut to control colors without having to reference outside controls.

if (use.color)
{  clr.pos <- plot.options$color$clr.pos    # color for positive residuals
clr.neg <- plot.options$color$clr.neg    # color for negative residuals
clr.ang <- plot.options$color$clr.ang    # color for angle plot
col.obsd <- plot.options$color$clr.obsd
col.pred <- plot.options$color$clr.pred
Y1Col  <- plot.options$color$clr.line
}  else
{  clr.pos <- plot.options$bw$clr.pos    # color for positive residuals
clr.neg <- plot.options$bw$clr.neg    # color for negative residuals
clr.ang <- plot.options$bw$clr.ang    # color for angle plot
col.obsd <- plot.options$bw$clr.obsd
col.pred <- plot.options$bw$clr.pred
Y1Col  <- plot.options$bw$clr.line
}

##---Cumulative fit plots-------------------------------------------------------------------   

for (iplot in 1:nplots)
{  
  m1.all <- cm[[iplot*2-1]]         # Matrix of observed
  m2.all <- cm[[iplot*2]]           # matrix of predicted
  
  ### Get various string representations of data series:
  #  gfileroot is used in the name for the graphics file(s):
  gfileroot <- FGTrimName(names(cm)[iplot * 2], removePrefix = 0, removeSuffix = 1)
  gfilename <- paste("pooled.",gfileroot, sep="")
  # titleroot is used as part of the plot title:
  titleroot <- paste("Fishery: ", gfileroot)
  
  #Exclude those yrs that don't make the min sample size requirement, designated by n<0
  nname<-paste(gfileroot,".n", sep="")
  if (nname%in%names(x$t.series)) {
    nseries<-x$t.series[,names(x$t.series)==nname]
    yrs.include<-x$t.series$year[nseries>0]
    m1<-m1.all[rownames(m1.all)%in%yrs.include,]
    m2<-m2.all[rownames(m2.all)%in%yrs.include,]
  } else {
    m1<-m1.all 
    m2<-m2.all
  }
  
  ### Set Y-axis title according to data type:
  if(substr(gfileroot, 1, 1)  == "l"){title.y <- FGMakeLabel("Length bin", units)
  }else{title.y <- "Age class"}
  
  ob.m=apply(m1,2,mean)
  pr.m=apply(m2,2,mean)
  ymax=max(c(ob.m,pr.m))
  x.vec=as.numeric(dimnames(m1)[[2]])
  #plot(x=x.vec,y=ob.m,type="b",col=1,lwd=2,cex=2,ylim=c(0,ymax),xlab=title.y,ylab="Mean Composition",main=titleroot)
  par(las=FGSetLas(c(ob.m, pr.m)))
  plot(x=x.vec,y=ob.m,col=NULL,ylim=c(0,ymax),xlab=title.y,ylab="Pooled composition",main=titleroot)
  #polygon(x=c(min(x.vec),x.vec,max(x.vec)),y=c(0,ob.m,0),border="lightgray",col="lightgray")
  grid(col = "lightgray", lty = 1)     
  points(x=x.vec,y=ob.m,col=col.obsd,pch=21, cex = 1.25, lwd=2)
  lines(x=x.vec,y=pr.m,col=Y1Col,lwd=2,type="l"); points(x=x.vec,y=pr.m,col=Y1Col,lwd=2, pch=16)
  #legend(x="topright",bg="white",legend=c("Observed","Predicted"),lwd=2,col=c(col.obsd, Y1Col),lty=c(-1,1),pch=c(1,16))
  if (write.graphs) FGSavePlot(GraphicsDirName, DataName, GraphName = gfilename,
                               graphics.type)
}
#-------------------------------------------------------------------------------------------      

for (iplot in 1:nplots)
{  #split.screen(smatrix)
  m1.all <- cm[[iplot*2-1]]         # Matrix of observed
  m2.all <- cm[[iplot*2]]           # matrix of predicted
  
  ### Get various string representations of data series:
  #  gfileroot is name for the graphics file(s):
  gfileroot <- FGTrimName(names(cm)[iplot * 2], removePrefix = 0, removeSuffix = 1)
  # titleroot is used as part of the plot title:
  titleroot <- paste("Fishery: ", gfileroot)
  #get effective sample size for calculating pearson residuals
  
  nname<-paste(gfileroot,".n", sep="")
  if (nname%in%names(x$t.series)) {
    nseries<-x$t.series[,names(x$t.series)==nname]
    yrs.include<-x$t.series$year[nseries>0]
    m1<-m1.all[rownames(m1.all)%in%yrs.include,]
    m2<-m2.all[rownames(m2.all)%in%yrs.include,]
  } else {
    m1<-m1.all 
    m2<-m2.all
  }
  
  ### Set Y-axis title according to data type:
  if(substr(gfileroot, 1, 1)  == "l") title.y <- FGMakeLabel("Length bin", units)
  else title.y <- "Age class"
  
  ## Get coordinates of bubbles:
  irn <- as.integer(rownames(m1))                        # year names
  x1 <- as.integer(rep(irn, ncol(m1)))                   # year names
  y1 <- sort(rep(as.numeric(colnames(m1)), nrow(m1)))    # age- or length-class names
  
  ### Get size and color of the bubbles:
  if(p.resid){
    wt=ts[names(ts)==paste(gfileroot,"neff",sep=".")] #get sample sized associated with each year
    wt=wt[wt>0&is.na(wt)==FALSE] 
    z1=(m1-m2)/sqrt(m2*(1-m2)/wt)   #pearson residuals
  }
  else {z1 <- c(m1 - m2)} # Residuals 
  z2 <- max.bub*(sqrt(abs(z1))/sqrt(max(abs(z1))))  ###plots area of bubble
  colvec <- ifelse(z1 < 0.0, clr.neg, clr.pos)
  
  ### Compute angular deviation for each year using the formula:
  # angle - arccos (dotprod(a,b)/(sqrt(dotprod(a,a) * dotprot(b,b)))
  m12dp <- apply(m1*m2, 1, sum)   # This returns a vector of length nyear
  m11dp <- apply(m1*m1, 1, sum)   # Ditto
  m22dp <- apply(m2*m2, 1, sum)   # Ditto
  angdev = acos(m12dp/sqrt(m11dp*m22dp)) * 180.0 / pi   # Converts to degrees
  fit.metric=rep(0,dim(m1)[1])
  for (i in 1:length(fit.metric)){
    fit.metric[i]=cor(m1[i,],m2[i,],method='pearson')
  }
  # Note: Set x limits so axis range is >= 4
  # This prevents plotting fractional years (e.g. 1995.5)
  xmin = irn[1]
  xmax = max(max(irn), xmin + 4)
  layout(matrix(c(1,2,3),3,1),heights=c(1,4,1))
  par(cex = 1, cex.main = 1, cex.axis = 0.85)
  {  if (draft) par(mar = c(0, 4, 3, 1 ))
    else par(mar = c(0, 4, 1, 1))
  }
  bub.scale=c(max.bub,max.bub*0.75,max.bub*0.5,max.bub*0.25,max.bub*0.1)
  legx=c(1,2,3,4,5)
  legy=rep(1,length(bub.scale))
  find.bub.label=function(x){x*max(abs(z1))/max.bub}
  bub.label=round(find.bub.label(bub.scale),2)
  
  plot(legx,legy,pch=21,cex=bub.scale,ylim=c(0.8,1.2),xlim=c(0,7),xaxt="n",
       yaxt="n",xlab="",ylab="",bty="n")
  text(1:5,rep(0.85,6),bub.label)
  if (draft)
  {  if (use.color)
    title(main = FGMakeTitle(paste(titleroot, "    Orange: underestimate"),
                             DataName))
    else
      title(main = FGMakeTitle(paste(titleroot, "    Light: underestimate"),
                               DataName))
  }   #end if draft
  
  
  par(mar=c(0,4,0,1))
  #### Draw the main (bubble) plot:
  plot(x1, y1, xlab = "", ylab = title.y, type = "n", las = 1, xaxt = "n",
       xlim = c(xmin, xmax))
  grid(col = "lightgray", lty = 1)
  points(x1, y1, cex = z2, col = 1, bg = colvec, pch = 21)
  par(mar=c(3,4, 0 , 1 ))
  if(corr==FALSE){
    plot(irn, angdev, xlab = "Year", xaxt = "n", ylab = "Ang.dev.",
         ylim = c(0, 90), axes = FALSE, frame.plot = TRUE, type = "n",
         xlim = c(xmin,xmax))
    axis(side = 2, at = seq(from = 0, to = 90, by = 10), las = 1,
         tck = 1, col = "gray75", lty = "dotted", labels = FALSE)
    axis(side = 2, at = c(20), tck = 1, col = "gray55", labels = FALSE)
    axis(side = 2, at = c(0, 30, 60, 90), las = 1, labels = TRUE, cex.axis = 0.75 )
    box()
    points(irn, angdev, pch = 18, col = clr.ang, cex = 1.3)
  }else
  {
    plot(rownames(m1),fit.metric,ylim=c(c.min,1.1),type="n",pch=16,ylab="Corr.",las=1, cex.lab=0.9)
    grid(col = "lightgray", lty = 1)
    fit.col=rep(0,length(fit.metric))
    fit.pch=rep(0,length(fit.metric))
    for (i in 1:length(fit.metric)){
      
      if(fit.metric[i]<c.min) {fit.pch[i]=13; fit.col[i]="red"; fit.metric[i]=c.min}
      else{fit.pch[i]=18; fit.col[i]="black"; fit.metric[i]=fit.metric[i]}}
    points(rownames(m1),fit.metric,pch=fit.pch,col=fit.col)
  }
  if (write.graphs) FGSavePlot(GraphicsDirName, DataName, GraphName = gfileroot,
                               graphics.type)
}     # end (for ....)
# #--------------boxplots--------------------------------------------------------------      
   par(savepar)
   par(mar=c(4,4,2,2))
   par(las=0)
   par(oma=c(2,2,0,0))
   if(b.plot == TRUE){
    for (iplot in 1:nplots)
    {  
      m1.all <- cm[[iplot*2-1]]         # Matrix of observed
      m2.all <- cm[[iplot*2]]           # matrix of predicted

     ### Get various string representations of data series:
     #  gfileroot is name for the graphics file(s):
     gfileroot <- FGTrimName(names(cm)[iplot * 2], removePrefix = 0, removeSuffix = 1)
     gfilename <- paste("boxplot.",gfileroot,sep="")
     # titleroot is used as part of the plot title:
     titleroot <- paste("Fishery: ", gfileroot)
     #get effective sample size for calculating pearson residuals
   #   
     nname<-paste(gfileroot,".n", sep="")
     if (nname%in%names(ts)) {
       nseries<-ts[,names(ts)==nname]
       yrs.include<-ts$year[nseries>0]
       m1<-m1.all[rownames(m1.all)%in%yrs.include,]
       m2<-m2.all[rownames(m2.all)%in%yrs.include,]
     } else {
       m1<-m1.all
       m2<-m2.all
     }
   #   
     ### calc resids
     if(p.resid){
       wt=ts[names(ts)==paste(gfileroot,"neff",sep=".")]
       wt=wt[wt>0&is.na(wt)==FALSE]
       z1=(m1-m2)/sqrt(m2*(1-m2)/wt)
       ylabresidtxt="Pearson Residuals"   #pearson residuals
     } else {z1 <- m1 - m2
     ylabresidtxt="Deviance Residuals"} # Residuals

     ### Set Y-axis title according to data type:
     if(substr(gfileroot, 1, 1)  == "l") {title.x <- FGMakeLabel("Length bin", units)
      } else {title.x <- "Age class"}

     par(cex = 1, cex.main = 1, cex.axis = 0.85)
     {  if (draft) par(mar = c(2, 4, 3, 1 ))
       else par(mar = c(2, 4, 1, 1))
     }

     # if (draft)
     # { title(main = FGMakeTitle(titleroot,DataName))
     # }   #end if draft
    par(mfrow=c(2,1))
      boxplot(z1,xlab=title.x,main=titleroot,ylab="")
      boxplot(t(z1),xlab='Year',main="",ylab="")
      mtext(ylabresidtxt,side=2,outer=TRUE,line=0)
      if (write.graphs) FGSavePlot(GraphicsDirName, DataName, GraphName = gfilename,
                                   graphics.type)
    } }     # end (for ....)			
par(savepar)
return(invisible(NULL))
} 
# End function definition
###########################################################################################