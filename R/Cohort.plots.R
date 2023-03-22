#' Plots of age-composition data observed and model predicted with color coding by cohort
#'
#' The function \code{Cohort.plots} generates bar plot of observed age composition data
#' with bars colored to track cohorts through time
#' and model predicted values indicated by black dots and connected lines.
#' Color coding tracks cohorts, accounting for skipped years;
#' colors may be recycled depending on the number of ages.
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
#' Cohort.plots(gag)
#' }
#' @export
Cohort.plots <-
function(x, DataName = deparse(substitute(x)), draft = TRUE, graphics.type = NULL)
#######################################################################################
#  ARGUMENTS:
#  x - an R list with output from the assessment models
#     The list x must have a component x$comp.mats that is a list of matrices
#     These matrices must be found in pairs.  First, xxxx.ob, then xxxx.pr.
#  DataName - a string representation an identifier for the data (run) in use.
#  draft - if TRUE the figure has a main title
#  graphics.type - a character vector with graphics-file types
#
#######################################################################################
{  Errstring = ("No composition data found.  Terminating Cohort.plots");
   if (! ("comp.mats" %in% names(x))) stop(Errstring)

   ### Make local copy of needed data components
   cm <- x$comp.mats

   # Is number of columns odd?  This is a problem -- they should be in pairs!
   if ( (length(cm) %% 2) != 0 ) stop("Odd number of matrices found in Cohort.plots!\n")

   ### Set graphics parameters
   savepar <- FGSetPar(draft)

   ### If writing graphics files, make sure there is a directory for them:
   if (! is.null(graphics.type))
   {write.graphs <- TRUE
      GraphicsDirName <- paste(DataName, "-figs/compyr", sep="")
   } else {write.graphs <- FALSE}

   nplots <- length(cm) %/% 2               # integer division operator


   for (iplot in 1:nplots)
   {
      m1.all <- cm[[iplot*2-1]]         # Matrix of observed
      m2.all <- cm[[iplot*2]]           # matrix of predicted

      ### Get various string representations of data series:
      #  gfileroot is name for the graphics file(s):
      gfileroot <- FGTrimName(names(cm)[iplot * 2], removePrefix = 0, removeSuffix = 1)
      # titleroot is used as part of the plot title:
      gfileroot2 <- FGTrimName(names(cm)[iplot * 2], removePrefix = 1, removeSuffix = 1)
      titleroot <- paste("Fishery:", gfileroot2)

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

      # select only age comp matrices
      if(substr(gfileroot, 1, 1)  == "a" & !is.null(dim(m1)))
      {
        ob=m1
        pr=m2

        # get dimensions for plotting (max proportion, max age, and age vector)
        p.max=max(c(ob,pr))*1.05
        a.max=max(as.numeric(dimnames(ob)[[2]]))
        a.vec=as.numeric(dimnames(ob)[[2]])

        # plot fixed at 3 columns, with number of rows decided by dimension of matrix
        n.rows=ceiling(nrow(ob)/3)
        if(n.rows<3){n.rows=3}

        layout(cbind(1:n.rows,(n.rows+1):(n.rows*2),(n.rows*2+1):(n.rows*3)))
        par(mai=c(0,0,0.02,0.02), oma=c(4,4,4,0.2), las=1)
        #vector for rotating colors to follow cohorts
        col.vec.diff=c(0,diff(as.numeric(dimnames(ob)[[1]]))-1)
        col.vec=rep(c(ncol(ob):1),10)[1:(nrow(ob)+sum(col.vec.diff))]
        col.vec.skip=cumsum(col.vec.diff)

        for(i in 1:nrow(ob))
        {
          x.bar=barplot(ob[i,],ylim=c(0,p.max),axes=F,axisnames=F,col=rainbow(ncol(ob))[c(col.vec[i+col.vec.skip[i]]:ncol(ob),1:(col.vec[i+col.vec.skip[i]]-1))])
          box()
          if(i<=n.rows){axis(2)}
          if(i==n.rows|i==(n.rows*2)|i==nrow(ob)){axis(1,at=x.bar,labels=a.vec)}
          text(x=a.max,y=p.max*0.85,labels=dimnames(ob)[[1]][i], cex=1.5)
          lines(x=x.bar,y=pr[i,],type="b",pch=16,lwd=1,cex=1.5)
        }
        mtext(text="Proportion",side=2,cex=1.,outer=T,line=2.5,las=0)
        mtext(text="Age",side=1,cex=1.,outer=T,line=2.5)
        if (draft)
           {mtext(text=paste(titleroot,", Observed (bars), Predicted (dots)",sep=""),side=3,cex=1.2,outer=T,line=2.5)}

        if (write.graphs) FGSavePlot(GraphicsDirName, DataName,
                                     GraphName = paste("cohort.",gfileroot2,sep=""),
                                     graphics.type)
      }  # end age comp subset

   }     # end (for ....)
   par(savepar)
   return(invisible(NULL))
}     # End function definition

