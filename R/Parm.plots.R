#' 
#' Graphically displays parameter and bound values for a stock-assessment model runs.
#' 
#' @param x an R list with output from the assessment models.
#' @param DataName string used in plot titles.  Defaults to argument \code{x}.
#' @param graphics.type a vector of graphics file types to which graphics are saved.  
#' When \code{NULL}, no plots are saved.
#' @param bound.tol the tolerance, as percentage of bound range, for flagging estimates close to bounds.

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
#' Parm.plots(gag)
#' }
#' 
Parm.plots <- function(x, DataName = deparse(substitute(x)),
                              graphics.type = NULL, bound.tol=0.01)
#=================================================================================
{

  ### Check for data presence:
  Errstring = ("No parameter data found.  Terminating Parm.plots");
  if (! ("parm.cons" %in% names(x))) stop(Errstring)
  
  ### Check dimensionality of data:
  Errstring = ("Component parm.cons does not have the required dimensions (8 entries for each parameter).
               Terminating Parm.plots");
  if (nrow(x$parm.cons)!=8) {stop(Errstring)}
  
  ### Set up plotting-related stuff:
  savepar <- FGSetPar(draft=TRUE)
  PlotTitle <- ""
  if (! is.null(graphics.type))
  {   write.graphs <- TRUE
      GraphicsDirName <- paste(DataName, "-figs/parms", sep="")
      FGCheckGraphDir(GraphicsDirName)
  } else {write.graphs <- FALSE}
  
  
  #=================================================================================
  ##### plot condensed bounds, estimate, and initial guess #########################
  
  dimnames(x$parm.cons)=list(c("Guess","Lower Bound","Upper Bound","Phase","Prior Mean","Prior Var/-CV","Prior PDF","Estimate"),names(x$parm.cons))
  
  namevec=names(x$parm.cons) #get names of parameters
  numpars=ncol(x$parm.cons)  #get number of parameters to plot
  
  numpages=ceiling(numpars/25) #params per page
  pagebreaks=25*(1:numpages)
  pagebreaksplus=pagebreaks+1
  pagecounter=1
  par(mai=c(0.25,0.25,0.25,0.25))
  plot(1,1,ylim=c(-25,0),xlim=c(0,3),xlab="",ylab="",bty="n",col.axis="transparent",xaxt="n",yaxt="n")
  for(p in 1:numpars){
    
    pp=p
    if (p %in% pagebreaksplus)
    {
      plot(1,1,ylim=c(-25,0),xlim=c(0,3),xlab="",ylab="",bty="n",col.axis="transparent",xaxt="n",yaxt="n")
    }
    
    if (numpages>1){
       for (ipage in 2:numpages) {
          if ((p>pagebreaks[ipage-1]) && (p<=pagebreaks[ipage])) {
            pp=p-pagebreaks[ipage-1]
            pagecounter=ipage
          }
       }      
    }
    
    
    #--get the parameter constraints and estimates---------------------
    pname=namevec[p]
    init=x$parm.cons[1,p]
    lo.b=x$parm.cons[2,p]
    hi.b=x$parm.cons[3,p]
    phase=x$parm.cons[4,p]
    pmean=x$parm.cons[5,p]
    varcv=x$parm.cons[6,p]
    p.pdf=x$parm.cons[7,p]
    est=x$parm.cons[8,p]
    
    ###--plot prior, bounds, estimate, and initial guess ----------------------------------------
    xmin=min(c(init,lo.b,hi.b,est))
    xmax=max(c(init,lo.b,hi.b,est))
    
    txt=0.9  #size of text to use in cex
    
    b.range=xmax-xmin
    b.tol=bound.tol  #tolerance, as percentage of bound range, for flagging estimates close to bounds
       
    text(x=0.9,y=-pp,labels=pname,adj=c(1,0.5),cex=txt,font=2)
    if (phase<=0) {
      if (est>lo.b&&est<hi.b) {text(x=2.1,y=-pp,labels="Fixed Parameter",adj=c(0,0.5),cex=txt,font=2,col="purple")
      } else {text(x=2.1,y=-pp,labels="Fixed, Outside Bounds!",adj=c(0,0.5),cex=txt,font=2,col="red")}
    }   
    if (phase>0) {
      if (est<lo.b||est>hi.b){text(x=2.1,y=-pp,labels="Estimate Outside Bounds!",adj=c(0,0.5),cex=txt,font=2,col="red")}
      else if (est<(lo.b+(b.range*b.tol))){text(x=2.1,y=-pp,labels="Estimate Near Lower Bound",adj=c(0,0.5),cex=txt,font=2,col="orange")}
      else if (est>(hi.b-(b.range*b.tol))){text(x=2.1,y=-pp,labels="Estimate Near Upper Bound",adj=c(0,0.5),cex=txt,font=2,col="orange")}
      else {text(x=2.1,y=-pp,labels="No Bounding Issues",adj=c(0,0.5),cex=txt,font=2,col="green")}
    }
    
    lhgt=0.25  # height of vertical lines
    lines(x=c(1,2),y=c(-pp,-pp),lwd=2)
    lines(x=c((lo.b-xmin)/b.range+1,(lo.b-xmin)/b.range+1),y=c(-pp+lhgt,-pp-lhgt),lwd=4,col="red")
    lines(x=c((hi.b-xmin)/b.range+1,(hi.b-xmin)/b.range+1),y=c(-pp+lhgt,-pp-lhgt),lwd=4,col="red")
    lines(x=c((init-xmin)/b.range+1,(init-xmin)/b.range+1),y=c(-pp+lhgt,-pp-lhgt),lwd=4,col="orange")
    lines(x=c((est-xmin)/b.range+1,(est-xmin)/b.range+1),y=c(-pp+lhgt,-pp-lhgt),lwd=4,col="blue")
    
    legend(x="top",legend=c("Bounds","Initial Guess","Estimate")
           ,col=c("red","orange","blue"),lwd=c(4,4,4),cex=0.9
           ,ncol=3,seg.len=0.9,bg="white")
    
    if(write.graphs) {
      if ((p == pagebreaks[pagecounter]) || (p == numpars)) {
        gname=paste("parms.bounds.page", sprintf("%02.0f", pagecounter), sep = "")
        FGSavePlot(GraphicsDirName, DataName,                      
                 GraphName = gname, graphics.type)
      }
    }
    
  } #end p numpars loop
  
  
  #=================================================================================
  ##### plot prior, bounds, estimate, and initial guess ############################
  par(mfrow=c(2,2),mai=c(0.8,0.8,0.2,0.2))
  numpages=ceiling(numpars/4) #four panels per page
  pagebreaks=4*(1:numpages)
  pagecounter=1

  for(p in 1:numpars){

    if (numpages>1){
      for (ipage in 2:numpages) {
        if ((p>pagebreaks[ipage-1]) && (p<=pagebreaks[ipage])) {pagecounter=ipage}
      }      
    }
    
    #--get the parameter constraints and estimates---------------------
    pname=namevec[p]
    init=x$parm.cons[1,p]
    lo.b=x$parm.cons[2,p]
    hi.b=x$parm.cons[3,p]
    phase=x$parm.cons[4,p]
    pmean=x$parm.cons[5,p]
    varcv=x$parm.cons[6,p]
    p.pdf=x$parm.cons[7,p]
    est=x$parm.cons[8,p]
    
    #---compute number of points to use for plotting prior line-----------------
    numpts=100
    xvec=seq(lo.b,hi.b,(hi.b-lo.b)/numpts)
  
    #---compute the PDFs of the specified prior---------------------------------------
    if(p.pdf==1){
      yvec=rep(0,length(xvec))  
    }else if(p.pdf==2){
      varx=varcv
      if(varcv<0.0){varx=log(1+varcv*varcv)}
      yvec=0.5*(log(xvec/pmean))^2/varx+log(varx)
    }else if(p.pdf==3){
      varx=varcv
      if(varcv<0.0 && pmean!=0.0){varx=(varcv*pmean)^2
      }else if(varx<0.0 && pmean==0.0){varx=-varcv}
      yvec=0.5*(xvec-pmean)^2/varx+log(varx)
    }else if(p.pdf==4){
      varx=varcv
      if(varcv<0.0){varx=(varcv*pmean)^2}      
      ab_iq=pmean*(1-pmean)/varx-1
      alpha=pmean*ab_iq
      beta=(1-pmean)*ab_iq
      yvec=(1-alpha)*log(xvec)+(1-beta)*log(1-xvec)-lgamma(alpha+beta)+lgamma(alpha)+lgamma(beta)
    }else if(p.pdf<1|p.pdf>4){ print(paste("Unknown prior pdf type for parameter -",pname))}
    yvec=-yvec
    yvec=yvec-min(yvec)
    
    ###--plot prior, bounds, estimate, and initial guess ----------------------------------------
    ymin=min(yvec)
    ymax=max(yvec)
    if(ymax<1.0){ymax=1}
    xmax=max(c(xvec,init,lo.b,hi.b,est))
    xmin=min(c(xvec,init,lo.b,hi.b,est))
    ymax=ymax+ymax*0.2
    bckg="transparent"
    par(bg=bckg, new=FALSE, las=FGSetLas(yvec))    
    plot(xvec,yvec,xlim=c(xmin,xmax),ylim=c(ymin,ymax),xlab=pname,ylab="Likelihood",type="l"
         ,lwd=3,cex.lab=1.5,font.lab=2,bg=bckg)
    lines(xvec, yvec,lwd=3)
    abline(v=lo.b,lwd=2,col="red")
    abline(v=hi.b,lwd=2,col="red")
    abline(v=init,lwd=2,col="orange")
    abline(v=est,lwd=2,col="blue")
    #legend(x=xmin,y=ymax+(ymax*0.22),legend=c("Prior","Bound","Guess","Estim")
    if(phase<0){
      legend(x="topleft",legend=c("Prior","Bound","Guess","Fixed")
             ,col=c("black","red","orange","blue"),lwd=c(3,2,2,2),cex=0.7
             ,ncol=2,seg.len=1,bg="gray90")
      text(grconvertX(0.50,"npc"),grconvertY(0.1,"npc"), "Fixed Parameter", cex=1.)
    }
    else{
      legend(x="topleft",legend=c("Prior","Bound","Guess","Estimate")
             ,col=c("black","red","orange","blue"),lwd=c(3,2,2,2),cex=0.7
             ,ncol=2,seg.len=1,bg="white")
    }
    
    if(write.graphs) {
      if ((p == pagebreaks[pagecounter]) || (p == numpars)) {
        gname=paste("parms.lkhd.page", sprintf("%02.0f", pagecounter), sep = "")
        FGSavePlot(GraphicsDirName, DataName,                      
                   GraphName = gname, graphics.type)
      }
    }
    
  } #end p numpars loop
  
    
  par(savepar)
  return(invisible(0))
  
} # close plotting function










