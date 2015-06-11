#' Parameter and bounds plots for stock-assessment models
#' 
#' Graphically displays parameter and bound values for a stock-assessment model runs.
#' 
#' @param x an R list with output from the assessment models.
#' @param DataName string used in plot titles.  Defaults to argument \code{x}.
#' @param draft modifies plots for use in a report.  When \code{FALSE} main titles 
#' are omitted.
#' @param graphics.type a vector of graphics file types to which graphics are saved.  
#' When \code{NULL}, no plots are saved.
#' @param use.color plots are made in grayscale when \code{FALSE}
#' @param bound.tol the tolerance, as percentage of bound range, for flagging estimates close to bounds.

#' 
#' @return Graphics
#' 
#' @author Erik H. Williams
#' @author KM Purcell

#' 
#' @examples \donttest{
#' Param.bounds.plots(gag)
#' }
#' 
Parm.bounds.plots <- function(x, DataName = deparse(substitute(x)),
                              draft = TRUE, graphics.type = NULL, use.color = TRUE,
                              bound.tol=0.01)
#=================================================================================
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
  
  
  
dimnames(x$parm.cons)=list(c("Guess","Lower Bound","Upper Bound","Phase","Prior Mean","Prior Var/-CV","Prior PDF","Estimate"),names(x$parm.cons))
  

  ###### get number of parameters to plot ################################################
  namevec=names(x$parm.cons)
  numpars=ncol(x$parm.cons)
  
  ##### plot condensed bounds, estimate, and initial guess #######################################
  par(mai=c(0.25,0.25,0.25,0.25))
  plot(1,1,ylim=c(-25,0),xlim=c(0,3),xlab="",ylab="",bty="n",col.axis="transparent",xaxt="n",yaxt="n")
  for(p in 1:numpars){
    
    pp=p
    if(p==26||p==51||p==76)
    {
      plot(1,1,ylim=c(-25,0),xlim=c(0,3),xlab="",ylab="",bty="n",col.axis="transparent",xaxt="n",yaxt="n")
    }
    if(p>25){pp=p-25}
    if(p>50){pp=p-50} 
    if(p>75){pp=p-75}
    
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
    
    
    # The order was restructured to allow for "fixed, Outside Bounds"
    text(x=0.9,y=-pp,labels=pname,adj=c(1,0.5),cex=txt,font=2)
    if(phase<0&&est<lo.b||est>hi.b){text(x=2.1,y=-pp,labels="Fixed Parameter, Outside Bounds!",adj=c(0,0.5),cex=txt,font=2,col="red")}
    else if(phase<0){text(x=2.1,y=-pp,labels="Fixed Parameter",adj=c(0,0.5),cex=txt,font=2,col="purple")}
    else if(est<lo.b||est>hi.b){text(x=2.1,y=-pp,labels="Estimate Outside Bounds!",adj=c(0,0.5),cex=txt,font=2,col="red")}
    else if(est<(lo.b+(b.range*b.tol))){text(x=2.1,y=-pp,labels="w/in tolerance of Lower Bound",adj=c(0,0.5),cex=txt,font=2,col="orange")}
    else if(est>(hi.b-(b.range*b.tol))){text(x=2.1,y=-pp,labels="w/in tolerance of Upper Bound",adj=c(0,0.5),cex=txt,font=2,col="orange")}
    else{text(x=2.1,y=-pp,labels="No Bounding Issues",adj=c(0,0.5),cex=txt,font=2,col="green")}
    
    lhgt=0.25  # height of vertical lines
    lines(x=c(1,2),y=c(-pp,-pp),lwd=2)
    lines(x=c((lo.b-xmin)/b.range+1,(lo.b-xmin)/b.range+1),y=c(-pp+lhgt,-pp-lhgt),lwd=4,col="red")
    lines(x=c((hi.b-xmin)/b.range+1,(hi.b-xmin)/b.range+1),y=c(-pp+lhgt,-pp-lhgt),lwd=4,col="red")
    lines(x=c((init-xmin)/b.range+1,(init-xmin)/b.range+1),y=c(-pp+lhgt,-pp-lhgt),lwd=4,col="orange")
    lines(x=c((est-xmin)/b.range+1,(est-xmin)/b.range+1),y=c(-pp+lhgt,-pp-lhgt),lwd=4,col="blue")
    
    legend(x="top",legend=c("Bounds","Guess","Estimate")
           ,col=c("red","orange","blue"),lwd=c(4,4,4),cex=0.9
           ,ncol=3,seg.len=0.9,bg="white")
    
  } #end p numpars loop
  
  
  
  
  ##### plot prior, bounds, estimate, and initial guess ###################################
  par(mfrow=c(2,2),mai=c(0.7,0.7,0.2,0.2))
  for(p in 1:numpars){
    
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
      if(varcv<0.0){varx=(varcv*pmean)^2}
      varx=varcv
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
    par(bg=bckg,new=FALSE)
    plot(xvec,yvec,xlim=c(xmin,xmax),ylim=c(ymin,ymax),xlab=pname,ylab="Likelihood",type="l"
         ,lwd=3,cex.lab=1.5,font.lab=2,bg=bckg)
    grid()
    abline(v=lo.b,lwd=2,col="black")
    abline(v=hi.b,lwd=2,col="black")
    abline(v=init,lwd=2,col="orange")
    abline(v=est,lwd=2,col="blue")
    #legend(x=xmin,y=ymax+(ymax*0.22),legend=c("Prior","Bound","Guess","Estim")
    if(phase<0){
      legend(x="topleft",legend=c("Prior","Bound","Guess","Fixed")
             ,col=c("black","red","orange","blue"),lwd=c(3,2,2,2),cex=0.7
             ,ncol=4,seg.len=0.7,bg="gray")
      text(grconvertX(0.70,"npc"),grconvertY(0.95,"npc"), "Fixed Param", cex=1.5)
    }
    else{
      legend(x="topleft",legend=c("Prior","Bound","Guess","Fixed")
             ,col=c("black","red","orange","blue"),lwd=c(3,2,2,2),cex=0.7
             ,ncol=4,seg.len=0.7,bg="white")
    }
    
  } #end p numpars loop
  
  
  
  ##### plot bounds, estimate, and initial guess of dev vectors ###################################
  ###### get number of vectors to plot ################################################
  par(mfrow=c(1,1),mai=c(1.5,1,1.5,0.5))
  
  namevec=names(x$parm.vec)[-1]
  numvecs=ncol(x$parm.vec)-1
  nameveccons=names(x$parm.vec.cons)
  numveccons=ncol(x$parm.vec.cons)
  
  for(p in 1:numvecs)
  {
    name=namevec[p]
    colnum=sum(as.numeric(name==nameveccons)*c(1:numveccons))
    xdat=x$parm.vec[,1]
    ydat=x$parm.vec[,1+p]
    ymin=min(na.omit(c(x$parm.vec.cons[,colnum][1],ydat)))
    ymax=max(na.omit(c(x$parm.vec.cons[,colnum][2],ydat)))
    plot(xdat,ydat,ylim=c(ymin,ymax),xlab="Year",ylab="Dev",main=paste("Vector:",name),cex=1,lwd=2,pch=16)
    grid()
    abline(h=ymin,col=2,lwd=2)
    abline(h=ymax,col=2,lwd=2)  
  } # end numvecs loop
  
  par(savepar)
  return(invisible(0))
  
} # close plotting function










