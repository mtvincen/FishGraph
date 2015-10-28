##################################################################################
###   Sample R program for testing and demonstrating the "FishGraph" collection
###     of R graphics functions for analysis of stock-assessment results.
##################################################################################

##### Start fresh ###########
rm(list=ls(all=TRUE))
graphics.off()
.SavedPlots <- NULL

library(FishGraph)

##### Read in the data from the ASCII .rdat file: #####
##### Commented out in this example because FishGraph already has the gag data set in memory.  
#gag <- dget("gag5.rdat") 

##### Common arguments in FishGraph functions. Convenient to define them once.
ptype=NULL #NULL (no quotes) for no plots saved; other options: "pdf", "wmf", "eps"
dtype="TRUE"  #draft type
ctype="TRUE"  #color type 
########## Open a graphics device ###########
windows(width = 8, height = 8, record = TRUE)

########## Call the functions #########################

Parm.plots(gag, graphics.type=ptype)

gag$parm.tvec=gag$parm.vec            #Rename for FishGraph compatibility
gag$parm.tvec.cons=gag$parm.vec.cons  #Rename for FishGraph compatibility
Bound.vec.plots(gag, draft=dtype, graphics.type=ptype)

Landings.plots(gag, draft=dtype, use.color=ctype, graphics.type=ptype, 
        L.units=c("1000 lb gutted","1000 lb gutted","1000 fish","1000 fish"), D.units="1000 dead fish", L.obs.pre=FALSE)

Index.plots(gag, draft=dtype, use.color=ctype, graphics.type=ptype)


#windows(width = 10, height = 8, record = TRUE)
Comp.plots(gag, draft=dtype, use.color=ctype, graphics.type=ptype, p.corr=T, c.min=0.2)

#windows(width = 8, height = 10, record = TRUE)
Comp.yearly.plots(gag, draft=dtype, use.color=ctype, graphics.type=ptype, plot.neff=FALSE, print.neff=FALSE,
                  compact = TRUE, print.n=TRUE, print.angle=FALSE)

Cohort.plots(gag,draft=dtype, graphics.type=ptype)

Growth.plots(gag, draft=dtype, use.color=ctype, graphics.type=ptype, plot.all = TRUE)

StockRec.plots(gag, draft=dtype, use.color=ctype, graphics.type=ptype, 
               draw.lowess = FALSE, start.drop = 10, units.rec="number age-1 fish")

gag$parms$F40=0.54 #add to parms, calculated post-assessment
gag$parms$F50=0.38 #add to parms, calculated post-assessment
F.time.plots(gag, draft=dtype, use.color=ctype, graphics.type=ptype, 
             F.references=list(a="F40",b=c("Fmsy","F50")))

#windows(width = 6, height = 4, record = TRUE, xpos = 10, ypos = 10)
Selectivity.plots(gag, draft=dtype, use.color=ctype, graphics.type=ptype, plot.points=T)


NFZ.age.plots(gag,draft=dtype, use.color=ctype, graphics.type=ptype, 
              user.plots="N.age.mdyr", start.drop=10)

CLD.total.plots(gag, draft=dtype, use.color=ctype, graphics.type=ptype, first.year = "1965",
                units.CLD.w="1000 lb gutted", CLD.w.references=list(NULL,"msy.klb", NULL),
                plot.proportion = TRUE)

BSR.time.plots(gag, start.drop = 10, draft=dtype, use.color=ctype, graphics.type=ptype, legend.pos="top")

#windows(width = 8, height = 6, record = TRUE)
PerRec.plots(gag, draft=dtype, use.color=ctype, graphics.type=ptype, 
             user.PR = list("SPR", "ypr.lb.gutted"), F.references=list("Fmsy","F40"))

gag$eq.series$L.eq=gag$eq.series$L.eq.gutklb  #Rename for FishGraph compatibility
gag$eq.series$D.eq=gag$eq.series$D.eq.knum    #Rename for FishGraph compatibility
Eq.plots(gag, draft=dtype, use.color=ctype, graphics.type=ptype, 
         F.references=list("Fmsy", "F40"))    

#windows(width = 8, height = 8, record = TRUE)
Phase.plots(gag, start.drop=10, draft=dtype, use.color=ctype, graphics.type=ptype, year.pos=2)



####### object into an ASCII file (for spreadsheet users, etc.)
#rdat2ascii(gag)
#library(xlsReadWrite); 


