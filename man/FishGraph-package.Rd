\name{FishGraph-package}
\alias{FishGraph-package}
\alias{FishGraph}
\docType{package}
\title{
Graphics for fish stock-assessment models
}
\description{
\code{FishGraph} is a set of \code{R} functions that generates plots from output of stock assessment models.
}
\details{
\tabular{ll}{
Package: \tab FishGraph\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2015-10-28\cr
License: \tab none required \cr
}
By calling a few \code{FishGraph} functions, the user can create hundreds of finished plots rapidly.  Plots are displayed on screen and may be saved to files.  The plots are suitable for diagnostic use and for inclusion in manuscripts.  

A user's guide, input data structure, and list of software updates are installed with the package in the \code{doc} folder.  An example script \code{demo/go.r} is also stored in the package directory and creates plots from an included data set on gag grouper. 

List of functions.

\code{Bound.vec.plots:}  Plots of vector quantities that are bounded in their estimation.

\code{BSR.time.plots:}  Time-series plots of stock biomass, spawning stock, and recruitment.

\code{CLD.total.plots:}  Barplots of estimated catch, landings, and discards (in numbers and weight) over time.

\code{Cohort.plots:}  Barplots of age-composition by year and data series with cohorts identified by color.

\code{Comp.plots:}  Bubble plots of residuals of age- and length-composition fits for the entire time frame of the assessment.

\code{Comp.yearly.plots:}  Plots of age- and length-composition fits by year.

\code{Eq.plots:} Plots of quantities at equilibrium as a function of fishing mortality rate F.

\code{F.time.plots:}  Plots of fishing mortality rate F over time.

\code{Growth.plots:}  Plots of length, weight, and other quantities at age.

\code{Index.plots:}  Plots of fits to abundance indices and residual diagnostics.

\code{Landings.plots:}  Time trajectories of landings and discards by fishery.

\code{NFZ.age.plots:}  Barplots of estimated abundance and mortality at age, and bubble plots of estimated abundance at age and biomass at age over time.

\code{Parm.plots:} Diagnostic plots of parameter estimates, priors, initial guesses, and bounds.

\code{PerRec.plots:} Plots of quantities on a per-recruit basis as a function of fishing mortality rate F. 

\code{Phase.plots:}  Plot of estimated fishing rate versus estimated biomass (or spawning biomass).

\code{Selectivity.plots:}  Plots of selectivity curves at age and/or length.

\code{StockRec.plots:}  Plots of stock vs. recruitment and stock vs. recruits per spawner on the logarithmic scale.
} 

\author{
M Prager, E Williams, K Shertzer, R Cheshire, K Purcell

Maintainers: 
Kyle Shertzer <kyle.shertzer@noaa.gov>
Rob Cheshire <rob.cheshire@noaa.gov>
}
\references{
Prager et al. (XXXX) User's guide to \code{FishGraph}: A set of R graphics functions for fishery stock assessment.  US. Department of Commerce, NOAA Technical Memorandum NMFS-SEFSC-xxx.
}

\keyword{ package }

\examples{
See go.r example in demo library installed in package folder.
}
