\name{FishGraph-package}
\alias{FishGraph-package}
\alias{FishGraph}
\docType{package}
\title{
Generates graphics output for stock-assessment models
}
\description{
Use Beaufort Assessment Model output to produce graphics
}
\details{
\tabular{ll}{
Package: \tab FishGraph\cr
Type: \tab Package\cr
Version: \tab 1.05\cr
Date: \tab 2009-08-08\cr
License: \tab No current license \cr
}
\code{FishGraph} is a set of \code{R} functions that generates plots from output of stock assessment models, specifically the Beaufort Assessment Model (BAM).  By calling a few \code{FishGraph} functions, the user can create hundreds of finished plots rapidly.  Plots are displayed on screen and may be saved to files.  The plots are suitable for diagnostic use and for inclusion in manuscripts.  We developed \code{FishGraph} for use during assessment workshop sessions and to ease assembly of assessment reports.
}
\author{
MH Prager, EH Williams, A Stephens, KW Shertzer

Maintainer: Kevin Purcell <kevin.purcell@noaa.gov>
}
\references{
Prager et al. (XXXX) User's guide to \code{FishGraph}: A set of R graphics functions for fishery stock assessment.  US. Department of Commerce, NOAA Technical Memorandum NMFS-SEFSC-xxx.
}
fisheries, stock assessment, graphics
~~ documentation directory ~~
\keyword{ package }
\examples{
BSR.time.plots(gag)
}
