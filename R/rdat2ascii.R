#' Rdat conversion program
#'
#' The function \code{rdat2ascii} converts an \code{.Rdat} object into an ASCII
#' file for alternative uses.
#'
#' @param x an R list with output from the assessment models.
#' @param file the output file name and directory
#'
#' @return ASCII file
#'
#' @author M Prager
#' @author E Williams
#' @author K Shertzer
#' @author R Cheshire
#' @author K Purcell
#'
#' @examples \donttest{
#' rdat2ascii(gag)
#' }
#' @export
rdat2ascii <-
function(x, file = paste(deparse(substitute(x)), ".txt", sep = "")) {
   #
   # ARGUMENTS:
   #  x:          R data object to save as ascii
   #  filename:   Name of file to save. Default is name of x with ".txt" extension
   #
   tmp.wid = getOption("width")  # save current width
   options(width = 10000)        # increase output width
   sink(file)                    # redirect output to file
   print(x)                      # print the object
   sink()                        # cancel redirection
   options(width = tmp.wid)      # restore linewidth
   return(invisible(NULL))       # return (nothing) from function
   }

