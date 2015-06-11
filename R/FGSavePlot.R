FGSavePlot <-
function(GraphicsDirName, DataName, GraphName, graphics.type)
{
   if (graphics.type == "pdf" || graphics.type == "eps") {
     cur <- names(dev.cur())
     if (cur == "pdf" || cur == "postscript") {
       # do nothing, since we're already using a pdf/eps device.
       return(invisible(NULL))
     }
   }

   FGCheckGraphDir(GraphicsDirName)
   fn <- paste(GraphicsDirName, "/", DataName, ".", GraphName, sep = "")
   for (i in 1:length(graphics.type))
      {  # Figure out the correct filename. If R version is >= 2.7,
         # the file extension is REQUIRED. If version is less, file extension
         # is NOT USED, as R will append it automatically.
         {  if (getRversion() < "2.7.0") fname <- fn
            else
            fname <- paste(fn, graphics.type[i], sep = ".")
         }
         # Save plot, avoiding user error of graphics.type = "" rather than NULL:
         if (nchar(graphics.type[i]) > 0) {
           if (graphics.type[i] == "pdf") {
             dev.copy2pdf(file = fname)
           }
           else if (graphics.type[i] == "eps") {
             dev.copy2eps(file = fname)
           }
           else {
             savePlot(filename = fname, type = graphics.type[i])
           }
         }
      }
   return(invisible(NULL))
}  # end function FGSavePlot

