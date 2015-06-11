FGCheckGraphDir <-
function(GraphicsDirName)
{
   if (! file.exists(GraphicsDirName)) {
      ok <- dir.create(GraphicsDirName, recursive = TRUE)
      if (ok) {
         cat("Note: directory", GraphicsDirName, "created for graphics files.\n")
      } else {
         cat("Note: directory", GraphicsDirName, "could not be created")
         return(invisible(NULL))
      }  # end else group
   }  # end file.exists group
}  # end function FGCheckGraphDir

