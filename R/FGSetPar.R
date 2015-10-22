FGSetPar <-
function(draft)
{  # This function is called for its side effects.
   # It sets margins for FishGraph plots
   savepar <- par(no.readonly = TRUE)
   par(cex = 1, cex.main = 1, cex.axis = 0.9, font.lab = 1, font.main = 2, mfrow=c(1,1))
   if(draft)
   {  par(mar = c(4.5, 5, 2, 1))}
   else
   {  par(mar = c(4.5, 5, 1, 1))}
   return(savepar)
}  # End of function FGSetPar

