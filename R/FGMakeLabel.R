FGMakeLabel <-
function(base.title, units)
{
   if (is.null(units) || is.na(units))
      {  mytitle <- base.title }
   else
      {  mytitle <- paste(base.title, " (", units, ")", sep = "") }
   return(mytitle)
}

