FGSetLas <-
function(x)
{   # Argument X is the values that make up the Y-axis
    len <- max(nchar(pretty(x), type = "chars"), na.rm = TRUE)
    return(ifelse(len > 4, 0, 1))
}

