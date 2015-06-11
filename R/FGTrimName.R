FGTrimName <-
function(x, removePrefix = 0, removeSuffix = 0)
    # MHP  December 2006
    # Shorten a string that has segments delimited by dots.
    # x - string to be shortened
    # removePrefix - TRUE to remove first segment
    # removeSuffix - TRUE to remove last segment
{   xparts <- unlist(strsplit(x, ".", fixed = TRUE))
    if (removePrefix + removeSuffix >= length(xparts))
    {   warning("Too much trimming")
        return(NULL)
    }
    else
    {   ss = (1 + removePrefix) : (length(xparts) - removeSuffix)
        return(paste(xparts[ss], collapse="."))
    }
}   # end function FGTrimName

