FGGetPal <-
function(ncolor, use.color)
{   if(use.color) colvec <- rainbow(ncolor, s = 0.55, v = 1, start = 0.0, end = 0.85)
    else colvec <- gray(sqrt(seq(from = 0.05, to = 0.95, length = ncolor)))
    return(colvec)
}

