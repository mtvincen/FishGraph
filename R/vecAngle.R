vecAngle <-
function(x, y=c(1,numeric(length(x)-1)), degrees=FALSE)
    # Note: default y is vector (1,0,0, ... ,0)
{
    num <- FGdotProduct(x,y)
    den <- sqrt(FGdotProduct(x,x) * FGdotProduct(y,y))
    if (!degrees) return(acos(num/den))
    else return(acos(num/den) * 180.0 / pi)
}   # end function vecAngle

