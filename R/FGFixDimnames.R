FGFixDimnames <-
function(x){
        if (is.null(rownames(x)))
        {   rownames(x) <- 1:dim(x)[1]
            warning("In barplot, missing row names were generated",
                call. = FALSE, immediate. = TRUE)
        }
        if (is.null(colnames(x)))
        {   colnames(x) <- 1:dim(x)[2]
            warning("In barplot, missing column names were generated",
                call. = FALSE, immediate. = TRUE)
        }
        return(x)
    }   # END OF FUNCTION FixDimnames

