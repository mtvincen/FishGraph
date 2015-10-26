FGBarplot <-
function(x, lab.y, use.color, PlotTitle, leg.title="",
        proportion = FALSE)
    {   # Reduce R margin to zero:
        oldmar <- par()$mar
        par(mar = c(oldmar[1:3], 0))
        colvec <- FGGetPal(ncol(x), use.color)
        lab.tick <- as.integer(rownames(FGFixDimnames(x)))
        if (proportion) x <- x / rowSums(x)
        if (leg.title == "Age" && ncol(x) > 22) leg.col <- 2 else leg.col <- 1
        layout(matrix(c(1,2), byrow=TRUE, ncol = 2), widths = c(6,1))
        mp <- barplot(t(x), col = colvec, xlab = "Year", ylab = lab.y,
            axisnames = FALSE, space = 0, las = FGSetLas(rowSums(x)),
            main = "")
        title(main = PlotTitle, cex = 0.9)
        # Get major and minor multiples for choosing labels:
        ntick <- length(mp)
        {   if (ntick < 16) mult = c(2, 2)
            else if(ntick < 41) mult = c(5, 5)
            else if (ntick < 101) mult = c(10, 5)
            else mult = c(20, 5)
        }
        label.index <- which(lab.tick %% mult[1] == 0)
        minor.index <- which(lab.tick %% mult[2] == 0)
        axis(side = 1, at = mp, labels = FALSE, tcl = -0.2)
        axis(side = 1, at = mp[minor.index], labels = FALSE, tcl = -0.4)
        axis(side = 1, at = mp[label.index], labels = lab.tick[label.index], tcl = -0.6)
        par(mar = c(0,0,0,0))
        plot.new()
        legend("center", fill = rev(colvec), cex = 0.85, ncol = leg.col,
            legend = rev(colnames(x)), title = leg.title, inset = c(0.01, 0.01))
        layout(1)
        par(mar = oldmar)
        return(invisible(NULL))
    }   # END OF FUNCTION MakeBarplot

