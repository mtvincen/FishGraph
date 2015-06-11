FGTimePlot <-
function(x, y, y2 = NULL, lab.x = "Year", lab.y = deparse(substitute(y)),
    href = NULL, hrefnames = NULL, Y1Col = y1col, Y2Col = y2col,
    FGtype = "1line", use.color = TRUE, legend.pos = "topleft", ylim = NULL, plot.options = FGGetOptions(), ...)

{   las.local <- FGSetLas(c(y, y2, ylim))
    nlines <- max(0, length(href))

    ### Set up colors and line types
    if (use.color) parlist <- plot.options$color else parlist <- plot.options$bw
    y1col <- parlist$clr.line
    y2col <- parlist$clr.obsd
    clr.grid <- parlist$clr.grid
    lty.grid <- parlist$lty.grid
    if (use.color)
    {   ltyvec  <- rep("dashed", nlines)
        colvec <- rainbow(nlines, s = 0.9, v = 0.65, start = 0.2, end = 0.8)
        if (nlines == 1) colvec = "gray55"
    }
    else
    {   ltyvec <- c("dashed", "dotdash", "twodash", "431313", "dotted", "22848222")
        colvec  <- rep("gray55", nlines)
    }
    ### Draw axes
    plot(x, y, type = 'n', xlab = lab.x, ylab = lab.y, las = las.local, ylim = ylim, ...)
    #grid()
    grid(col = clr.grid, lty = lty.grid)
    ### Draw reference line(s) and labels(2)
    if (length(href) > 0) for (i in 1:length(href))  ## Test works w/ NULL or length 0
    {   abline(h = href[i], lty = ltyvec[i], lwd = 2, col = colvec[i])
        # For positioning label on reference line,
        # find where smoothed curve and line are most separated:
        # Turn off warnings about NAs while smoothing:
        warnlevel <- getOption("warn")
        options(warn = -1)
        ytmp <- supsmu(x, y, bass = 5)$y
        options(warn = warnlevel)
        maxndx = which.max(abs(ytmp - href[i]))
        # Decide on x-justification of the label:
        xadj = 0.5
        if (maxndx < 3)  xadj <- 0.0
        if (maxndx > length(ytmp) - 2) xadj <- 1.0

        # Decide whether to put label above or below the line:
        {   if (is.na(y[maxndx]) || (y[maxndx] < href[i]))
                yadj <- -0.5
            else
                yadj <- 1.5
        }
        # Write the label if only one, otherwise wait & write a legend:
        if (length(href) == 1)
        {   text(x[maxndx], href[i], labels=hrefnames[i], adj=c(xadj, yadj), cex = 0.9)
        }
    }
    if (length(href) > 1)
    {   legend(legend.pos, inset = 0.03 / par()$pin, legend = hrefnames, lty = ltyvec,
            lwd=2, col = colvec, bg = "white")
    }
    # Now draw the data:
    ty <- switch(FGtype,
            "1line" = "o",
            "2line" = "o",
            "linepoint" = "o",
            "linepointnodots" = "l",
            "stick" = "h",
            "dots" = "p",
            "circles" = "p")
    if (FGtype == "circles") pch <- 1 else pch <- 16
    lines(x, y, lwd = 2, type = ty, pch = pch, col = Y1Col, cex = 1.0, las = las.local)
    if (FGtype == "stick") lines(x, y, lwd = 2, type = "p", pch = 16,
        col = Y1Col, cex = 1.0)
    # Now plot the second data series
    if (FGtype == "2line") lines(x, y2, lwd = 2, type = "b", pch = 1,
        col = Y2Col, cex = 1.5)
    if ((substr(FGtype, 1, 9) == "linepoint") && (! is.null(y2)))
        points(x, y2, lwd = 2, pch = 1, col = Y2Col, cex = 1.5)
}

