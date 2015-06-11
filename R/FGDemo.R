FGDemo <- function(display = TRUE, clear = TRUE, graphics.type, open.cmd)
{
  data(gag)

  if (missing(graphics.type)) {
    graphics.type <- switch(.Platform$OS.type, windows = "wmf", "pdf")
  }

  if (display) {
    ########## Open a graphics device & delete plot history ###########
    if (clear == TRUE) graphics.off()
    dev.new(width = 6, height = 4, record = TRUE)
    if (clear == TRUE) .SavedPlots <- NULL
    FGSetDefaults()             # set up default colors, etc.
  }
  else {
    if (missing(open.cmd)) {
      open.cmd <- NULL
      if (.Platform$OS.type == "windows") {
        open.cmd <- "start"
      }
      else {
        # Try to find another command
        for (cmd in c("xdg-open", "open")) {
          if (system(paste("which", cmd, "> /dev/null")) == 0) {
            open.cmd <- cmd
            break
          }
        }
      }
    }

    filename <- paste("fishgraph-plots.", switch(graphics.type, eps = "eps", "pdf"), sep="")
    if (graphics.type == "eps") {
      postscript(file = filename)
    }
    else {
      pdf(file = filename)
      if (graphics.type != "pdf") {
        warning("graphics.type can only be 'pdf' or 'eps' when display is FALSE; assuming 'pdf'")
        graphics.type <- "pdf"
      }
    }
  }

  ########## Call the functions #########################

  # Diagnostics plots:
  Comp.plots(gag, graphics.type = graphics.type, draft = TRUE)

  Comp.yearly.plots(gag, graphics.type = graphics.type, compact = FALSE)
  BSR.time.plots(gag, start.drop = 4, graphics.type = graphics.type)

  Index.plots(gag, DataName = "gag", graphics.type = graphics.type, log.resid = FALSE)

  # Data plots:

  CLD.total.plots(gag, graphics.type = ifelse(graphics.type == "png", graphics.type, c(graphics.type, "png")), first.year = 1960,
      plot.proportion = TRUE)

  Landings.plots(gag, graphics.type = graphics.type)

  Growth.plots(gag, graphics.type = graphics.type, plot.all = TRUE)

  # Results plots:

  F.time.plots(gag, graphics.type = ifelse(graphics.type == "png", graphics.type, c(graphics.type, "png")), start.drop=23, F.references =
     list(a = "F01", b = c("F30", "F40", "F50", "F60")))

  PerRec.plots(gag, graphics.type = graphics.type, use.color = TRUE,
      user.PR = list("spr.biomass", "E.spr"))

  EqRec.plots(gag, graphics.type = graphics.type, use.color = TRUE,
      user.Eq = list("spr", "E.eq"))

  Selectivity.plots(gag, graphics.type = graphics.type)

  StockRec.plots(gag, graphics.type = graphics.type, draw.lowess = TRUE, use.color = TRUE,
      start.drop = 17, rec.model = NULL)

  if (display == FALSE) {
    dev.off()
    if (!is.null(open.cmd)) {
      system(paste(open.cmd, filename))
    }
  }

  ##### Uncomment the following lines to open a second graphics window and illustrate
  ##### the "compact" option of Comp.yearly.plots().

  # dev.new(width = 7.3, height = 10, record = TRUE)    #  FOR COMPACT
  # Comp.yearly.plots(gag, graphics.type = graphics.type, compact = TRUE, print.angle=TRUE)

  ##### Save the data oject into an ASCII file (for spreadsheet users, etc.)
  rdat2ascii(gag)
}
