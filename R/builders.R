# This file holds functions associated with assembling figures on a page

#' @title build.page
#' @description  Takes page dimensions, figure layout dimenesions and an ordered list of grobs/ggplot objects orients them on a page
#' @inheritParams graphic.params

build.page <- 
  function (
    interior.h=c(1/3,1/3,1/3),
    interior.w=c(.5, .25, .25),
    ncol=3, nrow=4,
    interior= list(synced.fps[[1]], dtest1, ttest1, 
                   synced.fps[[2]], dtest2, ttest2, 
                   synced.fps[[3]], dtest3, ttest3, 
                   synced.fps[[4]], dtest4, ttest4),
    test.dim=FALSE,
    page.height=8.5,
    page.width=11,
    right.margin=.75,
    left.margin=.75,
    top.margin=1.4-.5,
    bottom.margin=1.75-.5)
{
    if(sum(interior.h) !=1) return("Argument interior.h is not equal to 1.")
    if(sum(interior.w) !=1) return("Argument interior.w is not equal to 1.")
    if(length(interior.h) != nrow || length(interior.w) != ncol) return(cat("Check arguments: page.heights/page.widths does not correspond with ncol/nrow."))
    
    page.widths <- unit(c(right.margin, interior.w*(page.width-right.margin-left.margin), left.margin), units="inches")
    page.heights <- unit(c(top.margin, interior.h*(page.height - top.margin - bottom.margin), bottom.margin), units="inches")
    
    if (test.dim == TRUE) {
      grid.show.layout(grid.layout(nrow = nrow + 2, ncol = ncol + 2, 
                                   heights = page.heights, widths = page.widths))
    }
    if(test.dim==FALSE){
    fill.it <- function(int.nrow = nrow, int.ncol = ncol, interior.list = interior, 
                        heights = page.heights, widths = page.widths) {
      padded <- list()
      for (i in 1:((int.ncol + 2) * (int.nrow + 2))) {
  #   blankPanel <<- grid.rect(gp=gpar(col="white"), draw=FALSE) # From default settings
        padded[[length(padded) + 1]] <- grid.rect(gp=gpar(col="white"), draw=FALSE)
      }
      names(padded) <- rep("blankPanel", (int.nrow + 2) * (int.ncol + 
                                                             2))
      for (i in 1:length(interior.list)) {
        padded[[((i - 1)%%(int.ncol)) + 2 + ((i - 1)%/%int.ncol + 
                                               1) * (int.ncol + 2)]] <- interior.list[[i]]
        names(padded)[((i - 1)%%(int.ncol)) + 1 + ((i - 1)%/%int.ncol + 
                                                     1) * (int.ncol + 2)] <- paste("Plot", i)
      }
      padded[[length(padded) + 1]] <- int.ncol + 2
      names(padded)[length(padded)] <- "ncol"
      padded[[length(padded) + 1]] <- int.nrow + 2
      names(padded)[length(padded)] <- "nrow"
      padded[[length(padded) + 1]] <- heights
      names(padded)[length(padded)] <- "heights"
      padded[[length(padded) + 1]] <- widths
      names(padded)[length(padded)] <- "widths"
      return(padded)
    }
    args.list <- fill.it(int.nrow = nrow, int.ncol = ncol, interior.list = interior, 
                         heights = page.heights, widths = page.widths)
    do.call(grid.arrange, args.list)
}
  }

#' @title annotate.page
#' @description Adds titles, headers, and footers
#' @param title vector of title lines
#' @param urh vector for upper right headers
#' @param ulh vector for upper left headers
#' @param fnote vector of 5 footnotes. 5th row is traditionally reserved for filepath, table reference and time stamp. Populate from bottom up. 
#' @param fnote.buffer fine-control of vertical position
#' @param header.buffer fine-control of vertical position
#' @param fignum.buffer fine-control of vertical position
#' @param title.buffer fine-control of vertical position
#' @param add.fignum logical
annotate.page <- function (
  page.height = 8.5, 
  page.width = 11, 
  top.margin = 1 - 0.5, 
  bottom.margin = 1 - 0.5, 
  right.margin = 0.75, 
  left.margin = 0.75, 
  foot.size = 10, 
  head.size = 10, 
  title.size = 14, 
  add.fignum = TRUE, 
  fnote.buffer = 0, 
  header.buffer = 0, 
  fignum.buffer = 1, 
  title.buffer = 2, 
  fignum = "1.100", 
  title = list(
    "Title Line 1: Printing atop of graphic to show location", 
    "Title Line 2: In practice ", 
    "Title Line 3: Graphic region height can be shunken", 
    "Title Line 4: to accomdate multiple title lines"), 
  ulh = list(
    "Upper Left Header 1",
    "Upper Left Header 2", 
    "Upper Left Header 3"), 
  urh = list("Upper Right Header 1", 
             "Upper Right Header 2", 
             "Upper Right Header 3"), 
  fnote = list(
    "Footnote1: Printing all footnote levels to demonstrate position",   
    "Footnote2: In practice, graphic region height can be increased when less footnotes are needed.",       
    "Footnote3", "Footnote4", "Footnote5: reserved for filepath and reference to table"), 
  fileloc = "gfileloc", 
  override = "", addTime=TRUE) 
{
  time.stamp <- ifelse(addTime==TRUE,toupper(format(Sys.time(), tz = "GMT", format = "%d%b%Y %H:%M")),"")
  if (override == "outputplan") {
    title = list(outputplan$FigureTitle1[i], outputplan$FigureTitle2[i], 
                 outputplan$FigureTitle3[i], outputplan$FigureTitle4[i])
    urh = list(outputplan$urh1[i], outputplan$urh2[i], outputplan$urh3[i])
    ulh = list(outputplan$ulh1[i], outputplan$ulh2[i], outputplan$ulh3[i])
    fnote = list(outputplan$fnote1[i], outputplan$fnote2[i], 
                 outputplan$fnote3[i], outputplan$fnote4[i], 
                 outputplan$fnote5[i])
    fignum = outputplan$FigureNumber[i]
  }
    if (add.fignum == T & is.null(fignum) == FALSE) {
      grid.text(x = unit((page.width)/2, "inch"), 
                y = unit(page.height - top.margin, "inches") - unit(fignum.buffer, "line"), 
                just = "center", 
                paste("Figure", fignum), 
                gp = gpar(fontsize = title.size))
    }
  grid.text(x = unit(page.width/2, "inch"), 
            y = unit(page.height - top.margin, "inches") - unit(title.buffer, "line"), 
            just = "center", title[[1]], gp = gpar(fontsize = title.size))
    grid.text(x = unit(page.width/2, "inch"), 
              y = unit(page.height - top.margin, "inches") - unit(title.buffer + 1, "line"), 
              just = "center", title[[2]], gp = gpar(fontsize = title.size))
    grid.text(x = unit(page.width/2, "inch"), y = unit(page.height - 
                                                         top.margin, "inches") - unit(title.buffer + 2, "line"), 
              just = "center", title[[3]], gp = gpar(fontsize = title.size))
    grid.text(x = unit(page.width/2, "inch"), y = unit(page.height - 
                                                         top.margin, "inches") - unit(title.buffer + 3, "line"), 
              just = "center", title[[4]], gp = gpar(fontsize = title.size))
    grid.text(x = unit(left.margin, "inches"), y = unit(page.height - 
                                                          top.margin, "inches") - unit(header.buffer, "line"), 
              just = "left", ulh[[1]], gp = gpar(fontsize = head.size))
    grid.text(x = unit(left.margin, "inches"), y = unit(page.height - 
                                                          top.margin, "inches") - unit(header.buffer + 1, "line"), 
              just = "left", ulh[[2]], gp = gpar(fontsize = head.size))
    grid.text(x = unit(left.margin, "inches"), y = unit(page.height - 
                                                          top.margin, "inches") - unit(header.buffer + 2, "line"), 
              just = "left", ulh[[3]], gp = gpar(fontsize = head.size))
    grid.text(x = unit(page.width - right.margin, "inches"), 
              y = unit(page.height - top.margin, "inches") - unit(header.buffer, 
                                                                  "line"), just = "right", urh[[1]], gp = gpar(fontsize = head.size))
    grid.text(x = unit(page.width - right.margin, "inches"), 
              y = unit(page.height - top.margin, "inches") - unit(header.buffer + 
                                                                    1, "line"), just = "right", urh[[2]], gp = gpar(fontsize = head.size))
    grid.text(x = unit(page.width - right.margin, "inches"), 
              y = unit(page.height - top.margin, "inches") - unit(header.buffer + 
                                                                    2, "line"), just = "right", urh[[3]], gp = gpar(fontsize = head.size))
    grid.text(x = unit(left.margin, "inch"), y = unit(bottom.margin, 
                                                      "inches") + unit(0 + fnote.buffer, "line"), just = "left", 
              paste(fileloc, time.stamp), gp = gpar(fontsize = foot.size))
    if (override == "outputplan") {
      if (outputplan$TableID[i] != "") {
        grid.text(x = unit(page.width - right.margin, "inch"), 
                  y = unit(bottom.margin, "inches") + unit(0 + 
                                                             fnote.buffer, "line"), just = "right", outputplan$TableID[i], 
                  gp = gpar(fontsize = foot.size))
      }
    }
    grid.text(x = unit(left.margin, "inch"), y = unit(bottom.margin, 
                                                      "inches") + unit(1 + fnote.buffer, "line"), just = "left", 
              fnote[[4]], gp = gpar(fontsize = foot.size))
    grid.text(x = unit(left.margin, "inch"), y = unit(bottom.margin, 
                                                      "inches") + unit(2 + fnote.buffer, "line"), just = "left", 
              fnote[[3]], gp = gpar(fontsize = foot.size))
    grid.text(x = unit(left.margin, "inch"), y = unit(bottom.margin, 
                                                      "inches") + unit(3 + fnote.buffer, "line"), just = "left", 
              fnote[[2]], gp = gpar(fontsize = foot.size))
    grid.text(x = unit(left.margin, "inch"), y = unit(bottom.margin, 
                                                      "inches") + unit(4 + fnote.buffer, "line"), just = "left", 
              fnote[[1]], gp = gpar(fontsize = foot.size))
  }

#' @title sync.ylab.widths
#' @description Aligns the widths of ggplot objects to ensure common plot regions. The maximum length required for y-axis labels among the list is determined and applied to the other plots. This assists in syncing the widths of ggplot objects for the purpose of align figures on a page.
#' @param gg.list a list of ggplot objects
sync.ylab.widths <- 
  function(gg.list=list(lp, tp), default.length=2){    
    gtable.list <- list()
    maxWidthList <- list()    
    for(i in 1:length(gg.list)){
      gtable.list[[length(gtable.list)+1]] <- ggplot_gtable(ggplot_build(gg.list[[i]]))
      maxWidthList[[length(maxWidthList)+1]] <- gtable.list[[i]]$widths[2:3]
    }
    
    for (i in 1:length(gg.list)) gtable.list[[i]]$widths[2:3] <- do.call(unit.pmax, maxWidthList)
    
    return(gtable.list)
  }

#' @title get.top.xaxis
#' @description This takes two ggplot objects, steals the bottom x-axis from 2nd object and returns a gtable object with that bottom x-axis per object 1 and top x-axis per object 2
#' @param p1 ggplot object with bottom x-axis
#' @param p2 ggplot object with intended top x-axis in bottom position
get.top.xaxis <- function(p1=p1.1, p2=p1.2){  
  # Extract gtable
  g1 <- ggplot_gtable(ggplot_build(p1))
  g2 <- ggplot_gtable(ggplot_build(p2))
  
  ## overlap the panel of the 2nd plot on that of the 1st plot
  pp <- c(subset(g1$layout, name=="panel", se=t:r))
  g <- gtable_add_grob(g1, 
                       g2$grobs[[which(g2$layout$name=="panel")]], pp$t, pp$l, pp$b, pp$l)
  # EDIT to have the grid lines align with the lower axis ticks, replace the above line with: 
  # g <- gtable_add_grob(g1, g1$grobs[[which(g1$layout$name=="panel")]], pp$t, pp$l, pp$b, pp$l)
  
  ## steal axis from second plot and modify
  ia <- which(g2$layout$name == "axis-b")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  
  ## switch position of ticks and labels
  ax$heights <- rev(ax$heights)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[2]]$y <- ax$grobs[[2]]$y - unit(1, "npc") + unit(0.15, "cm")
  
  ## modify existing row to be tall enough for axis
  g$heights[[2]] <- g$heights[g2$layout[ia,]$t]
  
  ## add new axis
  g <- gtable_add_grob(g, ax, 2, 4, 2, 4)
  
  ## add new row for upper axis label
  g <- gtable_add_rows(g, g2$heights[1], 1)
  g <- gtable_add_grob(g, g2$grob[[6]], 2, 4, 2, 4)
  
  return(g)
}
