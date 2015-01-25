
# Standard Graphics Names -------------------------------------------------
#' @title Standard graphics names
#' @description This is a dummy function, purpose is to serve as repositiory for function parameter names.
#' @param at.risk.palette colors to be assocated with categorical variable in accompanying at.risk table
#' @param background.palette palette gets passed to scale_fill_manual
#' @param category.col parent.df column associated with explantory variable
#' @param category.label passed to x-axis label
#' @param category.palette colors assoicated with categorical variable
#' @param censor.col name of parent.df column associated with censor variable
#' @param centime.col name of parent.df column associated with censored time
#' @param label.col name of parent.df column containing labels 
#' @param line.col name of parent.df column associated with linetype
#' @param line.size value gets passed to size within geom_line, geom_step
#' @param linetype.palette values passed to scale_linetype_manual
#' @param page.height used by build.page and annotate.page
#' @param page.width used by build.page and annotate.page
#' @param right.margin used by build.page and annotate.page
#' @param left.margin used by build.page and annotate.page
#' @param top.margin used by build.page and annotate.page
#' @param bottom.margin used by build.page and annotate.page
#' @param shape.label value sets passed to labs
#' @param shape.palette values passed to scale_shape_manual
#' @param text.size value gets passed to geom_text
#' @param x.label value gets passed to labs
#' @param x.limits value gets passed to scale_x_continuous
#' @param x.ticks value gets passed to scale_x_continuous
#' @param x.ticks.labels passed to scale_x_continuous
#' @param y.col parent.df column associated with response vairable
#' @param y.digits passed to scale_y_continuous label's, fmt
#' @param y.label value gets passed to labs
#' @param y.limits passed to scale_y_continuous
#' @param y.ticks  passed to scale_y_continuous
#' @param y.label.rank.col column holding ranks for labels in forest/dot/table plots
#' @param y.label.col column holding labels for forest/dot/table plots
#' @param y.rank.col column holding ranks for line items in forest/dot/table plots
#' @param ymin.col name of parent.df column assoicated with ymin
#' @param ymax.col name of parent.df column associated with ymax

graphic.params <- function(
  at.risk.palette,
  background.palette,
  category.col, 
  category.label,
  category.palette, 
  censor.col,
  centime.col,
  label.col,
  line.col, 
  line.size,
  linetype.palette,
  page.height,
  page.width,
  right.margin,
  left.margin,
  top.margin,
  bottom.margin,
  shape.label,
  shape.palette, 
  x.label,
  x.limits,
  x.ticks,
  x.ticks.labels,
  y.col,
  y.label,
  y.limits,
  ymin.col,
  ymax.col,
  y.ticks){
return("hello")
}

# bar.plot ----------------------------------------------------------------
#' @title bar.plot
#' @description A function for creating bar charts; adds tabular data to bar chart.
#' @details In building the driver files, manual adjustment of y.limits and y.ticks is required. Note that this function computes summary statistics from raw data.
#' @inheritParams graphic.params
#' @param bar.position passed to geom_bar
#' @param killMissing
#' @author Greg Cicconetti
bar.plot <-   function (
  parent.df = working.df, 
  y.col = "GWHRT", 
  y.label = "Percentage of Subjects", 
  category.col = "TRTGRP",
  category.label = "Treatment Group", 
  y.limits = c(0, 0.7), 
  y.ticks = seq(0, 0.3, 0.05), 
  bar.position="dodge",
  category.palette = c("red", "blue"),
  text.size=3,
  killMissing = TRUE) 
  {
  
  if(is.null(y.limits) || is.null(y.ticks)) {
    y.limits = c(0, 1)
    y.ticks <- seq(0,1,.1)
    cat("Either y.limits or y.ticks are set to NULL; defaults are used.\n")
  }
  tab <- data.frame(table(parent.df[, y.col], parent.df[, category.col]))
  if (killMissing == T) {
    tab <- subset(tab, Var1 != "Missing")
    tab <- subset(tab, Var2 != "Missing")
    tab$Var1 <- factor(tab$Var1)
  }
  
  barplot.df <- ddply(tab, .(Var1), mutate, Prop=Freq/sum(Freq))
  
  p1 <- ggplot(data = barplot.df, aes(x = Var1, y = Prop, fill = Var2)) + 
    geom_bar(stat = "identity", position = bar.position) + 
    coord_flip() + 
    geom_text( position=position_dodge(width=.9), size=text.size, 
               aes(y=Prop+.05, label=paste(" n = ", Freq, sep = "")))+
      scale_y_continuous(limits = y.limits, breaks = y.ticks, labels = percent_format()) + 
      scale_fill_manual(values = category.palette) + 
      labs(y = y.label, x = category.label, fill = category.label)+
    theme(legend.position="bottom")
  return(p1)
  }

# Box.plot ----------------------------------------------------------------
#' @title box.plot
#' @description Produces boxplots 
#' @details Adjust y.limits, y.ticks and y.digits at the the driver level. Note: This function computes summary statistics from raw data.
#' @inheritParams graphic.params
#' @author Greg Cicconetti
box.plot <-
  function (parent.df = working.df, 
            y.col = "VSBMI", 
            y.label = "BMI (m/kg^2)", 
            category.col = "TRTGRP",
            category.label = "Treatment Group", 
            y.limits = c(10, 100), 
            y.ticks = seq(10, 100, 10), 
            y.digits = 0,
            shape.palette = c(1,2),
            category.palette = c("red", "blue"),
            bplot.text = 3) {
    
    get.whiskers <- function(dframe) {
      bplot <- boxplot(dframe$RESPONSE ~ dframe$CATEGORY, plot = F)
      whiskers <- with(bplot, 
                       data.frame(CATEGORY = names, 
                                  LOWER = stats[1, ], 
                                  MEDIAN = stats[3, ], 
                                  UPPER = stats[5, ],
                                  N=n))
      return(whiskers)
    }

    names(parent.df) <- toupper(names(parent.df))
    
    boxplot.df <- data.frame(
      RESPONSE= parent.df[, y.col],
      CATEGORY= parent.df[, category.col])
    
    # Set resonable default limits and ticks if NULL
    if(is.null(y.limits) || is.null(y.ticks)) {
      y.limits = c(min(boxplot.df$RESPONSE, na.rm=T),
                   max(boxplot.df$RESPONSE, na.rm=T)); 
      y.ticks <- pretty(boxplot.df$RESPONSE)
      cat("Either y.limits or y.ticks are set to NULL; defaults are used.\n")
    }
    
    whiskers <- get.whiskers(dframe = boxplot.df)
    boxplot.df$cat.lab <- (boxplot.df$CATEGORY)
    levels(boxplot.df$cat.lab)<- paste(levels(boxplot.df$CATEGORY),"\n n =", table(boxplot.df$CATEGORY))
    
    p1 <- ggplot(boxplot.df, 
                 aes(x = CATEGORY, y = RESPONSE, shape = factor(CATEGORY), fill = factor(CATEGORY))) + 
      geom_boxplot(outlier.size = 2, outlier.colour = alpha("black", 0.2)) + 
      geom_point(aes(x = CATEGORY, y = mean(RESPONSE), shape = CATEGORY), size = 3, bg = "white") + 
      stat_summary(fun.y = mean, geom = "point", 
                   shape = shape.palette, 
                   size = 3, bg = "white") + 
      scale_shape_manual(values = shape.palette) + 
      scale_fill_manual(values = category.palette, name = category.label) +
      scale_x_discrete(breaks = levels(boxplot.df$CATEGORY), 
                       labels = paste(levels(boxplot.df$CATEGORY),"\n n =", whiskers$N)) + 
      scale_y_continuous(limits = y.limits, breaks = y.ticks, labels = fmt(y.digits)) + 
      geom_segment(data = whiskers, aes(x = as.numeric(CATEGORY) - 0.1, 
                                        y = LOWER, 
                                        xend = as.numeric(CATEGORY) + 0.1, 
                                        yend = LOWER)) + 
      geom_segment(data = whiskers, aes(x = as.numeric(CATEGORY) - 0.1, 
                                        y = UPPER, xend = as.numeric(CATEGORY) + 0.1, 
                                        yend = UPPER)) + 
      geom_text(data = whiskers, aes(x = as.numeric(CATEGORY) - 0.5, 
                                     y = MEDIAN, label = format(round(MEDIAN, 2)), nsmall = 2), 
                size = I(bplot.text)) + 
      scale_colour_manual(values = category.palette) + 
      guides(colour = F) + 
      labs(y = y.label, x = category.label, fill = category.label, shape = category.label)+
      theme(legend.position="bottom")
    return(p1)
  }

# Jitter.plot -------------------------------------------------------------
#' @title jitter.plot - REVISIT - should sync with box.plot
#' @description Produces boxplots assuming TRTGRP is the explantory variable.
#' @details Adjust y.limits, y.ticks and y.digits at the the driver level. Note: This function computes summary statistics from raw data.
#' @inheritParams graphic.params
#' @author Greg Cicconetti
jitter.plot <- function (parent.df = working.df, 
                         y.col = "VSBMI", 
                         y.label = "BMI (m/kg^2)", 
                         category.col = "TRTGRP",
                         category.label = "Treatment Group", 
                         y.limits = c(10, 100), 
                         y.ticks = seq(10, 100, 10), 
                         y.digits = 0,
                         shape.palette = bplot.shapes,
                         category.palette = treatment.palette){
  names(parent.df) <- toupper(names(parent.df))
  
  jitter.df <- data.frame(
    RESPONSE= parent.df[, y.col],
    CATEGORY= parent.df[, category.col])
  
  # Set resonable default limits and ticks if NULL
  if(is.null(y.limits) || is.null(y.ticks)) {
    y.limits = c(min(jitter.df$RESPONSE, na.rm=T),
                 max(jitter.df$RESPONSE, na.rm=T)); 
    y.ticks <- pretty(jitter.df$RESPONSE)
    cat("Either y.limits or y.ticks are set to NULL; defaults are used.\n")
  }
  
  jitter.df.ply <- ddply(jitter.df, .(CATEGORY), 
                         summarise,
                         mean=mean(RESPONSE, na.rm=T),
                         lower=mean(RESPONSE, na.rm=T) - qt(.975, df=sum(is.na(RESPONSE)==F)) *sd(RESPONSE, na.rm=T),
                         upper=mean(RESPONSE, na.rm=T) + qt(.975, df=sum(is.na(RESPONSE)==F))*sd(RESPONSE, na.rm=T))
  names(jitter.df.ply)[2] <- "RESPONSE"
  jitter.plot <- ggplot(data=jitter.df, aes(x= CATEGORY, y= RESPONSE, color=CATEGORY)) + 
    geom_point(position="jitter", alpha=.1)+
    geom_errorbar(data=jitter.df.ply,
                  aes(x=CATEGORY,
                      ymin=lower, ymax=upper, 
                      color=CATEGORY), size=.75)+
    geom_point(data=jitter.df.ply,
               aes(x=CATEGORY, y=RESPONSE, color=CATEGORY),
               size=3)+
    scale_shape_manual(values = shape.palette) + 
    scale_color_manual(values = category.palette) + 
    scale_y_continuous(limits = y.limits, breaks = y.ticks, labels = fmt(y.digits)) + 
    labs(x=category.label, y=y.label, color=category.label)
  
  return(jitter.plot)
}

# cdf.plot ----------------------------------------------------------------
#' @title cdf.plot
#' @description Function to produce Cumulative Distribution plot.  Statistics computed by stat_ecdf().
#' @inheritParams graphic.params
#' @param exclude
#' @author Greg Cicconetti
cdf.plot <-
  function (parent.df= working.df, 
            category.col = "TRTGRP",
            category.label   = "Treatment Group",
            response.col = "EXDURMTH", 
            x.label = "Month 6 HDL Cholesterol", 
            x.limits=c(0,60),
            x.ticks=seq(0,60,10),
            y.label = "Percentage with 3 Month HDL Cholesterol Less Than x-value", 
            y.limits= c(0,1),
            y.ticks = seq(0,1,.2),
            line.size=.75,
            category.palette=c("red", "blue"),
            exclude=c(0))
{
    names(parent.df) <- toupper(names(parent.df))
    
    names(parent.df) <- toupper(names(parent.df))
    
    cdf.df <- data.frame(
      RESPONSE= parent.df[, response.col],
      CATEGORY= parent.df[, category.col])
    
    # Set reasonable limits & ticks if NULL
    if(is.null(x.limits) || is.null(x.ticks)) {
      x.limits = c(min(cdf.df$RESPONSE, na.rm=T),
                   max(cdf.df$RESPONSE, na.rm=T)) 
      x.ticks <- pretty(cdf.df$RESPONSE)
      cat("Either x.limits or x.ticks are set to NULL; defaults are used.\n")
    }
    
    if(is.null(y.limits) || is.null(y.ticks)) {
      y.limits = c(0,1) 
      y.ticks <- seq(0,1,.1)
      cat("Either y.limits or y.ticks are set to NULL; defaults are used.\n")
    }
    
    p1 <- ggplot(data = cdf.df, aes(x = RESPONSE, colour = CATEGORY, linetype = CATEGORY)) + 
      stat_ecdf(size = line.size) + 
      scale_y_continuous(labels = percent_format()) + 
      scale_x_continuous(limits=x.limits, breaks=x.ticks) +
      labs(x = x.label, y = y.label, colour = category.label, linetype = category.label) + 
      scale_colour_manual(values = category.palette) +
      theme(legend.position="bottom")
    return(p1)
  }

# table.plot --------------------------------------------------------------
#' @title table.plot
#' @description A function for plotting columns of text in a figure offering compatiability with forest.plot and dot.plot. 
#' @inheritParams graphic.params
#' @param text.col1 name of column holding text for column 1
#' @param text.col2 name of column holding text for column 2; can be NULL
#' @param text.col3 name of column holding text for column 3; can be NULL
#' @param text.col4 name of column holding text for column 4; can be NULL
#' @param xtick.labs names for x tick labels - should be same length as non-NULL text columns
#' @param label name of column containing labels for y-axis
#' @param y.buff numerical buffer added to top of figure - essentially a mechanism for adding space to top of figure
#' @author Greg Cicconetti

table.plot <-
  function(
    parent.df = working.df,
    y.rank.col="Subcategory",
    category.col="Treatment",
    text.col1 = "Point_Est",
    text.col2 = "LCI",
    text.col3 = "UCI",
    text.col4 = NULL,
    text.size = 12,
    xtick.labs = c("Estimate", "LCI", "UCI"),
    x.limits=NULL,
    y.limits=NULL,
    x.label="Text",
    y.label="Item",
    y.label.rank.col ="rank",  #  this identifies the y-axis values for labels
    y.label.col = "subcategory", 
    category.palette = c("red", "blue")){
    
    if(is.null(y.limits) ) {
      y.limits = c(min(parent.df[,y.rank.col], na.rm=T)-.25,
                   max(parent.df[,y.rank.col], na.rm=T)+.25) 
      cat("y.limits are set to NULL; defaults are used.\n")
    }
    if(is.null(x.limits) ) {
      x.limits = 1:(4-sum(c(is.null(text.col4),is.null(text.col3),is.null(text.col2))))
      cat("x.limits are set to NULL; defaults are used.\n")
    }
    # Cap names
    names(parent.df) <- toupper(names(parent.df))
    category.col <- toupper(category.col)
    y.rank.col <- toupper(y.rank.col)
    text.col1 <- toupper(text.col1)
    if(is.null(text.col2)==F) text.col2 <- toupper(text.col2)
    if(is.null(text.col3)==F)text.col3 <- toupper(text.col3)
    if(is.null(text.col4)==F) text.col4 <- toupper(text.col4)
    y.label.rank.col <- toupper(y.label.rank.col)
    y.label.col <- toupper(y.label.col)
    y.label.rank.col <- toupper(y.label.rank.col)
    
    table.df <- data.frame(
      RANK = parent.df[, y.rank.col],
      CATEGORY = parent.df[, category.col],
      TEXT.COL1 = parent.df[, text.col1],
      TEXT.COL2 = parent.df[, text.col2],
      TEXT.COL3 = parent.df[, text.col3],
      TEXT.COL4 = parent.df[, text.col4],
      LABEL.RANKS = parent.df[, y.label.rank.col],
      LABEL.VALUES = parent.df[, y.label.col])
    
    for.return <-  ggplot()+
      geom_text(data = table.df,  size=text.size,
                aes(x = 1,
                    colour = CATEGORY,
                    y = RANK, 
                    label = TEXT.COL1, hjust = 0.5))
    
    if(is.null(text.col2)==F)
      for.return <- for.return +
      geom_text(data = table.df,   size=text.size,
                aes(x = 2,  
                    colour = CATEGORY, 
                    y = RANK, 
                    label = TEXT.COL2, hjust = 0.5))
    
    if(is.null(text.col3)==F)
      for.return <- for.return +
      geom_text(data = table.df,   size=text.size,
                aes(x = 3,   
                    colour = CATEGORY, 
                    y = RANK, 
                    label = TEXT.COL3, hjust = 0.5))
    
    if(is.null(text.col4)==F)
      for.return <- for.return +
      geom_text(data = table.df,  size=text.size,
                aes(x = 4,   
                    colour = CATEGORY, 
                    y = RANK, 
                    label = TEXT.COL4, hjust = 0.5))
    
    for.return <- for.return +
      scale_x_continuous(limits = c(0.5, 4.5 -(is.null(text.col2)+
                                                 is.null(text.col3)+
                                                 is.null(text.col4))), 
                         breaks=1:(4 -(is.null(text.col2)+
                                           is.null(text.col3)+
                                           is.null(text.col4))), labels=xtick.labs) + 
      scale_y_continuous(limits=y.limits,
                         breaks=table.df$LABEL.RANKS, 
                         labels=table.df$LABEL.VALUES)+
      guides(size=FALSE, color=FALSE)+
      labs(x=x.label, y=y.label)+
      scale_color_manual(values=rev(category.palette))+
      theme(plot.background = element_rect(colour = "white"), 
            panel.background = element_rect(fill = "white", colour = NA), 
            axis.text.x = element_text(vjust = 1, colour = "black"), 
            axis.ticks.x = element_line(colour = "transparent"), 
            axis.ticks.y = element_line(color="transparent"),
            panel.grid.minor = element_line(colour = "white", size = 0.25)
      )
    
    return(for.return)
  }

#' @title table.plot2
#' @description A function for plotting columns of text in a figure offering compatiability with forest.plot and dot.plot. 
#' @inheritParams graphic.params
#' @param text.col1 name of column holding text for column 1
#' @param text.col2 name of column holding text for column 2; can be NULL
#' @param text.col3 name of column holding text for column 3; can be NULL
#' @param text.col4 name of column holding text for column 4; can be NULL
#' @param xtick.labs names for x tick labels - should be same length as non-NULL text columns
#' @param label name of column containing labels for y-axis
#' @param y.buff numerical buffer added to top of figure - essentially a mechanism for adding space to top of figure
#' @author Greg Cicconetti

table.plot2 <-
  function(
    parent.df = working.df,
    y.rank.col="Subcategory",
    category.col="Treatment",
    text.col1 = "Point_Est",
    text.col2 = "LCI",
    text.col3 = "UCI",
    text.col4 = NULL,
    text.size = 12,
    xtick.labs = c("Estimate", "LCI", "UCI"),
    x.ticks = 1:3,
    x.limits=NULL,
    y.limits=NULL,
    x.label="Text",
    y.label="Item",
    y.label.rank.col ="rank",  #  this identifies the y-axis values for labels
    y.label.col = "subcategory", 
    category.palette = c("red", "blue")){
    
    if(is.null(y.limits) ) {
      y.limits = c(min(parent.df[,y.rank.col], na.rm=T)-.25,
                   max(parent.df[,y.rank.col], na.rm=T)+.25) 
      cat("y.limits are set to NULL; defaults are used.\n")
    }
    if(is.null(x.limits) || is.null(x.ticks)) {
      x.limits = 1:(4-sum(c(is.null(text.col4),is.null(text.col3),is.null(text.col2))))
      x.ticks <- 1:(4-sum(c(is.null(text.col4),is.null(text.col3),is.null(text.col2))))
      cat("x.limits are set to NULL; defaults are used.\n")
      
    }
    # Cap names
    names(parent.df) <- toupper(names(parent.df))
    category.col <- toupper(category.col)
    y.rank.col <- toupper(y.rank.col)
    text.col1 <- toupper(text.col1)
    if(is.null(text.col2)==F) text.col2 <- toupper(text.col2)
    if(is.null(text.col3)==F)text.col3 <- toupper(text.col3)
    if(is.null(text.col4)==F) text.col4 <- toupper(text.col4)
    y.label.rank.col <- toupper(y.label.rank.col)
    y.label.col <- toupper(y.label.col)
    y.label.rank.col <- toupper(y.label.rank.col)
    
    table.df <- data.frame(
      RANK = parent.df[, y.rank.col],
      CATEGORY = parent.df[, category.col],
      TEXT.COL1 = parent.df[, text.col1],
      TEXT.COL2 = parent.df[, text.col2],
      TEXT.COL3 = parent.df[, text.col3],
      TEXT.COL4 = parent.df[, text.col4],
      LABEL.RANKS = parent.df[, y.label.rank.col],
      LABEL.VALUES = parent.df[, y.label.col])
    
    for.return <-  ggplot()+
      geom_text(data = table.df,  size=text.size,
                aes(x = x.ticks[1],
                    colour = CATEGORY,
                    y = RANK, 
                    label = TEXT.COL1, hjust = 0.5))
    
    if(is.null(text.col2)==F)
      for.return <- for.return +
      geom_text(data = table.df,   size=text.size,
                aes(x = x.ticks[2], 
                    colour = CATEGORY, 
                    y = RANK, 
                    label = TEXT.COL2, hjust = 0.5))
    
    if(is.null(text.col3)==F)
      for.return <- for.return +
      geom_text(data = table.df,   size=text.size,
                aes(x = x.ticks[3],  
                    colour = CATEGORY, 
                    y = RANK, 
                    label = TEXT.COL3, hjust = 0.5))
    
    if(is.null(text.col4)==F)
      for.return <- for.return +
      geom_text(data = table.df,  size=text.size,
                aes(x = x.ticks[4],   
                    colour = CATEGORY, 
                    y = RANK, 
                    label = TEXT.COL4, hjust = 0.5))
    
    for.return <- for.return +
      scale_x_continuous(limits = x.limits, 
                         breaks=x.ticks) + 
      scale_y_continuous(limits=y.limits,
                         breaks=table.df$LABEL.RANKS, 
                         labels=table.df$LABEL.VALUES)+
      guides(size=FALSE, color=FALSE)+
      labs(x=x.label, y=y.label)+
      scale_color_manual(values=rev(category.palette))+
      theme(plot.background = element_rect(colour = "white"), 
            panel.background = element_rect(fill = "white", colour = NA), 
            axis.text.x = element_text(vjust = 1, colour = "black"), 
            axis.ticks.x = element_line(colour = "transparent"), 
            axis.ticks.y = element_line(color="transparent"),
            panel.grid.minor = element_line(colour = "white", size = 0.25)
      )
    
    return(for.return)
  }

# forest.plot -------------------------------------------------------------

#' @title forest.plot
#' @description A function for plotting forest plots offering compatiability with table.plot and dot.plot. 
#' @inheritParams graphic.params
#' @param Point.Est = "Point_Est",
#' @param Lower.CI column holding lower limit of CI
#' @param Upper.CI column holding upper limit of CI
#' @param logtrans Logical; if true log transformation is applied to x axis (ensure x.limits are positive!)
#' @param flip.palette logical; if TRUE it reverse the order of colors used for background
#' @author Greg Cicconetti
forest.plot <-
  function (parent.df = working.df, 
            y.rank.col = "rank",  # this maps the line segments to y-axis
            Point.Est = "hr",  
            Lower.CI = "low", 
            Upper.CI = "high", 
            y.label.rank.col ="rank",  #  this identifies the y-axis values for labels
            y.label.col = "subcategory", # This holds the labels which should sync with y.label.breaks
            x.label = "Estimate", 
            y.label = "Item",
            log.trans = TRUE, 
            x.limits = c(0.21, 5), 
            x.ticks = 2^(-2:2), 
            y.limits=NULL,
            category.col = "category", # This colors the points and line segments
            background.palette = c("red", "blue"), 
            category.palette = c("red", "blue"), 
            shape.palette = c(16, 16), 
            flip.palette = FALSE) 
{
    
    if(is.null(y.limits) ) {
      y.limits = c(min(parent.df[,y.rank.col], na.rm=T)-.25,
                   max(parent.df[,y.rank.col], na.rm=T)+.25) 
      cat("y.limits are set to NULL; defaults are used.\n")
    }
    
    # Cap names
    names(parent.df) <- toupper(names(parent.df))
    category.col <- toupper(category.col)
    y.rank.col <- toupper(y.rank.col)
    Point.Est <- toupper(Point.Est)
    Lower.CI <- toupper(Lower.CI)
    Upper.CI <- toupper(Upper.CI)
    y.label.rank.col <- toupper(y.label.rank.col)
    y.label.col <- toupper(y.label.col)
    y.label.rank.col <- toupper(y.label.rank.col)
    
    # Break if x.limits or x.ticks are not supplied
    if (is.null(x.limits) || is.null(x.ticks)) {
      cat("Either x.limits or x.ticks are set to NULL; specify.")
      return()
    }
    
    forest.df <- data.frame(RANK = parent.df[, y.rank.col],
                            POINT.EST = parent.df[, Point.Est], 
                            LOWER.CI = parent.df[, Lower.CI], 
                            UPPER.CI = parent.df[, Upper.CI],
                            LABEL.RANKS = parent.df[, y.label.rank.col],
                            LABEL.VALUES = parent.df[, y.label.col],
                            CATEGORY = parent.df[, category.col])
    
    # Flips the sequence of background colors
    ifelse(flip.palette == FALSE, 
           rects <- data.frame(xmin = c(x.limits[1], log.trans * 1), 
                               xmax = c(log.trans * 1, x.limits[2]), 
                               ymin = c(-Inf, -Inf),
                               ymax = c(Inf, Inf), 
                               period = c("A", "B")), 
           rects <- data.frame(xmin = c(x.limits[1], log.trans * 1), 
                               xmax = c(log.trans * 1, x.limits[2]), 
                               ymin = c(-Inf, -Inf), 
                               ymax = c(Inf, Inf), 
                               period = c("B", "A")))
    # Add the background rectangles
    # Add the point estimate
    # Add the error bars
    # Manipulate the y-axis
    # Shut the guides off
    # Change the background colors
    # Change the category colors
    # Change the shapes
    # Change the axis titles
    
    for.return <- ggplot() + 
      geom_rect(data = rects, 
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = period), 
                alpha = 0.2) + 
      geom_point(data = forest.df, aes(x = POINT.EST, y = (RANK), color = CATEGORY)) + 
      geom_errorbarh(data = forest.df, height = 0.2, lwd = .75,
                     aes(y = RANK, x = POINT.EST, xmin = as.numeric(LOWER.CI), xmax = as.numeric(UPPER.CI), 
                         color = CATEGORY)) + 
      scale_y_continuous(limits=y.limits,
                         breaks = (forest.df$LABEL.RANK), 
                         labels = (forest.df$LABEL.VALUES)) + 
      guides(color = F, fill = F)+  
      scale_fill_manual(values = background.palette) + 
      scale_color_manual(values = category.palette) + 
      scale_shape_manual(values = shape.palette) + 
      labs(x = x.label, y = y.label) 
    
    # Manipulate the x-axis if necessary
    # Add the reference line
    ifelse(log.trans == TRUE, for.return <- for.return + 
             scale_x_continuous(trans = log_trans(), 
                                limits = x.limits, 
                                breaks = x.ticks, expand = c(0, 0.001)) + 
             geom_vline(xintercept = log.trans * 1), 
           for.return <- for.return + 
             scale_x_continuous(limits = x.limits, 
                                breaks = x.ticks, expand = c(0, 0.001)) +
             geom_vline(xintercept = log.trans * 1))
    
    for.return$data <- forest.df
    return(for.return)
  }

# dot.plot ----------------------------------------------------------------
#' @title dot.plot
#' @description A function for plotting dotplots offering compatiability with table.plot and dot.plot. 
#' @inheritParams graphic.params
#' @param Point.Est = "Point_Est",
#' @author Greg Cicconetti
dot.plot <- function (parent.df = dot.df.melt, category.col = "Treatment", 
                      y.rank.col = "rank", y.label.rank.col = "label.rank", y.label.col = "subgroup", 
                      Point.Est = "percent", x.limits = c(0, 1),
                      x.ticks = seq(0, 1, 0.2),
                      y.limits = NULL, shape.palette = c(16, 17), 
                      x.label = "Estimate", y.label = "Item",
                      category.palette = c("red", "blue")) 
{
  
  names(parent.df) <- toupper(names(parent.df))
  if (is.null(y.limits)) {
    y.limits = c(min(parent.df[, y.rank.col], na.rm = T) - 
                   0.25, max(parent.df[, y.rank.col], na.rm = T) + 0.25)
    cat("y.limits are set to NULL; defaults are used.\n")
  }
  if (is.null(x.limits) || is.null(x.ticks)) {
    x.limits = c(0, 1)
    x.ticks <- seq(0, 1, 0.1)
    cat("Either x.limits or x.ticks are set to NULL; defaults are used.\n")
  }
  dotplot.df <- data.frame(CATEGORY = parent.df[, category.col], 
                           RANK = parent.df[, y.rank.col], 
                           POINT.EST = parent.df[,   Point.Est],
                           LABEL.RANKS = parent.df[, y.label.rank.col], 
                           LABEL.VALUES = parent.df[, y.label.col])
  for.return <- ggplot(data = dotplot.df, aes(y = RANK, x = POINT.EST, 
                                              shape = CATEGORY, colour = CATEGORY)) + 
    geom_point(size = 2.75,    alpha = 0.7) + 
    scale_shape_manual(values = shape.palette) + 
    labs(x = x.label, y = y.label) + scale_colour_manual(values = category.palette) + 
    scale_x_continuous(limits = x.limits, breaks = x.ticks, 
                       labels = percent_format()) + 
    scale_y_continuous(limits = y.limits, breaks = (dotplot.df$LABEL.RANK), 
                       labels = (dotplot.df$LABEL.VALUES))
  return(for.return)
}

# funnel.plot -------------------------------------------------------------

#' @title funnel.plot
#' @description Creates a funnel plot tailored for regions.
#' @details Appeals to Poisson-distribution for confidence bounds and makes continuity correction. Outliers are identified by name.  # This function should return a series of ggplot objects
#' The number of graphics returned should depend on the reference.category: If reference category is "global" one graphic is returned; number of points equals number of unique categories. 
#' If reference category is region, then one graphic is returned for each region.  When category is country, a dot for each country is used. When category is centreid a dot for each centreid is used.
#' If reference category is country, then one graphic for each country is returned.  Category is assumed to be centreid; one dot for each centre.
#' @param response Column holding the count variable (in case of 1st MACE: 0 censored, 1 observed)
#' @param base Could be "centre", "country", or "region"
#' @param reference should be at least one level above base:  "country", "region", "global"
#' @param region  if reference == "region", specify 
#' @param country  if reference == "country", specify
#' @param voffset vertical offset for text
#' @section Value: 
#' \describe{
#' A revamped version of the older funnel plot.  Previous version produced funnel plots for regions relative to  study's global average.  This version allows comparison of 'base' relative to a 'reference'.  
#' }
#' @references David J. Spiegelhalter. Funnel plots for comparing institutional performance. Statist. Med. 2005; 24:1185-1202
#' @author Greg Cicconetti
funnel.plot <- 
  function (parent.df = read.csv(paste(dd, "g_KMfmace.csv", sep = "")), 
            censored.time="centime",
            censor.var = "censor", 
            base = "region", 
            reference = "global", 
            region = "Asia/Pacific", 
            country = "Hong Kong", 
            voffset = 0,
            text.size=4) 
  {
    names(parent.df) <- toupper(names(parent.df))
    
    parent.df$centime <- parent.df[,censored.time]
    parent.df$censor <- parent.df[, censor.var]
    counts.reference <- data.frame(table(parent.df[, response]))
    counts.reference <- counts.reference[counts.reference$Var1 == 1, ]
    names(counts.reference) <- c("Var1", "Freq")
    centime.sum <- sum(parent.df$centime, na.rm = T)
    forfunnel.reference <- merge(counts.reference, centime.sum)
    names(forfunnel.reference) <- c("Var1", "Freq", "centime.sum")
    forfunnel.reference$otime.years <- forfunnel.reference$centime.sum/365.25
    forfunnel.reference$rate <- forfunnel.reference$Freq/forfunnel.reference$otime.years
    global.data <- forfunnel.reference
    counts.reference <- data.frame(table(parent.df[, response], 
                                         parent.df[, "REGRAP"]))
    counts.reference <- counts.reference[counts.reference$Var1 == 1, ]
    names(counts.reference) <- c("Var1", "REGRAP", "Freq")
    centime.sum <- ddply(parent.df, .(REGRAP), summarize, centime.sum = sum(centime, na.rm = T))
    forfunnel.reference <- merge(counts.reference, centime.sum)
    forfunnel.reference$otime.years <- forfunnel.reference$centime.sum/365.25
    forfunnel.reference$rate <- forfunnel.reference$Freq/forfunnel.reference$otime.years
    regional.data <- forfunnel.reference
    counts.reference <- data.frame(table(dataframe[, response], dataframe[, "COUNTRY"]))
    counts.reference <- counts.reference[counts.reference$Var1 ==  1, ]
    names(counts.reference) <- c("Var1", "COUNTRY", "Freq")
    centime.sum <- ddply(dataframe, .(COUNTRY, REGRAP), summarize, centime.sum = sum(centime))
    forfunnel.reference <- merge(counts.reference, centime.sum)
    forfunnel.reference$otime.years <- forfunnel.reference$centime.sum/365.25
    forfunnel.reference$rate <- forfunnel.reference$Freq/forfunnel.reference$otime.years
    country.data <- forfunnel.reference
    counts.reference <- data.frame(table(dataframe[, response], dataframe[, "CENTREID"]))
    counts.reference <- counts.reference[counts.reference$Var1 == 
                                           1, ]
    names(counts.reference) <- c("Var1", "CENTREID", "Freq")
    centime.sum <- ddply(dataframe, .(CENTREID, COUNTRY, REGRAP), 
                         summarize, centime.sum = sum(centime))
    forfunnel.reference <- merge(counts.reference, centime.sum)
    forfunnel.reference$otime.years <- forfunnel.reference$centime.sum/365.25
    forfunnel.reference$rate <- forfunnel.reference$Freq/forfunnel.reference$otime.years
    centre.data <- forfunnel.reference
    switch(reference, 
           "global"= {lam0 <- global.data$rate},
           "region" = {lam0 <- subset(regional.data, REGRAP == region)$rate},
           "country" = {centre.data <- subset(centre.data, COUNTRY == country)
                        lam0 <- subset(country.data, COUNTRY == country)$rate}
    )
    
    switch(base, 
           "centre" = {
             switch(reference, 
                    "global" = {figdframe <- centre.data},
                    "region" = {figdframe <- subset(centre.data, REGRAP == region)},
                    "country" = {figdframe <- subset(centre.data, COUNTRY == country)}
             )},
           "country" = {
             switch(reference,
                    "global"= {figdframe <- country.data},
                    "region" = {figdframe <- subset(country.data, REGRAP == region)}
             )},
           "region" = {figdframe <- regional.data}
    )
                    
    y1 <- c(0, max(figdframe$rate) * 1.25)
    x1 <- c(0, max(figdframe$otime.years))
    py <- figdframe$otime.years
    mn <- lam0 * py
    elowq.95 <- qpois(0.025, mn)
    ehghq.95 <- qpois(0.975, mn)
    elowq.99 <- qpois(0.005, mn)
    ehghq.99 <- qpois(0.995, mn)
    elowq.c.95 <- elowq.95 - 1
    elowq.c.99 <- elowq.99 - 1
    a.95 <- (ppois(elowq.95, mn) - 0.025)/(ppois(elowq.95, mn) - ppois(elowq.c.95, mn))
    a.99 <- (ppois(elowq.99, mn) - 0.005)/(ppois(elowq.99, mn) -  ppois(elowq.c.99, mn))
    lowq.c.95 <- pmax(0, (elowq.95 - a.95)/py)
    lowq.c.99 <- pmax(0, (elowq.99 - a.99)/py)
    ehghq.c.95 <- ehghq.95 - 1
    a.95 <- (ppois(ehghq.95, mn) - 0.975)/(ppois(ehghq.95, mn) - ppois(ehghq.c.95, mn))
    ehghq.c.99 <- ehghq.99 - 1 
    a.99 <- (ppois(ehghq.99, mn) - 0.995)/(ppois(ehghq.99, mn) -  ppois(ehghq.c.99, mn))
    hghq.c.95 <- (ehghq.95 - a.95)/py
    hghq.c.99 <- (ehghq.99 - a.99)/py
    figdframe$lowq.c.95 <- lowq.c.95
    figdframe$lowq.c.99 <- lowq.c.99
    figdframe$hghq.c.95 <- hghq.c.95
    figdframe$hghq.c.99 <- hghq.c.99
    figdframe$flag.95 <- ifelse(figdframe$rate >= lowq.c.95 & 
                                  figdframe$rate <= hghq.c.95, "N", "Y")
    figdframe$flag.99 <- ifelse(figdframe$rate >= lowq.c.99 & 
                                  figdframe$rate <= hghq.c.99, "N", "Y")
    figdframe.melt <- melt(data = figdframe, id = ("otime.years"), 
                           measure.vars = c("lowq.c.95", "lowq.c.99", "hghq.c.95", 
                                            "hghq.c.99"))
    levels(figdframe.melt$variable) <- c("95% band", "99% band", 
                                         "hghq.c.95", "hghq.c.99")
    names(figdframe)[1] <- "base"
    figdframe$rate2 <- figdframe$rate - voffset

    p1 <- ggplot(figdframe, aes(x = otime.years, y = rate)) + 
      geom_line(size = 1.25, data = figdframe.melt, 
                aes(x = otime.years, y = value, color = variable, linetype = variable)) + 
      scale_color_manual(breaks = c("95% band", "99% band"), 
                         values = category.palette[c(1, 2, 1, 2)]) + 
      scale_linetype_manual(breaks = c("lowq.c.95", "lowq.c.99"), values = fplot.linetypes) + 
      geom_point(data = figdframe, size = 3) + 
      scale_y_continuous(label = percent) + 
      labs(x = "Subject Years", y = "Annualized Rates", 
           colour = "Confidence Level", linetype = "Confidence Level") + 
      guides(colour = guide_legend(override.aes = list(linetype = c("solid", "dotted"))))
    
    if (sum(figdframe$flag.95 == "Y") > 0) 
      p1 <- p1 + 
      geom_text(data = subset(figdframe, flag.95 == "Y"), 
                aes(x = otime.years, y = rate2, label = base), 
                hjust = 0.5, vjust = 1.05, size = text.size, show_guide = F)
    if (nrow(figdframe) < 10) 
      p1 <- p1 +
      geom_text(data = (figdframe), aes(x = otime.years, y = rate2, label = base), 
                hjust = 0.5, vjust = 1.05, size=text.size, show_guide = F)
    if (base == "centre") {
      figdframe$focus <- ifelse(figdframe$COUNTRY == country & 
                                  figdframe$REGRAP == region, 0.4, 0.1) + ifelse(figdframe$COUNTRY == country, 0.4, 0.1)
      p2 <- ggplot(figdframe, aes(x = otime.years, y = rate)) + 
        geom_line(size = 1.25, data = figdframe.melt, aes(x = otime.years, 
                                                          y = value, color = variable, linetype = variable)) + 
        scale_color_manual(breaks = c("95% band", "99% band"), 
                           values = category.palette[c(1, 2, 1, 2)]) + 
        scale_linetype_manual(breaks = c("lowq.c.95", "lowq.c.99"), 
                              values = fplot.linetypes) + geom_point(size = 3, 
                                                                     color = factor(figdframe$flag.95), alpha = figdframe$focus) + 
        scale_y_continuous(label = percent) + labs(x = "Subject Years", 
                                                   y = "Annualized Rates", colour = "Confidence Level", 
                                                   linetype = "Confidence Level")
      if (sum(figdframe$flag.95 == "Y" & figdframe$COUNTRY == 
                country) > 0) 
        p2 <- p2 + geom_text(data = subset(figdframe, flag.95 == 
                                             "Y" & COUNTRY == country), aes(x = otime.years, 
                                                                            y = rate, label = base), hjust = 0.5, vjust = 1.05, 
                             size = text.size, show_guide = F)
      if (nrow(figdframe) < 10) 
        p2 <- p2 + geom_text(data = subset(figdframe, COUNTRY == 
                                             country), aes(x = otime.years, y = rate, label = base), 
                             hjust = 0.5, vjust = 1.05, size = text.size, show_guide = F)
      return(list(p1, p2, figdframe))
    }
    else return(list(p1, figdframe))
  }

# line.plot ---------------------------------------------------------------
#' @title line.plot 
#' @description A function to create lineplots.
#' @inheritParams graphic.params
#' @param addBars logical to add error bars
#' @param pdval value passed to position_dodge
#' @author Greg Cicconetti
#' @examples
#' \dontrun{     my.plot <- line.plot(parent.df=working.df, 
#' category.palette = c("red","blue"),
#' linetype.palette = c("dotted", "blank", "solid","blank"),
#' line.size = 0.75,
#' shape.palette = c(24, 21),
#' x.label = "Visit", 
#' y.label = "Response",
#' category.label = "Treatment Group", 
#' x.limits = NULL,
#' x.ticks = unique(working.df$XVALUES), 
#' x.ticks.labels = unique(working.df$XVALUES),
#' addBars = TRUE, 
#' pdval = 0.25,
#' x.col = "XVALUES",
#' y.col = "YVALUES",
#' y.limits = NULL, 
#' y.ticks = NULL,
#' category.col = "CATEGORY",
#' category.symbol.col="CATEGORY.SYMBOL",
#' y.digits = 0, 
#' ymin.col = "YMIN",
#' ymax.col = "YMAX",
#' line.col = "LTYPE") 
#' }
#' @author Greg Cicconetti/David Wade
line.plot <- function (parent.df = working.df,
                       category.palette = c("red","blue"),
                       linetype.palette = c("dotted", "blank", "solid","blank"),
                       line.size = 0.75,
                       shape.palette = c(24, 21),
                       x.label = "Visit", 
                       y.label = "Response",
                       category.label = "Treatment Group", 
                       x.limits = NULL,
                       x.ticks = unique(working.df$XVALUES), 
                       x.ticks.labels = unique(working.df$XVALUES),
                       addBars = TRUE, 
                       bar.width=1,
                       pdval = 0.25,
                       x.col = "XVALUES",
                       y.col = "YVALUES",
                       y.limits = NULL, 
                       y.ticks = NULL,
                       category.col = "CATEGORY",
                       category.symbol.col="CATEGORY.SYMBOL",
                       y.digits = 0, 
                       ymin.col = "YMIN",
                       ymax.col = "YMAX",
                       line.col = "LTYPE") 
{
  names(parent.df) <- toupper(names(parent.df))
  
  lineplot.df <- data.frame(XVALUES = parent.df[, x.col],
                            YVALUES = parent.df[, y.col],
                            CATEGORY = parent.df[, category.col],
                            CATEGORY.SYMBOL = parent.df[, category.symbol.col],
                            YMIN = parent.df[,ymin.col],
                            YMAX = parent.df[, ymax.col],
                            LTYPE = parent.df[,line.col])
  
  if (is.null(x.limits) || is.null(x.ticks)) {
    x.limits = c(min(lineplot.df$XVALUES, na.rm = T), max(lineplot.df$XVALUES, 
                                                          na.rm = T))
    x.ticks <- pretty(lineplot.df$XVALUES)
    x.ticks.labels <- cat("Either x.limits or x.ticks are set to NULL; defaults are used.\n")
  }
  if (is.null(y.limits) || is.null(y.ticks)) {
    y.limits = c(min(lineplot.df$YVALUES, na.rm = T), max(lineplot.df$YVALUES, 
                                                          na.rm = T))
    y.ticks <- pretty(lineplot.df$YVALUES)
    cat("Either y.limits or y.ticks are set to NULL; defaults are used.\n")
  }
  pd <- position_dodge(pdval)
  
  # this is the basic line plot
  p1 <- ggplot(data = lineplot.df, aes(x = XVALUES, y = YVALUES, 
                                       ymin = YMIN, ymax = YMAX, colour = CATEGORY, linetype = LTYPE, 
                                       shape = CATEGORY)) + geom_line(position = pd, size = line.size)
  
  # this adds the vertical lines representing whatever measure of variability is fed in
  if (addBars == TRUE)     
    p1 <- p1 + geom_errorbar(linetype = "solid", width = bar.width, 
                             position = pd, size = 0.2)
  
  # this adds the blank circle symbols onto which the character symbols are superimposed
  p1 <- p1 + geom_point(position=pd, size=4, shape=16) 
  
  # this adds the character symbols that get superimposed on the blank cicrle symbols
  # plus some legends and axis controls 
  # the geom_point colour is the character symbol color
  # p1 <- p1 + geom_point(position = pd, size = 2.4, fill = "white", colour="black") +
  p1 <- p1 + geom_point(position = pd, size = 2.4,colour="black") +
    scale_colour_manual(values = category.palette) +
    scale_linetype_manual(values = linetype.palette) + 
    scale_shape_manual(values = shape.palette) + 
    labs(x = x.label, y = y.label, colour = category.label, shape = category.label) + 
    scale_x_continuous(limits = x.limits, breaks = x.ticks,labels = x.ticks.labels) +
    scale_y_continuous(labels = fmt(y.digits)) + 
    guides(linetype = "none")
  return(p1)
}

# profile.plot ------------------------------------------------------------

#' @title profile.plot  - NEEDS updating along lines of line.plot
#' @description A function to create profile plots
#' @inheritParams graphic.params
#' @param addBars logical to add error bars
#' @param pdval value passed to position_dodge
#' @author Greg Cicconetti
profile.plot <- function(
  parent.df=working.df,
  category.palette = c("red", "blue"),
  linetype.palette = c("dotted", "blank", "solid", "blank"),
  line.size = .75,
  shape.palette = c(24, 24), 
  x.label = "Visit",
  y.label = "Response",
  category.label = "Category",
  shape.label = "Category",
  x.limits = c(0,110),
  x.ticks = unique(working.df$XVALUES),
  x.ticks.labels = unique(working.df$XVALUES),
  pdval=0.25,
  x.col="XVALUES",
  y.col="YVALUES",
  category.col="CATEGORY",
  facet.col="SUBJID",
  y.limits = NULL,
  y.ticks = NULL,
  y.digits=0
){
  profile.df <- data.frame(XVALUES=parent.df[,x.col],
                           YVALUES=parent.df[,y.col],
                           CATEGORY=parent.df[,category.col],
                           FACET=parent.df[,facet.col])
  
  # Set reasonable limits/ticks if NULL
  if(is.null(x.limits) || is.null(x.ticks)) {
    x.limits = c(min(profile.df$XVALUES, na.rm=T),
                 max(profile.df$XVALUES, na.rm=T)); 
    x.ticks <- pretty(profile.df$XVALUES)
    x.ticks.labels <- 
      cat("Either x.limits or x.ticks are set to NULL; defaults are used.\n")
  }
  
  if(is.null(y.limits) || is.null(y.ticks)) {
    y.limits = c(min(profile.df$YVALUES, na.rm=T),
                 max(profile.df$YVALUES, na.rm=T)); 
    y.ticks <- pretty(profile.df$YVALUES)
    cat("Either y.limits or y.ticks are set to NULL; defaults are used.\n")
  }
  
  pd <- position_dodge(pdval)
  
  p1 <- ggplot(data=profile.df, aes(x=XVALUES, 
                                    y= YVALUES,
                                    group=FACET,
                                    color=CATEGORY))+
    geom_line()+
    geom_point()+
    facet_wrap(~FACET)
  return(p1)
}

# nsubj.plot --------------------------------------------------------------
#' @title nsubj.plot 
#' @description A function to create tables to accompany KMs and lineplots
#' @inheritParams graphic.params
#' @param parent.df incoming data.frame with special structure
#' @examples
#' \dontrun{ 
#' }
#' @author Greg Cicconetti/David Wade

nsubj.plot <- 
  function (parent.df = working.df,
            category.palette = c("red","blue"), 
            x.label = "Number of Subjects", 
            y.label = "Treatment\nGroup", 
            
            text.size = 4,
            x.col="XVALUES",
            text.col="N",
            category.col="CATEGORY",
            x.limits = c(0.5, 18), 
            x.ticks = unique(parent.df$XVALUES), 
            x.ticks.labels = unique(parent.df$XVALUES)
  ) 
{
    if (is.null(x.limits) || is.null(x.ticks)) {
      cat("Either x.limits or x.ticks are set to NULL; specify.\n")
      break
    }
    names(parent.df) <- toupper(names(parent.df))
    
    table.df <- data.frame(XVALUES = parent.df[,x.col],
                           YVALUES = parent.df[,category.col],
                           COLOR.COL = parent.df[,category.col],
                           TEXT = parent.df[,text.col])
    
    p1 <- ggplot(data = table.df, aes(x = XVALUES, y = YVALUES, 
                                      colour = COLOR.COL, label = TEXT)) +
      geom_text(size = text.size) + 
      labs(y = y.label, x = x.label) + scale_color_manual(values = category.palette) + 
      scale_x_continuous(limits = x.limits, breaks = x.ticks, 
                         labels = x.ticks.labels) + guides(color = FALSE) + 
      theme_table_nomargins() + theme(axis.ticks = element_line(color = "white"), 
                                      axis.text.x = element_text(color = "white"))
    
    return(p1)}              

# km.plot -----------------------------------------------------------------
#' @title km.plot 
#' @description A function to create km plots
#' @inheritParams graphic.params
#' @param fromthetop logical.  If TRUE KM curve decends from 1, if FALSE KM curve ascends from 0. Ensure you have an appropriate censor.col passed above!
#' @author Greg Cicconetti
km.plot <- 
function (parent.df, 
          censor.col = "CENSOR", 
          centime.col = "CENTIME", 
          category.col = "REGRAP", 
          category.palette = rainbow(5), 
          at.risk.palette = rainbow(5), 
          category.label = "Treatment Group", 
          nsubj.plot.label="Number at Risk",
          linetype.palette = 1:6, 
          x.label = "Time Since Randomization", 
          y.label = "Percetage of Subjects", 
          x.limits = c(0, 48), 
          x.ticks = seq(0, 48, 3), 
          y.ticks = seq(0, 1, 0.2), 
          y.limits = c(0, 1), 
          line.size = 0.75,
          fromthetop=FALSE,
          text.size=4) 
{

  names(parent.df) <- toupper(names(parent.df))
  
  if(is.null(category.col)==FALSE){
    km.df <- data.frame(CENTIME = parent.df[, centime.col], 
                        CENSOR = parent.df[, censor.col], 
                        CATEGORY = parent.df[, category.col])
    
    kmfit <- survfit(Surv(km.df$CENTIME, km.df$CENSOR) ~ km.df$CATEGORY)
    ifelse(fromthetop, 
           kmfit.out <- data.frame(XVALUES = summary(kmfit)$time, 
                                   YVALUES = summary(kmfit)$surv, 
                                   AT.RISK = summary(kmfit)$n.risk, 
                                   CATEGORY = factor(summary(kmfit)$strata)),
           kmfit.out <- data.frame(XVALUES = summary(kmfit)$time, 
                                   YVALUES = 1 - summary(kmfit)$surv, 
                                   AT.RISK = summary(kmfit)$n.risk,
                                   CATEGORY = factor(summary(kmfit)$strata))
    )
    # This renames the levels
    levels(kmfit.out$CATEGORY) <- unlist(strsplit(x = levels(kmfit.out$CATEGORY), 
                                                  split = "="))[seq(2, nlevels(kmfit.out$CATEGORY) * 2, 2)]
    add0 <- ddply(kmfit.out, .(CATEGORY), summarize, AT.RISK = max(AT.RISK))
    
    # Note: Adding values to the data set to ensure the step functions appropriately begin at the points
    # (0,1) or (0,0), resp.
    ifelse(fromthetop, 
{
  add0$XVALUES <- rep(0, nlevels(kmfit.out$CATEGORY))
  ad0$YVALUES <- rep(1, nlevels(kmfit.out$CATEGORY))
},
{
  add0$XVALUES <- rep(0, nlevels(kmfit.out$CATEGORY))
  add0$YVALUES <- rep(0, nlevels(kmfit.out$CATEGORY))
}
    )

add0 <- add0[, c("XVALUES", "YVALUES", "AT.RISK", "CATEGORY")]
kmfit.out <- rbind(kmfit.out, add0)

if(is.null(x.limits) || is.null(x.ticks)) {
  x.limits = c(min(kmfit.out$XVALUES, na.rm=T),
               max(kmfit.out$XVALUES, na.rm=T)); 
  x.ticks <- pretty(kmfit.out$XVALUES)
  cat("Either x.limits or x.ticks are set to NULL; defaults are used.\n")
}

if(is.null(y.limits) || is.null(y.ticks)) {
  y.limits = c(min(kmfit.out$YVALUES, na.rm=T),
               max(kmfit.out$YVALUES, na.rm=T)); 
  y.ticks <- pretty(kmfit.out$YVALUES)
  cat("Either y.limits or y.ticks are set to NULL; defaults are used.\n")
}

p1 <- ggplot(data = kmfit.out, 
             aes(x = XVALUES, y = YVALUES, 
                 colour = CATEGORY, linetype = CATEGORY)) + 
  geom_step(size = line.size) + 
  labs(x = x.label, y = y.label, colour = category.label, 
       linetype = category.label) + 
  scale_x_continuous(limits = x.limits, breaks = x.ticks) + 
  expand_limits(y = 0) + 
  scale_y_continuous(breaks = y.ticks, limits = y.limits, 
                     labels = percent_format()) + 
  scale_colour_manual(values = category.palette) + 
  scale_linetype_manual(values = linetype.palette) + 
  guides(colour = guide_legend(category.label), 
         linetype = guide_legend(category.label))

at.risk <- data.frame(XVALUES = summary(kmfit, time = x.ticks)$time, 
                      N = summary(kmfit, time = x.ticks)$n.risk, 
                      RISK = 1 - summary(kmfit, time = x.ticks)$surv, 
                      CATEGORY = summary(kmfit, time = x.ticks)$strata, 
                      TRTPOST = as.numeric(summary(kmfit, time = x.ticks)$strata)
)

levels(at.risk$CATEGORY) <- levels(kmfit.out$CATEGORY)

p2 <- nsubj.plot(parent.df = at.risk, 
                 category.palette = at.risk.palette, 
                 x.label = "Number of Subjects", 
                 y.label = nsubj.plot.label, 
                 text.size=text.size,
                 x.limits = x.limits, 
                 x.ticks = x.ticks, 
                 x.ticks.labels = rep("", length(x.ticks))
)

return(list(p1, p2, at.risk))}


if(is.null(category.col)==TRUE){
  
  km.df <- data.frame(CENTIME = parent.df[, centime.col], 
                      CENSOR = parent.df[, censor.col])
  
  kmfit <- survfit(Surv(km.df$CENTIME, km.df$CENSOR) ~ 1)
  kmfit
  
  ifelse(fromthetop, 
         kmfit.out <- data.frame(XVALUES = summary(kmfit)$time, 
                                 YVALUES = summary(kmfit)$surv, 
                                 AT.RISK = summary(kmfit)$n.risk),
         kmfit.out <- data.frame(XVALUES = summary(kmfit)$time, 
                                 YVALUES = 1 - summary(kmfit)$surv, 
                                 AT.RISK = summary(kmfit)$n.risk)
  )  
  # This renames the levels
  # Note: Adding values to the data set to ensure the step functions appropriately begin at the points
  # (0,1) or (0,0), resp.
  ifelse(fromthetop, {
    add0 <- data.frame(XVALUES=0, YVALUES=1, AT.RISK = 0)},
{add0 <- data.frame(XVALUES=0, YVALUES=0, AT.RISK = 0)}
  )
add0 <- add0[, c("XVALUES", "YVALUES", "AT.RISK")]
kmfit.out <- rbind(kmfit.out, add0)

if(is.null(x.limits) || is.null(x.ticks)) {
  x.limits = c(min(kmfit.out$XVALUES, na.rm=T),
               max(kmfit.out$XVALUES, na.rm=T))
  x.ticks <- pretty(kmfit.out$XVALUES)
  cat("Either x.limits or x.ticks are set to NULL; defaults are used.\n")
}
if(is.null(y.limits) || is.null(y.ticks)) {
  y.limits = c(min(kmfit.out$YVALUES, na.rm=T),
               max(kmfit.out$YVALUES, na.rm=T)); 
  y.ticks <- pretty(kmfit.out$YVALUES)
  cat("Either y.limits or y.ticks are set to NULL; defaults are used.\n")
}

p1 <- ggplot(data = kmfit.out, 
             aes(x = XVALUES, y = YVALUES )) + 
  geom_step(size = line.size) + 
  labs(x = x.label, y = y.label) + 
  scale_x_continuous(limits = x.limits, breaks = x.ticks) + 
  expand_limits(y = 0) + 
  scale_y_continuous(breaks = y.ticks, limits = y.limits, 
                     labels = percent_format()) 

at.risk <- data.frame(XVALUES = summary(kmfit, time = x.ticks)$time, 
                      N = summary(kmfit, time = x.ticks)$n.risk, 
                      RISK = 1 - summary(kmfit, time = x.ticks)$surv ,
                      CATEGORY = factor(1)
)
p2 <- nsubj.plot(parent.df = at.risk, 
                 category.palette = at.risk.palette, 
                 x.label = "Number of Subjects", 
                 y.label = nsubj.plot.label,                  
                 x.limits = x.limits, 
                 x.ticks = x.ticks, 
                 x.ticks.labels = rep("", length(x.ticks)))
return(list(p1, p2, at.risk))}
}

# gcurve ----
#' @title gcurve 
#' @description A function to exploit base R's curve function.  This returns a data.frame holding x and y values returned from a call to curve, but suppress the plotting of that function
#' @inheritParams graphics::curve
#' @param category option to add a column populated with a factor
#' @author Greg Cicconetti
gcurve <- function (expr, from = NULL, to = NULL, n = 101, add = FALSE, 
          type = "l", xname = "x", xlab = xname, ylab = NULL, log = NULL, 
          xlim = NULL,category=NULL,...) 
{
  sexpr <- substitute(expr)
  if (is.name(sexpr)) {
    expr <- call(as.character(sexpr), as.name(xname))
  }
  else {
    if (!((is.call(sexpr) || is.expression(sexpr)) && xname %in% 
            all.vars(sexpr))) 
      stop(gettextf("'expr' must be a function, or a call or an expression containing '%s'", 
                    xname), domain = NA)
    expr <- sexpr
  }
  if (dev.cur() == 1L && !identical(add, FALSE)) {
    warning("'add' will be ignored as there is no existing plot")
    add <- FALSE
  }
  addF <- identical(add, FALSE)
  if (is.null(ylab)) 
    ylab <- deparse(expr)
  if (is.null(from) || is.null(to)) {
    xl <- if (!is.null(xlim)) 
      xlim
    else if (!addF) {
      pu <- par("usr")[1L:2L]
      if (par("xaxs") == "r") 
        pu <- extendrange(pu, f = -1/27)
      if (par("xlog")) 
        10^pu
      else pu
    }
    else c(0, 1)
    if (is.null(from)) 
      from <- xl[1L]
    if (is.null(to)) 
      to <- xl[2L]
  }
  lg <- if (length(log)) 
    log
  else if (!addF && par("xlog")) 
    "x"
  else ""
  if (length(lg) == 0) 
    lg <- ""
  if (grepl("x", lg, fixed = TRUE)) {
    if (from <= 0 || to <= 0) 
      stop("'from' and 'to' must be > 0 with log=\"x\"")
    x <- exp(seq.int(log(from), log(to), length.out = n))
  }
  else x <- seq.int(from, to, length.out = n)
  ll <- list(x = x)
  names(ll) <- xname
  y <- eval(expr, envir = ll, enclos = parent.frame())
#   if (length(y) != length(x)) 
#     stop("'expr' did not evaluate to an object of length 'n'")
#   if (isTRUE(add)) 
#     lines(x = x, y = y, type = type, ...)
#   else plot(x = x, y = y, type = type, xlab = xlab, ylab = ylab, 
#             xlim = xlim, log = lg, ...)
for.return <- data.frame(x = x, y = y)
if(is.null(category)==FALSE) for.return$category <- factor(category)
return(for.return)
}

# lasso.plot-----
#' @title lasso.plot
#' @description This function takes a glmnet object and builds a data.frame create ggplot versions of glmnet plot methods - expand to offer 3 standards and return a list
#' @param lasso.model glmnet object
#' @author Greg Cicconetti

lasso.plot <- function(lasso.model=lasso.mod){
  lasso.df <- data.frame(t(rbind(lasso.model$lambda, 
                                 as.matrix(lasso.mod$beta))))
  names(lasso.df)[1]<- "lambda"
  # Compute L1 Norm of coefficient vector
  lasso.df$L1.norm <- rowSums(abs(lasso.df[,-1]))
  lasso.df.melt <- melt(lasso.df, id=c("lambda", "L1.norm"))
  ggplot(lasso.df.melt) + 
    aes(x=L1.norm/max(L1.norm), y=value, color=variable) + 
    geom_line(size=.75)+
    labs(x=expression(paste(L[1], " Norm = ", sum)), y="Coefficients")
  
  head(testing)
  testing$L1.norm <- rowSums(abs(testing[,-1]))
  head(testing)
  testing.melt <- melt(testing, id=c("lambda", "L1.norm"))
  ggplot(testing.melt) + 
    aes(x=L1.norm, y=value, color=variable) + 
    geom_line(size=.75)
  
  ggplot(testing.melt) + 
    aes(x=log(lambda), y=value, color=variable) + 
    geom_line(size=.75)+xlim(-8,-3)
  
  plot(lasso.mod, xvar="norm")
  ggplot(models.l2.melt) + 
    aes(x=abs(value)/sum(abs(value)), y=value, color=variable) + 
    geom_line(size=.75)
  
  hist(abs(models.l2.melt$value)/sum(abs(models.l2.melt$value)))
  
  
  reg.l1 <- glmnet(features, target, alpha=1)
  
  
  models.l2$dev.ratio <-lasso.mod$dev.ratio
  
  colnames(models.l2)[1] <- "lambda"
  models.l2.melt <- melt(models.l2, c("lambda"))
  
  coef_l1 <- ggplot(models.l2.melt) + 
    aes(x=-log(lambda), y=value, color=variable) + geom_line(size=.75)+
    xlim(3.5, 10)
  
 # plot(lasso.mod, xvar="lambda")
  coef_l1 <- ggplot(models.l2.melt) + 
    aes(x=log(lambda), y=value, color=variable) + geom_line(size=.75)
  
 # plot(lasso.mod, xvar="dev")
  ggplot(models.l2.melt) + 
    aes(x=dev.ratio, y=value, color=variable) + geom_line(size=.75)
  
}

