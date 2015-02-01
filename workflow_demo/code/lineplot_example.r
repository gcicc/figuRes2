#*******************************************************************************
# Program Name: StudyXYZ 
# Datafile Name: lineplot_data.csv
# Output Names: lineplot_example.PDF
# Protocol ID: Protocol: Comp1234
# Plot Type: Boxplot
# Figure Title: Lineplot Example 
# table references: Table 1.1
# Author: Boxplot of Age
# Creation Date: 2015-01-31
#******************************************************************************* 

# Begin tracking newly created objects, sync with outputplan and start log file ----
starting.objects <- ls()

filename <- "lineplot_example.PDF"
i <- which(outputplan$outputfile==filename) # i identifies the outputplan row associated with figure
if(!(filename %in% outputplan$outputfile)) cat(paste("Warning:", filename, "is not in the outputplan.\n"))

start.session.log(outputfile=filename)

# Address mathematical symbols for headers/footers/titles here ----
# This step is only needed a minority of figures

# Load or create data here ----
# Data typically comes from a .csv or other file type
incoming.df <- read.csv(paste0(dd, "lineplot_data.csv"))

# Pre-Process data here ----
# Adjust factor levels, re-order data, etc.
working.df <- incoming.df

# Create ggplot objects ----
# Typically a call to a canned figuRes package function or ggplot objects built from scratch
# There may be multiple graphs to produce in this step

for.build <- box.plot(parent.df = working.df, 
         y.col = "GC", 
         y.label = "GC",
         category.col = "TRTGRP", 
         category.label = "Treatment Group",
         y.limits = NULL, 
         y.ticks = NULL, 
         y.digits = 0,
         shape.palette = boxplot.shape.palette, 
         category.palette = treatment.palette,
         bplot.text = 4)

# Post process ggplot objects ----
# E.g., when working with annotate.page, all top-row graphs need to have dummy blank titles installed
# Objects may need further manipulation, e.g., mathematical symbols for tick labels

for.build <- for.build+ggtitle(paste("", rep("\n", outputplan[i,]$nTitleLines-1), collapse=""))

# Assemble objects on page----
# Adjust the bottom margin and graph.region width to account for variable footnote lines
variable.bottom.margin <- .92 + (outputplan[i,]$nFootLines+1)*.165 -.5
# page.height, right.margin, left.margin were set by default.settings

# h.partition and w.partition length should correspond with nrow and ncol, resp.
h.partition <- c(1)
w.partition <- c(1)

# The call that assembles the graphics
build.page(
  interior.h = h.partition,
  interior.w = w.partition,
  bottom.margin=variable.bottom.margin,
  ncol=1, nrow=1,
  interior = list(for.build))


# Annotate-----
# The call to add annotation
annotate.page(override="outputplan")


# Clean up ----
## This closes log file and removes any objects that were created since the start of the driver
stop.session.log()
ending.objects <- ls()
remove(list=ls()[ls() %in% setdiff(ending.objects, starting.objects )] )

