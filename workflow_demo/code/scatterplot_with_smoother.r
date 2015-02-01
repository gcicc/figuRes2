#*******************************************************************************
# Program Name: StudyXYZ 
# Datafile Name: g_bslchar.csv  
# Output Name: scatter_smooth_facet.PDF
# Protocol ID: Protocol: Comp1234
# Plot Type: Scatterplot
# Figure Title: Scatterplot of Height vs Weight by Region with Smoothers By Region
# table references: Table 1.3
# Author: GC
# Creation Date: 2014-08-13
#******************************************************************************* 

# Begin tracking newly created objects, sync with outputplan and start log file ----
starting.objects <- ls()

filename <- "scatter_smooth_facet.PDF"
i <- which(outputplan$outputfile==filename) # i identifies the outputplan row associated with figure
if(!(filename %in% outputplan$outputfile)) cat(paste("Warning:", filename, "is not in the outputplan.\n"))

start.session.log(outputfile=filename)

# Address mathematical symbols for headers/footers/titles here-------------
# This step is only needed a minority of figures

# Load or create data here---------------
# Data typically comes from a .csv or other file type
parent.df <- read.csv(paste0(dd, "g_bslchar.csv"))

# Pre-Process data here----
# Adjust factor levels, re-order data, etc.
working.df <- parent.df

# Create ggplot objects ----
# Typically a call to a canned figuRes package function or ggplot objects built from scratch
# There may be multiple graphs to produce in this step
for.build <- ggplot(data=working.df, aes(x=HEIGHT, y=WEIGHT, color=REGRAP))+
  geom_point(alpha=.4)+
  facet_wrap(~REGRAP)+
  stat_smooth(se=FALSE, size=.75, color="black")+
  guides(color=FALSE)+
  labs(x="Height", y="Weigth")

# Post process ggplot objects----
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
## This step removes any objects that were created since the start of the driver
stop.session.log()
ending.objects <- ls()
remove(list=ls()[ls() %in% setdiff(ending.objects, starting.objects )] )

