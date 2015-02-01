#*******************************************************************************
# Program Name: StudyXYZ 
# Datafile Name: g_bslchar.csv  
# Output Name:  jitter_weight_faceted.PDF
# Protocol ID: Protocol: Comp1234
# Plot Type: CDF
# Figure Title: Plot of Individual Weigths with Man and 95% Confidence Interval
# table references: Table 1.3
# Author: GC
# Creation Date: 2014-08-13
#******************************************************************************* 

# Begin tracking newly created objects, sync with outputplan and start log file ----
starting.objects <- ls()

filename <- "jitter_weight_faceted.PDF"
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

jitter.df <- working.df
jitter.df.ply <- ddply(working.df, .(TRTGRP, REGRAP), 
                       summarise,
                       mean=mean(WEIGHT, na.rm=T),
                       lower=mean(WEIGHT, na.rm=T) - qt(.975, df=sum(is.na(WEIGHT)==F)) *sd(WEIGHT, na.rm=T),
                       upper=mean(WEIGHT, na.rm=T) + qt(.975, df=sum(is.na(WEIGHT)==F))*sd(WEIGHT, na.rm=T))
names(jitter.df.ply)[3] <- "WEIGHT"

# Create ggplot objects ----
# Typically a call to a canned figuRes package function or ggplot objects built from scratch
# There may be multiple graphs to produce in this step
for.build <- ggplot(data=jitter.df, aes(x= TRTGRP, y= WEIGHT)) + 
  geom_point(position="jitter", alpha=.1)+
  geom_errorbar(data=jitter.df.ply,
                aes(x=TRTGRP,
                    ymin=lower, ymax=upper, 
                    color=TRTGRP), size=.75, width=.25)+
  geom_point(data=jitter.df.ply,
             aes(x=TRTGRP, y=WEIGHT),
             color="red", size=3, bg = "white", shape=23)+
  labs(x="Treatment Group", y="Weight", color="Treatment Group")+
  facet_wrap(~REGRAP)

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
ending.objects <- ls()
remove(list=ls()[ls() %in% setdiff(ending.objects, starting.objects )] )

