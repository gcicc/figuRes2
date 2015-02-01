#*******************************************************************************
# Program Name: StudyXYZ 
# Datafile Name: None  
# Output Name: priordens.PDF
# Protocol ID: Protocol: Comp1234
# Plot Type: Density Plots
# Figure Title: Density Plot E0 (Normal Distribution) for Primary and Sensitivity Priors 
# table references: Table 1.7
# Author: GC
# Creation Date: 2014-08-13
#******************************************************************************* 

# Begin tracking newly created objects, sync with outputplan and start log file ----
starting.objects <- ls()

filename <- "priordens.PDF"
i <- which(outputplan$outputfile==filename) # i identifies the outputplan row associated with figure
if(!(filename %in% outputplan$outputfile)) cat(paste("Warning:", filename, "is not in the outputplan.\n"))

start.session.log(outputfile=filename)

# Address mathematical symbols for headers/footers/titles here-------------
# This step is only needed a minority of figures
outputplan[i,]$FigureTitle1 <- expression(paste("Density Plots for E", phantom()[0]))
outputplan[i,]$FigureTitle2 <- "Normal Distributions are Used for Primary and Sensitivity Priors"
outputplan[i,]$nTitleLines <- 2

# Load or create data here---------------
# Data typically comes from a .csv or other file type
my.mean <- 1.2
my.sd <- c(.45, 1)
working.df <- rbind(
  gcurve(dnorm(x, my.mean, my.sd[1]), from=-4, 6, n=1000, category="Primary"),
  gcurve(dnorm(x, my.mean, my.sd[2]), from=-4, 6, n=1000, category="Sensitivity"))


# Pre-Process data here----
# Adjust factor levels, re-order data, etc.


# Create ggplot objects ----
# Typically a call to a canned figuRes package function or ggplot objects built from scratch
# There may be multiple graphs to produce in this step
for.build <-   ggplot(data=working.df, aes(x=x,y=y, color=category,linetype=category))+ 
  geom_hline(yintercept=0)+
  geom_line(size=.75)+
  labs(x=expression(paste("Possible E", phantom()[0], " Values")), y="Density", color="Prior", linetype="Prior")+
  scale_color_manual(values=c("red","blue"))+
  scale_x_continuous(breaks=c(1.2, seq(-4, 6, 2)))+
  geom_vline(xintercept=1.2, linetype="dotted", color="grey20")

# Post process ggplot objects----
# E.g., when working with annotate.page, all top-row graphs need to have dummy blank titles installed
# Objects may need further manipulation, e.g., mathematical symbols for tick labels
for.build <- for.build+ggtitle(paste("", rep("\n", outputplan[i,]$nTitleLines-1), collapse=""))

paste(rep("\n",4-1), collapse="")
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
