## -----------------------------------------------------------------------------
require(tidyverse)
require(gridExtra)
require(scales)
require(figuRes2)

## -----------------------------------------------------------------------------
x <- rnorm(100)
hist(x)
boxplot(x)
plot(x)

## -----------------------------------------------------------------------------
my.df <- data.frame(x=x)
ggplot(data=my.df, aes(x=x)) + geom_histogram()
ggplot(data=my.df, aes(x=factor(""), y=x)) + geom_boxplot()
ggplot(data=my.df, aes(x=1:100, y=x))+geom_point()

## -----------------------------------------------------------------------------
# dframe=read.csv("demog.csv")
data(demog.data)

## -----------------------------------------------------------------------------
head(demog.data)

## -----------------------------------------------------------------------------
table(demog.data$REGION)
data.frame(table(demog.data$REGION))

## -----------------------------------------------------------------------------
ggplot(data=demog.data, aes(x=REGION)) + geom_bar()

## -----------------------------------------------------------------------------
ggplot(data=demog.data, aes(x=REGION)) + geom_bar() + coord_flip()

## -----------------------------------------------------------------------------
p <- ggplot(data=demog.data, aes(x=REGION)) + geom_bar() + coord_flip()
# print or simply send p to command line
print(p)

## -----------------------------------------------------------------------------
p + labs(title="Barchart of Regions", y = "Count", x="Region", subtitle="My subtitle", caption="My caption")

## -----------------------------------------------------------------------------
p + scale_y_continuous(breaks=seq(0,6000, 500), limits= c(0, 5000))

# Note that tick mark at 5000 is not printed.
p + scale_y_continuous(breaks=seq(0,4500, 500), limits= c(0, 5000))

## -----------------------------------------------------------------------------
p + labs(title="Barchart of Regions", y = "Count", x="Region", subtitle="My subtitle", caption="My caption")+ scale_y_continuous(breaks=seq(0,4000, 500))

## -----------------------------------------------------------------------------
class(demog.data)
demog.data$REGION
class(demog.data$REGION)
levels(demog.data$REGION) # <-- we need to convert this to a factor

## -----------------------------------------------------------------------------
levels(factor(demog.data$REGION)) 

# Indeed, Alphabetical ordering
all.equal(levels(factor(demog.data$REGION)),
          levels(factor(demog.data$REGION, c("Asia/Pacific",
                                         "Eastern Europe",
                                         "North America",
                                         "South America",  
                                         "Western Europe"))))

## -----------------------------------------------------------------------------
levels(factor(demog.data$REGION, c("Asia/Pacific",
                               "Eastern Europe",
                               "Western Europe",
                               "North America",
                               "South America")))

## -----------------------------------------------------------------------------
# We really want to order these by frequency...
data.frame(original.order = levels(factor(demog.data$REGION)),
           original.frequency = data.frame(table(demog.data$REGION))$Freq,
           map=order(table(demog.data$REGION)),
           new.order=levels(factor(demog.data$REGION))[order(table(demog.data$REGION))])

## -----------------------------------------------------------------------------
demog.data$REGION2 <- factor(demog.data$REGION, c("Asia/Pacific",
                                          "Eastern Europe",
                                          "North America",
                                          "South America",  
                                          "Western Europe")[order(table(demog.data$REGION))])

demog.data$REGION3 <- factor(demog.data$REGION, c("Asia/Pacific",
                                          "Eastern Europe",
                                          "North America",
                                          "South America",  
                                          "Western Europe")[rev(order(table(demog.data$REGION)))])

## -----------------------------------------------------------------------------
p1 <- ggplot(data=demog.data, aes(x=REGION)) + geom_bar() + coord_flip()
p2 <- ggplot(data=demog.data, aes(x=REGION2)) + geom_bar() + coord_flip()
p3 <- ggplot(data=demog.data, aes(x=REGION3)) + geom_bar() + coord_flip()

grid.arrange(p1 + labs(title="Default ordering - alphabetical"), 
             p2 + labs(title="Ordered by frequency"),
             p3 + labs(title="Ordered by Frequency & reversed"), ncol=1)

## -----------------------------------------------------------------------------
# Note the use of dplyr::rename to rename columns

demog.data.table.original <- data.frame(table(demog.data$REGION))
demog.data.table.renamed <- demog.data.table.original %>% rename(REGION = Var1)

demog.data.table.original
demog.data.table.renamed

## -----------------------------------------------------------------------------
ggplot(data=demog.data.table.renamed, aes(x=REGION, y=Freq)) + geom_bar(stat="identity")

## -----------------------------------------------------------------------------
demog.data.table.original$prop <- demog.data.table.original$Freq/sum(demog.data.table.original$Freq)
ggplot(data=demog.data.table.original, aes(x=Var1, y=prop)) + geom_bar(stat="identity")

## -----------------------------------------------------------------------------
demog.data.table <- data.frame(table(demog.data$REGION, demog.data$SEX))
demog.data.table$prop <- demog.data.table$Freq/sum(demog.data.table$Freq)



p1 <- ggplot(data = demog.data.table, aes(x = Var1, y = prop, fill = Var2)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  geom_text(position=position_dodge(width=1), size = 5, 
             aes(y=prop+.02, label=paste(" n = ", Freq, sep = "")))+
  scale_y_continuous(limits = c(0,.3), breaks = seq(0,1,.05), labels = percent_format()) + 
  scale_fill_manual(values = c("blue", "brown")) + 
  labs(y = "Proportion", x = "Region", fill = "Gender", title="Barchart of Region and Gender")+
  theme(legend.position= "bottom")
p1

## -----------------------------------------------------------------------------
curve(expr = dnorm(x), from=-6, to=6, n=1001)
temp <- curve(expr = dnorm(x), from=-6, to=6, n=1001)
class(temp)
str(temp)

## ----eval=F-------------------------------------------------------------------
#  head(data.frame(temp))

## ----eval=F-------------------------------------------------------------------
#  head(figuRes2::gcurve(expr = dnorm(x), from=-6, to=6, n=101, category="Standard Normal"))

## ----eval=F-------------------------------------------------------------------
#  my.df <- rbind(
#    gcurve(expr = dnorm(x), from=-6, to=6, n=101, category="N(0, 1)"),
#    gcurve(expr = dnorm(x, -1, 1), from=-6, to=6, n=101, category="N(-1, 1)"),
#    gcurve(expr = dnorm(x, 1, 1), from=-6, to=6, n=101, category="N(1, 1)"))
#  
#  ggplot(data=my.df, aes(x=x, y=y, color=category))+geom_line()

## ----eval=F-------------------------------------------------------------------
#  grid.arrange(
#  ggplot(data=my.df, aes(x=x, y=y, color=category))+
#    geom_line(size=.75)+
#    labs(x="x", y="",
#         title="Normal Density Functions",
#         color="Density")+
#    theme(legend.position="bottom"),
#  
#  ggplot(data=my.df, aes(x=x, y=y, color=category))+
#    geom_line(size=.75)+
#    labs(x="x", y=NULL,
#         title="Normal Density Functions",
#         color="Density")+
#    theme(legend.position="bottom"),
#  ggplot(data=my.df, aes(x=x, y=y, color=category))+
#    geom_line(size=.75)+
#    labs(x="x", y=NULL,
#         title="Normal Density Functions",
#         color="Density")+
#    theme(legend.position="bottom")+
#    scale_y_continuous(breaks=NULL)
#  )

## ----eval=F-------------------------------------------------------------------
#  
#  head(demog.data)
#  str(demog.data)
#  sort(names(demog.data))

## ----eval=F-------------------------------------------------------------------
#  ggplot(data=demog.data, aes(x=HEIGHT)) + geom_histogram()
#  summary(demog.data$HEIGHT)

## ----eval=F-------------------------------------------------------------------
#  # note na.rm = T is needed
#  max(demog.data$HEIGHT, na.rm=T) - min(demog.data$HEIGHT, na.rm=T)
#  median(demog.data$HEIGHT)
#  mean(demog.data$HEIGHT)
#  median(demog.data$HEIGHT, na.rm=T)
#  mean(demog.data$HEIGHT, na.rm=T)

## ----eval=F-------------------------------------------------------------------
#  grid.arrange(
#  ggplot(data=demog.data, aes(x=HEIGHT)) + geom_histogram(binwidth = .5) + ggtitle("binwidth = .5"),
#  ggplot(data=demog.data, aes(x=HEIGHT)) + geom_histogram(binwidth = 1)+ ggtitle("binwidth = 1"),
#  ggplot(data=demog.data, aes(x=HEIGHT)) + geom_histogram(binwidth = 2.5)+ ggtitle("binwidth = 2.5"),
#  ggplot(data=demog.data, aes(x=HEIGHT)) + geom_histogram(binwidth = 25)+ ggtitle("binwidth = 25"),
#  ncol=2)

## ----eval=F-------------------------------------------------------------------
#  grid.arrange(
#  ggplot(data=demog.data, aes(x=HEIGHT)) + geom_histogram(binwidth = .5, color="red", fill="blue") + ggtitle("binwidth = .5"),
#  ggplot(data=demog.data, aes(x=HEIGHT)) + geom_histogram(binwidth = 1, color="red", fill="blue")+ ggtitle("binwidth = 1"),
#  ggplot(data=demog.data, aes(x=HEIGHT)) + geom_histogram(binwidth = 2.5, color="red", fill="blue")+ ggtitle("binwidth = 2.5"),
#  ggplot(data=demog.data, aes(x=HEIGHT)) + geom_histogram(binwidth = 25, color="red", fill="blue")+ ggtitle("binwidth = 25"),
#  ncol=2)

## ----eval=F-------------------------------------------------------------------
#  q <- ggplot(data=demog.data, aes(x=HEIGHT)) +  ggtitle("binwidth = 2.5")
#  
#  grid.arrange(
#    q + geom_histogram(binwidth = 2.5, color="red", fill="blue", alpha=0) + labs(subtitle = "alpha = 0"),
#    q + geom_histogram(binwidth = 2.5, color="red", fill="blue", alpha=.2) + labs(subtitle = "alpha = 0.2"),
#    q + geom_histogram(binwidth = 2.5, color="red", fill="blue", alpha=.4) + labs(subtitle = "alpha = 0.4"),
#    q + geom_histogram(binwidth = 2.5, color="red", fill="blue", alpha=.6) + labs(subtitle = "alpha = 0.6"),
#    q + geom_histogram(binwidth = 2.5, color="red", fill="blue", alpha=.8) + labs(subtitle = "alpha = 0.8"),
#    q + geom_histogram(binwidth = 2.5, color="red", fill="blue", alpha=1) + labs(subtitle = "alpha = 1.0")
#  )

## ----eval=F-------------------------------------------------------------------
#   q + geom_histogram(binwidth = 2.5, color="red", fill="blue", alpha=.6) +
#    scale_x_continuous(breaks=seq(0,500,10), limits=c(100,225)) +
#    scale_y_continuous(breaks=seq(0, 3000, 250)) +
#    labs(subtitle = "alpha = 0.6", caption="Scaling of axes accomplished with:\nscale_x_continuous(breaks=seq(0,500,10), limits=c(100,225))\nscale_y_continuous(breaks=seq(0, 3000, 250))")

## ----eval=F-------------------------------------------------------------------
#  demog.data$SEX <- factor(demog.data$SEX, c("F", "M")) # Alphabetical ordering
#  demog.data$SEX2 <- factor(demog.data$SEX, c("M", "F")) # Preferred order
#  table(demog.data$SEX)
#  table(demog.data$SEX2)
#  
#  grid.arrange(
#  ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX)) +
#    geom_histogram(bin=2.5),
#  ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX2)) +
#    geom_histogram(bin=2.5))

## ----eval=F-------------------------------------------------------------------
#  grid.arrange(
#  ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX)) +
#    geom_histogram(bin=2.5, alpha=.2),
#  ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX2)) +
#    geom_histogram(bin=2.5, alpha=.2))
#  

## ----eval=F-------------------------------------------------------------------
#  grid.arrange(
#  ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX)) +
#    geom_histogram(bin=2.5, alpha=.2) + facet_wrap(~SEX) + ggtitle("facet_wrap default"),
#  ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX)) +
#    geom_histogram(bin=2.5, alpha=.2) + facet_wrap(~SEX, scales="free_x") + ggtitle('scales="free_x"'),
#  ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX)) +
#    geom_histogram(bin=2.5, alpha=.2) + facet_wrap(~SEX, scales="free_y") + ggtitle('scales="free_y"'),
#  ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX)) +
#    geom_histogram(bin=2.5, alpha=.2) + facet_wrap(~SEX, scales="free") + ggtitle('scales="free"'), ncol=2)
#  
#  grid.arrange(
#  ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX)) +
#    geom_histogram(bin=2.5, alpha=.2) + facet_wrap(~SEX, ncol=1) + ggtitle("facet_wrap default, ncol=1"),
#  ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX)) +
#    geom_histogram(bin=2.5, alpha=.2) + facet_wrap(~SEX, scales="free_x", ncol=1) + ggtitle('scales="free_x", ncol=1'),
#  ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX)) +
#    geom_histogram(bin=2.5, alpha=.2) + facet_wrap(~SEX, scales="free_y", ncol=1) + ggtitle('scales="free_y", ncol=1'),
#  ggplot(data=demog.data, aes(x=HEIGHT, fill=SEX)) +
#    geom_histogram(bin=2.5, alpha=.2) + facet_wrap(~SEX, scales="free", ncol=1) + ggtitle('scales="free", ncol=1'), ncol=2)

## ----eval=F-------------------------------------------------------------------
#  summary(demog.data$HSCRP) # Note we have: Mean >> median
#  ggplot(data=demog.data, aes(x=factor("All Subjects"), y=HSCRP))+geom_boxplot()
#  ggplot(data=demog.data, aes(x=HSCRP))+geom_histogram()

## ----eval=F-------------------------------------------------------------------
#  ggplot(data=demog.data, aes(x=factor("All Subjects"), y=log(HSCRP)))+geom_boxplot()
#  ggplot(data=demog.data, aes(x=SEX, y=log(HSCRP)))+geom_boxplot()

## ----eval=F-------------------------------------------------------------------
#  p1 <- ggplot(data=demog.data, aes(x=factor("All Subjects"), y=HSCRP))+geom_boxplot()
#  p2 <- ggplot(data=demog.data, aes(x=factor("All Subjects"), y=log(HSCRP)))+geom_boxplot()
#  
#  grid.arrange(p1, p2)
#  grid.arrange(p1, p2, ncol=2)
#  # First we need to melt the data from wide to narrow form
#  # This requires the reshape package

## ----eval=F-------------------------------------------------------------------
#  grid.arrange(
#  demog.data %>%
#    # Add new variable
#    mutate(log.HSCRP = log(HSCRP)) %>%
#    # Retain pertinent columns
#    select(STUDYID,  INVID ,CENTREID, USUBJID, SUBJID, HSCRP, log.HSCRP) %>%
#    pivot_longer(cols=c(HSCRP, log.HSCRP)) %>%
#    ggplot(aes(x=name, y=value))+geom_boxplot() + ggtitle("First pass after pivot_longer"),
#  
#  
#  demog.data %>%
#    # Add new variable
#    mutate(log.HSCRP = log(HSCRP)) %>%
#    # Retain pertinent columns
#    select(STUDYID,  INVID ,CENTREID, USUBJID, SUBJID, HSCRP, log.HSCRP) %>%
#    pivot_longer(cols=c(HSCRP, log.HSCRP)) %>%
#    ggplot(aes(x=name, y=value))+geom_boxplot()+labs(title="Plotting on same scale obscures interpretation."),
#  
#  demog.data %>%
#    # Add new variable
#    mutate(log.HSCRP = log(HSCRP)) %>%
#    # Retain pertinent columns
#    select(STUDYID,  INVID ,CENTREID, USUBJID, SUBJID, HSCRP, log.HSCRP) %>%
#    pivot_longer(cols=c(HSCRP, log.HSCRP)) %>%
#    ggplot(aes(x=name, y=value))+geom_boxplot() + facet_wrap(~name) + labs(title="aesthetic mapping associates 'name' to x-axis", subtitle="Superfluous category labels on x-axis (we have facet labels);\nsuperfluous tick marks on x-axis"),
#  
#  demog.data %>%
#    # Add new variable
#    mutate(log.HSCRP = log(HSCRP)) %>%
#    # Retain pertinent columns
#    select(STUDYID,  INVID ,CENTREID, USUBJID, SUBJID, HSCRP, log.HSCRP) %>%
#    pivot_longer(cols=c(HSCRP, log.HSCRP)) %>%
#    ggplot(aes(y=value))+geom_boxplot() + facet_wrap(~name) + labs(title="aesthetic mapping simplified", subtitle="No option passed to facet_wrap"),
#  
#  demog.data %>%
#    # Add new variable
#    mutate(log.HSCRP = log(HSCRP)) %>%
#    # Retain pertinent columns
#    select(STUDYID,  INVID ,CENTREID, USUBJID, SUBJID, HSCRP, log.HSCRP) %>%
#    pivot_longer(cols=c(HSCRP, log.HSCRP)) %>%
#    ggplot(aes(y=value))+geom_boxplot() + facet_wrap(~name, scales="free_y")+ labs(title="aesthetic mapping simplified", subtitle="'free_y' passed to facet_wrap"),
#  
#  demog.data %>%
#    # Add new variable
#    mutate(log.HSCRP = log(HSCRP)) %>%
#    # Retain pertinent columns
#    select(STUDYID,  INVID ,CENTREID, USUBJID, SUBJID, HSCRP, log.HSCRP) %>%
#    pivot_longer(cols=c(HSCRP, log.HSCRP)) %>%
#    ggplot(aes(y=value))+geom_boxplot() + facet_wrap(~name, scales="free_y")+
#    scale_x_continuous(breaks=NULL)+ labs(title="x-axis text suppressed - it serves no role. ", subtitle="No option passed to facet_wrap"), ncol=2)
#  

## ----eval=F-------------------------------------------------------------------
#  # Short coming of this: An attempt to increase ticks one facet impacts appearance on the other.
#  # The alternative is plot these two graphics separately and then work with functions from grid package to paste things together
#  ggplot(data=demog.data2.melt, aes(x=TRTGRP, y=value, fill=TRTGRP)) +
#    geom_boxplot() +
#    facet_wrap(~variable, scales="free_y")+theme(legend.position="bottom", axis.text.x=element_text(angle=45, hjust=1,vjust=1) )+
#    scale_y_continuous(breaks= c(seq(-5,5,1), seq(0,150,25)))

## ----eval=F-------------------------------------------------------------------
#  ggplot(data=demog.data, aes(x=factor(""), y=HEIGHT)) + geom_violin()
#  ggplot(data=demog.data, aes(x=factor(""), y=HEIGHT)) + geom_violin() + labs(x="All Patients", y="Height")
#  ggplot(data=demog.data, aes(x=SEX, y=HEIGHT, fill=VSBMIG)) + geom_violin()
#  
#  ggplot(data=demog.data, aes(x=SEX, y=HEIGHT, fill=SEX)) + geom_violin() + facet_wrap(~REGION)
#  ggplot(data=demog.data, aes(x=SEX, y=HEIGHT, fill=SEX)) + geom_violin() + facet_wrap(~REGION, nrow=1)
#  ggplot(data=demog.data, aes(x=SEX, y=HEIGHT, fill=SEX)) + geom_violin() + facet_wrap(VSBMIG~REGION)
#  ggplot(data=demog.data, aes(x=SEX, y=HEIGHT, fill=SEX)) + geom_violin() + facet_grid(REGION~VSBMIG)
#  

## ----eval=F-------------------------------------------------------------------
#  # First get rid of missing category
#  demog.data2 <- subset(demog.data, VSBMIG != "Missing")
#  levels(demog.data$VSBMIG)
#  # Problem 1 - levels are out of order
#  demog.data2$VSBMIG <- factor(demog.data2$VSBMIG, c("<25 kg/m2", "25-<30 kg/m2", ">=30 kg/m2" , "Missing"))
#  levels(demog.data2$VSBMIG)
#  # Better
#  ggplot(data=demog.data2, aes(x=SEX, y=HEIGHT, fill=SEX)) + geom_violin() + facet_grid(REGION~VSBMIG)
#  # Punching it up; Note use of escape character '\n' for new line.
#  p <- ggplot(data=demog.data2, aes(x=SEX, y=HEIGHT, fill=SEX)) + geom_violin() + facet_grid(REGION~VSBMIG)+
#    theme(legend.position="none", strip.text=element_text(size=13)) + labs(x="Gender", y="Height", title="Violin plots of Height by Gender\nfor each BMI Group and Region")
#  p

## ----eval=F-------------------------------------------------------------------
#  # A little more work to get symbols - this is just to whet your appetite
#  # Redefining factor with 3 levels.  These are expressions that allow superscripts.
#  # look at help files for phantom - it will introduce you to displaying mathematical expressions
#  #
#  temp1 <- expression(paste(phantom()<=25," kg/",m^2))
#  temp2 <- expression(paste("25-<30 kg/", m^2))
#  temp3 <- expression(paste(phantom() >= 30, " kg/", m^2))
#  demog.data2$VSBMIG2 <- factor(demog.data2$VSBMIG, levels=c("<25 kg/m2", "25-<30 kg/m2", ">=30 kg/m2"),labels=c(temp1, temp2, temp3))
#  
#  

## ----eval=F-------------------------------------------------------------------
#  t1 <- expression(paste("Asia/ Pacific"))
#  t2 <- expression(paste("Eastern Europe"))
#  t3 <- expression(paste("North America"))
#  t4 <- expression(paste("South America"))
#  t5 <- expression(paste("Western Europe"))

## ----eval=F-------------------------------------------------------------------
#  demog.data2$REGION2 <- factor(demog.data2$REGION, levels =levels(demog.data2$REGION), labels=c(t1, t2, t3, t4, t5) )
#  
#  d <- ggplot(data=demog.data2, aes(x=SEX, y=HEIGHT, fill=SEX)) +
#    geom_violin() +
#    facet_grid(REGION~VSBMIG2,labeller="label_parsed")+
#    labs(x="Gender", y="Height", title=expression(paste("Gender vs. Height by Region and BMI Group" )))+
#    theme(legend.position="none", strip.text=element_text(size=13)) +
#    labs(x="Gender", y="Height", title="Violin plots of Height by Gender\nfor each BMI Group and Region")
#  
#  d

## ----eval=F-------------------------------------------------------------------
#  ggplot(data=demog.data2, aes(x=SEX, y=HEIGHT, fill=SEX)) +
#    geom_boxplot() +
#    facet_grid(REGION2~VSBMIG2,labeller="label_parsed")+
#    labs(x="Gender", y="Height", title=expression(paste("Gender vs. Height by Region and BMI Group" )))+
#    theme(legend.position="none", strip.text=element_text(size=13)) + labs(x="Gender", y="Height", title="Violin plots of Height by Gender\nfor each BMI Group and Region")
#  

## ----eval=F-------------------------------------------------------------------
#  ggplot(data=demog.data2, aes(x=HEIGHT, fill=SEX))+geom_density()
#  # Now transparency really means something
#  ggplot(data=demog.data2, aes(x=HEIGHT, fill=SEX))+geom_density(alpha=.3)
#  
#  ggplot(data=demog.data2, aes(x=HEIGHT, fill=SEX))+geom_density(alpha=.3)+facet_wrap(~REGION)
#  ggplot(data=demog.data2, aes(x=HEIGHT, fill=SEX))+geom_density(alpha=.3)+facet_wrap(~REGION, nrow=1) + theme(legend.position="bottom")
#  # Making use of the work we did earlier for superscripts
#  ggplot(data=demog.data2, aes(x=HEIGHT, fill=SEX))+geom_density(alpha=.3)+facet_grid(VSBMIG2~REGION2, label=label_parsed) + theme(legend.position="bottom")

## ----eval=F-------------------------------------------------------------------
#  
#  # explore csv file
#  head(demog.data)
#  
#  # build table for REGION - a variable holding info on Region
#  table(demog.data$REGION)

## ----eval=F-------------------------------------------------------------------
#  # Let's grab a sample
#  useme <- sample(x=1:nrow(demog.data), size=300)
#  demog.data2 <- demog.data[useme,]
#  # Overplotting obscures
#  ggplot(data=demog.data2, aes(x=REGION, y= HEIGHT)) + geom_violin() + geom_point()
#  # Jittering helps to see the spread
#  ggplot(data=demog.data2, aes(x=REGION, HEIGHT, colour=REGION)) + geom_violin() + geom_point(position="jitter")
#  # Violin and boxplot are interchangable
#  ggplot(data=demog.data2, aes(x=REGION, HEIGHT)) + geom_boxplot() + geom_point(position="jitter")

## ----eval=F-------------------------------------------------------------------
#  ggplot(data=demog.data, aes(x=HEIGHT, y=WEIGHT)) + geom_point()
#  # Jittering is also helpful with scatterplots
#  ggplot(data=demog.data, aes(x=HEIGHT, y=WEIGHT)) + geom_point(position="jitter")
#  # Transparency helps
#  ggplot(data=demog.data, aes(x=HEIGHT, y=WEIGHT)) + geom_point(position="jitter", alpha=.2)
#  
#  # Back to the smaller set to see color
#  ggplot(data=demog.data2, aes(x=HEIGHT, y=WEIGHT, color=REGION)) + geom_point()
#  # Shape mapped to SEX
#  ggplot(data=demog.data2, aes(x=HEIGHT, y=WEIGHT, color=REGION, shape=SEX)) + geom_point()
#  # Let's increase the size
#  ggplot(data=demog.data2, aes(x=HEIGHT, y=WEIGHT, color=REGION, shape=SEX)) + geom_point(size=3)
#  
#  # We can map size to a factor
#  ggplot(data=demog.data2, aes(x=HEIGHT, y=WEIGHT, color=REGION, shape=SEX, size=factor(VSBMIGCD))) + geom_point()
#  

## ----eval=F-------------------------------------------------------------------
#  ggplot(data=demog.data2, aes(x=HEIGHT, y=WEIGHT, color=REGION, shape=SEX, size=factor(VSBMIGCD))) + geom_point()+
#    scale_size_manual(breaks=c(1,2,3,9), values=c(2,3, 4,5))
#  
#  #We can map size to a continuous endpoint
#  ggplot(data=demog.data2, aes(x=HEIGHT, y=WEIGHT, color=REGION, shape=SEX, size=VSBMI)) + geom_point()
#  
#  # Facetting works
#  ggplot(data=demog.data2, aes(x=HEIGHT, y=WEIGHT, color=REGION, shape=SEX, size=factor(VSBMIGCD))) + geom_point()+
#    scale_size_manual(breaks=c(1,2,3, 9), values=c(2,3, 4, 5)) +
#    facet_grid(SEX~VSBMIG)+
#    labs(x="Height", y="Weight", color="Region", size="BMI Group", shape="Gender")+
#    theme(legend.position="bottom")+
#    guides(shape="none", size="none")+
#    ggtitle("Height vs. Weight by BMI Group and Gender")
#  

## ----eval=F-------------------------------------------------------------------
#  # We can map color to scatter
#  ggplot(data=demog.data, aes(x=HEIGHT, y=WEIGHT, color=SEX)) + geom_point(position="jitter", alpha=.2)
#  # We can add contours to scatterplot
#  ggplot(data=demog.data, aes(x=HEIGHT, y=WEIGHT, color=SEX)) + geom_point(position="jitter", alpha=.2)+ geom_density2d()
#  # Note that to override contour color we do so within the geom
#  ggplot(data=demog.data, aes(x=HEIGHT, y=WEIGHT, color=SEX,linetype=SEX)) + geom_point(position="jitter", alpha=.2)+ geom_density2d(color="black")
#  # Facetting
#  ggplot(data=demog.data, aes(x=HEIGHT, y=WEIGHT, color=SEX,linetype=SEX)) + geom_point(position="jitter", alpha=.2)+ geom_density2d(color="black")+facet_grid(SEX~REGION)+
#    theme(legend.position="bottom")

## ----eval=F-------------------------------------------------------------------
#  help(mahalanobis)
#  # Assign the mahalanobis distance to the dataframe
#  # MD is chi-sq when data is MV normal distributed.
#  demog.data.sub <- subset(demog.data, select=c("HEIGHT", "WEIGHT"))
#  demog.data$MD.HW <- mahalanobis(demog.data.sub, center=colMeans(demog.data.sub, na.rm=T), cov=cov(demog.data.sub,use="pairwise.complete.obs"))
#  ggplot(data=demog.data, aes(x=MD.HW))+geom_density()
#  #qqplot - suggest more outliers than chisq
#  ggplot(data=demog.data, aes(sample=MD.HW))+ stat_qq(dist=qchisq, dparam=list(df=2))
#  # culprits highlighted in scatterplot
#  ggplot(data=demog.data, aes(x=HEIGHT, y=WEIGHT, color=MD.HW)) + geom_point(position="jitter")+
#    scale_colour_gradient(limits=c(12, 75), low="orange", high="red")
#  # Exaggeration
#  ggplot(data=demog.data, aes(x=HEIGHT, y=WEIGHT, color=MD.HW, size=MD.HW)) + geom_point(position="jitter")+
#    scale_colour_gradient(limits=c( qchisq(df=2,.99), 75), low="orange", high="red")+
#    labs(x="Height", y="Weight", size="Distance", color="Distance", title="Height vs. Weight with Mahalanobis Distance\nYellow-Red scale used for Distances beyond 99th percentile of Chisq(2)")
#  # Note: in practice we should split data by gender, compute MD based on Gender-specific means and covarainces
#  ggplot(data=demog.data, aes(x=HEIGHT, y=WEIGHT, color=MD.HW, size=MD.HW, shape=SEX)) + geom_point(position="jitter")+
#    scale_colour_gradient(limits=c( qchisq(df=2,.99), 75), low="orange", high="red")+
#    labs(x="Height", y="Weight", size="Distance", color="Distance", title="Height vs. Weight with Mahalanobis Distance\nYellow-Red scale used for Distances beyond 99th percentile of Chisq(2)")
#  
#  
#  # Faceting works again
#  ggplot(data=demog.data, aes(x=HEIGHT, y=WEIGHT, color=MD.HW, size=MD.HW, shape=SEX)) + geom_point(position="jitter")+
#    scale_colour_gradient(limits=c( qchisq(df=2,.99), 75), low="orange", high="red")+
#    labs(x="Height", y="Weight", size="Distance", color="Distance", title="Height vs. Weight with Mahalanobis Distance\nYellow-Red scale used for Distances beyond 99th percentile of Chisq(2)")+
#    facet_wrap(~SEX)
#  

## ----eval=F-------------------------------------------------------------------
#  demog.data <- read.csv(paste("g_l_abpm_3month.csv",sep=""))
#  
#  dframe.dia<- subset(dframe, abtest="Diast. BP")
#  head(dframe.dia)
#  ggplot(data=dframe.dia, aes(x=visitnum, y=MEAN))+geom_point()
#  
#  # We need to define new variable
#  dframe$month <- floor(dframe$visitnum)
#  table(dframe$month)
#  dframe$month <- factor(dframe$month)
#  levels(dframe$month) <- c("Baseline", "Month 3")
#  ggplot(data=dframe, aes(x=visitnum, y=MEAN))+geom_point()+facet_wrap(~month,nrow=1, scales="free")
#  
#  # We need to define new variable
#  dframe$hour <- (dframe$visitnum - floor(dframe$visitnum))*100
#  ggplot(data=dframe, aes(x=hour, y=MEAN))+geom_point()+facet_wrap(~month,nrow=1, scales="free")
#  
#  # We need to facet by abtest
#  ggplot(data=dframe, aes(x=hour, y=MEAN))+geom_point()+facet_grid(abtest~month, scales="free")
#  
#  ggplot(data=dframe, aes(x=hour, y=MEAN, color=TRTGRP, shape=TRTGRP, linetype=TRTGRP))+
#    geom_line(size=1)+
#    geom_point(size=3)+
#    facet_grid(abtest~month, scales="free")+
#    theme(legend.position="bottom")
#  
#  # Let's install error bars
#  # Defining upper and lower limits based on standard error
#  dframe$UP <- dframe$MEAN+dframe$STD/sqrt(dframe$N)
#  dframe$DOWN <- dframe$MEAN-dframe$STD/sqrt(dframe$N)
#  
#  # We need to correct for overplotting; recall we saw position=dodge with barcharts
#  # Here we impose a specific value to override default for width
#  pd = position_dodge(0)
#  
#  # Punched up
#  ggplot(data=dframe, aes(x=hour, y=MEAN, ymin=DOWN, ymax=UP, color=TRTGRP, shape=TRTGRP, linetype=TRTGRP))+
#    geom_line(size=1,position=pd)+
#    geom_point(size=3, position=pd, fill="white")+
#    facet_grid(abtest~month, scales="free")+
#    scale_shape_manual(values=c(24,16))+
#    geom_errorbar( position=pd, size=1)+
#    geom_point(size=3, position=pd, fill="white")+
#  
#    theme(legend.position="bottom")
#  
#  pd = position_dodge(0.5)
#  ggplot(data=dframe, aes(x=hour, y=MEAN, ymin=DOWN, ymax=UP, color=TRTGRP, shape=TRTGRP, linetype=TRTGRP))+
#    geom_line(size=1,position=pd)+
#    geom_point(size=3, position=pd, fill="white")+
#    facet_grid(abtest~month, scales="free")+
#    scale_shape_manual(values=c(24,16))+
#    geom_errorbar( position=pd, size=1)+
#    geom_point(size=3, position=pd, fill="white")+
#    theme(legend.position="bottom")

## ----eval=F-------------------------------------------------------------------
#  # Function to compute predictive p-value ----------------------------------
#  
#  # The preliminary bit is an example of how R help files are punched up by Roxygen when building packages
#  # and help files.
#  
#  #' @title Return Predictive P-value based on Negative Binomial
#  #' @description A function to assist in generating reports associated with under reporting
#  #' @details Function returns the probability of encountering data at least as extreme as what was observed, given prior and data from neighboring centres.
#  #' @param dframe A dataframe with rows holding centre level data - required columns include REGION and COUNTRY
#  #' @param remove.me identifies the centre to be removed from conditioning (to include all chose some value that doesn't match any centre)
#  #' @param alpha.gam value of shape parameter for gamma prior - taken to be zero by default
#  #' @param beta.gam value of rate parameter for gamma prior - taken to be zero by default
#  #' @param remove.centre logical indicating weather or not to condition on the centre that under scrutiny
#  #' @param reference function pivots on "country" or "region".  This indicates who neighbors are
#  #' @examples
#  #' \dontrun{
#  #' # No example
#  #' }
#  #' @author Greg Cicconetti
#  return.pred.pval <- function(dframe=centre.level,
#                               remove.me=184,
#                               alpha.gam=0,
#                               beta.gam=0,
#                               freq.col="Freq.AE",
#                               exp.col="Exposure",
#                               remove.centre=TRUE,
#                               reference="country"){
#    # Get observed frequency, exposure from centre under investigation
#    obs.freq <- dframe[remove.me,freq.col]
#    obs.exposure <- dframe[remove.me, exp.col]
#    # id the centre
#    centre <- dframe$CENTREID[remove.me]
#  
#    if (reference == "country"){
#      # Identify the Country the centre belongs to
#      use.me <- subset(dframe, COUNTRY==dframe$COUNTRY[remove.me])
#      if (remove.centre==TRUE) use.me <- subset(use.me, CENTREID != centre)
#      # summing from zero to observed frequency from appropriate negative binomial
#      pval <- sum(dnbinom(x=0:obs.freq,
#                          size=alpha.gam + sum(use.me[,freq.col]),
#                          mu=obs.exposure*(alpha.gam + sum(use.me[,freq.col]))/((beta.gam + sum(use.me[,exp.col])))))
#    } else if( reference == "region"){
#      # Identify the region centre belongs to
#      use.me <- subset(dframe, REGION==dframe$REGION[remove.me])
#      if (remove.centre==TRUE) use.me <- subset(use.me, CENTREID != centre)
#      # summing from zero to observed frequency from appropriate negative binomial
#      pval <- sum(dnbinom(x=0:obs.freq,
#                          size=alpha.gam + sum(use.me[,freq.col]),
#                          mu=obs.exposure*(alpha.gam + sum(use.me[,freq.col]))/((beta.gam + sum(use.me[,exp.col])))))
#    }
#  
#    return(pval)	
#  }

## ----eval=F-------------------------------------------------------------------
#  # fmace <- read.csv("g_KMfmace.csv")
#  # AE <-read.csv("AE_stability_RTU.csv");
#  # SAE <- read.csv("SAE_stability_RTU.csv")
#  
#  # Investigate the files ---------------------------------------------------
#  head(fmace)  # We're interested in censor, centime
#  head(AE) # We're interested in numb_AE and daysonstudy
#  head(SAE) # We're interested in numb_SAE and daysonstudy
#  
#  # Despite being sorted already on SUBJID, there's still a dirty data issue:
#  rbind(dim(fmace), dim(AE),dim(SAE))
#  
#  # Due to differences in datacut dates there are more subjects in one file
#  # Here we retain only those SUBJID rows in fmace that are among the SUBJID values in AE
#  
#  fmace <- fmace[fmace$SUBJID %in% AE$SUBJID,]
#  fmace <- subset(fmace, SUBJID %in% AE$SUBJID)
#  # Check that we now have equivalent subjids across datasets
#  sum(SAE$SUBJID == AE$SUBJID)
#  sum(AE$SUBJID == fmace$SUBJID)
#  
#  # We'll add columns to SAE data.frame
#  # Bring in numb_AE count into SAE
#  SAE$numb_AE <- AE$numb_AE
#  # Bring in the censor variable from fmace (1 if first mace was observed; 0 otherwise)
#  SAE$fmace <- fmace$censor
#  
#  # Now for first MACE with need to use censored time to first mace, not days in study
#  SAE$centime <- fmace$centime
#  
#  head(SAE)  # we're interested in numb_AE, numb_SAE and daysonstudy.  Additionally, we're interested in fmace and centime
#  

## ----eval=F-------------------------------------------------------------------
#  # Process data from SUBJ level to CENTRE level ----------------------------
#  
#  # Example of SPLIT-APPLY-COMBINE Strategy
#  # We want to move from subject level data to centre level summaries
#  # we wish to summarize for each Centre 1) total observation time relative to AE and SAE (Exposure), 2) AE Frequency,
#  # 3) SAE Frequency, 4) First MACE frequency, and 5) total censored time associated with 1st MACE
#  
#  # The alternative and computationally less efficient way to accomplish would use for-loops, to subset (i.e., split),
#  # compute summaries (apply), and a final step to bring the pieces back together. This also requires more lines of code!
#  
#  require(plyr)
#  centre.level.a <- ddply(SAE, .(CENTREID), summarize,
#                        Exposure=sum(daysonstudy,na.rm=T),
#                        Freq.AE=sum(numb_AE,na.rm=T),
#                        Freq.SAE=sum(numb_SAE,na.rm=T),
#                        Freq.fmace = sum(fmace,na.rm=T),
#                        Exposure.fmace=sum(centime,na.rm=T))
#  
#  
#  centre.level <- ddply(SAE, .(CENTREID, COUNTRY, REGION), summarize,
#                        Exposure=sum(daysonstudy,na.rm=T),
#                        Freq.AE=sum(numb_AE,na.rm=T),
#                        Freq.SAE=sum(numb_SAE,na.rm=T),
#                        Freq.fmace = sum(fmace,na.rm=T),
#                        Exposure.fmace=sum(centime,na.rm=T))
#  
#  centre.level.b <- ddply(SAE, .(CENTREID, SEX), summarize,
#                        Exposure=sum(daysonstudy,na.rm=T),
#                        Freq.AE=sum(numb_AE,na.rm=T),
#                        Freq.SAE=sum(numb_SAE,na.rm=T),
#                        Freq.fmace = sum(fmace,na.rm=T),
#                        Exposure.fmace=sum(centime,na.rm=T))
#  
#  head(centre.level)
#  dim(centre.level)
#  require(ggplot2)
#  ggplot(data=centre.level, aes(x=Exposure, y=Freq.AE))+geom_point()
#  ggplot(data=centre.level, aes(x=Exposure, y=Freq.AE, colour=REGION))+geom_point()
#  ggplot(data=centre.level, aes(x=Exposure, y=Freq.AE, colour=REGION))+geom_point()+facet_wrap(~REGION, nrow=2)
#  ggplot(data=centre.level, aes(x=Exposure, y=Freq.SAE, colour=REGION))+geom_point()+facet_wrap(~REGION, nrow=2)
#  ggplot(data=centre.level, aes(x=Exposure.fmace, y=Freq.fmace, colour=REGION))+geom_point()+facet_wrap(~REGION, nrow=2)
#  ggplot(data=centre.level, aes(x=Exposure.fmace, y=Freq.fmace, colour=REGION))+geom_point(position="jitter")+facet_wrap(~REGION, nrow=2)

## ----eval=F-------------------------------------------------------------------
#  # Compute and attach the predictive p-values ------------------------------
#  # -------------------------------------------------Get the predicitive p-values associated with AE
#  get.it <- c() # get.it is temporary storage
#  for(i in 1:nrow(centre.level)){
#    get.it <- c(get.it,
#                return.pred.pval(centre.level, remove.me=i, alpha.gam=0, beta.gam=0, freq.col="Freq.AE",exp.col="Exposure" ))
#  }
#  centre.level$AE.pval <- get.it
#  
#  # -------------------------------------------------Repeat with SAE
#  get.it <- c()
#  for(i in 1:nrow(centre.level)){
#    get.it <- c(get.it,
#                return.pred.pval(centre.level,remove.me=i,alpha.gam=0, beta.gam=0, freq.col="Freq.SAE",exp.col="Exposure" ))
#  }
#  centre.level$SAE.pval <- get.it
#  
#  # -------------------------------------------------Repeat with first MACE
#  get.it <- c()
#  for(i in 1:nrow(centre.level)){
#    get.it <- c(get.it,
#                return.pred.pval(centre.level,remove.me=i,alpha.gam=0, beta.gam=0, freq.col="Freq.fmace",exp.col="Exposure.fmace" ))
#  }
#  centre.level$fmace.pval <- get.it
#  
#  head(centre.level)
#  

## ----eval=F-------------------------------------------------------------------
#  ggplot(data=centre.level, aes(x=AE.pval))+geom_density()
#  ggplot(data=centre.level, aes(x=SAE.pval))+geom_density()
#  ggplot(data=centre.level, aes(x=fmace.pval))+geom_density()
#  
#  # Reshaping the data to do more with ggplot -------------------------------
#  # we need to move from wide for to narrow form
#  # I want values to hold p-values, variables to hold AE.pval, SAE.pval, fmace.pval
#  require(reshape2)
#  head(centre.level)
#  centre.level.melt <- melt(centre.level, id=c(1:4, 8), measure=9:11)
#  head(centre.level.melt)
#  
#  ggplot(data=centre.level.melt, aes(x=value, fill=variable)) + geom_density(alpha=.2)
#  ggplot(data=centre.level.melt, aes(x=value, fill=variable)) + geom_density(alpha=.2) + facet_wrap(~REGION)+
#    labs(x="Value", y="Density", title="Distribution of Predictive P-value by Endpoint", fill="Endpoint")+
#    theme(legend.position="bottom")
#  

## ----eval=F-------------------------------------------------------------------
#  # Now I want values to hold counts and variable to hold Frequencies for AEs, SAEs, and first mace
#  centre.level.melt2 <- melt(centre.level, id=c(1:4,8), measure=5:7)
#  head(centre.level.melt2)
#  levels(centre.level.melt$variable) <- c("AE", "SAE", "First MACE")
#  
#  # Now let's join some columns
#  centre.level.melt$Frequency <- centre.level.melt2$variable
#  centre.level.melt$Freqval <- centre.level.melt2$value
#  
#  # Recall that we have 'total observation time' and 'censored time to first mace'
#  # Since we're in long form, I want a corrected Exposure column that holds the total observation time for rows associatedwith AE/SAE
#  # and censored time for first MACE
#  
#  # This initializes a column of zeros
#  centre.level.melt$corExposure <- rep(0, nrow(centre.level.melt))
#  
#  # Pulling from centre.level.melt's Exposure variable to populate corExposure
#  centre.level.melt$corExposure[centre.level.melt$variable %in% c("AE","SAE")] <- centre.level.melt$Exposure[centre.level.melt$variable %in% c("AE","SAE")]
#  centre.level.melt$corExposure[(centre.level.melt$variable %in% c("First MACE"))] <- centre.level.melt$Exposure.fmace[(centre.level.melt$variable %in% c("First MACE"))]
#  rbind(head(centre.level.melt), tail(centre.level.melt))
#  
#  
#  # Another application of SPLIT-APPLY-COMBINE
#  # For AE, SAE and fmace, I want to rescale the predictive p-values in such a way that exaggerates those centres with small p-values while
#  # attemping to bring them all to a common frame of reference.  So the max here will be relative to AE, SAE, fmace, resp.
#  centre.level.melt <- ddply(.data=centre.level.melt,
#                             .(variable), transform, nvalue = log(1/value)/max(log(1/value)) )
#  
#  
#  

## ----eval=F-------------------------------------------------------------------
#  # Although narrow form is required for plotting, we still need a spreadsheet for customer
#  attach(centre.level)
#  centre.level$nvalue.AE <- log(1/AE.pval)/max(log(1/AE.pval))
#  centre.level$nvalue.SAE <- log(1/SAE.pval)/max(log(1/SAE.pval))
#  centre.level$nvalue.fmace <- log(1/fmace.pval)/max(log(1/fmace.pval))
#  detach(centre.level)
#  #write.csv(centre.level, file="Centre Level Report.csv")

## ----eval=F-------------------------------------------------------------------
#  pdf("Stability Centre Report - centre a.pdf", h=8.5, w=11)
#  ggplot(data=centre.level.melt, aes(x=value,fill=variable))+geom_density(alpha=.6)+
#    labs(x="Posterior Predictive P-value",
#         fill="Endpoint",
#         title="Densities of Posterior Predictive P-value")
#  
#  ggplot(data=centre.level.melt, aes(x=value,fill=variable))+geom_density(alpha=.6)+
#    facet_wrap(REGION~variable, scales="free_y",ncol=3)+
#    labs(x="Posterior Predictive P-value Are Not Uniform",
#         fill="Endpoint",
#         title="Densities of Posterior Predictive P-value")+
#    geom_vline(x=c(.05, .1),colour="red")
#  
#  
#  ggplot(data=centre.level.melt, aes(x=nvalue,fill=variable))+geom_density(alpha=.6)+
#    #facet_wrap(REGION~variable, scales="free_y",ncol=3)+
#    labs(x="Density of Under Reporting Metric:log(1/PPP)/max(log(1/PPP)\nWhere PPP is the Posterior Predictive P-value",
#         fill="Endpoint",
#         title="Densities of Under-reporting Metric: log(1/PPP)/max(log(1/PPP)\nInitial Threshold suggested at 0.3. Change threshold by Endpoint/Region to Calibrate")+
#    geom_vline(x=c(.3),colour="red")
#  
#  
#  ggplot(data=centre.level.melt, aes(x=nvalue,fill=variable))+geom_density(alpha=.6)+
#    facet_wrap(REGION~variable, scales="free_y",ncol=3)+
#    labs(x="Density of Under Reporting Metric:log(1/PPP)/max(log(1/PPP)\nWhere PPP is the Posterior Predictive P-value",
#         fill="Endpoint",
#         title="Densities of Under-reporting Metric: log(1/PPP)/max(log(1/PPP)\nInitial Threshold suggested at 0.3. Change threshold by Endpoint/Region to Calibrate")+
#    geom_vline(x=c(.3),colour="red")
#  
#  pred.pval.report <- data.frame( "less than 0.1"= c(
#    mean(centre.level$SAE.pval < .1),
#    mean(centre.level$AE.pval < .1),
#    mean(centre.level$fmace.pval < .1)), "less than .01"= c(
#      mean(centre.level$SAE.pval < .01),
#      mean(centre.level$AE.pval < .01),
#      mean(centre.level$fmace.pval < .01)))
#  
#  pred.pval.report
#  
#  ggplot(data=centre.level.melt, aes(x=nvalue,fill=variable))+geom_density(alpha=.6)+
#    labs(x="Density of Under Reporting Metric:log(1/PPP)/max(log(1/PPP)\n\nWhere PPP: Posterior Predictive P-value",
#         fill="Endpoint",
#         title="Densities of Under-reporting Metric:\n log(1/PPP)/max(log(1/PPP)")
#  
#  ggplot(data=centre.level.melt, aes(x=nvalue,fill=variable))+
#    geom_density(alpha=.6)+
#    facet_wrap(REGION~variable, scales="free_y",ncol=3)+
#    labs(x="Density of Under Reporting Metric:log(1/PPP)/max(log(1/PPP)\n\nWhere PPP: Posterior Predictive P-value",
#         fill="Endpoint",
#         title="Densities of Under-reporting Metric:\n log(1/PPP)/max(log(1/PPP)")+
#    geom_vline(x=.3, colour="red")
#  
#  
#  ggplot(data=centre.level.melt, aes(size=nvalue,
#                                     x=corExposure/365,
#                                     y=Freqval/(corExposure/365),
#                                     color=nvalue)) + geom_point()+
#    scale_color_gradient(low="grey30", high="red")+
#    facet_wrap(variable~ REGION, scales="free_y",nrow=3)+
#    labs(x="Total Corrected Patient Years",
#         y="Incidence per Patient Year",
#         size="Under Reporting Metric",
#         colour="Under Reporting Metric",
#         title=("Total Corrected Patient Years vs. Incidence Per Patient Year\nColor and Size Associated with Under Reporting Metric"))
#  
#  
#  
#  dev.off()

