# set working directory to figuRes
setwd("C:/Users/Greg/Dropbox/R/figuRes")
destination.dir <- "C:/Users/Greg/Dropbox/R/figuRes/data"
summary.lineplot.data <- read.csv(paste0(getwd(),"/predata", "/g_LP_bilt_stats.csv"))
head(summary.lineplot.data)
levels(summary.lineplot.data$TRTGRP) <- c("Placebo", "Treatment")
save(summary.lineplot.data, file=paste0(destination.dir,"/summary.lineplot.data.rda"))


raw.lineplot.data <- read.csv(paste0(getwd(),"/predata", "/g_lineplot_lppla2.csv"))
head(raw.lineplot.data)
levels(raw.lineplot.data$TRTGRP) <- c("Placebo", "Treatment")
levels(raw.lineplot.data$LBTEST) <- "Lab Test XYZ"
raw.lineplot.data <- raw.lineplot.data[,c(1, 2, 4, 5, 6, 7, 8, 10, 11, 12)]
save(raw.lineplot.data, file=paste0(destination.dir,"/raw.lineplot.data.rda"))


km.data <- read.csv(paste0(getwd(),"/predata", "/g_kmfmace.csv"))
head(km.data)
levels(km.data$TRTGRP) <- c("Placebo", "Treatment")
km.data <- km.data[,c(1, 2, 3, 4, 7, 8, 10, 22, 23)]
save(km.data, file=paste0(destination.dir,"/km.data.rda"))

cdf.data <- read.csv(paste0(getwd(),"/predata", "/g_CDFipexp.csv"))
levels(cdf.data$TRTGRP) <- c("Placebo", "Treatment")
head(cdf.data)
cdf.data <- cdf.data[,c(1,2,3,6, 7, 9, 12, 13)]
save(cdf.data, file=paste0(destination.dir,"/cdf.data.rda"))

demog.data <- read.csv(paste0(getwd(),"/predata", "/g_bslchar.csv"))
head(demog.data)
levels(demog.data$TRTGRP) <- c("Placebo", "Treatment")
demog.data <- demog.data[sample(1:nrow(demog.data), size=5000), ]
demog.data <- demog.data[,c(1,2,3,4,8, 15, 22, 23,57,56, 58, 65, 67, 75, 79)]
save(demog.data, file=paste0(destination.dir,"/demog.data.rda"))

forest.data <- read.csv(paste0(getwd(),"/predata", "/g_subgrp_intg_page_all.csv"))
head(forest.data)
save(forest.data, file=paste0(destination.dir,"/forest.data.rda"))


benrisk2.data <- read.csv(paste0(getwd(),"/predata", "/BR Data Example 2.csv"))
save(benrisk2.data, file=paste0(destination.dir,"/benrisk2.data.rda"))

lineplot.data <- read.csv(paste0(getwd(),"/predata", "/lineplot_data.csv"))
save(lineplot.data, file=paste0(destination.dir,"/lineplot.data.rda"))

#benrisk2.data
#save(forest.data, file="benrisk2.data.rda")


outputplan <- read.csv(paste0(getwd(),"/predata", "/dummyOutputplan.csv"))
save(outputplan, file=paste0(destination.dir,"/outputplan.rda"))


driver.names <- c("continuous_by_visit_and_treatment.r",
"category_by_visit.r",
"scatter_smooth.r",
"scatter_smooth_facet.r",
"scatterplot_with_smoother.R",
"jitter_weight_faceted.r",
"jitter_weight.r",
"cdf_weight.R",
"priordens.r",
"lineplot_example.r")


# change to support
setwd("C:/Users/Greg/Dropbox/R/figuRes/support")
drivers <- list()
for(i in 1:length(driver.names)){
drivers[[i]] <- readLines(driver.names[i])
}

driver1 <- drivers[[1]]
driver2 <- drivers[[2]]
driver3 <- drivers[[3]]
driver4 <- drivers[[4]]
driver5 <- drivers[[5]]
driver6 <- drivers[[6]]
driver7 <- drivers[[7]]
driver8 <- drivers[[8]]
driver9 <- drivers[[9]]
driver10 <- drivers[[10]]


save(driver1, file=paste0(destination.dir,"/",unlist(strsplit(driver.names[1],"\\."))[1],".rda"))
save(driver2, file=paste0(destination.dir,"/",unlist(strsplit(driver.names[2],"\\."))[1],".rda"))
save(driver3, file=paste0(destination.dir,"/",unlist(strsplit(driver.names[3],"\\."))[1],".rda"))
save(driver4, file=paste0(destination.dir,"/",unlist(strsplit(driver.names[4],"\\."))[1],".rda"))
save(driver5, file=paste0(destination.dir,"/",unlist(strsplit(driver.names[5],"\\."))[1],".rda"))
save(driver6, file=paste0(destination.dir,"/",unlist(strsplit(driver.names[6],"\\."))[1],".rda"))
save(driver7, file=paste0(destination.dir,"/",unlist(strsplit(driver.names[7],"\\."))[1],".rda"))
save(driver8, file=paste0(destination.dir,"/",unlist(strsplit(driver.names[8],"\\."))[1],".rda"))
save(driver9, file=paste0(destination.dir,"/",unlist(strsplit(driver.names[9],"\\."))[1],".rda"))
save(driver10, file=paste0(destination.dir,"/",unlist(strsplit(driver.names[10],"\\."))[1],".rda"))


boxplot.driver <- readLines("C:/Users/Greg/Dropbox/R/figuRes/support/boxplot.driver.txt")
save(boxplot.driver, file=paste0(destination.dir,"/boxplot.driver.rda"))

