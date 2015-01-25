
# Functions for working with outputplan --------------------------------------
#' @title Refresh the Output Plan
#' @description Reloads outputplan_study.csv file and applies canonical formatting changes.
#' @details Ensure all columns are read in as character vectors. Ensure all missing entries are replaced with blank character string. Ensure all escape characters for carrige returns are respected. Grabs the 'modified time' from file attributes associated with .csv files named in the outputplan.
#' @param loadplan logical; if true then it loads from the filename, else it presumes outputplan is an object in Global Environment
#' @param filename Should point to the outputplan_study.csv typically located in the /code directory.
#' @author Greg Cicconetti
refresh.outputplan <- 
  function (
    loadplan=TRUE,
    filename = "C:/Users/gqc70045/Documents/LOCALR/Darapladib Graphics/Stability Graphics/refdata/outputplan_stability 22NOV13.csv") 
  {
    
    if(loadplan==TRUE) temp <- read.csv(filename) else temp <-  outputplan
    
    # step through column of outputplan
    # convert columns to text class, use blank space for missing values when reading numeric columns
    # search for \n in FigureTitle column
    for (i in 1:ncol(temp)) {
      temp[, i] <- as.character(temp[, i], na = "")
      temp[, i][is.na(temp[, i]) == T] <- ""
    }
    
    # Replace 
    temp$FigureTitle <- gsub(pattern = "\\n", x = temp$FigureTitle, 
                             replacement = "\n", fixed = T)
    temp$FigureTitle <- gsub(pattern = "COMMA", x = temp$FigureTitle, 
                             replacement = ",", fixed = T)
    temp$TableID <- gsub(pattern = "COMMA", x = temp$TableID, 
                             replacement = ",", fixed = T)
    temp$fnote1 <- gsub(pattern = "COMMA", x = temp$fnote1, 
                         replacement = ",", fixed = T)
    temp$fnote2 <- gsub(pattern = "COMMA", x = temp$fnote2, 
                         replacement = ",", fixed = T)
    temp$fnote3 <- gsub(pattern = "COMMA", x = temp$fnote3, 
                         replacement = ",", fixed = T)
    temp$fnote4 <- gsub(pattern = "COMMA", x = temp$fnote4, 
                         replacement = ",", fixed = T)
    # Prepare to split FigureTitle Column into constituent title lines
    temp$FigureTitle1 <- temp$FigureTitle2 <- temp$FigureTitle3 <- temp$FigureTitle4 <- ""
    temp$nTitleLines <- 1
    
    
    # Run though rows, identify number of lines needed and populate constituent title lines
    for(i in 1:nrow(temp)){
      if(length(str_split(temp$FigureTitle[i], pattern="\n")[[1]]) == 1){
        temp$FigureTitle1[i] <- temp$FigureTitle[i]
        temp$nFootLines[i] <- sum(c(temp$fnote1[i]!="",temp$fnote2[i]!="",temp$fnote3[i]!="",temp$fnote4[i]!=""))
      }
      if(  length(str_split(temp$FigureTitle[i], pattern="\n")[[1]]) == 2){
        temp$FigureTitle1[i] <- str_split(temp$FigureTitle[i],pattern="\n")[[1]][1]
        temp$FigureTitle2[i] <- str_split(temp$FigureTitle[i],pattern="\n")[[1]][2]
        temp$nTitleLines[i] <- 2
        temp$nFootLines[i] <- sum(c(temp$fnote1[i]!="",temp$fnote2[i]!="",temp$fnote3[i]!="",temp$fnote4[i]!=""))
        
      }
      if(  length(str_split(temp$FigureTitle[i], pattern="\n")[[1]]) == 3){
        temp$FigureTitle1[i] <- str_split(temp$FigureTitle[i],pattern="\n")[[1]][1]
        temp$FigureTitle2[i] <- str_split(temp$FigureTitle[i],pattern="\n")[[1]][2]
        temp$FigureTitle3[i] <- str_split(temp$FigureTitle[i],pattern="\n")[[1]][3]
        temp$nTitleLines[i] <- 3
        temp$nFootLines[i] <- sum(c(temp$fnote1[i]!="",temp$fnote2[i]!="",temp$fnote3[i]!="",temp$fnote4[i]!=""))
        
      }
      if(  length(str_split(temp$FigureTitle[i], pattern="\n")[[1]]) == 4){
        temp$FigureTitle1[i] <- str_split(temp$FigureTitle[i],pattern="\n")[[1]][1]
        temp$FigureTitle2[i] <- str_split(temp$FigureTitle[i],pattern="\n")[[1]][2]
        temp$FigureTitle3[i] <- str_split(temp$FigureTitle[i],pattern="\n")[[1]][3]
        temp$FigureTitle4[i] <- str_split(temp$FigureTitle[i],pattern="\n")[[1]][4]
        temp$nTitleLines[i] <- 4
        temp$nFootLines[i] <- sum(c(temp$fnote1[i]!="",temp$fnote2[i]!="",temp$fnote3[i]!="",temp$fnote4[i]!=""))
        
      }
    }
        
    temp$datadate <- file.info(paste(dd, temp$csv, sep = ""))$mtime
    temp$nFootLines <- as.numeric( temp$nFootLines)
    temp$nTitleLines <- as.numeric(temp$nTitleLines)
    outputplan <<- temp
    if (any(duplicated(outputplan$output))) 
      cat(paste("Note: outputplan has duplicated values in the output column.\nThe outputplan should be edited or subseted to ensure no duplicates.\nCulprits are:", 
                outputplan$output[duplicated(outputplan$output)]), 
          "\n")
    if (any(duplicated(outputplan$rcode))) 
      cat(paste("Note: outputplan has duplicated values in the rcode column.\nThe outputplan should be edited or subseted to ensure no duplicates.\nCulprits are:", 
                outputplan$rcode[duplicated(outputplan$rcode)]), 
          "\n")
  }


# Functions for Output control --------------------------------------------------------
#' @title run.specific
#' @description This function sources a .r driver file and sends its product to a newly opened 8.5in x 11in screen or a pdf file with 8.5in x 11in dimensions.
#' @param source.code This is intended to be a darapladib graphics driver file returning a graphic possibly with complete headers and footers.
#' @param outfile  If (toPDF==T & outfile == "") a .pdf file with root name taken from outputplan$outfile[which(outputplan$rcode==source.code)]. Otherwise a .pdf will be created the value of outfile. The pdf is stored in mypath/od defined in setpaths.r.
#' @param toPDF Logical. If TRUE a .pdf file will be created. If FALSE graphic is sent to screen.
#' @param toWMF Logical. If TRUE a .wmf file will be created.
#' @param toJPEG Logical. If TRUE a .jpeg file will be created.
#' @param toPNG Logical. If TRUE a .png file will be created.
#' @param toBMP Logical. If TRUE a .bmp file will be created.
#' @param toEPS Logical. If TRUE a .eps file will be created.
#' @examples
#' \dontrun{
#' }
#' @author David wade
run.specific<-
  function (source.code = "g_AErr2.r", 
            outfile = "", toPDF=F, toWMF=F, toJPEG=F, toPNG=F, toBMP=F, toEPS=F, dpires=600)
    { 
    if (!exists("outputplan"))
      stop("outputplan does not exist in memory.")
    if (!(source.code %in% outputplan$rcode))
      stop("source.code not found within outputplan")
    filename <- source.code
    if(exists("od2")==FALSE) od2 <- od
    i <- which(outputplan$rcode == filename)
    if (toPDF == T) {
      if (outfile == "")
        pdf(paste(od, outputplan[i, ]$outputfile, sep = ""),            
            height = 8.5, width = 11)    
      if (outfile != "")        
        pdf(paste(od2, outfile, sep = ""), height = 8.5, width = 11)      
      tryCatch({source(paste(cd, source.code, sep = ""))},error=function(e) {        
        myError <<- e$message        
        # this first message goes into the R history file        
        cat("\n\nfiguRes Error: A fatal error ocurred causing the function to stop running\n")        
        cat("********************************************************************************\n")        
        cat(paste("  The error message was: ",myError))        
        closeAllConnections()        
        # this second message goes to the console to alert the user        
        cat("figuRes Error: A fatal error ocurred causing the function to stop running")        
        cat(paste("\n  The error message was: ",myError))        
      })      
      dev.off()      
    }
    if (toWMF == T) {      
      if (outfile == "")        
        win.metafile(paste(od2,unlist(strsplit(outputplan[i, ]$outputfile,"\\."))[1],".wmf",sep = ""),                     
                     height = 8.5, width = 11)      
      if (outfile != "")        
        win.metafile(paste(od2,unlist(strsplit(outfile,"\\."))[1],".wmf", sep = ""), height = 8.5, width = 11)      
      tryCatch({source(paste(cd, source.code, sep = ""))},error=function(e) {        
        myError <<- e$message        
        # this first message goes into the R history file        
        cat("\n\nfiguRes Error: A fatal error ocurred causing the function to stop running\n")        
        cat("********************************************************************************\n")        
        cat(paste("  The error message was: ",myError))        
        closeAllConnections()        
        # this second message goes to the console to alert the user        
        cat("figuRes Error: A fatal error ocurred causing the function to stop running")        
        cat(paste("\n  The error message was: ",myError))        
      })      
      dev.off()      
    } 
    if (toJPEG == T) {      
      if (outfile == "")        
        jpeg(paste(od2,unlist(strsplit(outputplan[i, ]$outputfile,"\\."))[1],".jpeg",sep = ""),             
             height = 8.5, width = 11, units="in", res=dpires)      
      if (outfile != "")        
        jpg(paste(od2,unlist(strsplit(outfile,"\\."))[1],".jpeg", sep = ""), 
            height = 8.5, width = 11, units="in", res=dpires)      
      tryCatch({source(paste(cd, source.code, sep = ""))},error=function(e) {        
        myError <<- e$message        
        # this first message goes into the R history file        
        cat("\n\nfiguRes Error: A fatal error ocurred causing the function to stop running\n")        
        cat("********************************************************************************\n")        
        cat(paste("  The error message was: ",myError))        
        closeAllConnections()        
        # this second message goes to the console to alert the user        
        cat("figuRes Error: A fatal error ocurred causing the function to stop running")        
        cat(paste("\n  The error message was: ",myError))       
      })      
      dev.off()      
    }    
    if (toPNG == T) {      
      if (outfile == "")        
        png(paste(od2,unlist(strsplit(outputplan[i, ]$outputfile,"\\."))[1],".png",sep = ""),            
            height = 8.5, width = 11, units="in", res=dpires)      
      if (outfile != "")        
        png(paste(od2,unlist(strsplit(outfile,"\\."))[1],".png", sep = ""), 
            height = 8.5, width = 11, units="in", res=dpires)      
      tryCatch({source(paste(cd, source.code, sep = ""))},error=function(e) {        
        myError <<- e$message        
        # this first message goes into the R history file        
        cat("\n\nfiguRes Error: A fatal error ocurred causing the function to stop running\n")        
        cat("********************************************************************************\n")        
        cat(paste("  The error message was: ",myError))        
        closeAllConnections()        
        # this second message goes to the console to alert the user        
        cat("figuRes Error: A fatal error ocurred causing the function to stop running")        
        cat(paste("\n  The error message was: ",myError))        
      })      
      dev.off()      
    }    
    if (toBMP == T) {      
      if (outfile == "")        
        bmp(paste(od2,unlist(strsplit(outputplan[i, ]$outputfile,"\\."))[1],".bmp",sep = ""),            
            height = 8.5, width = 11, units="in", res=dpires)      
      if (outfile != "")        
        bmp(paste(od2,unlist(strsplit(outfile,"\\."))[1],".bmp", sep = ""), height = 8.5, width = 11, units="in", res=dpires)      
      tryCatch({source(paste(cd, source.code, sep = ""))},error=function(e) {        
        myError <<- e$message        
        # this first message goes into the R history file        
        cat("\n\nfiguRes Error: A fatal error ocurred causing the function to stop running\n")        
        cat("********************************************************************************\n")        
        cat(paste("  The error message was: ",myError))        
        closeAllConnections()        
        # this second message goes to the console to alert the user        
        cat("figuRes Error: A fatal error ocurred causing the function to stop running")        
        cat(paste("\n  The error message was: ",myError))        
      })      
      dev.off()      
    }   
    if (toEPS == T) {      
      if (outfile == "")        
        postscript(paste(od2,unlist(strsplit(outputplan[i, ]$outputfile,"\\."))[1],".eps",sep = ""),                   
                   height = 8.5, width = 12, pagecentre=T, horizontal=T, paper="letter")      
      if (outfile != "")        
        postscript(paste(od2,unlist(strsplit(outfile,"\\."))[1],".eps", sep = ""), 
                   height = 8.5, width = 12, pagecentre=T, horizontal=T, paper="letter")      
      tryCatch({source(paste(cd, source.code, sep = ""))},error=function(e) {        
        myError <<- e$message        
        # this first message goes into the R history file        
        cat("\n\nfiguRes Error: A fatal error ocurred causing the function to stop running\n")        
        cat("********************************************************************************\n")        
        cat(paste("  The error message was: ",myError))        
        closeAllConnections()        
        # this second message goes to the console to alert the user        
        cat("figuRes Error: A fatal error ocurred causing the function to stop running")        
        cat(paste("\n  The error message was: ",myError))        
      })      
      dev.off()      
    }    
    if (toPDF == F & toWMF == F & toJPEG == F & toPNG == F & toBMP == F & toEPS == F) {      
      if (names(dev.cur()) == "windows")        
        tryCatch({source(paste(cd, source.code, sep = ""))},error=function(e) {          
          myError <<- e$message          
          # this first message goes into the R history file          
          cat("\n\nfiguRes Error: A fatal error ocurred causing the function to stop running\n")          
          cat("********************************************************************************\n")          
          cat(paste("  The error message was: ",myError))          
          closeAllConnections()          
          # this second message goes to the console to alert the user          
          cat("figuRes Error: A fatal error ocurred causing the function to stop running")
          cat(paste("\n  The error message was: ",myError))
        })
      else {
        graphics.off()
        windows(height = 8.5, width = 11)
        tryCatch({source(paste(cd, source.code, sep = ""))},error=function(e) {
          myError <<- e$message
          # this first message goes into the R history file
          cat("\n\nfiguRes Error: A fatal error ocurred causing the function to stop running\n")
          cat("********************************************************************************\n")
          cat(paste("  The error message was: ",myError))
          closeAllConnections()
          # this second message goes to the console to alert the user
          cat("figuRes Error: A fatal error ocurred causing the function to stop running")
          cat(paste("\n  The error message was: ",myError))
        }) 
      } 
    }
  }

#' @title all.in.one
#' @description Produces pdf files with batches of graphics based on flags in outputplan. A progress bar is displayed.
#' @details Prerequisites: You need to have output, code, data directory paths defined in your workspace. These should take variable names od, cd, dd, respectively. This can be done by running a personalized set of the following commands:
#' 
#' Code directory needs to hold the .r files associated with the subset of figures to be produced.
#' 
#' Suggest running outputplan.report() first. A progress bar also helps to see run is incomplete. A manual check on the total number of pages in the final pdf should be made.
#' @section Value: 
#' \describe{
#' A .pdf file called filename.pdf is deposited in the output directory.
#' }
#' @param UseSubset Corresponds to a column name in outputplan holding flags
#' @param filename	Should take form: "myPDFfile.pdf"
#' @param reportNR If TRUE, a plot with missing figure numbers and titles is produced
#' @examples
#' \dontrun{
#' }
#' @author Greg Cicconetti
all.in.one <- function (UseSubset = "SAC", filename = "SAC.pdf", reportNR=TRUE) 
{
  if (!exists("outputplan")) 
    stop("outputplan does not exist in memory.")
  if (!(UseSubset %in% names(outputplan))) 
    stop("Subset not defined in outputplan.")
  total <- length(which(outputplan[, UseSubset] == "Y"))
  counter <- 0
  Start.time <- Sys.time()
  pb <- winProgressBar(title = "Example progress bar", label = "0% done", 
                       width = 500, min = 0, max = total, initial = 0)
  pdf(paste(od, filename, sep = ""), height = 8.5, width = 11)
  
  if(reportNR ==TRUE){
    nr <- 
      r<-  ggplot(data=outputplan[outputplan[,UseSubset]=="N",], aes(y=FigureNumber, x=0, label=paste(FigureStatus))) + 
      geom_text(size=4, hjust=0)+
      geom_text(data= outputplan[outputplan[,UseSubset]=="N",], aes(hjust=0, y=FigureNumber, x = .5, label=FigureTitle, size=4))+
      xlim(0, 5)+
      theme_classic()+
      guides(size=F) + 
      theme(axis.line.x=element_blank(),axis.text.x= element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank())
    print(r)
  }
  
  for (j in which(outputplan[, UseSubset] == "Y")) {
    counter <- counter + 1
    info <- paste("Sourcing: ", outputplan[j, ]$rcode, ", file ", 
                  counter, " of ", total, " (", round((counter/total) * 
                                                        100), "% done), Elapsed Time (mins): ", round(difftime(Sys.time(), 
                                                                                                               Start.time, units = "mins"), 2), sep = "")
    setWinProgressBar(pb, counter, title = paste(filename, 
                                                 "Progress Bar"), label = info)
    source(paste(cd, outputplan$rcode[j], sep = ""))
  }
  dev.off()
  close(pb)
  elasped <- round(difftime(Sys.time(), Start.time, units = "mins"), 
                   2)
  elasped
}

#' @title Insert a New Line
#' @description This function takes a string and returns a list containing 4 components.  
#' @details Fill in details
#' @param text.string a text string holding the title to a display
#' @param nlhere a vector of integers identifying after which ordered word in the title a new line should be placed
#' @examples 
#' \dontrun{
#' insertNL(outputplan$FigureTitle[90])
#' }
#' @author Greg Cicconetti
insertNL <- function(text.string = "Kaplan-Meier Plot of Time to First Occurence of the Total Coronary Events Excluding Target Lesion Revascularizations in Subjects Treated with Percutaneous Coronary Intervention Prior to Randomization During the Time Period for Follow-up of Cardiovascular Efficacy Endpoints",
                     nlhere=seq(7,70,7)){
  text.string <- gsub("\\n", " ", x=text.string)
  # 2 lines:  
  NL.2 <- 
    paste(
      paste(unlist(strsplit(text.string," "))[1:floor(length(unlist(strsplit(text.string," ")))/2)], collapse=" ")
      ,"\n",
      paste(unlist(strsplit(text.string," "))[(floor(length(unlist(strsplit(text.string," ")))/2)+1):length(unlist(strsplit(text.string," ")))], collapse=" ")
      ,sep="")
  
  # 3 lines:
  NL.3 <- 
    paste(
      paste(unlist(strsplit(text.string," "))[1:floor(length(unlist(strsplit(text.string," ")))/3)], collapse=" ")
      ,"\n",
      paste(unlist(strsplit(text.string," "))[(floor(length(unlist(strsplit(text.string," ")))/3)+1):(2*floor(length(unlist(strsplit(text.string," ")))/3))], collapse=" ")
      ,"\n",
      paste(unlist(strsplit(text.string," "))[(2*floor(length(unlist(strsplit(text.string," ")))/3)+1):length(unlist(strsplit(text.string," ")))], collapse=" ")
      ,sep="")
  
  # 4 lines
  NL.4 <- 
    paste(
      paste(unlist(strsplit(text.string," "))[1:floor(length(unlist(strsplit(text.string," ")))/4)], collapse=" ")
      ,"\n",
      paste(unlist(strsplit(text.string," "))[(floor(length(unlist(strsplit(text.string," ")))/4)+1):(2*floor(length(unlist(strsplit(text.string," ")))/4))], collapse=" ")
      ,"\n",
      paste(unlist(strsplit(text.string," "))[(floor(length(unlist(strsplit(text.string," ")))/4*2)+1):(3*floor(length(unlist(strsplit(text.string," ")))/4))], collapse=" ")
      ,"\n",
      paste(unlist(strsplit(text.string," "))[(3*floor(length(unlist(strsplit(text.string," ")))/4)+1):length(unlist(strsplit(text.string," ")))], collapse=" ")
      ,sep="")
  
  length(unlist(strsplit(text.string," ")))
  # Insert arbitrarily:  vector 
  
  nlhere <- c(nlhere, length(unlist(strsplit(text.string," "))))
  nlhere <- nlhere[nlhere <= length(unlist(strsplit(text.string," ")))]
  nlhere <- unique(nlhere)
  nlhere <- sort(nlhere)
  NL.here <- character()
  for(i in 1:length(nlhere)){
    if(i != length(nlhere))
      NL.here <- paste(NL.here, paste(unlist(strsplit(text.string," "))[(length(unlist(strsplit(NL.here," "))) +1): nlhere[i] ], collapse= " "), "\n", sep="")
    if(i == length(nlhere))
      NL.here <- paste(NL.here, paste(unlist(strsplit(text.string," "))[(nlhere[i-1]+1): nlhere[i] ], collapse= " "), sep="")
  }
  
  return(c(NL.2, NL.3, NL.4, NL.here))
  
}

#' @title standardize
#' @description A helper function for determining y-axis position
#' @param x a vector
standardize <- function(x){(x-mean(x))/(length(unique(x)))}

# Utility Functions ------------------------------------------------------
#' @title ddmmyy.chron
#' @description This function converts text dates to Date dates via lubridate
#' @param string character string vector with ddmmyy format
#' @seealso convert.dates
#' @examples
#' \dontrun{
#' }
#' @author Greg Cicconetti
ddmmmyy.data   <- function(string){
  as.Date(dmy(paste(substring(as.character(string), 1,2), 
                    substring(as.character(string), 3,5), 
                    substring(as.character(string), 6,9))))
}

#' @title convert.dates
#' @description This function applies ddmmmyy.data to all columns of a data.frame ending in 'DT'
#' @param dataset a data.frame
#' @examples
#' \dontrun{
#' }
#' @author Greg Cicconetti
convert.dates   <- function(dataset){
  # Applys ddmmyy.chron to all columns in a data.frame ending in 'DT'
  for(i in names(dataset)[substring(names(dataset),nchar(names(dataset))-1, nchar(names(dataset))) =="DT"]) {
    dataset[,colnames(dataset) == i] <- ddmmmyy.data((dataset[,colnames(dataset) == i]))
  } 
  dataset
}

#' @title fmt
#' @description A function to control number of digits used in graphics.
#' @details This function is used within ggplot, e.g. (scale_y_continuous(labels=fmt(digits=3))) to control the number of digits presented. By default, axis labels will truncate zeros so that labels might read: 0, 2.5, 5, 7.5. Using this will result in labels: 0.0, 2.5, 5.0, 7.5.
#' @param digits number of digits displayed
#' @examples
#' \dontrun{
#' }
#' @author Greg Cicconetti
fmt <- function(digits=2){
  function(x) format(x, nsmall = digits, scientific = FALSE)
}

#' @title FacetLabelAdjuster
#' @description This function takes a 'facet wrapped' ggplot and adds axis labels when a rxc grid is incomplete  
#' @details Taken in whole from: http://stackoverflow.com/questions/13297155/add-floating-axis-labels-in-facet-wrap-plot
#' @param x a ggplot object
#' @param pos maintain default
#' @param newpage maintain default
#' @param vp maintain default
#' @examples
#' \dontrun{
#' }
#' @author Ripped from stackoverflow
facetAdjust <-
  function (x, pos = c("up", "down"), newpage = is.null(vp), vp = NULL) 
  {
    ggplot2:::set_last_plot(x)
    if (newpage) 
      grid.newpage()
    pos <- match.arg(pos)
    p <- ggplot_build(x)
    gtable <- ggplot_gtable(p)
    dims <- apply(p$panel$layout[2:3], 2, max)
    nrow <- dims[1]
    ncol <- dims[2]
    panels <- sum(grepl("panel", names(gtable$grobs)))
    space <- ncol * nrow
    n <- space - panels
    if (panels != space) {
      idx <- (space - ncol - n + 1):(space - ncol)
      gtable$grobs[paste0("axis_b", idx)] <- list(gtable$grobs[[paste0("axis_b", 
                                                                       panels)]])
      if (pos == "down") {
        rows <- grep(paste0("axis_b\\-[", idx[1], "-", idx[n], 
                            "]"), gtable$layout$name)
        lastAxis <- grep(paste0("axis_b\\-", panels), gtable$layout$name)
        gtable$layout[rows, c("t", "b")] <- gtable$layout[lastAxis, 
                                                          c("t")]
      }
    }
    if (is.null(vp)) {
      grid.draw(gtable)
    }
    else {
      if (is.character(vp)) 
        seekViewport(vp)
      else pushViewport(vp)
      grid.draw(gtable)
      upViewport()
    }
    invisible(list(p, gtable))
  }


# David's functions-----
#' @title start.session.log
#' @description A function to start logging the session history for a graphic driver run 
#' @details Note that the stop.session.log function is used to stop the logging and save the log file.
#' @param outputfile passed to name the session history log file
#' @section Value: 
#' \describe{
#' No objects are returned by this function.
#' }
#' @author David Wade
start.session.log<-function(outputfile="example.PDF")
{
  # Strip the extension off of outputfile and replace with .rhis
  log.file.name <- paste0(logd,strsplit(outputfile,split = "[.]")[[1]][1],".rhis")
  # start directing all console output to a log file
  options(warn = 1) # default setting
  sink(log.file.name)
  sink(stdout(), type="message")  
  # print log header information
  cat("*******************************************************************\n")
  cat("*                                                                 *\n")
  cat("* GlaxoSmithKline                                                 *\n")
  cat("* FiguRes R Graphics System                                       *\n")
  cat("*                                                                 *\n")    
  cat("* Graphics session history log file                               *\n")
  cat("*                                                                 *\n")
  cat("*******************************************************************\n")
  cat(" \n")
  cat(paste("***"," R session history log file: ",log.file.name," ***\n",sep=""))  
  cat(" \n")
  log.start.time<<-Sys.time()        # the double-headed arrow assigns to a global value outside of the function
  cat(paste("Run time logging commenced at: ",Sys.time(),"\n"))
  cat(" \n")
  cat(" \n")
  cat(" \n")
  # log information about the R session in which the graphics driver was run
  cat(" \n")
  cat(" \n")
  cat("* R Session Information                                           *\n")     
  cat("*******************************************************************\n")
  save.the.info<-sessionInfo()
  #x<-alien              # these commands gets logged if executed outside of function but not in it.
  #cat("hello world\n")
  #Sys.info()            # these commands gets logged if executed outside of function but not in it.
  print(save.the.info)
  cat(" \n") 
}

#' @title stop.session.log
#' @description A function to stop logging the session history for a graphic driver run and save the session history file
#' @details Note that the start.session.log function is used to start the logging, and it must be called first.
#' @section Value: 
#' \describe{
#' No objects are returned by this function.
#' }
#' @examples
#' \dontrun{ }
#' @author David Wade
stop.session.log<-function()
{
  cat(" \n")
  cat(" \n")
  cat(" \n")
  cat(paste("*** log ended at ", Sys.time(), " ***",sep=""))
  log.stop.time<-Sys.time()
  log.elapsed.time<-round((log.stop.time-log.start.time))
  cat(paste("*** elapsed run time was ",log.elapsed.time," seconds ***",sep=""))
  # stop directing all console output to a log file
  sink(type="message")
  sink()
}



#' @title code column names
#' @description This function reads in a dataframe from a HARP dddata datafile .csv and returns a copy of the dataframe with a format-specified subset of the column names coded to a format-specified set of numerical values specified by the format name passed.
#' @param parent.df The name of the dataframe to be processed.
#' @param format.name  The name of a pre-specified format
#' @author David Wade
code.column.names <- function(parent.df = working.df, format.name = "TRT633")
{                     
  working.df <- parent.df   
  # Code any treatment columns by assigning code numbers to them
  format.name.found <- FALSE  
  if (format.name=="TRT633") {
    format.name.found <- TRUE
    names(working.df)[grep('Control',names(working.df))] <- 't_0'
    names(working.df)[grep('X4.mg',  names(working.df))] <- 't_1'
    names(working.df)[grep('X6.mg',  names(working.df))] <- 't_2'    
    names(working.df)[grep('X8.mg',  names(working.df))] <- 't_3'    
    names(working.df)[grep('X10.mg', names(working.df))] <- 't_4'    
    names(working.df)[grep('X12.mg', names(working.df))] <- 't_5'    
    names(working.df)[grep('X15.mg', names(working.df))] <- 't_6'    
    names(working.df)[grep('X25.mg', names(working.df))] <- 't_7'    
  }
  
  if (format.name.found==FALSE){    
    cat(paste("figuRes Warning:","The format name",format.name,"was not found.\n"))    
  }  
  parent.df <- working.df
  return(parent.df)
}

#' @title decode.column.values
#' @description This function reads in a dataframe that has a column with numerically coded values and returns a copy of the dataframe with a new column containing the decoded text values and the decoded plot symbols for those same values
#' @param parent.df The name of the dataframe to be processed
#' @param format.name  The name of a pre-specified treatment format
#' @param coded.col  The name of the existing column containing the numerically coded treatment values
#' @param formatted.col  The name of the new column to contain the decoded treatment text values
#' @param formatted.symbol.col  The name of the new column to contain the decoded treatment plot symbols
#' @examples
#' \dontrun{ 
#' # Assign formatted values to TREATMENT
#' working.df <- decode.column.values (parent.df = working.df, format.name  ="TRT633", coded.col ="TREATMENT", formatted.col="TREATMENT.TEXT", formatted.symbol.col="TREATMENT.SYMBOL")
#' working.df <- (parent.df=working.df, format.name="TRT633")
#' }
#' @author David Wade
decode.column.values <- function (parent.df    = working.df,
                                  format.name  = "TRT633",
                                  coded.col    ="TREATMENT",
                                  formatted.col="TREATMENT.TEXT",
                                  formatted.symbol.col="TREATMENT.SYMBOL")
  {
  working.df <- parent.df 
  working.df[,formatted.symbol.col] <- NULL  
  working.df[,formatted.col]        <- NULL
  format.name.found<-FALSE
  if (format.name=="TRT633") {
    format.name.found<-TRUE    
    working.df[,formatted.symbol.col][working.df[,coded.col]=="0"] <- "C"    
    working.df[,formatted.symbol.col][working.df[,coded.col]=="1"] <- "4"    
    working.df[,formatted.symbol.col][working.df[,coded.col]=="2"] <- "6"    
    working.df[,formatted.symbol.col][working.df[,coded.col]=="3"] <- "8"    
    working.df[,formatted.symbol.col][working.df[,coded.col]=="4"] <- "0"    
    working.df[,formatted.symbol.col][working.df[,coded.col]=="5"] <- "2"    
    working.df[,formatted.symbol.col][working.df[,coded.col]=="6"] <- "5"    
    working.df[,formatted.symbol.col][working.df[,coded.col]=="7"] <- "H"
    working.df[,formatted.col][working.df[,coded.col]=="0"] <- "Control"    
    working.df[,formatted.col][working.df[,coded.col]=="1"] <- "4 mg"    
    working.df[,formatted.col][working.df[,coded.col]=="2"] <- "6 mg"    
    working.df[,formatted.col][working.df[,coded.col]=="3"] <- "8 mg"    
    working.df[,formatted.col][working.df[,coded.col]=="4"] <- "10 mg"    
    working.df[,formatted.col][working.df[,coded.col]=="5"] <- "12 mg"    
    working.df[,formatted.col][working.df[,coded.col]=="6"] <- "15 mg"    
    working.df[,formatted.col][working.df[,coded.col]=="7"] <- "25 mg"    
    working.df[,formatted.col]<-factor(working.df[,formatted.col],
                                       c("Control","4 mg", "6 mg", "8 mg", "10 mg", "12 mg", "15 mg", "25 mg"))
    }
  if (format.name.found==FALSE){    
    cat(paste("figuRes Warning:","The format name",format.name,"was not found."))    
  } 
  
  parent.df <- working.df  
  return(parent.df)
}




#' @title code.treatment.values
#' @description blah
#' @param format.name
#' @param coded.col
#' @param raw.col
code.treatment.values <- function (parent.df    = working.df,
                                   format.name  = "TRT633",
                                   coded.col    ="TREATMENT",                                                                   
                                   raw.col   ="Planned.Treatment"
)
{
  
  working.df <- parent.df  
  
  working.df[,coded.col] <- "Null"
  
  
  format.name.found=FALSE
  
  if (format.name=="TRT633") {
    format.name.found=TRUE
    
    
    working.df[,coded.col][working.df[,raw.col]=="Control"] <- "0"
    working.df[,coded.col][working.df[,raw.col]=="4 mg"] <- "1"
    working.df[,coded.col][working.df[,raw.col]=="6 mg"] <- "2"
    working.df[,coded.col][working.df[,raw.col]=="8 mg"] <- "3"
    working.df[,coded.col][working.df[,raw.col]=="10 mg"] <- "4"
    working.df[,coded.col][working.df[,raw.col]=="12 mg"] <- "5"
    working.df[,coded.col][working.df[,raw.col]=="15 mg"] <- "6"
    working.df[,coded.col][working.df[,raw.col]=="25 mg"] <- "7"
    
    
  }
  
  if (format.name.found==FALSE){
    cat(paste("figuRes Warning:","The format name",format.name,"was not found."))
  }
  
  parent.df <- working.df
  
  return(parent.df)
}


#' @title code.treatment.columns
#' @description This function reads in a dataframe from a HARP dddata datafile .csv and returns a copy of the dataframe with a format-specified subset of the column names coded to a format-specified set of numerical values specified by the format name passed. 
#' @param parent.df The name of the dataframe to be processed.
#' @param format.name  The name of a pre-specified format (valid values are: ?TRT633?)
#' @examples
#' \dontrun{     working.df <- (parent.df=working.df, format.name=?TRT633?)
#' }
#' @author David Wade
code.treatment.columns <- function (parent.df = working.df,
                                    format.name = "TRT633"
)
{
  
  working.df <- parent.df  
  
  # Code any treatment columns by assigning code numbers to them
  
  format.name.found=FALSE
  
  if (format.name=="TRT633") {
    format.name.found=TRUE
    names(working.df)[grep('Control',names(working.df))] <- 't_0'
    names(working.df)[grep('X4.mg',  names(working.df))] <- 't_1'
    names(working.df)[grep('X6.mg',  names(working.df))] <- 't_2'
    names(working.df)[grep('X8.mg',  names(working.df))] <- 't_3'
    names(working.df)[grep('X10.mg', names(working.df))] <- 't_4'
    names(working.df)[grep('X12.mg', names(working.df))] <- 't_5'
    names(working.df)[grep('X15.mg', names(working.df))] <- 't_6'
    names(working.df)[grep('X25.mg', names(working.df))] <- 't_7'
  }
  
  if (format.name.found==FALSE){
    cat(paste("figuRes Warning:","The format name",format.name,"was not found."))
  }
  
  parent.df <- working.df
  
  return(parent.df)
}


#' @title decode.treatment.values
#' @description This function reads in a dataframe that has a column with numerically coded values and returns a copy of the dataframe with a new column containing the decoded text values and the decoded plot symbols for those same values
#' @param parent.df The name of the dataframe to be processed
#' @param format.name  The name of a pre-specified treatment format (valid values are: ?TRT633?)
#? @param coded.col  The name of the existing column containing the numerically coded treatment values
#? @param formatted.col  The name of the new column to contain the decoded treatment text values
#? @param formatted.symbol.col  The name of the new column to contain the decoded treatment plot symbols
#' @examples
#' \dontrun{    }
#' @author David Wade
decode.treatment.values <- function (parent.df    = working.df,
                                     format.name  = "TRT633",
                                     coded.col    ="TREATMENT",
                                     formatted.col="TREATMENT.TEXT",
                                     formatted.symbol.col="TREATMENT.SYMBOL"
)
{
  
  working.df <- parent.df  
  
  working.df[,formatted.symbol.col] <- "Null"
  working.df[,formatted.col]        <- "Null"
  
  format.name.found=FALSE
  
  if (format.name=="TRT633") {
    format.name.found=TRUE
    
    working.df[,formatted.symbol.col][working.df[,coded.col]=="0"] <- "C"
    working.df[,formatted.symbol.col][working.df[,coded.col]=="1"] <- "4"
    working.df[,formatted.symbol.col][working.df[,coded.col]=="2"] <- "6"
    working.df[,formatted.symbol.col][working.df[,coded.col]=="3"] <- "8"
    working.df[,formatted.symbol.col][working.df[,coded.col]=="4"] <- "0"
    working.df[,formatted.symbol.col][working.df[,coded.col]=="5"] <- "2"
    working.df[,formatted.symbol.col][working.df[,coded.col]=="6"] <- "5"
    working.df[,formatted.symbol.col][working.df[,coded.col]=="7"] <- "H"
    
    working.df[,formatted.col][working.df[,coded.col]=="0"] <- "Control"
    working.df[,formatted.col][working.df[,coded.col]=="1"] <- "4 mg"
    working.df[,formatted.col][working.df[,coded.col]=="2"] <- "6 mg"
    working.df[,formatted.col][working.df[,coded.col]=="3"] <- "8 mg"
    working.df[,formatted.col][working.df[,coded.col]=="4"] <- "10 mg"
    working.df[,formatted.col][working.df[,coded.col]=="5"] <- "12 mg"
    working.df[,formatted.col][working.df[,coded.col]=="6"] <- "15 mg"
    working.df[,formatted.col][working.df[,coded.col]=="7"] <- "25 mg"
    
    working.df[,formatted.col]<-factor(working.df[,formatted.col],
                                       c("Control","4 mg", "6 mg", "8 mg", "10 mg", "12 mg", "15 mg", "25 mg"))
    
  }
  
  if (format.name.found==FALSE){
    cat(paste("figuRes Warning:","The format name",format.name,"was not found."))
  }
  
  parent.df <- working.df
  
  return(parent.df)
}