# Global functions for figures2 
# -----------------------------

# default.settins----
#' @title default.settings
#' @description Global Defaults
#' @details Global Defaults
#' @section Value: 
#' \describe{
#' The following are assigned to global environment upon calling:
#'  \itemize{
#'  my.path <<- getwd()
#'   \item dd
#'   \item cd
#'   \item od
#'   \item blankPanel 
#' }
#' }
#' @author Greg Cicconetti
default.settings <- function() 
{
  # Directories
  my.path <<- getwd()
  dd <<- paste0(my.path, "/dddata/")
  cd <<- paste0(my.path, "/code/")
  od <<- paste0(my.path, "/output/")
  logd <<- paste0(my.path, "/log/")
  cat("my.path is set to:", my.path, "\n")
  cat("dd is set to:", dd, "\n")
  cat("cd is set to:", cd, "\n")
  cat("od is set to:", od, "\n")
  cat("logd is set to:", logd, "\n")
  cat("supd is set to:", supd, "\n")
  cat("devd is set to:", devd, "\n")
  cat("docd is set to:", docd, "\n")
  cat("\nNote: If a path is assigned to an object called od2, non-pdf files created by\nrun.specific will be diverted to that folder.\n")
  
  page.width <<- 11
  page.height <<- 8.5
  right.margin <<- .75
  left.margin <<- .75  
  top.margin <<- 1.4-.5
  bottom.margin <<- 1.75-.5
  
  cat("The default page dimension is set to landscape:", 
      page.width, "in. x", page.height,"in.\n")
  cat("The default page left and right page margins:", 
      left.margin, "in. and", right.margin  ,"in., respectively.\n")
  cat("The default top an bottom margins:", 
      top.margin, "in. and ", bottom.margin, "in., respectively.")
  
  graph.region.h <<- page.height - (right.margin + left.margin)
  graph.region.w <<- page.width - (top.margin + bottom.margin)
  blankPanel <<- grid.rect(gp=gpar(col="white"), draw=FALSE)
  
  theme_set(theme_grey2_nomargins())
  cat("\nThe default theme: theme_grey2_nomargins", "\n")
}

# default.settings2----
#' @title default.settings2 - next iteration of default.settings
#' @description Global Defaults
#' @details Global Defaults
#' @param my.path path to main directory,
#' @param dd directory where data is stored
#' @param cd directory where driver (code) files are stored
#' @param od directory where output files are sent
#' @param logd directory where log files are sent
#' @param main.theme text string name of theme to be called by theme_set,
#' @param page.width presumed to be inches
#' @param page.height presumed to be inches
#' @param right.margin presumed to be inches
#' @param left.margin presumed to be inches 
#' @param top.margin presumed to be inches
#' @param bottom.margin presumed to be inches
#' @section Value: 
#' \describe{
#' The following are assigned to global environment upon calling:
#'  \itemize{
#'  my.path <<- getwd()
#'   \item dd
#'   \item cd
#'   \item od
#'   \item blankPanel 
#'   \item page.width 
#'   \item page.height 
#'   \item right.margin 
#'   \item left.margin 
#'   \item top.margin 
#'   \item bottom.margin 
#'   \item graph.region.h
#'   \item graph.region.w
#'   \item blankPanel 
#' }
#' }
#' @author Greg Cicconetti
default.settings2 <- function(
  my.path=getwd(),
  main.theme="theme_bw",
  dd = paste0(getwd(), "/dddata/"),
  cd = paste0(getwd(), "/code/"),
  od = paste0(getwd(), "/output/"),
  logd = paste0(getwd(), "/log/"),
  page.width = 11,
  page.height = 8.5,
  right.margin = .75,
  left.margin = .75,  
  top.margin = 1.4-.5,
  bottom.margin = 1.75-.5)
{
  # Directories
  my.path <<-my.path
  dd <<- dd
  cd <<- cd
  od <<- od
  logd <<- logd

  cat("my.path is set to:", my.path, "\n")
  cat("dd is set to:", dd, "\n")
  cat("cd is set to:", cd, "\n")
  cat("od is set to:", od, "\n")
  cat("logd is set to:", logd, "\n")
  
  page.width <<- page.width
  page.height <<- page.height
  right.margin <<- right.margin
  left.margin <<- left.margin  
  top.margin <<- top.margin
  bottom.margin <<- bottom.margin
  cat("The default page dimension is set to landscape:", 
      page.width, "in. x", page.height,"in.\n")
  cat("The default page left and right page margins:",
      left.margin, "in. and", right.margin  ,"in., respectively.\n")
  cat("The default top an bottom margins:", 
      top.margin, "in. and ", bottom.margin, "in., respectively.")
  
  graph.region.h <<- page.height - (right.margin + left.margin)
  graph.region.w <<- page.width - (top.margin + bottom.margin)
  blankPanel <<- grid.rect(gp=gpar(col="white"), draw=FALSE)
  
  do.call("theme_set", list(do.call(main.theme, list())))
  cat(paste("\nThe default theme:", main.theme, "\n"))
}

# Custom Themes ----------------------------------------

# theme_grey2_nomargins----
#' @title theme_grey2_nomargins
#' @description Adapts theme_grey() found in ggplot2 
#' @details axis.text colour changed from "grey50" to "black"; legend.position changed from "right" to "bottom"; legend.direction changed to "horizontal"; plot.margin changed from default unit(c(1, 1, 0.5, 0.5), "lines") to unit(c(0, 0, 0, 0), "in")
#' @param base_size use default
#' @param base_family  use default
#' @examples
#' \dontrun{
#' theme_set(theme_grey2_nomargins())
#' }
#' @author Greg Cicconetti
theme_grey2_nomargins <-  function (base_size = 12, base_family = ""){
    theme(line = element_line(colour = "black", 
                              size = 0.5, 
                              linetype = 1, 
                              lineend = "butt"), 
          rect = element_rect(fill = "white",
                              colour = "black", 
                              size = 0.5, 
                              linetype = 1), 
          text = element_text(family = base_family,
                              face = "plain", 
                              colour = "black", 
                              size = base_size, 
                              hjust = 0.5, 
                              vjust = 0.5, 
                              angle = 0, 
                              lineheight = 0.9), 
          axis.text = element_text(size = rel(0.8),  colour = "black"), 
          strip.text = element_text(size = rel(0.8)), 
          axis.line = element_blank(), 
          axis.text.x = element_text(vjust = 1), 
          axis.text.y = element_text(hjust = 1), 
          axis.ticks = element_line(colour = "grey50"), 
          axis.title.x = element_text(), 
          axis.title.y = element_text(angle = 90, vjust=1), 
          axis.ticks.length = unit(0.15, "cm"), 
          axis.ticks.margin = unit(0.1, "cm"), 
          legend.background = element_rect(colour = NA), 
          legend.margin = unit(0.2, "cm"), 
          legend.key = element_rect(fill = "grey95",   colour = "white"), 
          legend.key.size = unit(1.2, "lines"), 
          legend.key.height = NULL, 
          legend.key.width = NULL, 
          legend.text = element_text(size = rel(0.8)), 
          legend.text.align = NULL, 
          legend.title = element_text(size = rel(0.8), 
                                      face = "bold", 
                                      hjust = 0), 
          legend.title.align = NULL, 
          legend.position = "bottom",	 
          legend.direction = "horizontal",  
          legend.justification = "center", 
          legend.box = NULL, 
          panel.background = element_rect(fill = "grey90", 
                                          colour = NA), 
          panel.border = element_blank(), 
          panel.grid.major = element_line(colour = "white"), 
          panel.grid.minor = element_line(colour = "grey95", size = 0.25), 
          panel.margin = unit(0.25, "lines"), 
          strip.background = element_rect(fill = "grey80", 
                                          colour = NA), 
          strip.text.x = element_text(), 
          strip.text.y = element_text(angle = -90), 
          plot.background = element_rect(colour = "white"), 
          plot.title = element_text(size = rel(1.2)), 
          panel.margin.x = NULL, 
          panel.margin.y = NULL,
          plot.margin = unit(c(0, 0, 0, 0), "in"), 
          complete = TRUE)
  }

# theme_grey2_default_margins----
#' @describeIn theme_grey2_nomargins Same as theme_grey2_nomargins but with margins set to ggplot defaults, unit(c(1, 1, 0.5, 0.5), "lines")
theme_grey2_default_margins <-  function (base_size = 12, base_family = ""){
    theme(line = element_line(colour = "black", 
                              size = 0.5, 
                              linetype = 1, 
                              lineend = "butt"), 
          rect = element_rect(fill = "white",
                              colour = "black", 
                              size = 0.5, 
                              linetype = 1), 
          text = element_text(family = base_family,
                              face = "plain", 
                              colour = "black", 
                              size = base_size, 
                              hjust = 0.5, 
                              vjust = 0.5, 
                              angle = 0, 
                              lineheight = 0.9), 
          axis.text = element_text(size = rel(0.8),  colour = "black"), 
          strip.text = element_text(size = rel(0.8)), 
          axis.line = element_blank(), 
          axis.text.x = element_text(vjust = 1), 
          axis.text.y = element_text(hjust = 1), 
          axis.ticks = element_line(colour = "grey50"), 
          axis.title.x = element_text(), 
          axis.title.y = element_text(angle = 90, vjust=1), 
          axis.ticks.length = unit(0.15, "cm"), 
          axis.ticks.margin = unit(0.1, "cm"), 
          legend.background = element_rect(colour = NA), 
          legend.margin = unit(0.2, "cm"), 
          legend.key = element_rect(fill = "grey95",   colour = "white"), 
          legend.key.size = unit(1.2, "lines"), 
          legend.key.height = NULL, 
          legend.key.width = NULL, 
          legend.text = element_text(size = rel(0.8)), 
          legend.text.align = NULL, 
          legend.title = element_text(size = rel(0.8), 
                                      face = "bold", 
                                      hjust = 0), 
          legend.title.align = NULL, 
          legend.position = "bottom",   
          legend.direction = "horizontal",  
          legend.justification = "center", 
          legend.box = NULL, 
          panel.background = element_rect(fill = "grey90", 
                                          colour = NA), 
          panel.border = element_blank(), 
          panel.grid.major = element_line(colour = "white"), 
          panel.grid.minor = element_line(colour = "grey95", size = 0.25), 
          panel.margin = unit(0.25, "lines"), 
          strip.background = element_rect(fill = "grey80", 
                                          colour = NA), 
          strip.text.x = element_text(), 
          strip.text.y = element_text(angle = -90), 
          plot.background = element_rect(colour = "white"), 
          plot.title = element_text(size = rel(1.2)), 
          panel.margin.x = NULL, 
          panel.margin.y = NULL,
          plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"), 
          complete = TRUE)
  }

# theme_grey2_nomargins----
#' @describeIn theme_grey2_nomargins Similar to theme_grey2
theme_bw2_nomargins <- function (base_size = 12, base_family = "") 
{
  theme_grey2_nomargins(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text = element_text(size = rel(0.8)), axis.ticks = element_line(colour = "black"), 
          legend.key = element_rect(colour = "grey80"), panel.background = element_rect(fill = "white", 
                                                                                        colour = NA), panel.border = element_rect(fill = NA, 
                                                                                                                                  colour = "grey50"), panel.grid.major = element_line(colour = "grey90", 
                                                                                                                                                                                      size = 0.2), panel.grid.minor = element_line(colour = "grey98", 
                                                                                                                                                                                                                                   size = 0.5), strip.background = element_rect(fill = "grey80", 
                                                                                                                                                                                                                                                                                colour = "grey50"), strip.background = element_rect(fill = "grey80", 
                                                                                                                                                                                                                                                                                                                                    colour = "grey50"))
}

#' @describeIn theme_grey2_nomargins Similar to theme_bw_nomargins but with margins set to ggplot defaults, unit(c(1, 1, 0.5, 0.5), "lines")
theme_bw2_default_margins <- function(base_size = 12, base_family = ""){
  theme_grey2_default_margins(base_size = base_size, base_family =base_family) %+replace%
    theme(axis.text = element_text(size = rel(0.8)), 
          axis.ticks = element_line(colour = "black"), 
          legend.key = element_rect(colour = "grey80"), 
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA, colour = "grey50"), 
          panel.grid.major = element_line(colour = "grey90", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
          strip.background = element_rect(fill = "grey80", colour = "grey50"), 
          strip.background = element_rect(fill = "grey80", colour = "grey50"),
          plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"))
}

#' @describeIn theme_grey2_nomargins alteration to theme_grey
theme_table_nomargins <- function (base_size = 12, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text = element_text(size = rel(0.8)), 
          axis.ticks = element_line(colour = "black"), 
          legend.key = element_rect(colour = "grey80"), 
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA, colour = "white"), 
          panel.grid.major = element_line(colour = "white", size = 0.2), 
          panel.grid.minor = element_line(colour = "white", size = 0.5), 
          strip.background = element_rect(fill = "white", colour = "white", size = 0.2),
          plot.margin = unit(c(0, 0, 0, 0), "lines"))
}
