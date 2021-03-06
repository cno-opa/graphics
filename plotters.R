# plotters.R
# A ggplot theme and some helpful functions to make making charts a breeze
#
# Important: Either use the function here, buildChart, to cut up the chart elements are arrange them properly, or include three line breaks ('\n\n\n') in the chart title of your ggplot call
#
#
# This is what is included:
# ==============================
#
# theme_opa : A sweet little diddy.
# buildChart : A function that cuts up and rearranges grobs to get that left-justification we've always dreamed of.
# lineOPA : A generic line chart generator with a few options. See below for details.
# barOPA : A generic histogram generator. Also, see below.
# schigoda : All hail the area chart of might.
# some aesthetic variables used by the chart functions for their color schemes.
#
# ===============================
#
#
# TODO: fn for facet grids, fn to control ggsave defaults

library("grid")
library("gridExtra")
library("gtable")
library("ggplot2")
require(lubridate)
require(zoo)
require(dplyr)
require(ggplot2)
require(scales)
require(reshape2)
require(maps)
require(maptools)
require(sp)
require(rgdal)
require(jsonlite)
require(dplyr)
require(rgeos)
require(RColorBrewer)
require(classInt)
require(ggmap)


theme_opa <- function (base_size = 14, base_family = "")
{
  theme(
    line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text = element_text(family = base_family, face = "plain", colour = "black", size = base_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9,margin = margin(), debug = FALSE),

    axis.text = element_text(size = rel(0.6), colour = "grey50"),
    strip.text = element_text(size = rel(0.8)),
    axis.line = element_blank(),
    axis.line.y = element_blank(),
    #axis.text.x = element_text(vjust = 1, angle = 45, hjust = .97),
    #axis.text.y = element_text(hjust = 1),
    axis.ticks = element_blank(),
    axis.title.x = element_text(),
    axis.title.y = element_text(angle = 90),
    axis.ticks.length = unit(0.15, "cm"),
    #axis.ticks.margin = unit(0.1, "cm"),

    legend.background = element_rect(colour = NA),
    legend.margin = unit(0.2, "cm"),
    legend.key = element_rect(colour = NA, fill = "white"),
    legend.key.size = unit(.8, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(0.75)),
    legend.text.align = NULL,
    legend.title = element_blank(),
    legend.title.align = NULL,
    legend.position = c(0,1),
    legend.direction = "horizontal",
    legend.justification = c(-0.02, 0.8),
    legend.box = NULL,

    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_blank(),
    panel.grid.major = element_line(colour = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey70"),
    panel.grid.minor = element_blank(),
    panel.margin = unit(0.25, "lines"),
    panel.margin.x = NULL,
    panel.margin.y = NULL,
    

    strip.background = element_rect(fill = "grey80", colour = NA),
    strip.text.x = element_text(),
    strip.text.y = element_text(angle = -90),

    plot.background = element_rect(colour = "white"),
    plot.title = element_text(size = rel(1.2), hjust = 0.05),
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    
    ##new additions for ggplot2 2.1.0
    
    # new parameter has been added to package
    panel.ontop = FALSE,
    
    # modified to accomodate deprecated axis.ticks.margin in ggplot2 2.1.0
    axis.text.x = element_text(vjust = 1, angle = 45, hjust = .97,margin = unit(0.1, "cm")),
    axis.text.y = element_text(hjust = 1,margin = unit(0.1, "cm")),
    
    #missing elements generating warnings 
    strip.switch.pad.grid = grid::unit(0, 'cm'),
    strip.switch.pad.wrap = grid::unit(0, 'cm'),
    axis.line.x = element_blank(),
    complete = TRUE)
}

theme_opa_minimal <- function (base_size = 14, base_family = "")
{
  theme(
    line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text = element_text(family = base_family, face = "plain", colour = "black", size = base_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),

    axis.text = element_text(size = rel(0.6), colour = "grey70"),
    strip.text = element_text(size = rel(0.8)),
    axis.line = element_line(colour = "gray70"),
    
    axis.ticks = element_line(colour = "gray70"),
    axis.title.x = element_text(),
    axis.title.y = element_text(angle = 90),
    axis.ticks.length = unit(0.15, "cm"),
    # deprecated (axis.ticks.margin = unit(0.1, "cm"),)

    legend.background = element_rect(colour = NA),
    legend.margin = unit(0.2, "cm"),
    legend.key = element_rect(colour = NA, fill = "white"),
    legend.key.size = unit(.8, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(0.75)),
    legend.text.align = NULL,
    legend.title = element_blank(),
    legend.title.align = NULL,
    legend.position = c(0,1),
    legend.direction = "horizontal",
    legend.justification = c(-0.02, 0.8),
    legend.box = NULL,

    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.margin = unit(0.25, "lines"),
    panel.margin.x = NULL,
    panel.margin.y = NULL,
    
    strip.background = element_rect(fill = "grey80", colour = NA),
    strip.text.x = element_text(),
    strip.text.y = element_text(angle = -90),

    plot.background = element_rect(colour = "white"),
    plot.title = element_text(size = rel(1.2), hjust = 0.05),
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    
    ##new additions for ggplot2 2.1.0
    
    # new parameter has been added to package
    panel.ontop = FALSE,
    
    # modified to accomodate deprecated axis.ticks.margin in ggplot2 2.1.0
    axis.text.x = element_text(vjust = 1, angle = 45, hjust = .97,margin = unit(0.1, "cm")),
    axis.text.y = element_text(hjust = 1,margin = unit(0.1, "cm")),
    axis.line.x = element_blank(),
    
    #missing elements generating warnings 
    strip.switch.pad.grid = grid::unit(0, 'cm'),
    strip.switch.pad.wrap = grid::unit(0, 'cm'),
    axis.line.x = element_blank(),
      
    complete = TRUE)
}

#aesthetic variables
lightBlue <- "#72d0ff"
darkBlue <- "#005983"
red <- "tomato"

#good/bad fills scale for using barOPA to show net change (and other pos-neg comparisons)
good_bad <- c(darkBlue, red)
names(good_bad) <- c("good", "bad")
good_bad_scale <- scale_fill_manual(name = "shade", values = good_bad)

#chart builder!
buildChart <- function(p) {

  gt <- ggplot_gtable(ggplot_build(p))

    gtl <- gt[[1]]

  t <- gtable_filter(gt, "title")[[1]]
  t[[1]]$just <- "left"

  p <- p + theme(legend.position = "none", plot.title = element_blank())

  if( TRUE %in% grepl("guide-box", gtl) ) {
    l <- gtable_filter(gt, "guide-box")[[1]]

    built <- arrangeGrob(
              t[[1]],
              l[[1]],
              p,
              nrow = 3,
              heights = c(1, 1.2, 10)
             )
  } else {
    built <- arrangeGrob(
              t[[1]],
              p,
              nrow = 2,
              heights = c(1, 10)
             )
  }

  return(built)
}

lineOPA <- function(data, x, y, title = "Title!", group = 1, percent = FALSE, currency = FALSE, last_label = TRUE, lab.size = 4, min_date_labels = FALSE, ...) {
  # most of the options are passed as dots parameters:
  # set data labels with `labels = "label_column"`
  # set highlight with `highlight = "group_to_highlight"`
  # set y-axis label with `ylab = "label"`
  # set legend labels with `legend.labels = character vector`
  # percent = FALSE refers to whether or not y-axis should be in percent
  # currency = FALSE refers to whether or not y-axis should be in currency
  # lab.size = 4, set custom label size
  # use `target.line = x` to draw dashed horizontal line at y-intercept of x

  dots <- eval(substitute(alist(...)))

  #make y variable continuous
  data[y] <- as.numeric(data[y][[1]])

  lab.size = as.numeric(lab.size)

  #get max y value
  ymax <- max(data[y], na.rm = TRUE)

  #function used by highlighting feature
  remap <- function(input, matcher, value) {
    matcher <- paste0(matcher, "(?! .)")
    i <- grep(matcher, names(input), perl = TRUE)
    input[i] <- value
    return(input)
  }

  if(group == 1) {
    blues <- darkBlue
  } else {
    blues <- colorRampPalette( c(darkBlue, lightBlue) )(nrow(unique(data[group])))
    names(blues) <- as.matrix(unique(data[group]))[,1]
  }

  if( !is.null(dots$highlight) ) {
    blues <- remap(blues, dots$highlight, red)
  }

  #the very basic base
  base <- ggplot(data, aes_string(x = x, y = y, group = group, colour = group)) +
          geom_line(size = 1.5) +
          labs(title = title, x = "", y = "")

  if( !is.null(dots$ylab) ) {
    base <- base + labs(y = dots$ylab)
  }

  if(group == 1) {
    base <- base + geom_line(colour = blues) + guides(colour = FALSE)
  } else {
    base <- base + scale_colour_manual( values = blues )
  }

  if( !is.null(dots$legend.labels) ) {
    legend.labels <- eval(dots$legend.labels)
    base <- base + scale_colour_manual( values = blues, labels = legend.labels )
  }

  if( !is.null(dots$labels) & last_label == TRUE ) {
    #hacky way to get labels data when there is more than one series. pulls all data in df for the named period
    getLabelsData <- function() {
      if(group != 1 & exists("r_period")) {
        d <- data[data[x] == r_period,]
        return(d)
      } else if (group!= 1 & !exists("r_period")) {
        data_ <- data[complete.cases(data),]
        last_x <- data_[x][nrow(data_),]
        d <- data_[data_[,x] == last_x,]
        return(d)
      } else {
        return(data[nrow(data), ])
      }
    }

    labels_data <- getLabelsData()

    base <- base +
            geom_text(data = labels_data, size = lab.size, colour = "grey33", hjust = -0.2, aes_string(label = dots$labels, y = y)) +
            scale_x_discrete(expand = c(0, 2.5)) #extend the width of the plot area so label doesn't get cut off
  } else if( !is.null(dots$labels) ) {
    base <- base +
            geom_text(size = lab.size, colour = "grey33", vjust = -0.5, aes_string(label = dots$labels, y = y))
  }

  if(currency == TRUE){
    base <- base +
            scale_y_continuous(breaks = brks, labels = dollar_format(largest_with_cents=0)) +
            expand_limits(y = c(0, brks[length(brks)]))
  }

  if(percent == FALSE) {
    brks <- pretty_breaks(4, min.n = 4)(0:ymax)

    #handle cases where ymax is float
    if(brks[length(brks)] <= ymax) {
      brks <- c(brks, (brks[length(brks)] + abs(brks[2] - brks[1])))
    }

    yul  <- brks[length(brks)]
    base <- base + scale_y_continuous(breaks = brks) + expand_limits(y = c(0, yul))
  } else {
    #hack to get pretty_breaks to work for percents. can also use prettyPercentBreaks() in utils
    ymax <- ymax * 100
    brks <- (pretty_breaks(4, min.n = 4)(0:ymax))/100
    for(i in 1:length(brks)) {
      if(brks[i] > 1) {
        brks[i] <- 1
      }
    }
    brks <- unique(brks)
    base <- base + scale_y_continuous(breaks = brks, labels = percent(brks)) + expand_limits(y = c(0, brks[length(brks)]))
  }

  if( !is.null(dots$target.line) ) {
    t <- as.numeric(dots$target.line)
    base <- base + geom_hline(aes(group = 1), yintercept = t, colour = "grey55", linetype = "dashed", size = 1)

    if(t >= brks[length(brks)] && percent == TRUE) {
      ymax <- t * 100
      brks <- (pretty_breaks(4, min.n = 4)(0:ymax))/100
      for(i in 1:length(brks)) {
        if(brks[i] > 1) {
          brks[i] <- 1
        }
      }
      brks <- unique(brks)
      base <- base + scale_y_continuous(breaks = brks, labels = percent(brks)) + expand_limits(y = c(0, brks[length(brks)]))
    } else if(t >= brks[length(brks)] && percent == FALSE) {
      ymax <- t
      brks <- pretty_breaks(4, min.n = 4)(0:ymax)
      yul  <- brks[length(brks)]

      base <- base + scale_y_continuous(breaks = brks) + expand_limits(y = c(0, yul))
    }
  }

  if ( min_date_labels == TRUE ) {
    labs <- ifelse(grepl("[Jj]an", data[x][[1]]), as.character(data[x][[1]]), substr(data[x][[1]], 1, 1))
    base <- base + scale_x_discrete(breaks = data[x][[1]], labels = labs)
  }

  return(base)
}

barOPA <- function(data, x, y, title = "Title", stat = "identity", position = "identity", percent = FALSE, currency = FALSE, ...) {
  # set fill with `fill = "variable"` if you have multiple groups
  # set y-axis label with `ylab = "label"`
  # set data labels with `labels = "label_column"`
  # set legend labels with `legend.labels = character vector`

  dots <- eval(substitute(alist(...)))

  #get max y value and set breaks
  if( position == "stack" ) {
    x_ <- as.symbol(x)
    y_ <- as.symbol(y)

    group_sums <- group_by_(data, x_) %>%
                  summarise(s = sum(y_))

    ymax <- max(group_sums$s, na.rm = TRUE)
    brks <- pretty_breaks(4, min.n = 4)(0:ymax)
    yul  <- brks[length(brks)]
  } else {
    ymax <- max(data[y], na.rm = TRUE)
    brks <- pretty_breaks(4, min.n = 4)(0:ymax)
    yul  <- brks[length(brks)]
  }

  #the basic base
  base <- ggplot(data, aes_string(x = x, y = y, fill = dots$fill)) +
          geom_bar(stat = stat, position = position) +
          labs(title = title, y = "", x = "") +
          scale_y_continuous(breaks = brks) + expand_limits(y = c(0, yul))


  if(currency == TRUE) {
    base <- base +
            scale_y_continuous(breaks = brks, labels = dollar_format(largest_with_cents=0)) +
            expand_limits(y = c(0, brks[length(brks)]))
  }

  if(percent == TRUE) {
    ymax <- ymax * 100
    brks <- (pretty_breaks(4, min.n = 4)(0:ymax))/100
    for(i in 1:length(brks)) {
      if(brks[i] > 1) {
        brks[i] <- 1
      }
    }
    brks <- unique(brks)
    base <- base + scale_y_continuous(breaks = brks, labels = percent(brks)) + expand_limits(y = c(0, brks[length(brks)]))
  }

  if( !is.null(dots$ylab) ) {
    base <- base + labs(y = dots$ylab)
  }

  if( !is.null(dots$labels) ) {
    base <- base +
            geom_text(size = 2, colour = "grey33",  vjust = ifelse(data[y] >= 0, -0.5, 1.5), aes_string(label = dots$labels, y = y))
  }

  if( !is.null(dots$fill) ) {
    blues <- colorRampPalette( c(darkBlue, lightBlue) )(nrow(unique(data[dots$fill])))
    base <- base + geom_bar(stat = stat, position = position) + scale_fill_manual(values = blues)
  } else {
    base <- base + geom_bar(stat = stat, position = position, fill = darkBlue)
  }

  if( !is.null(dots$legend.labels) ) {
    legend.labels <- eval(dots$legend.labels)
    base <- base + scale_fill_manual( values = blues, labels = legend.labels )
  }

  return(base)
}

schigoda <- function(data, x, y, title = "Schigoda!", fill, ...) {
  # fill is not optional on this area chart of might
  # set legend labels with `legend.labels = character vector`

  dots <- eval(substitute(alist(...)))

  blues <- colorRampPalette( c(darkBlue, lightBlue) )(nrow(unique(data[fill])))

  #area
  data[, x] <- as.Date(as.yearmon(data[, x]))
  base <- ggplot(data, aes_string(x = x, y = y, fill = fill)) +
          geom_area(position = "identity") +
          labs(title = title, x = "", y = "") +
          scale_fill_manual(values = blues) +
          scale_x_date(breaks = pretty_breaks(9)(data[, x]), labels = date_format("%b %Y"))

  if( !is.null(dots$legend.labels) ) {
    legend.labels <- eval(dots$legend.labels)
    base <- base + scale_fill_manual( values = blues, labels = legend.labels )
  }

  return(base)
}

wiseChart <- function(data, x, y, formula, title = "Title!", title.dates = TRUE) {
  # attach a highlight column to `data` to highlight certain facets. this data vector will be mapped to the colour aesthetics of the graph
  # set title.dates = TRUE if you want the title of the graph to have the date range in it. use this to remove dates from the x-axes per Schigoda's Razor

  brks <- unique(data[,x])[seq(1, 13, 5)] #keep in case you want to turn the x-axis dates on (turned off by theme call below)

  if(!("highlight" %in% colnames(data))) {
    data$highlight <- "no"
  }

  if(title.dates == TRUE) {
    if(is.factor(data[,x]) == TRUE) {
      u <- levels(data[,x])[length(levels(data[,x]))]
      l <- levels(data[,x])[1]
    } else {
      u <- max(data[,x])
      l <- min(data[,x])
    }
    title <- paste(title, as.character(l), "to", as.character(u))
  }

  base <- ggplot(data, aes_string(x = x, y = y, group = 1, colour = "highlight")) +
          geom_line(size = 1) +
          facet_grid(formula, scales = "free_y") +
          labs(title = title, x = "", y = "") +
          scale_x_discrete(breaks = brks) +
          scale_colour_manual(values = c("grey70", darkBlue)) +
          theme(panel.grid.major.y = element_blank(),
                panel.background = element_rect(fill = "grey90"),
                legend.position = "none",
                strip.background = element_blank(),
                strip.text.x = element_text(face = "bold"),
                strip.text.y = element_text(face = "bold"),
                axis.text.x = element_blank()
               )

  return(base)
}

area100pOPA <- function(data, x, y, title = "Title!", group, percent = FALSE, last_label = TRUE, ...){

  dots <- eval(substitute(alist(...)))

  #make y variable continuous
  data[y] <- as.numeric(data[y][[1]])

  #Define colors
  blues <- colorRampPalette( c(darkBlue, lightBlue) )(nrow(unique(data[group])))
  names(blues) <- as.matrix(unique(data[group]))[,1]

  #make the basic plot
  base <- ggplot(data, aes_string(x = x, y = y)) +
    geom_area(aes_string(fill = group, group = group), position='stack') +
    labs(title = title, x = "", y = "")


  if( !is.null(dots$legend.labels) ) {
    legend.labels <- eval(dots$legend.labels)
    base <- base + scale_fill_manual( values = blues, labels = legend.labels )
  }

  else{
    base <- base + scale_fill_manual( values = blues )
  }

  if( !is.null(dots$labels) & last_label == TRUE ) {
  #hacky way to get labels data when there is more than one series. pulls all data in df for the named period
  getLabelsData <- function() {
      return(data[nrow(data), ])
    }
  labels_data <- getLabelsData()
  base <- base +
          geom_text(data = labels_data, size = 4, colour = "grey33", hjust = -0.2, aes_string(label = dots$labels, y = y)) +
            scale_x_discrete(expand = c(0, 2.4)) #extend the width of the plot area so label doesn't get cut off
  }

  else if( !is.null(dots$labels) ) {
    base <- base +
            geom_text(size = 4, colour = "grey33", vjust = -0.5, aes_string(label = dots$labels, y = y))
  }

  if(percent == TRUE) {
    ymax <- 100
    brks <- (pretty_breaks(4, min.n = 4)(0:ymax))/100
    for(i in 1:length(brks)) {
      if(brks[i] > 1) {
        brks[i] <- 1
      }
    }
    brks <- unique(brks)
    base <- base + scale_y_continuous(breaks = brks, labels = percent(brks)) + expand_limits(y = c(0, brks[length(brks)]))
  }


  return(base)
}
