# PRETTY_NUMBERS
# Formats the numbers so that they are more human-readable
pretty_numbers <- function(number) {
  if (number < 1000) {
    # Do almost nothing
    out <- format(number, big.mark=",", scientific=FALSE)
  } else if (number < 10000) {
    # Format as ab,cde
    out <- format(number, big.mark=",", scientific=FALSE)
  } else if (number < 1000000) {
    # Format with K
    factor <- 1/1000
    out <- paste(number*factor,"K",sep="")
  } else if (number < 1000000000) {
    # Format with M
    factor <- 1/1000000
    out <- paste(round(number*factor,2),"M",sep="")
  } else {
    # Format with B
    factor <- 1/1000000000
    out <- paste(round(number*factor,2),"B",sep="")
  }
  # # Remove 0unit, just have 0
  # if ("0K" %in% labels) {
  #   labels[which(labels=="0K")]="0"
  # }
  # if ("0M" %in% labels) {
  #   labels[which(labels=="0M")]="0"
  # }
  # if ("0B" %in% labels) {
  #   labels[which(labels=="0B")]="0"
  # }
  return(out)
}




# MAKE_Y_AXIS
# Formats the y axis ticks and labels so that they are easier to read.
# Also returns a multiplicative factor for the plot so that the plot is on the right scale.
make_y_axis <- function(yrange) {
  y_max <- yrange[2]
  if (y_max < 1000) {
    # Do almost nothing
    factor <- 1
    ticks <- pretty(yrange)
    labels <- format(ticks, big.mark=",", scientific=FALSE)    
  } else if (y_max < 10000) {
    # Label with ab,cde
    factor <- 1
    ticks <- pretty(yrange)
    labels <- format(ticks, big.mark=",", scientific=FALSE)
  } else if (y_max < 1000000) {
    # Label with K
    factor <- 1/1000
    ticks <- pretty(yrange*factor)
    labels <- paste(ticks,"K",sep="")
  } else if (y_max < 1000000000) {
    # Label with M
    factor <- 1/1000000
    ticks <- pretty(yrange*factor)
    labels <- paste(ticks,"M",sep="")
  } else {
    # Label with B
    factor <- 1/1000000000
    ticks <- pretty(yrange*factor)
    labels <- paste(ticks,"B",sep="")
  }
  # Remove 0unit, just have 0
  if ("0K" %in% labels) {
    labels[which(labels=="0K")]="0"
  }
  if ("0M" %in% labels) {
    labels[which(labels=="0M")]="0"
  }
  if ("0B" %in% labels) {
    labels[which(labels=="0B")]="0"
  }
  y_axis <- list(factor=factor,ticks=ticks,labels=labels)
  return(y_axis)
}

# MAKE_X_AXIS_WEEK
# Formats the x axis ticks and labels so that they are easier to read.
make_x_axis_week = function(vec_dates) {
  l = length(vec_dates)
  vec_idx=1:l
  begin=vec_dates[1]
  end=vec_dates[l]
  if (l<8){
    # Labels using only weeks
    vec=seq.Date(as.Date(begin),as.Date(end),by="week")
    ticks <- seq(0,l,by=1)
    labels <- ticks
  } else if (l<54){
    # Labels using only month
    labels=seq.Date(as.Date(begin),as.Date(end),by="month")
    ticks = c()
    for(i in 1:length(labels)){
      dte = labels[i]
      indx = which.min(abs(as.Date(dte)-vec_dates))
      ticks = c(ticks,indx)
    }
    # labels <- vec
  } else {
    # Labels using only year
    labels=seq.Date(as.Date(begin),as.Date(end),by="year")
    print("labels")
    print(labels)
    ticks = c()
    for(i in 1:length(labels)){
      dte = labels[i]
      indx = which.min(abs(as.Date(dte)-vec_dates))
      ticks = c(ticks,indx)
    }
    print("ticks")
    print((ticks))
    # labels <- vec
  }
  return(list(ticks=ticks,labels=labels))
}

# PLOT_HR_YAXIS
#
# Plot data using a human readable y-axis
plot_hr_yaxis <- function(x, y, ...) {
  y_range = range(y, na.rm = TRUE)
  y_axis <- make_y_axis(y_range)
  plot(x,y*y_axis$factor,
       yaxt = "n", ...)
  axis(2, at = y_axis$ticks,
       labels = y_axis$labels,
       las = 1, cex.axis=0.8)
}

# PLOT_HR_XAXIS_YAXIS
#
# Plot data using a human readable y-axis
# x must be sim_constants$time$dateFull
plot_hr_xaxis_yaxis <- function(x, y, ...) {
  
  y_range = range(y, na.rm = TRUE)
  y_axis <- make_y_axis(y_range)
  
  x_axis = make_x_axis_week(x)
  # print(x_axis$labels)
  # print(x_axis$ticks)
  
  plot(x,y*y_axis$factor,
       yaxt = "n", ...)
  axis(1, at = x_axis$ticks,
       labels = x_axis$labels,
       las=2, cex.axis=0.8)
  axis(2, at = y_axis$ticks,
       labels = y_axis$labels,
       las = 1, cex.axis=0.8)
}

# PLOT_HR_XAXIS
#
# Plot data using a human readable y-axis
# x must be sim_constants$time$dateFull
plot_hr_xaxis <- function(x, y, ...) {
  
  x_axis = make_x_axis_week(x)
  # y_range = range(y)
  # y_ticks = pretty(ylim)
  
  plot(x,y,yaxt = "n", ...)
  axis(1, at = x_axis$ticks,
       labels = x_axis$labels,
       las=2, cex.axis=0.8)
  # axis(2, at = y_ticks,
       # labels = as.numeric(y_ticks),
       # las = 1, cex.axis=0.8)
}


crop_figure = function(fileFull) {
  fileName = tools::file_path_sans_ext(fileFull)
  fileExt = tools::file_ext(fileFull)
  if (fileExt == "pdf") {
    command_str = sprintf("pdfcrop %s",fileFull)
    system(command_str)
    command_str = sprintf("mv %s-crop.pdf %s.pdf",fileName,fileName)
    system(command_str)
  }
  if (fileExt == "png") {
    command_str = sprintf("convert %s -trim %s-trim.png",fileName,fileName)
    system(command_str)
    command_str = sprintf("mv %s-trim.png %s",fileName,fileName)
    system(command_str)
  }
}

# LATEST_VALUES_GENERAL
#
# For each country/country group in a data frame v, find the latest
# value
latest_values_general <- function(v,c1,c2) {
  l_c1 <- unique(v[[c1]])
  idx <- c()
  for (c in l_c1) {
    tmp <- v[which(v[[c1]] == c),]
    tmp <- tmp[order(tmp[[c2]], decreasing = TRUE),]
    idx1 <- which(v[[c2]] == tmp[[c2]][1])
    idx2 <- which(v[[c1]] == c)
    idx <- c(idx,intersect(idx1,idx2))
  }
  return(v[idx,])
}

