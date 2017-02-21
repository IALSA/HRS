basic_line <- function(
  d_observed,
  variable_name,
  time_metric, 
  color_name="black",
  line_alpha=1,
  line_size =.5, 
  smoothed = FALSE,
  main_title     = variable_name,
  x_title        = paste0("Time metric: ", time_metric),
  y_title        = variable_name,
  rounded_digits = 0L
) {
  
  d_observed <- as.data.frame(d_observed) #Hack so dplyr datasets don't mess up things
  d_observed <- d_observed[!base::is.na(d_observed[, variable_name]), ]
  
  g <- ggplot(d_observed, aes_string(x=time_metric, y = variable_name)) 
  if(!smoothed){
    g <- g + geom_line(aes_string(group="id"), size=line_size, color=scales::alpha(color_name,line_alpha), na.rm=T)   
  } else{
    g <- g + geom_smooth(aes_string(group="id"),size=line_size,  method="lm",color=scales::alpha(color_name,line_alpha), na.rm=T, se=F )
    g <- g + geom_smooth(method="loess", color="blue", size=1, fill="gray80", alpha=.3, na.rm=T)
    
  }  
  
  g <- g + scale_x_continuous(labels=scales::comma_format()) +
    scale_y_continuous(labels=scales::comma_format()) +
    labs(title=main_title, x=x_title, y=y_title) +
    theme_light() +
    theme(axis.ticks.length = grid::unit(0, "cm"))
  return( g )
}

# g <- basic_line(d, "cogn_global", "fu_year", "salmon", .9, .1, F)
# g

basic_line_v2 <- function(
  d_observed,
  variable_name,
  time_metric, 
  color_name="black",
  line_alpha=1,
  line_size =.5, 
  smoothed = FALSE,
  main_title     = variable_name,
  x_title        = paste0("Time metric: ", time_metric),
  y_title        = variable_name,
  rounded_digits = 0L
) {
  
  d_observed <- as.data.frame(d_observed) #Hack so dplyr datasets don't mess up things
  d_observed <- d_observed[!base::is.na(d_observed[, variable_name]), ]
  
  g <- ggplot(d_observed, aes_string(x=time_metric, y = variable_name)) 
  if(!smoothed){
    g <- g + geom_line(aes_string(group="id"), size=line_size, color=scales::alpha(color_name,line_alpha), na.rm=T)   
  } else{
    g <- g + geom_smooth(aes_string(group="id"),size=line_size,  method="lm",color=scales::alpha(color_name,line_alpha), na.rm=T, se=F )
    g <- g + geom_smooth(method="loess", color="blue", size=1, fill="gray80", alpha=.3, na.rm=T)
    
  }  
  
  g <- g + 
    # scale_x_continuous(labels=scales::comma_format()) +
    scale_y_continuous(labels=scales::comma_format()) +
    labs(title=main_title, x=x_title, y=y_title) +
    theme_light() +
    theme(axis.ticks.length = grid::unit(0, "cm"))
  return( g )
}



# function to create discrete histogram. taken from RAnalysisSkeleton
histogram_discrete <- function(
  d_observed,
  variable_name,
  levels_to_exclude   = character(0),
  main_title          = variable_name,
  x_title             = NULL,
  y_title             = "Number of Included Records",
  text_size_percentage= 6,
  bin_width           = 1L) {
  
  d_observed <- as.data.frame(d_observed) #Hack so dplyr datasets don't mess up things
  if( !base::is.factor(d_observed[, variable_name]) )
    d_observed[, variable_name] <- base::factor(d_observed[, variable_name])
  
  d_observed$iv <- base::ordered(d_observed[, variable_name], levels=rev(levels(d_observed[, variable_name])))
  
  ds_count <- plyr::count(d_observed, vars=c("iv"))
  # if( base::length(levels_to_exclude)>0 ) { }
  ds_count <- ds_count[!(ds_count$iv %in% levels_to_exclude), ]
  
  ds_summary <- plyr::ddply(ds_count, .variables=NULL, transform, count=freq, proportion = freq/sum(freq) )
  ds_summary$percentage <- base::paste0(base::round(ds_summary$proportion*100), "%")
  
  y_title <- base::paste0(y_title, " (n=", scales::comma(base::sum(ds_summary$freq)), ")")
  
  g <- ggplot(ds_summary, aes_string(x="iv", y="count", fill="iv", label="percentage")) +
    geom_bar(stat="identity") +
    geom_text(stat="identity", size=text_size_percentage, hjust=.8) +
    scale_y_continuous(labels=scales::comma_format()) +
    labs(title=main_title, x=x_title, y=y_title) +
    coord_flip()
  
  theme  <- theme_light(base_size=14) +
    theme(legend.position = "none") +
    theme(panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank()) +
    theme(axis.text.x=element_text(colour="gray40")) +
    theme(axis.title.x=element_text(colour="gray40")) +
    theme(axis.text.y=element_text(size=14)) +
    theme(panel.border = element_rect(colour="gray80")) +
    theme(axis.ticks.length = grid::unit(0, "cm"))
  
  return( g + theme )
}

# function to create continuous histogram. taken from RAnalysisSkeleton
histogram_continuous <- function(
  d_observed,
  variable_name,
  bin_width      = NULL,
  main_title     = variable_name,
  x_title        = paste0(variable_name, " (each bin is ", scales::comma(bin_width), " units wide)"),
  y_title        = "Frequency",
  rounded_digits = 0L
) {
  
  d_observed <- as.data.frame(d_observed) #Hack so dplyr datasets don't mess up things
  d_observed <- d_observed[!base::is.na(d_observed[, variable_name]), ]
  
  ds_mid_points <- base::data.frame(label=c("italic(X)[50]", "bar(italic(X))"), stringsAsFactors=FALSE)
  ds_mid_points$value <- c(stats::median(d_observed[, variable_name]), base::mean(d_observed[, variable_name]))
  ds_mid_points$value_rounded <- base::round(ds_mid_points$value, rounded_digits)
  
  g <- ggplot(d_observed, aes_string(x=variable_name)) +
    geom_histogram(binwidth=bin_width, fill="gray70", color="gray90", position=position_identity()) +
    geom_vline(xintercept=ds_mid_points$value, color="gray30") +
    geom_text(data=ds_mid_points, aes_string(x="value", y=0, label="value_rounded"), color="tomato", hjust=c(1, 0), vjust=.5) +
    scale_x_continuous(labels=scales::comma_format()) +
    scale_y_continuous(labels=scales::comma_format()) +
    labs(title=main_title, x=x_title, y=y_title) +
    theme_light() +
    theme(axis.ticks.length = grid::unit(0, "cm"))
  
  ds_mid_points$top <- stats::quantile(ggplot2::ggplot_build(g)$panel$ranges[[1]]$y.range, .8)
  g <- g + ggplot2::geom_text(data=ds_mid_points, ggplot2::aes_string(x="value", y="top", label="label"), color="tomato", hjust=c(1, 0), parse=TRUE)
  return( g )
}


