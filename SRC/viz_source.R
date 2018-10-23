library(reshape2)
library(ggplot2)
library(Hmisc)
library(dplyr)

# Load data #

# load("./DATA/model_data.rda")

# Distribution plots #

factor_dist_plot <- function(data, x, weight = NULL, y_trans = 'identity', ...){
  
  if(is.null(weight)){
    data <- data %>%
      mutate(weight = 1)
    weight <- 'weight'
  }
  
  x <- as.name(x)
  weight <- as.name(weight)
  
  p <- data %>%
    ggplot(aes_(x = x, y = weight)) +
    stat_summary(geom = 'bar', fun.y = 'sum', ...) +
    scale_y_continuous(labels = scales::comma, trans = y_trans) +
    xlab(x) +
    theme_minimal() +
    theme(legend.position = "none")
  
  return(p)
}

# TEST #

# model_data %>%
#   factor_dist_plot('GENDER', fill = 'navy') %>%
#   print()

numeric_dist_plot_new <- function(data, x, weight = NULL, nbins = 30, x_trans = 'identity', y_trans = 'identity', ...){
  
  if(is.null(weight)){
    data <- data %>%
      mutate(weight = 1)
    weight <- 'weight'
  }
  
  x <- as.name(x)
  weight <- as.name(weight)
  
  p <- data %>%
    ggplot(aes_(x = x, weight = weight)) +
    geom_histogram(bins = nbins, ...) +
    scale_y_continuous(labels = scales::comma, trans = y_trans) +
    scale_x_continuous(labels = scales::comma, trans = x_trans) +
    xlab(x) + 
    theme_minimal() +
    theme(legend.position = "none")
  
  return(p)
}

# TEST #

# model_data %>%
#   numeric_dist_plot_new('FERTILITY_RATE', fill = 'navy') %>%
#   print()

# Target dependence plots #

factor_dep_plot_data <- function(data, x, y, weight = NULL, quantile = 0.95){
  
  if(is.null(weight)){
    data <- data %>%
      mutate(weight = 1)
    weight <- 'weight'
  }
  
  cols_to_select <- c(x, y, weight)
 
  x <- as.name(x)
  y <- as.name(y)
  weight <- as.name(weight)
  
  data <- data %>%
    select(one_of(cols_to_select)) %>%
    group_by(!!x) %>%
    summarise(response = wtd.mean(!!y, !!weight),
              sd = sqrt(wtd.var(!!y, !!weight)),
              count = n()) %>%
    mutate(lower = response - qnorm(quantile) * sd/sqrt(count),
           upper = response + qnorm(quantile) * sd/sqrt(count)) %>%
    data.frame()
  
  return(data)
}

# TEST #

model_data %>%
  mutate(TARGET = TARGET/POPULATION) %>%
  factor_dep_plot_data('AGE_GROUP', 'TARGET', weight = 'POPULATION')
  

factor_dep_plot <- function(data, x, y = 'response', ymin = 'lower', ymax = 'upper'){
  
  labels <- unique(data[[x]])
  
  x <- as.name(x)
  y <- as.name(y)
  ymin <- as.name(ymin)
  ymax <- as.name(ymax)
  
  p <- data %>%
    ggplot() +
    geom_errorbar(aes_(x, ymin = ymin, ymax = ymax, width = 1/length(labels))) + 
    geom_point(aes_(x, y)) +
    theme_minimal() +
    xlab(x) +
    theme(legend.position = "none")
  
  return(p)
}

# TEST #

# model_data %>%
#   mutate(TARGET = TARGET/POPULATION) %>%
#   select(AGE_GROUP, POPULATION, TARGET) %>%
#   factor_dep_plot_data('AGE_GROUP', 'TARGET', weight = 'POPULATION') %>%
#   factor_dep_plot('AGE_GROUP')

bin_column <- function(col, bins = 10, equally_spaced = TRUE){
  #Breaks can be either a vector of breaks or scalar indicating number of intervals to split into
  if(length(bins) > 1){
    breaks <- bins
  } else {
    if(equally_spaced){
      breaks <- seq(from = min(col), to = max(col), length.out = bins)
    } else {
      breaks <- quantile(col, probs = seq(0, 1, length.out = bins))
    }
  }

  # If there are more bins than unique values, don't break into bins but just use unique values
  if(length(breaks) >= length(unique(col))){
    res <- col
  } else {
    res <- cut(col, breaks = breaks, include.lowest = TRUE)
  }
  return(res)
}

cont_dep_plot_data <- function(data, x, y, weight = NULL, bins = 10, equally_spaced = TRUE, quantile = 0.95){
  
  if(is.null(weight)){
    data <- data %>%
      mutate(weight = 1)
    weight <- 'weight'
  }
  
  cols_to_select <- c(x, y, weight)
  
  y <- as.name(y)
  weight <- as.name(weight)
  x <- as.name(x)
  
  res <- data %>%
    select(one_of(cols_to_select)) %>%
    mutate(BIN = bin_column(!!x)) %>%
    group_by(BIN) %>%
    summarise(!!x := median(!!x),
              response = wtd.mean(!!y, !!weight),
              sd = sqrt(wtd.var(!!y, !!weight)),
              count = n()
    ) %>%
    mutate(lower = response - qnorm(quantile) * sd/sqrt(count),
           upper = response + qnorm(quantile) * sd/sqrt(count)) %>%
    select(-BIN) %>%
    data.frame()
  
  return(res)
}

# TEST #

# model_data %>%
#   mutate(TARGET = TARGET/POPULATION) %>%
#   select(YEAR, POPULATION, TARGET) %>%
#   cont_dep_plot_data('YEAR', 'TARGET', weight = 'POPULATION')

cont_dep_plot <- function(data, x, y, ymin, ymax){
  
  labels <- unique(data[[x]])
                   
  x <- as.name(x)
  y <- as.name(y)
  ymin <- as.name(ymin)
  ymax <- as.name(ymax)
  
  p <- data %>%
    ggplot(aes_(x=x, y=y)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes_(ymin = ymin, ymax = ymax, width = 1/length(labels))) + 
    scale_x_continuous(labels = labels, breaks = labels) +
    scale_y_continuous(labels = scales::comma, trans = 'identity') +
    theme_minimal() +
    theme(legend.position = "none")
    
  return(p)
}

# TEST #

# model_data %>%
#   mutate(TARGET = TARGET/POPULATION) %>%
#   select(YEAR, POPULATION, TARGET) %>%
#   cont_dep_plot_data('YEAR', 'TARGET', weight = 'POPULATION') %>%
#   cont_dep_plot('label', 'response', 'lower', 'upper') %>%
#   print()