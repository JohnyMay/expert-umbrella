library(reshape2)
library(ggplot2)
library(Hmisc)
library(dplyr)

# Load data #

load("./DATA/model_data.rda")

# Distribution plots #

factor_dist_plot <- function(df, value, variable, weight = NULL, ncol = 2,
                             y_trans = 'identity'){
  value <- as.name(value)
  variable <- as.name(variable)
  
  if(is.null(weight)){
    my_aes <- aes_(x = value, fill = variable)
    my_plot <- geom_bar()
  } else {
    weight <- as.name(weight)
    my_aes <- aes_(x = value, y = weight, fill = variable)
    my_plot <-  stat_summary(geom = 'bar', fun.y = 'sum')
  }
  
  p <- df %>%
    ggplot(my_aes) +
    my_plot +
    facet_wrap(~variable, scales = "free", ncol = ncol) +
    scale_y_continuous(labels = scales::comma, trans = y_trans) +
    theme_minimal() +
    theme(legend.position = "none")
  return(p)
}

numeric_dist_plot <- function(df, value, variable, weight = NULL, nbins = 30,
                              ncol = 2, x_trans = 'identity',
                              y_trans = 'identity'){
  value <- as.name(value)
  variable <- as.name(variable)
  
  if(is.null(weight)) {
    my_aes <- aes_(value, fill = variable)
  } else {
    weight <- as.name(weight)
    my_aes <- aes_(value, fill = variable, weight = weight)
  }
  
  p <- df %>%
    ggplot(my_aes) +
    geom_histogram(bins = nbins) +
    facet_wrap(~variable, scales = "free", ncol = ncol) +
    scale_y_continuous(labels = scales::comma, trans = y_trans) +
    scale_x_continuous(labels = scales::comma, trans = x_trans) +
    theme_minimal() +
    theme(legend.position = "none")
  return(p)
}  

# TEST #

# model_data %>%
#   select(GENDER, AGE_GROUP, IS_CITY, POPULATION) %>%
#   melt(id.vars = "POPULATION") %>%
#   factor_dist_plot('value', 'variable', weight = "POPULATION") %>%
#   print()
# 
# model_data %>%
#   select(GENDER, AGE_GROUP, IS_CITY, POPULATION) %>%
#   melt(id.vars = "POPULATION") %>%
#   factor_dist_plot('value', 'variable', weight = NULL) %>%
#   print()

# model_data %>%
#   select(SALARY, ALCOHOL, FERTILITY_RATE, LATITUDE, LONGITUDE, POPULATION) %>%
#   melt(id.vars="POPULATION") %>%
#   numeric_dist_plot("value", "variable", weight = "POPULATION") %>%
#   print()
# 
# model_data %>%
#   select(SALARY, ALCOHOL, FERTILITY_RATE, LATITUDE, LONGITUDE, POPULATION) %>%
#   melt(id.vars="POPULATION") %>%
#   numeric_dist_plot("value", "variable") %>%
#   print()

# Target dependence plots #

# cont_dep_plot_data <- function(data, x, y, weight = NULL

factor_dep_plot_data <- function(data, x, y, weight = NULL, quantile = 0.95){
  
  if(is.null(weight)){
    df <- df %>%
      mutate(weight = 1)
    weight <- 'weight'
  }
  
  y <- as.name(y)
  weight <- as.name(weight)
  
  df <- data %>%
    select(!!x, !!y, !!weight) %>%
    melt(id.vars=c(y, weight)) %>%
    group_by(variable, value) %>%
    summarise(response = wtd.mean(!!y, !!weight),
              sd = sqrt(wtd.var(!!y, !!weight)),
              count = n()) %>%
    mutate(lower = response - qnorm(quantile) * sd/sqrt(count),
           upper = response + qnorm(quantile) * sd/sqrt(count)) %>%
    data.frame()
  
  return(df)
}

factor_dep_plot <- function(data, x, y, lower_ci, upper_ci, title = 'variable'){
  
  labels <- unique(data[[x]])
  
  x <- as.name(x)
  y <- as.name(y)
  lower_ci <- as.name(lower_ci)
  upper_ci <- as.name(upper_ci)
  title <- as.name(title)
  
  p <- data %>%
    ggplot() +
    geom_errorbar(aes_(x, ymin = lower_ci, ymax = upper_ci, width=1/length(labels))) + 
    geom_point(aes_(x, y)) +
    facet_wrap(as.formula(paste0('~', title)), scales = "free", ncol = 2) +
    theme_minimal() +
    theme(legend.position = "none")
  return(p)
}

# TEST #

model_data %>%
  mutate(TARGET = TARGET/POPULATION) %>%
  select(AGE_GROUP, GENDER, IS_CITY, POPULATION, TARGET) %>%
  factor_dep_plot_data('AGE_GROUP', 'TARGET', 'POPULATION') %>%
  factor_dep_plot('value', 'response', 'lower', 'upper') %>%
  print()


bin_column <- function(col, bins, equally_spaced = TRUE){
  
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
  if(length(breaks)>=length(unique(col))){
    res <- col
  } else {
    res <- cut(col, breaks = breaks, include.lowest = TRUE)
  }
  return(res)
}

cont_dep_plot_data <- function(data, x, y, weight = NULL, bins=10, equally_spaced=TRUE, quantile=0.95){
  
  if(is.null(weight)){
    data <- data %>%
      mutate(weight = 1)
    weight <- 'weight'
  }
  
  y <- as.name(y)
  weight <- as.name(weight)
  
  res <- data %>%
    mutate_at(x, .funs = funs('BIN' = bin_column), bins = bins) %>%
    group_by(BIN) %>%
    summarise(label = median(!!as.name(x)),
              response = wtd.mean(!!y, !!weight),
              sd = sqrt(wtd.var(!!y, !!weight)),
              count = n()
    ) %>%
    mutate(lower = response - qnorm(quantile) * sd/sqrt(count),
           upper = response + qnorm(quantile) * sd/sqrt(count)) 
  
  return(res)
}

# TEST #

# model_data %>%
#   mutate(TARGET = TARGET/POPULATION) %>%
#   select(YEAR, POPULATION, TARGET) %>%
#   cont_dep_plot_data('YEAR', 'TARGET', weight = 'POPULATION')

cont_dep_plot <- function(data, x, y, lower_ci, upper_ci){
  
  labels <- unique(data[[x]])
                   
  x <- as.name(x)
  y <- as.name(y)
  lower_ci <- as.name(lower_ci)
  upper_ci <- as.name(upper_ci)
  
  p <- data %>%
    ggplot(aes_(x=x, y=y)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes_(ymin = lower_ci, ymax = upper_ci, width = 1/length(labels))) + 
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