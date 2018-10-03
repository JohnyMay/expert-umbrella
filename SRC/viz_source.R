library(reshape2)
library(ggplot2)
library(Hmisc)

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

factor_dep_plot <- function(df, value, variable, response, lower_ci, upper_ci){
  
  value <- as.name(value)
  variable <- as.name(variable)
  response <- as.name(response)
  lower_ci <- as.name(lower_ci)
  upper_ci <- as.name(upper_ci)
  
  p <- df %>%
    ggplot() +
    geom_errorbar(aes_(value, ymin = lower_ci, ymax = upper_ci, color=variable)) + 
    geom_point(aes_(value, response, color = variable)) +
    facet_wrap(~variable, scales = "free", ncol = 2) +
    scale_y_continuous(labels = scales::comma, trans = 'identity') +
    theme_minimal() +
    theme(legend.position = "none")
  return(p)
}


model_data %>%
  select(AGE_GROUP, GENDER, IS_CITY, POPULATION, TARGET) %>%
  melt(id.vars=c("POPULATION", "TARGET")) %>%
  group_by(variable, value) %>%
  summarise(response = wtd.mean(TARGET/POPULATION, POPULATION),
            sd = sqrt(wtd.var(TARGET/POPULATION, POPULATION)),
            count = n()) %>%
  mutate(lower = response - qnorm(0.95) * sd/sqrt(count),
         upper = response + qnorm(0.95) * sd/sqrt(count)) %>%
  factor_dep_plot("value", "variable", "response", "lower", "upper") %>%
  print()


model_data %>%
  select(AGE_GROUP, GENDER, IS_CITY, POPULATION, TARGET) %>%
  melt(id.vars=c("POPULATION", "TARGET")) %>%
  group_by(variable, value) %>%
  summarise(response = wtd.mean(TARGET/POPULATION, POPULATION),
            sd = sqrt(wtd.var(TARGET/POPULATION, POPULATION)),
            count = n()) %>%
  mutate(lower = response - qnorm(0.95) * sd/sqrt(count),
         upper = response + qnorm(0.95) * sd/sqrt(count))


model_data %>%
  select(AGE_GROUP, GENDER, IS_CITY, POPULATION, TARGET) %>%
  melt(id.vars=c("POPULATION", "TARGET")) %>%
  group_by(variable, value) %>%
  summarise(response = wtd.mean(TARGET/1, 1),
            sd = sqrt(wtd.var(TARGET/1, 1)),
            count = n()) %>%
  mutate(lower = response - qnorm(0.95) * sd/sqrt(count),
         upper = response + qnorm(0.95) * sd/sqrt(count))


wtd.mean(c(1,2,3), rep(1, 3))
