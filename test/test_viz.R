library(testthat)

source('../SRC/viz_source.R')

data(iris)
df <- iris %>% 
  mutate(SpeciesBinary = ifelse(Species == 'setosa', 1, 0))

test_that("Test factor_dep_plot_data", {
  res <- df %>% 
    factor_dep_plot_data('Species', 'SpeciesBinary')
  
  expect_equal(names(res), c('Species', 'response', 'sd', 'count', 'lower', 'upper'))
  expect_equal(dim(res), c(3, 6))
  expect_is(res, 'data.frame')
})

test_that('TEST factor_dep_plot', {
  res <- df %>%
    factor_dep_plot_data('Species', 'SpeciesBinary') %>%
    factor_dep_plot('Species')
  
  expect_is(res, 'ggplot')
})

test_that('TEST cont_dep_plot_data', {
  res <- df %>%
    cont_dep_plot_data('Sepal.Length', 'SpeciesBinary')
  
  expect_equal(names(res), c('Sepal.Length', 'response', 'sd', 'count', 'lower', 'upper'))
  expect_equal(dim(res), c(9, 6))
  expect_is(res, 'data.frame')
})

#testthat::test_dir("../test/", reporter="summary")

  