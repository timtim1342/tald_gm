suppressPackageStartupMessages(library(tidyverse))
library(testthat)

test_that("Test number and file names in orig folders:
I expect all files in orig_table, orig_rmd and orig_bib have the same 
names as in the `../data/features.csv`", {
  read_tsv("../data/features.csv", progress = FALSE, show_col_types = FALSE) %>% 
    pull(filename) %>% 
    sort() ->
    expected
  
  expect_equal(list.files("../data/orig_table"), str_c(expected, ".tsv"))
  expect_equal(list.files("../data/orig_rmd"), str_c(expected, ".Rmd"))
  expect_equal(list.files("../data/orig_bib"), str_c(expected, ".bib"))
})
