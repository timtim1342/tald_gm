library(tidyverse)
library(testthat)

test_that("Test for wrong languages:
I expect that all languages in the `lang` field of tables in `../data/orig_table`
match with `../data/genlangpoints.csv`", {
  read_tsv("../data/genlangpoints.csv",
           progress = FALSE, 
           show_col_types = FALSE) %>% 
    pull(lang) %>% 
    unique() %>% 
    sort() ->
    expected_langs
  
  observed <- map_chr(list.files("../data/orig_table", full.names = TRUE), function(table){
    read_tsv(table,
             progress = FALSE, 
             show_col_types = FALSE) %>% 
      distinct(lang) %>% 
      pull(lang) ->
      langs_from_table
    wrong_langs <- langs_from_table[which(!(langs_from_table %in% expected_langs))]
    ifelse(length(wrong_langs) > 0, 
           str_c("Wrong language names in the dataset ", 
                 str_remove(table, "../data/orig_table/"), ": ", 
                 str_c(wrong_langs, collapse = ", ")), 
           str_c("The dataset ", 
                 str_remove(table, "../data/orig_table/"), 
                 " is ok"))
  })
  expect_equal(observed, 
               str_c("The dataset ", list.files("../data/orig_table"), " is ok"))
})
