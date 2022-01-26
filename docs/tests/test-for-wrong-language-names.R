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
  
  map_dfr(list.files("../data/orig_table", full.names = TRUE), function(tsv){
    read_tsv(tsv,
             progress = FALSE, 
             show_col_types = FALSE) %>% 
      distinct(lang) %>% 
      mutate(file = tsv, 
             file = str_remove(file, "../data/orig_table/")) %>% 
      filter(!(lang %in% expected_langs))
  }) ->
    wrong_langs_df

  if(nrow(wrong_langs_df) > 0){
    observed <- str_c("The wrong langage name ", 
                      wrong_langs_df$lang, 
                      " in the file ",
                      wrong_langs_df$file)
    expected <- rep("", nrow(wrong_langs_df))
  } else {
    observed <- "everything is ok"
    expected <- "everything is ok"
  }
  expect_equal(observed, expected)    
})
