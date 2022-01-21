suppressPackageStartupMessages(library(tidyverse))
library(testthat)

test_that("Test number and file names in orig folders:
I expect all files in orig_table, orig_rmd and orig_bib have the same 
names as in the `data/features.csv`", {
  read_tsv("data/features.csv", progress = FALSE, show_col_types = FALSE) %>% 
    pull(filename) %>% 
    sort() ->
    expected
  
  expect_equal(list.files("data/orig_table"), str_c(expected, ".tsv"))
  expect_equal(list.files("data/orig_rmd"), str_c(expected, ".Rmd"))
  expect_equal(list.files("data/orig_bib"), str_c(expected, ".bib"))
})

test_that("Test hashes in rmarkdown text:
I expect that the h1 header `#` is absent in Rmd files.", {
  
  n_wrong_hashes <- map_dbl(list.files("data/orig_rmd", full.names = TRUE), function(rmd){
    sum(str_detect(read_lines(rmd, progress = FALSE), "^# ")) 
  })
  
  observed <- str_c(file = list.files("data/orig_rmd"), 
                    ": ",
                    n_wrong_hashes = n_wrong_hashes, 
                    " wrong hashes")
  expected <- str_c(file = list.files("data/orig_rmd"), 
                    ": 0 wrong hashes")
  
  expect_equal(observed, expected)
})

test_that("Test column names and separator in data:
I expect that each file in `data/orig_table` is in `.tsv` format and have 
columns listed in `expected_columns`", {
  expected_columns <- c("id", 
                        "lang", 
                        "idiom", 
                        "type", 
                        "genlang_point", 
                        "map", 
                        "feature", 
                        "source", 
                        "page", 
                        "comment", 
                        "contributor", 
                        "date")
  
  check_col_names <- expand.grid(expected_columns, 
                                 list.files("data/orig_table", full.names = TRUE),
                                 stringsAsFactors = FALSE) 
  
  check_col_names$observed <- map2(check_col_names$Var1, check_col_names$Var2, .f = function(var, table){
    var %in% colnames(read_tsv(table, 
                               n_max = 1, 
                               progress = FALSE, 
                               show_col_types = FALSE))
  }) 
  check_col_names %>% 
    mutate(Var2 = str_remove(Var2, "data/orig_table/"),
           observed = str_c("column ", 
                            Var1,
                            " is in the file ",
                            Var2,
                            ": ",
                            observed),
           expected = str_c("column ", 
                            Var1,
                            " is in the file ", 
                            Var2,
                            ": TRUE")) ->
    check_col_names
  
  expect_equal(check_col_names$observed, check_col_names$expected)
})

test_that("Test for wrong languages:
I expect that all languages in the `lang` field of tables in `data/orig_table`
match with `data/genlangpoints.csv`", {
  read_tsv("data/genlangpoints.csv",
           progress = FALSE, 
           show_col_types = FALSE) %>% 
    pull(lang) %>% 
    unique() %>% 
    sort() ->
    expected_langs
  
  observed <- map_chr(list.files("data/orig_table", full.names = TRUE), function(table){
    read_tsv(table,
             progress = FALSE, 
             show_col_types = FALSE) %>% 
      distinct(lang) %>% 
      pull(lang) ->
      langs_from_table
    wrong_langs <- langs_from_table[which(!(langs_from_table %in% expected_langs))]
    ifelse(length(wrong_langs) > 0, 
           str_c("Wrong language names in the dataset ", 
                 str_remove(table, "data/orig_table/"), ": ", 
                 str_c(wrong_langs, collapse = ", ")), 
           str_c("The dataset ", 
                 str_remove(table, "data/orig_table/"), 
                 " is ok"))
  })
  expect_equal(observed, 
               str_c("The dataset ", list.files("data/orig_table"), " is ok"))
})
