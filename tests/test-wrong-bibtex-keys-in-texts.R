suppressPackageStartupMessages(library(tidyverse))
library(testthat)

test_that("Test bibkeys in articles:", {
  refs_in_rmd <- map_dfr(list.files("../data/orig_rmd", full.names = TRUE), function(rmd){
    tibble(refs = str_extract_all(read_lines(rmd, progress = FALSE), "@\\w*?\\d{4}"),
           files = str_remove(rmd, "../data/orig_rmd/")) %>% 
      unnest_longer(refs) %>% 
      mutate(refs = str_remove_all(refs, "\\@"),
             files = str_remove(files, ".Rmd$")) %>% 
      na.omit() %>% 
      distinct()
  })
  
  map_dbl(seq_along(refs_in_rmd$files), function(i){
    sum(str_detect(read_lines(str_c("../data/orig_bib/", refs_in_rmd$files[i], ".bib"),
                              progress = FALSE),
                   refs_in_rmd$refs[i]))
  }) %>% 
    as.logical() ->
    refs_in_rmd$detected
  
  refs_in_rmd %>% 
    filter(!detected) ->
    refs_absent_in_bib
  
  observed <- if(nrow(refs_absent_in_bib) > 0){
    str_c("There is no bibentry ", refs_absent_in_bib$refs, " in the ", 
          refs_absent_in_bib$files, ".bib")
  } else {
    "everything is ok"
  }
  
  expected <- if(nrow(refs_absent_in_bib) > 0){
    rep("everything is ok", nrow(refs_absent_in_bib))
  } else {
    "everything is ok"
  }
  expect_equal(observed, expected)
})
