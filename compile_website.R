library(tidyverse)

# TESTS --------------------------------------------------------------------
library(testthat)

# I expect all files in orig_table, orig_rmd and orig_bib have the same names
test_that("test number and file names", {
  expect_equal(str_remove(list.files("data/orig_table"), ".tsv"), 
               str_remove(list.files("data/orig_rmd"), ".Rmd"))
  expect_equal(str_remove(list.files("data/orig_table"), ".tsv"),
               str_remove(list.files("data/orig_bib"), ".bib"))
})

# I expect that the h1 header `# ` is present in rmd files just once.
test_that("test wrong hashes in rmarkdown text", {
  
  n_wrong_hashes <- map_dbl(list.files("data/orig_rmd", full.names = TRUE), function(rmd){
    # there should be just one hash in '# Chapter'
    sum(str_detect(read_lines(rmd), "^# "))  - 1 
  })
  
  observed <- str_c(file = list.files("data/orig_rmd"), 
                    ": ",
                    n_wrong_hashes = n_wrong_hashes, 
                    " wrong hashes")
  expected <- str_c(file = list.files("data/orig_rmd"), 
                    ": 0 wrong hashes")
  
  expect_equal(observed, expected)
})

# I expect that each file in `data/orig_table` is in tsv and have columns 
# listed in `expected_columns`

test_that("test column names and separator in data", {
  expected_columns <- c("id", 
                        "lang", 
                        "idiom", 
                        "type", 
                        "genlang_point", 
                        "map", 
                        "feature", 
                        "value1", 
                        "value1_name", 
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

# I expect that all languages in the `lang` field of tables in `data/orig_table`
# match with `data/genlangpoints.csv`

test_that("Those are wrong languages", {
  read_tsv("data/genlangpoints.csv",
           progress = FALSE, 
           show_col_types = FALSE) %>% 
    pull(lang) %>% 
    unique() %>% 
    sort() ->
    expected_langs
  
  observed <- map_chr(list.files("data/orig_table", full.names = TRUE), function(table){
    read_tsv(table) %>% 
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

# GENERATION OF THE RMD ----------------------------------------------------

# remove everything that starts with number and ends with Rmd --------------
file.remove(grep("\\d{1,}_.*.Rmd", list.files(), value = TRUE))


# read our fetures data ----------------------------------------------------
features <- read_tsv("data/features.csv", 
                     progress = FALSE, 
                     show_col_types = FALSE)

# create variable with leading 0 -------------------------------------------
features$id_0 <- sprintf(paste0("%0", nchar(length(features$id)), "d_"), 
                         seq_along(features$id))


# create Rmd names ---------------------------------------------------------
rmd_filenames <- paste0(features$id_0, features$filename, ".Rmd")

# create key for bibtex ----------------------------------------------------
first_authors <- tolower(str_remove(map(str_split(features$author, " "), 2), ","))

# create Rmd files ---------------------------------------------------------
map(seq_along(rmd_filenames), function(i){
  options(ymlthis.rmd_body = "
```{r, include = FALSE}
knitr::opts_chunk$set(echo = FALSE, message = FALSE, comment = '')
```
")
  ymlthis::yml_empty() %>% 
    ymlthis::yml_title(features$title[i]) %>% 
    ymlthis::yml_author(features$author[i]) %>% 
    ymlthis::yml_date(paste0('Last update: ', features$updated_text[i])) %>% 
    ymlthis::yml_citations(bibliography = paste0("./data/orig_bib/", 
                                                 features$filename[i], 
                                                 ".bib"), 
                           csl = './data/apa.csl') %>% 
    ymlthis::use_rmarkdown(path = rmd_filenames[i], 
                           open_doc = FALSE, 
                           quiet = TRUE,
                           include_body = FALSE,
                           body = NULL) 
    write_lines(c(paste0("```{r, child='data/orig_rmd/", features$filename[i], ".Rmd'}"),
                "```",
                "",
                "```{r}",
                "article_citation <- RefManageR::BibEntry(bibtype = 'Incollection', ",
                paste0(" key='", first_authors[i], features$created_date[i], "',"),
                paste0(" title='", features$title[i], "',"),
                paste0(" author='", features$author[i], "',"),
                paste0(" year='", features$created_date[i], "',"),
                " editor= 'Daniel, Michael  and Filatov, Konstantin and Moroz, George and Mukhin, Timofey and Naccarato, Chiara and Verhees, Samira',",
                " publisher='Linguistic Convergence Laboratory, NRU HSE',",
                " address='Moscow',",
                " booktitle= 'Typological Atlas of the languages of Daghestan (TALD)',",
                " url='http://lingconlab.ru/dagatlas')",
                "```",
                "",
                "## {.tabset .tabset-fade .tabset-pills} ",
                "### Plain text",
                "",
                "```{r, results = 'asis'}",
                "print(article_citation, .opts = list(style = 'text'))", # deal with the [1]!
                "```",
                "",
                "### BibTeX",
                "",
                "```{r}",
                "print(article_citation, .opts = list(style = 'Bibtex'))",
                "```",
                "",
                "## References"),
              rmd_filenames[i], append = TRUE)
  
})

# RENDER AND CLEAN ---------------------------------------------------------
rmarkdown::render_site()

file.remove(list.files("docs/data", full.names = TRUE, recursive = TRUE))
file.remove(c(list.files("docs/data", full.names = TRUE), "docs/data"))
