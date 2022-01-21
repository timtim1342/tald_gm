library(tidyverse)

# RUN TESTS ----------------------------------------------------------------
testthat::test_dir("tests")

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

features %>% 
  mutate(filename = str_c(filename, "_map")) %>% 
  bind_rows(features) ->
  features

# create Rmd names ---------------------------------------------------------
rmd_filenames <- c(str_c(features$id_0, features$filename, ".Rmd"))

# create key for bibtex ----------------------------------------------------
first_authors <- tolower(str_remove(map(str_split(features$author, " "), 2), ","))

# create Rmd files ---------------------------------------------------------
options(ymlthis.rmd_body = "
```{r, include = FALSE}
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE, comment = '')
library(lingglosses)
```
")

map(seq_along(rmd_filenames), function(i){
  ymlthis::yml_empty() %>% 
    ymlthis::yml_title(ifelse(str_detect(rmd_filenames[i], "_map.Rmd"), 
                              str_c(features$title[i], " (Maps & Data)"), 
                              features$title[i])) %>% 
    ymlthis::yml_author(features$author[i]) %>% 
    ymlthis::yml_date(paste0('Last update: ', features$updated_text[i])) %>% 
    ymlthis::yml_citations(bibliography = paste0("./data/orig_bib/", 
                                                 str_remove(features$filename[i], "_map"), 
                                                 ".bib"), 
                           csl = './data/apa.csl') %>% 
    ymlthis::use_rmarkdown(path = rmd_filenames[i], 
                           open_doc = FALSE, 
                           quiet = TRUE,
                           include_body = FALSE,
                           body = NULL) 
  write_lines(c(
    # add link to map/chapter page
    paste0("See [",
           ifelse(str_detect(rmd_filenames[i], "_map.Rmd"), 
                  "the chapter text", 
                  "data and maps"),
           "](", 
           str_remove(rmd_filenames[i], "(_map)?.Rmd"), 
           ifelse(str_detect(rmd_filenames[i], "_map.Rmd"), 
                  ".html)",
                  "_map.html).")),
    "",
    # create and add citation
    "```{r}",
    "library(RefManageR)",
    "BibOptions(check.entries = FALSE, style = 'text', bib.style = 'authoryear')",
    "article_citation <- BibEntry(bibtype = 'Incollection', ",
    paste0(" key='", first_authors[i], features$created_date[i], "',"),
    paste0(" title='", features$title[i], "',"),
    paste0(" author='", str_replace(features$author[i], ",", " and"), "',"),
    paste0(" year='", features$created_date[i], "',"),
    " editor= 'Daniel, Michael  and Filatov, Konstantin and Moroz, George and Mukhin, Timofey and Naccarato, Chiara and Verhees, Samira',",
    " publisher='Linguistic Convergence Laboratory, NRU HSE',",
    " address='Moscow',",
    " booktitle= 'Typological Atlas of the languages of Daghestan (TALD)',",
    " url='http://lingconlab.ru/dagatlas')",
    "```",
    "",
    "## {.tabset .tabset-fade .tabset-pills} ",
    "",
    "### Plain text",
    "```{r, results = 'asis'}",
    "print(article_citation, .opts = list(style = 'text'))",
    "```",
    "",
    "### BibTeX",
    "",
    "```{r}",
    "print(article_citation, .opts = list(style = 'Bibtex'))",
    "```",
    # add text of the Rmd
    "",
    ifelse(str_detect(rmd_filenames[i], "_map.Rmd"), 
           "```{r}",
           str_c("```{r, child='data/orig_rmd/", features$filename[i], ".Rmd'}")),
    "```",
    "",
    # do not print "## List of glosses", if there is no glosses
    "```{r, results='asis'}",
    "gloss_file_name <- getOption('lingglosses.glosses_list')",
    "if(file.exists(gloss_file_name) && file.size(gloss_file_name) > 0){cat('## List of glosses')}",
    "```",
    "",
    # make a gloss list
    "```{r}",
    "make_gloss_list()",
    "```",
    "",
    # add refferences
    "## References"),
    rmd_filenames[i], append = TRUE)
})

# RENDER AND CLEAN ---------------------------------------------------------
rmarkdown::render_site()

file.remove(list.files("docs/data", full.names = TRUE, recursive = TRUE))
file.remove(c(list.files("docs/data", full.names = TRUE), "docs/data"))
