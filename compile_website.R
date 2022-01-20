library(tidyverse)

# RUN TESTS ----------------------------------------------------------------
source("tests.R")

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
