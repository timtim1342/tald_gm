library(tidyverse)

# RUN TESTS ----------------------------------------------------------------
testthat::test_dir("tests")

# GENERATION OF THE RMD ----------------------------------------------------

# remove everything that starts with number and ends with Rmd --------------
file.remove(grep("\\d{1,}_.*.Rmd", list.files(), value = TRUE))

# convert .bib.tsv to .bib -------------------------------------------------

map(list.files("data/orig_bib_tsv", full.names = TRUE), function(bib_tsv){
  read_tsv(bib_tsv, progress = FALSE, show_col_types = FALSE) %>% 
    bib2df::df2bib(bib_tsv %>% 
                     str_remove_all("[_\\.]tsv") %>% 
                     str_replace("_bib$", "\\.bib"))
  })

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

# create orig_rmd/..._map.Rmd files ----------------------------------------------------

map(rmd_filenames[str_detect(rmd_filenames, "_map.Rmd")], function(i){
  write_lines(
    c("
##

```{r}
library(tidyverse)
",
str_c('df <- read_tsv("../orig_table/', 
      str_remove(str_remove(i, "_map.Rmd"), "\\d{1,}_"),
      '.tsv")'),
"
bib <- ReadBib(file = '../bibliography.bib')
df %>% 
  select(lang, idiom, source, page) %>% 
  rename(language = lang) %>% 
  rowwise() %>% 
  mutate(source = Cite(bib, source)) %>% 
  DT::datatable()
```
"),
    file = str_c("data/orig_rmd/", str_remove(i, "\\d{1,}_"))
  )
})

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
    ymlthis::yml_date(paste0('Last update: ', 
                             ifelse(str_detect(rmd_filenames[i], "_map.Rmd"), 
                                    features$updated_map[i], 
                                    features$updated_text[i]))) %>% 
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
    str_c("```{r, child='data/orig_rmd/", features$filename[i], ".Rmd'}"),
    "```",
    "",
    # add refferences
    "## References",
    "",
    "```{r, results='asis'}",
    ifelse(str_detect(rmd_filenames[i], "_map.Rmd"), 
           "PrintBibliography(bib)",
           ""),
    "```",
    ""),
    rmd_filenames[i], append = TRUE)
})

# RENDER AND CLEAN ---------------------------------------------------------
rmarkdown::render_site()

file.remove(list.files("docs/data", full.names = TRUE, recursive = TRUE))
file.remove(c(list.files("docs/data", full.names = TRUE), "docs/data"))
