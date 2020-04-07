
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-packages --------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
library(dplyr)    # data wrangling
library(ggplot2)  # graphs
library(tidyr)    # data tidying
library(plotly)

# ---- load-sources ---------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.
# source("./scripts/common-functions.R") # used in multiple reports
# source("./scripts/graphing/graph-presets.R") # fonts, colors, themes

# ---- declare-globals --------------------------

# ---- load-data -------------------------------
# function to load the questions from each source
list_input_files <- function(path_folder){
  # path_folder = "./data-unshared/raw/CSV-APR_2019"
  # path_folder = "./data-unshared/raw/Sample-APR"

  path_files <- list.files(path_folder, full.names = T) %>% sort()
  dto <- list()
  for(item_i in seq_along(path_files)){
    # item_i <- 1
    item_path <- path_files[item_i]
    item_name <- item_path %>% basename() %>% stringr::str_replace(".csv","") %>% tolower()
    dto[["path"]][[item_name]] <- item_path
    # dto[[item_name]] <- read.csv(item_path,  header=TRUE,stringsAsFactors = FALSE)
  }
  return(dto)
}
# load both sources.
allQuestions_old <- list_input_files("./data-unshared/raw/Sample-APR")
allQuestions_new <- list_input_files("./data-unshared/raw/CSV-APR_2019")
dto <- allQuestions_new
# ---- ---------------------
#
# # inspect individual questions
# focal_name <- "q15"
# print(focal_name)
# View(allQuestions_old[[focal_name]]); View(allQuestions_new[[focal_name]])

# question_names <- names(allQuestions_new)
# # ---- q4a -------------------
# path_files <- list.files("./data-unshared/raw/CSV-APR_2019", full.names = T)
# question_names[1]
# d <- read
#
# # ---- --------------------------
lsd <- dto
# question_names <- names(dto)
qnum <- 19
qname <- dto[["path"]][qnum] %>% names()

d <- read.csv(dto[["path"]][1] ,header = TRUE, stringsAsFactors = F)
d <- read.csv(dto[["path"]][2] ,header = TRUE, stringsAsFactors = F)
d <- read.csv(dto[["path"]][3] ,header = TRUE, stringsAsFactors = F)
d <- read.csv(dto[["path"]][4] ,header = TRUE, stringsAsFactors = F)
d <- read.csv(dto[["path"]][5] ,header = TRUE, stringsAsFactors = F)
d <- read.csv(dto[["path"]][6] ,header = TRUE, stringsAsFactors = F)
d <- read.csv(dto[["path"]][7] ,header = TRUE, stringsAsFactors = F)
d <- read.csv(dto[["path"]][8] ,header = TRUE, stringsAsFactors = F)
d <- read.csv(dto[["path"]][9] ,header = TRUE, stringsAsFactors = F)
d <- read.csv(dto[["path"]][10],header = TRUE, stringsAsFactors = F)
d <- read.csv(dto[["path"]][11],header = TRUE, stringsAsFactors = F)
d <- read.csv(dto[["path"]][12],header = TRUE, stringsAsFactors = F)
d <- read.csv(dto[["path"]][13],header = TRUE, stringsAsFactors = F)
d <- read.csv(dto[["path"]][14],header = TRUE, stringsAsFactors = F)
d <- read.csv(dto[["path"]][15],header = TRUE, stringsAsFactors = F)#%>% tibble::as_tibble()
d <- d %>%
  dplyr::mutate(
    X = gsub('"','',X)
    ,category = ifelse(is.na(Total),X,NA)
  ) %>%
  tidyr::fill(category) %>%
  dplyr::mutate(
    category = ifelse(X == "Total", X, category)
  ) %>%
  dplyr::filter(!is.na(Total)) %>%
  dplyr::select(category,X, dplyr::everything())
#
d <- read.csv(dto[["path"]][16], stringsAsFactors = F)
d <- read.csv(dto[["path"]][17], stringsAsFactors = F)
d <- read.csv(dto[["path"]][18], stringsAsFactors = F)
d <- read.csv(dto[["path"]][19], stringsAsFactors = F)
d <- read.csv(dto[["path"]][20], stringsAsFactors = F)
d <- read.csv(dto[["path"]][21], stringsAsFactors = F)
d <- read.csv(dto[["path"]][22], stringsAsFactors = F)
d <- read.csv(dto[["path"]][23], stringsAsFactors = F)
d <- read.csv(dto[["path"]][24], stringsAsFactors = F)
d <- read.csv(dto[["path"]][25], stringsAsFactors = F)
d <- read.csv(dto[["path"]][26], stringsAsFactors = F)
d <- read.csv(dto[["path"]][27], stringsAsFactors = F)
d <- read.csv(dto[["path"]][28], stringsAsFactors = F)
d <- read.csv(dto[["path"]][29], stringsAsFactors = F)
d <- d %>%
  dplyr::mutate(
    X = gsub('"','',X)
    ,category = ifelse(is.na(Total),X,NA)
  ) %>%
  tidyr::fill(category) %>%
  dplyr::mutate(
    category = ifelse(X %in% c("Total","Total persons exiting to positive housing destinations",
                               "Total persons whose destinations excluded them from the calculation",
                               "Percentage"), X, category)
  ) %>%
  dplyr::filter(!is.na(Total)) %>%
  dplyr::select(category,X, dplyr::everything())
d <- read.csv(dto[["path"]][30], stringsAsFactors = F)
d <- read.csv(dto[["path"]][31], stringsAsFactors = F)
d <- read.csv(dto[["path"]][32], stringsAsFactors = F)
d <- read.csv(dto[["path"]][33], stringsAsFactors = F)
d <- read.csv(dto[["path"]][34], stringsAsFactors = F)
d <- read.csv(dto[["path"]][35], stringsAsFactors = F)
d <- read.csv(dto[["path"]][36], stringsAsFactors = F)
d <- read.csv(dto[["path"]][37], stringsAsFactors = F)
d <- read.csv(dto[["path"]][38], stringsAsFactors = F)
d <- d %>%
  dplyr::mutate(
    X = gsub('"','',X)
    ,category = ifelse(is.na(Total),X,NA)
  ) %>%
  tidyr::fill(category) %>%
  dplyr::mutate(
    category = ifelse(X %in% c("Total","Total persons exiting to positive housing destinations",
                               "Total persons whose destinations excluded them from the calculation",
                               "Percentage"), X, category)
  ) %>%
  dplyr::filter(!is.na(Total)) %>%
  dplyr::select(category,X, dplyr::everything())
d <- read.csv(dto[["path"]][39], stringsAsFactors = F)
d <- read.csv(dto[["path"]][40], stringsAsFactors = F)
d <- read.csv(dto[["path"]][41], stringsAsFactors = F)
d <- read.csv(dto[["path"]][42], stringsAsFactors = F)
d <- read.csv(dto[["path"]][43], stringsAsFactors = F)
d <- read.csv(dto[["path"]][44], stringsAsFactors = F)
d <- read.csv(dto[["path"]][45], stringsAsFactors = F)
d <- read.csv(dto[["path"]][46], stringsAsFactors = F)
d <- read.csv(dto[["path"]][47], stringsAsFactors = F)
d <- read.csv(dto[["path"]][48], stringsAsFactors = F)
d <- read.csv(dto[["path"]][49], stringsAsFactors = F)
d <- read.csv(dto[["path"]][50], stringsAsFactors = F)
d <- d %>%
  dplyr::mutate(
    X = gsub('"','',X)
    ,category = ifelse(is.na(Total),X,NA)
  ) %>%
  tidyr::fill(category) %>%
  dplyr::mutate(
    category = ifelse(X %in% c("Total","Total persons exiting to positive housing destinations",
                               "Total persons whose destinations excluded them from the calculation",
                               "Percentage"), X, category)
  ) %>%
  dplyr::filter(!is.na(Total)) %>%
  dplyr::select(category,X, dplyr::everything())
d <- read.csv(dto[["path"]][51], stringsAsFactors = F)
d <- read.csv(dto[["path"]][52], stringsAsFactors = F)
d <- d %>%
  dplyr::mutate(
    X = gsub('"','',X)
    ,category = ifelse(is.na(Total),X,NA)
  ) %>%
  tidyr::fill(category) %>%
  dplyr::mutate(
    category = ifelse(X %in% c("Total","Total persons exiting to positive housing destinations",
                               "Total persons whose destinations excluded them from the calculation",
                               "Percentage"), X, category)
  ) %>%
  dplyr::filter(!is.na(Total)) %>%
  dplyr::select(category,X, dplyr::everything())
d <- read.csv(dto[["path"]][53], stringsAsFactors = F)
d <- read.csv(dto[["path"]][54], stringsAsFactors = F)
d <- read.csv(dto[["path"]][55], stringsAsFactors = F)
d <- read.csv(dto[["path"]][56], stringsAsFactors = F)
d <- read.csv(dto[["path"]][57],header = F, stringsAsFactors = F)

d <- read.csv(dto[["path"]][58], stringsAsFactors = F)
d <- read.csv(dto[["path"]][59], stringsAsFactors = F)
d <- read.csv(dto[["path"]][60], stringsAsFactors = F)
d <- read.csv(dto[["path"]][61], stringsAsFactors = F)
d <- read.csv(dto[["path"]][62], stringsAsFactors = F)
d <- read.csv(dto[["path"]][63], stringsAsFactors = F)
d <- read.csv(dto[["path"]][64], stringsAsFactors = F)
d <- read.csv(dto[["path"]][65], stringsAsFactors = F)
d <- read.csv(dto[["path"]][66], stringsAsFactors = F)
d <- read.csv(dto[["path"]][67], stringsAsFactors = F)
d <- read.csv(dto[["path"]][68], stringsAsFactors = F)
d <- read.csv(dto[["path"]][69], stringsAsFactors = F)





