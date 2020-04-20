
# rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
# This is not called by knitr, because it's above the first chunk.
# cat("\f") # clear console when working in RStudio

# ---- load-packages --------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
# library(dplyr)    # data wrangling
# library(ggplot2)  # graphs
# library(tidyr)    # data tidying
# library(plotly)

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
# allQuestions_old <- list_input_files("./data-unshared/raw/Sample-APR")
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

# ---- replace-quotes -------------

replace_quotes <- function(d){
  d_out <- d %>%
  dplyr::mutate_if(
    is.character,
    ~stringr::str_replace_all(
      .,
      c(
        '"'  = ""
        ,"'" = ""
      )
    )
  ) %>%
  dplyr::mutate_if(
    is.character,
    ~trimws(.)
  )
  return(d_out)
}

lsd <- dto
# question_names <- names(dto)
qnum <- 19
qname <- dto[["path"]][qnum] %>% names()

exceptions <- c(15,29,38,50,52)
# exceptions <- c(57)
question_position <- seq_along(dto[["path"]] )
without_exceptions <- setdiff(question_position, exceptions)


dto[["path"]][15] %>% names()
dto[["path"]][29] %>% names()
dto[["path"]][38] %>% names()
dto[["path"]][50] %>% names()
dto[["path"]][52] %>% names()
dto[["path"]][57] %>% names()

for(q_name in names(dto[["path"]]) ){

  if(q_name != "q5a" ){
    # i <- 1
    dto[["data"]][[q_name]] <- read.csv(dto[["path"]][q_name] ,header = TRUE, stringsAsFactors = F) %>%
    replace_quotes()
  }

  if(q_name == "q5a" ){
    dto[["data"]][[q_name]] <- read.csv(dto[["path"]][q_name] ,header = FALSE, stringsAsFactors = F) %>%
    replace_quotes()
  }

  if(q_name == "q15"){
    dto[["data"]][[q_name]] <- dto[["data"]][[q_name]] %>%
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
  }
  if(q_name %in% c("q23c", "q25i","q27d","q27f" ) ){
    dto[["data"]][[q_name]] <- dto[["data"]][[q_name]] %>%
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
  }

}

# dto[["data"]][["q5a"]] %>% View()
# dto[["data"]][["q15"]] %>% View()
# dto[["data"]][["q23c"]] %>% View()
# dto[["data"]][["q25i"]] %>% View()
# dto[["data"]][["q27d"]] %>% View()
# dto[["data"]][["q27f"]] %>% View()
allQuestions_new <- dto[["data"]]

