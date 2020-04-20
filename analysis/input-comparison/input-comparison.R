
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
config <- config::get()
# ---- declare-globals --------------------------

# ---- load-data -------------------------------
ds_project_type <- readr::read_csv(config$project_type)

# function to load the questions from each source
input_report_data <- function(path_folder){
  # path_folder = "./data-unshared/raw/CSV-APR_2019"
  # path_folder = "./data-unshared/raw/Sample-APR"

  path_files <- list.files(path_folder, full.names = T) %>% sort()
  dto <- list()
  for(item_i in seq_along(path_files)){
    # item_i <- 1
    item_path <- path_files[item_i]
    item_name <- item_path %>% basename() %>% stringr::str_replace(".csv","") %>% tolower()
    dto[[item_name]] <- read.csv(item_path,  header=TRUE,stringsAsFactors = FALSE)
  }
  return(dto)
}
# load both sources.
allQuestions_old <- input_report_data("./data-unshared/raw/Sample-APR")
# allQuestions_new <- input_report_data("./data-unshared/raw/CSV-APR_2019")

names(all)

allQuestions_old %>% purrr::map(row.names)
# allQuestions_new %>% purrr::map(row.names)
# dto <- allQuestions_new

# allQuestions_old[["q5a"]] %>% row.names()
# ---- new-input ------------------
base::source("./manipulation/question-greeter.R")
allQuestions_new <- dto$data

# ----- -------
# allQuestions_new[["q7a"]] %>% glimpse()
# allQuestions_old[["q7a"]] %>% glimpse()

# ---- -------

allQuestions_new[["q4a"]] %>% glimpse()
allQuestions_old[["q4a"]] %>% glimpse()

allQuestions_new[["q4a"]] %>% View()
allQuestions_old[["q4a"]] %>% View()




projType <- as.numeric(allQuestions_old[["q4a"]][4,2])
totalPersons <- allQuestions_old[["q7a"]][5,2]
stayers <- allQuestions_old[["q5a"]][8,1]
excluded <- allQuestions_old[["q23a"]][41,2] + allQuestions_old[["q23b"]][41,2]
goodDest <- allQuestions_old[["q23a"]][13,2]+allQuestions_old[["q23b"]][13,2]
posDestPer <- sprintf("%1.2f%%",100*goodDest/(allQuestions_old[["q23a"]][39,2]+allQuestions_old[["q23b"]][39,2]-excluded))
posDestStayPerPH <-sprintf("%1.2f%%",100*(goodDest+stayers)/(totalPersons-excluded))
if(projType==3)
  return(paste("Clients staying in PSH or exiting to permanent destinations:",posDestStayPerPH))
paste("Clients exiting to permanent destinations:",posDestPer)

projType <- as.numeric(allQuestions_new[["q4a"]][4,2])




# ---- --------
# Adults gaining or maintaining earned income
# Hold on this item, awaiting clarification from Tino
allQuestions_old[["q19a3"]] %>% View()

paste(
  "Adults gaining or maintaining earned income:"
  , sprintf("%1.0f%%",100*rowSums(allQuestions_old[["q19a3"]][1:5,4:6])[1]/allQuestions_old[["q19a3"]][1,8])
)

names(allQuestions_old[["q19a3"]][4:6])
d <- allQuestions_old[["q19a3"]][1:5, 4:6]
d1 <- rowSums(d)[1]

compute_over_columns <- c(
"Retained.Income.Category.and.Same...at.Annual.Assessment.Exit.as.at.Start"
,"Retained.Income.Category.and.Increased...at.Annual.Assessment.Exit"
,"Did.Not.Have.the.Income.Category.at.Start.and.Gained.the.Income.Category.at.Annual.Assessment.Exit"
)
comput_in_row <- '"Number of Adults with Earned Income (ie.e, Employment Income)"'

x2 <- allQuestions_old[["q19a3"]] %>%
  dplyr::filter(
    'X.Income.Change.by.Income.Category..Universe..Adults.with.Income.Information.at.Start.and.Annual.Assessment.Exit..' ==
      '"Number of Adults with Earned Income (ie.e, Employment Income)"'
  )

x <- allQuestions_old[["q19a3"]][1:5,4:6]
x1 <- rowSums(allQuestions_old[["q19a3"]][1:5,4:6])


allQuestions_new[["q19a2"]] %>% View()


# ----- --------------------
# Clients Entering from Homeless Situations

adults <- allQuestions_old[["q7a"]][1,2]
children <- allQuestions_old[["q7a"]][2,2]
childHoH <- allQuestions_old[["q5a"]][15,1] #%>% View()
paste("Clients Entering from Homeless Situations:",sprintf("%1.0f%%",100*allQuestions_old[["q15"]][7,2]/(adults+childHoH)))

# adults <- allQuestions_new[["q7a"]][1,2]
adults <- allQuestions_new[["q7a"]] %>%
  dplyr::filter(X == "Adults") %>%
  dplyr::pull(Total)
# children <- allQuestions_new[["q7a"]][2,2]
children <- allQuestions_new[["q7a"]] %>% ### THIS VALUE IS NOT USED IN CALCULATION. CHECK WITH TINO
    dplyr::filter(X == "Children") %>%
    dplyr::pull(Total)
# childHoH <- allQuestions_new[["q5a"]][15,2]
childHoH <- allQuestions_new[["q5a"]] %>%
  dplyr::filter(V1 == "Number of Child and Unknown-Age Heads of Household") %>%
  dplyr::pull(V2)
subsection_total <- allQuestions_new[["q15"]] %>%
  dplyr::filter(category == "Homeless Situations", X == "Subtotal") %>%
  dplyr::pull(Total)

paste(
  "Clients Entering from Homeless Situations:"
  ,sprintf("%1.0f%%",100*subsection_total/(adults+childHoH) )
)



#
# inspect individual questions
focal_name <- "q15"
print(focal_name)
View(allQuestions_old[[focal_name]]); View(allQuestions_new[[focal_name]])

items_header_false <- c("q5a")

# ---- q4a -------------------
# Project Name
allQuestions_old[["q4a"]][2,2]
allQuestions_new[["q4a"]][1,3]

# Project Type

allQuestions_old[["q4a"]][4,2]
allQuestions_new[["q4a"]][1,"HMIS.Project.Type"]

allQuestions_old[["q15"]][7,2]
allQuestions_new[["q15"]][7,2]

d1 <-allQuestions_new[["q15"]] %>% select(X) %>% mutate(Xold = X)
d2 <- allQuestions_old[["q15"]] %>% select(X)

setdiff(allQuestions_new[["q15"]] %>% select(X),allQuestions_old[["q15"]] %>% select(X))

d3 <- dplyr::left_join(d2, d1)
dplyr::left_join(allQuestions_old[["q15"]] %>% select(X))







# ---- q11a -------------------

  plot_ly(x=c("  Under 5"," 5-12","13-17","18-24","25-34","35-44","45-54","55-61","62+"),
          # y=allQuestions_old[["q11"]]$Total[1:9],
          y=allQuestions_new[["q11"]]$Total[1:9],
          name="Age Distribution",type='bar')%>%
    layout(xaxis = list(title = "Age Range"),
           yaxis = list(title ="Client Count"))

# ---- q12a -------------------
plot_ly(x=c(" White", " Black", "Asian", "Am. Indian","NHPI","Multiple","DK/R","Missing"),
        # y=allQuestions_old[["q12a"]]$Total[1:8],
        y=allQuestions_new[["q12a"]]$Total[1:8],
        name='Race ',type='bar')%>%
  layout(xaxis = list(title = "Race"),
         yaxis = list(title ="Client Count"))

# ---- q12b -------------------
plot_ly(x=c(" Non-Latino"," Hispanic/Latino","DK/R","Missing"),
        # y=allQuestions_old[["q12b"]]$Total[1:4],
        y=allQuestions_new[["q12b"]]$Total[1:4],
        name="Ethnicity Distribution",type='bar')%>%
  layout(xaxis = list(title = "Ethnicity"),
         yaxis = list(title ="Client Count"))


# ---- q-list -------------
  # Comment out the questions that are breaking input, may need to add back in
  # with an error checking function
  # q19a3=read.csv("Q19a3.csv",header=TRUE,stringsAsFactors = FALSE),
  # q23a=read.csv("Q23a.csv",header=TRUE,stringsAsFactors = FALSE),
  # q23b=read.csv("Q23b.csv",header=TRUE,stringsAsFactors = FALSE),
folder_path <- "./data-unshared/raw/Sample-APR/"

  q4a=read.csv   (paste0(folder_path,"Q4a.csv"),  header=TRUE,stringsAsFactors = FALSE)
  # q4a=read.csv   (paste0(folder_path,"Q4a.csv"),  header=TRUE,stringsAsFactors = FALSE)
  q5a=read.csv   (paste0(folder_path,"Q5a.csv"),  header=TRUE,stringsAsFactors = FALSE)
  q6a=read.csv   (paste0(folder_path,"Q6a.csv"),  header=TRUE,stringsAsFactors = FALSE)
  q6b=read.csv   (paste0(folder_path,"Q6b.csv"),  header=TRUE,stringsAsFactors = FALSE)
  q6c=read.csv   (paste0(folder_path,"Q6c.csv"),  header=TRUE,stringsAsFactors = FALSE)
  q6d=read.csv   (paste0(folder_path,"Q6d.csv"),  header=TRUE,stringsAsFactors = FALSE)
  q6e=read.csv   (paste0(folder_path,"Q6e.csv"),  header=TRUE,stringsAsFactors = FALSE)
  q6f=read.csv   (paste0(folder_path,"Q6f.csv"),  header=TRUE,stringsAsFactors = FALSE)
  q7a=read.csv   (paste0(folder_path,"Q7a.csv"),  header=TRUE,stringsAsFactors = FALSE)
  q7b=read.csv   (paste0(folder_path,"Q7b.csv"),  header=TRUE,stringsAsFactors = FALSE)
  q8a=read.csv   (paste0(folder_path,"Q8a.csv"),  header=TRUE,stringsAsFactors = FALSE)
  q8b=read.csv   (paste0(folder_path,"Q8b.csv"),  header=TRUE,stringsAsFactors = FALSE)
  q9a=read.csv   (paste0(folder_path,"Q9a.csv"),  header=TRUE,stringsAsFactors = FALSE)
  q9b=read.csv   (paste0(folder_path,"Q9b.csv"),  header=TRUE,stringsAsFactors = FALSE)
  q10a=read.csv  (paste0(folder_path,"Q10a.csv"), header=TRUE,stringsAsFactors = FALSE)
  q10b=read.csv  (paste0(folder_path,"Q10b.csv"), header=TRUE,stringsAsFactors = FALSE)
  q10c=read.csv  (paste0(folder_path,"Q10c.csv"), header=TRUE,stringsAsFactors = FALSE)
  q11=read.csv   (paste0(folder_path,"Q11.csv"),  header=TRUE,stringsAsFactors = FALSE)
  q12a=read.csv  (paste0(folder_path,"Q12a.csv"), header=TRUE,stringsAsFactors = FALSE)
  q12b=read.csv  (paste0(folder_path,"Q12b.csv"), header=TRUE,stringsAsFactors = FALSE)
  q13a1=read.csv (paste0(folder_path,"Q13a1.csv"),header=TRUE,stringsAsFactors = FALSE)
  q13a2=read.csv (paste0(folder_path,"Q13a2.csv"),header=TRUE,stringsAsFactors = FALSE)
  q13b1=read.csv (paste0(folder_path,"Q13b1.csv"),header=TRUE,stringsAsFactors = FALSE)
  q13b2=read.csv (paste0(folder_path,"Q13b2.csv"),header=TRUE,stringsAsFactors = FALSE)
  q13c1=read.csv (paste0(folder_path,"Q13c1.csv"),header=TRUE,stringsAsFactors = FALSE)
  q13c2=read.csv (paste0(folder_path,"Q13c2.csv"),header=TRUE,stringsAsFactors = FALSE)
  q14a=read.csv  (paste0(folder_path,"Q14a.csv"), header=TRUE,stringsAsFactors = FALSE)
  q14b=read.csv  (paste0(folder_path,"Q14b.csv"), header=TRUE,stringsAsFactors = FALSE)
  q15=read.csv   (paste0(folder_path,"Q15.csv"),  header=TRUE,stringsAsFactors = FALSE)
  q16=read.csv   (paste0(folder_path,"Q16.csv"),  header=TRUE,stringsAsFactors = FALSE)
  q17=read.csv   (paste0(folder_path,"Q17.csv"),  header=TRUE,stringsAsFactors = FALSE)
  q18=read.csv   (paste0(folder_path,"Q18.csv"),  header=TRUE,stringsAsFactors = FALSE)
  q19a1=read.csv (paste0(folder_path,"Q19a1.csv"),header=TRUE,stringsAsFactors = FALSE)
  q19a2=read.csv (paste0(folder_path,"Q19a2.csv"),header=TRUE,stringsAsFactors = FALSE)
  q19a3=read.csv (paste0(folder_path,"Q19a3.csv"),header=TRUE,stringsAsFactors = FALSE) # missing in the new
  q20a=read.csv  (paste0(folder_path,"Q20a.csv"), header=TRUE,stringsAsFactors = FALSE)
  q20b=read.csv  (paste0(folder_path,"Q20b.csv"), header=TRUE,stringsAsFactors = FALSE)
  q21=read.csv   (paste0(folder_path,"Q21.csv"),  header=TRUE,stringsAsFactors = FALSE)
  q22a1=read.csv (paste0(folder_path,"Q22a1.csv"),header=TRUE,stringsAsFactors = FALSE)
  q22b=read.csv  (paste0(folder_path,"Q22b.csv"), header=TRUE,stringsAsFactors = FALSE)
  q23a=read.csv  (paste0(folder_path,"Q23a.csv"), header=TRUE,stringsAsFactors = FALSE)# missing in the new
  q23b=read.csv  (paste0(folder_path,"Q23b.csv"), header=TRUE,stringsAsFactors = FALSE)# missing in the new
  q25a=read.csv  (paste0(folder_path,"Q25a.csv"), header=TRUE,stringsAsFactors = FALSE)
  q25b=read.csv  (paste0(folder_path,"Q25b.csv"), header=TRUE,stringsAsFactors = FALSE)
  q25c=read.csv  (paste0(folder_path,"Q25c.csv"), header=TRUE,stringsAsFactors = FALSE)
  q25d=read.csv  (paste0(folder_path,"Q25d.csv"), header=TRUE,stringsAsFactors = FALSE)
  q25e=read.csv  (paste0(folder_path,"Q25e.csv"), header=TRUE,stringsAsFactors = FALSE)
  q25f=read.csv  (paste0(folder_path,"Q25f.csv"), header=TRUE,stringsAsFactors = FALSE)
  q25g=read.csv  (paste0(folder_path,"Q25g.csv"), header=TRUE,stringsAsFactors = FALSE)
  q25h=read.csv  (paste0(folder_path,"Q25h.csv"), header=TRUE,stringsAsFactors = FALSE)
  q25i=read.csv  (paste0(folder_path,"Q25i.csv"), header=TRUE,stringsAsFactors = FALSE)
  q26a=read.csv  (paste0(folder_path,"Q26a.csv"), header=TRUE,stringsAsFactors = FALSE)
  q26b=read.csv  (paste0(folder_path,"Q26b.csv"), header=TRUE,stringsAsFactors = FALSE)
  q26c=read.csv  (paste0(folder_path,"Q26c.csv"), header=TRUE,stringsAsFactors = FALSE)
  q26d=read.csv  (paste0(folder_path,"Q26d.csv"), header=TRUE,stringsAsFactors = FALSE)
  q26e=read.csv  (paste0(folder_path,"Q26e.csv"), header=TRUE,stringsAsFactors = FALSE)
  q26f=read.csv  (paste0(folder_path,"Q26f.csv"), header=TRUE,stringsAsFactors = FALSE)
  q26g=read.csv  (paste0(folder_path,"Q26g.csv"), header=TRUE,stringsAsFactors = FALSE)
  q26h=read.csv  (paste0(folder_path,"Q26h.csv"), header=TRUE,stringsAsFactors = FALSE)
  q27a=read.csv  (paste0(folder_path,"Q27a.csv"), header=TRUE,stringsAsFactors = FALSE)
  q27b=read.csv  (paste0(folder_path,"Q27b.csv"), header=TRUE,stringsAsFactors = FALSE)
  q27c=read.csv  (paste0(folder_path,"Q27c.csv"), header=TRUE,stringsAsFactors = FALSE)
  q27d=read.csv  (paste0(folder_path,"Q27d.csv"), header=TRUE,stringsAsFactors = FALSE)
  q27e=read.csv  (paste0(folder_path,"Q27e.csv"), header=TRUE,stringsAsFactors = FALSE)
  q27f=read.csv  (paste0(folder_path,"Q27f.csv"), header=TRUE,stringsAsFactors = FALSE)
