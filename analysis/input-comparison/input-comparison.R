
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

dto <- readr::read_rds("./data-unshared/derived/sample-reports.rds")
rep_old <- dto$old
rep_new <- dto$new

rep_old %>% purrr::map(row.names)
# ---- new-input ------------------

# ---- output-names ----------
# if(is.null(input$aprZip))
#   return("Select an APR to Begin")
# as.character(allQuestions()[["q4a"]][2,2])
as.character(rep_old[["q4a"]][2,2])
as.character(rep_new[["q4a"]][1,3])

project_name <- rep_new[["q4a"]] %>% dplyr::pull("Project.Name")

# ---- output-type ---------------
# if(is.null(input$aprZip))
#   return(NULL)
# projType <- c("Emergency Shelter","Transitional Housing","PH - Permanent Supportive Housing","Street Outreach","RETIRED","Services Only","Other","Safe Haven","PH - Housing Only","PH - Housing with Services","Day Shelter","Homelessness Prevention","PH - Rapid Re-Housing","Coordinated Assessment")
# index <- as.numeric(allQuestions()[["q4a"]][1,5])
# projType[index]
ds_project_type <- readr::read_csv(config$project_type)
project_type_id_input <-  rep_new[["q4a"]] %>% dplyr::pull("HMIS.Project.Type")
project_type <- ds_project_type %>%
  dplyr::filter(project_type_id == project_type_id_input) %>%
  dplyr::pull(project_type_label)
print(project_type)

# ---- output-clientCounts ------
# totalPersons <- allQuestions()[["q7a"]][5,2]
# paste("Total Clients:",totalPersons)

totalPersons <- rep_old[["q7a"]][5,2]
paste("Total Clients:",totalPersons)

totalPersons <- rep_new[["q7a"]][5,2]
paste("Total Clients:",totalPersons)

totalPerson <- rep_new[["q7a"]] %>%
  dplyr::filter(X == "Total") %>%
  dplyr::pull("Total")
paste("Total Clients:",totalPersons)


# ---- homelessPer ---------------
# Clients Entering from Homeless Situations

# adults   <- allQuestions()[["q7a"]][1,2]
# children <- allQuestions()[["q7a"]][2,2]
# childHoH <- allQuestions()[["q5a"]][15,2]
# # (allQuestions()[["q15"]][7,2])
# paste("Clients Entering from Homeless Situations:",sprintf("%1.0f%%",100*allQuestions()[["q15"]][7,2]/(adults+childHoH)))

adults   <- rep_old[["q7a"]][1,2]
children <- rep_old[["q7a"]][2,2]
childHoH <- rep_old[["q5a"]][15,1]
# (allQuestions()[["q15"]][7,2])
paste("Clients Entering from Homeless Situations:",sprintf("%1.0f%%",100*rep_old[["q15"]][7,2]/(adults+childHoH)))


# adults <- allQuestions_new[["q7a"]][1,2]
adults <- rep_new[["q7a"]] %>%
  dplyr::filter(X == "Adults") %>%
  dplyr::pull("Total")
# children <- allQuestions_new[["q7a"]][2,2]
children <- rep_new[["q7a"]] %>% ### THIS VALUE IS NOT USED IN CALCULATION. CHECK WITH TINO
  dplyr::filter(X == "Children") %>%
  dplyr::pull("Total")
# childHoH <- allQuestions_new[["q5a"]][15,2]
childHoH <- rep_new[["q5a"]] %>%
  dplyr::filter(V1 == "Number of Child and Unknown-Age Heads of Household") %>%
  dplyr::pull(V2)
subsection_total <- rep_new[["q15"]] %>%
  dplyr::filter(category == "Homeless Situations", X == "Subtotal") %>%
  dplyr::pull("Total")

paste(
  "Clients Entering from Homeless Situations:"
  ,sprintf("%1.0f%%",100*subsection_total/( adults + childHoH) )
)

# ----- incSummary -------------

# ----- incSummary2 -------------


# ----- destPos2 -------------
# projType <- as.numeric(allQuestions()[["q4a"]][4,2])
# totalPersons <- allQuestions()[["q7a"]][5,2]
# stayers <- allQuestions()[["q5a"]][8,1]
# excluded <- allQuestions()[["q23a"]][41,2] + allQuestions()[["q23b"]][41,2]
# goodDest <- allQuestions()[["q23a"]][13,2]+allQuestions()[["q23b"]][13,2]
# posDestPer <- sprintf("%1.2f%%",100*goodDest/(allQuestions()[["q23a"]][39,2]+allQuestions()[["q23b"]][39,2]-excluded))
# posDestStayPerPH <-sprintf("%1.2f%%",100*(goodDest+stayers)/(totalPersons-excluded))
# if(projType==3)
#    return(paste("Clients staying in PSH or exiting to permanent destinations:",posDestStayPerPH))
# paste("Clients exiting to permanent destinations:",posDestPer)

projType <- as.numeric(rep_old[["q4a"]][4,2])
totalPersons <- rep_old[["q7a"]][5,2]
stayers <- rep_old[["q5a"]][8,1]
excluded <- rep_old[["q23a"]][41,2] + rep_old[["q23b"]][41,2]
goodDest <- rep_old[["q23a"]][13,2]+rep_old[["q23b"]][13,2]
posDestPer <- sprintf("%1.2f%%",100*goodDest/(rep_old[["q23a"]][39,2]+rep_old[["q23b"]][39,2]-excluded))
posDestStayPerPH <-sprintf("%1.2f%%",100*(goodDest+stayers)/(totalPersons-excluded))
if(projType==3)
  return(paste("Clients staying in PSH or exiting to permanent destinations:",posDestStayPerPH))
paste("Clients exiting to permanent destinations:",posDestPer)


projType <- rep_new[["q4a"]] %>% dplyr::pull("HMIS.Project.Type")
totalPersons <- rep_new[["q7a"]] %>%
  dplyr::filter(X == "Total") %>%
  dplyr::pull("Total")
stayers <- rep_new[["q5a"]] %>%
  dplyr::filter(V1 == "Number of Stayers") %>%
  dplyr::pull("V2")
excluded <- rep_new[["q23c"]] %>%
  dplyr::filter(category == "Total persons whose destinations excluded them from the calculation") %>%
  dplyr::pull("Total")
goodDest <- rep_new[["q23c"]] %>%
  dplyr::filter(category == "Permanent Destinations", X == "Subtotal") %>%
  dplyr::pull("Total")
Total <- rep_new[["q23c"]] %>%
  dplyr::filter(category == "Total", X == "Total") %>%
  dplyr::pull("Total")
posDestPer <- sprintf( "%1.2f%%", 100 * ( goodDest / (Total - excluded) ) )
posDestStayPerPH <- sprintf( "%1.2f%%",100*(goodDest + stayers)/(totalPersons - excluded) )
if(projType==3){
  return(
    paste(
      "Clients staying in PSH or exiting to permanent destinations:",
      posDestStayPerPH
    )
  )
}else{
  paste("Clients exiting to permanent destinations:",posDestPer)
}



# ---- -------
# Clients exiting to permanent destinations
allQuestions_new[["q4a"]] %>% glimpse()
allQuestions_old[["q4a"]] %>% glimpse()

allQuestions_new[["q23a"]] %>% View()
allQuestions_old[["q23a"]] %>% View()





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


