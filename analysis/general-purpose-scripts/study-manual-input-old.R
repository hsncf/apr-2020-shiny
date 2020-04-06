path_zip <- "./data-unshared/raw/CSV-APR_2019.zip"
folder_unzip <- gsub(".zip$","", basename(path_zip))
# unzip(path_zip, exdir=folder_unzip, overwrite=TRUE) # run only once
folder_path <- paste0(gsub(".zip$","", path_zip),"/")
path_files <- list.files(gsub(".zip$","", path_zip), full.names = T)

present_in_old_missing_in_new <- c("Q19a3","Q23a","Q23b" )
missing_in_old_present_in_new <- c("Q19b", "Q22e", "Q23c", "Q27g" ,"Q27h", "Q27i")

ls_new <- list()
for(question_i in seq_along(path_files) ){
  # i <- 1
  question_name <- path_files[question_i] %>% basename() %>% stringr::str_replace(".csv", "")
  ls_new[[question_name]] <- read.csv(path_files[question_i], header = TRUE, stringsAsFactors = TRUE)
}



ls_new <- list(
  # Comment out the questions that are breaking input, may need to add back in
  # with an error checking function
  # q19a3=read.csv("Q19a3.csv",header=TRUE,stringsAsFactors = FALSE),
  # q23a=read.csv("Q23a.csv",header=TRUE,stringsAsFactors = FALSE),
  # q23b=read.csv("Q23b.csv",header=TRUE,stringsAsFactors = FALSE),
  q4a=read.csv   (paste0(folder_path,"Q4a.csv"),  header=TRUE,stringsAsFactors = FALSE),
  q5a=read.csv   (paste0(folder_path,"Q5a.csv"),  header=TRUE,stringsAsFactors = FALSE),
  q6a=read.csv   (paste0(folder_path,"Q6a.csv"),  header=TRUE,stringsAsFactors = FALSE),
  q6b=read.csv   (paste0(folder_path,"Q6b.csv"),  header=TRUE,stringsAsFactors = FALSE),
  q6c=read.csv   (paste0(folder_path,"Q6c.csv"),  header=TRUE,stringsAsFactors = FALSE),
  q6d=read.csv   (paste0(folder_path,"Q6d.csv"),  header=TRUE,stringsAsFactors = FALSE),
  q6e=read.csv   (paste0(folder_path,"Q6e.csv"),  header=TRUE,stringsAsFactors = FALSE),
  q6f=read.csv   (paste0(folder_path,"Q6f.csv"),  header=TRUE,stringsAsFactors = FALSE),
  q7a=read.csv   (paste0(folder_path,"Q7a.csv"),  header=TRUE,stringsAsFactors = FALSE),
  q7b=read.csv   (paste0(folder_path,"Q7b.csv"),  header=TRUE,stringsAsFactors = FALSE),
  q8a=read.csv   (paste0(folder_path,"Q8a.csv"),  header=TRUE,stringsAsFactors = FALSE),
  q8b=read.csv   (paste0(folder_path,"Q8b.csv"),  header=TRUE,stringsAsFactors = FALSE),
  q9a=read.csv   (paste0(folder_path,"Q9a.csv"),  header=TRUE,stringsAsFactors = FALSE),
  q9b=read.csv   (paste0(folder_path,"Q9b.csv"),  header=TRUE,stringsAsFactors = FALSE),
  q10a=read.csv  (paste0(folder_path,"Q10a.csv"), header=TRUE,stringsAsFactors = FALSE),
  q10b=read.csv  (paste0(folder_path,"Q10b.csv"), header=TRUE,stringsAsFactors = FALSE),
  q10c=read.csv  (paste0(folder_path,"Q10c.csv"), header=TRUE,stringsAsFactors = FALSE),
  q11=read.csv   (paste0(folder_path,"Q11.csv"),  header=TRUE,stringsAsFactors = FALSE),
  q12a=read.csv  (paste0(folder_path,"Q12a.csv"), header=TRUE,stringsAsFactors = FALSE),
  q12b=read.csv  (paste0(folder_path,"Q12b.csv"), header=TRUE,stringsAsFactors = FALSE),
  q13a1=read.csv (paste0(folder_path,"Q13a1.csv"),header=TRUE,stringsAsFactors = FALSE),
  q13a2=read.csv (paste0(folder_path,"Q13a2.csv"),header=TRUE,stringsAsFactors = FALSE),
  q13b1=read.csv (paste0(folder_path,"Q13b1.csv"),header=TRUE,stringsAsFactors = FALSE),
  q13b2=read.csv (paste0(folder_path,"Q13b2.csv"),header=TRUE,stringsAsFactors = FALSE),
  q13c1=read.csv (paste0(folder_path,"Q13c1.csv"),header=TRUE,stringsAsFactors = FALSE),
  q13c2=read.csv (paste0(folder_path,"Q13c2.csv"),header=TRUE,stringsAsFactors = FALSE),
  q14a=read.csv  (paste0(folder_path,"Q14a.csv"), header=TRUE,stringsAsFactors = FALSE),
  q14b=read.csv  (paste0(folder_path,"Q14b.csv"), header=TRUE,stringsAsFactors = FALSE),
  q15=read.csv   (paste0(folder_path,"Q15.csv"),  header=TRUE,stringsAsFactors = FALSE),
  q16=read.csv   (paste0(folder_path,"Q16.csv"),  header=TRUE,stringsAsFactors = FALSE),
  q17=read.csv   (paste0(folder_path,"Q17.csv"),  header=TRUE,stringsAsFactors = FALSE),
  q18=read.csv   (paste0(folder_path,"Q18.csv"),  header=TRUE,stringsAsFactors = FALSE),
  q19a1=read.csv (paste0(folder_path,"Q19a1.csv"),header=TRUE,stringsAsFactors = FALSE),
  q19a2=read.csv (paste0(folder_path,"Q19a2.csv"),header=TRUE,stringsAsFactors = FALSE),
  # q19a3=read.csv (paste0(folder_path,"Q19a3.csv"),header=TRUE,stringsAsFactors = FALSE), # missing in the new
  q20a=read.csv  (paste0(folder_path,"Q20a.csv"), header=TRUE,stringsAsFactors = FALSE),
  q20b=read.csv  (paste0(folder_path,"Q20b.csv"), header=TRUE,stringsAsFactors = FALSE),
  q21=read.csv   (paste0(folder_path,"Q21.csv"),  header=TRUE,stringsAsFactors = FALSE),
  q22a1=read.csv (paste0(folder_path,"Q22a1.csv"),header=TRUE,stringsAsFactors = FALSE),
  q22b=read.csv  (paste0(folder_path,"Q22b.csv"), header=TRUE,stringsAsFactors = FALSE),
  # q23a=read.csv  (paste0(folder_path,"Q23a.csv"), header=TRUE,stringsAsFactors = FALSE),# missing in the new
  # q23b=read.csv  (paste0(folder_path,"Q23b.csv"), header=TRUE,stringsAsFactors = FALSE),# missing in the new
  q25a=read.csv  (paste0(folder_path,"Q25a.csv"), header=TRUE,stringsAsFactors = FALSE),
  q25b=read.csv  (paste0(folder_path,"Q25b.csv"), header=TRUE,stringsAsFactors = FALSE),
  q25c=read.csv  (paste0(folder_path,"Q25c.csv"), header=TRUE,stringsAsFactors = FALSE),
  q25d=read.csv  (paste0(folder_path,"Q25d.csv"), header=TRUE,stringsAsFactors = FALSE),
  q25e=read.csv  (paste0(folder_path,"Q25e.csv"), header=TRUE,stringsAsFactors = FALSE),
  q25f=read.csv  (paste0(folder_path,"Q25f.csv"), header=TRUE,stringsAsFactors = FALSE),
  q25g=read.csv  (paste0(folder_path,"Q25g.csv"), header=TRUE,stringsAsFactors = FALSE),
  q25h=read.csv  (paste0(folder_path,"Q25h.csv"), header=TRUE,stringsAsFactors = FALSE),
  q25i=read.csv  (paste0(folder_path,"Q25i.csv"), header=TRUE,stringsAsFactors = FALSE),
  q26a=read.csv  (paste0(folder_path,"Q26a.csv"), header=TRUE,stringsAsFactors = FALSE),
  q26b=read.csv  (paste0(folder_path,"Q26b.csv"), header=TRUE,stringsAsFactors = FALSE),
  q26c=read.csv  (paste0(folder_path,"Q26c.csv"), header=TRUE,stringsAsFactors = FALSE),
  q26d=read.csv  (paste0(folder_path,"Q26d.csv"), header=TRUE,stringsAsFactors = FALSE),
  q26e=read.csv  (paste0(folder_path,"Q26e.csv"), header=TRUE,stringsAsFactors = FALSE),
  q26f=read.csv  (paste0(folder_path,"Q26f.csv"), header=TRUE,stringsAsFactors = FALSE),
  q26g=read.csv  (paste0(folder_path,"Q26g.csv"), header=TRUE,stringsAsFactors = FALSE),
  q26h=read.csv  (paste0(folder_path,"Q26h.csv"), header=TRUE,stringsAsFactors = FALSE),
  q27a=read.csv  (paste0(folder_path,"Q27a.csv"), header=TRUE,stringsAsFactors = FALSE),
  q27b=read.csv  (paste0(folder_path,"Q27b.csv"), header=TRUE,stringsAsFactors = FALSE),
  q27c=read.csv  (paste0(folder_path,"Q27c.csv"), header=TRUE,stringsAsFactors = FALSE),
  q27d=read.csv  (paste0(folder_path,"Q27d.csv"), header=TRUE,stringsAsFactors = FALSE),
  q27e=read.csv  (paste0(folder_path,"Q27e.csv"), header=TRUE,stringsAsFactors = FALSE),
  q27f=read.csv  (paste0(folder_path,"Q27f.csv"), header=TRUE,stringsAsFactors = FALSE)
)



(adults <- ls_new[["q7a"]][1,2])
(children <- ls_new[["q7a"]][2,2])
(childHoH <- ls_new[["q5a"]][15,1])
paste("Clients Entering from Homeless Situations:",sprintf("%1.0f%%",100*ls_new[["q15"]][7,2]/(adults+childHoH)))
