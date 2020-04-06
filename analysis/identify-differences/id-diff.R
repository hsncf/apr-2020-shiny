
file_path_old <- "../../data-unshared/raw/Sample-APR/"
# file_path_new <- "../../data-unshared/raw/CSV-APR_FY2020/"
file_path_new <- "../../data-unshared/raw/CSV-APR_2019/"

names_old <- gsub(pattern = ".csv$","", list.files("./data-unshared/raw/Sample-APR/"))
names_new <-  gsub(pattern = ".csv$","",list.files("./data-unshared/raw/CSV-APR_FY2020/"))

# present in the old, but missing in the new
setdiff(names_old, names_new)
present_in_old_missing_in_new <- c("Q19a3","Q23a","Q23b" )
# present in both
union(names_old, names_new)

# present in the new, but not in the old
setdiff(names_new, names_old)
missing_in_old_present_in_new <- c("Q19b", "Q22e", "Q23c", "Q27g" ,"Q27h", "Q27i")

