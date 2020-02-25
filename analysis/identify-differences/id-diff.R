
file_path_old <- "../../data-unshared/raw/Sample-APR/"
file_path_new <- "../../data-unshared/raw/CSV-APR_FY2020/"

names_old <- gsub(pattern = ".csv$","", list.files("./data-unshared/raw/Sample-APR/"))
names_new <-  gsub(pattern = ".csv$","",list.files("./data-unshared/raw/CSV-APR_FY2020/"))

# present in the old, but missing in the new
setdiff(names_old, names_new)

# present in both
union(names_old, names_new)

# present in the new, but not in the old
setdiff(names_new, names_old)

