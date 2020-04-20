projType <- c("Emergency Shelter"
              ,"Transitional Housing"
              ,"PH - Permanent Supportive Housing"
              ,"Street Outreach"
              ,"RETIRED"
              ,"Services Only"
              ,"Other"
              ,"Safe Haven"
              ,"PH - Housing Only"
              ,"PH - Housing with Services"
              ,"Day Shelter"
              ,"Homelessness Prevention"
              ,"PH - Rapid Re-Housing"
              ,"Coordinated Assessment")


projType1 <- tibble::tibble(1:14,projType)
colnames(projType1) <- c("x","project_type")


readr::write_csv(projType1,"./data-public/metadata/project-type.csv")
