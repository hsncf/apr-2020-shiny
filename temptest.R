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
adsf
