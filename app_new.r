#upload zip file
# this looks promising to read in all of the csv files
#dataFiles <- lapply(Sys.glob("data*.csv"), read.csv)
#rsconnect::deployApp(appName="interactive-apr")
#0019A8
#0947B2
#00629B
#09A0B2



# present_in_old_missing_in_new <- c("Q19a3","Q23a","Q23b" )
# missing_in_old_present_in_new <- c("Q19b", "Q22e", "Q23c", "Q27g" ,"Q27h", "Q27i")


library(shiny)
library(shinythemes)
library(DT)
library(plotly)
library(highcharter)
library(flexdashboard)

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

# ---- ui --------------

ui <- navbarPage(
   title = "Explore your Annual Performance Report",
   theme = shinytheme("united"),
   # SUMMARY
   tabPanel(
      title="Summary",
      sidebarLayout(
         sidebarPanel(
            tags$img(align = "left",height = 200, width = 200, src="HSN-logo-color.JPG"),
            h3("Upload Your APR to Analyze:"),
            fileInput(
               inputId="aprZip", "Choose .zip file",
               accept = c(
                  "application/x-compressed",
                  "application/x-zip-compressed application/zip",
                  "multipart/x-zip","application/octet-stream",".zip"
               )
            )
            #downloadButton("report","Generate report")
         ),
         mainPanel(
            h2(textOutput("names"),style="color: #09A0B2"),
            h4(textOutput("type")),
            h3(textOutput("clientCounts")),
            h4(textOutput("homelessPer")),
            h4(textOutput("incSummary")),
            h4(textOutput("incSummary2")),
            h4(textOutput("destPos2"))
         )
      )
   ),
   # DATA QUALITY
   tabPanel(
      title = "Data Quality",
      sidebarPanel(
         tags$img(height = 200, width = 200, src="HSNlogo1.jpg"),
         h2(textOutput("name1"),style="color: #09A0B2"),
         tags$br(),
         width = 3
      ),
      mainPanel(
         tabsetPanel(
            tabPanel(
               "Personal Identity Information",
               h3(textOutput("dq")),
               column(4,gaugeOutput("piigauge")),
               column(8,plotlyOutput("piiPlot")),
               tags$br(),
               dataTableOutput("PIITable")
            ),
            tabPanel(
               "Income and Housing",
               h3(textOutput("dqInc")),
               tags$br(),
               column(4,gaugeOutput("entryGauge")),
               column(4,gaugeOutput("annualGauge")),
               column(4,gaugeOutput("exitGauge")),
               column(12,plotlyOutput("incDqPlot"))
            ),
            tabPanel(
               "Universal Data Elements",
               tags$h3("Data Quality for Universal Data Elements:"),
               column(12,plotlyOutput("udePlot")),
               tags$br(),
               dataTableOutput("UDETable")
            ),
            tabPanel(
               "Timeliness",
               tags$h3("Timeliness of Data Entry within 3 days"),
               h4(textOutput("timeA")),
               column(4,gaugeOutput("timeAgauge")),
               column(8,plotlyOutput("time1")),
               tags$br(),
               tags$h3("Timeliness of Data Entry after 3 days"),
               h4(textOutput("timeB")),
               column(4,gaugeOutput("timeBgauge")),
               column(8,plotlyOutput("time2"))
            ),
            tabPanel(
               "Chronic Homelesness",
               tags$h3("Percentage of Error Rate for Chronic Homeless"),
               tags$br(),
               column(6,gaugeOutput("dqchronic")),
               column(12,dataTableOutput("dqchronicTable"))
            )
         ) # closes tabsetPanel
      ) # closes mainPanel
   ), # closes tabPanel
   # CLIENTS SERVED
   tabPanel(
      title = "Clients Served",
      sidebarPanel(
         tags$img(height = 200, width = 200, src="HSNlogo1.jpg"),
         h2(textOutput("name2"), style="color: #09A0B2"),
         tags$br(), width = 3
      ),
      mainPanel(
         tabsetPanel(
            tabPanel(
               "Clients Served",
               h3(textOutput("clientCounts2")),
               plotlyOutput("served")
            ),
            tabPanel(
               "Chronically Homeless Served",
               h3(textOutput("chronicNum")),
               h3(textOutput("chronicHOHNum")),
               plotlyOutput("chronic")
            ),
            tabPanel(
               "Unemployed Clients Served",
               h3(textOutput("unemployed")),
               h3(textOutput("employed")),
               plotlyOutput("unemp")
            ),
            tabPanel(
               "Report Validations Table",
               tags$br(),
               dataTableOutput("newTryTable")
            )
         ) # close tabsetPanel
      ) # close mainPanel
   ), # close tabPanel
   tabPanel(
      title = "Client Demographics",
      sidebarPanel(
         tags$img(height = 200, width = 200, src="HSNlogo1.jpg"),
         h2(textOutput("name3")),
         tags$br(),width = 3
      ),
      mainPanel(
         tabsetPanel(
            tabPanel(
               title = "Age",
               tags$h2("What are the Ages of Clients in the Project?"),
               h3(textOutput("adultKidCount")),
               plotlyOutput("plot")
            ),
            tabPanel(
               title = "Race",
               tags$h2("Client Race"),
               plotlyOutput("racePlot")
            ),
            tabPanel(
               title = "Ethnicity",
               tags$h2("Client Ethnicity"),
               plotlyOutput("ethPlot")
            ),
            tabPanel(
               title = "Gender",
               plotlyOutput("genderPlot")
            )
         ) # close tabsetPanel
      ) # close mainPanel
   ), # close tabPanel
   tabPanel(
      title = "Other Data",
      sidebarPanel(
         tags$img(height = 200, width = 200, src="HSNlogo1.jpg"),
         h2(textOutput("name4")),
         tags$br(),width = 3
      ),
      mainPanel(
         tabsetPanel(
            tabPanel(
               title = "PIT",
               tags$h3("Point in Time Counts of Persons and Households"),
               column(12,plotlyOutput("pit"))
            ),
            tabPanel(
               title = "Disabilities",
               tags$h2("What Health and Mental Health Conditions Did Clients Have at Entry?"),
               plotlyOutput("phyPlot")
            ),
            tabPanel(
               title = "Residence Prior",
               plotlyOutput("livSitPlot")
            ),
            tabPanel(
               title = "Income Sources",
               plotlyOutput("incStatus"),
               tags$h3("Adult Stayers (365 days or more)"),
               tableOutput("incStayTable"),
               tags$h4(textOutput("eIncStay")),
               tags$h3("Adult Leavers"),
               tableOutput("incLeaveTable"),
               tags$h4(textOutput("eIncLeave"))
            ),
            tabPanel(
               title = "Non-Cash and Health Insurance",
               tags$h3("Non-Cash Benefits and Health Insurance"),
               plotOutput("benefitEntry"),
               plotOutput("insStatus"),
               plotOutput("healthIns")
            ),
            tabPanel(
               title = "Length of Participation",
               tags$h3("Length of Participation in Project"),
               plotOutput("lengthTotal"),
               plotOutput("lengthLeaver"),
               plotOutput("lengthStayer"),
               tags$h3("")
            ),
            tabPanel(
               title = "Exit Destination",
               tags$h2("To which destinations did clients exit?"),
               tags$h2(textOutput("destPos")),
               plotOutput("destPlot"),
               tags$h2(textOutput("names_exit"),style="color: #09A0B2"),
               tags$h4(textOutput("type_exit"))
            )
         ) # close tabsetPanel
      ) # close mainPanel
   ), # close tabPanel
   tabPanel(
      title = "About",
      tags$img(height = 200, width = 200, src="HSNlogo1.jpg"),
      tags$h3("About the Annual Performance Report"),
      tags$h5("HUD requires CoC-funded projects to complete an Annual Performance Report (APR).  HUD uses the APR to track performance of CoC-funded projects.  We encourage you to use it to track your own performance as well!"),
      tags$h5("Projects must upload a .zip file that contains 66 .csv files to Sage for APR submission."),
      tags$a(href="https://www.hudexchange.info/programs/e-snaps/guides/apr/#sage-hmis-reporting-repository","Learn more",target="_blank"),
      tags$h3("About this Tool"),
      tags$h5("This tool allows you to see the data that you'll be submitting to HUD in your APR upload to Sage. Simply upload the .zip file and explore!"),
      tags$a(href="https://www.hmiscfl.org/","Created by Homeless Service Netwotrk of Central Florida",target="_blank"),
      tags$h3("APR Version"),
      tags$h5("Consistent with Version 1.2 of HUD APR specifications. Updated 11/6/2017.")
   ) # close tabPanel
) # close navbarPage
# ---- server ---------------
server <- function(input, output) {
   allQuestions <- reactive({
      td <- tempdir()
      unzip(input$aprZip$datapath, exdir=td, overwrite=TRUE)
      setwd(td)
      #myList <- lapply(Sys.glob("Q*.csv"), read.csv)
      #setNames(myList, Sys.glob("Q*.csv"))
      nextTry <- list(

         # Comment out the questions that are breaking input, may need to add back in
         # with an error checking function
         # q19a3=read.csv("Q19a3.csv",header=TRUE,stringsAsFactors = FALSE),
         # q23a=read.csv("Q23a.csv",header=TRUE,stringsAsFactors = FALSE),
         # q23b=read.csv("Q23b.csv",header=TRUE,stringsAsFactors = FALSE),
         q4a=read.csv("Q4a.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q5a=read.csv("Q5a.csv",header=FALSE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q6a=read.csv("Q6a.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q6b=read.csv("Q6b.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q6c=read.csv("Q6c.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q6d=read.csv("Q6d.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q6e=read.csv("Q6e.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q6f=read.csv("Q6f.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q7a=read.csv("Q7a.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q7b=read.csv("Q7b.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q8a=read.csv("Q8a.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q8b=read.csv("Q8b.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q9a=read.csv("Q9a.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q9b=read.csv("Q9b.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q10a=read.csv("Q10a.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q10b=read.csv("Q10b.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q10c=read.csv("Q10c.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q11=read.csv("Q11.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q12a=read.csv("Q12a.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q12b=read.csv("Q12b.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q13a1=read.csv("Q13a1.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q13a2=read.csv("Q13a2.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q13b1=read.csv("Q13b1.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q13b2=read.csv("Q13b2.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q13c1=read.csv("Q13c1.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q13c2=read.csv("Q13c2.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q14a=read.csv("Q14a.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q14b=read.csv("Q14b.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes(),
         q15=read.csv("Q15.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes() %>%
            dplyr::mutate(
               X = gsub('"','',X)
               ,category = ifelse(is.na(Total),X,NA)
            ) %>%
            tidyr::fill(category) %>%
            dplyr::mutate(
               category = ifelse(X == "Total", X, category)
            ) %>%
            dplyr::filter(!is.na(Total)) %>%
            dplyr::select(category,X, dplyr::everything()) %>% replace_quotes()
         ,
         q16=read.csv("Q16.csv",header=TRUE,stringsAsFactors       = FALSE) %>% replace_quotes() ,
         q17=read.csv("Q17.csv",header=TRUE,stringsAsFactors       = FALSE) %>% replace_quotes() ,
         q18=read.csv("Q18.csv",header=TRUE,stringsAsFactors       = FALSE) %>% replace_quotes() ,
         q19a1=read.csv("Q19a1.csv",header=TRUE,stringsAsFactors   = FALSE) %>% replace_quotes() ,
         q19a2=read.csv("Q19a2.csv",header=TRUE,stringsAsFactors   = FALSE) %>% replace_quotes() ,
         # q19a3=read.csv("Q19a3.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes() ,
         q20a=read.csv("Q20a.csv",header=TRUE,stringsAsFactors     = FALSE) %>% replace_quotes() ,
         q20b=read.csv("Q20b.csv",header=TRUE,stringsAsFactors     = FALSE) %>% replace_quotes() ,
         q21=read.csv("Q21.csv",header=TRUE,stringsAsFactors       = FALSE) %>% replace_quotes() ,
         q22a1=read.csv("Q22a1.csv",header=TRUE,stringsAsFactors   = FALSE) %>% replace_quotes() ,
         q22b=read.csv("Q22b.csv",header=TRUE,stringsAsFactors     = FALSE) %>% replace_quotes() ,
         # q23a=read.csv("Q23a.csv",header=TRUE,stringsAsFactors   = FALSE) %>% replace_quotes() ,
         # q23b=read.csv("Q23b.csv",header=TRUE,stringsAsFactors   = FALSE) %>% replace_quotes() ,
         q23c=read.csv("Q23c.csv",header=TRUE,stringsAsFactors     = FALSE)%>%
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
               dplyr::select(category,X, dplyr::everything()) %>% replace_quotes()
         ,
         q25a=read.csv("Q25a.csv",header=TRUE,stringsAsFactors = FALSE),
         q25b=read.csv("Q25b.csv",header=TRUE,stringsAsFactors = FALSE),
         q25c=read.csv("Q25c.csv",header=TRUE,stringsAsFactors = FALSE),
         q25d=read.csv("Q25d.csv",header=TRUE,stringsAsFactors = FALSE),
         q25e=read.csv("Q25e.csv",header=TRUE,stringsAsFactors = FALSE),
         q25f=read.csv("Q25f.csv",header=TRUE,stringsAsFactors = FALSE),
         q25g=read.csv("Q25g.csv",header=TRUE,stringsAsFactors = FALSE),
         q25h=read.csv("Q25h.csv",header=TRUE,stringsAsFactors = FALSE),
         q25i=read.csv("Q25i.csv",header=TRUE,stringsAsFactors = FALSE)%>%
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
            dplyr::select(category,X, dplyr::everything()) %>% replace_quotes()
         ,
         q26a=read.csv("Q26a.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes() ,
         q26b=read.csv("Q26b.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes() ,
         q26c=read.csv("Q26c.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes() ,
         q26d=read.csv("Q26d.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes() ,
         q26e=read.csv("Q26e.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes() ,
         q26f=read.csv("Q26f.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes() ,
         q26g=read.csv("Q26g.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes() ,
         q26h=read.csv("Q26h.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes() ,
         q27a=read.csv("Q27a.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes() ,
         q27b=read.csv("Q27b.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes() ,
         q27c=read.csv("Q27c.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes() ,
         q27d=read.csv("Q27d.csv",header=TRUE,stringsAsFactors = FALSE)%>%
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
            dplyr::select(category,X, dplyr::everything()) %>% replace_quotes()
         ,
         q27e=read.csv("Q27e.csv",header=TRUE,stringsAsFactors = FALSE) %>% replace_quotes() ,
         q27f=read.csv("Q27f.csv",header=TRUE,stringsAsFactors = FALSE)%>%
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
            dplyr::select(category,X, dplyr::everything()) %>% replace_quotes()
      )
      #print("it worked")
      projName <- as.character(nextTry[["q4a"]][1,3])
      print(projName)
      nextTry
   })
   #output$print <- renderPrint({
   #inFile <- input$aprZip
   #if(is.null(inFile))
   #return(NULL)

   #print(allQuestions()["q11"])
   # })

   output$print2 <- renderTable({
      if(is.null(input$aprZip))
         return(NULL)
      allQuestions()["q11"]
   })
   output$plot <-renderPlotly({
      if(is.null(input$aprZip))
         return(NULL)
      plot_ly(x=c("  Under 5"," 5-12","13-17","18-24","25-34","35-44","45-54","55-61","62+"),
              y=allQuestions()[["q11"]]$Total[1:9],
              name="Age Distribution",type='bar')%>%
         layout(xaxis = list(title = "Age Range"),
                yaxis = list(title ="Client Count"))
   })
   output$racePlot <-renderPlotly({
      if(is.null(input$aprZip))
         return(NULL)
      plot_ly(x=c(" White", " Black", "Asian", "Am. Indian","NHPI","Multiple","DK/R","Missing"),
              y=allQuestions()[["q12a"]]$Total[1:8],
              name='Race ',type='bar')%>%
         layout(xaxis = list(title = "Race"),
                yaxis = list(title ="Client Count"))
   })
   output$ethPlot <-renderPlotly({
      if(is.null(input$aprZip))
         return(NULL)
      plot_ly(x=c(" Non-Latino"," Hispanic/Latino","DK/R","Missing"),
              y=allQuestions()[["q12b"]]$Total[1:4],
              name="Ethnicity Distribution",type='bar')%>%
         layout(xaxis = list(title = "Ethnicity"),
                yaxis = list(title ="Client Count"))
   })
   output$genderPlot <- renderPlotly({
      if(is.null(input$aprZip))
         return(NULL)
      y<-c("Male","Female"," Trans Female"," Trans Male"," Non-Binary","  DK/R ","  Missing")
      adult<-allQuestions()[["q10a"]]$Total[1:7]
      kid<-allQuestions()[["q10b"]]$Total[1:7]
      dt<-data.frame(y,adult,kid)
      plot_ly(dt,x=~adult,y=~y,type = 'bar', orientation='h',name = 'Adults',
              marker=list(color='rgba(246,78,139,0.6)',
                          line=list(color='rgba(246,78,139,1.0)',
                                    width=3)))%>%
         add_trace(x=~kid,y=~y,type = 'bar', orientation='h',name = 'Kids',
                   marker=list(color='rgba(58,71,80,0.6)',
                               line=list(color='rgba(58,71,80,1.0)',
                                         width=3)))%>%
         layout(barmode='stack',
                xaxis = list(title = "Client Count"),
                yaxis = list(title =""))
   })
   # output$genderKidPlot <-renderPlot({
   #   if(is.null(input$aprZip))
   #     return(NULL)
   #   bp<-barplot(allQuestions()[["q10b"]]$Total[1:7], main="Genders of Children",
   #           names.arg=c("Male","Female","Trans Female","Trans Male","Non-Binary","DK/R","Missing"),ylab="Number of Clients",col="#08A88D")
   #   text(bp,0,allQuestions()[["q10b"]]$Total[1:7],cex=1,pos=3)
   # })

   output$livSitPlot <- renderPlotly({
      if(is.null(input$aprZip))
         return(NULL)
      plot_ly(x=c("Homeless","Institutional","Other"),
              y=c(allQuestions()[["q15"]][7,2],allQuestions()[["q15"]][16,2],allQuestions()[["q15"]][30,2]),
              name="Residence Prior to Project Entry",type='bar')%>%
         layout(xaxis = list(title = "Living Situdation"),
                yaxis = list(title ="Client Count"))
   })
   output$destPlot <- renderPlot({
      if(is.null(input$aprZip))
         return(NULL)
      names <- c("Perm","Temp","Inst","Other")
      data <- data.frame(c(allQuestions()[["q23a"]][13,2],allQuestions()[["q23b"]][13,2]), c(allQuestions()[["q23a"]][23,2],allQuestions()[["q23b"]][23,2]), c(allQuestions()[["q23a"]][31,2],allQuestions()[["q23b"]][31,2]), c(allQuestions()[["q23a"]][38,2],allQuestions()[["q23b"]][38,2]))
      colnames(data) <- names
      bp<-barplot(as.matrix(data), main="Exit Destinations",
                  xlab="Destination", col=c("#0947B2","#08A88D"),
                  legend = c("90 days or more","<90 days"))
   })
   # output$piiPlot <- renderPlot({
   #   if(is.null(input$aprZip))
   #     return(NULL)
   #   piiData <- t(allQuestions()[["q6a"]][1:6,2:4])
   #   colnames(piiData) <- c("Name","SSN","DOB","Race","Ethnicity","Gender")
   #   bp<-barplot(as.matrix(piiData), main="PII Data Issues",
   #                   xlab="Number of Errors", col=c("yellow","red","orange"),
   #                   legend = c("DK/R","Missing","Other Issue"),horiz=TRUE,las=2,dataLabels = list(enabled=TRUE))
   # })
   # output$incDqPlot <- renderPlot({
   #   if(is.null(input$aprZip))
   #     return(NULL)
   #   incDQ <- t(allQuestions()[["q6c"]][,2])
   #   colnames(incDQ) <- c("Destination","Income at Entry","Income at Annual","Income at Exit")
   #   par(las=2)
   #   par(mar=c(5,10,4,2))
   #   bp<-barplot(as.matrix(incDQ), main="Income and Housing Data Quality",
   #           xlab="Number of Errors", col="orange",
   #           horiz=TRUE)
   # })
   # output$udePlot <- renderPlot({
   #   if(is.null(input$aprZip))
   #     return(NULL)
   #   udeDQ <- t(allQuestions()[["q6b"]][,2])
   #   colnames(udeDQ) <- c("Vet Status","Entry Date","HoH Relat.","Location","Disabling Condition")
   #   par(las=2)
   #   par(mar=c(5,10,4,2))
   #   bp<-barplot(as.matrix(udeDQ), main="Universal Data Elements",
   #           xlab="Number of Errors", col="orange",
   #           horiz=TRUE)
   #   text(bp,0,as.matrix(udeDQ),cex=1,pos=3)
   # })
   output$names <- renderText({
      if(is.null(input$aprZip))
         return("Select an APR to Begin")
      as.character(allQuestions()[["q4a"]][1,3])
   })
   output$names_exit <- renderText({
      if(is.null(input$aprZip))
         return("Select an APR to Begin")
      as.character(allQuestions()[["q4a"]][2,2])
   })
   output$adultKidCount <- renderText({
      if(is.null(input$aprZip))
         return(NULL)
      paste(allQuestions()[["q7a"]]$Total[1],"Adults,",allQuestions()[["q7a"]]$Total[2],"Children")
   })
   output$name1 <- renderText({
      if(is.null(input$aprZip))
         return("Select an APR to Begin")
      as.character(allQuestions()[["q4a"]][2,2])
   })
   output$name2 <- renderText({
      if(is.null(input$aprZip))
         return("Select an APR to Begin")
      as.character(allQuestions()[["q4a"]][2,2])
   })
   output$name3 <- renderText({
      if(is.null(input$aprZip))
         return("Select an APR to Begin")
      as.character(allQuestions()[["q4a"]][2,2])
   })
   output$name4 <- renderText({
      if(is.null(input$aprZip))
         return("Select an APR to Begin")
      as.character(allQuestions()[["q4a"]][2,2])
   })
   output$name5 <- renderText({
      if(is.null(input$aprZip))
         return("Select an APR to Begin")
      as.character(allQuestions()[["q4a"]][2,2])
   })
   output$name6 <- renderText({
      if(is.null(input$aprZip))
         return("Select an APR to Begin")
      as.character(allQuestions()[["q4a"]][2,2])
   })
   output$name7 <- renderText({
      if(is.null(input$aprZip))
         return("Select an APR to Begin")
      as.character(allQuestions()[["q4a"]][2,2])
   })
   output$type <- renderText({
      if(is.null(input$aprZip))
         return(NULL)
      projType <- c("Emergency Shelter","Transitional Housing","PH - Permanent Supportive Housing","Street Outreach","RETIRED","Services Only","Other","Safe Haven","PH - Housing Only","PH - Housing with Services","Day Shelter","Homelessness Prevention","PH - Rapid Re-Housing","Coordinated Assessment")
      index <- as.numeric(allQuestions()[["q4a"]][1,5])
      projType[index]
   })
   output$type_exit <- renderText({
      if(is.null(input$aprZip))
         return(NULL)
      projType <- c("Emergency Shelter","Transitional Housing","PH - Permanent Supportive Housing","Street Outreach","RETIRED","Services Only","Other","Safe Haven","PH - Housing Only","PH - Housing with Services","Day Shelter","Homelessness Prevention","PH - Rapid Re-Housing","Coordinated Assessment")
      index <- as.numeric(allQuestions()[["q4a"]][1,"HMIS.Project.Type"])
      projType[index]
   })
   output$dq <- renderText({
      if(is.null(input$aprZip))
         return(NULL)
      pii<-(allQuestions()[["q6a"]][7,5])*100
      paste("Percentage of Error Rate for Personally Identifiable Information: ",pii,"%")
   })
   output$dqInc <- renderText({
      if(is.null(input$aprZip))
         return(NULL)
      incHousingMissing <- sum(allQuestions()[["q6c"]][,2])
      HoH <- allQuestions()[["q5a"]][14,1] + allQuestions()[["q5a"]][15,1]
      leavers <- allQuestions()[["q5a"]][5,1]
      stayersHohAdult <- allQuestions()[["q5a"]][16,1]
      leaversHohAdult <- allQuestions()[["q5a"]][7,1]
      #paste("Data Quality for Income and Housing Data Quality: ", sprintf("%1.2f%%", 100*(1-(piiMissing/(6*allQuestions()[["q5a"]][1,1])))))
      paste("Data Quality for Income and Housing Data Quality: ", sprintf("%1.2f%%", 100*(1-(incHousingMissing/(HoH+leavers+stayersHohAdult+leaversHohAdult)))))
   })
   # output$dqUde <- renderText({
   #   if(is.null(input$aprZip))
   #     return(NULL)
   #   udeMissing <- sum(allQuestions()[["q6b"]][,2])
   #   HoH <- allQuestions()[["q5a"]][14,1] + allQuestions()[["q5a"]][15,1]
   #   adults <- allQuestions()[["q5a"]][2,1]
   #   all <- allQuestions()[["q5a"]][1,1]
   #   paste("Data Quality for Universal Data Elements: ", sprintf("%1.2f%%", 100*(1-(udeMissing/(adults+3*all+HoH)))))
   # })
   output$clientCounts <- renderText({
      if(is.null(input$aprZip))
         return(NULL)
      totalPersons <- allQuestions()[["q7a"]][5,2]
      paste("Total Clients:",totalPersons)
   })
   output$clientCounts2 <- renderText({
      if(is.null(input$aprZip))
         return(NULL)
      totalPersons <- allQuestions()[["q7a"]][5,2]
      paste("Total Clients:",totalPersons)
   })
   output$homelessPer <- renderText({
      if(is.null(input$aprZip))
         return("This tool allows you to see the data that you'll be submitting to HUD in your APR upload to Sage. Simply upload the .zip file and explore!")
      # adults <- allQuestions()[["q7a"]][1,2]
      # children <- allQuestions()[["q7a"]][2,2]
      # childHoH <- allQuestions()[["q5a"]][15,2]
      # # (allQuestions()[["q15"]][7,2])
      # paste("Clients Entering from Homeless Situations:",sprintf("%1.0f%%",100*allQuestions()[["q15"]][7,2]/(adults+childHoH)))
      adults <- allQuestions()[["q7a"]] %>%
         dplyr::filter(X == "Adults") %>%
         dplyr::pull(Total)
      # children <- allQuestions()[["q7a"]][2,2]
      children <- allQuestions()[["q7a"]] %>% ### THIS VALUE IS NOT USED IN CALCULATION. CHECK WITH TINO
         dplyr::filter(X == "Children") %>%
         dplyr::pull(Total)
      # childHoH <- allQuestions_new[["q5a"]][15,2]
      childHoH <- allQuestions()[["q5a"]] %>%
         dplyr::filter(V1 == "Number of Child and Unknown-Age Heads of Household") %>%
         dplyr::pull(V2)
      subsection_total <- allQuestions()[["q15"]] %>%
         dplyr::filter(category == "Homeless Situations", X == "Subtotal") %>%
         dplyr::pull(Total)

      paste(
         "Clients Entering from Homeless Situations:"
         ,sprintf("%1.0f%%",100*subsection_total/(adults+childHoH) )
      )
   })
   output$eIncLeave <- renderText({
      if(is.null(input$aprZip))
         return(NULL)
      paste("Adult leavers gaining or maintaining earned income:", sprintf("%1.0f%%",100*rowSums(allQuestions()[["q19a2"]][c(1,3,5),4:6])[1]/allQuestions()[["q19a2"]][1,8]))
   })
   output$eIncStay <- renderText({
      if(is.null(input$aprZip))
         return(NULL)
      paste("Adult stayers gaining or maintaining earned income:", sprintf("%1.0f%%",100*rowSums(allQuestions()[["q19a1"]][c(1,3,5),4:6])[1]/allQuestions()[["q19a1"]][1,8]))
   })
   output$incSummary <- renderText({
      if(is.null(input$aprZip))
         return("For more information, navigate to the 'About' tab")
      paste("Pending clarification from Tino")
      # paste("Adults gaining or maintaining earned income:", sprintf("%1.0f%%",100*rowSums(allQuestions()[["q19a3"]][1:5,4:6])[1]/allQuestions()[["q19a3"]][1,8]))
   })
   output$incSummary2 <- renderText({
      if(is.null(input$aprZip))
         return(NULL)
      paste("Pending clarification from Tino")
      # paste( "Adults gaining or maintaining other income:",sprintf("%1.0f%%",100*rowSums(allQuestions()[["q19a3"]][1:5,4:6])[3]/allQuestions()[["q19a3"]][1,8]))
   })
   output$destPos <- renderText({
      if(is.null(input$aprZip))
         return(NULL)
      projType <- as.numeric(allQuestions()[["q4a"]][4,2])
      totalPersons <- allQuestions()[["q7a"]][5,2]
      stayers <- allQuestions()[["q5a"]][8,1]
      excluded <- allQuestions()[["q23a"]][41,2] + allQuestions()[["q23b"]][41,2]
      goodDest <- allQuestions()[["q23a"]][13,2]+allQuestions()[["q23b"]][13,2]
      posDestPer <- sprintf("%1.2f%%",100*goodDest/(allQuestions()[["q23a"]][39,2]+allQuestions()[["q23b"]][39,2]-excluded))
      posDestStayPerPH <-sprintf("%1.2f%%",100*(goodDest+stayers)/(totalPersons-excluded))
      if(projType==3)
         return(paste("Clients staying in PSH or exiting to permanent destinations:",posDestStayPerPH))
      paste("Clients exiting to permanent destinations:",posDestPer)
   })
   output$destPos2 <- renderText({
      if(is.null(input$aprZip))
         return(NULL)
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
      projType <- allQuestions()[["q4a"]] %>% dplyr::pull(HMIS.Project.Type)
      totalPersons <- allQuestions()[["q7a"]] %>%
         dplyr::filter(X == "Total") %>%
         dplyr::pull(Total)
      stayers <- allQuestions()[["q5a"]] %>%
         dplyr::filter(V1 == "Number of Stayers") %>%
         dplyr::pull(V2)
      excluded <- allQuestions()[["q23c"]] %>%
         dplyr::filter(category == "Total persons whose destinations excluded them from the calculation") %>%
         dplyr::pull(Total)
      goodDest <- allQuestions()[["q23c"]] %>%
         dplyr::filter(category == "Permanent Destinations", X == "Subtotal") %>%
         dplyr::pull(Total)
      Total <- allQuestions()[["q23c"]] %>%
         dplyr::filter(category == "Total", X == "Total") %>%
         dplyr::pull(Total)
      posDestPer <- sprintf("%1.2f%%",100*(goodDest/(Total-excluded)))
      posDestStayPerPH <-sprintf("%1.2f%%",100*(goodDest+stayers)/(totalPersons-excluded))
      if(projType==3)
         return(paste("Clients staying in PSH or exiting to permanent destinations:",posDestStayPerPH))
      paste("Clients exiting to permanent destinations:",posDestPer)

   })
   output$validTable <- renderTable({
      labels <- c("1. Total Number of Persons Served","2. Number of Adults (age 18 or over)","3. Number of Children (under age 18)","4. Number of Persons with Unknown Age","5. Number of Leavers","6. Number of Adult Leavers","7. Number of Adult and Head of Household Leavers","8. Number of Stayers","9. Number of Adult Stayers","10. Number of Veterans","11. Number of Chronically Homeless Persons","12. Number of Youth Under Age 25","13. Number of Parenting Youth Under Age 25 with Children","14. Number of Adult Heads of Household","15. Number of Child and Unknown-Age Heads of Household","16. Heads of Households and Adult Stayers in the Project 365 Days or More")
      as.matrix(allQuestions()[["q5a"]])
   })
   output$newTryTable <- DT::renderDataTable({
      if(is.null(input$aprZip))
         return(NULL)
      q5a <- allQuestions()[["q5a"]]
      colnames(q5a) <-"Client Count"
      datatable(q5a,rownames=TRUE)
   })
   output$pit <- renderPlotly({
      if(is.null(input$aprZip))
         return(NULL)
      pitPer<-allQuestions()[["q7b"]]$Total
      pitHH<-allQuestions()[["q8b"]]$Total
      month<-c('January','April','July','October')
      dt<-data.frame(month,pitPer,pitHH)
      dt$month <- factor(dt$month, levels = dt[["month"]])
      plot_ly(dt,x=~month,y=~pitPer,name="Point-In-Time Count of Persons",type='scatter',mode='lines',
              line=list(color='rgb(205,12,24)',width=4))%>%
         add_trace(y=~pitHH,name="Point-In-Time Count of Households",
                   line=list(color='rgb(22,96,167)',width=4))%>%
         layout(title="Point in Time Counts on the Last Wednesday",
                xaxis=list(title="Month"),
                yaxis=list(title="Count"))
      # heading <- "Point-In-Time Count of Persons"
      # xlabel <- "Month (May not be chronological)"
      # ylabel <- "Total Persons"
      # plot(as.Date(c("2016-01-01", "2016-04-01","2016-07-01","2016-10-01")),allQuestions()[["q7b"]]$Total,type="n",main=heading,xlab=xlabel,ylab=ylabel,ylim=c(0,max(allQuestions()[["q7b"]]$Total)))
      # lines(as.Date(c("2016-01-01", "2016-04-01","2016-07-01","2016-10-01")),allQuestions()[["q7b"]]$Total,type="o",pch=19,col="#09A0B2")
   })
   # outputHH <- renderPlotly({
   #   if(is.null(input$aprZip))
   #     return(NULL)
   #   heading <- "Point-In-Time Count of Households"
   #   xlabel <- "Month (May not be chronological)"
   #   ylabel <- "Total Households"
   #   plot(as.Date(c("2016-01-01", "2016-04-01","2016-07-01","2016-10-01")),allQuestions()[["q8b"]]$Total,type="n",main=heading,xlab=xlabel,ylab=ylabel,ylim=c(0,max(allQuestions()[["q8b"]]$Total)))
   #   lines(as.Date(c("2016-01-01", "2016-04-01","2016-07-01","2016-10-01")),allQuestions()[["q8b"]]$Total,type="o",pch=19,col="#09A0B2")
   # })
   output$lengthTotal <- renderPlot({
      if(is.null(input$aprZip))
         return(NULL)
      par(mar=c(8,3,2,1))
      bp<-barplot(allQuestions()[["q22a1"]]$Total[1:11],main="Length of Participation for All Clients",ylab="Client Count",names.arg=c("<=30 days","31-60 days","61-90 days","91-180 days","181-365 days","1-2 Yrs","2-3 Yrs","3-4 Yrs","4-5 Yrs",">5 Yrs","Missing"),las=2,col="#0947B2")
      text(bp,0,allQuestions()[["q22a1"]]$Total[1:11],cex=1,pos=3)
   })
   output$lengthLeaver <- renderPlot({
      if(is.null(input$aprZip))
         return(NULL)
      par(mar=c(8,3,2,1))
      bp<-barplot(allQuestions()[["q22a1"]]$Leavers[1:11],main="Length of Participation for Leavers",ylab="Client Count",names.arg=c("<=30 days","31-60 days","61-90 days","91-180 days","181-365 days","1-2 Yrs","2-3 Yrs","3-4 Yrs","4-5 Yrs",">5 Yrs","Missing"),las=2, col="#09A0B2")
      text(bp,0,allQuestions()[["q22a1"]]$Leavers[1:11],cex=1,pos=3)
   })
   output$lengthStayer <- renderPlot({
      if(is.null(input$aprZip))
         return(NULL)
      par(mar=c(8,3,2,1))
      bp<-barplot(allQuestions()[["q22a1"]]$Stayers[1:11],main="Length of Participation for Stayers",ylab="Client Count",names.arg=c("<=30 days","31-60 days","61-90 days","91-180 days","181-365 days","1-2 Yrs","2-3 Yrs","3-4 Yrs","4-5 Yrs",">5 Yrs","Missing"),las=2,col="#08A88D")
      text(bp,0,allQuestions()[["q22a1"]]$Stayers[1:11],cex=1,pos=3)
   })
   output$benefitEntry <- renderPlot({
      if(is.null(input$aprZip))
         return(NULL)
      bp<-barplot(allQuestions()[["q20a"]]$Benefit.at.Start,main="Benefit at Start",names.arg = c("SNAP","WIC","TANF Child Care","TANF Xport","Other TANF","Other"),ylab="Client Count", col="#0019A8")
      text(bp,0,allQuestions()[["q20a"]]$Benefit.at.Start,cex=1,pos=3)
   })
   output$healthIns <- renderPlot({
      if(is.null(input$aprZip))
         return(NULL)
      bp<-barplot(allQuestions()[["q21"]]$At.Start[1:10],main="Health Insurance at Start",names.arg = c("Medicaid","Medicare","SCHIP","VA","Employer","COBRA","Private","State-Adults","Indian","Other"),ylab="Client Count",col="#09A0B2")
      text(bp,0,allQuestions()[["q21"]]$At.Start[1:10],cex=1,pos=3)
   })
   output$insStatus <- renderPlot({
      if(is.null(input$aprZip))
         return(NULL)
      bp<-barplot(allQuestions()[["q21"]][c(11,15,16),2],main="Insurance Status at Entry",names.arg = c("No Health Ins","1 Source","2+ Sources"),ylab="Client Count",col="#08A88D")
      text(bp,0,allQuestions()[["q21"]][c(11,15,16),2],cex=1,pos=3)
   })
   output$incStatus <- renderPlotly({
      if(is.null(input$aprZip))
         return(NULL)
      plot_ly(x=c(" Earned ONLY"," Other ONLY"," Earned + Other"," No Income","DK/R","Missing"),
              y=allQuestions()[["q18"]][1:6,2],
              name="Income Types for Adults at Entry",type='bar')%>%
         layout(xaxis=list(title=""),
                yaxis=list(title="Client Count"))
   })
   output$incStayTable <-renderTable({
      if(is.null(input$aprZip))
         return(NULL)
      Population <- c("Maintained Earned Income","Increased Earned Income","Maintained Other Income","Increased Other Income","Maintained Total Income","Increased Total Income","Total Adults")
      Clients <- c(allQuestions()[["q19a1"]][1,4],allQuestions()[["q19a1"]][1,5] + allQuestions()[["q19a1"]][1,6],allQuestions()[["q19a1"]][3,4],allQuestions()[["q19a1"]][3,5] + allQuestions()[["q19a1"]][3,6],allQuestions()[["q19a1"]][5,4],allQuestions()[["q19a1"]][5,5] + allQuestions()[["q19a1"]][5,6],allQuestions()[["q19a1"]][1,8])
      data.frame(Population,Clients)
   })
   output$incLeaveTable <-renderTable({
      if(is.null(input$aprZip))
         return(NULL)
      Population <- c("Maintained Earned Income","Increased Earned Income","Maintained Other Income","Increased Other Income","Maintained Total Income","Increased Total Income","Total Adults")
      Clients <- c(allQuestions()[["q19a2"]][1,4],allQuestions()[["q19a2"]][1,5] + allQuestions()[["q19a2"]][1,6],allQuestions()[["q19a2"]][3,4],allQuestions()[["q19a2"]][3,5] + allQuestions()[["q19a2"]][3,6],allQuestions()[["q19a2"]][5,4],allQuestions()[["q19a2"]][5,5] + allQuestions()[["q19a2"]][5,6],allQuestions()[["q19a2"]][1,8])
      data.frame(Population,Clients)
   })
   output$report <- downloadHandler(
      filename = "report.html",
      content = function(file) {
         tempReport <- file.path(tempdir(), "report.Rmd")
         file.copy("report.Rmd", tempReport, overwrite = TRUE)
         rmarkdown::render(tempReport, output_file = file,
                           params = allQuestions,
                           envir = new.env(parent = globalenv())
         )
      }
   )

   ######## HSN APPROVED METRICS ############

   output$served <- renderPlotly({
      if(is.null(input$aprZip))
         return(NULL)
      plot_ly(x=c("Client Exited", "Clients Stayers"),y=allQuestions()[["q5a"]][c(5,8),1],name="Clients Served",type='bar')%>%
         layout(xaxis = list(title = ""),
                yaxis = list(title ="Client Count"))
   })
   output$chronic <- renderPlotly({
      if(is.null(input$aprZip))
         return(NULL)
      plot_ly(x=c("Chronically Homeless","Not Chronically Homeless","Client DK/Client Refused","DNC"),
              y=allQuestions()[["q26a"]][1:4,2],
              name="Chronically Homeless Served",type='bar')%>%
         layout(xaxis = list(title = ""),
                yaxis = list(title ="Head of Household"))
   })
   output$chronicNum <- renderText({
      if(is.null(input$aprZip))
         return(NULL)
      chronic <- allQuestions()[["q26b"]][1,2]
      adults <- allQuestions()[["q26b"]][5,2]
      chronicage <- (chronic/adults)*100
      paste("Percentage of Chronically Homeless Clients: ",round(chronicage,2),"%")
   })
   output$chronicHOHNum <- renderText({
      if(is.null(input$aprZip))
         return(NULL)
      chronicHOH <- allQuestions()[["q26a"]][1,2]
      HOHadults <- allQuestions()[["q26a"]][5,2]
      chronicHOHage <- (chronicHOH/HOHadults)*100
      paste("Percentage of Chronically Homeless HOH Clients: ",round(chronicHOHage,2),"%")
   })
   output$unemp <- renderPlotly({
      if(is.null(input$aprZip))
         return(NULL)
      unempClient <- allQuestions()[["q16"]][1,2]
      tClient <- allQuestions()[["q16"]][13,2]
      empClient <- tClient-unempClient
      val <- matrix(c(unempClient,empClient),ncol=2,byrow = TRUE)
      colnames(val) <- c("Unemployed Clients", "Employed Clients")
      rownames(val) <- c("Clients")
      val <- as.table(val)
      plot_ly(x=c("Clients w/ No Income","Others"),y=val,name="Serving the Unemployed",type='bar')%>%
         layout(xaxis = list(title = ""),
                yaxis = list(title ="Adults"))
   })
   output$unemployed <- renderText({
      unempClient <- allQuestions()[["q16"]][1,2]
      tClient <- allQuestions()[["q16"]][13,2]
      empClient <- tClient-unempClient
      unempage <- (unempClient/tClient)*100
      paste("Percentage of Unemployed Clients Served at Entry: ",round(unempage,2),"%")
   })
   output$employed <- renderText({
      unempClient <- allQuestions()[["q16"]][1,2]
      tClient <- allQuestions()[["q16"]][13,2]
      empClient <- tClient-unempClient
      empage <- (empClient/tClient)*100
      paste("Percentage of Employed Clients Served at Entry: ",round(empage,2),"%")
   })
   output$piiPlot <- renderPlotly({
      if(is.null(input$aprZip))
         return(NULL)
      y<-c('Name ','SSN ','DOB ','Race ','Ethnicity ','Gender ')
      dkr<-allQuestions()[["q6a"]][1:6,2]
      miss<-allQuestions()[["q6a"]][1:6,3]
      iss<-allQuestions()[["q6a"]][1:6,4]
      dt<-data.frame(y,dkr,miss,iss)
      # print(dt)
      plot_ly(dt,x=~dkr,y=~y,type='bar',orientation='h',name='DK/R',
              marker=list(color = 'rgba(246, 78, 139, 0.6)',
                          line = list(color = 'rgba(246, 78, 139, 1.0)',
                                      width = 3)))%>%
         add_trace(x=~miss,name='Missing',
                   marker = list(color = 'rgba(58, 71, 80, 0.6)',
                                 line = list(color = 'rgba(58, 71, 80, 1.0)',
                                             width = 3)))%>%
         add_trace(x=~iss,name='Data Issues',
                   marker = list(color = 'rgba(74, 140, 247, 0.6)',
                                 line = list(color = 'rgba(74, 140, 247, 1.0)',
                                             width = 3)))%>%
         layout(barmode='stack',
                xaxis = list(title = "Number of Errors"),
                yaxis = list(title =""))
   })
   output$incDqPlot <- renderPlotly({
      if(is.null(input$aprZip))
         return(NULL)
      plot_ly(x=(allQuestions()[["q6c"]][,2]),y=c("Destination ","Income at Entry ","Income at Annual ","Income at Exit "),type='bar',orientation='h')%>%
         layout(xaxis = list(title = "Number of Errors"),
                yaxis = list(title =""))
   })
   output$udePlot <- renderPlotly({
      if(is.null(input$aprZip))
         return(NULL)
      plot_ly(x=allQuestions()[["q6b"]][,2],
              y=c("Vet Status ","Entry Date ","HoH Relat. ","Location ","Disabling Condition "),
              type='bar',orentation='h')%>%
         layout(xaxis = list(title = "Number of Errors"),
                yaxis = list(title =""))
   })
   output$time1 <- renderPlotly({
      if(is.null(input$aprZip))
         return(NULL)
      y<-c("0 days","1-3 days")
      entry<-allQuestions()[["q6e"]][1:2,2]
      exit<-allQuestions()[["q6e"]][1:2,3]
      dt<-data.frame(y,entry,exit)
      # print(dt)
      plot_ly(dt,x=~entry,y=~y,type='bar',orientation='h',name="Entry Records",
              marker=list(color = 'rgba(246, 78, 139, 0.6)',
                          line = list(color = 'rgba(246, 78, 139, 1.0)',
                                      width = 3)))%>%
         add_trace(x=~exit,name="Exit Records",
                   marker = list(color = 'rgba(58, 71, 80, 0.6)',
                                 line = list(color = 'rgba(58, 71, 80, 1.0)',
                                             width = 3)))%>%
         layout(xaxis = list(title = "Days"),
                yaxis = list(title =""))
   })
   output$time2 <- renderPlotly({
      if(is.null(input$aprZip))
         return(NULL)
      y<-c("4-6 days","7-10 days","11+ days")
      entry<-allQuestions()[["q6e"]][3:5,2]
      exit<-allQuestions()[["q6e"]][3:5,3]
      dt<-data.frame(y,entry,exit)
      # print(dt)
      plot_ly(dt,x=~entry,y=~y,type='bar',orientation='h',name="Entry Records",
              marker=list(color = 'rgba(246, 78, 139, 0.6)',
                          line = list(color = 'rgba(246, 78, 139, 1.0)',
                                      width = 3)))%>%
         add_trace(x=~exit,name="Exit Records",
                   marker = list(color = 'rgba(58, 71, 80, 0.6)',
                                 line = list(color = 'rgba(58, 71, 80, 1.0)',
                                             width = 3)))%>%
         layout(xaxis = list(title = "Days"),
                yaxis = list(title =""))
   })
   output$phyPlot <- renderPlotly({
      if(is.null(input$aprZip))
         return(NULL)
      plot_ly(x=allQuestions()[["q13a1"]][,2],
              y=c("Ment Health","Alcohol","Drug","AODA","Chronic Health","HIV/AIDS","Develop.","Physical"),
              type='bar',orientation='h')%>%
         layout(xaxis = list(title = "Client Count"),
                yaxis = list(title =""))
   })
   output$timeA <- renderText({
      if(is.null(input$aprZip))
         return(NULL)
      early<-sum(allQuestions()[["q6e"]][1:2,2])+sum(allQuestions()[["q6e"]][1:2,3])
      clients<-allQuestions()[["q7a"]][5,2]
      timeage<-(early/clients)*100
      paste("Percentage of Client Records entered in Time: ",round(timeage,2),"%")
   })
   output$timeB <- renderText({
      if(is.null(input$aprZip))
         return(NULL)
      early<-sum(allQuestions()[["q6e"]][3:5,2])+sum(allQuestions()[["q6e"]][3:5,3])
      clients<-allQuestions()[["q7a"]][5,2]
      timeage<-(early/clients)*100
      paste("Percentage of Client Records NOT entered in Time: ",round(timeage,2),"%")
   })
   output$UDETable <- DT::renderDataTable({
      if(is.null(input$aprZip))
         return(NULL)
      q6b <- allQuestions()[["q6b"]]
      colnames(q6b) <- c("Data Element","Error Count","% of Error Rate")
      datatable(q6b,rownames=TRUE)
   })
   output$PIITable <- DT::renderDataTable({
      if(is.null(input$aprZip))
         return(NULL)
      q6a <- allQuestions()[["q6a"]]
      colnames(q6a) <- c("Data Element","DK/R","Info Missing","Data Issues","% of Error Rate")
      datatable(q6a,rownames=TRUE)
   })
   output$entryGauge <- renderGauge({
      if(is.null(input$aprZip))
         return(NULL)
      a<-allQuestions()[["q6c"]][2,3]
      gauge(a,min=0,max=1,label = "Entry",
            sectors=gaugeSectors(success=c(0,0.3),
                                 warning=c(0.3,0.5),
                                 danger=c(0.5,1)))
   })
   output$annualGauge <- renderGauge({
      if(is.null(input$aprZip))
         return(NULL)
      a<-allQuestions()[["q6c"]][3,3]
      gauge(a,min=0,max=1,label = "Annual",
            sectors=gaugeSectors(success=c(0,0.3),
                                 warning=c(0.3,0.5),
                                 danger=c(0.5,1)))
   })
   output$exitGauge <- renderGauge({
      if(is.null(input$aprZip))
         return(NULL)
      a<-allQuestions()[["q6c"]][4,3]
      gauge(a,min=0,max=1,label = "Exit",
            sectors=gaugeSectors(success=c(0,0.3),
                                 warning=c(0.3,0.5),
                                 danger=c(0.5,1)))
   })
   output$dqchronic <- renderGauge({
      if(is.null(input$aprZip))
         return(NULL)
      a<-allQuestions()[["q6d"]][4,8]
      gauge(a,min=0,max=1,label="Chronic DQ",
            sectors=gaugeSectors(success=c(0,0.3),
                                 warning=c(0.3,0.5),
                                 danger=c(0.5,1)))
   })
   output$dqchronicTable <- DT::renderDataTable({
      if(is.null(input$aprZip))
         return(NULL)
      q6d <- allQuestions()[["q6d"]]
      # print(q6d)
      colnames(q6d) <- c("Project Type Entered","Total Records","Missing time in institution","Missing time in housing",
                         "Approximate Date started DK/R/missing","Number of times DK/R/missing",
                         "Number of months DK/R/missing","% of records unable to calculate")
      datatable(q6d)
   })
   output$piigauge <- renderGauge({
      if(is.null(input$aprZip))
         return(NULL)
      a<-allQuestions()[["q6a"]][7,5]
      gauge(a,min=0,max=1,label="PII DQ",
            sectors=gaugeSectors(success=c(0,0.3),
                                 warning=c(0.3,0.5),
                                 danger=c(0.5,1)))
   })
   output$timeAgauge <- renderGauge({
      if(is.null(input$aprZip))
         return(NULL)
      early<-sum(allQuestions()[["q6e"]][1:2,2])+sum(allQuestions()[["q6e"]][1:2,3])
      clients<-allQuestions()[["q7a"]][5,2]
      timeage<-round((early/clients)*100,2)
      gauge(timeage,min=0,max=1,label = "0-3 days",
            sectors=gaugeSectors(success=c(0.5,1),
                                 warning=c(0.5,0.3),
                                 danger=c(0.3,0)))
   })
   output$timeBgauge <- renderGauge({
      if(is.null(input$aprZip))
         return(NULL)
      early<-sum(allQuestions()[["q6e"]][3:5,2])+sum(allQuestions()[["q6e"]][3:5,3])
      clients<-allQuestions()[["q7a"]][5,2]
      timeage<-round((early/clients)*100,2)
      gauge(timeage,min=0,max=1,label = "More than 3 days",
            sectors=gaugeSectors(success=c(0,0.3),
                                 warning=c(0.3,0.5),
                                 danger=c(0.5,1)))
   })
}

shinyApp(ui = ui, server = server)
