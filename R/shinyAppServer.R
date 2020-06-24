#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#'


# Define server logic required to draw a histogram
shinyAppServer <- function(input, output) {



  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  library(readxl)
  Report <- reactive({
    file1 <- input$uploadedfile
    if(is.null(file1)){return()}
    read_excel(path = file1$datapath,
               skip = 3,
               na = "",
               col_names = c(
                 "Course.Code","Course.Name",
                 "Credit.Hours","Grade",
                 "Point", "Notes", "Result"
               )
    )
  })

  Report1 <- reactive({
    Report <- Report()
    transform(
      as.data.frame(Report),
      StudentID = substr(
        Report[2,2],
        1,
        15
      )
    )
  })

  Report2 <- reactive({
    Report1 <- Report1()
    transform(
      as.data.frame(Report1),
      AcadLevel = Report1[,1]
    )
  })

  Report3 <- reactive({
    Report2 <- Report2()
    transform(
      as.data.frame(Report2),
      AcadLevel = ifelse(
        Report2$AcadLevel %in%
          c('Diploma First Year',
            'Diploma Second Year',
            'Advanced Diploma',
            'Bachelor'
          ),
        as.character(Report2$AcadLevel),
        NA
      )
    )
  })

  library(zoo)

  Report4 <- reactive({
    Report3 <- Report3()
    transform(
      as.data.frame(Report3),
      AcadLevel = na.locf(
        as.character(Report3$AcadLevel),
        fromLast = FALSE
      )
    )
  })

  Report5 <- reactive({
    Report4 <- Report4()
    transform(
      as.data.frame(Report4),
      AYSem = substr(Report4[,1],
                     3,
                     26
      )
    )
  })


  Report6 <- reactive({
    Report5 <- Report5()
    transform(
      as.data.frame(Report5),
      Yearonly = substr(Report5[,1],
                        3,
                        6
      )
    )
  })

  Report7 <- reactive({
    Report6 <- Report6()
    transform(
      as.data.frame(Report6),
      Yearonly = as.numeric(as.character(Report6$Yearonly))
    )
  })

  Report8 <- reactive({
    Report7 <- Report7()
    tail(Report7, -6)
  })


  Report9 <- reactive({
    Report8 <- Report8()
    transform(
      as.data.frame(Report8),
      Yearonly = ifelse(
        test = Report8$Yearonly != "NA",
        yes = as.character(Report8$AYSem),
        no = "NA"
      )
    )
  })

  Report10 <- reactive({
    Report9 <- Report9()
    transform(
      as.data.frame(Report9),
      Yearonly = na.locf(as.character(Report9$Yearonly),fromLast = FALSE)
    )
  })

  Report11 <- reactive({
    Report10 <- Report10()
    Report10[,-10]
  })

  Report12 <- reactive({
    Report11 <- Report11()
    transform(as.data.frame(Report11),
              Credit.Hours = as.numeric(Credit.Hours))

  })

  Report13 <- reactive({
    Report12 <- Report12()
    Report12[complete.cases(Report12[ , 3]),]
  })

  Report14 <- reactive({
    Report13 <- Report13()
    Report13[complete.cases(Report13[ , 7]),]
  })

  library(data.table)

  Report15 <- reactive({
    Report14 <- Report14()
    columnNames <- c("Course.Code", "Course.Name",
                     "Credit.Hours", "Grade", "Point",
                     "Notes", "Result", "Student.ID",
                     "AcadLevel", "AYSemesterNo")

    setnames(Report14, columnNames)

  })


  Report16 <- reactive({
    Report15 <- Report15()
    transform(as.data.frame(Report15),
              Point=round(as.numeric(Report15$Point), 2))
  })


  Report17 <- reactive({
    Report16 <- Report16()
    Report16[ ! Report16$Course.Name %in% c("Pure Math", "Applied Math"), ]
  })

  Report18 <- reactive({
    Report17 <- Report17()
    Report17[ ! Report17$Notes %in% c("NC"), ]
  })

  Report19 <- reactive({
    Report18 <- Report18()
    transform(
      as.data.frame(Report18),
      Notes = ifelse(
        test = is.na(Report18$Notes),
        yes = "",
        no = as.character(Report18$Notes)
      )
    )
  })


  Report20 <- reactive({
    Report19 <- Report19()
    Report19[ , c(8:10,1:7) ]
  })

  #Persistent data storage####
  # observeEvent(input$uploadedfile, {
  #   saveData(Report20())
  # })

  # observeEvent(input$uploadedfile, {
  #   loadData()
  # }) #invoke the two output directories and two functions saveData and loadData
  #

  #Semester GPA Calculations####
  library(dplyr)

  Report21 <- reactive({
    Report20 <- Report20()
    Report20 %>%
      group_by(.dots=c("AYSemesterNo"))%>%
      mutate(SemGPA1=sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })

  Report22 <- reactive({
    Report21 <- Report21()
    unique(Report21[,c(1,2,3,11)])

  })






  Report23 <- reactive({
    Report20 <- Report20()
    Report20 %>%
      group_by(.dots=c("AcadLevel","AYSemesterNo"))%>%
      mutate(SemGPA2=sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })

  Report24 <- reactive({
    Report23 <- Report23()
    unique(Report23[,c(1,2,3,11)])
  })

  Report23new <- reactive({
    Report23 <- Report23()
    transform(as.data.frame(Report23),
              AcadLevel = ifelse(Report23$AcadLevel=="Advanced Diploma",
                                 "Advanced Diploma",
                                 ifelse(Report23$AcadLevel=="Bachelor",
                                        "Bachelor",
                                        ifelse(Report23$AcadLevel=="Diploma First Year",
                                               "Diploma Level",
                                               ifelse(Report23$AcadLevel=="Diploma Second Year",
                                                      "Diploma Level","Check"))))
    )
  })


  #for Level GPA Calculations data pre-processing
  #1Sem####
  Report25 <- reactive({
    Report23new <- Report23new()
    transform(Report23new,
              AYSemesterID=as.numeric(factor(
                Report23new$AYSemesterNo)))
  })

  Report26 <- reactive({
    Report25 <- Report25()
    transform(Report25,
              After1Sem=ifelse(
                AYSemesterID==1,1,NA))
  })

  Report27 <- reactive({
    Report26 <- Report26()
    Report26[complete.cases(Report26[ , 13]),]
  })

  Report28 <- reactive({
    Report27 <- Report27()
    Report27 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })

  Report29 <- reactive({
    Report28 <- Report28()
    subset(Report28,
           AYSemesterID==max(
             Report28$AYSemesterID),
           select=Student.ID:LGPA)
  })

  Report30 <- reactive({
    Report29 <- Report29()
    unique(Report29[,c(1:3,14)])
  })

  Report31 <- reactive({
    Report30 <- Report30()
    na.omit(Report30)
  })

  #2Sem####

  Report32 <- reactive({
    Report25 <- Report25()
    transform(Report25,
              After1Sem=ifelse(
                AYSemesterID==1 | AYSemesterID==2,
                yes = 2,no = NA))
  })

  Report33 <- reactive({
    Report32 <- Report32()
    Report32[complete.cases(Report32[ , 13]),]
  })

  Report34 <- reactive({
    Report33 <- Report33()
    Report33[which(duplicated(
      Report33$Course.Code) | duplicated(
        Report33$Course.Code, fromLast = TRUE)),]
  })

  Report35 <- reactive({
    Report34 <- Report34()
    unique(Report34[duplicated(
      Report34$Course.Code,
      fromLast = FALSE),1:13])
  })

  Report36 <- reactive({
    Report34 <- Report34()
    Report35 <- Report35()
    setdiff(Report34,Report35)
  })

  Report37 <- reactive({
    Report33 <- Report33()
    Report36 <- Report36()
    setdiff(Report33,Report36)
  })

  Report38 <- reactive({
    Report37 <- Report37()
    Report37 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  Report39 <- reactive({
    Report38 <- Report38()
    subset(Report38,
           AYSemesterID==max(
             Report38$AYSemesterID),
           select=Student.ID:LGPA)
  })

  Report40 <- reactive({
    Report31 <- Report31()
    Report39 <- Report39()
    rbind(as.data.frame(Report31),
          as.data.frame(unique(
            Report39[,c(1:3,14)])))
  })

  Report41 <- reactive({
    Report40 <- Report40()
    na.omit(Report40)
  })

  #Sem3
  Report42 <- reactive({
    Report25 <- Report25()
    transform(Report25,
              After1Sem=ifelse(
                AYSemesterID==1 | AYSemesterID==2| AYSemesterID==3,
                yes = 3,no = NA))
  })

  Report43 <- reactive({
    Report42 <- Report42()
    Report42[complete.cases(Report42[ , 13]),]
  })

  Report44 <- reactive({
    Report43 <- Report43()
    Report43[which(duplicated(
      Report43$Course.Code) | duplicated(
        Report43$Course.Code, fromLast = TRUE)),]
  })

  Report45 <- reactive({
    Report44 <- Report44()
    unique(Report44[duplicated(
      Report44$Course.Code,
      fromLast = FALSE),1:13])
  })

  Report46 <- reactive({
    Report44 <- Report44()
    Report45 <- Report45()
    setdiff(Report44,Report45)
  })

  Report47 <- reactive({
    Report43 <- Report43()
    Report46 <- Report46()
    setdiff(Report43,Report46)
  })

  Report48 <- reactive({
    Report47 <- Report47()
    Report47 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  Report49 <- reactive({
    Report48 <- Report48()
    subset(Report48,
           AYSemesterID==max(
             Report48$AYSemesterID),
           select=Student.ID:LGPA)
  })

  Report50 <- reactive({
    Report41 <- Report41()
    Report49 <- Report49()
    rbind(as.data.frame(Report41),
          as.data.frame(unique(
            Report49[,c(1:3,14)])))
  })

  Report51 <- reactive({
    Report50 <- Report50()
    na.omit(Report50)
  })

  #4Sem####

  Report52 <- reactive({
    Report25 <- Report25()
    transform(Report25,
              After1Sem=ifelse(
                AYSemesterID==1 | AYSemesterID==2|
                  AYSemesterID==3| AYSemesterID==4,
                yes = 4,no = NA))
  })

  Report53 <- reactive({
    Report52 <- Report52()
    Report52[complete.cases(Report52[ , 13]),]
  })

  Report54 <- reactive({
    Report53 <- Report53()
    Report53[which(duplicated(
      Report53$Course.Code) | duplicated(
        Report53$Course.Code, fromLast = TRUE)),]
  })

  Report55 <- reactive({
    Report54 <- Report54()
    unique(Report54[duplicated(
      Report54$Course.Code,
      fromLast = FALSE),1:13])
  })

  Report56 <- reactive({
    Report54 <- Report54()
    Report55 <- Report55()
    setdiff(Report54,Report55)
  })

  Report57 <- reactive({
    Report53 <- Report53()
    Report56 <- Report56()
    setdiff(Report53,Report56)
  })

  Report58 <- reactive({
    Report57 <- Report57()
    Report57 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  Report59 <- reactive({
    Report58 <- Report58()
    subset(Report58,
           AYSemesterID==max(
             Report58$AYSemesterID),
           select=Student.ID:LGPA)
  })

  Report60 <- reactive({
    Report51 <- Report51()
    Report59 <- Report59()
    rbind(as.data.frame(Report51),
          as.data.frame(unique(
            Report59[,c(1:3,14)])))
  })

  Report61 <- reactive({
    Report60 <- Report60()
    unique(na.omit(Report60))
  })

  #5Sem####
  Report62 <- reactive({
    Report25 <- Report25()
    transform(Report25,
              After1Sem=ifelse(
                AYSemesterID==1 | AYSemesterID==2|
                  AYSemesterID==3| AYSemesterID==4|
                  AYSemesterID==5,
                yes = 5,no = NA))
  })

  Report63 <- reactive({
    Report62 <- Report62()
    Report62[complete.cases(Report62[ , 13]),]
  })

  Report64 <- reactive({
    Report63 <- Report63()
    Report63[which(duplicated(
      Report63$Course.Code) | duplicated(
        Report63$Course.Code, fromLast = TRUE)),]
  })

  Report65 <- reactive({
    Report64 <- Report64()
    unique(Report64[duplicated(
      Report64$Course.Code,
      fromLast = FALSE),1:13])
  })

  Report66 <- reactive({
    Report64 <- Report64()
    Report65 <- Report65()
    setdiff(Report64,Report65)
  })

  Report67 <- reactive({
    Report63 <- Report63()
    Report66 <- Report66()
    setdiff(Report63,Report66)
  })

  Report68 <- reactive({
    Report67 <- Report67()
    Report67 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  Report69 <- reactive({
    Report68 <- Report68()
    subset(Report68,
           AYSemesterID==max(
             Report68$AYSemesterID),
           select=Student.ID:LGPA)
  })

  Report70 <- reactive({
    Report61 <- Report61()
    Report69 <- Report69()
    rbind(as.data.frame(Report61),
          as.data.frame(unique(
            Report69[,c(1:3,14)])))
  })

  Report71 <- reactive({
    Report70 <- Report70()
    unique(na.omit(Report70))
  })

  #6Sem####

  Report72 <- reactive({
    Report25 <- Report25()
    transform(Report25,
              After1Sem=ifelse(
                AYSemesterID==1 | AYSemesterID==2|
                  AYSemesterID==3| AYSemesterID==4|
                  AYSemesterID==5| AYSemesterID==6,
                yes = 6,no = NA))
  })

  Report73 <- reactive({
    Report72 <- Report72()
    Report72[complete.cases(Report72[ , 13]),]
  })

  Report74 <- reactive({
    Report73 <- Report73()
    Report73[which(duplicated(
      Report73$Course.Code) | duplicated(
        Report73$Course.Code, fromLast = TRUE)),]
  })

  Report75 <- reactive({
    Report74 <- Report74()
    unique(Report74[duplicated(
      Report74$Course.Code,
      fromLast = FALSE),1:13])
  })

  Report76 <- reactive({
    Report74 <- Report74()
    Report75 <- Report75()
    setdiff(Report74,Report75)
  })

  Report77 <- reactive({
    Report73 <- Report73()
    Report76 <- Report76()
    setdiff(Report73,Report76)
  })

  Report78 <- reactive({
    Report77 <- Report77()
    Report77 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  Report79 <- reactive({
    Report78 <- Report78()
    subset(Report78,
           AYSemesterID==max(
             Report78$AYSemesterID),
           select=Student.ID:LGPA)
  })

  Report80 <- reactive({
    Report71 <- Report71()
    Report79 <- Report79()
    rbind(as.data.frame(Report71),
          as.data.frame(unique(
            Report79[,c(1:3,14)])))
  })

  Report81 <- reactive({
    Report80 <- Report80()
    unique(na.omit(Report80))
  })

  #7Sem####
  Report82 <- reactive({
    Report25 <- Report25()
    transform(Report25,
              After1Sem=ifelse(
                AYSemesterID==1 | AYSemesterID==2|
                  AYSemesterID==3| AYSemesterID==4|
                  AYSemesterID==5| AYSemesterID==6|
                  AYSemesterID==7,
                yes = 7,no = NA))
  })

  Report83 <- reactive({
    Report82 <- Report82()
    Report82[complete.cases(Report82[ , 13]),]
  })

  Report84 <- reactive({
    Report83 <- Report83()
    Report83[which(duplicated(
      Report83$Course.Code) | duplicated(
        Report83$Course.Code, fromLast = TRUE)),]
  })

  Report85 <- reactive({
    Report84 <- Report84()
    unique(Report84[duplicated(
      Report84$Course.Code,
      fromLast = FALSE),1:13])
  })

  Report86 <- reactive({
    Report84 <- Report84()
    Report85 <- Report85()
    setdiff(Report84,Report85)
  })

  Report87 <- reactive({
    Report83 <- Report83()
    Report86 <- Report86()
    setdiff(Report83,Report86)
  })

  Report88 <- reactive({
    Report87 <- Report87()
    Report87 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  Report89 <- reactive({
    Report88 <- Report88()
    subset(Report88,
           AYSemesterID==max(
             Report88$AYSemesterID),
           select=Student.ID:LGPA)
  })

  Report90 <- reactive({
    Report81 <- Report81()
    Report89 <- Report89()
    rbind(as.data.frame(Report81),
          as.data.frame(unique(
            Report89[,c(1:3,14)])))
  })

  Report91 <- reactive({
    Report90 <- Report90()
    unique(na.omit(Report90))
  })



  #8Sem####

  Report92 <- reactive({
    Report25 <- Report25()
    transform(Report25,
              After1Sem=ifelse(
                AYSemesterID==1 | AYSemesterID==2|
                  AYSemesterID==3| AYSemesterID==4|
                  AYSemesterID==5| AYSemesterID==6|
                  AYSemesterID==7| AYSemesterID==8,
                yes = 8,no = NA))
  })

  Report93 <- reactive({
    Report92 <- Report92()
    Report92[complete.cases(Report92[ , 13]),]
  })

  Report94 <- reactive({
    Report93 <- Report93()
    Report93[which(duplicated(
      Report93$Course.Code) | duplicated(
        Report93$Course.Code, fromLast = TRUE)),]
  })

  Report95 <- reactive({
    Report94 <- Report94()
    unique(Report94[duplicated(
      Report94$Course.Code,
      fromLast = FALSE),1:13])
  })

  Report96 <- reactive({
    Report94 <- Report94()
    Report95 <- Report95()
    setdiff(Report94,Report95)
  })

  Report97 <- reactive({
    Report93 <- Report93()
    Report96 <- Report96()
    setdiff(Report93,Report96)
  })

  Report98 <- reactive({
    Report97 <- Report97()
    Report97 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  Report99 <- reactive({
    Report98 <- Report98()
    subset(Report98,
           AYSemesterID==max(
             Report98$AYSemesterID),
           select=Student.ID:LGPA)
  })

  Report100 <- reactive({
    Report91 <- Report91()
    Report99 <- Report99()
    rbind(as.data.frame(Report91),
          as.data.frame(unique(
            Report99[,c(1:3,14)])))
  })

  Report101 <- reactive({
    Report100 <- Report100()
    unique(na.omit(Report100))
  })

  #9Sem####
  Report102 <- reactive({
    Report25 <- Report25()
    transform(Report25,
              After1Sem=ifelse(
                AYSemesterID==1 | AYSemesterID==2|
                  AYSemesterID==3| AYSemesterID==4|
                  AYSemesterID==5| AYSemesterID==6|
                  AYSemesterID==7| AYSemesterID==8|
                  AYSemesterID==9,
                yes = 9,no = NA))
  })

  Report103 <- reactive({
    Report102 <- Report102()
    Report102[complete.cases(Report102[ , 13]),]
  })

  Report104 <- reactive({
    Report103 <- Report103()
    Report103[which(duplicated(
      Report103$Course.Code) | duplicated(
        Report103$Course.Code, fromLast = TRUE)),]
  })

  Report105 <- reactive({
    Report104 <- Report104()
    unique(Report104[duplicated(
      Report104$Course.Code,
      fromLast = FALSE),1:13])
  })

  Report106 <- reactive({
    Report104 <- Report104()
    Report105 <- Report105()
    setdiff(Report104,Report105)
  })

  Report107 <- reactive({
    Report103 <- Report103()
    Report106 <- Report106()
    setdiff(Report103,Report106)
  })

  Report108 <- reactive({
    Report107 <- Report107()
    Report107 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  Report109 <- reactive({
    Report108 <- Report108()
    subset(Report108,
           AYSemesterID==max(
             Report108$AYSemesterID),
           select=Student.ID:LGPA)
  })

  Report110 <- reactive({
    Report101 <- Report101()
    Report109 <- Report109()
    rbind(as.data.frame(Report101),
          as.data.frame(unique(
            Report109[,c(1:3,14)])))
  })

  Report111 <- reactive({
    Report110 <- Report110()
    unique(na.omit(Report110))
  })

  #10Sem####
  Report112 <- reactive({
    Report25 <- Report25()
    transform(Report25,
              After1Sem=ifelse(
                AYSemesterID==1 | AYSemesterID==2|
                  AYSemesterID==3| AYSemesterID==4|
                  AYSemesterID==5| AYSemesterID==6|
                  AYSemesterID==7| AYSemesterID==8|
                  AYSemesterID==9| AYSemesterID==10,
                yes = 10,no = NA))
  })

  Report113 <- reactive({
    Report112 <- Report112()
    Report112[complete.cases(Report112[ , 13]),]
  })

  Report114 <- reactive({
    Report113 <- Report113()
    Report113[which(duplicated(
      Report113$Course.Code) | duplicated(
        Report113$Course.Code, fromLast = TRUE)),]
  })

  Report115 <- reactive({
    Report114 <- Report114()
    unique(Report114[duplicated(
      Report114$Course.Code,
      fromLast = FALSE),1:13])
  })

  Report116 <- reactive({
    Report114 <- Report114()
    Report115 <- Report115()
    setdiff(Report114,Report115)
  })

  Report117 <- reactive({
    Report113 <- Report113()
    Report116 <- Report116()
    setdiff(Report113,Report116)
  })

  Report118 <- reactive({
    Report117 <- Report117()
    Report117 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  Report119 <- reactive({
    Report118 <- Report118()
    subset(Report118,
           AYSemesterID==max(
             Report118$AYSemesterID),
           select=Student.ID:LGPA)
  })

  Report120 <- reactive({
    Report111 <- Report111()
    Report119 <- Report119()
    rbind(as.data.frame(Report111),
          as.data.frame(unique(
            Report119[,c(1:3,14)])))
  })

  Report121 <- reactive({
    Report120 <- Report120()
    unique(na.omit(Report120))
  })

  #11Sem####

  Report122 <- reactive({
    Report25 <- Report25()
    transform(Report25,
              After1Sem=ifelse(
                AYSemesterID==1 | AYSemesterID==2|
                  AYSemesterID==3| AYSemesterID==4|
                  AYSemesterID==5| AYSemesterID==6|
                  AYSemesterID==7| AYSemesterID==8|
                  AYSemesterID==9| AYSemesterID==10|
                  AYSemesterID==11,
                yes = 11,no = NA))
  })

  Report123 <- reactive({
    Report122 <- Report122()
    Report122[complete.cases(Report122[ , 13]),]
  })

  Report124 <- reactive({
    Report123 <- Report123()
    Report123[which(duplicated(
      Report123$Course.Code) | duplicated(
        Report123$Course.Code, fromLast = TRUE)),]
  })

  Report125 <- reactive({
    Report124 <- Report124()
    unique(Report124[duplicated(
      Report124$Course.Code,
      fromLast = FALSE),1:13])
  })

  Report126 <- reactive({
    Report124 <- Report124()
    Report125 <- Report125()
    setdiff(Report124,Report125)
  })

  Report127 <- reactive({
    Report123 <- Report123()
    Report126 <- Report126()
    setdiff(Report123,Report126)
  })

  Report128 <- reactive({
    Report127 <- Report127()
    Report127 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  Report129 <- reactive({
    Report128 <- Report128()
    subset(Report128,
           AYSemesterID==max(
             Report128$AYSemesterID),
           select=Student.ID:LGPA)
  })

  Report130 <- reactive({
    Report121 <- Report121()
    Report129 <- Report129()
    rbind(as.data.frame(Report121),
          as.data.frame(unique(
            Report129[,c(1:3,14)])))
  })

  Report131 <- reactive({
    Report130 <- Report130()
    unique(na.omit(Report130))
  })

  #12Sem####

  Report132 <- reactive({
    Report25 <- Report25()
    transform(Report25,
              After1Sem=ifelse(
                AYSemesterID==1 | AYSemesterID==2|
                  AYSemesterID==3| AYSemesterID==4|
                  AYSemesterID==5| AYSemesterID==6|
                  AYSemesterID==7| AYSemesterID==8|
                  AYSemesterID==9| AYSemesterID==10|
                  AYSemesterID==11| AYSemesterID==12,
                yes = 12,no = NA))
  })

  Report133 <- reactive({
    Report132 <- Report132()
    Report132[complete.cases(Report132[ , 13]),]
  })

  Report134 <- reactive({
    Report133 <- Report133()
    Report133[which(duplicated(
      Report133$Course.Code) | duplicated(
        Report133$Course.Code, fromLast = TRUE)),]
  })

  Report135 <- reactive({
    Report134 <- Report134()
    unique(Report134[duplicated(
      Report134$Course.Code,
      fromLast = FALSE),1:13])
  })

  Report136 <- reactive({
    Report134 <- Report134()
    Report135 <- Report135()
    setdiff(Report134,Report135)
  })

  Report137 <- reactive({
    Report133 <- Report133()
    Report136 <- Report136()
    setdiff(Report133,Report136)
  })

  Report138 <- reactive({
    Report137 <- Report137()
    Report137 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  Report139 <- reactive({
    Report138 <- Report138()
    subset(Report138,
           AYSemesterID==max(
             Report138$AYSemesterID),
           select=Student.ID:LGPA)
  })

  Report140 <- reactive({
    Report131 <- Report131()
    Report139 <- Report139()
    rbind(as.data.frame(Report131),
          as.data.frame(unique(
            Report139[,c(1:3,14)])))
  })

  Report141 <- reactive({
    Report140 <- Report140()
    unique(na.omit(Report140))
  })

  #13Sem####

  Report142 <- reactive({
    Report25 <- Report25()
    transform(Report25,
              After1Sem=ifelse(
                AYSemesterID==1 | AYSemesterID==2|
                  AYSemesterID==3| AYSemesterID==4|
                  AYSemesterID==5| AYSemesterID==6|
                  AYSemesterID==7| AYSemesterID==8|
                  AYSemesterID==9| AYSemesterID==10|
                  AYSemesterID==11| AYSemesterID==12|
                  AYSemesterID==13,
                yes = 13,no = NA))
  })

  Report143 <- reactive({
    Report142 <- Report142()
    Report142[complete.cases(Report142[ , 13]),]
  })

  Report144 <- reactive({
    Report143 <- Report143()
    Report143[which(duplicated(
      Report143$Course.Code) | duplicated(
        Report143$Course.Code, fromLast = TRUE)),]
  })

  Report145 <- reactive({
    Report144 <- Report144()
    unique(Report144[duplicated(
      Report144$Course.Code,
      fromLast = FALSE),1:13])
  })

  Report146 <- reactive({
    Report144 <- Report144()
    Report145 <- Report145()
    setdiff(Report144,Report145)
  })

  Report147 <- reactive({
    Report143 <- Report143()
    Report146 <- Report146()
    setdiff(Report143,Report146)
  })

  Report148 <- reactive({
    Report147 <- Report147()
    Report147 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  Report149 <- reactive({
    Report148 <- Report148()
    subset(Report148,
           AYSemesterID==max(
             Report148$AYSemesterID),
           select=Student.ID:LGPA)
  })

  Report150 <- reactive({
    Report141 <- Report141()
    Report149 <- Report149()
    rbind(as.data.frame(Report141),
          as.data.frame(unique(
            Report149[,c(1:3,14)])))
  })

  Report151 <- reactive({
    Report150 <- Report150()
    unique(na.omit(Report150))
  })

  #14Sem####

  Report152 <- reactive({
    Report25 <- Report25()
    transform(Report25,
              After1Sem=ifelse(
                AYSemesterID==1 | AYSemesterID==2|
                  AYSemesterID==3| AYSemesterID==4|
                  AYSemesterID==5| AYSemesterID==6|
                  AYSemesterID==7| AYSemesterID==8|
                  AYSemesterID==9| AYSemesterID==10|
                  AYSemesterID==11| AYSemesterID==12|
                  AYSemesterID==13| AYSemesterID==14,
                yes = 14,no = NA))
  })

  Report153 <- reactive({
    Report152 <- Report152()
    Report152[complete.cases(Report152[ , 13]),]
  })

  Report154 <- reactive({
    Report153 <- Report153()
    Report153[which(duplicated(
      Report153$Course.Code) | duplicated(
        Report153$Course.Code, fromLast = TRUE)),]
  })

  Report155 <- reactive({
    Report154 <- Report154()
    unique(Report154[duplicated(
      Report154$Course.Code,
      fromLast = FALSE),1:13])
  })

  Report156 <- reactive({
    Report154 <- Report154()
    Report155 <- Report155()
    setdiff(Report154,Report155)
  })

  Report157 <- reactive({
    Report153 <- Report153()
    Report156 <- Report156()
    setdiff(Report153,Report156)
  })

  Report158 <- reactive({
    Report157 <- Report157()
    Report157 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  Report159 <- reactive({
    Report158 <- Report158()
    subset(Report158,
           AYSemesterID==max(
             Report158$AYSemesterID),
           select=Student.ID:LGPA)
  })

  Report160 <- reactive({
    Report151 <- Report151()
    Report159 <- Report159()
    rbind(as.data.frame(Report151),
          as.data.frame(unique(
            Report159[,c(1:3,14)])))
  })

  Report161 <- reactive({
    Report160 <- Report160()
    unique(na.omit(Report160))
  })

  #15Sem####

  Report162 <- reactive({
    Report25 <- Report25()
    transform(Report25,
              After1Sem=ifelse(
                AYSemesterID==1 | AYSemesterID==2|
                  AYSemesterID==3| AYSemesterID==4|
                  AYSemesterID==5| AYSemesterID==6|
                  AYSemesterID==7| AYSemesterID==8|
                  AYSemesterID==9| AYSemesterID==10|
                  AYSemesterID==11| AYSemesterID==12|
                  AYSemesterID==13| AYSemesterID==14|
                  AYSemesterID==15,
                yes = 15,no = NA))
  })

  Report163 <- reactive({
    Report162 <- Report162()
    Report162[complete.cases(Report162[ , 13]),]
  })

  Report164 <- reactive({
    Report163 <- Report163()
    Report163[which(duplicated(
      Report163$Course.Code) | duplicated(
        Report163$Course.Code, fromLast = TRUE)),]
  })

  Report165 <- reactive({
    Report164 <- Report164()
    unique(Report164[duplicated(
      Report164$Course.Code,
      fromLast = FALSE),1:13])
  })

  Report166 <- reactive({
    Report164 <- Report164()
    Report165 <- Report165()
    setdiff(Report164,Report165)
  })

  Report167 <- reactive({
    Report163 <- Report163()
    Report166 <- Report166()
    setdiff(Report163,Report166)
  })

  Report168 <- reactive({
    Report167 <- Report167()
    Report167 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  Report169 <- reactive({
    Report168 <- Report168()
    subset(Report168,
           AYSemesterID==max(
             Report168$AYSemesterID),
           select=Student.ID:LGPA)
  })

  Report170 <- reactive({
    Report161 <- Report161()
    Report169 <- Report169()
    rbind(as.data.frame(Report161),
          as.data.frame(unique(
            Report169[,c(1:3,14)])))
  })

  Report171 <- reactive({
    Report170 <- Report170()
    unique(na.omit(Report170))
  })

  #16Sem####

  Report172 <- reactive({
    Report25 <- Report25()
    transform(Report25,
              After1Sem=ifelse(
                AYSemesterID==1 | AYSemesterID==2|
                  AYSemesterID==3| AYSemesterID==4|
                  AYSemesterID==5| AYSemesterID==6|
                  AYSemesterID==7| AYSemesterID==8|
                  AYSemesterID==9| AYSemesterID==10|
                  AYSemesterID==11| AYSemesterID==12|
                  AYSemesterID==13| AYSemesterID==14|
                  AYSemesterID==15 | AYSemesterID==16,
                yes = 16,no = NA))
  })

  Report173 <- reactive({
    Report172 <- Report172()
    Report172[complete.cases(Report172[ , 13]),]
  })

  Report174 <- reactive({
    Report173 <- Report173()
    Report173[which(duplicated(
      Report173$Course.Code) | duplicated(
        Report173$Course.Code, fromLast = TRUE)),]
  })

  Report175 <- reactive({
    Report174 <- Report174()
    unique(Report174[duplicated(
      Report174$Course.Code,
      fromLast = FALSE),1:13])
  })

  Report176 <- reactive({
    Report174 <- Report174()
    Report175 <- Report175()
    setdiff(Report174,Report175)
  })

  Report177 <- reactive({
    Report173 <- Report173()
    Report176 <- Report176()
    setdiff(Report173,Report176)
  })

  Report178 <- reactive({
    Report177 <- Report177()
    Report177 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  Report179 <- reactive({
    Report178 <- Report178()
    subset(Report178,
           AYSemesterID==max(
             Report178$AYSemesterID),
           select=Student.ID:LGPA)
  })

  Report180 <- reactive({
    Report171 <- Report171()
    Report179 <- Report179()
    rbind(as.data.frame(Report171),
          as.data.frame(unique(
            Report179[,c(1:3,14)])))
  })

  Report181 <- reactive({
    Report180 <- Report180()
    unique(na.omit(Report180))
  })


  #17Sem####

  Report182 <- reactive({
    Report25 <- Report25()
    transform(Report25,
              After1Sem=ifelse(
                AYSemesterID==1 | AYSemesterID==2|
                  AYSemesterID==3| AYSemesterID==4|
                  AYSemesterID==5| AYSemesterID==6|
                  AYSemesterID==7| AYSemesterID==8|
                  AYSemesterID==9| AYSemesterID==10|
                  AYSemesterID==11| AYSemesterID==12|
                  AYSemesterID==13| AYSemesterID==14|
                  AYSemesterID==15 | AYSemesterID==16|
                  AYSemesterID==17,
                yes = 17,no = NA))
  })

  Report183 <- reactive({
    Report182 <- Report182()
    Report182[complete.cases(Report182[ , 13]),]
  })

  Report184<- reactive({
    Report183 <- Report183()
    Report183[which(duplicated(
      Report183$Course.Code) | duplicated(
        Report183$Course.Code, fromLast = TRUE)),]
  })

  Report185 <- reactive({
    Report184 <- Report184()
    unique(Report184[duplicated(
      Report184$Course.Code,
      fromLast = FALSE),1:13])
  })

  Report186 <- reactive({
    Report184 <- Report184()
    Report185 <- Report185()
    setdiff(Report184,
            Report185)
  })

  Report187 <- reactive({
    Report183 <- Report183()
    Report186 <- Report186()
    setdiff(Report183,
            Report186)
  })

  Report188 <- reactive({
    Report187 <- Report187()
    Report187 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  Report189 <- reactive({
    Report188 <- Report188()
    subset(Report188,
           AYSemesterID==max(
             Report188$AYSemesterID),
           select=Student.ID:LGPA)
  })

  Report190 <- reactive({
    Report181 <- Report181()
    Report189 <- Report189()
    rbind(as.data.frame(Report181),
          as.data.frame(unique(
            Report189[,c(1:3,14)])))
  })

  Report191 <- reactive({
    Report190 <-   Report190()
    unique(na.omit(Report190))
  })

  #18Sem####
  Report192 <- reactive({
    Report25 <- Report25()
    transform(Report25,
              After1Sem=ifelse(
                AYSemesterID==1 | AYSemesterID==2|
                  AYSemesterID==3| AYSemesterID==4|
                  AYSemesterID==5| AYSemesterID==6|
                  AYSemesterID==7| AYSemesterID==8|
                  AYSemesterID==9| AYSemesterID==10|
                  AYSemesterID==11| AYSemesterID==12|
                  AYSemesterID==13| AYSemesterID==14|
                  AYSemesterID==15 | AYSemesterID==16|
                  AYSemesterID==17| AYSemesterID==18,
                yes = 18,no = NA))
  })

  Report193 <- reactive({
    Report192 <- Report192()
    Report192[complete.cases(
      Report192[ , 13]),]
  })

  Report194<- reactive({
    Report193 <- Report193()
    Report193[which(duplicated(
      Report193$Course.Code) | duplicated(
        Report193$Course.Code, fromLast = TRUE)),]
  })

  Report195 <- reactive({
    Report194 <- Report194()
    unique(Report194[duplicated(
      Report194$Course.Code,
      fromLast = FALSE),1:13])
  })

  Report196 <- reactive({
    Report194 <- Report194()
    Report195 <- Report195()
    setdiff(Report194,
            Report195)
  })

  Report197 <- reactive({
    Report193 <- Report193()
    Report196 <- Report196()
    setdiff(Report193,
            Report196)
  })

  Report198 <- reactive({
    Report197 <- Report197()
    Report197 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  Report199 <- reactive({
    Report198 <- Report198()
    subset(Report198,
           AYSemesterID==max(
             Report198$AYSemesterID),
           select=Student.ID:LGPA)
  })

  Report200 <- reactive({
    Report191 <- Report191()
    Report199 <- Report199()
    rbind(as.data.frame(Report191),
          as.data.frame(unique(
            Report199[,c(1:3,14)])))
  })

  Report201 <- reactive({
    Report200 <-   Report200()
    unique(na.omit(Report200))
  })

  #19Sem####

  Report202 <- reactive({
    Report25 <- Report25()
    transform(Report25,
              After1Sem=ifelse(
                AYSemesterID==1 | AYSemesterID==2|
                  AYSemesterID==3| AYSemesterID==4|
                  AYSemesterID==5| AYSemesterID==6|
                  AYSemesterID==7| AYSemesterID==8|
                  AYSemesterID==9| AYSemesterID==10|
                  AYSemesterID==11| AYSemesterID==12|
                  AYSemesterID==13| AYSemesterID==14|
                  AYSemesterID==15 | AYSemesterID==16|
                  AYSemesterID==17| AYSemesterID==18|
                  AYSemesterID==19,
                yes = 19,no = NA))
  })

  Report203 <- reactive({
    Report202 <- Report202()
    Report202[complete.cases(
      Report202[ , 13]),]
  })

  Report204<- reactive({
    Report203 <- Report203()
    Report203[which(duplicated(
      Report203$Course.Code) | duplicated(
        Report203$Course.Code, fromLast = TRUE)),]
  })

  Report205 <- reactive({
    Report204 <- Report204()
    unique(Report204[duplicated(
      Report204$Course.Code,
      fromLast = FALSE),1:13])
  })

  Report206 <- reactive({
    Report204 <- Report204()
    Report205 <- Report205()
    setdiff(Report204,
            Report205)
  })

  Report207 <- reactive({
    Report203 <- Report203()
    Report206 <- Report206()
    setdiff(Report203,
            Report206)
  })

  Report208 <- reactive({
    Report207 <- Report207()
    Report207 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  Report209 <- reactive({
    Report208 <- Report208()
    subset(Report208,
           AYSemesterID==max(
             Report208$AYSemesterID),
           select=Student.ID:LGPA)
  })

  Report210 <- reactive({
    Report201 <- Report201()
    Report209 <- Report209()
    rbind(as.data.frame(Report201),
          as.data.frame(unique(
            Report209[,c(1:3,14)])))
  })

  Report211 <- reactive({
    Report210 <-   Report210()
    unique(na.omit(Report210))
  })

  #20Sem####

  Report212 <- reactive({
    Report25 <- Report25()
    transform(Report25,
              After1Sem=ifelse(
                AYSemesterID==1 | AYSemesterID==2|
                  AYSemesterID==3| AYSemesterID==4|
                  AYSemesterID==5| AYSemesterID==6|
                  AYSemesterID==7| AYSemesterID==8|
                  AYSemesterID==9| AYSemesterID==10|
                  AYSemesterID==11| AYSemesterID==12|
                  AYSemesterID==13| AYSemesterID==14|
                  AYSemesterID==15 | AYSemesterID==16|
                  AYSemesterID==17| AYSemesterID==18|
                  AYSemesterID==19| AYSemesterID==20,
                yes = 20,no = NA))
  })

  Report213 <- reactive({
    Report212 <- Report212()
    Report212[complete.cases(
      Report212[ , 13]),]
  })

  Report214<- reactive({
    Report213 <- Report213()
    Report213[which(duplicated(
      Report213$Course.Code) | duplicated(
        Report213$Course.Code, fromLast = TRUE)),]
  })

  Report215 <- reactive({
    Report214 <- Report214()
    unique(Report214[duplicated(
      Report214$Course.Code,
      fromLast = FALSE),1:13])
  })

  Report216 <- reactive({
    Report214 <- Report214()
    Report215 <- Report215()
    setdiff(Report214,
            Report215)
  })

  Report217 <- reactive({
    Report213 <- Report213()
    Report216 <- Report216()
    setdiff(Report213,
            Report216)
  })

  Report218 <- reactive({
    Report217 <- Report217()
    Report217 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  Report219 <- reactive({
    Report218 <- Report218()
    subset(Report218,
           AYSemesterID==max(
             Report218$AYSemesterID),
           select=Student.ID:LGPA)
  })

  Report220 <- reactive({
    Report211 <- Report211()
    Report219 <- Report219()
    rbind(as.data.frame(Report211),
          as.data.frame(unique(
            Report219[,c(1:3,14)])))
  })

  Report221 <- reactive({
    Report220 <-   Report220()
    unique(na.omit(Report220))
  })
  #21Sem####

  Report222 <- reactive({
    Report25 <- Report25()
    transform(Report25,
              After1Sem=ifelse(
                AYSemesterID==1 | AYSemesterID==2|
                  AYSemesterID==3| AYSemesterID==4|
                  AYSemesterID==5| AYSemesterID==6|
                  AYSemesterID==7| AYSemesterID==8|
                  AYSemesterID==9| AYSemesterID==10|
                  AYSemesterID==11| AYSemesterID==12|
                  AYSemesterID==13| AYSemesterID==14|
                  AYSemesterID==15 | AYSemesterID==16|
                  AYSemesterID==17| AYSemesterID==18|
                  AYSemesterID==19| AYSemesterID==20|
                  AYSemesterID==21,
                yes = 21,no = NA))
  })

  Report223 <- reactive({
    Report222 <- Report222()
    Report222[complete.cases(
      Report222[ , 13]),]
  })

  Report224<- reactive({
    Report223 <- Report223()
    Report223[which(duplicated(
      Report223$Course.Code) | duplicated(
        Report223$Course.Code, fromLast = TRUE)),]
  })

  Report225 <- reactive({
    Report224 <- Report224()
    unique(Report224[duplicated(
      Report224$Course.Code,
      fromLast = FALSE),1:13])
  })

  Report226 <- reactive({
    Report224 <- Report224()
    Report225 <- Report225()
    setdiff(Report224,
            Report225)
  })

  Report227 <- reactive({
    Report223 <- Report223()
    Report226 <- Report226()
    setdiff(Report223,
            Report226)
  })

  Report228 <- reactive({
    Report227 <- Report227()
    Report227 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  Report229 <- reactive({
    Report228 <- Report228()
    subset(Report228,
           AYSemesterID==max(
             Report228$AYSemesterID),
           select=Student.ID:LGPA)
  })

  Report230 <- reactive({
    Report221 <- Report221()
    Report229 <- Report229()
    rbind(as.data.frame(Report221),
          as.data.frame(unique(
            Report229[,c(1:3,14)])))
  })

  Report231 <- reactive({
    Report230 <-   Report230()
    unique(na.omit(Report230))
  })

  ####NEW OGPA Calculation Script####
  #for Overall Cumulative GPA @ Diploma First Year
  # Dip1stOGPA <- reactive({
  #   Report20 <- Report20()
  #   transform(as.data.frame(Report20),
  #             AcadLevel = ifelse(Report20$AcadLevel=="Advanced Diploma",
  #                                NA,
  #                                ifelse(Report20$AcadLevel=="Bachelor",
  #                                       NA,
  #                                       ifelse(Report20$AcadLevel=="Diploma First Year",
  #                                              "Diploma First Year",
  #                                              ifelse(Report20$AcadLevel=="Diploma Second Year",
  #                                                     NA,"Check"))))
  #   )
  # })
  #
  # Dip1stOGPA2 <- reactive({
  #   Dip1stOGPA <- Dip1stOGPA()
  #   Dip1stOGPA[complete.cases(Dip1stOGPA[,2]),]
  # })
  #
  # Dip1stOGPA3 <- reactive({
  #   Dip1stOGPA2 <- Dip1stOGPA2()
  #   transform(Dip1stOGPA2,
  #             AYSemesterID=as.numeric(factor(
  #               Dip1stOGPA2$AYSemesterNo)))
  # })
  #
  # Dip1stOGPA4<- reactive({
  #   Dip1stOGPA3 <- Dip1stOGPA3()
  #   Dip1stOGPA3[which(duplicated(
  #     Dip1stOGPA3$Course.Code) | duplicated(
  #       Dip1stOGPA3$Course.Code, fromLast = TRUE)),]
  # })
  #
  # Dip1stOGPA5 <- reactive({
  #   Dip1stOGPA4 <- Dip1stOGPA4()
  #   unique(Dip1stOGPA4[duplicated(
  #     Dip1stOGPA4$Course.Code,
  #     fromLast = FALSE),])
  # })
  #
  # Dip1stOGPA6 <- reactive({
  #   Dip1stOGPA4 <- Dip1stOGPA4()
  #   Dip1stOGPA5 <- Dip1stOGPA5()
  #   setdiff(Dip1stOGPA4,
  #           Dip1stOGPA5)
  # })
  #
  # Dip1stOGPA7 <- reactive({
  #   Dip1stOGPA3 <- Dip1stOGPA3()
  #   Dip1stOGPA6 <- Dip1stOGPA6()
  #   setdiff(Dip1stOGPA3,
  #           Dip1stOGPA6)
  # })
  #
  # Dip1stOGPA8 <- reactive({
  #   Dip1stOGPA7 <- Dip1stOGPA7()
  #   Dip1stOGPA7 %>%
  #     group_by(AcadLevel) %>%
  #     mutate(LGPA=
  #              sum(Credit.Hours*Point,na.rm=T)/
  #              sum(Credit.Hours,na.rm = T))
  # })
  #
  #
  # Dip1stOGPA9 <- reactive({
  #   Dip1stOGPA8 <- Dip1stOGPA8()
  #   subset(Dip1stOGPA8,
  #          AYSemesterID==max(
  #            Dip1stOGPA8$AYSemesterID),
  #          select=Student.ID:LGPA)
  # })
  #
  # Dip1stOGPA10 <- reactive({
  #   Dip1stOGPA9<-Dip1stOGPA9()
  #   Dip1stOGPA9[,c(1:3,12)]
  # })
  #
  # Dip1stOGPA11 <- reactive({
  #   Dip1stOGPA10 <- Dip1stOGPA10()
  #   columnNames <- c("Student ID",
  #                    "Academic Level", "Last Semester on the Level", "Overall Cumulative GPA")
  #
  #   setnames(Dip1stOGPA10, columnNames)
  #
  # })
  #for Overall Cumulative GPA @ Diploma Second Year
  Dip2ndOGPA <- reactive({
    Report20 <- Report20()
    transform(as.data.frame(Report20),
              AcadLevel = ifelse(Report20$AcadLevel=="Advanced Diploma",
                                 NA,
                                 ifelse(Report20$AcadLevel=="Bachelor",
                                        NA,
                                        ifelse(Report20$AcadLevel=="Diploma First Year",
                                               "Diploma Level",
                                               ifelse(Report20$AcadLevel=="Diploma Second Year",
                                                      "Diploma Level","Check"))))
    )
  })

  Dip2ndOGPA2 <- reactive({
    Dip2ndOGPA <- Dip2ndOGPA()
    Dip2ndOGPA[complete.cases(Dip2ndOGPA[,2]),]
  })

  Dip2ndOGPA3 <- reactive({
    Dip2ndOGPA2 <- Dip2ndOGPA2()
    transform(Dip2ndOGPA2,
              AYSemesterID=as.numeric(factor(
                Dip2ndOGPA2$AYSemesterNo)))
  })

  Dip2ndOGPA4<- reactive({
    Dip2ndOGPA3 <- Dip2ndOGPA3()
    Dip2ndOGPA3[which(duplicated(
      Dip2ndOGPA3$Course.Code) | duplicated(
        Dip2ndOGPA3$Course.Code, fromLast = TRUE)),]
  })

  Dip2ndOGPA5 <- reactive({
    Dip2ndOGPA4 <- Dip2ndOGPA4()
    unique(Dip2ndOGPA4[duplicated(
      Dip2ndOGPA4$Course.Code,
      fromLast = FALSE),])
  })

  Dip2ndOGPA6 <- reactive({
    Dip2ndOGPA4 <- Dip2ndOGPA4()
    Dip2ndOGPA5 <- Dip2ndOGPA5()
    setdiff(Dip2ndOGPA4,
            Dip2ndOGPA5)
  })

  Dip2ndOGPA7 <- reactive({
    Dip2ndOGPA3 <- Dip2ndOGPA3()
    Dip2ndOGPA6 <- Dip2ndOGPA6()
    setdiff(Dip2ndOGPA3,
            Dip2ndOGPA6)
  })

  Dip2ndOGPA8 <- reactive({
    Dip2ndOGPA7 <- Dip2ndOGPA7()
    Dip2ndOGPA7 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  Dip2ndOGPA9 <- reactive({
    Dip2ndOGPA8 <- Dip2ndOGPA8()
    subset(Dip2ndOGPA8,
           AYSemesterID==max(
             Dip2ndOGPA8$AYSemesterID),
           select=Student.ID:LGPA)
  })

  Dip2ndOGPA10 <- reactive({
    Dip2ndOGPA9<-Dip2ndOGPA9()
    Dip2ndOGPA9[,c(1:3,12)]
  })

  Dip2ndOGPA11 <- reactive({
    Dip2ndOGPA10 <- Dip2ndOGPA10()
    columnNames <- c("Student ID",
                     "Academic Level", "Last Semester on the Level", "Overall Cumulative GPA")

    setnames(Dip2ndOGPA10, columnNames)

  })
  #for Overall Cumulative GPA @ Advanced Diploma
  AdvOGPA <- reactive({
    Report20 <- Report20()
    transform(as.data.frame(Report20),
              AcadLevel = ifelse(Report20$AcadLevel=="Advanced Diploma",
                                 "Advanced Diploma",
                                 ifelse(Report20$AcadLevel=="Bachelor",
                                        NA,
                                        ifelse(Report20$AcadLevel=="Diploma First Year",
                                               "Advanced Diploma",
                                               ifelse(Report20$AcadLevel=="Diploma Second Year",
                                                      "Advanced Diploma","Check"))))
    )
  })

  AdvOGPA2 <- reactive({
    AdvOGPA <- AdvOGPA()
    AdvOGPA[complete.cases(AdvOGPA[,2]),]
  })

  AdvOGPA3 <- reactive({
    AdvOGPA2 <- AdvOGPA2()
    transform(AdvOGPA2,
              AYSemesterID=as.numeric(factor(
                AdvOGPA2$AYSemesterNo)))
  })

  AdvOGPA4<- reactive({
    AdvOGPA3 <- AdvOGPA3()
    AdvOGPA3[which(duplicated(
      AdvOGPA3$Course.Code) | duplicated(
        AdvOGPA3$Course.Code, fromLast = TRUE)),]
  })

  AdvOGPA5 <- reactive({
    AdvOGPA4 <- AdvOGPA4()
    unique(AdvOGPA4[duplicated(
      AdvOGPA4$Course.Code,
      fromLast = FALSE),])
  })

  AdvOGPA6 <- reactive({
    AdvOGPA4 <- AdvOGPA4()
    AdvOGPA5 <- AdvOGPA5()
    setdiff(AdvOGPA4,
            AdvOGPA5)
  })

  AdvOGPA7 <- reactive({
    AdvOGPA3 <- AdvOGPA3()
    AdvOGPA6 <- AdvOGPA6()
    setdiff(AdvOGPA3,
            AdvOGPA6)
  })

  AdvOGPA8 <- reactive({
    AdvOGPA7 <- AdvOGPA7()
    AdvOGPA7 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  AdvOGPA9 <- reactive({
    AdvOGPA8 <- AdvOGPA8()
    subset(AdvOGPA8,
           AYSemesterID==max(
             AdvOGPA8$AYSemesterID),
           select=Student.ID:LGPA)
  })

  AdvOGPA10 <- reactive({
    AdvOGPA9<-AdvOGPA9()
    AdvOGPA9[,c(1:3,12)]
  })

  AdvOGPA11 <- reactive({
    AdvOGPA10 <- AdvOGPA10()
    columnNames <- c("Student ID",
                     "Academic Level", "Last Semester on the Level", "Overall Cumulative GPA")

    setnames(AdvOGPA10, columnNames)

  })
  #for Overall Cumulative GPA @ Bachelor
  BtechOGPA <- reactive({
    Report20 <- Report20()
    transform(as.data.frame(Report20),
              AcadLevel = ifelse(Report20$AcadLevel=="Advanced Diploma",
                                 "Bachelor",
                                 ifelse(Report20$AcadLevel=="Bachelor",
                                        "Bachelor",
                                        ifelse(Report20$AcadLevel=="Diploma First Year",
                                               "Bachelor",
                                               ifelse(Report20$AcadLevel=="Diploma Second Year",
                                                      "Bachelor","Check"))))
    )
  })

  BtechOGPA2 <- reactive({
    BtechOGPA <- BtechOGPA()
    BtechOGPA[complete.cases(BtechOGPA[,2]),]
  })

  BtechOGPA3 <- reactive({
    BtechOGPA2 <- BtechOGPA2()
    transform(BtechOGPA2,
              AYSemesterID=as.numeric(factor(
                BtechOGPA2$AYSemesterNo)))
  })

  BtechOGPA4<- reactive({
    BtechOGPA3 <- BtechOGPA3()
    BtechOGPA3[which(duplicated(
      BtechOGPA3$Course.Code) | duplicated(
        BtechOGPA3$Course.Code, fromLast = TRUE)),]
  })

  BtechOGPA5 <- reactive({
    BtechOGPA4 <- BtechOGPA4()
    unique(BtechOGPA4[duplicated(
      BtechOGPA4$Course.Code,
      fromLast = FALSE),])
  })

  BtechOGPA6 <- reactive({
    BtechOGPA4 <- BtechOGPA4()
    BtechOGPA5 <- BtechOGPA5()
    setdiff(BtechOGPA4,
            BtechOGPA5)
  })

  BtechOGPA7 <- reactive({
    BtechOGPA3 <- BtechOGPA3()
    BtechOGPA6 <- BtechOGPA6()
    setdiff(BtechOGPA3,
            BtechOGPA6)
  })

  BtechOGPA8 <- reactive({
    BtechOGPA7 <- BtechOGPA7()
    BtechOGPA7 %>%
      group_by(AcadLevel) %>%
      mutate(LGPA=
               sum(Credit.Hours*Point,na.rm=T)/
               sum(Credit.Hours,na.rm = T))
  })


  BtechOGPA9 <- reactive({
    BtechOGPA8 <- BtechOGPA8()
    subset(BtechOGPA8,
           AYSemesterID==max(
             BtechOGPA8$AYSemesterID),
           select=Student.ID:LGPA)
  })

  BtechOGPA10 <- reactive({
    BtechOGPA9<-BtechOGPA9()
    BtechOGPA9[,c(1:3,12)]
  })

  BtechOGPA11 <- reactive({
    BtechOGPA10 <- BtechOGPA10()
    columnNames <- c("Student ID",
                     "Academic Level", "Last Semester on the Level", "Overall Cumulative GPA")

    setnames(BtechOGPA10, columnNames)

  })

  overallGPAnew <- reactive({
    # Dip1stOGPA11 <- Dip1stOGPA11()
    Dip2ndOGPA11 <- Dip2ndOGPA11()
    AdvOGPA11 <- AdvOGPA11()
    BtechOGPA11 <- BtechOGPA11()
    rbind(#as.data.frame(Dip1stOGPA11),
      as.data.frame(Dip2ndOGPA11),
      as.data.frame(AdvOGPA11),
      as.data.frame(BtechOGPA11))
  })

  overallGPAnew1 <- reactive({
    overallGPAnew <- overallGPAnew()
    as.data.frame(unique(overallGPAnew))
  })

  library(dplyr)

  overallGPAnew2 <- reactive({
    overallGPAnew1 <- overallGPAnew1()
    distinct(overallGPAnew1,`Last Semester on the Level`, .keep_all= TRUE)
  })




  #Output####
  output$LGPA <- renderTable({
    if(is.null(Report())){return()}

    Report231()

  })

  output$OGPA <- renderTable({
    if(is.null(Report())){return()}

    overallGPAnew2()

  })

  output$rawdata <- renderTable({
    if(is.null(Report())){return()}

    Report20()


  })

  #observeEvent(input$uploadedfile, {
  # saveData(Report20())  })


  # output$SemGPA1 <- renderTable({
  #   if(is.null(Report())){return()}
  #
  #   Report22()
  #
  #   })

  output$SemGPA2 <- renderTable({
    if(is.null(Report())){return()}

    Report24()

  })



  #Download Handler####
  output$downloadRawData <- downloadHandler(
    filename = function() {
      paste("RawData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(Report20(),file, row.names = FALSE)
    }
  )

  # output$downloadGPA1 <- downloadHandler(
  #   filename = function() {
  #     paste("SemGPA1", ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(Report22(),file, row.names = FALSE)
  #   }
  # )

  output$downloadGPA2 <- downloadHandler(
    filename = function() {
      paste("SemGPA2", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(Report24(),file, row.names = FALSE)
    }
  )

  output$downloadLGPA <- downloadHandler(
    filename = function() {
      paste("CumulativeGPA", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(Report231(),file, row.names = FALSE)
    }
  )

  output$downloadOGPA <- downloadHandler(
    filename = function() {
      paste("OverallCumGPA", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(overallGPAnew2(),file, row.names = FALSE)
    }
  )


  #OBSERVE ####

  observeEvent(input$timeOut, {
    print(paste0("Session (", session$token, ") timed out at: ", Sys.time()))
    showModal(modalDialog(
      title = "Timeout",
      paste("Session timeout due to", input$timeOut, "inactivity -", Sys.time()),
      footer = NULL
    ))
    session$close()
  })




  #Summary Statistics####
  # nooftimesuse <- observe({
  #   write.table(Report20, file = "Sem GPA.csv",
  #               sep = ",", row.names = FALSE,
  #               col.names = TRUE,
  #               qmethod = "double")
  # })


  output$tb <- renderUI({

    if(is.null(Report()))
      h3("CIMS Transcript Re-calculator Web Application by IT Department")

    else
      tabsetPanel(
        tabPanel(
          "Raw Data",
          tableOutput("rawdata")),
        # tabPanel(
        #   "Semester GPA1", tableOutput("SemGPA1")),
        tabPanel(
          "Semester GPA", tableOutput("SemGPA2")),
        tabPanel(
          "Cumulative GPA", tableOutput("LGPA")),
        tabPanel(
          "Overall Cumulative GPA", tableOutput("OGPA")))
  })

}
