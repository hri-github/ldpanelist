# Author:         Tyler Huang
# Last Modified:  070219

# setwd("F:\\Projects\\LD Panelist")
library(shiny); library(dplyr); library(janitor)
library(readxl); library(writexl)
library(tidyr); library(lubridate)
options(stringsAsFactors=FALSE)  
Sys.setenv(TZ="Asia/Singapore")

#### USER INTERFACE ui ####
ui <- fluidPage(
  shiny::tags$head(shiny::tags$style(HTML("#nodataText{color: red; font-size: 16px; font-style: italic;}
                                          .tabbable > .nav > li[class=active]    > a {background-color: RoyalBlue; color:white}
                                          "))),
  titlePanel("LD Interview Panelist Matching"),
  sidebarLayout(
    sidebarPanel(
      numericInput("MTHINPUT", "Work conflict check between panelist and candidate over past x months", value=60, min=1, max=999, step=1),
      numericInput("MTHCUTOFF", "No. of months panelist and candidate work episodes are allowed to overlap", value=6, min=1, max=999, step=1),
      fileInput("file1", "Upload the XLSX workbook"),
      uiOutput("m_button_ui"),
      p("(v1.0.0) For enquiries, please e-mail tyler_huang@psd.gov.sg"),
      shiny::img(src="psd_logo.jpg", align="bottom", width=200, height=200)
    ),
    mainPanel(
      tabsetPanel(id="mytabset",
                  tabPanel("Upload", value="uploadtab",
                           br(),
                           p("The Panelist information should be in an XLSX workbook, on the 1st sheet.
                             Kindly ensure that the 4 columns are filled accordingly."),
                           shiny::img(src="PanelistScreenshot.png", width="50%", height="50%"),
                           br(), br(),
                           p("The Panelist information should be in an XLSX workbook, on the 2nd sheet. Kindly ensure that the 4 columns are filled accordingly."),
                           shiny::img(src="CandidateScreenshot.png", width="48%", height="48%"),
                           br()
                  ),
                  tabPanel("Results", value="resulttab",
                           br(),
                           textOutput("nodataText"),
                           p("Once the matching is complete, you will see a Download Results button."),
                           textOutput("text1"),
                           uiOutput("dl_button_ui")
                  )  # last tabPanel
      )  # tabsetPanel
    ),  # mainPanel
  ) # sidebarLayout; put sidebarPanel & mainPanel inside
  ) # fluidPage

server <- function(input, output, session) {
  output$nodataText <- renderText({ "WARNING: Please upload your XLSX workbook first." })
  
  filler <- function(x, missing="") {
    Log <- x != missing
    y <- x[Log]
    y[cumsum(Log)]
  }

  Dataset1 <- reactive({
    if (is.null(input$file1)) { return(data.frame()) } # User has not uploaded a file yet
    Dataset1 <- read_xlsx(input$file1$datapath, sheet=1, col_types="text") %>% as.data.frame()
    return(Dataset1)
  })
  Dataset2 <- reactive({
    if (is.null(input$file1)) { return(data.frame()) } # User has not uploaded a file yet
    Dataset2 <- read_xlsx(input$file1$datapath, sheet=2, col_types="text") %>% as.data.frame()
    return(Dataset2)
  })
  
  MTHINPUT_R <- reactive({ input$MTHINPUT }) 
  MTHCUTOFF_R <- reactive({ input$MTHCUTOFF }) 
  
  observeEvent( !is.null(input$file1), {
    output$m_button_ui <- renderUI({
      actionButton("button_match", "Match!", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
      })
  }, ignoreInit=TRUE)
  
  observeEvent({
    req(input$button_match)
    }, {
        output$nodataText <- renderText({ "" })
        MTHINPUT <- MTHINPUT_R() # 60
        MTHCUTOFF <- MTHCUTOFF_R() # 6
        DATECUT <- today() - months(MTHINPUT)
        TDATE <- today()
        
        pandf <- Dataset1()
        # pandf <- read_xlsx("appt sample for Shiny.xlsx", sheet=1, col_types="text")
        colnames(pandf) <- c("name", "agency", "start", "end")
        pandf <- pandf[pandf$agency != "NPL", ]  # just in case
        pandf$name <- ifelse(is.na(pandf$name), "", pandf$name)
        pandf$name <- filler(pandf$name)
        pandf$start <- as.Date(as.numeric(pandf$start), origin="1899-12-30")
        pandf$end <- as.Date(as.numeric(pandf$end), origin="1899-12-30")
        pandf$end <- if_else(is.na(pandf$end), TDATE, pandf$end)  # PRO TIP
        pandf$interval <- interval(pandf$start, pandf$end)
        
        candf <- Dataset2()
        # candf <- read_xlsx("appt sample for Shiny.xlsx", sheet=2, col_types="text")
        candf <- candf[, 1:4]
        colnames(candf) <- c("name", "agency", "start", "end")
        candf <- candf[candf$agency != "NPL", ]
        candf$name <- ifelse(is.na(candf$name), "", candf$name)
        candf$name <- filler(candf$name)
        candf$start <- as.Date(as.numeric(candf$start), origin="1899-12-30")
        candf$end <- as.Date(as.numeric(candf$end), origin="1899-12-30")
        candf$end <- if_else(is.na(candf$end), TDATE, candf$end)  # PRO TIP
        
        agency <- read_xlsx("Format for LD Appointment (mapping).xlsx")
        candf <- left_join(candf, agency)
        candf$minf <- ifelse(is.na(candf$minf), candf$agency, candf$minf)
        
        candf$interval <- interval(candf$start, candf$end)
        candf$timemth <- lubridate::time_length(candf$interval, "month")
        candf$period <- as.period(candf$interval)
        
        summary1<- group_by(candf, name) %>%
          summarise(summth=sum(timemth))
        summary1$yrs <- floor(summary1$summth / 12)
        summary1$mths <- summary1$summth %% 12
        
        ## BEFORE LOOP, PURGE ALL OUTDATED ROWS ####
        keep1 <- DATECUT %within% pandf$interval
        keep2 <- pandf$start >= DATECUT
        keep3 <- as.logical(keep1 + keep2)
        pandf <- pandf[keep3, ]
        
        keep1 <- DATECUT %within% candf$interval
        keep2 <- candf$start >= DATECUT
        keep3 <- as.logical(keep1 + keep2)
        candf <- candf[keep3, ]
        
        canunique <- unique(candf$name)
        
        withProgress(message = 'Matching', value=0, {
          n <- length(canunique)
          for (i in seq_along(canunique)) {
            incProgress(1/n, detail = paste("Candidate", i))
            can_i <- candf[candf$name==canunique[i], ]
            can_i_tibble <- tbl_df(can_i)
            can_i_tibble$pnames <- vector("list", nrow(can_i_tibble))
            can_i_tibble$pperiod <- vector("list", nrow(can_i_tibble))
            can_i_tibble$pinterval <- vector("list", nrow(can_i_tibble))
            can_i_tibble$pminf <- vector("list", nrow(can_i_tibble))
            for (j in seq_along(can_i$minf)) {
              minf <- can_i$minf[j]
              ag <- can_i$agency[j]
              can_i_minf <- can_i[can_i$minf==minf | can_i$agency==ag, ]
              pan_minf <- pandf[pandf$agency==minf | pandf$agency==ag, ]
              keep <- int_overlaps(can_i_minf$interval, pan_minf$interval)  # returns TRUE, TRUE
              can_i_tibble$pnames[[j]] <- pan_minf$name[keep] 
              pvec <- as.period(lubridate::intersect(can_i_minf$interval, pan_minf$interval), "months")
              if (length(pvec) == 0) {
                can_i_tibble$pperiod[[j]] <- character(0)
              } else { 
                can_i_tibble$pperiod[[j]] <- as.character(pvec)
              }
              if (length(pvec) == 0) {
                can_i_tibble$pinterval[[j]] <- character(0)
              } else { 
                can_i_tibble$pinterval[[j]] <- as.character(pan_minf$interval[keep])
              }
              can_i_tibble$pminf[[j]] <- pan_minf$agency[keep]
            }  # for loop 
            can_i_unnest <- unnest(can_i_tibble)
            if (i == 1) {
              can_out <- can_i_unnest
            } else {
              can_out <- rbind.data.frame(can_out, can_i_unnest)
            }
          }
        })  # withProgress
        
        # can_outW <- reshape2::dcast(can_out, name + agency + minf + start + end ~ pnames, fun.aggregate=length, value.var="pnames", fill=0)
        can_outW <- reshape2::dcast(can_out, name + agency + minf + start + end ~ pnames, value.var="timemth", fill=0)
        can_outW[, 6:ncol(can_outW)] <- sapply(can_outW[, 6:ncol(can_outW)],
                                               function(x) ifelse(x > MTHCUTOFF, "FAIL", "PASS"))
        can_outW$fail <- apply(can_outW, 1, function(x) 
          paste(colnames(can_outW)[x=="FAIL"], collapse=", ") )
        
        y <- select(can_outW, name, fail, agency)
        out1 <- left_join(summary1, y)
        out1$fail <- ifelse(is.na(out1$fail), "", out1$fail)
        out1 <- select(out1, -summth)
        
        can_out$conflict <- ifelse(can_out$timemth > MTHCUTOFF, "CONFLICT", "PASS")
        can_out1 <- select(can_out, cname=name, cagency=agency, cminf=minf, cstart=start, cend=end, pnames, pperiod, pinterval, pminf, conflict)
        canprint <- mutate(candf, interval=as.character(interval),
                           period=as.character(period))
        panprint <- mutate(pandf, interval=as.character(interval))
        
        bn <- basename(input$file1$name)
        params <- data.frame(date_analysed=TDATE, mthcutoff=MTHCUTOFF, lastmonths=MTHINPUT, inputfile=bn)
        
        colnames(out1) <- c("candidate_name", "YIS_yrs", "YIS_mths", "work_conflict", "candidate_conflict_agency")
        colnames(can_out1) <- c("candidate_name", "candidate_agency",	"candidate_ministry_family",	"candidate_startdate", "candidate_enddate",	"panel_name",	"work_conflict_period",	"panel_work_interval", "panel_ministry")
        colnames(canprint) <- c("candidate_name",	"candidate_agency",	"candidate_startdate",	"candidate_enddate", "candidate_ministry_family",	"candidate_work_interval",	"work_month",	"work_year_month")
        canprint <- select(canprint, -work_month, -work_year_month)
        colnames(panprint) <- c("panel_name",	"panel_agency",	"panel_startdate",	"panel_enddate",	"panel_work_interval")
        
        xllist <- list(All_Candidates=out1, Conflicts=can_out1, Candidates_Recent=canprint, Panelists_Recent=panprint, Parameters=params)
        fn <- paste0("LD Appointment ", today(), ".xlsx")
        # writexl::write_xlsx(xllist, fn)
        
        ## OUTPUT TAB
        updateTabsetPanel(session, inputId="mytabset", selected="resulttab")
        output$download_item <- downloadHandler(
              filename = fn,
              content = function(file){
                write_xlsx(xllist, file)
              }
            )
        output$dl_button_ui <- renderUI({
          downloadButton('download_item', label="Download Results")
        })

      }, ignoreNULL=TRUE) # observeEvent buttonQuery; ignoreINIT prevents click button first, then upload
  #   }  # end of if !is.null(file1)
  # })  # observe
  
} # server function()

# Must be enabled for EC2 
shinyApp(ui=ui, server=server)
