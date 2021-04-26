#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(DT)

# check github for available files
req <- "https://api.github.com/repos/guga31bb/dots-data/git/trees/master?recursive=1" %>%
    httr::GET()
    
filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
files <- grep("data/", filelist, value = TRUE, fixed = TRUE)

# load everything
gif_list <- sub("\\..*", "", files) %>%
    str_split_fixed("_", 3) %>%
    as_tibble(.name_repair = "universal") %>%
    set_names(c("dots", "game_id", "play_id")) %>%
    select(-dots) %>%
    mutate(
        has_gif = 1,
        play_id = as.numeric(play_id),
        game_id = as.numeric(game_id)
        )

nflfastr <- readRDS("data/nflfastR_2018.rds")

coverages <- read_csv("data/coverages_2018.csv") %>%
    mutate(coverage = case_when(
        coverage == "3 Seam" ~ "Cover 3 Zone",
        coverage == "Cover 1 Double" ~ "Cover 1 Man",
        coverage %in% c("Red Zone", "Goal Line") ~ "Red zone / goal line",
        coverage == "Mis" | is.na(coverage) ~ "Other / misc",
        TRUE ~ coverage
    ))

stub <- "https://raw.githubusercontent.com/guga31bb/dots-data/master/data/dots"

play_list <- nflfastr %>%
    left_join(gif_list, by = c("game_id", "play_id")) %>%
    left_join(coverages, by = c("game_id", "play_id")) %>%
    filter(has_gif == 1) %>%
    mutate(filename = 
            glue::glue("{stub}_{game_id}_{play_id}.gif")
               )

table <- play_list %>%
    select(
        nflfastr_id,
        posteam, defteam,
        qtr, yardline_100, down, ydstogo,
        coverage, desc, filename
    )

coverage_game <- table %>%
    filter(coverage %in% c(
        "Cover 0 Man",
        "Cover 1 Man",
        "Cover 2 Man",
        "Cover 2 Zone",
        "Cover 3 Zone",
        "Cover 4 Zone",
        "Cover 6 Zone"
    ))

# Define UI for application that draws a histogram
ui <- navbarPage(
    
    title = 'Dots from 2018', id = 'x0',

    tabPanel(
        'Play list', 
        
        fluidRow(
            column(4, align = "center",
                   pickerInput(
                       inputId = "posteam",
                       label = "Offense team", 
                       choices = unique(nflfastr$posteam),
                       selected = unique(nflfastr$posteam),
                       options = list(
                           `actions-box` = TRUE), 
                       multiple = TRUE
                   )
                   ),
            column(4, align = "center",
                   pickerInput(
                       inputId = "defteam",
                       label = "Defense team", 
                       choices = unique(nflfastr$posteam),
                       selected = unique(nflfastr$posteam),
                       options = list(
                           `actions-box` = TRUE), 
                       multiple = TRUE
                   )
            ),
            column(4, align = "center",
                   pickerInput(
                       inputId = "coverage",
                       label = "Coverage", 
                       choices = unique(coverages$coverage),
                       selected = unique(coverages$coverage),
                       options = list(
                           `actions-box` = TRUE), 
                       multiple = TRUE
                   )
            )
        ),
        
        fluidRow(
            column(12, align = "center",
                   actionBttn(
                       inputId = "update",
                       label = "Apply filters",
                       style = "jelly", 
                       color = "danger"
                   )
            )

        ),
        
        tags$br(),

        
        tags$p("Click on a play to see the dots"),
        DT::dataTableOutput('tbl')
        ),
    
    tabPanel(
        'Dots', 
        fluidRow(
          column(6, align = "right",
                 actionBttn(
                     inputId = "click_b",
                     label = "Previous play",
                     style = "jelly", 
                     color = "danger"
                 )
            ),
          
          column(6, align = "left",
                 actionBttn(
                     inputId = "click_f",
                     label = "Next play",
                     style = "jelly", 
                     color = "success"
                )
        )
        ),
        
        fluidRow(
            column(12, align = "center",
                   imageOutput("dummy", height = "90%", width = "90%"),
                   uiOutput("plot1", height = "80%", width = "80%")
                   )
        )
        
        
        ),
    tabPanel(
        'Guessing game',
        
        actionBttn(
            inputId = "update2",
            label = "Another play",
            style = "jelly", 
            color = "success"
        ),
        
        tags$br(),
        tags$br(),
        
        radioGroupButtons(
            inputId = "coverage2",
            label = "What is the coverage?", 
            choices = c("None", unique(coverage_game$coverage))
        ),
        
        textOutput("text"),
        textOutput("text2"),
        uiOutput("plot2", height = "80%", width = "80%"),
        imageOutput("dummy2", height = "90%", width = "90%")
        
    ),
    
    tabPanel("About",
             fluidRow(column(12, align="left",
                             includeMarkdown("README.md")
             ))
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    data <- eventReactive(
        input$update, {
            
            table %>%
                filter(
                    posteam %in% input$posteam,
                    defteam %in% input$defteam,
                    coverage %in% input$coverage
                )
            
        }, ignoreNULL = FALSE
    )
    
    output$tbl = renderDT(
        data(), 
        extensions = c('FixedHeader'),
        
        options = list(
            autoWidth = TRUE,
            pageLength = 30,
            # lengthChange = FALSE,
            dom = "tfp",
            ordering = FALSE,
            scrollX = TRUE,
            fixedHeader = TRUE,
            columnDefs = list(
                list(width = '60px', targets = c(0, 1, 2, 3, 4, 5)),
                list(visible=FALSE, targets=c(9))
                )

            ),
        # filter = list(position = "top"),
        selection = list(mode = 'single', selected = 1
                         ),
        
        # change column names
        colnames = c("ID", "Off", "Def", "QTR", "Yardline",
                     "Down", "YTG", "Coverage", "Play", "File"
        ),
        rownames = FALSE
    )
    
    # initialize value to keep track of row
    value <- reactiveVal(1)
    
    DTproxy <- DT::dataTableProxy("tbl")
    
    observeEvent(input$tbl_cell_clicked, {
        info = input$tbl_cell_clicked
        # do nothing if not clicked yet, or the clicked cell is not in the 1st column
        if (is.null(info$value)) return()
        updateTabsetPanel(session, 'x0', selected = 'Dots')
        value(info$row)
    })
    
    # forward button clicked
    observeEvent(input$click_f, {
        row_count <- input$tbl_rows_selected
        
        # if nothing is already selected, make everything 2
        if (is.null(row_count)) {
            row_count <- 2
            DT::selectRows(
                DTproxy,
                2
            )
            value(2)
            return()
        }
        
        # if already at the max, don't do anything
        if (row_count == nrow(data())) return()

        # otherwise increment 1
        DT::selectRows(
            DTproxy,
            row_count + 1
            )
        value(input$tbl_rows_selected + 1)
    })
    
    # back button clicked
    observeEvent(input$click_b, {

        row_count <- input$tbl_rows_selected
        
        # if nothing is already selected, make everything 1
        if (is.null(row_count)) {
            row_count <- 1
            DT::selectRows(
                DTproxy,
                1
            )
            value(1)
            return()
        }
        
        # if already at the min, don't do anything
        if (row_count == 1) return()
        
        # otherwise, move back 1
        DT::selectRows(
            DTproxy,
            row_count - 1
        )
        
        value(input$tbl_rows_selected - 1)
        
    })
    
    # initialize random play
    rand_id <- reactiveVal(runif(1, min=1, max = nrow(coverage_game)) %>% floor())
    
    streak <- reactiveVal(0)
    has_guessed <- reactiveVal(0)
    
    # new play button clicked: get new random play
    observeEvent(input$update2, {
        
        # reset guess to 0 for new play
        has_guessed(0)
        
        # if user didn't make a guess, reset streak to 0
        if (input$coverage2 == "None") {
            streak(0)
        }
        
        # reset coverage
        updateRadioGroupButtons(
            session = session,
            inputId = "coverage2",
            selected = "None"
        )
        
        # pick new random play
        val = runif(1, min=1, max = nrow(coverage_game)) %>% floor()

        rand_id(val)
        
    })
    
    observeEvent(
        input$coverage2, {
            
            label <- coverage_game %>% dplyr::slice(rand_id()) %>% pull(coverage)
            
            if (input$coverage2 == label & has_guessed() == 0) {
                streak(streak() + 1)
                has_guessed(1)
            } else if (input$coverage2 == "None") return()
            else {
                has_guessed(1)
                streak(0)
            }
            
        }
        )
    
    # an extremely stupid way of figuring out the right widow size
    # for the gifs below
    output$dummy <- renderImage({
        
        # A temp file to save the output.
        # This file will be removed later by renderImage
        
        play <- table %>% dplyr::slice(rand_id())
        
        file = paste(play$filename)
        
        # Return a list containing the filename
        list(src = "gifs/dots_2018090600_75.gif",
             contentType = 'image/gif',
             width = 1,
             height = 1
             # alt = "This is alternate text"
        )}, deleteFile = FALSE)
    
    # do it again for other window
    output$dummy2 <- renderImage({
        
        # A temp file to save the output.
        # This file will be removed later by renderImage
        
        play <- table %>% dplyr::slice(rand_id())
        
        file = paste(play$filename)
        
        # Return a list containing the filename
        list(src = "gifs/dots_2018090600_75.gif",
             contentType = 'image/gif',
             width = 1,
             height = 1
             # alt = "This is alternate text"
        )}, deleteFile = FALSE)
    
    
    output$plot1 <- renderUI({
        
        w  <- session$clientData$output_dummy_width
        h <- (9/18) * w
        
        play <- data() %>% dplyr::slice(value())
        file <- paste(play$filename)
        
        tags$img(src = file, height = h, width = w)
    })
    
    output$plot2 <- renderUI({
        
        w  <- session$clientData$output_dummy2_width
        h <- (9/18) * w
        
        play <- coverage_game %>% dplyr::slice(rand_id())
        
        file <- paste(play$filename)
        
        tags$img(src = file, height = h, width = w)
        })
    

    output$text <- renderText({
        cvg <- coverage_game %>% dplyr::slice(rand_id()) %>% pull(coverage)
        if (input$coverage2 == "None") {
            return("Please take a guess")
        } else if (input$coverage2 == cvg) {
            return("PFF thinks you are correct!")
        } else {
            return("Try again! Back to the film room for you!")
        }

    })
    
    output$text2 <- renderText({
        glue::glue("Current streak: {streak()}")
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
