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

# load everything
gif_list <- sub("\\..*", "", list.files("gifs/")) %>%
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

play_list <- nflfastr %>%
    left_join(gif_list, by = c("game_id", "play_id")) %>%
    left_join(coverages, by = c("game_id", "play_id")) %>%
    filter(has_gif == 1) %>%
    mutate(filename = 
            glue::glue("gifs/dots_{game_id}_{play_id}.gif")
               )

table <- play_list %>%
    select(
        nflfastr_id,
        posteam, defteam,
        qtr, yardline_100, down, ydstogo,
        coverage, desc, filename
    )

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
        
        actionBttn(
            inputId = "click_b",
            label = "Previous play",
            style = "jelly", 
            color = "danger"
        ),
        
        actionBttn(
            inputId = "click_f",
            label = "Next play",
            style = "jelly", 
            color = "success"
        ),
        
        imageOutput("plot1")
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
        
        if (row_count == nrow(data())) return()
        if (is.null(row_count)) row_count <- 1

        DT::selectRows(
            DTproxy,
            row_count + 1
            )
        value(input$tbl_rows_selected + 1)
    })
    
    # back button clicked
    observeEvent(input$click_b, {

        row_count <- input$tbl_rows_selected
        
        if (is.null(row_count)) row_count <- 1
        if (row_count == 1) return()
        
        DT::selectRows(
            DTproxy,
            row_count - 1
        )
        
        value(input$tbl_rows_selected - 1)
        
        
    })
    
    output$plot1 <- renderImage({
        
        width  <- session$clientData$output_plot1_width
        height <- (9/18) * width
        
        # A temp file to save the output.
        # This file will be removed later by renderImage
        
        play <- data() %>% dplyr::slice(value())
        
        file = paste(play$filename)

        # Return a list containing the filename
        list(src = file,
             contentType = 'image/gif',
             width = width,
             height = height
             # alt = "This is alternate text"
        )}, deleteFile = FALSE)

}

# Run the application 
shinyApp(ui = ui, server = server)
