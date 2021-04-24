#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
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
    filter(has_gif == 1) 

table <- play_list %>%
    select(
        nflfastr_id,
        posteam, defteam,
        qtr, yardline_100, down, ydstogo,
        coverage, desc
    )

# Define UI for application that draws a histogram
ui <- navbarPage(
    
    title = 'Dots from 2018', id = 'x0',

    tabPanel('Play list', DT::dataTableOutput('tbl')),
    
    tabPanel(
        'Dots', 
        imageOutput("plot1")
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    value <- reactiveVal(1)

    output$tbl = renderDT(
        table, 
        extensions = c('FixedHeader'),
        
        options = list(
            autoWidth = TRUE,
            pageLength = 30,
            # lengthChange = FALSE,
            dom = "tfp",
            scrollX = TRUE,
            fixedHeader = TRUE,
            columnDefs = list(
                list(width = '80px', targets = c(1, 2, 3, 4, 5, 6, 7))
                )
            
            ),
        filter = list(position = "top"),
        selection = list(mode = 'single' #, selected = 1
                         ),
        
        # change column names
        colnames = c("ID", "Off", "Def", "QTR", "Yardline",
                     "Down", "YTG", "Coverage", "Play"
        ),
        rownames = FALSE
    )
    
    observeEvent(input$tbl_cell_clicked, {
        info = input$tbl_cell_clicked
        # do nothing if not clicked yet, or the clicked cell is not in the 1st column
        if (is.null(info$value)) return()
        updateTabsetPanel(session, 'x0', selected = 'Dots')
        value(info$row)
    })
    
    output$plot1 <- renderImage({
        
        width  <- session$clientData$output_plot1_width
        height <- (9/18) * width
        
        # A temp file to save the output.
        # This file will be removed later by renderImage
        
        play <- play_list %>% dplyr::slice(value())
        
        file = glue::glue("gifs/dots_{play$game_id}_{play$play_id}.gif")

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
