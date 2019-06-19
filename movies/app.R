#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(ggplot2movies)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("thisyear",
                        "Year:",
                        min = 1980,
                        max = 2006,
                        value = 2002),
            checkboxGroupInput('thesempaa',
                               'MPAA Rating:',
                               choices = c('', 'PG-13', 'R'),
                               selected = ''),
            sliderInput('minrating',
                        'Rating:',
                        min = 0,
                        max = 10,
                        value = 8),
            verbatimTextOutput('yearvalue'),
            verbatimTextOutput('mpaavalues')
        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                column(width = 6,
                    plotOutput("yearPlot")),
                column(width = 6,
                    plotOutput('mpaaPlot'))
            ),
            dataTableOutput('movieTable')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$yearvalue <- renderPrint({
        
        return(input$thisyear)
    })
    
    output$mpaavalues <- renderPrint({
        
        return(input$thesempaa)
    })
    
    subsetmoviesbyyear <- function(thisyear) {
        print('in function subsetmoviesbyyear()')
        
        movieyear <- movies %>%
            filter(
                year == thisyear
            )
        return(movieyear)
    }
    
    moviesYear <- reactive({
        print('in reactive function moviesYear')
        
        movieyear <- movies %>%
            filter(
                year == input$thisyear
            )
        return(movieyear)
    })
    
    moviesMpaa <- reactive({
        print('in reactive function moviesMpaa')
        
        movieyear <- moviesYear()
        
        moviesmpaa <- movieyear %>%
            filter(
                mpaa %in% input$thesempaa
            )
    })
    
    moviesTable <- reactive({
        print('in reactive function moviesTable')
        
        moviesmpaa <- moviesMpaa()
        
        movielist <- moviesmpaa %>%
            filter(
                rating >= input$minrating
            )
        
    })
    
    output$yearPlot <- renderPlot({
        print('At render year plot')
        
        # movieyear <- movies %>%
        #     filter(
        #         year == input$thisyear
        #     )
        
        # movieyear <- subsetmoviesbyyear(input$thisyear)
        
        movieyear <- moviesYear()
        
        ggplot(movieyear, aes(x = rating)) +
            geom_histogram()    
        
    })
    
    output$mpaaPlot <- renderPlot({
        print('At render mpaa plot')
        # movieyear <- movies %>%
        #     filter(
        #         year == input$thisyear
        #     )
        # movieyear <- subsetmoviesbyyear(input$thisyear)
        
        # movieyear <- moviesYear()
        
        # moviesmpaa <- movieyear %>%
        #     filter(
        #         mpaa %in% input$thesempaa
        #     )
        
        moviesmpaa <- moviesMpaa()
        
        ggplot(moviesmpaa, aes(x = length, y = rating, color = mpaa)) +
            geom_smooth()
    })
    
    output$movieTable <- renderDataTable({
        print('At render movies list')
        # movieyear <- movies %>%
        #     filter(
        #         year == input$thisyear
        #     )
        #movieyear <- subsetmoviesbyyear(input$thisyear)
        
        # movieyear <- moviesYear()
        # 
        # moviesmpaa <- movieyear %>%
        #     filter(
        #         mpaa %in% input$thesempaa
        #     )
        # 
        # movielist <- moviesmpaa %>%
        #     filter(
        #         rating >= input$minrating
        #     )
        
        movielist <- moviesTable()
        return(movielist)
        
    })
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
