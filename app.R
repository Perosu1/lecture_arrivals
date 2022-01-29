library(ggplot2)
library(shiny)
#-----------------------------------------------
#------------------

date <- Sys.Date()


#-------------------------------------------------

ui <- fluidPage(
    titlePanel(paste("Lecture Arrivals on", date, '| version 0.1')),
    sidebarLayout(
        sidebarPanel(
            actionButton("plot", "START APP"),
            actionButton("arrival", "Record Arrival"),
            actionButton("lecture", "Record Lecture Start"),
            #numericInput("binwidth", 'binwidth', 60, 1, 120),
            sliderInput("binwidth", 'binwidth', 1, 120, 60, step = 1, round = FALSE,
                        ticks = FALSE, animate = FALSE,
                        width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                        timezone = NULL, dragRange = TRUE),
            textOutput("text1")
            
        ),
        mainPanel(
            p(" "),
            plotOutput(outputId = "graph")
        ))
)


#-------------------------------------------------

server <- function(input, output) {
    
    timestamps <- reactiveValues()
    timestamps$values <- c(Sys.time())
    
    lecture_start <- reactiveVal(0)
    
    ########################  lecture_start <- 0
    
    count <- function() {
        print(Sys.time())
        print(c('Arrival no.', length(timestamps$values)))
        timestamps$values <- c(timestamps$values, Sys.time())
    }
    
    lectstart <- function() {
        lecture_start <- Sys.time()
        print(lecture_start)
    } 
    
    plot <- function() {
        mean <- mean(timestamps$values)
        df <- data.frame(timestamps$values)
        df |>  
            ggplot(aes(timestamps$values)) +
            geom_histogram(binwidth = input$binwidth) + #############change this to 60
            #geom_vline(aes(xintercept = lecture_start$values[1]), color = 'red', size = 1) +
            geom_vline(aes(xintercept = mean), color = 'blue', size = 1) +
            geom_hline(yintercept = 0) +
            labs(
                caption = 'blue is average | designed by Peter Kenda'
            ) +
            xlab('Time') +
            ylab('Student Arrivals')
        
    }
    
    
    #------------------
    
    observeEvent(input$arrival, {
        count()
        output$text1 <- renderText(c(length(timestamps$values), 'people in lecture'))
    })
    
    observeEvent(input$lecture, {lectstart()})
    
    observeEvent(input$plot, {output$graph <- renderPlot({plot()})})
}

#-------------------------------------------------

shinyApp(ui = ui, server = server)


