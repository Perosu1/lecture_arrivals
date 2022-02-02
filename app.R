library(ggplot2)
library(shiny)

#-----------------------------------------------

date <- Sys.Date()

#-------------------------------------------------

ui <- fluidPage(
    titlePanel(paste("Lecture Arrivals on", date, '| version 0.3')),
    sidebarLayout(
        sidebarPanel(
            actionButton("plot", "START APP"),
            actionButton("arrival", "Record Arrival"),
            actionButton("lecture", "Record Lecture Start"),
            sliderInput("binwidth", 'binwidth', 1, 120, 60, step = 1, round = FALSE,
                        ticks = FALSE, animate = FALSE,
                        width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                        timezone = NULL, dragRange = TRUE),
            textOutput("numpeople"),
            textOutput("lectstarttext"),
            p(' '),
            downloadButton("download_data", "Download .csv"),
            downloadButton("download_plot", "Download .png"),
            p(' '),
            tags$a(href="https://github.com/Perosu1/lecture_arrivals/blob/master/app.R", 
                   "Check out the code on Github!"),
            tags$a(href="https://github.com/Perosu1/lecture_arrivals#readme", 
                   "changes in v. 0.3")
            
            
            
            
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
    
    
    lecture <- reactiveValues()
    lecture$a <- Sys.time()-1000
    
    #lecture_start <-  reactiveValues
    #lecture_start$values <- (Sys.time())
    
    ########################  lecture_start <- 0
    
    count <- function() {
        print(Sys.time())
        print(c('Arrival no.', length(timestamps$values)))
        timestamps$values <- c(timestamps$values, Sys.time())
    }
    
    lectstart <- function() {
        lecture$a <- Sys.time()
        print(lecture$a)
    } 
    
    plot <- function() {
        df <- data.frame(timestamps$values)
        df |>  
            ggplot(aes(timestamps$values)) +
            geom_histogram(binwidth = input$binwidth) + #############change this to 60
            geom_vline(aes(xintercept = mean(timestamps$values)), color = 'blue', size = 1) +
            geom_vline(aes(xintercept = as.numeric(lecture$a)), color = 'red', size = 1) +
            labs(
                title = paste('Lecture arrivals on', date),
                subtitle = 'red = lecture start, blue = average arrival',
                caption = 'Built in RShiny by Peter Kenda',
            ) +
            xlab('Time') +
            ylab('Student Arrivals') +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) 
        
        # could add density - as here https://datavizpyr.com/histograms-with-ggplot2-in-r/
        #df |> 
        #    ggplot(aes(timestamps$values)) +
        #    geom_density() -> plot_2
        
        #grid.arrange(plot_1, plot_2, nrow = 2)
            
    }
    
    #------------------

    
    observeEvent(input$arrival, {
        count()
        output$numpeople <- renderText(c(length(timestamps$values), 'people in lecture'))
    })
    
    observeEvent(input$lecture, {
        lectstart()
        output$lectstarttext <- renderText(c('Lecture started at', as.character.POSIXt(lecture$a, format="%H:%M:%OS")))
        })
    
    observeEvent(input$plot, {output$graph <- renderPlot({plot()})})
    
    output$download_data <- downloadHandler(
        filename = function() {paste('data-',Sys.Date(),".csv", sep="")},
        content = function(file) {write.csv(data.frame(timestamps$values), file, row.names = FALSE)}
    )
    
    output$download_plot <- downloadHandler(
        filename = function() {paste('plot-',Sys.Date(),".png", sep="")},
        content = function(file) {ggsave(file, plot = last_plot(), device = "png")
        }
    )
    
}

#-------------------------------------------------

shinyApp(ui = ui, server = server)