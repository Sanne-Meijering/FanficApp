#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(RJSONIO)
library(ggplot2)

# Dataset Per rating
data <- fromJSON("www/database3_complete.json")

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$header(tags$style(HTML("
                h2{
                    font-size: xx-large;
                    text-align: center;
                }
                div{
                    font-size: large;
                }
                body{
                    background-color: coral;
                }
                img{
                    border-radius: 25px;
                }
                "))),
    
    theme = "bootstrap.css",

    
    # Application title
    titlePanel("Fanfiction for TV series 2000-01/2017"),

    # Sidebar with boxes for input
    sidebarLayout(
        sidebarPanel(
            # Select the website
            selectInput("series", "Choose a series:", choices = names(data),
                        selected = "All"),
            selectInput("year", "Choose a year, or the entire period (total):", 
                        choices = as.list(c("total", as.character(seq(2000, 2017)))),
                        selected = "All"),
            selectInput("type", "Choose the variable:", 
                        choices = list("Chapters"="chapters", "Maturity rating" = "rating", 
                                       "Length"="words"), selected = "chapters"),
            selectInput("site", "Choose a webite:",
                        choices = list("Archive of Our Own" = "ao3",
                                       "Fanfiction.net"="ffnet"), selected = "ffnet")),

        # Show a plot of the generated distribution
        mainPanel(
            column(6,
            plotOutput("piePlot"))),
            position = "left"
        ))


# Define server logic required to draw a histogram
server <- function(input, output, session){
        observeEvent(input$series, {
            updateSelectInput(session, "year", 
                              choices = names(data[[input$series]]), selected = "total")
        })
        observeEvent(input$year, {
            updateSelectInput(session, "type",
                              choices = names(data[[input$series]][[input$year]]))
        })
        output$piePlot <- renderPlot({
            data_cur <- unlist(data[[input$series]][[input$year]][[input$type]])
            data_temp <- t(as.data.frame(data_cur))
            data_temp <- rbind(data_temp[1:3], data_temp[4:6], data_temp[7:9])
            data_temp[,1:2] <- as.numeric(data_temp[,1:2])
            data_temp <- as.data.frame(data_temp)
            if(input$site == "ao3"){
                colnames(data_temp) <- c("current", "ffnet", "class")
            }
            else{
                colnames(data_temp) <- c("ao3", "current", "class")
            }
            
            pie <- ggplot(data_temp[order(data_temp$current, decreasing = T),], 
                          aes(x="", y=current, fill=factor(class))) +
                geom_bar(width=2, stat="identity") +
                theme(axis.line = element_blank()) +
                labs(fill=input$type, x=NULL, y=NULL)
            pie + coord_polar(theta="y", start=0)
                
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
