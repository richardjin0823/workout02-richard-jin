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
library(yarrr)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Investment Strategies - Richard Jin"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
      column(4,
         sliderInput("init",
                    "Initial Amount",
                     min = 0,
                     max = 100000,
                     value = 1000,
                     step = 500)),
      column(4,
         sliderInput("ret",
                     "Return Rate (in %)",
                     min = 0,
                     max = 20, 
                     value = 5,
                     step = 0.1)),
      column(4,
         sliderInput("yrs",
                    "Years",
                    min = 0, 
                    max = 50, 
                    value = 10, 
                    step = 1))),
   fluidRow(
     column(4,
        sliderInput("annc",
                    "Annual Contribution",
                    min = 0,
                    max = 50000,
                    value = 2000, 
                    step = 500)),
     column(4, 
       sliderInput("grate",
                   "Growth Rate (in %)",
                   min = 0, 
                   max = 20, 
                   value = 2, 
                   step = 0.1)),
     column(4,
       selectInput("facet",
                   "Facet?",
                   choices = c("No", "Yes")))),
   
  fluidRow(
    column(width = 12),
     mainPanel( "Timelines")),
  fluidRow(
    column(width = 12),
       plotOutput("distPlot")),
  
  fluidRow(
    column(width = 12),
    mainPanel ("Balance")),
   
  fluidRow(
    column(width = 12),
       verbatimTextOutput("table")))

server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     future_value <- function (amount, rate, years) {
       amount * (1 + rate) ^ years
     }
     
     annuity <- function (contrib, rate, years) {
       contrib * ((1 + rate) ^ years - 1)/ rate
     }
     
     growing_annuity <- function (contrib, rate, growth, years) {
       contrib * ((1 + rate) ^ years - (1 + growth) ^ years) / (rate - growth) 
     }
     
     no_contrib <- rep (0, (input$yrs + 1))
     fixed_contrib <- rep (0, (input$yrs + 1))
     growing_contrib <- rep (0, (input$yrs + 1))
     year <- c(0:(input$yrs))
     
     for (i in 0:(input$yrs)) {
       no_contrib[i+1] <- future_value(amount = input$init, rate = (input$ret)/100, years = i)
     }
     
     for (i in 0:(input$yrs)) { 
       fixed_contrib[i+1] <- future_value(amount = input$init, rate = (input$ret)/100, years = i) + annuity(contrib = input$annc, rate = (input$ret)/100, years = i)
     }
     
     for (i in 0:(input$yrs)) {
       growing_contrib[i+1] <- future_value(amount = input$init, rate = (input$ret)/100, years = i) + growing_annuity(contrib = input$annc, rate = (input$ret)/100, growth = (input$grate)/100, years = i)
     }
     
     modalities2 <- data.frame(year, no_contrib, fixed_contrib, growing_contrib)
     no_contrib <- data.frame(type = "no_contrib", color = rep(yarrr::transparent('green', .6), (input$yrs+1)), year, contrib = no_contrib) 
     fixed_contrib <- data.frame(type = "fixed_contrib", color = rep(yarrr::transparent('red', .6), (input$yrs+1)),year,  contrib = fixed_contrib)
     growing_contrib <- data.frame(type = "growing_contrib", color = rep(yarrr::transparent('blue', .6), (input$yrs+1)),year, contrib = growing_contrib)
     modalities <- rbind(no_contrib, fixed_contrib, growing_contrib)
     
     if (input$facet == "Yes") {ggplot(modalities, aes(x = year)) + geom_line(aes(y= contrib,colour = modalities$type)) + geom_point(aes(y = contrib, color = modalities$type)) + facet_wrap( ~ type) +
     xlab("Year") + ylab("value") + ggtitle("Three modes of investing") + labs(colour="variable") + geom_area(aes(y = contrib), fill = modalities$color)}
     else {ggplot(modalities, aes(x = year)) + geom_line(aes(y= contrib,colour = modalities$type)) + geom_point(aes(y = contrib, color = modalities$type)) +
     xlab("Year") + ylab("value") + ggtitle("Three modes of investing") + labs(colour="variable")}
   })
   
   output$table <- renderPrint({
     future_value <- function (amount, rate, years) {
       amount * (1 + rate) ^ years
     }
     
     annuity <- function (contrib, rate, years) {
       contrib * ((1 + rate) ^ years - 1)/ rate
     }
     
     growing_annuity <- function (contrib, rate, growth, years) {
       contrib * ((1 + rate) ^ years - (1 + growth) ^ years) / (rate - growth) 
     }
     
     no_contrib <- rep (0, (input$yrs + 1))
     fixed_contrib <- rep (0, (input$yrs + 1))
     growing_contrib <- rep (0, (input$yrs + 1))
     year <- c(0:(input$yrs))
     
     for (i in 0:(input$yrs)) {
       no_contrib[i+1] <- future_value(amount = input$init, rate = (input$ret)/100, years = i)
     }
     
     for (i in 0:(input$yrs)) { 
       fixed_contrib[i+1] <- future_value(amount = input$init, rate = (input$ret)/100, years = i) + annuity(contrib = input$annc, rate = (input$ret)/100, years = i)
     }
     
     for (i in 0:(input$yrs)) {
       growing_contrib[i+1] <- future_value(amount = input$init, rate = (input$ret)/100, years = i) + growing_annuity(contrib = input$annc, rate = (input$ret)/100, growth = (input$grate)/100, years = i)
     }
     
     data.frame(year, no_contrib, fixed_contrib, growing_contrib)
     
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

