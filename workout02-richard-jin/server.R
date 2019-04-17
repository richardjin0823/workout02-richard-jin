#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(yarrr)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
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
    
    
  })
  
 
  
