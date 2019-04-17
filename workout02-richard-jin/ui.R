#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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