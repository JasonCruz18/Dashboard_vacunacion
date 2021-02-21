library(shiny)
library(shinydashboard)
library(data.table)
library(plyr)
library(dplyr)
library(highcharter)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggiraph)


# UI
shinyUI(fluidPage(

    # Application title
    titlePanel("Dashboard_vacunacion"),
    fluidRow(
        box(girafeOutput("grafico2_1")),
        box(highchartOutput("grafico1")),
        box(highchartOutput("grafico2_2"))
    )
))
