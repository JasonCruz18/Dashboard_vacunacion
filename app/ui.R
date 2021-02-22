library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(data.table)
library(plyr)
library(dplyr)
library(highcharter)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggiraph)


# UI
shinyUI(
    fluidPage(
    # Application title
    titlePanel(title=div(img(src="logo.png", style="width: 500px"))),
    h1("VACUNAS COVID-19 AL 20 DE FEBRERO DEL 2021", align = "center"),
    
    fluidRow(
        valueBox(6868, "Personal de Salud Vacunado", icon = icon("list"), color = "red"),
        valueBox("66%", "Avance"),
        valueBox(10353, "Meta"),
    ),
    h3("AVANCE DE VACUNACION COVID-19 AL 18 DE FEBRERO DEL 2021, PROVINCIAS"),
    h5("PERSONAL DE SALUD VACUNADO"),
    fluidRow(
        box(girafeOutput("grafico2_1"))
    ),
    h3("AVANCE DE VACUNACION COVID-19 AL 18 DE FEBRERO DEL 2021, HOSPITALES"),
    h5("PERSONAL DE SALUD VACUNADO"),
    fluidRow(
        box(highchartOutput("grafico2_2regional")),
        box(highchartOutput("grafico2_2antoniolorena")),
        box(highchartOutput("grafico2_2espinar")),
        box(highchartOutput("grafico2_2quillabamba")),
        box(highchartOutput("grafico2_2sicuani"))
    ),
    h3("AVANCE DE VACUNACION COVID-19 AL 18 DE FEBRERO DEL 2021, REDES"),
    h5("PERSONAL DE SALUD VACUNADO"),
    fluidRow(
        box(highchartOutput("grafico_cusconorte")),
        box(highchartOutput("grafico_cuscosur"))
        # box(highchartOutput("grafico_laconvencion"))
    ),
    fluidRow(
        box(),
        box(),
        box()
    )
))
