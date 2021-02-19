library(data.table)
library(plyr)
library(dplyr)
library(highcharter)
library(readxl)

##########
#  Data  #
##########

# fans_react = fread("https://raw.githubusercontent.com/manasi1096/Highcharts-HIMYM/master/FansReact-HIMYM.csv")
base1 <- read_excel("D:/DIRESA Cusco/MatLab DIRESA/Vacunación/base provincias 18.02.2021.xlsx")

# Paleta de colores

mypal <- c(
  rgb(0, 4, 69, maxColorValue = 255),
  rgb(61, 0, 255, maxColorValue = 255)
)
print(mypal)

##############
#   Gráfico  #
##############

require(highcharter)

highchart() %>% 
  hc_chart(polar = TRUE) %>% 
  hc_title(text = "PROVINCIAS",
           style = list(fontWeight = "bold", fontSize = "25px", color="#144746", useHTML=TRUE),
           align = "center") %>% 
  hc_subtitle(text = "'Avance de vacunación'",
              style = list(fontWeight = "bold"),
              align = "center") %>% 
  hc_xAxis(categories = base1$PROVINCIA,
           style = list(fontWeight = "bold")) %>% 
  # hc_add_theme(hc_theme_sandsignika()) %>% 
  hc_credits(enabled = TRUE,
             text = "Data Source:Based on a study done by Canvs as reported by The Atlantic") %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_colors(mypal) %>% 
  hc_series(
    list(
      name = "Avance",
      data = round(base1$AVANCE),
      pointPlacement = "on",
      type = "area")) %>% 
   hc_tooltip(pointFormat = "{point.y}%")  






























    
    
    