#+++++++++++++++++++++++++++++
# Librerías
#+++++++++++++++++++++++++++++

library(data.table)
library(plyr)
library(dplyr)
library(highcharter)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggiraph)

#--------------------------
# 1. P R O V I N C I A S
#--------------------------

#+++++++++++++++++++++++++++++
# Data
#+++++++++++++++++++++++++++++

# fans_react = fread("https://raw.githubusercontent.com/manasi1096/Highcharts-HIMYM/master/FansReact-HIMYM.csv")
# base1 <- read_excel("D:/DIRESA Cusco/Dashboard_vacunacion/data/base_provincias_18.xlsx")

base1 <- read_excel("/Users/bran/Documents/GitHub/Dashboard_vacunacion/data/base_provincias_18.xlsx")

# Paleta de colores

mypal <- c(
  rgb(0, 4, 69, maxColorValue = 255),
  rgb(61, 0, 255, maxColorValue = 255)
)
print(mypal)

#+++++++++++++++++++++++++++++
# Gráfico 1
#+++++++++++++++++++++++++++++

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


#--------------------------
# 2. H O S P I T A L E S
#--------------------------

#+++++++++++++++++++++++++++++
# Gráfico 2
#+++++++++++++++++++++++++++++

# Gráfico 2.1.
#---------------

donut_data <- data.frame(type = c("Vacunados", "Por_vacunar"), value = c(3716, 313)) %>%
  mutate(
    percentage = value / 4029,
    hover_text = paste0(type, ": ", value)
  ) %>%
  mutate(percentage_label = paste0(round(100 * percentage, 1), "%"))

donut_plot <- ggplot(donut_data, aes(y = value, fill = type)) +
  geom_bar_interactive(
    aes(x = 1, tooltip = hover_text),
    width = 0.2,
    stat = "identity",
    show.legend = T
  ) +
  labs(fill = "GOBIERNO REGIONAL",
       x = " ") +
  annotate(
    geom = "text",
    x = 0,
    y = 0,
    label = donut_data[["percentage_label"]][donut_data[["type"]] == "Vacunados"],
    size = 18,
    color = "#d00000"
  ) +
  scale_fill_manual(values = c(Vacunados = "#d00000", Por_vacunar = "#03071e")) +
  coord_polar(theta = "y") +
  theme_minimal()

ggiraph(ggobj = donut_plot)


highchart(width = 400, height = 400) %>% 
  hc_chart(type = "solidgauge",backgroundColor = "#F0F0F0",marginTop = 50) %>% 
  hc_title(text = "Activity",style = list(fontSize = "24px")) %>% 
  hc_tooltip(borderWidth = 0,backgroundColor = 'none',shadow = FALSE,style = list(fontSize = '16px'),
             pointFormat = '{series.name}<br><span style="font-size:2em; color: {point.color}; font-weight: bold">{point.y}%</span>',
             positioner = JS("function (labelWidth, labelHeight) {return {x: 200 - labelWidth / 2,y: 180};}")) %>% 
  hc_pane(startAngle = 0,endAngle = 360,
          background = list(
            list(outerRadius = '112%',innerRadius = '88%',backgroundColor = JS("Highcharts.Color('#F62366').setOpacity(0.1).get()"),borderWidth =  0),
            list(outerRadius = '87%',innerRadius = '63%',backgroundColor = JS("Highcharts.Color('#9DFF02').setOpacity(0.1).get()"),borderWidth = 0),
            list(outerRadius = '62%',innerRadius =  '38%',backgroundColor = JS("Highcharts.Color('#0CCDD6').setOpacity(0.1).get()"),borderWidth = 0))) %>% 
  hc_yAxis(min = 0,max = 100,lineWidth = 0,tickPositions = list()) %>% 
  hc_plotOptions(solidgauge = list(borderWidth = '34px',dataLabels = list(enabled = FALSE),linecap = 'round',stickyTracking = FALSE)) %>% 
  hc_add_series(name = "Move",borderColor = JS("Highcharts.getOptions().colors[0]"),data = list(list(color = JS("Highcharts.getOptions().colors[0]"),radius = "100%",innerRadius = "100%",y = 80))) %>% 
  hc_add_series(name = "Exercise",borderColor = JS("Highcharts.getOptions().colors[1]"),data = list(list(color = JS("Highcharts.getOptions().colors[1]"),radius = "75%",innerRadius = "75%",y = 65))) %>% 
  hc_add_series(name = "Stand",borderColor = JS("Highcharts.getOptions().colors[2]"),data = list(list(color = JS("Highcharts.getOptions().colors[2]"),radius = "50%",innerRadius = "50%",y = 50)))


# Gráfico 2.2. 
#--------------

highchart(width = 400, height = 400) %>% 
  hc_chart(type = "solidgauge",backgroundColor = "#F0F0F0",marginTop = 50) %>% 
  hc_title(text = "Activity",style = list(fontSize = "24px")) %>% 
  hc_tooltip(borderWidth = 0,backgroundColor = 'none',shadow = FALSE,style = list(fontSize = '16px'),
             pointFormat = '{series.name}<br><span style="font-size:2em; color: {point.color}; font-weight: bold">{point.y}%</span>',
             positioner = JS("function (labelWidth, labelHeight) {return {x: 200 - labelWidth / 2,y: 180};}")) %>% 
  hc_pane(startAngle = 0,endAngle = 360,
          background = list(
            list(outerRadius = '112%',innerRadius = '88%',backgroundColor = JS("Highcharts.Color('#F62366').setOpacity(0.1).get()"),borderWidth =  0),
            list(outerRadius = '87%',innerRadius = '63%',backgroundColor = JS("Highcharts.Color('#9DFF02').setOpacity(0.1).get()"),borderWidth = 0),
            list(outerRadius = '62%',innerRadius =  '38%',backgroundColor = JS("Highcharts.Color('#0CCDD6').setOpacity(0.1).get()"),borderWidth = 0))) %>% 
  hc_yAxis(min = 0,max = 100,lineWidth = 0,tickPositions = list()) %>% 
  hc_plotOptions(solidgauge = list(borderWidth = '34px',dataLabels = list(enabled = FALSE),linecap = 'round',stickyTracking = FALSE)) %>% 
  hc_add_series(name = "Move",borderColor = JS("Highcharts.getOptions().colors[0]"),data = list(list(color = JS("Highcharts.getOptions().colors[0]"),radius = "100%",innerRadius = "100%",y = 80))) %>% 
  hc_add_series(name = "Exercise",borderColor = JS("Highcharts.getOptions().colors[1]"),data = list(list(color = JS("Highcharts.getOptions().colors[1]"),radius = "75%",innerRadius = "75%",y = 65))) %>% 
  hc_add_series(name = "Stand",borderColor = JS("Highcharts.getOptions().colors[2]"),data = list(list(color = JS("Highcharts.getOptions().colors[2]"),radius = "50%",innerRadius = "50%",y = 50)))




















    
    
    