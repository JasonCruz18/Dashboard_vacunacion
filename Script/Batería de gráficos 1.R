#+++++++++++++++++++++++++++++
# Librerías
#+++++++++++++++++++++++++++++

library(data.table)
library(plyr)
library(dplyr)
library(highcharter)
library(readxl)
library(ggplot2)
library(ggiraph)
library(readxl)
library(fmsb)


#--------------------------
# 1. P R O V I N C I A S
#--------------------------

#+++++++++++++++++++++++++++++
# Data
#+++++++++++++++++++++++++++++

# fans_react = fread("https://raw.githubusercontent.com/manasi1096/Highcharts-HIMYM/master/FansReact-HIMYM.csv")

base1 <- read_excel("/Users/bran/Documents/GitHub/Dashboard_vacunacion/data/base_provincias_18.xlsx")

base1 <- read_excel("D:/DIRESA Cusco/Dashboard_vacunacion/data/base_provincias_20.xlsx")


# Paleta de colores

mypal1 <- c(
  rgb(255, 99, 7, maxColorValue = 255),
  rgb(233, 41, 15, maxColorValue = 255)
)
print(mypal1)

#+++++++++++++++++++++++++++++
# Gráfico 1
#+++++++++++++++++++++++++++++

require(highcharter)

highchart() %>% 
  hc_chart(polar = TRUE) %>% 
  hc_title(text = "PROVINCIAS",
           style = list(fontWeight = "bold", fontSize = "25px", color="#03071e", useHTML=TRUE),
           align = "center") %>% 
  hc_subtitle(text = "'Avance de vacunación'",
              style = list(fontWeight = "bold"),
              align = "center") %>% 
  hc_xAxis(categories = base1$PROVINCIA,
           style = list(fontWeight = "bold")) %>% 
  # hc_add_theme(hc_theme_sandsignika()) %>% 
  hc_credits(enabled = TRUE,
             text = "GERESA CUSCO") %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_colors(mypal) %>% 
  hc_series(
    list(
      name = "Avance",
      data = round(base1$AVANCE),
      pointPlacement = "on",
      type = "area"),
    list(
      name = "Avance",
      data = round(base1$MAXIMO),
      pointPlacement = "on",
      type = "line")) %>% 
   hc_tooltip(pointFormat = "{point.y}%")  


#--------------------------
# 2. H O S P I T A L E S
#--------------------------

# Gráfico 2.2. 
#--------------

# highchart(width = 400, height = 400) %>% 
#   hc_chart(type = "solidgauge",backgroundColor = "#F0F0F0",marginTop = 50) %>% 
#   hc_title(text = "Activity",style = list(fontSize = "24px")) %>% 
#   hc_tooltip(borderWidth = 0,backgroundColor = 'none',shadow = FALSE,style = list(fontSize = '16px'),
#              pointFormat = '{series.name}<br><span style="font-size:2em; color: {point.color}; font-weight: bold">{point.y}%</span>',
#              positioner = JS("function (labelWidth, labelHeight) {return {x: 200 - labelWidth / 2,y: 180};}")) %>% 
#   hc_pane(startAngle = 0,endAngle = 360,
#           background = list(
#             list(outerRadius = '112%',innerRadius = '88%',backgroundColor = JS("Highcharts.Color('#F62366').setOpacity(0.1).get()"),borderWidth =  0),
#             list(outerRadius = '87%',innerRadius = '63%',backgroundColor = JS("Highcharts.Color('#9DFF02').setOpacity(0.1).get()"),borderWidth = 0),
#             list(outerRadius = '62%',innerRadius =  '38%',backgroundColor = JS("Highcharts.Color('#0CCDD6').setOpacity(0.1).get()"),borderWidth = 0))) %>% 
#   hc_yAxis(min = 0,max = 100,lineWidth = 0,tickPositions = list()) %>% 
#   hc_plotOptions(solidgauge = list(borderWidth = '34px',dataLabels = list(enabled = FALSE),linecap = 'round',stickyTracking = FALSE)) %>% 
#   hc_add_series(name = "Move",borderColor = JS("Highcharts.getOptions().colors[0]"),data = list(list(color = JS("Highcharts.getOptions().colors[0]"),radius = "100%",innerRadius = "100%",y = 80))) %>% 
#   hc_add_series(name = "Exercise",borderColor = JS("Highcharts.getOptions().colors[1]"),data = list(list(color = JS("Highcharts.getOptions().colors[1]"),radius = "75%",innerRadius = "75%",y = 65))) %>% 
#   hc_add_series(name = "Stand",borderColor = JS("Highcharts.getOptions().colors[2]"),data = list(list(color = JS("Highcharts.getOptions().colors[2]"),radius = "50%",innerRadius = "50%",y = 50)))

#+++++++++++++++++++++++++++++
# Gráfico 2
#+++++++++++++++++++++++++++++

# H. REGIONAL DEL CUSCO
#-----------------------------

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

# Gráfico 2.2. 
#--------------

per=87.9
col=ifelse(per <= 100,"#ff6107",ifelse(per <=80,"#ff6107","#ff6107")) 

dataLabel <- paste0("<div style=text-align:center><span style=font-size:135%;>", per, 
                    "%</span><br/><span style='font-size:100%;color:#ff6107;'>de avance</div>")

highchart() %>%
  hc_chart(type = "solidgauge",spacingTop=-30) %>%
  # hc_title(y=50,text = "Planificación de Actividades") %>%
  hc_pane(startAngle = -90,
          endAngle = 90,background= list(
            outerRadius = '100%',
            innerRadius = '60%',
            backgroundColor = JS("Highcharts.Color('#fef2bf').setOpacity(0.5).get()"),
            shape="arc" )) %>%  
  hc_yAxis(
    # stops=col.stops,
    lineWidth=0,
    minorTickWidth=0,
    tickAmount=2,
    min = 0,
    max = 100,
    labels=list(x=-50,y=10,style = list(fontSize = "90%"))) %>%  
  hc_add_series(data = c(per),dataLabels=list(y=-20,borderWidth=0, useHTML=TRUE,format = dataLabel) ) %>%hc_colors(col)

# H. ANTONIO LORENA DEL CUSCO
#-----------------------------

# Gráfico 2.2. 
#--------------

per=93.2
col=ifelse(per <= 100,"#e9290f",ifelse(per <=80,"#e9290f","#e9290f")) 

dataLabel <- paste0("<div style=text-align:center><span style=font-size:135%;>", per, 
                    "%</span><br/><span style='font-size:100%;color:#e9290f;'>de avance</div>")

highchart() %>%
  hc_chart(type = "solidgauge",spacingTop=-30) %>%
  # hc_title(y=50,text = "Planificación de Actividades") %>%
  hc_pane(startAngle = -90,
          endAngle = 90,background= list(
            outerRadius = '100%',
            innerRadius = '60%',
            backgroundColor = JS("Highcharts.Color('#fef2bf').setOpacity(0.1).get()"),
            shape="arc" )) %>%  
  hc_yAxis(
    # stops=col.stops,
    lineWidth=0,
    minorTickWidth=0,
    tickAmount=2,
    min = 0,
    max = 100,
    labels=list(x=-50,y=10,style = list(fontSize = "90%"))) %>%  
  hc_add_series(data = c(per),dataLabels=list(y=-20,borderWidth=0, useHTML=TRUE,format = dataLabel) ) %>%hc_colors(col)

# H. ESPINAR
#-----------------------------

# Gráfico 2.2. 
#--------------

per=67.1
col=ifelse(per <= 100,"#c40018",ifelse(per <=80,"#c40018","#c40018")) 

dataLabel <- paste0("<div style=text-align:center><span style=font-size:135%;>", per, 
                    "%</span><br/><span style='font-size:100%;color:#c40018;'>de avance</div>")

highchart() %>%
  hc_chart(type = "solidgauge",spacingTop=-30) %>%
  # hc_title(y=50,text = "Planificación de Actividades") %>%
  hc_pane(startAngle = -90,
          endAngle = 90,background= list(
            outerRadius = '100%',
            innerRadius = '60%',
            backgroundColor = JS("Highcharts.Color('#fef2bf').setOpacity(0.1).get()"),
            shape="arc" )) %>%  
  hc_yAxis(
    # stops=col.stops,
    lineWidth=0,
    minorTickWidth=0,
    tickAmount=2,
    min = 0,
    max = 100,
    labels=list(x=-50,y=10,style = list(fontSize = "90%"))) %>%  
  hc_add_series(data = c(per),dataLabels=list(y=-20,borderWidth=0, useHTML=TRUE,format = dataLabel) ) %>%hc_colors(col)

# H. QUILLABAMBA
#-----------------------------

# Gráfico 2.2. 
#--------------

per=76.2
col=ifelse(per <= 100,"#292725",ifelse(per <=80,"#292725","#292725")) 

dataLabel <- paste0("<div style=text-align:center><span style=font-size:135%;>", per, 
                    "%</span><br/><span style='font-size:100%;color:#292725;'>de avance</div>")

highchart() %>%
  hc_chart(type = "solidgauge",spacingTop=-30) %>%
  # hc_title(y=50,text = "Planificación de Actividades") %>%
  hc_pane(startAngle = -90,
          endAngle = 90,background= list(
            outerRadius = '100%',
            innerRadius = '60%',
            backgroundColor = JS("Highcharts.Color('#fef2bf').setOpacity(0.1).get()"),
            shape="arc" )) %>%  
  hc_yAxis(
    # stops=col.stops,
    lineWidth=0,
    minorTickWidth=0,
    tickAmount=2,
    min = 0,
    max = 100,
    labels=list(x=-50,y=10,style = list(fontSize = "90%"))) %>%  
  hc_add_series(data = c(per),dataLabels=list(y=-20,borderWidth=0, useHTML=TRUE,format = dataLabel) ) %>%hc_colors(col)

# H. SICUANI
#-----------------------------

# Gráfico 2.2. 
#--------------

per=61.2
col=ifelse(per <= 100,"#011f3f",ifelse(per <=80,"#011f3f","#011f3f")) 

dataLabel <- paste0("<div style=text-align:center><span style=font-size:135%;>", per, 
                    "%</span><br/><span style='font-size:100%;color:#011f3f;'>de avance</div>")

highchart() %>%
  hc_chart(type = "solidgauge",spacingTop=-30) %>%
  # hc_title(y=50,text = "Planificación de Actividades") %>%
  hc_pane(startAngle = -90,
          endAngle = 90,background= list(
            outerRadius = '100%',
            innerRadius = '60%',
            backgroundColor = JS("Highcharts.Color('#fef2bf').setOpacity(0.1).get()"),
            shape="arc" )) %>%  
  hc_yAxis(
    # stops=col.stops,
    lineWidth=0,
    minorTickWidth=0,
    tickAmount=2,
    min = 0,
    max = 100,
    labels=list(x=-50,y=10,style = list(fontSize = "90%"))) %>%  
  hc_add_series(data = c(per),dataLabels=list(y=-20,borderWidth=0, useHTML=TRUE,format = dataLabel) ) %>%hc_colors(col)


#--------------------------
# 3. R E D E S
#--------------------------

# _________________________________________________________
#
#  RED CANAS-CANCHIS-ESPINAR
#__________________________________________________________

# Data 1

data2 <- read_excel("D:/DIRESA Cusco/MatLab DIRESA/Vacunación/Canas Canchis Espinar.xlsx")

require(highcharter)

highchart() %>% 
  hc_chart(polar = TRUE) %>% 
  hc_title(text = "PROVINCIAS",
           style = list(fontWeight = "bold", fontSize = "25px", color="#03071e", useHTML=TRUE),
           align = "center") %>% 
  hc_subtitle(text = "'Avance de vacunación'",
              style = list(fontWeight = "bold"),
              align = "center") %>% 
  hc_xAxis(categories = base1$PROVINCIA,
           style = list(fontWeight = "bold")) %>% 
  # hc_add_theme(hc_theme_sandsignika()) %>% 
  hc_credits(enabled = TRUE,
             text = "GERESA CUSCO") %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_colors(mypal) %>% 
  hc_series(
    list(
      name = "Avance",
      data = round(base1$AVANCE),
      pointPlacement = "on",
      type = "area"),
    list(
      name = "Avance",
      data = round(base1$MAXIMO),
      pointPlacement = "on",
      type = "line")) %>% 
  hc_tooltip(pointFormat = "{point.y}%")


# _________________________________________________________
#
#  RED CUSCO NORTE
#__________________________________________________________

data3 <- read_excel("D:/DIRESA Cusco/MatLab DIRESA/Vacunación/Cusco Norte.xlsx")


# Define fill colors

colors_line <- c(scales::alpha("#e9290f", 1))
colors_fill <- c(scales::alpha("#e9290f", 0.8))

# create a plot

radarchart(data3, axisHAL=1,
           seg=5, 
           pcol = colors_line,
           pfcol = colors_fill,
           plwd = 3,
           plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           vlcex=0.8,
           pty = 16
)


# _________________________________________________________
#
#  RED CUSCO SUR
#__________________________________________________________

data4 <- read_excel("D:/DIRESA Cusco/MatLab DIRESA/Vacunación/Cusco Sur.xlsx")


# Define fill colors

colors_line <- c(scales::alpha("#c40018", 1))
colors_fill <- c(scales::alpha("#c40018", 0.8))

# create a plot

radarchart(data4, axisHAL=1,
           seg=5, 
           pcol = colors_line,
           pfcol = colors_fill,
           plwd = 3,
           plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           vlcex=0.8,
           pty = 16
)

# _________________________________________________________
#
#  RED LA CONVENCION
#__________________________________________________________

data5 <- read_excel("D:/DIRESA Cusco/MatLab DIRESA/Vacunación/La Convención.xlsx")

# Define fill colors

colors_line <- c(scales::alpha("#292725", 1))
colors_fill <- c(scales::alpha("#292725", 0.8))

# create a plot

radarchart(data5, axisHAL=1,
           seg=5, 
           pcol = colors_line,
           pfcol = colors_fill,
           plwd = 3,
           plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           vlcex=0.8,
           pty = 16
)













    
    
    