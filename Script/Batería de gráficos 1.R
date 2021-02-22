#---------------------------------------
# GRÁFICOS PARA DASHBOARD VACUNACIÓN   #
#---------------------------------------


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

# Paleta de colores

mypal <- c(
  rgb(255, 99, 7, maxColorValue = 255),
  rgb(233, 41, 15, maxColorValue = 255)
)
print(mypal)

#--------------------------
# 0. E N T I D A D 
#--------------------------


# GORE
#------------------

per=62
col=ifelse(per <= 100,"#011f3f",ifelse(per <=80,"#011f3f","#011f3f")) 

dataLabel <- paste0("<div style=text-align:center><span style=font-size:135%;>", per, 
                    "%</span><br/><span style='font-size:100%;color:#011f3f;'>de avance</div>")

highchart() %>%
  hc_chart(type = "solidgauge",spacingTop=-30) %>%
  hc_title(y=50,text = "GOBIERNO REGIONAL") %>%
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


# ESSALUD
#------------------

per=98
col=ifelse(per <= 100,"#163a5f",ifelse(per <=80,"#163a5f","#163a5f")) 

dataLabel <- paste0("<div style=text-align:center><span style=font-size:135%;>", per, 
                    "%</span><br/><span style='font-size:100%;color:#163a5f;'>de avance</div>")

highchart() %>%
  hc_chart(type = "solidgauge",spacingTop=-30) %>%
  hc_title(y=50,text = "ESSALUD") %>%
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
# 1. P R O V I N C I A S
#--------------------------

#+++++++++++++++++++++++++++++
# Data
#+++++++++++++++++++++++++++++

base1 <- read_excel("D:/DIRESA Cusco/Dashboard_vacunacion/data/base_provincias_20.xlsx")


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
      name = "Meta",
      data = round(base1$MAXIMO),
      pointPlacement = "on",
      type = "line")) %>% 
  hc_tooltip(pointFormat = "{point.y}%")  


#--------------------------
# 2. H O S P I T A L E S
#--------------------------

#+++++++++++++++++++++++++++++
# Gráfico 2
#+++++++++++++++++++++++++++++

# H. REGIONAL DEL CUSCO
#-----------------------------

# Gráfico 2.2. 
#--------------

per=87.9
col=ifelse(per <= 100,"#ff6107",ifelse(per <=80,"#ff6107","#ff6107")) 

dataLabel <- paste0("<div style=text-align:center><span style=font-size:135%;>", per, 
                    "%</span><br/><span style='font-size:100%;color:#ff6107;'>de avance</div>")

highchart() %>%
  hc_chart(type = "solidgauge",spacingTop=-30) %>%
  hc_title(y=50,text = "HOSPITAL REGIONAL DEL CUSCO") %>%
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
    labels=list(x=-5,y=20,style = list(fontSize = "90%"))) %>%  
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
  hc_title(y=50,text = "HOSPITAL ANTONIO LORENA DEL CUSCO") %>%
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
    labels=list(x=-5,y=10,style = list(fontSize = "90%"))) %>%  
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
  hc_title(y=50,text = "HOSPITAL ESPINAR") %>%
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
    labels=list(x=-5,y=10,style = list(fontSize = "90%"))) %>%  
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
  hc_title(y=50,text = "HOSPITAL QUILLABAMBA") %>%
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
    labels=list(x=-5,y=10,style = list(fontSize = "90%"))) %>%  
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
  hc_title(y=50,text = "HOSPITAL SICUANI") %>%
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
    labels=list(x=-5,y=10,style = list(fontSize = "90%"))) %>%  
  hc_add_series(data = c(per),dataLabels=list(y=-20,borderWidth=0, useHTML=TRUE,format = dataLabel) ) %>%hc_colors(col)


#--------------------------
# 3. R E D E S
#--------------------------

# _________________________________________________________
#
#  RED CANAS-CANCHIS-ESPINAR
#__________________________________________________________

# Data 1

data2 <- read_excel("D:/DIRESA Cusco/Dashboard_vacunacion/data/RED Canas Canchis Espinar.xlsx")

require(highcharter)

highchart() %>% 
  hc_chart(polar = TRUE) %>% 
  hc_title(text = "RED CANAS-CANCHIS-ESPINAR",
           style = list(fontWeight = "bold", fontSize = "25px", color="#03071e", useHTML=TRUE),
           align = "center") %>% 
  hc_subtitle(text = "'Avance de vacunación'",
              style = list(fontWeight = "bold"),
              align = "center") %>% 
  hc_xAxis(categories = data2$IPRESS,
           style = list(fontWeight = "bold")) %>% 
  # hc_add_theme(hc_theme_sandsignika()) %>% 
  hc_credits(enabled = TRUE,
             text = "GERESA CUSCO") %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_colors(mypal) %>% 
  hc_series(
    list(
      name = "Avance",
      data = round(data2$AVANCE),
      pointPlacement = "on",
      type = "area"),
    list(
      name = "Avance",
      data = round(data2$MAXIMO),
      pointPlacement = "on",
      type = "line")) %>% 
  hc_tooltip(pointFormat = "{point.y}%")


# _________________________________________________________
#
#  RED CUSCO NORTE
#__________________________________________________________

data3 <- read_excel("D:/DIRESA Cusco/Dashboard_vacunacion/data/RED Cusco Norte.xlsx")


require(highcharter)

highchart() %>% 
  hc_chart(polar = TRUE) %>% 
  hc_title(text = "RED CUSCO NORTE",
           style = list(fontWeight = "bold", fontSize = "25px", color="#03071e", useHTML=TRUE),
           align = "center") %>% 
  hc_subtitle(text = "'Avance de vacunación'",
              style = list(fontWeight = "bold"),
              align = "center") %>% 
  hc_xAxis(categories = data3$IPRESS,
           style = list(fontWeight = "bold")) %>% 
  # hc_add_theme(hc_theme_sandsignika()) %>% 
  hc_credits(enabled = TRUE,
             text = "GERESA CUSCO") %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_colors(mypal) %>% 
  hc_series(
    list(
      name = "Avance",
      data = round(data3$AVANCE),
      pointPlacement = "on",
      type = "area"),
    list(
      name = "Avance",
      data = round(data3$MAXIMO),
      pointPlacement = "on",
      type = "line")) %>% 
  hc_tooltip(pointFormat = "{point.y}%")

# _________________________________________________________
#
#  RED CUSCO SUR
#__________________________________________________________

data4 <- read_excel("D:/DIRESA Cusco/Dashboard_vacunacion/data/RED Cusco Sur.xlsx")


require(highcharter)

highchart() %>% 
  hc_chart(polar = TRUE) %>% 
  hc_title(text = "RED CUSCO SUR",
           style = list(fontWeight = "bold", fontSize = "25px", color="#03071e", useHTML=TRUE),
           align = "center") %>% 
  hc_subtitle(text = "'Avance de vacunación'",
              style = list(fontWeight = "bold"),
              align = "center") %>% 
  hc_xAxis(categories = data4$IPRESS,
           style = list(fontWeight = "bold")) %>% 
  # hc_add_theme(hc_theme_sandsignika()) %>% 
  hc_credits(enabled = TRUE,
             text = "GERESA CUSCO") %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_colors(mypal) %>% 
  hc_series(
    list(
      name = "Avance",
      data = round(data4$AVANCE),
      pointPlacement = "on",
      type = "area"),
    list(
      name = "Avance",
      data = round(data4$MAXIMO),
      pointPlacement = "on",
      type = "line")) %>% 
  hc_tooltip(pointFormat = "{point.y}%")

# _________________________________________________________
#
#  RED LA CONVENCION
#__________________________________________________________

data5 <- read_excel("D:/DIRESA Cusco/Dashboard_vacunacion/data/RED La Convención.xlsx")

require(highcharter)

highchart() %>% 
  hc_chart(polar = TRUE) %>% 
  hc_title(text = "RED LA CONVENCIÓN",
           style = list(fontWeight = "bold", fontSize = "25px", color="#03071e", useHTML=TRUE),
           align = "center") %>% 
  hc_subtitle(text = "'Avance de vacunación'",
              style = list(fontWeight = "bold"),
              align = "center") %>% 
  hc_xAxis(categories = data5$IPRESS,
           style = list(fontWeight = "bold")) %>% 
  # hc_add_theme(hc_theme_sandsignika()) %>% 
  hc_credits(enabled = TRUE,
             text = "GERESA CUSCO") %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_colors(mypal) %>% 
  hc_series(
    list(
      name = "Avance",
      data = round(data5$AVANCE),
      pointPlacement = "on",
      type = "area"),
    list(
      name = "Avance",
      data = round(data5$MAXIMO),
      pointPlacement = "on",
      type = "line")) %>% 
  hc_tooltip(pointFormat = "{point.y}%")


# _________________________________________________________
#
#  RED CHUMBIVILCAS
#__________________________________________________________

# Gráfico 2.1. 
#--------------

per=73
col=ifelse(per <= 100,"#ff6107",ifelse(per <=80,"#ff6107","#ff6107")) 

dataLabel <- paste0("<div style=text-align:center><span style=font-size:135%;>", per, 
                    "%</span><br/><span style='font-size:100%;color:#ff6107;'>de avance</div>")

highchart() %>%
  hc_chart(type = "solidgauge",spacingTop=-30) %>%
  hc_title(y=50,text = "LIVITACA") %>%
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

# Gráfico 2.2. 
#--------------

per=40.7
col=ifelse(per <= 100,"#ff6107",ifelse(per <=80,"#ff6107","#ff6107")) 

dataLabel <- paste0("<div style=text-align:center><span style=font-size:135%;>", per, 
                    "%</span><br/><span style='font-size:100%;color:#ff6107;'>de avance</div>")

highchart() %>%
  hc_chart(type = "solidgauge",spacingTop=-30) %>%
  hc_title(y=50,text = "HOSPITAL SANTO TOMAS") %>%
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


# _________________________________________________________
#
#  RED KIMBIRI-PICHARI
#__________________________________________________________

# Gráfico 2.1. 
#--------------

per=87.9
col=ifelse(per <= 100,"#ff6107",ifelse(per <=80,"#ff6107","#ff6107")) 

dataLabel <- paste0("<div style=text-align:center><span style=font-size:135%;>", per, 
                    "%</span><br/><span style='font-size:100%;color:#ff6107;'>de avance</div>")

highchart() %>%
  hc_chart(type = "solidgauge",spacingTop=-30) %>%
  hc_title(y=50,text = "PICHARI") %>%
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


# Gráfico 2.2. 
#--------------

per=79.1
col=ifelse(per <= 100,"#ff6107",ifelse(per <=80,"#ff6107","#ff6107")) 

dataLabel <- paste0("<div style=text-align:center><span style=font-size:135%;>", per, 
                    "%</span><br/><span style='font-size:100%;color:#ff6107;'>de avance</div>")

highchart() %>%
  hc_chart(type = "solidgauge",spacingTop=-30) %>%
  hc_title(y=50,text = "SAN JUAN DE KIMBIRI-VRAEM") %>%
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

