#--------------------------
# 1. D A T A
#--------------------------

base1 <- fread("https://raw.githubusercontent.com/JasonCruz18/Dashboard_vacunacion/main/data/base_provincias_20.csv")
data2 <- fread("https://raw.githubusercontent.com/JasonCruz18/Dashboard_vacunacion/main/Data/RED%20Canas%20Canchis%20Espinar.csv")
data3 <- fread("https://raw.githubusercontent.com/JasonCruz18/Dashboard_vacunacion/main/Data/RED%20Cusco%20Norte.csv")
data4 <- fread("https://raw.githubusercontent.com/JasonCruz18/Dashboard_vacunacion/main/Data/RED%20Cusco%20Sur.csv")
# data5 <- fread("https://raw.githubusercontent.com/JasonCruz18/Dashboard_vacunacion/main/Data/RED%20La%20Convención.csv")


mypal <- c(
  rgb(0, 4, 69, maxColorValue = 255),
  rgb(61, 0, 255, maxColorValue = 255))

print(mypal)


shinyServer(function(input, output) {

#++++++++++++++++++++++++++----
# Gráfico 1
#+++++++++++++++++++++++++++---
    
  output$grafico1 <- renderHighchart({
    
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
  })

  
  
  
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
  
  output$grafico2_1 <-renderGirafe({
  girafe(ggobj = donut_plot)
  })
  
  
  # Gráfico 2.2. 
  #--------------
  
  per=87.9
  col=ifelse(per <= 100,"#ff6107",ifelse(per <=80,"#ff6107","#ff6107")) 
  
  dataLabel <- paste0("<div style=text-align:center><span style=font-size:135%;>", per, 
                      "%</span><br/><span style='font-size:100%;color:#ff6107;'>de avance</div>")
  
  output$grafico2_2regional <- renderHighchart({
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
  })
    
  # H. ANTONIO LORENA DEL CUSCO
  #-----------------------------
  
  # Gráfico 2.2. 
  #--------------
  
  per=93.2
  col=ifelse(per <= 100,"#e9290f",ifelse(per <=80,"#e9290f","#e9290f")) 
  
  dataLabel <- paste0("<div style=text-align:center><span style=font-size:135%;>", per, 
                      "%</span><br/><span style='font-size:100%;color:#e9290f;'>de avance</div>")
  
  output$grafico2_2antoniolorena <- renderHighchart({
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
  })
    
    
  # H. ESPINAR
  #-----------------------------
  
  # Gráfico 2.2. 
  #--------------
  
  per=67.1
  col=ifelse(per <= 100,"#c40018",ifelse(per <=80,"#c40018","#c40018")) 
  
  dataLabel <- paste0("<div style=text-align:center><span style=font-size:135%;>", per, 
                      "%</span><br/><span style='font-size:100%;color:#c40018;'>de avance</div>")
  
  output$grafico2_2espinar <- renderHighchart({
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
  })
    
  # H. QUILLABAMBA
  #-----------------------------
  
  # Gráfico 2.2. 
  #--------------
  
  per=76.2
  col=ifelse(per <= 100,"#292725",ifelse(per <=80,"#292725","#292725")) 
  
  dataLabel <- paste0("<div style=text-align:center><span style=font-size:135%;>", per, 
                      "%</span><br/><span style='font-size:100%;color:#292725;'>de avance</div>")
  
  output$grafico2_2quillabamba <- renderHighchart({
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
  })
  
  # H. SICUANI
  #-----------------------------
  
  # Gráfico 2.2. 
  #--------------
  
  per=61.2
  col=ifelse(per <= 100,"#011f3f",ifelse(per <=80,"#011f3f","#011f3f")) 
  
  dataLabel <- paste0("<div style=text-align:center><span style=font-size:135%;>", per, 
                      "%</span><br/><span style='font-size:100%;color:#011f3f;'>de avance</div>")
  
  output$grafico2_2sicuani <- renderHighchart({
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
  })
  
  #--------------------------
  # 3. R E D E S
  #--------------------------
  
  # _________________________________________________________
  #
  #  RED CANAS-CANCHIS-ESPINAR
  #__________________________________________________________
  
  # Data 1
  
  output$grafico_canascanchis <- renderHighchart({
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
  })
  
  # _________________________________________________________
  #
  #  RED CUSCO NORTE
  #__________________________________________________________
  
  
  output$grafico_cusconorte <- renderHighchart({
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
  })
  
  # _________________________________________________________
  #
  #  RED CUSCO SUR
  #__________________________________________________________
  

  output$grafico_cuscosur <- renderHighchart({
    
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
  })
  
  # _________________________________________________________
  #
  #  RED LA CONVENCION
  #__________________________________________________________
  
  
  # output$grafico_laconvencion <- renderHighchart({
  # highchart() %>% 
  #   hc_chart(polar = TRUE) %>% 
  #   hc_title(text = "RED LA CONVENCIÓN",
  #            style = list(fontWeight = "bold", fontSize = "25px", color="#03071e", useHTML=TRUE),
  #            align = "center") %>% 
  #   hc_subtitle(text = "'Avance de vacunación'",
  #               style = list(fontWeight = "bold"),
  #               align = "center") %>% 
  #   hc_xAxis(categories = data5$IPRESS,
  #            style = list(fontWeight = "bold")) %>% 
  #   # hc_add_theme(hc_theme_sandsignika()) %>% 
  #   hc_credits(enabled = TRUE,
  #              text = "GERESA CUSCO") %>% 
  #   hc_legend(enabled = TRUE) %>% 
  #   hc_colors(mypal) %>% 
  #   hc_series(
  #     list(
  #       name = "Avance",
  #       data = round(data5$AVANCE),
  #       pointPlacement = "on",
  #       type = "area"),
  #     list(
  #       name = "Avance",
  #       data = round(data5$MAXIMO),
  #       pointPlacement = "on",
  #       type = "line")) %>% 
  #   hc_tooltip(pointFormat = "{point.y}%")
  # })
  
  
  
  
})
  
  
  
  
  