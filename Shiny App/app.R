library("shiny")
library("shinythemes")
library("tseries")
library("forecast")
library("lmtest")
library("readxl")
library("gtable")
library('ggplot2')
library('dplyr')
library('plotly')
library('hrbrthemes')
library('cowplot')
library("psych")
library('FinTS')


# CARGANDO DATOS --------------------------------------------------------------------------------------------# CARGA DE DATOS --------------------------------------------------------------------------------------------
 file <- "Datos/Turismo_Internacional.csv"

# Estos datos estan dados en miles.
turismo <- read.csv(file)

# Convertimos a serie de tiempo tomando solo los primeros 38 anios para poder hacer la observacion 
# y comparacion de nuestra prediccion con el Ultimo anio dado, que sera el 2019. 
ts1 <- ts(turismo$Turismo, start = c(1980, 1), end = c(2018,12), frequency = 12)

# MODELO-------------------------------------------------------------------------------------

# Serie de tiempo a dataframe 
ts_df<- data.frame(Fecha = as.Date(yearmon(time(ts1))), Turistas = ts1)

# SARIMA(13,1,0)(12,0,0)
SARIMA_13_1_0_12_0_0 <- arima(log(ts1), order = c(13, 1, 0), seasonal = list(order = c(1,0,0), period = 12))




# Define UI for app that draws a histogram ----
ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage(
                    # theme = "flatly",  # <--- To use a theme, uncomment this
                    "Turismo Internacional",
                    tabPanel("Predicción",
                             sidebarPanel(
                                 
                                 # Input: Slider for the number of anios ----
                                 sliderInput(inputId = "anios_predecir",
                                             label = "Años a predecir:",
                                             min = 1,
                                             max = 10,
                                             value = 2)
                                 
                             ), # sidebarPanel
                             mainPanel(
                                 
                                 plotOutput(outputId = "serie_tiempo")
                                 
                             ) # mainPanel
                             
                    ), # Navbar 1, tabPanel
                    tabPanel("Mapas de Calor",
                             sidebarPanel(
                                 
                                 # Input: Slider for the number of anios ----
                                 sliderInput(inputId = "anio_pais",
                                             label = "Año",
                                             min = 1992,
                                             max = 2019,
                                             value = 1992,
                                             step = 1)
                                 
                             ), # sidebarPanel
                             mainPanel(
                                 h4("Mapa de Calor de la cantidad de turistas internacionales para cada estado."),
                                 imageOutput("myImage")
                                 
                             ) # mainPanel
                             
                    ),
                    tabPanel("Navbar 3", "This panel is intentionally left blank")
                    
                ) # navbarPage
) # fluidPage

# Define server logic required to draw a histogram ----
server <- function(input, output) {
    
    # GRAFICA DE PREDICCIÓN
    output$serie_tiempo <- renderPlot({
        
        anios <- input$anios_predecir #Se mueve para los anios que se quieran predecir
        anios_title <- 2019 + anios #El tamanio del grafico
        
        # Dado lo anterior veamos el intervalo de confianza a una predicción de los años que se quieran
        predic <-predict(SARIMA_13_1_0_12_0_0, n.ahead = anios*12) # Ya que trabajamos en meses
        
        # Prediccion a dataframe 
        predic_df<- data.frame(Fecha = as.Date(yearmon(time(predic$pred))), Turistas = exp(predic$pred))
        
        # Limites 
        U <- exp(predic$pred + predic$se) 
        L <- exp(predic$pred - predic$se)
        limites<- data.frame(Fecha = as.Date(yearmon(time(U))), li = L, ls = U)
        predic_df1 <- bind_cols(predic_df,limites[-1])
        
        # Grafica de la serie de tiempo original 2015-2018 junto con la prediccion 2019-2020
        p_data_predic <- ggplot(bind_rows(ts_df,predic_df1) ,aes(x=Fecha)) + 
            geom_line(aes(y=Turistas),color= '#386890', size = 1.6, alpha = .5) + 
            geom_ribbon(aes(ymin = li,ymax = ls), fill = "black", alpha=.2)  +
            labs(title=paste0("Turismo Internacional \n Predicción 2019", " - ", anios + 2018),
                 y = "Turistas en Miles") + 
            scale_x_date(limits = c(as.Date("2015-01-01"), as.Date(paste(anios_title,"-01-01", sep = '')))) +
            theme(plot.title = element_text(hjust = 0.5,size=22, face="bold.italic"),
                  axis.title.x = element_text(hjust = 0.5, size=14, face="bold"),
                  axis.title.y = element_text(hjust = 0.5, size=14, face="bold"),
                  plot.caption = element_text(face="bold.italic")) + 
            annotate(geom = "text", x = as.Date("2015-06-06"), y = 5500, col="black", 
                     label = paste("'MAPE del 2019: '*","4.36"), parse=T)
        
        # Plot de la grafica
        p_data_predic
        
    })
    
    
    # HEATMAP POR ESTADO
    output$myImage <- renderImage({
        
        # Return a list containing the filename
        list(src = paste0("Datos/Imagenes/Imagen_", input$anio_pais, ".png"),
             contentType = 'image/png',
             width = 800,
             height = 500,
             alt = "This is alternate text")
    }, deleteFile = FALSE)
    
}



# Create Shiny app ----
shinyApp(ui = ui, server = server)

