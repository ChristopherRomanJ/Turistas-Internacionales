library("tseries")
library("forecast")
library("lmtest")
library("readxl")
library("gtable")
library('ggplot2')
library('dplyr')
library('plotly')
library('hrbrthemes')
#library('tidyverse')
library('cowplot')
library("psych")
#library('dySeries')
library('FinTS')

path <- "C:/Users/ferro/Desktop/Series de tiempo/Proyecto final/ENTREGAMOS/Datos"
setwd(path)
dir()

# CARGA DE DATOS --------------------------------------------------------------------------------------------
  
  # Estos datos están dados en miles.
    turismo <- read.csv("Turismo_Internacional.csv")
    turismo
  # Convertimos a serie de tiempo tomando solo los primeros 38 años para así poder hacer la observación 
  # y comparación de nuestra predicción con el último año dado, que será el 2019. 
    ts1 <- ts(turismo$Turismo, start = c(1980, 1), end = c(2018,12), frequency = 12)
  
# ANÁLISIS DESCRIPTIVO --------------------------------------------------------------------------------------
  
  # Serie de tiempo a dataframe 
    ts_df<- data.frame(Fecha = as.Date(yearmon(time(ts1))), Turistas = ts1)
    ts_df
  # Gráfica de toda la serie de tiempo
    p_data <- ggplot(ts_df, aes(x=Fecha)) + geom_line(aes(y=Turistas),color= '#386890', size = 1.6, 
                                                      alpha = .5) + theme_ipsum() + 
      labs(title="Turismo Internacional / Miles", subtitle =  "1980 - 2018", 
           caption = 'Fuente: Elaboración propia con base en los datos de DATATUR',
           tag = 'Figura 1',x = '\n Fecha') +
      theme(plot.title = element_text(hjust = 0.5,size=22, face="bold.italic"),
          plot.subtitle = element_text(hjust = 0.5,size=17, face="bold.italic"),
          axis.title.x = element_text(hjust = 0.5, size=14, face="bold"),
          axis.title.y = element_text(hjust = 0.5, size=14, face="bold"),
          plot.tag = element_text(face="italic"))
    ggplotly(p_data)
    #p_data

  # Histograma
    histo <- ggplot(ts_df,aes(x = Turistas))+ geom_histogram(bins = 20, fill='#386890', alpha=.5)+
      ylab("Frecuencia") + theme_ipsum()+ 
      labs(title=" Histograma", subtitle = "Turismo Internacional",
           caption = 'Fuente: Elaboración propia con base en los datos de DATATUR', tag = 'Figura 2') +
      theme(plot.title = element_text(hjust = 0.5,size=22, face="bold.italic"),
            plot.subtitle = element_text(hjust = 0.5,size=17, face="bold.italic"),
            axis.title.x = element_text(hjust = 0.5, size=14, face="bold"),
            axis.title.y = element_text(hjust = 0.5,size=14, face="bold"),
            legend.title = element_text(hjust = 0.5, size=10, face="bold"), 
            plot.tag = element_text(face="italic")) 
    
    ggplotly(histo)
    #histo
    
  # Box Plot
    box_p <- ggplot(ts_df, aes(y=Turistas)) + geom_boxplot(size=.8,alpha =.6) + 
      #geom_jitter(mapping = aes (x = 0), alpha=.8, color='gray')+
      theme_ipsum()+
      labs(title=" Diagrama de caja", subtitle = "Turismo Internacional", 
           caption = 'Fuente: Elaboración propia con base en los datos de DATATUR',
           tag = 'Figura 3') + stat_summary(fun = mean, geom = "point", shape = 18, 
                                           size = 4,mapping = aes (x = 0),
                                           color = '#386890', alpha=.8)+
      theme(plot.title = element_text(hjust = 0.5,size=22, face="bold.italic"),
            plot.subtitle = element_text(hjust = 0.5,size=17, face="bold.italic"),
            axis.title.x = element_text(color = 'white', hjust = 0.5, size=14, face="bold"),
            axis.title.y = element_text(hjust = 0.5,size=14, face="bold"),
            legend.title = element_text(hjust = 0.5, size=10, face="bold"), 
            plot.tag = element_text(face="italic")) 
    
    ggplotly(box_p)
    #box_p 
    
    
  # Gráfica del movimiento del turismo internacional dados los sexenios, data
    #SEXENIOS
    #1976-1980-1982 José López Portillo y Pacheco
    #1982-1988 Miguel de la Madrid Hurtado
    #1988-1994 Carlos Salinas de Gortari
    #1994-2000 Ernesto zedillo Ponce de León
    #2000-2006 Vicente Fox de Quesada
    #2006-2012 Felipe Calderón Hnojosa
    #2012-2018 Enríque Peña Nieto

    JLP <- ts_df %>% select(Fecha, Turistas) %>% 
      filter(between(Fecha, as.Date("1980-01-01"), as.Date("1982-12-01")))
    JLP["1976-1982"] <- JLP[-1]
    JLP$Turistas <- NULL
    JLP$orden <- format(JLP$Fecha, "%Y")
    JLP$id <- 37:72
    
    Sexenios <- data.frame(Fecha = ts_df[1:72,]$Fecha)
      for (i in 1983:2013){
        aux <- ts_df %>% select(Fecha, Turistas) %>% 
          filter(between(Fecha, as.Date(paste(i,"-01-01", sep = '')), as.Date(paste(i+6-1,"-12-01", sep = ''))))
        col <- paste(i-1,'-',i+6-1, sep = '')
        aux[col] <- aux[-1]
        aux$Turistas <- NULL
        Sexenios <- cbind(Sexenios, aux[-1])
      }
    
    Sexenios <- select(Sexenios,"Fecha","1982-1988","1988-1994","1994-2000","2000-2006","2006-2012","2012-2018")
    Sexenios$orden <- format(Sexenios$Fecha, "%Y")
    Sexenios$id <- 1:72
    
      for (i in 1:6){
        Sexenios$orden[Sexenios$orden == 1980+i-1] <- i
      }
      for (i in 1:3){
        JLP$orden[JLP$orden == 1980+i-1] <- i+3
      }
    
    JLP
    Sexenios
    
    Sexenios_total <- left_join(Sexenios,select(JLP,colnames(JLP)[-1]), by = c("orden",'id'))
    Sexenios_total <-subset(Sexenios_total, select = - c(orden,id))
    Sexenios_total
    colnames(Sexenios_total) <- c('Años', 'MDLMH','CSDG', 'EZPDL', 'VFDQ','FCH','EPN', 'JLPP')
    
  # Gráfico de los sexenios 
    p_sexenios <- ggplot(Sexenios_total, aes(x=Años)) +
      ylab("Turistas") +
      geom_line(aes(y=JLPP, color = 'Portillo'), size = 1.6,alpha=.6 ,show.legend = TRUE) +
      geom_line(aes(y=MDLMH, color = 'De la Madrid'), size = 1.6, alpha=.6, show.legend = TRUE) + 
      geom_line(aes(y=CSDG,color = 'Salinas'), size = 1.6, alpha=.6, show.legend = TRUE) +
      geom_line(aes(y=EZPDL, color = "Zedillo"), size = 1.6, alpha=.6, show.legend = TRUE) +
      geom_line(aes(y=VFDQ, color = "Fox de Quesada"), size = 1.6, alpha=.7, show.legend = TRUE) +
      geom_line(aes(y=FCH, color = "Calderón"), size = 1.6, alpha=.7, show.legend = TRUE) +
      geom_line(aes(y=EPN, color = "Peña Nieto"), size = 1.6, alpha=.5, show.legend = TRUE) +
      theme_ipsum()+ labs(title="Turismo Internacional / Miles", subtitle = "Sexenios", 
                          caption = 'Fuente: Elaboración propia con base en los datos de DATATUR',
                          tag = 'Figura 4') +
      scale_color_manual('Presidentes' , values = c("Portillo" = '#FD8B1B',"De la Madrid" = '#A17DEB',
                                                    'Salinas'='#7BB94F',"Zedillo"='#EB7DCA',
                                                    "Fox de Quesada"='#FFCB5B',"Calderón"='#FF3636',
                                                    "Peña Nieto"='#59C4FF'))+
      theme(plot.title = element_text(hjust = 0.5,size=22, face="bold.italic"),
            plot.subtitle = element_text(hjust = 0.5,size=17, face="bold.italic"),
            axis.text.x = element_text(colour="white"),
            axis.title.x = element_text(hjust = 0.5, size=14, face="bold"),
            axis.title.y = element_text(hjust = 0.5,size=14, face="bold"),
            legend.title = element_text(hjust = 0.5, size=10, face="bold"), 
            plot.tag = element_text(face="italic"))        
    
    ggplotly(p_sexenios)
    #p_sexenios
    
  
    summary(ts_df$Turistas)
    describe(ts_df$Turistas)

# SERIE DE TIEMPO -------------------------------------------------------------------------------------------
  
  # Antes de comenzar nos intereza hacer una función para graficar al gusto la ACF y PACF, así entonces
    acf_pacf= function(data, funcion , alpha= 0.05, color_b){
      lag = 1
      df = with(data, data.frame(lag, acf))
      # IC alpha
      lim_sup= qnorm((1 + (1 - alpha))/2)/sqrt(data$n.used)
      lim_inf= -lim_sup
      # Grafico
      ggplot(data = df, mapping = aes(x = lag, y = acf) ) + geom_hline(aes(yintercept = 0)) +
        geom_segment(mapping = aes(xend = lag, yend = 0), color= '#386890', size=1.5, alpha =.5) + 
        labs(y= funcion) + xlab("\n Rezagos") + geom_hline(aes(yintercept = lim_sup), linetype = 2, 
                                                      size = 1,color = 'black', alpha = .6) +
        geom_hline(aes(yintercept = lim_inf), linetype = 2, size = 1, color = 'black', alpha = .6) + 
        theme_ipsum()+ theme(axis.title.x = element_text(hjust = 0.5, size=14, face="bold"),
                             axis.title.y = element_text(hjust = 0.5,size=14, face="bold")) 
      }
    # Dado esto comenzamos graficando la ACF y PACF de los datos originales dados en la figura 4
      acf <- acf_pacf(data=acf(ts1,lag.max=100,plot= F), funcion="ACF",color_b = '#386890')
      pacf <- acf_pacf(data=acf(ts1,lag.max=100,type = "partial", plot= F),funcion= "PACF",color_b = '#386890')
      plot_grid(acf, pacf, ncol = 2, nrow = 1) + 
        labs(title=" ACF y PACF", subtitle= "Turismo Internacional", 
             caption = 'Fuente: Elaboración propia con base en los datos de DATATUR',
             tag = 'Figura 5') + theme_ipsum()+ 
        theme(plot.title = element_text(hjust = 0.5,size=24, face="bold.italic"),
              plot.subtitle = element_text(hjust = 0.5,size=19, face="bold.italic"),
              plot.tag = element_text(face="italic",size = 16), plot.caption = element_text(size = 12)) 
     
       
      
    # Como vimos, la desviación estandár es grande, por tanto la varianza lo es así que la estabilizaremos 
    # aplicando logaritmo 
      ts1_l <- log(ts1)
    # Serie de tiempo a dataframe 
      tsl_df<- data.frame(Fecha = as.Date(yearmon(time(ts1_l))), Turistas = ts1_l)
      tsl_df
    # Gráfica de toda la serie de tiempo aplicando el logaritmo
      p_datal <- ggplot(tsl_df, aes(x=Fecha)) + geom_line(aes(y=Turistas),color= '#386890', size = 1.6, 
                                                        alpha = .5) + theme_ipsum() + 
        labs(title="Turismo Internacional / Miles - Estabilización de la varianza", subtitle =  "1980 - 2018", 
             caption = 'Fuente: Elaboración propia con base en los datos de DATATUR',
             tag = 'Figura 6',x = '\n Fecha') +
        theme(plot.title = element_text(hjust = 0.5,size=22, face="bold.italic"),
              plot.subtitle = element_text(hjust = 0.5,size=17, face="bold.italic"),
              axis.title.x = element_text(hjust = 0.5, size=14, face="bold"),
              axis.title.y = element_text(hjust = 0.5, size=14, face="bold"),
              plot.tag = element_text(face="italic"))
      ggplotly(p_datal)
      #p_datal
    # Ahora veamos con que media y varianza contamos
      print(paste0("La media es: ", mean(ts1_l)))
      print(paste0("La varianza es: ", var(ts1_l)))
    # Graficamos la ACF y PACF de los datos dados en la figura 6
      acf_l <- acf_pacf(data=acf(ts1_l,lag.max=100,plot= F), funcion="ACF",color_b = '#386890')
      pacf_l <- acf_pacf(data=acf(ts1_l,lag.max=100,type = "partial", plot= F),funcion= "PACF",
                         color_b = '#386890')
      plot_grid(acf_l, pacf_l, ncol = 2, nrow = 1) + 
        labs(title="ACF y PACF - Estabilización de la varianza", subtitle= "Turismo Internacional", 
             caption = 'Fuente: Elaboración propia con base en los datos de DATATUR',
             tag = 'Figura 7') + theme_ipsum()+ 
        theme(plot.title = element_text(hjust = 0.5,size=24, face="bold.italic"),
              plot.subtitle = element_text(hjust = 0.5,size=19, face="bold.italic"),
              plot.tag = element_text(face="italic",size = 16), plot.caption = element_text(size = 12)) 
     
       
      
    # Ahora bien, buscamos eliminar la tendencia que como vemos desde un inicio hay. Aplicar el logaritmo solo 
    # nos sirvio para estabilizar la varianza.
      ts1_ld <- diff(ts1_l, 1)
    # Serie de tiempo a dataframe 
      tsld_df<- data.frame(Fecha = as.Date(yearmon(time(ts1_ld))), Turistas = ts1_ld)
      tsld_df
    # Gráfica de toda la serie de tiempo aplicando el logaritmo
      p_datald <- ggplot(tsld_df, aes(x=Fecha)) + geom_line(aes(y=Turistas),color= '#386890', size = 1.6, 
                                                          alpha = .5) + theme_ipsum() + 
        labs(title="Turismo Internacional / Miles \n Estabilización de la varianza y eliminación de tendencia", 
             subtitle =  "1980 - 2018", caption = 'Fuente: Elaboración propia con base en los datos de DATATUR',
             tag = 'Figura 8',x = '\n Fecha') +
        theme(plot.title = element_text(hjust = 0.5,size=22, face="bold.italic"),
              plot.subtitle = element_text(hjust = 0.5,size=17, face="bold.italic"),
              axis.title.x = element_text(hjust = 0.5, size=14, face="bold"),
              axis.title.y = element_text(hjust = 0.5, size=14, face="bold"),
              plot.tag = element_text(face="italic"))
      ggplotly(p_datald)
      p_datald
    # Ahora veamos con que media y varianza contamos
      print(paste0("La media es: ", mean(ts1_ld)))
      print(paste0("La varianza es: ", var(ts1_ld)))
    # Graficamos la ACF y PACF de los datos dados en la figura 6
      acf_ld <- acf_pacf(data=acf(ts1_ld,lag.max=100,plot= F), funcion="ACF",color_b = '#386890')
      pacf_ld <- acf_pacf(data=acf(ts1_ld,lag.max=100,type = "partial", plot= F),funcion= "PACF",color_b = '#386890')
      plot_grid(acf_ld, pacf_ld, ncol = 2, nrow = 1) + 
        labs(title="ACF y PACF \n Estabilización de la varianza y eliminación de tendencia", 
             subtitle= "Turismo Internacional",
             caption = 'Fuente: Elaboración propia con base en los datos de DATATUR',
             tag = 'Figura 9') + theme_ipsum()+ 
        theme(plot.title = element_text(hjust = 0.5,size=24, face="bold.italic"),
              plot.subtitle = element_text(hjust = 0.5,size=19, face="bold.italic"),
              plot.tag = element_text(face="italic",size = 16), plot.caption = element_text(size = 12))
    
      
      
    # Veamos las tranformaciones que tuvieron nuestros datos
      data1 <- ggplot(ts_df, aes(x=Fecha)) + geom_line(aes(y=Turistas),color= '#386890', size = 1.6,alpha = .5) + 
        theme_ipsum() + labs(x = '\n Fecha') + theme(axis.title.x = element_text(hjust = 0.5, size=14, face="bold"),
                                                     axis.title.y = element_text(hjust = 0.5, size=14, face="bold"))
      data2 <-ggplot(tsl_df, aes(x=Fecha)) + geom_line(aes(y=Turistas),color= '#386890', size = 1.6,alpha = .5) + 
        theme_ipsum() + labs(x = '\n Fecha') + theme(axis.title.x = element_text(hjust = 0.5, size=14, face="bold"),
                                                     axis.title.y = element_text(hjust = 0.5, size=14, face="bold"))
      data3 <-ggplot(tsld_df, aes(x=Fecha)) + geom_line(aes(y=Turistas),color= '#386890', size = 1.6,alpha = .5) + 
        theme_ipsum() + labs(x = '\n Fecha') + theme(axis.title.x = element_text(hjust = 0.5, size=14, face="bold"),
                                                     axis.title.y = element_text(hjust = 0.5, size=14, face="bold"))
      union<- plot_grid(data1, data2, ncol = 2, nrow = 1) 
      plot_grid(union, data3,ncol = 1, nrow = 2) + 
        labs(title="Tranformación de los datos",
             subtitle= "Turismo Internacional",
             caption = 'Fuente: Elaboración propia con base en los datos de DATATUR',
             tag = 'Figura 10') + theme_ipsum()+ 
        theme(plot.title = element_text(hjust = 0.5,size=22, face="bold.italic"),
              plot.subtitle = element_text(hjust = 0.5,size=17, face="bold.italic"),
              plot.tag = element_text(face="italic",size = 15), plot.caption = element_text(size = 12))
      
      
      
    # Empezamos con las propuestas de modelos
    # SARIMA(20,1,0)(6,0,0)
      SARIMA_20_1_0_6_0_0 <- arima(log(ts1), order = c(20, 1, 0), seasonal = list(order = c(1,0,0), period = 6))
      SARIMA_20_1_0_6_0_0
      coeftest(SARIMA_20_1_0_6_0_0)
    # ARIMA(13,1,0)
      ARIMA_13_1_0 <- arima(log(ts1), order = c(13, 1, 0))
      ARIMA_13_1_0
      coeftest(ARIMA_13_1_0)
    # SARIMA(13,1,0)(12,0,0)
      SARIMA_13_1_0_12_0_0 <- arima(log(ts1), order = c(13, 1, 0), seasonal = list(order = c(1,0,0), period = 12))
      SARIMA_13_1_0_12_0_0
      coeftest(SARIMA_13_1_0_12_0_0)
      
      
      
    # Comparación
      AIC(SARIMA_20_1_0_6_0_0) 
      AIC(SARIMA_13_1_0_12_0_0)# Más grande
      AIC(ARIMA_13_1_0) 
      
      BIC(SARIMA_20_1_0_6_0_0)
      BIC(SARIMA_13_1_0_12_0_0)# Más grande
      BIC(ARIMA_13_1_0) 
      
    
      
    # Validación de supuestos
      resid<-SARIMA_13_1_0_12_0_0$residuals ## Obtenemos los residuales
      resid
    # Reciduales a dataframe 
      resid_df<- data.frame(Fecha = as.Date(yearmon(time(resid))), Residuales = resid)
      resid_df
    # Histograma
      histo_resid <- ggplot(resid_df,aes(x = Residuales))+ geom_histogram(aes(y =..density..),
                                                                          bins = 25, fill='#386890', alpha=.5)+
        ylab("Densidad") + geom_density(color = '#386890',size= 1, alpha = .3) +theme_ipsum()+ 
        labs(title=" Histograma de Residuales",
             caption = 'Fuente: Elaboración propia con base en los datos de DATATUR', tag = 'Figura 11') +
        theme(plot.title = element_text(hjust = 0.5,size=22, face="bold.italic"),
              plot.subtitle = element_text(hjust = 0.5,size=17, face="bold.italic"),
              axis.title.x = element_text(hjust = 0.5, size=14, face="bold"),
              axis.title.y = element_text(hjust = 0.5,size=14, face="bold"),
              legend.title = element_text(hjust = 0.5, size=10, face="bold"), 
              plot.tag = element_text(face="italic")) 
      
      ggplotly(histo_resid)
      #histo_resid
    # Veamos como es el movimiento de los datos así como sus ACF y PACF  
    # Gráfica de residuales
      p_data_resid <- ggplot(resid_df, aes(x=Fecha)) + geom_line(aes(y=Residuales),color= '#386890', size = 1.6, 
                                                        alpha = .5) + theme_ipsum() + 
        labs(title="Residuales", caption = 'Fuente: Elaboración propia con base en los datos de DATATUR',
             tag = 'Figura 12',x = '\n Fecha') +
        theme(plot.title = element_text(hjust = 0.5,size=22, face="bold.italic"),
              plot.subtitle = element_text(hjust = 0.5,size=17, face="bold.italic"),
              axis.title.x = element_text(hjust = 0.5, size=14, face="bold"),
              axis.title.y = element_text(hjust = 0.5, size=14, face="bold"),
              plot.tag = element_text(face="italic"))
      ggplotly(p_data_resid)
      p_data_resid  
    # Graficamos la ACF y PACF de los datos dados en la figura 12
      acf_resid <- acf_pacf(data=acf(resid,lag.max=100,plot= F), funcion="ACF",color_b = '#386890')
      pacf_resid <- acf_pacf(data=acf(resid,lag.max=100,type = "partial", plot= F),funcion= "PACF",
                             color_b = '#386890')
      plot_grid(acf_resid, pacf_resid, ncol = 2, nrow = 1) + 
        labs(title="ACF y PACF ", subtitle= "Residuales",
             caption = 'Fuente: Elaboración propia con base en los datos de DATATUR',
             tag = 'Figura 13') + theme_ipsum()+ 
        theme(plot.title = element_text(hjust = 0.5,size=24, face="bold.italic"),
              plot.subtitle = element_text(hjust = 0.5,size=19, face="bold.italic"),
              plot.tag = element_text(face="italic",size = 16), plot.caption = element_text(size = 12))
    
    # Test
    # ADF Test
      adf_pv_resid<-NULL
      for(i in 1:100){adf_pv_resid[i]<-adf.test(resid, k=i)$p.value}
      adf_pv_resid #P-value menor a 0.5, por tanto rechazamos la hipotesis nula, por tanto es estacionaria.
      
    # Ljnug-Box Test
      lb_pv_resid<-NULL
      for(i in 1:100){lb_pv_resid[i]<-as.numeric(Box.test(resid, type = 'Ljung-Box', lag = i)$p.value)}
      lb_pv_resid #P-value mayor a 0.5, por tanto no rechazamos la hipotesis nula, por tanto es ruido blanco.
      
      
    # Hetroscedasticidad
      resid2 <- resid^2
    # Residuales al cuadrado  dataframe 
      resid2_df<- data.frame(Fecha = as.Date(yearmon(time(resid2))), Residuales2 = resid2)
      resid2_df
    # Histograma
      histo_resid2 <- ggplot(resid2_df,aes(x = Residuales2))+ geom_histogram(aes(y =..density..),
                                                                          bins = 25, fill='#386890', alpha=.5)+
        ylab("Densidad") + geom_density(color = '#386890',size= 1, alpha = .3) +theme_ipsum()+ 
        labs(title=" Histograma Residuales al cuadrado", x = 'Residuales al cuadrado',
             caption = 'Fuente: Elaboración propia con base en los datos de DATATUR', tag = 'Figura 14') +
        theme(plot.title = element_text(hjust = 0.5,size=22, face="bold.italic"),
              plot.subtitle = element_text(hjust = 0.5,size=17, face="bold.italic"),
              axis.title.x = element_text(hjust = 0.5, size=14, face="bold"),
              axis.title.y = element_text(hjust = 0.5,size=14, face="bold"),
              legend.title = element_text(hjust = 0.5, size=10, face="bold"), 
              plot.tag = element_text(face="italic")) 
      ggplotly(histo_resid2)
      #histo_resid2
    # Gráfica de residuales al cuadrado
      p_data_resid2 <- ggplot(resid2_df, aes(x=Fecha)) + geom_line(aes(y=Residuales2),color= '#386890', size = 1.6, 
                                                                 alpha = .5) + theme_ipsum() + 
        labs(title="Residuales al cuadrado", caption = 'Fuente: Elaboración propia con base en los datos de DATATUR',
             x = '\n Fecha', y = 'Residuales al cuadrado') +
        theme(plot.title = element_text(hjust = 0.5,size=22, face="bold.italic"),
              plot.subtitle = element_text(hjust = 0.5,size=17, face="bold.italic"),
              axis.title.x = element_text(hjust = 0.5, size=14, face="bold"),
              axis.title.y = element_text(hjust = 0.5, size=14, face="bold"),
              plot.tag = element_text(face="italic"))
      ggplotly(p_data_resid2)
      #p_data_resid2  
      # Graficamos la ACF y PACF de los datos dados en la figura 17
      acf_resid2 <- acf_pacf(data=acf(resid2,lag.max=100,plot= F), funcion="ACF",color_b = '#386890')
      pacf_resid2 <- acf_pacf(data=acf(resid2,lag.max=100,type = "partial", plot= F),funcion= "PACF",
                             color_b = '#386890')
      plot_grid(acf_resid2, pacf_resid2, ncol = 2, nrow = 1) + 
        labs(title="ACF y PACF ", subtitle= "Hetroscedastidad",
             caption = 'Fuente: Elaboración propia con base en los datos de DATATUR') + theme_ipsum()+ 
        theme(plot.title = element_text(hjust = 0.5,size=24, face="bold.italic"),
              plot.subtitle = element_text(hjust = 0.5,size=19, face="bold.italic"),
              plot.tag = element_text(face="italic",size = 16), plot.caption = element_text(size = 12))
      
      lb_pv_resid2<-NULL
      for(i in 1:100){lb_pv_resid2[i]<-as.numeric(Box.test(resid2, type = 'Ljung-Box', lag = i)$p.value)}
      lb_pv_resid2
      
      ArchTest(resid2)# No necesita de Arch ni Garch

      
    # Prediccón e intervalo de confianza
    # Veremos si nuestro analisis fue bueno, tomando un comparativo entre nuestra predicción al siguiente año (2019) 
    #  y los datos reales, los cuales no fueron tomados de inicio.
      data_19 <- ts(turismo$Turismo[469:480], start = c(2019, 1), frequency = 12)
      data_19
      predic_comp <- predict(SARIMA_13_1_0_12_0_0, n.ahead = 12) # Ya que trabajamos en meses
      predic_comp
      # Comparación dataframe 
      predic_comp_df<- data.frame(Fecha = as.Date(yearmon(time(predic_comp$pred))), Prediccion = exp(predic_comp$pred), 
                                  Real = data_19)
      predic_comp_df
      # Gráfica de la serie de tiempo predicha vs la real - 2019
      p_data_comp <- ggplot(predic_comp_df,aes(x=Fecha)) + 
        geom_line(aes(y=Prediccion,color= 'Predicción'), size = 1.6, alpha = .5) + theme_ipsum() + 
        geom_line(aes(y=Real, color = 'Real'),  alpha = .5,size = 1.6) + 
        labs(title="Turismo Internacional / Miles \n Comparación", y = 'Turistas',
             subtitle =  "2019", caption = 'Fuente: Elaboración propia con base en los datos de DATATUR',
             tag = 'Figura 15',x = '\n Fecha')+ scale_color_manual('' , values = c("Predicción" = '#386890',
                                                                                              "Real" = 'black'))+
        theme(plot.title = element_text(hjust = 0.5,size=22, face="bold.italic"),
              plot.subtitle = element_text(hjust = 0.5,size=17, face="bold.italic"),
              axis.title.x = element_text(hjust = 0.5, size=14, face="bold"),
              axis.title.y = element_text(hjust = 0.5, size=14, face="bold"),
              plot.tag = element_text(face="italic"))
      ggplotly(p_data_comp)
      #p_data_comp
    # Obtendremos el MAPE de nuestros datos
      mean(abs(predic_comp_df$Prediccion-predic_comp_df$Real)/predic_comp_df$Real)
       
      
    # Dado lo anterior veamos el intervalo de confianza a una predicción de dos años 2019-2020 
      predic <-predict(SARIMA_13_1_0_12_0_0, n.ahead = 24) # Ya que trabajamos en meses
      predic
    # Predicción a dataframe 
      predic_df<- data.frame(Fecha = as.Date(yearmon(time(predic$pred))), Turistas = exp(predic$pred))
      predic_df
    # Limites 
      U <- exp(predic$pred + predic$se) 
      L <- exp(predic$pred - predic$se)
      limites<- data.frame(Fecha = as.Date(yearmon(time(U))), li = L, ls = U)
      predic_df1 <- bind_cols(predic_df,limites[-1])
    # Gráfica de la serie de tiempo original 2015-2018 junto con la predicción 2019-2020
      p_data_predic <- ggplot(bind_rows(ts_df,predic_df1) ,aes(x=Fecha)) + 
        geom_line(aes(y=Turistas),color= '#386890', size = 1.6, alpha = .5) + 
        geom_ribbon(aes(ymin = li,ymax = ls), fill = "black", alpha=.2) +theme_ipsum() + 
        labs(title="Turismo Internacional / Miles \n Predicción", 
             subtitle =  "2015 - 2020", caption = 'Fuente: Elaboración propia con base en los datos de DATATUR',
             tag = 'Figura 16',x = '\n Fecha')+  scale_x_date(limits = c(as.Date("2015-01-01"), as.Date("2021-01-01")))+
      theme(plot.title = element_text(hjust = 0.5,size=22, face="bold.italic"),
              plot.subtitle = element_text(hjust = 0.5,size=17, face="bold.italic"),
              axis.title.x = element_text(hjust = 0.5, size=14, face="bold"),
              axis.title.y = element_text(hjust = 0.5, size=14, face="bold"),
              plot.tag = element_text(face="italic"))
      ggplotly(p_data_predic)
      #p_data_predic
      
      
      
    # Seasonal
      estac <- decompose(ts1_l)$seasonal
    # Seasonal a dataframe 
      s_df<- data.frame(Fecha = as.Date(yearmon(time(estac))), seasonal = estac)
      s_df
    # Gráfica de Seasonal
      p_data_s <- ggplot(s_df, aes(x=Fecha)) + geom_line(aes(y=seasonal),color= '#386890', size = 1.2, 
                                                                   alpha = .6) + theme_ipsum() + 
        labs(title="Turismo Internacional - Estacionariedad", subtitle = "1980-2019",
             caption = 'Fuente: Elaboración propia con base en los datos de DATATUR',
             tag = 'Figura 19',x = '\n Fecha', y = 'Visitantes Internacionales') +
        theme(plot.title = element_text(hjust = 0.5,size=22, face="bold.italic"),
              plot.subtitle = element_text(hjust = 0.5,size=17, face="bold.italic"),
              axis.title.x = element_text(hjust = 0.5, size=14, face="bold"),
              axis.title.y = element_text(hjust = 0.5, size=14, face="bold"),
              plot.tag = element_text(face="italic"))
      ggplotly(p_data_s)
      #p_data_s  
      # Graficamos la ACF y PACF de los datos dados en la figura 17
      acf_s <- acf_pacf(data=acf(estac,lag.max=100,plot= F), funcion="ACF",color_b = '#386890')
      pacf_s <- acf_pacf(data=acf(estac,lag.max=100,type = "partial", plot= F),funcion= "PACF",
                              color_b = '#386890')
      plot_grid(acf_s, pacf_s, ncol = 2, nrow = 1) + 
        labs(title="ACF y PACF ", subtitle= "Estacionariedad",
             caption = 'Fuente: Elaboración propia con base en los datos de DATATUR')+ theme_ipsum()+ 
        theme(plot.title = element_text(hjust = 0.5,size=24, face="bold.italic"),
              plot.subtitle = element_text(hjust = 0.5,size=19, face="bold.italic"),
              plot.tag = element_text(face="italic",size = 16), plot.caption = element_text(size = 12))
      

      

      
    
      
      
      
      
      
      
      
      
      
      
      

      
      
      
      