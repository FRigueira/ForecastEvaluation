library(gridExtra)
library(tsfeatures)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Monthly Exploration Analisys
# --- --- --- --- --- --- --- --- --- --- --- --- --- --
seriesPeriodsPlots<-function(data,dataType){
  Plot<-NULL
  if (is.null(data))
    return(Plot)
  
  # Prepare data
  colnames(data)<-c('date','value')
  data<-tibble::as_tibble(data)
  data<-tibbletime::as_tbl_time(data, date)
  
  # Build series
  monthlySeries<-data %>% tibbletime::collapse_by("monthly") %>%  dplyr::group_by(date) %>%  dplyr::summarise_if(is.numeric, c(sum,mean,min,max))
  quarterlySeries<-data %>% tibbletime::collapse_by("quarter") %>%  dplyr::group_by(date) %>%  dplyr::summarise_if(is.numeric, c(sum,mean,min,max))
  semesterSeries<-data %>% tibbletime::collapse_by("2 quarter") %>%  dplyr::group_by(date) %>%  dplyr::summarise_if(is.numeric, c(sum,mean,min,max))
  yearlySeries<-data %>% tibbletime::collapse_by("yearly") %>%  dplyr::group_by(date) %>%  dplyr::summarise_if(is.numeric, c(sum,mean,min,max))
  colnames(monthlySeries)<-c('date','sum','mean','min','max')
  colnames(quarterlySeries)<-c('date','sum','mean','min','max')
  colnames(semesterSeries)<-c('date','sum','mean','min','max')
  colnames(yearlySeries)<-c('date','sum','mean','min','max')
  
  # Theme adjust for ths particular Plots
  tm_old <- theme_get()
  tm_new <- tm_old %+replace% theme(rect = element_rect(fill = "#ffffff"))
  tm_new <- tm_new %+replace% theme(panel.background = element_rect(fill = "#ffffff"))
  tm_new <- tm_new %+replace% theme(plot.background = element_rect(fill = "#ffffff"))
  tm_new <- tm_new %+replace% theme(panel.grid.major = element_line(colour = "#6C6C69"))
  theme_set(tm_new)  
  
  # Build Plot
  montlhyPlot<- ggplot(monthlySeries)+
    geom_line(aes(date,min),colour='blue', size=0.8)+
    geom_line(aes(date,mean),colour='green', size=0.8)+
    geom_line(aes(date,max),colour='red', size=0.8) +
    geom_line(aes(date,sum),colour='black', size=1)+
    scale_x_date(date_labels = "%y-%m",date_breaks = "6 month")+
    #scale_y_continuous(name = "Accumulated",sec_axis(~.,name = "Detail"))+
    labs(title = "Monthly",x = "Date",y = "Value")
  
  quarterlyPlot<- ggplot(quarterlySeries)+
    geom_line(aes(date,min),colour='blue', size=0.8)+
    geom_line(aes(date,sum),colour='black', size=1)+
    geom_line(aes(date,mean),colour='green', size=0.8)+
    geom_line(aes(date,max),colour='red', size=0.8)  +
    scale_x_date(date_labels = "%y-%m",date_breaks = "6 month")+ 
    labs(title = "Quarterly",x = "Date",y = "Value")
  
  semesterPlot<- ggplot(semesterSeries)+
    geom_line(aes(date,min),colour='blue', size=0.8)+
    geom_line(aes(date,sum),colour='black', size=1)+
    geom_line(aes(date,mean),colour='green', size=0.8)+
    geom_line(aes(date,max),colour='red', size=0.8)  +
    scale_x_date(date_labels = "%y-%m",date_breaks = "6 month") +
    labs(title = "Half Year",x = "Date",y = "Value")
  
  yearlyPlot<- ggplot(yearlySeries)+
    geom_line(aes(date,min),colour='blue', size=0.8)+
    geom_line(aes(date,sum),colour='black', size=1)+
    geom_line(aes(date,mean),colour='green', size=0.8)+
    geom_line(aes(date,max),colour='red', size=0.8)  +
    scale_x_date(date_labels = "%y",date_breaks = "year")+
    labs(title = "Yearly",x = "Date",y = "Value")+ theme()
  
  # Group Plots  
  Plot<-gridExtra::grid.arrange(montlhyPlot, quarterlyPlot,semesterPlot,yearlyPlot,  ncol = 2)
  
  theme_set(tm_old)
  
  return(Plot)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Load file data 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --
evolutionFacetPlots<-function(data,dataType){
  Plot<-NULL
  if (is.null(data))
    return(Plot)
  
  # Prepare data
  colnames(data)<-c('date','value')
  data<-tibble::as_tibble(data)
  data<-tibbletime::as_tbl_time(data, date)
  
  # Build series
  monthlySeries<-data %>% tibbletime::collapse_by("monthly") %>%  dplyr::group_by(date) %>%  dplyr::summarise_if(is.numeric, c(sum,mean,min,max))
  quarterlySeries<-data %>% tibbletime::collapse_by("quarter") %>%  dplyr::group_by(date) %>%  dplyr::summarise_if(is.numeric, c(sum,mean,min,max))
  semesterSeries<-data %>% tibbletime::collapse_by("2 quarter") %>%  dplyr::group_by(date) %>%  dplyr::summarise_if(is.numeric, c(sum,mean,min,max))
  yearlySeries<-data %>% tibbletime::collapse_by("yearly") %>%  dplyr::group_by(date) %>%  dplyr::summarise_if(is.numeric, c(sum,mean,min,max))
  colnames(monthlySeries)<-c('date','sum','mean','min','max')
  colnames(quarterlySeries)<-c('date','sum','mean','min','max')
  colnames(semesterSeries)<-c('date','sum','mean','min','max')
  colnames(yearlySeries)<-c('date','sum','mean','min','max')
  Dados1<-dplyr::mutate(data,m = lubridate::month(data[['date']]),y = lubridate::year(data[['date']]))
  medAno<-Dados1[c('y','value')] %>% dplyr::group_by(y) %>%  dplyr::summarise_if(is.numeric, c(mean))
  medMes<-Dados1[c('m','value')] %>% dplyr::group_by(m) %>%  dplyr::summarise_if(is.numeric, c(mean))
  Dados1<-dplyr::full_join(Dados1, medMes, by = "m")
  Dados1<-dplyr::full_join(Dados1, medAno, by = "y")
  colnames(Dados1)<-c('date','value','m','y','medMes','medAno')
  
  # Years
  years<-Dados1[c('y')] %>% dplyr::group_by(y)%>%  dplyr::summarise_if(is.numeric, c(mean))
  Dados1 %>% dplyr::filter(Dados1$y %in% years) 
  
  # Theme adjust for ths particular Plots
  tm_old <- theme_get()
  tm_new <- tm_old %+replace% theme(rect = element_rect(fill = "#b3edff"))
  tm_new <- tm_new %+replace% theme(panel.background = element_rect(fill = "#ffffff"))
  tm_new <- tm_new %+replace% theme(plot.background = element_rect(fill = "#ffffff"))
  tm_new <- tm_new %+replace% theme(panel.grid.major = element_line(colour = "#6C6C69"))
  theme_set(tm_new)    
  
  # Build Plot
  yearlyFacetPlot<-ggplotly(
    yearlyFacetPlot<-  ggplot(Dados1,aes(m,value))+ 
      geom_line(colouplotsr='black', size=1) + 
      geom_line(aes(m,medAno),colour='red', size=2)+ 
      scale_x_continuous(breaks = c(3,6,9))+
      facet_grid(~y)+
      labs(title = "Yearly Evolution",x = "Date",y = "Value"))
  monthlyFacetPlot<-ggplotly(
    ggplot(Dados1,aes(y,value))+ 
      geom_line(colour='black', size=1)  + 
      geom_line(aes(y,medMes),colour='red', size=2)+ 
      scale_x_continuous()+
      facet_grid(~m)+
      labs(title = "Monthly Evolution",x = "Date",y = "Value"))
  
  # Group Plots  
  Plot<-subplot(yearlyFacetPlot,monthlyFacetPlot, nrows=2)
  
  theme_set(tm_old)
  
  return(Plot)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Monthly Exploration Analisys
# --- --- --- --- --- --- --- --- --- --- --- --- --- --
MonthlySeriesPeriodsPlots<-function(data,dataType){
  Plot<-NULL
  if (is.null(data) | dataType=="week" | dataType=="year")
    return(Plot)
  
  
  minDate<-min(data[,1])
  maxDate<-max(data[,1])
  minYear<-lubridate::year(minDate)
  maxYear<-lubridate::year(maxDate)
  
  if (lubridate::month(minDate)>1) 
  {
   minYear<-(lubridate::year(minDate)+1)
   minDate<-as.Date(with(df, paste(minYear, 1, 1,sep="-")), "%Y-%m-%d")
   data<-data %>% dplyr::filter(date>=minDate)
  }
  if (lubridate::month(maxDate)<12) 
  {
    maxYear<-(lubridate::year(maxDate)-1)
    maxDate<-as.Date(with(df, paste(maxYear, 12, 31,sep="-")), "%Y-%m-%d")
    data<-data %>% dplyr::filter(date<=maxDate)
  }
  
  if (nrow(data)==0)
    return(Plot)
  
  # Prepare data
  colnames(data)<-c('date','value')
  data<-tibble::as_tibble(data)
  data<-tibbletime::as_tbl_time(data, date)
  
  # Build series
  monthlySeries<-data %>% tibbletime::collapse_by("monthly") %>%  dplyr::group_by(date) %>%  dplyr::summarise_if(is.numeric, c(sum,mean,min,max))
  colnames(monthlySeries)<-c('date','sum','mean','min','max')
  
  # Theme adjust for ths particular Plots
  tm_old <- theme_get()
  tm_new <- tm_old %+replace% theme(rect = element_rect(fill = "#b3edff"))
  tm_new <- tm_new %+replace% theme(panel.background = element_rect(fill = "#ffffff"))
  tm_new <- tm_new %+replace% theme(plot.background = element_rect(fill = "#ffffff"))
  tm_new <- tm_new %+replace% theme(panel.grid.major = element_line(colour = "#6C6C69"))
  theme_set(tm_new)  
  
  # Build Plot
  montlhyPlot<- ggplot(monthlySeries)+
    geom_line(aes(date,min),colour='blue', size=0.8)+
    geom_line(aes(date,mean),colour='green', size=0.8)+
    geom_line(aes(date,max),colour='red', size=0.8) +
    geom_line(aes(date,sum),colour='black', size=1)+
    scale_x_date(date_labels = "%y-%m",date_breaks = "6 month")+
    #scale_y_continuous(name = "Accumulated",sec_axis(~.,name = "Detail"))+
    labs(title = "Monthly",x = "Date",y = "Value")
  
  #Monthly Seasonality
  df<-as.data.frame(monthlySeries[,c(1,3)])
  minDate<-min(df[,1])
  minYear<-lubridate::year(minDate)
  minMonth<-lubridate::month(minDate)
  dados_df<-data.frame(df[,1:2])
  dados_ts<-ts(dados_df[,2],start = c(minYear,minMonth),frequency = 12)
  seasonalityPlot<- ggseasonplot(dados_ts) + geom_line(size = 1) + ggtitle("Monthly Seasonality")
  
  # Group Plots  
  Plot<-gridExtra::grid.arrange(montlhyPlot, seasonalityPlot,  ncol = 2)
  
  theme_set(tm_old)
  
  return(Plot)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Quarterly Exploration Analisys
# --- --- --- --- --- --- --- --- --- --- --- --- --- --
QuarterlySeriesPeriodsPlots<-function(data,dataType){
  Plot<-NULL
  if (is.null(data) | nrow(data)==0 | dataType=="week" | dataType=="year")
    return(Plot)

  minDate<-min(data[,1])
  maxDate<-max(data[,1])
  minYear<-lubridate::year(minDate)
  maxYear<-lubridate::year(maxDate)
  
  if (lubridate::month(minDate)>1) 
  {
    minYear<-(lubridate::year(minDate)+1)
    minDate<-as.Date(with(df, paste(minYear, 1, 1,sep="-")), "%Y-%m-%d")
    data<-data %>% dplyr::filter(date>=minDate)
  }
  if (lubridate::month(maxDate)<12) 
  {
    maxYear<-(lubridate::year(maxDate)-1)
    maxDate<-as.Date(with(df, paste(maxYear, 12, 31,sep="-")), "%Y-%m-%d")
    data<-data %>% dplyr::filter(date<=maxDate)
  }
  
  if (nrow(data)==0)
    return(Plot)
    
  # Prepare data
  colnames(data)<-c('date','value')
  data<-tibble::as_tibble(data)
  data<-tibbletime::as_tbl_time(data, date)
  
  # Build series
  quarterlySeries<-data %>% tibbletime::collapse_by("quarter") %>%  dplyr::group_by(date) %>%  dplyr::summarise_if(is.numeric, c(sum,mean,min,max))
  colnames(quarterlySeries)<-c('date','sum','mean','min','max')
  
  # Theme adjust for ths particular Plots
  tm_old <- theme_get()
  tm_new <- tm_old %+replace% theme(rect = element_rect(fill = "#b3edff"))
  tm_new <- tm_new %+replace% theme(panel.background = element_rect(fill = "#ffffff"))
  tm_new <- tm_new %+replace% theme(plot.background = element_rect(fill = "#ffffff"))
  tm_new <- tm_new %+replace% theme(panel.grid.major = element_line(colour = "#6C6C69"))
  theme_set(tm_new)  
  
  # Build Plot
  quarterlyPlot<- ggplot(quarterlySeries)+
    geom_line(aes(date,min),colour='blue', size=0.8)+
    geom_line(aes(date,mean),colour='green', size=0.8)+
    geom_line(aes(date,max),colour='red', size=0.8) +
    geom_line(aes(date,sum),colour='black', size=1)+
    scale_x_date(date_labels = "%y-%m",date_breaks = "6 month")+
    #scale_y_continuous(name = "Accumulated",sec_axis(~.,name = "Detail"))+
    labs(title = "Quarterly",x = "Date",y = "Value")
  
  #Quarterly Seasonality
  df<-as.data.frame(quarterlySeries[,c(1,2)])
  minDate<-min(df[,1])
  minYear<-lubridate::year(minDate)
  minMonth<-lubridate::month(minDate)
  minQuarter<-trunc(minMonth/3)
  dados_df<-data.frame(df[,1:2])
  dados_ts<-ts(dados_df[,2],start = c(minYear,minQuarter),frequency = 4)
  seasonalityPlot<- ggseasonplot(dados_ts) + geom_line(size = 1) + ggtitle("Quarterly Seasonality")
  
  # Group Plots  
  Plot<-gridExtra::grid.arrange(quarterlyPlot, seasonalityPlot,  ncol = 2)
  
  theme_set(tm_old)
  
  return(Plot)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Half-Year Exploration Analisys
# --- --- --- --- --- --- --- --- --- --- --- --- --- --
HalfYearSeriesPeriodsPlots<-function(data,dataType){
  Plot<-NULL

  if (is.null(data) | nrow(data)==0)
    return(Plot)
  
  minDate<-min(data[,1])
  maxDate<-max(data[,1])
  minYear<-lubridate::year(minDate)
  maxYear<-lubridate::year(maxDate)
  
  if (lubridate::month(minDate)>1) 
  {
    minYear<-(lubridate::year(minDate)+1)
    minDate<-as.Date(with(df, paste(minYear, 1, 1,sep="-")), "%Y-%m-%d")
    data<-data %>% dplyr::filter(date>=minDate)
  }
  if (lubridate::month(maxDate)<12) 
  {
    maxYear<-(lubridate::year(maxDate)-1)
    maxDate<-as.Date(with(df, paste(maxYear, 12, 31,sep="-")), "%Y-%m-%d")
    data<-data %>% dplyr::filter(date<=maxDate)
  }
  
  if (nrow(data)==0)
    return(Plot)
  
  # Prepare data
  colnames(data)<-c('date','value')
  data<-tibble::as_tibble(data)
  data<-tibbletime::as_tbl_time(data, date)
  
  # Build series
  semesterSeries<-data %>% tibbletime::collapse_by("2 quarter") %>%  dplyr::group_by(date) %>%  dplyr::summarise_if(is.numeric, c(sum,mean,min,max))
  colnames(semesterSeries)<-c('date','sum','mean','min','max')
  
  # Theme adjust for ths particular Plots
  tm_old <- theme_get()
  tm_new <- tm_old %+replace% theme(rect = element_rect(fill = "#ffffff"))
  tm_new <- tm_new %+replace% theme(panel.background = element_rect(fill = "#ffffff"))
  tm_new <- tm_new %+replace% theme(plot.background = element_rect(fill = "#ffffff"))
  tm_new <- tm_new %+replace% theme(panel.grid.major = element_line(colour = "#6C6C69"))
  theme_set(tm_new)  
  
  # Build Plot
  HalfYearPlot<- ggplot(semesterSeries)+
    geom_line(aes(date,min),colour='blue', size=0.8)+
    geom_line(aes(date,mean),colour='green', size=0.8)+
    geom_line(aes(date,max),colour='red', size=0.8) +
    geom_line(aes(date,sum),colour='black', size=1)+
    scale_x_date(date_labels = "%y-%m",date_breaks = "6 month")+
    #scale_y_continuous(name = "Accumulated",sec_axis(~.,name = "Detail"))+
    labs(title = "Half-Year",x = "Date",y = "Value")
  
  #Half-Year Seasonality
  df<-as.data.frame(semesterSeries[,c(1,2)])
  minDate<-min(df[,1])
  minYear<-lubridate::year(minDate)
  minMonth<-lubridate::month(minDate)
  minSemester<-trunc(minMonth/6)
  dados_df<-data.frame(df[,1:2])
  dados_ts<-ts(dados_df[,2],start = c(minYear,minSemester),frequency = 2)
  seasonalityPlot<- ggseasonplot(dados_ts) + geom_line(size = 1) + ggtitle("Half-Year Seasonality")
  
  # Group Plots  
  Plot<-gridExtra::grid.arrange(HalfYearPlot, seasonalityPlot,  ncol = 2)
  
  theme_set(tm_old)
  
  return(Plot)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Yearly Exploration Analisys
# --- --- --- --- --- --- --- --- --- --- --- --- --- --
YearSeriesPeriodsPlots<-function(data,dataType){
  Plot<-NULL
  if (is.null(data) | nrow(data)==0)
    return(Plot)

  minDate<-min(data[,1])
  maxDate<-max(data[,1])
  minYear<-lubridate::year(minDate)
  maxYear<-lubridate::year(maxDate)
  
  if (lubridate::month(minDate)>1) 
  {
    minYear<-(lubridate::year(minDate)+1)
    minDate<-as.Date(with(df, paste(minYear, 1, 1,sep="-")), "%Y-%m-%d")
    data<-data %>% dplyr::filter(date>=minDate)
  }
  if (lubridate::month(maxDate)<12) 
  {
    maxYear<-(lubridate::year(maxDate)-1)
    maxDate<-as.Date(with(df, paste(maxYear, 12, 31,sep="-")), "%Y-%m-%d")
    data<-data %>% dplyr::filter(date<=maxDate)
  }
  
  if (nrow(data)==0)
    return(Plot)

  # Prepare data
  colnames(data)<-c('date','value')
  data<-tibble::as_tibble(data)
  data<-tibbletime::as_tbl_time(data, date)
  
  # Build series
  yearlySeries<-data %>% tibbletime::collapse_by("yearly") %>%  dplyr::group_by(date) %>%  dplyr::summarise_if(is.numeric, c(sum,mean,min,max))
  colnames(yearlySeries)<-c('date','sum','mean','min','max')
  
  # Theme adjust for ths particular Plots
  tm_old <- theme_get()
  tm_new <- tm_old %+replace% theme(rect = element_rect(fill = "#FFFFFF"))
  tm_new <- tm_new %+replace% theme(panel.background = element_rect(fill = "#FFFFFF"))
  tm_new <- tm_new %+replace% theme(plot.background = element_rect(fill = "#FFFFFF"))
  tm_new <- tm_new %+replace% theme(panel.grid.major = element_line(colour = "#6C6C69"))
  theme_set(tm_new)  
  
  # Build Plot
  yearlyPlot<- ggplot(yearlySeries)+
    geom_line(aes(date,min),colour='blue', size=0.8)+
    geom_line(aes(date,sum),colour='black', size=1)+
    geom_line(aes(date,mean),colour='green', size=0.8)+
    geom_line(aes(date,max),colour='red', size=0.8)  +
    scale_x_date(date_labels = "%y",date_breaks = "year")+
    labs(title = "Yearly",x = "Date",y = "Value")
  
  # Group Plots  
  Plot<-yearlyPlot
  
  theme_set(tm_old)
  
  return(Plot)
}

individualBoxPlot<-function(df_in){
  df<-as.data.frame(df_in)
  df<-cbind(df_in,stringr::str_sub(df[['date']],1,4))
  colnames(df)=c('date','value','year')
  df[['value']] <- as.numeric(df[['value']])
  df[['year']] <- as.numeric(df[['year']])
  
  boxPlot<-plot_ly(df, x = ~year, y = ~value) %>%  add_boxplot()  #%>% layout(title = "Yearly Details")
  return (boxPlot)
}

individualSeriesPlot<-function(df_in){

    # Theme adjust for ths particular Plots
    tm_old <- theme_get()
    tm_new <- tm_old %+replace% theme(rect = element_rect(fill = "##FFFFFF"))
    tm_new <- tm_new %+replace% theme(panel.background = element_rect(fill = "#FFFFFF"))
    tm_new <- tm_new %+replace% theme(plot.background = element_rect(fill = "#FFFFFF"))
    tm_new <- tm_new %+replace% theme(panel.grid.major = element_line(colour = "#999E9D"))
    theme_set(tm_new)
    
    minDate<-min(df_in[,1])
    minYear<-lubridate::year(minDate)
    minMonth<-lubridate::month(minDate)
    dados_TS<-xts::xts(as.numeric(df_in[,2]),order.by = as.Date(df_in[,1]),frequency=12)
    colnames(dados_TS)<-c('value')
    seriesPlots<-dygraph(dados_TS) %>% dyRangeSelector
    theme_set(tm_old)
    return (seriesPlots)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Moving Average 
# movingAveragePlot(df,norder=c(1,3,6,12),center=TRUE)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --
movingAveragePlot<-function(df,norder=3,center=TRUE){
  Plot<-NULL
  if (!is.null(df))
  {
    try({
      minDate<-min(df[,1])
      minYear<-lubridate::year(minDate)
      minMonth<-lubridate::month(minDate)
      dados_df<-data.frame(df[,1:2])
      dados_ts<-ts(Dados[['value']],start = c(minYear,minMonth),frequency = 12)
      Plot<-plot_ly(dados_df, x = ~date, y = ~value, type='scatter', name='Serie', mode = 'lines')
      for (n in norder)
      {
        lineName<-paste0("MA-Cente:",center,"-Order:")
        ma<-dados_ts %>% forecast::ma(order=n, centre=center)
        Plot<-add_trace(Plot, y=ma, x=df[['date']], name=paste0(lineName,n), type = 'scatter', mode = 'lines', line = list(shape = 'linear', dash = 'dash'))
      }
    },silent=TRUE
    )
  }
  return (Plot)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Cassical Decompositon 
# decompositionPlots(df,"additive")
# decompositionPlots(df,"multiplicative")
# --- --- --- --- --- --- --- --- --- --- --- --- --- --
decompositionPlots<-function(df,decompositionType="additive"){
  subPlot<-NULL
  if (!is.null(df))
  {
    try({
      minDate<-min(df[,1])
      minYear<-lubridate::year(minDate)
      minMonth<-lubridate::month(minDate)
      dados_df<-data.frame(df[,1:2])
      dados_df[,2]<-as.numeric(dados_df[,2])
      dados_ts<-ts(dados_df[['value']],start = c(minYear,minMonth),frequency = 12)
      # Decomposition
      decompose<-dados_ts %>% stats::decompose(type=decompositionType)
      decomposeTrend<-decompose$trend
      decomposeSeasonal<-decompose$seasonal
      decomposeResidual<-decompose$random
      
      
      # Construcao dos graficos
      #seriesPlot<-plot_ly(dados_df, x = ~date, y = ~value, type='scatter', name='Serie', mode = 'lines', height = 300)
      trendPlot<-plot_ly(dados_df, x = ~date, y=decomposeTrend, name="Trend", type = 'scatter', mode = 'lines')
      seasonalPlot<-plot_ly(dados_df, x = ~date, y=decomposeSeasonal, name="Seasonal", type = 'scatter', mode = 'lines')
      residualPlot<-plot_ly(dados_df, x = ~date, y=decomposeResidual, name="Residual", type = 'scatter', mode = 'lines')
      histogramPlot<-plot_ly(x=decomposeResidual, name="Histogram", type = 'histogram')
      subPlot1<-subplot(trendPlot,seasonalPlot,nrows=2,shareX=TRUE, margin = c(0.1,0.1,0.1,0.1))
      subPlot2<-subplot(residualPlot,histogramPlot,nrows=2,margin = c(0.1,0.1,0.1,0.1))
      subPlot<-subplot(subPlot1,subPlot2,nrows=1,margin = c(0.1,0.1,0.1,0.1))                 
    },silent=TRUE
    )
  }
  return (subPlot)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --
# STL Decompositon 
# decomposeSTL<-dados %>%stl(t.window=13, s.window="periodic", robust=TRUE)
# decomposeMSTL<-dados %>%mstl()
# --- --- --- --- --- --- --- --- --- --- --- --- --- --
STLdecompositionPlots<-function(df,decompositionType="additive"){
  subPlot<-NULL
  if (!is.null(df))
  {
    try({
      minDate<-min(df[,1])
      minYear<-lubridate::year(minDate)
      minMonth<-lubridate::month(minDate)
      dados_df<-data.frame(df[,1:2])
      dados_df[,2]<-as.numeric(dados_df[,2])
      dados_ts<-ts(dados_df[['value']],start = c(minYear,minMonth),frequency = 12)
      # Decomposition
      decompose<-dados_ts %>% forecast::mstl()
      decomposeTrend<-decompose[,2]
      decomposeSeasonal<-decompose[,3]
      decomposeResidual<-decompose[,4]
      decomposeSeasonalTred<-decompose[,3]+decompose[,2]
      decomposeSeasonalResiduals<-decompose[,3]+decompose[,4]
      decomposeTrendResiduals<-decompose[,2]+decompose[,4]
      # Construcao dos graficos
      #seriesPlot<-plot_ly(dados_df, x = ~date, y = ~value, type='scatter', name='Serie', mode = 'lines', height = 300)
      # trendPlot<-plot_ly(dados_df, x = ~date, y=decomposeTrend, name="Trend", type = 'scatter', mode = 'lines')
      # seasonalPlot<-plot_ly(dados_df, x = ~date, y=decomposeSeasonal, name="Seasonal", type = 'scatter', mode = 'lines')
      # residualPlot<-plot_ly(dados_df, x = ~date, y=decomposeResidual, name="Residual", type = 'scatter', mode = 'lines')
      # sessonaltrendPlot<-plot_ly(dados_df, x = ~date, y=decomposeSeasonalTred, name="Seasonal+Trend", type = 'scatter', mode = 'lines')
      # seasonalResidualPlot<-plot_ly(dados_df, x = ~date, y=decomposeSeasonalResiduals, name="Seasonal+Residuals", type = 'scatter', mode = 'lines')
      # trendResidualPlot<-plot_ly(dados_df, x = ~date, y=decomposeTrendResiduals, name="Seasonal+Residuals", type = 'scatter', mode = 'lines')
      
      histogramPlot<-plot_ly(x=decomposeResidual, name="Histogram", type = 'histogram')
      # subPlot1<-subplot(trendPlot,seasonalPlot,nrows=2,shareX=TRUE, margin = c(0.1,0.1,0.1,0.1))
      # subPlot2<-subplot(residualPlot,histogramPlot,nrows=2,margin = c(0.1,0.1,0.1,0.1))
      # #subPlot3<-subplot(sessonaltrendPlot,seasonalResidualPlot,nrows=2,margin = c(0.1,0.1,0.1,0.1))
      # subPlot3<-subplot(subPlot1,subPlot2,nrows=1,margin = c(0.1,0.1,0.1,0.1))                 
      # subPlot<-subplot(subPlot3,sessonaltrendPlot,nrows=2,margin = c(0.1,0.1,0.1,0.1))                 
      
    },silent=TRUE
    )
  }
  # return (subPlot)
  return (histogramPlot)
}

STLdecompositionPlots2 <-
  function(df, decompositionType = "additive",seriesTypes,dateType) {
    Plot <- NULL
    if (!is.null(df))
    {
      try({
        .frequency<-12
        minDate <- min(df[, 1])
        maxDate <- max(df[, 1])
        minYear <- lubridate::year(minDate)
        maxYear <- lubridate::year(maxDate)
        minMonth <- lubridate::month(minDate)
        dados_df <- data.frame(df[, 1:2])
        dados_df[, 2] <- as.numeric(dados_df[, 2])

        if(dateType=="day"){
          minDate <- lubridate::ymd(minDate)
          minDate <- lubridate::decimal_date(minDate)
          #dados_ts <- ts(as.numeric(dados_df[['value']]),start = minDate,freq=365.25/7)
          dados_ts <- ts(as.numeric(dados_df[['value']]),start = minDate,freq=1)
          
        }
        else
        {
          dados_ts <-
            ts(dados_df[['value']],
               start = c(minYear, minMonth),
               frequency = .frequency) 
        }
        
        # Decomposition
        decPlot<-NULL
        decCols<-c(1)
        decNames<-c("Data")
        decCor<-c("blue")
        dec <- dados_ts %>% mstl()
        if ('Trend' %in% colnames(dec))
          {Trend<-dec[,"Trend"]}
        else
          {Trend<-dec[,"Data"]
          Trend[1:length(Trend)]<-0}
        
        if ('Seasonal12' %in% colnames(dec))
          {Seasonal12<-dec[,"Seasonal12"]}
        else
          {Seasonal12<-dec[,"Data"]
           Seasonal12[1:length(Seasonal12)]<-0}
        
        if ('Remainder' %in% colnames(dec))
          {Remainder<-dec[,"Remainder"]}
        else
          {Remainder<-dec[,"Data"]
          Remainder[1:length(Seasonal12)]<-0}
        
        #TrendResiduals<-dec[,"Trend"]+dec[,"Remainder"]
        TrendResiduals<-Trend+Remainder
        dec<-cbind(dec[,"Data"],Trend,Seasonal12,Remainder,TrendResiduals)
        colnames(dec)<-c("Data", "Trend", "Seasonal12", "Remainder","TrendResiduals")
        if ('Trend' %in% seriesTypes)
        {
          decCols<-append(decCols,2)
          decNames<-append(decNames,"Trend")
          decCor<-append(decCor,"cyan")
        }
        if ('Seasonal12' %in% seriesTypes)
        {
          decCols<-append(decCols,3)
          decNames<-append(decNames,"Seasonal12")
          decCor<-append(decCor,"green")
        }
        if ('Remainder' %in% seriesTypes)
        {
          decCols<-append(decCols,4)
          decNames<-append(decNames,"Remainder")
          decCor<-append(decCor,"black")
        }
        if ('TrendResiduals' %in% seriesTypes)
        {
          decCols<-append(decCols,5)
          decNames<-append(decNames,"TrendResiduals")
          decCor<-append(decCor,"red")
        }
        dec<-dec[,decCols]

        Plot <- dygraphs::dygraph(dec, main = "STL Decomposition") %>%
          dygraphs::dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))%>%
          dygraphs::dySeries("Data", stepPlot = FALSE, color = "black") %>%
          dygraphs::dyGroup(
            #c("Trend", "Seasonal12", "Remainder","TrendResiduals"),
            decNames,
            drawPoints = TRUE,
            color=decCor
            #color = c("blue", "green", "black","red")
          ) %>%
          dyRangeSelector()
        
        
      }, silent = TRUE)
    }
    
    return (Plot)
  }

library(urca)

# Identify first stationary diferencing lag 
getFirstStationarySerie<-function(dados_ts){

  if (is.null(dados_ts))
    {return (NULL)}
  
  lag<-0
  if (checkStationary(dados_ts))
    {return(lag)}
  
  for (lg in c(1,2,3,4,5,6,7,8,9,10,11,12))
    {
    serie<-diff(dados_ts,lag=lg)
    if (checkStationary(serie))
      {lag<-lg
      break
      }
  }
  return(lag)
}

getDiffStationarySerie<-function(dados_ts){
  
  if (is.null(dados_ts))
    {return (NULL)}
  
  seriesStationaryLags<-matrix(nrow = 13,ncol=2)
  seriesStationaryLags[,1]<-seq(from=0, to=12,by=1)
  seriesStationaryLags[,2]<-rep("NO", 13)
  colnames(seriesStationaryLags)<-c("Lag","Estationary")
  for (lg in seriesStationaryLags[,1])
  {
    pos<-as.numeric(lg)
    if (pos==0)
    {cs<-checkStationary(dados_ts)}
    else
    {cs<-checkStationary(diff(dados_ts,lag=pos))}
    if(cs){seriesStationaryLags[pos+1,2]<-"YES"}
  }
    
  return(seriesStationaryLags)
}

 
# Check for if TS is statioary
checkStationary<-function(serie){
  
  if (is.null(serie))
  {return (0)}
  
  kpss.res<-serie %>% urca::ur.kpss() %>% summary()
  if (kpss.res@teststat<=0.05)
  {return(1)} 
  else
  {return (0)}

}

stationaryPlots<-function(dados_ts){
  subPlot<-NULL
  dif<-getFirstStationarySerie(dados_ts)
  if (!is.null(dados_ts))
  {
    try({
      serie<-diff(dados_ts,differences =dif)
      plt1<-plot_ly(y=serie, name="Diferenced Serie", type = 'scatter',mode = 'lines')      
      plt2<-plot_ly(x=serie, name="Histogram", type = 'histogram')    
      subPlot<-subplot(plt1,plt2,nrows=1,shareX=TRUE, margin = c(0.1,0.1,0.1,0.1))
      
    },silent=TRUE
    )
  }
  return (subPlot)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Anomaly Diagnostic Plot ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --
AnomalyDiagnosticPlot <- function(df,alpha=0.2) {
  plot <- NULL
  if (!is.null(df))
  {
    try({
      #df <- data.frame(df[, 1:2])
      df<-tibble::as_tibble(df[, 1:2])
      colnames(df)<-c('date','value')
      plot <-
        df %>%timetk::plot_anomaly_diagnostics(date, value, .interactive = TRUE, .title = "Outliers",.line_color='blue',.alpha=alpha)
    }, silent = TRUE)
  }
  return(plot)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Anomaly Diagnostic Values ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --
AnomalyDiagnosticData <- function(df,alpha=0.5,maxAnomalies=0.2) {
  .data <- NULL
  if (!is.null(df))
  {
    try({
      df[,1] <- as.character.Date(lubridate::as_date(as.numeric(df[,1])))
      #df[,2] <- as.integer(df[,2])
      df<-tibble::as_tibble(df[, 1:2])
      colnames(df)<-c('date','value')
      .data <-
        df %>%timetk::tk_anomaly_diagnostics(date, value, .frequency = "auto",.trend = "auto",.alpha=alpha,.max_anomalies = maxAnomalies,.message = TRUE)
    }, silent = TRUE)
  }
  return(.data)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Anomaly Diagnostic Methods Outliers ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --
AnomalyDiagnosticDataOutliers <- function(dfDate,dfImputed,.alpha=0.5,.maxAnomalies=0.2) {
  .data <- NULL
  if (!is.null(df))
  {
    try({
      df<-cbind(as.character(dfDate),as.numeric(dfImputed))
      colnames(df)<-c('date','value')
      .data <-AnomalyDiagnosticData(df,alpha=.alpha,maxAnomalies=.maxAnomalies)
      .data<-.data %>% dplyr::filter(anomaly=="Yes") %>% count() %>% as.numeric()
      }, silent = TRUE)
  }
  return(.data)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Anomaly Diagnostic Methods Outliers ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --
AnomalyDiagnosticDataOutliersSummary <- function(df,.frequency,.alpha=0.5,.maxAnomalies=0.2) {
  .data <- NULL
  if (!is.null(df))
  {
    .matrixOuliers<-cbind(c("MICE","Linear Interpolation","Spline Interpolation","Stineman Interpolation",
                            "Kalman StructTS","Kalman StructTS Smooth","Kalman Space Models",
                            "Seasonally Decomposed - Interpolation","Seasonally Decomposed - Mean",
                            "Seasonally Decomposed - Kalman"),c(0,0,0,0,0,0,0,0,0,0))
    colnames(.matrixOuliers)<-c("Imputation method","Outliers")
    try({
      .miceData<-miceData(df,input$dateType)
      .naInterpDataLinear<-naInterpData(df,"linear",.frequency)
      .naInterpDataSpline<-naInterpData(df,"spline",.frequency)
      .naInterpDataStine<-naInterpData(df,"stine",.frequency)
      .naKalmanData1<-naKalmanData(df,'StructTS','FALSE',.frequency)
      .naKalmanData2<-naKalmanData(df,'StructTS','TRUE',.frequency)
      .naKalmanData3<-naKalmanData(df,'SpaceModels','TRUE',.frequency)
      .naSeadecData1<-naSeadecData(df,'interpolation',.frequency)
      .naSeadecData2<-naSeadecData(df,'mean',.frequency)
      .naSeadecData3<-naSeadecData(df,'kalman',.frequency)
      
      # Mice
      dfTst<-as.data.frame(cbind(date=lubridate::as_date(as.numeric(df[,1])),value=.miceData))
      if (!is.null(dfTst))
      { dfTst[,1] <- lubridate::as_date(dfTst[,1])
        .Data <- dfTst %>%timetk::tk_anomaly_diagnostics(date, value, .frequency = "auto",.trend = "auto",.alpha=.alpha,.max_anomalies = 0.2,.message = TRUE)
        if (!is.null(.Data))
        { .Data <-as.matrix(.Data %>% dplyr::filter(anomaly == "Yes") %>% count() %>% as.numeric())
          .matrixOuliers[1,2]<-.Data}}
      #naInterpDataLinear
      dfTst<-as.data.frame(cbind(date=lubridate::as_date(as.numeric(df[,1])),value=.naInterpDataLinear))
      if (!is.null(dfTst))
      { dfTst[,1] <- lubridate::as_date(dfTst[,1])
      .Data <- dfTst %>%timetk::tk_anomaly_diagnostics(date, value, .frequency = "auto",.trend = "auto",.alpha=.alpha,.max_anomalies = 0.2,.message = TRUE)
      if (!is.null(.Data))
      { .Data <-as.matrix(.Data %>% dplyr::filter(anomaly == "Yes") %>% count() %>% as.numeric())
      .matrixOuliers[2,2]<-.Data}}
      
      #naInterpDataSpline
      dfTst<-as.data.frame(cbind(date=lubridate::as_date(as.numeric(df[,1])),value=.naInterpDataSpline))
      if (!is.null(dfTst))
      { dfTst[,1] <- lubridate::as_date(dfTst[,1])
      .Data <- dfTst %>%timetk::tk_anomaly_diagnostics(date, value, .frequency = "auto",.trend = "auto",.alpha=.alpha,.max_anomalies = 0.2,.message = TRUE)
      if (!is.null(.Data))
      { .Data <-as.matrix(.Data %>% dplyr::filter(anomaly == "Yes") %>% count() %>% as.numeric())
      .matrixOuliers[3,2]<-.Data}}
      
      #naInterpDataStine
      dfTst<-as.data.frame(cbind(date=lubridate::as_date(as.numeric(df[,1])),value=.naInterpDataStine))
      if (!is.null(dfTst))
      { dfTst[,1] <- lubridate::as_date(dfTst[,1])
      .Data <- dfTst %>%timetk::tk_anomaly_diagnostics(date, value, .frequency = "auto",.trend = "auto",.alpha=.alpha,.max_anomalies = 0.2,.message = TRUE)
      if (!is.null(.Data))
      { .Data <-as.matrix(.Data %>% dplyr::filter(anomaly == "Yes") %>% count() %>% as.numeric())
      .matrixOuliers[4,2]<-.Data}}
      
      #naKalmanData1
      dfTst<-as.data.frame(cbind(date=lubridate::as_date(as.numeric(df[,1])),value=.naKalmanData1))
      if (!is.null(dfTst))
      { dfTst[,1] <- lubridate::as_date(dfTst[,1])
      .Data <- dfTst %>%timetk::tk_anomaly_diagnostics(date, value, .frequency = "auto",.trend = "auto",.alpha=.alpha,.max_anomalies = 0.2,.message = TRUE)
      if (!is.null(.Data))
      { .Data <-as.matrix(.Data %>% dplyr::filter(anomaly == "Yes") %>% count() %>% as.numeric())
      .matrixOuliers[5,2]<-.Data}}
      
      #naKalmanData2
      dfTst<-as.data.frame(cbind(date=lubridate::as_date(as.numeric(df[,1])),value=.naKalmanData2))
      if (!is.null(dfTst))
      { dfTst[,1] <- lubridate::as_date(dfTst[,1])
      .Data <- dfTst %>%timetk::tk_anomaly_diagnostics(date, value, .frequency = "auto",.trend = "auto",.alpha=.alpha,.max_anomalies = 0.2,.message = TRUE)
      if (!is.null(.Data))
      { .Data <-as.matrix(.Data %>% dplyr::filter(anomaly == "Yes") %>% count() %>% as.numeric())
      .matrixOuliers[6,2]<-.Data}}
      
      #naKalmanData3
      dfTst<-as.data.frame(cbind(date=lubridate::as_date(as.numeric(df[,1])),value=.naKalmanData3))
      if (!is.null(dfTst))
      { dfTst[,1] <- lubridate::as_date(dfTst[,1])
      .Data <- dfTst %>%timetk::tk_anomaly_diagnostics(date, value, .frequency = "auto",.trend = "auto",.alpha=.alpha,.max_anomalies = 0.2,.message = TRUE)
      if (!is.null(.Data))
      { .Data <-as.matrix(.Data %>% dplyr::filter(anomaly == "Yes") %>% count() %>% as.numeric())
      .matrixOuliers[7,2]<-.Data}}

      #naSeadecData1
      dfTst<-as.data.frame(cbind(date=lubridate::as_date(as.numeric(df[,1])),value=.naSeadecData1))
      if (!is.null(dfTst))
      { dfTst[,1] <- lubridate::as_date(dfTst[,1])
      .Data <- dfTst %>%timetk::tk_anomaly_diagnostics(date, value, .frequency = "auto",.trend = "auto",.alpha=.alpha,.max_anomalies = 0.2,.message = TRUE)
      if (!is.null(.Data))
      { .Data <-as.matrix(.Data %>% dplyr::filter(anomaly == "Yes") %>% count() %>% as.numeric())
      .matrixOuliers[8,2]<-.Data}}
      
      #naSeadecData2
      dfTst<-as.data.frame(cbind(date=lubridate::as_date(as.numeric(df[,1])),value=.naSeadecData2))
      if (!is.null(dfTst))
      { dfTst[,1] <- lubridate::as_date(dfTst[,1])
      .Data <- dfTst %>%timetk::tk_anomaly_diagnostics(date, value, .frequency = "auto",.trend = "auto",.alpha=.alpha,.max_anomalies = 0.2,.message = TRUE)
      if (!is.null(.Data))
      { .Data <-as.matrix(.Data %>% dplyr::filter(anomaly == "Yes") %>% count() %>% as.numeric())
      .matrixOuliers[9,2]<-.Data}}
      
      #naSeadecData3
      dfTst<-as.data.frame(cbind(date=lubridate::as_date(as.numeric(df[,1])),value=.naSeadecData3))
      if (!is.null(dfTst))
      { dfTst[,1] <- lubridate::as_date(dfTst[,1])
      .Data <- dfTst %>%timetk::tk_anomaly_diagnostics(date, value, .frequency = "auto",.trend = "auto",.alpha=.alpha,.max_anomalies = 0.2,.message = TRUE)
      if (!is.null(.Data))
      { .Data <-as.matrix(.Data %>% dplyr::filter(anomaly == "Yes") %>% count() %>% as.numeric())
      .matrixOuliers[10,2]<-.Data}}
      
    }, silent = TRUE)
  }
  return(.matrixOuliers)
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --
# STL Features  ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --
STLStrengthPlot <- function(df,period=12) {
  TrendStrengthPlot <- NULL
  if (!is.null(df))
  {
    try({
      fstl <- feasts::feat_stl(df[,2], period)
      trend_strength <- fstl[1]
      seasonal_strength_12 <- fstl[2]
      
      .decomp <- data.frame(trend = trend_strength, sasonality = seasonal_strength_12)
      .xref=0.5
      .yref=0.5
      .xtics=c(0,.xref,1)
      .ytics=c(0,.yref,1)
      
      TrendStrengthPlot<-ggplot(.decomp, aes(x = trend, y = sasonality)) +
        xlim(0, 1)+
        ylim(0, 1)+
        annotate("rect", xmin=c(.xref,.xref), xmax=c(1,1), ymin=c(.yref,.yref) , ymax=c(1,1), alpha=0.75, color="green", fill="green")+
        annotate("rect", xmin=c(.xref,.xref), xmax=c(1,1), ymin=c(0.00,0.00) , ymax=c(0.5,0.5), alpha=0.25, color="green", fill="green")+
        annotate("rect", xmin=c(0,0), xmax=c(.xref,.xref), ymin=c(.yref,.yref) , ymax=c(1,1), alpha=0.25, color="green", fill="green")+
        geom_point(colour = "black", size = 5)#+
        # scale_x_continuous(limits = c(0,1),breaks = .xtics)+
        # scale_y_continuous(limits = c(0,1),breaks = .ytics) #+
        # geom_hline(yintercept=.yref, color="black", size=1) + 
        # geom_vline(xintercept=.xref, color="black", size=1)
      
      TrendStrengthPlot <- TrendStrengthPlot + theme(
        panel.background = element_rect(fill = "transparent", colour = NA), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()
      )  
      
      
    }, silent = TRUE)
  }
  return(TrendStrengthPlot)
}