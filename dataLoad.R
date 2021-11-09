library(mice)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Load file data ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --
dataLoad<-function(filePath,fileHeaders,colSeparator){
  df<-NULL
  if (is.null(filePath))
    return(df)
  if (is.null(fileHeaders))
    fileHeaders<-FALSE
  df<-utils::read.csv(filePath, header = fileHeaders,sep = colSeparator)
  return(df)
}

dataSetBuild<-function(df,colDateNumber,colValueNumber,dateFormat){
  if (is.null(df))
    return(df)
  
  cols<-ncol(df)
  if (cols<2 | cols<colDateNumber | cols<colValueNumber  )
    return(NULL)
  
  colDateName="date"
  colValueName="value"
  
  if (is.null(colDateNumber) | is.null(colValueNumber))
    return(df)
  
  if (dateFormat=="ymd"){df<-cbind((df[,colDateNumber]),as.numeric(df[,colValueNumber]))}
  if (dateFormat=="ydm"){df<-cbind(lubridate::ydm(df[,colDateNumber]),as.numeric(df[,colValueNumber]))}
  if (dateFormat=="mdy"){df<-cbind(lubridate::mdy(df[,colDateNumber]),as.numeric(df[,colValueNumber]))}
  if (dateFormat=="myd"){df<-cbind(lubridate::myd(df[,colDateNumber]),as.numeric(df[,colValueNumber]))}
  if (dateFormat=="dmy"){df<-cbind(lubridate::dmy(df[,colDateNumber]),as.numeric(df[,colValueNumber]))}
  if (dateFormat=="dym"){df<-cbind(lubridate::dym(df[,colDateNumber]),as.numeric(df[,colValueNumber]))}
  
  colnames(df)<-c(colDateName,colValueName)
  if (is.na(df[1,colDateNumber])){
    return (NULL)
    
  }
    return(df)
}

summaryData<-function(df_in){
  if (is.null(df_in) | ncol(df_in)!=2)
    return (NULL)
  
  colnames(df_in)<-c('date','value')
  res<-Hmisc::describe(df_in)
  
  sumData<-NULL
  sumData<-rbind(sumData,c('date','count',as.numeric(length(df_in[,1]))))
  sumData<-rbind(sumData,c('date','missing',as.numeric(res$date$counts[2])))
  sumData<-rbind(sumData,c('date','distinct',as.numeric(res$date$counts[3])))
  
  sumData<-rbind(sumData,c('date','min',as.character(min(df_in[,1]))))
  sumData<-rbind(sumData,c('date','max',as.character(max(df_in[,1]))))
  sumData<-rbind(sumData,c('date','median',as.character(median(df_in[,1]))))
  # 
  sumData<-rbind(sumData,c('value','count',as.numeric(length(df_in[,1]))))
  sumData<-rbind(sumData,c('value','missing',as.numeric(res$value$counts[2])))
  sumData<-rbind(sumData,c('value','distinct',as.numeric(res$vaue$counts[3])))
  
  sumData<-rbind(sumData,c('value','min',min(df_in[,2])))
  sumData<-rbind(sumData,c('vaue','max',max(df_in[,2])))
  sumData<-rbind(sumData,c('value','median',median(df_in[,2])))
  sumData<-rbind(sumData,c('value','mean',round(mean(df_in[,2]),4)))
  sumData<-rbind(sumData,c('value','sd',round(sd(df_in[,2]),4)))
  
  if (!is.null(sumData)|ncol(sumData!=3))
    colnames(sumData)<-c('variable','metric','value')
  
  return(sumData)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Completa dados em falta - MICE ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- ---
miceData<-function(df_in,miceType){
  if (is.null(df_in) | ncol(df_in)!=2)
    return (NULL)
  
  date<-lubridate::as_date(df_in[,1])
  value<-as.numeric(df_in[,2])
  year<-lubridate::year(date)
  month<-lubridate::month(date)
  quarter<-ceiling(month/3)
  semester<-ceiling(month/6)
  df_mice<-cbind.data.frame(date,year,month,quarter,semester,value)
  
  preds_mice <-mice::mice(df_mice)
  pred_values <- mice::complete(preds_mice)
  
  df_out<-pred_values[,6]
  
  return(df_out)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Completa dados em falta - imputeTS::na.interpolation ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- ---
naInterpData<-function(df_in,interpType="linear",pFrequency=12){
  if (is.null(df_in) | ncol(df_in)!=2)
    return (NULL)

  df_ts<-xts::xts(as.numeric(df_in[,2]),order.by=as.Date(df_in[,1]),frequency=pFrequency)
  df_out<-as.numeric(imputeTS::na_interpolation(df_ts,option=interpType))
  
  
  return(df_out)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Completa dados em falta - imputeTS::na_kalman ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- ---
naKalmanData<-function(df_in,pModel="StructTS",pSmooth="TRUE",pFrequency=12)
  {
  if (is.null(df_in) | ncol(df_in)!=2)
    return (NULL)
  
  df_ts<-xts::xts(as.numeric(df_in[,2]),order.by=as.Date(df_in[,1]),frequency=pFrequency)
  if (pModel=="StructTS" & pSmooth=="TRUE")
    df_out<-as.numeric(imputeTS::na_kalman(df_ts,model=pModel,smooth =TRUE))
  if (pModel=="StructTS" & pSmooth=="FALSE")
    df_out<-as.numeric(imputeTS::na_kalman(df_ts,model=pModel,smooth =FALSE))
  if (pModel=="SpaceModels" & pSmooth=="TRUE")
    df_out<-as.numeric(imputeTS::na_kalman(df_ts,smooth =TRUE))
  
  return(df_out)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Completa dados em falta - imputeTS::na_seadec ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- ---
naSeadecData<-function(df_in,pModel="interpolation",pFrequency=12){
  if (is.null(df_in) | ncol(df_in)!=2)
    return (NULL)
  
  df_ts<-xts::xts(as.numeric(df_in[,2]),order.by=as.Date(df_in[,1]),frequency=pFrequency)
  df_out<-as.numeric(imputeTS::na_seadec(df_ts,algorithm =pModel))
  
  
  return(df_out)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Convert Data Received ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --
convertDataReceivedTS<-function(df_in,dataType,colValue){
  ts_out<-NULL
  rec<-length(df_in)
  
  if (is.null(df_in))
    return(NULL)
  
  if (!is.data.frame(df_in))
    df<-as.data.frame(df_in[,c(1,colValue)])
  else
    df<-df_in
  minDate<-min(df[,1])
  minYear<-lubridate::year(minDate)
  minMonth<-lubridate::month(minDate)
  minDay<-lubridate::day(minDate)
  minWeek<-lubridate::week(minDate)
  minQuarter<-ceiling(minMonth/3)
  minSemester<-ceiling(minMonth/6)
  
  maxDate<-max(df[,1])
  maxYear<-lubridate::year(maxDate)
  maxMonth<-lubridate::month(maxDate)
  maxDay<-lubridate::day(maxDate)
  maxWeek<-lubridate::week(maxDate)
  maxQuarter<-ceiling(maxMonth/3)
  maxSemester<-ceiling(maxMonth/6)
  
  
  if (dataType=="day")
  {
    dates<-seq(minDate, maxDate, by = "day")
    df_ref<-data.frame(dates)
    colnames(df_ref)=c('date')
    df<-dplyr::full_join(df_ref, df, by = "date")
    refDate<-as.Date(paste0(minYear,stringr::str_pad(1,2,side='left',pad="0"),stringr::str_pad(1,2,side='left',pad="0")), format = "%Y%m%d")
    minDay<-as.numeric(minDate - refDate)+1
    maxDay<-as.numeric(maxDate - refDate)+1
    # ts_out<-ts(df[,2],start=c(minYear,minDay),end=c(maxYear,maxDay),frequency = 365)
    ts_out<-xts::xts(df[,2],order.by = df[,1])
    colnames(ts_out)<-'value'
  }
  
  if (dataType=="week")
  {
    dates<-seq(minDate, maxDate, by = "week")
    df_ref<-data.frame(dates)
    colnames(df_ref)=c('date')
    df<-dplyr::full_join(df_ref, df, by = "date")
    ts_out<-ts(df[,2],start=c(minYear,minWeek),end=c(maxYear,maxWeek),frequency = 52)
  }
  
  if (dataType=="month")
  {
    dates<-seq(minDate, maxDate, by = "month")
    df_ref<-data.frame(dates)
    colnames(df_ref)=c('date')
    df<-dplyr::full_join(df_ref, df, by = "date")
    ts_out<-ts(df[,colValue],start=c(minYear,minMonth),end=c(maxYear,maxMonth),frequency = 12)
  }
  
  if (dataType=="quarter")
  {
    dates<-seq(minDate, maxDate, by = "quarter")
    df_ref<-data.frame(dates)
    colnames(df_ref)=c('date')
    df<-dplyr::full_join(df_ref, df, by = "date")
    ts_out<-xts::xts(df[,2],order.by = df[,1])
    colnames(ts_out)<-'value'
  }
  
  if (dataType=="semester")
  {
    minRefDate<-lubridate::ymd(paste0(stringr::str_pad(minYear,4,side='left','0'),stringr::str_pad(1,2,side='left','0'),stringr::str_pad(1,2,side='left','0')))
    maxRefDate<-lubridate::ymd(paste0(stringr::str_pad(maxYear,4,side='left','0'),stringr::str_pad(1,2,side='left','0'),stringr::str_pad(1,2,side='left','0')))
    dates<-seq(minRefDate, maxRefDate, by = "2 quarter")
    df_ref<-data.frame(dates)
    colnames(df_ref)=c('date')
    df<-dplyr::full_join(df_ref, df, by = "date")
    ts_out<-ts(df[,2],start = c(minYear,minSemester),end = c(maxYear,maxSemester),frequency = 2)
  }
  
  if (dataType=="year")
  {
    minRefDate<-lubridate::ymd(paste0(stringr::str_pad(minYear,4,side='left','0'),stringr::str_pad(1,2,side='left','0'),stringr::str_pad(1,2,side='left','0')))
    maxRefDate<-lubridate::ymd(paste0(stringr::str_pad(maxYear,4,side='left','0'),stringr::str_pad(1,2,side='left','0'),stringr::str_pad(1,2,side='left','0')))
    dates<-seq(minRefDate, maxRefDate, by = "year")
    df_ref<-data.frame(dates)
    colnames(df_ref)=c('date')
    df<-dplyr::full_join(df_ref, df, by = "date")
    ts_out<-ts(df[,colValue],start=c(minYear),end=c(maxYear),frequency = 1)
  }
  
  return (ts_out)
}

convertDataReceivedDF<-function(df_in,dataType,colValue){
  
  if (is.null(df_in))
    return(NULL)
  
  date<-lubridate::as_date(df_in[,1])
  value<-as.numeric(df_in[,2])
  df<-cbind.data.frame(date,value)
  colnames(df)<-c('date','value')
  
  rec<-length(df)
  
  minDate<-min(df[,1])
  minYear<-lubridate::year(minDate)
  minMonth<-lubridate::month(minDate)
  minDay<-lubridate::day(minDate)
  minWeek<-lubridate::week(minDate)
  minQuarter<-ceiling(minMonth/3)
  minSemester<-ceiling(minMonth/6)
  
  maxDate<-max(df[,1])
  maxYear<-lubridate::year(maxDate)
  maxMonth<-lubridate::month(maxDate)
  maxDay<-lubridate::day(maxDate)
  maxWeek<-lubridate::week(maxDate)
  maxQuarter<-ceiling(maxMonth/3)
  maxSemester<-ceiling(maxMonth/6)
  
  
  if (dataType=="day")
  {
    dates<-seq(minDate, maxDate, by = "day")
  }
  
  if (dataType=="week")
  {
    dates<-seq(minDate, maxDate, by = "week")
  }
  
  if (dataType=="month")
  {
    dates<-seq(minDate, maxDate, by = "month")
  }
  
  if (dataType=="quarter")
  {
    dates<-seq(minDate, maxDate, by = "quarter")
  }
  
  if (dataType=="semester")
  {
    minRefDate<-lubridate::ymd(paste0(stringr::str_pad(minYear,4,side='left','0'),stringr::str_pad(1,2,side='left','0'),stringr::str_pad(1,2,side='left','0')))
    maxRefDate<-lubridate::ymd(paste0(stringr::str_pad(maxYear,4,side='left','0'),stringr::str_pad(1,2,side='left','0'),stringr::str_pad(1,2,side='left','0')))
    dates<-seq(minRefDate, maxRefDate, by = "2 quarter")
  }
  
  if (dataType=="year")
  {
    minRefDate<-lubridate::ymd(paste0(stringr::str_pad(minYear,4,side='left','0'),stringr::str_pad(1,2,side='left','0'),stringr::str_pad(1,2,side='left','0')))
    maxRefDate<-lubridate::ymd(paste0(stringr::str_pad(maxYear,4,side='left','0'),stringr::str_pad(1,2,side='left','0'),stringr::str_pad(1,2,side='left','0')))
    dates<-seq(minRefDate, maxRefDate, by = "year")
  }
  
  df_ref<-data.frame(dates)
  colnames(df_ref)=c('date')
  df<-dplyr::full_join(df_ref, df, by = "date")
  
  return (df)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Tranning Models DataFrame Convertion ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --
invertedDF<-function(originalDF){
  nCol<-0
  invertedDF<-NULL
  if (is.null(originalDF))
    return (invertedDF)
  dataModel<-data.frame(model="",date="yyyy-mm-dd",value=0000.000,forecast=0000.000)
  for (colName in colnames(originalDF))
  {
    nCol<-nCol+1
    if (nCol>3 & !colName=="residuals"){
      dataModel<-cbind(model=colName,
                       date=as.Date(as.numeric(originalDF[,1]), '1970-01-01'),
                       value=as.numeric(originalDF[,3]),
                       forecast=round(as.numeric(originalDF[,nCol]),3))
      if (is.null(invertedDF))
      {invertedDF<-dataModel}
      else
      {invertedDF<-rbind(invertedDF,dataModel)}
    } #End if
  } #End for
  return (data.frame(invertedDF))
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Tranning Models Residuals DataFrame ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --
residualsDF<-function(residualModelName,residualsDF,originalDF){
  if (nrow(residualsDF)==0 | is.null(residualModelName))
  {return(originalDF)}
  df<-as.data.frame(residualsDF)
  dataModel<-cbind(model=residualModelName,
                   date=df[,1],
                   value=round(df[,2],3))
  if (is.null(originalDF))
  {originalDF<-dataModel}
  else
  {originalDF<-rbind(originalDF,dataModel)}
  
  return (originalDF)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Tranning Models Results Individal Plot ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --
modelsfacetPlot<-function(df_in){
  invertedFacetPlot<-NULL
  if(is.null(df_in))
    return(invertedFacetPlot)
  
  df<-as.data.frame(df_in)
  df[,2] <- as.numeric(df[,2])
  df[,2] <- as.Date(df[,2], '1970-01-01')
  df[,3] <- as.numeric(df[,3])
  df[,4] <- as.numeric(df[,4])
  
  invertedFacetPlot<- ggplot(df) 
  invertedFacetPlot<- invertedFacetPlot + geom_line(aes(date,value),colour='red')
  invertedFacetPlot<- invertedFacetPlot + geom_line(aes(date,forecast),colour='blue')
  invertedFacetPlot<- invertedFacetPlot + facet_wrap(vars(model), ncol = 2)
  return (invertedFacetPlot)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Tranning Models Residuals Individal Plot ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --
residualsfacetPlot<-function(df_in){
  df<-as.data.frame(df_in)
  colnames(df)=c('model','date','value')
  df[['date']] <- as.numeric(df[['date']])
  df[['date']] <- as.Date(df[['date']], '1970-01-01')
  df[['value']] <- as.numeric(df[['value']])
  #df<- dplyr::filter(df,model=='arima')
  #df<- dplyr::filter(df,date>='2018-01-01')
  
  residualsFacetPlot<- ggplot(df) 
  residualsFacetPlot<- residualsFacetPlot + geom_line(aes(date,value),colour='red')
  residualsFacetPlot<- residualsFacetPlot + facet_wrap(vars(model), ncol = 2)
  return (residualsFacetPlot)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Time Series Plot ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --
TimeSeriesPlot<-function(df){
  df<-as.data.frame(df)
  colnames(df)=c('date','value')
  df[['date']]<-as.Date(df[['date']],'%Y-%m-%d')
  df[['value']]<-as.numeric(df[['value']])
  plot<-
    df %>%
    timetk::plot_time_series(date, value, .interactive = FALSE) +
    ggtitle("Time Series",  subtitle = "...")
  return(plot)
}

