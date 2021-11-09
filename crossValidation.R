library(ggplot2)
library(forecast)
library(dplyr)


# ------------------------------------------------------
# Model definition
# ------------------------------------------------------
mbats <- function(x, h,vrs){forecast(forecast::bats(x, use.box.cox=TRUE,use.trend =TRUE,use.damped.trend=TRUE,use.arma.errors=TRUE), h=h)}
mets <- function(x, h,vrs){forecast(forecast::ets(x, model=vrs), h=h)}
mnnetar <- function(x, h,vrs){forecast(forecast::nnetar(x, lambda=TRUE, decay=0.5, maxit=150), h=h)}
mautoarima <- function(x, h,vrs){forecast(forecast::auto.arima(x, lambda=TRUE, stepwise=TRUE, nmodels=12), h=h)}
mstruct <- function(x, h,vrs){forecast(stats::StructTS(x, type = c("level", "trend", "BSM"), optim.control = "L-BFGS-B"), h=h)}
mhw <- function(x, h,vrs){forecast(stats::HoltWinters(x), h=h)}
mrwf<- function(x, h,vrs){forecast::rwf(x, h=h, drift=TRUE, lambda="auto")}
mnaive<- function(x, h,vrs){forecast::naive(x, h=h)}
msnaive<- function(x, h,vrs){forecast::snaive(x, h=h, drift=TRUE, lambda="auto")}

mprophet<- function(x, h)
{
  colnames(x)<-c('ds','y')
  m<-prophet::prophet(x)
  future <- prophet::make_future_dataframe(m, periods = h)
  l2<-as.numeric(count(future))
  l1<-as.numeric(l2-h)+1
  
  try({
  pred<-stats::predict(m,future)
  pred<-pred$yhat
  pred<-as.data.frame(pred)
  return(pred[l1:l2,])
  }, silent = TRUE)
  return(NULL)
}

mthetaf<- function(x, h, vrs){forecast::thetaf(x, h=h)}

# ------------------------------------------------------
# Data Series 
# df_data<-cvData(df,.h,.Window,.initial,.step,.max)
# ------------------------------------------------------
cvData <- function(df,.forecast,.train,.initial,.step,.max,.type)
{
  if (is.null(df))
    return(NULL)
  
  # Adjust initial parameters
  .total<-nrow(df)
  
  df_result<-NULL
  
  try({
    if(.type=="Fixed Window")
  {
    .initial<-ifelse((.initial+.train+.forecast)<.total,.initial,.total-.train-.forecast)
    .train<-ifelse(.train>=.forecast,.train,.forecast)
    .s<-.initial
    .e<-.total-.train-.forecast
    .seq<-seq(from=.s,to=.e,by=.step)
    n<-0
    for (i in .seq) 
    {
      n<-n+1
      if (n>.max)
        break
      
      .start<-i
      .end<-.start+.train+.forecast
      .mid<-.end-.forecast
      
      # df_train<-cbind(cv=paste0("FOREC-",stringr::str_pad(n, 2, pad = "0")),type="train",dados_df[.start:.mid,])
      # df_test<-cbind(cv=paste0("FOREC-",stringr::str_pad(n, 2, pad = "0")),type="test",dados_df[(.mid+1):.end,])
      df_train<-cbind(cv=paste0("FOREC-",stringr::str_pad(n, 2, pad = "0")),type="train",df[.start:.mid,])
      df_test<-cbind(cv=paste0("FOREC-",stringr::str_pad(n, 2, pad = "0")),type="test",df[(.mid+1):.end,])
      df_result<-rbind(df_result,df_train,df_test)
    }
  }}, silent = TRUE)
  
  try({
    if(.type=="Random Fixed Window")
  {
    .initial<-ifelse((.initial+.train+.forecast)<.total,.initial,.total-.train-.forecast)
    .train<-ifelse(.train>=.forecast,.train,.forecast)
    .step<-1
    .s<-.initial
    .e<-.total-.train-.forecast
    .seq<-seq(from=.s,to=.e,by=1)
    .seq<-sample(x=.seq, size=.max, replace=FALSE)
    
    n<-0
    for (i in .seq) 
    {
      n<-n+1
      if (n>.max)
        break
      
      .start<-i
      .end<-.start+.train+.forecast
      .mid<-.end-.forecast
      # df_train<-cbind(cv=paste0("FOREC-",stringr::str_pad(n, 2, pad = "0")),type="train",dados_df[.start:.mid,])
      # df_test<-cbind(cv=paste0("FOREC-",stringr::str_pad(n, 2, pad = "0")),type="test",dados_df[(.mid+1):.end,])
      df_train<-cbind(cv=paste0("FOREC-",stringr::str_pad(n, 2, pad = "0")),type="train",df[.start:.mid,])
      df_test<-cbind(cv=paste0("FOREC-",stringr::str_pad(n, 2, pad = "0")),type="test",df[(.mid+1):.end,])
      df_result<-rbind(df_result,df_train,df_test)
    }
  }}, silent = TRUE)
    try({
      if(.type=="Rolling")
  {
    .initial<-ifelse((.initial+.train+.forecast)<.total,.initial,.total-.train-.forecast)
    .train<-ifelse(.train>=.forecast,.train,.forecast)
    .step<-1
    .s<-.initial
    .e<-.total-.train-.forecast
    .seq<-seq(from=.s,to=.e,by=1)
    
    n<-0
    for (i in .seq) 
    {
      n<-n+1
      if (n>.max)
        break
      
      .start<-.s
      .end<-i+.train+.forecast
      .mid<-.end-.forecast
      # df_train<-cbind(cv=paste0("FOREC-",stringr::str_pad(n, 2, pad = "0")),type="train",dados_df[.start:.mid,])
      # df_test<-cbind(cv=paste0("FOREC-",stringr::str_pad(n, 2, pad = "0")),type="test",dados_df[(.mid+1):.end,])
      df_train<-cbind(cv=paste0("FOREC-",stringr::str_pad(n, 2, pad = "0")),type="train",df[.start:.mid,])
      df_test<-cbind(cv=paste0("FOREC-",stringr::str_pad(n, 2, pad = "0")),type="test",df[(.mid+1):.end,])
      df_result<-rbind(df_result,df_train,df_test)
    }
  }}, silent = TRUE)
  
    
      try({
        if(.type=="Random Rolling")
  {
    .initial<-ifelse((.initial+.train+.forecast)<.total,.initial,.total-.train-.forecast)
    .train<-ifelse(.train>=.forecast,.train,.forecast)
    .step<-1
    .s<-.initial
    .e<-.total-.train-.forecast
    .seq<-seq(from=.s,to=.e,by=1)
    .seq<-sample(x=.seq, size=.max, replace=FALSE)
    
    n<-0
    for (i in .seq) 
    {
      n<-n+1
      if (n>.max)
        break
      
      .start<-.s
      .end<-i+.train+.forecast
      .mid<-.end-.forecast
      # df_train<-cbind(cv=paste0("FOREC-",stringr::str_pad(n, 2, pad = "0")),type="train",dados_df[.start:.mid,])
      # df_test<-cbind(cv=paste0("FOREC-",stringr::str_pad(n, 2, pad = "0")),type="test",dados_df[(.mid+1):.end,])
      df_train<-cbind(cv=paste0("FOREC-",stringr::str_pad(n, 2, pad = "0")),type="train",df[.start:.mid,])
      df_test<-cbind(cv=paste0("FOREC-",stringr::str_pad(n, 2, pad = "0")),type="test",df[(.mid+1):.end,])
      df_result<-rbind(df_result,df_train,df_test)
    }
  }}, silent = TRUE)
  
  
  return(df_result)
}

# ------------------------------------------------------
# Data Series - Plot
# ------------------------------------------------------
cvDataPlots <- function(df,.forecast,.train,.initial,.step,.max,.type)
{
  if (is.null(df))
    return(NULL)
  
  df_result<-df
  
  .Plot<-ggplot(df_result, aes(x=date,y=value,group=type)) + 
    geom_line(aes(color=type)) + 
    facet_grid(rows = vars(cv))+
    ggtitle("Cross Validation - Data Series") + 
    theme_bw()
  
  return(.Plot)
}

# ------------------------------------------------------
# Calculate residual for specific model & Serie
# ------------------------------------------------------
cvErrors<-function(df,forecastfunction,h=1,vrs=NULL){
  # Split Series between train & test data
  .train<-df %>% dplyr::filter(type=="train")
  .test<-df %>% dplyr::filter(type!="train")
  y<-.train[,3:4]
  y$date<-as.Date(y$date)
  y<-xts::xts(y$value, y$date)
  y<-as.ts(y)
  h<-ifelse(nrow(.test)<h,nrow(.test),h)
  vt<-list()
  
  # Forecast test values based on tran data
  .r<-try(suppressWarnings
      (forecastfunction(y,h,vrs)),
      silent = TRUE)
  if (is.element("try-error",class(.r)))
    return(NULL)
  
  # Calculate residuals
  .residuals<-.test[,4]-.r$mean
  vt$actual<-as.vector(.test$value)
  vt$forec<-as.vector(.r$mean)
  vt$residuals<-as.vector(.residuals)
  
  return(vt)
}

cvErrorsProphet<-function(df,h=1){
  # Split Series between train & test data
  .train<-df %>% dplyr::filter(type=="train")
  .test<-df %>% dplyr::filter(type!="train")
  y<-.train[,3:4]
  y$date<-as.Date(y$date)
  h<-ifelse(nrow(.test)<h,nrow(.test),h)
  vt<-list()
  
  # Forecast test values based on tran data
  .r<-try(suppressWarnings
          (mprophet(y,h)),
          silent = TRUE)
  if (is.element("try-error",class(.r)))
    return(NULL)
  # Calculate residuals
  .residuals<-.test[,4]-.r
  vt$actual<-as.vector(.test$value)
  vt$forec<-as.vector(.r)
  vt$residuals<-as.vector(.residuals)
  
  return(vt)
}

# ------------------------------------------------------
# Calculate residual for specific model & Serie
# df_results<-cvResults(df_data,mautoarima,.h)
# ------------------------------------------------------
cvResults<-function(df,forecastfunction,h=1,vrs=NULL){
  if (is.null(df))
    return (NULL)
  
  .cvs <- df %>% summarise(cv) %>% distinct()
  .cvR <- ts(matrix(NA_real_, nrow = nrow(.cvs), ncol = h*3))
  
  for (i in 1:nrow(.cvs))
  {
    .filter <- .cvs$cv[i]
    df_cv <- df %>% filter(cv == .filter)
    .cve <- cvErrors(df_cv, forecastfunction, h = h, vrs = vrs)
    ci<-1
    cf<-h
    cl<-length(.cve$residuals)
    if (cl>0)
      {
      .cvR[i,ci:cf]<-.cve$actual
      ci<-ci+h
      cf<-ci+h-1
      .cvR[i,ci:cf]<-.cve$forec
      ci<-ci+h
      cf<-ci+h-1
      .cvR[i,ci:cf] <- .cve$residuals
    }
  }
  return(.cvR)
}

cvResultsProphet<-function(df,h=1){
  if (is.null(df))
    return (NULL)
  
  .cvs <- df %>% summarise(cv) %>% distinct()
  .cvR <- ts(matrix(NA_real_, nrow = nrow(.cvs), ncol = h*3))
  
  for (i in 1:nrow(.cvs))
  {
    .filter <- .cvs$cv[i]
    df_cv <- df %>% filter(cv == .filter)
    .cve <- cvErrorsProphet(df_cv, h = h)
    ci<-1
    cf<-h
    cl<-length(.cve$residuals)
    if (cl>0)
    {
      .cvR[i,ci:cf]<-.cve$actual
      ci<-ci+h
      cf<-ci+h-1
      .cvR[i,ci:cf]<-.cve$forec
      ci<-ci+h
      cf<-ci+h-1
      .cvR[i,ci:cf] <- .cve$residuals
    }
  }
  return(.cvR)
}

# ------------------------------------------------------
# Medium Error
# ------------------------------------------------------
mtrME <- function(mtr)
{
  if(is.null(mtr))
    return(NULL)
  
  # if (is.null(ncol(mtr))){
  #   if (length(mtr)==0)
  #     return(NULL)
  #   else{
  #     .ME <- vector("numeric", 1)
  #     .ME[1] <- mean(mtr, na.rm = TRUE)
  #     return(.ME)
  #   }
  # }
     
  cl <- ncol(mtr)
  if (is.null(cl)){return(as.vector(mtr))}
  .ME <- vector("numeric", cl)
  for (i in 1:cl){.ME[i] <- mean(mtr[, i], na.rm = TRUE)}
  return(.ME)
}

# ------------------------------------------------------
# Medium Absolute Error
# ------------------------------------------------------
mtrMAE <- function(mtr)
{
  if(is.null(mtr))
    return(NULL)
  
  # if (is.null(ncol(mtr))){
  #   if (length(mtr)==0)
  #     return(NULL)
  #   else{
  #     .MAE <- vector("numeric", 1)
  #     .MAE[1] <- mean(abs(mtr), na.rm = TRUE)
  #     return(.MAE)
  #   }
  # }
  
  cl <- ncol(mtr)
  if (is.null(cl))
  {
    .MAE <- as.vector(mtr)
    cl <- length(mtr)
    for (i in 1:cl){.MAE[i] <- mean(abs(mtr[i]), na.rm = TRUE)}
    return(.MAE)
  }
  
  .MAE <- vector("numeric", cl)
  for (i in 1:cl){.MAE[i] <- mean(abs(mtr[, i]), na.rm = TRUE)}
  return(.MAE)
}

# ------------------------------------------------------
# Medium Absolute Percentage Error
# ------------------------------------------------------
mtrMAPE <- function(mtr,actual)
{
  if(is.null(mtr))
    return(NULL)
  
  cl <- ncol(mtr)
  if (is.null(cl))
  {
    .MAPE <- as.vector(mtr)
    .ACTUAL<-as.vector(actual)
    cl <- length(mtr)
    for (i in 1:cl){.MAPE[i] <- sum(abs(.MAPE[i]), na.rm = TRUE)/sum(.ACTUAL[i], na.rm = TRUE)}
    return(.MAPE)
  }
  
  .MAPE <- vector("numeric", cl)
  for (i in 1:cl){
    .MAPE[i] <- sum(abs(mtr[, i]), na.rm = TRUE)/sum(actual, na.rm = TRUE)
  }
  return(.MAPE)
}

# ------------------------------------------------------
# Medium Squared Error
# ------------------------------------------------------
mtrMSE <- function(mtr)
{
  if(is.null(mtr))
    return(NULL)
  
  cl <- ncol(mtr)
  if (is.null(cl))
  {
    .MSE <- as.vector(mtr)
    cl <- length(mtr)
    for (i in 1:cl){.MSE[i] <- mean(.MSE[i]^2, na.rm = TRUE)}
    return(.MSE)
  }
  .MSE <- vector("numeric", cl)
  for (i in 1:cl){.MSE[i] <- mean(mtr[, i]^2, na.rm = TRUE)}
  return(.MSE)
}

# ------------------------------------------------------
# Root Medium Squared Error
# ------------------------------------------------------
mtrRMSE <- function(mtr)
{
  if(is.null(mtr))
    return(NULL)
  
  cl <- ncol(mtr)
  if (is.null(cl))
  {
    .RMSE <- as.vector(mtr)
    cl <- length(mtr)
    for (i in 1:cl){.RMSE[i] <- sqrt(mean(.RMSE[i]^2, na.rm = TRUE))}
    return(.RMSE)
    }
  .RMSE <- vector("numeric", cl)
  for (i in 1:cl){.RMSE[i] <- sqrt(mean(mtr[, i]^2, na.rm = TRUE))}
  return(.RMSE)
}

# ------------------------------------------------------
# Standard Deviation
# ------------------------------------------------------
mtrSD <- function(mtr)
{
  if(is.null(mtr))
    return(NULL)
  
  cl <- ncol(mtr)
  .SD <- vector("numeric", cl)
  for (i in 1:cl){.SD[i] <- sd(mtr[, i], na.rm = TRUE)}
  return(.SD)
}

# ------------------------------------------------------
# Mean
# ------------------------------------------------------
mtrMEAN <- function(mtr)
{
  if(is.null(mtr))
    return(NULL)
  
  cl <- ncol(mtr)
  .MEAN <- vector("numeric", cl)
  for (i in 1:cl){.MEAN[i] <- mean(mtr[, i], na.rm = TRUE)}
  return(.MEAN)
}

# ------------------------------------------------------
# $$$$$$
# ------------------------------------------------------
mtrLINE <- function(h)
{
  if(is.null(h))
    return(NULL)
  
  .LINE <- vector("character", h+2)
  for (i in 1:h){.LINE[i] <- paste0("FOREC-",stringr::str_pad(i, 2, pad = "0"))}
  i<-i+1
  .LINE[i]<-"Mean"
  i<-i+1
  .LINE[i]<-"SD"
  
  return(.LINE)
}

# ------------------------------------------------------
# Table convertion 
# ------------------------------------------------------
tabConv2 <- function(df) {
  
  .ncols<-length(df)
  if (is.null(df) | .ncols<=1)
    return (NULL)
  
  df2<-NULL
  .col<-2
  for (.i in seq(from=2,to=.ncols)){
    .colName<-colnames(df)[.i]
    if (is.null(df2)){df2<-cbind(.colName, df[,1], df[,.i])}
    else {df2<-rbind(df2, cbind(.colName, df[, 1], df[,.i]))}
  }

  colnames(df2) <- c("Methods", "CV", "Value")
  
  df2 <- as.data.frame(df2)
  df2[, 3] <- as.double(df2[, 3])
  df2 <-df2 %>% dplyr::filter(CV != "Mean") %>% dplyr::filter(CV != "SD")
  
  return (df2)
}

# ------------------------------------------------------
# Cross Values Errors Table
# ------------------------------------------------------
CrossValuesErrors <- function(df, methodName, colNames) {
  df2<-NULL
  n <- 1
  length(colNames)
  for (c in colNames)
  {
    if (n == 1)  {df2 <- cbind(methodName, colNames[n], if(is.null(ncol(df))){df[1]} else df[, n])}
    else {df2 <- rbind(df2, cbind(methodName, colNames[n], if(is.null(ncol(df))){df[1]} else df[, n]))}
    n <- n + 1
  }
  
  colnames(df2) <- c("Methods", "CV", "Value")
  
  df2 <- as.data.frame(df2)
  df2[, 3] <- as.double(df2[, 3])
  df2 <- df2 %>% dplyr::filter(CV != "Mean") %>% dplyr::filter(CV != "SD")
  
  return (df2)
}

CrossValuesErrorsBoxPlot <- function(df) {

  .boxplotError<-NULL
  .Title1<-'Cross Validation - Errors - BoxPlot'
  .boxplotError <- ggplot(df, aes(x = CV, y = Value,group=CV)) +
    geom_boxplot(aes(color = CV)) +
    geom_hline(yintercept = 0,
               linetype = "dashed",
               color = "red") +
    geom_jitter(aes(color = CV)) +
    facet_wrap(~Methods)+
    ggtitle(.Title1) +
    theme_bw()
  
  return(.boxplotError)
}

colNamesFunc<-function(t,s){
  .colNames<-NULL
  for (n in s)
  {
    .colNames<-append(.colNames,paste0(t,stringr::str_pad(n,2,side="left",pad="0")))
  }
  return(.colNames)
}

# ------------------------------------------------------
# Build ME Matrix
# ------------------------------------------------------
tabME <-
  function(cvrwf,
           cvnaive,
           cvsnaive,
           cvbats,
           cvetsANN,
           cvetsMAM,
           cvetsZZZ,
           cvnnetar,
           cvautoarima,
           cvstruct,
           cvhw,
           cvprophet,
           cvthetaf,
           .h) {
    
    ME<-NULL
    colsn <- NULL
      
    if (!is.null(cvrwf)){
      colsn<-append(colsn,"rwf")
      if(!is.null(ME)){ME<-cbind(ME, as.data.frame(as.numeric(mtrME(cvrwf))))}
      else {ME<-as.data.frame(as.numeric(mtrME(cvrwf)))}}
        

    if (!is.null(cvnaive)){
      colsn<-append(colsn,"naive")
      if(!is.null(ME)){ME<-cbind(ME, as.data.frame(as.numeric(mtrME(cvnaive))))}
      else {ME<-as.data.frame(as.numeric(mtrME(cvnaive)))}}
    
    if (!is.null(cvsnaive)){
      colsn<-append(colsn,"snaive")
      if(!is.null(ME)){ME<-cbind(ME, as.data.frame(as.numeric(mtrME(cvsnaive))))}
      else {ME<-as.data.frame(as.numeric(mtrME(cvsnaive)))}}
    
    if (!is.null(cvbats)){
      colsn<-append(colsn,"bats")
      if(!is.null(ME)){ME<-cbind(ME, as.data.frame(as.numeric(mtrME(cvbats))))}
      else {ME<-as.data.frame(as.numeric(mtrME(cvbats)))}}
    
    if (!is.null(cvetsANN)){
      colsn<-append(colsn,"etsANN")
      if(!is.null(ME)){ME<-cbind(ME, as.data.frame(as.numeric(mtrME(cvetsANN))))}
      else {ME<-as.data.frame(as.numeric(mtrME(cvetsANN)))}}
    
    if (!is.null(cvetsMAM)){
      colsn<-append(colsn,"etsMAM")
      if(!is.null(ME)){ME<-cbind(ME, as.data.frame(as.numeric(mtrME(cvetsMAM))))}
      else {ME<-as.data.frame(as.numeric(mtrME(cvetsMAM)))}}
    
    if (!is.null(cvetsZZZ)){
      colsn<-append(colsn,"etsZZZ")
      if(!is.null(ME)){ME<-cbind(ME, as.data.frame(as.numeric(mtrME(cvetsZZZ))))}
      else {ME<-as.data.frame(as.numeric(mtrME(cvetsZZZ)))}}
    
    if (!is.null(cvnnetar)){
      colsn<-append(colsn,"nnetar")
      if(!is.null(ME)){ME<-cbind(ME, as.data.frame(as.numeric(mtrME(cvnnetar))))}
      else {ME<-as.data.frame(as.numeric(mtrME(cvnnetar)))}}
    
    if (!is.null(cvautoarima)){
      colsn<-append(colsn,"autoarima")
      if(!is.null(ME)){ME<-cbind(ME, as.data.frame(as.numeric(mtrME(cvautoarima))))}
      else {ME<-as.data.frame(as.numeric(mtrME(cvautoarima)))}}
    
    if (!is.null(cvstruct)){
      colsn<-append(colsn,"struct")
      if(!is.null(ME)){ME<-cbind(ME, as.data.frame(as.numeric(mtrME(cvstruct))))}
      else {ME<-as.data.frame(as.numeric(mtrME(cvstruct)))}}
    
    if (!is.null(cvhw)){
      colsn<-append(colsn,"hw")
      if(!is.null(ME)){ME<-cbind(ME, as.data.frame(as.numeric(mtrME(cvhw))))}
      else {ME<-as.data.frame(as.numeric(mtrME(cvhw)))}}
    
    if (!is.null(cvprophet)){
      colsn<-append(colsn,"prophet")
      if(!is.null(ME)){ME<-cbind(ME, as.data.frame(as.numeric(mtrME(cvprophet))))}
      else {ME<-as.data.frame(as.numeric(mtrME(cvprophet)))}}
    
    if (!is.null(cvthetaf)){
      colsn<-append(colsn,"thetaf")
      if(!is.null(ME)){ME<-cbind(ME, as.data.frame(as.numeric(mtrME(cvthetaf))))}
      else {ME<-as.data.frame(as.numeric(mtrME(cvthetaf)))}}
    
    ME <- rbind(ME, as.numeric(mtrMEAN(ME)))
    ME <- rbind(ME, as.numeric(mtrSD(ME)))
    ME <- cbind(as.data.frame(mtrLINE(.h)), ME)
    
    colnames(ME) <- c('Type', colsn)
    
    return (ME)
  }

# ------------------------------------------------------
# Build MSE Matrix
# ------------------------------------------------------
tabMSE <-
  function(cvrwf,
           cvnaive,
           cvsnaive,
           cvbats,
           cvetsANN,
           cvetsMAM,
           cvetsZZZ,
           cvnnetar,
           cvautoarima,
           cvstruct,
           cvhw,
           cvprophet,
           cvthetaf,
           .h) {

    MSE<-NULL
    colsn <- NULL
    
    if (!is.null(cvrwf)){
      colsn<-append(colsn,"rwf")
      if(!is.null(MSE)){MSE<-cbind(MSE, as.data.frame(as.numeric(mtrMSE(cvrwf))))}
      else {MSE<-as.data.frame(as.numeric(mtrMSE(cvrwf)))}}
    
    
    if (!is.null(cvnaive)){
      colsn<-append(colsn,"naive")
      if(!is.null(MSE)){MSE<-cbind(MSE, as.data.frame(as.numeric(mtrMSE(cvnaive))))}
      else {MSE<-as.data.frame(as.numeric(mtrMSE(cvnaive)))}}
    
    if (!is.null(cvsnaive)){
      colsn<-append(colsn,"snaive")
      if(!is.null(MSE)){MSE<-cbind(MSE, as.data.frame(as.numeric(mtrMSE(cvsnaive))))}
      else {MSE<-as.data.frame(as.numeric(mtrMSE(cvsnaive)))}}
    
    if (!is.null(cvbats)){
      colsn<-append(colsn,"bats")
      if(!is.null(MSE)){MSE<-cbind(MSE, as.data.frame(as.numeric(mtrMSE(cvbats))))}
      else {MSE<-as.data.frame(as.numeric(mtrMSE(cvbats)))}}
    
    if (!is.null(cvetsANN)){
      colsn<-append(colsn,"etsANN")
      if(!is.null(MSE)){MSE<-cbind(MSE, as.data.frame(as.numeric(mtrMSE(cvetsANN))))}
      else {MSE<-as.data.frame(as.numeric(mtrMSE(cvetsANN)))}}
    
    if (!is.null(cvetsMAM)){
      colsn<-append(colsn,"etsMAM")
      if(!is.null(MSE)){MSE<-cbind(MSE, as.data.frame(as.numeric(mtrMSE(cvetsMAM))))}
      else {MSE<-as.data.frame(as.numeric(mtrMSE(cvetsMAM)))}}
    
    if (!is.null(cvetsZZZ)){
      colsn<-append(colsn,"etsZZZ")
      if(!is.null(MSE)){MSE<-cbind(MSE, as.data.frame(as.numeric(mtrMSE(cvetsZZZ))))}
      else {MSE<-as.data.frame(as.numeric(mtrMSE(cvetsZZZ)))}}
    
    if (!is.null(cvnnetar)){
      colsn<-append(colsn,"nnetar")
      if(!is.null(MSE)){MSE<-cbind(MSE, as.data.frame(as.numeric(mtrMSE(cvnnetar))))}
      else {MSE<-as.data.frame(as.numeric(mtrMSE(cvnnetar)))}}
    
    if (!is.null(cvautoarima)){
      colsn<-append(colsn,"autoarima")
      if(!is.null(MSE)){MSE<-cbind(MSE, as.data.frame(as.numeric(mtrMSE(cvautoarima))))}
      else {MSE<-as.data.frame(as.numeric(mtrMSE(cvautoarima)))}}
    
    if (!is.null(cvstruct)){
      colsn<-append(colsn,"struct")
      if(!is.null(MSE)){MSE<-cbind(MSE, as.data.frame(as.numeric(mtrMSE(cvstruct))))}
      else {MSE<-as.data.frame(as.numeric(mtrMSE(cvstruct)))}}
    
    if (!is.null(cvhw)){
      colsn<-append(colsn,"hw")
      if(!is.null(MSE)){MSE<-cbind(MSE, as.data.frame(as.numeric(mtrMSE(cvhw))))}
      else {MSE<-as.data.frame(as.numeric(mtrMSE(cvhw)))}}
    
    if (!is.null(cvprophet)){
      colsn<-append(colsn,"prophet")
      if(!is.null(MSE)){MSE<-cbind(MSE, as.data.frame(as.numeric(mtrMSE(cvprophet))))}
      else {MSE<-as.data.frame(as.numeric(mtrMSE(cvprophet)))}}
    
    if (!is.null(cvthetaf)){
      colsn<-append(colsn,"thetaf")
      if(!is.null(MSE)){MSE<-cbind(MSE, as.data.frame(as.numeric(mtrMSE(cvthetaf))))}
      else {MSE<-as.data.frame(as.numeric(mtrMSE(cvthetaf)))}}
    
    MSE <- rbind(MSE, as.numeric(mtrMEAN(MSE)))
    MSE <- rbind(MSE, as.numeric(mtrSD(MSE)))
    MSE <- cbind(as.data.frame(mtrLINE(.h)), MSE)
    
    colnames(MSE) <- c('Type', colsn)
    
    return (MSE)
  }

# ------------------------------------------------------
# Build RRMSE Matrix
# ------------------------------------------------------
tabRMSE <-
  function(cvrwf,
           cvnaive,
           cvsnaive,
           cvbats,
           cvetsANN,
           cvetsMAM,
           cvetsZZZ,
           cvnnetar,
           cvautoarima,
           cvstruct,
           cvhw,
           cvprophet,
           cvthetaf,
           .h) {
    
    RMSE<-NULL
    colsn <- NULL
    
    if (!is.null(cvrwf)){
      colsn<-append(colsn,"rwf")
      if(!is.null(RMSE)){RMSE<-cbind(RMSE, as.data.frame(as.numeric(mtrRMSE(cvrwf))))}
      else {RMSE<-as.data.frame(as.numeric(mtrRMSE(cvrwf)))}}
    
    
    if (!is.null(cvnaive)){
      colsn<-append(colsn,"naive")
      if(!is.null(RMSE)){RMSE<-cbind(RMSE, as.data.frame(as.numeric(mtrRMSE(cvnaive))))}
      else {RMSE<-as.data.frame(as.numeric(mtrRMSE(cvnaive)))}}
    
    if (!is.null(cvsnaive)){
      colsn<-append(colsn,"snaive")
      if(!is.null(RMSE)){RMSE<-cbind(RMSE, as.data.frame(as.numeric(mtrRMSE(cvsnaive))))}
      else {RMSE<-as.data.frame(as.numeric(mtrRMSE(cvsnaive)))}}
    
    if (!is.null(cvbats)){
      colsn<-append(colsn,"bats")
      if(!is.null(RMSE)){RMSE<-cbind(RMSE, as.data.frame(as.numeric(mtrRMSE(cvbats))))}
      else {RMSE<-as.data.frame(as.numeric(mtrRMSE(cvbats)))}}
    
    if (!is.null(cvetsANN)){
      colsn<-append(colsn,"etsANN")
      if(!is.null(RMSE)){RMSE<-cbind(RMSE, as.data.frame(as.numeric(mtrRMSE(cvetsANN))))}
      else {RMSE<-as.data.frame(as.numeric(mtrRMSE(cvetsANN)))}}
    
    if (!is.null(cvetsMAM)){
      colsn<-append(colsn,"etsMAM")
      if(!is.null(RMSE)){RMSE<-cbind(RMSE, as.data.frame(as.numeric(mtrRMSE(cvetsMAM))))}
      else {RMSE<-as.data.frame(as.numeric(mtrRMSE(cvetsMAM)))}}
    
    if (!is.null(cvetsZZZ)){
      colsn<-append(colsn,"etsZZZ")
      if(!is.null(RMSE)){RMSE<-cbind(RMSE, as.data.frame(as.numeric(mtrRMSE(cvetsZZZ))))}
      else {RMSE<-as.data.frame(as.numeric(mtrRMSE(cvetsZZZ)))}}
    
    if (!is.null(cvnnetar)){
      colsn<-append(colsn,"nnetar")
      if(!is.null(RMSE)){RMSE<-cbind(RMSE, as.data.frame(as.numeric(mtrRMSE(cvnnetar))))}
      else {RMSE<-as.data.frame(as.numeric(mtrRMSE(cvnnetar)))}}
    
    if (!is.null(cvautoarima)){
      colsn<-append(colsn,"autoarima")
      if(!is.null(RMSE)){RMSE<-cbind(RMSE, as.data.frame(as.numeric(mtrRMSE(cvautoarima))))}
      else {RMSE<-as.data.frame(as.numeric(mtrRMSE(cvautoarima)))}}
    
    if (!is.null(cvstruct)){
      colsn<-append(colsn,"struct")
      if(!is.null(RMSE)){RMSE<-cbind(RMSE, as.data.frame(as.numeric(mtrRMSE(cvstruct))))}
      else {RMSE<-as.data.frame(as.numeric(mtrRMSE(cvstruct)))}}
    
    if (!is.null(cvhw)){
      colsn<-append(colsn,"hw")
      if(!is.null(RMSE)){RMSE<-cbind(RMSE, as.data.frame(as.numeric(mtrRMSE(cvhw))))}
      else {RMSE<-as.data.frame(as.numeric(mtrRMSE(cvhw)))}}
    
    if (!is.null(cvprophet)){
      colsn<-append(colsn,"prophet")
      if(!is.null(RMSE)){RMSE<-cbind(RMSE, as.data.frame(as.numeric(mtrRMSE(cvprophet))))}
      else {RMSE<-as.data.frame(as.numeric(mtrRMSE(cvprophet)))}}
    
    if (!is.null(cvthetaf)){
      colsn<-append(colsn,"thetaf")
      if(!is.null(RMSE)){RMSE<-cbind(RMSE, as.data.frame(as.numeric(mtrRMSE(cvthetaf))))}
      else {RMSE<-as.data.frame(as.numeric(mtrRMSE(cvthetaf)))}}
    
    RMSE <- rbind(RMSE, as.numeric(mtrMEAN(RMSE)))
    RMSE <- rbind(RMSE, as.numeric(mtrSD(RMSE)))
    RMSE <- cbind(as.data.frame(mtrLINE(.h)), RMSE)
    
    colnames(RMSE) <- c('Type', colsn)
    
    return (RMSE)
  }

# ------------------------------------------------------
# Build MAE Matrix
# ------------------------------------------------------
tabMAE <-
  function(cvrwf,
           cvnaive,
           cvsnaive,
           cvbats,
           cvetsANN,
           cvetsMAM,
           cvetsZZZ,
           cvnnetar,
           cvautoarima,
           cvstruct,
           cvhw,
           cvprophet,
           cvthetaf,
           .h) {
    
    MAE<-NULL
    colsn <- NULL
    
    if (!is.null(cvrwf)){
      colsn<-append(colsn,"rwf")
      if(!is.null(MAE)){MAE<-cbind(MAE, as.data.frame(as.numeric(mtrMAE(cvrwf))))}
      else {MAE<-as.data.frame(as.numeric(mtrMAE(cvrwf)))}}
    
    
    if (!is.null(cvnaive)){
      colsn<-append(colsn,"naive")
      if(!is.null(MAE)){MAE<-cbind(MAE, as.data.frame(as.numeric(mtrMAE(cvnaive))))}
      else {MAE<-as.data.frame(as.numeric(mtrMAE(cvnaive)))}}
    
    if (!is.null(cvsnaive)){
      colsn<-append(colsn,"snaive")
      if(!is.null(MAE)){MAE<-cbind(MAE, as.data.frame(as.numeric(mtrMAE(cvsnaive))))}
      else {MAE<-as.data.frame(as.numeric(mtrMAE(cvsnaive)))}}
    
    if (!is.null(cvbats)){
      colsn<-append(colsn,"bats")
      if(!is.null(MAE)){MAE<-cbind(MAE, as.data.frame(as.numeric(mtrMAE(cvbats))))}
      else {MAE<-as.data.frame(as.numeric(mtrMAE(cvbats)))}}
    
    if (!is.null(cvetsANN)){
      colsn<-append(colsn,"etsANN")
      if(!is.null(MAE)){MAE<-cbind(MAE, as.data.frame(as.numeric(mtrMAE(cvetsANN))))}
      else {MAE<-as.data.frame(as.numeric(mtrMAE(cvetsANN)))}}
    
    if (!is.null(cvetsMAM)){
      colsn<-append(colsn,"etsMAM")
      if(!is.null(MAE)){MAE<-cbind(MAE, as.data.frame(as.numeric(mtrMAE(cvetsMAM))))}
      else {MAE<-as.data.frame(as.numeric(mtrMAE(cvetsMAM)))}}
    
    if (!is.null(cvetsZZZ)){
      colsn<-append(colsn,"etsZZZ")
      if(!is.null(MAE)){MAE<-cbind(MAE, as.data.frame(as.numeric(mtrMAE(cvetsZZZ))))}
      else {MAE<-as.data.frame(as.numeric(mtrMAE(cvetsZZZ)))}}
    
    if (!is.null(cvnnetar)){
      colsn<-append(colsn,"nnetar")
      if(!is.null(MAE)){MAE<-cbind(MAE, as.data.frame(as.numeric(mtrMAE(cvnnetar))))}
      else {MAE<-as.data.frame(as.numeric(mtrMAE(cvnnetar)))}}
    
    if (!is.null(cvautoarima)){
      colsn<-append(colsn,"autoarima")
      if(!is.null(MAE)){MAE<-cbind(MAE, as.data.frame(as.numeric(mtrMAE(cvautoarima))))}
      else {MAE<-as.data.frame(as.numeric(mtrMAE(cvautoarima)))}}
    
    if (!is.null(cvstruct)){
      colsn<-append(colsn,"struct")
      if(!is.null(MAE)){MAE<-cbind(MAE, as.data.frame(as.numeric(mtrMAE(cvstruct))))}
      else {MAE<-as.data.frame(as.numeric(mtrMAE(cvstruct)))}}
    
    if (!is.null(cvhw)){
      colsn<-append(colsn,"hw")
      if(!is.null(MAE)){MAE<-cbind(MAE, as.data.frame(as.numeric(mtrMAE(cvhw))))}
      else {MAE<-as.data.frame(as.numeric(mtrMAE(cvhw)))}}
    
    if (!is.null(cvprophet)){
      colsn<-append(colsn,"prophet")
      if(!is.null(MAE)){MAE<-cbind(MAE, as.data.frame(as.numeric(mtrMAE(cvprophet))))}
      else {MAE<-as.data.frame(as.numeric(mtrMAE(cvprophet)))}}
    
    if (!is.null(cvthetaf)){
      colsn<-append(colsn,"thetaf")
      if(!is.null(MAE)){MAE<-cbind(MAE, as.data.frame(as.numeric(mtrMAE(cvthetaf))))}
      else {MAE<-as.data.frame(as.numeric(mtrMAE(cvthetaf)))}}
    
    
    MAE <- rbind(MAE, as.numeric(mtrMEAN(MAE)))
    MAE <- rbind(MAE, as.numeric(mtrSD(MAE)))
    MAE <- cbind(as.data.frame(mtrLINE(.h)), MAE)
    
    colnames(MAE) <- c('Type', colsn)
    
    return (MAE)
  }

# ------------------------------------------------------
# Build MAE Matrix
# ------------------------------------------------------
tabMAPE <-
  function(cvrwf,
           cvnaive,
           cvsnaive,
           cvbats,
           cvetsANN,
           cvetsMAM,
           cvetsZZZ,
           cvnnetar,
           cvautoarima,
           cvstruct,
           cvhw,
           cvprophet,
           cvthetaf,
           .h) {
    
    ai<-1
    af<-.h
    rf<-.h*3
    ri<-rf-.h+1
    
    MAPE<-NULL
    colsn <- NULL
    
    if (!is.null(cvrwf)){
      colsn<-append(colsn,"rwf")
      if(!is.null(MAPE)){MAPE<-cbind(MAPE, as.data.frame(as.numeric(mtrMAPE(cvrwf[,ri:rf],cvrwf[,ai:af]))))}
      else {MAPE<-as.data.frame(as.numeric(mtrMAPE(cvrwf[,ri:rf],cvrwf[,ai:af])))}}
    
    if (!is.null(cvnaive)){
      colsn<-append(colsn,"naive")
      if(!is.null(MAPE)){MAPE<-cbind(MAPE, as.data.frame(as.numeric(mtrMAPE(cvnaive[,ri:rf],cvnaive[,ai:af]))))}
      else {MAPE<-as.data.frame(as.numeric(mtrMAPE(cvnaive[,ri:rf],cvnaive[,ai:af])))}}
    
    if (!is.null(cvsnaive)){
      colsn<-append(colsn,"snaive")
      if(!is.null(MAPE)){MAPE<-cbind(MAPE, as.data.frame(as.numeric(mtrMAPE(cvsnaive[,ri:rf],cvsnaive[,ai:af]))))}
      else {MAPE<-as.data.frame(as.numeric(mtrMAPE(cvsnaive[,ri:rf],cvsnaive[,ai:af])))}}
    
    if (!is.null(cvbats)){
      colsn<-append(colsn,"bats")
      if(!is.null(MAPE)){MAPE<-cbind(MAPE, as.data.frame(as.numeric(mtrMAPE(cvbats[,ri:rf],cvbats[,ai:af]))))}
      else {MAPE<-as.data.frame(as.numeric(mtrMAPE(cvbats[,ri:rf],cvbats[,ai:af])))}}

    if (!is.null(cvetsANN)){
      colsn<-append(colsn,"etsANN")
      if(!is.null(MAPE)){MAPE<-cbind(MAPE, as.data.frame(as.numeric(mtrMAPE(cvetsANN[,ri:rf],cvetsANN[,ai:af]))))}
      else {MAPE<-as.data.frame(as.numeric(mtrMAPE(cvetsANN[,ri:rf],cvetsANN[,ai:af])))}}
    
    if (!is.null(cvetsMAM)){
      colsn<-append(colsn,"etsMAM")
      if(!is.null(MAPE)){MAPE<-cbind(MAPE, as.data.frame(as.numeric(mtrMAPE(cvetsMAM[,ri:rf],cvetsMAM[,ai:af]))))}
      else {MAPE<-as.data.frame(as.numeric(mtrMAPE(cvetsMAM[,ri:rf],cvetsMAM[,ai:af])))}}
    
    if (!is.null(cvetsZZZ)){
      colsn<-append(colsn,"etsZZZ")
      if(!is.null(MAPE)){MAPE<-cbind(MAPE, as.data.frame(as.numeric(mtrMAPE(cvetsZZZ[,ri:rf],cvetsZZZ[,ai:af]))))}
      else {MAPE<-as.data.frame(as.numeric(mtrMAPE(cvetsZZZ[,ri:rf],cvetsZZZ[,ai:af])))}}
    
    if (!is.null(cvnnetar)){
      colsn<-append(colsn,"nnetar")
      if(!is.null(MAPE)){MAPE<-cbind(MAPE, as.data.frame(as.numeric(mtrMAPE(cvnnetar[,ri:rf],cvnnetar[,ai:af]))))}
      else {MAPE<-as.data.frame(as.numeric(mtrMAPE(cvnnetar[,ri:rf],cvnnetar[,ai:af])))}}
    
    if (!is.null(cvautoarima)){
      colsn<-append(colsn,"autoarima")
      if(!is.null(MAPE)){MAPE<-cbind(MAPE, as.data.frame(as.numeric(mtrMAPE(cvautoarima[,ri:rf],cvautoarima[,ai:af]))))}
      else {MAPE<-as.data.frame(as.numeric(mtrMAPE(cvautoarima[,ri:rf],cvautoarima[,ai:af])))}}
    
    if (!is.null(cvstruct)){
      colsn<-append(colsn,"struct")
      if(!is.null(MAPE)){MAPE<-cbind(MAPE, as.data.frame(as.numeric(mtrMAPE(cvstruct[,ri:rf],cvstruct[,ai:af]))))}
      else {MAPE<-as.data.frame(as.numeric(mtrMAPE(cvstruct[,ri:rf],cvstruct[,ai:af])))}}
    
    if (!is.null(cvhw)){
      colsn<-append(colsn,"hw")
      if(!is.null(MAPE)){MAPE<-cbind(MAPE, as.data.frame(as.numeric(mtrMAPE(cvhw[,ri:rf],cvhw[,ai:af]))))}
      else {MAPE<-as.data.frame(as.numeric(mtrMAPE(cvhw[,ri:rf],cvhw[,ai:af])))}}
    
    if (!is.null(cvprophet)){
      colsn<-append(colsn,"prophet")
      if(!is.null(MAPE)){MAPE<-cbind(MAPE, as.data.frame(as.numeric(mtrMAPE(cvprophet[,ri:rf],cvprophet[,ai:af]))))}
      else {MAPE<-as.data.frame(as.numeric(mtrMAPE(cvprophet[,ri:rf],cvprophet[,ai:af])))}}
    
    if (!is.null(cvthetaf)){
      colsn<-append(colsn,"thetaf")
      if(!is.null(MAPE)){MAPE<-cbind(MAPE, as.data.frame(as.numeric(mtrMAPE(cvthetaf[,ri:rf],cvthetaf[,ai:af]))))}
      else {MAPE<-as.data.frame(as.numeric(mtrMAPE(cvthetaf[,ri:rf],cvthetaf[,ai:af])))}}
    
    MAPE <- rbind(MAPE, as.numeric(mtrMEAN(MAPE)))
    MAPE <- rbind(MAPE, as.numeric(mtrSD(MAPE)))
    MAPE <- cbind(as.data.frame(mtrLINE(.h)), MAPE)
    
    colnames(MAPE) <- c('Type', colsn)
    
    return (MAPE)
  }



# ------------------------------------------------------
# cv Error Plots
# ------------------------------------------------------
cvPlotErrors <-  function(cvErrorValues, .Title1, .Title2) {

  if (is.null(cvErrorValues))
    return(NULL)
  
  boxplotError <- ggplot(cvErrorValues, aes(x = Methods, y = Value)) +
    geom_boxplot(aes(color = Methods)) +
    geom_hline(yintercept = 0,
               linetype = "dashed",
               color = "red") +
    geom_jitter(aes(color = Methods)) +
    ggtitle(.Title1) +
    theme_bw()
  
  plotError <-
    ggplot(cvErrorValues, aes(x = CV, y = Value, group = Methods)) +
    geom_line(aes(color = Methods)) +
    geom_point(aes(color = Methods, shape = Methods)) +
    ggtitle(.Title2) +
    theme_bw()
  
  .plot<-(grid.arrange(boxplotError,
                plotError,
                nrow = 2,
                ncol = 1))

  return (.plot)
}

# ------------------------------------------------------
# Calculate forecast Model
# ------------------------------------------------------
forcastModel <- function(df,
                         forecastfunction,
                         h = 1,
                         vrs = NULL) {
  if (is.null(df))
    return (NULL)
  
  y <- df[, 1:2]
  y$date <- as.Date(y$date)
  y <- xts::xts(y$value, y$date)
  y <- as.ts(y)
  
  # Forecast test values based on tran data
  .r <- try(suppressWarnings
            (forecastfunction(y, h, vrs)),
            silent = TRUE)
  if (is.element("try-error", class(.r)))
    return(NULL)
  
  return(.r$mean)
}

forcastModelTS <- function(dt_ts,
                         forecastfunction,
                         h = 1,
                         vrs = NULL) {
  if (is.null(dt_ts))
    return (NULL)
  
  # Forecast test values based on tran data
  .r <- try(suppressWarnings
            (forecastfunction(dt_ts, h, vrs)),
            silent = TRUE)
  if (is.element("try-error", class(.r)))
    return(NULL)
  
  return(.r$mean)
}

# ------------------------------------------------------
# Calculate Interval forecast Model
# ------------------------------------------------------
IntervalforcastModel <- function(df,
                                 forecastfunction,
                                 h = 1,
                                 vrs = NULL) {
  if (is.null(df))
    return (NULL)
  
  y <- df[, 1:2]
  y$date <- as.Date(y$date)
  y <- xts::xts(y$value, y$date)
  y <- as.ts(y)
  
  # Forecast test values based on tran data
  .r <- try(suppressWarnings
            (forecastfunction(y, h, vrs)),
            silent = TRUE)
  if (is.element("try-error", class(.r)))
    return(NULL)
  
  return(cbind(.r$mean,.r$upper,.r$lower))
}

IntervalforcastModelTS <- function(dt_ts,
                           forecastfunction,
                           h = 1,
                           vrs = NULL) {
  if (is.null(dt_ts))
    return (NULL)
  
  # Forecast test values based on tran data
  .r <- try(suppressWarnings
            (forecastfunction(dt_ts, h, vrs)),
            silent = TRUE)
  if (is.element("try-error", class(.r)))
    return(NULL)
  
  return(cbind(.r$mean,.r$upper,.r$lower))
}


# ------------------------------------------------------
# Calculate forecast Values
# ------------------------------------------------------
forecastValues <- function(df,
                           models,
                           h = 1,
                           dataType="month")
{
  if (is.null(df) | is.null(models))
    return (NULL)
  
  startDate<-as.Date(max(df[,1]))
  dates <- seq(startDate, by = dataType, length.out = h+1)
  dates <- dates[2:(h+1)]
  
  
  .results<-cbind(model="ACTUAL",date=as.Date(as.numeric(df[,1])),value=as.numeric(df[,2]))
  prophetForecast<-forcastModel(df,prophet,h)
  #showModal(modalDialog("Forecasting - NNETAR", footer=NULL))
  nnetarForecast<-forcastModel(df,mnnetar,h)
  if (!is.null(nnetarForecast) & 'NNETAR' %in% models) {.results<-rbind(.results,cbind(model="nnetar",date=as.Date(dates),value=nnetarForecast))}
  #showModal(modalDialog("Forecasting -ETS ANNr", footer=NULL))
  etsANNForecast<-forcastModel(df,mets,h, vrs="ANN")
  if (!is.null(etsANNForecast) & 'ETS ANN' %in% models) {.results<-rbind(.results,cbind(model="etsANN",date=as.Date(dates),value=etsANNForecast))}
  #showModal(modalDialog("Forecasting - ETS MAM", footer=NULL))
  etsMAMForecast<-forcastModel(df,mets,h, vrs="MAM")
  if (!is.null(etsMAMForecast) & 'ETS MAM' %in% models) {.results<-rbind(.results,cbind(model="etsMAM",date=as.Date(dates),value=etsMAMForecast))}
  #showModal(modalDialog("Forecasting - ETS ZZZ", footer=NULL))
  etsZZZForecast<-forcastModel(df,mets,h, vrs="ZZZ")
  if (!is.null(etsZZZForecast)) {.results<-rbind(.results,cbind(model="etsZZZ",date=as.Date(dates),value=etsZZZForecast))}
  #showModal(modalDialog("Forecasting - BATS", footer=NULL))
  batsForecast<-forcastModel(df,mbats,h)
  if (!is.null(batsForecast)) {.results<-rbind(.results,cbind(model="BATS",date=as.Date(dates),value=batsForecast))}
  #showModal(modalDialog("Forecasting - Auto Arima", footer=NULL))
  autoarimaForecast<-forcastModel(df,mautoarima,h)
  if (!is.null(autoarimaForecast)) {.results<-rbind(.results,cbind(model="Auto.Arima",date=as.Date(dates),value=autoarimaForecast))}
  #showModal(modalDialog("Forecasting - Struct", footer=NULL))
  structForecast<-forcastModel(df,mstruct,h)
  if (!is.null(structForecast)) {.results<-rbind(.results,cbind(model="Struct",date=as.Date(dates),value=structForecast))}
  #showModal(modalDialog("Forecasting - Holt Winters", footer=NULL))
  hwForecast<-forcastModel(df,mhw,h)
  if (!is.null(hwForecast)) {.results<-rbind(.results,cbind(model="HoltWinters",date=as.Date(dates),value=hwForecast))}
  #showModal(modalDialog("Forecasting - NAIVE", footer=NULL))
  naiveForecast<-forcastModel(df,mnaive,h)
  if (!is.null(naiveForecast)) {.results<-rbind(.results,cbind(model="NAIVE",date=as.Date(dates),value=naiveForecast))}
  #showModal(modalDialog("Forecasting -SNAIVE", footer=NULL))
  snaiveForecast<-forcastModel(df,msnaive,h)
  if (!is.null(snaiveForecast)) {.results<-rbind(.results,cbind(model="SNAIVE",date=as.Date(dates),value=snaiveForecast))}
  #showModal(modalDialog("Forecasting - RWF", footer=NULL))
  rwfForecast<-forcastModel(df,mrwf,h)
  if (!is.null(rwfForecast)) {.results<-rbind(.results,cbind(model="RWF",date=as.Date(dates),value=rwfForecast))}
  thetafForecast<-forcastModel(df,mthetaf,h)
  if (!is.null(thetafForecast)) {.results<-rbind(.results,cbind(model="THETAF",date=as.Date(dates),value=rwfForecast))}
  
  return(.results)
}

# ------------------------------------------------------
# Data Series - Plot
# ------------------------------------------------------
plotforecValues <- function(df)
{
  if (is.null(df))
    return(NULL)
  df<-data_frame(model=df[,1],date=lubridate::as_date(as.numeric(df[,2])),value=as.numeric(df[,3]))
  .Plot<-ggplot(df, aes(x=date,y=value,group=model)) +
    geom_line(aes(color=model,linetype=model)) +
    ggtitle("Forecast Values - Multiple Models ") +
    theme_bw()
  
  return(.Plot)
}

forecastTabValues <- function(df,
                              models,
                              h = 1,
                              dataType="month")
{
  if (is.null(df) | is.null(models))
    return (NULL)
  
  startDate<-as.Date(max(df[,1]))
  dates <- seq(startDate, by = dataType, length.out = h+1)
  dates <- dates[2:(h+1)]
  
  .cols<-c('date','actual')
  .actuals <- as.matrix(rep(0,times=length(df[,2])))
  .dates <- cbind(date=dates,actual=0)
  .results<-cbind(date=lubridate::as_date(as.numeric(df[,1])),actual=as.numeric(df[,2]))
  .results<-rbind(.results,.dates)
  
  dt_ts<-convertDataReceivedTS(df,dataType,2)
  
  #prophetForecast<-mprophet(df,h)
  
  if ('PROPHET' %in% models){
    prophetForecast <- mprophet(df, h)
    if (!is.null(prophetForecast)){
      prophetForecast<-round(prophetForecast,3)
      .prophet <- rbind(.actuals, as.matrix(prophetForecast))
      .cols <- append(.cols, 'prophet')
      .results <- cbind(.results, prophet = .prophet)
    }
  }
  
  if ('NNETAR' %in% models){
    nnetarForecast <- forcastModel(df, mnnetar, h)
    if (!is.null(nnetarForecast)){
      nnetarForecast<-round(nnetarForecast,3)
      .nnetar <- rbind(.actuals, as.matrix(nnetarForecast))
      .cols <- append(.cols, 'nnetar')
      .results <- cbind(.results, nnetar = .nnetar)
    }
  }
  
  if ('ETS ANN' %in% models){
    etsANNForecast <- forcastModelTS(dt_ts, mets, h, vrs = "ANN")
    if (!is.null(etsANNForecast)){
      etsANNForecast<-round(etsANNForecast,3)
      .etsANN <- rbind(.actuals, as.matrix(etsANNForecast))
      .cols <- append(.cols, 'etsANN')
      .results <- cbind(.results, etsANN = .etsANN)
    }
  }
  
  if ('ETS MAM' %in% models){
    etsMAMForecast <- forcastModelTS(dt_ts, mets, h, vrs = "MAM")
    if (!is.null(etsMAMForecast)){
      etsMAMForecast<-round(etsMAMForecast,3)
      .etsMAM <- rbind(.actuals, as.matrix(etsMAMForecast))
      .cols <- append(.cols, 'etsMAM')
      .results <- cbind(.results, etsMAM = .etsMAM)
    }
  }

  if ('ETS ZZZ' %in% models){
    etsZZZForecast <- forcastModelTS(dt_ts, mets, h, vrs = "ZZZ")
    if (!is.null(etsZZZForecast)){
      etsZZZForecast<-round(etsZZZForecast,3)
      .etsZZZ <- rbind(.actuals, as.matrix(etsZZZForecast))
      .cols <- append(.cols, 'etsZZZ')
      .results <- cbind(.results, etsZZZ = .etsZZZ)
    }
  }

  if ('BATS' %in% models) {
    batsForecast <- forcastModel(df, mbats, h)
    if (!is.null(batsForecast)) {
      batsForecast<-round(batsForecast,3)
      .bats <- rbind(.actuals, as.matrix(batsForecast))
      .cols <- append(.cols, 'BATS')
      .results <- cbind(.results, bats = .bats)
    }
  }
  
  if ('Auto Arima' %in% models) {
    autoarimaForecast <- forcastModelTS(dt_ts, mautoarima, h)
    if (!is.null(autoarimaForecast)) {
      autoarimaForecast<-round(autoarimaForecast,3)
      .autoarima <- rbind(.actuals, as.matrix(autoarimaForecast))
      .cols <- append(.cols, 'autoarima')
      .results <- cbind(.results, bats = .autoarima)
    }
  }
  
  if ('Struct' %in% models) {
    structForecast <- forcastModelTS(dt_ts, mstruct, h)
    if (!is.null(structForecast)) {
      structForecast<-round(structForecast,3)
      .struct <- rbind(.actuals, as.matrix(structForecast))
      .cols <- append(.cols, 'struct')
      .results <- cbind(.results, struct = .struct)
    }
  }
  
  if ('HW' %in% models) {
    hwForecast <- forcastModelTS(dt_ts, mhw, h)
    if (!is.null(hwForecast)) {
      hwForecast<-round(hwForecast,3)
      .hw <- rbind(.actuals, as.matrix(hwForecast))
      .cols <- append(.cols, 'hw')
      .results <- cbind(.results, struct = .hw)
    }
  }
  
  if ('NAIVE' %in% models) {
    naiveForecast <- forcastModelTS(dt_ts, mnaive, h)
    if (!is.null(naiveForecast)) {
      naiveForecast<-round(naiveForecast,3)
      .naive <- rbind(.actuals, as.matrix(naiveForecast))
      .cols <- append(.cols, 'naive')
      .results <- cbind(.results, struct = .naive)
    }
  }
  
  if ('SNAIVE' %in% models) {
    snaiveForecast <- forcastModelTS(dt_ts, msnaive, h)
    if (!is.null(snaiveForecast)) {
      snaiveForecast<-round(snaiveForecast,3)
      .snaive <- rbind(.actuals, as.matrix(snaiveForecast))
      .cols <- append(.cols, 'snaive')
      .results <- cbind(.results, struct = .snaive)
    }
  }
  
  if ('RWF' %in% models) {
    rwfForecast <- forcastModelTS(dt_ts, mrwf, h)
    if (!is.null(rwfForecast)) {
      rwfForecast<-round(rwfForecast,3)
      .rwf <- rbind(.actuals, as.matrix(rwfForecast))
      .cols <- append(.cols, 'rwf')
      .results <- cbind(.results, struct = .rwf)
    }
  }
  
  if ('THETAF' %in% models) {
    thetafForecast <- forcastModel(df, mthetaf, h)
    if (!is.null(thetafForecast)) {
      thetafForecast<-round(thetafForecast,3)
      .thetaf <- rbind(.actuals, as.matrix(thetafForecast))
      .cols <- append(.cols, 'thetaf')
      .results <- cbind(.results, struct = .thetaf)
    }
  }
  
  colnames(.results)<-.cols
  
  return(.results)
}


dygraphforecValues <- function(df)
{
  if (is.null(df))
    return(NULL)
  df[,2:3]<-df[,2:3] %>% dplyr::na_if(0)
  .cols<-length(df[1,])
  df[,1]<-as.character.Date(lubridate::as_date(as.numeric(df[,1])))
  ts<-xts::xts(df[,2:.cols],order.by = as.Date(df[,1]))
  .Plot<-dygraph(ts) %>% dyRangeSelector %>% dyLegend(width = 1000 , show = "follow", hideOnMouseOut = TRUE )
  
  return(.Plot)
}

rankingErrorsValues <- function(df,rnk)
{
  if (is.null(df))
    return(NULL)

  if (is.null(rnk))
    rnk<-3
  
  df[,3]<-sqrt(df[,3]^2)
  .cl<-0
  .cv<-df %>% group_by(CV) %>% summarise(n())
  .nCV<-as.numeric(count(.cv))
  .cv<-.cv[1]
  .mat<-matrix(ncol=.nCV,nrow=rnk)
  rownames(.mat)<-seq(from=1,to=rnk)
  colnames(.mat)<-colNamesFunc("FOREC-",seq(from=1,to=.nCV))
  .CV<-data.frame(.cv)
  
  for (i in seq(from=1,to=.nCV))
  {
    .cl<-.cl+1
    .ref<-.CV[.cl,]
    .vals<-df %>% dplyr::arrange(CV,Value,by_group=FALSE)%>%dplyr::filter(!is.na(Value))%>%dplyr::filter(CV==.ref)
    for (r in seq(from=1,to=rnk))
    {
      .mat[r,i]<-.vals[r,1]
    }
  }
  
  return(.mat)
}

IntervalforecastTabValues <- function(df,
                              models,
                              h = 1,
                              dataType="month")
{
  if (is.null(df) | is.null(models))
    return (NULL)
  
  startDate<-as.Date(max(df[,1]))
  dates <- seq(startDate, by = dataType, length.out = h+1)
  dates <- dates[2:(h+1)]
  
  .cols<-c('date','actual','forecast','upper80','upper95','lower80','lower95')
  .vals<-rep(0,times=length(df[,2]))
  .actuals <- as.matrix(cbind(.vals,.vals,.vals,.vals,.vals))
  .dates <- cbind(date=dates,actual=0)
  .results<-cbind(date=lubridate::as_date(as.numeric(df[,1])),actual=as.numeric(df[,2]))
  .results<-rbind(.results,.dates)
  
  dt_ts<-convertDataReceivedTS(df,dataType,2)
  
  if ('ETS ANN' %in% models){
    etsANNForecast <- IntervalforcastModelTS(dt_ts, mets, h, vrs = "ANN")
    if (!is.null(etsANNForecast)){
      etsANNForecast<-round(etsANNForecast,3)
      .etsANN <- rbind(.actuals, as.matrix(etsANNForecast))
      .results <- cbind(.results, etsANN = .etsANN)
    }
  }
  
  if ('ETS MAM' %in% models){
    etsMAMForecast <- IntervalforcastModelTS(dt_ts, mets, h, vrs = "MAM")
    if (!is.null(etsMAMForecast)){
      etsMAMForecast<-round(etsMAMForecast,3)
      .etsMAM <- rbind(.actuals, as.matrix(etsMAMForecast))
      .results <- cbind(.results, etsMAM = .etsMAM)
    }
  }
  
  if ('ETS ZZZ' %in% models){
    etsZZZForecast <- IntervalforcastModelTS(dt_ts, mets, h, vrs = "ZZZ")
    if (!is.null(etsZZZForecast)){
      etsZZZForecast<-round(etsZZZForecast,3)
      .etsZZZ <- rbind(.actuals, as.matrix(etsZZZForecast))
      .results <- cbind(.results, etsZZZ = .etsZZZ)
    }
  }
  
  if ('BATS' %in% models) {
    batsForecast <- IntervalforcastModel(df, mbats, h)
    if (!is.null(batsForecast)) {
      batsForecast<-round(batsForecast,3)
      .bats <- rbind(.actuals, as.matrix(batsForecast))
      .results <- cbind(.results, bats = .bats)
    }
  }
  
  if ('Auto Arima' %in% models) {
    autoarimaForecast <- IntervalforcastModelTS(dt_ts, mautoarima, h)
    if (!is.null(autoarimaForecast)) {
      autoarimaForecast<-round(autoarimaForecast,3)
      .autoarima <- rbind(.actuals, as.matrix(autoarimaForecast))
      .results <- cbind(.results, bats = .autoarima)
    }
  }
  
  if ('Struct' %in% models) {
    structForecast <- IntervalforcastModelTS(dt_ts, mstruct, h)
    if (!is.null(structForecast)) {
      structForecast<-round(structForecast,3)
      .struct <- rbind(.actuals, as.matrix(structForecast))
      .results <- cbind(.results, struct = .struct)
    }
  }
  
  if ('HW' %in% models) {
    hwForecast <- IntervalforcastModelTS(dt_ts, mhw, h)
    if (!is.null(hwForecast)) {
      hwForecast<-round(hwForecast,3)
      .hw <- rbind(.actuals, as.matrix(hwForecast))
      .results <- cbind(.results, struct = .hw)
    }
  }
  
  if ('NAIVE' %in% models) {
    naiveForecast <- IntervalforcastModelTS(dt_ts, mnaive, h)
    if (!is.null(naiveForecast)) {
      naiveForecast<-round(naiveForecast,3)
      .naive <- rbind(.actuals, as.matrix(naiveForecast))
      .results <- cbind(.results, struct = .naive)
    }
  }
  
  if ('SNAIVE' %in% models) {
    snaiveForecast <- IntervalforcastModelTS(dt_ts, msnaive, h)
    if (!is.null(snaiveForecast)) {
      snaiveForecast<-round(snaiveForecast,3)
      .snaive <- rbind(.actuals, as.matrix(snaiveForecast))
      .results <- cbind(.results, struct = .snaive)
    }
  }
  
  if ('RWF' %in% models) {
    rwfForecast <- IntervalforcastModelTS(dt_ts, mrwf, h)
    if (!is.null(rwfForecast)) {
      rwfForecast<-round(rwfForecast,3)
      .rwf <- rbind(.actuals, as.matrix(rwfForecast))
      .results <- cbind(.results, struct = .rwf)
    }
  }
  
  if ('THETAF' %in% models) {
    thetafForecast <- IntervalforcastModel(df, mthetaf, h)
    if (!is.null(thetafForecast)) {
      thetafForecast<-round(thetafForecast,3)
      .thetaf <- rbind(.actuals, as.matrix(thetafForecast))
      .results <- cbind(.results, struct = .thetaf)
    }
  }
  
  colnames(.results)<-.cols
  
  return(.results)
}

dygraphIntervalForecValues <- function(df)
{
  if (is.null(df))
    return(NULL)
  df<-df %>% dplyr::na_if(0)
  .cols<-length(df[1,])
  df[,1]<-as.character.Date(lubridate::as_date(as.numeric(df[,1])))
  ts<-xts::xts(df[,2:.cols],order.by = as.Date(df[,1]))
  .Plot<-dygraph(ts, main = "Interval Forecasting") %>%
         dySeries(c("lower95","forecast", "upper95"), color = 'blue') %>%
         dySeries(c("lower80","forecast", "upper80"), color = 'green') %>%
         dyRangeSelector
  
  return(.Plot)
}
