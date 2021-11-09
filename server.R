library(shiny)
library(shinydisconnect)
library(ggplot2)
library(timetk)
library(tsbox)
library(plotly)
library(parsnip)
library(generics)
library(rsample)

## load functions
source('dataLoad.R')
source('dataExploration.R')
source('crossValidation.R')

shinyServer(
  function(input,output,session){
    rv <- reactiveValues(tituloGrafico = NULL)
    rv$nomeFicheiro=NULL
    rv$dadosFicheiro=NULL
    rv$dataOriginal=NULL
    rv$dataForecast=NULL
    rv$dataModelsResiduals=NULL #data.frame(Model="",Date="yyyy-mm-dd",value=0000.000)
    rv$dataResiduals=NULL
    rv$accuracy=NULL
    # rv$modelAccuracy<-data.frame(Optimization="",Models="",Weights="",ME="",RMSE="",MAE="",MPE="",MAPE="",ACF1="",TheilsU="")
    rv$train_values <- NULL
    rv$teste_values <- NULL
    rv$pred_values <- NULL
    rv$forec<-NULL
    
    rv$dadosAnalise<-NULL
    rv$individualModels<-NULL
    rv$individualAccuracy<-NULL
    rv$DieboldMariano<-NULL
    rv$ResidualsPlots<-NULL
    rv$Forecast<-NULL
    rv$ForecastValues<-NULL
    rv$IntervalForecast<-NULL
    rv$IntervalForecastValues<-NULL
    
    rv$cv_actual<-NULL
    rv$cv_errors<-NULL
    rv$cv_forecast<-NULL
    rv$cv_cvnnetar<-NULL
    rv$cv_cvetsANN<-NULL
    rv$cv_cvetsMAM<-NULL
    rv$cv_cvetsZZZ<-NULL
    rv$cv_cvbats<-NULL
    rv$cv_cvautoarima<-NULL
    rv$cv_cvstruct<-NULL
    rv$cv_cvhw<-NULL
    rv$cv_cvnaive<-NULL
    rv$cv_cvsnaive<-NULL
    rv$cv_cvrwf<-NULL
    rv$cv_cvprophet<-NULL
    rv$cv_cvthetaf<-NULL
    
    rv$cvmtrME <- NULL
    rv$cvmtrME2 <- NULL
    rv$cvmtrMSE <- NULL
    rv$cvmtrMSE2 <- NULL
    rv$cvmtrMAE <- NULL
    rv$cvmtrMAE2 <- NULL
    
    rv$imputedData<-NULL
    rv$plotData<-NULL

    rv$dfErrors <- NULL
    rv$Stationary <- NULL
    rv$Frequency <- NULL
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --- -
    # Disconnect event ####
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --- -
    observeEvent(input$disconnect,
                 {session$close()})
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --- -
    # Load File event ####
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --- -
    observeEvent(input$loadFileButton,
                 {
                   #Clean all variables contents
                   rv$nomeFicheiro<-input$ficheiro
                   rv$dadosFicheiro=NULL
                   rv$dataOriginal=NULL
                   rv$dataForecast=NULL
                   rv$dataModelsResiduals=NULL
                   rv$dataResiduals=NULL
                   rv$accuracy=NULL
                   rv$train_values <- NULL
                   rv$teste_values <- NULL
                   rv$pred_values <- NULL
                   rv$forec<-NULL
                   rv$dadosAnalise<-NULL
                   rv$individualModels<-NULL
                   rv$individualAccuracy<-NULL
                   rv$DieboldMariano<-NULL
                   rv$ResidualsPlots<-NULL
                   
                   #Load New Data
                   rv$Frequency<-12
                   if (input$dateType=="day") {rv$Frequency<-365}
                   if (input$dateType=="week") {rv$Frequency<-52}
                   if (input$dateType=="month") {rv$Frequency<-12}
                   if (input$dateType=="quarter") {rv$Frequency<-4}
                   if (input$dateType=="semester") {rv$Frequency<-2}
                   if (input$dateType=="year") {rv$Frequency<-1}
                   
                   
                   rv$dadosFicheiro<-dataLoad(input$ficheiro$datapath, input$cabecalhos, input$separador)
                   #rv$dadosAnalise<-rv$dadosFicheiro
                   tabs<-c("CVDefinition","ErrorAnalisysPlots","ErrorPlots","ErrorAnalysis","ForecastDefinition",
                           "Anomalies",
                           "STLAditional","STLResidualsAnalisys","Features-1","Features-2","Features-3",
                           "OriginalSerie","SeriesPeriods","MonthlyAnalisys","QuarterlyAnalisys","Half-YearAnalisys",
                           "YearlyAnalisys","YearlyDataSummary","DataEvolutions")
                   for (tbs in tabs)
                     hideTab(inputId = "tbs", target = tbs )
                 })
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --- -
    # Adjust Time Series event ####
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --- -
    observeEvent(input$df2_Button,
                 {
                   showModal(modalDialog("Running process", footer=NULL))
                   # Try
                   try({
                   rv$dadosFicheiro<-dataLoad(input$ficheiro$datapath, input$cabecalhos, input$separador)
                   rv$dadosFicheiro<-dataSetBuild(rv$dadosFicheiro,
                                                  input$colDateNumber,
                                                  input$colValueNumber,
                                                  input$dateFormat)
                   if (!is.null(rv$dadosFicheiro)){
                     rv$dadosFicheiro<-convertDataReceivedDF(rv$dadosFicheiro,input$dateType,input$colValueNumber)
                     
                     rv$imputedData<-miceData(rv$dadosFicheiro,input$dateType)
                     rv$plotData<-cbind(rv$dadosFicheiro,rv$imputedData)
                     colnames(rv$plotData)<-c('date','actual','imputed')
                     
                     rv$dadosAnalise<-rv$dadosFicheiro
                     rv$dataOriginal<-rv$dadosFicheiro
                     dados_ts<-convertDataReceivedTS(rv$dadosFicheiro,input$dateType,input$colValueNumber)
                     if (checkStationary(dados_ts))
                     {rv$Stationary<-"Stationary data"}
                     else 
                     {rv$Stationary<-"Non-Stationary data"}
                     .nRows<-count(rv$dataOriginal)
                     
                     #Diaria
                     if (input$dateType=="day")
                     {
                       .Treino<-14
                       .Teste<-7
                       .CVM<-7
                       .Total<-.Treino+.Teste+.CVM-1
                       if(.nRows>167)
                       {
                         .Treino<-140
                         .Teste<-14
                         .CVM<-14
                         .Total<-.Treino+.Teste+.CVM-1
                       }
                       else
                       {
                         .Treino<-.nRows-.Teste-.CVM+1
                         .Total<-.Treino+.Teste+.CVM-1
                       }
                     }
                     
                     #Mensal
                     if (input$dateType=="month")
                     {
                       .Treino<-24
                       .Teste<-12
                       .CVM<-12
                       .Total<-.Treino+.Teste+.CVM-1
                       if(.nRows>143)
                       {
                         .Treino<-120
                         .Total<-.Treino+.Teste+.CVM-1
                       }
                       else
                       {
                         .Treino<-.nRows-.Teste-.CVM+1
                         .Total<-.Treino+.Teste+.CVM-1
                       }
                     }
                     #Anual
                     if (input$dateType=="year")
                     {
                       .Treino<-10
                       .Teste<-5
                       .CVM<-10
                       .Total<-.Treino+.Teste+.CVM-1
                       if(.nRows>119)
                       {
                         .Treino<-100
                         .Teste<-trunc(.Treino/3)
                         .Total<-.Treino+.Teste+.CVM-1
                       }
                       else
                       {
                         .Treino<-.nRows-.Teste-.CVM+1
                         .Teste<-trunc(.Treino/3)
                         .Total<-.Treino+.Teste+.CVM-1
                       }
                       
                     }
                     if(.nRows<.Total)
                     {
                       .Teste<-(round((.nRows-.CVM)*0.3,0))
                       .Treino<-.nRows-.Teste
                       .Total<-.Treino+.Teste+.CVM-1
                     }

                     .nTrain<-.Treino
                     .nTest<-.Teste
                     updateSliderInput(session, "cvInitial", value = as.numeric(as.numeric(.nRows)-.Total),min = as.numeric(as.numeric(.nRows)-.Total), max = as.numeric(as.numeric(.nRows)-1), step = 1)
                     updateSliderInput(session, "cvH", value = as.numeric(.nTest),min = 1, max = 3*as.numeric(.nTest), step = 1)
                     updateSliderInput(session, "cvWindow", value = as.numeric(.nTrain),min = 1, max = 3*as.numeric(.nTrain), step = 1)
                     updateSliderInput(session, "cvStep", value = 1,min = 1, max = as.numeric(.nTest), step = 1)
                     updateSliderInput(session, "cvMax", value = as.numeric(.CVM),min = 1, max = 4*as.numeric(.CVM), step = 1)
                     
                     tabs<-c("CVDefinition","ForecastDefinition","Anomalies",
                            "STLAditional","STLResidualsAnalisys","Features-1","Features-2","Features-3",
                            "OriginalSerie","SeriesPeriods","MonthlyAnalisys","QuarterlyAnalisys","Half-YearAnalisys",
                            "YearlyAnalisys","YearlyDataSummary","DataEvolutions")
                     
                     if (input$dateType=="year")
                     {tabs<-c("CVDefinition","ForecastDefinition","Anomalies",
                              "STLAditional","STLResidualsAnalisys","Features-1","Features-2","Features-3",
                              "OriginalSerie","YearlyAnalisys","YearlyDataSummary","DataEvolutions")
                     }
                       
                     for (tbs in tabs)
                       showTab(inputId = "tbs", target = tbs )
                   }
                   }, silent = TRUE)
                     
                   removeModal()
                   
                   
                 })
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --- -
    # Imputation Methods ####
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --- -
    observeEvent(input$imp_Button,
                 {
                   showModal(modalDialog("Running process", footer=NULL))
                   rv$dadosFicheiro[,2]<-rv$imputedData
                   rv$dadosAnalise<-rv$dadosFicheiro
                   rv$dataOriginal<-rv$dadosFicheiro
                   dados_ts<-convertDataReceivedTS(rv$dadosFicheiro,input$dateType,input$colValueNumber)
                   if (checkStationary(dados_ts))
                   {rv$Stationary<-"Stationary data"}
                   else 
                   {rv$Stationary<-"Non-Stationary data"}
                   removeModal()
                 })
    
    observeEvent(input$imp_Button2,
                 {
                   showModal(modalDialog("Running process", footer=NULL))
                   rv$plotData<-NULL
                   rv$imputedData<-NULL
                   if (input$imputationMethod=="mice" & !is.null(rv$dadosFicheiro))
                     rv$imputedData<-miceData(rv$dadosFicheiro,input$dateType)
                   if (input$imputationMethod=="linear.interp" & !is.null(rv$dadosFicheiro))
                     rv$imputedData<-naInterpData(rv$dadosFicheiro,"linear",rv$Frequency)
                   if (input$imputationMethod=="spline.interp" & !is.null(rv$dadosFicheiro))
                     rv$imputedData<-naInterpData(rv$dadosFicheiro,"spline",rv$Frequency)
                   if (input$imputationMethod=="stine.interp" & !is.null(rv$dadosFicheiro))
                     rv$imputedData<-naInterpData(rv$dadosFicheiro,"stine",rv$Frequency)
                   if (input$imputationMethod=="na.struct" & !is.null(rv$dadosFicheiro))
                     rv$imputedData<-naStructTSData(rv$dadosFicheiro,rv$Frequency)
                   if (input$imputationMethod=="kalman.1" & !is.null(rv$dadosFicheiro))
                     rv$imputedData<-naKalmanData(rv$dadosFicheiro,'StructTS','FALSE',rv$Frequency)
                   if (input$imputationMethod=="kalman.2" & !is.null(rv$dadosFicheiro))
                     rv$imputedData<-naKalmanData(rv$dadosFicheiro,'StructTS','TRUE',rv$Frequency)
                   if (input$imputationMethod=="kalman.3" & !is.null(rv$dadosFicheiro))
                     rv$imputedData<-naKalmanData(rv$dadosFicheiro,'SpaceModels','TRUE',rv$Frequency)
                   if (input$imputationMethod=="seadec.1" & !is.null(rv$dadosFicheiro))
                     rv$imputedData<-naSeadecData(rv$dadosFicheiro,'interpolation',rv$Frequency)
                   if (input$imputationMethod=="seadec.2" & !is.null(rv$dadosFicheiro))
                     rv$imputedData<-naSeadecData(rv$dadosFicheiro,'mean',rv$Frequency)
                   if (input$imputationMethod=="seadec.3" & !is.null(rv$dadosFicheiro))
                     rv$imputedData<-naSeadecData(rv$dadosFicheiro,'kalman',rv$Frequency)
                   
                   rv$plotData<-cbind(rv$dadosFicheiro,rv$imputedData)
                   colnames(rv$plotData)<-c('date','actual','imputed')
                   #View(rv$plotData[3])
                   removeModal()
                 })
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --- -
    # Differencing Methods ####
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --- -
    observeEvent(input$diff_tst_Button,
                 {
                   showModal(modalDialog("Running process", footer=NULL))
                   dados_ts<-convertDataReceivedTS(rv$dadosFicheiro,input$dateType,input$colValueNumber)
                   dados_ts<-diff(dados_ts,differences = input$diffLag)
                   rv$dadosAnalise<-ts_df(dados_ts)
                   colnames(rv$dadosAnalise)<-c('date','value')
                   if (checkStationary(dados_ts))
                   {rv$Stationary<-"Stationary data"}
                   else 
                   {rv$Stationary<-"Non-Stationary data"}
                   removeModal()
                 })
    
    observeEvent(input$diff_apply_Button,
                 {
                   showModal(modalDialog("Running process", footer=NULL))
                   dados_ts<-convertDataReceivedTS(rv$dadosFicheiro,input$dateType,input$colValueNumber)
                   dados_ts<-diff(dados_ts,differences = input$diffLag)
                   rv$dadosAnalise<-ts_df(dados_ts)
                   colnames(rv$dadosAnalise)<-c('date','value')
                   rv$dataOriginal<-rv$dadosAnalise
                   removeModal()
                 })
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --- -
    # Calculate CV ####
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --- -
    observeEvent(input$calc_CV,
                 {
                   showModal(modalDialog("Running process", footer=NULL))
                   
                   if (!is.null(rv$cv_actual))
                   {
                     # Array return Actual + Forecast + Residuals
                     # ci + cf are the REsiduals columns
                     cf<-3*input$cvH
                     ci<-cf-input$cvH+1
                     
                     df_result<-rv$cv_actual
                     rv$cv_cvprophet<-NULL
                     rv$cv_cvthetaf<-NULL
                     rv$cv_cvnnetar<-NULL
                     rv$cv_cvetsANN<-NULL
                     rv$cv_cvetsMAM<-NULL
                     rv$cv_cvetsZZZ<-NULL
                     rv$cv_cvbats<-NULL
                     rv$cv_cvautoarima<-NULL
                     rv$cv_cvstruct<-NULL
                     rv$cv_cvhw<-NULL
                     rv$cv_cvnaive<-NULL
                     rv$cv_cvsnaive<-NULL
                     rv$cv_cvrwf<-NULL
                     
                     if ("PROPHET" %in% input$cvModels){
                       showModal(modalDialog("Running - PROPHET", footer = NULL))
                       rv$cv_cvprophet <-cvResultsProphet(df_result, input$cvH)}
                     
                     if ("THETAF" %in% input$cvModels){
                       showModal(modalDialog("Running - THETAF", footer =NULL))
                       rv$cv_cvthetaf <-cvResults(df_result, mthetaf, input$cvH)}
                     
                     if ("NNETAR" %in% input$cvModels){
                       showModal(modalDialog("Running - NNETAR", footer=NULL))
                       rv$cv_cvnnetar<-cvResults(df_result,mnnetar,input$cvH)}
                     
                     if ("ETS ANN" %in% input$cvModels){
                       showModal(modalDialog("Running - SES", footer=NULL))
                       rv$cv_cvetsANN<-cvResults(df_result,mets,input$cvH, vrs="ANN")}
                     
                     if ("ETS MAM" %in% input$cvModels) {
                       showModal(modalDialog("Running - ETS MAM", footer=NULL))
                       rv$cv_cvetsMAM<-cvResults(df_result,mets,input$cvH, vrs="MAM")}
                     
                     if ("ETS ZZZ" %in% input$cvModels) {
                       showModal(modalDialog("Running - ETS ZZZ", footer=NULL))
                       rv$cv_cvetsZZZ<-cvResults(df_result,mets,input$cvH, vrs="ZZZ")}
                     
                     if ("BATS" %in% input$cvModels) {
                       showModal(modalDialog("Running - BATS", footer=NULL))
                       rv$cv_cvbats<-cvResults(df_result,mbats,input$cvH)}
                     
                     if ("Auto Arima" %in% input$cvModels) {
                       showModal(modalDialog("Running - Auto Arima", footer=NULL))
                       rv$cv_cvautoarima<-cvResults(df_result,mautoarima,input$cvH)}
                     
                     if ("Struct" %in% input$cvModels) {
                       showModal(modalDialog("Running process - Struct", footer=NULL))
                       rv$cv_cvstruct<-cvResults(df_result,mstruct,input$cvH)}
                     
                     if ("HW" %in% input$cvModels) {
                       showModal(modalDialog("Running - Holt Winters", footer=NULL))
                       rv$cv_cvhw<-cvResults(df_result,mhw,input$cvH)}
                     
                     if ("NAIVE" %in% input$cvModels) {
                       showModal(modalDialog("Running - NAIVE", footer=NULL))
                       rv$cv_cvnaive<-cvResults(df_result,mnaive,input$cvH)}
                     
                     if ("SNAIVE" %in% input$cvModels) {
                       showModal(modalDialog("Running process -SNAIVE", footer=NULL))
                       rv$cv_cvsnaive<-cvResults(df_result,msnaive,input$cvH)}
                     
                     if ("RWF" %in% input$cvModels) {
                       showModal(modalDialog("Running - RWF", footer=NULL))
                       rv$cv_cvrwf<-cvResults(df_result,mrwf,input$cvH)}
                     
                     showModal(modalDialog("Running - ME", footer=NULL))
                     rv$cvmtrME <- tabME(rv$cv_cvrwf[,ci:cf],rv$cv_cvnaive[,ci:cf],rv$cv_cvsnaive[,ci:cf],rv$cv_cvbats[,ci:cf],
                                            rv$cv_cvetsANN[,ci:cf],rv$cv_cvetsMAM[,ci:cf],rv$cv_cvetsZZZ[,ci:cf],rv$cv_cvnnetar[,ci:cf],
                                            rv$cv_cvautoarima[,ci:cf],rv$cv_cvstruct[,ci:cf],rv$cv_cvhw[,ci:cf],rv$cv_cvprophet[,ci:cf],
                                         rv$cv_cvthetaf[,ci:cf],input$cvH)
                     rv$cvmtrME2<- tabConv2(rv$cvmtrME)
                     showModal(modalDialog("Running - MSE", footer=NULL))
                     rv$cvmtrMSE <- tabMSE(rv$cv_cvrwf[,ci:cf],rv$cv_cvnaive[,ci:cf],rv$cv_cvsnaive[,ci:cf],rv$cv_cvbats[,ci:cf],
                                              rv$cv_cvetsANN[,ci:cf],rv$cv_cvetsMAM[,ci:cf],rv$cv_cvetsZZZ[,ci:cf],rv$cv_cvnnetar[,ci:cf],
                                              rv$cv_cvautoarima[,ci:cf],rv$cv_cvstruct[,ci:cf],rv$cv_cvhw[,ci:cf],rv$cv_cvprophet[,ci:cf],
                                           rv$cv_cvthetaf[,ci:cf],input$cvH)
                     rv$cvmtrMSE2<- tabConv2(rv$cvmtrMSE)
                     showModal(modalDialog("Running - RMSE", footer=NULL))
                     rv$cvmtrRMSE <- tabRMSE(rv$cv_cvrwf[,ci:cf],rv$cv_cvnaive[,ci:cf],rv$cv_cvsnaive[,ci:cf],rv$cv_cvbats[,ci:cf],
                                              rv$cv_cvetsANN[,ci:cf],rv$cv_cvetsMAM[,ci:cf],rv$cv_cvetsZZZ[,ci:cf],rv$cv_cvnnetar[,ci:cf],
                                              rv$cv_cvautoarima[,ci:cf],rv$cv_cvstruct[,ci:cf],rv$cv_cvhw[,ci:cf],rv$cv_cvprophet[,ci:cf],
                                             rv$cv_cvthetaf[,ci:cf],input$cvH)
                     rv$cvmtrRMSE2<- tabConv2(rv$cvmtrRMSE)
                     showModal(modalDialog("Running - MAE", footer=NULL))
                     rv$cvmtrMAE <- tabMAE(rv$cv_cvrwf[,ci:cf],rv$cv_cvnaive[,ci:cf],rv$cv_cvsnaive[,ci:cf],rv$cv_cvbats[,ci:cf],
                                           rv$cv_cvetsANN[,ci:cf],rv$cv_cvetsMAM[,ci:cf],rv$cv_cvetsZZZ[,ci:cf],rv$cv_cvnnetar[,ci:cf],
                                           rv$cv_cvautoarima[,ci:cf],rv$cv_cvstruct[,ci:cf],rv$cv_cvhw[,ci:cf],rv$cv_cvprophet[,ci:cf],
                                           rv$cv_cvthetaf[,ci:cf],input$cvH)
                     rv$cvmtrMAE2<- tabConv2(rv$cvmtrMAE)
                     showModal(modalDialog("Running - MAPE", footer=NULL))
                     rv$cvmtrMAPE <- tabMAPE(rv$cv_cvrwf,rv$cv_cvnaive,rv$cv_cvsnaive,rv$cv_cvbats,
                                              rv$cv_cvetsANN,rv$cv_cvetsMAM,rv$cv_cvetsZZZ,rv$cv_cvnnetar,
                                              rv$cv_cvautoarima,rv$cv_cvstruct,rv$cv_cvhw,rv$cv_cvprophet,
                                             rv$cv_cvthetaf,input$cvH)
                     rv$cvmtrMAPE2<- tabConv2(rv$cvmtrMAPE)
                     showModal(modalDialog("Running - Cross Validation Residuals Analisys", footer=NULL))
                     
                     c1<-(input$cvH*2+1)
                     c2<-(c1+input$cvH-1)
                     colNames<-colNamesFunc("FOREC-",seq(from=1,to=input$cvH))
                     dfErrors<-NULL

                     if(!is.null(rv$cv_cvrwf)){
                       if(is.null(dfErrors)){dfErrors<-CrossValuesErrors(rv$cv_cvrwf[,c1:c2],"rwf",colNames)}
                       else dfErrors<-rbind(dfErrors,CrossValuesErrors(rv$cv_cvrwf[,c1:c2],"rwf",colNames))}
                   
                     if(!is.null(rv$cv_cvprophet)){
                       if(is.null(dfErrors)){dfErrors<-CrossValuesErrors(rv$cv_cvprophet[,c1:c2],"prophet",colNames)}
                       else dfErrors<-rbind(dfErrors,CrossValuesErrors(rv$cv_cvprophet[,c1:c2],"prophet",colNames))}
                     if(!is.null(rv$cv_cvnnetar)){
                       if(is.null(dfErrors)){dfErrors<-CrossValuesErrors(rv$cv_cvnnetar[,c1:c2],"nnetar",colNames)}
                       else dfErrors<-rbind(dfErrors,CrossValuesErrors(rv$cv_cvnnetar[,c1:c2],"nnetar",colNames))}
                     if(!is.null(rv$cv_cvthetaf)){
                       if(is.null(dfErrors)){dfErrors<-CrossValuesErrors(rv$cv_cvthetaf[,c1:c2],"thetaf",colNames)}
                       else dfErrors<-rbind(dfErrors,CrossValuesErrors(rv$cv_cvthetaf[,c1:c2],"thetaf",colNames))}
                     if(!is.null(rv$cv_cvetsANN)){
                       if(is.null(dfErrors)){dfErrors<-CrossValuesErrors(rv$cv_cvetsANN[,c1:c2],"etsANN",colNames)}
                       else dfErrors<-rbind(dfErrors,CrossValuesErrors(rv$cv_cvetsANN[,c1:c2],"etsANN",colNames))}
                     if(!is.null(rv$cv_cvetsMAM)){
                       if(is.null(dfErrors)){dfErrors<-CrossValuesErrors(rv$cv_cvetsMAM[,c1:c2],"etsMAM",colNames)}
                       else dfErrors<-rbind(dfErrors,CrossValuesErrors(rv$cv_cvetsMAM[,c1:c2],"etsMAM",colNames))}
                     if(!is.null(rv$cv_cvetsZZZ)){
                       if(is.null(dfErrors)){dfErrors<-CrossValuesErrors(rv$cv_cvetsZZZ[,c1:c2],"etsZZZ",colNames)}
                       else dfErrors<-rbind(dfErrors,CrossValuesErrors(rv$cv_cvetsZZZ[,c1:c2],"etsZZZ",colNames))}
                     if(!is.null(rv$cv_cvbats)){
                       if(is.null(dfErrors)){dfErrors<-CrossValuesErrors(rv$cv_cvbats[,c1:c2],"bats",colNames)}
                       else dfErrors<-rbind(dfErrors,CrossValuesErrors(rv$cv_cvbats[,c1:c2],"bats",colNames))}
                     if(!is.null(rv$cv_cvautoarima)){
                       if(is.null(dfErrors)){dfErrors<-CrossValuesErrors(rv$cv_cvautoarima[,c1:c2],"autoarima",colNames)}
                       else dfErrors<-rbind(dfErrors,CrossValuesErrors(rv$cv_cvautoarima[,c1:c2],"autoarima",colNames))}
                     if(!is.null(rv$cv_cvstruct)){
                       if(is.null(dfErrors)){dfErrors<-CrossValuesErrors(rv$cv_cvstruct[,c1:c2],"struct",colNames)}
                       else dfErrors<-rbind(dfErrors,CrossValuesErrors(rv$cv_cvstruct[,c1:c2],"struct",colNames))}
                     if(!is.null(rv$cv_cvhw)){
                       if(is.null(dfErrors)){dfErrors<-CrossValuesErrors(rv$cv_cvhw[,c1:c2],"hw",colNames)}
                       else dfErrors<-rbind(dfErrors,CrossValuesErrors(rv$cv_cvhw[,c1:c2],"hw",colNames))}
                     if(!is.null(rv$cv_cvsnaive)){
                       if(is.null(dfErrors)){dfErrors<-CrossValuesErrors(rv$cv_cvsnaive[,c1:c2],"snaive",colNames)}
                       else dfErrors<-rbind(dfErrors,CrossValuesErrors(rv$cv_cvsnaive[,c1:c2],"snaive",colNames))}
                     if(!is.null(rv$cv_cvnaive)){
                       if(is.null(dfErrors)){dfErrors<-CrossValuesErrors(rv$cv_cvnaive[,c1:c2],"naive",colNames)}
                       else dfErrors<-rbind(dfErrors,CrossValuesErrors(rv$cv_cvnaive[,c1:c2],"naive",colNames))}
                     
                     colnames(dfErrors) <- c("Methods", "CV", "Value")
                     dfErrors <- as.data.frame(dfErrors)
                     dfErrors[, 3] <- as.double(dfErrors[, 3])
                     dfErrors <- dfErrors %>% dplyr::filter(!is.na(Value))
                     rv$dfErrors<-dfErrors
                     
                     showModal(modalDialog("Please check results: Errors Plots & Errors Analysis option", footer=NULL))
                     Sys.sleep(2)
                     tabs<-c("CVDefinition","ErrorAnalisysPlots","ErrorPlots","ErrorAnalysis")
                     for (tbs in tabs)
                        showTab(inputId = "tbs", target = tbs )
                     
                     updateSliderInput(session, "nFor", value = input$cvH ,min = 1, max = 24, step = 1)
                   }  
                   removeModal()
                 }
                )
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --- -
    # Point Forecasting event ####
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --- -
    observeEvent(input$calcForecast,
                 {
                   #Clean Forecast Values
                   showModal(modalDialog("Please wait while forecasting process runs.", footer=NULL, fade=TRUE))
                   rv$Forecast<-NULL
                   rv$ForecastValues<-NULL
                   df<-data.frame(rv$dataOriginal)
                   rv$Forecast<-forecastTabValues(df,input$forecastModels,input$nFor,input$dateType)
                   forec<-tail(rv$Forecast,n=input$nFor)
                   .nc<-ncol(forec)
                   rv$ForecastValues<-forec[,c(1,3:.nc)]
                   if (is.null(nrow(rv$ForecastValues)))
                   {rv$ForecastValues[1]<-as.character.Date(lubridate::as_date(as.numeric(rv$ForecastValues[1])))}
                   else
                   {rv$ForecastValues[,1]<-as.character.Date(lubridate::as_date(as.numeric(rv$ForecastValues[,1])))}
                   
                   removeModal()
                 })

    # --- --- --- --- --- --- --- --- --- --- --- --- --- --- -
    # Interval Forecasting event ####
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --- -
    observeEvent(input$calcIntervalForecast,
                 {
                   #Clean Forecast Values
                   showModal(modalDialog("Please wait while forecasting process runs.", footer=NULL, fade=TRUE))
                   rv$IntervalForecast<-NULL
                   rv$IntervalForecastValues<-NULL
                   df<-data.frame(rv$dataOriginal)
                   rv$IntervalForecast<-IntervalforecastTabValues(df,input$intervalforecastModels,input$nIntFor,input$dateType)
                   forec<-tail(rv$IntervalForecast,n=input$nIntFor)
                   .nc<-ncol(forec)
                   rv$IntervalForecastValues<-forec[,c(1,3:.nc)]
                   if (is.null(nrow(rv$IntervalForecastValues)))
                   {rv$IntervalForecastValues[1]<-as.character.Date(lubridate::as_date(as.numeric(rv$IntervalForecastValues[1])))}
                   else
                   {rv$IntervalForecastValues[,1]<-as.character.Date(lubridate::as_date(as.numeric(rv$IntervalForecastValues[,1])))}
                   
                   removeModal()
                 })
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --
    # Series Summary Statistics - Date ####
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --
    output$summaryDateDataTable <- renderTable({
      if (is.null(rv$dadosAnalise))
      {return(NULL)}
      else 
      {
        sumData<-summaryData(rv$dadosAnalise)
        sumData<-subset(sumData,sumData[,1]=="date")
        sumData<-sumData[,2:3]
        colnames(sumData)<-c('Metric','Date')
        sumData
      }
    })
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --
    # Series Summary Statistics - Value ####
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --
    output$summaryValueDataTable <- renderTable({
      if (is.null(rv$dadosAnalise))
      {return(NULL)}
      else 
      {
        sumData<-summaryData(rv$dadosAnalise)
        sumData<-subset(sumData,sumData[,1]=="value")
        sumData<-sumData[,2:3]
        colnames(sumData)<-c('Metric','Observation')
        sumData
      }
    })
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --
    # Time Series - Top xx Records ####
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --
    output$topRecordsTable <- renderTable({
      if (is.null(rv$dadosFicheiro))
      {return(NULL)}
      else 
      {
        dados<-head(rv$dadosFicheiro,n=6)
        colnames(dados)<-c('Date','Observation')
        dados[1] <- lapply(dados[1], as.character)
        dados
      }
    })
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --
    # Time Series - Tail XX Records ####
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --
    output$tailRecordsTable <- renderTable({
      if (is.null(rv$dadosFicheiro))
        {return(NULL)}
      else 
      {
        dados<-tail(rv$dadosFicheiro,n=6)
        colnames(dados)<-c('Date','Observation')
        dados[1] <- lapply(dados[1], as.character)
        dados
        }
    })
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --
    # Plot Time Series Anomality Analisys ####
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --
    output$anomalyDiagnosticPlot <- renderPlotly({
      dataOriginal<-NULL
      if (is.null(rv$imputedData) | is.null(rv$dadosFicheiro))
        return(NULL)
      else
      {
        dataOriginal<-as.data.frame(cbind(date=rv$dadosFicheiro[,1],value=rv$imputedData))
        if (is.null(dataOriginal))
          {return(NULL)}
        
        dataOriginal[,1]<-lubridate::as_date(as.numeric(dataOriginal[,1]))
        AnomalyDiagnosticPlot(dataOriginal,(0.15/input$alpha))  
      }
    })

      # --- --- --- --- --- --- --- --- --- --- --- --- --- --
      # Plot Time Series Anomality Table ####
      # --- --- --- --- --- --- --- --- --- --- --- --- --- --
      output$anomalyDiagnosticTable <- renderTable(
        striped = TRUE,
        bordered = TRUE,
        width = "30%",
        hover = TRUE,
        align = 'l',
        spacing = 'xs',
        colnames = FALSE,
        rownames = TRUE,
        digits = 0,
        {
        dataOriginal<-NULL
        if (is.null(rv$imputedData) | is.null(rv$dadosFicheiro))
          return(NULL)
        else
        {
          
          dataOriginal<-as.data.frame(cbind(date=lubridate::as_date(as.numeric(rv$dadosFicheiro[,1])),value=rv$imputedData))
          if (!is.null(dataOriginal))
          { dataOriginal[,1] <- lubridate::as_date(dataOriginal[,1])
          .Data <- dataOriginal %>%timetk::tk_anomaly_diagnostics(date, value, .frequency = "auto",.trend = "auto",.alpha=(0.15/input$alpha),.max_anomalies = 0.2,.message = TRUE)
          if (!is.null(.Data))
          { .Data <-as.matrix(.Data %>% dplyr::filter(anomaly == "Yes") %>% count() %>% as.numeric())
          .matrixOuliers[1,2]<-.Data}}
          rownames(.Data)<-input$imputationMethod #c("Anomalies")
          colnames(.Data)<-c("Number")
          .Data
        }
    })
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --
    # Plot Time Series Anomality Summary Table ####
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --
    output$AnomalyDiagnosticDataOutliersSummaryTable <- renderTable(
      striped = TRUE,
      bordered = TRUE,
      width = "100%",
      hover = TRUE,
      align = 'l',
      spacing = 'xs',
      colnames = FALSE,
      rownames = TRUE,
      digits = 0,
      {
        if (is.null(rv$dadosFicheiro))
          return(NULL)
        else
        {
          try(
            AnomalyDiagnosticDataOutliersSummary(rv$dadosFicheiro,.frequency=rv$Frequency,.alpha=(0.15/input$alpha))
          , silent = TRUE)
        }
      })

    # --- --- --- --- --- --- --- --- --- --- --- --- --- --
    # Plot Time Series Details ####
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --
    output$TimeSeriesPlot <- renderDygraph({
      #if (is.null(rv$dadosAnalise))
      if (is.null(rv$dataOriginal))
        {return(NULL)}
      else
      {  
        #dados<-rv$dadosAnalise
        dados<-rv$dataOriginal
        dados_ts<-convertDataReceivedTS(dados,input$dateType,input$colValueNumber)
        dygraph(dados_ts) %>% dyRangeSelector
      }
    })

    output$naSeriesPlot <- renderPlot({
      if (is.null(rv$plotData))
      {return(NULL)}
      else
      { 
        tm_old <- theme_get()
        tm_new <- tm_old %+replace% theme(rect = element_rect(fill = "#FFFFFF"))
        tm_new <- tm_new %+replace% theme(panel.background = element_rect(fill = "#FFFFFF"))
        tm_new <- tm_new %+replace% theme(plot.background = element_rect(fill = "#FFFFFF"))
        tm_new <- tm_new %+replace% theme(panel.grid.major = element_line(colour = "#999E9D"))
        theme_set(tm_new)
        Plot<- ggplot(rv$plotData)
        Plot<- Plot + geom_line(aes(date,imputed,colour="imputed"), linetype = 1, size=1)
        Plot<- Plot + geom_line(aes(date,actual,colour="Actual"),colour="black", size=2)
        Plot<- Plot + labs(title = "Missing Values Imputation",x = "Date",y = "Value",color="Legend")
        Plot
        #theme_set(tm_old )
        
      }
    })
    
    output$diffSeriesPlot <- renderDygraph({
      if (is.null(rv$dadosAnalise))
      {return(NULL)}
      else
      {  
        dados<-rv$dadosAnalise
        dados_ts<-convertDataReceivedTS(dados,input$dateType,input$colValueNumber)
        dygraph(dados_ts) %>% dyRangeSelector
      }
    })
    
    output$verb <- renderText({rv$Stationary })
    
    output$cvDataPlot <- renderPlot({
      if (is.null(rv$dadosAnalise))
      {return(NULL)}
      else
      { 
        dataOriginal<-rv$dadosAnalise[,1:2]
        rv$cv_actual <- cvData(dataOriginal,input$cvH,input$cvWindow,input$cvInitial,input$cvStep,input$cvMax,input$cvDataType)
        cvDataPlots(rv$cv_actual,input$cvH,input$cvWindow,input$cvInitial,input$cvStep,input$cvMax,input$cvDataType)
        
      }
    })
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --
    # Cross Validation - Errors Rankings ####
    # Representar em tabela a performamce dos modelos calibrados dados de teste
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --
    output$cvErrorRankingTable<- renderTable(digits = 3,striped = TRUE, bordered = TRUE,  
                                             hover = TRUE, align='c', spacing = 'xs',  
                                             rownames = TRUE, width = "100%", {
      if (input$cvErrorVal=="ME" && !is.null(rv$cvmtrME))
        {rankingErrorsValues(rv$cvmtrME2,3)}
      else if (input$cvErrorVal=="MSE" && !is.null(rv$cvmtrMSE))
        {rankingErrorsValues(rv$cvmtrMSE2,3)}
      else if (input$cvErrorVal=="RMSE" && !is.null(rv$cvmtrRMSE))
        {rankingErrorsValues(rv$cvmtrRMSE2,3)}
      else if (input$cvErrorVal=="MAE" && !is.null(rv$cvmtrMAE))
        {rankingErrorsValues(rv$cvmtrMAE2,3)}
      else if (input$cvErrorVal=="MAPE" && !is.null(rv$cvmtrMAPE))
        {rankingErrorsValues(rv$cvmtrMAPE2,3)}
      else 
        NULL
    })

    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --
    # Cross Validation - Errors ####
    # Representar em tabela a performamce dos modelos calibrados dados de teste
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --
    output$cvErrorTable<- renderTable(digits = 3,striped = TRUE, bordered = TRUE,  
                                      hover = TRUE, align='c', spacing = 'xs',  
                                      rownames = FALSE, width = "100%",{
      if (input$cvErrorVal=="ME" && !is.null(rv$cvmtrME))
        {rv$cvmtrME}
      else if (input$cvErrorVal=="MSE" && !is.null(rv$cvmtrMSE))
        {rv$cvmtrMSE}
      else if (input$cvErrorVal=="RMSE" && !is.null(rv$cvmtrRMSE))
        {rv$cvmtrRMSE}
      else if (input$cvErrorVal=="MAE" && !is.null(rv$cvmtrMAE))
        {rv$cvmtrMAE}
      else if (input$cvErrorVal=="MAPE" && !is.null(rv$cvmtrMAPE))
        {rv$cvmtrMAPE}
      else 
        NULL
    })

    output$cvErrorPlot <- renderPlot({
      if (input$cvErrorVal=="ME" && !is.null(rv$cvmtrME2))
        {cvPlotErrors(rv$cvmtrME2,'Cross Validation - ME - Boxplot','Cross Validation - ME  - Evolution')}
      else if (input$cvErrorVal=="MSE" && !is.null(rv$cvmtrMSE2))
        {cvPlotErrors(rv$cvmtrMSE2,'Cross Validation - MSE - Boxplot','Cross Validation - MSE  - Evolution')}
      else if (input$cvErrorVal=="RMSE" && !is.null(rv$cvmtrRMSE2))
        {cvPlotErrors(rv$cvmtrRMSE2,'Cross Validation - RMSE - Boxplot','Cross Validation - RMSE  - Evolution')}
      else if (input$cvErrorVal=="MAE" && !is.null(rv$cvmtrMAE2))
        {cvPlotErrors(rv$cvmtrMAE2,'Cross Validation - MAE - Boxplot','Cross Validation - MAE  - Evolution')}
      else if (input$cvErrorVal=="MAPE" && !is.null(rv$cvmtrMAPE2))
        {cvPlotErrors(rv$cvmtrMAPE2,'Cross Validation - MAPE - Boxplot','Cross Validation - MAPE  - Evolution')}
      else 
        NULL
    })
    
    output$CrossValuesErrorsBoxPlot <- renderPlot({
      if (is.null(rv$dfErrors))
        {return(NULL)}
      else
      {  
        CrossValuesErrorsBoxPlot(rv$dfErrors)
      }
    })
    
    output$cvErrorsPlot <- renderPlot({
      if (is.null(rv$cvmtrME2))
      {return(NULL)}
      else
      { 
        boxplotME2 <- ggplot(rv$cvmtrME2, aes(x = Methods, y = Value)) +
          geom_boxplot(aes(color = Methods)) +
          geom_hline(yintercept = 0,
                     linetype = "dashed",
                     color = "red") +
          geom_jitter(aes(color = Methods)) +
          ggtitle("Cross Validation - ME - Boxplot ") +
          theme_bw()
        
        plotME2 <- ggplot(rv$cvmtrME2, aes(x = CV, y = Value, group = Methods)) +
          geom_line(aes(color = Methods)) +
          geom_point(aes(color = Methods, shape = Methods)) +
          ggtitle("Cross Validation - ME  - Evolution ") +
          theme_bw()
        
        boxplotMSE2 <- ggplot(rv$cvmtrMSE2, aes(x = Methods, y = Value)) +
          geom_boxplot(aes(color = Methods)) +
          geom_hline(yintercept = 0,
                     linetype = "dashed",
                     color = "red") +
          geom_jitter(aes(color = Methods)) +
          ggtitle("Cross Validation - MSE - Boxplot ") +
          theme_bw()
        
        plotMSE2 <- ggplot(rv$cvmtrMSE2, aes(x = CV, y = Value, group = Methods)) +
          geom_line(aes(color = Methods)) +
          geom_point(aes(color = Methods, shape = Methods)) +
          ggtitle("Cross Validation - MSE  - Evolution ") +
          theme_bw()
        
        boxplotRMSE2 <- ggplot(rv$cvmtrRMSE2, aes(x = Methods, y = Value)) +
          geom_boxplot(aes(color = Methods)) +
          geom_hline(yintercept = 0,
                     linetype = "dashed",
                     color = "red") +
          geom_jitter(aes(color = Methods)) +
          ggtitle("Cross Validation - RMSE - Boxplot ") +
          theme_bw()
        
        plotRMSE2 <- ggplot(rv$cvmtrRMSE2, aes(x = CV, y = Value, group = Methods)) +
          geom_line(aes(color = Methods)) +
          geom_point(aes(color = Methods, shape = Methods)) +
          ggtitle("Cross Validation - RMSE  - Evolution ") +
          theme_bw()
        
        boxplotMAE2 <- ggplot(rv$cvmtrMAE2, aes(x = Methods, y = Value)) +
          geom_boxplot(aes(color = Methods)) +
          geom_hline(yintercept = 0,
                     linetype = "dashed",
                     color = "red") +
          geom_jitter(aes(color = Methods)) +
          ggtitle("Cross Validation - MAE - Boxplot ") +
          theme_bw()
        
        plotMAE2 <- ggplot(rv$cvmtrMAE2, aes(x = CV, y = Value, group = Methods)) +
          geom_line(aes(color = Methods)) +
          geom_point(aes(color = Methods, shape = Methods)) +
          ggtitle("Cross Validation - MAE  - Evolution ") +
          theme_bw()
        
        boxplotMAPE2 <- ggplot(rv$cvmtrMAPE2, aes(x = Methods, y = Value)) +
          geom_boxplot(aes(color = Methods)) +
          geom_hline(yintercept = 0,
                     linetype = "dashed",
                     color = "red") +
          geom_jitter(aes(color = Methods)) +
          ggtitle("Cross Validation - MAPE - Boxplot ") +
          theme_bw()
        
        plotMAPE2 <- ggplot(rv$cvmtrMAPE2, aes(x = CV, y = Value, group = Methods)) +
          geom_line(aes(color = Methods)) +
          geom_point(aes(color = Methods, shape = Methods)) +
          ggtitle("Cross Validation - MAPE  - Evolution ") +
          theme_bw()
        
        grid.arrange(
          boxplotME2,
          plotME2,
          boxplotMSE2,
          plotMSE2,
          boxplotRMSE2,
          plotRMSE2,
          boxplotMAE2,
          plotMAE2,
          boxplotMAPE2,
          plotMAPE2,
          nrow = 5,
          ncol = 2
        )
      }
    })
    
    output$seriesPeriodsPlot <- renderPlot({
      if (is.null(rv$dadosAnalise))
        {return(NULL)}
      else
        {seriesPeriodsPlots(rv$dadosAnalise[,1:2],"Montlhy")}
    })
  
      output$MonthlySeriesPeriodsPlot <- renderPlot({
      if (is.null(rv$dadosAnalise))
        {return(NULL)}
      else
        {MonthlySeriesPeriodsPlots(rv$dadosAnalise[,1:2],"Montlhy")}
    })
    output$QuarterlySeriesPeriodsPlot <- renderPlot({
      if (is.null(rv$dadosAnalise))
        {return(NULL)}
      else
        {QuarterlySeriesPeriodsPlots(rv$dadosAnalise[,1:2],"Quarterly")}
    })
    output$HalfYearSeriesPeriodsPlot <- renderPlot({
      if (is.null(rv$dadosAnalise))
        {return(NULL)}
      else
        {HalfYearSeriesPeriodsPlots(rv$dadosAnalise[,1:2],"Half-Year")}
    })
    output$YearSeriesPeriodsPlot <- renderPlot({
      if (is.null(rv$dadosAnalise))
        {return(NULL)}
      else
        {YearSeriesPeriodsPlots(rv$dadosAnalise[,1:2],"Year")}
    })
    
    output$evolutionFacetPlot <- renderPlotly({
      if (is.null(rv$dadosAnalise))
        {return(NULL)}
      else
        {evolutionFacetPlots(rv$dadosAnalise[,1:2],"Montlhy")}
    })
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --
    # Data Analysis: Time Series STL Decomposition Plot 2 ####
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --
    output$STLdecompositionPlot2 <- renderDygraph({
      if (is.null(rv$dadosAnalise))
      {return(NULL)}
      else
      {  
        seriesType<-input$decompositionAnalysis
        #print("aa")
        STLdecompositionPlots2(rv$dadosAnalise[,1:2],"additive",seriesType,input$dateType)
      }
    })
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --
    # Data Analysis: Time Series STL Decomposition Plot ####
    # --- --- --- --- --- --- --- --- --- --- --- --- --- --
    output$STLdecompositionPlot <- renderPlotly({
      if (is.null(rv$dadosAnalise))
        {return(NULL)}
      else
        {STLdecompositionPlots(rv$dadosAnalise[,1:2],"additive")}
    })
    
    output$STLStrengthPlots <- renderPlot({
      if (is.null(rv$dadosAnalise))
      {return(NULL)}
      else
      {STLStrengthPlot(rv$dadosAnalise[,1:2],12)}
    })
    
    output$IndividualSeriesPlotly <- renderDygraph({
      if (is.null(rv$dataOriginal))
      {return(NULL)}
      else
      {
        df<-data.frame(rv$dataOriginal)
        individualSeriesPlot(df)
      }
    })
    
    output$IndividualBoxPlotly <- renderPlotly({
      if (is.null(rv$dataOriginal))
      {return(NULL)}
      else
      { 
        df<-data.frame(rv$dataOriginal)
        individualBoxPlot(df)
      }
    })

    output$ForecastValuesPlots<- renderDygraph({
      if (is.null(rv$Forecast))
      {return(NULL)}
      else
      {
        df<-data.frame(rv$Forecast)
        nCols<-length(df)
        nRows<-nrow(df)
        nRowsOriginals<-nRows-input$nFor
        nFirstRowsForecast<-nRows-input$nFor+1
        df[1:nRowsOriginals,4:nCols]<-NA
        df[nFirstRowsForecast:nRows,2:2]<-NA
        dygraphforecValues(df)
        
      }
    })
    
    output$ForecastValuesTable <-
      renderTable(
        digits = 3,
        striped = TRUE,
        bordered = TRUE,
        width = "100%",
        hover = TRUE,
        align = 'c',
        spacing = 'xs',
        rownames = FALSE,
        {
          if (!is.null(rv$ForecastValues))
            {rv$ForecastValues}
          else
            NULL
        }
      )
    
    output$IntervalForecastValuesTable <-
      renderTable(
        digits = 3,
        striped = TRUE,
        bordered = TRUE,
        width = "100%",
        hover = TRUE,
        align = 'c',
        spacing = 'xs',
        rownames = FALSE,
        {
          if (!is.null(rv$IntervalForecastValues))
          {rv$IntervalForecastValues}
          else
            NULL
        }
      )

    output$IntervalForecastValuesPlots<- renderDygraph({
      if (is.null(rv$IntervalForecast))
      {return(NULL)}
      else
      {
        df<-data.frame(rv$IntervalForecast)
        dygraphIntervalForecValues(df)
        
      }
    })

    output$getDiffStationaryTable <-
      renderTable(
        striped = TRUE,
        bordered = TRUE,
        width = "100%",
        hover = TRUE,
        align = 'c',
        spacing = 'xs',
        rownames = FALSE,
        {
          if (!is.null(rv$dadosAnalise))
          {
          dados_ts<-convertDataReceivedTS(rv$dadosAnalise,input$dateType,input$colValueNumber)
          getDiffStationarySerie(dados_ts)
          }
          else
            NULL
        }
      )
    
    
  }
)
