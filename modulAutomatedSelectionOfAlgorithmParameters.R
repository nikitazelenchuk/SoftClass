moduleUI_4 <- function(id, label = "moduleUI_4") {
  ns <- NS(id)
  
  tagList(
    
    
    tags$head(# цвет выбранного для .selectize
      tags$style(
        HTML(
          "
     #.item {
     #  background: #2196f3 !important;
     #  color: white !important;
     #}
     .selectize-dropdown-content .active {
       background: #00a65a !important;
       color: black !important;
     }
  "
        )
      )),
    tabBox(
      id = ns("tabsetAuto"),
      width = 12,
      
      tabPanel(
        "Градиентный бустинг",
        fluidRow(column(12, uiOutput(
          ns("auto1ClassifLearningView")
        )),
        column(12, uiOutput(ns("ViewXGB")))
        )),
      
      tabPanel(
        "Случайный лес",
        fluidRow(column(12, uiOutput(
          ns("auto2ClassifLearningView")
        )),
        column(12, uiOutput(ns("ViewRF")))
        ))
    ))
  
}


moduleServer_4 <- function(id, rvClass, prefix = "") {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 setsH2O <- reactiveValues(test_df_h2o = NULL, train_df_h2o = NULL)
                 setsMet <- reactiveValues(testsetMet = NULL, trainsetMet = NULL)
                 xy <- reactiveValues(x = NULL, y = NULL)
                 
                 targetVar <- reactive({
                   targetVar <- rvClass$targetVar
                   return(targetVar)
                 })
                 
                 predictor <- reactive({
                   predictor <- rvClass$predictor
                   return(predictor)
                 })
                 
                 dataTest <- reactive({
                   dataTest <- rvClass$dataTest
                   return(dataTest)
                 })
                 
                 dataTrain <- reactive({
                   dataTrain <- rvClass$dataTrain
                   return(dataTrain)
                 })
                 
                 output$auto1ClassifLearningView <- renderUI({
                   if (!is.null(dataTrain()) && !is.null(dataTest())) {
                     
                     fluidRow(
                       column(
                         3,
                         style = "margin-bottom: 10px;",
                         #style = "margin-top: 25px;",
                         style = "margin-bottom: 10px;",
                         actionButton(ns("auto1Learning"), "Запустить процесс подбора параметров алгоритма")
                       ),
                       
                       #column(12, uiOutput(ns("balanceView")))
                       
                       
                     )
                   }
                 })
                 
                 output$auto2ClassifLearningView <- renderUI({
                   if (!is.null(dataTrain()) && !is.null(dataTest())) {
                     
                     fluidRow(
                       column(
                         3,
                         style = "margin-bottom: 10px;",
                         #style = "margin-top: 25px;",
                         style = "margin-bottom: 10px;",
                         actionButton(ns("auto2Learning"), "Запустить процесс подбора параметров алгоритма")
                       ),
                       
                       #column(12, uiOutput(ns("balanceView")))
                       
                       
                     )
                   }
                 })

                 
                 auto1ClassifLearning <- eventReactive(input$auto1Learning, {
                   
                   # waiter_show( # show the waiter
                   #   html = spin_solar(), # use a spinner
                   #     "Подождите, алгоритмы обучаются...",
                   #   color = "#00a65a"
                   # )
                   
                   waiter_show( # show the waiter
                     color = "#00a65a",
                     html = tagList(
                       spin_solar(), # use a spinner
                       "Подождите, идёт подбор параметров алгоритма градиентного бустинга..."
                     )
                   )
                   
                   h2o.init()
                   train_df_h2o <- as.h2o(dataTrain())
                   test_df_h2o <- as.h2o(dataTest())
                   
                   setsH2O$train_df_h2o <- train_df_h2o
                   setsH2O$test_df_h2o <- test_df_h2o
                   
                   testsetMet <- as.data.frame(dataTest())
                   trainsetMet <- as.data.frame(dataTrain())
                   
                   setsMet$testsetMet <- testsetMet
                   setsMet$trainsetMet <- trainsetMet
                   
                   y <- names(train_df_h2o[, targetVar()])
                   x <- setdiff(names(train_df_h2o[, predictor()]), y)
                   
                   #print(x)
                   
                   xy$x <- x
                   xy$y <- y
                   
                   hyper_params <- list(ntrees = seq(20, 50),
                                        learn_rate = seq(0.0001, 0.2, 0.0001),
                                        max_depth = seq(1, 20, 1),
                                        sample_rate = seq(0.5, 1.0, 0.0001),
                                        col_sample_rate = seq(0.2, 1.0, 0.0001))
                   
                   search_criteria <- list(strategy = "RandomDiscrete",
                                           max_models = 20, 
                                           seed = 101)
                   
                   xgb_grid <- h2o.grid(algorithm = "xgboost",
                                        x = x, y = y,
                                        grid_id = "xgb_grid",
                                        distribution = "multinomial",
                                        training_frame = train_df_h2o,
                                        validation_frame = test_df_h2o,
                                        categorical_encoding = "OneHotExplicit",
                                        nfolds = 10,
                                        fold_assignment = "Modulo",
                                        keep_cross_validation_predictions = TRUE,
                                        seed = 101,
                                        hyper_params = hyper_params,
                                        search_criteria = search_criteria,
                                        auc_type="WEIGHTED_OVO")
                   
                   print(xgb_grid)
                   
                   xgb_gridperf <- h2o.getGrid(grid_id = "xgb_grid",
                                                sort_by = "auc",
                                                decreasing = TRUE)
                   print(xgb_gridperf)
                   
                   best_xgb <- h2o.getModel(xgb_gridperf@model_ids[[1]])
                   
                   print(best_xgb)
                   
                   best_xgb_perf <- h2o.performance(model = best_xgb,
                                                     newdata = test_df_h2o)
                   
                   print(best_xgb_perf)
                   
                   auc_table <- h2o.getGrid(grid_id = "xgb_grid", sort_by = "auc", decreasing = TRUE)
                   print(auc_table)
                   
                   predTestXGBA <- predict(best_xgb, test_df_h2o, type = "class")
                   AccTestXGBA <- mean(predTestXGBA == test_df_h2o[, targetVar()])
                   ATestXGBA <- paste(strong("Точность (accuracy): "), round(AccTestXGBA, 4), sep = "")
                   ATestXGBA <- ATestXGBA[c(1)]
                   
                   predTrainXGBA <- predict(best_xgb, train_df_h2o, type = "class")
                   AccTrainXGBA <- mean(predTrainXGBA == train_df_h2o[, targetVar()])
                   ATrainXGBA <- paste(strong("Точность (accuracy): "), round(AccTrainXGBA, 4), sep = "")
                   ATrainXGBA <- ATrainXGBA[c(1)]
                   
                   perfTestXGBA <- h2o.performance(best_xgb, test_df_h2o)
                   perfTrainXGBA <- h2o.performance(best_xgb, train_df_h2o)
                   
                   colTestXGBA <- ncol(h2o.confusionMatrix(best_xgb,test_df_h2o))
                   colTrainXGBA <- ncol(h2o.confusionMatrix(best_xgb,train_df_h2o))
                   
                   matrixTestXGBA = h2o.confusionMatrix(best_xgb,test_df_h2o)[,1:colTestXGBA]
                   matrixTrainXGBA = h2o.confusionMatrix(best_xgb,train_df_h2o)[,1:colTrainXGBA]
                   
                   predTestXGBAC <- as.data.frame(predTestXGBA[1])
                   predTrainXGBAC <- as.data.frame(predTrainXGBA[1])
                   
                   FTestXGBA <- f_meas_vec(testsetMet[, y], factor(predTestXGBAC$predict, levels = levels(testsetMet[, y])))
                   FTrainXGBA <- f_meas_vec(trainsetMet[, y], factor(predTrainXGBAC$predict, levels = levels(testsetMet[, y])))
                   
                   KapTestXGBA <- kap_vec(testsetMet[, y], factor(predTestXGBAC$predict, levels = levels(testsetMet[, y])))
                   KapTrainXGBA <- kap_vec(trainsetMet[, y], factor(predTrainXGBAC$predict, levels = levels(testsetMet[, y])))
                   
                   MccTestXGBA <- mcc_vec(testsetMet[, y], factor(predTestXGBAC$predict, levels = levels(testsetMet[, y])))
                   MccTrainXGBA <- mcc_vec(trainsetMet[, y], factor(predTrainXGBAC$predict, levels = levels(testsetMet[, y])))
                   
                   BATestXGBA <- bal_accuracy_vec(testsetMet[, y], factor(predTestXGBAC$predict, levels = levels(testsetMet[, y])))
                   BATrainXGBA <- bal_accuracy_vec(trainsetMet[, y], factor(predTrainXGBAC$predict, levels = levels(testsetMet[, y])))
                   
                   PTestXGBA <- precision_vec(testsetMet[, y], factor(predTestXGBAC$predict, levels = levels(testsetMet[, y])))
                   PTrainXGBA <- precision_vec(trainsetMet[, y], factor(predTrainXGBAC$predict, levels = levels(testsetMet[, y])))
                   
                   waiter_hide()
                   
                   output$aucXGB <- renderPrint({
                     auc_table
                   })
                   
                   if (input$auto1Learning) {
                     
                     output$ViewAuto1 <- renderUI({
                       
                       fluidRow(
                         column(5, pickerInput(inputId = ns('testTrainA1'),
                                               #selected = "",
                                               width = '100%',
                                               label = 'Показать результаты классификации на выборке:', 
                                               choices = list("Тестовая" = "Тестовая", "Обучающая" = "Обучающая"),
                                               options = list(`actions-box` = FALSE,
                                                              `none-selected-text` = "Ничего не выбрано"),
                                               multiple = F
                         )),
                         column(
                           2,
                           style = "margin-bottom: 10px;",
                           style = "margin-top: 25px;",
                           style = "margin-bottom: 10px;",
                           actionButton(ns("resultVA1"), "Отобразить")
                         ),
                         
                         uiOutput(ns("matrixA1View"))
                         
                       )})
                   }
                   
                   output$ViewResA1 <- renderUI({
                     
                     if (input$resultVA1){
                       
                       if (input$testTrainA1 == "Тестовая"){
                         str01 <- paste(strong("Модель-классификатор: "), "Градиентный бустинг")
                         str02 <- paste(strong("Результаты получены на тестовой выборке"))
                         str1 <- paste("")
                         str2 <- paste(strong("MSE (оценка Брайера): "), round(h2o.mse(perfTestXGBA), 4))
                         str3 <- paste(strong("RMSE: "), round(h2o.rmse(perfTestXGBA), 4))
                         str4 <- paste(strong("Logloss: "), round(h2o.logloss(perfTestXGBA), 4))
                         str5 <- paste(strong("AUC: "), round(h2o.auc(perfTestXGBA), 4))
                         str6 <- paste(ATestXGBA)
                         str7 <- paste(strong("F-мера: "), round(FTestXGBA, 4))
                         str8 <- paste(strong("Каппа Коэна: "), round(KapTestXGBA, 4))
                         str9 <- paste(strong("MCC (коэффициент Мэттьюса): "), round(MccTestXGBA, 4))
                         str10 <- paste(strong("Сбалансированная точность: "), round(BATestXGBA, 4))
                         str11 <- paste(strong("Точность (precision): "), round(PTestXGBA, 4))
                         str12 <- paste(strong("Матрица ошибок: "))
                         HTML(paste(str01, str02, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str1, str12, sep = '<br/>'))
                       }
                       
                       else if (input$testTrainA1 == "Обучающая"){
                         str01 <- paste(strong("Модель-классификатор: "), "Градиентный бустинг")
                         str02 <- paste(strong("Результаты получены на обучающей выборке"))
                         str1 <- paste("")
                         str2 <- paste(strong("MSE (оценка Брайера): "), round(h2o.mse(perfTrainXGBA), 4))
                         str3 <- paste(strong("RMSE: "), round(h2o.rmse(perfTrainXGBA), 4))
                         str4 <- paste(strong("Logloss: "), round(h2o.logloss(perfTrainXGBA), 4))
                         str5 <- paste(strong("AUC: "), round(h2o.auc(perfTrainXGBA), 4))
                         str6 <- paste(ATrainXGBA)
                         str7 <- paste(strong("F-мера: "), round(FTrainXGBA, 4))
                         str8 <- paste(strong("Каппа Коэна: "), round(KapTrainXGBA, 4))
                         str9 <- paste(strong("MCC (коэффициент Мэттьюса): "), round(MccTrainXGBA, 4))
                         str10 <- paste(strong("Сбалансированная точность: "), round(BATrainXGBA, 4))
                         str11 <- paste(strong("Точность (precision): "), round(PTrainXGBA, 4))
                         str12 <- paste(strong("Матрица ошибок: "))
                         HTML(paste(str01, str02, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str1, str12, sep = '<br/>'))
                       }
                     }
                   })
                   
                   
                   output$matrixA1 <- renderUI({
                     
                     if (input$resultVA1){
                       
                       if (input$testTrainA1 == "Тестовая"){
                         M <- matrixTestXGBA
                         rownames(M) <- gsub(" ", ".", rownames(M))
                         colnames(M) <- gsub(" ", ".", colnames(M))
                         M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                                    floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
                         html <- paste0("$$", M, "$$")
                         list(
                           withMathJax(HTML(html))
                         )
                       }
                       
                       else if (input$testTrainA1 == "Обучающая"){
                         M <- matrixTrainXGBA
                         rownames(M) <- gsub(" ", ".", rownames(M))
                         colnames(M) <- gsub(" ", ".", colnames(M))
                         M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                                    floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
                         html <- paste0("$$", M, "$$")
                         list(
                           withMathJax(HTML(html))
                         )
                       }
                     }
                   })
                 #})
                 
                 output$matrixA1View <- renderUI({
                   
                   if (input$resultVA1){
                     
                     fluidRow(column(
                       12,
                       box(
                         id = "resultA1",
                         title = "Результаты обучения",
                         status = "success",
                         solidHeader = TRUE,
                         width = 12,
                         uiOutput(ns("modelXGB")),
                         uiOutput(ns("ViewResA1")),
                         uiOutput(ns("matrixA1")),
                         if (input$testTrainA1 == "Обучающая"){
                           str1 <- paste("")
                           str13 <- paste(strong("Оценка важности предикторов (диаграмма): "))
                           str14 <- paste(imageOutput(ns("plotA1")))
                           str15 <- paste(strong("Оценка важности предикторов: "))
                           str16 <- paste(verbatimTextOutput(ns("varimpA1")))
                           HTML(paste(str1, str13, str14, str15, str16, sep = '<br/>'))
                           #imageOutput(ns("plot"))
                         }
                         #plotOutput(ns("plot"))
                       ),
                       #column(width = 12, align = "center",
                       #       plotOutput(ns("plot")),)
                       #plotOutput(ns("plot")),
                       # column(
                       #   12, plotOutput(ns("plot")))
                       
                       
                     ),
                     
                     )
                   }
                 })
                   
                 output$plotA1 <- renderPlot({
                   
                   if (input$resultVA1){
                     h2o.varimp_plot(best_xgb)
                   }
                   
                 })
                 
                 output$varimpA1 <- renderPrint({
                   
                   if (input$resultVA1){
                     
                     if (input$testTrainA1 == "Обучающая"){
                       
                       as.data.frame(h2o.varimp(best_xgb))
                       
                     }
                   }
                 })
                 })
  

                 
                 output$modelXGB <- renderUI({
                   if (input$testTrainA1 == "Обучающая"){
                   fluidRow(column(12, strong("")),
                            column(12, strong("Результаты поиска оптимальных гиперпараметров модели:")),
                            column(
                     12, verbatimTextOutput(ns("aucXGB"))))
                   }
                 })
                 
                 
                 
                 output$ViewXGB <- renderUI({

                   auto1ClassifLearning()
                   uiOutput(
                     ns("ViewAuto1"))
                   #fluidRow(
                    #   column(
                     #    12,uiOutput(ns("modelXGB"))))
                   
                   
                 })
                 
                 
                 #-------------------------
                 
                 auto2ClassifLearning <- eventReactive(input$auto2Learning, {
                   
                   
                   waiter_show( # show the waiter
                     color = "#00a65a",
                     html = tagList(
                       spin_solar(), # use a spinner
                       "Подождите, идёт подбор параметров алгоритма случайного леса..."
                     )
                   )
                   
                   h2o.init()
                   train_df_h2o <- as.h2o(dataTrain())
                   test_df_h2o <- as.h2o(dataTest())
                   
                   setsH2O$train_df_h2o <- train_df_h2o
                   setsH2O$test_df_h2o <- test_df_h2o
                   
                   testsetMet <- as.data.frame(dataTest())
                   trainsetMet <- as.data.frame(dataTrain())
                   
                   setsMet$testsetMet <- testsetMet
                   setsMet$trainsetMet <- trainsetMet
                   
                   y <- names(train_df_h2o[, targetVar()])
                   x <- setdiff(names(train_df_h2o[, predictor()]), y)
                   
                   #print(x)
                   
                   xy$x <- x
                   xy$y <- y
                   
                   hyper_params <- list(ntrees = seq(50, 70),
                                        mtries = seq(3, 5, by = 1),
                                        max_depth = seq(10, 30, by = 10),
                                        # min_rows = seq(1, 3, by = 1),
                                        # nbins = seq(20, 30, by = 10),
                                        sample_rate = c(0.55, 0.632, 0.75))
                   
                   search_criteria <- list(strategy = "RandomDiscrete",
                                           max_models = 20, 
                                           seed = 101)
                   
                   rf_grid <- h2o.grid(algorithm = "randomForest",
                                        x = x, y = y,
                                        grid_id = "rf_grid",
                                        distribution = "multinomial",
                                        training_frame = train_df_h2o,
                                        validation_frame = test_df_h2o,
                                        categorical_encoding = "OneHotExplicit",
                                        nfolds = 10,
                                        fold_assignment = "Modulo",
                                        keep_cross_validation_predictions = TRUE,
                                        seed = 101,
                                        hyper_params = hyper_params,
                                        search_criteria = search_criteria,
                                        auc_type="WEIGHTED_OVO")
                   
                   print(rf_grid)
                   
                   rf_gridperf <- h2o.getGrid(grid_id = "rf_grid",
                                               sort_by = "auc",
                                               decreasing = TRUE)
                   #print(rf_gridperf)
                   
                   best_rf <- h2o.getModel(rf_gridperf@model_ids[[1]])
                   
                   #print(best_rf)
                   
                   best_rf_perf <- h2o.performance(model = best_rf,
                                                    newdata = test_df_h2o)
                   
                   #print(best_rf_perf)
                   
                   auc_table <- h2o.getGrid(grid_id = "rf_grid", sort_by = "auc", decreasing = TRUE)
                   #print(auc_table)
                   
                   predTestRFA <- predict(best_rf, test_df_h2o, type = "class")
                   AccTestRFA <- mean(predTestRFA == test_df_h2o[, targetVar()])
                   ATestRFA <- paste(strong("Точность (accuracy): "), round(AccTestRFA, 4), sep = "")
                   ATestRFA <- ATestRFA[c(1)]
                   
                   predTrainRFA <- predict(best_rf, train_df_h2o, type = "class")
                   AccTrainRFA <- mean(predTrainRFA == train_df_h2o[, targetVar()])
                   ATrainRFA <- paste(strong("Точность (accuracy): "), round(AccTrainRFA, 4), sep = "")
                   ATrainRFA <- ATrainRFA[c(1)]
                   
                   perfTestRFA <- h2o.performance(best_rf, test_df_h2o)
                   perfTrainRFA <- h2o.performance(best_rf, train_df_h2o)
                   
                   colTestRFA <- ncol(h2o.confusionMatrix(best_rf,test_df_h2o))
                   colTrainRFA <- ncol(h2o.confusionMatrix(best_rf,train_df_h2o))
                   
                   matrixTestRFA = h2o.confusionMatrix(best_rf,test_df_h2o)[,1:colTestRFA]
                   matrixTrainRFA = h2o.confusionMatrix(best_rf,train_df_h2o)[,1:colTrainRFA]
                   
                   predTestRFAC <- as.data.frame(predTestRFA[1])
                   predTrainRFAC <- as.data.frame(predTrainRFA[1])
                   
                   FTestRFA <- f_meas_vec(testsetMet[, y], factor(predTestRFAC$predict, levels = levels(testsetMet[, y])))
                   FTrainRFA <- f_meas_vec(trainsetMet[, y], factor(predTrainRFAC$predict, levels = levels(testsetMet[, y])))
                   
                   KapTestRFA <- kap_vec(testsetMet[, y], factor(predTestRFAC$predict, levels = levels(testsetMet[, y])))
                   KapTrainRFA <- kap_vec(trainsetMet[, y], factor(predTrainRFAC$predict, levels = levels(testsetMet[, y])))
                   
                   MccTestRFA <- mcc_vec(testsetMet[, y], factor(predTestRFAC$predict, levels = levels(testsetMet[, y])))
                   MccTrainRFA <- mcc_vec(trainsetMet[, y], factor(predTrainRFAC$predict, levels = levels(testsetMet[, y])))
                   
                   BATestRFA <- bal_accuracy_vec(testsetMet[, y], factor(predTestRFAC$predict, levels = levels(testsetMet[, y])))
                   BATrainRFA <- bal_accuracy_vec(trainsetMet[, y], factor(predTrainRFAC$predict, levels = levels(testsetMet[, y])))
                   
                   PTestRFA <- precision_vec(testsetMet[, y], factor(predTestRFAC$predict, levels = levels(testsetMet[, y])))
                   PTrainRFA <- precision_vec(trainsetMet[, y], factor(predTrainRFAC$predict, levels = levels(testsetMet[, y])))
                   
                   waiter_hide()
                   
                   output$aucRF <- renderPrint({
                     auc_table
                   })
                   
                   if (input$auto2Learning) {
                     
                     output$ViewAuto2 <- renderUI({
                       
                       fluidRow(
                         column(5, pickerInput(inputId = ns('testTrainA2'),
                                               #selected = "",
                                               width = '100%',
                                               label = 'Показать результаты классификации на выборке:', 
                                               choices = list("Тестовая" = "Тестовая", "Обучающая" = "Обучающая"),
                                               options = list(`actions-box` = FALSE,
                                                              `none-selected-text` = "Ничего не выбрано"),
                                               multiple = F
                         )),
                         column(
                           2,
                           style = "margin-bottom: 10px;",
                           style = "margin-top: 25px;",
                           style = "margin-bottom: 10px;",
                           actionButton(ns("resultVA2"), "Отобразить")
                         ),
                         
                         uiOutput(ns("matrixA2View"))
                         
                       )})
                   }
                   
                   output$ViewResA2 <- renderUI({
                     
                     if (input$resultVA2){
                       
                       if (input$testTrainA2 == "Тестовая"){
                         str01 <- paste(strong("Модель-классификатор: "), "Случайный лес")
                         str02 <- paste(strong("Результаты получены на тестовой выборке"))
                         str1 <- paste("")
                         str2 <- paste(strong("MSE (оценка Брайера): "), round(h2o.mse(perfTestRFA), 4))
                         str3 <- paste(strong("RMSE: "), round(h2o.rmse(perfTestRFA), 4))
                         str4 <- paste(strong("Logloss: "), round(h2o.logloss(perfTestRFA), 4))
                         str5 <- paste(strong("AUC: "), round(h2o.auc(perfTestRFA), 4))
                         str6 <- paste(ATestRFA)
                         str7 <- paste(strong("F-мера: "), round(FTestRFA, 4))
                         str8 <- paste(strong("Каппа Коэна: "), round(KapTestRFA, 4))
                         str9 <- paste(strong("MCC (коэффициент Мэттьюса): "), round(MccTestRFA, 4))
                         str10 <- paste(strong("Сбалансированная точность: "), round(BATestRFA, 4))
                         str11 <- paste(strong("Точность (precision): "), round(PTestRFA, 4))
                         str12 <- paste(strong("Матрица ошибок: "))
                         HTML(paste(str01, str02, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str1, str12, sep = '<br/>'))
                       }
                       
                       else if (input$testTrainA2 == "Обучающая"){
                         str01 <- paste(strong("Модель-классификатор: "), "Случайный лес")
                         str02 <- paste(strong("Результаты получены на обучающей выборке"))
                         str1 <- paste("")
                         str2 <- paste(strong("MSE (оценка Брайера): "), round(h2o.mse(perfTrainRFA), 4))
                         str3 <- paste(strong("RMSE: "), round(h2o.rmse(perfTrainRFA), 4))
                         str4 <- paste(strong("Logloss: "), round(h2o.logloss(perfTrainRFA), 4))
                         str5 <- paste(strong("AUC: "), round(h2o.auc(perfTrainRFA), 4))
                         str6 <- paste(ATrainRFA)
                         str7 <- paste(strong("F-мера: "), round(FTrainRFA, 4))
                         str8 <- paste(strong("Каппа Коэна: "), round(KapTrainRFA, 4))
                         str9 <- paste(strong("MCC (коэффициент Мэттьюса): "), round(MccTrainRFA, 4))
                         str10 <- paste(strong("Сбалансированная точность: "), round(BATrainRFA, 4))
                         str11 <- paste(strong("Точность (precision): "), round(PTrainRFA, 4))
                         str12 <- paste(strong("Матрица ошибок: "))
                         HTML(paste(str01, str02, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str1, str12, sep = '<br/>'))
                       }
                     }
                   })
                   
                   
                   output$matrixA2 <- renderUI({
                     
                     if (input$resultVA2){
                       
                       if (input$testTrainA2 == "Тестовая"){
                         M <- matrixTestRFA
                         rownames(M) <- gsub(" ", ".", rownames(M))
                         colnames(M) <- gsub(" ", ".", colnames(M))
                         M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                                    floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
                         html <- paste0("$$", M, "$$")
                         list(
                           withMathJax(HTML(html))
                         )
                       }
                       
                       else if (input$testTrainA2 == "Обучающая"){
                         M <- matrixTrainRFA
                         rownames(M) <- gsub(" ", ".", rownames(M))
                         colnames(M) <- gsub(" ", ".", colnames(M))
                         M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                                    floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
                         html <- paste0("$$", M, "$$")
                         list(
                           withMathJax(HTML(html))
                         )
                       }
                     }
                   })
                   #})
                   
                   output$matrixA2View <- renderUI({
                     
                     if (input$resultVA2){
                       
                       fluidRow(column(
                         12,
                         box(
                           id = "resultA2",
                           title = "Результаты обучения",
                           status = "success",
                           solidHeader = TRUE,
                           width = 12,
                           uiOutput(ns("modelRF")),
                           uiOutput(ns("ViewResA2")),
                           uiOutput(ns("matrixA2")),
                           if (input$testTrainA2 == "Обучающая"){
                             str1 <- paste("")
                             str13 <- paste(strong("Оценка важности предикторов (диаграмма): "))
                             str14 <- paste(imageOutput(ns("plotA2")))
                             str15 <- paste(strong("Оценка важности предикторов: "))
                             str16 <- paste(verbatimTextOutput(ns("varimpA2")))
                             HTML(paste(str1, str13, str14, str15, str16, sep = '<br/>'))
                             #imageOutput(ns("plot"))
                           }
                           #plotOutput(ns("plot"))
                         ),
                         #column(width = 12, align = "center",
                         #       plotOutput(ns("plot")),)
                         #plotOutput(ns("plot")),
                         # column(
                         #   12, plotOutput(ns("plot")))
                         
                         
                       ),
                       
                       )
                     }
                   })
                 
                 
                 output$plotA2 <- renderPlot({
                   
                   if (input$resultVA2){
                     h2o.varimp_plot(best_rf)
                   }
                   
                 })
                 
                 output$varimpA2 <- renderPrint({
                   
                   if (input$resultVA2){
                     
                     if (input$testTrainA2 == "Обучающая"){
                       
                       as.data.frame(h2o.varimp(best_rf))
                       
                     }
                   }
                 })
               })
                 
                 
                 output$modelRF <- renderUI({
                   if (input$testTrainA2 == "Обучающая"){
                     fluidRow(column(12, strong("")),
                              column(12, strong("Результаты поиска оптимальных гиперпараметров модели:")),
                              column(
                                12, verbatimTextOutput(ns("aucRF"))))
                   }
                 })
                 
                 output$ViewRF <- renderUI({
                   
                   auto2ClassifLearning()
                   uiOutput(
                     ns("ViewAuto2"))
                   
                 })
                 
                 onStop(function() {
                   try({h2o.shutdown(prompt = FALSE)}, silent=TRUE)
                 })
                 
               })
}