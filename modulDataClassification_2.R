moduleUI_3.2 <- function(id, label = "moduleUI_3.2") {
  ns <- NS(id)
  #useWaiter()
  
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
    id = ns("tabsetClassif"),
    width = 12,

    tabPanel(
      "Одиночные классификаторы",
      fluidRow(column(12, uiOutput(
        ns("oClassifLearningView")
      )),
      column(12, uiOutput(ns("ViewO"))
      ))),

      
    tabPanel(
      "Базовые классификаторы и однородные ансамбли",
      useWaiter(),
      fluidRow(column(12, uiOutput(
        ns("bClassifLearningView")
      ),
      uiOutput(ns("View"))),
     # column(
    #    12,
    #    box(
     #     id = "classif3",
    #      title = "Ансамбли классификаторов",
    #      status = "success",
    #      solidHeader = TRUE,
    #      width = 12
    #    )
    #  )
      )
    ),
    tabPanel("Неоднородные ансамбли классификаторов",
             fluidRow(column(12, uiOutput(
             ns('stackingClassifLearningView')
             ),
             uiOutput(ns("ViewS"))
             ))),
  ))
  
}


moduleServer_3.2 <- function(id, rvClass, prefix = "") {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 modelsO <- reactiveValues(mNN = NULL, mT = NULL, mTF = NULL)
                 
                 setsH2O <- reactiveValues(test_df_h2o = NULL, train_df_h2o = NULL)
                 setsMet <- reactiveValues(testsetMet = NULL, trainsetMet = NULL)
                 xy <- reactiveValues(x = NULL, y = NULL)
                 
                 rvAcc <- reactiveValues(d=data.frame())
                 models <- reactiveValues(mNB = NULL, mLR = NULL, mRF20 = NULL, mRF6 = NULL, 
                                          mRF = NULL, mXGB = NULL, mE = NULL)
                 
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
                 
                 output$oClassifLearningView <- renderUI({
                   if (!is.null(dataTrain()) && !is.null(dataTest())) {
                     options(spinner.color="#00a65a")
                     fluidRow(
                       column(
                         9,
                         tags$style(
                           HTML(
                             "
                           .selectize-input {
                           width: 100%;
                           }
                           .selectize-input .active {
                           background: #00a65a !important;
                           color: black !important;
                           }
                           "
                           )
                         ),
                         selectInput(
                           inputId = ns('oClassifLearning'),
                           selected = "",
                           width = '100%',
                           label = 'Выбор метода решения задачи классификации:',
                           choices = list(
                             "Нейросетевой метод (персептрон)" = "Нейросетевой метод (персептрон)",
                             "Дерево принятия решений" = "Дерево принятия решений"
                           ),
                           
                           multiple = T,
                           selectize = TRUE,
                         )
                       ),
                       column(
                         3,
                         style = "margin-bottom: 10px;",
                         style = "margin-top: 25px;",
                         style = "margin-bottom: 10px;",
                         actionButton(ns("oLearning"), "Обучить")
                       ),
                     
                       #column(12, uiOutput(ns("balanceView")))
                       
                       
                     )
                   }
                 })
                 
                 output$bClassifLearningView <- renderUI({
                   if (!is.null(dataTrain()) && !is.null(dataTest())) {
                     options(spinner.color="#00a65a")
                     fluidRow(
                       column(
                         9,
                         tags$style(
                           HTML(
                             "
                           .selectize-input {
                           width: 100%;
                           }
                           .selectize-input .active {
                           background: #00a65a !important;
                           color: black !important;
                           }
                           "
                           )
                         ),
                         selectInput(
                           inputId = ns('bClassifLearning'),
                           selected = "",
                           width = '100%',
                           label = 'Выбор метода решения задачи классификации:',
                           choices = list(
                             "Наивный Байес" = "Наивный Байес",
                             "Мультиномиальная логистическая регрессия" = "Мультиномиальная логистическая регрессия",
                             "Дерево с максимальной глубиной равной 20" = "Дерево с максимальной глубиной равной 20",
                             "Дерево с максимальной глубиной равной 6" = "Дерево с максимальной глубиной равной 6",
                             "Случайный лес" = "Случайный лес",
                             "Градиентный бустинг" = "Градиентный бустинг"
                           ),
                           
                           multiple = T,
                           selectize = TRUE,
                         )
                       ),
                       column(
                         3,
                         style = "margin-bottom: 10px;",
                         style = "margin-top: 25px;",
                         style = "margin-bottom: 10px;",
                         actionButton(ns("learning"), "Обучить")
                       ),
                       #column(12, uiOutput(ns("balanceView")))
                       
                       
                     )
                   }
                 })
                 
                 output$stackingClassifLearningView <- renderUI({
                   if (!is.null(dataTrain()) && !is.null(dataTest()) && !is.null(input$bClassifLearning)) {
                     options(spinner.color="#00a65a")
               #      fluidRow(
               #        column(
               #          5,
               #          tags$style(
               #            HTML(
               #              "
                #           .selectize-input {
                #           width: 100%;
                #           }
                 #          .selectize-input .active {
                #           background: #00a65a !important;
                #           color: black !important;
                #           }
                #           "
                #           )
                #         ),
                         fluidRow(
                           column(5, pickerInput(inputId = ns('stackingClassifLearning'),
                                                 #selected = "",
                                                 width = '100%',
                                                 label = 'Выбор мета-классификатора:', 
                                                 choices = list(
                                                   "Наивный Байес" = "Наивный Байес",
                                                   "Мультиномиальная логистическая регрессия" = "Мультиномиальная логистическая регрессия",
                                                   "Случайный лес" = "Случайный лес",
                                                   "Градиентный бустинг" = "Градиентный бустинг"
                                                 ),
                                                 options = list(`actions-box` = FALSE,
                                                                `none-selected-text` = "Ничего не выбрано"),
                                                 multiple = F
                           )), 
              #           selectInput(
              #             inputId = ns('stackingClassifLearning'),
              #             selected = "",
              #             width = '100%',
              #             label = 'Выбор мета-классификатора:',
              #             choices = list(
              #               "Наивный Байес" = "Наивный Байес",
              #               "Мультиномиальная логистическая регрессия" = "Мультиномиальная логистическая регрессия",
              #               "Случайный лес" = "Случайный лес",
              #               "Градиентный бустинг" = "Градиентный бустинг"
              #             ),
                           
              #             multiple = T,
              #             selectize = TRUE,
              #           )
              #         ),
                       column(
                         5,
                         tags$style(
                           HTML(
                             "
                           .selectize-input {
                           width: 100%;
                           }
                           .selectize-input .active {
                           background: #00a65a !important;
                           color: black !important;
                           }
                           "
                           )
                         ),
                       selectInput(
                         inputId = ns('bstackingClassifLearning'),
                         selected = "",
                         width = '100%',
                         label = 'Выбор базовых алгоритмов:',
                         choices = as.list(input$bClassifLearning),
                         multiple = T,
                         selectize = TRUE,
                       )
                     ),
                       column(
                         2,
                         style = "margin-bottom: 10px;",
                         style = "margin-top: 25px;",
                         style = "margin-bottom: 10px;",
                         actionButton(ns("stackingLearning"), "Обучить")
                       ),
                 )
                   }
                   
                 })
                 
                 
                 bClassifLearning <- eventReactive(input$learning, {
                   
                  # waiter_show( # show the waiter
                  #   html = spin_solar(), # use a spinner
                  #     "Подождите, алгоритмы обучаются...",
                  #   color = "#00a65a"
                  # )
                   
                   waiter_show( # show the waiter
                     color = "#00a65a",
                     html = tagList(
                       spin_solar(), # use a spinner
                       "Подождите, алгоритмы обучаются..."
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
                   
                   #naiveBayes <- function(){
                   if ("Наивный Байес" %in% input$bClassifLearning){
                     m_nb <- h2o.naiveBayes(x = x,
                                             y = y,
                                             training_frame = train_df_h2o,
                                             validation_frame = test_df_h2o,
                                             nfolds = 10,
                                             #fold_column = "Засорённость.класс",
                                             fold_assignment = "Modulo",
                                             keep_cross_validation_predictions = TRUE,
                                             auc_type="WEIGHTED_OVO",
                                             seed = 101)
                     models$mNB <- m_nb
                     
                     predTestNB <- predict(m_nb, test_df_h2o, type = "class")
                     AccTestNB <- mean(predTestNB == test_df_h2o[, targetVar()])
                     ATestNB <- paste(strong("Точность (accuracy): "), round(AccTestNB, 4), sep = "")
                     ATestNB <- ATestNB[c(1)]
                     rvAcc$d <- ATestNB
                     
                     predTrainNB <- predict(m_nb, train_df_h2o, type = "class")
                     AccTrainNB <- mean(predTrainNB == train_df_h2o[, targetVar()])
                     ATrainNB <- paste(strong("Точность (accuracy): "), round(AccTrainNB, 4), sep = "")
                     ATrainNB <- ATrainNB[c(1)]
                     
                     #print(A)
                     #print(str(test_df_h2o))
                     ###print(my_nb)
                     perfTestNB <- h2o.performance(m_nb, test_df_h2o)
                     perfTrainNB <- h2o.performance(m_nb, train_df_h2o)
                     ###h2o.mse(perf)
                     #print(h2o.mse(my_nb, valid = test_df_h2o))
                     #print(h2o.mse(my_nb, valid))
                     #print(h2o.confusionMatrix(my_nb,test_df_h2o))
                     
                     #ВАЖНО  print(paste("Тестовая"))
                     #ВАЖНО  print(paste("MSE (оценка Брайера): ", round(h2o.mse(perf), 4)))
                     #ВАЖНО   print(paste("RMSE: ", round(h2o.rmse(perf), 4)))
                     #ВАЖНО    print(paste("Logloss: ", round(h2o.logloss(perf), 4)))
                     #ВАЖНО    print(paste("AUC: ", round(h2o.auc(perf), 4)))
                     #ВАЖНО    print(paste("Матрица ошибок: "))
                     colTestNB <- ncol(h2o.confusionMatrix(m_nb,test_df_h2o))
                     colTrainNB <- ncol(h2o.confusionMatrix(m_nb,train_df_h2o))
                     #col
                     #paste(as.table(h2o.confusionMatrix(my_nb,test_df_h2o)[,1:col]))
                  #ВАЖНО   print(h2o.confusionMatrix(my_nb,test_df_h2o)[,1:col])
                     matrixTestNB <- h2o.confusionMatrix(m_nb,test_df_h2o)[,1:colTestNB]
                     matrixTrainNB <- h2o.confusionMatrix(m_nb,train_df_h2o)[,1:colTrainNB]
                   #  print("----")
                   #  print(capture.output(a))
                   #  print("----")
                   #  print(capture.output(a))
                   #  print("-3452345---")
                     ####!!!!!matrixTest = paste(cat(capture.output(aaaa), sep = "\n"))
                     #print(paste(h2o.confusionMatrix(my_nb,test_df_h2o)[,1:col]))
                     #as.table(h2o.confusionMatrix(my_nb,test_df_h2o))

                    # return(listperf)
                     
                     predTestNBC <- as.data.frame(predTestNB[1])
                     predTrainNBC <- as.data.frame(predTrainNB[1])
                     
                     #ttttttt <- dataTest()
                     #a<- as.data.frame(pred[1])
                     
                     #a<- as.data.frame(pred[1])
                     
                     
                     FTestNB <- f_meas_vec(testsetMet[, y], factor(predTestNBC$predict, levels = levels(testsetMet[, y])))
                     FTrainNB <- f_meas_vec(trainsetMet[, y], factor(predTrainNBC$predict, levels = levels(testsetMet[, y])))
                     
                     KapTestNB <- kap_vec(testsetMet[, y], factor(predTestNBC$predict, levels = levels(testsetMet[, y])))
                     KapTrainNB <- kap_vec(trainsetMet[, y], factor(predTrainNBC$predict, levels = levels(testsetMet[, y])))
                     
                     MccTestNB <- mcc_vec(testsetMet[, y], factor(predTestNBC$predict, levels = levels(testsetMet[, y])))
                     MccTrainNB <- mcc_vec(trainsetMet[, y], factor(predTrainNBC$predict, levels = levels(testsetMet[, y])))
                     
                     BATestNB <- bal_accuracy_vec(testsetMet[, y], factor(predTestNBC$predict, levels = levels(testsetMet[, y])))
                     BATrainNB <- bal_accuracy_vec(trainsetMet[, y], factor(predTrainNBC$predict, levels = levels(testsetMet[, y])))
                     
                     PTestNB <- precision_vec(testsetMet[, y], factor(predTestNBC$predict, levels = levels(testsetMet[, y])))
                     PTrainNB <- precision_vec(trainsetMet[, y], factor(predTrainNBC$predict, levels = levels(testsetMet[, y])))
                   }
                   
                   if ("Мультиномиальная логистическая регрессия" %in% input$bClassifLearning){
                     m_lr <- h2o.glm(x = x,
                                     y = y,
                                     family = "multinomial",
                                     training_frame = train_df_h2o,
                                     validation_frame = test_df_h2o,
                                     nfolds = 10,
                                     #fold_column = "Засорённость.класс",
                                     fold_assignment = "Modulo",
                                     keep_cross_validation_predictions = TRUE,
                                     auc_type="WEIGHTED_OVO",
                                     seed = 101)
                     
                     models$mLR <- m_lr
                     
                     predTestLR <- predict(m_lr, test_df_h2o, type = "class")
                     AccTestLR <- mean(predTestLR == test_df_h2o[, targetVar()])
                     ATestLR <- paste(strong("Точность (accuracy): "), round(AccTestLR, 4), sep = "")
                     ATestLR <- ATestLR[c(1)]
                     
                     predTrainLR <- predict(m_lr, train_df_h2o, type = "class")
                     AccTrainLR <- mean(predTrainLR == train_df_h2o[, targetVar()])
                     ATrainLR <- paste(strong("Точность (accuracy): "), round(AccTrainLR, 4), sep = "")
                     ATrainLR <- ATrainLR[c(1)]
                     
                     perfTestLR <- h2o.performance(m_lr, test_df_h2o)
                     perfTrainLR <- h2o.performance(m_lr, train_df_h2o)
                     
                     colTestLR <- ncol(h2o.confusionMatrix(m_lr,test_df_h2o))
                     colTrainLR <- ncol(h2o.confusionMatrix(m_lr,train_df_h2o))
                     
                     matrixTestLR = h2o.confusionMatrix(m_lr,test_df_h2o)[,1:colTestLR]
                     matrixTrainLR = h2o.confusionMatrix(m_lr,train_df_h2o)[,1:colTrainLR]
                     
                     predTestLRC <- as.data.frame(predTestLR[1])
                     predTrainLRC <- as.data.frame(predTrainLR[1])
                     
                     #print(predTestLRC)
                     
                     FTestLR <- f_meas_vec(testsetMet[, y], factor(predTestLRC$predict, levels = levels(testsetMet[, y])))
                     FTrainLR <- f_meas_vec(trainsetMet[, y], factor(predTrainLRC$predict, levels = levels(testsetMet[, y])))
                     
                     KapTestLR <- kap_vec(testsetMet[, y], factor(predTestLRC$predict, levels = levels(testsetMet[, y])))
                     KapTrainLR <- kap_vec(trainsetMet[, y], factor(predTrainLRC$predict, levels = levels(testsetMet[, y])))
                     
                     MccTestLR <- mcc_vec(testsetMet[, y], factor(predTestLRC$predict, levels = levels(testsetMet[, y])))
                     MccTrainLR <- mcc_vec(trainsetMet[, y], factor(predTrainLRC$predict, levels = levels(testsetMet[, y])))
                     
                     BATestLR <- bal_accuracy_vec(testsetMet[, y], factor(predTestLRC$predict, levels = levels(testsetMet[, y])))
                     BATrainLR <- bal_accuracy_vec(trainsetMet[, y], factor(predTrainLRC$predict, levels = levels(testsetMet[, y])))
                     
                     PTestLR <- precision_vec(testsetMet[, y], factor(predTestLRC$predict, levels = levels(testsetMet[, y])))
                     PTrainLR <- precision_vec(trainsetMet[, y], factor(predTrainLRC$predict, levels = levels(testsetMet[, y])))
                   }
                   
                   if ("Дерево с максимальной глубиной равной 20" %in% input$bClassifLearning){
                     m_rf20 <- h2o.randomForest(x = x,
                                              y = y,
                                              #distribution = "multinomial",
                                              ntrees = 1,
                                              training_frame = train_df_h2o,
                                              validation_frame = test_df_h2o,
                                              categorical_encoding = "OneHotExplicit",
                                              nfolds = 10,
                                              fold_assignment = "Modulo",
                                              keep_cross_validation_predictions = TRUE,
                                              auc_type="WEIGHTED_OVO",
                                              seed = 101)
                     
                     #print(m_rf20)
                     models$mRF20 <- m_rf20
                     
                     predTestRF20 <- predict(m_rf20, test_df_h2o, type = "class")
                     AccTestRF20 <- mean(predTestRF20 == test_df_h2o[, targetVar()])
                     ATestRF20 <- paste(strong("Точность (accuracy): "), round(AccTestRF20, 4), sep = "")
                     ATestRF20 <- ATestRF20[c(1)]
                     
                     predTrainRF20 <- predict(m_rf20, train_df_h2o, type = "class")
                     AccTrainRF20 <- mean(predTrainRF20 == train_df_h2o[, targetVar()])
                     ATrainRF20 <- paste(strong("Точность (accuracy): "), round(AccTrainRF20, 4), sep = "")
                     ATrainRF20 <- ATrainRF20[c(1)]
                     
                     perfTestRF20 <- h2o.performance(m_rf20, test_df_h2o)
                     perfTrainRF20 <- h2o.performance(m_rf20, train_df_h2o)
                     
                     colTestRF20 <- ncol(h2o.confusionMatrix(m_rf20,test_df_h2o))
                     colTrainRF20 <- ncol(h2o.confusionMatrix(m_rf20,train_df_h2o))
                     
                     matrixTestRF20 = h2o.confusionMatrix(m_rf20,test_df_h2o)[,1:colTestRF20]
                     matrixTrainRF20 = h2o.confusionMatrix(m_rf20,train_df_h2o)[,1:colTrainRF20]
                     
                     predTestRF20C <- as.data.frame(predTestRF20[1])
                     predTrainRF20C <- as.data.frame(predTrainRF20[1])
                     
                     FTestRF20 <- f_meas_vec(testsetMet[, y], factor(predTestRF20C$predict, levels = levels(testsetMet[, y])))
                     FTrainRF20 <- f_meas_vec(trainsetMet[, y], factor(predTrainRF20C$predict, levels = levels(testsetMet[, y])))
                     
                     KapTestRF20 <- kap_vec(testsetMet[, y], factor(predTestRF20C$predict, levels = levels(testsetMet[, y])))
                     KapTrainRF20 <- kap_vec(trainsetMet[, y], factor(predTrainRF20C$predict, levels = levels(testsetMet[, y])))
                     
                     MccTestRF20 <- mcc_vec(testsetMet[, y], factor(predTestRF20C$predict, levels = levels(testsetMet[, y])))
                     MccTrainRF20 <- mcc_vec(trainsetMet[, y], factor(predTrainRF20C$predict, levels = levels(testsetMet[, y])))
                     
                     BATestRF20 <- bal_accuracy_vec(testsetMet[, y], factor(predTestRF20C$predict, levels = levels(testsetMet[, y])))
                     BATrainRF20 <- bal_accuracy_vec(trainsetMet[, y], factor(predTrainRF20C$predict, levels = levels(testsetMet[, y])))
                     
                     PTestRF20 <- precision_vec(testsetMet[, y], factor(predTestRF20C$predict, levels = levels(testsetMet[, y])))
                     PTrainRF20 <- precision_vec(trainsetMet[, y], factor(predTrainRF20C$predict, levels = levels(testsetMet[, y])))
    
                   }
                   
                   if ("Дерево с максимальной глубиной равной 6" %in% input$bClassifLearning){
                     m_xgb6 <- h2o.xgboost(x = x,
                                          y = y,
                                          ntrees = 1,
                                          #distribution = "multinomial",
                                          training_frame = train_df_h2o,
                                          validation_frame = test_df_h2o,
                                          categorical_encoding = "OneHotExplicit",
                                          nfolds = 10,
                                          fold_assignment = "Modulo",
                                          keep_cross_validation_predictions = TRUE,
                                          auc_type="WEIGHTED_OVO",
                                          seed = 101)
                     #print(m_xgb6)
                     models$mXGB6 <- m_xgb6
                     
                     predTestXGB6 <- predict(m_xgb6, test_df_h2o, type = "class")
                     AccTestXGB6 <- mean(predTestXGB6 == test_df_h2o[, targetVar()])
                     ATestXGB6 <- paste(strong("Точность (accuracy): "), round(AccTestXGB6, 4), sep = "")
                     ATestXGB6 <- ATestXGB6[c(1)]
                     
                     predTrainXGB6 <- predict(m_xgb6, train_df_h2o, type = "class")
                     AccTrainXGB6 <- mean(predTrainXGB6 == train_df_h2o[, targetVar()])
                     ATrainXGB6 <- paste(strong("Точность (accuracy): "), round(AccTrainXGB6, 4), sep = "")
                     ATrainXGB6 <- ATrainXGB6[c(1)]
                     
                     perfTestXGB6 <- h2o.performance(m_xgb6, test_df_h2o)
                     perfTrainXGB6 <- h2o.performance(m_xgb6, train_df_h2o)
                     
                     colTestXGB6 <- ncol(h2o.confusionMatrix(m_xgb6,test_df_h2o))
                     colTrainXGB6 <- ncol(h2o.confusionMatrix(m_xgb6,train_df_h2o))
                     
                     matrixTestXGB6 = h2o.confusionMatrix(m_xgb6,test_df_h2o)[,1:colTestXGB6]
                     matrixTrainXGB6 = h2o.confusionMatrix(m_xgb6,train_df_h2o)[,1:colTrainXGB6]
                     
                     #  output$plotXGB <- renderPlot({
                     #input$newplot
                     # Add a little noise to the cars data
                     #cars2 <- cars + rnorm(nrow(cars))
                     #    plot(h2o.varimp_plot(m_xgb))
                     #  })
                     #varPlotXGB <- h2o.varimp_plot(m_xgb)
                     
                     predTestXGB6C <- as.data.frame(predTestXGB6[1])
                     predTrainXGB6C <- as.data.frame(predTrainXGB6[1])
                     
                     FTestXGB6 <- f_meas_vec(testsetMet[, y], factor(predTestXGB6C$predict, levels = levels(testsetMet[, y])))
                     FTrainXGB6 <- f_meas_vec(trainsetMet[, y], factor(predTrainXGB6C$predict, levels = levels(testsetMet[, y])))
                     
                     KapTestXGB6 <- kap_vec(testsetMet[, y], factor(predTestXGB6C$predict, levels = levels(testsetMet[, y])))
                     KapTrainXGB6 <- kap_vec(trainsetMet[, y], factor(predTrainXGB6C$predict, levels = levels(testsetMet[, y])))
                     
                     MccTestXGB6 <- mcc_vec(testsetMet[, y], factor(predTestXGB6C$predict, levels = levels(testsetMet[, y])))
                     MccTrainXGB6 <- mcc_vec(trainsetMet[, y], factor(predTrainXGB6C$predict, levels = levels(testsetMet[, y])))
                     
                     BATestXGB6 <- bal_accuracy_vec(testsetMet[, y], factor(predTestXGB6C$predict, levels = levels(testsetMet[, y])))
                     BATrainXGB6 <- bal_accuracy_vec(trainsetMet[, y], factor(predTrainXGB6C$predict, levels = levels(testsetMet[, y])))
                     
                     PTestXGB6 <- precision_vec(testsetMet[, y], factor(predTestXGB6C$predict, levels = levels(testsetMet[, y])))
                     PTrainXGB6 <- precision_vec(trainsetMet[, y], factor(predTrainXGB6C$predict, levels = levels(testsetMet[, y])))
                   }
                   
                   if ("Случайный лес" %in% input$bClassifLearning){
                     m_rf <- h2o.randomForest(x = x,
                                               y = y,
                                               #distribution = "multinomial",
                                               ntrees = 50,
                                               training_frame = train_df_h2o,
                                               validation_frame = test_df_h2o,
                                               categorical_encoding = "OneHotExplicit",
                                               nfolds = 10,
                                               fold_assignment = "Modulo",
                                               keep_cross_validation_predictions = TRUE,
                                               auc_type="WEIGHTED_OVO",
                                               seed = 101)
                     
                     models$mRF <- m_rf

                     
                     predTestRF <- predict(m_rf, test_df_h2o, type = "class")
                     AccTestRF <- mean(predTestRF == test_df_h2o[, targetVar()])
                     ATestRF <- paste(strong("Точность (accuracy): "), round(AccTestRF, 4), sep = "")
                     ATestRF <- ATestRF[c(1)]
                     
                     predTrainRF <- predict(m_rf, train_df_h2o, type = "class")
                     AccTrainRF <- mean(predTrainRF == train_df_h2o[, targetVar()])
                     ATrainRF <- paste(strong("Точность (accuracy): "), round(AccTrainRF, 4), sep = "")
                     ATrainRF <- ATrainRF[c(1)]
                     
                     perfTestRF <- h2o.performance(m_rf, test_df_h2o)
                     perfTrainRF <- h2o.performance(m_rf, train_df_h2o)
                     
                     colTestRF <- ncol(h2o.confusionMatrix(m_rf,test_df_h2o))
                     colTrainRF <- ncol(h2o.confusionMatrix(m_rf,train_df_h2o))
                     
                     matrixTestRF = h2o.confusionMatrix(m_rf,test_df_h2o)[,1:colTestRF]
                     matrixTrainRF = h2o.confusionMatrix(m_rf,train_df_h2o)[,1:colTrainRF]
                     
                     predTestRFC <- as.data.frame(predTestRF[1])
                     predTrainRFC <- as.data.frame(predTrainRF[1])
                     
                     FTestRF <- f_meas_vec(testsetMet[, y], factor(predTestRFC$predict, levels = levels(testsetMet[, y])))
                     FTrainRF <- f_meas_vec(trainsetMet[, y], factor(predTrainRFC$predict, levels = levels(testsetMet[, y])))
                     
                     KapTestRF <- kap_vec(testsetMet[, y], factor(predTestRFC$predict, levels = levels(testsetMet[, y])))
                     KapTrainRF <- kap_vec(trainsetMet[, y], factor(predTrainRFC$predict, levels = levels(testsetMet[, y])))
                     
                     MccTestRF <- mcc_vec(testsetMet[, y], factor(predTestRFC$predict, levels = levels(testsetMet[, y])))
                     MccTrainRF <- mcc_vec(trainsetMet[, y], factor(predTrainRFC$predict, levels = levels(testsetMet[, y])))
                     
                     BATestRF <- bal_accuracy_vec(testsetMet[, y], factor(predTestRFC$predict, levels = levels(testsetMet[, y])))
                     BATrainRF <- bal_accuracy_vec(trainsetMet[, y], factor(predTrainRFC$predict, levels = levels(testsetMet[, y])))
                     
                     PTestRF <- precision_vec(testsetMet[, y], factor(predTestRFC$predict, levels = levels(testsetMet[, y])))
                     PTrainRF <- precision_vec(trainsetMet[, y], factor(predTrainRFC$predict, levels = levels(testsetMet[, y])))
                     
                   }
                   
                   if ("Градиентный бустинг" %in% input$bClassifLearning){
                     m_xgb <- h2o.xgboost(x = x,
                                           y = y,
                                           ntrees = 50,
                                           #distribution = "multinomial",
                                           training_frame = train_df_h2o,
                                           validation_frame = test_df_h2o,
                                           categorical_encoding = "OneHotExplicit",
                                           nfolds = 10,
                                           fold_assignment = "Modulo",
                                           keep_cross_validation_predictions = TRUE,
                                           auc_type="WEIGHTED_OVO",
                                           seed = 101)
                     
                     models$mXGB <- m_xgb
                     
                     predTestXGB <- predict(m_xgb, test_df_h2o, type = "class")
                     AccTestXGB <- mean(predTestXGB == test_df_h2o[, targetVar()])
                     ATestXGB <- paste(strong("Точность (accuracy): "), round(AccTestXGB, 4), sep = "")
                     ATestXGB <- ATestXGB[c(1)]
                     
                     predTrainXGB <- predict(m_xgb, train_df_h2o, type = "class")
                     AccTrainXGB <- mean(predTrainXGB == train_df_h2o[, targetVar()])
                     ATrainXGB <- paste(strong("Точность (accuracy): "), round(AccTrainXGB, 4), sep = "")
                     ATrainXGB <- ATrainXGB[c(1)]
                     
                     perfTestXGB <- h2o.performance(m_xgb, test_df_h2o)
                     perfTrainXGB <- h2o.performance(m_xgb, train_df_h2o)
                     
                     colTestXGB <- ncol(h2o.confusionMatrix(m_xgb,test_df_h2o))
                     colTrainXGB <- ncol(h2o.confusionMatrix(m_xgb,train_df_h2o))
                     
                     matrixTestXGB = h2o.confusionMatrix(m_xgb,test_df_h2o)[,1:colTestXGB]
                     matrixTrainXGB = h2o.confusionMatrix(m_xgb,train_df_h2o)[,1:colTrainXGB]
                     
                   #  output$plotXGB <- renderPlot({
                       #input$newplot
                       # Add a little noise to the cars data
                       #cars2 <- cars + rnorm(nrow(cars))
                   #    plot(h2o.varimp_plot(m_xgb))
                   #  })
                     #varPlotXGB <- h2o.varimp_plot(m_xgb)
                     
                     predTestXGBC <- as.data.frame(predTestXGB[1])
                     predTrainXGBC <- as.data.frame(predTrainXGB[1])
                     
                     FTestXGB <- f_meas_vec(testsetMet[, y], factor(predTestXGBC$predict, levels = levels(testsetMet[, y])))
                     FTrainXGB <- f_meas_vec(trainsetMet[, y], factor(predTrainXGBC$predict, levels = levels(testsetMet[, y])))
                     
                     KapTestXGB <- kap_vec(testsetMet[, y], factor(predTestXGBC$predict, levels = levels(testsetMet[, y])))
                     KapTrainXGB <- kap_vec(trainsetMet[, y], factor(predTrainXGBC$predict, levels = levels(testsetMet[, y])))
                     
                     MccTestXGB <- mcc_vec(testsetMet[, y], factor(predTestXGBC$predict, levels = levels(testsetMet[, y])))
                     MccTrainXGB <- mcc_vec(trainsetMet[, y], factor(predTrainXGBC$predict, levels = levels(testsetMet[, y])))
                     
                     BATestXGB <- bal_accuracy_vec(testsetMet[, y], factor(predTestXGBC$predict, levels = levels(testsetMet[, y])))
                     BATrainXGB <- bal_accuracy_vec(trainsetMet[, y], factor(predTrainXGBC$predict, levels = levels(testsetMet[, y])))
                     
                     PTestXGB <- precision_vec(testsetMet[, y], factor(predTestXGBC$predict, levels = levels(testsetMet[, y])))
                     PTrainXGB <- precision_vec(trainsetMet[, y], factor(predTrainXGBC$predict, levels = levels(testsetMet[, y])))
                   }

                   waiter_hide()
                   
                   
                  # if (input$learning) {
                     observeEvent(input$learning, {
                     #naiveBayes()

                     
                     #options(spinner.color="#00a65a")
                     output$ViewNB <- renderUI({
                       #options(spinner.color="#00a65a")
                       fluidRow(
                         column(5, pickerInput(inputId = ns('modelClass'),
                                               #selected = "",
                                               width = '100%',
                                               label = 'Выбор обученной модели:', 
                                               choices = as.list(input$bClassifLearning),
                                               options = list(`actions-box` = FALSE,
                                                              `none-selected-text` = "Ничего не выбрано"),
                                               multiple = F
                         )), 
                         column(5, pickerInput(inputId = ns('testTrain'),
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
                           actionButton(ns("resultV"), "Отобразить")
                         ),
                         
                        # column(12, uiOutput(
                       #    ns("resultView"))),
                         #column(12, uiOutput(ns("ViewRes"))),
                       
                       uiOutput(ns("matrixView"))
                       
                         
                         )
                       #waiter_hide()
                       })
                     })
                       
                    #? })
                     
                     #my_nb
                     #}
                   
                 
                   
                 #  if (input$bClassifLearning == 2){}
                   
                 #  if (input$bClassifLearning == 3){
                     #randomForest()
                 #  }
                   
                   #randomForest <- function(){

                  # }
                   
                   
                #   if (input$bClassifLearning == 4) {
                     #nb()
                #   }
                   
                 #})
                     
                    
                   
                   output$ViewRes <- renderUI({
                     options(spinner.color="#00a65a")
                     if (input$resultV){
                       options(spinner.color="#00a65a")
                     if (input$modelClass == "Наивный Байес" && input$testTrain == "Тестовая"){
                     str01 <- paste(strong("Модель-классификатор: "), "Наивный Байес")
                     str02 <- paste(strong("Результаты получены на тестовой выборке"))
                     str1 <- paste("")
                     str2 <- paste(strong("MSE (оценка Брайера): "), round(h2o.mse(perfTestNB), 4))
                     str3 <- paste(strong("RMSE: "), round(h2o.rmse(perfTestNB), 4))
                     str4 <- paste(strong("Logloss: "), round(h2o.logloss(perfTestNB), 4))
                     str5 <- paste(strong("AUC: "), round(h2o.auc(perfTestNB), 4))
                     str6 <- paste(ATestNB)
                     str7 <- paste(strong("F-мера: "), round(FTestNB, 4))
                     str8 <- paste(strong("Каппа Коэна: "), round(KapTestNB, 4))
                     str9 <- paste(strong("MCC (коэффициент Мэттьюса): "), round(MccTestNB, 4))
                     str10 <- paste(strong("Сбалансированная точность: "), round(BATestNB, 4))
                     str11 <- paste(strong("Точность (precision): "), round(PTestNB, 4))
                     str12 <- paste(strong("Матрица ошибок: "))
                     HTML(paste(str01, str02, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str1, str12, sep = '<br/>'))
                     }
                     
                     else if (input$modelClass == "Наивный Байес" && input$testTrain == "Обучающая"){
                       str01 <- paste(strong("Модель-классификатор: "), "Наивный Байес")
                       str02 <- paste(strong("Результаты получены на обучающей выборке"))
                       str1 <- paste("")
                       str2 <- paste(strong("MSE (оценка Брайера): "), round(h2o.mse(perfTrainNB), 4))
                       str3 <- paste(strong("RMSE: "), round(h2o.rmse(perfTrainNB), 4))
                       str4 <- paste(strong("Logloss: "), round(h2o.logloss(perfTrainNB), 4))
                       str5 <- paste(strong("AUC: "), round(h2o.auc(perfTrainNB), 4))
                       str6 <- paste(ATrainNB)
                       str7 <- paste(strong("F-мера: "), round(FTrainNB, 4))
                       str8 <- paste(strong("Каппа Коэна: "), round(KapTrainNB, 4))
                       str9 <- paste(strong("MCC (коэффициент Мэттьюса): "), round(MccTrainNB, 4))
                       str10 <- paste(strong("Сбалансированная точность: "), round(BATrainNB, 4))
                       str11 <- paste(strong("Точность (precision): "), round(PTrainNB, 4))
                       str12 <- paste(strong("Матрица ошибок: "))
                       HTML(paste(str01, str02, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str1, str12, sep = '<br/>'))
                     }
                    
            
                       else if (input$modelClass == "Мультиномиальная логистическая регрессия" && input$testTrain == "Тестовая"){
                         str01 <- paste(strong("Модель-классификатор: "), "Мультиномиальная логистическая регрессия")
                         str02 <- paste(strong("Результаты получены на тестовой выборке"))
                         str1 <- paste("")
                         str2 <- paste(strong("MSE (оценка Брайера): "), round(h2o.mse(perfTestLR), 4))
                         str3 <- paste(strong("RMSE: "), round(h2o.rmse(perfTestLR), 4))
                         str4 <- paste(strong("Logloss: "), round(h2o.logloss(perfTestLR), 4))
                         str5 <- paste(strong("AUC: "), round(h2o.auc(perfTestLR), 4))
                         str6 <- paste(ATestLR)
                         str7 <- paste(strong("F-мера: "), round(FTestLR, 4))
                         str8 <- paste(strong("Каппа Коэна: "), round(KapTestLR, 4))
                         str9 <- paste(strong("MCC (коэффициент Мэттьюса): "), round(MccTestLR, 4))
                         str10 <- paste(strong("Сбалансированная точность: "), round(BATestLR, 4))
                         str11 <- paste(strong("Точность (precision): "), round(PTestLR, 4))
                         str12 <- paste(strong("Матрица ошибок: "))
                         HTML(paste(str01, str02, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str1, str12, sep = '<br/>'))
                       }
                       
                       else if (input$modelClass == "Мультиномиальная логистическая регрессия" && input$testTrain == "Обучающая"){
                         str01 <- paste(strong("Модель-классификатор: "), "Мультиномиальная логистическая регрессия")
                         str02 <- paste(strong("Результаты получены на обучающей выборке"))
                         str1 <- paste("")
                         str2 <- paste(strong("MSE (оценка Брайера): "), round(h2o.mse(perfTrainLR), 4))
                         str3 <- paste(strong("RMSE: "), round(h2o.rmse(perfTrainLR), 4))
                         str4 <- paste(strong("Logloss: "), round(h2o.logloss(perfTrainLR), 4))
                         str5 <- paste(strong("AUC: "), round(h2o.auc(perfTrainLR), 4))
                         str6 <- paste(ATrainLR)
                         str7 <- paste(strong("F-мера: "), round(FTrainLR, 4))
                         str8 <- paste(strong("Каппа Коэна: "), round(KapTrainLR, 4))
                         str9 <- paste(strong("MCC (коэффициент Мэттьюса): "), round(MccTrainLR, 4))
                         str10 <- paste(strong("Сбалансированная точность: "), round(BATrainLR, 4))
                         str11 <- paste(strong("Точность (precision): "), round(PTrainLR, 4))
                         str12 <- paste(strong("Матрица ошибок: "))
                         HTML(paste(str01, str02, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str1, str12, sep = '<br/>'))
                       }

                       else if (input$modelClass == "Дерево с максимальной глубиной равной 20" && input$testTrain == "Тестовая"){
                         str01 <- paste(strong("Модель-классификатор: "), "Дерево с максимальной глубиной равной 20")
                         str02 <- paste(strong("Результаты получены на тестовой выборке"))
                         str1 <- paste("")
                         str2 <- paste(strong("MSE (оценка Брайера): "), round(h2o.mse(perfTestRF20), 4))
                         str3 <- paste(strong("RMSE: "), round(h2o.rmse(perfTestRF20), 4))
                         str4 <- paste(strong("Logloss: "), round(h2o.logloss(perfTestRF20), 4))
                         str5 <- paste(strong("AUC: "), round(h2o.auc(perfTestRF20), 4))
                         str6 <- paste(ATestRF20)
                         str7 <- paste(strong("F-мера: "), round(FTestRF20, 4))
                         str8 <- paste(strong("Каппа Коэна: "), round(KapTestRF20, 4))
                         str9 <- paste(strong("MCC (коэффициент Мэттьюса): "), round(MccTestRF20, 4))
                         str10 <- paste(strong("Сбалансированная точность: "), round(BATestRF20, 4))
                         str11 <- paste(strong("Точность (precision): "), round(PTestRF20, 4))
                         str12 <- paste(strong("Матрица ошибок: "))
                         HTML(paste(str01, str02, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str1, str12, sep = '<br/>'))
                       }
                       
                       else if (input$modelClass == "Дерево с максимальной глубиной равной 20" && input$testTrain == "Обучающая"){
                         str01 <- paste(strong("Модель-классификатор: "), "Дерево с максимальной глубиной равной 20")
                         str02 <- paste(strong("Результаты получены на обучающей выборке"))
                         str1 <- paste("")
                         str2 <- paste(strong("MSE (оценка Брайера): "), round(h2o.mse(perfTrainRF20), 4))
                         str3 <- paste(strong("RMSE: "), round(h2o.rmse(perfTrainRF20), 4))
                         str4 <- paste(strong("Logloss: "), round(h2o.logloss(perfTrainRF20), 4))
                         str5 <- paste(strong("AUC: "), round(h2o.auc(perfTrainRF20), 4))
                         str6 <- paste(ATrainRF20)
                         str7 <- paste(strong("F-мера: "), round(FTrainRF20, 4))
                         str8 <- paste(strong("Каппа Коэна: "), round(KapTrainRF20, 4))
                         str9 <- paste(strong("MCC (коэффициент Мэттьюса): "), round(MccTrainRF20, 4))
                         str10 <- paste(strong("Сбалансированная точность: "), round(BATrainRF20, 4))
                         str11 <- paste(strong("Точность (precision): "), round(PTrainRF20, 4))
                         str12 <- paste(strong("Матрица ошибок: "))
                         HTML(paste(str01, str02, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str1, str12, sep = '<br/>'))
                       }
                       
                       
                       else if (input$modelClass == "Дерево с максимальной глубиной равной 6" && input$testTrain == "Тестовая"){
                         str01 <- paste(strong("Модель-классификатор: "), "Дерево с максимальной глубиной равной 6")
                         str02 <- paste(strong("Результаты получены на тестовой выборке"))
                         str1 <- paste("")
                         str2 <- paste(strong("MSE (оценка Брайера): "), round(h2o.mse(perfTestXGB6), 4))
                         str3 <- paste(strong("RMSE: "), round(h2o.rmse(perfTestXGB6), 4))
                         str4 <- paste(strong("Logloss: "), round(h2o.logloss(perfTestXGB6), 4))
                         str5 <- paste(strong("AUC: "), round(h2o.auc(perfTestXGB6), 4))
                         str6 <- paste(ATestXGB6)
                         str7 <- paste(strong("F-мера: "), round(FTestXGB6, 4))
                         str8 <- paste(strong("Каппа Коэна: "), round(KapTestXGB6, 4))
                         str9 <- paste(strong("MCC (коэффициент Мэттьюса): "), round(MccTestXGB6, 4))
                         str10 <- paste(strong("Сбалансированная точность: "), round(BATestXGB6, 4))
                         str11 <- paste(strong("Точность (precision): "), round(PTestXGB6, 4))
                         str12 <- paste(strong("Матрица ошибок: "))
                         HTML(paste(str01, str02, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str1, str12, sep = '<br/>'))
                         
                       }
                       
                       else if (input$modelClass == "Дерево с максимальной глубиной равной 6" && input$testTrain == "Обучающая"){
                         #imageOutput(ns("plot"))
                         str01 <- paste(strong("Модель-классификатор: "), "Дерево с максимальной глубиной равной 6")
                         str02 <- paste(strong("Результаты получены на обучающей выборке"))
                         str1 <- paste("")
                         str2 <- paste(strong("MSE (оценка Брайера): "), round(h2o.mse(perfTrainXGB6), 4))
                         str3 <- paste(strong("RMSE: "), round(h2o.rmse(perfTrainXGB6), 4))
                         str4 <- paste(strong("Logloss: "), round(h2o.logloss(perfTrainXGB6), 4))
                         str5 <- paste(strong("AUC: "), round(h2o.auc(perfTrainXGB6), 4))
                         str6 <- paste(ATrainXGB6)
                         str7 <- paste(strong("F-мера: "), round(FTrainXGB6, 4))
                         str8 <- paste(strong("Каппа Коэна: "), round(KapTrainXGB6, 4))
                         str9 <- paste(strong("MCC (коэффициент Мэттьюса): "), round(MccTrainXGB6, 4))
                         str10 <- paste(strong("Сбалансированная точность: "), round(BATrainXGB6, 4))
                         str11 <- paste(strong("Точность (precision): "), round(PTrainXGB6, 4))
                         str12 <- paste(strong("Матрица ошибок: "))
                         HTML(paste(str01, str02, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str1, str12, sep = '<br/>'))
                         #plotOutput(ns("plot"))
                         
                         #plotOutput("plot")
                         #   fluidRow(
                         #     column(2,plotOutput("plotXGB"))
                         # )
                         #column(12, imageOutput(ns("varPlotXGB")))
                       }
                       
                       else if (input$modelClass == "Случайный лес" && input$testTrain == "Тестовая"){
                         str01 <- paste(strong("Модель-классификатор: "), "Случайный лес")
                         str02 <- paste(strong("Результаты получены на тестовой выборке"))
                         str1 <- paste("")
                         str2 <- paste(strong("MSE (оценка Брайера): "), round(h2o.mse(perfTestRF), 4))
                         str3 <- paste(strong("RMSE: "), round(h2o.rmse(perfTestRF), 4))
                         str4 <- paste(strong("Logloss: "), round(h2o.logloss(perfTestRF), 4))
                         str5 <- paste(strong("AUC: "), round(h2o.auc(perfTestRF), 4))
                         str6 <- paste(ATestRF)
                         str7 <- paste(strong("F-мера: "), round(FTestRF, 4))
                         str8 <- paste(strong("Каппа Коэна: "), round(KapTestRF, 4))
                         str9 <- paste(strong("MCC (коэффициент Мэттьюса): "), round(MccTestRF, 4))
                         str10 <- paste(strong("Сбалансированная точность: "), round(BATestRF, 4))
                         str11 <- paste(strong("Точность (precision): "), round(PTestRF, 4))
                         str12 <- paste(strong("Матрица ошибок: "))
                         HTML(paste(str01, str02, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str1, str12, sep = '<br/>'))
                       }
                       
                       else if (input$modelClass == "Случайный лес" && input$testTrain == "Обучающая"){
                         str01 <- paste(strong("Модель-классификатор: "), "Случайный лес")
                         str02 <- paste(strong("Результаты получены на обучающей выборке"))
                         str1 <- paste("")
                         str2 <- paste(strong("MSE (оценка Брайера): "), round(h2o.mse(perfTrainRF), 4))
                         str3 <- paste(strong("RMSE: "), round(h2o.rmse(perfTrainRF), 4))
                         str4 <- paste(strong("Logloss: "), round(h2o.logloss(perfTrainRF), 4))
                         str5 <- paste(strong("AUC: "), round(h2o.auc(perfTrainRF), 4))
                         str6 <- paste(ATrainRF)
                         str7 <- paste(strong("F-мера: "), round(FTrainRF, 4))
                         str8 <- paste(strong("Каппа Коэна: "), round(KapTrainRF, 4))
                         str9 <- paste(strong("MCC (коэффициент Мэттьюса): "), round(MccTrainRF, 4))
                         str10 <- paste(strong("Сбалансированная точность: "), round(BATrainRF, 4))
                         str11 <- paste(strong("Точность (precision): "), round(PTrainRF, 4))
                         str12 <- paste(strong("Матрица ошибок: "))
                         HTML(paste(str01, str02, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str1, str12, sep = '<br/>'))
                       }
                       
                       
                       else if (input$modelClass == "Градиентный бустинг" && input$testTrain == "Тестовая"){
                         str01 <- paste(strong("Модель-классификатор: "), "Градиентный бустинг")
                         str02 <- paste(strong("Результаты получены на тестовой выборке"))
                         str1 <- paste("")
                         str2 <- paste(strong("MSE (оценка Брайера): "), round(h2o.mse(perfTestXGB), 4))
                         str3 <- paste(strong("RMSE: "), round(h2o.rmse(perfTestXGB), 4))
                         str4 <- paste(strong("Logloss: "), round(h2o.logloss(perfTestXGB), 4))
                         str5 <- paste(strong("AUC: "), round(h2o.auc(perfTestXGB), 4))
                         str6 <- paste(ATestXGB)
                         str7 <- paste(strong("F-мера: "), round(FTestXGB, 4))
                         str8 <- paste(strong("Каппа Коэна: "), round(KapTestXGB, 4))
                         str9 <- paste(strong("MCC (коэффициент Мэттьюса): "), round(MccTestXGB, 4))
                         str10 <- paste(strong("Сбалансированная точность: "), round(BATestXGB, 4))
                         str11 <- paste(strong("Точность (precision): "), round(PTestXGB, 4))
                         str12 <- paste(strong("Матрица ошибок: "))
                         HTML(paste(str01, str02, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str1, str12, sep = '<br/>'))

                       }
                       
                       else if (input$modelClass == "Градиентный бустинг" && input$testTrain == "Обучающая"){
                         #imageOutput(ns("plot"))
                         str01 <- paste(strong("Модель-классификатор: "), "Градиентный бустинг")
                         str02 <- paste(strong("Результаты получены на обучающей выборке"))
                         str1 <- paste("")
                         str2 <- paste(strong("MSE (оценка Брайера): "), round(h2o.mse(perfTrainXGB), 4))
                         str3 <- paste(strong("RMSE: "), round(h2o.rmse(perfTrainXGB), 4))
                         str4 <- paste(strong("Logloss: "), round(h2o.logloss(perfTrainXGB), 4))
                         str5 <- paste(strong("AUC: "), round(h2o.auc(perfTrainXGB), 4))
                         str6 <- paste(ATrainXGB)
                         str7 <- paste(strong("F-мера: "), round(FTrainXGB, 4))
                         str8 <- paste(strong("Каппа Коэна: "), round(KapTrainXGB, 4))
                         str9 <- paste(strong("MCC (коэффициент Мэттьюса): "), round(MccTrainXGB, 4))
                         str10 <- paste(strong("Сбалансированная точность: "), round(BATrainXGB, 4))
                         str11 <- paste(strong("Точность (precision): "), round(PTrainXGB, 4))
                         str12 <- paste(strong("Матрица ошибок: "))
                         HTML(paste(str01, str02, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str1, str12, sep = '<br/>'))
                         #plotOutput(ns("plot"))
                         
                         #plotOutput("plot")
                      #   fluidRow(
                      #     column(2,plotOutput("plotXGB"))
                        # )
                         #column(12, imageOutput(ns("varPlotXGB")))
                       }
                       
                     }
                   })
                   

                   
                   output$matrix <- renderUI({
                     if (input$resultV){
                       options(spinner.color="#00a65a")
                     if (input$modelClass == "Наивный Байес" && input$testTrain == "Тестовая"){
                     M <- matrixTestNB
                     rownames(M) <- gsub(" ", ".", rownames(M))
                     colnames(M) <- gsub(" ", ".", colnames(M))
                     M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                                floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
                     html <- paste0("$$", M, "$$")
                     list(
                       withMathJax(HTML(html))
                     )
                     }
                     
                     else if (input$modelClass == "Наивный Байес" && input$testTrain == "Обучающая"){
                       M <- matrixTrainNB
                       rownames(M) <- gsub(" ", ".", rownames(M))
                       colnames(M) <- gsub(" ", ".", colnames(M))
                       M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                                  floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
                       html <- paste0("$$", M, "$$")
                       list(
                         withMathJax(HTML(html))
                       )
                     }
                       
                       else if (input$modelClass == "Мультиномиальная логистическая регрессия" && input$testTrain == "Тестовая"){
                         M <- matrixTestLR
                         rownames(M) <- gsub(" ", ".", rownames(M))
                         colnames(M) <- gsub(" ", ".", colnames(M))
                         M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                                    floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
                         html <- paste0("$$", M, "$$")
                         list(
                           withMathJax(HTML(html))
                         )
                       }
                       
                       else if (input$modelClass == "Мультиномиальная логистическая регрессия" && input$testTrain == "Обучающая"){
                         M <- matrixTrainLR
                         rownames(M) <- gsub(" ", ".", rownames(M))
                         colnames(M) <- gsub(" ", ".", colnames(M))
                         M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                                    floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
                         html <- paste0("$$", M, "$$")
                         list(
                           withMathJax(HTML(html))
                         )
                       }
                       
                       else if (input$modelClass == "Дерево с максимальной глубиной равной 20" && input$testTrain == "Тестовая"){
                         M <- matrixTestRF20
                         rownames(M) <- gsub(" ", ".", rownames(M))
                         colnames(M) <- gsub(" ", ".", colnames(M))
                         M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                                    floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
                         html <- paste0("$$", M, "$$")
                         list(
                           withMathJax(HTML(html))
                         )
                       }
                       
                       else if (input$modelClass == "Дерево с максимальной глубиной равной 20" && input$testTrain == "Обучающая"){
                         M <- matrixTrainRF20
                         rownames(M) <- gsub(" ", ".", rownames(M))
                         colnames(M) <- gsub(" ", ".", colnames(M))
                         M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                                    floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
                         html <- paste0("$$", M, "$$")
                         list(
                           withMathJax(HTML(html))
                         )
                       }
                       
                       
                       else if (input$modelClass == "Дерево с максимальной глубиной равной 6" && input$testTrain == "Тестовая"){
                         M <- matrixTestXGB6
                         rownames(M) <- gsub(" ", ".", rownames(M))
                         colnames(M) <- gsub(" ", ".", colnames(M))
                         M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                                    floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
                         html <- paste0("$$", M, "$$")
                         list(
                           withMathJax(HTML(html))
                         )
                       }
                       
                       else if (input$modelClass == "Дерево с максимальной глубиной равной 6" && input$testTrain == "Обучающая"){
                         M <- matrixTrainXGB6
                         rownames(M) <- gsub(" ", ".", rownames(M))
                         colnames(M) <- gsub(" ", ".", colnames(M))
                         M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                                    floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
                         html <- paste0("$$", M, "$$")
                         list(
                           withMathJax(HTML(html))
                         )
                         
                       }
                       
                       else if (input$modelClass == "Случайный лес" && input$testTrain == "Тестовая"){
                         M <- matrixTestRF
                         rownames(M) <- gsub(" ", ".", rownames(M))
                         colnames(M) <- gsub(" ", ".", colnames(M))
                         M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                                    floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
                         html <- paste0("$$", M, "$$")
                         list(
                           withMathJax(HTML(html))
                         )
                       }
                       
                       else if (input$modelClass == "Случайный лес" && input$testTrain == "Обучающая"){
                         M <- matrixTrainRF
                         rownames(M) <- gsub(" ", ".", rownames(M))
                         colnames(M) <- gsub(" ", ".", colnames(M))
                         M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                                    floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
                         html <- paste0("$$", M, "$$")
                         list(
                           withMathJax(HTML(html))
                         )
                       }
                       
                       
                       else if (input$modelClass == "Градиентный бустинг" && input$testTrain == "Тестовая"){
                         M <- matrixTestXGB
                         rownames(M) <- gsub(" ", ".", rownames(M))
                         colnames(M) <- gsub(" ", ".", colnames(M))
                         M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                                    floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
                         html <- paste0("$$", M, "$$")
                         list(
                           withMathJax(HTML(html))
                         )
                       }
                       
                       else if (input$modelClass == "Градиентный бустинг" && input$testTrain == "Обучающая"){
                         M <- matrixTrainXGB
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
                     # }
                     
                   })
                   
                   #if (input$modelClass == "Случайный лес" && input$testTrain == "Обучающая"){
                   output$plot <- renderPlot({
                     
                     if (input$resultV){

                     #if (input$modelClass == "Наивный Байес" && input$testTrain == "Обучающая"){

                     #  h2o.varimp_plot(m_nb)
                       
                     #}
                     
                     if (input$modelClass == "Мультиномиальная логистическая регрессия" && input$testTrain == "Обучающая"){
                       
                       h2o.varimp_plot(m_lr)
                       
                     }
                     else if (input$modelClass == "Дерево с максимальной глубиной равной 20" && input$testTrain == "Обучающая"){
                       
                       h2o.varimp_plot(m_rf20)
                       
                     }
                     else if (input$modelClass == "Дерево с максимальной глубиной равной 6" && input$testTrain == "Обучающая"){
                       
                       h2o.varimp_plot(m_xgb6)
                       
                     }
                     else if (input$modelClass == "Случайный лес" && input$testTrain == "Обучающая"){
                       
                       h2o.varimp_plot(m_rf)
                       
                     }
                     else if (input$modelClass == "Градиентный бустинг" && input$testTrain == "Обучающая"){
                       
                       h2o.varimp_plot(m_xgb)
                       
                     }
                     }

                        # })
                     #  }
                    # }
                   })
                     #}
                   
                   output$varimp <- renderPrint({
                     
                     if (input$resultV){
                     
                     if (input$modelClass == "Мультиномиальная логистическая регрессия" && input$testTrain == "Обучающая"){
                       
                       as.data.frame(h2o.varimp(m_lr))
                       
                     }
                     else if (input$modelClass == "Дерево с максимальной глубиной равной 20" && input$testTrain == "Обучающая"){
                       
                       as.data.frame(h2o.varimp(m_rf20))
                       
                     }
                     else if (input$modelClass == "Дерево с максимальной глубиной равной 6" && input$testTrain == "Обучающая"){
                       
                       as.data.frame(h2o.varimp(m_xgb6))
                       
                     }
                     else if (input$modelClass == "Случайный лес" && input$testTrain == "Обучающая"){
                       
                       as.data.frame(h2o.varimp(m_rf))
                       
                     }
                     else if (input$modelClass == "Градиентный бустинг" && input$testTrain == "Обучающая"){
                       
                       as.data.frame(h2o.varimp(m_xgb))
                       
                       
                     }
                     }
                     
                     # })
                     #  }
                     # }
                   })
                 
                   
                   output$matrixView <- renderUI({
                     
                     if (input$resultV){
                       
                     fluidRow(column(
                       12,
                       box(
                         id = "result",
                         title = "Результаты обучения",
                         status = "success",
                         solidHeader = TRUE,
                         width = 12,
                         uiOutput(ns("ViewRes")),
                         uiOutput(ns("matrix")),
                         if (input$testTrain == "Обучающая" && input$modelClass != "Наивный Байес"){
                           str1 <- paste("")
                           str13 <- paste(strong("Оценка важности предикторов (диаграмма): "))
                           str14 <- paste(imageOutput(ns("plot")))
                           str15 <- paste(strong("Оценка важности предикторов: "))
                           str16 <- paste(verbatimTextOutput(ns("varimp")))
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
                   
                 
                 })
                

                 output$View <- renderUI({
                   #options(spinner.color="#00a65a")

                   
               #    waiter_show(
                #     html = tagList(
                  #     spin_fading_circles(),
                 #      "Загрузка ..."
                 #    )
                 #  )
                  # Sys.sleep(3)
                   bClassifLearning()
                   #stackClassifLearning()
                   #plotOutput(ns("plot"))
                   uiOutput(
                     ns("ViewNB"))
                                    #  waiter_hide()
                   
                   })
                 
                 stackClassifLearning <- eventReactive(input$stackingLearning, {
                   
                   waiter_show( # show the waiter
                     color = "#00a65a",
                     html = tagList(
                       spin_solar(), # use a spinner
                       "Подождите, алгоритм обучается..."
                     )
                   )
                   
                   #print(rvAcc$d)
                   bClassifList <- NULL
                   
                   if (input$stackingClassifLearning == "Наивный Байес"){
                     
                     metalearner = "naivebayes"
                     
                   }
                   
                   if (input$stackingClassifLearning == "Мультиномиальная логистическая регрессия"){
                     
                     metalearner = "glm"
                     
                   }
                   
                   if (input$stackingClassifLearning == "Случайный лес"){
                     
                     metalearner = "drf"
                     
                   }
                   
                   if (input$stackingClassifLearning == "Градиентный бустинг"){
                     
                     metalearner = "xgboost"
                     
                   }
                   
                   if ("Наивный Байес" %in% input$bstackingClassifLearning){
                     if (is.null(bClassifList)) {
                     bClassifList <- list(models$mNB)
                     }
                     else {
                       len <- length(bClassifList)
                       bClassifList[[len+1]] <- models$mNB
                     }
                   }
                   
                   if ("Мультиномиальная логистическая регрессия" %in% input$bstackingClassifLearning){
                     if (is.null(bClassifList)) {
                       bClassifList <- list(models$mLR)
                     }
                     else {
                       len <- length(bClassifList)
                       bClassifList[[len+1]] <- models$mLR
                     }
                   }
                   
                   if ("Дерево с максимальной глубиной равной 20" %in% input$bstackingClassifLearning){
                     if (is.null(bClassifList)) {
                       bClassifList <- list(models$mRF20)
                     }
                     else {
                       len <- length(bClassifList)
                       bClassifList[[len+1]] <- models$mRF20
                     }
                   }
                   
                   if ("Дерево с максимальной глубиной равной 6" %in% input$bstackingClassifLearning){
                     if (is.null(bClassifList)) {
                       bClassifList <- list(models$mRF6)
                     }
                     else {
                       len <- length(bClassifList)
                       bClassifList[[len+1]] <- models$mRF6
                     }
                   }
                   
                   if ("Случайный лес" %in% input$bstackingClassifLearning){
                     if (is.null(bClassifList)) {
                       bClassifList <- list(models$mRF)
                     }
                     else {
                       len <- length(bClassifList)
                       bClassifList[[len+1]] <- models$mRF
                     }
                   }
                   
                   if ("Градиентный бустинг" %in% input$bstackingClassifLearning){
                     if (is.null(bClassifList)) {
                       bClassifList <- list(models$mXGB)
                     }
                     else {
                       len <- length(bClassifList)
                       bClassifList[[len+1]] <- models$mXGB
                     }
                   }
                   
                   
                   
                   
                   
                   #print(metalearner)
                   
                   #train_df_h2o<-as.h2o(dataTrain())
                   #test_df_h2o<-as.h2o(dataTest())
                   
                   #testsetMet <- as.data.frame(dataTest())
                   #trainsetMet <- as.data.frame(dataTrain())
                   
                   #y <- names(train_df_h2o[, targetVar()])
                   #x <- setdiff(names(train_df_h2o[, predictor()]), y)
                   #models$mNB <- m_nb
                   
                   train_df_h2o <- setsH2O$train_df_h2o
                   test_df_h2o <- setsH2O$test_df_h2o

                   testsetMet <- setsMet$testsetMet
                   trainsetMet <- setsMet$trainsetMet
                   
                   x <- xy$x
                   y <- xy$y
                   
                   m_ensemble <- h2o.stackedEnsemble(x = x,
                                                    y = y,
                                                    metalearner_algorithm=metalearner,
                                                    metalearner_params = list(auc_type="WEIGHTED_OVO", 
                                                                              categorical_encoding = "OneHotExplicit"),
                                                    training_frame = train_df_h2o,
                                                    validation_frame = test_df_h2o,
                                                    metalearner_nfolds = 10,
                                                    metalearner_fold_assignment = "Modulo",
                                                    #keep_cross_validation_predictions = TRUE,
                                                    #keep_levelone_frame = T,
                                                    #fold_column = "Засорённость.класс",
                                                    # metalearner_transform = "Logit",
                                                    #metalearner_fold_column = "Засорённость.класс",
                                                    base_models = bClassifList,
                                                     #as.list(input$bstackingClassifLearning),
                                                    #auc_type="MACRO_OVR",
                                                    seed = 101)
                   #print(ensemble)
                   models$mE <- m_ensemble
                   
                   predTestE <- predict(m_ensemble, test_df_h2o, type = "class")
                   AccTestE <- mean(predTestE == test_df_h2o[, targetVar()])
                   ATestE <- paste(strong("Точность (accuracy): "), round(AccTestE, 4), sep = "")
                   ATestE <- ATestE[c(1)]
                   
                   predTrainE <- predict(m_ensemble, train_df_h2o, type = "class")
                   AccTrainE <- mean(predTrainE == train_df_h2o[, targetVar()])
                   ATrainE <- paste(strong("Точность (accuracy): "), round(AccTrainE, 4), sep = "")
                   ATrainE <- ATrainE[c(1)]
                   
                   perfTestE <- h2o.performance(m_ensemble, test_df_h2o)
                   perfTrainE <- h2o.performance(m_ensemble, train_df_h2o)
                   
                   colTestE <- ncol(h2o.confusionMatrix(m_ensemble,test_df_h2o))
                   colTrainE <- ncol(h2o.confusionMatrix(m_ensemble,train_df_h2o))
                   
                   matrixTestE = h2o.confusionMatrix(m_ensemble,test_df_h2o)[,1:colTestE]
                   matrixTrainE = h2o.confusionMatrix(m_ensemble,train_df_h2o)[,1:colTrainE]
                   
                   predTestEC <- as.data.frame(predTestE[1])
                   predTrainEC <- as.data.frame(predTrainE[1])
                   
                   FTestE <- f_meas_vec(testsetMet[, y], factor(predTestEC$predict, levels = levels(testsetMet[, y])))
                   FTrainE <- f_meas_vec(trainsetMet[, y], factor(predTrainEC$predict, levels = levels(testsetMet[, y])))
                   
                   KapTestE <- kap_vec(testsetMet[, y], factor(predTestEC$predict, levels = levels(testsetMet[, y])))
                   KapTrainE <- kap_vec(trainsetMet[, y], factor(predTrainEC$predict, levels = levels(testsetMet[, y])))
                   
                   MccTestE <- mcc_vec(testsetMet[, y], factor(predTestEC$predict, levels = levels(testsetMet[, y])))
                   MccTrainE <- mcc_vec(trainsetMet[, y], factor(predTrainEC$predict, levels = levels(testsetMet[, y])))
                   
                   BATestE <- bal_accuracy_vec(testsetMet[, y], factor(predTestEC$predict, levels = levels(testsetMet[, y])))
                   BATrainE <- bal_accuracy_vec(trainsetMet[, y], factor(predTrainEC$predict, levels = levels(testsetMet[, y])))
                   
                   PTestE <- precision_vec(testsetMet[, y], factor(predTestEC$predict, levels = levels(testsetMet[, y])))
                   PTrainE <- precision_vec(trainsetMet[, y], factor(predTrainEC$predict, levels = levels(testsetMet[, y])))
                   
                   waiter_hide()
                   
                   
                   
                   if (input$stackingLearning) {
                     options(spinner.color="#00a65a")
                     #naiveBayes()
                     #stackClassifLearning()
                     output$ViewStack <- renderUI({
                       options(spinner.color="#00a65a")
                       fluidRow(
                         column(5, pickerInput(inputId = ns('testTrainS'),
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
                           actionButton(ns("resultVS"), "Отобразить")
                         ),
                         
                         # column(12, uiOutput(
                         #    ns("resultView"))),
                         #column(12, uiOutput(ns("ViewRes"))),
                         
                         uiOutput(ns("matrixViewE"))
                         
                       )})
                   }
                   
                   output$ViewResE <- renderUI({
                     options(spinner.color="#00a65a")
                     if (input$resultVS){
                       options(spinner.color="#00a65a")
                       if (input$testTrainS == "Тестовая"){
                         str01 <- paste(strong("Модель-классификатор: "), "Неоднородный ансамбль")
                         str000 <- paste(strong("Мета-алгоритм: "), input$stackingClassifLearning)
                         str001 <- paste(strong("Базовые алгоритмы: "), paste(input$bstackingClassifLearning, collapse = ', '))
                         str02 <- paste(strong("Результаты получены на тестовой выборке"))
                         str1 <- paste("")
                         str2 <- paste(strong("MSE (оценка Брайера): "), round(h2o.mse(perfTestE), 4))
                         str3 <- paste(strong("RMSE: "), round(h2o.rmse(perfTestE), 4))
                         str4 <- paste(strong("Logloss: "), round(h2o.logloss(perfTestE), 4))
                         str5 <- paste(strong("AUC: "), round(h2o.auc(perfTestE), 4))
                         str6 <- paste(ATestE)
                         str7 <- paste(strong("F-мера: "), round(FTestE, 4))
                         str8 <- paste(strong("Каппа Коэна: "), round(KapTestE, 4))
                         str9 <- paste(strong("MCC (коэффициент Мэттьюса): "), round(MccTestE, 4))
                         str10 <- paste(strong("Сбалансированная точность: "), round(BATestE, 4))
                         str11 <- paste(strong("Точность (precision): "), round(PTestE, 4))
                         str12 <- paste(strong("Матрица ошибок: "))
                         HTML(paste(str01, str000, str001, str02, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str1, str12, sep = '<br/>'))
                       }
                       
                       else if (input$testTrainS == "Обучающая"){
                         str01 <- paste(strong("Модель-классификатор: "), "Неоднородный ансамбль")
                         str000 <- paste(strong("Мета-алгоритм: "), input$stackingClassifLearning)
                         str001 <- paste(strong("Базовые алгоритмы: "), paste(input$bstackingClassifLearning, collapse = ', '))
                         str02 <- paste(strong("Результаты получены на обучающей выборке"))
                         str1 <- paste("")
                         str2 <- paste(strong("MSE (оценка Брайера): "), round(h2o.mse(perfTrainE), 4))
                         str3 <- paste(strong("RMSE: "), round(h2o.rmse(perfTrainE), 4))
                         str4 <- paste(strong("Logloss: "), round(h2o.logloss(perfTrainE), 4))
                         str5 <- paste(strong("AUC: "), round(h2o.auc(perfTrainE), 4))
                         str6 <- paste(ATrainE)
                         str7 <- paste(strong("F-мера: "), round(FTrainE, 4))
                         str8 <- paste(strong("Каппа Коэна: "), round(KapTrainE, 4))
                         str9 <- paste(strong("MCC (коэффициент Мэттьюса): "), round(MccTrainE, 4))
                         str10 <- paste(strong("Сбалансированная точность: "), round(BATrainE, 4))
                         str11 <- paste(strong("Точность (precision): "), round(PTrainE, 4))
                         str12 <- paste(strong("Матрица ошибок: "))
                         HTML(paste(str01, str000, str001, str02, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str1, str12, sep = '<br/>'))
                       }
                     }
                 })
                   
                 
                 output$matrixE <- renderUI({
                   options(spinner.color="#00a65a")
                   if (input$resultVS){
                     options(spinner.color="#00a65a")
                     if (input$testTrainS == "Тестовая"){
                       M <- matrixTestE
                       rownames(M) <- gsub(" ", ".", rownames(M))
                       colnames(M) <- gsub(" ", ".", colnames(M))
                       M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                                  floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
                       html <- paste0("$$", M, "$$")
                       list(
                         withMathJax(HTML(html))
                       )
                     }
                     
                     else if (input$testTrainS == "Обучающая"){
                       M <- matrixTrainE
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
                 })
                 
                # output$plot <- renderPlot({
                   
                  # if (input$testTrainS == "Обучающая"){
                     
                 #    h2o.varimp_plot(m_ensemble)
                     
                 #  }
                 #})
                 
                 output$matrixViewE <- renderUI({
                   
                   options(spinner.color="#00a65a")
                   
                   fluidRow(column(
                     12,
                     box(
                       id = "resultE",
                       title = "Результаты обучения",
                       status = "success",
                       solidHeader = TRUE,
                       width = 12,
                       uiOutput(ns("ViewResE")),
                       uiOutput(ns("matrixE"))
                       #if (input$testTrainS == "Обучающая"){
                       #  str1 <- paste("")
                       #  str13 <- paste(strong("Оценка важности предикторов: "))
                       #  str14 <- paste(imageOutput(ns("plot")))
                       #  HTML(paste(str1, str13, str14, sep = '<br/>'))
                         #imageOutput(ns("plot"))
                     #  }
                       #plotOutput(ns("plot"))
                     ),
                     #column(width = 12, align = "center",
                     #       plotOutput(ns("plot")),)
                     #plotOutput(ns("plot")),
                     # column(
                     #   12, plotOutput(ns("plot")))
                     
                     
                   ),
                   
                   )
                   
                 })
                 
                 output$ViewS <- renderUI({
                   options(spinner.color="#00a65a")
                   #bClassifLearning()
                   stackClassifLearning()
                   #plotOutput(ns("plot"))
                   uiOutput(
                     ns("ViewStack"))
                   
                 })
                 
                 
                 oClassifLearning <- eventReactive(input$oLearning, {
                   
                   waiter_show( # show the waiter
                     color = "#00a65a",
                     html = tagList(
                       spin_solar(), # use a spinner
                       "Подождите, алгоритмы обучаются..."
                     )
                   )
                   
                   dataTrainN <- dataTrain()
                   dataTestN <- dataTest()
                   
                   y <- targetVar()
                   x <- setdiff(names(dataTrainN[, predictor()]), y)
                   
                   xy <- c(x, y)
                   
                   
                   #print(x)
                   
                   if (ncol(dataTrainN[, x] %>% select_if(is.factor)) > 0) {
                     
                   xx <- colnames(dataTrainN[, x] %>% select_if(is.factor))
                   
                   dataTrainN <- one_hot(as.data.table(dataTrainN[, xy]), cols = xx)
                   dataTestN <- one_hot(as.data.table(dataTestN[, xy]), cols = xx)
                   xxx <- setdiff(names(dataTrainN), y)
                   #print("------------------------------")
                   
                   #print(xxx)
                   }
                   else
                     xxx <- x
                   #print(xx)
                   
                  
                   
                   #print(dataTrain)
                   
                   
                   
                   #y <- names(train_df_h2o[, targetVar()])
                   #x <- setdiff(names(train_df_h2o[, predictor()]), y)
                   
                   #dataTrain[] <- lapply(dataTrain, function(x) gsub(" ", ".", x))
                   #dataTrain
                   
                   #dataTrain <- lapply(dataTrain, function(x) gsub(" ", ".", x))
                   
                   #print(dataTrain[, targetVar()])
                   

                   #print(dataTrain[, "Засорённость.класс"])
                   
                   
                   #print(y)
                  # print(dataTrain[, targetVar()])
                   ##a <- lapply(dataTrainN[, "Засорённость.класс"], levels) 
                   #print(a)
                   
                   ##yy <- paste("'", a, collapse = " + ", "'")
                   #print(yy)
                   
                   #print(dataTrain[, y])
                   
                   #print(dataTrain[, targetVar()])
                   
                   
                   #form <- paste(dataset[, input$yDA], " ~ ", paste(input$xDA, collapse = " + "))
                   form <- as.formula(paste(y, " ~ ", paste(xxx, collapse = " + ")))
                   
                   
                   
                   #form2form2form2 <- "Засорённость.класс ~ Обработка + Культура.после.пара + Фон.химизации + Темп.1д.мая + Темп.2д.мая + 
                   #  Темп.3д.мая + Осадки.3д.апреля"
                   #print(form)
                   #model <- lm(as.formula(form), data = dataTrain)
                   
                   #print(form)
                   #print(model)
                   
                  ## text <- input$text
                   #pp <- formula(Reduce(paste, deparse(form)))
                   #print(pp)
                   
                   
                   ##nnnnnn <- nnet(form, data = dataTrainN,
                    ##              maxit =6000, decay = 0.05, size = 12, niter = 200, trace = FALSE)
                   ##print(nnnnnn)
                   #plotnet(nnnnnn, cex_val=0.8, circle_cex=4)
                   
                   print(input$oClassifLearning)

                   
                   if ("Нейросетевой метод (персептрон)" %in% input$oClassifLearning){
                     
                   train.NN <- train(form, data = dataTrainN,
                                      method = "nnet", trace = FALSE, linout = F,
                                      tuneGrid = expand.grid(.decay = c(0, 0.05, 0.2), .size = 7:12),
                                      trControl = trainControl(method = "cv"))
                   
                   modelsO$mNN <- train.NN
                   
                   
                   #print(table(dataTestN[, targetVar()], predict(train.NN, dataTestN, type = "raw")))
                   
                   predTestNN <- predict(train.NN, dataTestN, type = "raw")
                   predTestNNP <- predict(train.NN, dataTestN, type = "prob")
                   
                   #print(predTestNNP)
                   #print(predTestNN)
                   #print(dataTestN[, "Засорённость.класс"])
                   AccTestNN <- mean(predTestNN == pull(dataTestN, targetVar())) 
                   ATestNN <- paste(strong("Точность (accuracy): "), round(AccTestNN, 4), sep = "")
                   ATestNN <- ATestNN[c(1)]
                   
                   #nn.table <- table(dataTestN[, targetVar()], predTestNN)
                   #print(confusionMatrix(nn.table))
                   
                   ##print(ATestNN) 
                   
                   predTrainNN <- predict(train.NN, dataTrainN, type = "raw")
                   predTrainNNP <- predict(train.NN, dataTrainN, type = "prob")
                   
                   AccTrainNN <- mean(predTrainNN == pull(dataTrainN, targetVar()))
                   ATrainNN <- paste(strong("Точность (accuracy): "), round(AccTrainNN, 4), sep = "")
                   ATrainNN <- ATrainNN[c(1)]
                   
                   ###print(ATrainNN) 
                   
                   ###print("---------")
                   #print(BrierScore(pull(dataTestN, targetVar()), predTestNN))
                   
                   AUCTestNN <- roc_auc_vec(pull(dataTestN, targetVar()), as.matrix(predTestNNP))
                   AUCTrainNN <- roc_auc_vec(pull(dataTrainN, targetVar()), as.matrix(predTrainNNP))
                   
                   LLTestNN <- mn_log_loss_vec(pull(dataTestN, targetVar()), as.matrix(predTestNNP))
                   LLTrainNN <- mn_log_loss_vec(pull(dataTrainN, targetVar()), as.matrix(predTrainNNP))
                   
                   #print(mn_log_loss_vec(pull(dataTestN, targetVar()), as.matrix(predTestNNP)))
                   #print(brier_factor(pull(dataTestN, targetVar()), as.matrix(predTrainNNP)))
                   
                   BSTestNN <- multiclass.Brier(as.matrix(predTestNNP), pull(dataTestN, targetVar()))
                   BSTrainNN <- multiclass.Brier(as.matrix(predTrainNNP), pull(dataTrainN, targetVar()))
                   
                   SQRTBSTestNN <- sqrt(BSTestNN)
                   SQRTBSTrainNN <- sqrt(BSTrainNN)
                   
                   FTestNN <- f_meas_vec(pull(dataTestN, targetVar()), predTestNN)
                   FTrainNN <- f_meas_vec(pull(dataTrainN, targetVar()), predTrainNN)
                   
                   KapTestNN <- kap_vec(pull(dataTestN, targetVar()), predTestNN)
                   KapTrainNN <- kap_vec(pull(dataTrainN, targetVar()), predTrainNN)
                   
                   MccTestNN <- mcc_vec(pull(dataTestN, targetVar()), predTestNN)
                   MccTrainNN <- mcc_vec(pull(dataTrainN, targetVar()), predTrainNN)
                   
                   BATestNN <- bal_accuracy_vec(pull(dataTestN, targetVar()), predTestNN)
                   BATrainNN <- bal_accuracy_vec(pull(dataTrainN, targetVar()), predTrainNN)
                   
                   PTestNN <- precision_vec(pull(dataTestN, targetVar()), predTestNN)
                   PTrainNN <- precision_vec(pull(dataTrainN, targetVar()), predTrainNN)
                   
                   matrixTestNN = table(pull(dataTestN, targetVar()), predTestNN)
                   #print(matrixTestNN)
                   matrixTrainNN = table(pull(dataTrainN, targetVar()), predTrainNN)
                   }
                   
                   ###print(train.NN)
                   
                   
                   
                   #forndndndn <- Reduce(paste, deparse(form))
                   #print(forndndndn)
                   
                   
                   
                   #print(text)
                   
                   #yyyy <- targetVar()
                   
                   #formform <- as.formula(paste(yyyy, " ~ ", paste(xxx, collapse = " + ")))
                   #print('--------')
                   #print(formform)
                   

                    #formF$formform <- train.NN
                   
                   #formF$formform <- nnnnnn
                   
                   #par(mar = numeric(4), family = 'serif')
                   
                   #plotnet(nn, pad_x =0.8, circle_cex=5)
                   
                   #plot.nnet(mod1)
                   
                   
                   
                   
                   #plot(nnnnnn)
                   
                   if ("Дерево принятия решений" %in% input$oClassifLearning){
                    
#                     train_control<- trainControl(method="repeatedcv", number=10, repeats=10, savePredictions = TRUE, classProbs=TRUE,summaryFunction =multiClassSummary)
                     train_control<- trainControl(method="repeatedcv", number=10, repeats=10, classProbs=TRUE, summaryFunction = multiClassSummary)
                     
                     train.T<- train(form, data = dataTrainN, trControl = train_control, method = "rpart", tuneLength = 10)

                     modelsO$mT <- train.T
                     modelsO$mTF <- train.T$finalModel
                     
                     predTestT <- predict(train.T, dataTestN, type = "raw")
                     predTestTP <- predict(train.T, dataTestN, type = "prob")
                   
                   
                     AccTestT <- mean(predTestT == pull(dataTestN, targetVar())) 
                     ATestT <- paste(strong("Точность (accuracy): "), round(AccTestT, 4), sep = "")
                     ATestT <- ATestT[c(1)]
                     
                     predTrainT <- predict(train.T, dataTrainN, type = "raw")
                     predTrainTP <- predict(train.T, dataTrainN, type = "prob")
                     
                     AccTrainT <- mean(predTrainT == pull(dataTrainN, targetVar()))
                     ATrainT <- paste(strong("Точность (accuracy): "), round(AccTrainT, 4), sep = "")
                     ATrainT <- ATrainT[c(1)]
                   
                     AUCTestT <- roc_auc_vec(pull(dataTestN, targetVar()), as.matrix(predTestTP))
                     AUCTrainT <- roc_auc_vec(pull(dataTrainN, targetVar()), as.matrix(predTrainTP))
                   
                     LLTestT <- mn_log_loss_vec(pull(dataTestN, targetVar()), as.matrix(predTestTP))
                     LLTrainT <- mn_log_loss_vec(pull(dataTrainN, targetVar()), as.matrix(predTrainTP))
                     
                     BSTestT <- multiclass.Brier(as.matrix(predTestTP), pull(dataTestN, targetVar()))
                     BSTrainT <- multiclass.Brier(as.matrix(predTrainTP), pull(dataTrainN, targetVar()))
                     
                     SQRTBSTestT <- sqrt(BSTestT)
                     SQRTBSTrainT <- sqrt(BSTrainT)

                   
                   #print(mn_log_loss_vec(pull(dataTestN, targetVar()), as.matrix(predTestNNP)))
                   #print(brier_factor(pull(dataTestN, targetVar()), as.matrix(predTrainNNP)))
                     
                     FTestT <- f_meas_vec(pull(dataTestN, targetVar()), predTestT)
                     FTrainT <- f_meas_vec(pull(dataTrainN, targetVar()), predTrainT)
                   
                     KapTestT <- kap_vec(pull(dataTestN, targetVar()), predTestT)
                     KapTrainT <- kap_vec(pull(dataTrainN, targetVar()), predTrainT)
                   
                     MccTestT <- mcc_vec(pull(dataTestN, targetVar()), predTestT)
                     MccTrainT <- mcc_vec(pull(dataTrainN, targetVar()), predTrainT)
                   
                     BATestT <- bal_accuracy_vec(pull(dataTestN, targetVar()), predTestT)
                     BATrainT <- bal_accuracy_vec(pull(dataTrainN, targetVar()), predTrainT)
                   
                     PTestT <- precision_vec(pull(dataTestN, targetVar()), predTestT)
                     PTrainT <- precision_vec(pull(dataTrainN, targetVar()), predTrainT)
                     
                     matrixTestT = table(pull(dataTestN, targetVar()), predTestT)
                   #print(matrixTestNN)
                     matrixTrainT = table(pull(dataTrainN, targetVar()), predTrainT)
                     
                   }
                   
                   waiter_hide()
                   
                   output$ViewResO <- renderUI({
                     
                     if (input$resultO){
                       
                       if (input$modelClassO == "Нейросетевой метод (персептрон)" && input$testTrainO == "Обучающая"){
                         str01 <- paste(strong("Модель-классификатор: "), "Нейросетевой метод (персептрон)")
                         str02 <- paste(strong("Результаты получены на обучающей выборке"))
                         str1 <- paste("")
                         str2 <- paste(strong("MSE (оценка Брайера): "), round(BSTrainNN, 4))
                         str3 <- paste(strong("RMSE: "), round(SQRTBSTrainNN, 4))
                         str4 <- paste(strong("Logloss: "), round(LLTrainNN, 4))
                         str5 <- paste(strong("AUC: "), round(AUCTrainNN, 4))
                         str6 <- paste(ATrainNN)
                         str7 <- paste(strong("F-мера: "), round(FTrainNN, 4))
                         str8 <- paste(strong("Каппа Коэна: "), round(KapTrainNN, 4))
                         str9 <- paste(strong("MCC (коэффициент Мэттьюса): "), round(MccTrainNN, 4))
                         str10 <- paste(strong("Сбалансированная точность: "), round(BATrainNN, 4))
                         str11 <- paste(strong("Точность (precision): "), round(PTrainNN, 4))
                         str12 <- paste(strong("Матрица ошибок: "))
                         HTML(paste(str01, str02, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str1, str12, sep = '<br/>'))
                       }
                       
                       else if (input$modelClassO == "Нейросетевой метод (персептрон)" && input$testTrainO == "Тестовая"){
                         str01 <- paste(strong("Модель-классификатор: "), "Нейросетевой метод (персептрон)")
                         str02 <- paste(strong("Результаты получены на тестовой выборке"))
                         str1 <- paste("")
                         str2 <- paste(strong("MSE (оценка Брайера): "), round(BSTestNN, 4))
                         str3 <- paste(strong("RMSE: "), round(SQRTBSTestNN, 4))
                         str4 <- paste(strong("Logloss: "), round(LLTestNN, 4))
                         str5 <- paste(strong("AUC: "), round(AUCTestNN, 4))
                         str6 <- paste(ATestNN)
                         str7 <- paste(strong("F-мера: "), round(FTestNN, 4))
                         str8 <- paste(strong("Каппа Коэна: "), round(KapTrainNN, 4))
                         str9 <- paste(strong("MCC (коэффициент Мэттьюса): "), round(MccTestNN, 4))
                         str10 <- paste(strong("Сбалансированная точность: "), round(BATestNN, 4))
                         str11 <- paste(strong("Точность (precision): "), round(PTestNN, 4))
                         str12 <- paste(strong("Матрица ошибок: "))
                         HTML(paste(str01, str02, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str1, str12, sep = '<br/>'))
                       }
                       
                       else if (input$modelClassO == "Дерево принятия решений" && input$testTrainO == "Обучающая"){
                         str01 <- paste(strong("Модель-классификатор: "), "Дерево принятия решений")
                         str02 <- paste(strong("Результаты получены на обучающей выборке"))
                         str1 <- paste("")
                         str2 <- paste(strong("MSE (оценка Брайера): "), round(BSTrainT, 4))
                         str3 <- paste(strong("RMSE: "), round(SQRTBSTrainT, 4))
                         str4 <- paste(strong("Logloss: "), round(LLTrainT, 4))
                         str5 <- paste(strong("AUC: "), round(AUCTrainT, 4))
                         str6 <- paste(ATrainT)
                         str7 <- paste(strong("F-мера: "), round(FTrainT, 4))
                         str8 <- paste(strong("Каппа Коэна: "), round(KapTrainT, 4))
                         str9 <- paste(strong("MCC (коэффициент Мэттьюса): "), round(MccTrainT, 4))
                         str10 <- paste(strong("Сбалансированная точность: "), round(BATrainT, 4))
                         str11 <- paste(strong("Точность (precision): "), round(PTrainT, 4))
                         str12 <- paste(strong("Матрица ошибок: "))
                         HTML(paste(str01, str02, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str1, str12, sep = '<br/>'))
                       }
                       
                       else if (input$modelClassO == "Дерево принятия решений" && input$testTrainO == "Тестовая"){
                         str01 <- paste(strong("Модель-классификатор: "), "Дерево принятия решений")
                         str02 <- paste(strong("Результаты получены на тестовой выборке"))
                         str1 <- paste("")
                         str2 <- paste(strong("MSE (оценка Брайера): "), round(BSTestT, 4))
                         str3 <- paste(strong("RMSE: "), round(SQRTBSTestT, 4))
                         str4 <- paste(strong("Logloss: "), round(LLTestT, 4))
                         str5 <- paste(strong("AUC: "), round(AUCTestT, 4))
                         str6 <- paste(ATestT)
                         str7 <- paste(strong("F-мера: "), round(FTestT, 4))
                         str8 <- paste(strong("Каппа Коэна: "), round(KapTrainT, 4))
                         str9 <- paste(strong("MCC (коэффициент Мэттьюса): "), round(MccTestT, 4))
                         str10 <- paste(strong("Сбалансированная точность: "), round(BATestT, 4))
                         str11 <- paste(strong("Точность (precision): "), round(PTestT, 4))
                         str12 <- paste(strong("Матрица ошибок: "))
                         HTML(paste(str01, str02, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str1, str12, sep = '<br/>'))
                       }
                     }
                   })
                   
                   output$matrixO <- renderUI({
                     if (input$resultO){
                       options(spinner.color="#00a65a")
                       if (input$modelClassO == "Нейросетевой метод (персептрон)" && input$testTrainO == "Тестовая"){
                         M <- matrixTestNN
                         rownames(M) <- gsub(" ", ".", rownames(M))
                         colnames(M) <- gsub(" ", ".", colnames(M))
                         M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                                    floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
                         html <- paste0("$$", M, "$$")
                         list(
                           withMathJax(HTML(html))
                         )
                       }
                       
                       else if (input$modelClassO == "Нейросетевой метод (персептрон)" && input$testTrainO == "Обучающая"){
                         M <- matrixTrainNN
                         rownames(M) <- gsub(" ", ".", rownames(M))
                         colnames(M) <- gsub(" ", ".", colnames(M))
                         M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                                    floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
                         html <- paste0("$$", M, "$$")
                         list(
                           withMathJax(HTML(html))
                         )
                       }
                       
                       else if (input$modelClassO == "Дерево принятия решений" && input$testTrainO == "Тестовая"){
                         M <- matrixTestT
                         rownames(M) <- gsub(" ", ".", rownames(M))
                         colnames(M) <- gsub(" ", ".", colnames(M))
                         M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
                                    floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
                         html <- paste0("$$", M, "$$")
                         list(
                           withMathJax(HTML(html))
                         )
                       }
                       
                       else if (input$modelClassO == "Дерево принятия решений" && input$testTrainO == "Обучающая"){
                         M <- matrixTrainT
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
                   
                 })
                 
                 observeEvent(input$oLearning, {
                   
                   output$ViewOR <- renderUI({
                     
                     fluidRow(
                       column(5, pickerInput(inputId = ns('modelClassO'),
                                             #selected = "",
                                             width = '100%',
                                             label = 'Выбор обученной модели:', 
                                             choices = as.list(input$oClassifLearning),
                                             options = list(`actions-box` = FALSE,
                                                            `none-selected-text` = "Ничего не выбрано"),
                                             multiple = F
                       )), 
                       column(5, pickerInput(inputId = ns('testTrainO'),
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
                         actionButton(ns("resultO"), "Отобразить")
                       ),
                       
                       # column(12, uiOutput(
                       #    ns("resultView"))),
                       #column(12, uiOutput(ns("ViewRes"))),
                       
                       uiOutput(ns("matrixViewO"))
                       
                     )
                   })
                 })
                 

                 
                 output$matrixViewO <- renderUI({
                   if (input$resultO){
                   
                   fluidRow(column(
                     12,
                     box(
                       id = "resultO",
                       title = "Результаты обучения",
                       status = "success",
                       solidHeader = TRUE,
                       width = 12,
                       uiOutput(ns("ViewResO")),
                       column(
                       12, uiOutput(ns("matrixO"))),
                       column(
                         12,uiOutput(ns("modelNNTrain")))

                       
                      ## if (input$testTrain == "Обучающая" && input$modelClass != "Наивный Байес"){
                      ##   str1 <- paste("")
                      ##   str13 <- paste(strong("Оценка важности предикторов: "))
                      ##   str14 <- paste(imageOutput(ns("plot")))
                      ##   HTML(paste(str1, str13, str14, sep = '<br/>'))
                         #imageOutput(ns("plot"))
                      ## }
                       #plotOutput(ns("plot"))
                     )
                     #column(width = 12, align = "center",
                     #       plotOutput(ns("plot")),)
                     #plotOutput(ns("plot")),
                     # column(
                     #   12, plotOutput(ns("plot")))
                     
                   
                   )
                   
                   )

                   }
                 })
                 

                 
                 
                 output$modelNNTrain <- renderUI({ 
                 if (input$modelClassO == "Нейросетевой метод (персептрон)" && input$testTrainO == "Обучающая"){
                   fluidRow(
                     uiOutput(ns("modelNNView")),
                     strong("График, отображающий результаты подбора оптимальных параметров персептрона:"),
                     imageOutput(ns("plotG1"), width = "100%", height = "600px"),
                     strong("Изображение искусственной нейронной сети (персептрона):"),
                     imageOutput(ns("plotG2"), width = "100%", height = "800px")
                     
                     #plotOutput(ns("plot"))
                     #uiOutput(
                     #  ns("ViewStack"))
                   )
                 }
                   else if  (input$modelClassO == "Дерево принятия решений" && input$testTrainO == "Обучающая"){
                     fluidRow(
                       uiOutput(ns("modelTView")),
                       uiOutput(ns("modelTFView")),
                       strong("График, отображающий результаты подбора оптимального значения сложности дерева принятия решений:"),
                       imageOutput(ns("plotGT1"), width = "100%", height = "600px"),
                       strong("Изображение дерева принятия решений:"),
                       imageOutput(ns("plotGT2"), width = "100%", height = "700px")
                       
                       #plotOutput(ns("plot"))
                       #uiOutput(
                       #  ns("ViewStack"))
                     )
                   }
                   #else {NULL}
                 })
                 
                 output$modelNN <- renderPrint({
                   modelsO$mNN
                 })
                 
                 output$modelNNView <- renderUI({ 
                   #train.NN <- formF$formform
                   
                   #          dataNew <- as.data.frame(dataT())
                   if (!is.null(modelsO$mNN)) {
                     fluidRow(column(12, strong("")),
                              column(12, strong("Результаты по подбору оптимальных значений числа скрытых нейронов и
параметра «ослабления весов»:")),
                              column(12, verbatimTextOutput(ns("modelNN"))))
                   }
                 })
                 
                 output$modelT <- renderPrint({
                   modelsO$mT
                 })
                 
                 output$modelTF <- renderPrint({
                   modelsO$mTF
                 })
                 
                 output$modelTView <- renderUI({ 
                   
                   if (!is.null(modelsO$mT)) {
                     fluidRow(column(12, strong("")),
                              column(12, strong("Результаты по подбору оптимального значения сложности дерева классификации:")),
                              column(12, verbatimTextOutput(ns("modelT"))))
                   }
                 })
                 
                 output$modelTFView <- renderUI({ 
                   
                   if (!is.null(modelsO$mTF)) {
                     fluidRow(column(12, strong("")),
                              column(12, strong("Структура дерева принятия решений:")),
                              column(12, verbatimTextOutput(ns("modelTF"))))
                   }
                 })
                 
                 output$plotG1 <- renderPlot({
                   par(mar = numeric(4), family = 'sans')
                   plot(modelsO$mNN, cex_val=0.7, circle_cex=3)
                   #nnnnnn <- formF$formform
                   #plotnet(nnnnnn, cex_val=0.8, circle_cex=4)
                 })
                 
                 output$plotG2 <- renderPlot({
                   par(mar = numeric(4), family = 'sans')
                   plotnet(modelsO$mNN, cex_val=0.7, circle_cex=3)
                   #nnnnnn <- formF$formform
                   #plotnet(nnnnnn, cex_val=0.8, circle_cex=4)
                 })
                 
                 output$plotGT1 <- renderPlot({
                   par(mar = numeric(4), family = 'sans')
                   plot(modelsO$mT)
                   
                 })
                 
                 output$plotGT2 <- renderPlot({
                   par(mar = numeric(4), family = 'sans')
                   #rpart.plot(modelsO$mT)
                   fancyRpartPlot(modelsO$mTF)
                   
                 })
                 
                 
                 
                 output$ViewO <- renderUI({
                   oClassifLearning()
                   uiOutput(
                     ns("ViewOR"))
                 })
                 

                 
                 #rvAcc <- reactiveValues(d=data.frame())
                 #rvAcc$d <- input$bClassifLearning$ATestNB
                 #rvAcc <- data.frame()
                 #r <<- ATestNB
                 #rvAcc$d<-ATestNB
                 
               #  errorStatus <- tryCatch({
               #    h2o.init(startH2O = FALSE)
               #  }, error = function(err) {
               #    errorStatus <- err[1]$message
                #   message(paste0(errorStatus,"\n Initializing new H2O cluster..."))
                 #  # Inititialize H2o cluster
                 #  try({h2o.shutdown(prompt = FALSE)}, silent=TRUE)
                 #  h2o.init(ip = 'localhost', port = 54321, nthreads= -1, max_mem_size = '4g')
                 #  return(errorStatus)
                 #}) # END tryCatch
                 
                 # Shut down H2O cluster on app exit
                 onStop(function() {
                   try({h2o.shutdown(prompt = FALSE)}, silent=TRUE)
                 })

                 
               })
}