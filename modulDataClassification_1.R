moduleUI_3.1 <- function(id, label = "moduleUI_3.1") {
  ns <- NS(id)
  
  tagList(tabBox(
    id = ns("tabsetSampling"),
    width = 12,
    tabPanel(
      "Выбор и разделение данных",
      fluidRow(
        column(12, box(id = "targetPredictor1", title = "Задание целевой переменной и статистически значимых предикторов", status = "success", solidHeader = TRUE, width = 12, uiOutput(ns("varView")))),
        column(12, box(id = "targetPredictor2", title = "Разделение данных на обучающую и тестовую выборки", status = "success", solidHeader = TRUE, width = 12, uiOutput(ns("testPredView"))))
        
      )),
    tabPanel("Решение проблемы несбалансированности данных",
             uiOutput(ns("samplingView")))
  )
  )
  
}

moduleServer_3.1 <- function(id, rv, prefix = "") {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 data <- reactive({
                   data <- rv$datasetInputNewD
                   return(data)
                 })
                 
                 colN <- reactive({ names(data()) })
                 colNF <- reactive({
                   names(data() %>% select_if(is.factor))
                 })
                 colNOT <- reactive({ 
                   #colNA <- eventReactive(input$yDA,{
                   
                   dataNA <- data()
                   dataNA[,input$targetVar] = NULL
                   return(names(dataNA))})
                 
                 
                 
                 observeEvent(input$targetVar, {
                   
                   updatePickerInput(session, "predictor",
                                     #label = "Select Columns",
                                     choices = colNOT(),
                                     selected = "") 
                   
                 })
                 
                 output$varView <- renderUI({ 
                   
                   if (!is.null(data())) {
                   
                   fluidRow(
                     column(3, pickerInput(inputId = ns('targetVar'),
                                           #selected = "",
                                           width = '100%',
                                           label = 'Выбор целевой переменной:', 
                                           choices = colNF(),
                                           options = list(`actions-box` = FALSE,
                                                          `none-selected-text` = "Ничего не выбрано"),
                                           multiple = F
                     )), 
                     column(6, pickerInput(inputId = ns('predictor'),
                                           selected = "",
                                           width = '100%',
                                           label = 'Выбор предикторов:', 
                                           choices = colN(),
                                           options = list(`actions-box` = TRUE,
                                                          `none-selected-text` = "Ничего не выбрано",
                                                          `deselect-all-text` = "Отменить выбор",
                                                          `select-all-text` = "Выбрать всё"),
                                           multiple = T), style = "margin-bottom: 10px;"),
                     column(3, style = "margin-bottom: 10px;",
                            style = "margin-top: 25px;",
                            style = "margin-bottom: 10px;", 
                            actionButton(ns("varB"), "Сохранить")),
                     column(12, uiOutput(ns("variableView")))      
                     
                     
                     #column(12, strong("Результаты дисперсионного анализа (ANOVA-таблица):")),
                     
                   )
                     }
                 })
                 
                 output$variableView <- renderUI({
                   if (input$varB){
                     str1 <- paste(strong("Успешно заданы целевая и предикторные переменные, где:"))
                     str2 <- paste("1. Целевая переменная: ", input$targetVar)
                     str3 <- paste("2. Предикторы: ", paste(input$predictor, collapse = ', '))
                     HTML(paste(str1, str2, str3, sep = '<br/>'))
                   }
                   
                 })
                 
                 output$testPredView <- renderUI({ 
                   
                   if (!is.null(data())) {
                     fluidRow(
                       column(6, sliderInput(ns("sliderTest"), "Доля тестовой выборки (%):",
                                             min = 10, max = 40, value = 30)),
                       column(3, style = "margin-bottom: 10px;",
                              style = "margin-top: 35px;",
                              style = "margin-bottom: 20px;", 
                              actionButton(ns("splitting"), "Разделить")),
                       column(12, uiOutput(ns("testPredDataP"))),
                       
                     )
                     
                   }
                 })
                 
                 testPredData <- eventReactive(input$splitting,{
                   set.seed(101)
                   testV <- input$sliderTest/100
                   trainV <- 1-testV
                   data <- data()
                   ind = sample(2, nrow(data), replace = TRUE, prob=c(trainV, testV))
                   trainset = data[ind == 1,]
                   testset = data[ind == 2,]
                   return(
                     list(
                       trainset = trainset,
                       testset = testset
                     )
                   )
                   
                 })
                 
                 output$testPredDataP <- renderUI({
                   if (input$splitting){
                     str1 <- paste(strong("Данные успешно разделены"))
                     str2 <- paste("В исходном наборе данных содержится ", nrow(data()), " наблюдений (-ия).")
                     str3 <- paste("Обучающая выборка содержит ", nrow(testPredData()$trainset), " наблюдений (-ия).")
                     str4 <- paste("Тестовая выборка содержит ", nrow(testPredData()$testset), " наблюдений (-ия).")
                     HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
                   }
                 })
                 
                 
                 output$samplingView <- renderUI({ 
                   
                   if (!is.null(testPredData()$trainset)) {
                     fluidRow(column(4, pickerInput(inputId = ns('samplingT'),
                                                    selected = 1,
                                                    width = '100%',
                                                    label = 'Выбор метода семплирования:', 
                                                    choices = list("Не использовать балансировку данных" = 1,
                                                                   "Увеличение выборки (oversampling)" = 2, 
                                                                   "SMOTE" = 3, 
                                                                   "Комбинация SMOTE и oversampling" = 4, 
                                                                   "Сокращение выборки (undersampling)" = 5,
                                                                   "ADASYN" = 6),
                                                    options = list(`actions-box` = FALSE,
                                                                   `none-selected-text` = "Ничего не выбрано"),
                                                    multiple = F
                     )),
                     column(3, style = "margin-bottom: 10px;",
                            style = "margin-top: 25px;",
                            style = "margin-bottom: 10px;", 
                            actionButton(ns("balance"), "Перебалансировать")),
                     column(12, uiOutput(ns("balanceView")))
                     
                     
                     )
                   }
                 })
                 
                 
                 
                 output$balanceView <- renderUI(
                   #samplingData()
                   if (input$balance){
                   fluidRow(
                   #column(6, strong("Количество объектов в каждом факторе (классе) до перебалансировки:")),
                   column(6, strong("Количество объектов в каждом классе до перебалансировки:"),
                          imageOutput(ns("plotFactor")), 
                          style = "margin-bottom: 10px;"),
                   #column(6, strong("Количество объектов в каждом факторе (классе) после перебалансировки:")),
                   column(6, strong("Количество объектов в каждом классе после перебалансировки:"),
                          imageOutput(ns("plotFactorN")), 
                          style = "margin-bottom: 10px;")
                   )
                   
                 })
                 
                 samplingData <- eventReactive(input$balance,{
                   dataset <- as.data.frame(testPredData()$trainset)
                   
                   if (input$samplingT == 1) {
                     dataB <- dataset
                   }
                   
                   if (input$samplingT == 2) {
                     #RandOverClassif(Засорённость.класс ~ ., trainset[,-15], C.perc = "balance", repl = TRUE)
                     set.seed(9560)
                     form <- paste0(input$targetVar, "~ .")
                     #print(as.formula(form))
                     dataB <- RandOverClassif(as.formula(form), dataset, C.perc = "balance", repl = TRUE)
                     
                   }
                   
                   if (input$samplingT == 3) {
                     set.seed(9560)
                     #print(dataset)
                     #print(input$targetVar)
                     #print(colnames(dataset)[input$targetVar])
                     #dataset[, input$targetVar] <- lapply(dataset[, input$targetVar], as.factor)
                     #as.formula(paste0(colnames(trainset)[14], "~ ."))
                     form <- paste0(input$targetVar, "~ .")
                     #print(as.formula(form))
                     dataB <- SmoteClassif(as.formula(form), dataset, C.perc = "balance", dist = "HEOM", k=8)
                     #AdasynClassif(form, dat, baseClass = NULL, beta = 1, dth = 0.95,
                     #              k = 5, dist = "Euclidean", p = 2)
                     #dataB <- AdasynClassif(as.formula(form), dataset, baseClass = NULL, beta = 1, dth = 0.95,
                     #                       k = 5, dist = "HEOM", p = 2)
                   }
                   
                   if (input$samplingT == 4) {
                     #RandOverClassif(Засорённость.класс ~ ., oveBalan, "balance")
                     #SmoteClassif(Засорённость.класс ~ ., trainset, list(trainset$Засорённость.класс = 5), dist = "HEOM")
                     set.seed(9560)
                     form <- paste0(input$targetVar, "~ .")
                     #print(as.formula(form))
                     
                    # your_length <- 10
                    # empty_vec <- rep(2, your_length)
                    # empty_vec
                    # R <- lapply(dataNsk[14], levels)
                   #  length(R[[1]]) #длина
                    # B <- R[[1]]
                    # setNames(as.list( sort( unlist(A))), sort(B))
                     
                     levels <- lapply(dataset[input$targetVar], levels)
                     print(levels)
                     len <- length(levels[[1]])
                     print(len)
                     empty_vec <- as.list(rep(2,len))
                     print(empty_vec)
                     lev <- levels[[1]]
                     listB <- setNames(as.list( sort( unlist(empty_vec))), sort(lev))
                     
                     print(listB)
                     
                     dataB1 <- SmoteClassif(as.formula(form),dataset, C.perc = listB, dist = "HEOM", k=5, p = 2)
                     dataB <- RandOverClassif(as.formula(form), dataB1, C.perc = "balance", repl = TRUE)
                    
                   }
                   
                   if (input$samplingT == 5) {
                     #RandUnderClassif(form, dat, C.perc = "balance", repl = FALSE)
                     set.seed(9560)
                     form <- paste0(input$targetVar, "~ .")
                     #print(as.formula(form))
                     dataB <- RandUnderClassif(as.formula(form), dataset, C.perc = "balance", repl = TRUE)
                     
                   }
                   
                   if (input$samplingT == 6) {
                     set.seed(9560)
                     #print(dataset)
                     #print(input$targetVar)
                     #print(colnames(dataset)[input$targetVar])
                     #dataset[, input$targetVar] <- lapply(dataset[, input$targetVar], as.factor)
                     #as.formula(paste0(colnames(trainset)[14], "~ ."))
                     form <- paste0(input$targetVar, "~ .")
                     #print(as.formula(form))
                     #dataB <- SmoteClassif(as.formula(form), dataset, C.perc = "balance", dist = "HEOM", k=8)
                     #AdasynClassif(form, dat, baseClass = NULL, beta = 1, dth = 0.95,
                     #              k = 5, dist = "Euclidean", p = 2)
                     dataB <- AdasynClassif(as.formula(form), dataset, baseClass = NULL, beta = 1, dth = 0.95,
                                            k = 5, dist = "HEOM", p = 2)
                   }
                   
                   dataB
                   #print(table(dataB[input$targetVar]))
                   
                 })
                 
                 plotFactorIm <- eventReactive(input$balance,{
                   dataset <- as.data.frame(testPredData()$trainset)
                   
                   dataset %>% 
                     count(dataset[,input$targetVar]) %>%
                     select(col = 1, everything() ) %>% 
                     ggplot() +
                     geom_col(aes(x = reorder(col , desc(n)) , y = n ,
                                  fill = col), size = 6, show.legend = FALSE) +
                     geom_text(aes(x = col, y = n/2, label = n),
                               size = 6, fontface = "bold") +
                     labs(x = 'Уровни категориального фактора' , y = 'Количество наблюдений') +
                     coord_flip() +
                     theme_bw()+
                     theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)) 
                 })
                 
                 output$plotFactor <- renderPlot({
                   plotFactorIm()
                 })
                 
                 plotFactorImN <- eventReactive(input$balance,{
                   dataset <- as.data.frame(samplingData())
                   
                   dataset %>% 
                     count(dataset[,input$targetVar]) %>%
                     select(col = 1, everything() ) %>% 
                     ggplot() +
                     geom_col(aes(x = reorder(col , desc(n)) , y = n ,
                                  fill = col), size = 6, show.legend = FALSE) +
                     geom_text(aes(x = col, y = n/2, label = n),
                               size = 6, fontface = "bold") +
                     labs(x = 'Уровни категориального фактора' , y = 'Количество наблюдений') +
                     coord_flip() +
                     theme_bw()+
                     theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)) 
                 })
                 
                 output$plotFactorN <- renderPlot({
                   plotFactorImN()
                 })
                 
                 rvClass <- reactiveValues()
                 observeEvent(input$balance, {
                   rvClass$targetVar <- input$targetVar
                   rvClass$predictor <- input$predictor
                   rvClass$dataTest <- as.data.frame(testPredData()$testset)
                   #rvClass$dataTrain <- as.data.frame(testPredData()$trainset)}, 
                   rvClass$dataTrain <- as.data.frame(samplingData())}, 
                   ignoreInit = FALSE)
                 return(rvClass)
                 
                 
                 
               }
  )
  
  
  
  
  
  
  
  
  
  
  
  #})
}
