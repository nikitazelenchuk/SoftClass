moduleUI_2 <- function(id, label = "moduleUI_2") {
  ns <- NS(id)
  
  tagList(tabBox(id = ns("tabsetPrimary"),
                 width = 12,
                 tabPanel("Визуализация данных",
                          uiOutput(ns("diagramsView"))),
                 tabPanel("Числовые характеристики",
                          uiOutput(ns("summaryView"))
                 ),
                 tabPanel("Законы распределения",
                          uiOutput(ns("distributionLawView"))),
                 tabPanel("Исследование взаимосвязей",
                          uiOutput(ns("CorANOVAView")))))
}

moduleServer_2 <- function(id, rv, prefix = "") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      data <- reactive({
        data <- rv$datasetInputNewD
        return(data)
        #View(dataT)
        #updateSelectInput( session, "colonnes", choices = colonnes)
      })
      
      
      #datasetInputNewD <- paste(YProxy$statsView)
      
      output$summaryFac <- renderPrint({
        summary(data() %>% select_if(is.factor))
      })
      
      output$summaryNum <- renderPrint({
        summary(data() %>%   select_if(is.numeric))
      })
      
      
      
      output$summaryView <- renderUI({ 
        
        #          dataNew <- as.data.frame(dataT())
        if (!is.null(data())) {
          fluidRow(column(12, strong("Объёмы выборок по каждому уровню категориальных факторов:")), 
                   column(12, verbatimTextOutput(ns("summaryFac"))),
                   column(12, strong("Основные статистические характеристики количественных признаков:")), 
                   column(12, verbatimTextOutput(ns("summaryNum"))),)}})
      
      
      output$diagramsView <- renderUI({ 
        
        if (!is.null(data())) {
        fluidRow(
          column(12, radioGroupButtons(
            inputId = ns("diagrams"),
            choices = c("Линейчатые диаграммы" = 1, "Категоризованные диаграммы размахов" = 2),
            #btn_icon = paste0("align-", c("left", "center", "justify", "right")),
            status = "success"
          ), style = "margin-bottom: 10px;"),
          column(12, uiOutput(ns("factorView"))),
          column(12, uiOutput(ns("boxView"))),
        )
        }
      })
      
      
      colNF <- reactive({ names(data() %>% select_if(is.factor)) })
      
      output$factorView <- renderUI({ 
        
        #          dataNew <- as.data.frame(dataT())
        #if (!is.null(data())) {
        if (input$diagrams == 1 && !is.null(data()) && ncol(data() %>% select_if(is.factor)) > 0) {
          fluidRow(
            column(6, pickerInput(inputId = ns('categFactor'),
                                  selected = "",
                                  width = '100%',
                                  label = 'Выбор категориального фактора:', 
                                  choices = colNF(),
                                  options = list(`actions-box` = FALSE,
                                                 `none-selected-text` = "Ничего не выбрано"),
                                  multiple = F), 
                   style = "margin-bottom: 10px;"),
            column(12, strong("Линейчатая диаграмма, отображающая количество объектов в каждом факторе (классе):")),
            column(12, imageOutput(ns("plotFactor")), 
                   style = "margin-bottom: 10px;"),
            column(12, strong("Процентное соотношение количества объектов в каждом факторе (классе):")),
            column(12, verbatimTextOutput(ns("roundFactor")))
          )  
        } 
      })
      
      output$boxView <- renderUI({ 
        
        #          dataNew <- as.data.frame(dataT())
        #if (!is.null(data())) {
        if (input$diagrams == 2 && !is.null(data()) && ncol(data() %>% select_if(is.factor)) > 0) {
          fluidRow(
            column(4, pickerInput(inputId = ns('categFactorInBox'),
                                  selected = "",
                                  width = '100%',
                                  label = 'Выбор анализируемого фактора:', #  категориального
                                  choices = colNF(),
                                  options = list(`actions-box` = FALSE,
                                                 `none-selected-text` = "Ничего не выбрано"),
                                  multiple = F), 
                   style = "margin-bottom: 10px;"),
            column(3, pickerInput(inputId = ns('factorX'),
                                  selected = "",
                                  width = '100%',
                                  label = 'Выбор фактора (ось X):', 
                                  choices = colN(),
                                  options = list(`actions-box` = FALSE,
                                                 `none-selected-text` = "Ничего не выбрано"),
                                  multiple = F), 
                   style = "margin-bottom: 10px;"),
            column(3, pickerInput(inputId = ns('factorY'),
                                  selected = "",
                                  width = '100%',
                                  label = 'Выбор фактора (ось Y):', 
                                  choices = colN(),
                                  options = list(`actions-box` = FALSE,
                                                 `none-selected-text` = "Ничего не выбрано"),
                                  multiple = F), 
                   style = "margin-bottom: 10px;"),
            column(1, style = "margin-bottom: 10px;",
                   style = "margin-top: 25px;",
                   style = "margin-bottom: 10px;", 
                   #align = "center",
                   actionButton(ns("viewBox"), "Построить")),
            column(12, uiOutput(ns("box"))), 
            
            #column(12, strong("Линейчатая диаграмма, отображающая количество объектов в каждом факторе (классе):")),
            #column(12, imageOutput(ns("plotBox"))) 
            #       style = "margin-bottom: 10px;"),
            #column(12, strong("Процентное соотношение количества объектов в каждом факторе (классе):")),
            #column(12, verbatimTextOutput(ns("roundFactor"))
          )
          #)  
        }
      })
      
      plotBoxIm <- eventReactive(input$viewBox,{
        
        dataset <- data()
        
        xx <- paste(input$factorX)
        
        ggplot(data = dataset, aes_string(x = input$factorX, y = input$factorY)) +
          geom_boxplot(aes_string(fill = input$categFactorInBox))+
          theme_bw()+
          theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14),
                legend.text = element_text(size = 14), legend.title = element_text(size = 16))

      })
      
      output$plotBox <- renderPlot({
       # if (input$viewBox){
        plotBoxIm()
       # }
      })
      
      output$box <- renderUI({
        if (input$viewBox){
          #options(spinner.color="#00a65a")
          fluidRow(
            column(12, strong("Диаграмма размахов:")),
            column(12, imageOutput(ns("plotBox"), height = "600px",)))
        }
        
      })
      
      #observeEvent(input$chgType, datasetInput()[, input$typeVar])
      
      plotFactorIm <- eventReactive(input$categFactor,{
        
        data() %>% 
          count(data()[,input$categFactor]) %>%
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
      
      output$roundFactor <- renderPrint({
        round(prop.table(table(data()[input$categFactor])) * 100, digits = 2)
      })
      
      colN <- reactive({ names(data()) })
      
      output$CorANOVAView <- renderUI({ 
        
        if (!is.null(data())) {
          fluidRow(
            column(12, radioGroupButtons(
              inputId = ns("CorANOVADA"),
              choices = c("Корреляционный анализ данных" = 1, "Дисперсионный анализ данных" = 2),
              #btn_icon = paste0("align-", c("left", "center", "justify", "right")),
              status = "success"
            ), style = "margin-bottom: 10px;"),
            column(12, uiOutput(ns("corView"))),
            column(12, uiOutput(ns("ANOVAView"))),
          )
        }
      })
      
      
      output$corView <- renderUI({ 
        
        if (input$CorANOVADA == 1) {
          fluidRow(
            column(12, pickerInput(inputId = ns('cor'),
                                   selected = "",
                                   width = '100%',
                                   label = 'Выбор переменных:', 
                                   choices = colN(),
                                   options = list(`actions-box` = TRUE,
                                                  `none-selected-text` = "Ничего не выбрано",
                                                  `deselect-all-text` = "Отменить выбор",
                                                  `select-all-text` = "Выбрать всё"),
                                   multiple = T), style = "margin-bottom: 10px;"),
            column(4, pickerInput(inputId = ns('corType'),
                                  selected = NULL,
                                  width = '100%',
                                  label = 'Выбор типа ранговой корреляции:', 
                                  #"Ничего не выбрано" = 0,
                                  choices = list(Спирмена = 1, Кендалла = 2),
                                  options = list(`actions-box` = FALSE,
                                                 `none-selected-text` = "Ничего не выбрано"),
                                  multiple = F
            )), 
            column(4, style = "margin-bottom: 10px;",
                   style = "margin-top: 25px;",
                   style = "margin-bottom: 10px;", 
                   #align = "center",
                   actionButton(ns("viewCor"), "Создать матрицу")),
            column(12, uiOutput(ns("correlation"))))
        }
      })
      
      output$correlation <- renderUI({
        if (input$viewCor){
          options(spinner.color="#00a65a")
          fluidRow(
            #column(12, strong("Корреляционная матрица:")),
            uiOutput(ns("corM1")),
            column(12, withSpinner(imageOutput(ns("plotCor"), height = "700px",), type = 1)),
            uiOutput(ns("corM2")),
            uiOutput(ns("corM4")),
            column(12, verbatimTextOutput(ns("CorM"))),
            uiOutput(ns("corM3")),
            column(12, verbatimTextOutput(ns("CorMP"))))
        }
        
      })
      
      plotCorIm <- eventReactive(input$viewCor,{
        dataset <- data()
        dataset[, input$cor] <- lapply(dataset[, input$cor], as.numeric)
        
        if (input$corType == 1) {
          chart.Correlation(dataset[, input$cor],  histogram=TRUE, pch=19, method = c("spearman"))
        }
        
        if (input$corType == 2) {
          chart.Correlation(dataset[, input$cor],  histogram=TRUE, pch=19, method = c("kendall"))  
        }
      })
      
      output$plotCor <- renderPlot({
        if (input$viewCor){
          plotCorIm()}
      })
      
      output$corM1 <-  renderUI({
        #n <- ncol(data()[, input$cor])
        if (ncol(data()[, input$cor]) > 0  && input$viewCor)
          column(12, strong("Корреляционная матрица:"))
      })
      
      output$corM4 <-  renderUI({
        if (ncol(data()[, input$cor]) == 2 && input$viewCor)
          column(12, strong("Результаты корреляционного анализа данных:"))
      })
      
      output$corM2 <- renderUI({
        if (ncol(data()[, input$cor]) > 2 && input$viewCor)
          column(12, strong("Матрица ранговых коэффициентов корреляции:"))
      })
      
      output$corM3 <- renderUI({
        if (ncol(data()[, input$cor]) > 2 && input$viewCor)
          column(12, strong("Матрица p-значений, соответствующих ранговым коэффициентам корреляции:"))
      })
      
      
      
      CorMX <- eventReactive(input$viewCor,{
        dataset <- data()
        dataset[, input$cor] <- lapply(dataset[, input$cor], as.numeric)
        
        if (input$corType == 1) {
          corMatrix <- dataset[, input$cor] %>% cor_mat(method = "spearman")
          #return(corMatrix)
          #chart.Correlation(dataset[, input$cor],  histogram=TRUE, pch=19, method = c("spearman"))
        }
        
        if (input$corType == 2) {
          corMatrix <- dataset[, input$cor] %>% cor_mat(method = "kendall")
          #return(corMatrix)
          #chart.Correlation(dataset[, input$cor],  histogram=TRUE, pch=19, method = c("kendall"))  
        }
        corMatrix
      })
      
      CorMX2 <- eventReactive(input$viewCor,{
        dataset <- data()
        dataset[, input$cor] <- lapply(dataset[, input$cor], as.numeric)
        
        if (input$corType == 1) {
          corMatrix <- dataset[, input$cor] %>% cor_test(method = "spearman")
          #return(corMatrix)
          #chart.Correlation(dataset[, input$cor],  histogram=TRUE, pch=19, method = c("spearman"))
        }
        
        if (input$corType == 2) {
          corMatrix <- dataset[, input$cor] %>% cor_test(method = "kendall")
          #return(corMatrix)
          #chart.Correlation(dataset[, input$cor],  histogram=TRUE, pch=19, method = c("kendall"))  
        }
        corMatrix
      })
      
      output$CorM <- renderPrint({
        #corMatrix <- dataset[, input$cor] %>% cor_mat(method = "spearman")
        #return(corMatrix)
        n <- ncol(data()[, input$cor])
        if (n > 2){
          CorMX()}
        else {
          CorMX2()
        }
      })
      
      output$CorMP <- renderPrint({
        
        if (ncol(data()[, input$cor]) > 2){
          #corMatrix <- dataset[, input$cor] %>% cor_mat(method = "spearman")
          #return(corMatrix)
          CorMX() %>% cor_get_pval()}
      })
      
      # removecolumn <- function(data, input$yDA){
      #   df[ , -which(names(df) %in% nameofthecolumn)]
      #}
      
      colNA <- reactive({ 
        #colNA <- eventReactive(input$yDA,{
        
        dataNA <- data()
        dataNA[,input$yDA] = NULL
        return(names(dataNA))})
      
      observeEvent(input$yDA, {
        
        updatePickerInput(session, "xDA",
                          #label = "Select Columns",
                          choices = colNA(),
                          selected = "") 
        
      })
      
      
      output$ANOVAView <- renderUI({ 
        
        if (input$CorANOVADA == 2) {
          # fit <- lm(swiss[,input$outcome] ~ swiss[,input$indepvar])
          fluidRow(column(3, pickerInput(inputId = ns('yDA'),
                                         selected = "",
                                         width = '100%',
                                         label = 'Выбор целевого показателя:', 
                                         choices = colN(),
                                         options = list(`actions-box` = FALSE,
                                                        `none-selected-text` = "Ничего не выбрано"),
                                         multiple = F
          )), 
          column(6, pickerInput(inputId = ns('xDA'),
                                selected = "",
                                width = '100%',
                                label = 'Выбор предикторных переменных:', 
                                choices = colN(),
                                options = list(`actions-box` = TRUE,
                                               `none-selected-text` = "Ничего не выбрано",
                                               `deselect-all-text` = "Отменить выбор",
                                               `select-all-text` = "Выбрать всё"),
                                multiple = T), style = "margin-bottom: 10px;"),
          column(3, style = "margin-bottom: 10px;",
                 style = "margin-top: 25px;",
                 style = "margin-bottom: 10px;", 
                 actionButton(ns("viewANOVA"), "Выполнить")),
          uiOutput(ns("ANOVADAView"))   ,   
          
          
          #column(12, strong("Результаты дисперсионного анализа (ANOVA-таблица):")),
          column(12, verbatimTextOutput(ns("ANOVADA"))),
          
          )
        }
      })
      
      output$ANOVADAView <- renderUI(
        if (input$viewANOVA){
          column(12, strong("Результаты дисперсионного анализа (ANOVA-таблица):"))})
      
      output$ANOVADA<- renderPrint({
        ANOVA()
      })
      
      ANOVA <- eventReactive(input$viewANOVA,{
        dataset <- data()
        
        #y <- extract(dataset, input$yDA)
        #x <- extract(dataset, input$xDA)
        
        dataset[, input$yDA] <- lapply(dataset[, input$yDA], as.numeric)
        
        #f <- reactive({
        #  as.formula(paste(input$yDA, "~", ))
        #})
        form <- paste(dataset[, input$yDA], " ~ ", paste(input$xDA, collapse = " + "))
        
        modelANOVA <- lm(as.formula(form), data = data())
        SumAOV <- summary.aov(modelANOVA)
        SumAOV
      })
      
      colNNum <- reactive({ names(data() %>% select_if(is.numeric)) })
      
      output$distributionLawView  <- renderUI(
        if (!is.null(data())) {
        fluidRow(column(3, pickerInput(inputId = ns('DistValue'),
                                       selected = "",
                                       width = '100%',
                                       label = 'Выбор переменной:', 
                                       choices = colNNum(),
                                       options = list(`actions-box` = FALSE,
                                                      `none-selected-text` = "Ничего не выбрано"),
                                       multiple = F
        )),
        column(3, pickerInput(inputId = ns('DistСrit'),
                              selected = "",
                              width = '100%',
                              label = 'Выбор критерия согласия:', 
                              choices = list("Шапиро-Уилка" = 1, 
                                             "Андерсона-Дарлинга" = 2, 
                                             "Крамера-фон Мизеса" = 3, 
                                             "Лиллиефорса" = 4,
                                             "Шапиро-Франсия" = 5),
                              options = list(`actions-box` = FALSE,
                                             `none-selected-text` = "Ничего не выбрано"),
                              multiple = F
        )),
        column(3, style = "margin-bottom: 10px;",
               style = "margin-top: 25px;",
               style = "margin-bottom: 10px;", 
               actionButton(ns("viewDistributionLaw"), "Рассчитать")),
        column(12, uiOutput(ns("distributionView"))),
        #column(12, strong("Результаты дисперсионного анализа (ANOVA-таблица):")),
        #column(3, pickerInput(inputId = ns('DistValueF'),
        #                      selected = "",
        #                      width = '100%',
        #                      label = 'Выбор переменной:', 
        #                      choices = colNF(),
        #                      options = list(`actions-box` = FALSE,
        #                                     `none-selected-text` = "Ничего не выбрано"),
        #                      multiple = F
        #)),
        #column(3, style = "margin-bottom: 10px;",
        #       style = "margin-top: 25px;",
        #       style = "margin-bottom: 10px;", 
        #       actionButton(ns("viewDistributionLawF"), "Рассчитать")),
        
        ) }
      )
      
      dist <- eventReactive(input$viewDistributionLaw,{
        #renderPrint({
        #corMatrix <- dataset[, input$cor] %>% cor_mat(method = "spearman")
        #return(corMatrix)
        #n <- ncol(data()[, input$cor])
        dataset <- data()
        dataset[, input$DistValue] <- lapply(dataset[, input$DistValue], as.numeric)
        #View(data()[[input$DistValue]])
        
        if (input$DistСrit == 1){
          test <- shapiro.test(dataset[[input$DistValue]])
        }
        
        if (input$DistСrit == 2){
          test <- ad.test(dataset[[input$DistValue]])
        }
        
        if (input$DistСrit == 3){
          test <- cvm.test(dataset[[input$DistValue]])
        }
        
        if (input$DistСrit == 4){
          test <- lillie.test(dataset[[input$DistValue]])
        }  
        
        if (input$DistСrit == 5){
          test <- sf.test(dataset[[input$DistValue]])
        }
        
        test
        #})
        
      })
      
      output$distribution  <- renderPrint({
        dist()
      })
      
      output$distributionView <- 
        #eventReactive(input$viewDistributionLaw,{
        renderUI({
          if (input$viewDistributionLaw){
            fluidRow(
              column(12, strong("Результаты проверки на соответсвие распределения нормальному закону:")),
              column(12, verbatimTextOutput(ns("distribution"))))
            #  }
            # )
          }})
      
      
    }
  )
}