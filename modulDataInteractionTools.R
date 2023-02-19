js <- c(
  "function(){", 
  "  this.api().columns().every(function(i){",
  "    var column = this;",
  "    var $select =",
  "      $('<select multiple=\"multiple\">' +",
  "         '<option value=\"\"></option>' +",
  "      '</select>')",
  "      .appendTo($('#th'+i).empty())", 
  "      .on('change', function(){",
  "        var vals = $('option:selected', this).map(function(idx, element){",
  "          return $.fn.dataTable.util.escapeRegex($(element).val());",
  "        }).toArray().join('|');",
  "        column.search(",
  "          vals.length > 0 ? '^(' + vals + ')$' : '', true, false",
  "        ).draw();",
  "      });",
  "    var data = column.data();",
  "    if(i == 0){",
  "      data.each(function(d, j){",
  "        $select.append('<option value=\"' + d + '\">' + d + '</option>');",
  "      });",
  "    }else{",
  "      data.unique().sort().each(function(d, j){",
  "        $select.append('<option value=\"' + d + '\">' + d + '</option>');",
  "      });",
  "    }",
  "    $select.select2({width: '100%', closeOnSelect: false});",
  "  });",
  "}")

js2 <- c(
  "function(){", 
  "  this.api().columns().every(function(i){",
  "    var column = this;",
  "    var $select =",
  "      $('<select multiple=\"multiple\">' +",
  "         '<option value=\"\"></option>' +",
  "      '</select>')",
  "      .appendTo($('#th'+i).empty())", 
  "      .on('change', function(){",
  "        var vals = $('option:selected', this).map(function(idx, element){",
  "          return $.fn.dataTable.util.escapeRegex($(element).val());",
  "        }).toArray().join('|');",
  "        column.search(",
  "          vals.length > 0 ? '^(' + vals + ')$' : '', true, false",
  "        ).draw();",
  "      });",
  "    var data = column.data();",
  "    if(i == 0){",
  "      data.each(function(d, j){",
  "        $select.append('<option value=\"' + d + '\">' + d + '</option>');",
  "      });",
  "    }else{",
  "      data.unique().sort().each(function(d, j){",
  "        $select.append('<option value=\"' + d + '\">' + d + '</option>');",
  "      });",
  "    }",
  "    $select.select2({width: '100%', closeOnSelect: false});",
  "  });",
  "}")

moduleUI_1 <- function(id, label = "moduleUI_1") {
  ns <- NS(id)
  


  tagList(

    tabBox(id = ns("tabsetData"),
           
           
      width = 12,
      # The id lets us use input$tabset1 on the server to find the current tab
      
      tabPanel("Загрузка данных", 
               tags$script(src = "fileInput_text.js"),
               #tags$script(jscode_upload_msg),
               fluidRow(column(5,
                               #tags$style(HTML("

      #.selectize-input {
      #  width: 110px;

      #}
      
      

    #")

    tags$style(HTML("

      .selectize-input {
        width: 180px;

      }

    ")),
               #"Загрузить исходные данные из файла формата:",
               selectInput(ns("fileTypeInput"), label = "Загрузить исходные данные из файла формата:",
                           width = '100%',
                           choices = list("xls | xlsx" = 1, "csv | txt" = 2), 
                           selected = "",
                           multiple = FALSE
                           )),
    
    column(12, uiOutput(ns("radiobuttonChoiceOutput"))),
    column(12, fileInput(ns("file2"), NULL, width = '70%', buttonLabel = "Выбрать...", placeholder = "Файл не выбран", 
              accept = c(
                'text/csv',
                'text/comma-separated-values,text/plain',
                '.csv',
                '.xlsx',
                '.xls'))),
               #uiOutput(ns("radiobuttonChoiceOutput"))
    uiOutput(ns("table1Output"))),
               ),
      tabPanel("Отбор столбцов", 
               tags$script(src = "fileInput_text.js"),
               fluidRow(
               column(12, uiOutput(ns("pickerСolumn"))),
               column(12, uiOutput(ns("table2Output"))),)),
      tabPanel("Задание и преобразование типов",  
               fluidRow(column(12, uiOutput(ns("statsView"))),
                        column(12, uiOutput(ns("statsViewNew"))))
)
  ))
}

moduleServer_1 <- function(id, prefix = "") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observe({
        req(input$file2)
        session$sendCustomMessage("upload_msg", "Загрузка данных завершена")
      })
      
      output$radiobuttonChoiceOutput <- renderUI({
        #ns <- session$ns
        #ns <- NS(id)
        if (input$fileTypeInput == 2) {
          tagList(
            checkboxInput(ns("header"), "Заголовок", TRUE),
            fluidRow(column(2, 
                            style=list("padding-right: 190px;"),
          selectInput(ns("sep"), label = "Разделитель:",
                      #width = '100%',
                      choices = list("Запятая" = ",",
                                     "Точка с запятой" = ";",
                                     "Табуляция" = "\t"), 
                      selected = ";")),
          column(2, 
                 style=list("padding-right: 190px; padding-left: 0px;"),
                 selectInput(ns("quote"), "Кавычки:",
                             #width = '100%',
                       choices = list("Нет" = ";",
                                   "Двойные кавычки" = '"',
                                   "Одинарные кавычки" = "'"),
                       selected = '')),
          column(6, 
                 style=list("padding-left: 0px;"),
                 #width = '100%',
                 selectInput(ns("dec"), "Десятичный разделитель:",
                       choices = list("Запятая" = ",",
                                   "Точка" = "."),
                       selected = ',')))
          )
        }
        


        
      })
      
      
      dataT <- reactive({
        req(input$file2)
        File <- input$file2
        
        if (input$fileTypeInput == "2" && file_ext(File$name) %in% c(
          'text/csv',
          'text/comma-separated-values',
          'text/tab-separated-values',
          'text/plain',
          'csv',
          'tsv',
          'txt'
        )){
          df <- read.csv(input$file2$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote,
                         dec = input$dec)
        } else if (input$fileTypeInput == "2"){
          shinyalert("Внимание!", "Загружен файл неверного формата.
                     Пожалуйста, проверьте формат загружаемого файла.", type = "error", confirmButtonCol = "#00a65a",)
          return (df<-NULL)
        } else if (input$fileTypeInput == "1" && file_ext(File$name) %in% c(
          'xlsx', 
          'xls'
          )){
          df <- read_excel(input$file2$datapath)
        } else if (input$fileTypeInput == "1"){
          shinyalert("Внимание!", "Загружен файл неверного формата.
                     Пожалуйста, проверьте формат загружаемого файла.", type = "error")
          return (df<-NULL)
        }
        return(v$data <- df)
        #v$data
        
        })
      

      
    
      sketch <- reactive({
      sketch <- tags$table(
        tags$thead(
          tags$tr(
            tags$th(), lapply(names(dataT()), tags$th)
          ),
          tags$tr(
            lapply(c(0, 1:ncol(dataT())), function(i) tags$th(id = paste0("th", i)))
          )
        )
      )
      })
      
      
      output$table1 <- DT::renderDataTable(DT::datatable({
        dataNew <<- as.data.frame(dataT())
        dataNew
      },
      #container = sketch(),
      #НАДОcallback=JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Все" )'),
      #filter = list(position = 'top'),
      options = list(
        scrollX = TRUE, lengthMenu = list(c(5, 10, 20, 50, 100, -1), 
                                          c('5','10', '20', '50', '100', 'Все')),
        language = list(
          search = "Поиск:", infoEmpty = "Показаны записи от 0 до 0 из 0", 
          info = "Показаны записи от _START_ до _END_ из _TOTAL_",
          lengthMenu = "Показать _MENU_ записей",
          infoFiltered = " — число записей после фильтрации (общее число записей без фильтрации равно _MAX_)",
          paginate = list("previous"= "Предыдущая", "next" = "Следующая"),
          zeroRecords = "Совпадений не найдено"),
        orderCellsTop = TRUE,
        initComplete = JS(js),
        columnDefs = list(
          list(targets = "_all", className = "dt-center")
        ))), server = FALSE)

      
      #output$table1 <- c(table1, list(htmlDep))
      output$table1Output <- renderUI({ 

        #          dataNew <- as.data.frame(dataT())
        if (!is.null(dataT())) {
          box(id = "table1Output", title = "Загруженный набор данных", status = "success", collapsible = TRUE, width = 12, DTOutput(ns("table1")))
        }
      })
      
      output$pickerСolumn <- renderUI({
        dataNew <<- as.data.frame(dataT())
        tagList(fluidRow(
          column(12, pickerInput(inputId = ns('pickСolumn'),
                    width = "100%",
                    selected = colnames(dataNew),
                    label = 'Выбор столбцов из исходного набора данных:', 
                    choices = colnames(dataNew),
                    options = list(`actions-box` = TRUE,
                                   `none-selected-text` = "Ничего не выбрано",
                                   `deselect-all-text` = "Отменить выбор",
                                   `select-all-text` = "Выбрать всё"),
                    #choicesOpt = list(style = rep(("color: black;" ),100)),
                    multiple = T), style = "margin-bottom: 10px;"),
          #br(),
          #br(),
          #br(),
          actionButton(ns("viewNewData"), "Просмотр выбранного", style = "margin-left: 15px; margin-bottom: 10px;"),
          #br(),
          column(12, strong("Сохранить сформированный набор данных в файл формата:")),
          #actionButton(ns("downloadNewData"), "Скачать полученный набор данных", style = "margin-bottom: 10px;"),
          column(12, downloadButton(ns("downloadDataCsv"), "CSV"), downloadButton(ns("downloadDataXlsx"), "XLSX"), style = "margin-bottom: 10px;")))
      })
      
    #  eventReactive(input$viewNewData,{
    #    counter(counter() == 1)
     # })
      
      datasetInput <- eventReactive(input$viewNewData,{
        #counter(counter() == 1)
        datasetInput <- dataT() %>% 
          select(input$pickСolumn)
        names(datasetInput) <- str_replace_all(names(datasetInput), c(" " = "." , "," = "" ))
        
        v$data <- datasetInput
        
        
        #counter() <- 0
        
        
        return(datasetInput)
      })
      
      sketch2 <- reactive({
        sketch2 <- tags$table(
          tags$thead(
            tags$tr(
              tags$th(), lapply(names(datasetInput()), tags$th)
            ),
            tags$tr(
              lapply(c(0, 1:ncol(datasetInput())), function(i) tags$th(id = paste0("th", i)))
            )
          )
        )
      })
      
      v <- reactiveValues(data = NULL)
      
      proxy = dataTableProxy("table1_cell_edit")
      
      observeEvent(input$table2_cell_edit, {
        #print(v$data)
        #datasetInput <- v$data
        #######v$data <- datasetInput()
        row  <- input$table2_cell_edit$row
        clmn <- input$table2_cell_edit$col
        v$data[row, clmn] <- input$table2_cell_edit$value
        #View(v$data)
        #datasetInput <- function()(
        #  datasetInput <- v$data)
        #  return(counter() <- 0)
        ##########counter(counter() == 1)

         # return(datasetInput)
      
        #View(datasetInput())
        
      })
      
      output$table2 <- DT::renderDataTable(DT::datatable({
        #dataNew <<- as.data.frame(v$data)
        dataNew <<- as.data.frame(datasetInput())
        dataNew
      }, editable = TRUE,
      container = sketch2(),
      #НАДОcallback=JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Все" )'),
      #filter = list(position = 'top'),
      options = list(
        orderCellsTop = TRUE,
        initComplete = JS(js2),
        scrollX = TRUE, lengthMenu = list(c(5, 10, 20, 50, 100, -1), 
                                          c('5','10', '20', '50', '100', 'Все')),
        language = list(
          search = "Поиск:", infoEmpty = "Показаны записи от 0 до 0 из 0", 
          info = "Показаны записи от _START_ до _END_ из _TOTAL_",
          lengthMenu = "Показать _MENU_ записей",
          infoFiltered = " — число записей после фильтрации (общее число записей без фильтрации равно _MAX_)",
          paginate = list("previous"= "Предыдущая", "next" = "Следующая"),
          zeroRecords = "Совпадений не найдено"),
        
        columnDefs = list(
          list(targets = "_all", className = "dt-center")
        ))), server = FALSE)
      
      
      
      output$table2Output <- renderUI({ 
        
        #          dataNew <- as.data.frame(dataT())
        if (!is.null(datasetInput())) {
          box(id = "table2Output", title = "Данные (после выбора столбцов)", status = "success", collapsible = TRUE, width = NULL, DTOutput(ns("table2")))
        }
      })
      
      output$downloadDataCsv <- downloadHandler(
        filename = function() {
          paste(input$dataset, ".csv", sep = "")
        },
        content = function(file) {
          #write.csv(datasetInput(), file, row.names = TRUE)
          write.csv(v$data, file, row.names = TRUE)
        }
      )
      
      output$downloadDataXlsx <- downloadHandler(
        filename = function() {
          paste(input$dataset, ".xlsx", sep = "")
        },
        content = function(file) {
          #write.xlsx(datasetInput(), file, row.names = TRUE)
          write.xlsx(v$data, file, row.names = TRUE)
        }
      )
      
      datasetInputNewD <- eventReactive(input$chgType,{
        
        datasetInputNewD <- datasetInputNew() 
        return(datasetInputNewD)
      })
      
      colN <- reactive({ names(datasetInput()) })
      
      output$stats <- renderPrint({
        #str(datasetInput())
        str(v$data)
      })
      
      output$statsView <- renderUI({ 
        
        #          dataNew <- as.data.frame(dataT())
        if (!is.null(datasetInput())) {
          fluidRow(column(12, strong("Структура данных:")), 
                   column(12, verbatimTextOutput(ns("stats"))),
          column(6, pickerInput(inputId = ns('typeVar'),
                                         selected = "",
                                         width = '100%',
                                         label = 'Выбор переменных:', 
                                         choices = colN(),
                                         options = list(`actions-box` = FALSE,
                                                        #`deselect-all-text` = "Отменить выбор",
                                                        `none-selected-text` = "Ничего не выбрано"),
                                         multiple = T)),

                   column(3, pickerInput(inputId = ns('chooseType'),
                                         selected = "",
                                         label = 'Выбор типа переменных:', 
                                         choices = list(Числовой = "Числовой", Категориальный = "Категориальный", Символьный = "Символьный", Дата = "Дата"),
                                         options = list(`actions-box` = FALSE,
                                                        `none-selected-text` = "Ничего не выбрано"),
                                         multiple = F
                   )),
                   column(3, 
                          #style="dpadding-top: 25px;",          
                          #style = "margin-bottom: 10px;",
                          style = "margin-top: 25px;",
                          style = "margin-bottom: 10px;", 
                          align = "center", actionButton(ns("chgType"), "Преобразовать")))  
        }
        })
      
      #observeEvent(input$chgType, datasetInput()[, input$typeVar])
      observeEvent(input$chgType, v$data[, input$typeVar])
      
      counter <- reactiveVal(0)
      counterT <- reactiveVal(0)
      
      observeEvent(input$viewNewData, {
        #counterT(counterT() + 1)
        #  if (counterT() > 2){      #проверить
        #counter(counter(1) == 1)
        counter(0)
        #counter()
        #return()
        #  }
      })
      
      observeEvent(input$chgType, counter(counter() + 1))
      #observeEvent(input$viewNewData, counterT(counterT() + 1))
      

      
#      observeEvent(input$chgType,{
#        if (counter() == 1){
#          datasetInput <- datasetInput()
          
#        }else {
#          datasetInput <- datasetInputNewD()
#        }
#      })
      
    #  data2 <- reactive({     
     #   if(counter() == 1){
      #    datasetInput()}else{
     #       datasetInputNewDD <- datasetInputNewD() 
            #return(datasetInputNewD)
            
        #    View(datasetInputNewDD())
            #return(datasetInputNewDD)
            #str(datasetInputNewD())
          #  }        })
      #!is.null(input$fileupaziendedata) && 
      
      
      
     ## eventReactive(input$viewNewData,{
      ##  counter() == 0
      ##  datasetInputNewD() <- datasetInput()
        #observeEvent(input$chgType, {
     ##     rv$datasetInputNewD <- datasetInput()
     ##     ignoreInit = FALSE
        #observeEvent(input$chgType,{
        #  r$datasetInputNewD <- datasetInputNewD()
        #View(r$datasetInputNewD)
        #return(r$datasetInputNewD)
        #})
      ##  return(rv)
        #counter <- reactiveVal(0)
        #datasetInputNew <- datasetInput()
        #rv$datasetInputNewD <- datasetInput()
      ##})
      
      
      datasetInputNew <- function(){
        
        if(counter() > 1){
          datasetInputNew <- rv$datasetInputNewD
        } else {
          datasetInputNew <- v$data #datasetInput()
        }
          
        columnName <- input$typeVar
        
        if( input$chooseType == "Числовой"){
          datasetInputNew[, columnName] <- lapply(datasetInputNew[, columnName], as.numeric)
        }
        else if(input$chooseType == "Категориальный"){
          datasetInputNew[, columnName] <- lapply(datasetInputNew[, columnName], as.factor)
        } else if( input$chooseType == "Символьный"){
          datasetInputNew[, columnName] <- lapply(datasetInputNew[, columnName], as.character)
        } else if( input$chooseType == "Дата"){
          datasetInputNew[, columnName] <- lapply(datasetInputNew[, columnName], as.Date)
        } else { datasetInputNew }
        
        
        datasetInputNew
        
      }

      
      output$statsNew <- renderPrint({
        str(datasetInputNewD())
      })
      
      output$statsViewNew <- renderUI({ 
        
        #          dataNew <- as.data.frame(dataT())
        if (!is.null(datasetInputNewD())) {
          fluidRow(column(12, strong("Структура данных после преобразования типов:")), 
                   column(12, verbatimTextOutput(ns("statsNew"))))
          }
      })
      

      observeEvent(input$chgType, {
        
        updatePickerInput(session, "typeVar",
                          selected = "") 
        
      })
      
      #list(
      #  chooseType = reactive({input$chooseType})
      #)
      
      #dataData <- reactive({datasetInputNewD()})
      
      rv <- reactiveValues()
      observeEvent(input$chgType, {
        rv$datasetInputNewD <- datasetInputNewD()}, 
        ignoreInit = FALSE)
      #observeEvent(input$chgType,{
      #  r$datasetInputNewD <- datasetInputNewD()
        #View(r$datasetInputNewD)
        #return(r$datasetInputNewD)
      #})
      return(rv)
     
      #return(reactive(r$datasetInputNewD))
      
      #reactive({ r$datasetInputNewD })
      
      #callModule(moduleServer_2, id = ns("module_2"), session = session, r = r)
      
      #moduleServer_2("module_2") 
      #callModule(moduleServer_2, id = "module_2", session = session)
      #vals <- reactiveValues()
      #observe({vals$statsView <- input$statsView})
      #return(vals)

                   
    })
  
}
