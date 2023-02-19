source('str.R')
source('chart.Correlation.R')

#library(devtools)
#source_url('https://gist.github.com/fawda123/7471137/raw/cd6e6a0b0bdb4e065c597e52165e5ac887f5fe95/nnet_plot_update.r')
#source('test.R')

# список необходимых пакетов
# , "PerformanceAnalytics" delete iz kataloga shinyThings!!!!
list.of.packages <-
  c("neuralnet",
    "NeuralNetTools",
    "nnet",
    "caret",
    "shiny",
    "shinydashboard",
    "shinydashboardPlus",
    "shinycssloaders",
    "shinyalert",
    "js",
    "readxl",
    "DT",
    "shinyWidgets",
    "dplyr",
    "ggplot2",
    "funModeling",
    "ggfortify",
    "tidyverse",
    "tools",
    "xlsx",
    "PerformanceAnalytics",
    "rstatix",
    "nortest",
    "htmltools",
    "UBL",
    "h2o",
    "xtable",
    "yardstick",
    "waiter",
    "devtools",
    "mltools",
    "data.table",
    "pROC",
    #"DescTools", #"BrierScore"
    "rpart",
    #"rpart.plot"
    "rattle",
    "measures"
  )
#devtools::install_github("gadenbuie/shinyThings")

# проверка на отсутствие пакетов из списка
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

# установить недостающие
if (length(new.packages))
  install.packages(new.packages, dependencies = TRUE)

# запуск пакетов
for (package in list.of.packages) {
  do.call("library", list(package = package))
}



source('modulDataInteractionTools.R') # средства взаимодействия с данными (_1)
source('modulPrimaryDataProcessing.R') # первичная обработка данных (_2)
source('modulDataClassification_1.R') # классификация данных (_3)
source('modulDataClassification_2.R') # классификация данных (_3)
source('modulAutomatedSelectionOfAlgorithmParameters.R') # автоматический подбор параметров алгоритмов (_4)
#source('modulIntelligent.R') # «интеллектуальный»  (_5)

ui <- dashboardPage(
  skin = "green",
  options = list(sidebarExpandOnHover = TRUE),
  header = dashboardHeader(
    title = "ML : Анализ и классификация",
    titleWidth = 335,
    controlbarIcon = shiny::icon("", verify_fa = FALSE)
  ),
  sidebar = dashboardSidebar(
    width = 335,
    sidebarMenu(
      #sidebarSearchForm(textId = "searchText", buttonId = "searchButton",label = "Поиск..."),
      menuItem(
        "Средства взаимодействия с данными",
        tabName = "data",
        icon = icon("database")
      ),
      menuItem(
        "Первичная обработка данных",
        tabName = "processing",
        icon = icon("chart-bar")
      ),
      #chart-area
      menuItem(
        "Классификация данных",
        tabName = "classification",
        icon = icon("diagram-project"),
        startExpanded = TRUE,
        #server
        menuSubItem(
          "Подготовка выборок",
          tabName = "classification_1",
          icon = icon("stream", verify_fa = FALSE)
        ),
        #database
        menuSubItem(
          "Выбор и обучение классификаторов",
          tabName = "classification_2",
          icon = icon("brain")
        ),
        #brain-circuit
        #menuSubItem(
        #  "Визуализация",
        #  tabName = "classification_3",
        #  icon = icon("sitemap")
        #),
        #stream
        menuSubItem(
          "Оценка точности моделей",
          tabName = "classification_4",
          icon = icon("cubes")
        )
      ),
      menuItem(
        "Автоматический подбор параметров",
        tabName = "automated",
        icon = icon("brain")
      ),
      #sitemap
      menuItem(
        "Интеллектуальный модуль",
        tabName = "intelligent",
        icon = icon("question")
      )
    )
  ),
  body = dashboardBody(
    tags$link(rel = "stylesheet", type = "text/css", href = "customLogo.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "customSidebar-mini.css"),
    tags$script(src = "sidebarChildren.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "customSidebar.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "customTabBox.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "customFileInput.css"),
    #######tags$style(HTML(".selected {background-color:#00a65a !important;}")),
    tags$style(
      HTML(
        ".dropdown-menu > li .selected {background-color:#00a65a !important; color: black;}"
      )
    ),
    #стиль для pickerInput
    #tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/select2/4.0.13/css/select2.min.css"),
    #tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/select2/4.0.13/js/select2.min.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "select2.min.css"),
    tags$script(src = "select2.min.js"),
    
    #tags$style(HTML(".alert .confirm {background-color: #00a65a !important;}")), #цвет кнопки в уведомлении
    tabItems(
      tabItem("data", moduleUI_1("module_1")),
      tabItem("processing", moduleUI_2("module_2")),
      tabItem("classification_1", moduleUI_3.1("module_3.1")),
      tabItem("classification_2", moduleUI_3.2("module_3.2")),
      tabItem("automated", moduleUI_4("module_4"))
    )
    #moduleUI_1("module_1"),
    #moduleUI_2("module_2")
    
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize=300*1024^2)
  proxy <- moduleServer_1("module_1")
  moduleServer_2("module_2", proxy)
  proxyBalance <- moduleServer_3.1("module_3.1", proxy)
  moduleServer_3.2("module_3.2", proxyBalance)
  moduleServer_4("module_4", proxyBalance)
  
  
}
shinyApp(ui, server)
