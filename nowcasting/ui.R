library(shiny)
library(rmedb)
library(ggplot2)
library(dplyr)


shinyUI(
    navbarPage("Прогнозы для экономики России с помощью методов машинного обучения",
               tabPanel("Прогноз",
                        fluidRow(
                            column(2,
                                   tags$div(
                                   actionButton("add", "Добавить", icon = icon("plus"))
                                   ),

                                   tags$div(
                                     actionButton("remove", "Удалить", icon = icon("minus"))
                                   )
                                   ,
                                   tags$div(
                                     actionButton("reset", "Очистить", icon = icon("trash"))
                                   )
                                   ,
                                   hr(),
                                   fileInput('json_file', 'Загрузить JSON-файл с описанием вычислений',
                                             accept = ".json"),
                                   checkboxInput('show_code', 'Показать JSON',value = TRUE),
                                   hr(),
                                   actionButton('submit_calc', 'Вычислить прогнозы', icon = icon("cogs")),
                                   downloadButton('downloadData', 'Скачать результаты',icon = icon("download") )

                            ),

                            mainPanel(column(4,conditionalPanel(
                                condition = "input.show_code == true",
                                tags$div(
                                                HTML(paste0("<p>",
                                                  "Спецификации для прогнозов задаются в формате JSON.
                                       Вы можете изменить JSON-файл внутри приложения или
                                       в ","<a href = 'https://jsoneditoronline.org' target = '_blank'>",
                                                  "удобном онлайн-редакторе</a>",
                                         ", а также можете скачать созданную спецификацию,
                                                чтобы использовать ее позднее.",
                                         "</p>")

                                )
                                ),
                                downloadButton('downloadJSON', 'Скачать JSON'),
                                htmlOutput("args_html")
                            )
                            ),
                            column(6,
                                      dataTableOutput("pred")
                            )
                            )

                        )
               ),
               tabPanel("Описание переменных",
                        fluidRow(mainPanel((DT::dataTableOutput("table"))))
               )
    )
)
