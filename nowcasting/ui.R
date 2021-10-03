library(shiny)
library(macroparsing)
library(ggplot2)
library(dplyr)


shinyUI(
    navbarPage("Прогнозы для экономики России с помощью методов машинного обучения",
               tabPanel("Прогноз",
                        fluidRow(
                            column(1,
                                   actionButton("add", "Добавить", icon = icon("plus")),
                                   actionButton("remove", "Удалить", icon = icon("minus")),
                                   actionButton("reset", "Очистить", icon = icon("trash")),
                                   hr(),
                                   fileInput('json_file', 'Загрузить JSON-файл с описанием вычислений',
                                             accept = ".json"),
                                   actionButton('submit_calc', 'Вычислить'),
                                   downloadButton('downloadData', 'Загрузить' ),
                                   checkboxInput('show_code', 'Показать код',value = TRUE)
                            ),

                            mainPanel(conditionalPanel(
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
                                downloadButton('downloadJSON', 'Скачать'),
                                htmlOutput("args_html")
                            ),
                                      dataTableOutput("pred")
                            )

                        )
               ),
               tabPanel("Описание переменных",
                        fluidRow(mainPanel((dataTableOutput("table"))))
               )
    )
)
