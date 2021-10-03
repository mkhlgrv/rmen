
shinyServer(function(input, output) {

    reactive_input <- reactiveValues(
    )

    freqs <- reactive({
        if(is.null(input$target)){
            NULL
        } else{
            c(
                                   "Квартал"="q",
                                   "Месяц"="m")

        }
    })


    observeEvent(input$reset, {
        showModal(modalDialog(
            tags$h2('Очистить выбор'),
            footer=tagList(
                actionButton('submit_reset', 'Подтвердить'),
                modalButton('Отменить')
            )
        ))
    })



    observeEvent(input$add, {
        # display a modal dialog with a header, textinput and action buttons
        showModal(modalDialog(
            tags$h2('Добавить спецификацию'),
            fluidRow(
            column(width = 4,
            tags$p(
                textInput("name", "Название", ""),
                tags$b("Целевая переменная"),
                checkboxInput("only_main", "Показывать избранные переменные", TRUE),
                uiOutput("variables_input"),
                dateInput('start_date', 'Не использовать наблюдения раньше этой даты',
                          value = as.Date("2007-01-01"),
                          min = as.Date("2000-01-01"), max = as.Date("2016-01-01"),
                          format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                          language = "ru",
                          width = NULL),
                numericInput('horizon', 'Горизонт прогнозирования', min = 0, max =12, value=0, step = 1),
                selectizeInput('target_deseason',"Преобразование целевой переменной",
                               choices = c("В уровнях"="level",
                                           "Разность логарифмов к аналогичному периоду прошлого году" = "logdiff",
                                           "Разность к аналогичному периоду прошлого году" = "diff"),
                               selected='logdiff')

            )
            ),
            column(width = 8,
                   tags$p(
                    uiOutput("argument_in_modal")
                     )

            )),

            footer=tagList(
                actionButton('submit_add_more', 'Добавить ещё'),
                actionButton('submit_add', 'Добавить и завершить'),
                modalButton('Отменить')
            )
        ,size="l"))
    })


    textarea.with.dynamic.rows <- function(value, id){
        nrow <- min(max(stringr::str_count(value, "\n"),3),25)
        HTML(paste0(
            "<textarea cols = '80' rows = '",
            nrow,
            "' id = '",
            id,
            "' spellcheck='false' style =  'resize: none;'>",
            value,
            "</textarea>"
        )
        )
    }
    output$args_html <- renderUI({
            if(is.null(reactive_input$names)) {
                value <- ""
            } else {
                value <- jsonlite::toJSON(reactive_input$items,pretty=TRUE, auto_unbox=TRUE)
                }
        textarea.with.dynamic.rows(value, id = "codearea")

        })

    output$argument_in_modal <- renderUI({

        value <- list(list("target" = input$target,
            "name" = input$name,
            "start_date"=as.character(input$start_date),
            "horizon"=as.integer(input$horizon),
            "target_deseason" = input$target_deseason,
            "predictors" =  list(list(ticker = 'cons_real',lag = 1, deseason = "logdiff"),
                              list(ticker = "SP500", lag=0, deseason = "logdiff")),
            "methods" = list(list("name" = "xgbTree", tuneLength=20,
                                  method = "xgbTree",
                                  metric = "RMSE",
                                  preprocessing='pca',
                                  trControl= list(method = "repeatedcv",
                                                  number = 10,
                                                  repeats = 10,
                                                  search = "random"),
                                  tungerid=list(nrounds = 100,
                                                max_depth = c(5),
                                                eta = 0.1,
                                                gamma = 0,
                                                colsample_bytree = 0.3,
                                                min_child_weight = 1,
                                                subsample = 1)))
             ))
        value_json <- jsonlite::toJSON(value,
                                       pretty=TRUE,
                                       auto_unbox=TRUE)


        textarea.with.dynamic.rows(value_json, id = "codearea_in_modal")
    })
    observeEvent(input$json_file,{
       json_from_file  <- jsonlite::fromJSON(input$json_file$datapath,
                                                            simplifyDataFrame = FALSE,
                                        simplifyVector = TRUE)
       append.args(json_from_file)
    })


    check.args <- function(args){
        res <- purrr::map_lgl(args, function(json_item){
            tryCatch({
                R.utils::withTimeout({do.call(new, c("Class"="nowcast",
                                                     "nowcast_date" = as.character(Sys.Date()),
                                                     json_item)) %>%
                        collect.data() %>%
                        fit()},
                        timeout = 0.00001,
                        onTimeout = "error")


            }, error = function(e) any(class(e)=="TimeoutException"), finally = NULL)
        })

        return(all(res))
    }


    observeEvent(input$remove, {
        showModal(modalDialog(
            tags$h2('Удалить спецификацию'),

            checkboxGroupInput(
                "names_to_remove",
                "",
                choices = {
                    if(length(reactive_input$names)==0){
                        NULL
                    } else{
                        reactive_input$names
                    }
                },
            )
            ,
            footer=tagList(
                actionButton('submit_remove', 'Подтвердить'),
                modalButton('Отменить')
            )
        ))
    })
    observeEvent(input$submit_remove, {
        removeModal()
        if(length(input$names_to_remove)>0){
            n_to_remove <- as.integer(input$names_to_remove)
            reactive_input$names <- reactive_input$names[-n_to_remove]
            reactive_input$items <- reactive_input$items[-n_to_remove]
        }

    })

    observeEvent(input$submit_reset, {
        removeModal()
        reactive_input$names <- NULL
        reactive_input$items <- NULL
    })

    append.args <- function(new_args){
        len_old <- length(reactive_input$items)
        len_new <- length(new_args)
        index_to_append <- (len_old+1):(len_old+len_new)

        reactive_input$names[index_to_append] <- sapply(new_args, function(x){x$name})
        reactive_input$items[index_to_append] <- new_args
    }

    submit.add <- function(remove_modal){
        if(input$name %in% reactive_input$names){
            showNotification("Вы уже добавили спецификацию с таким названием")
        } else{
            args <- jsonlite::fromJSON(input$codearea_in_modal,
                                       simplifyDataFrame = FALSE,
                                       simplifyVector = TRUE)
            if(check.args(args)){
                append.args(args)
                if(remove_modal){
                    removeModal()
                }

            } else {
                showNotification("Введенный вами текст не может быть скомпилирован.
                                 Проверьте правильность нотации.")
            }

        }
    }
    observeEvent(input$submit_add,submit.add(TRUE))

    observeEvent(input$submit_add_more,submit.add(FALSE))


    observeEvent(input$submit_calc, {
        if(!is.null(reactive_input$names)){
            reactive_input$result <-
                purrr::map_dfr(reactive_input$items,
                               fit.from.arglist)
        }

    })

    output$pred <- renderDataTable({
        if(is.null(reactive_input$result)) return(NULL)
        reactive_input$result
    })

    output$downloadData <- downloadHandler(
        filename = function() {
            paste0('prediction-', Sys.Date(), '.csv')
        },
        content = function(con) {

            if(is.null(reactive_input$result)) return(NULL)
            data.table::fwrite(reactive_input$result,
                               con)
        }
    )

    output$downloadJSON <- downloadHandler(
        filename = function() {
            paste0('arguments-', Sys.Date(), '.json')
        },
        content = function(con) {

            if(is.null(reactive_input$items)) return(NULL)

            write(jsonlite::toJSON(reactive_input$items,pretty=TRUE, auto_unbox=TRUE), con)
        }
    )





    output$table <- renderDataTable({
        macroparsing::variables[,c("name_rus_short","observation_start", "freq", "source")] %>%
            inner_join(macroparsing::sources[,c("source", "name_rus")], by = "source") %>%
            select(-source) %>%
            dplyr::rename(Переменная="name_rus_short",
                                    "Начало наблюдений" = "observation_start",
                                    "Периодичность" = 'freq',
                                    "Источник" = "name_rus")
    }

    )




    output$variables_input <- renderUI( {

        selectizeInput(
                "target",
                label = NULL,
            choices ={
                if(input$only_main){
                    x <- macroparsing::variables$ticker
                    names(x) <- macroparsing::variables$name_rus_short


                    x[which(x %in% c("cons_real", "gdp_real", "cpi"))]

                } else{
                    split({
                        x <- macroparsing::variables$ticker
                        names(x) <- macroparsing::variables$name_rus_short
                        x

                    },
                    macroparsing::variables$source
                    )
                }

            }
            ,
            selected = 'usd',
            multiple = FALSE
        )
    })


    fit.from.arglist <- function(args){

        result <- do.call(new, c("Class"="nowcast",
                                 "nowcast_date" = as.character(Sys.Date()),
                                 args)) %>%
            collect.data() %>%
            fit()


        result@pred

    }

})





