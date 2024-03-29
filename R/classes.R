setClass(
  'nowcast',
  slots = c(
    target = 'character',
    name = 'character',
    methods = 'list',
    start_date = 'Date',
    nowcast_date = 'Date',
    horizon = 'integer',
    oos_test = 'logical',
    test_start_date = 'Date',
    predictors = 'list',
    freq = 'character',
    y = 'data.frame',
    X = 'data.frame',
    data = 'data.frame',
    dates = 'Date',
    target_deseason = 'character',
    pred = 'data.frame',
    fit = 'list'
  )
)


setMethod("initialize", "nowcast",
          function(.Object,
                   target,name,
                   start_date,
                   nowcast_date = as.character(Sys.Date()),
                   horizon,
                   oos_test,
                   test_start_date=NULL,
                   target_deseason,
                   predictors,
                   methods) {
            .Object@target  <-  target
            .Object@name <- name
            .Object@methods <- methods
            .Object@start_date <- as.Date(start_date)
            .Object@nowcast_date <- as.Date(nowcast_date)
            .Object@horizon <- as.integer(horizon)
            .Object@oos_test <-  oos_test
            .Object@test_start_date <- as.Date(test_start_date)
            .Object@predictors <- predictors
            .Object@target_deseason<-target_deseason
            .Object@dates <- lubridate::ymd()
            .Object@freq <- rmedb::variables[which(rmedb::variables$ticker == target),'freq']
            .Object@pred <- data.frame(method=character(), y_pred=numeric())

            for(i in 1:length(.Object@predictors)){


              .Object@predictors[[i]]$freq <- rmedb::variables[which(rmedb::variables$ticker ==
                                                                            .Object@predictors[[i]]$ticker), 'freq']
            }

            validObject(.Object)
            return(.Object)
            }
)


collect.data <- function(object, directory = NULL){
  UseMethod('collect.data')
}

plot.data <- function(object){
  UseMethod('plot.data')
}
fit <- function(object){
  UseMethod('fit')
}

#' Collect by ticker
#'
#' Returns transformed data for specified ticker
#'
#' @param ticker character ticker
#' @param directory character path to database directory with subfolder "tf" (usually \code{Sys.getenv('directory')})
#'
#' @return data.table
#' @keywords internal
#'
#' @examples
#' head(collect.data.by.ticker('gdp_real',
#'  Sys.getenv('directory')))
#' #           date gdp_real
#' # 1:  1995-01-01 10757.47
#' # 2:  1995-04-01 11072.24
#' # 3:  1995-07-01 11666.89
#' # 4:  1995-10-01 11845.80
#' # 5:  1996-01-01 10518.41

collect.data.by.ticker <- function(ticker, directory){
  file_name = paste0(directory, '/data/tf/', ticker,'.csv')
  data.table::fread(file_name,
                    select = c('date', 'value'),
                    col.names = c('date', ticker))
}
#' Filter by frequency
#'
#' Returns a date frame with a frequency that
#'  matches the target variable frequency (\code{object@freq})
#'
#' @param df data.frame without NA's
#' @param object nowcast
#' @param predictor_freq character from ('d','w', 'm', 'q')
#'
#' If the target variable frequency and predictor frequency matches, returns original df.
#' If the target variable is quarterly and predictor is monthly, returns every third row
#' of df, ending with last df's row.
#' If the target variable is monthly and predictor is quarterly, repeats each df's row
#' three times.
#' If the predictor is daily or weekly and
#'  the target variable is quarterly or monthly,
#' firstly for the df's last date defines day number in its quarter or months (\code{dn}),
#' then returns df, where each row corresponds to the \code{dn} day of quarter of month.
#'
#'
#' @return data.frame
#' @export
#' @keywords internal
filter.data.by.frequency <- function(df, object, predictor_freq){
  target_freq <- object@freq
  nowcast_date <- object@nowcast_date
  start_date <- object@start_date

  if(any(is.na(df))){
    message("df has NA's")
    break
  }
  if(target_freq==predictor_freq){

    df

  } else if(target_freq=="q" & predictor_freq == "m"){

    df[seq(from = nrow(df), to = 1, by = -3),]

  } else if(target_freq=="m" & predictor_freq == "q"){

    df <- df %>% tidyr::uncount(3) # Repeats every row 3 times
    df$date <- data.table::as.IDate(seq.Date(from = df$date[1], by = "1 month", length.out = nrow(df)))
    df

  } else if(target_freq%in%c("m", "q") & predictor_freq%in%c("w", "d")){

    timeunit_begin_sequence <- timeunit.begin.sequence(start_date, nowcast_date, by = target_freq) # Sequence of time unit (quarter or month) first dates

    timeunit_begin_sequence <- data.table::as.IDate(timeunit_begin_sequence)

    day_diff <- as.integer(df$date[nrow(df)] - timeunit_begin_sequence[length(timeunit_begin_sequence)])


    timeunit_begin_sequence <- timeunit_begin_sequence + day_diff # Corrects sequence to predictor variable publication lag

    timeunit_begin_df <- data.frame("date" = timeunit_begin_sequence)


    dplyr::left_join(timeunit_begin_df, df, by ="date") %>%
      dplyr::arrange(date) %>%
      zoo::na.locf() %>%
      dplyr::right_join(timeunit_begin_df, by = "date")
  }
}

#' Make monthly
#'
#' Transforms weekly or daily df to monthly df,
#'  returning only last observation for each month
#'
#' @param df data.frame
#'
#' @return data.frame
#' @export
#' @keywords internal
make.monthly <- function(df){
  df %>%
    dplyr::group_by(mon = zoo::as.yearmon(date)) %>%
    dplyr::filter(dplyr::row_number()==max(dplyr::row_number())) %>%
    dplyr::ungroup() %>%
    dplyr::select(-mon)
}

#' Sequence of quarter/ month start dates
#'
#' @param from Date
#' @param to Date
#' @param by "q" or "m"
#'
#' @return Date sequence
#' @export
#' @keywords internal
#'
#' @examples
#' timeunit.begin.sequence(as.Date('2020-01-19'), as.Date('2021-02-07'), by = "q")
#' # "2020-01-01" "2020-04-01" "2020-07-01" "2020-10-01" "2021-01-01"
timeunit.begin.sequence = function(from, to, by = c("q", "m")){
  as.unit <- switch(by,
                    'q' = zoo::as.yearqtr,
                    'm' = zoo::as.yearmon)
  seq.Date(from = zoo::as.Date(as.unit(from)), to =to, by = by)
}

#' Create lag
#'
#' Returns data.frame with lags of second column
#'
#' @param df data.frame
#' @param lags integer vector
#'
#' @return data.frame
#' @export
#' @keywords internal
#'
#' @examples
#' create.lag(data.frame("index"=c(1:5), x = c(1:5)), lags = c(1,2))
#' # index x x__lag1 x__lag2
#' #     3 3       2       1
#' #     4 4       3       2
#' #     5 5       4       3
#'
create.lag <- function(df, lags){
  if(length(lags)==1&lags[1]==0){
    df
  } else{
    ticker <- colnames(df)[2]
    for(l in lags){
      if(l!=0){
        df[,paste0(ticker,"__lag", l)] <- xts::lag.xts(df[,ticker], l)
      }

    }
    if(!(0 %in% lags)){
      df[,ticker] <- NULL
    }
    na.omit(df)
  }
}
#' Cut df
#'
#' Removes first \code{nrow(df)}-\code{len} rows from df
#'
#' @param df data.frame
#' @param len integer
#'
#' @return data.frame
#' @export
#' @keywords internal
#'
#' @examples
#' cut.df.from.start(data.frame(x=c(1:10)), len = 5)
#' #   x y
#' # 4 4 4
#' # 5 5 5
cut.df.from.start <- function(df,len){

  df[(nrow(df)-len+1): nrow(df),]
}

#' Remove columns
#'
#' Removes specified \code{column} from df or breaks with message if \code{column} not in df's columns names
#'
#' @param df data.frame
#' @param column character column name
#'
#' @return data.frame
#' @export
#' @keywords internal
#'
#' @examples
#' remove.column(data.frame(x=c(1:3), y = c(1:3)), column = "y")
#' #   x
#' # 1 1
#' # 2 2
#' # 3 3
#' remove.column(data.frame(x=c(1:3), y = c(1:3)), column = "value")
#' #   Can't remove column
#' # value
#' # Error in remove.column(data.frame(x = c(1:3), y = c(1:3)), column = "value") :
#' # no loop for break/next, jumping to top level
remove.column <- function(df,column){
  if(column%in% colnames(df)){
    df[,which(colnames(df)==column)] <- NULL
    df
  } else{
    message('Can\'t remove column')
    message(column)
    break
  }

}

get.dates <- function(df, freq){
  as.unit <- switch(freq,
                    'q' = zoo::as.yearqtr,
                    'm' = zoo::as.yearmon)

  dates <- zoo::as.Date(as.unit(df$date))
  prediction_date <- seq(dates[length(dates)], length.out =2, by = freq)[2]
  c(dates, prediction_date)
}

deseason <- function(df, deseason, freq){ # target freq
  freq_n <- switch(freq,
                   "q" = 4L,
                   "m" = 12L)
  deseason_function <- switch(deseason,
                              "level" = function(x){x},
                              "diff" = function(x){xts::diff.xts(x, lag = freq_n)},
                              "logdiff" = function(x){xts::diff.xts(x, lag = freq_n, log = TRUE)}
  )

  for(i in 2:ncol(df)){
    df[,i] <- deseason_function(df[,i])
  }
  na.omit(df)


}

timeslices.by.testlen <- function(y, initialWindow = NULL, testlen = NULL, ...){
  if(is.null(initialWindow)){
    initialWindow <- length(y) - testlen
  }
  caret::createTimeSlices(y, initialWindow, ...)
}
call.train <- function(object, X_train, X_test, y_train, y_test=NA, save_fit = FALSE, date){


  for(i in 1:length(object@methods)){



    tune_grid <- expand.grid(object@methods[[i]]$tungerid)

    trControl <- do.call(caret::trainControl, object@methods[[i]]$trControl)


    arglist <- object@methods[[i]]$train
    arglist[c("x", "y", "tuneGrid", "trControl")] <- list(X_train,
                                                          y_train,
                                                          tune_grid,
                                                          trControl)

    model <- do.call(caret::train, arglist)

    pred <- predict(model,
                    newdata = X_test) %>%
      as.numeric()


    if(save_fit){
      object@fit[[(length(object@fit)+1)]] <- list("name" = object@methods[[i]]$name,
                                                   "model" = model)
    }



    object@pred <- rbind(object@pred,data.frame(method = object@methods[[i]]$name,
                                                horizon = object@horizon,
                                                target =object@target,
                                                name =object@name,
                                                date = date,
                                                y_pred = pred,
                                                y_test = y_test))

  }
  object
}


#' Collect data
#'
#' @param nowcast
#'
#' @return nowcast
#' @export
setMethod("collect.data", "nowcast",
          function(object, directory = NULL) {
            if(is.null(directory)){
              directory <- Sys.getenv('directory')
            }


            object@y <-  collect.data.by.ticker(object@target, directory)


            if(object@freq %in% c('w', 'd')){
              object@y <- make.monthly(object@y)
              object@freq <- "m"
            }

            object@y <- deseason(object@y,object@target_deseason, object@freq)

            x_list <- purrr::map(1:length(object@predictors),function(i){

              ticker <-object@predictors[[i]]$ticker

              collect.data.by.ticker(ticker, directory) %>%
                create.lag(object@predictors[[i]]$lag) %>%
                filter.data.by.frequency(object, object@predictors[[i]]$freq) %>%
                deseason(object@predictors[[i]]$deseason,
                         object@freq)
            })


            min_nrow <- min(nrow(object@y)+object@horizon, sapply(x_list, nrow))

            object@y <- cut.df.from.start(object@y, min_nrow-object@horizon)

            object@data <- purrr::reduce( x_list,
                                      dplyr::full_join,
                                      by = "date") %>%
              dplyr::arrange(date)

            object@X <- lapply(x_list, cut.df.from.start, min_nrow) %>%
              lapply(remove.column, "date") %>%
              dplyr::bind_cols()



            object@dates <- get.dates(object@y, object@freq)

            object@y$date <- NULL

            validObject(object)
            return(object)
          }
          )


setMethod("plot.data", "nowcast",
          function(object){

            scale_column <- function(x){
              (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
            }
            object@data$any_na <- as.character(rowSums(is.na(object@data))>0)
            print(object@data)
            object@data %>%
              data.table::melt(id.vars = c("date", "any_na")) %>%
              dplyr::group_by(variable) %>%
              dplyr::mutate(value = scale_column(value)) %>%
              print %>%
              ggplot(aes(x = date,
                         y = value,
                         color= variable,
                         alpha =any_na
                         ))+
              geom_point()+
              scale_alpha_manual(values = c(1, 0.2), labels = c('FALSE', 'TRUE'))
          })
#' Fit
#'
#' @param nowcast
#'
#' @return nowcast
#' @export
setMethod("fit", "nowcast",
          function(object){


            X <- model.matrix(~0+., object@X, drop=FALSE)
            len <- nrow(X)
            X_train <- X[1:(len-object@horizon),,drop=FALSE]
            X_test <- X[len,,drop=FALSE]
            y_train <- as.numeric(object@y[[1]])


            test_date <- object@dates[length(object@dates)]

            if(object@oos_test){
              testlen <- sum(object@dates>=object@test_start_date)-object@horizon


              timeslice <- timeslices.by.testlen(y_train, testlen = testlen)

              for(i in 1:testlen){

                traini <- timeslice$train[[i]]
                testi <- timeslice$test[[i]]

                X_traini <- X_train[traini,,drop=FALSE]
                X_testi <- X_train[testi,,drop=FALSE]
                y_traini <- y_train[traini]
                y_testi <- y_train[testi]
                datei = object@dates[testi]

                object <- call.train(object, X_traini, X_testi, y_traini, y_testi, date = datei)
              }

            }



            object <- call.train(object, X_train, X_test, y_train, save_fit = TRUE, date = test_date)


            validObject(object)
            return(object)

          })


#' Run from JSON
#'
#' @param input path to .json configuration file
#' @param output path to .Rda/.Rdata
#'
#' @return list
#' @export
run.from.json <- function(input, output=NULL){
  input_list <- jsonlite::fromJSON(readLines(input),
                                  simplifyDataFrame = FALSE,
                                  simplifyVector = TRUE)

  result <- input_list %>% purrr::map(function(item){
    do.call(new, c("Class"="nowcast",
                   item)) %>%
      collect.data() %>%
      fit()
  })
  if(!is.null(output)){
    save(result, file = output)
  }
  result

}

#' Export result to csv
#'
#' @param input path to .Rda/.Rdata with nowcasting resuls
#' @param output path to .csv
#'
#' @export
export.nowcast.to.csv <- function(input,output){
  load(input,envir = environment())
  out <- purrr::map_dfr(result, function(x){x@pred})
  data.table::fwrite(out, file = output)
}

