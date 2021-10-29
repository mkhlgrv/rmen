# library(rmedb)
# x = rmedb::get.variables.df() %>%
#   dplyr::filter(observation_start < "2007-01-01",
#                 freq!="d",
#                 !grepl("_15",ticker)
#   ) %>% .[,c("ticker", "deseason")] %>%
#   dplyr::mutate(lag=list(0))
# x$lag[which(x$ticker=="cpi")] <- list(list(0,1,2,3))
# json_list <- jsonlite::fromJSON("info/cpi_all_predictors.json",
#                                 simplifyDataFrame = FALSE,
#                                 simplifyVector = TRUE)
# json_list[[1]]$predictors <- x
# x_json <- json_list %>% jsonlite::toJSON(pretty=TRUE)
# write(x_json, "info/cpi_all_predictors.json")
#
#
# template <- jsonlite::fromJSON('info/cpi_all_predictors.json',
#                    simplifyDataFrame = FALSE,
#                    simplifyVector = TRUE)
# vars_to_nowcast <- rio::import("examples/vars_to_nowcast.csv")
# result_list <- lapply(vars_to_nowcast$ticker,
#        function(ticker){
#          result <- template[[1]]
#          location <- which(
#            sapply(
#              result$predictors,
#              function(x){x$ticker==ticker}))
#
#          result$target_deseason <- ticker
#          result$name <- ticker
#          result$predictors[[location]]$lag <- c(0,1,2,3)
#          result$target_deseason <-
#            result$predictors[[location]]$deseason
#          result
#        })
#
# result_json <- jsonlite::toJSON(result_list,,pretty = TRUE)
# write(result_json,file = "examples/full_model.json")
#
#
# json_list <- jsonlite::fromJSON('info/cpi_all_predictors.json',
#                                 simplifyDataFrame = FALSE,
#                                 simplifyVector = TRUE)
# result <- do.call(new, c("Class"="nowcast",
#                          json_list[[1]])) %>%
#   collect.data() %>%
#   fit()
# plot(result@fit[[2]]$model)
# res=resamples(list('RF' = result@fit[[1]]$model,
#                    'xgbTree' = result@fit[[2]]$model,
#                    'EN' = result@fit[[3]]$model))
# #
# # bwplot(res, layout = c(1, 3),scales = list(relation = "free"))
# # summary(res)
# # result@fit[[1]]$model$finalModel$importance*100
# # coef(result@fit[[3]]$model$finalModel, result@fit[[3]]$model$finalModel$lambdaOpt/2)
# # # new_pred= result@pred$y_pred
# #
# # pred1=predict(result@fit[[1]]$model,result@X)
# # pred2=predict(result@fit[[2]]$model,result@X)
# # pred3=predict(result@fit[[3]]$model,result@X)
# # y=c(result@y$cpi %>% as.numeric(), NA)
# # x=result@dates[]
# #
# # library(ggplot2)
# # ggplot(mapping = aes(x))+
# #   geom_line(aes(y = y), size=4,alpha=0.3)+
# #   geom_line(aes(y=pred1), color="darkgreen", size =0.5)+
# #   geom_line(aes(y=pred2), color="blue", size =0.5)+
# #   geom_line(aes(y=pred3), color="blue", linetype=2,size =0.5)+
# #   theme_minimal()
# #
# # result@pred %>%
# #   ggplot(aes(date))+
# #   # geom_point(aes(y=y_pred, color = "prediction"))+
# #   # geom_line(aes(y=y_test))+
# #   # geom_point(aes(y=y_test))+
# #   geom_ma(aes(y=y_test), ma_fun = SMA, n=12)+
# #   geom_ma(aes(y=y_pred,color = "prediction"), ma_fun = SMA, n=12)
# #
# # result@pred %>%
# #   ggplot(aes(date))+
# #   # geom_point(aes(y=y_pred, color = "prediction"))+
# #   # geom_line(aes(y=y_test))+
# #   # geom_point(aes(y=y_test))+
# #   # geom_ma(aes(y=(y_test-y_pred)^2), ma_fun = SMA, n=12)#+
# #   # geom_ma(aes(y=y_pred,color = "prediction"), ma_fun = SMA, n=12)
# #
# # # save(result, file="info/example.Rda")
