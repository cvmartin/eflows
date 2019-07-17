library(eflows)
library(eflows.viz)
library(dplyr)
library(dygraphs)
library(purrr)
library(xts)

# calculations (eflows) ---------------------------------------------------


test_object <- e_frame$new(sept$datetime[1:168])$
  set_demand(e_demand$new(fixed = sept$d_house_smooth[1:168]*300))$
  set_production(e_production$new(fixed = list(solar = sept$solar[1:168]*120)))$
  set_price(sept$eprice[1:168]*0.6, unit = "euro/mWh")$
  set_storage(e_storage$new(input = list(
    storage$new(vol = 23, 
                eff = list(0.95,0.95), 
                self_discharge = 0.01,
                name = "battery"),
    storage$new(vol = 13, 
                eff = list(0.9,0.9), 
                self_discharge = 0.2,
                name = "buffer"))))

init_input_vct <- list(.demand_fixed = test_object$demand$input$fixed %||% NULL, 
                       .production_fixed = test_object$production$sum_fixed %||% NULL, 
                       .price = test_object$utility$input$price %||% NULL, 
                       .cap = test_object$infrastructure$input$grid$capacity %||% NULL)

clean_input_vct <-  Filter(Negate((is.null)), init_input_vct)
total_input_vct <- c(clean_input_vct, NULL)

thehorizon = 12
thefit = ~.demand - .production_fixed

bshifted <- backshift(
  input_consumption = test_object$demand$input$fixed,
  horizon = thehorizon, 
  params_df = test_object$storage$params_df %||% as.data.frame(list()),
  input_vct = total_input_vct,
  fit = thefit
)

####



test_object$do_backshift(horizon = thehorizon, fit = thefit)

pre <- viz_back_potential(test_object)
post <- viz_back_output(test_object)

solar <- data.frame(datetime = sept$datetime[1:168], 
                    solar =test_object$production$sum_fixed) %>% 
  df_to_ts() %>% 
  dygraph(height = 200)

battery <- data.frame(datetime = sept$datetime[1:168], vol = bshifted$v_soc) %>% 
  df_to_ts() %>% 
  dygraph(height = 200) %>% 
  dyOptions(fillGraph = TRUE)

battery2 <- data.frame(datetime = sept$datetime[1:168], vol = cumsum(bshifted$v_soc)) %>% 
  df_to_ts() %>% 
  dygraph(height = 200) %>% 
  dyOptions(fillGraph = TRUE)


htmltools::browsable(htmltools::tagList(list(solar, pre, post, battery, battery2)))

viz_storage_soc(test_object)
