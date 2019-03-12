library(dplyr)
library(tidyr)
library(purrr)
library(dygraphs)
library(eflows)
library(eflows.viz)

input_vct <- list()
input_vct[[".demand"]] <- sept$d_household[1:100]*100

bsh <- eflows:::backshiftCpp(consumption = sept$d_household[1:100]*100, 
                 self_discharge = 0.01, 
                 eff = list(0.9, 0.9),
                 horizon = 12,
                 env_fit = list2env(input_vct),
                 call_fit = (~ 1*.demand)[[2]], 
                 env_aux = new.env(), 
                 call_aux = (~ 1*.demand)[[2]])

bsh$mtx_postbsh


bsh$mtx_prebsh %>% apply(2,sum)
bsh$mtx_postbsh %>% apply(2,sum)




# movs <- as.data.frame(bsh$mtx_moves)
# bsh$mtx_prebsh
# bsh$mtx_postbsh




# prev --------------------------------------------------------------------
piece <- diff(range(eflows::sept$d_household[1:100])) / 100

movs <- as.data.frame(bsh$mtx_moves)
  
thepot <- movs %>% 
  set_names(c("from", "to", "gain", "potential")) %>% 
  mutate(from = from + 1, 
         potential = as.numeric(cut(movs$V3, breaks = 5))) %>% 
  group_by(from, potential) %>% 
  summarise(sumpot = n() * piece)


gridgains <- expand.grid(seq(1:100),seq(1,5)) %>% 
  set_names(c("from", "potential")) %>% 
  left_join(thepot, by = c("from", "potential")) %>% 
  spread(potential, sumpot) %>% 
  mutate_all(function(x){x[is.na(x)] <- 0; x}) 


gridgains %>%
  select(from,`5`, `4`, `3`, `2`, `1`) %>%
  mutate(fixed = sept$d_household[1:100] - `1` - `2` - `3` - `4` - `5`) %>%
  dygraph() %>%
  dyOptions(stackedGraph = TRUE)


# bsh$final_consumption %>% plot(type = "l")





# from mtx_bsh ------------------------------------------------------------


bind_cols(x = seq(1,100),
          as_data_frame(eflows.viz:::mtx_reverse(bsh$mtx_prebsh)),
          y = eflows::sept$d_household[1:100]*100 - apply(bsh$mtx_prebsh,1,sum)) %>% 
  dygraph() %>% 
  dyHighlight() %>% 
  dyOptions(stackedGraph = TRUE)

bind_cols(x = seq(1,100),
          as_data_frame(eflows.viz:::mtx_reverse(bsh$mtx_postbsh)),
          final = bsh$final_consumption[,1] - apply(bsh$mtx_postbsh,1,sum)) %>% 
  dygraph() %>% 
  dyHighlight() %>% 
  dyOptions(stackedGraph = TRUE)

bind_cols(x = seq(1,100),
          init = eflows::sept$d_household[1:100]*100,
          final = bsh$final_consumption[,1]) %>% 
  dygraph() %>% 
  dyHighlight() 

sum(eflows::sept$d_household[1:100]*100) - sum(bsh$final_consumption[,1])







