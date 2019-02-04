library(dplyr)
library(tidyr)
library(purrr)
library(dygraphs)
library(eflows)

bsh <- eflows:::backshiftCpp(eflows::sept$d_household[1:100], 
                      0, 
                      list(0.9, 0.9), 
                      8)

movs <- as.data.frame(bsh$mtx_moves)
bsh$mtx_prebsh

#####

eflows:::fct_chop(seq(1,20), 4)

# questioning <- data.frame(vect = bsh$mtx_moves,
#                           index = seq(1,length(bsh$mtx_moves))) %>% 
#   arrange(vect)
#   

# questioning <- as.data.frame(bsh$mtx_moves) %>% 
#   mutate(index = seq(1,nrow(bsh$mtx_moves))) %>% 
#   arrange(V3)
# 
# questioning$index == (bsh$mtx_moves_idx + 1)
# 
# 
# what 





# data.frame(seq(1: length(bsh$veccon)),
#            bsh$veccon,
#            bsh$vecdep) %>% 
#   dygraph()


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
          y = bsh$final_consumption) %>% 
  dygraph() %>% 
  dyHighlight() %>% 
  dyOptions(stackedGraph = TRUE)





