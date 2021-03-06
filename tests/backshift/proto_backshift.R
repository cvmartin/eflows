vec_depreciated <- function(length, deprec = 0.05) {
  sapply(seq(1,length),(function(x) {(1-deprec)^x}))
}

df_to_ts <- function(df) {
  require(xts)
  new_ts <- xts(x = df[, 2:(length(colnames(df)))], order.by = df[[1]])
  if (is.null(colnames(new_ts))) colnames(new_ts) <- colnames(df[2])
  return(new_ts)
}

ts_to_df <- function(tseries){
  as_tibble(data.frame(datetime=index(tseries), coredata(tseries)))
}

appreciate <- function(vector, eff = list(timestep, to_battery, from_battery), 
                       backwards = FALSE, depreciate) {
  
  res <- numeric(length(vector))
  
  if (backwards == TRUE) vector <- rev(vector)
  
  diff <- vector[1] - vector[1]*eff[[2]]*eff[[3]]
  
  for (i in 2:length(res)) {
    if (depreciate == TRUE) {
      res[i] <- (vector[i] - diff)* eff[[1]]^(i-1)
    } else {
      res[i] <- (vector[i] + diff)/ eff[[1]]^(i-1)
    }
  }
  res[1] <- vector[1]
  
  if (backwards == TRUE) res <- rev(res)
  
  res
}

which_if <- function(vector, func, condition){
  sel <- vector[which(condition)]
  if (length(sel) == 0) return(NA)
  match(func(sel), vector)
}

## The main one

backshift <- function(consumption, 
                       fit, 
                       range, 
                       eff = list(self_discharge, to_battery, from_battery), 
                       cap = list(consumption, to_storage), 
                       vol, 
                       vol_init = 0
) {
  
  piece_def <- signif(median(consumption)/20,2)
  
  comp_init <- as.matrix(consumption)[,1]
  comp <- as.matrix(consumption)[,1]
  batt <- numeric(length(comp))
  
  # Account for the initial state of the battery (batt_init)
  batt[1] <- vol_left <- vol_init
  
  for (i in 1:(length(comp))) {
    # Substract the consumption from the initial volume, from the start
    subst <- ifelse((comp[i] < vol_left), comp[i], vol_left)
    comp[i] <- comp[i] - subst
    vol_left <- vol_left - subst
    # Update vol_left and the battery 
    batt[i] <- vol_left <- vol_left*eff[[1]]
    # Exit once there is no more battery left
    if (vol_left <= 0) break
  }
  
  # Helper functions
  batt_update <- function(){
    for (n in j:k) {
      batt_diff <- comp[n] - comp_init[n]
      # if (batt_diff > 0) batt_diff <- batt_diff*eff[[2]]
      # if (batt_diff < 0) batt_diff <- batt_diff/eff[[3]]
      
      batt[n] <<- batt[n-1] + batt_diff #(comp[n] - comp_init[n])/eff[[2]]/eff[[3]]
      batt[n] <<- batt[n]*eff[[1]]
      if (near(batt[n], 0, tol = (piece_def/100))) batt[n] <<- 0
    }
  }
  
  batt_revert <- function(){
    comp[k] <<- origpos
    comp[j+pos-1] <<- endpos
    batt_update()
  }
  
  eff_piecesize <- function(x){
    (x/(eff[[1]]^(k-(j+pos-1))))#/eff[[2]]/eff[[3]]
  }
  
  # in the range between such timestep and the time horizon
  for (i in 2:(length(comp)-1)) {
    # Go until the end
    if (i+range > length(comp)) range <- length(comp) - i
    
    # pick up a timestep (in reverse)
    for (j in i:(i+range-1)){
      # Define the fit and appreciate it backwards
      seq_for_k <- (i+range):(j+1)
      # ... except in the first steps. Then it is forward
      if (i <= range) seq_for_k <- (j+1):(i+range)
      
      for (k in seq_for_k){
        fit_init <- fit[j:k]
        fit_corr <- appreciate(fit_init, eff = eff,
                               backwards = TRUE, depreciate = FALSE)
        
        while(comp[k] > 0){
          comp_present <- comp[j:k]
          # Find the position where it compensates to move the piece
          pos <- which_if(fit_corr, min, 
                          (if (cap[[1]] > 0) {
                            (comp_present < cap[[1]]) & fit_corr < fit[k]
                          } else {
                            fit_corr < fit[k] 
                          } )
          )
          # if there is none, break
          if (is.na(pos)) break
          
          # find piece size with some limitations
          if (comp[k] < piece_def){
            # the consumption in k is running out
            piecesize <- comp[k]
          } else if ((cap[[1]] - comp[j+pos-1] < piece_def) 
                     & cap[[1]] > 0){
            # the cap to storage is about to be reached
            piecesize <- cap[[1]] - comp[j+pos-1]
          } else if ((cap[[2]] - (comp[j+pos-1] - comp_init[j+pos-1]) < piece_def) 
                     & cap[[2]] > 0){
            # the cap of consumption is about to be reached
            piecesize <- cap[[2]] - (comp[j+pos-1] - comp_init[j+pos-1])
          } else {
            piecesize <- piece_def
          }
          
          # if piece size is very small, do nothing
          if (near(piecesize, 0)) break
          
          # save originals in case of revert
          origpos <- comp[k]
          endpos <- comp[j+pos-1]
          
          comp[k] <-  comp[k] - piecesize
          comp[j+pos-1] <- comp[j+pos-1] + eff_piecesize(piecesize)
          
          # if the caps are reched, break
          if (cap[[2]] > 0){
            if (comp[j+pos-1] - comp_init[j+pos-1] >= cap[[2]]) break
          }
          if (cap[[1]] > 0){
            if (comp[j+pos-1] >= cap[[1]]) break
          }
          
          # Batery: Use helper functions and update
          batt_update()
          
          # Case: SOC full
          # if (any(batt[j:k] > vol)) {
          #   batt_revert()
          # 
          #   pos2 <- which.min(vol - batt[j:k])
          #   piecesize2 <- vol - batt[j+pos2-1]
          # 
          #   comp[k] <-  comp[k] - piecesize2
          #   comp[j+pos2-1] <- comp[j+pos2-1] + eff_piecesize(piecesize2)
          # 
          #   batt_update()
          #   break
          # }
          
          # Case: SOC empty
          # if (any(batt[j:k] < 0)) {
          #   batt_revert()
          # 
          #   pos2 <- which.min(batt[j:k])
          #   piecesize2 <- batt[j+pos2-1]
          # 
          #   comp[k] <-  comp[k] + piecesize2
          #   comp[j+pos2-1] <- comp[j+pos2-1] - eff_piecesize(piecesize2)
          # 
          #   batt_update()
          #   break
          # }
        }
      }
    }
  }
  # Ensure the battery at the end is zero
  # batt[length(batt)] <- 0
  
  # if ("POSIXct" %in% attr(consumption, "tclass")) {
  #   print("yeah")
  # }
  
  sol <- cbind(consumption = comp, batt)
  sol
}
