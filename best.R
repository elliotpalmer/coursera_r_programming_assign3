best <- function(state,outcome) {
  
  outcome <- tolower(outcome)
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
      
     ## State
        stck <- sum(data$State == state)
        if (stck == 0) {
          # throw error: invalid state
        }
     ## Outcome 
        occk <- c("heart attack","heart failure","pneumonia")
        occka <- sum(occk == outcome)
        if (occka == 0) {
          # throw error : invalid outcome
        }
        
      collu <- data.frame(oc = occk,col = c(11,17,23))    
      
      occol <- collu[collu$oc == outcome, 2]
    
  ## Return hospital name in that state with lowest 30-day death
     
       # create evaludation data frame
          rankdf <- data.frame(hosp = data$Hospital.Name, st = data$State, met = data[,occol])
          
          rankdf$hosp <- as.character(rankdf$hosp)        
  
          rankdf <- rankdf[rankdf$st == state,]
  
          rankdf$met <- as.numeric(rankdf$met)
          rankdfcl <- rankdf[!is.na(rankdf$met),]
          
          win <- rankdfcl[rankdfcl$met == min(rankdfcl$met),]      
  
          winsort <- sort(win$hosp)
          
          winsort[1]
  ##rate
  
}