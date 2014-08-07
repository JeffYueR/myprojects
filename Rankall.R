rankall <- function (outcome, num = "best") {
    #set working directory and append directory in which data is found to it
    wd <- setwd("E:/Users/JeffPat390039/Documents/Machine_Learning/R/Data_Science/2_R_Programming/Programming_Assignment3/rprog-data-ProgAssignment3-data")
    
    # read outcome data
    read.outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # check that outcome is valid
    if (outcome != "heart attack") {
        if (outcome != "heart failure") {
            if (outcome != "pneumonia"){
                stop("Error in best(state, outcome) : invalid outcome")
            }
        }
    }
    
    h.s <- data.frame(read.outcome$Hospital.Name,read.outcome$State)
    names(h.s) <- c("hospital","state")
    
            
    # Order and rank data if outcome is "heart attack"
    if (outcome == "heart attack"){
            # column bind mortality rate to data frame with hospital and state
            h.a <- cbind(h.s, as.numeric(read.outcome[,11]))            
            names(h.a)[3] <- "30 Day Heart Attack Mortality Rate"
            
            # arrange State followed by mortality rate in ascending order and palce NA at the last
            h.a <- h.a[order(h.a[,2], h.a[,3],h.a[,1], na.last = TRUE),]
            
            # Split h.a into States and assign to a list called state
            state <- split(h.a, h.a$state)
            
            # Calculate the mortality rank for each state 
            for (i in 1:length(state)){
                Rank <- c(1:nrow(state[[i]]))
                state[[i]] <- cbind(state[[i]], Rank)
            }
            
     # Order and rank data if outcome is "heart failure"
    } else if (outcome == "heart failure"){
        # column bind mortality rate to data frame with hospital and state
        h.f <- cbind(h.s, as.numeric(read.outcome[,17]))            
        names(h.f)[3] <- "30 Day Heart Failure Mortality Rate"
        
        # arrange State followed by mortality rate in ascending order and palce NA at the last
        h.f <- h.f[order(h.f[,2], h.f[,3],h.f[,1], na.last = TRUE),]
        
        # Split h.a into States and assign to a list called state
        state <- split(h.f, h.f$state)
        
        # Calculate the mortality rank for each state 
        for (i in 1:length(state)){
            Rank <- c(1:nrow(state[[i]]))
            state[[i]] <- cbind(state[[i]], Rank)
        }
    
     # Order and rank data if outcome is "pnuemonia"
    } else if (outcome == "pneumonia"){
        # column bind mortality rate to data frame with hospital and state
        p <- cbind(h.s, as.numeric(read.outcome[,23]))            
        names(p)[3] <- "30 Day Pneumonia Mortality Rate"
        
        # arrange State followed by mortality rate in ascending order and palce NA at the last
        p <- p[order(p[,2], p[,3],p[,1], na.last = TRUE),]
        
        # Split h.a into States and assign to a list called state
        state <- split(p, p$state)
        
        # Calculate the mortality rank for each state 
        for (i in 1:length(state)){
            Rank <- c(1:nrow(state[[i]]))
            state[[i]] <- cbind(state[[i]], Rank)
        }
    }
         
    # Use lapply on the split data frame state to return the hospital name and state
    
    # First create a data frame called hosp
    hosp <- data.frame()
    
    # Perform lapply on state using function that evaluates the value of num and then pulls the respective
    # rank in which hospital name and state are located and assign to hosp
    # lapply will return a list of data frames for each value of num
    # Then call do.call with rbind.data.frame to re-constitute the list from lapply back into a data.frame
    hosp <- do.call(rbind.data.frame, lapply(state, function(z){
        if (num == "best")                    hosp <- z[1,1:2]
        else if (num == "worst")              hosp <- z[nrow(z),1:2]
        else if (as.numeric(num) > nrow(z)) hosp <- "NA"
        else                                  hosp <- z[as.numeric(num),1:2]
    }))
    
    return(hosp)
}
    
    
    