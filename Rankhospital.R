rankhospital <- function (state, outcome, num = "best") {
    #set working directory and append directory in which data is found to it
    wd <- setwd("E:/Users/JeffPat390039/Documents/Machine_Learning/R/Data_Science/2_R_Programming/Programming_Assignment3/rprog-data-ProgAssignment3-data")
    
    # read outcome data
    read.outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # check that state is valid
    if (sum(as.numeric(state == read.outcome[,7])) == 0) stop("Error in best(state, outcome) : invalid state")
    
    
    # check that outcome is valid
    if (outcome != "heart attack") {
        if (outcome != "heart failure") {
            if (outcome != "pneumonia"){
                stop("Error in best(state, outcome) : invalid outcome")
            }
        }
    }
    
    # Check if num is larger than the number of hospitals in a particular state
    # Read names of hospitals from read.outcome from the corresponding state into a vector called hospital
    hospital <- read.outcome[which(state == read.outcome$State),2]
    
    
    
    
   # If num = "best", call Best.R function to evaluate the best hospital
   if (num == "best"){
       best.hospital <- best(state, outcome)
       return(best.hospital)
   }
   else if (num == "worst") {
    
    # for num = "worst" do the following:
    # Check if outcome is "heart attack" and then create a data frame called heart attack
   
    # Read in values of heart attack mortality rates from the corresponding state into a vector called
    # heart.attack.rate and coerce the values to numeric
    # cbind hospital and heart.attack.rate into the data frame, heart attack
    # use the which.max function to find the indices in heart.attack that has the highest mortality rate and assign
    # to worst.hospital
    # Check using a if statement if there are more than 1 hospital with the worst mortality rate and sort
    # alphabetically and assign the first worst hospital to the variable called first.worst.hospital
            if (outcome == "heart attack"){
                heart.attack <- data.frame()
                heart.attack.rate <- as.numeric(read.outcome[which(state == read.outcome$State),11])
                heart.attack <- cbind(hospital,heart.attack.rate)
                worst.hospital <- heart.attack[which.max(heart.attack[,2])]
                if (length(worst.hospital > 1)){
                    sort(worst.hospital, decreasing = FALSE)
                    first.worst.hospital <-worst.hospital[1]     
                }
            
                return(first.worst.hospital)
            }
        # Do the similar operations for "heart failure" and "pneumonia"
            
            else if (outcome == "heart failure"){
                heart.failure <- data.frame()
                heart.failure.rate <- as.numeric(read.outcome[which(state == read.outcome$State),17])
                heart.failure <- cbind(hospital,heart.failure.rate)
                worst.hospital <- heart.failure[which.max(heart.failure[,2])]
                if (length(worst.hospital > 1)){
                    sort(worst.hospital, decreasing = FALSE)
                    first.worst.hospital <- worst.hospital[1]     
                }
        
                return(first.worst.hospital)
        
            } 
            else if (outcome == "pneumonia"){
                pneumonia <- data.frame()
                pneumonia.rate <- as.numeric(read.outcome[which(state == read.outcome$State),23])
                pneumonia <- cbind(hospital,pneumonia.rate)
                worst.hospital <- pneumonia[which.max(pneumonia[,2])]
                if (length(worst.hospital > 1)){
                    sort(worst.hospital, decreasing = FALSE)
                    first.worst.hospital <- worst.hospital[1]     
                }
        
                return(first.worst.hospital)
            }
    }
    # if num is not "best" nor "worst" but a number  
    
    if (as.numeric(num) <= length(hospital)) {
       if (outcome == "heart attack"){
           heart.attack <- data.frame()
           heart.attack.rate <- as.numeric(read.outcome[which(state == read.outcome$State),11])
           heart.attack <- cbind(hospital,heart.attack.rate)
           heart.attack <- heart.attack[order(heart.attack[,2], heart.attack[,1],na.last = TRUE),]
           rank <- c(1:nrow(heart.attack))
           heart.attack <- cbind(heart.attack,rank)
           Ranked.hospital <- heart.attack[which(heart.attack[,3] == as.numeric(num)),1]
           
           return(Ranked.hospital)
        
       } 
       else if (outcome == "heart failure"){
           heart.failure <- data.frame()
           heart.failure.rate <- as.numeric(read.outcome[which(state == read.outcome$State),17])
           heart.failure <- cbind(hospital,heart.failure.rate)
           heart.failure <- heart.failure[order(heart.failure[,2], heart.failure[,1],na.last = TRUE),]
           rank <- c(1:nrow(heart.failure))
           heart.failure <- cbind(heart.failure,rank)
           Ranked.hospital <- heart.failure[which(heart.failure[,3] == as.numeric(num)),1]
           
           return(Ranked.hospital)
       
        } 
       else if (outcome == "pneumonia"){
           pneumonia <- data.frame()
           pneumonia.rate <- as.numeric(read.outcome[which(state == read.outcome$State),23])
           pneumonia <- cbind(hospital,pneumonia.rate)
           pneumonia <- pneumonia[order(pneumonia[,2], pneumonia[,1],na.last = TRUE),]
           rank <- c(1:nrow(pneumonia))
           pneumonia <- cbind(pneumonia,rank)
           Ranked.hospital <- pneumonia[which(pneumonia[,3] == as.numeric(num)),1]
        
           return(Ranked.hospital)
        
        }
       return(Ranked.hospital)
    } else if (as.numeric(num) > length(hospital)) return(NA)

}