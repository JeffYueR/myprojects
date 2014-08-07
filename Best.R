best <- function (state, outcome){
#set working directory and append directory in which data is found to it
wd <- setwd("E:/Users/JeffPat390039/Documents/Machine_Learning/R/Data_Science/2_R_Programming/Programming_Assignment3/PA3-repo/rprog-data-ProgAssignment3-data")

# read outcome data
read.outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

# check that state is valid
if (sum(as.numeric(state == read.outcome[,7])) == 0){
    stop("Error in best(state, outcome) : invalid state")
}

# check that outcome is valid
if (outcome != "heart attack") {
    if (outcome != "heart failure") {
        if (outcome != "pneumonia"){
    stop("Error in best(state, outcome) : invalid outcome")
}
}
}

# Check if outcome is "heart attack" and then create a data frame called heart attack
# Read names of hospitals from read.outcome from the corresponding state into a vector called hospital
# Read in values of heart attack mortality rates from the corresponding state into a vector called
# heart.attack.rate and coerce the values to numeric
# cbind hospital and heart.attack.rate into the data frame, heart attack
# use the which.min function to find the indices in heart.attack that has the lowest mortality rate and assign
# to best.hospital
# Check using a if statement if there are more than 1 hospital with the best mortality rate and sort
# alphabetically and assign the first best hospital to the variable called first.best.hospital
if (outcome == "heart attack"){
    heart.attack <- data.frame()
    hospital <- read.outcome[which(state == read.outcome$State),2]
    heart.attack.rate <- as.numeric(read.outcome[which(state == read.outcome$State),11])
    heart.attack <- cbind(hospital,heart.attack.rate)
    best.hospital <- heart.attack[which.min(heart.attack[,2])]
    if (length(best.hospital > 1)){
        sort(best.hospital, decreasing = FALSE)
        first.best.hospital <- best.hospital[1]     
    }
    
    return(first.best.hospital)
    
    # Do the similar operations for "heart failure" and "pneumonia"
} else if (outcome == "heart failure"){
    heart.failure <- data.frame()
    hospital <- read.outcome[which(state == read.outcome$State),2]
    heart.failure.rate <- as.numeric(read.outcome[which(state == read.outcome$State),17])
    heart.failure <- cbind(hospital,heart.failure.rate)
    best.hospital <- heart.failure[which.min(heart.failure[,2])]
    if (length(best.hospital > 1)){
        sort(best.hospital, decreasing = FALSE)
        first.best.hospital <- best.hospital[1]     
    }
    
    return(first.best.hospital)
    
} else if (outcome == "pneumonia"){
    pneumonia <- data.frame()
    hospital <- read.outcome[which(state == read.outcome$State),2]
    pneumonia.rate <- as.numeric(read.outcome[which(state == read.outcome$State),23])
    pneumonia <- cbind(hospital,pneumonia.rate)
    best.hospital <- pneumonia[which.min(pneumonia[,2])]
    if (length(best.hospital > 1)){
        sort(best.hospital, decreasing = FALSE)
        first.best.hospital <- best.hospital[1]     
    }
    
    return(first.best.hospital)
}
}
