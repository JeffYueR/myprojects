corr <- function(directory, threshold = 0){
    #set working directory and append directory in which data is found to it
    wd <- setwd("E:/Users/JeffPat390039/Documents/Machine_Learning/R/Data_Science/2_R_Programming/Programming_Assignment1")
    wd1 <- paste(wd,"/",directory, sep = "", collapse = NULL)
 
    # Read in data with path name to file
    file <- list.files(wd1,full.names = TRUE)
    
    source("E:/Users/JeffPat390039/Documents/Machine_Learning/R/Data_Science/2_R_Programming/complete.R")
    
    # Generate a data frame of complete observations for all monitors and assign to comp
    comp <- complete("specdata",1:332)
    
     
    # Initialize a vector cr to a NULL vector
    cr <- NULL
    
    if (threshold > max(comp$nobs)){
        # if threshold exceeds max observations set cr to a numeric vector of length zero
        cr <- numeric(length = 0) 
    } else {
        # For each monitor check if no. of observations is larger than threshold,
        # if yes, then read in the file for that monitor after removing all NAs
        for (i in 1:length(comp$nobs)){
            
           if (comp$nobs[i] > threshold){
                dat <- na.omit(read.csv(file[i]))
                
                              
                # Calculate correlation for sulfate and nitrate and assign to vector cr
                # if length of cr is zero, then assign correlation value to first element in cr
                # else if length of cr is not zero, then concatenate other correlation values to cr
                if (length(cr) == 0){
                    cr <- cor(dat$sulfate, dat$nitrate, use = "everything")
                } else {
                    cr <- c(cr,cor(dat$sulfate, dat$nitrate, use = "everything"))
                }
            
            } 
                
            }
        }
        
  return(cr)  
  
}