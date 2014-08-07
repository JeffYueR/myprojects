complete <- function(directory, id = 1:332){
    #set working directory and append directory in which data is found to it
    wd <- setwd("E:/Users/JeffPat390039/Documents/Machine_Learning/R/Data_Science/2_R_Programming/Programming_Assignment1")
    wd1 <- paste(wd,"/",directory, sep = "", collapse = NULL)
    
    # Read in data with path name to file
    file <- list.files(wd1,full.names = TRUE)
    
    # Create an empty data frame for collecting the read data and for counting the completed observations 
    obs <- data.frame()
    
   # read data specified in id to dat
    for (i in id){
        dat <- read.csv(file[i])
    
        # For each monitor number count the number of completed cases with no NA
        completed.obs <- complete.cases(dat$sulfate) * complete.cases(dat$nitrate)
        
        # Input count data into a vector
        a <- c(i,sum(completed.obs))
        
        # Then column bind data specified in id to dat
        obs <- rbind(obs,a)
      }
    
    names(obs) <- c("id","nobs")
    return(obs)
}