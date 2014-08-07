pollutantmean <- function(directory, pollutant, id=1:332){
    #set working directory and append directory in which data is found to it
    wd <- setwd("E:/Users/JeffPat390039/Documents/Machine_Learning/R/Data_Science/2_R_Programming/Programming_Assignment3/PA3-repo")
    wd1 <- paste(wd,"/",directory, sep = "", collapse = NULL)
   
    # Read in data with path name to file
    file <- list.files(wd1,full.names = TRUE)
    
    # Create an empty data frame
    dat <- data.frame()
    
    # row bind data specified in id to dat
    for (i in id){
        dat <- rbind(dat,read.csv(file[i]))
    }
    
    # assign names of header in dat to a character vector
    header <- names(dat)
    
    if (pollutant == header[2]){
       mn <- mean(dat$sulfate, na.rm = TRUE)
    }
    else if (pollutant == header[3]) {
        mn <- mean(dat$nitrate,na.rm = TRUE)
    }
return(mn)
}
