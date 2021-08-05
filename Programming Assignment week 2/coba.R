pollutantmean <- function(directory, pollutant, id = 1:332) {
  file_list <- list.files(directory, full.names = TRUE) #create a list of files
  dat <- data.frame() #create an empty data frame to accommodate all files
  for (i in id) {
    dat <- rbind(dat, read.csv(file_list[i]))
    #loop through all the files and rbind them together into the data frame
  }
  mean(dat[, pollutant], na.rm = TRUE)
  #return the mean of the pollutant across all monitors list
}

complete <- function(directory, id = 1:332) {
  files <- list.files(directory, full.names = TRUE) #create a list of files
  dat <- data.frame()
  for (i in id) {
    #loop through all the files
    temp <- read.csv(files[i])
    #delete all the rows with NAs
    temp <- na.omit(temp)
    tNobs <- nrow(temp)
    #count of the rows of complete data
    dat <- rbind(dat, data.frame(i, tNobs))
    #enumerate the complete cases by index
  }
  dat
  #return the final data frame
}

corr <- function(directory, shreshold = 0) {
  #create a file list
  files <- list.files(directory, full.names = TRUE)
  #create an empty vector
  dat <- vector(mode = "numeric", length = 0)
  #loop through all files
  for (i in 1:length(files)) {
    temp <- read.csv(files[i])
    #remove all NAs
    temp <- temp[complete.cases(temp), ]
    #count the number of observed cases
    sum_complete <- nrow(temp)
    if (sum_complete > shreshold){
      dat <- c(dat, cor(temp$sulfate, temp$nitrate))
    }
  }
  dat
}
