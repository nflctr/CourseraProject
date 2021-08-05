pollutantmean <- function(directory,pollutant,id=1:332){
  pollutant_mean <- c()
  for(monitor in id){
    path <- paste(getwd(),"/",directory,"/",sprintf("%03d",monitor),".csv",sep = "")
    monitor_data <- read.csv(path)
    pollutant_data <- monitor_data[pollutant]
    pollutant_mean <- c(pollutant_mean, pollutant_data[!is.na(pollutant_data)])
  }
  mean(pollutant_mean)
}
