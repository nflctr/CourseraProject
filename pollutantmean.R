pollutantmean <- function(directory, pollutant, id = 1:332){
  data <- paste0(directory, '/', formatC(id, width=3, flag ="0"), ".csv")
  daftar <- lapply(fileNames, data.table::fread)
  a <- rbindlist(daftar)
  
  if (c(pollutant) %in% names(a)){
    return(a[, lapply(.SD, mean, na.rm = TRUE), .SDcols = pollutant][[1]])
  }
}