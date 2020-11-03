pollutantmean <- function (directory, pollutant, id = 1:322)
{
  merge_df <- data.frame ()
  for (i in id)
  {
    name = formatC(i, width = 3, flag = "0")
    filename <- paste(directory,"\\", name,".csv", sep="")
    reading <- read.csv(file = filename)
    merge_df <- rbind(merge_df, reading)
  }
  pollut_mean <- mean(merge_df[[pollutant]], na.rm = TRUE)
  print(pollut_mean)
}

pollutantmean("rprog_data_specdata\\specdata", "sulfate", id=1:10)
pollutantmean("rprog_data_specdata\\specdata", "nitrate", 70:72)
pollutantmean("rprog_data_specdata\\specdata","nitrate", 23)
