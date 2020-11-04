
pollutantmean <- function (directory, pollutant, id = 1:332)
{
  merge_df <- data.frame ()
  for (i in id)
  {
    name = formatC(i, width = 3, flag = "0")
    # format the file name to 3 width string
    filename <- paste(directory,"\\", name,".csv", sep="")
    # add suffix
    reading <- read.csv(file = filename)
    #read data sheet
    merge_df <- rbind(merge_df, reading)
    # merge all data sheet to a data frame
  }
  pollut_mean <- mean(merge_df[[pollutant]], na.rm = TRUE)
  # calculate column mean according to column name, and remove NA number
  pollut_mean
}

complete <- function (directory, id = 1:332)
  {
  complete_df <- data.frame ()
  for (i in id)
  {
    name = formatC(i, width = 3, flag = "0")
    filename <- paste(directory,"\\", name,".csv", sep="")
    filename
    reading <- read.csv(file = filename)
    reading_sub <- na.omit(reading)
    # other way:reading_sub <- reading[complet.cases(reading),]
    # remove rows contain NA
    final_df <- data.frame("id" = i, "nobs" = nrow(reading_sub))
    complete_df <- rbind(complete_df, final_df)
  }
  complete_df
  }

pollutantmean("rprog_data_specdata\\specdata", "sulfate", id=1:10)
pollutantmean("rprog_data_specdata\\specdata", "nitrate", 70:72)
pollutantmean("rprog_data_specdata\\specdata","nitrate", 23)
pollutantmean("rprog_data_specdata\\specdata", "sulfate", 34)
pollutantmean("rprog_data_specdata\\specdata","nitrate")

complete ("rprog_data_specdata\\specdata", id=3)
complete ("rprog_data_specdata\\specdata", 1)
complete("rprog_data_specdata\\specdata", c(2, 4, 8, 10, 12))
complete ("rprog_data_specdata\\specdata", 30:25)   
complete ("rprog_data_specdata\\specdata", 54)
RNGversion("3.5.1")  
set.seed(42)
cc <- complete ("rprog_data_specdata\\specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])
