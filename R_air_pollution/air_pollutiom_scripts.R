#calculte mean of sulfate or nitrate, remove NA 
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
  # calculate column mean according to column name, and remove NA value
  pollut_mean
}

#calculate completed cases of each monitor recordings
complete <- function (directory, id = 1:332)
  {
  complete_df <- data.frame ()
  for (i in id)
  {
    name = formatC(i, width = 3, flag = "0")
    filename <- paste(directory,"\\", name,".csv", sep="")
    filename
    reading <- read.csv(file = filename)
    #read csv file
    reading_sub <- na.omit(reading)
    # other way:reading_sub <- reading[complet.cases(reading),]
    # remove rows containing NA
    final_df <- data.frame("id" = i, "nobs" = nrow(reading_sub))
    complete_df <- rbind(complete_df, final_df)
  }
  complete_df
}

#calculate the correlate between sulfate and nitrate of monitor which record completed values over threshold
corr <- function (directory, threshold = 0)
{
  cor_vec <- c()
  for (i in 1:332)
  {
    name = formatC(i, width = 3, flag = "0")
    # conver file name to 3 length string
    filename <- paste(directory,"\\", name,".csv", sep="")
    filename
    reading <- read.csv(file = filename)
    # read csv file
    cplt_sub <- na.omit(reading)
    #remove rows containing NA
    n <- nrow(cplt_sub)
    #calcute the completed case number = row number
    if (n > threshold)
    {
      x <- reading[["sulfate"]]
      y <- reading[["nitrate"]]
      corr<- cor(x, y, use = "complete.obs")
      #use cor()function to calculate corrletaion
      cor_vec <- c(cor_vec, corr)
    }
    else
    {
      cor_vec
    }
  }
  cor_vec
}



# calculate mean
pollutantmean("rprog_data_specdata\\specdata", "sulfate", id=1:10)
pollutantmean("rprog_data_specdata\\specdata", "nitrate", 70:72)
pollutantmean("rprog_data_specdata\\specdata","nitrate", 23)
pollutantmean("rprog_data_specdata\\specdata", "sulfate", 34)
pollutantmean("rprog_data_specdata\\specdata","nitrate")

#calculate completed cases
complete ("rprog_data_specdata\\specdata", id=3)
complete ("rprog_data_specdata\\specdata", 1)
complete("rprog_data_specdata\\specdata", c(2, 4, 8, 10, 12))
complete ("rprog_data_specdata\\specdata", 30:25)
complete("rprog_data_specdata\\specdata", c(6, 10, 20, 34, 100, 200, 310))
complete ("rprog_data_specdata\\specdata", 54)
RNGversion("3.5.1")  
set.seed(42)
cc <- complete ("rprog_data_specdata\\specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

# calculate correlation between sulfate and nitrate
source("corr.R")
source("complete.R")
cr <- corr("rprog_data_specdata\\specdata", 150)
head(cr)
summary(cr)

cr <- corr("rprog_data_specdata\\specdata", 400)
head(cr)
summary(cr)

cr <- corr("rprog_data_specdata\\specdata", 5000)
summary(cr)
length(cr)

cr <- corr("rprog_data_specdata\\specdata")
summary(cr)
length(cr)

cr <- corr("rprog_data_specdata\\specdata")
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("rprog_data_specdata\\specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("rprog_data_specdata\\specdata", 2000)                
n <- length(cr)                
cr <- corr("rprog_data_specdata\\specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
