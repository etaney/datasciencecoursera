pollutantmean <- function(directory, pollutant, id = 1:332){
  dir <- paste(getwd(),directory,sep="/")
  sensor <- id
  i <- 1
  while(i <= length(sensor)){
    if(sensor[i] < 100){
      if(sensor[i] < 10){
        filename <- paste("0", "0", as.character(sensor[i]), sep="")
      }
      else {
        filename <- paste("0", as.character(sensor[i]), sep="")
      }
    }
    else{
      filename <- as.character(sensor[i])
    }
    
    if(i==1){
      allsensors <- read.csv(paste(dir, "/", filename, ".csv", sep=""))
    }
    else{
      currentsensor <- read.csv(paste(dir, "/", filename, ".csv", sep=""))
      allsensors <- rbind(allsensors, currentsensor)
    }
    
    i <- i+1
    
  }
  
  
  mean(allsensors[,pollutant], na.rm=TRUE)
}

complete <- function(directory, id = 1:332){
  
  sensorlist <- id
  totalnobs <- data.frame(ID=integer(), nobs=integer())
  i <- 1
  while(i <= length(sensorlist)){
    
    sensornum <- str_pad(as.character(sensorlist[i]), 3, side="left", pad="0")
    dir <- paste(getwd(), "/", directory, "/",  sensornum, ".csv", sep="")
    tempdf <- read.csv(dir)
    completetempdf <- tempdf[complete.cases(tempdf),]
    temprow <- data.frame(ID=tempdf[1,"ID"], nobs=nrow(completetempdf))
    totalnobs <- rbind(totalnobs, temprow)
    i <- i+1
    
  }
  
  totalnobs
  
}

corr <- function(directory, threshhold = 0){
  
  threshholdlist <- subset.data.frame(complete(directory), nobs > threshhold,  select="ID")
  i <- 1
  result <- double()
  print(result)
  while(i <= nrow(threshholdlist)){
    
    index <- paste(str_pad(as.character(threshholdlist[i,"ID"]), 3, side="left", pad="0"), "csv", sep=".")
    location <- paste(directory, index, sep="/")
    tempset <- read.csv(location)
    tempcomplete <- subset(tempset, complete.cases(tempset))
    result <- c(result, cor(tempcomplete$sulfate, tempcomplete$nitrate))
    
    i <- i+1
    
  }
  result
}