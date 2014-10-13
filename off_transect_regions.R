
#function to find date and time of start of CTD drop
region_times <- read.csv(file = "C:/Users/43439535/Documents/Lisa/KAOS/regions/off_transect_times.csv")

EVRegionTimes <- function(region_times){
  
  start_time <- 0
  end_time <- 0
  for(i in 1:nrow(region_times)){
    
    #pad times with 0's to make correct length for .evr file
    start <- chron(times. = as.character(region_times$start_time[i]), format = "h:m:s", out.format = "hms")
    end <- chron(times. = as.character(region_times$end_time[i]), format = "h:m:s", out.format = "hms")
    
    start_time[i]  <- paste(start, 0, 0, 0, 0, sep = "")
    end_time[i]    <- paste(end, 0, 0, 0, 0, sep = "")
        
  }
  return(list(start_time = start_time, end_time = end_time))  
}

start_time <- EVRegionTimes(region_times)$start_time
end_time <- EVRegionTimes(region_times)$end_time
start_date <- region_times$start_date
end_date <- region_times$end_date

for (i in 1:nrow(region_times)) {

  #write echoview region definitions file for the CTD region
  line_1 <- paste("EVRG 7 5.4.96.24494", sep = "")
  line_2 <- "1"
  line_3 <- ""
  line_4 <- paste("13 4 1 0 4 -1 1", start_date[i], start_time[i],  "0", end_date[i], end_time[i], "250", sep = " ")
  line_5 <- "0"
  line_6 <- "0"
  line_7 <- "off_transect"
  line_8 <- paste(start_date[i], start_time[i],  "0", start_date[i], start_time[i],  "250", end_date[i], end_time[i], "250", end_date[i], end_time[i], "0", "0", sep = " ")
  line_9 <- paste("region_", start_time, sep = "")
  file_lines <- c(line_1, line_2, line_3, line_4, line_5, line_6, line_7, line_8, line_9)
  
  file_name <- paste("C:/Users/43439535/Documents/Lisa/KAOS/regions/off transect/", start_date[i], "_", start_time[i], ".evr", sep = "")
  dummy.def <- file(file_name, 'w')
  for (j in 1:9){
    writeLines(file_lines[j], con = dummy.def)
  }
  close(dummy.def)
  
  print(i)
  if(1 == 1) flush.console()
  
}


