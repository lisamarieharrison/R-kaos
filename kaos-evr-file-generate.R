# creates region definition files (.evr) for KAOS CTD stations to extract acoustic data using EV
# author: Lisa-Marie Harrison
# date: 13/08/2015

for (i in 1:nrow(ctd.info)) {
  
  #write echoview region definitions file for the CTD region
  line_1 <- paste("EVRG 7 5.4.96.24494", sep = "")
  line_2 <- "1"
  line_3 <- ""
  line_4 <- paste("13 4 1 0 3 -1 1", ctd.date.new[i], ctd.start.time.new[i],  "0", ctd.date.end[i], ctd.end.time.new[i], "250", sep = " ")
  line_5 <- "0"
  line_6 <- "0"
  line_7 <- "CTD class"
  line_8 <- paste(ctd.date.new[i], ctd.start.time.new[i],  "0", ctd.date.new[i], ctd.start.time.new[i],  "250", ctd.date.end[i], ctd.end.time.new[i], "250", ctd.date.end[i], ctd.end.time.new[i], "0", "1", sep = " ")
  line_9 <- paste("CTD_", i, sep = "")
  file_lines <- c(line_1, line_2, line_3, line_4, line_5, line_6, line_7, line_8, line_9)
  
  dummy.def <- file(paste("C:/Users/Lisa/Documents/phd/southern ocean/KAOS/regions/ctd_regions/region_def_file_stn_", i, ".evr", sep = ""), 'w')
  for (j in 1:9){
    writeLines(file_lines[j], con = dummy.def)
  }
  close(dummy.def)
  
}