#This code is used to combine multiple excel worksheets into one dataframe. Particularly useful when combining multiple worksheets of production data with each worksheet is a date of a month, and in each worksheet, data is saved in hourly basis.

#the first method, using XlConnect.


# --> this method has a limitation that XlConnect doesnt work well with excel files with dynamic links. When importing into R, it gives NA values.

# --> to make sure that importing is perfect. It is advisable to disable all dynamic links in excel file by going to Data --> Edit Links --> Delink


library(XLConnect)
# load data file (excel files ended with cls, xlsc, or xlsm)
datafile <- loadWorkbook("data1.xlsx") # This is a static worksheet, without any dynamic links

# obtain sheet names
worksheets <- getSheets(datafile)
names(worksheets) <- worksheets


# dataframe
worksheets_list <- lapply(worksheets, function(.sheet){readWorksheet(object=datafile, .sheet)})


# limit worksheet_list to sheets with at least 1 dimension 
worksheets_list2 <- worksheets_list[sapply(worksheets_list, function(x) dim(x)[1]) > 0]


# code to read in each excel worksheet as individual dataframes
 for (i in 2:length(worksheets_list2)){assign(paste0("df", i), as.data.frame(worksheets_list2[i]))}

# define function to clean data in each data frame (updated based on your data). You must define here carefully otherwise it will not work well with some certain type of data. The fastest way is only drop out missing values. Other value can be dealed with using query in MySQL
cleaner <- function(df){
  # drop rows with missing values 
  df <- df[rowSums(is.na(df)) == 0,] 
  # remove serial comma from all variables 
 # df[,-1] <- as.numeric(gsub(",", "", as.matrix(df[,-1])))
  # create numeric version of year variable for graphing 
 # df$Year <- as.numeric(substr(df$year, 1, 4))
# return cleaned df      
  return(df)
}

# clean sheets and create one data frame
data1 <- do.call(rbind,lapply(names(worksheets_list2), function(x) cleaner(worksheets_list2[[x]])))

cat("Print out the data 1 frame \n")

print(data1)


# Method is with readxl package. This is superior than the former one as readxl can handle excel files with dynamic links. This means it will retain values and ignore the links.


# ----------------
library(readxl)    
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


#start to read and write data into csv file
worksheets <- read_excel_allsheets("data1.xlsx")
source("cleaning.R")
filedata <- do.call(rbind,lapply(names(worksheets), function(x) cleaner(worksheets[[x]])))
write.table(filedata, "myDF.csv", sep = ",", col.names = !file.exists("myDF.csv"), row.names=FALSE, append = T)


worksheets <- read_excel_allsheets("data2.xlsm")
source("cleaning.R")
filedata <- do.call(rbind,lapply(names(worksheets), function(x) cleaner(worksheets[[x]])))
write.table(filedata, "myDF.csv", sep = ",", col.names = !file.exists("myDF.csv"), row.names=FALSE, append = T)

#end

#https://medium.com/@niharika.goel/merge-multiple-csv-excel-files-in-a-folder-using-r-e385d962a90a

