# code to read in each excel worksheet as individual dataframes
for (i in 2:length(worksheets)){assign(paste0("df", i), as.data.frame(worksheets[i]))}
# define function to clean data in each data frame (updated based on your data)
cleaner <- function(df){
  # drop rows with missing values 
  df <- df[rowSums(is.na(df)) == 0,] 
  # remove serial comma from all variables 
  # df[,-1] <- as.numeric(gsub(",", "", as.matrix(df[,-1])))
  # create numeric version of year variable for graphing 
  # df$Year <- as.numeric(substr(df$year, 1, 4))
  
  # drop rows with 0 values 
  
  
  
  return(df)
}