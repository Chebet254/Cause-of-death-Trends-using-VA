library(dplyr)
library(haven)
ddf <- read_dta("C:/Users/mochola/Downloads/va_updated.dta")%>% as_factor()
ddf <- read_dta("C:/Users/mochola/Downloads/YLLL (1).dta")%>% as_factor()     
ddf <- read_dta("C:/Users/mochola/Downloads/CODwithSES1.dta")%>% as_factor() 
nuhdss <- read_dta("C:/Users/mochola/Downloads/NUHDSSWITH ICD.dta")%>% as_factor()   #NUHDSSWITH ICD.dta
magu <- read_dta("C:/Users/mochola/Downloads/Magu with ICD.dta")%>% as_factor()




#create age_groups Neonate Infants 1-4 15-49 5-14 50-64 65+


nuhdss$age_grouped <- ifelse(nuhdss$agegroupatdeath == 'neonate:0-28 days', 'neonate',
                             ifelse(nuhdss$agegroupatdeath == 'infant:<1 Year', 'infant',
                                    ifelse(nuhdss$agegroupatdeath %in% c('5-9','10-14'), '5-14',
                                           ifelse(nuhdss$agegroupatdeath %in% c('15-19','20-24','25-29','30-34','35-39','40-44','45-49'), '15-49',
                                                  ifelse(nuhdss$agegroupatdeath %in% c('50-54','55-59','60-64'), '50-64', '65+')))))






# Descriptives on magu dataset

reg = freq_gen(magu, 'sex')



















#1.0
generate_frequency_table <- function(dataset, column, output_file) {
  # Calculate frequency table
  frequency_table <- table(dataset[[column]])
  
  # Calculate percentages
  percentages <- prop.table(frequency_table) * 100
  
  # Create a data frame with the results
  result <- data.frame(Category = names(frequency_table), Frequency = as.vector(frequency_table), Percentage = as.vector(percentages))
  
  # Write the result to a CSV file
  write.csv(result, file = output_file, row.names = FALSE)
  
  # Print the result
  print(result)
}



generate_frequency_table(data, "category", "output.csv")



#2.0 Generating cross tabs

generate_frequency_table <- function(dataset, column1, column2, output_file) {
  # Calculate frequency table
  frequency_table <- table(dataset[[column1]], dataset[[column2]])
  
  # Calculate row percentages
  row_percentages <- prop.table(frequency_table, margin = 1) * 100
  
  # Calculate column totals
  column_totals <- colSums(frequency_table)
  
  # Add row percentages and column totals to the frequency table
  frequency_table <- cbind(frequency_table, row_percentages)
  frequency_table <- rbind(frequency_table, column_totals)
  
  # Add column names
  #colnames(frequency_table) <- c(column1, column2, "Percentage")
  
  # Write the frequency table to a CSV file
  write.csv(frequency_table, file = output_file, row.names = TRUE)
  
  # Print the frequency table
  print(frequency_table)
}





reg = generate_frequency_table(ddf, column1 = 'GroupedCauses', column2 = 'gender', output_file = "output.csv")

reg = generate_frequency_table(ddf, column1 = 'GroupedCauses', column2 = 'age_cat', output_file = "output.csv")


