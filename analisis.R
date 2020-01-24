# For this project you need to install fitdistrplus, please go to the packages section and install it
library(fitdistrplus)

# Please write the path file here
path_file = "/path/to/your/file/here.csv"

# read the csv file
dataframe <- read.csv2(file=path_file, header=TRUE, sep=",")

columns <- c("DENGUE", "DENGUE.GRAVE", "ZIKA", "LEISHMANIASIS.CUTANEA", "MALARIA")

for (column_name in columns) {
  cat("-------------", column_name, "-------------\n")
  # Select de columns to do the analisis
  illness = subset(dataframe, select=c("SEMANAS.2017", column_name))
  # Plot the data found
  plot(x=illness$SEMANAS.2017, y=illness[[column_name]], main=column_name, xlab="SEMANAS 2017", ylab="CANTIDAD")
  # Draw a regression line
  lines(lowess(illness$SEMANAS.2017,illness[[column_name]]), col="red")
  # Show a summary about the data
  print(summary(illness))
  # Plot a histogram with the summary data
  hist(illness[[column_name]], main = column_name, xlab = column_name)
  # Try to analize if these graphs have a normal behavior
  normal = fitdist(illness[[column_name]], "norm")
  print(summary(normal))
  denscomp(normal, main = column_name)
  cat("-----------------------------------\n")
}
