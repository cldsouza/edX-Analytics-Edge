#Variables
  
#Date: the date of the stock price, always given as the first of the month.
#StockPrice: the average stock price of the company in the given month.

#Read files into DataFrames
IBM = read.csv ( "IBMStock.csv" )
GE = read.csv ( "GEStock.csv")
ProcterGamble = read.csv ( "ProcterGambleStock.csv")
CocaCola = read.csv (" CocaColaStock.csv")
Boeing = read.csv ("BoeingStock.csv")

# Right now, the date variable is stored as a factor. We can convert this to a "Date" object in R 
# by using the following five commands (one for each data set):

#The first argument to the as.Date function is the variable we want to convert
#and the second argument is the format of the Date variable. 

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

#Standard Deviation
sd(ProcterGamble$StockPrice)

#Plotting Graphs
#we would really like to see a line instead, since this is a continuous time period. 
#To do this, add the argument type="l" to your plot command

plot(CocaCola$Date, CocaCola$StockPrice, "l") 

plot(CocaCola$Date, CocaCola$StockPrice, "l", col = "red") 
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)

tapply (IBM$StockPrice, months(IBM$Date), mean)

