requestDataRaw <- read.csv("FlexRFR2017_53118")
#demo() runs opens demo file
typesRequests <- requestDataRaw[,3]

#Unpacking and reorganizing the date data into a format more easily used by Rstudio
datesAndTimes <- strsplit(toString(requestDataRaw[,2])," ")
temp <- matrix(unlist(datesAndTimes), ncol = 3, byrow = TRUE)
dates <- temp[,1]
datesAndTimes <- strsplit(dates, "/")
finalDates <- matrix(unlist(datesAndTimes), ncol = 3, byrow = TRUE)

#Separates the raw date data into two years
year2017Date <- subset(finalDates, finalDates[,3] == "2017")
year2018Date <- subset(finalDates, finalDates[,3] == "2018")

#Next separate the fequency and date pairs into two lists (three total, one for the months 
#and two for the frequencies for each month)
freqMonthlyDate2017 <- table(year2017Date[,1])

#There are the same number of elements in list, if one is short fill it with zeros until they are the same length
freqMonthlyDate2018 <- table(year2018Date[,1])

#Order the months from 1 to 12
year2017Freq <- c()
year2018Freq <- c()

for(i in months){
  year2017Freq <- append(year2017Freq, freqMonthlyDate2017[toString(i)])
  year2018Freq <- append(year2018Freq, freqMonthlyDate2018[toString(i)])
  
}

#set months equal to a vector of months from the longest set
lenDateSets <- c(length(year2017Freq),length(year2018Freq))
months <- c(1:max(lenDateSets))
#
months17 <- c(matrix(year2017Freq, ncol=1, byrow=TRUE))
months18 <- c(matrix(year2018Freq, ncol=1, byrow=TRUE))

#normalize data so they are the same lenght by adding 0s to the end of the hsorter list
while(length(months17) != length(months18))
{
  if(length(months17) > length(months18)){
    months18 <- append(months18,0)
  }
  else{
    months17 <- append(months17,0)
  }
}

#Plot normalized years on same plot
#plot monthly
#plot daily on separate plots but with same scale

library(reshape)
library(ggplot2)
df1 <- data.frame(months17,months18,months)
avgRequestsPerMonth = (sum(df1$months17,na.rm=TRUE) + sum(df1$months18,na.rm=TRUE)) / 24
df2 <- melt(df1, id.vars='months')

#head(df2)

d <- ggplot(df2, aes(x=months, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge')

#For Request Type data
#Look into changing the names of the columns
#Also what does "fill=variable" do?
df3 <- data.frame(table(typesRequests))
bt <- ggplot(df3, aes(x=typesRequests, y=Freq, fill=typesRequests)) + geom_bar(stat='identity') #create a bar graph
t <- ggplot(df3, aes(x="", y=Freq, fill=typesRequests)) + geom_bar(stat='identity') #Create a bar chart
tp <- t + coord_polar("y",start=0) #Create a pie chart
#Next plot the request ype according to the respective year as well as the order of months to see
#if certain requests have seasonal trends
#Take top 2-4 request types and plot them together by months and years

datesAndTypesRaw <- data.frame(finalDates,typesRequests)
#levels(droplevels(subdf$letters))      # drops the levels that do not occur
#Separates the raw date data into two years

temp <- as.numeric(as.character(datesAndTypesRaw[,3]))
datesAndTypes2017 <- subset(datesAndTypesRaw, temp == 2017)
datesAndTypes2018 <- subset(datesAndTypesRaw, temp == 2018)

dfTypes2017 <- data.frame(table(as.character(datesAndTypes2017[,4])))
dfTypes2018 <- data.frame(table(as.character(datesAndTypes2018[,4])))

#Need to make sure both sets have the same number of data types
newTypes <- setdiff(dfTypes2017$Var1,dfTypes2018$Var1)
if(length(dfTypes2018$Var1) > length(dfTypes2017$Var1)){
  levels(dfTypes2017[,1]) <- c(levels(dfTypes2017[,1]),newTypes)
  dfTypes2017[nrow(dfTypes2017) + 1,] =  list(newTypes,integer(length(newTypes)))
}

if(length(dfTypes2018$Var1) < length(dfTypes2017$Var1)){
  levels(dfTypes2018[,1]) <- c(levels(dfTypes2018[,1]),newTypes)
  dfTypes2018[nrow(dfTypes2018) + 1,] =  list(newTypes,integer(length(newTypes)))
}

dfTypesBothYears <- data.frame(dfTypes2017[,2],dfTypes2018[,2],dfTypes2017[,1])
df4 <- melt(dfTypesBothYears,id.vars='dfTypes2017...1.')
#Difference in number of request per year
dt <- ggplot(df4, aes(x="", y=value, fill=variable)) + geom_bar(stat='identity', position='dodge')
#Compares each request type for the years 2017 and 2018
dt2 <- ggplot(df4, aes(x=dfTypes2017...1., y=value, fill=variable)) + geom_bar(stat='identity', position='dodge')

#Next we compare the top 2-4 request types for each month of a given year
#a <- c(1:100)
#tail(sort(a),5)
#Separate the top 2 request types for each year and plot them together on the same bar plot
# Online and TASC CARD: MyBenefits
tascCardRequest2017 <- subset(datesAndTypes2017,typesRequests == "TASC CARD: MyBenefits")
onlineRequest2017 <- subset(datesAndTypes2017,typesRequests == "Online")
#mobile

tascCardRequest2018 <- subset(datesAndTypes2018,typesRequests == "TASC CARD: MyBenefits")
onlineRequest2018 <- subset(datesAndTypes2018,typesRequests == "Online")
#mobile
# Month is in first column
#use months list in dataframe
tascCardRequest2017Freq <- table(tascCardRequest2017[,1])
onlineRequest2017Freq <- table(onlineRequest2017[,1])
months <- c(1:12)
tascCardRequest2018 <- table(tascCardRequest2018[,1])
onlineRequest2018 <- table(onlineRequest2018[,1])

card2017 <- c()
card2018 <- c()
online2017 <- c()
online2018 <- c()
months <- c(1:12)

for(i in months){
  card2017 <- append(card2017, tascCardRequest2017Freq[toString(i)])
  card2018 <- append(card2018, tascCardRequest2018[toString(i)])
  online2017 <- append(online2017, onlineRequest2017Freq[toString(i)])
  online2018 <- append(online2018, onlineRequest2018[toString(i)])
}
df5 <- data.frame(card2017,card2018,online2017,online2018, months)

df6 <- melt(df5, id.vars='months')

dft <- ggplot(df6, aes(x=months, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge')

months <- c("Jan","Feb","March","April","May","June","July","August","September","October","November","December")