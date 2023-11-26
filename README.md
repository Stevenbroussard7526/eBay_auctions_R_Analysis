#The data of over 1000 eBay auctions is provided in the file eBayAcution.csv. 
#Use RStudio to study this marketplace. 
#(Source: The data is adapted from this book: https://www.dataminingbook.com/book/r-2nd-edition-2023)

#1) Load the file: "eBayAcution.csv" and save it as auctionData.
auctionData = read.csv("eBayAuctions.csv")


#2) Write a code that checks if the dataset has any missing values, 
#   a code that returns the number of auctions (i.e., rows), 
#   and one to return the number of variables (i.e., columns).
anyNA(auctionData)
nrow(auctionData)
ncol(auctionData)

#3) What is the maximum auction duration? How many auctions were open for these many days? 
#   What is the average auction duration? What percentage of the auctions have an above average duration?
max(auctionData$Duration)
sum(auctionData$Duration==10)
averageduration=mean(auctionData$Duration,na.rm = T)
greaterMean= (auctionData[auctionData$Duration > averageduration,])
percentage= (round((nrow(greaterMean)/nrow(auctionData)*100),digits=2))
percentage


#4) Create a new variable called Ratio that calculates the ratio of the closing price over the opening price 
#   for each auction and add this variable to the dataset as a new column. 
#   What's the average ratio of all auctions? What's the average ratio of 'Computer' auctions?

x.list= list(Ratio=(auctionData$ClosePrice/auctionData$OpenPrice))
auctionData$Ratio=x.list$Ratio
mean(x.list$Ratio,na.rm = T)

mean(auctionData$Ratio[auctionData$Category=="Computer"],na.rm = T)

#5) Create an object named "catNames" that contains the names of unique auction categories, 
#   sorted in alphabetical order. Write a code to return the number of categories stored in this object.
catNames=unique(auctionData$Category)
catNames
sort(catNames)
length(catNames)


#6) Write a loop to go through "catNames" and calculate the number of auctions in each category. 
#   In so doing, save the results in a vector called "numAuctions". 
#   Write a code to return the values stored in this object.

for(eachcatname in length(catNames)){
  numAuctions= unique(table(auctionData$Category))
  print(numAuctions)
}


#7) Combine the two objects (catNames and numAuctions) into a new data frame called catInfo. 
# Write two different codes to return the fifth element of the second column in the catInfo dataframe.
catInfo=data.frame(catNames,numAuctions)
catInfo[[2]][5]
catInfo[["numAuctions"]][5]


#8) Write a piece of code that prints the name of each category and the number of auctions in that category.
for(category in 1:nrow(catInfo)){
  cat("In the category of",catInfo$catNames[category],"there was a total of",catInfo$numAuctions[category],"auctions","\n")
}

#9) Create a function, called weekendTest, that checks whether a given day is a weekend (endDay of 'Sat' or 'Sun') 
#   or not and returns TRUE or FALSE (logical constants in R).
#   Then use this function to create a new variable (called Weekend) that shows if each auction had an endDay of the weekend or not. 
#   Add this variable to the dataset as a new column. How many auction ended on weekend? (Write a code that returns this value)

weekendTest= function(x){
  if(x=='Sat' | x== 'Sun'){
    return(T)
  } else {
    return(F)
  }
}

weekendTest(x='Sat')
weekendTest(x='Sun')
weekendTest(x='Mon')


weekend= rep(NA, nrow(auctionData))
for(i in 1: nrow(auctionData)) {
  weekend[i] = weekendTest (auctionData$endDay[i])
}

weekend

auctionData$Weekend = weekend 
sum(weekend)
