##Capstone Project Start
##suppressMessages(require(rjson))
##suppressMessages(require(tm))
require(jsonlite)
require(ggplot2)


library(doParallel)
cl<-makeCluster(4)
registerDoParallel(cl)


##RJSON STYLE
##json_file <- "C:/Users/nswitzer/Documents/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_checkin.json"
##json_data <- fromJSON(json_file)

##jsonlite style

business_file <- ("C:/Users/nswitzer/Documents/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json")
checkin_file <- ("C:/Users/nswitzer/Documents/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_checkin.json")
review_file <- ("C:/Users/nswitzer/Documents/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_review.json")
tip_file <- ("C:/Users/nswitzer/Documents/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_tip.json")
user_file <- ("C:/Users/nswitzer/Documents/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_user.json")
##According to StackOverflow
##http://stackoverflow.com/questions/26519455/error-parsing-json-file-with-the-jsonlite-package?_sm_au_=iVVWtcSDTcntsVDP

checkins <- fromJSON(sprintf("[%s]", paste(readLines(checkin_file), collapse=",")))
businesses <- fromJSON(sprintf("[%s]", paste(readLines(business_file), collapse=",")))
reviews <- fromJSON(sprintf("[%s]", paste(readLines(review_file), collapse=",")))
tips <- fromJSON(sprintf("[%s]", paste(readLines(tip_file), collapse=",")))
users <- fromJSON(sprintf("[%s]", paste(readLines(user_file), collapse=",")))

head(checkins)

##Unlist Reviews

##df <- data.frame(matrix(unlist(reviews),nrow=150, byrow=T),stringsAsFactors = F)
##names(df) <- names(reviews[[1]])

##Ideas

##When do people play basketball somewhere???
##Which doctors get best reviews?

##Get doctors only
##Categories is list, so we are using GREP, not which

doctors <- grep("Doctor",businesses$categories)
docsData <- businesses[doctors,]

docReviews <- reviews[reviews$business_id %in% docsData$business_id,]

stryker <- grep("chairs", docReviews$text)
strykerInfo <- docReviews[stryker,]

fivers <- subset(reviews, stars ==5)

class(reviews$stars)

hist(docsData$stars)

##Doc Reviews Word Cloud
##https://deltadna.com/blog/text-mining-in-r-for-term-frequency/

library('wordcloud')
review_text <- paste(docReviews$text, collapse=" ")
review_source <- VectorSource(review_text)
corpus <- Corpus(review_source)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
words <- names(frequency)
wordcloud(words[1:100], frequency[1:100])

##Last User Question

##More than 1 fan

users$someFans <- sapply(users$fans,function(a) ifelse(a>1,1,0))
users$someFunnies <- sapply(users$votes$funny, function(a) ifelse(a>1,1,0))

mytable <- table(users$someFans,users$someFunnies)
fisher.test(mytable)

##Does flu shot affect pharmacy reviews?

pharms <- grep("Pharm|Drugs",businesses$categories)
pharmData <- businesses[pharms,]

pharmReviews <- reviews[reviews$business_id %in% pharmData$business_id, ]

pharmReviews$fluShot <- grepl("[Ff]lu [Ss]hot",pharmReviews$text)
pharmReviews$fluShot <- sapply(pharmReviews$fluShot, function(a) ifelse(a,"Flu Shot","No Flu Shot"))
boxplot(stars~fluShot,data=pharmReviews)
t.test(stars~fluShot,data=pharmReviews,alternative="greater")

ttestWord <- function(testWords,bizType,reviews,businesses, verbose){
  require(ggplot2)
  ##Runs TTest on words in reviews for effects
  ##Create Business Subset
  
  testBizData <- businesses[grep(bizType,businesses$categories,ignore.case=T),]
  
  ##Create Word Subset
  
  testWordData <- reviews[reviews$business_id %in% testBizData$business_id,]
  
  testWordData$newCol <- grepl(testWords,testWordData$text,ignore.case=T)
  
  print(paste0(sum(testWordData$newCol,na.rm=T)," reviews found with ",testWords," out of a total ",length(testWordData$newCol)," reviews"))
  
  testWordFoundOnly <- testWordData[testWordData$newCol,]
  
  testWordData$newCol <- sapply(testWordData$newCol, function(a) ifelse(a,paste0(testWords," included"),paste0(testWords," not included")))


  
  ##Run TTest and boxplot
  
  ##boxplot(stars~newCol,data=testWordData)
  sam<-ggplot(testWordData, aes(x=newCol, y=stars, fill=newCol)) + geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=5, size=4)
  sam
  print(t.test(stars~newCol,data=testWordData))
  
  if(verbose){print(testWordFoundOnly$text)}
  return(sam)
}




##Basketball Search

parks <- grep("Park|Playgrou",businesses$categories)
parkData <- businesses[parks,]

basketballTip <- grep("[Bb]asketball|[Bb][Bb]all|[Hh]oop|[Bb]allin",tips$text)
bballTips <- tips[basketballTip,]

parkBallTips <- bballTips[bballTips$business_id %in% parkData$business_id,]

parkBallTips$text


##What words are predictors of good/bad ratings for docotors (or anything) - chairs/germy/drugs/prescription?
##

##Simply fit a model for talking about wines in restaurants - what are the effects of mentioning a type of wine?
##Does price symbols positively correspond with reviews?

##Do happy hours help?

##Catholic - does being Orthodox help??

##Do longer reviews correllate with ratings?

##Good for kids correlate with ratings?

##Build an attribute tester

##Overlapping histograms...


##Do football outcomes have an impact on average yelp scores in the area??

names(reviews)
class(reviews$date)

reviews$date

reviews$date2 <- as.Date(reviews$date,'%Y-%m-%d')

print(max(reviews$date2))
print(min(reviews$date2))
print(class(reviews$date2))

dir()

steelers <- read.csv("SteelersHistory.csv")

##Make a clear date column

steelers$date2 <- paste0(steelers$Date,"/",steelers$Year)
steelers$date3 <- as.Date(steelers$date2,"%m/%d/%Y")
##Make a clear win column

steelers$outcome <- substr(steelers$Result,0,1)
##steelers$homescore <- substr(steelers$Result,)
##Make a clear score column

##Apply wins and losses to reviews in Pittsburg on those dates

names(reviews)
reviews$date2[1:3]

##Match up reviews date 2 and date 3

steelers$outcome[which(steelers$date3 == "2010-09-12")]
steelers$outcome[which(steelers$date3 == "2010-09-22")]

pittbusinesses <- businesses[businesses$city == "Pittsburgh",]
pittbusinesses <- businesses[grep("Bar",businesses$categories),]
pittReviews <- reviews[reviews$business_id %in% pittbusinesses$business_id,]

pittReviews$outcome <- sapply(pittReviews$date2, function(a) as.character(steelers$outcome[which(steelers$date3 == a)]))

pittReviews$outcome <- as.character(pittReviews$outcome)

##Filter out Pittsburg locales


pittStarsByOutcome <- aggregate(stars~outcome,data=pittReviews, FUN=function(a) c(mn=mean(a),tot=ifelse(a>0,1,0)))
ggplot(pittReviews, aes(x=outcome, y=stars, fill=outcome)) + geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=5, size=2)

gameDayReviews <- pittReviews[pittReviews$outcome == "W" | pittReviews$outcome == "L",]
head(gameDayReviews)

##Football keywords in the review

footballReviews <- gameDayReviews[grep("[Ff]ootb|[Pp]igskin|[Ss]teele|the game]",gameDayReviews$text),]

t.test(stars~outcome,data=gameDayReviews,alternative='greater')

##You have to get good at dealing with skeptics, and not taking their attacks or misunderstandings personally.


##Word Count function

library(stringr)

blue <- "the cat is awesome"

wordcount<- function(text){
  
  
  return (length(strsplit(gsub(' {2,}',' ',text),' ')[[1]]))
}

wordcount(blue)



##Which businesses are on Yelp?

businesses$count <- 1
businesses$mainCat <- sapply(businesses$categories,function(a) a[1])

bizByCat <- aggregate(count~mainCat,data=businesses,FUN=sum)
head(bizByCat)

bizByCat <- bizByCat[order(-count),]

print(bizByCat[1:10,])

bikeBiz <- businesses[grep("Bike|Bicycle",businesses$categories),]

print(dim(bikeBiz))

##200 bike business

##Give each review a category

reviews$mainCat <- sapply(reviews$business_id, function(a) businesses$mainCat[which(businesses$business_id == a)])
reviews$reviewCharLength <- nchar(reviews$text)
reviews$reviewWordLength <-  sapply(reviews$text, function(a) wordcount(a))
reviews$aveWordLength <- reviews$reviewCharLength/reviews$reviewWordLength


ggplot(reviews[1:100000,],aes(x=stars,y=aveWordLength))+geom_boxplot(shape=1)

t.test(reviews$aveWordLength[reviews$stars==5],reviews$aveWordLength[reviews$stars==1])

par(mfrow=c(2,2))
hist(reviews$aveWordLength[reviews$stars==1],xlim=c(0,10),breaks=c(0,4,4.5,5,5.5,6,6.5,7,8,9,10100000000000))
hist(reviews$aveWordLength[reviews$stars==5],xlim=c(0,10),breaks=c(0,4,4.5,5,5.5,6,6.5,7,8,9,10100000000000))
