##Capstone Project Start
##suppressMessages(require(rjson))
##suppressMessages(require(tm))
require(jsonlite)
require(ggplot2)


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