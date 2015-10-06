##Capstone Project Start
suppressMessages(require(rjson))
suppressMessages(require(tm))
require(jsonlite)


##RJSON STYLE
##json_file <- "C:/Users/nswitzer/Documents/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_checkin.json"
##json_data <- fromJSON(json_file)

##jsonlite style

business_file <- ("C:/Users/nswitzer/Documents/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json")
checkin_file <- ("C:/Users/nswitzer/Documents/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_checkin.json")
review_file <- ("C:/Users/nswitzer/Documents/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_review.json")

##According to StackOverflow
##http://stackoverflow.com/questions/26519455/error-parsing-json-file-with-the-jsonlite-package?_sm_au_=iVVWtcSDTcntsVDP

checkins <- fromJSON(sprintf("[%s]", paste(readLines(checkin_file), collapse=",")))
businesses <- fromJSON(sprintf("[%s]", paste(readLines(business_file), collapse=",")))
reviews <- fromJSON(sprintf("[%s]", paste(readLines(review_file,n=150), collapse=",")))

head(checkins)

##Unlist Reviews

df <- data.frame(matrix(unlist(reviews),nrow=150, byrow=T),stringsAsFactors = F)
##names(df) <- names(reviews[[1]])

##Ideas

##When do people play basketball somewhere???
##Which doctors get best reviews?

##Get doctors only
##Categories is list, so we are using GREP, not which

doctors <- grep("Doctor",businesses$categories)
docsData <- businesses[doctors,]

class(docsData$stars)

hist(docsData$stars)
