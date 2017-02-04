library(tm)

# PATH OF THE FOLDER CONTAINING THE INPUT FILES NEED TO BE SET BELOW
# E.G. filenames <- list.files("C:/Users/XYZ/Documents/rit-challenge-master/rit-challenge-master/transaction-data", pattern="*.csv", full.names=TRUE)

filenames <- list.files("     INPUT PATH GOES HERE----SHOULD CONTAIN THE .CSV FILES     ", pattern="*.csv", full.names=TRUE)

# Function that calculates the frequently occuring words and generates a wordcloud
# The images are stored in a output folder

calculate <- function(filename, id){

# read the .csv file
user = read.csv(filename)

# create a corpus out of the Vendor field so text mining can be performed
corp <- Corpus(VectorSource(user$Vendor))
vendortext <- tm_map(corp, PlainTextDocument)

# creating a content transformer to replace any given pattern to spaces
insertSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
vendortext <- tm_map(vendortext, insertSpace, "-")
vendortext <- tm_map(vendortext, insertSpace, ":")

# use inbuilt content transformers to remove unnecessary content
vendortext <- tm_map(vendortext, removePunctuation)
vendortext <- tm_map(vendortext, content_transformer(tolower))
vendortext <- tm_map(vendortext, removeNumbers)
vendortext <- tm_map(vendortext, removeWords, stopwords("english"))
# removing most occuring and unnecessay content
vendortext <- tm_map(vendortext, removeWords, c("amazon", "order" , "public" , "transportation", "store", "sprouts",
                                                "center", "supplies", "grocery", "delivery", "pass", "bus", "groceries", "whole",
                                                "ticket", "online", "membership", "subscription", "demand"))
vendortext <- tm_map(vendortext, stripWhitespace)

# creating a document term matrix which limits the word length between 5 and 15
doc_term_mat <- DocumentTermMatrix(vendortext, control = list(wordLengths = c(5, 15)))

# colsums calculates the sum of columns which gives the count of the occurence of each word
occurence <- colSums(as.matrix(doc_term_mat))

library(wordcloud)
library(RColorBrewer)

# path where the output files are stored
# E.G. out <- paste('C:/Users/XYZ/Documents/rit-challenge-master/rit-challenge-master/Output/user_', id , '.jpeg', sep='')
# path goes before '/user_'
out <- paste('       OUTPUT PATH GOES HERE/user_', id , '.jpeg', sep='')

# used to store the image to the provided location
jpeg(file = out)

# create a wordcloud of words whose frequency is greater than 40
wordcloud(names(occurence), occurence, min.freq = 40, max.words = 75, colors = brewer.pal(8, "Dark2"))
dev.off()
}

# loop that iterates through all the files in the input folder and runs the function to create the wordcloud
for (id in c(1:99)){
  calculate(filenames[id], id)
}
