# Load
library("tm") # for text mining
library("SnowballC") # for text stemming
library("wordcloud") # word-cloud generator 
library("RColorBrewer") # color palettes
library("wordcloud2") #word cloud 2 generator


# file reading
filePath <- "https://www.gutenberg.org/cache/epub/28000/pg28000.txt"
text <- readLines(filePath)



# Load data as a corpus
docs <- Corpus(VectorSource(text))

inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Text cleaning 

# Ttext to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Removing numbers
docs <- tm_map(docs, removeNumbers)
# Removing english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Removing  own stop word
# specify stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Removing punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminating extra white spaces
docs <- tm_map(docs, stripWhitespace)


# Text stemming
# docs <- tm_map(docs, stemDocument)

# Creating Term Document Matrix

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Creating Word Cloud2!

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

wordcloud2(data=d, shape = "cadio")
