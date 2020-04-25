#### INSTALLING PACKAGES ####


  install.packages('tm',repos='http://cran.us.r-project.org')
  install.packages('twitteR',repos='http://cran.us.r-project.org')
  install.packages('wordcloud',repos='http://cran.us.r-project.org')
  install.packages('RColorBrewer',repos='http://cran.us.r-project.org')
  install.packages('e1017',repos='http://cran.us.r-project.org')
  install.packages('class',repos='http://cran.us.r-project.org')
  install.packages("SnowballC")

#### INSPECTING THE DOCUMENT #####
#converting the text file into a corpus and inspect a few lines.

    library(SnowballC)
    library("tm")
    text <- readLines("D:\\desktop\\word cloud.txt")
    docs <- Corpus(VectorSource(text))
    length(docs)
    
    inspect(docs[10:15])
    
    
####DATA CLEANING ####
    
    #creating a function 'tospace' to replace specific characters with empty string
    
    toSpace <- content_transformer(function (y , pattern) gsub(pattern, "", y))
 
    #conerting text into lowercase and removing numbers   
    docs = tm_map(docs, tolower)
    docs = tm_map(docs, removeNumbers)
    inspect(docs[10:15])
    
    #removing the time stamp, name of the sender and special charaters
    docs = tm_map(docs, toSpace, "\\//, :\\w{2} -([a-z ]+|[^0-9A-Za-z///']+): ")
    docs = tm_map(docs, toSpace, "[^0-9A-Za-z///' ]+")
    inspect(docs[10:15])
    
    #removing white spaces, common english words and punctuation.
    docs = tm_map(docs, stripWhitespace)
    docs <- tm_map(docs, removeWords, stopwords("english"))
    docs <- tm_map(docs, removePunctuation)
    
    #removing more specific words
    docs <- tm_map(docs, removeWords, c("image", "omitted", "can", "haha", "hahaha", "will", "just", "lol", "one"))
    
   #using to get to a word's root such that "computational", "computation" and "compute" becomes the same root word "comput"
    docs <- tm_map(docs, stemDocument)
    inspect(docs[10:15])
    
  
  # creating a term document matrix by default it counts only cors with are atleast 3 alphatbet long
  # but we can adjust this using control document
  dtm <- TermDocumentMatrix(docs, control = list(wordLengths=c(2, Inf)))

#### PLOTING 30 MOST FREQUENT WORDS ####
  
  library("ggplot2")
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  ggplot(d[1:30,])+
    geom_col(aes(reorder(d$word[1:30],freq), d$freq[1:30]), fill="darkblue")+
    xlab("Words")+
    ylab("Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+coord_flip()+theme(axis.text.x = element_text(angle = 0))

####GENERATING WORD CLOUD ####

  library("wordcloud")
    
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=3000, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))

