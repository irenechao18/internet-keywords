setwd("/Users/irene")
install.packages("XML")
install.packages("RCurl")
install.packages("tm")
link <- NULL
url <- paste0("https://www.ptt.cc/bbs/movie/index", 9511, ".html")
html <- htmlParse(getURL(url))
url

url.list <- xpathSApply(html, "//div[@class='title']/a[@href]", xmlAttrs)
link <- c(link, paste0('https://www.ptt.cc', url.list))

link <- NULL
for( i in 9502:9511){
  url <- paste0("https://www.ptt.cc/bbs/movie/index",i, ".html")
  html <- htmlParse(getURL(url))
  url.list <- xpathSApply(html, "//div[@class='title']/a[@href]", xmlAttrs)
  link <- c(link, paste('https://www.ptt.cc', url.list, sep=''))
}
length(link)

article <- NULL
html <- htmlParse(getURL(link[3]))
doc <- xpathSApply(html,"//div[@id='main-content']", xmlValue)
doc

doc <- removePunctuation(doc)
doc <- gsub("[A-Za-z0-9]", "", doc)
doc <- gsub(" ", "", doc)
doc <- gsub("\n", "", doc)
doc <- gsub("。", "", doc)
doc <- gsub(":", "", doc)
doc <- gsub("，", "", doc)
doc <- gsub("→", "", doc)
doc <- gsub("　 ", "", doc)
doc

for(i in 1:length(link)){
  Sys.sleep(runif(1,1,2))
  html <- htmlParse(getURL(link[i]))
  doc <- xpathSApply(html, "//div[@id='main-content']", xmlValue)
  doc <- removePunctuation(doc)
  doc <- gsub("[A-Za-z0-9]", "", doc)
  doc <- gsub(" ", "", doc)
  doc <- gsub("\n", "", doc)
  doc <- gsub("。", "", doc)
  doc <- gsub(":", "", doc)
  doc <- gsub("，", "", doc)
  doc <- gsub("→", "", doc)
  doc <- gsub("※", "", doc)
  doc <- gsub("　 ", "", doc)
  article[i] <- doc
}

length(article)

install.packages("jiebaR")
seg <- worker()
corpus <- NULL
for(i in 1:length(article)){
  corpus[[i]] <- segment(article[i], seg)
}
corpus[[1]]
edit_dict()
seg <- worker()
corpus <- NULL
for(i in 1:length(article)){
  corpus[[i]] <- segment(article[i], seg)
}

seg <- worker(stop_word="309708011_StopWords.txt")
corpus <- NULL
for(i in 1:length(article)){
  corpus[[i]] <- segment(article[i], seg)
}

union <- NULL
for(i in 1: length(article)){
  union <- c(union, corpus[[i]])
}

corpus[[11]]
bag <- unique(union)
length(union)
length(bag)

tf1 <- matrix(0, nrow=length(article), ncol=length(bag),dimnames=list(NULL,bag))
tf2 <- matrix(0, nrow=length(article), ncol=length(bag),dimnames=list(NULL,bag))
for(i in 1:length(corpus)){
  matchID <- match(corpus[[i]], bag)
  len <- length(matchID)
  for(j in 1:len){
    tf1[i, matchID[j]] <- 1
    tf2[i, matchID[j]] <- (tf2[i, matchID[j]]+1)
  }
}
tf2[1:4,1:10]
colSums(tf1)
colSums(tf2)

idf <- 1+log(nrow(tf1)/colSums(tf1))
idf
tfidf <- tf2
for(word in names(idf)){
  tfidf[,word]<-tfidf[,word]*idf[word]
}
tfidf[1:10,1:10]

tf_count <- colSums(tf2)
tf_count[1:5]
tf_count <- sort(tf_count, decreasing = TRUE)
tf_count[2:6]


cor_term <- cor(tf2)
cor_guardian <- cor_term["母親",]
cor_guardian <- sort(cor_guardian, decreasing = TRUE)
cor_guardian

cor_term <- cor(tf2)
cor_guardian <- cor_term["女兒",]
cor_guardian <- sort(cor_guardian, decreasing = TRUE)
cor_guardian

cor_term <- cor(tf2)
cor_guardian <- cor_term["玩命",]
cor_guardian <- sort(cor_guardian, decreasing = TRUE)
cor_guardian

cor_term <- cor(tf2)
cor_guardian <- cor_term["上映",]
cor_guardian <- sort(cor_guardian, decreasing = TRUE)
cor_guardian

cor_term <- cor(tf2)
cor_guardian <- cor_term["宇宙",]
cor_guardian <- sort(cor_guardian, decreasing = TRUE)
cor_guardian