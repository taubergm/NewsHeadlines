# word blobs

if (!require(wordcloud)) {
  install.packages("wordcloud", repos="http://cran.us.r-project.org")
}
if (!require(tm)) {
  install.packages("tm", repos="http://cran.us.r-project.org")
}
if (!require(slam)) {
  install.packages("slam", repos="http://cran.us.r-project.org")
}
if (!require(SnowballC)) {
  install.packages("SnowballC", repos="http://cran.us.r-project.org")
}
if (!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cran.us.r-project.org")
}

library(slam)
library(tm)
library(wordcloud)
library(SnowballC)
library(ggplot2)

workingDir = '/Users/michaeltauberg/tmz/NewsScraper'
csvName = "headlines_all2.csv"
data_name = "headlines"
setwd(workingDir)
#options(encoding="latin1")
#dt = read.csv(file=csvName, stringsAsFactors=FALSE, fileEncoding="utf-8")
dt = read.csv(file=csvName)

# remove generic headlines
dt = dt[dt$headline != "FoxNews.com - Breaking News",]

# fix characters
dt$headline = gsub(": NPR", " ", dt$headline)
dt$headline = gsub("Two-Way ", " ", dt$headline)
dt$headline = gsub("\\(", " ", dt$headline)
dt$headline = gsub("\\)", " ", dt$headline)
dt$headline = gsub("\\'", " ", dt$headline)
dt$headline = gsub(":", " ", dt$headline)
dt$headline = gsub("- 360° video ", " ", dt$headline)
dt$headline = gsub("- video", " ", dt$headline)
dt$headline = gsub("–", " ", dt$headline)
dt$headline = gsub("—", " ", dt$headline)
dt$headline = gsub("-", " ", dt$headline)
dt$headline = gsub("foxnews", " ", dt$headline)
dt$headline = gsub("npr", " ", dt$headline)
dt$headline = gsub("error", " ", dt$headline)
dt$headline = gsub("‘", " ", dt$headline)
dt$headline = gsub("’", " ", dt$headline)

dt = dt[!duplicated(dt[,c('link')], fromLast=FALSE),]

# separate sources
fox = dt[grep("foxnews.com", dt$link),]
breitbart = dt[grep("breitbart.com", dt$link),] 
cnn = dt[grep("cnn.com", dt$link),] 
infowars = dt[grep("infowars.com", dt$link),]
bbc = dt[grep("bbc.co", dt$link),]
guardian = dt[grep("guardian.com", dt$link),]
msnbc = dt[grep("msnbc.com", dt$link),]
npr = dt[grep("npr.org", dt$link),]
wapo = dt[grep("washingtonpost.com", dt$link),]

# word clouds
GenerateWordClouds <- function(dt_uniq, data_name, color) {
  words = Corpus(VectorSource(dt_uniq$headline)) 
  corpus <- tm_map(words, content_transformer(tolower))
  
  words = tm_map(words, stripWhitespace)
  words = tm_map(words, tolower)
  
  #badwords = c("the", "and", "a")
  #words = tm_map(words, removeWords, badwords)
  #png(sprintf("%s_simple_wordcloud.png", data_name))
  #wordcloud(words, max.words = 120, random.order=FALSE, colors=brewer.pal(nrow(dt_uniq),"Dark2"))
  #dev.off()
  
  complete_stopwords = c(stopwords('english'), "watch", "breaking", "opinion", "say", "says", "news", "new", "can", "error")
  complete_stopwords = c(complete_stopwords, "com", "bbc", "video", "npr", "foxnews", "two-way", "-", "twoway", "explainer", "—")
  # Generate wordcloud removing all stop words
  png(sprintf("%s_stopwords_wordcloud.png", data_name))
  words = tm_map(words, removeWords, complete_stopwords)
  wordcloud(words, max.words = 75, random.order=FALSE, colors=brewer.pal(8,"Dark2"))
  
  dev.off()
  
  dtm = TermDocumentMatrix(words)
  m = as.matrix(dtm)
  v = sort(rowSums(m),decreasing=TRUE)
  d = data.frame(word = rownames(m), 
                        freq = rowSums(m), 
                        row.names = NULL)
  #d = data.frame(word = names(v),freq=v)
  d = d[order(d$freq, decreasing=TRUE),]
  d$word = factor(d$word, levels = d$word[order(d$freq, decreasing=TRUE)])
  
  top_words = d[1:5,]
  p = ggplot(top_words, aes(x=word, y=freq, fill=data_name)) + geom_bar(stat="identity") 
  p = p + ggtitle(sprintf("%s - Top Words", toupper(data_name)))
  p = p + theme(plot.title = element_text(hjust = 0.5))
  p = p + theme(axis.text.x=element_text(angle=90, hjust=1,face="bold"))
  p = p + theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=13), axis.title.x=element_blank())
  p = p + theme(plot.title = element_text(size=18,face="bold"))
  #p = p + xlab("Word")
  p = p + scale_fill_manual(values = c(color)) + guides(fill=FALSE)
  p = p + ylab("Number of Uses") 
  
  #p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
  ggsave(filename = sprintf("./%s_top5.png", data_name) , plot=p, width=4.5, height=6)
}

GenerateWordClouds(cnn, "cnn", '#F9D036')
GenerateWordClouds(fox, "fox", 'red')
GenerateWordClouds(breitbart, "breitbart", '#F15208')
GenerateWordClouds(infowars, "infowars", 'blue')
GenerateWordClouds(bbc, "bbc", 'black')
GenerateWordClouds(guardian, "guardian", 'blue')
GenerateWordClouds(msnbc, "msnbc", 'blue')
GenerateWordClouds(npr, "npr", 'orange')
GenerateWordClouds(wapo, "wapo", 'grey')


# sentiment scores
# why the news is so boring (and depressing)
if (!require(syuzhet)) {
  install.packages("syuzhet", repos="http://cran.us.r-project.org")
}
library(syuzhet)

# By individual word
cnn_a_v <- get_tokens(cnn$headline, pattern = "\\W")
cnn_word_vector <- get_sentiment(cnn_a_v, method="afinn")
cnn_word_avg = sum(cnn_word_vector)/length(cnn_word_vector)

fox_a_v <- get_tokens(fox$headline, pattern = "\\W")
fox_word_vector <- get_sentiment(fox_a_v, method="afinn")
fox_word_avg = sum(fox_word_vector)/length(fox_word_vector)

msnbc_a_v <- get_tokens(msnbc$headline, pattern = "\\W")
msnbc_word_vector <- get_sentiment(msnbc_a_v, method="afinn")
msnbc_word_avg = sum(msnbc_word_vector)/length(msnbc_word_vector)

bbc_a_v <- get_tokens(bbc$headline, pattern = "\\W")
bbc_word_vector <- get_sentiment(bbc_a_v, method="afinn")
bbc_word_avg = sum(bbc_word_vector)/length(bbc_word_vector)

guardian_a_v <- get_tokens(guardian$headline, pattern = "\\W")
guardian_word_vector <- get_sentiment(guardian_a_v, method="afinn")
guardian_word_avg = sum(guardian_word_vector)/length(guardian_word_vector)

npr_a_v <- get_tokens(npr$headline, pattern = "\\W")
npr_word_vector <- get_sentiment(npr_a_v, method="afinn")
npr_word_avg = sum(npr_word_vector)/length(npr_word_vector)

wapo_a_v <- get_tokens(wapo$headline, pattern = "\\W")
wapo_word_vector <- get_sentiment(wapo_a_v, method="afinn")
wapo_word_avg = sum(wapo_word_vector)/length(wapo_word_vector)

breitbart_a_v <- get_tokens(breitbart$headline, pattern = "\\W")
breitbart_word_vector <- get_sentiment(breitbart_a_v, method="afinn")
breitbart_word_avg = sum(breitbart_word_vector)/length(breitbart_word_vector)

infowars_a_v <- get_tokens(infowars$headline, pattern = "\\W")
infowars_word_vector <- get_sentiment(infowars_a_v, method="afinn")
infowars_word_avg = sum(infowars_word_vector)/length(infowars_word_vector)

# plot word averages
names = c("cnn","fox","msnbc","bbc","guardian","npr","wapo", "breitbart", "infowars")
sentiments = c(cnn_word_avg,fox_word_avg,msnbc_word_avg,bbc_word_avg,guardian_word_avg,npr_word_avg,wapo_word_avg,breitbart_word_avg,infowars_word_avg)
word_sentiments = data.frame(cbind(names,sentiments))
word_sentiments$sentiments = as.numeric(sentiments)
word_sentiments$names = factor(word_sentiments$names, levels = word_sentiments$names[order(word_sentiments$sentiments, decreasing=TRUE)])
word_sentiments = word_sentiments[order(word_sentiments$sentiments, decreasing=TRUE),]
p = ggplot(word_sentiments, aes(x=names, y=sentiments, fill=names)) + geom_bar(stat="identity") 
p = p + ggtitle("Sentiment by News Source (Based on Words)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("News Source") + ylab("Average Sentiment Score") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = "./word_sentiment.png", plot=p, width=6, height=5)

# By sentence
cnn_s_v <- get_sentences(cnn$headline)
cnn_afinn_vector <- get_sentiment(cnn$headline, method="afinn")
cnn_syuzhet_vector <- get_sentiment(cnn$headline, method="syuzhet")
cnn_bing_vector <- get_sentiment(cnn$headline, method="bing")
cnn_nrc_vector <- get_sentiment(cnn$headline, method="nrc", lang = "english")

# use sentimentr library
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sentimentr, dplyr, magrittr)
library('sentimentr')

cnn_sent = sentiment(cnn$headline)
cnn_sent_avg = sum(cnn_sent$sentiment)/nrow(cnn_sent)

fox_sent = sentiment(fox$headline)
fox_sent_avg = sum(fox_sent$sentiment)/nrow(fox_sent)

msnbc_sent = sentiment(msnbc$headline)
msnbc_sent_avg = sum(msnbc_sent$sentiment)/nrow(msnbc_sent)

breitbart_sent = sentiment(breitbart$headline)
breitbart_sent_avg = sum(breitbart_sent$sentiment)/nrow(breitbart_sent)

infowars_sent = sentiment(infowars$headline)
infowars_sent_avg = sum(infowars_sent$sentiment)/nrow(infowars_sent)

wapo_sent = sentiment(wapo$headline)
wapo_sent_avg = sum(wapo_sent$sentiment)/nrow(wapo_sent)

npr_sent = sentiment(npr$headline)
npr_sent_avg = sum(npr_sent$sentiment)/nrow(npr_sent)

guardian_sent = sentiment(guardian$headline)
guardian_sent_avg = sum(guardian_sent$sentiment)/nrow(guardian_sent)

bbc_sent = sentiment(bbc$headline)
bbc_sent_avg = sum(bbc_sent$sentiment)/nrow(bbc_sent)

# plot all averages to show the most negative
names = c("cnn","fox","msnbc","bbc","guardian","npr","wapo", "breitbart", "infowars")
sentiments = c(cnn_sent_avg,fox_sent_avg,msnbc_sent_avg,bbc_sent_avg,guardian_sent_avg,npr_sent_avg,wapo_sent_avg,breitbart_sent_avg,infowars_sent_avg)
sent_sentiments = data.frame(cbind(names,sentiments))
sent_sentiments$sentiments = as.numeric(sentiments)
sent_sentiments = sent_sentiments[order(sent_sentiments$sentiments, decreasing=TRUE),]
sent_sentiments$names = factor(sent_sentiments$names, levels=sent_sentiments$names[order(sent_sentiments$sentiments, decreasing=TRUE)])
p = ggplot(sent_sentiments, aes(x=names, y=sentiments, fill=names)) + geom_bar(stat="identity") 
p = p + ggtitle("Headline Sentiment by News Source")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("News Source") + ylab("Average Sentiment Score") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = "./headline_sentiment.png", plot=p, width=6, height=5)