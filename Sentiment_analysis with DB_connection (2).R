library(RODBC)
library(tm)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(wordcloud)
library(SnowballC)
library(sentiment)
con <- odbcConnect("SAIRAMANA", uid="SAIRAMANA", pwd="sairamana", rows_at_time = 500)
d <- sqlQuery(con, "select * from sentiment_fact")
write.csv(d,"sentiment.csv")
close(con)

#Importing records into R

reviews<-read.csv("sentiment.csv",stringsAsFactors = FALSE)

#cleansing of data. 
#Creating vector for pulling information
mywords <- Corpus(VectorSource(reviews$text))

#Converting the information into Plain text
mywords<-tm_map(mywords,PlainTextDocument)

#Removing Punctuations
mywords<-tm_map(mywords,removePunctuation)

#Removing Stopwords
mywords<-tm_map(mywords,removeWords,stopwords(kind="en"))

#stemming the words.
mywords<-tm_map(mywords,stemDocument)

#Creating Wordcloud
pallet <- brewer.pal(8,"Dark2")
#png("LG.png", width=1280,height=800)
wordcloud(mywords,scale=c(8,.3),min.freq=2,max.words=100,random.order=T, rot.per=.15, colors=pallet, vfont=c("sans serif","plain"))
#wordcloud(mywords,random.order =TRUE,colors=pallet) 
#wordcloud(words[1:100], frequency[1:100])


some_text<-read.csv("sentiment.csv",stringsAsFactors = FALSE)

#classify emotion
class_emo = classify_emotion(some_text,algorithm="bayes",prior=1.0)

#get emotion best fit
emotion =class_emo[,7]

#substitute NA by 'unknown' 
emotion[is.na(emotion)]="unknown"

#classify polarity
class_pol = classify_polarity(some_text,algorithm="bayes")


#get polarity best fit
polarity =class_pol[,4]


#data frame with reslts
sent_df=data.frame(text=some_text,emotion=emotion,polarity=polarity,stringAsFactors=FALSE)

#sort data frame
sent_df1=within(sent_df,emotion<-factor(emotion,levels=names(sort(table(emotion),decreasing=TRUE))))

# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of Reviews",title="Sentiment Analysis")

# plot distribution of polarity
ggplot(sent_df1, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of reviews",title="Landmarkgroup Review")








