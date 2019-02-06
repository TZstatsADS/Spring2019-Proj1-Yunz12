# ADS Project 1: What made you happy today?
### Code lib Folder

The lib directory contains various files with function definitions (but only function definitions - no code that actually runs).

number_words <- cleaned %>%  
  unnest_tokens(word, cleaned_hm) %>%  
  group_by(hmid) %>%  
  summarise(words_number = n()) %>%  
  arrange(desc(words_number))  
  
demographic <- demographic %>%  
  mutate(agestage =   
           ifelse(demographic$age %in% 11:25, "Teenager",   
           ifelse(demographic$age %in% 26:40, "Young adult",   
           ifelse(demographic$age %in% 41:65, "Middle aged",   
           ifelse(demographic$age %in% 66:90, "Old aged",   
           "NA")))))  

agestage_name=unique(cleaned_2$agestage)  
age_word=NULL  
for (age in agestage_name) {  
  data=as.vector(cleaned_2[which(cleaned_2$agestage==age),"cleaned_hm"])  
  word=Corpus(VectorSource(data))    
  ff.all1<-tm_map(word, stripWhitespace)   
  ff.all1<-tm_map(ff.all1, content_transformer(tolower))   
  ff.all1<-tm_map(ff.all1, removeWords, stopwords("english"))   
  ff.all1<-tm_map(ff.all1, removeWords, character(0))   
  ff.all1<-tm_map(ff.all1, removePunctuation)   
  ff.all1 <- tm_map(ff.all1, removeWords, limit_words)   
  tdm.all1<-TermDocumentMatrix(ff.all1)   
  tdm.tidy1=tidy(tdm.all1)   
  tdm.overall1=as.data.frame(summarise(group_by(tdm.tidy1, term), sum(count)))   
  tdm.overall1=tdm.overall1[order(tdm.overall1[,"sum(count)"],decreasing = TRUE),]   
  age_top_word=cbind(tdm.overall1[1:5,],rep(age,5))   
  age_word=rbind(age_word,age_top_word)   
}   
