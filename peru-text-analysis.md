---
title: "peru-text-analysis"
author: "Ayse D Lokmanoglu"
date: "2022-10-14"
output: html_document
---


```{r setup, }
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 
knitr::opts_chunk$set(eval = FALSE)
```
## Supplemental Code for: Social Media Amplification of Risk about COVID-19 Vaccination and Vaccine Acceptance Among Peruvian Social Media Users: A Longitudinal Time Series Analysis                             
### Authors: 
#### Ayse D. Lokmanoglu*, 
#### Erik C. Nisbet,      
#### Matthew T. Osborne, 
#### Joseph Tien, 
#### Sam Malloy,       
#### Lourdes Cueva Chacón, 
#### Esteban Villa Turek,         
#### Rod Abhari            

The code includes all the steps of the text, and regression analysis, and the visualizations. 

The data set accompanying the code: <>

For questions, or more information on the code please contact: 
Ayse D. Lokmanoglu\
ayse [dot] lokmanoglu [at] northwestern [dot] edu

### Text Analysis
```{r}
library(quanteda) # for text analysis
library(topicmodels) # running topic modeling
library(ldatuning) # choosing K
library(tidyverse) # tidy
library(lubridate) # dealing with dates
library(tidytext) # text analysis
```

Load TSV files and combine 
```{r, eval=FALSE}
test<-read.csv("DATA/peru_tweets_7_12_2021_8_9_2021.tsv", sep = "\t")
test2<-read.csv("DATA/peru_tweets_12_21_20_6_3_21.tsv", sep = "\t")
test3<-read.csv("DATA/peru_6_3_21_7_12_21.tsv", sep = "\t")

test2a<-test2 %>% select(-day) ##### data set 2 had an extra column day so dropped that #####

##### merge the three data frames by column ######
test1and2<-full_join(test, test2a)
testall <-full_join(test1and2, test3)

#### join with community names ####
testall2<-testall
testall2<-left_join(testall2, peru_user_community, copy=TRUE, by=c("username"))

###resave as testall and remove testall2 ####
testall<-testall2
rm(testall2)

####remove test, test 2, test2a, test 3 and test1and2#######
rm(test)
rm(test2)
rm(test2a)
rm(test3)
rm(test1and2)

#### save combined as a R file #######
save(testall,file="perutweetdata.Rda")
````
Check the data, and the select the necessary columns
```{r}
##### select tweet_id, date, text for cleaning up ###########
test1 <- testall %>% select(tweet_id, created_at, text, is_rt, community) 
### rename for date, and use lubridate package to save as date #####
test1<- test1%>% rename(date = created_at)
test1$date<-date(test1$date)

## add an index
test1$index<-seq(1,nrow(test1))
```
Check the language composition of the file using [textcat package](https://cran.r-project.org/web/packages/textcat/textcat.pdf)
```{r}
lang<-textcat(test1$text)
lang2<-as.data.frame(lang)
glimpse(lang2)

languages<-lang2 %>% count(lang) #count
languages<- languages %>%
  mutate(Percentage=paste0(round(n/sum(n)*100,2),"%")) #get percentages
```
```{r, eval=TRUE, echo=FALSE}
languages_df <- data.frame (Language  = c("Spanish", "English", "Other"),
                  Percentage = c("78.41%", "18.07% ", "3.52% ")
                  )
knitr::kable(languages_df,
caption = "Peru Corpus Language Composition")
```
Run a word frequency to identify problematic words
```{r}
###### do a simple word frequency table #####
textdf<- testsearch %>%
  unnest_tokens(word, text)

stopwords_and_single<-c(stopwords("spanish"),stopwords("english")) # stopwords list in Spanish and English

stopwords<-data.frame(stopwords_and_single)
names(stopwords)[1]<-"word"

textdf2<-text_frequency %>% anti_join(stopwords)

write.csv(textdf2, "RESULTS/text frequency.csv")
```
Run a text check package on a sample
```{r}
testsample1perc <- test1 %>% sample_frac(.01)

Sys.time()
textclean::check_text(testsample1perc$text)
Sys.time()
```
After identfying what needs to be cleaned, preprocess the data
```{r}
test1$text <- tolower(test1$text) #lower text
test1$text<-gsub("\\B@\\S+"," ",test1$text) # remove all the names tags so anything after @ sign
test1$text<-gsub("\\\\b"," ",test1$text)
test1$text<-gsub("(.*)(https)"," ",test1$text) # remove links
test1$text<-gsub("\\tc\\w*)"," ",test1$text) # remove twitter remnants
test1$text<-gsub("\\aa\\w*)"," ",test1$text)
test1$text<-gsub("rt"," ",test1$text) # remove twitter remnants
test1$text<-removeNumbers(test1$text)
test1$text<-removePunctuation(test1$text)

###save test1 #######
save(test1,file="test1.Rda")
```
Start LDA Analysis
- Tokenize the corpus
```{r}

stopwords_and_single<-c(stopwords("spanish"),stopwords("english"),LETTERS,letters, 
                        "brt", "bla", "nhttps", "https", "rt", "http", "tongue", "sticking",
                        "nn", "pa", "si", "ser", "tco", "keiko") # additional stopwords from word frequency 


toks <- tokens(test1$text,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_numbers = TRUE,
               remove_url = TRUE,
               remove_separators = TRUE,
               split_hyphens = FALSE,
               include_docvars = TRUE,
               padding = FALSE) %>%
  tokens_remove(c(stopwords("spanish"),stopwords("english"),LETTERS,letters, 
                  "brt", "bla", "nhttps", "https", "rt", "http", "nnhttps", "skeptical", "tongue", "sticking", "tcowbuapncc", "laughing", ""), padding = FALSE) %>%
  tokens_select(min_nchar = 4) # remove all words less than 4 characters
```
- Change it into a [document-feature matrix](https://quanteda.io/reference/dfm.html)
```{r}
dfm_counts<- dfm(toks) 
rm(toks) #remove unused files to save space
```
- Match your dfm object with your original data frame through index
```{r}
docnames(dfm_counts)<-test1$index
```
- Check for sparsity and trim accordingly
```{r}
sparsity(dfm_counts)
dfm_counts2<-dfm_trim(dfm_counts, max_docfreq = 0.95, min_docfreq=0.05,docfreq_type="prop")
sparsity(dfm_counts2)
rm(dfm_counts) #remove for space
```
- Convert dfm object to an LDA object
```{r}
dtm_lda <- convert(dfm_counts2, to = "topicmodels",docvars = dfm_counts2@docvars)
n <- nrow(dtm_lda) #number of rows fir future
```
-Choose optimal topic number **k** using [ldatuning package](https://cran.r-project.org/web/packages/ldatuning/index.html) `r FindTopicsNumber()`
```{r}
parallel::detectCores() # first detect your cores
```
*If during the process you get "out of memory" you can asign more RAM to drive but will be much slower when using this `r `memory.limit(16000)`*
```{r}
myalpha=0.1 # set your alpha

# Start running, record time for future
Sys.time()
result <- FindTopicsNumber(
  dtm_lda,
  topics = seq(2,70,by=5), # Specify how many topics you want to try.
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(alpha=myalpha, seed = 8),
  mc.cores = 38L,
  verbose = TRUE
)
Sys.time()
save(result, file="FindK_Result.Rda")
```
- Plot the results and choose candidate models
```{r}
FindTopicsNumber_plot(result) 
ggsave("~/AMISS Data/PeruTweets/PeruProject/Visuals/FigureS1_1_FindK_061722.tiff", width=8.5, height=5, dpi=300)
```
![Ldatuning Plot](https://github.com/nwccpp/peru_sarf/blob/main/images/Supplement_findk.png?raw=true)
- Both models for k=45 and k=65 were analyzed. The researched agreed that k=65 was the better model
```{r}
Sys.time()
lda <- LDA(dtm_lda,65,
           method = "Gibbs",
           control = list(alpha=0.05,seed=125231)) 
save(lda, file="Peru_k65_lda.Rda")
Sys.time()
```
Extract top words and texts
- Extract top words for each document
```{r}
datacolnum=which( colnames(test1)=="text") #mark text columns location in original data

myw=0.3
word_beta_sums<-rowSums(mybeta)
my_beta_for_frex<-mybeta
for (m in 1:ncol(my_beta_for_frex)) {
  for (n in 1:nrow(my_beta_for_frex)) {
    my_beta_for_frex[n,m]<-1/(myw/(my_beta_for_frex[n,m]/word_beta_sums[n])+((1-myw)/my_beta_for_frex[n,m]))
  }
  print (m)
}
nwords=100
topwords <- my_beta_for_frex[1:nwords,]
for (i in 1:LDAfit@k) {
  tempframe <- my_beta_for_frex[order(-my_beta_for_frex[,i]),]
  tempframe <- tempframe[1:nwords,]
  tempvec<-as.vector(rownames(tempframe))
  topwords[,i]<-tempvec
}
rownames(topwords)<-c(1:nwords)
topwords<-data.frame(topwords)
openxlsx::write.xlsx(topwords, file="Peru_k65_topwords_DATE.xlsx")
```
- Top tweets
```{r}
metadf<-test1  #your original dataframe
meta_theta_df<-cbind(metadf[,"text"],LDAfit@gamma)
ntext=30
toptexts <- mybeta[1:ntext,]
for (i in 1:LDAfit@k) {
  print(i)
  tempframe <- meta_theta_df[order(-as.numeric(meta_theta_df[,i+1])),]
  tempframe <- tempframe[1:ntext,]
  tempvec<-as.vector(tempframe[,1])
  toptexts[,i]<-tempvec
}
rownames(toptexts)<-c(1:ntext)
toptexts<-data.frame(toptexts)
openxlsx::write.xlsx(toptexts, file="Peru_k65_toptweets_DATE.xlsx")
```
- After researchers reached an agreement in clusters, combine clusters
```{r}
temp<- themebyday_k65 %>% mutate(covid_vac= X2+ X28 + X29 + X36 + X64) %>% 
  mutate(covid_nonpharma= X49 + X54 + X65) %>%
  mutate(covid_health = X30 + X55) %>%
  mutate(covid_econ = X3+X32 +X47) %>%
  mutate(covid_variant = X60) %>%
  mutate(covid_other = X10 + X19 + X26 + X33 + X40 + X42)  %>%
  mutate(peruvian_pol_gen = X8 + X12 + X16 + X34 + X35 + X45) %>%
  mutate(peruvian_pol_el = X1 + X5 + X43 + X50) %>%
  mutate(regional_protest = X62) %>%
  mutate(venezuela = X56 + X63) %>%
  mutate(international = X9+ X13 + X15 + X18 + X20 + X25 + X27 + X39 +
           X51 + X52 + X53 + X59 + X61) %>%
  mutate(other = X4 + X6 + X7 + X44 + X46 + X48 + X58 + X11 + X14 + 
           X17 + X21 + X22 + X23 + X24 + X31 + X37 + X38 + X41)
```
Sentiment Analysis
- We use the [NRC Word Lexicon](https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm) @Mohammad
```{r}
stopwords<-c(stopwords("spanish"),stopwords("english"),
             LETTERS,letters, 
             "brt", "bla", "nhttps", "https", "rt", 
             "http", "nnhttps", "tongue", 
             "sticking", "nn", "keiko", "smiley")
stopwords<-data.frame(stopwords)
names(stopwords)[1]<-"word"

temp_test<-temp2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stopwords) %>% # remove stopwords by antijoin
  inner_join(nrc) %>% # add sentiment
  group_by(index) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill=0) 

temp_test3<-inner_join(temp2, temp_test, by="index") # combine using the index
temp4<-temp_test3 #create a backup 
temp4<-temp4 %>% mutate(sentiment=positive-negative) #calculate net sentiment
save(temp4,file="FV_Peru_K65_Clusters_Sentiment_DATE.Rda")
```
Start calculating topic-sentiment scores for negative, positive, trust and net sentiment
```{r}
negative_k65<-select(temp_test3, covid_vac,
                     covid_nonpharma, covid_health, covid_econ, covid_variant, covid_other)
colnames(negative_k65) <- paste("negative", colnames(negative_k65), sep = "_")

for (i in 1:ncol(negative_k65)) {
  negative_k65[ , i] <- negative_k65[ , i] * temp_test3$negative
}
temp_test3<-cbind(temp_test3, negative_k65)
tempneg<-cbind(temp_test3[,"date"],negative_k65)
colnames(tempneg)[1]<-"date"


###positive
positive_k65<-select(temp_test3, covid_vac, covid_nonpharma, covid_health, covid_econ, covid_variant, covid_other)
colnames(positive_k65) <- paste("positive", colnames(positive_k65), sep = "_")

for (i in 1:ncol(positive_k65)) {
  positive_k65[ , i] <- positive_k65[ , i] * temp_test3$positive
}
temp_test3<-cbind(temp_test3, positive_k65)
temppositive<-select(temp_test3, date, positive_covid_vac, positive_covid_nonpharma, positive_covid_health, positive_covid_econ, positive_covid_variant, positive_covid_other)


###trust
trust_k65<-select(temp_test3, covid_vac, covid_nonpharma, covid_health, covid_econ, covid_variant, covid_other)
colnames(trust_k65) <- paste("trust", colnames(trust_k65), sep = "_")

for (i in 1:ncol(trust_k65)) {
  trust_k65[ , i] <- trust_k65[ , i] * temp_test3$trust
}
temp_test3<-cbind(temp_test3, trust_k65)
temptrust<-select(temp_test3, date, trust_covid_vac, trust_covid_nonpharma, trust_covid_health, trust_covid_econ, trust_covid_variant, trust_covid_other)

#####sentiment
sent_k65<-select(temp4, covid_vac, covid_nonpharma, 
                 covid_health, covid_econ, covid_variant, covid_other)
colnames(sent_k65) <- paste("sentiment", colnames(sent_k65), sep = "_")

for (i in 1:ncol(sent_k65)) {
  sent_k65[ , i] <- sent_k65[ , i] * temp4$sentiment
}

temp_test3<-cbind(temp4, sent_k65)
tempsent<-cbind(temp4[,"date"],sent_k65)
colnames(tempsent)[1]<-"date"

###save the master file 
save(temp_test3,file="FV_Peru_Unsupervised_Clusters_Sentiment_DATE.Rda")
```
Creat daily DV and IV's
```{r}
temp <-temp_test3 %>% 
   dplyr::select(date, index, 
                sentiment_covid_vac,
                trust_covid_vac)

temp_by_day<-aggregate(x=temp[,2:ncol(temp)],by=list(temp$date),FUN="mean")
temp_by_day<- temp_by_day %>%
  dplyr::select(-index)%>%
  rename('date' = 'Group.1')
```
Load [The UMD Global COVID-19 Trends and Impact Survey](https://gisumd.github.io/COVID-19-API-Documentation/), change column names and make date an date object 
- Indicator appointment_or_accept_covid_vaccine (V15a, V3a)
```{r}
UMDIndicatorsPeru_pct_vu_061722 <- read_csv("PeruTweets/PeruProject/CSV/UMDIndicatorsPeru_pct_vu_061722.csv", 
                                            col_types = cols(date = col_date(format = "%m/%d/%Y")))
Peru_UMD_pct_vu <- UMDIndicatorsPeru_pct_vu_061722 %>%
  select(date, percent_vu, sample_size) %>%
  rename('pct_vu' = 'percent_vu',
         'sample_size_pct_vu' = 'sample_size') %>%
  mutate(date = ymd(date))
```
- For robustness checks load indicators V3a and V15a
```{r}
UMDIndicatorsPeru_v3a_101422 <- read_csv("PeruTweets/PeruProject/CSV/UMDIndicatorsPeru_v3a_101422.csv")
Peru_UMD_v3a <- UMDIndicatorsPeru_v3a_101422 %>%
  select(date, pct_accept_covid_vaccine_no_appointment, sample_size) %>%
  rename('v3a' = 'pct_accept_covid_vaccine_no_appointment',
         'sample_size_v3a' = 'sample_size') %>%
  mutate(date = ymd(date))

UMDIndicatorsPeru_v15a_101422 <- read_csv("PeruTweets/PeruProject/CSV/UMDIndicatorsPeru_v15a_101422.csv")
Peru_UMD_v15a <- UMDIndicatorsPeru_v15a_101422 %>%
  select(date, pct_appointment_not_vaccinated, sample_size) %>%
  rename('v15aa' = 'pct_appointment_not_vaccinated',
         'sample_size_v15a' = 'sample_size') %>%
  mutate(date = ymd(date))

temp_robust <- full_join(Peru_UMD_v3a, Peru_UMD_v15a, by="date")
rm(UMDIndicatorsPeru_v15a_101422)
rm(UMDIndicatorsPeru_v3a_101422)
rm(Peru_UMD_v15a)
rm(Peru_UMD_v3a)
```
Combine IV and DVs daily
```{r}
temp<-left_join(temp_by_day, Peru_UMD_pct_vu, by="date")
temp <- left_join(temp, temp_robust, by="date")
save(temp, file="DataSet_Peru_DV_IV_S3.Rda")
write.csv(temp, "DataSet_Peru_DV_IV_S3.csv")
```




