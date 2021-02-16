##### Sentiment Analysis in R #####
#
# This script serves as a tutorial for conducting sentiment analysis in R.
# Borrowed heavily from @rtatman at Kaggle:
# https://www.kaggle.com/rtatman/tutorial-sentiment-analysis-in-r/comments
#
# M. Chiovaro (University of Connectiut)
# Last updated: 2020_02_09

##### 1. Set up #####

# load libraries
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)

# clear environment
rm(list=ls())

# get a list of the file names
files <- list.files("./archive")

##### 2. Test it out with one file #####

# add path name
fileName <- glue("./archive/", files[1], sep = "")
# get rid of any sneaky trailing spaces
fileName <- trimws(fileName)

# read in the new file
fileText <- glue(read_file(fileName))
# remove any dollar signs (they're special characters in R)
fileText <- gsub("\\$", "", fileText) 

# tokenize
tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)

# get the sentiment from the first text: 
tokens %>%
  
  # pull out only sentiment words
  inner_join(get_sentiments("bing")) %>% 
  
  # count the # of positive & negative words
  count(sentiment) %>% 
  
  # made data wide rather than narrow
  spread(sentiment, n, fill = 0) %>% 
  
  # number of positive words - number of negative words
  mutate(sentiment = positive - negative) 

##### 3. Loop through multiple files #####

# write a function that takes the name of a file and returns the number of postive &
# negative sentiment words, and the difference and the normalized difference
GetSentiment <- function(file){
  
  # get the file
  fileName <- glue("../input/", file, sep = "")
  
  # get rid of any sneaky trailing spaces
  fileName <- trimws(fileName)
  
  # read in the new file
  fileText <- glue(read_file(fileName))
  
  # remove any dollar signs (they're special characters in R)
  fileText <- gsub("\\$", "", fileText) 
  
  # tokenize
  tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)
  
  # get the sentiment from the first text: 
  sentiment <- tokens %>%
    
    # pull out only sentimen words
    inner_join(get_sentiments("bing")) %>% 
    
    # count the # of positive & negative words
    count(sentiment) %>% 
    
    # made data wide rather than narrow
    spread(sentiment, n, fill = 0) %>% 
    
    # # of positive words - # of negative owrds
    mutate(sentiment = positive - negative) %>% 
    
    # add the name of our file
    mutate(file = file) %>% 
    
    # add the year
    mutate(year = as.numeric(str_match(file, "\\d{4}"))) %>% 
    
    # add president
    mutate(president = str_match(file, "(.*?)_")[2]) 
  
  # return our sentiment dataframe
  return(sentiment)
}

##### 4. Run sentiment analysis #####

# file to put our output in
sentiments <- data_frame()

# get the sentiments for each file in our datset
for(i in files){
  sentiments <- rbind(sentiments, GetSentiment(i))
}

# disambiguate Bush Sr. and George W. Bush 
# correct president in applicable rows
bushSr <- sentiments %>% 
  
  # get rows where the president is named "Bush"
  filter(president == "Bush") %>%
  
  # and the year is before 200
  filter(year < 2000) %>% 
  
  #change "Bush" to "Bush Sr."
  mutate(president = "Bush Sr.") 

# remove incorrect rows
sentiments <- anti_join(sentiments, 
                        sentiments[sentiments$president == "Bush" & 
                                     sentiments$year < 2000, ])

# add corrected rows to data_frame 
sentiments <- full_join(sentiments, bushSr)

# summerize the sentiment measures
summary(sentiments)

# plot of sentiment over time
ggplot(sentiments, aes(x = as.numeric(year), y = sentiment)) + 
  
  # add points colored by persident
  geom_point(aes(color = president)) +
  
  # pick a method & fit a model
  geom_smooth(method = "auto") 

# plot of sentiment by president
ggplot(sentiments, aes(x = president, y = sentiment, color = president)) + 
  geom_boxplot() 

##### 5. Bonus exercise: Is there a difference between parties?

# get democratic presidents and add party affiliation
democrats <- sentiments %>%
  filter(president == c("Clinton","Obama")) %>%
  mutate(party = "D")

# get republican presidents and add party affiliation
republicans <- sentiments %>%
  filter(president != "Clinton" & president != "Obama") %>%
  mutate(party = "R")

# join both parties
byParty <- full_join(democrats, republicans)

# test for statistical difference
t.test(democrats$sentiment, republicans$sentiment)

# plot sentiment by party
ggplot(byParty, 
       aes(x = party, 
           y = sentiment, 
           color = party)) + 
  geom_boxplot() + 
  geom_point()
