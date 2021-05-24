
library(tidyverse)

reviews <- read_csv("reviews.csv")
reviews

library(tidytext)
tidy_reviews <- reviews %>%
unnest_tokens(word, Reviews) 
tidy_reviews

#we count the tokenized data to in descending number to see the no of stop words
tidy_reviews %>%
  count(word) %>%
  arrange(desc(n))

#Next, we'd remove stop words from the reviews data
library(tidytext)
tidy_reviews2 <- reviews %>%
  unnest_tokens(word, Reviews) %>%
anti_join(stop_words)
tidy_reviews2
View(tidy_reviews2)

#now lets count in descending order
tidy_reviews2 %>%
  count(word) %>%
  arrange(desc(n))
View(tidy_reviews2)

#we create a new ID column for the review data with the mutate function
#the mutate function counts the no of times an item appears in a row
#it counts the number of rows
tidy_reviews_mutate <- reviews %>%
  mutate(id = row_number()) %>%
  unnest_tokens(word, Reviews) %>%
  anti_join(stop_words)
View(tidy_reviews_mutate)  
tidy_reviews_mutate

#to see the the long list of words and their respective counts
#in order to identify the terms to b added as custom stop words
tidy_reviews_count <- tidy_reviews2 %>%
  count(word) %>%
  arrange(desc(n))
View(tidy_reviews_count)

#create a dataframe of custom stop words to remove words that are not
#informative and assign the tribble to custom stop words
library(tidyverse)

custom_stop_words <- tribble(
  ~word,         ~lexicon,
  "app",         "CUSTOM",
  "easy",        "CUSTOM",
  "time",        "CUSTOM",
  "buy",         "CUSTOM",
  "customer",    "CUSTOM",
  "power",       "CUSTOM",
  "experience",  "CUSTOM",
  "love",        "CUSTOM",
  "buypower",    "CUSTOM",
  "card",        "CUSTOM",
  "electricty",  "CUSTOM",
  "guys",        "CUSTOM",
  "nice",        "CUSTOM",  
  "perfect",     "CUSTOM",
  "buying",      "CUSTOM",
  "details",     "CUSTOM",
  "network",     "CUSTOM",
  "poor",        "CUSTOM",
  "units",       "CUSTOM",
  "money",       "CUSTOM",
  "pay",         "CUSTOM",  
  "satisfactory","CUSTOM",
  "send",        "CUSTOM",
  "unit",        "CUSTOM", 
  "application", "CUSTOM",  
  "bank",        "CUSTOM",   
  "downtime",    "CUSTOM",
  "generated",   "CUSTOM",
  "giving",      "CUSTOM",
  "metre",       "CUSTOM",
  "platform",    "CUSTOM",
  "purchase",    "CUSTOM",
  "rating",      "CUSTOM",
  "recharge",    "CUSTOM",
  "save",        "CUSTOM",
  "saves",       "CUSTOM",
  "started",     "CUSTOM",
  "account",     "CUSTOM",
  "banks",       "CUSTOM",
  "bought",      "CUSTOM",
  "care",        "CUSTOM",
  "check",       "CUSTOM",
  "customers",   "CUSTOM",
  "day",         "CUSTOM",
  "days",        "CUSTOM",
  "decided",     "CUSTOM",
  "enjoy",       "CUSTOM",
  "history",     "CUSTOM",
  "improvement", "CUSTOM",  
  "information", "CUSTOM",
  "input",       "CUSTOM",  
  "instantly",   "CUSTOM",
  "issue",       "CUSTOM",
  "issues",      "CUSTOM",
  "kudos",       "CUSTOM",
  "load",        "CUSTOM",
  "lot",         "CUSTOM",
  "march",       "CUSTOM",
  "means",       "CUSTOM",
  "month",       "CUSTOM",
  "office",      "CUSTOM",
  "paid",        "CUSTOM",
  "phone",       "CUSTOM",
  "reasons",     "CUSTOM",
  "received",    "CUSTOM",
  "receiving",   "CUSTOM",
  "recommend",   "CUSTOM",
  "recommended", "CUSTOM",  
  "redirecting", "CUSTOM",
  "safe",        "CUSTOM",
  "services",    "CUSTOM",
  "simply",      "CUSTOM",
  "star",        "CUSTOM",
  "stress",      "CUSTOM",  
  "stuck",       "CUSTOM",
  "support",     "CUSTOM",
  "swift",       "CUSTOM",
  "times",       "CUSTOM",
  "transaction", "CUSTOM",  
  "transactions","CUSTOM",  
  "transfer",    "CUSTOM",
  "ui",          "CUSTOM",
  "user",        "CUSTOM",
  "users",       "CUSTOM",
  "days",        "CUSTOM",
  "access",      "CUSTOM",
  "accessable",  "CUSTOM",
  "active",      "CUSTOM",  
  "actual",      "CUSTOM",
  "add",         "CUSTOM",
  "agent",       "CUSTOM",
  "ago",         "CUSTOM",
  "alternate",   "CUSTOM",
  "alternative", "CUSTOM",  
  "angry",       "CUSTOM",
  "anticipate",  "CUSTOM",
  "app.it",      "CUSTOM",
  "appointed",   "CUSTOM",
  "apps",        "CUSTOM",
  "atm",         "CUSTOM",
  "attention",   "CUSTOM",
  "authentication", "CUSTOM",
  "basic",       "CUSTOM",
  "beautiful",   "CUSTOM",
  "bebt",        "CUSTOM",
  "bit",         "CUSTOM",
  "borrow",      "CUSTOM",
  "call",        "CUSTOM",
  "challenge",   "CUSTOM",
  "channel",     "CUSTOM",
  "checkout",    "CUSTOM",
  "cluttered",   "CUSTOM",
  "code",        "CUSTOM",
  "collect",     "CUSTOM",
  "collection",  "CUSTOM",
  "commission",  "CUSTOM",
  "complaint",   "CUSTOM",
  "complaints",  "CUSTOM",
  "connection",  "CUSTOM",
  "consistent",  "CUSTOM",
  "consumption", "CUSTOM",
  "contact",     "CUSTOM",
  "continues",   "CUSTOM",
  "convenience", "CUSTOM",
  "conveniently","CUSTOM",
  "cool",        "CUSTOM",   
  "cosmas",      "CUSTOM",
  "couple",      "CUSTOM",
  "create",      "CUSTOM",
  "credited",    "CUSTOM",
  "data",        "CUSTOM",  
  "debitted",    "CUSTOM",
  "debt",        "CUSTOM",  
  "disappoint",  "CUSTOM",
  "disappointed","CUSTOM",
  "disappoints", "CUSTOM",
  "disheartening","CUSTOM",
  "double",       "CUSTOM",
  "easier",       "CUSTOM",
  "easily",       "CUSTOM",
  "eaten",        "CUSTOM",
  "effect",       "CUSTOM",
  "efficient",    "CUSTOM",
  "email",        "CUSTOM",
  "emergencies",  "CUSTOM",
  "energy",       "CUSTOM",
  "ensure",       "CUSTOM",
  "enter",        "CUSTOM",
  "error",        "CUSTOM",
"waoooowww",   "CUSTOM",
"magica", "CUSTOM",
"withing", "CUSTOM",
"service", "CUSTOM",
"relationship", "CUSTOM",
"reply", "CUSTOM",
"electricity", "CUSTOM",
"token", "CUSTOM",
"yesterday", "CUSTOM",
"service", "CUSTOM",
"terms", "CUSTOM",
"generation", "CUSTOM",
 "raymond", "CUSTOM",
"fast", "CUSTOM",
"served", "CUSTOM",
"goodi", "CUSTOM",
"ya'll", "CUSTOM",
"friends", "CUSTOM",
"loved", "CUSTOM",
"takes",  "CUSTOM",
"paying", "CUSTOM",
"okey", "CUSTOM",
"options", "CUSTOM",
"navigate", "CUSTOM",
"selected", "CUSTOM",
"flew", "CUSTOM",
"debited", "CUSTOM",
"funtional", "CUSTOM",
"handy", "CUSTOM",
"stop", "CUSTOM",
"solution", "CUSTOM",
"wonderfully", "CUSTOM",
"lessen", "CUSTOM",
"guy's", "CUSTOM",
"20k", "CUSTOM",
"worth", "CUSTOM",
"line", "CUSTOM",
"frustrating", "CUSTOM",
"3days", "CUSTOM",
"line", "CUSTOM",
"precise", "CUSTOM",
"feed", "CUSTOM",
"straightforward", "CUSTOM",
"massive", "CUSTOM",
"ooo", "CUSTOM",
"5", "CUSTOM",
"firstly", "CUSTOM",
"late", "CUSTOM",
"family", "CUSTOM",
"extremely", "CUSTOM",
"difficult", "CUSTOM",
"link", "CUSTOM",
"standing", "CUSTOM",
"front", "CUSTOM",
"difficult", "CUSTOM",
"god", "CUSTOM",
"start", "CUSTOM",
"technology", "CUSTOM",
"suggest", "CUSTOM",
"3", "CUSTOM",
"offline", "CUSTOM",
"essential", "CUSTOM",
"platforms", "CUSTOM",
"tie", "CUSTOM",
"online", "CUSTOM",
"simpler", "CUSTOM",
"ux", "CUSTOM",
"lay", "CUSTOM",
"infirmed", "CUSTOM",
"store", "CUSTOM",
"registered", "CUSTOM",
"starts", "CUSTOM",
"hiccups", "CUSTOM",
"fine", "CUSTOM",
"soo", "CUSTOM",
"payment", "CUSTOM",
"section", "CUSTOM",
"steps", "CUSTOM",
"protection", "CUSTOM",
"debited", "CUSTOM",
"pls", "CUSTOM",
"response", "CUSTOM",
"frame", "CUSTOM",
"resolve", "CUSTOM",
"quality", "CUSTOM",
"somedays", "CUSTOM",
"individuals", "CUSTOM",
"subscribe", "CUSTOM",
"home", "CUSTOM",
"phed", "CUSTOM",
"stars", "CUSTOM",
"props", "CUSTOM", 
"highly", "CUSTOM",
"refunded", "CUSTOM",
"experiencing", "CUSTOM",
"recently", "CUSTOM",
"1", "CUSTOM",
"makes", "CUSTOM",
"everytime", "CUSTOM",
"thia", "CUSTOM",
"ts", "CUSTOM",
"integration", "CUSTOM",
"hope", "CUSTOM",
"function", "CUSTOM",
"restored", "CUSTOM",
"jor", "CUSTOM",
"inputing", "CUSTOM",
"hardware", "CUSTOM",
"pin", "CUSTOM",
"page", "CUSTOM",
"trips", "CUSTOM",
"indefinitely", "CUSTOM",
"toll", "CUSTOM",
"free", "CUSTOM",
"process", "CUSTOM",
"thumbs", "CUSTOM",
"nigeria", "CUSTOM",
"operate", "CUSTOM",
"week", "CUSTOM",
"laid", "CUSTOM",
"inhave", "CUSTOM",
"23rd", "CUSTOM",
"000", "CUSTOM",
"15", "CUSTOM",
"29th", "CUSTOM",
"saving", "CUSTOM",
"hours", "CUSTOM",
"meter", "CUSTOM", 
"61", "CUSTOM",
"ommitted", "CUSTOM",
"havent", "CUSTOM",
"receive", "CUSTOM",
"reverse", "CUSTOM",
"inoperative", "CUSTOM",
"uninstall", "CUSTOM",
"fiew", "CUSTOM",


)

#we combine the original stop words with the custom stop words with bind_rows()

library(tidytext)
stop_words_final <- stop_words %>%
  bind_rows(custom_stop_words)

#now remove both custom and original stop words from the data set
tidy_reviews3 <- reviews %>%
  unnest_tokens(word, Reviews) %>%
  anti_join(stop_words_final)
tidy_reviews3
View(tidy_reviews3)

library(dplyr)
library(ggplot2)
  reviews_chart <- tidy_reviews3 %>%
    count(word)
ggplot(na.omit(reviews_chart), aes(x = word, y = n, fill = "green")) +
      geom_col(show.legend = FALSE) + coord_flip() + ggtitle("Review Word Counts") 
  
  
#PLOTTING WORDCLOUDS
library(wordcloud)
tidy_cloud <- tidy_reviews3 %>%
  count(word)
wordcloud(
  words = tidy_cloud$word,
  freq = tidy_cloud$n,
  colors = "orange"
  )
#you can add a maximum number of words
wordcloud(
  words = tidy_cloud$word,
  freq = tidy_cloud$n,
  max.words = 30
)

#WE WANT TO CLASSIFY THE REVIEWS AS NEGATIVE OR POSITIVE. WE USE THE LOUGHRAN
#DICTIONARY THAT COTAINS A LIST OF WORDS CLASSIFIED AS POSITIVE AND NEGATIVE

#we use inner_join() to append the dic to the tidied data
library(dplyr)
library(syuzhet)
library(tidytext)
sentiment_review <- tidy_reviews3 %>%
  inner_join(get_sentiments("loughran"))

#to access the the loughran dictionary, download the textdata package
library(textdata) 
get_sentiments("loughran")

#We can also count() by both word and sentiment to find what words 
#are used most often for each sentiment
sentiment_review %>%
  count(word, sentiment) %>%
arrange(desc(n))

#visualizing the sentiment review by positive and negative
sentiment_review2 <- sentiment_review %>%
  filter(sentiment %in% c("positive", "negative"))
sentiment_review2
review_category_count <- sentiment_review2 %>%
  count (word, sentiment) %>%
  group_by (sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word2 = fct_reorder(word, n))

#Now we visualize these top 10 positive and negative words using facet_wrap().
ggplot(review_category_count, aes(x = word2, y = n, fill = "sentiment")) + 
         geom_col(show.legend = FALSE) + 
         facet_wrap (~ sentiment, scales = "free") +
          coord_flip() + 
         labs(
           title = "Sentiment Word Counts",
           x = "Words"
         )
        
#VISUALIZING REVIEW RATING
library(tidytext)
get_sentiments("bing")

#append the bing dictionary
library(dplyr)
rating_review <- sentiment_review %>%
  inner_join(get_sentiments("bing")) %>%
count(Rating, sentiment)

#after counting, we have the problem of different rating showing up for 
#the same sentiment. Positive displays a rating of 5 and 1 on diff rows.
#WE USE SPREAD() to fix this by creating a column for positive and negative
#sentiments originally under the sentiment column

sentiment_rating <- sentiment_review %>%
  inner_join(get_sentiments("bing")) %>%
  count(Rating, sentiment) %>%
spread(sentiment, n)%>%
# by using spread(), we have a single row for the review rating instead of
#multiple rows

# computing the difference between the positive and negative counts is 
#simply a matter of using mutate().
#visualise sentiment by rating
mutate(overall_sentiment = positive - negative)
     
  ggplot(sentiment_rating, aes(x = Rating, y = overall_sentiment, 
                               fill = as.factor(Rating))) +
    geom_col(show.legend = FALSE) + 
  coord_flip() + 
  labs(
    title = "Overall Sentiment by Rating",
    x = "Rating",
    y = "Overall Sentiment"
  )
#this resulted in a chart for one rating as there was only one rating value of
  #5 after positive and negative had been subtracted
  #Instead I'd plot rating count 
  library(tidyverse)
  library(tidytext)
  sentiment_rating <- sentiment_review %>%
    inner_join(get_sentiments("bing")) %>%
    count(Rating)
    
  ggplot(sentiment_rating, aes(x = Rating, y = n, fill = "Rating")) +
    geom_col(show.legend = FALSE) + 
    coord_flip() + 
    labs(
      title = "Rating Count",
      x = "Rating",
      y = "Count"
    )
  