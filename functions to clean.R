library(stringi)
library('lsa')
library('tm')
library('tidyverse')
library('tidytext')
library('rio')
library('Hmisc')
library(conflicted)
conflict_prefer('mutate', 'dplyr')
conflict_prefer('filter', 'dplyr')

stopwords <- stopwords_de %>% 
  tibble()

prepare_text <- function(text_char) {
  text_char <- gsub('\n', ' ', text_char)
  text_char <- tibble(text_char) %>%
    unnest_tokens(
      tbl = .,
      input = .,
      #specific column
      output = 'word',
      token = 'ngrams',
      n = 1,
      collapse = T,
      to_lower = F
    ) %>%
    filter(nchar(word)!=1) %>% #filter all words that have just one character, mostly numbers and falsely read in words
    select(-c(text_char)) %>% #remove column that has the whole document
    anti_join(., stopwords, by=c('word'='.'))
  
  text_char$word <- stemDocument(text_char$word, language='german')
  
  text_char %>% 
    rowid_to_column("id") #provide a unique id
}

#Importing and unnesting the negative sentiment list
sent_negative <-
  import(
    '/Users/Constanze/Desktop/uni/ba arbeit/sentiment analysis/SentiWS_v2.0/SentiWS_v2.0_Negative.txt')

sent_negative_1 <- sent_negative %>% 
  mutate(V3=gsub(pattern = "\\|.*", replacement = "", x=sent_negative$V1))

sent_negative_2 <- sent_negative %>% 
  separate_rows(., V3, sep = ',')

sent_negative <- bind_rows(sent_negative_1, sent_negative_2)

rm(sent_negative_1, sent_negative_2)

sent_negative$V3 <- as.vector(stemDocument(sent_negative$V3, language = 'german'))

sent_negative <- sent_negative %>% 
  unique()

sent_negative <- sent_negative %>% 
  filter(!V3=='')

#Importing and unnesting the positive sentiment list
sent_positive <-
  import(
    '/Users/Constanze/Desktop/uni/ba arbeit/sentiment analysis/SentiWS_v2.0/SentiWS_v2.0_Positive.txt'
  )

sent_positive_1 <- sent_positive %>% 
  mutate(V3=gsub(pattern = "\\|.*", replacement = "", x=sent_positive$V1))

sent_positive_2 <- sent_positive %>% 
  separate_rows(., V3, sep = ',')

sent_positive <- bind_rows(sent_positive_1, sent_positive_2)

rm(sent_positive_1, sent_positive_2)

sent_positive <- sent_positive %>% 
  filter(!V1=='nachhaltig|ADJX') %>% 
  filter(!V1=='Nachhaltigkeit|NN')

sent_positive$V3 <- as.vector(stemDocument(sent_positive$V3, language = 'german'))

sent_positive <- sent_positive %>% 
  unique()

sent_positive <- sent_positive %>% 
  filter(!V3=='')

add_sent <- function(text_char, direction = 'negative') {
  if (direction == 'negative') {
    text_char %>%
      left_join(sent_negative, by = c('word' = 'V3')) %>%
      rename(wort_neg = V1, neg_score = V2)
    } else {
    text_char %>%
      left_join(sent_positive, by = c('word' = 'V3')) %>%
      rename(wort_pos = V1, pos_score = V2)
    }
}

create_overall_sentiment <- function(text_char) {
  text_char <- add_sent(text_char, 'negative')
  text_char <- add_sent(text_char, 'positive')
  text_char <- text_char %>%
    mutate(
      wortstamm = ifelse(is.na(wort_neg), wort_pos, wort_neg),
      sentiment = ifelse(is.na(neg_score), pos_score, neg_score)
    ) %>% 
    rowid_to_column(., "sent_id")
}

#negation
check_negation <- function(text_char) {
  negation1 <- c('nicht', 'kein', 'keine', 'keinem', 'keines', 'niemand') %>% 
    tibble()
  names(negation1) <- 'V1'
  negation2 <- import('verbal_shifters.bootstrapping.txt', header=F) %>% 
    filter(V2=='SHIFTER') %>% 
    select(V1)
  
  negation <- bind_rows(negation1, negation2)
  
  negation <- stemDocument(negation$V1, language='german') %>% 
    tibble()
  
  negation <- negation %>% 
      anti_join(sent_negative, by=c('.'='V3'))  %>% 
    anti_join(sent_positive, by=c('.'='V3'))
  
  rm(negation1, negation2)
  
  text_char <- text_char %>%
    mutate(negation = ifelse(word %in% negation$.,-1, 1))
}

#add distance
calculate_distance <- function(text_char, direction='sentiment') {
  
  if (direction == 'negative') {
    score <- text_char$neg_score
  } else if (direction == 'positive') {
    score <- text_char$pos_score
  } else {
    score <- text_char$sentiment
  }
  
  id_sent <- text_char %>%
    filter(!is.na(sentiment)) %>%
    select(sent_id) %>%
    as.vector()
  
  id_shift <- text_char %>%
    filter(negation == -1) %>%
    select(sent_id) %>%
    as.vector()
  
  if(nrow(id_shift)==0){
    result <- cbind(id_sent$sent_id, 99999) %>% 
      as.data.frame()
    names(result) <- c('dist_id', 'minimum distance')
    
    result <- left_join(text_char, result, by=c('sent_id'='dist_id'))
    return(result)
  } else {
    
    dist <- function(id_sentiment, id_shift) {
      abs(id_shift - id_sentiment)
    }
    
    distance <- sapply(id_shift$sent_id, dist, id_shift=(id_sent$sent_id))
    distance <- apply(distance, 1, FUN = min)
    result <- cbind(id_sent$sent_id, distance) %>% 
      as.data.frame()
    names(result) <- c('dist_id', 'minimum distance')
    
    result <- left_join(text_char, result, by=c('sent_id'='dist_id'))
    return(result)
  }
}


#Most important negative and positive words
most_sent <- function(text_char, score){
  text_char %>% 
    filter(is.na(text_char[[score]])==FALSE) %>%
    count(wortstamm) %>%
    arrange(desc(n))
}

#Summing up the pos and neg count 
total_sentiment <- function(text_char){
  sum_pos <-sum(text_char$pos_score, na.rm = T) / nrow(text_char)* 100
  sum_neg <-sum(text_char$neg_score, na.rm = T) / nrow(text_char)* 100
  net_sentiment <- sum_pos + sum_neg
  
  result <- list('percent_positive'=sum_pos, 'percent_negative'=sum_neg, 'percent_net_sentiment'=net_sentiment)

  return(result)
}

#Negation shift
negation_shift <- function(text_char, dist_threshold=6){
  text_char %>% 
    mutate(sentiment=ifelse(`minimum distance`<dist_threshold,
           sentiment*-sin(1/`minimum distance`),
           sentiment), 
           pos_score=ifelse(`minimum distance`<dist_threshold,
                            pos_score*-sin(1/`minimum distance`),
                            pos_score), 
           neg_score=ifelse(`minimum distance`<dist_threshold,
                            neg_score*-sin(1/`minimum distance`),
                            neg_score)
    )
}

#preprocess and sentiment together
text_final <- function(text_char){
  text_char <- prepare_text(text_char)
  text_char <- create_overall_sentiment(text_char)
  text_char <- check_negation(text_char)
  text_char <- calculate_distance(text_char)
  text_char <- negation_shift(text_char)
  return(text_char)
}
