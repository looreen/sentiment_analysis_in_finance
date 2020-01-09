#Read in the needed functions
source('functions to clean.R')
#Prepare text and add sentiment for all NB 
source('nb_ceo_read_in.R')

einsundeins_nb_ceo <- text_final(einsundeins_nb_ceo)
alstria_nb_ceo <- text_final(alstria_nb_ceo)
aurubis_nb_ceo <- text_final(aurubis_nb_ceo)
bechtle_nb_ceo <-  text_final(bechtle_nb_ceo)
brenntag_nb_ceo <-  text_final(brenntag_nb_ceo)
fielmann_nb_ceo <-  text_final(fielmann_nb_ceo)
fraport_nb_ceo <-  text_final(fraport_nb_ceo)
fuchs_nb_ceo <-  text_final(fuchs_nb_ceo)
hugoboss_nb_ceo <-  text_final(hugoboss_nb_ceo)
innogy_nb_ceo <-  text_final(innogy_nb_ceo)
kion_nb_ceo <-  text_final(kion_nb_ceo)
legimmobilien_nb_ceo <-  text_final(legimmobilien_nb_ceo)
metro_nb_ceo <-  text_final(metro_nb_ceo)
norma_nb_ceo <-  text_final(norma_nb_ceo)
osram_nb_ceo <-  text_final(osram_nb_ceo)
scout24_nb_ceo <-  text_final(scout24_nb_ceo)
siltronic_nb_ceo <-  text_final(siltronic_nb_ceo)
tag_nb_ceo <-  text_final(tag_nb_ceo)
telefonica_nb_ceo <- text_final(telefonica_nb_ceo)
uniper_nb_ceo <- text_final(uniper_nb_ceo)
unitedinternet_nb_ceo <- text_final(unitedinternet_nb_ceo)

rm(result_nb_ceo)
#Combining all the dataframes 
nb_ceo_all <- mget(ls(pattern = "_nb_ceo")) %>% 
  map_df(I, .id = "report") %>% 
  count(report, word, sentiment, pos_score, neg_score ,sort = T)

total_words <- nb_ceo_all %>% 
  group_by(report) %>% 
  summarise(total=sum(n))

nb_ceo_all <- left_join(nb_ceo_all, total_words) %>% 
  mutate(term_frequency=n/total)

#negativity in percent 
#as the proportion of negative words relative to the count of total words 
#positivity
#sum of negative and positive words relatively to the total word count as net sentiment. 
result_nb_ceo <- nb_ceo_all %>% 
  filter(sentiment>0) %>% 
  # select(report, total, word, sentiment, pos_score, neg_score) %>% 
  group_by(report, total) %>% 
  count(word) %>% 
  summarise(sum_pos_words=sum(n)) %>% 
  mutate(positivity_in_perc=sum_pos_words/total*100) %>% 
  left_join(., y=nb_ceo_all %>% 
              filter(sentiment<0) %>% 
              # select(report, total, word, sentiment, pos_score, neg_score) %>% 
              group_by(report, total) %>% 
              count(word) %>% 
              summarise(sum_neg_words=sum(n)) %>% 
              mutate(negativity_in_perc=sum_neg_words/total*100), by=c('report', 'total')) %>% 
  mutate(sum_neg_words=ifelse(is.na(sum_neg_words), 0, sum_neg_words),
         sum_pos_words=ifelse(is.na(sum_pos_words), 0, sum_pos_words),
         negativity_in_perc=ifelse(is.na(negativity_in_perc), 0, negativity_in_perc),
         net_sent_in_perc=(sum_pos_words-sum_neg_words)/total*100)

# #Add tf_idf 
nb_ceo_all <- nb_ceo_all %>% 
  # filter(is.na(sentiment)==FALSE) %>% 
  bind_tf_idf(word, report, n)

#weighted sent 
nb_ceo_all <-  nb_ceo_all %>%
  mutate(weighted_sent=sentiment*tf_idf, 
         weighted_pos=pos_score*tf_idf,
         weighted_neg=neg_score*tf_idf)

#calculate sum scores for company-report pairs
result_nb_ceo <- result_nb_ceo %>% 
  left_join(x=., y=nb_ceo_all %>%
              filter(weighted_sent<0) %>% 
              group_by(report) %>% 
              summarise(sum_neg_weight = sum(weighted_sent)),
            by='report') %>% 
  left_join(x=., y=nb_ceo_all %>%
              filter(weighted_sent>0) %>%
              group_by(report) %>% 
              summarise(sum_pos_weight =sum(weighted_sent)),
            by='report') %>% 
  mutate(sum_neg_weight=ifelse(is.na(sum_neg_weight), 0, sum_neg_weight),
         net_sum_weight=sum_neg_weight+sum_pos_weight)


#write_csv(result_nb_ceo, path = 'result_nb_ceo.csv')

