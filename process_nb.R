#Read in the needed functions
source('functions to clean.R')
#Prepare text and add sentiment for all NB 
source('nb read in.R')

#Add sentiment 
einsundeins_nb_nb <- text_final(einsundeins_nb_nb)
aareal_nb_nb <- text_final(aareal_nb_nb)
alstria_nb_nb <- text_final(alstria_nb_nb)
aurubis_nb_nb <- text_final(aurubis_nb_nb)
bechtle_nb_nb <-  text_final(bechtle_nb_nb)
brenntag_nb_nb <- text_final(brenntag_nb_nb)
commerzbank_nb_nb <- text_final(commerzbank_nb_nb)
ctseventim_nb_nb <- text_final(ctseventim_nb_nb)
tag_nb_nb <- text_final(tag_nb_nb)
deutschepfandbriefbank_nb_nb <- text_final(deutschepfandbriefbank_nb_nb)
duerr_nb_nb <- text_final(duerr_nb_nb)
evonik_nb_nb <- text_final(evonik_nb_nb)
evotec_nb_nb <- text_final(evotec_nb_nb)
fielmann_nb_nb <- text_final(fielmann_nb_nb)
fraport_nb_nb <- text_final(fraport_nb_nb)
fuchs_nb_nb <- text_final(fuchs_nb_nb)
gea_nb_nb <- text_final(gea_nb_nb)
hugoboss_nb_nb <- text_final(hugoboss_nb_nb)
innogy_nb_nb <- text_final(innogy_nb_nb)
kion_nb_nb <- text_final(kion_nb_nb)
lanxess_nb_nb <- text_final(lanxess_nb_nb)
legimmobilien_nb_nb <- text_final(legimmobilien_nb_nb)
metro_nb_nb <- text_final(metro_nb_nb)
nemetschek_nb_nb <- text_final(nemetschek_nb_nb)
norma_nb_nb <- text_final(norma_nb_nb)
osram_nb_nb <- text_final(osram_nb_nb)
rocketinternet_nb_nb <- text_final(rocketinternet_nb_nb)
salzgitter_nb_nb <- text_final(salzgitter_nb_nb)
schaeffler_nb_nb <- text_final(schaeffler_nb_nb)
scout24_nb_nb <- text_final(scout24_nb_nb)
siltronic_nb_nb <- text_final(siltronic_nb_nb)
softwareag_nb_nb <- text_final(softwareag_nb_nb)
telefonica_nb_nb <- text_final(telefonica_nb_nb)
uniper_nb_nb <- text_final(uniper_nb_nb)
unitedinternet_nb_nb <- text_final(unitedinternet_nb_nb)

#Combining all the dataframes 
nb_nb_all <- mget(ls(pattern = "_nb_nb")) %>% 
  map_df(I, .id = "report") %>% 
  count(report, word, sentiment, pos_score, neg_score ,sort = T)
#somehow does not work

total_words <- nb_nb_all %>% 
  group_by(report) %>% 
  summarise(total=sum(n))

nb_nb_all <- left_join(nb_nb_all, total_words) %>% 
  mutate(term_frequency=n/total)

#negativity in percent 
#as the proportion of negative words relative to the count of total words 
#positivity
#sum of negative and positive words relatively to the total word count as net sentiment. 
result_nb_nb <- nb_nb_all %>% 
  filter(sentiment>0) %>% 
  # select(report, total, word, sentiment, pos_score, neg_score) %>% 
  group_by(report, total) %>% 
  count(word) %>% 
  summarise(sum_pos_words=sum(n)) %>% 
  mutate(positivity_in_perc=sum_pos_words/total*100) %>% 
  left_join(., y=nb_nb_all %>% 
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
nb_nb_all <- nb_nb_all %>% 
  # filter(is.na(sentiment)==FALSE) %>% 
  bind_tf_idf(word, report, n)

#weighted sent 
nb_nb_all <-  nb_nb_all %>%
  mutate(weighted_sent=sentiment*tf_idf, 
         weighted_pos=pos_score*tf_idf,
         weighted_neg=neg_score*tf_idf)

#calculate sum scores for company-report pairs
result_nb_nb <- result_nb_nb %>% 
  left_join(x=., y=nb_nb_all %>%
              filter(weighted_sent<0) %>% 
              group_by(report) %>% 
              summarise(sum_neg_weight = sum(weighted_sent)),
            by='report') %>% 
  left_join(x=., y=nb_nb_all %>%
              filter(weighted_sent>0) %>%
              group_by(report) %>% 
              summarise(sum_pos_weight =sum(weighted_sent)),
            by='report') %>% 
  mutate(sum_neg_weight=ifelse(is.na(sum_neg_weight), 0, sum_neg_weight),
         net_sum_weight=sum_neg_weight+sum_pos_weight)


write_csv(result_nb_nb, path = 'result_nb_nb.csv')
