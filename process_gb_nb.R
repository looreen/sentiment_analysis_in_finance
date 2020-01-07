#Read in the needed functions
source('functions to clean.R')
#Prepare text and add sentiment for all NB 
source('gb nb read in.R')

#Add sentiment 
aurubis_gb_nb<- text_final(aurubis_gb_nb)
brenntag_gb_nb <- text_final(brenntag_gb_nb)
commerzbank_gb_nb <- text_final(commerzbank_gb_nb)
deliveryhero_gb_nb <- text_final(deliveryhero_gb_nb)
deutscheuroshop_gb_nb <- text_final(deutscheuroshop_gb_nb)
deutschepfandbriefbank_gb_nb <- text_final(deutschepfandbriefbank_gb_nb)
deutschewohnen_gb_nb <- text_final(deutschewohnen_gb_nb)
duerr_gb_nb <- text_final(duerr_gb_nb)
evonik_gb_nb <- text_final(evonik_gb_nb)
fraport_gb_nb <- text_final(fraport_gb_nb)
freenet_gb_nb <- text_final(freenet_gb_nb)
fuchs_gb_nb <- text_final(fuchs_gb_nb)
gea_gb_nb <- text_final(gea_gb_nb)
gerresheimer_gb_nb <- text_final(gerresheimer_gb_nb)
hella_gb_nb <- text_final(hella_gb_nb)
hochtief_gb_nb <- text_final(hochtief_gb_nb)
hugoboss_gb_nb <- text_final(hugoboss_gb_nb)
kunds_gb_nb <- text_final(kunds_gb_nb)
lanxess_gb_nb <- text_final(lanxess_gb_nb)
morphosys_gb_nb <- text_final(morphosys_gb_nb)
mtuaerosystems_gb_nb <- text_final(mtuaerosystems_gb_nb)
nemetschek_gb_nb <- text_final(nemetschek_gb_nb)
osram_gb_nb <- text_final(osram_gb_nb)
p7s1_gb_nb <- text_final(p7s1_gb_nb)
puma_gb_nb <- text_final(puma_gb_nb)
rheinmetall_gb_nb <- text_final(rheinmetall_gb_nb)
sartorius_gb_nb <- text_final(sartorius_gb_nb)
siltronic_gb_nb <- text_final(siltronic_gb_nb)
softwareag_gb_nb <- text_final(softwareag_gb_nb)
symrise_gb_nb <- text_final(symrise_gb_nb)
uniper_gb_nb <- text_final(uniper_gb_nb)
wackerchemie_gb_nb <- text_final(wackerchemie_gb_nb)
zalando_gb_nb <- text_final(zalando_gb_nb)

#Combining all the dataframes 
gb_nb_all <- mget(ls(pattern = '_gb_nb')) %>% 
  map_df(I, .id = "report") %>% 
  count(report, word, sentiment, pos_score, neg_score ,sort = T)

total_words <- gb_nb_all %>% 
  group_by(report) %>% 
  summarise(total=sum(n))

gb_nb_all <- left_join(gb_nb_all, total_words) %>% 
  mutate(term_frequency=n/total)

#negativity in percent 
#as the proportion of negative words relative to the count of total words 
#positivity
#sum of negative and positive words relatively to the total word count as net sentiment. 
result_gb_nb <- gb_nb_all %>% 
  filter(sentiment>0) %>% 
  # select(report, total, word, sentiment, pos_score, neg_score) %>% 
  group_by(report, total) %>% 
  count(word) %>% 
  summarise(sum_pos_words=sum(n)) %>% 
  mutate(positivity_in_perc=sum_pos_words/total*100) %>% 
  left_join(., y=gb_nb_all %>% 
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
gb_nb_all <- gb_nb_all %>% 
  # filter(is.na(sentiment)==FALSE) %>% 
  bind_tf_idf(word, report, n)

#weighted sent 
gb_nb_all <-  gb_nb_all %>%
  mutate(weighted_sent=sentiment*tf_idf, 
         weighted_pos=pos_score*tf_idf,
         weighted_neg=neg_score*tf_idf)

#calculate sum scores for company-report pairs
result_gb_nb <- result_gb_nb %>% 
  left_join(x=., y=gb_nb_all %>%
              filter(weighted_sent<0) %>% 
              group_by(report) %>% 
              summarise(sum_neg_weight = sum(weighted_sent)),
            by='report') %>% 
  left_join(x=., y=gb_nb_all %>%
              filter(weighted_sent>0) %>%
              group_by(report) %>% 
              summarise(sum_pos_weight =sum(weighted_sent)),
            by='report') %>% 
  mutate(sum_neg_weight=ifelse(is.na(sum_neg_weight), 0, sum_neg_weight),
         net_sum_weight=sum_neg_weight+sum_pos_weight)


write.csv2(result_gb_nb, file = 'result_gb_nb.csv')
