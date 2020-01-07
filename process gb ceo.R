#Read in the needed functions
source('functions to clean.R')
#Prepare text and add sentiment for all GB CEO 
source('gb_ceo_read_in.R')

#Prepare all ceo letters
einsundeins_gb_ceo <-text_final(einsundeins_gb_ceo)
schaeffler_gb_ceo <-text_final(schaeffler_gb_ceo)
aurubis_gb_ceo <- text_final(aurubis_gb_ceo)
axelspringer_gb_ceo <- text_final(axelspringer_gb_ceo)
bechtle_gb_ceo <- text_final(bechtle_gb_ceo)
brenntag_gb_ceo <- text_final(brenntag_gb_ceo)
commerzbank_gb_ceo <- text_final(commerzbank_gb_ceo)
ctseventim_gb_ceo <- text_final(ctseventim_gb_ceo)
deliveryhero_gb_ceo <- text_final(deliveryhero_gb_ceo)
deutscheeuroshop_gb_ceo <- text_final(deutscheeuroshop_gb_ceo)
deutschepfandbriefbank_gb_ceo <- text_final(deutschepfandbriefbank_gb_ceo)
deutschewohnen_gb_ceo <- text_final(deutschewohnen_gb_ceo)
duerr_gb_ceo <- text_final(duerr_gb_ceo)
evonik_gb_ceo <- text_final(evonik_gb_ceo)
evotec_gb_ceo <- text_final(evotec_gb_ceo)
fielmann_gb_ceo <- text_final(fielmann_gb_ceo)
fraport_gb_ceo <- text_final(fraport_gb_ceo)
freenet_gb_ceo <- text_final(freenet_gb_ceo)
fuchs_gb_ceo <- text_final(fuchs_gb_ceo)
gea_gb_ceo <- text_final(gea_gb_ceo)
gerresheimer_gb_ceo <- text_final(gerresheimer_gb_ceo)
hannoverrueck_gb_ceo <- text_final(hannoverrueck_gb_ceo)
hella_gb_ceo <- text_final(hella_gb_ceo)
hochtief_gb_ceo <- text_final(hochtief_gb_ceo)
hugoboss_gb_ceo <- text_final(hugoboss_gb_ceo)
innogy_gb_ceo <- text_final(innogy_gb_ceo)
kunds_gb_ceo <- text_final(kunds_gb_ceo)
kion_gb_ceo <- text_final(kion_gb_ceo)
lanxess_gb_ceo <- text_final(lanxess_gb_ceo)
legimmobilien_gb_ceo <- text_final(legimmobilien_gb_ceo)
metro_gb_ceo <- text_final(metro_gb_ceo)
morphosys_gb_ceo <- text_final(morphosys_gb_ceo)
mtuaerosystems_gb_ceo <- text_final(mtuaerosystems_gb_ceo)
nemetschek_gb_ceo <- text_final(nemetschek_gb_ceo)
norma_gb_ceo <- text_final(norma_gb_ceo)
p7s1_gb_ceo <- text_final(p7s1_gb_ceo)
puma_gb_ceo <- text_final(puma_gb_ceo)
rheinmetall_gb_ceo <- text_final(rheinmetall_gb_ceo)
rocketinternet_gb_ceo <- text_final(rocketinternet_gb_ceo)
salzgitter_gb_ceo <- text_final(salzgitter_gb_ceo)
sartorius_gb_ceo <- text_final(sartorius_gb_ceo)
scout24_gb_ceo <- text_final(scout24_gb_ceo)
siemenshealthineers_gb_ceo <- text_final(siemenshealthineers_gb_ceo)
siltronic_gb_ceo <- text_final(siltronic_gb_ceo)
softwareag_gb_ceo <- text_final(softwareag_gb_ceo)
symrise_gb_ceo <- text_final(symrise_gb_ceo)
tag_gb_ceo <- text_final(tag_gb_ceo)
telefonica_gb_ceo <- text_final(telefonica_gb_ceo)
uniper_gb_ceo <- text_final(uniper_gb_ceo)
unitedinternet_gb_ceo <- text_final(unitedinternet_gb_ceo)
wackerchemie_gb_ceo <- text_final(wackerchemie_gb_ceo)
zalando_gb_ceo <-text_final(zalando_gb_ceo)

rm(result_gb_ceo)
#Combining all the dataframes 
gb_ceo_all <- mget(ls(pattern = '_gb_ceo')) %>% 
  map_df(I, .id = "report") %>% 
  count(report, word, sentiment, pos_score, neg_score ,sort = T)

total_words <- gb_ceo_all %>% 
  group_by(report) %>% 
  summarise(total=sum(n))

gb_ceo_all <- left_join(gb_ceo_all, total_words) %>% 
 mutate(term_frequency=n/total)

#negativity in percent 
#as the proportion of negative words relative to the count of total words 
#positivity
#sum of negative and positive words relatively to the total word count as net sentiment. 
result_gb_ceo <- gb_ceo_all %>% 
              filter(sentiment>0) %>% 
              # select(report, total, word, sentiment, pos_score, neg_score) %>% 
              group_by(report, total) %>% 
              count(word) %>% 
              summarise(sum_pos_words=sum(n)) %>% 
              mutate(positivity_in_perc=sum_pos_words/total*100) %>% 
  left_join(., y=gb_ceo_all %>% 
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
gb_ceo_all <- gb_ceo_all %>% 
  # filter(is.na(sentiment)==FALSE) %>% 
  bind_tf_idf(word, report, n)

#weighted sent 
gb_ceo_all <-  gb_ceo_all %>%
  mutate(weighted_sent=sentiment*tf_idf, 
         weighted_pos=pos_score*tf_idf,
         weighted_neg=neg_score*tf_idf)

#calculate sum scores for company-report pairs
result_gb_ceo <- result_gb_ceo %>% 
  left_join(x=., y=gb_ceo_all %>%
              filter(weighted_sent<0) %>% 
              group_by(report) %>% 
              summarise(sum_neg_weight = sum(weighted_sent)),
            by='report') %>% 
  left_join(x=., y=gb_ceo_all %>%
              filter(weighted_sent>0) %>%
              group_by(report) %>% 
              summarise(sum_pos_weight =sum(weighted_sent)),
            by='report') %>% 
  mutate(sum_neg_weight=ifelse(is.na(sum_neg_weight), 0, sum_neg_weight),
         net_sum_weight=sum_neg_weight+sum_pos_weight)


write_csv(result_gb_ceo, path = 'results_gb_ceo.csv')

ggplot(result_gb_ceo, aes(net_sent_in_perc, y=net_sum_weight))+
  geom_point()+
  geom_smooth(method = lm)

cor.test(result_gb_ceo$net_sent_in_perc, y=result_gb_ceo$net_sum_weight)
ggplot(result_gb_ceo, aes(negativity_in_perc, y=sum_neg_weight))+
  geom_point()+
  geom_smooth(method = lm)
cor.test(result_gb_ceo$negativity_in_perc, y=result_gb_ceo$sum_neg_weight)
ggplot(result_gb_ceo, aes(positivity_in_perc, y=sum_pos_weight))+
  geom_point()+
  geom_smooth(method = lm)
cor.test(result_gb_ceo$positivity_in_perc, y=result_gb_ceo$sum_pos_weight)

ggplot(result_gb_ceo, aes(positivity_in_perc, negativity_in_perc))+
  geom_point()+
  geom_smooth(method = lm)

ggplot(result_gb_ceo, aes(sum_neg_weight, sum_pos_weight))+
  geom_point()+
  geom_smooth(method = lm)
cor.test(result_gb_ceo$sum_neg_weight, result_gb_ceo$sum_pos_weight)

