###Load necessary libraries
library('factoextra')
library(viridis)
library(tidyverse)
library(stringi)
library(Hmisc)


### Maximum values for the sentiment lexicon 
max(sent_positive$V2)
min(sent_positive$V2)
min(sent_negative$V2)
max(sent_negative$V2)


### ANNUAL REPORTS
##Ops: In order to run, 'process_gb_nb.R' and 'process_gb_ceo.R' must have run successfully before!

#Remove old data and combine the preloaded datasets related to the Annual Reports
rm(results_total)
results_total <- bind_rows(result_gb_ceo, result_gb_nb)

#Split the report_type variable in two: Company and report type, add back to df
company <- character()
report_type <- character()
for (a in 1:nrow(results_total)) {
  company[a] <- stri_split(results_total$report, regex='_')[[a]][1]
  report_type[a] <- paste(stri_split(results_total$report, regex='_')[[a]][2],
                      stri_split(results_total$report, regex='_')[[a]][3],
                     sep = '_')
}

company <- as.data.frame(company)
report_type <- as.data.frame(report_type)
results_total <- results_total %>% 
  bind_cols(company, report_type)

rm(filtered)
#Create a graph to look at the net_sum_weight for all report types for the same companies
columns_of_interest <- results_total[,10:12]
filtered <-  results_total[,10:12] %>% 
   dplyr::filter(company %in% (columns_of_interest %>% 
                                 dplyr::count(company) %>% 
                            dplyr::filter(n>1))$company) 

filtered$company <- factor(filtered$company)
company_names <- levels(filtered$company)

filtered$company <- as.numeric(filtered$company)

ggplot(filtered, aes(company,net_sum_weight, fill=report_type))+
    geom_area()+
 scale_x_continuous(breaks=seq(from=1, to=31, by=1), 
                  labels=c('Aurubis', 'Brenntag', 'Commerzbank', 'Delivery Hero', 'Deutsche Pfandbriefbank', 'Deutsche Wohnen',
                           'Dürr','Evonik Industries', 'Fraport', 'freenet', 'Fuchs Petrolub',  'GEA Group','Gerresheimer',
                           'HELLA GmbH & Co', 'Hochtief', 'Hugo Boss','K+S',  'Lanxess', 'Morphosys', 
                           'MTU Aero Systems', 'Nemetschek', 'ProSiebenSat.1 Media', 'Puma', 'Rheinmetall', 'Sartorius',
                           'Siltronic' ,'Software','Symrise', 'Uniper','Wacker Chemie', 'Zalando'))+
  geom_hline(yintercept=0, color='black', alpha=0.4)+
  coord_flip()+
  scale_fill_viridis(discrete = T,
                     name=NULL,
                     labels=c('Annual Report (CEO)', 'Annual Report (Sustainability)'))+  theme(
        text = element_text(family='Times', size = 20), 
        legend.position = 'bottom',
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 20)
  )+
  labs(y='Weighted net sentiment in absolute values',
       x= 'Company',
       fill='Report type')

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/net_sum_weight_gbs.pdf')

filtered %>% 
  ggplot(., aes(reorder(as.numeric(company), net_sum_weight), net_sum_weight, fill=report_type))+
  geom_bar(stat = 'identity')+
  geom_hline(yintercept = 0, alpha=0.7)+
  coord_flip()+
  scale_x_discrete(breaks=seq(from=1, to=31, by=1), 
                     labels=c('Aurubis', 'Brenntag', 'Commerzbank', 'Delivery Hero', 'Deutsche Pfandbriefbank', 'Deutsche Wohnen',
                              'Dürr','Evonik Industries', 'Fraport', 'freenet', 'Fuchs Petrolub',  'GEA Group','Gerresheimer',
                              'HELLA GmbH & Co', 'Hochtief', 'Hugo Boss','K+S',  'Lanxess', 'Morphosys', 
                              'MTU Aero Systems', 'Nemetschek', 'ProSiebenSat.1 Media', 'Puma', 'Rheinmetall', 'Sartorius',
                              'Siltronic' ,'Software','Symrise', 'Uniper','Wacker Chemie', 'Zalando'))+
  scale_fill_viridis(discrete = T,
                     name=NULL,
                     labels=c('Annual Report (CEO)', 'Annual Report (Sustainability)'))+
  theme(axis.text = element_text(size = 20),
        text = element_text(family='Times'),
        panel.background = element_rect(fill = 'grey97'), 
        axis.title = element_text(size = 20), 
        legend.position = 'bottom', 
        axis.text.x = element_text(vjust = 2), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 20))+
  labs(y='Weighted net sentiment in absolute values',
       x= 'Company')

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/barplot_gb.pdf')

gb_ceo_all %>% 
  filter(report=='wackerchemie_gb_ceo') %>% 
  arrange(desc(weighted_sent)) %>% 
  head(n=10) %>% 
  ggplot(aes(reorder(word, weighted_sent), weighted_sent, fill=sentiment))+
  geom_bar( stat = 'identity')+
  scale_fill_viridis(discrete = F, option = "D")+
  coord_flip()+
  theme(axis.text = element_text(size = 20),
        text = element_text(family='Times'),
        panel.background = element_rect(fill = 'grey97'), 
        axis.title = element_text(size = 20), 
        legend.position = 'bottom', 
        axis.text.x = element_text(vjust = 2), 
        legend.text = element_text(size = 20, angle = -315), 
        legend.title = element_text(size = 20))+
  labs(y='Weighted net sentiment in absolute values',
       x= 'Word', 
       fill='Original sentiment')

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/sentiment_words_wacker.pdf')

gb_ceo_all %>% 
  filter(report=='zalando_gb_ceo') %>% 
  arrange(desc(weighted_sent)) %>% 
  head(n=10) %>% 
  ggplot(aes(reorder(word, weighted_sent), weighted_sent, fill=sentiment))+
  geom_bar( stat = 'identity')+
  scale_fill_viridis(discrete = F, option = "D")+
  coord_flip()+
  theme(axis.text = element_text(size = 20),
        text = element_text(family='Times'),
        panel.background = element_rect(fill = 'grey97'), 
        axis.title = element_text(size = 20, vjust = 2), 
        legend.position = 'bottom', 
        axis.text.x = element_text(vjust = 2), 
        legend.text = element_text(size = 20, angle = -315), 
        legend.title = element_text(size = 20))+
  labs(y='Weighted net sentiment in absolute values',
       x= 'Word', 
       fill='Original sentiment')

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/sentiment_words_zalando.pdf')

gb_ceo_all %>% 
  filter(report=='symrise_gb_ceo') %>% 
  arrange(desc(weighted_sent)) %>% 
  head(n=10) %>% 
  ggplot(aes(reorder(word, weighted_sent), weighted_sent, fill=sentiment))+
  geom_bar( stat = 'identity')+
  scale_fill_viridis(discrete = F, option = "D")+
  coord_flip()+
  theme(axis.text = element_text(size = 20),
        text = element_text(family='Times'),
        panel.background = element_rect(fill = 'grey97'), 
        axis.title = element_text(size = 20), 
        legend.position = 'bottom', 
        axis.text.x = element_text(vjust = 2), 
        legend.text = element_text(size = 20, angle = -315), 
        legend.title = element_text(size = 20))+
  labs(y='Weighted net sentiment in absolute values',
       x= 'Word', 
       fill='Original sentiment')

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/sentiment_words_symrise.pdf')

gb_ceo_all %>% 
  filter(report=='p7s1_gb_ceo') %>% 
  arrange(desc(weighted_sent)) %>% 
  head(n=10) %>% 
  ggplot(aes(reorder(word, weighted_sent), weighted_sent, fill=sentiment))+
  geom_bar( stat = 'identity')+
  scale_fill_viridis(discrete = F, option = "D")+
  coord_flip()+
  theme(axis.text = element_text(size = 20),
        text = element_text(family='Times'),
        panel.background = element_rect(fill = 'grey97'), 
        axis.title = element_text(size = 20), 
        legend.position = 'bottom', 
        axis.text.x = element_text(vjust = 2), 
        legend.text = element_text(size = 20, angle = -315), 
        legend.title = element_text(size = 20))+
  labs(y='Weighted net sentiment in absolute values',
       x= 'Word', 
       fill='Original sentiment')

gb_ceo_all %>%
  filter(report=='zalando_gb_ceo') %>% 
  filter(weighted_sent>0) %>% 
  dplyr::count(weighted_sent) %>% 
  summarise(sum(n))

gb_ceo_all %>% 
  filter(report=='aurubis_gb_ceo') %>% 
  arrange((weighted_sent)) %>% 
  head(n=10) %>% 
  ggplot(aes(reorder(word, weighted_sent), weighted_sent, fill=sentiment))+
  geom_bar( stat = 'identity')+
  scale_fill_viridis(discrete = F, option = "D")+
  coord_flip()+
  theme(axis.text = element_text(size = 20),
        text = element_text(family='Times'),
        panel.background = element_rect(fill = 'grey97'), 
        axis.title = element_text(size = 20), 
        legend.position = 'bottom', 
        axis.text.x = element_text(vjust = 2), 
        legend.text = element_text(size = 20, angle = -315), 
        legend.title = element_text(size = 20))+
  labs(y='Weighted net sentiment in absolute values',
       x= 'Word', 
       fill='Original sentiment')

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/sentiment_aurubis.pdf')


gb_ceo_all %>% 
  filter(report=='commerzbank_gb_ceo') %>% 
  arrange(desc(weighted_sent)) %>% 
  head(n=10) %>% 
  ggplot(aes(reorder(word, weighted_sent), weighted_sent, fill=sentiment))+
  geom_bar( stat = 'identity')+
  scale_fill_viridis(discrete = F, option = "D")+
  coord_flip()+
  theme(axis.text = element_text(size = 20),
        text = element_text(family='Times'),
        panel.background = element_rect(fill = 'grey97'), 
        axis.title = element_text(size = 20), 
        legend.position = 'bottom', 
        axis.text.x = element_text(vjust = 2), 
        legend.text = element_text(size = 18, angle = -315), 
        legend.title = element_text(size = 20))+
  labs(y='Weighted net sentiment in absolute values',
       x= 'Word', 
       fill='Original sentiment')

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/negative_commerzbank.pdf')


filtered %>% 
  group_by(report_type) %>% 
  summarise(mean(net_sum_weight))

#Create a graph to look at the sum_pos_weight for all report types for the same companies
columns_of_interest <- results_total[,c(9,11,12)]
filtered <-  results_total[,c(9,11,12)] %>% 
  filter(company %in% (columns_of_interest %>% 
                         dplyr::count(company) %>% 
                         filter(n>1))$company) 

filtered$company <- factor(filtered$company)
company_names <- levels(filtered$company)



filtered$company <- as.numeric(filtered$company)

ggplot(filtered, aes(company,sum_pos_weight, fill=report_type))+
  geom_area()+
  scale_x_continuous(breaks=seq(from=1, to=31, by=1), 
                     labels=c('Aurubis', 'Brenntag', 'Commerzbank', 'Delivery Hero', 'Deutsche Pfandbriefbank', 'Deutsche Wohnen',
                              'Dürr','Evonik Industries', 'Fraport', 'freenet', 'Fuchs Petrolub',  'GEA Group','Gerresheimer',
                              'HELLA GmbH & Co', 'Hochtief', 'Hugo Boss','K+S',  'Lanxess', 'Morphosys', 
                              'MTU Aero Systems', 'Nemetschek', 'ProSiebenSat.1 Media', 'Puma', 'Rheinmetall', 'Sartorius',
                              'Siltronic' ,'Software','Symrise', 'Uniper','Wacker Chemie', 'Zalando'))+
  geom_hline(yintercept=0, color='black', alpha=0.4)+
  coord_flip()+
scale_fill_viridis(discrete = T,
                   name=NULL,
                   labels=c('Annual Report (CEO)', 'Annual Report (Sustainability)'))+
  theme(axis.text = element_text(size = 20),
        text = element_text(family='Times'),
        panel.background = element_rect(fill = 'grey97'), 
        axis.title = element_text(size = 20), 
        legend.position = 'bottom', 
        axis.text.x = element_text(vjust = 2), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 20))+
  labs(y='Weighted positive sentiment in absolute values',
       x= 'Company')

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/sum_pos_weight_gbs.pdf')

#Create a graph to look at the sum_neg_weight for all report types for the same companies
columns_of_interest <- results_total[,c(8,11,12)]
filtered <-  results_total[,c(8,11,12)] %>% 
  filter(company %in% (columns_of_interest %>% 
                         dplyr::count(company) %>% 
                         filter(n>1))$company) 

filtered$company <- factor(filtered$company)
company_names <- levels(filtered$company)

filtered$company <- as.numeric(filtered$company)

names_company <- c('Aurubis', 'Brenntag', 'Commerzbank', 'Delivery Hero', 'Deutsche Pfandbriefbank', 'Deutsche Wohnen',
                   'Dürr','Evonik Industries', 'Fraport', 'freenet', 'Fuchs Petrolub',  'GEA Group','Gerresheimer',
                   'HELLA GmbH & Co', 'Hochtief', 'Hugo Boss','K+S',  'Lanxess', 'Morphosys', 
                   'MTU Aero Systems', 'Nemetschek', 'ProSiebenSat.1 Media', 'Puma', 'Rheinmetall', 'Sartorius',
                   'Siltronic' ,'Software','Symrise', 'Uniper','Wacker Chemie', 'Zalando')
length(names_company)
ggplot(filtered, aes(company,sum_neg_weight, fill=report_type))+
  geom_area()+
  scale_x_continuous(breaks=seq(from=1, to=31, by=1), 
                     labels=names_company)+
  geom_hline(yintercept=0, color='black', alpha=0.4)+
  coord_flip()+
  scale_fill_viridis(discrete = T,
                     name=NULL,
                     labels=c('Annual Report (CEO)', 'Annual Report (Sustainability)'))+
  theme(axis.text = element_text(size = 20),
        text = element_text(family='Times'),
        panel.background = element_rect(fill = 'grey97'), 
        axis.title = element_text(size = 20), 
        legend.position = 'bottom', 
        axis.text.x = element_text(vjust = 2), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 20))+
  labs(y='Weighted positive sentiment in absolute values',
       x= 'Company')
ggsave('/Users/Constanze/Desktop/uni/ba arbeit/sum_neg_weight_gbs.pdf')

#Seem to have a correlation of net sentiment with length
cor.test(results_total[report_type=='gb_nb',]$total, results_total[report_type=='gb_nb',]$net_sum_weight)
cor.test(results_total[report_type=='gb_ceo',]$total, results_total[report_type=='gb_ceo',]$net_sum_weight)

ggplot(results_total, aes(total, net_sum_weight, color=report_type))+
  geom_point()+
  geom_smooth(method = 'lm')+
  scale_x_continuous(trans = 'log2')+
  geom_hline(yintercept = 0, alpha=0.4)


##Recreate for the nb files as well
rm(results_total_nb)
results_total_nb <- bind_rows(result_nb_ceo, result_nb_nb)

company <- character()
report_type <- character()
for (a in 1:nrow(results_total_nb)) {
  company[a] <- stri_split(results_total_nb$report, regex='_')[[a]][1]
  report_type[a] <- paste(stri_split(results_total_nb$report, regex='_')[[a]][2],
                          stri_split(results_total_nb$report, regex='_')[[a]][3],
                          sep = '_')
}

company <- as.data.frame(company)
report_type <- as.data.frame(report_type)



results_total_nb <- results_total_nb %>% 
  bind_cols(company, report_type)

rm(filtered)
#Create a graph to look at the net_sum_weight for all report types for the same companies
columns_of_interest <- results_total_nb[,10:12]
filtered <-  results_total_nb[,10:12] %>% 
  filter(company %in% (columns_of_interest %>% 
                         dplyr::count(company) %>% 
                         filter(n>1))$company) 

filtered$company <- factor(filtered$company, ordered = T)
company_names <- levels(filtered$company)

names_company <- c('Alstria Office', 'Aurubis', 'Bechtle', 'Brenntag', '1&1 Drillisch', 'Fielmann', 'Fraport', 'Fuchs Petrolub', 
                   'Hugo Boss', 'Innogy', 'Kion Group',  'LEG Immobilien', 'Metro','Norma Group', 'Osram Licht', 'Scout24',
                   'Siltronic' ,'TAG Immobilien','Telefonica Deutschland', 'Uniper','United Internet')

filtered$company <- as.numeric(filtered$company)

ggplot(filtered, aes(x=company, y=net_sum_weight,fill=report_type))+
  geom_area()+
 scale_x_continuous(breaks=seq(from=1, to=21, by=1),
                   labels=names_company)+
 # scale_x_discrete(labels=company_names)+
  geom_hline(yintercept=0, color='black', alpha=0.4)+
  coord_flip()+
  scale_fill_viridis(discrete = T,
                     name=NULL,
                     labels=c('Sustainability Report (CEO)', 'Sustainability Report (Total)'))+
  theme(axis.text = element_text(size = 20),
        text = element_text(family='Times'),
        panel.background = element_rect(fill = 'grey97'), 
        axis.title = element_text(size = 20), 
        legend.position = 'bottom', 
        axis.text.x = element_text(vjust = 2), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 20))+
  labs(y='Weighted net sentiment in absolute values',
       x= 'Company')

filtered %>% 
  ggplot(., aes(reorder(as.numeric(company), net_sum_weight), net_sum_weight, fill=report_type))+
  geom_bar(stat = 'identity')+
  geom_hline(yintercept = 0, alpha=0.7)+
  coord_flip()+
  scale_x_discrete(breaks=seq(from=1, to=21, by=1),
                   labels=names_company)+
  scale_fill_viridis(discrete = T,
                     name=NULL,
                     labels=c('Sustainability Report (CEO)', 'Sustainability Report (Total)'))+
  theme(axis.text = element_text(size = 20),
        text = element_text(family='Times'),
        panel.background = element_rect(fill = 'grey97'), 
        axis.title = element_text(size = 20), 
        legend.position = 'bottom', 
        axis.text.x = element_text(vjust = 2), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 20))+
  labs(y='Weighted net sentiment in absolute values',
       x= 'Company')

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/barplot_nb.pdf')


filtered %>% 
  filter(company==3)

max(filtered$net_sum_weight)
 filtered %>% 
   filter(report_type=='nb_ceo'&net_sum_weight<0)
 
 filtered %>% 
   filter(report_type=='nb_ceo') %>% 
   ggplot(., aes(reorder(company, net_sum_weight), net_sum_weight))+
   geom_bar(stat = 'identity')+
   scale_x_discrete(breaks=seq(from=1, to=21, by=1),labels=company_names)+
   coord_flip()
 
 
 ##show the most important sentiment words for the most positive CEO letters
 nb_ceo_all %>% 
   filter(report=='bechtle_nb_ceo'|report=='metro_nb_ceo'|report=='legimmobilien_nb_ceo') %>% 
   arrange(desc(weighted_sent)) %>% 
   head(n=10) %>% 
   ggplot(aes(reorder(word, weighted_sent), weighted_sent, fill=report))+
   geom_bar( stat = 'identity', position = 'dodge')+
   scale_fill_viridis(discrete = T, option = "D")+
   coord_flip()
 
 nb_ceo_all %>% 
   filter(report=='bechtle_nb_ceo'|report=='metro_nb_ceo'|report=='legimmobilien_nb_ceo') %>% 
   arrange((weighted_sent)) %>% 
   head(n=10) %>% 
   ggplot(aes(reorder(word, weighted_sent), weighted_sent, fill=report))+
   geom_bar( stat = 'identity', position = 'dodge')+
   scale_fill_viridis(discrete = T, option = "D")+
   coord_flip()
 
 ##show the most important sentiment words for the most negative CEO letters
  nb_ceo_all %>% 
   filter(report=='innogy_nb_ceo'|report=='alstria_nb_ceo'|report=='fraport_nb_ceo') %>% 
   arrange(desc(weighted_sent)) %>% 
   head(n=10) %>% 
   ggplot(aes(reorder(word, weighted_sent), weighted_sent, fill=report))+
   geom_bar( stat = 'identity')+
    scale_fill_viridis(discrete = T,
                       name=NULL,
                       labels=c('Alstria (Sustainability CEO)',  
                                'Fraport (Sustainability CEO)', 
                                'Innogy (Sustainability CEO)'))+
    theme(axis.text = element_text(size = 20),
          text = element_text(family='Times'),
          panel.background = element_rect(fill = 'grey97'), 
          axis.title = element_text(size = 20), 
          legend.position = 'bottom', 
          axis.text.x = element_text(vjust = 2), 
          legend.text = element_text(size = 20), 
          legend.title = element_text(size = 20))+
    labs(y='Weighted sentiment score in absolute values',
         x= 'Word')+
    coord_flip()
  
  ggsave('/Users/Constanze/Desktop/uni/ba arbeit/nb_ceo_most_sentiment.pdf')
 
  nb_ceo_all %>% 
    filter(report=='innogy_nb_ceo'|report=='alstria_nb_ceo'|report=='fraport_nb_ceo') %>% 
    arrange((weighted_sent)) %>% 
    head(n=10) %>% 
    ggplot(aes(reorder(word, weighted_sent), weighted_sent, fill=report))+
    geom_bar( stat = 'identity')+
    scale_fill_viridis(discrete = T,
                       name=NULL,
                       labels=c('Alstria (Sustainability CEO)',  
                                'Fraport (Sustainability CEO)', 
                                'Innogy (Sustainability CEO)'))+
    theme(axis.text = element_text(size = 20),
          text = element_text(family='Times'),
          panel.background = element_rect(fill = 'grey97'), 
          axis.title = element_text(size = 20), 
          legend.position = 'bottom', 
          axis.text.x = element_text(vjust = 2), 
          legend.text = element_text(size = 20), 
          legend.title = element_text(size = 20))+
    labs(y='Weighted sentiment score in absolute values',
         x= 'Word')+
    coord_flip()    
  
  ggsave('/Users/Constanze/Desktop/uni/ba arbeit/nb_ceo_least_sentiment.pdf')
  
  
  ###Looking in detail at INnogy
  nb_nb_all %>% 
    filter(report=='innogy_nb_nb') %>% 
    arrange((weighted_sent)) %>% 
    head(n=10) %>% 
    ggplot(aes(reorder(word, weighted_sent), weighted_sent, fill=report))+
    geom_bar( stat = 'identity')+
    scale_fill_viridis(discrete = T, option = "D")+
    coord_flip()+
    theme(legend.position = 'none',
          text = element_text(family='Times', size = 20))+
    labs(y='Weighted sentiment score in absolute values',
         x= 'Word')       
 


  nb_ceo_all %>% 
  filter(report=='bechtle_nb_ceo') %>% 
  arrange(desc(weighted_sent)) %>% 
  head(n=10) %>% 
  ggplot(aes(reorder(word, weighted_sent), weighted_sent, fill=sentiment))+
  geom_bar( stat = 'identity')+
  scale_fill_viridis(discrete = F, option = "D")+
  coord_flip()

nb_ceo_all %>% 
  filter(report=='innogy_nb_ceo') %>% 
  arrange(desc(weighted_sent)) %>% 
  head(n=10) %>% 
  ggplot(aes(reorder(word, weighted_sent), weighted_sent, fill=sentiment))+
  geom_bar( stat = 'identity')+
  scale_fill_viridis(discrete = F, option = "D", 
                     name='Original \nSentiment')+
  coord_flip()+
  theme(legend.position = 'right',
        text = element_text(family='Times', size = 20))+
  labs(y='Weighted sentiment score in absolute values',
       x= 'Word')  

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/innogy_words.pdf')

nb_ceo_all %>% 
  filter(report=='alstria_nb_ceo') %>% 
  arrange(desc(weighted_neg)) %>% 
  head(n=10) %>% 
  ggplot(aes(reorder(word, weighted_neg), weighted_neg, fill=sentiment))+
  geom_bar( stat = 'identity')+
  scale_fill_viridis(discrete = F, option = "D")+
  coord_flip()

nb_ceo_all %>% 
  filter(report=='aurubis_nb_ceo') %>% 
  arrange(desc(weighted_neg)) %>% 
  head(n=10) %>% 
  ggplot(aes(reorder(word, weighted_neg), weighted_neg, fill=sentiment))+
  geom_bar( stat = 'identity')+
  scale_fill_viridis(discrete = F, option = "D")+
  coord_flip()

nb_ceo_all %>% 
  filter(report=='unitedinternet_nb_ceo') %>% 
  arrange(desc(weighted_sent)) %>% 
  head(n=10) %>% 
  ggplot(aes(reorder(word, weighted_sent), weighted_sent, fill=sentiment))+
  geom_bar( stat = 'identity')+
  scale_fill_viridis(discrete = F, option = "D")+
  coord_flip()

nb_ceo_all %>% 
  filter(report=='uniper_nb_ceo') %>% 
  arrange(desc(weighted_sent)) %>% 
  head(n=10) %>% 
  ggplot(aes(reorder(word, weighted_sent), weighted_sent, fill=sentiment))+
  geom_bar( stat = 'identity')+
  scale_fill_viridis(discrete = F, option = "D")+
  coord_polar()


filtered %>% 
  group_by(report_type) %>% 
  summarise(mean(net_sum_weight))

#Create a graph to look at the sum_pos_weight for all report types for the same companies
columns_of_interest <- results_total_nb[,c(9,11,12)]
filtered <-  results_total_nb[,c(9,11,12)] %>%
  filter(company %in% (columns_of_interest %>%
                         dplyr::count(company) %>%
                         filter(n>1))$company)

filtered$company <- factor(filtered$company)
company_names <- levels(filtered$company)

filtered$company <- as.numeric(filtered$company)

ggplot(filtered, aes(company,sum_pos_weight, fill=report_type))+
  geom_area()+
  scale_x_continuous(breaks=seq(from=1, to=21, by=1),
                     labels=names_company)+
  geom_hline(yintercept=0, color='black', alpha=0.4)+
  coord_flip()+
  scale_fill_viridis(discrete = T, 
                     name=NULL, 
                     labels=c('Sustainability (CEO)',
                              'Sustainability (Total)'))+
  theme(legend.position = 'bottom',
        text = element_text(family='Times', size = 20))+
  labs(y='Sum of weighted positive scores',
       x= 'Company')  

 ggsave('/Users/Constanze/Desktop/uni/ba arbeit/sum_pos_weight_nbs.pdf')

##Join everything together

all_results <- bind_rows(results_total, results_total_nb)

#descriptive statistics

descriptive_all_results <- all_results %>% 
  group_by(company) %>% 
  summarise(mean=mean(net_sum_weight)) 

means <- all_results[-1] %>% 
  group_by(report_type) %>% 
  summarise(mean(positivity_in_perc),
            mean(negativity_in_perc), 
            mean(net_sent_in_perc), 
            mean(sum_pos_weight),
            mean(sum_neg_weight),
            mean(net_sum_weight))


all_results %>% 
  filter(report_type=='gb_ceo') %>% 
  ggplot(., aes(reorder(company, net_sent_in_perc), net_sent_in_perc, fill=negativity_in_perc)) +
  geom_bar(stat = 'identity')+
  coord_flip()

library('viridis')

report_names <- c(
  'gb_nb'="Annual Report (Sustainability)",
  'gb_ceo'="Annual Report (CEO)",
  'nb_ceo'="Sustainability (CEO)",
  'nb_nb'="Sustainability (Total)"
)

company_names <- all_results[-1] %>% 
  distinct(company)

company_names$real_name <- c('Aurubis', 'Axel Springer', 'Bechtle', 'Brenntag', 'Commerzbank', 'CTS Eventim',
                             'Delivery Hero', 'Deutsche Euroshop', 'Deutsche Pfandbriefbank', 'Deutsche Wohnen', 
                             'Dürr', '1&1 Drillisch', 'Evonik Industries', 'Evotec', 'Fielmann', 'Fraport', 'freenet', 
                             'Fuchs Petrolub', 'GEA Group', 'Gerresheimer', 'Hannover Rück', 'HELLA GmbH&Co', 'Hochtief',
                             'Hugo Boss', 'Innogy', 'Kion Group', 'K+S', 'LANXESS', 'LEG Immobilien', 'Metro', 'Morphosys',
                             'MTU Aero Systems', 'Nemetschek', 'Norma Group', 'ProSiebenSat.1 Media', 'Puma', 'Rheinmetall', 
                             'Rocket Internet', 'Salzgitter', 'Sartorius', 'Schaeffler', 'Scout24', 'Siemens Healthineers',
                             'Siltronic', 'Software', 'Symrise', 'TAG Immobilien','Telefonica Deutschland', 'Uniper', 
                             'United Internet', 'Wacker Chemie', 'Zalando', 'Deutsche EuroShop', 'Osram Licht',
                             'Alstria Office', 'Aareal Bank')
company_names$real_name

all_results <- left_join(all_results, company_names)

library(dplyr)
all_results %>% 
  ggplot(., aes(reorder((real_name), net_sum_weight), net_sum_weight, fill=net_sent_in_perc)) +
  geom_bar(stat = 'identity')+
  scale_fill_viridis(option = "D")+
  theme_minimal() +
  theme(legend.position = "bottom")+
  geom_hline(yintercept = 0, color='darkred', alpha=.6)+
  coord_flip()+
  facet_wrap(facets = vars(report_type), nrow = 2, labeller = as_labeller(report_names), scales = 'free')+
  theme(panel.background = element_rect(color='grey97'),
        text=element_text(family = 'Times', size = 18, lineheight = 0.5))+
  labs(x='Company',
       y='Weighted net sentiment', 
       fill='Net sentiment in percent')

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/net_sum_weight_per_report.pdf', height = 50, width = 35, units = 'cm')

all_results %>% 
  ggplot(., aes(reorder((real_name), net_sent_in_perc), net_sent_in_perc, fill=net_sum_weight)) +
  geom_bar(stat = 'identity')+
  scale_fill_viridis(option = "D", breaks=seq(from=1, to=56, by=1), labels=company_names$real_name)+
  theme_minimal() +
  theme(legend.position = "bottom")+
  geom_hline(yintercept = 0, color='darkred', alpha=.6)+
  coord_flip()+
  facet_wrap(facets = vars(report_type), nrow = 2,labeller = as_labeller(report_names), scales='free')+
  theme(panel.background = element_rect(color='grey97'),
        text=element_text(family = 'Times', size = 18, lineheight = 0.5), 
        legend.text = element_text(angle = 45))+
  labs(x='Company',
       y='Weighted net sentiment', 
       fill='Net sentiment in percent')

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/net_sent_in_perc_per_report.pdf', height = 50, width = 35, units = 'cm')

cor(all_results$net_sent_in_perc, all_results$net_sum_weight)
cor.test(all_results$net_sent_in_perc, all_results$net_sum_weight)
cov(all_results$net_sent_in_perc, all_results$net_sum_weight)
cor.test(all_results$net_sent_in_perc, all_results$net_sum_weight, method = 'spearman')


ggplot(all_results, aes(net_sum_weight, net_sent_in_perc, color=report_type))+
  scale_color_viridis(discrete = T, option = "D", 
                      name=NULL,
                      labels=c('AR (CEO)',
                               'AR (Sustainability)',
                               'SR (CEO)', 
                               'SR (Total)'))+
  geom_point()+
  geom_smooth(method = 'lm', alpha=0, size=0.3, span=0.5)+
  theme(panel.background = element_rect(color='grey97'),
        text=element_text(family = 'Times', size = 20), 
        legend.position = 'bottom')+
  labs(x='Weighted net sentiment',
       y='Net sentiment in percent', 
       col='Report type')

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/scatterplot.pdf')

#Test correlation between two sentiment metrics, weighted and unweighted 
all_results %>% 
  group_by(report_type) %>% 
  dplyr::summarise(correlation=cor.test(net_sent_in_perc,net_sum_weight, method = 'pearson')$estimate, 
                   corr_p=cor.test(net_sent_in_perc,net_sum_weight, method = 'pearson')$p.value,
                   spearman_rank=cor.test(net_sent_in_perc, net_sum_weight, method = 'spearman')$estimate,
                   spearman_p=cor.test(net_sent_in_perc, net_sum_weight, method = 'spearman')$p.value)


mean_sentiments_gb <- all_results[-1] %>% 
  filter(report_type=='gb_ceo'|
           report_type=='gb_nb') %>% 
  filter(company %in% (all_results[-1] %>% 
                         filter(report_type=='gb_ceo'|
                                  report_type=='gb_nb') %>% 
                         dplyr::count(company) %>% 
                         filter(n>1))$company) %>% 
  group_by(report_type) %>% 
  summarise(mean(positivity_in_perc), 
            mean(negativity_in_perc), 
            mean(net_sent_in_perc), 
            mean(sum_neg_weight), 
            mean(sum_pos_weight),
            mean(net_sum_weight))


mean_sentiments_nb <- all_results[-1] %>% 
  filter(report_type=='nb_ceo'|
           report_type=='nb_nb') %>% 
  filter(company %in% (all_results[-1] %>% 
                         filter(report_type=='nb_ceo'|
                                  report_type=='nb_nb') %>% 
                         dplyr::count(company) %>% 
                         filter(n>1))$company) %>% 
  group_by(report_type) %>% 
  summarise(mean(positivity_in_perc), 
            mean(negativity_in_perc), 
            mean(net_sent_in_perc), 
            mean(sum_neg_weight), 
            mean(sum_pos_weight),
            mean(net_sum_weight))

difference_in_length <- all_results[-1] %>% 
  filter(report_type=='gb_nb'|
           report_type=='nb_nb') %>% 
  filter(company %in% (all_results[-1] %>% 
                         filter(report_type=='gb_nb'|
                                  report_type=='nb_nb') %>% 
                         dplyr::count(company) %>% 
                         filter(n>1))$company) %>% 
  group_by(report_type) %>% 
  summarise(mean(positivity_in_perc), 
            mean(negativity_in_perc), 
            mean(net_sent_in_perc), 
            mean(sum_neg_weight), 
            mean(sum_pos_weight),
            mean(net_sum_weight)) 

#NB section and NB rport compared- percentage of sentiment words is pretty equal
difference_in_length %>% 
dplyr::select(-c(`mean(sum_neg_weight)`, `mean(sum_pos_weight)`,`mean(net_sum_weight)`)) %>% 
  gather(key='metric', value = 'value', -report_type) %>% 
  ggplot(., aes(report_type, value, fill= metric))+
  scale_fill_viridis(discrete = T, option = "D")+
  geom_bar(position = 'fill', stat = 'identity')


ggsave('/Users/Constanze/Desktop/uni/ba arbeit/proportion_nbs_fill.pdf')

difference_in_length %>% 
  select(-c(`mean(sum_neg_weight)`, `mean(sum_pos_weight)`,`mean(net_sum_weight)`)) %>% 
  gather(key='metric', value = 'value', -report_type) %>% 
  ggplot(., aes(report_type, value, fill= metric))+
  scale_fill_viridis(discrete = T, option = "D")+
  geom_bar(position = 'dodge', stat = 'identity')

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/proportion_nbs_dodge.pdf')

  
#weighted sentiment scores
difference_in_length %>% 
  select(-c(`mean(positivity_in_perc)`, `mean(negativity_in_perc)`, `mean(net_sent_in_perc)`)) %>% 
  gather(key='metric', value = 'value', -report_type) %>% 
  ggplot(., aes(report_type, value, fill= metric))+
  scale_fill_viridis(discrete = T, option = "D")+
  geom_bar(position = 'fill', stat = 'identity')

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/score_nbs_fill.pdf')

difference_in_length %>% 
  select(-c(`mean(positivity_in_perc)`, `mean(negativity_in_perc)`, `mean(net_sent_in_perc)`)) %>% 
  gather(key='metric', value = 'value', -report_type) %>% 
  ggplot(., aes(report_type, value, fill= metric))+
  scale_fill_viridis(discrete = T, option = "D")+
  geom_bar(position = 'dodge', stat = 'identity')

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/score_nb_dodge.pdf')

#Not looking on aggregate but individual level
all_results[-1] %>% 
  filter(report_type=='gb_nb'|
           report_type=='nb_nb') %>% 
  filter(company %in% (all_results[-1] %>% 
                         filter(report_type=='gb_nb'|
                                  report_type=='nb_nb') %>% 
                         dplyr::count(company) %>% 
                         filter(n>1))$company) %>% 
  group_by(company, report_type) %>% 
  summarise(pos=sum(sum_pos_weight), 
            neg=sum(sum_neg_weight),
            net=sum(net_sum_weight)) %>% 
  tidyr::gather(key = 'metric', value='value', -c(company, report_type)) %>% 
  ggplot(., aes(report_type, value, fill=metric))+
    geom_bar(position = 'dodge', stat = 'identity')+
    scale_fill_viridis(discrete = T, option = "D")+
    facet_wrap(facets =  vars(company), nrow = 3)

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/individual_nbs.pdf', height = 20, width = 30, units = 'cm')

## Descriptive statistics
all_results[-1] %>% 
  group_by(report_type) %>% 
  summarise(sum(total), 
            mean(total), 
            median(total), 
            min(total), 
            max(total))

all_results[-1] %>% 
  ggplot(., aes(total, fill=report_type))+
  geom_histogram(position = 'dodge')

all_results[-1] %>% 
  select(company, report_type) %>% 
  group_by(company) %>% 
  distinct()

all_results[-1] %>% 
  group_by(report_type) %>% 
  summarise(var(total))
  
##Comparing CEO letters

all_results[-1] %>% 
  filter((report_type=='nb_ceo'|report_type=='gb_ceo')&
           company %in% (all_results[-1] %>% 
                           filter(report_type=='nb_ceo'|report_type=='gb_ceo') %>% 
                           count(company) %>% 
                           filter(n==2))$company) %>% 
  ggplot(aes(reorder(capitalize(real_name),net_sum_weight), net_sum_weight, fill=report_type))+
  geom_bar(stat = 'identity', position = 'stack')+
  coord_flip()+
  scale_fill_viridis(discrete = T, 
                     name=NULL,
                     labels=c('Annual Report (CEO)', 'Sustainability report (CEO)'))+
  geom_hline(yintercept = 0, color='grey')+
  theme(panel.background = element_rect(color='grey97'),
        text=element_text(family = 'Times', size = 20), 
        legend.position = 'bottom')+
  labs(x='Company',
       y='Weighted net sentiment', 
       col='Report type')

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/ceoletters_weighted.pdf')

all_results[-1] %>% 
  filter((report_type=='nb_ceo'|report_type=='gb_ceo')&
           company %in% (all_results[-1] %>% 
                           filter(report_type=='nb_ceo'|report_type=='gb_ceo') %>% 
                           count(company) %>% 
                           filter(n==2))$company) %>% 
  ggplot(aes(reorder(((real_name)),net_sent_in_perc), net_sent_in_perc, fill=report_type))+
  geom_bar(stat = 'identity', position = 'stack')+
  coord_flip()+
  scale_fill_viridis(discrete = T,
                     name=NULL,
                     labels=c('Annual Report (CEO)', 'Sustainability report (CEO)'))+
  geom_hline(yintercept = 0, color='grey')+
  theme(panel.background = element_rect(color='grey97'),
        text=element_text(family = 'Times', size = 20), 
        legend.position = 'bottom')+
  labs(x='Company',
       y='Net sentiment in percent', 
       col='Report type')

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/ceoletters_net.pdf')

all_results[-1] %>% 
  filter((report_type=='nb_ceo'|report_type=='gb_ceo')&
           real_name %in% (all_results[-1] %>% 
                           filter(report_type=='nb_ceo'|report_type=='gb_ceo') %>% 
                           count(real_name) %>% 
                           filter(n==2))$real_name) %>% 
  group_by(real_name) %>% 
  summarise(sum_sent=sum(net_sum_weight)) %>% 
  ggplot(aes(reorder((real_name), sum_sent), sum_sent, fill="#440154FF"))+
  geom_bar(stat = 'identity')+
  coord_flip()+  
  scale_fill_viridis(option = "D", discrete = T)+
  geom_hline(yintercept = 0, color='grey')+
  theme(panel.background = element_rect(color='grey97'),
        text=element_text(family = 'Times', size = 20), 
        legend.position = 'none')+
  labs(x='Company',
       y='Weighted sentiment score')

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/ceolettersaddedsent.pdf')

############Cluster analysis
gradient.color <- list(low = "steelblue",  high = "white")

#filter for those who have two of the same reports
#as factor and scale too, then have no missing values. 
cluster_set <- all_results[-1] %>% 
  filter(company %in% (all_results[-1] %>% 
                         count(company) %>% 
                         filter(n>1))$company) %>% 
  select(-c(report_type, company, real_name)) %>% 
  scale() 

cluster_set %>% 
  get_clust_tendency(n=100, gradient = gradient.color)


#Cluster tendency is really bad, 0.1 

dist_all_results <- dist(cluster_set)
set.seed(222)
wards_cluster <- hclust(dist_all_results, method = 'ward.D2')
plot(wards_cluster)
