###Load necessary libraries 
library(readxl)
library(rio)
library('ape')
library('dendextend')
library('devtools')
library('plyr')
library(ggbiplot)
library(factoextra)
library('ggrepel')
library(tidyverse)
library(viridis)

###Read in EIKON data
rm(eikon_data)
eikon_number <- import('eikon/GridExport_July_13_2019_9_58_31.xlsx')
eikon_sector <- import('eikon/GridExport_July_22_2019_22_57_40.xlsx')

eikon_data <- left_join(eikon_number, eikon_sector)

eikon_data$company <- tolower(eikon_data$`Company Name`)

eikon_data <- eikon_data %>% 
  dplyr::mutate(
         company=gsub(x=company, pattern = 'group', replacement = ''),
         company=gsub(x=company, pattern = 'ag$', replacement = ''),
         company=gsub(x=company, pattern = 'se$', replacement = ''),
         company=trimws(company),
         company=ifelse(company=='fuchs petrolub', 'fuchs', company),
         company=ifelse(company=='aareal bank', 'aareal', company),
         company=ifelse(company=='1&1 drillisch', 'einsundeins', company),
         company=ifelse(company=='cts eventim ag & co kgaa', 'cts eventim', company),
         company=ifelse(company=='evonik industries', 'evonik', company),
         company=ifelse(company=='fraport ag frankfurt airport services worldwide', 'fraport', company),
         company=ifelse(company=='evonik industries', 'evonik', company),
         company=ifelse(company=='hella gmbh & co kgaa', 'hella', company),
         company=ifelse(company=='k&s', 'kunds', company),
         company=ifelse(company=='osram licht', 'osram', company),
         company=ifelse(company=='prosiebensat 1 media', 'p7s1', company),
         company=ifelse(company=='tag immobilien', 'tag', company),
         company=ifelse(company=='telefonica deutschland holding', 'telefonica', company),
         company=ifelse(company=='mtuaeroengines', 'mtuaerosystems', company),
         company=ifelse(company=='software', 'softwareag', company),
    company=ifelse(company=='deutscheeuroshop', 'deutscheuroshop', company),
    company=gsub(pattern = '\\s', replacement = '', x=company)
    )

eikon_data <- eikon_data[-1] %>% 
  distinct()


###Create the master dataset having the sentiment data and the ESG rating data in one df
company_esg <- all_results %>% 
  left_join(eikon_data, by=c('company'='company'))

###Filter resulting master data by report type
result_nb_nb_esg <- company_esg %>%
  filter(report_type=='nb_nb')

result_gb_nb_esg <- company_esg %>%
  filter(report_type=='gb_nb')

result_nb_ceo_esg <- company_esg %>%
  filter(report_type=='nb_ceo')

result_gb_ceo_esg <- company_esg %>%
  filter(report_type=='gb_ceo')

rows <- names(company_esg[c(2:10)])


###Test correlation of Sentiment values and ESG rating scores
correlation_nb_nb <- sapply(result_nb_nb_esg[15:22], cor, result_nb_nb_esg[2:10], use='complete.obs')
correlation_gb_nb <- sapply(result_gb_nb_esg[15:22], cor, result_gb_nb_esg[2:10], use='complete.obs')
correlation_nb_ceo <- sapply(result_nb_ceo_esg[15:22], cor, result_nb_ceo_esg[2:10], use='complete.obs')
correlation_gb_ceo <- sapply(result_gb_ceo_esg[15:22], cor, result_gb_ceo_esg[2:10], use='complete.obs')

correlation_nb_nb_s <- sapply(result_nb_nb_esg[15:22], cor, result_nb_nb_esg[2:10], use='complete.obs', method='spearman')
correlation_gb_nb_s <- sapply(result_gb_nb_esg[15:22], cor, result_gb_nb_esg[2:10], use='complete.obs', method='spearman')
correlation_nb_ceo_s <- sapply(result_nb_ceo_esg[15:22], cor, result_nb_ceo_esg[2:10], use='complete.obs', method='spearman')
correlation_gb_ceo_s <- sapply(result_gb_ceo_esg[15:22], cor, result_gb_ceo_esg[2:10], use='complete.obs', method='spearman')

rownames(correlation_nb_nb_s) <- c("Total words", "Sum positive words", "Positivity in %",
                                   "Sum negative words", "Negativity in %", "Net sentiment in %",
                                   "Weighted sum negative", "Weighted sum positive", "Weighted net sentiment")
rownames(correlation_gb_nb_s) <- c("Total words", "Sum positive words", "Positivity in %",
                                   "Sum negative words", "Negativity in %", "Net sentiment in %",
                                   "Weighted sum negative", "Weighted sum positive", "Weighted net sentiment")
rownames(correlation_nb_ceo_s) <- c("Total words", "Sum positive words", "Positivity in %",
                                    "Sum negative words", "Negativity in %", "Net sentiment in %",
                                    "Weighted sum negative", "Weighted sum positive", "Weighted net sentiment")
rownames(correlation_gb_ceo_s) <- c("Total words", "Sum positive words", "Positivity in %",
                                    "Sum negative words", "Negativity in %", "Net sentiment in %",
                                    "Weighted sum negative", "Weighted sum positive", "Weighted net sentiment")


###Create a heatmap to show correlation values in magnitude
mycolor <- c("#FDE725FF", "#9FDA3AFF", "#4AC16DFF", "#1FA187FF", "#277F8EFF", "#365C8DFF", "#46337EFF", "#440154FF",
             "#440154FF", "#46337EFF", "#365C8DFF", "#277F8EFF", "#1FA187FF", "#4AC16DFF", "#9FDA3AFF", "#FDE725FF")

colnames(correlation_nb_nb_s) <- trimws(gsub(colnames(correlation_nb_nb_s), pattern = '\\(FY0\\)', replacement = ''))
colnames(correlation_nb_ceo_s) <- trimws(gsub(colnames(correlation_nb_ceo_s), pattern = '\\(FY0\\)', replacement = ''))
heatmap_corr_nb_nb <- levelplot(correlation_nb_nb_s, scales=list(x=list(rot=40, fontfamily='Times', cex=1.5), y=list(fontfamily='Times', cex=1.5)), col.regions=mycolor, xlab='', ylab='', transparent=T, colorkey=list(labels=list(cex=1.2), font='Times'))
heatmap_corr_nb_ceo <- levelplot(correlation_nb_ceo_s, scales=list(x=list(rot=40, fontfamily='Times', cex=1.5), y=list(fontfamily='Times', cex=1.5)), col.regions=mycolor, xlab='', ylab='', transparent=T, colorkey=list(labels=list(cex=1.2), font='Times'))


correlation_gb_ceo_s <- as.data.frame(correlation_gb_ceo_s)
row.names(correlation_gb_ceo_s) <- rows
names(result_gb_ceo_esg) <- c("report",  "total", "sum_pos_words", "positivity_in_perc", "sum_neg_words" ,"negativity_in_perc"  ,           
                              "net_sent_in_perc", "sum_neg_weight"      ,           
                              "sum_pos_weight", "net_sum_weight"       ,          
                              "company", "report_type", 'Real Name'                ,    "Company Name",
                              "ESG Score", "ESG Combined Score"      ,
                              "ESG Controversies Score", "CSR Strategy Score"  ,    
                              "Environment Pillar Score", "Emissions Score"   ,      
                              "Social Pillar Score", "Governance Pillar Score",
                              "NAICS Sector Name", "NAICS Subsector Name", "NAICS Industry Group",
                              "NAICS National Industry Name")

result_gb_ceo_esg %>% 
  filter(negativity_in_perc<3) %>% 
  ggplot(., aes(negativity_in_perc, `CSR Strategy Score`, col=`ESG Score`))+
  geom_point(size=3)+
  geom_smooth(method='glm', col=viridis(1), alpha=0)+
  scale_color_viridis()


max(max(abs(correlation_gb_ceo_s)),
    max(abs(correlation_gb_nb_s)),
    max(abs(correlation_nb_ceo_s)),
    max(abs(correlation_nb_nb_s)))

max(max(abs(correlation_gb_ceo)),
    max(abs(correlation_gb_nb)),
    max(abs(correlation_nb_ceo)),
    max(abs(correlation_nb_nb)))

correlation_nb_nb_s <- as.data.frame(correlation_nb_nb_s)

row.names(correlation_nb_nb_s) <- rows
names(result_nb_nb_esg) <- c("report",  "total", "sum_pos_words", "positivity_in_perc", "sum_neg_words" ,"negativity_in_perc"  ,           
                              "net_sent_in_perc", "sum_neg_weight"      ,           
                              "sum_pos_weight", "net_sum_weight"       ,          
                              "company", "report_type"     , 'Real name'           ,    "Company Name",
                              "ESG Score", "ESG Combined Score"      ,
                              "ESG Controversies Score", "CSR Strategy Score"  ,    
                              "Environment Pillar Score", "Emissions Score"   ,      
                              "Social Pillar Score", "Governance Pillar Score",
                             "NAICS Sector Name", "NAICS Subsector Name", "NAICS Industry Group",
                             "NAICS National Industry Name")

library(dplyr)
ggplot(result_nb_nb_esg, aes(net_sent_in_perc, `CSR Strategy Score`, col=total))+
  geom_point(size=3)+
  geom_smooth(method='glm', formula = y ~ x, alpha=0,color=viridis(1))+
  scale_color_viridis()+
  theme(
        axis.text = element_text(size = 20),
        text = element_text(family='Times'),
        panel.background = element_rect(fill = 'grey97'), 
        axis.title = element_text(size = 20), 
        legend.position = 'right', 
        legend.text = element_text(size = 18, lineheight = 5), 
        legend.title = element_text(size = 18), 
        legend.box = "horizontal")+
  labs(y='CSR Strategy Score',
       x= 'Net sentiment in percent', 
       col='Total words')

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/corcsrstratnetsentperc.pdf')

ggplot(result_nb_nb_esg, aes(negativity_in_perc, `Emissions Score`, col=total))+
  geom_point(size=3)+
  geom_smooth(method='glm', formula = y ~ x, alpha=0,color=viridis(1))+
  scale_color_viridis()+
  theme(panel.background = element_rect(color='grey97'),
        text = element_text(family='Times', size = 20)) +
  labs(x='Negativity in percent',
       y='Emissions Score', 
       col='Total words')

correlation_nb_ceo_s <- as.data.frame(correlation_nb_ceo_s)
row.names(correlation_nb_ceo_s) <- rows

correlation_gb_ceo_s <- as.data.frame(correlation_gb_ceo_s)
row.names(correlation_gb_ceo_s) <- rows

correlation_gb_nb_s <- as.data.frame(correlation_gb_nb_s)
row.names(correlation_gb_nb_s) <- rows


max(abs(correlation_gb_nb_s))
max(abs(correlation_gb_ceo_s))

vars_nb_nb_s <- abs(correlation_nb_nb_s)>0.25
correlation_nb_nb_s <- vars_nb_nb_s*correlation_nb_nb_s
is.na(correlation_nb_nb_s) <- correlation_nb_nb_s == 0

vars_gb_nb_s <- abs(correlation_gb_nb_s)>0.25
correlation_gb_nb_s <- vars_gb_nb_s*correlation_gb_nb_s
is.na(correlation_gb_nb_s) <- correlation_gb_nb_s == 0

vars_nb_ceo_s <- abs(correlation_nb_ceo_s)>0.25
correlation_nb_ceo_s <- vars_nb_ceo_s*correlation_nb_ceo_s
is.na(correlation_nb_ceo_s) <- correlation_nb_ceo_s == 0

vars_gb_ceo_s <- abs(correlation_gb_ceo_s)>0.25
correlation_gb_ceo_s <- vars_gb_ceo_s*correlation_gb_ceo_s
is.na(correlation_gb_ceo_s) <- correlation_gb_ceo_s == 0

conflict_prefer('count', 'dplyr')
sum(abs(correlation_gb_ceo_s), na.rm = T)/correlation_gb_ceo_s %>% count()
sum(abs(correlation_nb_ceo_s), na.rm = T)/correlation_nb_ceo_s %>% count()
sum(abs(correlation_gb_nb_s), na.rm = T)/correlation_gb_nb_s %>% count()
sum(abs(correlation_nb_nb_s), na.rm = T)/correlation_nb_nb_s %>% count()


### Create a cluster analysis based on sentiment and ESG rating 
names(company_esg) <- c("report",  "total", "sum_pos_words", "positivity_in_perc", "sum_neg_words" ,"negativity_in_perc"  ,           
                        "net_sent_in_perc", "sum_neg_weight"      ,           
                        "sum_pos_weight", "net_sum_weight"       ,          
                        "company", "report_type", 'Real name'                ,    "Company Name",
                        "ESG Score", "ESG Combined Score"      ,
                        "ESG Controversies Score", "CSR Strategy Score"  ,    
                        "Environment Pillar Score", "Emissions Score"   ,      
                        "Social Pillar Score", "Governance Pillar Score",
                        "NAICS Sector Name", "NAICS Subsector Name", "NAICS Industry Group",
                        "NAICS National Industry Name")

##Calculate Hopkins statistic to assess cluster tendency
company_esg[-1] %>% 
  filter(report_type=='nb_ceo') %>% 
  select(-c(company, report_type,`Company Name`, `NAICS Sector Name`, `Real name`,
            `NAICS Subsector Name`, `NAICS National Industry Name`, `NAICS Industry Group`)) %>% 
  scale() %>% 
  get_clust_tendency(n=20)

#nbnb 0.37
#gbnb 0.32
#gb ceo 0.33
#nb_ceo 0.43

#Cluster tendency highest for nbceo, will be explored even though not good score

zesg <- company_esg[-1] %>% 
  filter(report_type=='nb_ceo') %>% 
  select(-c(company, report_type, `Company Name`, `NAICS Sector Name`, `Real name`,
            `NAICS Subsector Name`, `NAICS National Industry Name`, `NAICS Industry Group`)) %>% 
  scale() %>% 
  as.data.frame %>% 
  filter(!is.na(`ESG Score`))

d <- dist(zesg)

set.seed(0211)
dend <- hclust(d, method = 'ward.D2')
memb <- cutree(dend, 3)
groupm <- aggregate(zesg, list(memb), mean)

set.seed(1907)
dend2 <- kmeans(zesg, centers = groupm[-1])

table(memb, dend2$cluster)

dend <- dend %>% 
  as.dendrogram # and lastly, turn that object into a dendrogram.


dend <- dend %>% set("labels", c("Aurubis", "Bechtle", "Brenntag", "1&1", "Fiel-\nmann", "Fraport", "Fuchs", "Hugo \n Boss", "Innogy", "Kion", 
                                 "LEG \n Immobilien", "Metro", "Osram \n Licht", "Scout24", "Siltronic", "TAG", "Telefonica", "Uniper", "United \n Internet"))
dend <- dend %>% set('labels_cex', 5) 
  dend %>% plot()
mypal <- viridis(3)
clus3 <- cutree(dend, 3)
plot(ape::as.phylo(dend), type = "unrooted")
phylo <- plot(ape::as.phylo(dend), type = "fan", tip.color = mypal[clus3], cex = 1.3)

#has to be saved manually!


#Have Phylo with sector information 
sectors <- company_esg[-1] %>% select(company, `NAICS Sector Name`) %>% distinct()
dend_sector <- dend
clus_sect <- cutree(dend_sector, 3)
dend_sector <- dend_sector %>% 
  set('labels', c("Manufact.", "PST Service",
                  "Wholesale Trade", "Information", "Retail Trade","Transportation", 
                  "Manufact.", "Manufact.", "Utilities", "Manufact.", 
                  "Real Estate", "Retail Trade", "Manufact.", "Information", 
                  "Manufact.", "Real Estate", "Information", 
                  "Utilities", "Information"))

phylo2 <- plot(ape::as.phylo(dend_sector), type = "fan", tip.color = mypal[clus_sect], cex = 1.3)

#Has to be exported manually

ward_company_esg <- company_esg[-1] %>% 
  filter(report_type=='nb_ceo')

clus3 <- as.data.frame(clus3)
clus3$company <- c("aurubis", "bechtle", "brenntag", "einsundeins", "fielmann", "fraport", "fuchs", "hugoboss", "innogy", "kion", 
                   "legimmobilien", "metro", "osram", "scout24", "siltronic", "tag", "telefonica", "uniper", "unitedinternet")
ward_company_esg <- ward_company_esg %>% left_join(clus3, by=c('company'='company'))
ward_company_esg$clus3 <- factor(ward_company_esg$clus3)

### Check cluster association with sentiment and ESG data
sapply(ward_company_esg[c(1:9, 14:21)], cor, as.numeric(ward_company_esg$clus3), use='complete.obs', method='spearman')
sapply(ward_company_esg[c(1:9, 14:21)], cor.test, as.numeric(ward_company_esg$clus3), use='complete.obs', method='spearman')

names(ward_company_esg) <- c("total", "sum_pos_words", "positivity_in_perc", "sum_neg_words" ,"negativity_in_perc"  ,           
                             "net_sent_in_perc", "sum_neg_weight"      ,           
                             "sum_pos_weight", "net_sum_weight"       ,          
                             "company", "report_type","Real name"               ,    "Company Name",
                             "ESG Score", "ESG Combined Score"      ,
                             "ESG Controversies Score", "CSR Strategy Score"  ,    
                             "Environment Pillar Score", "Emissions Score"   ,      
                             "Social Pillar Score", "Governance Pillar Score", "NAICS Sector Name", "NAICS Subsector Name", "NAICS Industry Group",
                             "NAICS National Industry Name",'clus3')

ward_company_esg %>% 
  filter(!is.na(`Company Name`)) %>% 
ggplot(., aes(net_sent_in_perc, `ESG Controversies Score`,col=clus3))+
  geom_point(alpha=1)+
  geom_text_repel(aes(label=`Company Name`, size=3), show.legend=F)+
  scale_color_viridis(discrete = T)+
  theme(legend.position = 'bottom', 
        panel.background = element_rect(color='grey97'),
        text = element_text(family='Times', size = 20)) +
  labs(x='Net sentiment in percent',
       y='ESG Controversies Score',
       col='Cluster')
  

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/cluster.pdf')

ggplot(ward_company_esg, aes(positivity_in_perc, total, col=clus3))+
  geom_point(size=2)+
  scale_x_log10()+
  scale_y_log10()+
  scale_color_viridis(discrete = T)+
  theme(legend.position = 'bottom', 
        panel.background = element_rect(color='grey97'),
        text = element_text(family='Times', size = 20)) +
  labs(x='Positivity in percent (in log)',
       y='Total word count (in log)',
       col='Cluster')


#Using sector information 
ward_company_esg$`NAICS Sector Name` <- factor(ward_company_esg$`NAICS Sector Name`)

ward_company_esg %>% 
  filter(!is.na(`ESG Score`)) %>% 
  ggplot(., aes(`NAICS Sector Name`, clus3))+
  geom_point()
  
cor(as.numeric(ward_company_esg$clus3), as.numeric(ward_company_esg$`NAICS Sector Name`), use = 'complete.obs')

ward_company_esg %>% 
  filter(!is.na(`ESG Score`)) %>% 
  ggplot(., aes(net_sent_in_perc,reorder(`NAICS Sector Name`, net_sent_in_perc), col=report_type))+
  geom_point(position = 'jitter')

conflict_prefer('arrange', 'dplyr')
conflict_prefer('summarise', 'dplyr')

p1 <- ward_company_esg %>% 
  filter(!is.na(`ESG Score`)) %>% 
  group_by(`NAICS Sector Name`) %>% 
  summarise(score_sector=mean(net_sent_in_perc)) %>% 
  arrange(desc(score_sector)) %>% 
  ggplot(., aes(reorder(`NAICS Sector Name`, score_sector),score_sector))+
  geom_point(col=viridis(1))+
  theme(legend.position = 'bottom', 
        panel.background = element_rect(color='grey97'),
        text = element_text(family='Times', size = 20), 
        axis.text.x = element_text(angle = 45)) +
  labs(y='Mean weighted net sentiment',
       x='Sector')

p2 <- ward_company_esg %>% 
  filter(!is.na(`ESG Score`)) %>% 
  group_by(`NAICS Sector Name`) %>% 
  summarise(score_sector=mean(net_sum_weight)) %>% 
  arrange(desc(score_sector)) %>% 
  ggplot(., aes(reorder(`NAICS Sector Name`, score_sector), score_sector))+
  geom_point(col=viridis(1))+
  theme(legend.position = 'bottom', 
        panel.background = element_rect(color='grey97'),
        text = element_text(family='Times', size = 20), 
        axis.text.x = element_text(angle = 45)) +
  labs(y='Mean weighted net sentiment',
       x='Sector')
gridExtra::grid.arrange(p1, p2, ncol = 2)

#Being in controversies around ESG issues can be considered one major risk of firms in the context of CSR
#sentiment as an indicator for this 
ward_company_esg %>% 
  filter(!is.na(`ESG Score`)) %>% 
  ggplot(., aes(`ESG Controversies Score`, net_sent_in_perc, col=clus3))+
  geom_point(alpha=1, size=3)+
  scale_color_viridis(discrete = T)+
  labs(x='Net sentiment in percent', 
       y='ESG Controversies Score',
       col='Cluster')+
  theme(legend.position = 'bottom', 
        panel.background = element_rect(color='grey97'),
        text = element_text(family='Times', size = 20))

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/clustercontroversies.pdf')

ward_company_esg %>% 
  filter(!is.na(`ESG Score`)) %>% 
  ggplot(., aes(net_sent_in_perc, `Emissions Score`,col=clus3))+
  geom_point(alpha=1, size=3)+
  scale_color_viridis(discrete = T)+
  labs(x='Net sentiment in percent', 
       y='Emissions Score',
       col='Cluster')+
  theme(legend.position = 'bottom', 
        panel.background = element_rect(color='grey97'),
        text = element_text(family='Times', size = 20))

cor.test(as.numeric(ward_company_esg$clus3), ward_company_esg$`ESG Controversies Score`, na.rm=R)
cor.test(as.numeric(ward_company_esg$clus3), ward_company_esg$net_sent_in_perc, na.rm=T, method = 'spearman')
cor.test(as.numeric(ward_company_esg$clus3), ward_company_esg$`Emissions Score`, method = 'spearman')

cor.test(as.numeric(ward_company_esg[!is.na(ward_company_esg$`Company Name`),]$clus3), ward_company_esg[!is.na(ward_company_esg$`Company Name`),]$positivity_in_perc, na.rm=T)

ward_company_esg %>% 
  filter(!is.na(`Company Name`)) %>% 
  ggplot(., aes(clus3, positivity_in_perc))+
  geom_point(col=viridis(1))+
  geom_text_repel(aes(label=`Company Name`), direction = 'x', col=viridis(1),
                  family='Times', size=6)+
  theme(legend.position = 'none', 
        panel.background = element_rect(color='grey97'),
        text = element_text(family='Times', size = 20))+
  scale_y_log10()+
  labs(x='Cluster', 
       y='Positivity in percent (in log)')

ggsave('/Users/Constanze/Desktop/uni/ba arbeit/clusterpositivity.pdf')

company_sector <- company_esg[-1] %>% 
  select(`Company Name`, `NAICS Sector Name`) %>% 
  distinct()

company_esg[-1] %>% 
  distinct(`NAICS Sector Name`)

#Dimension reduction via PCA

pr_data <- company_esg[-1] %>% 
  filter(report_type=='nb_ceo') %>% 
  filter(complete.cases(.))

pr_company <- prcomp(x=pr_data[,c(1:9, 14:21)], scale = T)
 
screeplot(pr_company)

ggbiplot(pr_company)
ggbiplot(pr_company, labels=pr_data$company)
fviz_eig(pr_company)
fviz_pca_ind(pr_company,
             col.ind = "cos2", 
             geom='text',# Color by the quality of representation
             gradient.cols = viridis(5),
             repel = TRUE)+
  geom_text(aes(label=pr_data$company))

