correlation_nb_nb_st <- sapply(result_nb_nb_esg[15:22], cor.test, result_nb_nb_esg[2:10], use='complete.obs', method='spearman')
correlation_gb_nb_st <- sapply(result_gb_nb_esg[15:22], cor.test, result_gb_nb_esg[2:10], use='complete.obs', method='spearman')
correlation_nb_ceo_st <- sapply(result_nb_ceo_esg[15:22], cor.test, result_nb_ceo_esg[2:10], use='complete.obs', method='spearman')
correlation_gb_ceo_st <- sapply(result_gb_ceo_esg[15:22], cor.test, result_gb_ceo_esg[2:10], use='complete.obs', method='spearman')

#Testing correlation confidence interval for sentiment metrics against ESG ratings

####### CSR report total 
###total
sapply(result_nb_nb_esg[15:22], cor.test, result_nb_nb_esg$total, use='complete.obs', method='spearman')
###Sum pos words
sapply(result_nb_nb_esg[15:22], cor.test, result_nb_nb_esg$sum_pos_words, use='complete.obs', method='spearman')
###Positivity
sapply(result_nb_nb_esg[15:22], cor.test, result_nb_nb_esg$positivity_in_perc, use='complete.obs', method='spearman')
###Sum negative words
sapply(result_nb_nb_esg[15:22], cor.test, result_nb_nb_esg$sum_neg_words, use='complete.obs', method='spearman')
###Negativity
sapply(result_nb_nb_esg[15:22], cor.test, result_nb_nb_esg$negativity_in_perc, use='complete.obs', method='spearman')
###Net sentiment
sapply(result_nb_nb_esg[15:22], cor.test, result_nb_nb_esg$net_sent_in_perc, use='complete.obs', method='spearman')
###Weighted negative
sapply(result_nb_nb_esg[15:22], cor.test, result_nb_nb_esg$sum_neg_weight, use='complete.obs', method='spearman')
###Weighted positive
sapply(result_nb_nb_esg[15:22], cor.test, result_nb_nb_esg$sum_pos_weight, use='complete.obs', method='spearman')
###Weighted net
sapply(result_nb_nb_esg[15:22], cor.test, result_nb_nb_esg$net_sum_weight, use='complete.obs', method='spearman')



####### CSR report CEO letter
###total
sapply(result_nb_ceo_esg[15:22], cor.test, result_nb_ceo_esg$total, use='complete.obs', method='spearman')
###Sum pos words
sapply(result_nb_ceo_esg[15:22], cor.test, result_nb_ceo_esg$sum_pos_words, use='complete.obs', method='spearman')
###Positivity
sapply(result_nb_ceo_esg[15:22], cor.test, result_nb_ceo_esg$positivity_in_perc, use='complete.obs', method='spearman')
###Sum negative words
sapply(result_nb_ceo_esg[15:22], cor.test, result_nb_ceo_esg$sum_neg_words, use='complete.obs', method='spearman')
###Negativity
sapply(result_nb_ceo_esg[15:22], cor.test, result_nb_ceo_esg$negativity_in_perc, use='complete.obs', method='spearman')
###Net sentiment
sapply(result_nb_ceo_esg[15:22], cor.test, result_nb_ceo_esg$net_sent_in_perc, use='complete.obs', method='spearman')
###Weighted negative
sapply(result_nb_ceo_esg[15:22], cor.test, result_nb_ceo_esg$sum_neg_weight, use='complete.obs', method='spearman')
###Weighted positive
sapply(result_nb_ceo_esg[15:22], cor.test, result_nb_ceo_esg$sum_pos_weight, use='complete.obs', method='spearman')
###Weighted net
sapply(result_nb_ceo_esg[15:22], cor.test, result_nb_ceo_esg$net_sum_weight, use='complete.obs', method='spearman')


####### Annual Report CEO letter
###total
sapply(result_gb_ceo_esg[15:22], cor.test, result_gb_ceo_esg$total, use='complete.obs', method='spearman')
###Sum pos words
sapply(result_gb_ceo_esg[15:22], cor.test, result_gb_ceo_esg$sum_pos_words, use='complete.obs', method='spearman')
###Positivity
sapply(result_gb_ceo_esg[15:22], cor.test, result_gb_ceo_esg$positivity_in_perc, use='complete.obs', method='spearman')
###Sum negative words
sapply(result_gb_ceo_esg[15:22], cor.test, result_gb_ceo_esg$sum_neg_words, use='complete.obs', method='spearman')
###Negativity
sapply(result_gb_ceo_esg[15:22], cor.test, result_gb_ceo_esg$negativity_in_perc, use='complete.obs', method='spearman')
###Net sentiment
sapply(result_gb_ceo_esg[15:22], cor.test, result_gb_ceo_esg$net_sent_in_perc, use='complete.obs', method='spearman')
###Weighted negative
sapply(result_gb_ceo_esg[15:22], cor.test, result_gb_ceo_esg$sum_neg_weight, use='complete.obs', method='spearman')
###Weighted positive
sapply(result_gb_ceo_esg[15:22], cor.test, result_gb_ceo_esg$sum_pos_weight, use='complete.obs', method='spearman')
###Weighted net
sapply(result_gb_ceo_esg[15:22], cor.test, result_gb_ceo_esg$net_sum_weight, use='complete.obs', method='spearman')



####### Annual Report Sustainability section
###total
sapply(result_gb_nb_esg[15:22], cor.test, result_gb_nb_esg$total, use='complete.obs', method='spearman')
###Sum pos words
sapply(result_gb_nb_esg[15:22], cor.test, result_gb_nb_esg$sum_pos_words, use='complete.obs', method='spearman')
###Positivity
sapply(result_gb_nb_esg[15:22], cor.test, result_gb_nb_esg$positivity_in_perc, use='complete.obs', method='spearman')
###Sum negative words
sapply(result_gb_nb_esg[15:22], cor.test, result_gb_nb_esg$sum_neg_words, use='complete.obs', method='spearman')
###Negativity
sapply(result_gb_nb_esg[15:22], cor.test, result_gb_nb_esg$negativity_in_perc, use='complete.obs', method='spearman')
###Net sentiment
sapply(result_gb_nb_esg[15:22], cor.test, result_gb_nb_esg$net_sent_in_perc, use='complete.obs', method='spearman')
###Weighted negative
sapply(result_gb_nb_esg[15:22], cor.test, result_gb_nb_esg$sum_neg_weight, use='complete.obs', method='spearman')
###Weighted positive
sapply(result_gb_nb_esg[15:22], cor.test, result_gb_nb_esg$sum_pos_weight, use='complete.obs', method='spearman')
###Weighted net
sapply(result_gb_nb_esg[15:22], cor.test, result_gb_nb_esg$net_sum_weight, use='complete.obs', method='spearman')
