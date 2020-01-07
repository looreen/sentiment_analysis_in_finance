library(tabulizer)
library(tesseract)

file_path_nb <-
  '/Users/Constanze/Desktop/uni/ba arbeit/sentiment analysis/Berichte/nachhaltigkeitsberichte'
file_names_nb <- file.path(file_path_nb, list.files(file_path_nb), sep = '/')

all <- sapply(file_names_nb, extract_text)

einsundeins_nb_nb <- all[[1]]
aareal_nb_nb <- all[[2]]
alstria_nb_nb <- all[[3]]
aurubis_nb_nb <- all[[4]]
bechtle_nb_nb <- all[[5]]
brenntag_nb_nb <- all[[6]]
commerzbank_nb_nb <- all[[7]]
ctseventim_nb_nb <- all[[8]]
tag_nb_nb <- all[[9]]
deutschepfandbriefbank_nb_nb <- all[[10]]
duerr_nb_nb <- all[[11]]
#evonik_nb_nb <- all[[12]]
evotec_nb_nb <- all[[13]]
fielmann_nb_nb <- all[[14]]
fraport_nb_nb <- all[[15]]
fuchs_nb_nb <- all[[16]]
gea_nb_nb <- all[[17]]
hugoboss_nb_nb <- all[[18]]
innogy_nb_nb <- all[[19]]
kion_nb_nb <- all[[20]]
lanxess_nb_nb <- all[[21]]
legimmobilien_nb_nb <- all[[22]]
metro_nb_nb <- all[[23]]
nemetschek_nb_nb <- all[[24]]
norma_nb_nb <- all[[25]]
osram_nb_nb <- all[[26]]
rocketinternet_nb_nb <- all[[27]]
salzgitter_nb_nb <- all[[28]]
schaeffler_nb_nb <- all[[29]]
scout24_nb_nb <- all[[30]]
siltronic_nb_nb <- all[[31]]
softwareag_nb_nb <- all[[32]]
telefonica_nb_nb <- all[[33]]
uniper_nb_nb <- all[[34]]
unitedinternet_nb_nb <- all[[35]]

#takes forever, only use when necessary, might need some extra cleaning
#evonik_nb_nb <- ocr(image = file.path(file_path_nb, 'Evonik_Nichtfinanzieller_Bericht_2018.pdf'))

evonik_nb_nb <- ocr(image = c('Evonik_Nichtfinanzieller_Bericht_2018_1.png',
                                'Evonik_Nichtfinanzieller_Bericht_2018_2.png',
                                'Evonik_Nichtfinanzieller_Bericht_2018_3.png',
                                'Evonik_Nichtfinanzieller_Bericht_2018_4.png',
                                'Evonik_Nichtfinanzieller_Bericht_2018_5.png',
                                'Evonik_Nichtfinanzieller_Bericht_2018_6.png',
                                'Evonik_Nichtfinanzieller_Bericht_2018_7.png',
                                'Evonik_Nichtfinanzieller_Bericht_2018_8.png',
                                'Evonik_Nichtfinanzieller_Bericht_2018_9.png',
                                'Evonik_Nichtfinanzieller_Bericht_2018_10.png',
                                'Evonik_Nichtfinanzieller_Bericht_2018_11.png',
                                'Evonik_Nichtfinanzieller_Bericht_2018_12.png',
                                'Evonik_Nichtfinanzieller_Bericht_2018_13.png',
                                'Evonik_Nichtfinanzieller_Bericht_2018_14.png',
                                'Evonik_Nichtfinanzieller_Bericht_2018_15.png',
                                'Evonik_Nichtfinanzieller_Bericht_2018_16.png'
                                ))