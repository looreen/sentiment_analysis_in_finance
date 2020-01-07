library(tabulizer)
library(tesseract)

file_path_nb <-
  '/Users/Constanze/Desktop/uni/ba arbeit/sentiment analysis/Berichte/nachhaltigkeitsberichte'
file_names_nb <- file.path(file_path_nb, list.files(file_path_nb), sep = '/')

einsundeins_nb_ceo <- extract_text(file_names_nb[1], pages=4)
alstria_nb_ceo <- extract_text(file_names_nb[3], pages=10:11)
aurubis_nb_ceo <- extract_text(file_names_nb[4], pages=2:3)
bechtle_nb_ceo <-  extract_text(file_names_nb[5], pages=5)
brenntag_nb_ceo <-  extract_text(file_names_nb[6], pages=6:7)
fielmann_nb_ceo <-  extract_text(file_names_nb[14], pages=6:7)
fraport_nb_ceo <-  extract_text(file_names_nb[15], pages=4:5)
fuchs_nb_ceo <-  extract_text(file_names_nb[16], pages=3)
hugoboss_nb_ceo <-  extract_text(file_names_nb[18], pages=3)
innogy_nb_ceo <-  extract_text(file_names_nb[19], pages=7:8)
kion_nb_ceo <-  extract_text(file_names_nb[20], pages=7:8)
legimmobilien_nb_ceo <-  extract_text(file_names_nb[22], pages=4)
metro_nb_ceo <-  extract_text(file_names_nb[23], pages=5)
norma_nb_ceo <-  extract_text(file_names_nb[25], pages=4)
osram_nb_ceo <-  extract_text(file_names_nb[26], pages=4:5)
scout24_nb_ceo <-  extract_text(file_names_nb[30], pages=6:7)
siltronic_nb_ceo <-  extract_text(file_names_nb[31], pages=3)
tag_nb_ceo <-  extract_text(file_names_nb[9], pages=4:5)
telefonica_nb_ceo <- extract_text(file_names_nb[33], pages=5:6)
uniper_nb_ceo <- extract_text(file_names_nb[34], pages=3)
unitedinternet_nb_ceo <- extract_text(file_names_nb[35], pages=4:5)

