library(rvest)
library(xml2)
library(stringr)
library(readr)
rankings <- read_html('https://keeptradecut.com/dynasty-rankings')


allRanks <- rankings %>% html_elements('.onePlayer')

names <- allRanks %>% html_element('.player-name') %>%  html_element('a') %>% html_text2()
teams <- allRanks %>% html_element('.player-name') %>%  html_elements('.player-team') %>% html_text2()
rankNum <- allRanks %>% html_element('.rank-number') %>% html_text2() %>% str_replace_all("[\r\n]" , "")

positionTeam <- allRanks %>% html_elements(".position-team")
# agePlayer <- positionTeam  %>%  html_node("[class='age']") %>% html_text2()
posRank <- positionTeam  %>%  html_element('.position') %>% html_text2()
# agePlayer <- positionTeam %>%  html_element('.age') %>% html_text2()

tier <- allRanks %>% html_element('.player-info') %>%  html_element('.position') %>% html_text2()
value <- allRanks %>% html_element('.value') %>% html_text2() %>% str_replace_all("[\r\n]" , "")

# writes KTC ranks
ktc_ranks <- data.frame(names, posRank, rankNum, teams, tier, value)
date <-  as.character(Sys.Date())
doc_name <- "Documents/KTC/current_ktc_data_" + date + ".csv
write.csv(ktc_ranks,doc_name,row.names = FALSE)

# gets tan's IDs and writes .csv
file <- "https://github.com/dynastyprocess/data/blob/master/files/db_playerids.rds"
myRDS <- readRDS(file, refhook = NULL)
write.csv(mydata,"Documents/KTC/Tan's IDs.csv",row.names = TRUE)
