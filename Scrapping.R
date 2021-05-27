

######################################################
#      Getting dataset scrapped from the website     #
######################################################


### LIBRARIES ###
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tibble)



###### AGENTS ######
# Some of the most popular Chrome User Agents for windows
agents <-
  c(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.131 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.169 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:66.0) Gecko/20100101 Firefox/66.0",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.157 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/73.0.3683.103 Safari/537.36",
    "Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.106 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:67.0) Gecko/20100101 Firefox/67.0",
    "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.131 Safari/537.36"
  )


###### SCRAPPING DATA ######

#url
url <- "https://www.mlb.com/stats/all-time-totals"
mlb_Hitting_all_time_totals <- read_html(url)

#pagination
pagination <-
  mlb_Hitting_all_time_totals %>% html_nodes(".bui-button-group.pagination") %>% html_children() %>% html_text()

n_pages <- c(2:(pagination %>% tail(1)))

#getting data from all pages
mlb_Hitting_scrpping <-
  mlb_Hitting_all_time_totals %>% html_nodes(".table-wrapper-3-qU3HfQ") %>% html_table()
all_data_scrapped <- mlb_Hitting_scrpping[1][[1]]


for (page in n_pages) {
  paste0(url, "?page=", page) %>% print()
  
  user_a = sample(agents, 1)
  data <-
    read_html(paste0(url, "?page=", page),
              user_agent = httr::user_agent(user_a)) %>%
    html_nodes(".table-scroller-2FeRJsQr") %>%
    html_table()
  
  all_data_scrapped <- rbind(all_data_scrapped, data[1][[1]])
  
  Sys.sleep(5)
}

# Save data into rds file and in data_backup
saveRDS(all_data_scrapped, file = "scraped_dataset.rds")
saveRDS(all_data_scrapped,
        file = paste0(
          "data_backups/scraped_dataset_",
          format(Sys.time(), "%Y-%m-%d_%Hh%M"),
          ".rds"
        ))




###### FIXING DATA ######

# Scrapped data has several problems and they have to be fixed...

data_fixed <- all_data_scrapped #let work with another database
# data_fixed <- read_rds("scraped_dataset.rds") %>% as.data.frame() # or load the data from file


names(data_fixed) <- c("PLAYER", "G", "AB", "R", "H", "2B", "3B", "HR", "RBI", "BB", "SO",
                       "SB", "CS", "AVG", "OBP", "SLG", "OPS" )

# Cleaning column player
data_fixed$PLAYER <- str_replace_all(data_fixed$PLAYER, "[^[:alnum:]]", " ")
data_fixed <- data_fixed %>% mutate_if(is.character, str_trim)
data_fixed$PLAYER <- gsub("^\\d+|\\d+$", "", data_fixed$PLAYER)

# Getting positions
data_fixed <- data_fixed %>% 
  add_column(POS = str_extract(data_fixed$PLAYER, "([0-9]*[A-Z]+)$"), .after = "PLAYER")
data_fixed$PLAYER <- gsub("([0-9]*[A-Z]+)$", "", data_fixed$PLAYER)

# fixing players' names

data_fixed$PLAYER <- lapply(data_fixed$PLAYER, function(x) {

  output <- c()
  current <- strsplit(x, split = " ") %>% unlist()
  
  # most of the fist names had repeated the inicial capital letter at the end
  # print(current)
  f <- current %>% first()
  if (f %>% nchar() > 2 &
    f %>% str_extract("^[A-Z]") == f %>% str_extract("[A-Z]*$")) {
    current[1] <- str_replace(f, "[A-Z]$", "")
  }

  # most of the last names are repeated
  l <- current %>% last()
  if (l %>% nchar() %% 2 == 0 &
      l %>% substring(1, nchar(l) / 2) %>% stri_trans_general("Latin-ASCII") ==
      l %>% substring((nchar(l) / 2) + 1) %>% stri_trans_general("Latin-ASCII")) {
   current[length(current)] <- l %>% substring(1, nchar(l) / 2)
  }
  
  # in addition...
  
  # in three-words name... 
  # it's found that first word first letter is repeated at the end of second word 
  if(current%>%length()==3 &
     f %>% str_extract("^[A-Z]") == current[2] %>% str_extract("[A-Z]$")){
    current[2] <- str_replace(current[2], "[A-Z]$", "")
  }
  
  # in four-words name...
  # the third word is the union of second and fourth words or 
  # the first column if it has just one letter
  if(current%>%length()==4){
    current <- current[-3]
  }
  
  # in fith-words name...
  if(current%>%length()==5){
    current <- current[-c(4,5)]
  }

  # results
  current<-paste(current, collapse = " ")
  output <- c(output, current)
  return(output)

}) %>% as.character()


# Save fixed data into rds file and in data_backup
saveRDS(data_fixed, file = "fixed_scraped_dataset.rds")
saveRDS(data_fixed,
        file = paste0(
          "data_backups/fixed_scraped_dataset_",
          format(Sys.time(), "%Y-%m-%d_%Hh%M"),
          ".rds"
        ))









