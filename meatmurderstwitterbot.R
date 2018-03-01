### Load packages
library(magrittr)
library(tidyverse)
library(rvest)
library(twitteR)
library(lubridate)
library(janitor)

### Set options
options(stringsAsFactors = FALSE)

### Pull and clean table from HSUS Google Doc
hsus_url <- "https://docs.google.com/spreadsheets/d/11MOGsKjfgTD8rDs2pywFqybP-BycMUCjS1xxQbfKW6g/pubhtml"
df <- read_html(hsus_url) %>% 
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "waffle", " " ))]') %>% 
  html_table() %>%
  data.frame() %>% 
  filter(!grepl("Year to date", Var.3, ignore.case = TRUE),
         !grepl("Updated", Var.3, ignore.case = TRUE),
         !grepl("thousands of animals", Var.3, ignore.case = TRUE),
         Var.2 != "",
         Var.2 != "% Change") %>% 
  select(-Var.1)

df$Var.2 <- gsub("\\*", "", df$Var.2)
df[] <- lapply(df, gsub, pattern=',', replacement='')

names(df) <-  paste(df[1,], sep = ", ")
# Note: NAs introduced by coercion are simply replacing "N/A" string values.
df %<>%
  clean_names() %>% 
  filter(year != "Year") %>% 
  mutate_all(as.numeric) %>% 
  select(-total)

### Calculate estimated number of animals killed so far in year
day_deaths <- sapply(df[which(df$year == 2016),], function(x){ceiling(x*(as.numeric(Sys.Date() - floor_date(Sys.Date(), unit = "year")-1)/365))})[2:length(df)]

###
### Twitter Bot
###

### Post to Twitter
# Note: the if condition is written such that the script will always run,
# but also so that it is easy to change the time if not scheduled.
if(substr(Sys.time(), 12, 16) == substr(Sys.time(), 12, 16)){
  
  # Load Twitter API key data
  api_keys <- read.csv("C:/NoSpaces/twitter_access.csv")
  
  # Set up OAuth authentication
  setup_twitter_oauth(consumer_key = api_keys$consumer_key,
                      consumer_secret = api_keys$consumer_secret,
                      access_token = api_keys$access_token,
                      access_secret = api_keys$access_secret)
  
  # Assemble tweet
  tweet_text <- paste0("Today is ", format(Sys.Date(), "%A %B %d, %Y"), ". So far this year, over ",
                       formatC(sum(day_deaths), format="d", big.mark=","), 
                       " animals have been killed for meat. This includes:\n\n",
                       formatC(day_deaths["chickens"], format="d", big.mark=","), " chickens\n",
                       formatC(day_deaths["turkeys"], format="d", big.mark=","), " turkeys\n",
                       formatC(day_deaths["hogs"], format="d", big.mark=","), " pigs\n",
                       formatC(day_deaths["cattle"], format="d", big.mark=","), " cows\n",
                       formatC(day_deaths["ducks"], format="d", big.mark=","), " ducks\n",
                       formatC(day_deaths["sheep_lambs"], format="d", big.mark=","), " sheep and lambs\n\n",
                       "#govegan #meatmurders")
  
  # Post tweet. bypassCharLimit = TRUE to avoid 140 character limit of 'twitteR' package
  updateStatus(tweet_text, bypassCharLimit = TRUE)
}
