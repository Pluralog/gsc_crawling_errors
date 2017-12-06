#                __  __                  __  _           
#    ____ ____  / /_/ /__________ ______/ /_(_)___  ____ 
#   / __ `/ _ \/ __/ __/ ___/ __ `/ ___/ __/ / __ \/ __ \
#  / /_/ /  __/ /_/ /_/ /  / /_/ / /__/ /_/ / /_/ / / / /
#  \__, /\___/\__/\__/_/   \__,_/\___/\__/_/\____/_/ /_/ 
# /____/                                                 
#
# web: https://www.gettraction.de/
# twitter: @gettraction_om
#
# author: Patrick Lürwer
# twitter: @netzstreuner
# date: 2017-10-05  
#
# 

# Libraries ---------------------------------------------------------------

library(searchConsoleR)
library(googleAuthR)
library(tidyverse)
library(urltools)
library(stringr)
library(rvest)
library(httr)



# Setup -------------------------------------------------------------------

# Die Variable auf FALSE setzen, wenn bereits eine Authentifizierung durchgeführt und eine .httr-oauth-Datei gespeichert wurde.
# Zur Verfikation eines anderen Google-Accounts die httr.oauth-Datei löschen und Variable wieder auf TRUE setzen
new_auth <- TRUE

# Hier den Pfad für den CSV-Export eintragen, in den die zu überpüfenden URLs geschrieben werden
path_to_csv_export <- "C:/Pfad/zur/export.csv"

# Hier die Goolge API Client ID eintragen
client_id <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.apps.googleusercontent.com"

# Hier den Google API Clientschlüssel eintragen
client_secret <- "XXXXXXXXXXXXXXXXXXXXXXX" 



# Authentifizierung --------------------------------------------------------


options(googleAuthR.client_id = client_id,
        googleAuthR.client_secret = client_secret,
        googleAuthR.httr_oauth_cache = TRUE,
        googleAuthR.scopes.selected = "https://www.googleapis.com/auth/webmasters")


if (new_auth) {
  
  if(file.exists(".httr-oauth")) {
    file.remove(".httr-oauth")
  }
  gar_auth()
  
} else {
  
  Sys.setenv(GAR_AUTH_FILE = ".httr-oauth")
  gar_auto_auth(required_scopes = "https://www.googleapis.com/auth/webmasters")
  
}


# Auswahl der Property ---------------------------------------------------------

# Die Funktion gibt alle Properties des Google-Accounts in einem DataFrame zurück.
properties <- list_websites()

# Hier die gewünschte Property eintragen
property <- "https://example.com/"

domain <- domain(property)
protocol <- scheme(property)
platform <- c("web", "smartphoneOnly")


# User-Agent definieren ----------------------------------------------------------

set_config(user_agent("Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html"))



# Crawling-Fehler abfragen -----------------------------------------------------

not_found <- tibble(
  pageUrl = character(),
  linkedFromUrls = character(),
  platform = character()
)


for (i in platform) {
  
  temp_not_found <- list_crawl_error_samples(property,
                                             category = "notFound",
                                             platform = i)
  
  temp_not_found <- temp_not_found$urlDetails %>%
    bind_cols(temp_not_found %>% 
                select(pageUrl)) %>%
    mutate(is_null = map_lgl(linkedFromUrls, is.null))
  
  urls_solved <- temp_not_found %>%
    filter(is_null == TRUE)
  
  temp_not_found <- temp_not_found %>% 
    filter(is_null == FALSE) %>%
    select(-is_null) %>%
    unnest(linkedFromUrls) %>%
    mutate(platform = i)

  not_found <- not_found %>% 
    bind_rows(temp_not_found)
  
  rm(temp_not_found, i)
  
}



# Ein bissche aufräumen -----------------------------------------------------------

not_found <- not_found %>% 
  mutate(url_not_found = str_c(property, pageUrl)) %>%
  select(-pageUrl) %>% 
  rename(linked_from_url = linkedFromUrls) %>% 
  select(platform, url_not_found, linked_from_url)



# Überprüfen, ob die 4xx-URLs immer noch mit 4xx antworten ------------------------------------------------------

get_status_code <- function(url) {
  
  out <- tryCatch(
    {
      HEAD(url) %>% 
        status_code()
    },
    error = function(cond) {
      message:(paste("URL caused an error:", url))
      return(NA)
    },
    warning = function(cond) {
      message(paste("URL caused a warning:", url))
      return(NA)
    }
  )
  return(out)
  
}


not_found_status_code <- not_found %>%
  distinct(url_not_found) %>% 
  mutate(url_not_found_status_code = map_int(url_not_found, get_status_code))

mark_as_fixed <- not_found %>% 
  semi_join(not_found_status_code %>% 
              filter(url_not_found_status_code %in% c(200, 301)),
            by = "url_not_found") %>%
  mutate(url_not_found = path(url_not_found)) %>% 
  select(url_not_found, platform)

not_found <- not_found %>% 
  semi_join(not_found_status_code %>% 
              filter(url_not_found_status_code %in% c(404, 410)), 
            by = "url_not_found")



# Überprüfen, ob die verlinkende Seite noch erreichbar ist ----------------------------------------

linked_from_url_status_code <- not_found %>% 
  distinct(linked_from_url) %>%
  mutate(linked_from_url_status_code = map_int(linked_from_url, get_status_code)) %>%   
  filter(!is.na(linked_from_url_status_code))

mark_as_fixed <- not_found %>% 
  semi_join(linked_from_url_status_code %>% 
              filter(linked_from_url_status_code %in% c(404, 410)),
            by = "linked_from_url") %>% 
  select(url_not_found, platform) %>% 
  mutate(url_not_found = path(url_not_found)) %>% 
  bind_rows(mark_as_fixed)

not_found <- not_found %>% 
  semi_join(linked_from_url_status_code %>% 
              filter(linked_from_url_status_code %in% c(200, 301)),
            by = "linked_from_url")



# Den Quellcode der verlinkenden Seiten abrufen --------------------------------------------

outlinks <- tibble(
  url_not_found = character(),
  linked_from_url = character(),
  outlink = character()
)


for (i in 1:nrow(not_found)) {
  
  tryCatch(
    {
      temp_outlinks <- not_found$linked_from_url[[i]] %>% 
        read_html() %>% 
        html_nodes("a") %>% 
        html_attr("href") %>%
        as.tibble() %>%
        rename(outlink = value) %>% 
        filter(!is.na(outlink)) %>% 
        mutate(url_not_found = not_found$url_not_found[[i]],
               linked_from_url = not_found$linked_from_url[[i]],
               platform = not_found$platform[[i]]) 
      
      outlinks <- outlinks %>% 
        bind_rows(temp_outlinks)
    },
    error  = function(cond) {
      message(not_found$linked_from_url[[i]])
      message(cond)
    },
    warning = function(cond) {
      message(not_found$linked_from_url[[i]])
      
      message(cond)
    }
    
  )
  
}



# Überprüfen, ob die 404-URLs im Quellcode noch verlinkt sind -------------

link_exists <- outlinks %>% 
  mutate(url_not_found = path(url_not_found),
         outlink = path(outlink)) %>% 
  mutate(outlinks = url_not_found == outlink) %>% 
  group_by(url_not_found) %>% 
  summarise(outlinks = sum(outlinks, na.rm = T))

mark_as_fixed <- not_found %>%
  mutate(url_not_found = path(url_not_found)) %>% 
  semi_join(link_exists %>% 
              filter(outlinks == 0),
            by = "url_not_found") %>% 
  distinct(url_not_found, platform) %>% 
  bind_rows(mark_as_fixed) %>% 
  distinct(platform, url_not_found)



# URLs via API als behoben markieren --------------------------------------

mark_as_fixed_list <- list(siteURL = rep(property, nrow(mark_as_fixed)),
                           pageURL = mark_as_fixed$url_not_found,
                           category = rep("notFound", nrow(mark_as_fixed)),
                           platform = mark_as_fixed$platform)

mark_as_fixed_list %>% 
  pmap(safely(fix_sample_url))



# "Echte" 404-URLs in CSV schreiben ---------------------------------------

true_not_found <- not_found %>%
  mutate(url_not_found = path(url_not_found)) %>% 
  semi_join(link_exists %>% 
              filter(outlinks > 0),
            by = "url_not_found")

write_csv(true_not_found, path_to_csv_export)




