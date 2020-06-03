## Web Scraping Practice - NBA Team Data:

# Load library to extact information from ESPN webpage:
library(rvest)
library(dplyr)

## ---- Define function to scrape regular season team data for one season: -----
scrapeteams <- function (url) {
  
  # Read webpage:
  webpage <- read_html(url)
  
  # Extract team information:
  teams_data_html <- html_nodes(webpage,".Table__TD")
  teams_data <-html_text(teams_data_html)
  col_labels_html <- html_nodes(webpage, ".Table__TH")
  col_labels <- html_text(col_labels_html)
  
  # Store in dataframe:
  teams <- teams_data[seq(2, 60, 2)]
  teamstats <- matrix(NA, nrow = 30, ncol = 18)
  
  for (i in 1:30) {
    teamstats[i,] <- teams_data[seq((62 + 19*(i-1)), ((62+19*(i-1))+17),1)]
  }
  
  team_df <- data.frame(cbind(teams, teamstats))
  colnames(team_df)[2:19] <- col_labels[4:21]
  
  return(team_df)
}

## ---- Define function to scrape post-season (Playoff) team data for one season: ------

scrapeplayoff <- function(url) {
  
  # Read webpage:
  webpage <- read_html(url)
  
  # Extract team information:
  teams_data_html <- html_nodes(webpage,".Table__TD")
  teams_data <-html_text(teams_data_html)
  col_labels_html <- html_nodes(webpage, ".Table__TH")
  col_labels <- html_text(col_labels_html)
  
  # Store in dataframe:
  teams <- teams_data[seq(2, 32, 2)]
  teamstats <- matrix(NA, nrow = 16, ncol = 18)
  
  for (i in 1:16) {
    teamstats[i,] <- teams_data[seq((34 + 19*(i-1)), ((34+19*(i-1))+17),1)]
  }
  
  team_df <- data.frame(cbind(teams, teamstats))
  colnames(team_df)[2:19] <- col_labels[4:21]
  
  return(team_df)
}


# ---- Define function to retreive team information from multiple seasons: ----
getdata <- function(startyear, endyear) {
  
  # Get general team data:
  data_teams <- list()
    for (i in endyear:startyear) {
      url <- paste0("https://www.espn.com/nba/stats/team/_/season/",i,"/seasontype/2")
      data_teams[[i - startyear+1]] <- data.frame(scrapeteams(url))
    }
  data_teams <- plyr::ldply(data_teams, rbind)  # combine all list elements, row-wise
  
  # Get playoff data:
  data_playoff <- list()
      for (i in endyear:startyear) {
        url <- paste0("https://www.espn.com/nba/stats/team/_/season/",i,"/seasontype/3")
        data_playoff[[i-startyear+1]] <- data.frame(scrapeplayoff(url))
      }
  
  data_playoff <- plyr::ldply(data_playoff, rbind) # combine all list elements, row-wise
  
  # Add season label:
  data_playoff <- data_playoff %>% mutate(season = rep(startyear:endyear, each = 16), playoffs = 1)
  data_teams <- data_teams %>% mutate(season = rep(startyear:endyear, each = 30))
  
  # Combine all info into master dataframe:
  data_master <- left_join(x = data_teams, y = data_playoff[,c("teams", "season", "playoffs")],
                           by = c("teams", "season"))
  data_master[is.na(data_master)] <- 0
  names(data_master)<- gsub("\\.", "", colnames(data_master))
  data_master[,c(2:19)] = apply(data_master[,c(2:19)], 2, function(x) as.numeric(as.character(x)))
  
  return(data_master)
}

# ---- Pull data and write to csv: -----
df <- getdata(2005,2019)