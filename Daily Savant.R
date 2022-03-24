library(baseballr)
library(DBI)
library(RSQLite)
library(lubridate)
library(tidyverse)

anydate(18824)

db = dbConnect(SQLite(),"R/Baseball/statcastDaily2021.sqlite")

chad = get_chadwick_lu()

datH2 = baseballr::scrape_statcast_savant(start_date = Sys.Date()-17, 
                                                end_date = Sys.Date()-15,
                                                player_type = "batter") %>% arrange(game_pk,at_bat_number)

datP2 = baseballr::scrape_statcast_savant(start_date = Sys.Date()-17, 
                                          end_date = Sys.Date()-15,
                                          player_type = "pitcher") %>% arrange(game_pk,at_bat_number)

dbWriteTable(db,"statcast_hitting",datH2,overwrite = F, row.names = F, append = T)
dbWriteTable(db,"statcast_pitching",datP2,overwrite = F, row.names = F, append = T)
dbWriteTable(db,"chadwick",chad,overwrite = T, row.names = F, append = F)
dbDisconnect(db)