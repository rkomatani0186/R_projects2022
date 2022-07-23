#Statcast Database for obtaining MLB data
library(baseballr)
library(DBI)
library(RSQLite)
library(lubridate)

#obtain MLB data from 2021 season
db = dbConnect(SQLite(), "statcast_db.sqlite")
statcast_write <- function() {
  dat1 = baseballr::scrape_statcast_savant(start_date = "2021-03-13", end_date = "2021-03-20")
  
  dat2 = baseballr::scrape_statcast_savant(start_date = "2021-03-21", end_date = "2021-03-27")
  
  dat3 = baseballr::scrape_statcast_savant(start_date = "2021-03-28", end_date = "2021-04-05")
  
  dat4 = baseballr::scrape_statcast_savant(start_date = "2021-04-06", end_date = "2021-04-13")
  
  dat5 = baseballr::scrape_statcast_savant(start_date = "2021-04-14", end_date = "2021-04-21")
  
  dat6 = baseballr::scrape_statcast_savant(start_date = "2021-04-22", end_date = "2021-04-29")
  
  dat7 = baseballr::scrape_statcast_savant(start_date = "2021-05-04", end_date = "2021-05-11")
  
  dat8 = baseballr::scrape_statcast_savant(start_date = "2021-05-18", end_date = "2021-05-25")
  
  dat9 = baseballr::scrape_statcast_savant(start_date = "2021-05-26", end_date = "2021-06-03")
  
  dat10 = baseballr::scrape_statcast_savant(start_date = "2021-06-04", end_date = "2021-06-11")
  
  dat11 = baseballr::scrape_statcast_savant(start_date = "2021-06-12", end_date = "2021-06-19")
  
  dat12 = baseballr::scrape_statcast_savant(start_date = "2021-06-20", end_date = "2021-06-28")
  
  dat13 = baseballr::scrape_statcast_savant(start_date = "2021-06-29", end_date = "2021-07-06")
  
  dat14 = baseballr::scrape_statcast_savant(start_date = "2021-07-07", end_date = "2021-07-14")
  
  dat15 = baseballr::scrape_statcast_savant(start_date = "2021-07-15", end_date = "2021-07-22")
  
  dat16 = baseballr::scrape_statcast_savant(start_date = "2021-07-23", end_date = "2021-07-30")
  
  dat17 = baseballr::scrape_statcast_savant(start_date = "2021-07-31", end_date = "2021-08-07")
  
  dat18 = baseballr::scrape_statcast_savant(start_date = "2021-08-08", end_date = "2021-08-15")
  
  dat19 = baseballr::scrape_statcast_savant(start_date = "2021-08-16", end_date = "2021-08-23")
  
  dat20 = baseballr::scrape_statcast_savant(start_date = "2021-08-24", end_date = "2021-08-30")
  
  dat21 = baseballr::scrape_statcast_savant(start_date = "2021-08-31", end_date = "2021-09-07")
  
  dat22 = baseballr::scrape_statcast_savant(start_date = "2021-09-08", end_date = "2021-09-15")
  
  dat23 = baseballr::scrape_statcast_savant(start_date = "2021-09-16", end_date = "2021-09-23")
  
  dat24 = baseballr::scrape_statcast_savant(start_date = "2021-09-24", end_date = "2021-10-01")
  
  dat25 = baseballr::scrape_statcast_savant(start_date = "2021-10-02", end_date = "2021-10-09")
  
  dat26 = baseballr::scrape_statcast_savant(start_date = "2021-10-10", end_date = "2021-10-13")
  
  savant_data_2021 = rbind(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9, dat10,
                           dat11, dat12, dat13, dat14, dat15, dat16, dat17, dat18, dat19, dat20,
                           dat21, dat22, dat23, dat24, dat25, dat26)
  SavantData2021 = as.data.frame(savant_data_2021)
  dbWriteTable(db, "statcast_data", SavantData2021, overwrite = F, row.names = F, append = T)
  dbDisconnect(db)
}

statcast_write()

#Export as csv file:
#sc = dbGetQuery(conn = db, statement = "select * from statcast_data")
#write.csv(sc, "C:/Users/12244/CSV_2021/sc.csv", row.names = FALSE)

#To use this:
# db = dbConnect(SQLite(), "statcast_db.sqlite")
# dbListTables(db)
# sc = dbGetQuery(conn = db, statement = "select * from statcast_data")
# dbDisconnect(db)


