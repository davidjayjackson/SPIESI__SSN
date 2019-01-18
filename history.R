
##########################################################
#####     Libraries
##########################################################

library(data.table)
library(RMySQL)
library(RSQLite)


# Import history file : 2010-2018
HISTORY <- fread(file.choose())
# Ymd to history db
HISTORY$Ymd <- as.Date(paste(HISTORY$year, HISTORY$mon, HISTORY$day, sep = "-"))
HISTORY <- subset(HISTORY,year <=2018)
summary(HISTORY)
# MySQ and RMySQL stuff

mydb <- dbConnect(MySQL(),user='root',password='dJj12345',dbname="gn",
                  host='localhost')
#
dbListTables(mydb)
# Drop Tables Run once and comment out dbRomveTable
dbRemoveTable(mydb,"daily")
# Craete DAILY table from history data: 2010-2017
dbWriteTable(mydb,"DAILY",HISTORY, row.names=FALSE)
dbSendStatement(mydb, "ALTER TABLE DAILY MODIFY COLUMN Ymd date")
dbSendStatement(mydb, "ALTER TABLE DAILY MODIFY COLUMN w int")
# dbDisconnect(mydb)
#
# SQLite stuff
db <- dbConnect(SQLite(), dbname="Rhowe.sqlite3")
dbListTables(db)
dbRemoveTable(db,"daily")
# # SQLite: stuff
# 
dbListTables(db)
# # Creat table and Insert data.frame(overwrites existing table)
# Convert Ymd field to Character for import into sqlite
HISTORY$Ymd <- as.character(HISTORY$Ymd)
dbWriteTable(db, "DAILY", HISTORY,overwrite=TRUE)


# # SQLite: stuff
# 
dbListTables(db)

