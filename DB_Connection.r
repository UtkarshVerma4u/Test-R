setwd("D:\\Programs\\R\\R_Sessions")
# Load RODBC package
library(RODBC)
library(DBI)
pswd <- readline("Input Password: ")

# Create a connection to the database called "channel"
channel <- odbcConnect("isecdwh", uid="querydesk",  pwd=pswd, believeNRows=FALSE)

# Query the database and put the results into the data frame
# "dataframe"

dataframe <- sqlQuery(channel, "
SELECT * FROM FACT_MF_TRANSACTION")
library(rpivotTable)
rpivotTable(dataframe)
#############################################################
install.packages("DBI")
install.packages("odbc")

library(odbc)
sort(unique(odbcListDrivers()[[1]]))

con <- dbConnect(odbc(), "DSN name")

con <- dbConnect(odbc(), 
                 Driver = "SQL Server", 
                 Server = "localhost\\SQLEXPRESS", 
                 Database = "datawarehouse", 
                 Trusted_Connection = "True")

################### For Oracle DB ##############################

con <- DBI::dbConnect(odbc::odbc(),
                      Driver = "[your driver's name]",
                      Host   = "[your server's path]",
                      SVC    = "[your schema's name]",
                      UID    = rstudioapi::askForPassword("Database user"),
                      PWD    = rstudioapi::askForPassword("Database password"),
                      Port   = 1521)


download.file(url = "https://ndownloader.figshare.com/files/2292171",
              destfile = "data_raw/portal_mammals.sqlite", mode = "wb")

##########################################################################
library(DBI)
pswd <- readline("Input Password: ")

con <- dbConnect(odbc::odbc(), "isecdwh",uid="querydesk",  pwd=pswd, timeout = 10)
con
summary(con)

#List Tables
DBI::dbListTables(con)

#List Fields
DBI::dbListFields(con, "FACT_MF_TRANSACTION")

# Read Data in Batches
query  <- DBI::dbSendQuery(con, 'select * from FACT_MF_TRANSACTION')
query2 <- DBI::dbSendQuery(con,"SELECT * FROM isecdwh.DIM_CUSTOMER")
query3  <- DBI::dbSendQuery(con,"SELECT * FROM isecdwh.FACT_EQUITY_TRANSACTION")
query4  <- DBI::dbSendQuery(con,"SELECT * FROM isecdwh.DIM_FORM")

# first batch of 10 rows
require(data.table)
a<-data.table(DBI::dbFetch(query, n = 1000))
b<-data.table(DBI::dbFetch(query2, n = 1000))
c<-data.table(DBI::dbFetch(query3, n = 1000))
d<-data.table(DBI::dbFetch(query4, n = 1000))

str(b)
sapply(b, class)
b$DCG_CRM_CUSTOMER_ID <- as.character(b$DCG_CRM_CUSTOMER_ID)

# Write data to csv
fwrite(b, file = "D:\\Programs\\R\\R_Sessions\\sybase_DB\\DIM_Customer.csv", sep = "|")

fwrite(c, file = "D:\\Programs\\R\\R_Sessions\\sybase_DB\\EQUITY_TRANSACTION.csv", sep = "|")

fwrite(d, file = "D:\\Programs\\R\\R_Sessions\\sybase_DB\\DIM_FORM.csv", sep = "|")


head(b)

# Check Query Status Complete/Running
DBI::dbHasCompleted(query)

# Query Info
dbGetInfo(query) 

DBI::dbColumnInfo(query)

# Column Row Count
DBI::dbGetRowCount(query)

#Close Connection
DBI::dbDisconnect(con)
