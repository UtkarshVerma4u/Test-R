library(DBI)
require(data.table)
pswd <- readline("Input Password: ")

con <- dbConnect(odbc::odbc(), "isecdwh",uid="querydesk",  pwd=pswd, timeout = 10)
con
summary(con)

#List Tables
DBI::dbListTables(con)

#List Fields
DBI::dbListFields(con, "FACT_MF_TRANSACTION")

# Read Data in Batches
query  <- DBI::dbSendQuery(con, 'select * from ISECDWH.DIM_CUSTOMER_ACCOUNT')
q


# first batch of 10 rows
a<-data.table(DBI::dbFetch(query, n = 1000))

# Write data to csv
fwrite(a, file = "D:\\Programs\\R\\R_Sessions\\sybase_DB\\DIM_CUSTOMER_ACCOUNT.csv", sep = "|")

# Check Query Status Complete/Running
DBI::dbHasCompleted(query)

# Query Info
dbGetInfo(query) 

DBI::dbColumnInfo(query)

# Column Row Count
DBI::dbGetRowCount(query)

#Close Connection
DBI::dbDisconnect(con)
