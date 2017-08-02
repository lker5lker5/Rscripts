library(DBI)
library(RMySQL)
conn <- dbConnect(MySQL(),
                  dbname="marinefishery",
                  username="root",
                  password="root",
                  host="127.0.0.1", port=8889)
staff <- dbGetQuery(conn,"select * from staff")
staff
dbDisconnect(conn)