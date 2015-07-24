library(RMySQL)
con <- dbConnect(MySQL(),
                 user = 'jmoore1',
                 password = 'cleocleo',
                 host = 'intelligentpursuit.chyg0qy7bqrw.us-west-2.rds.amazonaws.com',
                 dbname='')