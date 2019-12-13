

setwd("C:\\Users\\d_log\\OneDrive\\MSIS5600- data")

data_all = read.csv("C:\\Users\\d_log\\OneDrive\\MSIS5600- data\\vgMetaFullCleanFinal.csv")

head(data_all)

nrow(data_all)

##mode for console
table(data_all$console)
prop.table(table(data_all$console))
round(100*prop.table(table(data_all$console)),1)

# mode for Genre
table(data_all$genre)
prop.table(table(data_all$genre))
round(100*prop.table(table(data_all$genre)),1)

# mode for ESRB 
table(data_all$esrb)
prop.table(table(data_all$esrb))
round(100*prop.table(table(data_all$esrb)),1)

# mode for number of players
table(data_all$numPlayers)
prop.table(table(data_all$numPlayers))
round(100*prop.table(table(data_all$numPlayers)),1)




mode(data_all$console)
class(data_all$week_posted) 
class(data_all$position)
class(data_all$title) 
class(data_all$url_title)
levels(data_all$position)
