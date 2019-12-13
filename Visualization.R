library(RSelenium)
library(stringr)
library(forcats)
library(ggplot2)
library(zoo)
library(extrafont)
library(grid)
library(dplyr)
library(dbplyr)
library(sqldf)
library(corrplot)
#Font
font_import()
loadfonts(device = "win")
##########################################GETTING DATA READY FOR PLOTS##############################
#working directory
setwd("C:\\Users\\emcat\\Desktop\\Data Science Programming")
#Read the tables
data <- read.csv("vgMetaFullCleanFinal.csv ", header = TRUE)
#SQL to find the highest sales value for the each distint name
#Testing number of distinct titles
distincttitle=sqldf("SELECT Distinct title
FROM data ;")
unitstotal= sqldf("SELECT Max(units_total), title
FROM data ;")
#testing sql query
test = sqldf("SELECT units_total,title
FROM data
Group By title;") 
#Unique genres to find the number of genre for later counts
genretest = sqldf("SELECT genre
FROM data
Group By Genre;")
#Unique publishers to find the number of publishers for later counts
publishertest = sqldf("SELECT publisher
FROM data
Group By publisher;
")
#Unique developers to find the number of developers in later couunts
developertest = sqldf("SELECT developer
FROM data
Group By developer;
")
#sql query that narrows dataset to the top count sold per game
newdata = sqldf("SELECT *
FROM data
Group By title;")
#New dataframe of only the top count sold per game
write.csv(newdata, 'test.csv')
#numeric values- dataframe of only the numeric values
numeric = data.frame(newdata$units_total, newdata$metascore, newdata$user_score_x10, newdata$score_diff, newdata$chart_week)
numeric
#Changing names of the columns of the dataframe
colnames(numeric) = c("Total Unit Sales", "Metascore", "User_Score", "Score_Diff", "ChartWeek")
colnames(numeric)
#Creates count of number of games per esrb score
esrb_game <- newdata %>%
    count(esrb, sort = TRUE) %>%
    #filters out NA's and otherwise useless values
    filter(esrb %in% c('M', 'T', 'E', 'E10+'))
#Creates filtered ESRB sales
esrb_game_sales <- newdata %>%
    select(esrb, units_total) %>%
    group_by(esrb) %>%
    filter(esrb %in% c('M', 'T', 'E', 'E10+')) %>%
    summarise(Units_Sold = sum(units_total))
#Multiplayer sales DOES NOT WORK
multiplayer_game_sales <- newdata %>%
    select(multi_count, units_total) %>%
    group_by(multi_count) %>%
    summarise(Units_Sold = sum(units_total))
#Creates count of number of games per console
Console_game <- newdata %>%
    count(console, sort = TRUE) %>%
    #filters out NA's and otherwise useless values
    filter(console %in% c('switch', 'playstation-4', 'xbox-one'))
#Top ten title
top10_title <- newdata %>%
    select(title, units_total) %>%
    arrange(desc(units_total)) %>%
    head(n = 10)
#botton 10 title
bottom10_title <- newdata %>%
    select(title, units_total) %>%
    arrange(desc(units_total)) %>%
    tail(n = 10)
#Top 10 developer
top10_developer <- newdata %>%
    select(developer, units_total) %>%
    arrange(desc(units_total)) %>%
    head(n = 10)
#Bottom 10 developer
bottom10_developer <- newdata %>%
    select(developer, units_total) %>%
    arrange(desc(units_total)) %>%
    tail(n = 10)

#Top 10 publisher
top10_publisher <- newdata %>%
    select(publisher, units_total) %>%
    arrange(desc(units_total)) %>%
    head(n = 10)
#Bottom 10 publisher
bottom10_publisher <- newdata %>%
    select(publisher, units_total) %>%
    arrange(desc(units_total)) %>%
    tail(n = 10)
#Creates count of number of genres
Genre_game <- newdata %>%
    count(genre, sort = TRUE) %>%
    #filters out NA's and otherwise useless values
    filter(genre %in% c('Action','Action - Adventure','Adventure','Fighting','Misc',
'Music',
 'Party',
 'Platform',
 'Puzzle',
 'Racing',
 'Role - Playing',
 'Sandbox',
 'Shooter',
'Simulation',
'Sports',
 'Strategy'))
#Creates count of number of publishers
Publishier_game <- newdata %>%
    count(publisher, sort = TRUE) %>%
    #filters out NA's and otherwise useless values
    filter(publisher %in% c(
 '505 Games'
, 'Activision'
, 'Aqua Plus'
, 'Arc System Works'
, 'Atlus'
, 'Bandai Namco Entertainment'
, 'Bethesda Softworks'
, 'Big Ben Interactive'
, 'CCP'
, 'Capcom'
, 'Capcom Entertainment'
, 'City Interactive'
, 'Codemasters'
, 'Crytek'
, 'D3Publisher'
, 'Deep Silver'
, 'Disney Interactive Studios'
, 'Dusenberry Martin Racing'
, 'Electronic Arts'
, 'Focus Home Interactive'
, 'From Software'
, 'Frontier Developments'
, 'FuRyu Corporation'
, 'Gearbox Software'
, 'Grey Box'
, 'Gun Media'
, 'Harmonix Music Systems'
, 'Hello Games'
, 'Idea Factory'
, 'Insomniac Games'
, 'Kadokawa Games'
, 'Kalypso Media'
, 'Koch Media'
, 'Konami Digital Entertainment'
, 'Marvelous'
, 'Maximum Games'
, 'Merge Games'
, 'Microsoft Game Studios'
, 'Microsoft Studios'
, 'Milestone'
, 'Milestone S.r.l.'
, 'Mojang'
, 'Namco Bandai Games'
, 'Natsume'
, 'Nicalis'
, 'Nighthawk Interactive'
, 'Nihon Falcom Corporation'
, 'Nintendo'
, 'Nippon Ichi Software'
, 'PQube'
, 'Rebellion Developments'
, 'Sega'
, 'Slightly Mad Studios'
, 'Sold Out'
, 'Sony Computer Entertainment'
, 'Sony Computer Entertainment America'
, 'Sony Computer Entertainment Europe'
, 'Sony Interactive Entertainment'
, 'Spike Chunsoft'
, 'Square Enix'
, 'Studio Wildcard'
, 'THQ Nordic'
, 'Take - Two Interactive'
, 'Tecmo Koei'
, 'Telltale Games'
, 'Ubisoft'
, 'Unknown'
, 'Warner Bros. Interactive Entertainment'
, 'Xseed Games'))

#Creates Count of games per developer
developer_game <- newdata %>%
    count(developer, sort = TRUE) %>%
    filter(!developer %in% c(""))



#add a boolean column for singleplayer/multiplayer
single <- c("2", "No Online Multiplayer")
newdata$multiplayer <- ""
for (i in c(1:nrow(newdata))) {
    if (newdata[i, ]$numPlayers %in% single) {
        newdata[i,]$multiplayer = "single"
    } else if (is.na(newdata[i, ]$numPlayers)) {
        newdata[i,]$multiplayer = "N/A"
    } else {
        newdata[i,]$multiplayer = "multi"
    }
}

newdata$multiplayer <- as.factor(newdata$multiplayer)

multi_count <- newdata %>%
    count(multiplayer, sort = TRUE)
newdata
#Omitting nulls for regression
ompleterecords <- na.omit(numeric)
###############################################################################################

###################GRAPHS BY SALES(TARGET VARIABLES) BY PREDICTOR VARIABLES########################
#Sales with metascore
salesmeetascore <- ggplot(data = newdata, aes(x = newdata$metascore, y = newdata$units_total)) +
    geom_point(color="#FF6666")

salesmeetascore + ggtitle("Total Units Sold by MetaScore") +
    xlab("MetaScore") + ylab("Units Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60"))

#Sales with user score
salesmeetascore <- ggplot(data = newdata, aes(x = newdata$user_score, y = newdata$units_total)) +
    geom_point(color = "cornflowerblue")

salesmeetascore + ggtitle("Total Units Sold by UserScore") +
    xlab("UserScore") + ylab("Units Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60"))

#total units sold with esrb
salesesrb <- ggplot(data = esrb_game_sales, aes(x = esrb, y = Units_Sold)) +
    geom_bar(stat = "identity", fill = "#FF6666")

salesesrb + ggtitle("Total Units Sold by ESRB") +
    xlab("ESRB") + ylab("Units Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60")) 


#total units sold with title
salestitle <- ggplot(data = newdata, aes(x = newdata$title, y = newdata$units_total)) +
    geom_bar(stat = "identity", fill = "#FF6666")
salestitle + ggtitle("Total Units Sold by Title") +
    xlab("Title") + ylab("Units Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 6, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60"))

# top 10 total units sold with title
top10salestitle <- ggplot(data = top10_title, aes(x = title, y = units_total)) +
    geom_bar(stat = "identity", fill = "#FF6666")
top10salestitle + ggtitle("Top 10 Total Units Sold by Title") +
    xlab("Title") + ylab("Units Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 6, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60"))

#Bottom total units sold with title
bottomsalestitle <- ggplot(data = bottom10_title, aes(x = title, y = units_total)) +
    geom_bar(stat = "identity", fill = "#FF6666")
bottomsalestitle + ggtitle("Bottom 10 Total Units Sold by Title") +
    xlab("Title") + ylab("Units Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 6, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60"))

#Units total with console
salesconsole <- ggplot(data = newdata, aes(x = newdata$console, y = newdata$units_total)) +
    geom_bar(stat = "identity", fill = "#FF6666")
salesconsole + ggtitle("Total Units Sold by Console") +
    xlab("Console") + ylab("Units Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60"))

#sales publisher
salespublisher <- ggplot(data = newdata, aes(x = newdata$publisher, y = newdata$units_total)) +
    geom_bar(stat = "identity", fill = "#FF6666")
salespublisher + ggtitle("Total Units Sold by Publisher") +
    xlab("Publisher") + ylab("Units Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60"))

#sales publisher top 10
salespublishertopten <- ggplot(data = top10_publisher, aes(x = publisher, y = units_total)) +
    geom_bar(stat = "identity", fill = "#FF6666")
salespublishertopten + ggtitle(" Top 10 Total Units Sold by Publisher") +
    xlab("Publisher") + ylab("Units Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60"))

#sales publisher bottom 10
salespublishertopten <- ggplot(data = bottom10_publisher, aes(x = publisher, y = units_total)) +
    geom_bar(stat = "identity", fill = "#FF6666")
salespublishertopten + ggtitle("Bottom 10 Total Units Sold by Publisher") +
    xlab("Publisher") + ylab("Units Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60"))



#total sales with developer
salesdeveloper <- ggplot(data = newdata, aes(x = newdata$developer, y = newdata$units_total)) +
    geom_bar(stat = "identity", fill = "cornflowerblue")
salesdeveloper + ggtitle("Total Units Sold by Developer") +
    xlab("Developer") + ylab("Units Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60"))


# Top 10 total sales with developer
salesdeveloper <- ggplot(data = top10_developer, aes(x = developer, y = units_total)) +
    geom_bar(stat = "identity", fill = "cornflowerblue")
salesdeveloper + ggtitle("Top 10 Total Units Sold by Developer") +
    xlab("Developer") + ylab("Units Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60"))

#Bottom 10 total sales with developer
salesdeveloper <- ggplot(data = bottom10_developer, aes(x = developer, y = units_total)) +
    geom_bar(stat = "identity", fill = "cornflowerblue")
salesdeveloper + ggtitle("Total Units Sold by Developer") +
    xlab("Developer") + ylab("Units Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60"))


#Total Sales by Genre
salesgenre <- ggplot(data = newdata, aes(x = newdata$genre, y = newdata$units_total)) +
    geom_bar(stat = "identity", fill = "cornflowerblue")
salesgenre + ggtitle("Total Units Sold by Genre") +
    xlab("Genre") + ylab("Units Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60"))


#Sales by Chart week
saleschartweek <- ggplot(data = newdata, aes(x = newdata$chart_week, y = newdata$units_total)) +
    geom_bar(stat = "identity", fill = "cornflowerblue")
saleschartweek + ggtitle("Total Units Sold by Chart Week") +
    xlab("Chart Week") + ylab("Units Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60"))

#Sales by score_diff
salesscorediff <- ggplot(data = newdata, aes(x = newdata$score_diff, y = newdata$units_total)) +
    geom_bar(stat = "identity", fill = "#FF6666")
salesscorediff + ggtitle("Total Units Sold by Score Diff") +
    xlab("Score Diff") + ylab("Units Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60"))


#GETTING ERROR 
salesmultiplayer <- ggplot(data = multi_count, aes(x = multiplayer, y = newdata$units_total)) +
    geom_bar(stat = "identity", fill = "#FF6666")
salesmultiplayer + ggtitle("Total Units Sold by Title") +
    xlab("Title") + ylab("Units Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 6, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60"))


############################################################################################


#########################STATISITCS PlOTS#################################################
#####################Scatterplots
#Units total plot
plot(newdata$units_total, main = "Total Sales Plot")
#metascore plot
plot(newdata$metascore, main = "MetaScore Plot")
#user score plot
plot(newdata$user_score_x10, main = "UserScore Plot")
#Score_diff plot
plot(newdata$score_diff, main = "Score_Diff Plot")
#Chart week plot
plot(newdata$chart_week, main = "Chart Week Plot")
#####################Boxplots
#boxplot All
boxplot(numeric, main = "Box Plot All")
#Metascore boxplot
boxplot(newdata$metascore, main = "MetaScore  Box Plot")
#units total sold boxplot
boxplot(newdata$units_total, main = "Total Sales  Box Plot")
#user score boxplot
boxplot(newdata$user_score_x10, main = "UserScore  Box Plot")
#score diff boxplot
boxplot(newdata$score_diff, main = "Score_Diff  Box Plot")
#Chart week boxplots
boxplot(newdata$chart_week, main = "chartweek  Box Plot")
#####Scatterplot pairs############################
pairs(numeric, panel = panel.smooth)
######################################QQPlots
#Units total
qqnorm(newdata$units_total, main = "Total sales QQ")
qqline(newdata$units_total, lty = 2)
shapiro.test(newdata$units_total)
#Metascore
qqnorm(newdata$metascore, main = "MetaScore QQ")
qqline(newdata$metascore, lty = 2)
shapiro.test(newdata$metascore)
#Userscore
qqnorm(newdata$user_score_x10, main = "UserScore QQ")
qqline(newdata$user_score_x10, lty = 2)
shapiro.test(newdata$user_score_x10)
#Score_diff
qqnorm(newdata$score_diff, main="Score_diff QQ")
qqline(newdata$score_diff, lty = 2)
shapiro.test(newdata$score_diff)
#Chart week
qqnorm(newdata$chart_week, main = "Chart Week QQ")
qqline(newdata$chart_week, lty = 2)
shapiro.test(newdata$chart_week)
##################################Histograms
hist(newdata$units_total, main = "Total Sales Histogram")
hist(newdata$metascore, main = "MetaScore Histogram")
hist(newdata$user_score_x10, main = "UserScore Histogram")
hist(newdata$score_diff, main = "Score_Diff Histogram")
hist(newdata$chart_week, main = "Chart Week Histogram")
############################################################################################

#########################################ADDITIONAL PLOTS#######################################
#Count of each ESRB
esrb_game_plot <- ggplot(esrb_game, aes(x = esrb, y = n)) +
    geom_bar(stat = "identity", fill = "#FF6666")
esrb_game_plot + ggtitle("Count of Games Per ESRB") +
    xlab("ESRB") + ylab("Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60"))
#Count of each console
Console_game_plot <- ggplot(Console_game, aes(x = console, y = n)) +
    geom_bar(stat = "identity", fill = "cornflowerblue")
Console_game_plot + ggtitle("Count of Games per Console") +
    xlab("Console") + ylab("Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60"))
#Count of each genre
Genre_game_plot <- ggplot(Genre_game, aes(x = genre, y = n)) +
    geom_bar(stat = "identity", fill = "cornflowerblue")
Genre_game_plot + ggtitle("Count of Games per Genre") +
    xlab("Genre") + ylab("Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60"))
#Count of each publisher
publisher_game_plot <- ggplot(Publishier_game, aes(x = publisher, y = n)) +
    geom_bar(stat = "identity", fill = "#FF6666")
publisher_game_plot + ggtitle("Count of Games per Publisher") +
    xlab("Publisher") + ylab("Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 6, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60"))
#Count of each developer
developer_game_plot <- ggplot(developer_game, aes(x = developer, y = n)) +
    geom_bar(stat = "identity", fill = "cornsilk")
developer_game_plot + ggtitle("Count of Games per Developer") +
    xlab("Developer") + ylab("Units Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60"))
#correlation matrix/correlation plot
res <- cor(ompleterecords)
round(res, 2)
corrplot(res, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
#Multiplayer Count
Multiplayer_game_plot <- ggplot(multi_count, aes(x = multiplayer, y = n)) +
    geom_bar(stat = "identity", fill = "#FF6666")
Multiplayer_game_plot + ggtitle("Count of Players in a game") +
    xlab("Number of Player") + ylab("Total") +
    theme(
#changes colors and fonts and size of axis labels
axis.title.x = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
axis.title.y = element_text(color = "white", size = 12, face = "bold", family = "Georgia"),
#changes colors and fonts and size of axis 
axis.text.x = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
axis.text.y = element_text(color = "white", size = 10, face = "bold", family = "Arial"),
#background color
plot.background = element_rect(fill = "gray"),
#panel color
panel.background = element_rect(fill = "gray60"))

#######Time Series
#Sales
data$week_posted=as.Date(data$week_posted,"%m / %d / %Y")
timeseriessales=ggplot(data, aes(data$week_posted, data$units_total)) + geom_line() +
    scale_x_date() +xlab("") + ylab("Daily Views")
timeseriesconsole = ggplot(data, aes(data$week_posted, data$console)) + geom_line() +
    scale_x_date() + xlab("") + ylab("Daily Views")

timeseriessales