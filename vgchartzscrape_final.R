library(RSelenium)
library(stringr)
library(dplyr)

#Set up vgchartz scraping
vgurl <- "http://www.vgchartz.com/weekly/43464/Global/"

rD <- rsDriver(verbose = FALSE, browser = 'firefox', port = 4568L)
remDr <- rD$client
remDr$navigate(vgurl)

#Help prevent timeout errors
remDr$setTimeout(type = "page load", milliseconds = 100000)

#Get all values for the chart urls
weekElements <- remDr$findElements(using = "tag", "option")
weekVals <- c()
for (i in c(1:length(weekElements))) {
    weekVals <- c(weekVals, weekElements[[i]]$getElementAttribute("value"))
}

#Chart is missing on this page
weekVals[[68]] <- NULL

global_chart_df <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(global_chart_df) <- c("chart_date", "position", "title", "url_title", "console", "developer", "genre", "units_week", "units_total", "chart_week")


for (val in weekVals) {
    url <- paste("https://vgchartz.com/weekly/", val, "/Global", sep = "")
    remDr$navigate(url)
    remDr$setTimeout(type = "page load", milliseconds = 100000)

    #Get chart and save all text results
    chart <- remDr$findElement(using = "class", "chart")
    results <- str_split(chart$getElementText(), "\n")

    #Parse out text results into the different variables
    pos_list <- c()
    titleConsole_list <- c()
    genreDev_list <- c()
    unitsWeek_list <- c()

    #The first result contains the column headers
    for (i in c(2:length(results[[1]]))) {
        if (str_detect(results[[1]][i], "^[0-9]{1,2}$")) {
            pos_list <- c(pos_list, results[[1]][i])
        }
        else if (str_detect(results[[1]][i], "\\)$")) {
            titleConsole_list <- c(titleConsole_list, results[[1]][i])
        }
        else if (str_detect(results[[1]][i], "[a-z]$")) {
            genreDev_list <- c(genreDev_list, results[[1]][i])
        }
        else if (str_detect(results[[1]][i], "\\s[0-9]*$")) {
            unitsWeek_list <- c(unitsWeek_list, results[[1]][i])
        }
    }

    #Split titleConsole into title and console, genreDev into genre and developer, and unitsWeek into units weekly, units total, and week
    title_list <- c()
    url_title_list <- c()
    console_list <- c()
    dev_list <- c()
    genre_list <- c()
    unitsWeekly_list <- c()
    unitsTotal_list <- c()
    week_list <- c()

    for (i in c(1:length(pos_list))) {
        title_split <- str_split(titleConsole_list[i], " ")
        len <- length(title_split[[1]]) - 1
        title <- paste(title_split[[1]][1:len], collapse = " ")
        console <- title_split[[1]][len + 1]

        #Certain titles are different between vgchartz and metacritic
        if (title == "Super Smash Bros. (2018)") {
            title <- "Super Smash Bros. Ultimate"
        }
        else if (str_detect(title, "IIII$")) {
            title <- "Call of Duty: Black Ops 4"
        }
        else if (title == "Spider-Man (PS4)") {
            title <- "Marvel's Spider-Man"
        }
        else if (title == "Crash Bandicoot N.Sane Trilogy") {
            title <- "Crash Bandicoot N Sane Trilogy"
        }
        else if (title == "Minecraft") {
            if (console == "(NS)") {
                title = "Minecraft: Switch Edition"
            }
            else if (console == "(PS4)") {
                title = "Minecraft: PlayStation 4 Edition"
            }
            else if (console == "(XOne)") {
                title = "Minecraft: Xbox One Edition"
            }
            else if (console == "(X360)") {
                title = "Minecraft: Xbox 360 Edition"
            }
            else if (console == "(PSV)") {
                title = "Minecraft: PlayStation Vita Edition"
            }
            else if (console == "(WiiU)") {
                title = "Minecraft: Wii U Edition"
            }
        }

        title_list <- c(title_list, title)

        #Alter the title to use in a metacritic url
        url_title <- tolower(title)
        url_title <- trimws(sub("\\(ps4\\)", "", url_title))
        url_title <- gsub(" ", "-", url_title)
        url_title <- gsub("[\\(|\\)]", "", url_title)
        url_title <- gsub("[\\.|\'|\\,|\\:]", "", url_title)
        url_title_list <- c(url_title_list, url_title)

        #Change console to how it appears in a metacritic url
        if (console == "(NS)") {
            console = "switch"
        }
        else if (console == "(PS4)") {
            console = "playstation-4"
        }
        else if (console == "(XOne)") {
            console = "xbox-one"
        }
        else if (console == "(3DS)") {
            console = "3ds"
        }
        else if (console == "(PC)") {
            console = "pc"
        }
        else if (console == "(Wii)") {
            console = "wii"
        }
        else if (console == "(WiiU)") {
            console = "wii-u"
        }
        else if (console == "(X360)") {
            console = "xbox-360"
        }
        else if (console == "(PSV)") {
            console = "playstation-vita"
        }
        else if (console == "(PS3)") {
            console = "playstation-3"
        }
        else if (console == "(PSP)") {
            console = "psp"
        }
        else if (console == "(DS)") {
            console = "ds"
        }
        else if (console == NA) {
            console
        }

        console_list <- c(console_list, console)
        
        #Assemble other lists
        dev_list <- c(dev_list, str_split(genreDev_list[i], ", ")[[1]][1])
        genre_list <- c(genre_list, str_split(genreDev_list[i], ", ")[[1]][2])
        unitsWeekly_list <- c(unitsWeekly_list, str_split(unitsWeek_list[i], " ")[[1]][1])
        unitsTotal_list <- c(unitsTotal_list, str_split(unitsWeek_list[i], " ")[[1]][2])
        week_list <- c(week_list, str_split(unitsWeek_list[i], " ")[[1]][3])
    }

    #Get text for the week of the chart
    weekSelector = paste("option[value='", val,"']", sep = "")
    chartWeek <- remDr$findElement(using = "css", weekSelector)$getElementText()

    #Assemble individual rows based on the previous lists and add them to the global data frame
    for (i in c(1:75)) {
        newrow <- data.frame("week_posted" = chartWeek[[1]], "position" = pos_list[i], "title" = title_list[i], "url_title" = url_title_list[i], "console" = console_list[i], "developer" = dev_list[i],
                "genre" = genre_list[i], "units_week" = unitsWeekly_list[i], "units_total" = unitsTotal_list[i],
                "chart_week" = week_list[i], stringsAsFactors = FALSE)
        global_chart_df <- rbind(global_chart_df, newrow)
    }

    #Print the week of the chart for reference in the console
    print(paste("Added ", chartWeek, " chart."))
}

#Narrow the full data frame to the top 30 of each chart to keep only rows with all data
global_top30_df <- global_chart_df[!(global_chart_df$units_week == "Pro"),]

#The script defaulted to 75 rows per chart, even though some of the later charts don't have 75 rows. This deletes the resulting NA rows
global_top30_clean_df <- na.omit(global_top30_df)

#Export data
setwd("C:\\Users\\Aaron\\Google Drive\\School Stuff\\Fall 2019\\Data Science Programming I\\DSP1 Semester Project\\Data")
write.csv(global_chart_df, "global_chart_full.csv")
write.csv(global_top30_df, "global_top30.csv")
write.csv(global_top30_clean_df, "top30_noNA.csv")

#Close the session
remDr$close()
rD$server$stop()