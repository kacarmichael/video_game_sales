library(RSelenium)
library(stringr)
library(plyr)
library(dplyr)
library(data.table)

setwd("C:\\Users\\Aaron\\Google Drive\\School Stuff\\Fall 2019\\Data Science Programming I\\DSP1 Semester Project\\Data")

#Import vgchartz data
vgData <- read.csv("top30_noNA.csv")

#21963 rows

current_gen <- c('switch', 'playstation-4', 'xbox-one')
currrent_gen_data <- vgData[(vgData$console %in% current_gen),]
write.csv(currrent_gen_data, "top30_currentGen.csv")

#5476 rows

#Split between multiple R environments to speed up the process.
#In 5 groups of 1095

index1 <- c(1:1095)
index2 <- c(1096:2190)
index3 <- c(2191:3285)
index4 <- c(3286:4380)
index5 <- c(4381:5476)

rD <- rsDriver(verbose = FALSE, browser = 'firefox', port = 4568L)
remDr <- rD$client

#Prevent timeout errors
remDr$setTimeout(type = "page load", milliseconds = 100000)

meta_df <- data.frame(matrix(ncol = 18, nrow = 0))
colnames(meta_df) <- c("week_posted", "position", "title", "url_title",
         "console", "developer", "genre", "units_week", "units_total", "chart_week",
         "numPlayers", "esrb", "metaGenre", "metaDevs", "release_date", "metascore",
         "user_score", "summary")

#Change index depending on environment and run simultaneously
for (i in index3) {
    metaURL <- paste("https://www.metacritic.com/game/", as.character(currrent_gen_data[i,]$console), "/", currrent_gen_data[i,]$url_title, sep = "")

    tryCatch({
        remDr$navigate(metaURL)
    }, warning = function(w) {
        print(paste("Warning: ", w))
    }, error = function(e) {
        print(paste("Error: ", e))
    }, finally = {
        print(paste("Started scan for ", currrent_gen_data[i,]$title, " on Metacritic."))
    })

    metascore <- tryCatch({
        remDr$findElement(using = "css", "span[itemprop='ratingValue']")$getElementText()[[1]]
    }, warning = function(w) {
        print(paste("Warning: ", w))
        metascore <- 999
    }, error = function(e) {
        print(paste("Error: ", e))
        metascore <- 999
    }, finally = {
        print(paste("Scanned MetaScore for ", currrent_gen_data[i,]$title, " on Metacritic."))
    })

    userscore <- tryCatch({
        remDr$findElement(using = "css", "div[class='metascore_w user large game positive'], div[class='metascore_w user large game negative'], div[class='metascore_w user large game mixed']")$getElementText()[[1]]
    }, warning = function(w) {
        print(paste("Warning: ", w))
        userscore <- 999
    }, error = function(e) {
        print(paste("Error: ", e))
        userscore <- 999
    }, finally = {
        print(paste("Scanned user score for ", currrent_gen_data[i,]$title, " on Metacritic."))
    })

    numPlayers <- tryCatch({
        remDr$findElement(using = "css", "li.summary_detail.product_players > span:nth-child(2)")$getElementText()[[1]]
    }, warning = function(w) {
        print(paste("Warning: ", w))
        numPlayers <- "WARNING"
    }, error = function(e) {
        print(paste("Error: ", e))
        numPlayers <- "ERROR"
    }, finally = {
        print(paste("Scanned # of players for ", currrent_gen_data[i,]$title, " on Metacritic."))
    })

    releaseDate <- tryCatch({
        remDr$findElement(using = "css", ".release_data > span:nth-child(2)")$getElementText()[[1]]
    }, warning = function(w) {
        print(paste("Warning: ", w))
        releaseDate <- "WARNING"
    }, error = function(e) {
        print(paste("Error: ", e))
        releaseDate <- "ERROR"
    }, finally = {
        print(paste("Scanned release date for ", currrent_gen_data[i,]$title, " on Metacritic."))
    })

    esrb_rating <- tryCatch({
        remDr$findElement(using = "css", "li.summary_detail:nth-child(5) > span:nth-child(2)")$getElementText()[[1]]
    }, warning = function(w) {
        print(paste("Warning: ", w))
        esrb_rating <- "WARNING"
    }, error = function(e) {
        print(paste("Error: ", e))
        esrb_rating <- "ERROR"
    }, finally = {
        print(paste("Scanned esrb rating for ", currrent_gen_data[i,]$title, " on Metacritic."))
    })

    #Check for existance of expanding button. If it exists, click and get the summary. If not, get the summary
    #Embedded try/catch should execute if the expand button is not present, throwing a NoSuchElement Exception
    if (length(remDr$findElements(using = "css", ".product_summary > span:nth-child(2) > span:nth-child(1) > span:nth-child(4)")) > 0) {
        remDr$findElement(using = "css", ".product_summary > span:nth-child(2) > span:nth-child(1) > span:nth-child(4)")$clickElement()
        tryCatch({
            summary <- remDr$findElement(using = "css", ".inline_expanded > span:nth-child(2)")$getElementText()[[1]]
        }, warning = function(w) {
            print(paste("Warning: ", w))
            summary <- "Warning"
        }, error = function(e) {
            print(paste("Error: ", e))
            summary <- "ERROR"
        })

        print(paste("Scanned summary for ", currrent_gen_data[i,]$title, " on Metacritic."))
    }

    else if (length(remDr$findElements(using = "css", ".summary_detail.product_summary > span:nth-child(2)")) > 0) {
        tryCatch({
            summary <- remDr$findElement(using = "css", ".summary_detail.product_summary > span:nth-child(2)")$getElementText()[[1]]
        }, warning = function(w) {
            print(paste("Warning: ", w))
            summary <- "Warning"
        }, error = function(e) {
            print(paste("Error: ", e))
            summary <- "ERROR"
        }, finally = {
            print(paste("Scanned summary for ", currrent_gen_data[i,]$title, " on Metacritic."))
        })
    }

    else {
        summary <- "No Summary Found"
        print("No Summary Found")
    }

    metagenre <- tryCatch({
        remDr$findElement(using = "css", ".product_genre")$getElementText()[[1]]
    }, warning = function(w) {
        print(paste("Warning: ", w))
        metagenre <- "WARNING"
    }, error = function(e) {
        print(paste("Error: ", e))
        metagenre <- "ERROR"
    }, finally = {
        print(paste("Scanned metagenre for ", currrent_gen_data[i,]$title, " on Metacritic."))
    })

    metadev <- tryCatch({
        remDr$findElement(using = "css", ".developer")$getElementText()[[1]]
    }, warning = function(w) {
        print(paste("Warning: ", w))
        metadev <- "WARNING"
    }, error = function(e) {
        print(paste("Error: ", e))
        metadev <- "ERROR"
    }, finally = {
        print(paste("Scanned metadev for ", currrent_gen_data[i,]$title, " on Metacritic."))
    })

    newrow <- data.frame("week_posted" = currrent_gen_data[i,]$week_posted,
                         "position" = currrent_gen_data[i,]$position,
                         "title" = currrent_gen_data[i,]$title,
                         "url_title" = currrent_gen_data[i,]$url_title,
                         "console" = currrent_gen_data[i,]$console,
                         "developer" = currrent_gen_data[i,]$developer,
                         "genre" = currrent_gen_data[i,]$genre,
                         "units_week" = currrent_gen_data[i,]$units_week,
                         "units_total" = currrent_gen_data[i,]$units_total,
                         "chart_week" = currrent_gen_data[i,]$chart_week,
                         "numPlayers" = numPlayers,
                         "esrb" = esrb_rating,
                         "metaGenre" = metagenre,
                         "metaDevs" = metadev,
                         "release_date" = releaseDate,
                         "metascore" = metascore,
                         "user_score" = userscore,
                         "summary" = summary)

    meta_df <- rbind(meta_df, newrow)
}

#Change name depending on index, will end with 5 different csv's
write.csv(meta_df, "vgMetaFullI3.csv")

#Combine the five individual indices
I1_df <- read.csv("vgMetaFullI1.csv")
I2_df <- read.csv("vgMetaFullI2.csv")
I3_df <- read.csv("vgMetaFullI3.csv")
I4_df <- read.csv("vgMetaFullI4.csv")
I5_df <- read.csv("vgMetaFullI5.csv")

vgMetaFull_df <- rbind(I1_df, I2_df, I3_df, I4_df, I5_df)
write.csv(vgMetaFull_df, "vgMetaFull.csv")

#These games are not on metacritic
gamesNotFound = c("Super Robot Wars X", "Super Robot Wars V", "Super Robot Wars OG: The Moon Dwellers", "Hokuto ga Gotoku",
                  "Girls und Panzer: Dream Tank Match", "Gintama Ranbu", "City Shrouded in Shadow",
                  "Fortune Street: Dragon Quest & Final Fantasy 30th Anniversary", "Monster Hunter Double Cross",
                  "Fate/Extella Link", "Fate/Extella: The Umbral Star", "Seiken Densetsu Collection", "Deformers",
                  "Musou Stars", "Blue Reflection: Maboroshi ni Mau - Shoujo no Ken",
                  "Dragon Quest Heroes I & II for Nintendo Switch", "Yonmegami Online: Cyber Dimension Neptune",
                  "Yakuza Zero: The Place of Oath", "New Danganronpa V3: Minna no Koroshiai Shin Gakki",
                  "Utawarerumono: Futari no Hakuoro", "Utawarerumono: itsuwari no kamen", "Utawarerumono: chiriyukusha e no komoriuta",
                  "The Idolmaster: Platinum Stars", "Jikkyou Powerful Pro Baseball 2016", "Jikkyou Powerful Pro Baseball 2018",
                  "Gundam Breaker 3", "Kamen Rider: Battride War Genesis", "Dragon's Dogma Online", "Sengoku Basara 4: Sumeragi",
                  "Hyperdimension Neptunia victory II", "Yakuza: Ishin", "The Legend of Heroes: Trails of Cold Steel IV",
                  "Little Dragon Cafe", "Pro Yakyuu Famista Evolution", "Persona Dancing: All-Star Triple Pack",
                  "The Snack World: Trejarers Gold", "Attack on Titan (KOEI)")

#Make a copy of the main data frame as a backup
vgMetaFull_df_clean <- copy(vgMetaFull_df)

#Eliminate games not on metacritic
vgMetaFull_df_clean <- vgMetaFull_df_clean[!(vgMetaFull_df_clean$title %in% gamesNotFound),]

#Fix popular games with faulty urls
for (i in c(1:nrow(vgMetaFull_df_clean))) {
    if (vgMetaFull_df_clean[i, ]$title == "Call of Duty: Black Ops 3" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "call-of-duty-black-ops-iii"
        vgMetaFull_df_clean[i,]$numPlayers <- "Up to 18"
        vgMetaFull_df_clean[i,]$esrb <- "M"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Action, Shooter, First-Person, Acrcade"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Treyarch"
        vgMetaFull_df_clean[i,]$release_date <- "Nov 6, 2015"
        vgMetaFull_df_clean[i,]$metascore <- "81"
        vgMetaFull_df_clean[i,]$user_score <- "4.8"
        vgMetaFull_df_clean[i,]$summary <- "In Black Ops 3, Treyarch introduces a new momentum-based chained-movement system which enables players to fluidly move through the environment with finesse, using controlled thrust jumps, slides, wall runs and mantling abilities in a myriad of combinations, all while continuously keeping full control over their weapon. Maps are designed from the ground-up for the new movement system, enabling players to be successful with traditional movement, as well as with advanced tactics and maneuvers."
    }
    else if (vgMetaFull_df_clean[i, ]$title == "Destiny 2" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "destiny-2"
        vgMetaFull_df_clean[i,]$numPlayers <- "Up to 12"
        vgMetaFull_df_clean[i,]$esrb <- "T"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Action, Shooter, First-Person, Acrcade"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Bungie"
        vgMetaFull_df_clean[i,]$release_date <- "Sep 6, 2017"
        vgMetaFull_df_clean[i,]$metascore <- "85"
        vgMetaFull_df_clean[i,]$user_score <- "4.9"
        vgMetaFull_df_clean[i,]$summary <- "In Destiny 2, the last safe city on Earth has fallen and lays in ruins, occupied by a powerful new enemy and his elite army, the Red Legion. Every player creates his own character called a “Guardian,” humanity’s chosen protectors. As a Guardian in Destiny 2, players must master new abilities and weapons to reunite the city’s forces, stand together and fight back to reclaim their home. In Destiny 2 players embark on a fresh story filled with new destinations around our solar system to explore, and an expansive amount of activities to discover. There is something for almost every type of gamer in Destiny 2, including gameplay for solo, cooperative and competitive players set within a vast, evolving and exciting universe. [Bungie]"
    }
    else if (vgMetaFull_df_clean[i, ]$title == "Doom (2016)" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "doom"
        vgMetaFull_df_clean[i,]$numPlayers <- "Up to 12"
        vgMetaFull_df_clean[i,]$esrb <- "M"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Sci-Fi, Third-Person, Action, Shooter, First-Person, Acrcade"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: id Software"
        vgMetaFull_df_clean[i,]$release_date <- "May 13, 2016"
        vgMetaFull_df_clean[i,]$metascore <- "85"
        vgMetaFull_df_clean[i,]$user_score <- "8.3"
        vgMetaFull_df_clean[i,]$summary <- "There is no taking cover or stopping to regenerate health in campaign mode as you beat back Hell’s raging demon hordes. Combine your arsenal of futuristic and iconic guns, upgrades, movement and an advanced melee system to knock-down, slash, stomp, crush, and blow apart demons in creative and violent ways. In multiplayer, dominate your opponents in DOOM’s signature, fast-paced arena-style combat. In both classic and all-new game modes, annihilate your enemies utilizing your personal blend of skill, powerful weapons, vertical movement, and unique power-ups that allow you to play as a demon. DOOM SnapMap is an easy-to-use game and level editor that allows for limitless gameplay experiences on every platform. Anyone can snap together and customize maps, add pre-defined or custom gameplay, and edit game logic to create new modes. Instantly play your creation or make it available to players around the world. [ID/Bethesda]"
    }
    else if (vgMetaFull_df_clean[i, ]$title == "Dragon Ball Fighter Z" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "dragon-ball-fighterz"
        vgMetaFull_df_clean[i,]$numPlayers <- "2"
        vgMetaFull_df_clean[i,]$esrb <- "T"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Action, Fighting, 2D"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Arc System Works"
        vgMetaFull_df_clean[i,]$release_date <- "May 13, 2016"
        vgMetaFull_df_clean[i,]$metascore <- "85"
        vgMetaFull_df_clean[i,]$user_score <- "8.3"
        vgMetaFull_df_clean[i,]$summary <- "After the success of the Xenoverse series, its time to introduce a new classic 2D DRAGON BALL fighting game for this generations consoles. DRAGON BALL FighterZ is born from what makes the DRAGON BALL series so loved and famous: endless spectacular fights with its allpowerful fighters. Partnering with Arc System Works, DRAGON BALL FighterZ maximizes high end Anime graphics and brings easy to learn but difficult to master fighting gameplay to audiences worldwide. Key Features: * 3vs3 TAG/SUPPORT Allows players to train and master more than one fighter/style which brings deeper gameplay * HIGH-END ANIME GRAPHICS Using the power of the Unreal engine and the talented team at Arc System Works, DRAGON BALL FighterZ is a visual tour-de-force * SPECTACULAR FIGHTS Experience aerial combos, destructible stages, famous scenes from the DRAGON BALL anime reproduced in 60FPS and 1080p resolution (Higher resolution will be supported on PS4 Pro and Xbox Scorpio)"

    }
    else if (vgMetaFull_df_clean[i, ]$title == "Dragon Quest XI" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "dragon-quest-xi-echoes-of-an-elusive-age"
        vgMetaFull_df_clean[i,]$numPlayers <- "No Online Multiplayer"
        vgMetaFull_df_clean[i,]$esrb <- "T"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Role-Playing, Japanese-Style"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Square Enix"
        vgMetaFull_df_clean[i,]$release_date <- "Sep 4, 2018"
        vgMetaFull_df_clean[i,]$metascore <- "86"
        vgMetaFull_df_clean[i,]$user_score <- "8.7"
        vgMetaFull_df_clean[i,]$summary <- "DRAGON QUEST XI: Echoes of an Elusive Age tells a captivating tale of a hunted hero and is the long-awaited role-playing game from series creator Yuji Horii, character designer Akira Toriyama and composer Koichi Sugiyama. While it is the eleventh mainline entry in the critically acclaimed series, DRAGON QUEST XI is a completely standalone experience that features entirely new characters, a beautifully detailed world, finely tuned turn-based combat, and an immersive story that promises to appeal to longtime fans and franchise newcomers alike."
    }
    else if (vgMetaFull_df_clean[i, ]$title == "F1 2016 (Codemasters)" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "f1-2016"
        vgMetaFull_df_clean[i,]$numPlayers <- "Up to 22"
        vgMetaFull_df_clean[i,]$esrb <- "E"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Racing, Simulation, Automobile"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Codemasters"
        vgMetaFull_df_clean[i,]$release_date <- "Aug 19, 2016"
        vgMetaFull_df_clean[i,]$metascore <- "82"
        vgMetaFull_df_clean[i,]$user_score <- "7.9"
        vgMetaFull_df_clean[i,]$summary <- "Includes the full 2016 season calendar of 21 tracks, including the brand new Baku circuit in Azerbaijan, and the full roster of 22 drivers and 11 teams, including the new Haas F1 Team. F1 2016 immerses you in not only the on-track excitement of a FORMULA ONE career, including the inclusion of the iconic Safety Car and also the Virtual Safety Car for the first time, but also uniquely offers the drama and vehicle development that goes on behind the scenes. Work with your agent, engineer and team to develop your car in the deepest ever career experience, spanning up to ten seasons. Forge your own path to glory, and rise to be the champion."
    }
    else if (vgMetaFull_df_clean[i, ]$title == "FIFA Soccer 14" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "fifa-14"
        vgMetaFull_df_clean[i,]$numPlayers <- "Up to 22"
        vgMetaFull_df_clean[i,]$esrb <- "E"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Sports, Traditional, Team, Soccer, Sim, Sim"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: EA Canada"
        vgMetaFull_df_clean[i,]$release_date <- "Nov 12, 2013"
        vgMetaFull_df_clean[i,]$metascore <- "87"
        vgMetaFull_df_clean[i,]$user_score <- "6.3"
        vgMetaFull_df_clean[i,]$summary <- "Fuelled by EA SPORTS IGNITE, FIFA 14 feels alive with players who think, move, and behave like world-class footballers, and dynamic stadiums that come to life. Players have four times the decision making ability and feel alive with human-like reactions, anticipation, and instincts. With 10 times more animation depth and detail than previous consoles, FIFA 14 delivers the dynamic movements and biomechanics of the worlds best footballers. Players are agile and athletic as they plant, pivot and cut, and explode out of each step. The increased fidelity has a game-changing effect on gameplay. Plus, the artistry and athleticism of footballers will come to life with the addition of hundreds of new types of skills and hundreds of new behaviors. And for the first time ever, fans will feel the electricity of a living stadium as the emotions of new 3D crowds rise and fall around the stories on the pitch."
    }
    else if (vgMetaFull_df_clean[i, ]$title == "Gears of War" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "gears-of-war-ultimate-edition"
        vgMetaFull_df_clean[i,]$numPlayers <- "Up to 8"
        vgMetaFull_df_clean[i,]$esrb <- "M"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Action, Shooter, Third-Person, Arcade"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Splash Damage, The Coalition"
        vgMetaFull_df_clean[i,]$release_date <- "Aug 25, 2015"
        vgMetaFull_df_clean[i,]$metascore <- "82"
        vgMetaFull_df_clean[i,]$user_score <- "7.6"
        vgMetaFull_df_clean[i,]$summary <- "The shooter has been remastered in 1080P and modernized for Xbox One, and is augmented with new content including five campaign chapters never released on Xbox."
    }
    else if (vgMetaFull_df_clean[i, ]$title == "Hitman (2016)" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "hitman"
        vgMetaFull_df_clean[i,]$numPlayers <- NA
        vgMetaFull_df_clean[i,]$esrb <- "M"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Modern, Action Adventure, General"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Io Interactive"
        vgMetaFull_df_clean[i,]$release_date <- "Oct 31, 2016"
        vgMetaFull_df_clean[i,]$metascore <- "85"
        vgMetaFull_df_clean[i,]$user_score <- "7.5"
        vgMetaFull_df_clean[i,]$summary <- "HITMAN empowers players to perform contract hits on powerful, high-profile targets in exotic locations around the globe, from Paris and sunny coast of Italy to the dust and hustle of the markets in Marrakesh in this stealth action title. Gameplay centers on taking out targets in vast sandbox levels with complete freedom of approach. Where to go, when to attack and who to kill is at your discretion."
    }
    else if (vgMetaFull_df_clean[i, ]$title == "Hyrule Warriors" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "hyrule-warriors-definitive-edition"
        vgMetaFull_df_clean[i,]$numPlayers <- "No Online Multiplayer"
        vgMetaFull_df_clean[i,]$esrb <- "T"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Action, Beat-'Em-Up, 3D"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Koei Tecmo Games"
        vgMetaFull_df_clean[i,]$release_date <- "May 18, 2018"
        vgMetaFull_df_clean[i,]$metascore <- "78"
        vgMetaFull_df_clean[i,]$user_score <- "8.1"
        vgMetaFull_df_clean[i,]$summary <- "A new, ultimate version of the exhilarating action game set in the Zelda universe will include every map and mission, plus all 29 playable characters from both the Wii U and Nintendo 3DS versions of the game, along with all of the previous paid downloadable content. Play as Link, Zelda, Midna, Skull Kid and dozens more in action-packed battles at home or on the go. Additionally, the game includes new outfits for Link and Zelda based on the Legend of Zelda: Breath of the Wild game."
    }
    else if (vgMetaFull_df_clean[i, ]$title == "Kingdom Hearts 1.5 + 2.5 Remix" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "kingdom-hearts-hd-i5-+-ii5-remix"
        vgMetaFull_df_clean[i,]$numPlayers <- "No Online Multiplayer"
        vgMetaFull_df_clean[i,]$esrb <- "E10+"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Miscellaneous, Compilation"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Square Enix"
        vgMetaFull_df_clean[i,]$release_date <- "Mar 28, 2017"
        vgMetaFull_df_clean[i,]$metascore <- "84"
        vgMetaFull_df_clean[i,]$user_score <- "9.0"
        vgMetaFull_df_clean[i,]$summary <- "Kingdom Hearts HD 1.5 + 2.5 Remix is an HD remastered collection of 6 Kingdom Hearts experiences available for the first time on PS4."
    }
    else if ((vgMetaFull_df_clean[i, ]$title == "MineCraft" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") && (vgMetaFull_df_clean[i, ]$console == "xbox-one" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found")) {
        vgMetaFull_df_clean[i,]$url_title <- "miencraft-xbox-one-edition"
        vgMetaFull_df_clean[i,]$numPlayers <- "Up to 8"
        vgMetaFull_df_clean[i,]$esrb <- "E10+"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Action Adventure, Adventure, 3D, Sandbox, First-Person, Fantasy, Fantasy"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: 4J Studios, Mojang AB"
        vgMetaFull_df_clean[i,]$release_date <- "Nov 18, 2014"
        vgMetaFull_df_clean[i,]$metascore <- "88"
        vgMetaFull_df_clean[i,]$user_score <- "6.7"
        vgMetaFull_df_clean[i,]$summary <- "Minecraft: Xbox One Edition is a game about placing blocks to build anything you can imagine and was revealed at E3 that it will be coming to the Xbox One."
    }
    else if (vgMetaFull_df_clean[i, ]$title == "Need for Speed (2015)" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "need-for-speed"
        vgMetaFull_df_clean[i,]$numPlayers <- NA
        vgMetaFull_df_clean[i,]$esrb <- "T"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Racing, Simulation, Automobile"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Ghost Games"
        vgMetaFull_df_clean[i,]$release_date <- "Nov 3, 2015"
        vgMetaFull_df_clean[i,]$metascore <- "65"
        vgMetaFull_df_clean[i,]$user_score <- "7.6"
        vgMetaFull_df_clean[i,]$summary <- "Jump behind the wheel of some iconic cars and put the pedal to the metal through Ventura Bay, a sprawling urban playground. Explore overlapping stories as you establish your reputation – and your dream car – and become the top racing icon. Play repeatedly this time, you have five distinct ways to win."
    }
    else if (vgMetaFull_df_clean[i, ]$title == "Nintendo Labo: Toy-Con 01 Variety Kit" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "nintendo-labo-toycon-01-variety-kit"
        vgMetaFull_df_clean[i,]$numPlayers <- "No Online Multiplayer"
        vgMetaFull_df_clean[i,]$esrb <- "E"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Miscellaneous, Party / Minigame"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Nintendo"
        vgMetaFull_df_clean[i,]$release_date <- "Apr 20, 2018"
        vgMetaFull_df_clean[i,]$metascore <- "77"
        vgMetaFull_df_clean[i,]$user_score <- "6.9"
        vgMetaFull_df_clean[i,]$summary <- "The Variety Kit includes five different projects to Make, Play, and Discover: two Toy-Con RC Cars, a Toy-Con Fishing Rod, a Toy-Con House, a Toy-Con Motorbike, and a Toy-Con Piano. Bring each Toy-Con creation to life with the power of the Nintendo Switch console and Joy-Con controllers."
    }
    else if (vgMetaFull_df_clean[i, ]$title == "Ni-Oh" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "nioh"
        vgMetaFull_df_clean[i,]$numPlayers <- "2"
        vgMetaFull_df_clean[i,]$esrb <- "M"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S):  Role-Playing, Action RPG"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Team Ninja"
        vgMetaFull_df_clean[i,]$release_date <- "Feb 7, 2017"
        vgMetaFull_df_clean[i,]$metascore <- "88"
        vgMetaFull_df_clean[i,]$user_score <- "8.5"
        vgMetaFull_df_clean[i,]$summary <- "Take up your sword and travel to Japans blood-bathed Sengoku period an era ravaged by warring states and dark, malevolent forces and cut a violent path through the land as the masterless samurai, William. Cross blades in brutal hand-to-hand combat, wielding swords, axes, spears and even war hammers against foes both human and demon. Endure the vicious encounters and learn from your mistakes: each death will bring you resurrection and each resurrection a greater resolve to overcome your foes."
    }
    else if (vgMetaFull_df_clean[i, ]$title == "Overwatch" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "overwatch"
        vgMetaFull_df_clean[i,]$numPlayers <- "Up to 12"
        vgMetaFull_df_clean[i,]$esrb <- "T"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Action, Shooter, First-Person, Tactical"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Blizzard Entertainment"
        vgMetaFull_df_clean[i,]$release_date <- "May 23, 2016"
        vgMetaFull_df_clean[i,]$metascore <- "90"
        vgMetaFull_df_clean[i,]$user_score <- "6.4"
        vgMetaFull_df_clean[i,]$summary <- "Overwatch is a highly stylized team-based shooter set on earth in the near future. Every match is an intense multiplayer showdown pitting a diverse cast of soldiers, mercenaries, scientists, adventurers, and oddities against each other in an epic, globe-spanning conflict."
    }
    else if (vgMetaFull_df_clean[i, ]$title == "Pokken Tournament" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "pokken-tournament-dx"
        vgMetaFull_df_clean[i,]$numPlayers <- "2"
        vgMetaFull_df_clean[i,]$esrb <- "E10+"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Action, Fighting, 3D"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Bandai Namco Games"
        vgMetaFull_df_clean[i,]$release_date <- "Sep 22, 2017"
        vgMetaFull_df_clean[i,]$metascore <- "79"
        vgMetaFull_df_clean[i,]$user_score <- "7.5"
        vgMetaFull_df_clean[i,]$summary <- "The popular Pokemon fighting game comes to Nintendo Switch with added Pokemon fighters and new ways to battle other players. Take direct control of one of over 20 prized Pokemon fighters to defeat other Pokemon in action-packed arena fights. With Nintendo Switch, you can battle at home or on the go to become the Ferrum League champion. Master the new fighting styles of Croagunk, Scizor, Empoleon, Darkrai, and the newly added Decidueye. New modes give you the ability to challenge your friends in intense competition. Duke it out with your favorite Pokemon Fighters in Team Battle. Then record and upload your favorite fights with the new replay feature. Take the battle on the road with the portable Nintendo Switch system or find competition online in the new Group Match Mode. Do you have what it takes to claim the title of Pokken Tournament DX champion? There's only one way to find out, and that's by stepping into the battle arena. * Fight as Decidueye, Darkrai , Scizor, Empoleon, Croagunk, and more * Stunning Pokemon battles come to life like never before * Execute powerful Pokemon moves with simple button combinations * Enjoy an intuitive fighting system for all skill levels"
    }
    else if (vgMetaFull_df_clean[i, ]$title == "Prey (2017)" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "prey"
        vgMetaFull_df_clean[i,]$numPlayers <- "No Online Multiplayer"
        vgMetaFull_df_clean[i,]$esrb <- "M"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): General, Action, Shooter, First-Person, Arcade"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Arkane Studios"
        vgMetaFull_df_clean[i,]$release_date <- "May 5, 2017"
        vgMetaFull_df_clean[i,]$metascore <- "79"
        vgMetaFull_df_clean[i,]$user_score <- "7.8"
        vgMetaFull_df_clean[i,]$summary <- "You awaken aboard Talos I, a space station orbiting the moon in the year 2032. You are the key subject in an experiment meant to change humanity forever – but things have gone terribly wrong. The space station has been overrun by hostile aliens and you are now being hunted. As you delve into the dark secrets of Talos I and your own past, you have to survive using the tools found on the station, your wits, weapons, and mind-bending abilities. [Bethesda Softworks]"
    }
    else if (vgMetaFull_df_clean[i, ]$title == "Rainbow Six: Siege" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "tom-clancys-rainbow-six-siege"
        vgMetaFull_df_clean[i,]$numPlayers <- "Up to 10"
        vgMetaFull_df_clean[i,]$esrb <- "M"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Modern, Action, Shooter, First-Person, Tactical"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Ubisoft Montreal"
        vgMetaFull_df_clean[i,]$release_date <- "Dec 1, 2015"
        vgMetaFull_df_clean[i,]$metascore <- "73"
        vgMetaFull_df_clean[i,]$user_score <- "7.4"
        vgMetaFull_df_clean[i,]$summary <- "Tom Clancy’s Rainbow Six Siege is inspired by real world counter-terrorist organizations, and inserts players in the middle of lethal close-quarters engagements. For the first time in a Tom Clancy’s Rainbow Six game, players can choose from a variety of unique Counter Terrorist Operators and engage in tangible sieges, a new style of assault in which enemies have the means to transform their environments into modern strongholds while Rainbow Six teams lead the assault to breach the enemy’s position."
    }
    else if ((vgMetaFull_df_clean[i, ]$title == "Ratchet & Clank" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") || (vgMetaFull_df_clean[i, ]$title == "Ratchet & Clank (2016)" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found")) {
        vgMetaFull_df_clean[i,]$url_title <- "ratchet-clank"
        vgMetaFull_df_clean[i,]$numPlayers <- "No Online Multiplayer"
        vgMetaFull_df_clean[i,]$esrb <- "E10+"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S):  Action, Platformer, 3D"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Insomniac Games"
        vgMetaFull_df_clean[i,]$release_date <- "Apr 12, 2016"
        vgMetaFull_df_clean[i,]$metascore <- "85"
        vgMetaFull_df_clean[i,]$user_score <- "8.6"
        vgMetaFull_df_clean[i,]$summary <- "Play the game, based on the movie, based on the game. Ratchet & Clank (PS4) is a new game based on elements from the original Ratchet & Clank (PS2). Developed alongside the major motion CG-animated picture coming to theatres in 2016, Ratchet & Clank (PS4) marks the PlayStation 4 debut of PlayStation's greatest heroes. Join Ratchet, Clank, Captain Qwark and new friends as they battle to save the Solana Galaxy from the evil Chairman Drek. With an hour of new cinematics (including footage from the film), Ratchet & Clank (PS4) takes a deeper look at the characters' origin stories and modernizes the original gameplay. Explore the galaxy in a game that features new planets, new and updated gameplay segments, all-new bosses, all-new Clank gameplay, all-new flight sequences, and much more --- with completely new visuals built to demonstrate the power of the PS4. Battle your enemies with an out-of-this-world arsenal, including new weapons and fan-favorite tools of destruction from the Ratchet & Clank Future series. From the brand new Pixelizer (which transforms enemies into explosive 8-bit pixels) to franchise favorites like the dance-party-inducing Groovitron and wise-cracking robotic bodyguard Mr. Zurkon, Ratchet & Clank's arsenal has never been better. Help Ratchet and Clank save the galaxy for the first time, again, exclusively on PlayStation 4."
    }
    else if (vgMetaFull_df_clean[i, ]$title == "Resident Evil VII: Biohazard" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "resident-evil-7-biohazard"
        vgMetaFull_df_clean[i,]$numPlayers <- "No Online Multiplayer"
        vgMetaFull_df_clean[i,]$esrb <- "M"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Action Adventure, Survival"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Capcom"
        vgMetaFull_df_clean[i,]$release_date <- "Aug 25, 2015"
        vgMetaFull_df_clean[i,]$metascore <- "86"
        vgMetaFull_df_clean[i,]$user_score <- "7.9"
        vgMetaFull_df_clean[i,]$summary <- "While Resident Evil 7 draws from the series’ roots of atmospheric survival horror, it also delivers a new level of terror. In the Resident Evil games of yesteryear, players braced for fear in the first-person via the creepy door-opening scenes, and Resident Evil 7 ramps up that tension with an immersive first-person view and a photorealistic graphical style. Capcom is able to achieve a higher degree of visual fidelity thanks to the new proprietary in-house RE Engine that includes VR oriented tools. With the RE Engine plus industry leading audio and visual technologies, you experience every abhorrent detail up close and personal in Resident Evil 7. Playing the game in the PlayStation VR Mode escalates the unsettling feeling of presence to a level that horror fans have never experienced. The full gameplay experience is available in the included PlayStation VR Mode from beginning to end. No Resident Evil you’ve ever survived could prepare you for this. [Capcom]"
    }
    else if (vgMetaFull_df_clean[i, ]$title == "Rise of the Tomb Raider" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "rise-of-the-tomb-raider-20-year-celebration"
        vgMetaFull_df_clean[i,]$numPlayers <- "2"
        vgMetaFull_df_clean[i,]$esrb <- "M"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Action Adventure, Linear"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Crystal Dynamics, Nixxes Software"
        vgMetaFull_df_clean[i,]$release_date <- "OCt 11, 2016"
        vgMetaFull_df_clean[i,]$metascore <- "88"
        vgMetaFull_df_clean[i,]$user_score <- "8.1"
        vgMetaFull_df_clean[i,]$summary <- "Lara Croft embarks on her first Tomb Raiding expedition to the most treacherous and remote regions of Siberia. Rise of the Tomb Raider: 20 Year Celebration also includes the new “Blood Ties” single player content, VR support, new Co-op play for Endurance mode, and more."
    }
    else if (vgMetaFull_df_clean[i, ]$title == "Star Wars Battlefront (2015)" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "star-wars-battlefront"
        vgMetaFull_df_clean[i,]$numPlayers <- "Up to 40"
        vgMetaFull_df_clean[i,]$esrb <- "T"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): General, General, Action, Shooter, First-Person, Tactical"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: EA DICE"
        vgMetaFull_df_clean[i,]$release_date <- "Nov 17, 2015"
        vgMetaFull_df_clean[i,]$metascore <- "73"
        vgMetaFull_df_clean[i,]$user_score <- "5.0"
        vgMetaFull_df_clean[i,]$summary <- "Star Wars Battlefront lets players live out a broad array of heroic moments and intense battle fantasies of their own – firing blasters, riding speeder bikes and snow speeders, commanding AT-ATs and piloting TIE fighters and the Millennium Falcon. These battles will take place on the iconic planets of the Star Wars universe, including Endor, Hoth, Tatooine and the previously unexplored planet, Sullust. Gamers can play as the memorable characters from the original trilogy such as Darth Vader and Boba Fett. Star Wars Battlefront features a wide range of modes tailored for different types of battles, from larger 40-person competitive multiplayer to crafted missions that are played solo, with a partner via split-screen offline or co-operatively online. [EA/Lucasfilm]"
    }
    else if (vgMetaFull_df_clean[i, ]$title == "Star Wars Battlefront II (2017)" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "star-wars-battlefront-ii"
        vgMetaFull_df_clean[i,]$numPlayers <- "Up to 40"
        vgMetaFull_df_clean[i,]$esrb <- "T"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Action, Shooter, First-Person, Tactical"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: EA DICE"
        vgMetaFull_df_clean[i,]$release_date <- "Nov 14, 2017"
        vgMetaFull_df_clean[i,]$metascore <- "82"
        vgMetaFull_df_clean[i,]$user_score <- "7.6"
        vgMetaFull_df_clean[i,]$summary <- " Star Wars Battlefront II allows players to experience the untold story of an Imperial elite special forces soldier in an all-new single player campaign. In epic multiplayer battles, players pilot a First Order TIE fighter through intense dogfights in space, and play as ground troopers or iconic heroes and villains, such as Yoda and Darth Maul, across all three Star Wars eras. Featuring new characters created by Motive in close partnership with the story group at Lucasfilm, the Star Wars Battlefront II single player campaign delivers a new story to the Star Wars universe. Taking place in the 30 years between the destruction of Death Star II and through the rise of the First Order, the single player story introduces Iden Versio, the leader of Inferno Squad, an elite special forces unit of the Galactic Empire, who are equally lethal on the ground and in space. In addition to Iden, players also encounter and play as other iconic Star Wars heroes and villains like Luke Skywalker and Kylo Ren during the campaign."
    }
    else if (vgMetaFull_df_clean[i, ]$title == "Star Wars: Battlefront (2015)" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "star-wars-battlefront"
        vgMetaFull_df_clean[i,]$numPlayers <- "Up to 40"
        vgMetaFull_df_clean[i,]$esrb <- "T"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): General, General, Action, Shooter, First-Person, Tactical"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: EA DICE"
        vgMetaFull_df_clean[i,]$release_date <- "Nov 17, 2015"
        vgMetaFull_df_clean[i,]$metascore <- "73"
        vgMetaFull_df_clean[i,]$user_score <- "5.0"
        vgMetaFull_df_clean[i,]$summary <- "Star Wars Battlefront lets players live out a broad array of heroic moments and intense battle fantasies of their own – firing blasters, riding speeder bikes and snow speeders, commanding AT-ATs and piloting TIE fighters and the Millennium Falcon. These battles will take place on the iconic planets of the Star Wars universe, including Endor, Hoth, Tatooine and the previously unexplored planet, Sullust. Gamers can play as the memorable characters from the original trilogy such as Darth Vader and Boba Fett. Star Wars Battlefront features a wide range of modes tailored for different types of battles, from larger 40-person competitive multiplayer to crafted missions that are played solo, with a partner via split-screen offline or co-operatively online. [EA/Lucasfilm]"
    }
    else if (vgMetaFull_df_clean[i, ]$title == "The Elder Scrolls V: Skyrim" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "the-elder-scrolls-v-skyrim-special-edition"
        vgMetaFull_df_clean[i,]$numPlayers <- "No Online Multiplayer"
        vgMetaFull_df_clean[i,]$esrb <- "M"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Role-Playing, Western-Style"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Bethesda Game Studios"
        vgMetaFull_df_clean[i,]$release_date <- "Oct 28, 2016"
        vgMetaFull_df_clean[i,]$metascore <- "81"
        vgMetaFull_df_clean[i,]$user_score <- "7.1"
        vgMetaFull_df_clean[i,]$summary <- "Skyrim Special Edition brings the epic fantasy to life in detail. The Special Edition includes the game and add-ons with all-new features like remastered art and effects, volumetric god rays, dynamic depth of field, screen-space reflections, and more."
    }
    else if (vgMetaFull_df_clean[i, ]$title == "The Evil Within II" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "the-evil-within-2"
        vgMetaFull_df_clean[i,]$numPlayers <- "No Online Multiplayer"
        vgMetaFull_df_clean[i,]$esrb <- "M"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Action Adventure, Survival"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Tango Gameworks"
        vgMetaFull_df_clean[i,]$release_date <- "Oct 13, 2017"
        vgMetaFull_df_clean[i,]$metascore <- "76"
        vgMetaFull_df_clean[i,]$user_score <- "8.5"
        vgMetaFull_df_clean[i,]$summary <- "You are Detective Sebastian Castellanos and at your lowest point. But when given a chance to save your daughter, you must enter a world filled with nightmares and discover the dark origins of a once-idyllic town to bring her back. Horrifying threats emerge from every corner as the world twists and warps around you. Will you face adversity head on with weapons and traps, or sneak through the shadows to survive?"
    }
    else if (vgMetaFull_df_clean[i, ]$title == "The Last of Us" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "the-last-of-us-remastered"
        vgMetaFull_df_clean[i,]$numPlayers <- "Up to 8"
        vgMetaFull_df_clean[i,]$esrb <- "M"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): Action Adventure, General, Modern"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Naughty Dog"
        vgMetaFull_df_clean[i,]$release_date <- "Jul 29, 2014"
        vgMetaFull_df_clean[i,]$metascore <- "95"
        vgMetaFull_df_clean[i,]$user_score <- "9.1"
        vgMetaFull_df_clean[i,]$summary <- "The Last of Us has been rebuilt for the PlayStation4 system. Now featuring full 1080p, higher resolution character models, improved shadows and lighting, in addition to several other gameplay improvements. 20 years after a pandemic has radically changed known civilization, infected humans run wild and survivors are killing each other for food, weapons; whatever they can get their hands on. Joel, a violent survivor, is hired to smuggle a 14 year-old girl, Ellie, out of an oppressive military quarantine zone, but what starts as a small job soon transforms into a brutal journey across the U.S. The Last of Us Remastered includes the Abandoned Territories Map Pack, Reclaimed Territories Map Pack, and the critically acclaimed The Last of Us: Left Behind Single Player campaign that combines themes of survival, loyalty, and love with tense, survival-action gameplay. Remastered Features: -Explore a brutal post-pandemic world, fully realized with the power of PlayStation4 system -Includes additional game content: over $30 in value -Delve into Ellies past in Left Behind, the single-player prequel chapter -Eight new multiplayer maps in the Abandoned and Reclaimed Territories packs -In-game cinematic commentary from the cast and creative director"
    }
    else if (vgMetaFull_df_clean[i, ]$title == "The Order 1866" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "the-order-1866"
        vgMetaFull_df_clean[i,]$numPlayers <- "No Online Multiplayer"
        vgMetaFull_df_clean[i,]$esrb <- "M"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): General, General, Action, Action Adventure, Shooter, Linear, Third-Person, Sci-Fi, Arcade"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Ready at Dawn, SCE Santa Monica"
        vgMetaFull_df_clean[i,]$release_date <- "Feb 20, 2015"
        vgMetaFull_df_clean[i,]$metascore <- "63"
        vgMetaFull_df_clean[i,]$user_score <- "6.7"
        vgMetaFull_df_clean[i,]$summary <- "The Order: 1886 introduces us to a unique vision of Victorian-Era London in which Man uses advanced technology to battle a powerful and ancient enemy. As Galahad, a member of an elite order of Knights, join a centuries-old war against a powerful threat that will determine the course of history in this third-person action-adventure shooter."
    }
    else if (vgMetaFull_df_clean[i, ]$title == "Uncharted (PS4)" && vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$url_title <- "uncharted-4-a-thiefs-end"
        vgMetaFull_df_clean[i,]$numPlayers <- "Up to 10"
        vgMetaFull_df_clean[i,]$esrb <- "T"
        vgMetaFull_df_clean[i,]$metaGenre <- "Genre(S): General, Modern, Action Adventure, Linear"
        vgMetaFull_df_clean[i,]$metaDevs <- "Developer: Naughty Dog"
        vgMetaFull_df_clean[i,]$release_date <- "May 10, 2016"
        vgMetaFull_df_clean[i,]$metascore <- "93"
        vgMetaFull_df_clean[i,]$user_score <- "8.4"
        vgMetaFull_df_clean[i,]$summary <- " Set 3 years after the events of Uncharted 3, Nathan Drake has apparently left the world of fortune hunting behind. However, it doesn’t take long for adventure to come calling when Drake’s brother, Sam, re-emerges asking for his help to save his own life and offering an adventure Drake cannot resist. On the hunt for Captain Henry Avery’s long-lost treasure, Sam and Drake embark on a journey to find Libertalia, the pirate utopia deep in the forests of Madagascar. Uncharted 4: A Thief’s End takes players around the globe, through jungle isles, urban cities and snow-capped peaks on the search for Avery’s fortune. [Naughty Dog]"
    }
}

#vgchartz provided the publisher rather than developer
colnames(vgMetaFull_df_clean)[which(names(vgMetaFull_df_clean) == "developer")] <- "publisher"
colnames(vgMetaFull_df_clean)[which(names(vgMetaFull_df_clean) == "metaDevs")] <- "developer"

#Set data types
vgMetaFull_df_clean %>% mutate_if(is.factor, as.character) -> vgMetaFull_df_clean
vgMetaFull_df_clean$week_posted <- sapply(vgMetaFull_df_clean$week_posted, gsub, pattern = "th", replacement = "")
vgMetaFull_df_clean$week_posted <- sapply(vgMetaFull_df_clean$week_posted, gsub, pattern = "nd", replacement = "")
vgMetaFull_df_clean$week_posted <- sapply(vgMetaFull_df_clean$week_posted, gsub, pattern = "st", replacement = "")
vgMetaFull_df_clean$week_posted <- sapply(vgMetaFull_df_clean$week_posted, gsub, pattern = "rd", replacement = "")
vgMetaFull_df_clean$week_posted <- as.Date(vgMetaFull_df_clean$week_posted, "%d %b %Y")
vgMetaFull_df_clean$console <- as.factor(vgMetaFull_df_clean$console)
vgMetaFull_df_clean$publisher <- as.factor(vgMetaFull_df_clean$publisher)
vgMetaFull_df_clean$genre <- as.factor(vgMetaFull_df_clean$genre)
vgMetaFull_df_clean$units_total <- as.numeric(gsub(",", "", vgMetaFull_df_clean$units_total))
vgMetaFull_df_clean$units_week <- as.numeric(gsub(",", "", vgMetaFull_df_clean$units_week))
vgMetaFull_df_clean$numPlayers <- as.factor(vgMetaFull_df_clean$numPlayers)
vgMetaFull_df_clean$esrb <- as.factor(vgMetaFull_df_clean$esrb)
vgMetaFull_df_clean$metaGenre <- as.factor(vgMetaFull_df_clean$metaGenre)
vgMetaFull_df_clean$developer <- as.factor(vgMetaFull_df_clean$developer)
vgMetaFull_df_clean$release_date <- sapply(vgMetaFull_df_clean$release_date, gsub, pattern = ",", replacement = "")
vgMetaFull_df_clean$release_date <- as.Date(vgMetaFull_df_clean$release_date, "%b %d %Y")
vgMetaFull_df_clean$metascore <- as.numeric(vgMetaFull_df_clean$metascore)
vgMetaFull_df_clean$user_score <- as.numeric(vgMetaFull_df_clean$user_score)


#Add two columns. One for the user score multiplied by 10, and another for the difference between the two columns
vgMetaFull_df_clean$user_score_x10 <- vgMetaFull_df_clean$user_score * 10
vgMetaFull_df_clean$score_diff <- vgMetaFull_df_clean$metascore - vgMetaFull_df_clean$user_score_x10

#Convert errors to NAs for functionality
revalue(vgMetaFull_df_clean$esrb, c("ERROR" = NA)) -> vgMetaFull_df_clean$esrb
revalue(vgMetaFull_df_clean$numPlayers, c("ERROR" = NA)) -> vgMetaFull_df_clean$numPlayers

for (i in c(1:nrow(vgMetaFull_df_clean))) {
    if (vgMetaFull_df_clean[i, ]$metaGenre == "") {
        vgMetaFull_df_clean[i,]$numPlayers = NA
    }
    if (vgMetaFull_df_clean[i, ]$developer == "") {
        vgMetaFull_df_clean[i,]$metaDevs = NA
    }
    if (vgMetaFull_df_clean[i, ]$summary == "No Summary Found") {
        vgMetaFull_df_clean[i,]$summary = NA
    }
}

#Clean metaGenre; remove "Genre(s): " 
vgMetaFull_df_clean$metaGenre <- sapply(vgMetaFull_df_clean$metaGenre, substr, start = 11, stop = 1000000L)

#Clean metaDev; remove "Developer: "
vgMetaFull_df_clean$developer <- sapply(vgMetaFull_df_clean$developer, substr, start = 12, stop = 1000000L)

#Reorder columns
vgMetaFull_df_clean <- vgMetaFull_df_clean %>% select(2:18, 20, 21, 19)

write.csv(vgMetaFull_df_clean, "vgMetaFullCleanFinal.csv")

remDr$close()
rD$server$stop()