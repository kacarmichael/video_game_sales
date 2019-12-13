setwd("C:\\Users\\arkad\\OneDrive\\Desktop\\Assignments\\MSIS 5600  DSP\\FINAL PROJECT")
int_data = read.csv("test.csv")
library("moments")
summary(int_data)
Position <- int_data$position
Unit_week <- int_data$units_week
Unit_total <- int_data$units_total  
Chart_week <- int_data$chart_week
Metascore <- int_data$metascore
User_score <- int_data$user_score
User_score_x_10 <- int_data$user_score_x10
Score_diff <- int_data$score_diff
summary(int_data$position)
summary(int_data$units_week)
summary(int_data$units_total)
summary(int_data$chart_week)
summary(int_data$metascore)
summary(int_data$user_score)
summary(int_data$user_score_x10)
summary(int_data$score_diff)


skewness(Position)
skewness(Unit_week)
skewness(Unit_total)
skewness(Chart_week)
skewness(Metascore, na.rm = TRUE)
skewness(User_score, na.rm = TRUE)
skewness(User_score_x_10, na.rm = TRUE)
skewness(Score_diff, na.rm = TRUE)

kurtosis(Position)
kurtosis(Unit_week)
kurtosis(Unit_total)
kurtosis(Chart_week)
kurtosis(Metascore, na.rm = TRUE)
kurtosis(User_score, na.rm = TRUE)
kurtosis(User_score_x_10, na.rm = TRUE)
kurtosis(Score_diff, na.rm = TRUE)

sd(Position)
sd(Unit_week)
sd(Unit_total)
sd(Chart_week)
sd(Metascore, na.rm = TRUE)
sd(User_score, na.rm = TRUE)
sd(User_score_x_10, na.rm = TRUE)
sd(Score_diff, na.rm = TRUE)

