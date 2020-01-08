# Part 1
data <- read.csv("Schedule18.csv")

frame <- data
frame$Away.Neutral <- as.character(frame$Away.Neutral)
frame$Home.Neutral <- as.character(frame$Home.Neutral)
frame$PTSA <- as.numeric(as.character(frame[[6]]))
frame$PTSH <- as.numeric(as.character(frame[[10]]))
frame <- frame[!is.na(frame$PTSA),]

head(frame)

games <- data.frame(frame$Away.Neutral,frame$PTSA,frame$Home.Neutral,frame$PTSH)
games$frame.Away.Neutral <- as.character(games$frame.Away.Neutral)
games$frame.Home.Neutral <- as.character(games$frame.Home.Neutral)
games$pt_dif <- games$frame.PTSH - games$frame.PTSA
head(games)

team <- character(length = 2*length(games$pt_dif))
opponent <- character(length = 2*length(games$pt_dif))
location <- character(length = 2*length(games$pt_dif))
ptdif <- vector(mode='numeric',length = 2*length(games$pt_dif))

clean <- data.frame(team,opponent,location,ptdif)
clean$team <- as.character(clean$team)
clean$opponent <- as.character(clean$opponent)
clean$location <- as.character(clean$location)

for(i in 1:length(games$pt_dif)){
  
  clean$team[i] <- games$frame.Home.Neutral[i]
  clean$opponent[i] <- games$frame.Away.Neutral[i]
  clean$location[i] <- "H"
  clean$ptdif[i] <- games$pt_dif[i]
  
  
  clean$team[i + length(games$pt_dif)] <- games$frame.Away.Neutral[i]
  clean$opponent[i + length(games$pt_dif)] <- games$frame.Home.Neutral[i]
  clean$location[i + length(games$pt_dif)] <- "A"
  clean$ptdif[i + length(games$pt_dif)] <- (-1)*games$pt_dif[i]
  
  
}

#Correct for the 3 NFL games this season that were played at neutral locations
#Remember when the Chiefs & Rams game was supposed to be in Mexico?
clean$location[84] <- "N"
clean$location[95] <- "N"
clean$location[109] <- "N"
clean$location[84 + length(games$pt_dif)] <- "N"
clean$location[95 + length(games$pt_dif)] <- "N"
clean$location[109 + length(games$pt_dif)] <- "N"
head(clean)

boxplot(clean$ptdif[1:(.5*length(clean$ptdif))], col = "blue", horizontal = TRUE, 
        main = "Home Point Differentials for 2018 NFL Season", 
        xlab = "Home Score - Away Score")
abline( v = 0, col = "red")

lm.NFLfootball <- lm(ptdif ~ team + opponent + location, data = clean) 
#Let's take a look at some of the values
lm.NFLfootball$coefficients[1:7]

rankings <- data.frame("team" = sort(unique(clean$team)),
                       "bs_coeff" = rep(NA, 32))
scale_factor <- mean(lm.NFLfootball$coefficients[2:32])
rankings$bs_coeff <- c(0, lm.NFLfootball$coefficients[2:32]) - scale_factor
rankings <- rankings[(order(rankings$bs_coeff, decreasing = T)),]
rankings

stripchart(rankings$bs_coeff, pch = 19 , col = "blue", 
           xlab = "BS Coefficient", main = "BS Coefficients for 2018 Season")

clean$predscore <- predict(lm.NFLfootball, newdata = clean)
clean$win <- ifelse(clean$ptdif > 0, 1, 0)
glm.pointspread <- glm(win ~ predscore, data = clean, family = "binomial"
                       ,control = list(maxit = 50))
clean$winprob <- predict(glm.pointspread, newdata = clean, type = "response")

#We can plot the win probability vs. predicted score 
plot(clean$predscore, clean$winprob, xlab = "Home Predicted Score Differential", 
     ylab = "Home Win Probability", 
     main = "Logistic Function for Predicting Games with the G.O.F.F. NFL Model", 
     pch = 4, col = c("red","green")[(clean$ptdif > 0) + 1])
legend(5,.4,legend=c("Actual Home Win", "Actual Road Win"), fill=c("green","red"))


# Part 2
library(XML)
library(RCurl)

u <- "https://www.pro-football-reference.com/years/2018/games.htm"
newu <- getURL(u)
raw <- readHTMLTable(newu, as.is = T)

head(raw)

#remove unnecessary games
today <- raw$games
today <-today[!(today$Week=="Week"),]
index <- today[today$Week == "16",] #min(which(today$PtsA==""))
cur_date <- '2018-12-23' 

slate <- which(today$Date == "December 23" )
predictions <- data.frame(today$`Loser/tie`[slate],today$`Winner/tie`[slate])
colnames(predictions) <- c("Home", "Away")
predictions$Home <- as.character(predictions$Home)
predictions$Away <- as.character(predictions$Away)
locs <- rep("H", length(slate))

predictions

ptdif_call <- function(home,away,HN){
  
  arr <- c(0,0)
  
  r1 <- rankings$bs_coeff[which(rankings$team == home)]
  r2 <- rankings$bs_coeff[which(rankings$team == away)]
  
  
  if(HN == "H"){
    pt_dif <- r1 - r2 - coefficients(lm.NFLfootball)[[1]]
  }
  
  if(HN == "N"){
    pt_dif <- r1 - r2
  }
  
  arr[1] <- pt_dif
  prob <- 1 / (1+ exp(- coefficients(glm.pointspread)[[2]] * pt_dif))
  arr[2] <- prob
  
  return(arr)
}

predictions$pt_dif <- rep(0,length(slate))
predictions$home_prob <- rep(0,length(slate))

for(i in 1:length(slate)){
  predictions$pt_dif[i] <- ptdif_call(predictions$Home[i], predictions$Away[i], locs[i])[1]
  
  predictions$home_prob[i] <- ptdif_call(predictions$Home[i], predictions$Away[i], locs[i])[2]
}

#clean up the final data by rounding and sorting

predictions$pt_dif <- round(predictions$pt_dif, digits = 2)
predictions$home_prob <- round(predictions$home_prob, digits = 2)
predictions <- predictions[order(predictions$home_prob,decreasing = T),]



