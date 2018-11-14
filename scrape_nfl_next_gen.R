library(rvest)
library(rpart)

url <- "C:/Users/agadgil/Documents/FF/Receiving Stats _ NFL Next Gen Stats.html"
webpage <- read_html(url)
tbls <- html_nodes(webpage,"table")
receivers <- html_table(tbls[[2]])
colnames(receivers) <- colnames(html_table(tbls[[3]]))

receivers$TAY <- receivers$`TAYAverage Targeted Air Yards (TAY)` * receivers$`TARTargets (TAR)`
receivers$EXYARDS <- (receivers$`TAYAverage Targeted Air Yards (TAY)` + receivers$`xYAC/RExpected YAC/Reception (EYAC/R)`) * receivers$`TARTargets (TAR)`*receivers$`CTCH%Catch Percentage (CTCH%)`/100
receivers$EXtoRECRatio <- receivers$`YDSReceiving Yards (YDS)`/receivers$EXYARDS
View(cor(receivers[,4:17]))

receivers2 <- subset(receivers, POS == 'WR' & `TARTargets (TAR)`>=30 & `TAYAverage Targeted Air Yards (TAY)`>=8)
View(cor(receivers2[,4:17]))

fit <- lm(receivers$`YDSReceiving Yards (YDS)` ~ receivers$`TARTargets (TAR)`)
fit2 <- lm(receivers$`YDSReceiving Yards (YDS)` ~ receivers$TAY)

url <- "C:/Users/agadgil/Documents/FF/Rushing Stats _ NFL Next Gen Stats.html"
webpage <- read_html(url)
tbls <- html_nodes(webpage,"table")
rushers <- html_table(tbls[[2]])
colnames(rushers) <- colnames(html_table(tbls[[3]]))
rushers$`8+D%8+ Defenders in the Box (8+D%)` <- as.numeric(rushers$`8+D%8+ Defenders in the Box (8+D%)`)
View(cor(rushers[,3:9]))
