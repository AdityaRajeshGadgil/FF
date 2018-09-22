library(rvest)
library(rpart)

url <- "C:/Users/agadgil/Documents/FF/Receiving Stats _ NFL Next Gen Stats.html"
webpage <- read_html(url)
tbls <- html_nodes(webpage,"table")
receivers <- html_table(tbls[[2]])
colnames(receivers) <- colnames(html_table(tbls[[3]]))

receivers$TAY <- receivers$`TAYAverage Targeted Air Yards (TAY)` * receivers$`TARTargets (TAR)`
View(cor(receivers[,4:13]))
fit <- lm(receivers$`YDSReceiving Yards (YDS)` ~ receivers$`TARTargets (TAR)`)
plot(receivers$`TARTargets (TAR)`, receivers$`YDSReceiving Yards (YDS)`)
abline(fit)

fit <- lm(receivers$`YDSReceiving Yards (YDS)` ~ receivers$`TARTargets (TAR)` + receivers$TAY)

receivers$pred_rec_yds <- 8.4676*receivers$`TARTargets (TAR)` + 
