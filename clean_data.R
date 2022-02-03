library(stringr)

#convert into the right data type
PC2$ID <- as.numeric(PC2$ID)
PC2$TotalScoreGuess <- as.numeric(PC2$TotalScoreGuess)
PC2$TimeTaken <- as.numeric(PC2$TimeTaken)


#remove all data from before we started collecting
PC2 <- PC2[PC2$ID>=20211210000000,] 
PC2 <- PC2[PC2$TimeTaken<25 & PC2$TimeTaken > 0.5,]
PC2 <- PC2[PC2$ID != PC2[is.na(PC2$TotalScoreGuess),1],]

#reformat ID back
PC2$ID <- format(PC2$ID,scientific = F)

#calculate the grey squares
PC2 <- cbind(PC2,'GreySquares' = str_count(PC2$board.ID,"1")) #add column 

#Calculate Correlations
CORS <- c()
for(P in unique(PC2$ID)){
  COR <- (cor(PC2[PC2$ID == P,2],PC2[PC2$ID == P,5]))^2
  CORS <- c(CORS,COR)
}

PCOR <- cbind(unique(PC2$ID),CORS)
PCOR <- PCOR[!is.na(CORS),]
PCOR <- as.data.frame(PCOR)
names(PCOR) <- c("ID","R2")

rm(COR,CORS,P)

exploration <- exploration[-which(exploration[,5] == "CTRL" | exploration[,5] == "YOK"), ]
exploration <- exploration[exploration$ID >= 20211210000000,]
exploration <- exploration[exploration$Duration.of.main >= 0.5,]

exploreINDX <- list(a = seq(from = 4, to = 52, by = 2), b = seq(from = 54, to = 102, by = 2), 
                    c = seq(from = 104, to = 152, by = 2), d = seq(from = 154, to = 202, by = 2))

EXPLR <- as.data.frame(cbind(exploration$ID,exploration$Reward.Frequency,
                             exploration$Control.Condition,
                             unname(apply(exploration[,exploreINDX$a],1,sum))/25,
                             unname(apply(exploration[,exploreINDX$b],1,sum))/25,
                             unname(apply(exploration[,exploreINDX$c],1,sum))/25,
                             unname(apply(exploration[,exploreINDX$d],1,sum))/25))

names(EXPLR) <- c("ID","Reward_Freq","CTRL","Block1","Block2","Block3","Block4")
EXPLR$Block1 <- as.numeric(EXPLR$Block1) ; EXPLR$Block2 <- as.numeric(EXPLR$Block2) ; EXPLR$Block3 <- as.numeric(EXPLR$Block3)
EXPLR$Block4 <- as.numeric(EXPLR$Block4) ; EXPLR$Reward_Freq <- as.factor(EXPLR$Reward_Freq) ; EXPLR$CTRL <- as.factor(EXPLR$CTRL)

rm(exploreINDX)

PC1 <- PC1[PC1$ID >= 20211210000000,]
PC1$ID <- as.character(format(PC1$ID,scientific = F))
PC1 <- cbind(PC1,'reward prob' = unname(apply(PC1[,3:4],1,sum)))
PC1$`reward prob` <- PC1$`reward prob`/100

names(PC1) <- c("ID","0guess","10guess","11guess","1guess","board.ID","Xkey","TimeTaken","BlockNummer","reward prob")

PC1var <- data.frame()
for(i in unique(PC1$ID)){
  for(b in 1:4){
    var <- var(unlist(PC1[PC1$ID==i & PC1$BlockNummer==b,10]))
    PC1var <- rbind(PC1var,cbind(i,b,var))
  }
}

names(PC1var) <- c("ID","BlockNumber","RewardVariance")


for(j in 1:ncol(directPC)) {
  for(i in 1:nrow(directPC)) {
    if(directPC[i, j] == "Strongly Disagree") {
      directPC[i, j] <- 1
    } else if (directPC[i, j] == "Strongly Agree") {
      directPC[i, j] <- 10
    }
  }
}


# covert to numeric
for(i in 1:4) {
  directPC[, 1+i] <- as.numeric(directPC[, 1+i])
}

directPC$ID <- as.character(format(directPC$ID,scientific = FALSE))

rm(b,i,j,var)

demographics[demographics$Gender==1,3] <- "female"
demographics[demographics$Gender==2,3] <- "male"
demographics[demographics$Gender==3,3] <- "other"
demographics[demographics$Occupation==1,4] <- "student"
demographics[demographics$Occupation==2,4] <- "other"
demographics$ID <- as.character(format(demographics$ID,scientific = F))

library(tidyr)
PC1var <- pivot_wider(PC1var,id_cols = ID,names_from = BlockNumber,values_from = RewardVariance)
names(PC1var) <- c("ID","VarBlock1","VarBlock2","VarBlock3","VarBlock4")
PC1var <- as.data.frame(PC1var)

