EXPLR <- EXPLR[which(EXPLR$ID %in% demographics$ID == T),] 
EXPLR <- EXPLR[which(EXPLR$ID %in% PCOR$ID == T),]
EXPLR <- EXPLR[which(EXPLR$ID %in% PC1var$ID == T),]
EXPLR <- EXPLR[which(EXPLR$ID %in% directPC$ID == T),]

PCOR <-  PCOR[which(PCOR$ID %in% demographics$ID == T),]
PCOR <- PCOR[which(PCOR$ID %in% PC1var$ID == T),]
PCOR <- PCOR[which(PCOR$ID %in% EXPLR$ID == T),]
PCOR <- PCOR[which(PCOR$ID %in% directPC$ID == T),]

PC1var <- PC1var[which(PC1var$ID %in% EXPLR$ID == T),]
PC1var <- PC1var[which(PC1var$ID %in% directPC$ID == T),]
PC1var <- PC1var[which(PC1var$ID %in% demographics$ID == T),]
PC1var <- PC1var[which(PC1var$ID %in% PCOR$ID == T),]

directPC <- directPC[which(directPC$ID %in% EXPLR$ID == T),] 
directPC <- directPC[which(directPC$ID %in% demographics$ID == T),] 
directPC <- directPC[which(directPC$ID %in% PCOR$ID == T),] 
directPC <- directPC[which(directPC$ID %in% PC1var$ID == T),]

demographics <- demographics[which(demographics$ID %in% EXPLR$ID == T),]
demographics <- demographics[which(demographics$ID %in% PCOR$ID == T),]
demographics <- demographics[which(demographics$ID %in% PC1var$ID == T),]
demographics <- demographics[which(demographics$ID %in% directPC$ID == T),]

demographics <- demographics[order(demographics$ID),]
EXPLR <- EXPLR[order(EXPLR$ID),]
PC1var <- PC1var[order(PC1var$ID),]
directPC <- directPC[order(directPC$ID),]
PCOR <- PCOR[order(directPC$ID),]

data <- cbind(demographics,EXPLR,PC1var,PCOR,directPC)
data <- data[,-c(5,12,17,19)]
names(data) <- c("ID","Age","Gender","Occupation","Reward_Condition","Control_Condition","Exploration_Rate_Block1","Exploration_Rate_Block2",
                 "Exploration_Rate_Block3","Exploration_Rate_Block4","Reward_Variance_Block1","Reward_Variance_Block2","Reward_Variance_Block3",
                 "Reward_Variance_Block4","Squared_Correlation","Direct_1","Direct_2","Direct_3","Direct_4")

data$Gender <- as.factor(data$Gender)
data$Occupation <- as.factor(data$Occupation)
data$Squared_Correlation <- as.numeric(data$Squared_Correlation)
data$Reward_Variance_Block1 <- as.numeric(data$Reward_Variance_Block1)
data$Reward_Variance_Block2 <- as.numeric(data$Reward_Variance_Block2)
data$Reward_Variance_Block3 <- as.numeric(data$Reward_Variance_Block3)
data$Reward_Variance_Block4 <- as.numeric(data$Reward_Variance_Block4)

write.csv(data,"C:/Users/Jeroe/OneDrive/Bureaublad/Thesis/Analysis/LearnedHelplesnessDATA.csv",row.names = F)

