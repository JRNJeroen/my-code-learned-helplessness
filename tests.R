data <- read.csv("C:/Users/Jeroe/OneDrive/Bureaublad/Thesis/Analysis/LearnedHelplesnessDATA.csv")

library(tidyr)
library(rstatix)

#tukey test DP: explore rates, IV: block, reward, control

#assumptions
normality <- data.frame() ; check <- list()
for(b in 1:4){
  for(r in c(0.1,0.2,0.4,1)){
    for(ctrl in c("YOK","CTRL")){
      x <- data[data$Reward_Condition == r & data$Control_Condition == ctrl,6+b]
        print(qqnorm(x))
      if(length(x)>2){
        s <- shapiro.test(x)
        normality <- rbind(normality,c(s$p.value,b,r,ctrl))
      }
      check <- c(check,list(x))
      
    }
  }
}
names(normality) <- c("pvalue","Block","RewardValue","CtrlCondition")

#conduct the test
pivot1 <- pivot_longer(data,cols = Exploration_Rate_Block1:Exploration_Rate_Block4,names_to = 'Block',values_to = 'ExplorationRates')
pivot1$Reward_Condition <- as.factor(pivot1$Reward_Condition)

TukeyHSD(aov(ExplorationRates~Block+Reward_Condition+Control_Condition,pivot1))





#tukey test DP: reward variance, IV: block, reward, control

#assumptions
normality <- data.frame() ; check <- list()
for(b in 1:4){
  for(r in c(0.1,0.2,0.4,1)){
    for(ctrl in c("YOK","CTRL")){
      x <- data[data$Reward_Condition == r & data$Control_Condition == ctrl,10+b]
      print(qqnorm(x))
      if(length(x)>2){
        s <- shapiro.test(x)
        normality <- rbind(normality,c(s$p.value,b,r,ctrl))
      }
      check <- c(check,list(x))
      
    }
  }
}
names(normality) <- c("pvalue","Block","RewardValue","CtrlCondition")
bartlett.test(check)

#conduct the test
pivot2 <- pivot_longer(data,cols = Reward_Variance_Block1:Reward_Variance_Block4,names_to = 'Block',values_to = 'IRPV')
pivot2$Reward_Condition <- as.factor(pivot2$Reward_Condition)

TukeyHSD(aov(IRPV~Block+Reward_Condition+Control_Condition,pivot2))

