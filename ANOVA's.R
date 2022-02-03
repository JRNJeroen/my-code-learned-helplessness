data <- read.csv("C:/Users/Jeroe/OneDrive/Bureaublad/Thesis/Analysis/LearnedHelplesnessDATA.csv")

library(rstatix)
library(tidyr)

# repeated measurement anova, DV: Exploration Rate, IV: Block, Reward, Control

#assumptions

normality <- data.frame()
for(b in 1:4){
  for(r in c(0.1,0.2,0.4,1)){
    for(ctrl in c("YOK","CTRL")){
      x <- data[data$Reward_Condition == r & data$Control_Condition == ctrl,6+b]
      #print(qqnorm(x))
      if(length(x)>2){
        s <- shapiro.test(x)
        normality <- rbind(normality,c(s$p.value,b,r,ctrl))
      }
    }
  }
}
names(normality) <- c("pvalue","Block","RewardValue","CtrlCondition")
normality$pvalue <- as.numeric(normality$pvalue)
sum(normality$pvalue < 0.05)

#test 
dataPIVOT <- pivot_longer(data,cols = Exploration_Rate_Block1:Exploration_Rate_Block4,names_to = 'Block',values_to = 'ExplorationRate')
nova <- anova_test(dataPIVOT,dv=ExplorationRate,wid = ID,within = Block,between = c(Reward_Condition,Control_Condition))
nova

# repeated measurement anova, DV: IRPV, IV: Block, Reward, Control

normality <- data.frame()
for(b in 1:4){
  for(r in c(0.1,0.2,0.4,1)){
    for(ctrl in c("YOK","CTRL")){
      x <- data[data$Reward_Condition == r & data$Control_Condition == ctrl,10+b]
      #print(qqnorm(x))
      if(length(x)>2){
        s <- shapiro.test(x)
        normality <- rbind(normality,c(s$p.value,b,r,ctrl))
      }
    }
  }
}
names(normality) <- c("pvalue","Block","RewardValue","CtrlCondition")
normality$pvalue <- as.numeric(normality$pvalue)
sum(normality$pvalue < 0.05)


dataPIVOT <- pivot_longer(data,cols = Reward_Variance_Block1:Reward_Variance_Block4,names_to = 'Block',values_to = 'IRPV')
nova <- anova_test(dataPIVOT,dv=IRPV,wid = ID,within = Block,between = c(Reward_Condition,Control_Condition))
nova

# repeated measurement anova, DV: RATING, IV: Block, Reward, Control

#assumptions
normality <- data.frame()
for(b in 1:4){
  for(r in c(0.1,0.2,0.4,1)){
    for(ctrl in c("YOK","CTRL")){
      x <- data[data$Reward_Condition == r & data$Control_Condition == ctrl,15+b]
      print(qqnorm(x))
      if(length(x)>2){
        s <- shapiro.test(x)
        normality <- rbind(normality,c(s$p.value,b,r,ctrl))
      }
    }
  }
}
names(normality) <- c("pvalue","Block","RewardValue","CtrlCondition")

#test
dataPIVOT <- pivot_longer(data,cols = Direct_1:Direct_4,names_to = 'Block',values_to = 'RATING')
nova <- anova_test(dataPIVOT,dv=RATING,wid = ID,within = Block,between = c(Reward_Condition,Control_Condition))
nova


#two way factorial ANOVA, DV: Squared Correlation, IV: Reward, Control

normality <- data.frame() ; check <- list()
for(r in c(0.1,0.2,0.4,1)){
  for(ctrl in c("YOK","CTRL")){
    x <- data[data$Reward_Condition == r & data$Control_Condition == ctrl,15]
    print(qqnorm(x))
    if(length(x)>2){
      s <- shapiro.test(x)
      normality <- rbind(normality,c(s$p.value,b,r,ctrl))
      check <- c(check,list(x))
    }
  }
}
bartlett.test(check)

normality$pvalue <- as.numeric(normality$pvalue)
sum(normality$pvalue < 0.05)

data$Reward_Condition <- as.factor(data$Reward_Condition)
nova <- aov(data = data,formula = Squared_Correlation ~ Reward_Condition + Control_Condition)
novaINT <- aov(data = data,formula = Squared_Correlation ~ Reward_Condition + Control_Condition + Reward_Condition * Control_Condition)
summary(nova)
get_anova_table(novaINT)
nova
TukeyHSD(nova)

