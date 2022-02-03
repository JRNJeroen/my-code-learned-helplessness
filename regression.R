data <- read.csv("C:/Users/Jeroe/OneDrive/Bureaublad/Thesis/Analysis/LearnedHelplesnessDATA.csv")

library(car)

for(p in c(0.1,0.2,0.4,1)){

explo_rates <- apply(data[data$Reward_Condition==p,7:10],1,mean)
rating <- apply(data[data$Reward_Condition==p,16:19],1,mean)
IRPV <- apply(data[data$Reward_Condition==p,11:14],1,mean)

dataTOTAL <- cbind(data[data$Reward_Condition==p,-c(7:14,16:19)],explo_rates,rating,IRPV)

model <- lm(explo_rates ~ IRPV + Squared_Correlation + rating,data = dataTOTAL)
print(shapiro.test(dataTOTAL$rating - predict(model)))
print(bartlett.test(dataTOTAL[,c(7,9,10)]))
print(vif(model))
print(summary(model))

}

rating <- apply(data[,16:19],1,mean)
IRPV <- apply(data[,11:14],1,mean)
cors <- cor(cbind(rating,IRPV,'R2'=data$Squared_Correlation),method = 'spearman')

PCA <- princomp(cors)
summary(PCA)
loadings(PCA)

transform_t <- function(r){
  print(r*sqrt((88-2)/(1-r^2)))
}

transform_t(cors[1,2])
transform_t(cors[1,3])
transform_t(cors[3,2])
