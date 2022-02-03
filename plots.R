data <- read.csv("C:/Users/Jeroe/OneDrive/Bureaublad/Thesis/Analysis/LearnedHelplesnessDATA.csv")

library(ggplot2)
library(grid)
library(scales)
library(plotrix)

#exploration rate plots
text <- c('Low','Moderate','High','Very high'); i <- 0

for(p in c(0.1,0.2,0.4,1)){

i <- i + 1

yoked_means <- apply(data[data$Control_Condition=="YOK" & data$Reward_Condition == p, 7:10],2,mean)
yoked_errors <- apply(data[data$Control_Condition=="YOK" & data$Reward_Condition == p, 7:10],2,std.error)

ctrl_means <- apply(data[data$Control_Condition=="CTRL" & data$Reward_Condition == p, 7:10],2,mean)
ctrl_errors <- apply(data[data$Control_Condition=="CTRL" & data$Reward_Condition == p, 7:10],2,std.error)

yoked <- data.frame('means'=yoked_means,'errors'=yoked_errors,'block'=1:4,'group'=rep('No control',4))
control <- data.frame('means'=ctrl_means,'errors'=ctrl_errors,'block'=1:4,'group'=rep('With control',4))
lineplot_df <- rbind(yoked,control,make.row.names=F) ; rm(yoked_errors,yoked_means,ctrl_errors,ctrl_means,yoked,control)

lineplot_df$means <- c(0.5,0.25,0.25,0.25,0.65,0.85,0.9,0.9) 

print(
  ggplot(lineplot_df,mapping = aes(y = means,x = block,group=group)) +
  geom_line(aes(colour=group),size=1) +
  geom_point(size=3) + 
  theme(legend.key.size = unit(0.8, 'mm'), #change legend key size
          legend.key.height = unit(0.8, 'mm'), #change legend key height
          legend.key.width = unit(3, 'mm'), #change legend key width
          legend.title = element_text(size=10), #change legend title font size
          legend.text = element_text(size=10)) + #change legend text font size 
  scale_y_continuous(labels = percent_format(accuracy = 1),limits = c(0,1)) + 
  theme_classic() +
  scale_color_manual(values = c('darkgrey','black')) +
  labs(y = 'Exploration Rate', x = 'Block') + 
  geom_vline(xintercept=2.5,linetype=11) +
  ggtitle(paste0('What learned helplessness should look like:')) +
  theme(plot.title = element_text(hjust = 0.5))
)
} 

### bar plot direct ###

text <- c('Low','Moderate','High','Very high'); i <- 0

for(p in c(0.1,0.2,0.4,1)){
  
  i <- i + 1

yoked_means <- apply(data[data$Control_Condition=="YOK" & data$Reward_Condition == p, 16:19],2,mean)
yoked_errors <- apply(data[data$Control_Condition=="YOK" & data$Reward_Condition == p, 16:19],2,std.error)

ctrl_means <- apply(data[data$Control_Condition=="CTRL" & data$Reward_Condition == p, 16:19],2,mean)
ctrl_errors <- apply(data[data$Control_Condition=="CTRL" & data$Reward_Condition == p, 16:19],2,std.error)

yoked <- data.frame('means'=yoked_means,'errors'=yoked_errors,'block'=1:4,'group'=rep('No control',4))
control <- data.frame('means'=ctrl_means,'errors'=ctrl_errors,'block'=1:4,'group'=rep('With control',4))
plot_df <- rbind(yoked,control,make.row.names=F) ; rm(yoked_errors,yoked_means,ctrl_errors,ctrl_means,yoked,control)

print(ggplot(plot_df,aes(x=block,y=means,fill=group)) +
  geom_bar(stat="identity",position=position_dodge(),colour='black') +
  theme_classic() + 
  labs(y = 'Rating', x = 'Block') +
  ylim(0,10) +
  theme(legend.key.size = unit(0.8, 'mm'), #change legend key size
          legend.key.height = unit(0.8, 'mm'), #change legend key height
          legend.key.width = unit(3, 'mm'), #change legend key width
          legend.title = element_text(size=10), #change legend title font size
          legend.text = element_text(size=10)) + #change legend text font size 
  geom_errorbar(aes(ymax=means+errors,ymin=means-errors),width=0.3, position = position_dodge(0.9)) +
  scale_fill_manual(values = c('darkgrey','darkslategrey')) +
  ggtitle(paste0(text[i],' \n P(reward) = ',p)) +
  theme(plot.title = element_text(hjust = 0.5))
  )  
}

### bar plot pc1 ###

text <- c('Low','Moderate','High','Very high'); i <- 0

for(p in c(0.1,0.2,0.4,1)){
  
  i <- i + 1
  
  yoked_means <- apply(data[data$Control_Condition=="YOK" & data$Reward_Condition == p, 11:14],2,mean)
  yoked_errors <- apply(data[data$Control_Condition=="YOK" & data$Reward_Condition == p, 11:14],2,std.error)
  
  ctrl_means <- apply(data[data$Control_Condition=="CTRL" & data$Reward_Condition == p, 11:14],2,mean)
  ctrl_errors <- apply(data[data$Control_Condition=="CTRL" & data$Reward_Condition == p, 11:14],2,std.error)
  
  yoked <- data.frame('means'=yoked_means,'errors'=yoked_errors,'block'=1:4,'group'=rep('No control',4))
  control <- data.frame('means'=ctrl_means,'errors'=ctrl_errors,'block'=1:4,'group'=rep('With control',4))
  indirect_df <- rbind(yoked,control,make.row.names=F) ; rm(yoked_errors,yoked_means,ctrl_errors,ctrl_means,yoked,control)
  
  print(ggplot(indirect_df,aes(x=block,y=means,fill=group)) +
          geom_bar(stat="identity",position=position_dodge(),colour='black') +
          theme_classic() + 
          theme(legend.key.size = unit(0.8, 'mm'), #change legend key size
                legend.key.height = unit(0.8, 'mm'), #change legend key height
                legend.key.width = unit(3, 'mm'), #change legend key width
                legend.title = element_text(size=10), #change legend title font size
                legend.text = element_text(size=10)) + #change legend text font size 
          labs(y = 'Indicated Reward Probability Variance', x = 'Block') +
          geom_errorbar(aes(ymax=means+errors,ymin=means-errors),width=0.3, position = position_dodge(0.9)) +
          scale_fill_manual(values = c('darkgrey','darkslategrey')) +
          ggtitle(paste0(text[i],' \n P(reward) = ',p)) +
          theme(plot.title = element_text(hjust = 0.5))
  )  
}


## bar plot PC2 ##


yoked_means0.1 <- mean(data[data$Control_Condition=="YOK" & data$Reward_Condition == 0.1,15])
yoked_errors0.1 <- std.error(data[data$Control_Condition=="YOK" & data$Reward_Condition == 0.1,15])

ctrl_means0.1 <- mean(data[data$Control_Condition=="CTRL" & data$Reward_Condition == 0.1,15])
ctrl_errors0.1 <- std.error(data[data$Control_Condition=="CTRL" & data$Reward_Condition == 0.1,15])

yoked_means0.2 <- mean(data[data$Control_Condition=="YOK" & data$Reward_Condition == 0.2,15])
yoked_errors0.2 <- std.error(data[data$Control_Condition=="YOK" & data$Reward_Condition == 0.2,15])

ctrl_means0.2 <- mean(data[data$Control_Condition=="CTRL" & data$Reward_Condition == 0.2,15])
ctrl_errors0.2 <- std.error(data[data$Control_Condition=="CTRL" & data$Reward_Condition == 0.2,15])

yoked_means0.4 <- mean(data[data$Control_Condition=="YOK" & data$Reward_Condition == 0.4,15])
yoked_errors0.4 <- std.error(data[data$Control_Condition=="YOK" & data$Reward_Condition == 0.4,15])

ctrl_means0.4 <- mean(data[data$Control_Condition=="CTRL" & data$Reward_Condition == 0.4,15])
ctrl_errors0.4 <- std.error(data[data$Control_Condition=="CTRL" & data$Reward_Condition == 0.4,15])

yoked_means1 <- mean(data[data$Control_Condition=="YOK" & data$Reward_Condition == 1,15])
yoked_errors1 <- std.error(data[data$Control_Condition=="YOK" & data$Reward_Condition == 1,15])

ctrl_means1 <- mean(data[data$Control_Condition=="CTRL" & data$Reward_Condition == 1,15])
ctrl_errors1<- std.error(data[data$Control_Condition=="CTRL" & data$Reward_Condition == 1,15])

yoked_means <- c(yoked_means0.1,yoked_means0.2,yoked_means0.4,yoked_means1)
yoked_errors <- c(yoked_errors0.1,yoked_errors0.2,yoked_errors0.4,yoked_errors1)

ctrl_means <- c(ctrl_means0.1,ctrl_means0.2,ctrl_means0.4,ctrl_means1)
ctrl_errors <- c(ctrl_errors0.1,ctrl_errors0.2,ctrl_errors0.4,ctrl_errors1)

yoked <- data.frame('means'=yoked_means,'errors'=yoked_errors,'p_condition'=c('Low (p=0.1)','Moderate (p=0.2)','High (p=0.4)','Very high (p=1)'),'group'=('No control'))
control <- data.frame('means'=ctrl_means,'errors'=ctrl_errors,'p_condition'=c('Low (p=0.1)','Moderate (p=0.2)','High (p=0.4)','Very high (p=1)'),'group'=('With control'))
indirect2_df <- rbind(yoked,control,make.row.names=F)
indirect2_df$p_condition <- factor(indirect2_df$p_condition,levels = c('Low (p=0.1)','Moderate (p=0.2)','High (p=0.4)','Very high (p=1)'))

ggplot(indirect2_df,aes(x=p_condition,y=means,fill=group)) +
  geom_bar(stat="identity",position=position_dodge(),colour='black') +
  theme_classic() + 
  labs(y = 'Squared correlation', x = 'Reward level') +
  theme(legend.key.size = unit(0.8, 'mm'), #change legend key size
        legend.key.height = unit(0.8, 'mm'), #change legend key height
        legend.key.width = unit(3, 'mm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=10)) + #change legend text font size 
  geom_errorbar(aes(ymax=means+errors,ymin=means-errors),width=0.3, position = position_dodge(0.9)) +
  ggtitle('Squared correlation between \n guessed scores and explored squares') +
  ylim(0,1) +
  scale_fill_manual(values = c('darkgrey','darkslategrey')) +
  theme(plot.title = element_text(hjust = 0.5))

