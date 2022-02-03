data <- read.csv("C:/Users/Jeroe/OneDrive/Bureaublad/Thesis/Analysis/LearnedHelplesnessDATA.csv")

library(ggplot2)
library(grid)
library(scales)
library(plotrix)
library(gridExtra)

p <- 0.1
i <- 1

text <- c('Low','Moderate','High','Very high')

yoked_means <- apply(data[data$Control_Condition=="YOK" & data$Reward_Condition == p, 11:14],2,mean)
yoked_errors <- apply(data[data$Control_Condition=="YOK" & data$Reward_Condition == p, 11:14],2,std.error)

ctrl_means <- apply(data[data$Control_Condition=="CTRL" & data$Reward_Condition == p, 11:14],2,mean)
ctrl_errors <- apply(data[data$Control_Condition=="CTRL" & data$Reward_Condition == p, 11:14],2,std.error)

yoked <- data.frame('means'=yoked_means,'errors'=yoked_errors,'block'=1:4,'group'=rep('No control',4))
control <- data.frame('means'=ctrl_means,'errors'=ctrl_errors,'block'=1:4,'group'=rep('With control',4))
indirect_df <- rbind(yoked,control,make.row.names=F) ; rm(yoked_errors,yoked_means,ctrl_errors,ctrl_means,yoked,control)

p1 <- ggplot(indirect_df,aes(x=block,y=means,fill=group)) +
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

grid.arrange(p1,p2,p3,p4,nrow=2)
