directory <- c("C:/Users/Jeroe/OneDrive/Bureaublad/Thesis/Analysis/demographics",
               "C:/Users/Jeroe/OneDrive/Bureaublad/Thesis/Analysis/DirectPC",
               "C:/Users/Jeroe/OneDrive/Bureaublad/Thesis/Analysis/exploration",
               "C:/Users/Jeroe/OneDrive/Bureaublad/Thesis/Analysis/perceived_control1",
               "C:/Users/Jeroe/OneDrive/Bureaublad/Thesis/Analysis/perceived_control2"
)

#demographics
setwd(directory[1])
files <- list.files(path=directory[1])

demographics <- read.csv(files[1])
for(f in files[-1]){
    demographics <- rbind(demographics,read.csv(f))
}

#direct perceived control
setwd(directory[2])
files <- list.files(path=directory[2])

directPC <- read.csv(files[1])
for(f in files[-1]){
  directPC <- rbind(directPC,read.csv(f))
}

#exploration
setwd(directory[3])
files <- list.files(path=directory[3])

exploration <- read.csv(files[1]) 
for(f in files[-1]){
  exploration <- rbind(exploration,read.csv(f))
}

#perceived control 1
setwd(directory[4])
files <- list.files(path=directory[4])

PC1 <-  cbind(read.csv(files[1]),c(rep(1,4),rep(2,4),rep(3,4),rep(4,4)))
for(f in files[-1]){
  PC1 <- rbind(PC1,cbind(read.csv(f),c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))))
}

#perceived control 2
setwd(directory[5])
files <- list.files(path=directory[5])

PC2 <- read.csv(files[1],colClasses = "character")
for(f in files[-1]){
  PC2 <- rbind(PC2,read.csv(f,colClasses = "character"))
}

rm(f) ; rm(directory) ; rm(files)
