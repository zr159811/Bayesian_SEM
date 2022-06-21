
fit1 <- readRDS("/home/z458r456/Music/KJ_stan_model.rds")

library(ggplot2)
int_dat <- data.frame(fit1@sim$samples[[2]])

index1 <- names(int_dat[16:(1160+15)])
holder <- list()
for(i in index1){
  holder[[i]] <- int_dat[,i]  
}
eta_dat<- unlist(holder)
rm(holder); rm(index1)


index2<- names(int_dat[12780:(12779+1160)])
holder <- list()
for(i in index2){
  holder[[i]] <- int_dat[,i]  
}
xi2_dat<- unlist(holder)
rm(holder); rm(index2)

#Xi1
index3  <- names(int_dat[(12780-1160):(12779)])
holder <- list()
for(i in index3){
  holder[[i]] <- int_dat[,i]  
}
xi1_dat<- unlist(holder)
rm(holder);rm(index3)

#Plotting Simple Slopes

plot_dat<- data.frame(eta_dat,xi1_dat,xi2_dat)

plot_dat$Sentiment <- ifelse(plot_dat$xi2_dat < 1 & plot_dat$xi2_dat > -1, "Middle",
                             ifelse(plot_dat$xi2_dat > 1, "+1 SD", "-1 SD"))

ggplot(data = plot_dat, aes(x = xi1_dat,y = eta_dat, color = Sentiment)) +
  geom_smooth(fullrange = TRUE, method = "lm") +
  xlab("Voters Position") +
  ylab("Voters Perception of Candidate Position") 
