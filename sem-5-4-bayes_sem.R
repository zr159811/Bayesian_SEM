### R code from vignette source 'sem-5-4-bayes_sem-tanglebackupstring.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: tmpout
###################################################
tdir <- "tmpout"
if(!dir.exists(tdir)) dir.create(tdir, showWarnings=FALSE)


###################################################
### code chunk number 3: sess10
###################################################
getOption("SweaveHooks")[["fig"]]()
library(MASS)
library(ggplot2)
set.seed(1856)
x1 <- data.frame(rep("College", 50),
                 mvrnorm(n = 50, mu = c(100, 100),
                         Sigma = matrix(c(15, 10, 10, 15),
                                        nrow = 2)))
colnames(x1) <- c("Education", "IQ","Income")
x2 <- data.frame(rep("HS", 50),
                 mvrnorm(n = 50, mu = c(100, 100),
                         Sigma = matrix(c(15, 3, 3, 15),
                                        nrow = 2)))
colnames(x2) <- c("Education", "IQ","Income")
dat2 <- rbind(x1,x2)
ggplot(data = dat2, aes(x = IQ, y = Income, color = Education)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, fullrange = T)+
  ggtitle("Interaction of Education and IQ predicting Income") +
  theme_bw()


###################################################
### code chunk number 4: sess0001
###################################################
set.seed(1857)
x1 <- rnorm(100,3,10)
x2 <- rnorm(100,2,10)
y <- x1+x2+x1*x2+rnorm(100,3,2)
dat <- data.frame(Income=y,IQ=x1,PSES=x2)
res <- lm(Income~PSES*IQ,data=dat)
z1 <- z2 <- seq(-1,1)
newdf <- expand.grid(IQ=z1,PSES=z2)

summary(lm(data = dat, Income ~ IQ*PSES))



###################################################
### code chunk number 5: sess11
###################################################
getOption("SweaveHooks")[["fig"]]()
ggplot(data=transform(newdf, Income=predict(res, newdf)), 
        aes(y=Income, x=PSES, color=factor(IQ))) + stat_smooth(method=lm)+ geom_point() +
        scale_colour_discrete(name="Parent SES SDs") + 
        labs(x="Standardized IQ", y="Standardized Income") + 
        scale_x_continuous(breaks=seq(-1,1)) + theme_bw() +
        ggtitle("Interaction of Parent SES and IQ on Income: Simple Slopes")


###################################################
### code chunk number 6: sess1
###################################################
dat<- read.table("data/data_kj.dat", header = TRUE)
head(dat)


###################################################
### code chunk number 7: sess2
###################################################
library(psych)
dat<- data.frame(scale(dat))
describe(dat,fast = T)


###################################################
### code chunk number 8: sess3
###################################################
library(ggplot2)
library(ggridges)
library(GGally)
library(reshape2)
dat_long <- melt(dat)

head(dat_long)
tail(dat_long)


###################################################
### code chunk number 9: sess4
###################################################
getOption("SweaveHooks")[["fig"]]()
ggplot(data = dat_long, aes(y = variable, x = value)) +
  geom_density_ridges() + theme_ridges()+
  scale_y_discrete(expand = c(0.01, 0))+
  scale_x_continuous(expand = c(0, 0))


###################################################
### code chunk number 10: sess5
###################################################
getOption("SweaveHooks")[["fig"]]()
ggpairs(dat,aes(alpha = 0.9),upper = list(continuous = wrap("cor", color = "black"))) +
  theme_bw() 


###################################################
### code chunk number 11: sess6
###################################################
getOption("SweaveHooks")[["fig"]]()
library(lavaan)
library(semPlot)
syntax1 <- '
V =~ v1 + v2
C =~ c1 + c2
S =~ s1 + s2'
mod1 <- cfa(model = syntax1,
            data = dat,
            std.lv = TRUE)
semPaths(mod1, layout = "tree2")



###################################################
### code chunk number 12: sess7
###################################################
summary(mod1)


###################################################
### code chunk number 13: sess8
###################################################
getOption("SweaveHooks")[["fig"]]()
syntax2 <- '
V =~ v1 + v2
C =~ c1 + c2
S =~ s1 + s2
C ~ V + S'
mod2 <- sem(model = syntax2,
            data = dat,
            std.lv = TRUE)
semPaths(mod2, layout = "tree")


###################################################
### code chunk number 14: sess9
###################################################
summary(mod2)


###################################################
### code chunk number 15: sess91
###################################################
getOption("SweaveHooks")[["fig"]]()
semPaths(mod2)


###################################################
### code chunk number 16: sess111 (eval = FALSE)
###################################################
## dat.kj <- read.table("data/data_kj.dat", header=T)
## 
## dat.kj$v1 <- dat.kj$v1-mean(dat.kj$v1)
## dat.kj$v2 <- dat.kj$v2-mean(dat.kj$v2)
## dat.kj$s1 <- dat.kj$s1-mean(dat.kj$s1)
## dat.kj$s2 <- dat.kj$s2-mean(dat.kj$s2)


###################################################
### code chunk number 17: sess12 (eval = FALSE)
###################################################
## N <- nrow(dat.kj)
## x <- cbind(dat.kj$v1,
##            dat.kj$v2,
##            dat.kj$s1,
##            dat.kj$s2)
## y <- cbind(dat.kj$c1,
##            dat.kj$c2)
## Kx <- ncol(x)
## Ky <- ncol(y)
## datstan <- list(N, Kx, Ky, y, x)


###################################################
### code chunk number 18: sess121
###################################################
getOption("SweaveHooks")[["fig"]]()
semPaths(mod2)


###################################################
### code chunk number 19: sess13
###################################################
fit1 <- readRDS("stan/KJ_stan_model.rds")
names(fit1)[1:6]  <- c("Intercept",
                       "Voter",
                       "Sentiment",
                       "Voter^2",
                       "VoterXSent",
                       "Sentiment^2")


###################################################
### code chunk number 20: sess14
###################################################
getOption("SweaveHooks")[["fig"]]()
rstan::stan_rhat(fit1)


###################################################
### code chunk number 21: sess15
###################################################
getOption("SweaveHooks")[["fig"]]()
rstan::stan_trace(fit1,"b1")


###################################################
### code chunk number 22: sess16
###################################################
getOption("SweaveHooks")[["fig"]]()
rstan::stan_dens(fit1,"b1",separate_chains =T)


###################################################
### code chunk number 23: sess17
###################################################

params <- c("b0","b1","sigmay","sigmax","sigmaeta","phi")

print(fit1,pars=params)



###################################################
### code chunk number 24: sess18
###################################################
tab_dat <- data.frame(`Regression Path` = c("Voter", "Sentiment", "Voter^2", "Sentiment^2", "Voter x Sentiment"),
                      `KJ 1984` = c("0.180", "-0.111", "0.009", "-0.019", "0.207"),
                      `Mplus LMS`= c("0.219", "-0.237", "0.048", "-0.032", "0.193"),
                      `Lavaan` = c("0.232","-0.243","NA","NA","NA"),
                      Bayesian = c("0.192", "-0.163", "0.042", "-0.031", "0.16"))

colnames(tab_dat) <- c("Regression Path", "K&J 1984", "Mplus LMS","Lavaan", "Bayesian")

pander::pander(tab_dat)


###################################################
### code chunk number 25: sess80
###################################################
sessionInfo()


###################################################
### code chunk number 26: opts81
###################################################
## Don't delete this. It puts the interactive session options
## back the way they were. If this is compiled within a session
## it is vital to do this.
options(opts.orig)
options(par.orig)


