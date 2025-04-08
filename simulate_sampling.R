###############################################################################################
### DEMO simulation
###############################################################################################

### 1) generate fake curve

L <- 20                      # plateau
x0 <- 172                    # peak day of spawning!!
k <- -10                     # scale (duration)
start <- 120.5
end <- 250.5
refq <- c(10,50,90)          # quantiles for reference

logi <- function(x,L,x0,k) L/(1+exp((x0-x)/k)) #

d1 <- data.frame(day=seq(start,end,1))
d1$pred <- logi(d1$day,L,x0,k)
d2 <- data.frame(day=seq(ceiling(start),floor(end),1))
for(j in 1:(nrow(d1)-1)){d2[j,'slope'] <- d1[j+1,'pred']-d1[j,'pred']}
d2$prob <- d2$slope/sum(d2$slope,na.rm=T)
d2$probcums <- cumsum(d2$prob*100)

par(mfrow=c(2,1))
plot(d1$day,d1$pred,type='l',xlim=c(start,end))
abline(v=152,col='grey')     # june 1st
abline(v=172,col='darkgrey') # june 21st
abline(v=212,col='grey')     # july 31st
plot(d2$day,d2$prob,type='l',xlim=c(start,end))

find.q <- function(x,cums)which(abs(cums-x)==min(abs(cums-x)))
qs <- sapply(refq,find.q,d2$probcums)

ref <- d2[qs,]  # true proportion of spawning at 10%, 50% and 90% of spawning season

### 2) generate fake observations

day.start <- 150 # May 30
day.end <- 213 # aug 1
day.freq <- 7
day.error <- 2
samp.n <- 1
samp.size <- 25
samp.sizesd <- 3
obs.cv <- 0.3  # applied a bit simply 

daytarget <- seq(day.start,day.end,day.freq)
daytrue <- sapply(daytarget,function(x)round(runif(1,x-day.error-0.4999,x+day.error+0.499),0))
dat <- data.frame(doy=rep(daytrue,each=round(rnorm(1,samp.size,samp.sizesd))))
dat$obssim <- sapply(dat$doy, function(x){logi(x,L,x0,k)})
dat$obs <- sapply(dat$obssim, function(x)max(0.01,rnorm(length(x),x,obs.cv*x)))

par(mfrow=c(2,1))
plot(d1$day,d1$pred,type='l',xlim=c(start,end))
abline(v=150,col='grey')     # start
abline(v=172,col='darkgrey') # peak
abline(v=213,col='grey')     # end
points(dat$doy,dat$obs,col='red',pch=16)

### 3 ) generate fake fit
model <- nls(obs ~ SSlogis(doy, a, b, c), data = dat)
lines(dat$doy, predict(model),col='green')

co <- coef(model)
d1pred <- data.frame(day=seq(start,end,1))
d1pred$pred <- logi(d1pred$day,co[1],co[2],co[3])
d2pred <- data.frame(day=seq(ceiling(start),floor(end),1))
for(j in 1:(nrow(d1pred)-1)){d2pred[j,'slope'] <- d1pred[j+1,'pred']-d1pred[j,'pred']}
d2pred$prob <- d2pred$slope/sum(d2pred$slope,na.rm=T)
d2pred$probcums <- cumsum(d2pred$prob*100)

plot(d2$day,d2$prob*100,type='l',xlim=c(start,end))
lines(d2pred$day,d2pred$prob*100,col='green')

### 4) relative error
pr <- d2pred[d2pred$day %in% ref$day,"probcums"]
re <- (pr- ref$probcums)*100/ref$probcums

###############################################################################################
### FULL SIMULATION
###############################################################################################
source("./R/sim.spawn.R")
library(ggplot2)

# load simulation scenarios
scen <- read.csv("data/simulate.sampling.csv",sep=',')
scen <- scen[,1:4]

# simulation
par(mfrow=c(ncol(scen)-1,2))
sims <- lapply(2:ncol(scen),function(x){
    print(names(scen)[x])
    arg <- as.list(scen[,x])
    names(arg) <- scen[,1]
    do.call('sim.spawn',arg)
})

# get relative error
re <- function(obs,true) (obs-true)*100/true
simsre <- lapply(sims,function(x){
    cbind(x[,1:2],sweep(x[,3:ncol(x)],1,x[,2],FUN = re))
})

# shortern series to 99% spawning period
find.q <- function(x,cums)which(abs(cums-x)==min(abs(cums-x)))
simshort <- lapply(simsre,function(x){
    qs <- sapply(c(1,10,50,90,99),find.q,cumsum(x$prop))
    x[qs[2]:qs[4],]
})

# add name
simsf <- lapply(1:length(simshort),function(x) cbind(sim=names(scen)[x+1],simshort[[x]]))
simsf <- do.call("rbind",simsf)
simsf <- reshape2::melt(simsf,id=c('sim','day','prop'))

# plot error
peaks <- reshape2::melt(scen,id='para',variable.name = 'sim',value.name = 'day')
peaks <- peaks[peaks$para=='x0',]
nsamp <- sapply(2:ncol(scen),function(x){
    l <- seq(scen[scen$para=='day.start',x],scen[scen$para=='day.end',x],scen[scen$para=='day.freq',x])
    length(l)
})
nsamp <- data.frame(sim=names(scen)[-1],nsamp=nsamp)

p <- ggplot(simsf,aes(y=value,x=day))+
    geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-10,ymax=10),alpha=0.5)+
    geom_vline(data=peaks,aes(xintercept=day),col='red')+
    geom_boxplot(aes(group=day))+
    facet_wrap(~sim)+
    geom_text(data=nsamp,aes(x=Inf,y=Inf,label=paste('n samples: ',nsamp)),hjust=1.1,vjust=1.1)+
    theme_bw()

ggsave("img/sim_error.png",p)


