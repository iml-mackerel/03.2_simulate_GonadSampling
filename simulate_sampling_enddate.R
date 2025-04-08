###############################################################################################
### SIMULATION
### question: is the end date of sampling important?
### simulations assume the same time steps (sample every 4 day); if the season is shortened
### less samples are overall collected
###############################################################################################
source("./R/sim.spawn.R")
library(ggplot2)

# load simulation scenarios
scen <- read.csv("data/simulate.sampling.csv",sep=',')
scen <- scen[,c(1,3)]
scen[8,2] <- 130
scen[9,2] <- 230
scen[10,2] <- 4

# simulation
par(mfrow=c(ncol(scen)-1,2))
sims <- lapply(0:45,function(x){
    print(x)
    arg <- as.list(scen[,2])
    names(arg) <- scen[,1]
    arg$day.end <- arg$day.end-x
    do.call('sim.spawn',arg)
})

# get relative error in parameter estimates
re <- function(obs,true) (obs-true)*100/true
library(plyr)
library(reshape2)
simsre <- ldply(1:length(sims),function(x){
    co <- data.frame(t(attr(sims[[x]],'coef')))
    co[,1] <- re(co[,1],scen[scen$para=="L",2])
    co[,2] <- re(co[,2],scen[scen$para=="x0",2])
    co[,3] <- re(co[,3],scen[scen$para=="k",2])
    names(co) <- c("L","x0","k")
    co <- cbind(end.date=arg$day.end-x+1,co)
    co
})

out <- melt(simsre,id=c("end.date"))

p1 <- ggplot(out,aes(x=end.date,y=value))+
    geom_boxplot(aes(group=end.date))+
    geom_smooth()+
    facet_wrap(~variable,scales = "free_y")+
    labs(y="Relative error",x="doy")

truegsi <-attr(get.season(L,x0,k),"GSIpred")
p2 <- ggplot(truegsi,aes(x=day,y=pred))+
    geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=min(out$end.date),xmax=max(out$end.date)),alpha=0.5)+
    geom_line()+
    labs(y="GSI (simulated)",x="doy")

gridExtra::grid.arrange(p2,p1)


ggsave("img/sim_error_end.png",p)


