logi <- function(x,L,x0,k) L/(1+exp((x0-x)/k))   # idem  as SSlogis

get.season <- function(L,x0,k,start=0.5,end=365.5){
    d1 <- data.frame(day=seq(start,end,1))
    d1$pred <- logi(d1$day,L,x0,k)
    d2 <- data.frame(day=seq(ceiling(start),floor(end),1))
    slope <- rep(NA,nrow(d2))
    for(j in 1:(nrow(d1)-1)){slope[j] <- d1[j+1,2]-d1[j,2]}
    d2$prop <- slope*100/sum(slope,na.rm=T)
    
    attr(d2,'GSIpred') <- d1
    return(d2)
}


##' sim.spawn
##' @param nsim number of simulations
##' @param L logistic parameter
##' @param x0 logistic parameter
##' @param k logistic parameter
##' @param samp.size number of GSI measurements per sample
##' @param samp.sizesd sd
##' @param obs.cv observation CV (spread around curve)
##' @param day.start start date of sampling
##' @param day.end end date of sampling
##' @param day.frey frequency of sampling (every x days)
##' @param day.margin error around frequency of sample (x days before or after desired sample date)
##' @param plot logical
##' @details Simulate GSI sampling process and estimation of daily proportion of eggs spawned.
##' @export
sim.spawn <- function(nsim=300,L,x0,k,samp.size,samp.sizesd,obs.cv,day.start,day.end,day.freq,day.margin,plot=TRUE){
    
    # true values
    trueprop <- get.season(L,x0,k)
    truegsi <-  attr(trueprop,"GSIpred")
    
    # simulated values
    simgsi <- matrix(NA,ncol=nsim,nrow=nrow(truegsi))
    simprop <- matrix(NA,ncol=nsim,nrow=nrow(trueprop))
    simco <- matrix(NA,ncol=nsim,nrow=3)
    
    daytarget <- seq(day.start,day.end,day.freq)
    print(paste0("number of samples: ",length(daytarget)))
    
    dummy <- lapply(1:nsim,function(i){
        daytrue <- sapply(daytarget,function(x)round(runif(1,x-day.margin-0.4999,x+day.margin+0.499),0))
        dat <- data.frame(doy=rep(daytrue,each=round(rnorm(1,samp.size,samp.sizesd)/2)))  # only females
        dat$obssim <- sapply(dat$doy, function(x){logi(x,L,x0,k)})
        dat$obs <- sapply(dat$obssim, function(x)max(0.01,rnorm(length(x),x,obs.cv*x)))
        
        if(plot & i==1){
            plot(truegsi$day,truegsi$pred,type='l',xlab='DOY',ylab='GSI',ylim=c(0,L+L*obs.cv*2))
            points(dat$doy,dat$obs,col='red',pch=16)
        }
        
        model <- nls(obs ~ SSlogis(doy, asym, xmid, scal), data = dat,control = list(maxiter = 1000))
        co <- coef(model)
        
        prop <- get.season(co[1],co[2],co[3])
        simprop[,i] <<- prop[,2]
        simgsi[,i] <<- attr(prop,"GSIpred")[,2]
        simco[,i] <<- co
        
        if(plot){
            if(i==1){
                plot(prop$day,prop$prop,type='l',xlab='DOY',ylab='Proportion of eggs spawned',col='darkgrey',ylim=c(0,max(prop$prop)*2))
            }else{
                lines(prop$day,prop$prop,col='darkgrey')
                if(i==nsim) lines(trueprop$day,trueprop$prop,col='red') 
            }
        }
    })
    
    simgsi <- cbind(truegsi,simgsi)
    simprop <- cbind(trueprop,simprop)
    
    attr(simprop,'GSIpred') <- simgsi
    attr(simprop,'coef') <- simco
    return(simprop)
}

