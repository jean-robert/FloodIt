source('utilsFI.R')
source('stratsFI.R')

stratList <- list(random=randomStrat,
                  lessRandom=lessRandomStrat,
                  maxiFlood=maxiFloodStrat,
                  miniConnex=miniConnexStrat,
                  maxiFlood2=maxiFlood2Strat,
                  maxiDiver=maxiDiverStrat)

strat100 <- lapply(stratList, backtestStrat, obs=100)
strat1000.stats <- lapply(stratList, backtestStrat, obs=1000, plot=F, withStats=T)
strat1000.stats.n30 <- lapply(stratList, backtestStrat, obs=1000, plot=F, withStats=T, n=30)

















