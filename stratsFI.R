randomStrat <- function(plateau) {
	sample(1:plateau$c, size=1)
}

lessRandomStrat <- function(plateau) {
	potentialMoves <- colorToFlood(plateau)
	sample(potentialMoves, size=1)
}

maxiFloodStrat <- function(plateau) {
	maxFlood <- calcFlood(plateau)
	potentialFloods <- colorToFlood(plateau)
	potentialMoves <- NULL
	for (i in potentialFloods) {
		tmpPlateau <- playPlateau(plateau, i, plot=FALSE)
		tmpFlood <- calcFlood(tmpPlateau)
		if(tmpFlood>maxFlood) {
			potentialMoves <- i
			maxFlood <- tmpFlood }
		if(tmpFlood==maxFlood)
			potentialMoves <- c(potentialMoves, i)
	}
	ifelse(is.null(potentialMoves), plateau$board[1,1], min(potentialMoves))
}

miniConnexStrat <- function(plateau) {
	minConnex <- calcConnexite(plateau)
	potentialFloods <- colorToFlood(plateau)
	potentialMoves <- NULL
	for (i in potentialFloods) {
		tmpPlateau <- playPlateau(plateau, i, plot=FALSE)
		tmpConnex <- calcConnexite(tmpPlateau)
		if(tmpConnex<minConnex) {
			potentialMoves <- i
			minConnex<- tmpConnex }
		if(tmpConnex==minConnex)
			potentialMoves <- c(potentialMoves, i)
	}
	min(potentialMoves)
}

maxiFlood3Strat <- function(plateau) {
	maxFlood <- calcFlood(plateau)
	potentialFloods <- colorToFlood(plateau)
	potentialMoves <- NULL
	for (i in potentialFloods) {
		tmpPlateau <- playPlateau(plateau, i, plot=FALSE)
		tmpNxtMove <- maxiFloodStrat(tmpPlateau)
		tmpNxtPlateau <- playPlateau(tmpPlateau, tmpNxtMove, plot=FALSE)
                tmpNxtNxtMove <- maxiFloodStrat(tmpNxtPlateau)
                tmpNxtNxtPlateau <- playPlateau(tmpNxtPlateau, tmpNxtNxtMove, plot=FALSE)
		tmpFlood <- calcFlood(tmpNxtNxtPlateau)
		if(tmpFlood>maxFlood) {
			potentialMoves <- i
			maxFlood <- tmpFlood }
		if(tmpFlood==maxFlood)
			potentialMoves <- c(potentialMoves, i)
	}
	ifelse(is.null(potentialMoves), plateau$board[1,1], min(potentialMoves))
}

maxiFlood2Strat <- function(plateau) {
	maxFlood <- calcFlood(plateau)
	potentialFloods <- colorToFlood(plateau)
	potentialMoves <- NULL
	for (i in potentialFloods) {
		tmpPlateau <- playPlateau(plateau, i, plot=FALSE)
		tmpNxtMove <- maxiFloodStrat(tmpPlateau)
		tmpNxtPlateau <- playPlateau(tmpPlateau, tmpNxtMove, plot=FALSE)
		tmpFlood <- calcFlood(tmpNxtPlateau)
		if(tmpFlood>maxFlood) {
			potentialMoves <- i
			maxFlood <- tmpFlood }
		if(tmpFlood==maxFlood)
			potentialMoves <- c(potentialMoves, i)
	}
	ifelse(is.null(potentialMoves), plateau$board[1,1], min(potentialMoves))
}

maxiDiverStrat <- function(plateau) {
	maxFlood <- 0 #calcFlood(plateau, weights=calcWeights(plateau))
	potentialFloods <- colorToFlood(plateau)
        #cat(potentialFloods,'\n')
	potentialMoves <- NULL
	for (i in potentialFloods) {
		tmpPlateau <- playPlateau(plateau, i, plot=FALSE)
		tmpFlood <- calcFlood(tmpPlateau, weights=calcWeights(tmpPlateau))
		if(tmpFlood>maxFlood) {
			potentialMoves <- i
			maxFlood <- tmpFlood }
		if(tmpFlood==maxFlood)
			potentialMoves <- c(potentialMoves, i)
	}
	ifelse(is.null(potentialMoves), plateau$board[1,1], min(potentialMoves))
}
