nouveauPlateau <- function(n = 14, c = 6) {
	plateau <- matrix(NA, n, n)
	for (i in 1:n) {
		plateau[i,] <- round(runif(n)*(c-1))+1 }
	list(board=plateau, n=n, c=c, steps=0, win=F)
	}

plotPlateau <- function(plateau) {
	myCol <- rainbow(plateau$c)[as.vector(t(plateau$board))]
	n <- plateau$n
	myX <- rep(1:n,n)
	myY <- as.vector(sapply(1:n, function(i) rep(i,n)))
	plot(0:(n+1), 0:(n+1), type="n",
	     xaxt="n", yaxt="n", xlab="", ylab="", bty="n",
	     main=paste("Nombre de coups :",plateau$steps),
	     sub=ifelse(plateau$win,"C'est gagné",""))
	rect(xleft=myX-0.5, y=myY-0.5, xright=myX+0.5, ytop=myY+0.5, col=myCol, border="transparent")
	text(x=myX, y=myY, labels=t(plateau$board))
	}

playPlateau <- function(plateau, move, plot=TRUE) {
	if(move!=plateau$board[1,1]) {
		plateau$board <- floodFill(plateau$board, plateau$board[1,1], move, plateau$n)
		plateau$steps <- plateau$steps + 1
		plateau$win <- min(plateau$board)==max(plateau$board)
		if(plot) plotPlateau(plateau) }
	plateau
	}


autoPlayPlateau <- function(plateau, strategie, plot=FALSE, withStats=FALSE) {
  ans <- NULL
  while(!plateau$win) {
    move <- strategie(plateau)
    plateau <- playPlateau(plateau, move, plot=plot)
    if(withStats) {
      ans <- rbind(ans, data.frame(steps=plateau$steps,
                                   move=move,
                                   flood=calcFlood(plateau),
                                   connex=calcConnexite(plateau),
                                   floodW=calcFlood(plateau, weights=calcWeights(plateau))))
    } else {
      ans <- rbind(ans, data.frame(steps=plateau$steps))
    }
  }
  ans
}

backtestStrat <- function(strategie, obs, plot=F, withStats=F, ...) {
  require(multicore)
  ans <- mclapply(1:obs, function(i) {
    cat(".")
    np <- nouveauPlateau(...)
    if(plot) {
      plotPlateau(np)
      title(sub=i) }
    autoPlayPlateau(np, strategie, plot=plot, withStats=withStats) })
  cat("\n")
  ans
}

floodFill <- function(board, old.col, new.col, n, x=1, y=1) {
	nodeQueue <- NULL
	if(board[x,y]==old.col)
		nodeQueue <- rbind(nodeQueue, c(x,y))
	while(length(nodeQueue) > 0) {
		node <- tail(nodeQueue, 1)
		nodeQueue <- rbind(nodeQueue[-nrow(nodeQueue),])
		if(board[node]==old.col) {
			w <- node[2]
			e <- node[2]
			checkW <- T
			while(checkW) {
				if(w>=1) {
					checkW <- board[node[1],w]==old.col
					w <- w - 1
				} else { checkW <- F
					   w <- w - 1 } }
			checkE <- T
			while(checkE) {
				if(e<=n) {
					checkE <- board[node[1],e]==old.col
					e <- e + 1
				} else { checkE <- F
					   e <- e + 1 } }
			w <- w + 2
			e <- e - 2
			board[node[1],w:e] <- new.col
			for(i in w:e) {
				if(node[1]>1)
					if(board[node[1]-1,i]==old.col)
						nodeQueue <- rbind(nodeQueue, c(node[1]-1,i))
				if(node[1]<n)
					if(board[node[1]+1,i]==old.col)
						nodeQueue <- rbind(nodeQueue, c(node[1]+1,i))
			}
		} }
	board
	}

colorToFlood <- function(plateau) {
	board <- plateau$board
	old.col <- board[1,1]
	nodeQueue <- rbind(NULL,c(1,1))
	colorList <- NULL
	while(length(nodeQueue) > 0) {
		node <- tail(nodeQueue, 1)
		nodeQueue <- rbind(nodeQueue[-nrow(nodeQueue),])
		if(board[node]==old.col) {
			w <- node[2]
			e <- node[2]
			checkW <- T
			while(checkW) {
				if(w>=1) {
					colorList <- c(colorList, board[node[1],w])
					checkW <- board[node[1],w]==old.col
					w <- w - 1
				} else { checkW <- F
					   w <- w - 1 } }
			checkE <- T
			while(checkE) {
				if(e<=plateau$n) {
					colorList <- c(colorList, board[node[1],e])
					checkE <- board[node[1],e]==old.col
					e <- e + 1
				} else { checkE <- F
					   e <- e + 1 } }
			w <- w + 2
			e <- e - 2
			board[node[1],w:e] <- plateau$c + 1
			for(i in w:e) {
				if(node[1]>1) {
					colorList <- c(colorList, board[node[1]-1,i])
					if(board[node[1]-1,i]==old.col)
					nodeQueue <- rbind(nodeQueue, c(node[1]-1,i))
				}
				if(node[1]<plateau$n) {
					colorList <- c(colorList, board[node[1]+1,i])
					if(board[node[1]+1,i]==old.col)
					nodeQueue <- rbind(nodeQueue, c(node[1]+1,i))
				}
			}
		} }
	unique(colorList[!(colorList %in% c(plateau$c + 1,old.col))])
}

calcFlood <- function(plateau, weights = matrix(1, plateau$n, plateau$n)) {
	board <- plateau$board
	old.col <- board[1,1]
	nodeQueue <- rbind(NULL,c(1,1))
	floodSize <- 0
	while(length(nodeQueue) > 0) {
		node <- tail(nodeQueue, 1)
		nodeQueue <- rbind(nodeQueue[-nrow(nodeQueue),])
		if(board[node]==old.col) {
			w <- node[2]
			e <- node[2]
			checkW <- T
			while(checkW) {
				if(w>=1) {
					checkW <- board[node[1],w]==old.col
					w <- w - 1
				} else { checkW <- F
					   w <- w - 1 } }
			checkE <- T
			while(checkE) {
				if(e<=plateau$n) {
					checkE <- board[node[1],e]==old.col
					e <- e + 1
				} else { checkE <- F
					   e <- e + 1 } }
			w <- w + 2
			e <- e - 2
			board[node[1],w:e] <- plateau$c + 1
			floodSize <- floodSize + sum(weights[node[1],w:e])
			for(i in w:e) {
				if(node[1]>1) {
					if(board[node[1]-1,i]==old.col)
						nodeQueue <- rbind(nodeQueue, c(node[1]-1,i))
				}
				if(node[1]<plateau$n) {
					if(board[node[1]+1,i]==old.col)
						nodeQueue <- rbind(nodeQueue, c(node[1]+1,i))
				}
			}
		} }
	floodSize
}

calcConnexite <- function(plateau) {
	board <- plateau$board
	nbConnexe <- 0
	while(min(board) <= plateau$c) {
		pointToFill <- which(board <= plateau$c)[1] - 1
		pointToFill <- c(pointToFill - plateau$n*(pointToFill %/% plateau$n) + 1, pointToFill %/% plateau$n + 1)
		nbConnexe <- nbConnexe + 1
		board <- floodFill(board, board[pointToFill[1], pointToFill[2]], plateau$c + nbConnexe, plateau$n, pointToFill[1], pointToFill[2])
	}
	board <- board - plateau$c
	log(max(board))/log(plateau$n*plateau$n)
}

calcWeights <- function(plateau) {
  biggerPlateau <- nouveauPlateau(plateau$n + 2, plateau$c)
  biggerPlateau$board[2:(plateau$n+1), 2:(plateau$n+1)] <- plateau$board
  biggerPlateau$board[1,2:(plateau$n+1)] <- plateau$board[1,]
  biggerPlateau$board[(plateau$n+2),2:(plateau$n+1)] <- plateau$board[plateau$n,]
  biggerPlateau$board[2:(plateau$n+1),1] <- plateau$board[,1]
  biggerPlateau$board[2:(plateau$n+1),(plateau$n+2)] <- plateau$board[,plateau$n]
  biggerPlateau$board[1,1] <- plateau$board[1,1]
  biggerPlateau$board[(plateau$n+2),1] <- plateau$board[plateau$n,1]
  biggerPlateau$board[1,(plateau$n+2)] <- plateau$board[1,plateau$n]
  biggerPlateau$board[(plateau$n+2),(plateau$n+2)] <- plateau$board[plateau$n,plateau$n]
  smallPlateau <- nouveauPlateau(3, plateau$c)
  sapply(1:plateau$n, function(j) {
    sapply(1:plateau$n, function(i) {
      smallPlateau$board <- biggerPlateau$board[i:(i+2),j:(j+2)]
      calcConnexite(smallPlateau)
    }) })
}
