library(dplyr)

input_data <- read.table("day9/input.txt")

# input_data <- read.table(text = "R 4
# U 4
# L 3
# D 1
# R 4
# D 1
# L 5
# R 2")
# 
# input_data <- read.table(text = "R 5
# U 8
# L 8
# D 3
# R 17
# D 10
# L 25
# U 20")

are_adjacent <- function(p1, p2){
	abs(p1[1] - p2[1]) <= 1 & abs(p1[2] - p2[2]) <= 1
}

catch_up_old <- function(p1, p2){
	#Not adjacent, move diagonally towards
	#Returns new p2
	# browser()
	if(p1[1] - p2[1] == 2){
		p2[2] <- p2[2]
		p2[1] <- p1[1] - 1
		return(p2)
	}
	if(p1[1] - p2[1] == -2){
		p2[2] <- p1[2]
		p2[1] <- p1[1] + 1
		return(p2)
	}
	if(p1[2] - p2[2] == 2){
		p2[1] <- p1[1]
		p2[2] <- p1[2] - 1
		return(p2)
	}
	if(p1[2] - p2[2] == -2){
		p2[1] <- p1[1]
		p2[2] <- p1[2] + 1
		return(p2)
	}	
	return(p2)
}

catch_up <- function(p1, p2){
	#Not adjacent, move diagonally towards
	#Returns new p2
	# browser()
	
	change <-  c((p1[1] - p2[1])/abs((p1[1] - p2[1])), (p1[2] - p2[2])/abs((p1[2] - p2[2])))
	change[is.na(change)] <- 0
	p2 <- p2 + change
	return(p2)
}

move <- function(pos, dir, dist){
	p1 <- pos[[1]]#head
	p2 <- pos[[2]]#tail
	moves <- list(U = c(0, 1), D = c(0, -1), L = c(-1, 0), R = c(1, 0))
	p2_list <- vector(mode = "list", length = dist)
	for(i in seq(dist)){
		p1 <- p1 + moves[[dir]]
		p2 <- if(are_adjacent(p1, p2)){p2}else{catch_up(p1, p2)}
		p2_list[[i]] <- p2
	}
	pos <- list(p1, p2, c(pos[[3]], list(p2_list)))
}

move_10 <- function(pos, dir, dist, n=10){
	moves <- list(U = c(0, 1), D = c(0, -1), L = c(-1, 0), R = c(1, 0))
	tail_list <- vector(mode = "list", length = dist)
	for(i in seq(dist)){
		pos[[1]] <- pos[[1]] + moves[[dir]]
		for(j in seq(2, n)){
			pos[[j]] <- if(are_adjacent(pos[[j - 1]], pos[[j]])){pos[[j]]}else{catch_up(pos[[j - 1]], pos[[j]])}
		}
		tail_list[[i]] <- pos[[n]]
	}
	pos[[n+1]] <- c(pos[[n + 1]], list(tail_list))
	pos
}


pos <- list(c(0,0), c(0,0), list(list(c(0,0))))
for(i in seq(nrow(input_data))){
	pos <- move_10(pos, dir = input_data[i, 1], dist = input_data[i, 2], n=2)
}

unlist(pos[[3]], recursive = FALSE) |> unique() |> length()


pos_10 <- list(c(0,0), c(0,0), c(0,0), c(0,0), c(0,0), c(0,0), c(0,0), c(0,0), c(0,0), c(0,0), list(list(c(0,0))))
for(i in seq(nrow(input_data))){
	pos_10 <- move_10(pos_10, dir = input_data[i, 1], dist = input_data[i, 2], n=10)
}


unlist(pos_10[[11]], recursive = FALSE) |> unique() |> length()
