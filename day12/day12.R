library(dplyr)

input_data <- readLines("day12/input.txt") |> strsplit("") |> lapply(match, c(letters, "S", "E"))

elevation <- input_data |> lapply(\(x){case_when(x==27~1L, x==28~26L, TRUE~x)}) |> unlist() |> matrix(ncol = length(input_data)) |> t()

m <- matrix(unlist(input_data), ncol = length(input_data)) |> t()
which(m==27, arr.ind = TRUE)

start_point <- which(m==27, arr.ind = TRUE) |> as.vector()
target_point <- which(m==28, arr.ind <- TRUE) |>  as.vector()



get_moves <- function(elevation, rx, ry){
	ch <- elevation[rx,ry]
	possibles <- lapply(list(c(0,1), c(0,-1), c(1,0), c(-1,0)), `+`, c(rx, ry))
	possibles <- possibles[unlist(lapply(possibles, \(x){x[1]*x[2] > 0}))] # Remove moves that go negative
	possibles <- possibles[unlist(lapply(possibles, \(x){x[1] <= nrow(elevation) & x[2] <= ncol(elevation)}))] # Remove moves that go negative
	possibles <- possibles[unlist(lapply(possibles, \(x){elevation[x[1], x[2]] <= ch + 1}))]#Remove squares that are too high
}


try_find_path2 <- function(start = c(1,1), end = c(3,6), elevation, fastest_to = elevation * Inf){
	fastest_to[start[1], start[2]] <- 0
	current_count <- fastest_to[start[1], start[2]]
	start_list <- list(start)
	while(length(start_list) > 0 & any(as.logical(lapply(start_list, \(x){fastest_to[x[1], x[2]] < fastest_to[end[1], end[2]]})))){
		new_start_list <- list()
		for(r in start_list){
			current_count <-  fastest_to[r[1], r[2]]
			move_from_here <- get_moves(elevation, r[1], r[2])
				for(t in move_from_here){
					if(fastest_to[t[1], t[2]] > current_count + 1 ){
						#This is the fastest way so far of getting to t
						fastest_to[t[1], t[2]] <- current_count + 1
						new_start_list <- c(new_start_list, list(t))
					}
				}
		}
		new_start_list <- unique(new_start_list)
		start_list <- new_start_list
	}
	list(count = fastest_to[end[1], end[2]], fastest_to = fastest_to)
}

try_find_backwards_path2 <- function(start = c(1,1), end = c(3,6), elevation, fastest_to = elevation * Inf){
	fastest_to[start[1], start[2]] <- 0
	current_count <- fastest_to[start[1], start[2]]
	start_list <- list(start)
	while(length(start_list) > 0){
		new_start_list <- list()
		for(r in start_list){
			current_count <-  fastest_to[r[1], r[2]]
			move_from_here <- get_moves(elevation, r[1], r[2])
			for(t in move_from_here){
				if(fastest_to[t[1], t[2]] > current_count + 1 ){
					#This is the fastest way so far of getting to t
					fastest_to[t[1], t[2]] <- current_count + 1
					new_start_list <- c(new_start_list, list(t))
				}
			}
		}
		new_start_list <- unique(new_start_list)
		start_list <- new_start_list
	}
	list(count = fastest_to[end[1], end[2]], fastest_to = fastest_to)
}


# tfp <- try_find_path(start_point, end = target_point, elevation)		

tfp2 <- try_find_backwards_path2(start_point, end = target_point, elevation)		

#Find the shortest from an "a" to end

a_squares <- which(elevation == 1, arr.ind = TRUE)
tfbp2 <- try_find_backwards_path2(end = start_point, start = target_point, -elevation, fastest_to = Inf*elevation)		

tfbp2[[2]][start_point[1], start_point[2]]#Same as above, good

mapply(\(x, y){tfbp2[["fastest_to"]][x, y]}, a_squares[,1], a_squares[,2]) |> min()

	