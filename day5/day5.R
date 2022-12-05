library(dplyr)


make_numbered_move <- function(x, n, f, t){
	x[[t]] <- c(x[[t]], rev(tail(x[[f]], n=n)))
	x[[f]] <- x[[f]][seq(1, length(x[[f]]) - n)]
	x
}

make_renumbered_move <- function(x, n, f, t){
	x[[t]] <- c(x[[t]], tail(x[[f]], n=n))
	x[[f]] <- x[[f]][seq(1, length(x[[f]]) - n)]
	x
}



get_tops <- function(x){
	paste0(unlist(lapply(x, \(y){tail(y, n=1)})), sep = "", collapse = "")
}


# Load in data ------------------------------------------------------------

input_state <- readLines("day5/input.txt", n = 9)

input_pos <- c(unlist(strsplit(input_state[length(input_state)], "")) != " ")
names(input_pos) <- seq(length(input_pos))
input_pos <- as.integer(names(input_pos[input_pos])) #Positions of starting cubes

move_description <- read.table("day5/input.txt", skip = 9) |> 
	transmute(across(c("V2", "V4", "V6"), .funs = as.integer))




# Reshape initial state ---------------------------------------------------


row_lists <- lapply(lapply(input_state[-length(input_state)], strsplit, ""), \(x, y){x[[1]][y]}, input_pos)


col_lists <- lapply(seq(length(input_pos)), \(x, y){l <- rev(unlist(lapply(y, `[`, x)));l[l!=" "]}, row_lists)

get_tops(col_lists)




col_lists_init <- col_lists
#A nice loop over all of the moves
for(i in seq(nrow(move_description))){
	n <- move_description[i, 1]
	f <- move_description[i, 2]
	t <- move_description[i, 3]
	col_lists <- make_numbered_move(col_lists, n, f, t)
}

get_tops(col_lists)




#A nice loop over all of the moves
for(i in seq(nrow(move_description))){
	n <- move_description[i, 1]
	f <- move_description[i, 2]
	t <- move_description[i, 3]
	col_lists_init <- make_renumbered_move(col_lists_init, n, f, t)
}

get_tops(col_lists_init)
