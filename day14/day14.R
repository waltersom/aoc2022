library(dplyr)
library(ggplot2)
library(tidyr)
input_data <- readLines("day14/input.txt")

#For each line, get all the coords

coord_vecs <- lapply(input_data, \(x){
	strsplit(x, "->") |> sapply(trimws) |> as.vector()
})

points_between <- function(x1, y1, x2, y2){
	if(is.na(x2)){
		return(data.frame(x = as.integer(x1), y = as.integer(y1)))
	}
	x <- seq(as.integer(x1), as.integer(x2))
	y <- seq(as.integer(y1), as.integer(y2))
	data.frame(x, y)
}

start_map <- coord_vecs |> lapply(\(p){
	 # browser()
	p1 <- p |> strsplit(",")
	p2 <- lead(p) |> strsplit(",")
	res <- data.frame()
	if(length(p1) == 0){
		return(data.frame(x = p2[[1]][1], y = p2[[1]][2]))
		}
	for(i in seq(length(p1))){
		res <- rbind(res, points_between(p1[[i]][1], p1[[i]][2], p2[[i]][1], p2[[i]][2]))
	}
	res
}) |> do.call(what = rbind) |> 
	distinct()



x_range <- range(start_map$x)
y_range <- range(start_map$y)
start_map2 <- start_map |> mutate(x = x - x_range[1] + 1, y = y + 1)
start_point <- data.frame(x=500 - x_range[1] + 1, y=1)
start_matrix <- matrix(0, nrow = y_range[2] + 1, ncol =x_range[2] - x_range[1] + 1) #[y, x]

for(i in seq(nrow(start_map2))){
	start_matrix[start_map2[i, 2], start_map2[i, 1]] <- 1
}

drop_sand <- function(map, start_x = 11, start_y = 1, min_x = 1, max_x = ncol(map)){
	pos <- c(start_y, start_x)
	while(between(pos[2], min_x, max_x)){
		# browser()
		if(pos[1] < nrow(map) && map[pos[1] + 1, pos[2]] == 0){#Can drop
			pos <- pos + c(1, 0)
		}else{
			if(pos[1] < nrow(map) && pos[2] > 1 && pos[2] < ncol(map) && map[pos[1] + 1, pos[2] - 1] == 0){#Can drop diagonal left
				pos <- pos + c(1, -1)
			}else{
				if(pos[2] < ncol(map) && pos[1] < nrow(map) && map[pos[1] + 1, pos[2] + 1] == 0){#Can drop diagonal right
					pos <- pos + c(1, 1)
				}else{#at rest
					if(pos[2] < ncol(map) & pos[2] > 1 & pos[1] < nrow(map)){
						map[pos[1], pos[2]] <- 2
						return(map)
					}else{
						#Abyss
						return(map)
					}
				}
			}
		}
	}
	map
}

count_sand <- function(start_matrix, start_point){
	start_count <- length(which(start_matrix == 2))
	last_count <- start_count
	while(TRUE){
		start_matrix <- drop_sand(start_matrix, start_x = start_point$x)
		new_count <- length(which(start_matrix == 2))
		if(new_count == last_count){
			# browser()
			return(new_count)
		}
		last_count <- new_count
	}
}
count_sand_block <- function(start_matrix, start_point){
	while(TRUE){
		start_matrix <- drop_sand(start_matrix, start_x = start_point$x)
		new_count <- length(which(start_matrix == 2))
		if(start_matrix[2, start_point$x] == 2 & start_matrix[2, start_point$x + 1] == 2 & start_matrix[2, start_point$x - 1] == 2){
			# browser()
			return(new_count + 1)
		}
		last_count <- new_count
	}
}
	
sand_count <- count_sand(start_matrix, start_point)
sand_count


start_matrix_floor <- matrix(0, nrow = y_range[2] + 3, ncol =1000) #[y, x]

for(i in seq(nrow(start_map))){
	start_matrix_floor[start_map[i, 2] + 1, start_map[i, 1]] <- 1
}
start_matrix_floor[nrow(start_matrix_floor),] <- 1
start_point_floor <- data.frame(x=500, y=1)

sand_count_floor <- count_sand_block(start_matrix_floor, start_point = start_point_floor)
sand_count_floor

#Run 500 times
#
new_matrix <- start_matrix_floor
for(i in seq(2000)){
	new_matrix <-  drop_sand(new_matrix, start_x = start_point_floor$x)
}
map_df <- as.data.frame(new_matrix) |> mutate(y = row_number()) |> pivot_longer(cols = -"y", names_to = "x", names_prefix = "V") |> 
	mutate(x = as.integer(x)) |> filter(between(x, 0, 1000))

ggplot(map_df, aes(x, as.factor(-y), fill=as.factor(value), colour = as.factor(value))) + geom_tile()
