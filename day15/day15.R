library(dplyr)
library(stringr)
library(ggplot2)
input_data <- readLines("day15/input.txt")

input_map <- str_extract_all(input_data, "[\\-0-9]+") |> lapply(function(x){data.frame(sx = x[1], sy = x[2], bx = x[3], by = x[4])}) |> 
	do.call(what = rbind) |> 
	mutate(across(.fns = as.integer)) |> 
	mutate(dist = abs(sx - bx) + abs(sy - by))

ggplot(input_map)+
	geom_point(aes(sx, -sy), colour = "red")+
	geom_point(aes(bx, -by), colour = "blue")

#For each sensor, find all of the positions that are closer that the beacon
#

x_range <- c(min(c(input_map$sx, input_map$bx)), max(c(input_map$sx, input_map$bx)))

get_closer_points <- function(r, row){
	y_change <- row - r[["sy"]]
	x_min <- -(r[["dist"]] - abs(y_change))
	x_max <- (r[["dist"]] - abs(y_change))
	if(x_min > x_max) return(data.frame())
	data.frame(x = seq(from = x_min, to = x_max) , y = y_change ) |> 
	filter(!(x == 0 & y == 0)) |> #Remove sensor
		mutate(x = x + r[["sx"]], y = y + r[["sy"]])
}


get_closer_points2 <- function(r, row){
	y_change <- row - r[["sy"]]
	x_min <- -(r[["dist"]] - abs(y_change))
	x_max <- (r[["dist"]] - abs(y_change))
	if(x_min > x_max) return(data.frame())
	data.frame(x = seq(from = x_min, to = x_max) , y = y_change ) |> 
		# filter(!(x == 0 & y == 0)) |> #Remove sensor
		mutate(x = x + r[["sx"]], y = y + r[["sy"]])
}


get_closer_xrange <- function(r, row){
	y_change <- row - r[["sy"]]
	x_min <- -(r[["dist"]] - abs(y_change))
	x_max <- (r[["dist"]] - abs(y_change))
	if(x_min > x_max) return(data.frame())
	data.frame(x_min, x_max, y = y_change) |> 
	mutate(x_min = x_min + r[["sx"]], x_max = x_max + r[["sx"]], y = y + r[["sy"]])
}
valid_points <- input_map |> rowwise() |> 
	summarise(get_closer_points(cur_data(), 2000000)) |> 
	ungroup() |> 
	distinct() |> 
	# filter(between(x, x_range[1], x_range[2])) |> 
	anti_join(input_map, by = c("x" = "bx", "y" = "by"))

ggplot(input_map)+
	geom_point(aes(sx, -sy), colour = "red")+
	geom_point(aes(bx, -by), colour = "blue")+
	geom_point(data = valid_points, aes(x, -y), colour = "brown")



valid_points |> nrow()
						

#Find points in a range that are not valid
get_invalid_points <- function(xrange = c(0, 20), yrange = c(0, 20)){
	y_order <- sample(replace = FALSE, size = yrange[2] - yrange[1] + 1, x = seq(yrange[1], yrange[2]))
	for(y in y_order){
		valid_ranges <- input_map |> rowwise() |> 
			summarise(get_closer_xrange(cur_data(), y)) |> 
			ungroup()
		# if(nrow(valid_ranges |> filter(x_min <= xrange[1], x_max >= xrange[2])) > 0) next;
		 # browser()
		valid_range <- valid_ranges |> arrange(x_min) |> 
			select(-y) 
		if(valid_range[[1]][1] > xrange[1]) return(c(0, y))
		current_max <- valid_range[[2]][1]
		for(i in seq(2, nrow(valid_range))){
			if(valid_range[[1]][i] > current_max)
				return(c(current_max+1, y))
			current_max <- max(current_max, valid_range[[2]][i])
			if(current_max >= xrange[2]) break;
		}
		if(current_max < xrange[2]){
			return(c(current_max + 1, y))
		}
		# 
		# browser()
		# valid_points <- input_map |> rowwise() |> 
		# 	summarise(get_closer_points2(cur_data(), y)) |> 
		# 	ungroup() |> 
		# 	distinct() |> 
		# 	filter(between(x, xrange[1], xrange[2]))
		# # browser()
		# if(nrow(valid_points) < xrange[2] - xrange[1] + 1){
		# 	#Find the point missing
		# 	missing_x <- which(!seq(xrange[1], xrange[2]) %in% valid_points$x)
		# 	return(c(seq(xrange[1], xrange[2])[missing_x], y))
		# }
		print(paste("Row", y, "checked"))
	}
}

yranges <- list(c(0, 999999), c(1000000, 1999999), c(2000000, 2999999), c(3000000, 3999999))
combined_missing_points <- parallel::mclapply(X = yranges, FUN=\(x){get_invalid_points(xrange = x, yrange = x)})
missing_point <- get_invalid_points(xrange = c(0, 4000000), yrange = c(0, 4000000))
# missing_point <- get_invalid_points()
sprintf("%1.0f", missing_point[1]* 4000000 + missing_point[2])
