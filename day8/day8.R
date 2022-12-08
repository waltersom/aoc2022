library(dplyr)

input_data <- readLines("day8/input.txt")
# 
# input_data <- c("30373",
# 								"25512",
# 								"65332",
# 								"33549",
# 								"35390")

tree_matrix <- input_data |> strsplit("") |> unlist() |> as.integer() |> matrix(ncol = length(input_data)) |> t()

# tree_matrix[1,]


#Indicate it each tree is seeable
seeable_vec <- function(height_vec, reverse=FALSE){
	if(reverse){
		height_vec <- rev(height_vec)
	}
	highest_until <- cummax(height_vec)
	can_see <- height_vec > c(-1,  highest_until[seq(length(height_vec)-1)])
	if(reverse){
		can_see <- rev(can_see)
	}
	can_see
}

#How far can each tree see?
distance_vec <- function(height_vec, reverse=FALSE){
	if(reverse){
		height_vec <- rev(height_vec)
	}
	#For each position, find the distance to the previous tree that is as high or higher?
	dist <- sapply(seq(length(height_vec)), FUN = function(i, x){if(i==1)return(0);t <- x[i]; s <- x[1:(i-1)];dist <- match(TRUE, rev(s>=t), nomatch = i-1)}, x = height_vec)
	if(reverse){
		dist <- rev(dist)
	}
	dist
}


#Need to transpose if margin = 1
from_left <- apply(X = tree_matrix, FUN = seeable_vec, MARGIN = 1) |> t()
from_right <- apply(X = tree_matrix, FUN = seeable_vec, MARGIN = 1, reverse = TRUE) |> t()
from_top <- apply(X = tree_matrix, FUN = seeable_vec, MARGIN = 2)
from_bottom <- apply(X = tree_matrix, FUN = seeable_vec, MARGIN = 2, reverse = TRUE)

from_all <- from_left | from_right | from_top | from_bottom

from_all |> apply(FUN=sum, MARGIN = c(1)) |> sum()



dist_from_left <- apply(X = tree_matrix, FUN = distance_vec, MARGIN = 1) |> t()
dist_from_right <- apply(X = tree_matrix, FUN = distance_vec, MARGIN = 1, reverse = TRUE) |> t()
dist_from_top <- apply(X = tree_matrix, FUN = distance_vec, MARGIN = 2)
dist_from_bottom <- apply(X = tree_matrix, FUN = distance_vec, MARGIN = 2, reverse = TRUE)

dist_from_all <- dist_from_left * dist_from_right * dist_from_top * dist_from_bottom

dist_from_all |> apply(FUN=max, MARGIN = c(1)) |> max()
