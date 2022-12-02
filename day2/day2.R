library(dplyr)
library(purrr)
input_data <- data.frame(read.table("day2/input.txt", col.names = c("p1", "p2")))

get_score <- function(p1, p2){
	#p1 - A rock, B paper, C scissors
	#p2 - X rock, Y paper, Z scissors
	loss <- 0
	draw <- 3
	win <- 6
	choice_map <- list(A = 1, B = 2, C = 3, X = 1, Y = 2, Z = 3)
	result_list <- list(c(draw, loss, win), c(win, draw, loss), c(loss, win, draw))
	p1 <- unlist(choice_map[p1], use.names = FALSE)
	p2 <- unlist(choice_map[p2], use.names = FALSE)

	map2_dfr(.f = function(x, y){
		x_score <- result_list[[x]][[y]]
		y_score <- win-x_score
		return(data.frame(p1 = x + x_score, p2 = y + y_score))
		}, .x = p1, .y = p2)
}

get_move <- function(p1, p2){
	#p1 - A rock, B paper, C scissors
	#p2 - X lose, Y draw, Z win
	loss <- 0
	draw <- 3
	win <- 6
	choice_map <- list(A = 1, B = 2, C = 3, X = loss, Y = draw, Z = win)
	
	result_list <- list(c(draw, loss, win), c(win, draw, loss), c(loss, win, draw))
	p1 <- unlist(choice_map[p1], use.names = FALSE)
	p2 <- unlist(choice_map[p2], use.names = FALSE)
	
	#work out what p2 should be; given p1, and the required value
	#that is, match(result_list[p1], choice_map[p2]) or something
	
	map2_int(.x = p1, .y = p2, 
					 .f = function(x, y){
					 	#get required move - note win - result_list[[x]] is because the result_list gives score for p1, we want for p2
					 	match(y, win - result_list[[x]])
					 }
	)
}


round_scores <- get_score(input_data$p1, input_data$p2)


total_score <- round_scores |> summarise(across(.cols = everything(), .fns = sum))

round_scores_test <- get_score(c("A", "B", "C"), c("Y", "X", "Z"))

total_score_test <- round_scores_test |> summarise(across(.cols = everything(), .fns = sum))

new_moves_test <- get_move(c("A", "B", "C"), c("Y", "X", "Z"))
new_round_scores_test <- get_score(c("A", "B", "C"), new_moves_test)
new_total_score_test <- new_round_scores_test |> summarise(across(.cols = everything(), .fns = sum))


new_p2_moves <- get_move(input_data$p1, input_data$p2)

new_round_scores <- get_score(input_data$p1, new_p2_moves)

new_total_score <- new_round_scores |> summarise(across(.cols = everything(), .fns = sum))
