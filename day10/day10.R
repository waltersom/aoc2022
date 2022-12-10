library(dplyr)
library(tidyr)

input_data <- readLines("day10/input.txt")

process_data <- function(d){
	lapply(X=strsplit(d, " "), FUN = \(x){data.frame(op = x[[1]], val = as.integer(ifelse(length(x) == 2, x[[2]], 0)))}) |> do.call(what = rbind)
}


df <- process_data(input_data) |> mutate(cycle_count = case_when(op == "noop"~1, TRUE~2),
																	 cycle = cumsum(cycle_count),
																	 start_cycle = cycle - cycle_count + 1,
																	 after_val = cumsum(val)+1, during_val = after_val - val) |> 
	rowwise() |> 
	mutate(covered_cycles = list(seq(start_cycle, cycle))) |> 
	ungroup()

target_cycles <- c(20, 60, 100, 140, 180, 220)

df |> rowwise() |> 
	filter(length(intersect(!!target_cycles, covered_cycles)) > 0) |> 
	ungroup() |> 
	cbind(target_cycles) |> 
	summarise(sum(during_val*target_cycles))


df
#Not entirely sure why I need this first row, an off-by-one error somewhere...
tibble::tibble(op = "noop", val = 1, cycle_count = 0, cycle = 1, start_cycle = 1, after_val = 1, during_val = 1, covered_cycles = list(c(0))) |> 
rbind(df) |> 
	unnest(covered_cycles) |> 
	mutate(after_val = case_when(cycle == covered_cycles~after_val, TRUE~during_val)) |> 
	rowwise() |> 
	mutate(row = (covered_cycles) %/% 40,
				 row_pos = covered_cycles - row * 40,
				 should_draw = row_pos %in% (c(-1, 0, 1) + after_val)) |> 
	mutate(char = case_when(should_draw~"*", TRUE~" "), ) |> 
	ungroup() |> 
	group_by(row) |> 
	summarise(pic = paste0(char, sep = "", collapse = ""))
	
