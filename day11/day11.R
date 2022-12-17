library(dplyr)
library(stringr)
# library(gmp)
library(pracma)

input_data <- readLines("day11/input.txt")

monkey_index <- input_data[str_starts(input_data, "Monkey")] |> str_extract("\\d+") |> as.integer() + 1

#List, vector of items for each monkey
monkey_items <- input_data[str_detect(input_data, "Starting items:")] |> str_extract_all("\\d+") |> lapply(as.integer)

monkey_operation <- input_data[str_detect(input_data, "Operation:")] |> str_extract("new = .*")

monkey_true <- input_data[str_detect(input_data, "If true:")] |> str_extract("\\d+") |> as.integer() + 1
monkey_false <- input_data[str_detect(input_data, "If false:")] |> str_extract("\\d+") |> as.integer() + 1
#all are "divisible by"
test_conds <- input_data[str_detect(input_data, "Test")] |> str_extract("\\d+")

monkey_df <- data.frame(index = as.integer(monkey_index), op = monkey_operation, true = monkey_true, false = monkey_false, test = as.integer(test_conds), items_inspected = 0)

do_round <- function(monkey_df, start_item_allocation, divide_factor = 3){
	for(ind in monkey_df[["index"]]){
		
		items <- start_item_allocation[[ind]]
		for(ii in seq_along(items)){
			item <- items[[ii]]
			worry_level <- within(list(old = item), eval(parse(text = monkey_df[ind, "op"])))[["new"]]
			if(divide_factor != 1){
				worry_level <- floor(worry_level/divide_factor)
			}
			worry_level <- worry_level %% Reduce(Lcm, x = monkey_df$test)
			if(worry_level %% monkey_df[ind, "test"] == 0){
				#Give to true
				start_item_allocation[[monkey_df[ind, "true"]]] <- c(start_item_allocation[[monkey_df[ind, "true"]]], worry_level)
			}else{
				#give to false
				start_item_allocation[[monkey_df[ind, "false"]]] <- c(start_item_allocation[[monkey_df[ind, "false"]]], worry_level)
			}
			monkey_df[ind, "items_inspected"] <- monkey_df[ind, "items_inspected"] + 1
			
		}
		start_item_allocation[[ind]] <- integer(0)
	}
	return(list(monkey_df = monkey_df, start_item_allocation = start_item_allocation))
}

do_rounds <- function(monkey_df, start_item_allocation, n=1, divide_factor = 3){
	l <- list(monkey_df = monkey_df, start_item_allocation = start_item_allocation)
	for(i in seq(n)){
		l <- do_round(l[[1]], l[[2]], divide_factor)
	}
	l
}


ans <- do_rounds(monkey_df, monkey_items, n=20)
ans[["monkey_df"]] |> pull(items_inspected) |> sort() |> tail(n=2) |> as.list() |> do.call(what = `*`, args = _)
						

ans2 <- do_rounds(monkey_df, monkey_items, n=10000, divide_factor = 1)
ans2[["monkey_df"]] |> pull(items_inspected) |> sort() |> tail(n=2) |> as.list() |> do.call(what = `*`, args = _)
