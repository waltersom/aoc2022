library(dplyr)

input_data <- data.frame(energy = readLines("day1/input.txt"))

energy_per_elf <- input_data |> 
	mutate(elf_count = cumsum(energy == ""), energy = as.integer(energy)) |> 
	group_by(elf_count) |> 
	summarize(energy = sum(energy, na.rm = TRUE)) |> 
	arrange(desc(energy))

energy_top_elf <- energy_per_elf |> head(n=1) |> pull(energy)
energy_top_3 <- energy_per_elf |> 
	head(n=3) |> pull(energy) |> sum()
