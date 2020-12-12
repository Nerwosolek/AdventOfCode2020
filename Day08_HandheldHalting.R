execute <- function(address, program){
  if (program[address, ]$visited == TRUE) return
  else {
    program[address, ]$visited = TRUE
    switch (program[address,]$instr,
      nop = execute(address + 1, program),
      acc = {
        accumulator <<- accumulator + as.integer(program[address, ]$operand)
        execute(address + 1, program)
      },
      jmp = execute(address + as.integer(program[address, ]$operand), program)
    )
  }
}

f <- read_delim("input8.txt", delim = " ", col_types = "cc", col_names = c("instr","operand"))
f <- f %>% mutate(visited = FALSE)
i <- 1
accumulator <- 0
execute(i, f)
print(accumulator)
