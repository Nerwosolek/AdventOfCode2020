execute <- function(address, program){
  if (program[address, 3] == TRUE || address > nrow(program)) return(address)
  else {
    program[address, 3] = TRUE
    a=program[address,]$instr
    switch (a,
            nop = execute(address + 1, program),
            acc = {
              accumulator <<- accumulator + as.integer(program[address, 2])
              execute(address + 1, program)
            },
            jmp = execute(address + as.integer(program[address, 2]), program)
    )
  }
}

f <- read_delim("input8.txt", delim = " ", col_types = "cc", col_names = c("instr","operand"))
f <- f %>% mutate(visited = FALSE)
i <- 1
accumulator <- 0
nops <- which(f[,1] == "nop")
for (address in nops) {
  t <- f
  t[address,1] = "jmp"
  accumulator <- 0
  last_address <- execute(i, t)
  if (last_address > nrow(f)){
    print(accumulator)
    break
  }
}
accumulator <- 0
jmps <- which(f[,1] == "jmp")
for (address in jmps) {
  t <- f
  t[address,1] = "nop"
  accumulator <- 0
  last_address <- execute(i, t)
  if (last_address > nrow(f)){
    print(accumulator)
    break
  }
}
