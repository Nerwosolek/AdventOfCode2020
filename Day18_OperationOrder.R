library(readr)
f <- scan("input18.txt", what = character(), sep="\n")
evaluate <- function(operator, op1, op2) {
  if (operator == "*") return (op1 * op2)
  if (operator == "+") return (op1 + op2)
}

precedence <- function (operator) {
  if (operator == "(") return(0)
  else if(operator == "*") return(1)
  else return(2)
}
results <- vector(mode="integer")
for (v in f) {
  formula <- unlist(str_split(str_split(v, " ")[[1]],""))
  operators <- vector(mode="character")
  operands <- vector(mode="double")
  
  for (token in formula) {
    if (str_detect(token, "\\(")) operators <- append(operators, token)
    else if (str_detect(token, "[0-9]")) operands <- append(operands, as.double(token))
    else if (str_detect(token, "\\)")) { #solve brace
      while(length(operators) > 0 && operators[length(operators)] != "(") {
        v2 <- operands[length(operands)]
        operands <- operands[-length(operands)]
        v1 <- operands[length(operands)]
        operands <- operands[-length(operands)]
        op <- operators[length(operators)]
        operators <- operators[-length(operators)]
        operands <- append(operands, evaluate(op, v1, v2))
      }
      if (length(operators) > 0) operators <- operators[-length(operators)] #pop (
    }
    else {
      while (length(operators) > 0 && precedence(operators[length(operators)]) >= 
             precedence(token)) {
        v2 <- operands[length(operands)]
        operands <- operands[-length(operands)]
        v1 <- operands[length(operands)]
        operands <- operands[-length(operands)]
        op <- operators[length(operators)]
        operators <- operators[-length(operators)]
        operands <- append(operands, evaluate(op, v1, v2))
      }
      operators <- append(operators, token)
    }
  }
  
  while (length(operators) > 0) {
    v2 <- operands[length(operands)]
    operands <- operands[-length(operands)]
    v1 <- operands[length(operands)]
    operands <- operands[-length(operands)]
    op <- operators[length(operators)]
    operators <- operators[-length(operators)]
    operands <- append(operands, evaluate(op, v1, v2))
  }
  
  results <- append(results, (operands[1]))
}

