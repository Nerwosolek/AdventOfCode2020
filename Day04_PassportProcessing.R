library(tidyr)
library(dplyr)
library(stringr)
valid_field <- function (name, value) {
  if (name == "byr") {
    if (nchar(value) != 4) return(FALSE)
    byr <- as.integer(value)
    if (byr < 1920 | byr > 2002) return(FALSE)
    return(TRUE)
  } else if (name == "iyr") {
    if (nchar(value) != 4) return(FALSE)
    byr <- as.integer(value)
    if (byr < 2010 | byr > 2020) return(FALSE)
    return(TRUE)
  } else if (name == "eyr"){
    if (nchar(value) != 4) return(FALSE)
    byr <- as.integer(value)
    if (byr < 2020 | byr > 2030) return(FALSE)
    return(TRUE)
  } else if (name == "hgt"){
    unit <- substr(value, nchar(value)-1,nchar(value))
    if (unit %in% c("cm","in")){
      hgt <- as.integer(substr(value,1,nchar(value)-2))
      if (unit == "cm" & hgt >= 150 & hgt <= 193) return (TRUE)
      if (unit == "in" & hgt >= 59 & hgt <= 76) return (TRUE)
    }
    return(FALSE)
  } else if (name == "hcl"){
    return(str_detect(value,"^#[a-f,\\d]{6}$"))
  } else if (name == "ecl"){
    return (value %in% c("amb","blu", "brn", "gry", "grn", "hzl", "oth"))
  } else if (name == "pid") {
    return (str_detect(value,"^\\d{9}$"))
  }
}

t  <- scan("input4.txt", what = character(), multi.line = TRUE, blank.lines.skip = FALSE)
count = 0;
all_count = 0;
valid_fields = c("byr","iyr","eyr","hgt","hcl","ecl","pid")

for (v in t) {
  #print(substr(v,1,3))
  if (substr(v,1,3) %in% valid_fields && valid_field(substr(v,1,3), substr(v, 5, nchar(v))))
  {
    count = count + 1
  }
  if (v == "")
  {
    if (count == 7) all_count = all_count + 1
    count = 0
  }
}
print (all_count)