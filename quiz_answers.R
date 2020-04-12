##Assignment 3 Quiz Answers

source("best.R")
##1. What result is returned by the following code?
best("SC", "heart attack")
#"MUSC MEDICAL CENTER"

##2. What result is returned by the following code?
best("NY", "pneumonia")
#"MAIMONIDES MEDICAL CENTER"

##3. What result is returned by the following code?
best("AK", "pneumonia")
#"YUKON KUSKOKWIM DELTA REG HOSPITAL"


source("hospital.R")
##4. What result is returned by the following code?
rankhospital("NC", "heart attack", "worst")
#"WAYNE MEMORIAL HOSPITAL"

##5. What result is returned by the following code?
rankhospital("WA", "heart attack", 7)
#"YAKIMA VALLEY MEMORIAL HOSPITAL"

##6. What result is returned by the following code?
rankhospital("TX", "pneumonia", 10)
#"SETON SMITHVILLE REGIONAL HOSPITAL"

##7. What result is returned by the following code?
rankhospital("NY", "heart attack", 7)
#"BELLEVUE HOSPITAL CENTER"

source(rankall.R)
##8. What result is returned by the following code?
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)
#"CASTLE MEDICAL CENTER"

##9. What result is returned by the following code?
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)
#"BERGEN REGIONAL MEDICAL CENTER"

##10. What result is returned by the following code?
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
#"RENOWN SOUTH MEADOWS MEDICAL CENTER"