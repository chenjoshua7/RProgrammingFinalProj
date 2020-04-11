##Final Project


#Part 1: Graphing the heart attack mortality rates.
#Import datasets
library(readr)
outcome<- read_csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")
View(outcome)

#Basic R Histogram
ha_mortality <- as.numeric(unlist(outcome[, 11]))
hist(ha_mortality)

#ggplot2 R Histogram
ha_mortality <- as.data.frame(outcome[, 11])
colnames(ha_mortality) <- "death_rate"
ha_mortality$death_rate <- as.numeric(ha_mortality$death_rate)

library(ggplot2)
ggplot(data = ha_mortality) +
geom_histogram(mapping = aes(x = ha_mortality$death_rate, color = ha_mortality$death_rate), stat = "count", na.rm = TRUE, color = "black", fill = "red") +
labs(title = "Hospital Count by Heart Mortality Rates", x = "Heart Attack Mortality Rate", y = "Total Count") +
theme(plot.title = element_text(hjust = 0.5))

hospital_data <- read_csv("rprog_data_ProgAssignment3-data/hospital-data.csv")
View(hospital_data)
