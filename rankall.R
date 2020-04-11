rankall <- function(outcome, num = 'best') {
    source("rankhospital.R")
    
    outcome <- tolower(outcome)
    
    #import dataset
    import <- read_csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")
    data <- as.data.frame(cbind(import[,2], #name
                                import[,7], #state
                                stringsAsFactors =FALSE))

    colnames(data) <- c("name", "states")
    unique_states <- unique(data$states)
    answer <- data.frame(matrix(nrow = 0, ncol = 2))
    
    for (x in unique_states) {
        result <- rankhospital(state = x, outcome = outcome, num = num)
        addition <- matrix(c(result, x), nrow = 1, ncol = 2)
        answer <- rbind(answer,addition)
    }
    return(answer)
}