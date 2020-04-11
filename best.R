## Final Project Part 2
## Best Hopital Function

best <- function(state, outcome) {
    outcome <- tolower(outcome)  
    state <- toupper(state)
    
    import <- read_csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")
    data <- as.data.frame(cbind(import[,2], #name
                                import[,7], #state
                                import[,11], #heart attack
                                import[,17], #heart failure
                                import[,23], #pneumonia
                                stringsAsFactors =FALSE))
    colnames(data) <- c("name", "states", "heart attack", "heart failure", "pneumonia")
    unique_states <- unique(data$states)
    
    #Invalid Messages
    if (!state %in% unique_states) {
      stop('Invalid State')
    }
    if (!outcome %in% c("heart attack","heart failure","pneumonia")) {
      stop('Invalid Outcome')
    }
    
    #Find Best Hospital for Outcome
    state_match <- which(data$state==state)
    relev_states <- data[state_match, ]
    ratings <- order(as.numeric(relev_states[, outcome]))
    first <- ratings[1]
    output <- relev_states[first,'name']
    return(output)
}