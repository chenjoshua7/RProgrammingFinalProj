##Part 2: Rank Hospital Function


rankhospital <- function(state, outcome, num = "best") {
    outcome <- tolower(outcome)
    state <- toupper(state)
    
    #import dataset
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

    #Create filtered list for desired state
    state_match <- which(data$state==state)
    relev_states <- data[state_match, ]
    relev_states <- relev_states[!is.na(as.numeric(relev_states[, outcome])), ]
    
    #organize by outcome and add rank column
    list <- as.numeric(relev_states[, outcome])
    ratings <- rank(list, ties.method = "last")
    n <- length(ratings)
    x <- 1
    ordered_data<-as.data.frame(matrix(nrow = n, ncol = 6))
    
    for (i in ratings) {
        ordered_data[i, ] <- relev_states[x, ]
        x <- x + 1
    }
    
    ordered_data <- cbind(ordered_data, 1:n)
    colnames(ordered_data) <- c(colnames(relev_states), "Rank")
    
    #calling by rank, including error message
    if (num == "best") {
        num <- 1
    }
    if (num == "worst") {
        num <- n
    }

    output <- ordered_data[num,'name']
    return(output)
}
