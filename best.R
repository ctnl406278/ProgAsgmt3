#The "best.R" function determines the hospital that has the best (lowest) 30-day
#mortality for a specified outcome in that state. The current outcomes being
#assessed are (1) "heart attack", (2) "heart failure" and (3) "pneumonia".

#Outcome.sub
#col1 - Mortality.Rates
#col2 - Hospital.Name
#col3 - State

#State.sub
#col1 - Mortality.Rates
#col2 - Hospital.name

best <<- function(state, outcome) {
    
    outcome_data <<-
        as.data.frame(x = read.csv(file = "outcome-of-care-measures.csv",
                                   as.is = TRUE))                            #read outcome data.
    states <<- factor(x = outcome_data$State)
    outcomes <<- c("heart attack", "heart failure", "pneumonia")
    
    i = 1
    while (i != nlevels(states)) {
        if (levels(states)[i] == state) {
            invalidState = FALSE
            message("Succesfull STATE input.")
            break
        }
        else {
            invalidState = TRUE
        }
        i = i + 1
    }
    
    i = 1
    while (i != length(outcomes)) {
        if (outcomes[i] == outcome) {
            invalidOutcome = FALSE
            message("Succesfull OUTCOME input.")
            break
        }
        else {
            invalidOutcome = TRUE
        }
        i = i + 1
    }
    
    if (invalidState == TRUE) {
        return(message(
            "Error in best('",state ,"','",outcome,"') : invalid state")
        )
    }
    else if(invalidOutcome == TRUE) {
        return(message(
            "Error in best('",state ,"','",outcome,"') : invalid outcome")
                )
    }
}
