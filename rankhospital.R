#rankhospital("MD", "heart failure", 5)
rankhospital <- function(state, outcome, num) {
    ## Read outcome data
    measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state is valid
    if(is.na(match(state,unique(measures[["State"]]))))
        stop("invalid state")    
    
    ## Check that outcome is valid     
    if(is.na(match(outcome,c("heart attack", "heart failure", "pneumonia"))))
        stop("invalid outcome")    
    
    ## define what is the relevant column for the death rate
    deathrate<-switch(outcome,
                      "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                      "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                      "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    
    
}