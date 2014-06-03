## this function returns the name of the best hospital from the "outcome-of-care-measures.csv" file,
## using as criteria a specific state and outcome
## the outcome can only be "heart attack", "heart failure" or "pneumonia"
## the state must be a valid state
## the best hospital will be the one with the lowest 30-day mortality rate
## in the specified state and for the specific chosen outcome
## if more than one hospital has the same best value, the first will be chosen alphabetically
best <- function(state, outcome) {
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
    
    ## filter only the rows related to the required state
    ## and the columns of the hospital name and death rate
    textdata<-measures[measures["State"]==state,c("State","Hospital.Name",deathrate)]
    
    ## take the numerical value of the column
    data<-textdata[,c("State","Hospital.Name")]
    data[deathrate]<-suppressWarnings(as.numeric(textdata[[deathrate]]))
    
    ## get the best rate in the state (aggregate with the formula will omit the NAs )
    bestrate<-aggregate(as.formula(paste(deathrate,"State",sep="~")),data,min)
        
    ## retrieve the rows which correspond to the best death rate
    besthospitals<-merge(bestrate,data)    
    
    ## Return hospital name in that state with lowest 30-day death rate    
    ## it must be the minimum name, i.e. the first in alphabetical order    
    min(besthospitals[["Hospital.Name"]])
}

#some test cases
testbest<- function(){
    state<-"TX"
    outcome <- "heart attack"
    expected<- "CYPRESS FAIRBANKS MEDICAL CENTER"
    result<-best(state,outcome)
    if(result!=expected)
    {
        message<-paste("Error: best(\"",state,"\", \"",outcome,"\") returns \"",result,"\". expected return:\"",expected,"\".")
        stop(message)
    }          
    state<-"TX"
    outcome <- "heart failure"
    expected<- "FORT DUNCAN MEDICAL CENTER"
    result<-best(state,outcome)
    if(result!=expected)
    {
        message<-paste("Error: best(\"",state,"\", \"",outcome,"\") returns \"",result,"\". expected return:\"",expected,"\".")
        stop(message)
    }          
    
    state<-"MD"
    outcome <- "heart attack"
    expected<- "JOHNS HOPKINS HOSPITAL, THE"
    result<-best(state,outcome)
    if(result!=expected)
    {
        message<-paste("Error: best(\"",state,"\", \"",outcome,"\") returns \"",result,"\". expected return:\"",expected,"\".")
        stop(message)
    }          
    
    state<-"MD"
    outcome <- "pneumonia"
    expected<- "GREATER BALTIMORE MEDICAL CENTER"
    result<-best(state,outcome)
    if(result!=expected)
    {
        message<-paste("Error: best(\"",state,"\", \"",outcome,"\") returns \"",result,"\". expected return:\"",expected,"\".")
        stop(message)
    } 
    
    "successful test"
    
}