#rankhospital("MD", "heart failure", 5)
rankhospital <- function(state, outcome, num = "best") {
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
    data<-data[!is.na(data[deathrate]),]
    
    n<-nrow(data)
    
    deathraterank<-rank(data[deathrate])
    namerank<-rank(data[["Hospital.Name"]])/n/n
    data["mainrank"]<-deathraterank+namerank
    #data["order"]<-order(data[["mainrank"]])
    data<-data[order(data[["mainrank"]]),]
    data["order"]<-order(data[["mainrank"]])
    
    if(num=="best")
        num <- 1
    if(num=="worst")
        num <- n
    
    if(num>=1 && num<=n) {
        besthospital<-data[data["order"]==num,]
        besthospital[["Hospital.Name"]]            
    }
    else
        NA
    
}