rankall <- function(outcome, num = "best") {
    ## Read outcome data
    measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
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
    textdata<-measures[,c("State","Hospital.Name",deathrate)]
    
    ## take the numerical value of the column
    data<-textdata[,c("State","Hospital.Name")]
    data[deathrate]<-suppressWarnings(as.numeric(textdata[[deathrate]]))
    data<-data[!is.na(data[deathrate]),]
    
    states<-data.frame()
    for(s in sort(unique(data[["State"]])))
    {
        sdata<-data[data[["State"]]==s,]
        n<-nrow(sdata)
        
        deathraterank<-rank(sdata[deathrate])
        namerank<-rank(sdata[["Hospital.Name"]])/n/n
        sdata["mainrank"]<-deathraterank+namerank
        #data["order"]<-order(data[["mainrank"]])
        sdata<-sdata[order(sdata[["mainrank"]]),]
        sdata["order"]<-order(sdata[["mainrank"]])
        
        if(num=="best")
            pick <- 1
        else if(num=="worst")
            pick <- n
        else pick<-num
        
        if(pick>=1 & pick<=n) {
            besthospital<-sdata[sdata["order"]==pick,]
            result<-besthospital[["Hospital.Name"]]            
        }
        else
            result<-NA
        states<-rbind(states,data.frame(hospital=result, state=s))
    }
    states 
    
}