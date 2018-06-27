rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        outcomedata <- read.csv("outcome-of-care-measures.csv",
                                colClasses = "character",
                                na.strings = c("Not Available"))
        
        ## Check that state and outcome are valid
        if(sum(outcomedata$State == state) == 0) stop("invalid state")
        if(outcome != "heart attack" &
           outcome != "heart failure" &
           outcome != "pneumonia") stop("invalid outcome")
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        # column index
        labelindex <- c(
                "heart attack" = match("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", names(outcomedata)),
                "heart failure" = match("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", names(outcomedata)),
                "pneumonia" = match("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia", names(outcomedata))
        )
        icol = labelindex[outcome]
        
        # tidy data processing
        outcomedata[, icol] <- as.numeric(outcomedata[, icol])
        
        # order by name
        sorteddata <- outcomedata[order(outcomedata[, icol], outcomedata$Hospital.Name), ]
        
        # extract by state
        rate <- sorteddata[sorteddata$State == state & !is.na(sorteddata[, icol]), ]
        raterange <- NULL
        if(num == "best") {
                raterange <- 1
        } else if(num == "worst") {
                raterange <- nrow(rate)
        } else {
                raterange <- num
        }
        rate[raterange, ]$Hospital.Name
}

