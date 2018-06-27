rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcomedata <- read.csv("outcome-of-care-measures.csv",
                                colClasses = "character",
                                na.strings = c("Not Available"))
        
        ## Check that state and outcome are valid
        if(outcome != "heart attack" &
           outcome != "heart failure" &
           outcome != "pneumonia") stop("invalid outcome")
        
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
        
        ## For each state, find the hospital of the given rank
        states = unique((outcomedata[order(outcomedata$State), ])$State)
        rankall <- NULL
        for(state in states) {
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
                rankall <- rbind(rankall, c(rate[raterange, ]$Hospital.Name, state))
        }
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        colnames(rankall) <- c("hospital", "state")
        rownames(rankall) <- states
        
        data.frame(rankall)
}