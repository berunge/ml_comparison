#Create dataframe of outcomes for the plan

create_outcomes <- function(){

  outcome_frame <- tribble(~outcome, ~levels, ~null_levels,
                           "ibs" , "Diagnosed by a medical professional (doctor, physician assistant)", "Unknown")

  return(outcome_frame)

}
