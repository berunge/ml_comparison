#Create dataframe of outcomes for the plan

create_outcomes <- function(){

  outcome_frame <- tribble(~outcome, ~levels,
                           "asthma" , "yes",
                           "lactose", "yes")

  return(outcome_frame)

}
