# H2 Data Science
# UG308490
# L04 - modelsDistributions 




# R tip -------------------------------------------------------------------

# vector
location <- c("Oban", "Glencoe", "Glasgow")
population <- c(8140, 374, 626410)
years <- 1990:1997
odd_nums <- seq(1, 9, by=2)

# matrix
number_mx <- matrix(1:12, nrow=3, ncol=4)
letter_mx <- matrix(c("A", "B", "C", "D", "E", "F"), nrow=2)
combined_mx <- matrix(c("Oban", "Glencoe", "Glasgow", 8140, 374, 626410), ncol=2)
str(combined_mx)

# dataframe
city_df <- data.frame(location=c("Oban", "Glencoe", "Glasgow"),
                      population=c(8140, 374, 626410))
str(city_df)

# list
oban_ls <- list(name="Oban", 
                pop=8140,
                nearbyIsles=c("Kerrera", "Lismore", "Mull"))
str(oban_ls)

