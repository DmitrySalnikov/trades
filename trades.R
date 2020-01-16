#setwd('R/trades/')
#install.packages('hash')

library(hash)

# take only 1 and 4 columns
data <- read.csv('trades.csv', colClasses = 'character', header = FALSE)[,c(1,4)]
# function returns difference between timestamps in seconds
dif <- function(a, b) {
  as.numeric(difftime(strptime(a, format="%H:%M:%OS"), strptime(b, format="%H:%M:%OS"), units = 'secs'))
}
# number of rows
n <- nrow(data)
# begin of te window
start <- 1
# end of the window
end <- 1
# hash of current number of exchanges in the window
now <- hash()
now[[data[end, 2]]] = 1
# hash of max numbers of exchanges in the 1 minute window
res <- hash()
res[[data[end, 2]]] = 1
# start with 1 element in the window
# if next elements timestamp is greater less than 1 min than first timestamp then add related exchange to "now" hash and 
# if its value greater than related max value then renew max value
# else decrease related element of "now" hash.
while(end < n) {
  if (dif(data[end+1, 1], data[start, 1]) < 60) {
    end <- end + 1
    now[[data[end, 2]]] = if (is.null(now[[data[end, 2]]])) 1 else now[[data[end, 2]]] + 1
    if (is.null(res[[data[end, 2]]]) || res[[data[end, 2]]] < now[[data[end, 2]]]) {
      res[[data[end, 2]]] = now[[data[end, 2]]]
    }
  } else {
    now[[data[start, 2]]] = now[[data[start, 2]]] - 1
    start <- start + 1
  }
}

res
