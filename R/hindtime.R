hindtime <-
function(alldata){
	dat <- alldata[, c("Title", grep(names(alldata), pattern="[1|2][0-9][0-9][0-9]", value=TRUE))]
}
