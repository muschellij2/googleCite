ss <-
function(x, pattern, slot=1,...) {
	sapply(strsplit(x,pattern,...), function(x) x[slot])
}
