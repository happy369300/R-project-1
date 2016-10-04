library(quantmod)

###############################################################################
# GET DATA
###############################################################################

# Produces a list of xts objects corresponding to the training data
# Function is for online use
# @return List of xts objects in OHLCV format

getAllData  <- function() {
	cat("Reading all data\n")
	fileNames <- sapply(1:10, function(x) paste("DATA/FULL/",sprintf("%02d",x),".csv",sep='')) 
	print(fileNames)
	dat <- lapply(fileNames, function(x) as.xts(read.zoo(x, index.column = 1,
	                                                     header = TRUE, sep=',')))
	return(dat)
}

getData  <- function(part=1) {
	cat("Reading part",part,"of the data\n")
	fileNames <- sapply(1:10, function(x) paste("DATA/PART",part,"/",sprintf("%02d",x),".csv",sep='')) 
	dat <- lapply(fileNames, function(x) as.xts(read.zoo(x, index.column = 1,
	                                                     header = TRUE, sep=',')))
	return(dat)
}
###############################################################################
# Produces a list of xts objects using data(sample_matrix) from package::xts
# getSymbols()
# Function is for offline use and just for testing purposes
# @param nSeries Number of series in dataset (default: 3)
# @param nRows Number of rows in dataset (default: 100, MAX: 180)
# @return List of xts objects in OHLCV format

offlineData <- function(nSeries = 10, nRows = 180) {
	# inbuilt example data from xts package (no volume or adjusted columns)
	data(sample_matrix)

	sm <- as.xts(sample_matrix)

	# add volumes, which are chosen uniformaly at random from 1:maxVol
	maxVol <- 100
	volumes <- sample(1:maxVol, size = nrow(sm), replace = TRUE)
	sm <- cbind(sm, volumes)
	colnames(sm)[ncol(sm)] <- "Volume"

	t <- lapply(1:nSeries, function(x) sm[1:nRows,]*x)
	
	return(t)
}

###############################################################################
# Produces a list of xts objects with format returned by getSymbols()
# Function is for online use
# @param nSeries Number of series to fetch data for (default: 3, MAX: 10)
# @return List of xts objects in OHLCV format

onlineData <- function(nSeries = 10) {
	if (! is.element(nSeries,1:10))
		nSeries <- 10

	symbols <- c("XOM", "AAPL", "IBM", "CVX", "MSFT", "JNJ", "PG", "GOOG", "GE", "KO")
	symbols <- symbols[1:nSeries]
	getSymbols(symbols)
	dat <- lapply(symbols, get)

	# remove symbol name from colnames
	colNames <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
	for (i in 1: length(dat)) {
		colnames(dat[[i]]) <- colNames
		dat[[i]]$Adjusted <- NULL # useful for real data, but not present
		# in Training or Test data for Assessment 2
	}
	return(dat)
}
