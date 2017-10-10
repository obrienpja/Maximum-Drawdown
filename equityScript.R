library(quantmod)

# Generate string for file name
skews <- c(-10:-2, 2:10)
fileName <- readline(prompt="Enter .csv filename: ")
directoryName <- paste(gsub('.csv', '', fileName), sep='')
allData <- read.csv(fileName, header = FALSE)

# Take percentage changes and number of business days, then create equity curve
equityCurve <- function(changes, numDays)
{
	randInd <- sample.int(length(changes), numDays, replace=TRUE)
	return(cumsum(changes[c(randInd)]))
}

# Takes a list of peak indices and returns a list of peak indices with monotonically increasing peak heights
increasingPeaks <- function(equitydata)
{
	peakIndices <- findPeaks(equitydata)-1
	maxima <- c()
  maxVal <- -1000000

  if((equitydata[1] - equitydata[2]) > 0)
	{
  	maxima <- c(maxima,1)
    maxVal <- equitydata[1]
  }

  for(i in peakIndices)
  {
  	if(equitydata[i] > maxVal)
		{
      maxima <- c(maxima, i)
      maxVal <- equitydata[i]
    }
  }

	return(maxima)
}

# Takes a list of increasing peak indices as well as the total valley list and returns max drawdown
findMaxDrawdown <- function(equitydata)
{
	peaks <- increasingPeaks(equitydata)
	valleys <- findValleys(equitydata)-1
  drawList <- c()

  if(length(peaks) == 1)
  {
	  sliced2 <- equitydata[peaks:length(equitydata)]
    return(equitydata[peaks] - min(sliced2))
  }
  else if(length(peaks) == 0)
  {
    return(0)
  }
  else
  {
    for(i in 1:(length(peaks)-1))
    {
      drawList <- c(drawList, equitydata[peaks[i]] - min(equitydata[c(valleys[(peaks[i] < valleys & valleys < peaks[i+1])])]))
    }

		sliced <- equitydata[last(peaks):length(equitydata)]
    drawList <- c(drawList, equitydata[last(peaks)] - min(sliced))

		return(max(drawList))
  }
}

# Get percentile value from a sorted list
percentile <- function(drawdownData, per, nIterations)
{
	return(drawdownData[as.integer(per*nIterations)])
}

# Generate filename to output data
genFilename <- function(skewVal, days, nIterations)
{
	paste(directoryName, "/Data/", skewVal, "skew_", days, "days_", nIterations, "iterations", sep="")
}

# Run simulation for a dataset
runSimulation <- function(dats, nIterations, filename, days)
{
	allMaxDrawdowns <- c()

	for(i in 1:nIterations)
	{
		equities <- equityCurve(dats, days)
    allMaxDrawdowns <- c(allMaxDrawdowns, findMaxDrawdown(equities))
	}

	sorted <- sort(allMaxDrawdowns, decreasing=TRUE)
	write(sorted, file = filename, sep = '\t')
	return(sorted)
}

worstPossibleCases <- function(days)
{
  allWorsts <- c()

	for(i in 1:length(skews))
  {
    start <- c(as.matrix(allData[i]))
    allWorsts <- c(allWorsts, -days*min(c(start[!is.na(start)])))
  }

	return(allWorsts)
}

plotFunction <- function(typeOfOutput, days, nIterations, dataset, plotTitle, yLabel)
{
	pdf(paste(directoryName,"/Plots/", typeOfOutput, "_", days, "days_", nIterations, "iterations", ".pdf", sep=''))
	plot(skews, dataset, main = paste(plotTitle, days, "days and", nIterations, "iterations"), xlab='Skew', ylab=yLabel)
	lines(skews, worstPossibleCases(days))
	dev.off()
}

printAllFiles <- function(days, nIterations, startIndex)
{
	worstDrawdown <- c()
	fifthPercentDrawdown <- c()
	firstPercentDrawdown <- c()
	medianDrawdown <- c()
	averageDrawdown <- c()

	for(i in startIndex:length(skews))
	{
		changesData <- c(as.matrix(allData[i]))
		changesData <- changesData[!is.na(changesData)]
		sortedDrawdowns <- runSimulation(changesData, nIterations, genFilename(skews[i], days, nIterations), days)
		worstDrawdown <- c(worstDrawdown, sortedDrawdowns[1])
		fifthPercentDrawdown <- c(fifthPercentDrawdown, percentile(sortedDrawdowns, .05, nIterations))
		firstPercentDrawdown <- c(firstPercentDrawdown, percentile(sortedDrawdowns, .01, nIterations))
		medianDrawdown <- c(medianDrawdown, percentile(sortedDrawdowns, .5, nIterations))
		averageDrawdown <- c(averageDrawdown, mean(sortedDrawdowns))
	}

	plotFunction('worstDrawdown', days, nIterations, worstDrawdown, 'Worst Case Drawdown for ', 'Worst Case Drawdown')
	plotFunction('fifthPercentDrawdown', days, nIterations, fifthPercentDrawdown, 'Fifth Percentile Drawdown for ', 'Fifth Percentile Drawdown')
	plotFunction('firstPercentDrawdown', days, nIterations, firstPercentDrawdown, 'First Percentile Drawdown for ', 'First Percentile Drawdown')
	plotFunction('medianDrawdown', days, nIterations, medianDrawdown, 'Median Drawdown for ', 'Median Drawdown')
	plotFunction('averageDrawdown', days, nIterations, averageDrawdown, 'Average Drawdown for ', 'Average Drawdown')
}

if(!dir.exists(directoryName))
{
	dir.create(directoryName)
	dir.create(paste(directoryName, '/Data/', sep=""))
	dir.create(paste(directoryName, '/Plots/', sep=""))

	for(day in c(22,250))
	{
  	printAllFiles(day, 10^6, 1)
	}
}
