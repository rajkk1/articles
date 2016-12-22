### Load Libraries
library("xlsx")
library("ggplot2")
library("gridExtra")
library("plyr")

### Constants
years <- c("2008","2012","2016")
# From http://fivethirtyeight.com/features/the-odds-of-an-electoral-college-popular-vote-split-are-increasing/
states <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado",
            "Connecticut","Delaware","District of Columbia","Florida","Georgia",
            "Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky",
            "Louisiana","Maine","Maryland","Massachusetts","Minnesota",
            "Mississippi","Missouri","Montana","Nebraska","Nevada",
            "New Hampshire","New Jersey","New Mexico","New York",
            "North Carolina","North Dakota","Ohio","Oklahoma","Oregon",
            "Pennsylvania","Rhode Island","South Carolina","South Dakota",
            "Tennessee","Texas","Utah","Vermont","Virginia","Washington",
            "West Virginia","Wisconsin","Wyoming")
swingStates <- c("Colorado","Florida","Iowa","Michigan","Nevada",
                 "New Hampshire","North Carolina","Ohio","Pennsylvania",
                 "Virginia","Wisconsin","Arizona","Georgia","Maine","Utah")

### Functions

##Not in
#Taken from http://stackoverflow.com/questions/5831794/opposite-of-in
'%!in%' <- function(x,y)!('%in%'(x,y))

## Make results table from results xlsx file
cleanResults <-function(resultsFile,sheetNo){
    results <- read.xlsx2(resultsFile,sheetNo,header=FALSE)
    results <- results[,c(1,4,7)]
    results[,2] <- as.numeric(sub("%", "", results[,2]))
    results[,3] <- as.numeric(sub("%", "", results[,3]))
    results[,4] <- results[,3]*100 - results[,2]*100
    names(results) <- c("state","dem_perc","rep_perc","diff")
    return(results)
}

## Make polls table from polls CSV file
cleanPollData <- function(pollFile,results){
    polls <- read.csv(pollFile,header=FALSE)
    if (ncol(polls)==5) {
        names(polls) <- c("state","poll","date","dem_perc","rep_perc")
    } else {
        names(polls) <- c("state","poll","date","year","dem_perc","rep_perc")
    }
    polls$state <- sapply(polls$state, as.character)
    polls$dem_perc <- as.numeric(sub("%", "", polls$dem_perc))
    polls$rep_perc <- as.numeric(sub("%", "", polls$rep_perc))
    polls$diff <- polls$rep_perc - polls$dem_perc
    for (i in 1:length(polls$state)){
        polls$diff_pred[i] <- polls$diff[i] - results$diff[results$state==polls$state[i]]
    }
    missingStates <- states[states %!in% polls$state]
    stateRow <- data.frame(state=missingStates)
    if (length(stateRow) > 0){
        polls <- rbind.fill(polls,stateRow)
    }
    return(polls)
}

## Make plot showing poll results
plotPoll <- function(polls){
    pollsMean <- aggregate(polls$diff_pred, list(polls$state), mean)
    names(pollsMean) <- c("state","diff_pred")
    g <- ggplot(polls, aes(x=diff_pred,y=state,color=diff_pred)) +
        geom_point() +
        scale_colour_gradient2(low="blue", high="red",limits=c(-10,10)) +
        geom_point(data = pollsMean,color="black",shape=16) +
        xlim(c(-20,20)) +
        labs(y="",x="")+
        theme(panel.border = element_rect(fill=NA))
    return(g)
}

## Remove axis and legend from poll plot
stripPollPlot <- function(pollPlot){
    barePlot <- pollPlot +
        guides(color=FALSE) +
        theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
}

## Extract legend from plot
## Taken from https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

## Extract axis from plot
## Modified from https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
## To extract y-axis, put axNum=1
## To extract x-axis, put axNum=2
g_axis<-function(a.gplot,axNum){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    ax <- which(startsWith(sapply(tmp$grobs, function(x) x$name),"GRID.absoluteGrob"))
    axis <- tmp$grobs[[ax[axNum]]]
    return(axis)
}

## Calculate mean of states in polls
calcPollsMean <- function(polls){
    polls <- polls[complete.cases(polls),]
    pollsMean <- aggregate(polls$diff_pred, list(polls$state), mean)
    names(pollsMean) <- c("state","diff_pred")
    mean(pollsMean$diff_pred)
}

## Calculate mean squared error of states in polls
calcPollsMSE <- function(polls){
    polls <- polls[complete.cases(polls),]
    pollsMSE <- aggregate(polls$diff_pred, list(polls$state), function(x) sum(x^2)/length(x))
    names(pollsMSE) <- c("state","diff_predMSE")
    mean(pollsMSE$diff_predMSE)
}
## Calculate mean of swing states in polls
calcPollsSwingMean <- function(polls){
    pollsSwing <- polls[polls$state %in% swingStates,]
    calcPollsMean(pollsSwing)
}

## Calculate MSE of swing states in polls
calcPollsSwingMSE <- function(polls){
    pollsSwing <- polls[polls$state %in% swingStates,]
    calcPollsMSE(pollsSwing)
}

### Results
results <- list()
for (year in years){
    fileName <- paste0("results",year,".xlsx")
    results[[year]] <- cleanResults(fileName,1)
}

### Polls
polls <- list()
for (year in years){
    fileName <- paste0("polls",year,".csv")
    polls[[year]] <- cleanPollData(fileName,results[[year]])
}

### Plots
## Stripped Plots
grid.arrange(stripPollPlot(plotPoll(polls[['2008']])),
             stripPollPlot(plotPoll(polls[['2012']])),
             stripPollPlot(plotPoll(polls[['2016']])),
             ncol=3)
## Legend and y-axis (the 2008 polls contain all the states)
legend<-g_legend(plotPoll(polls[['2008']]))
y_axis<-g_axis(plotPoll(polls[['2008']]),1)
grid.arrange(legend)
grid.arrange(y_axis)

### Data table of means and RMSE
data.frame(year = years, mean = sapply(polls, calcPollsMean),
   mean_swing = sapply(polls, calcPollsSwingMean),
   MSE = sapply(polls, calcPollsMSE),
   MSE_swing = sapply(polls, calcPollsSwingMSE))

