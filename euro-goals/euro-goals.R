# Filename: EurocupStats.R
# Description: Calculates different statistics of the Football Eurocup
# tournament throughout the years
################################################################################

require(xlsx)
require(dplyr)
require(ggplot2)

Year=c(1980,1984,1988,1992,1996,2000,2004,2008,2012,2016)
totalTeams=c(8,8,8,8,16,16,16,16,16,24)
teamsPerYear=data.frame(Year=Year,totalTeams=totalTeams)

path <- "C:\\Users\\Rajiv\\Dropbox\\Articles\\euro-goals\\"
file <- paste(path,"euro-goals.xlsx", sep="")
eurocup <- read.xlsx(file,sheetIndex=1)

eurocup <- mutate(eurocup,totalGoals=ScoreA+ScoreB,isGroupStage=Stage=="Group")
meanGoals <-aggregate(list(AverageGoals=eurocup$totalGoals),
                      by=list(Year=eurocup$Year,isGroupStage=eurocup$isGroupStage), 
                      FUN=mean, na.rm=TRUE)

goals <- merge(meanGoals,teamsPerYear,by="Year")
groupGoals <- goals[goals$isGroupStage,]

ggplot(goals, aes(x=Year, y=AverageGoals,color=isGroupStage)) +
    geom_line(size=1) +
    ylim(c(0,4)) +
    geom_point(size=4)+
    guides(color=guide_legend(reverse=TRUE)) +
    scale_color_discrete(name="Stage",labels=c("Knockout","Group")) +
    scale_x_continuous(breaks=seq(from=min(Year),to=max(Year),by=4)) +
    geom_hline(aes(yintercept=mean(goals$AverageGoals[goals$isGroupStage])),linetype="dashed",color="#00BA38") +
    geom_hline(aes(yintercept=mean(goals$AverageGoals[!goals$isGroupStage])),linetype="dashed",color="#F8766D") +
    labs(title="Average number of Goals in the Eurocup",x="Year",y="Average Number of Goals")

ggplot(groupGoals, aes(x=Year, y=AverageGoals)) +
    geom_line(size=1,color="blue") +
    ylim(c(0,3)) +
    geom_point(size=4)+
    theme(legend.position="none") +
    scale_x_continuous(breaks=seq(from=min(Year),to=max(Year),by=4)) +
    theme(text = element_text(size=25))+
    geom_hline(aes(yintercept=mean(groupGoals$AverageGoals)),linetype="dashed",color="blue") +
    labs(title="Goals scored during group stages of the Eurocup",x="Year",y="Average No. of Goals")

eurocupGroup2016 <- eurocup[eurocup$Year==2016 & eurocup$isGroupStage,]
thirdPlace <- c('Portugal','Slovakia','Northern Ireland','Republic of Ireland','Albania','Turkey')
thirdPlaceQual <- c('Portugal','Slovakia','Northern Ireland','Republic of Ireland')
eurocupGroup2016$isThirdPlace=eurocupGroup2016$TeamA %in% thirdPlace | eurocupGroup2016$TeamB %in% thirdPlace
eurocupGroup2016$isThirdPlaceQual=eurocupGroup2016$TeamA %in% thirdPlaceQual | eurocupGroup2016$TeamB %in% thirdPlaceQual
eurocupGroup2016 <- mutate(eurocupGroup2016,isTie=ScoreA==ScoreB)

ggplot(eurocupGroup2016, aes(factor(isThirdPlace), totalGoals)) +  geom_boxplot(aes(color=as.factor(isThirdPlace))) + geom_jitter(aes(color=as.factor(isThirdPlace))) +
    labs(title="Number of goals scored during 2016 Group Stages",x="",y="Number of goals") +
    scale_x_discrete(labels=c("Game without an eventual third place team","Game with an eventual third place team")) +
    theme(text = element_text(size=15)) +
    theme(legend.position="none") +
    scale_colour_brewer(palette = "Set1")

#eurocupGroup2016$isThirdPlace[eurocupGroup2016$isThirdPlaceQual]="zballs"
eurocupGroup2016$isThirdPlace<-as.character(eurocupGroup2016$isThirdPlace)
ggplot(eurocupGroup2016, aes(isThirdPlace, fill=isTie)) + geom_bar(stat="count",position="dodge") +
    labs(title="Results of games during 2016 Group Stages",x="",y="Number of matches") +
    #scale_x_discrete(labels=c("Other team","Third place team")) +
    scale_fill_discrete(labels=c("Win/Loss","Draw")) +
    theme(legend.title=element_blank()) +
    ylim(c(0,13))

eurocup <- mutate(eurocup,isTie=ScoreA==ScoreB)
meanTies <-aggregate(list(AverageTies=eurocup$isTie),
                      by=list(Year=eurocup$Year,isGroupStage=eurocup$isGroupStage), 
                      FUN=mean, na.rm=TRUE)
meanTiesGroup <- meanTies[meanTies$isGroupStage,]
ggplot(meanTiesGroup, aes(x=Year, y=AverageTies*100)) +
    geom_line(color="blue") +
    ylim(c(0,50)) +
    labs(y="Percentage of tie games", title="Percentage of Tie Games during the Group Stages") +
    geom_point(size=4,color="blue") +
    scale_x_continuous(breaks=seq(from=min(Year),to=max(Year),by=4)) +
    geom_hline(aes(yintercept=mean(meanTiesGroup$AverageTies)*100),linetype="dashed",color="blue") +
    theme(text = element_text(size=15))

