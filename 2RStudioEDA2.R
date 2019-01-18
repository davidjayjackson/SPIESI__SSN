##########################################################
#####     AAVSO Sunspot Counts
##########################################################

# Created  28 May 2011, Jamie Riggs
# Modified
#          14 Jun 2016, Jamie Riggs, use ggplots in place of lattice plots

##########################################################
#####     Initialization
##########################################################

library(MASS)
library(xtable)
library(lattice)
library(stats)
library(nortest)
library(ggplot2)
library(reshape2)
library(data.table)
library(RMySQL)
library(RSQLite)
#
# Delete old stuff
rm(list=ls())
#
# MysQL/SQLite connections:
mydb <- dbConnect(MySQL(),user='root',password='dJj12345',dbname="gn",
                  host='localhost')
db <- dbConnect(SQLite(), dbname="Rhowe.sqlite3")
#
# Pull last month's data from MySQL.
X <- dbGetQuery(mydb,"SELECT * from daily 
                WHERE mon=12 AND year=2018
                ORDER BY year,mon,day")
#
# Path <- "C:/Users/Howe/Desktop/SPESI/SSN/"
Path <- "C:/Users/davidjayjackson/Documents/GitHub/SPIESI__SSN"
setwd(Path)
WD <- getwd()


##########################################################
#####     Functions
##########################################################

FreqProp <- function(factor, factorLabel, dump) {
	table.x <- as.data.frame(table(factor),exclude=c(NA,dump))
	names(table.x)[1] <- c(factorLabel)
	prop <- table.x$Freq / sum(table.x$Freq)
	table.x <- data.frame(table.x, prop)
	sum.x <- colSums(table.x[,2:3])
	new.row <- table.x[1,]
	new.row[1] <- "Total"
	new.row[2:3] <- sum.x
	table.x <- rbind(table.x, new.row)
	}
  
FreqProp2 <- function(factor1, factor2, faclab1, faclab2, dump) {
	table.x <- as.data.frame(table(factor1,factor2),exclude=c(NA,dump))
	names(table.x)[1:2] <- c(faclab1,faclab2)
	prop <- table.x$Freq / sum(table.x$Freq)
	table.x <- data.frame(table.x, prop)
	sum.x <- colSums(table.x[,3:4])
	new.row <- table.x[1,]
	new.row[1:2] <- c("","Total")
	new.row[3:4] <- sum.x
	table.x <- rbind(table.x, new.row)
	}
  
##########################################################
#####     Initialization
##########################################################

 yr <- format(Sys.Date(), format="%Y")
 mo <- as.character(as.numeric(format(Sys.Date(), format="%m"))-1)
 
#   check if December's data and correct year and month
if (mo=="0") { 
	yr <- as.numeric(yr)-1
	mo <- 12
	}
(Ex <- ifelse(nchar(mo)==1,paste0(yr, "0", mo), paste0(yr,mo)))
(ver <- "00")


setwd(Path)
WD <- getwd()
load(paste(WD, "/", "ymtd.RData", sep=""))     # loads as X
summary(X)
nrow(X)

H <- as.data.frame(X)

#####     Use in report     #############################

#X <- H[,-1]
part <- "sum0"
(loc <- paste0("Tables/", Ex, ver, part, ".tex"))
print(xtable(summary(X[,1:5]), caption=paste(Ex," Summary of Sunspot Numbers",sep=""), label="tab:sum00"), caption.placement="top", include.rownames=F, file=loc)
part <- "sum1"
(loc <- paste0("Tables/", Ex, ver, part, ".tex"))
print(xtable(summary(X[,6:11]),caption="Summary of Sunspot Numbers",label="tab:sum00"), caption.placement="top", include.rownames=F, file=loc)


##########################################################
     part <- "Pairs"
##########################################################

A <- subset(X, select=g:w)
summary(A)
nrow(A)
(loc <- paste("Plots/", Ex, ver, part, ".png", sep=""))
# quartz(w=6.5,h=6.5)
pairs(A,main=paste("Scatter Plots of Groups (g), Numbers (s), and", "\n", "Wolf Numbers (w),", sep=" "), col="blue2", panel=panel.smooth, lwd=2)
# quartz.save(loc, type="png")

#print(xtable(summary(A), caption="Summary Statistics", label=paste("tab:",Ex,"ss",sep=""), digits=4), caption.placement="top", include.rownames=F)


##########################################################
     part <- "QQ"
##########################################################

A <- subset(X, select=g:w)

 ilk <- "g"
   y <- A$g
(loc <- paste("Plots/", Ex, ver, part, ilk, ".png", sep=""))
(main <- paste(ilk, "Normal Q-Q Plot"))
v = quantile(y[!is.na(y)], c(0.25, 0.75))
h = qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
int <- v[1L] - slope * h[1L]
gp <- ggplot(A, aes(sample=y)) + 
	stat_qq(col='grey45') +
	geom_abline(slope = slope, intercept = int) + 
	ggtitle(main) + 
	xlab("Theoretical Quantiles") + 
	ylab("Observed Quantiles") +
	theme(plot.title = element_text(hjust = 0.5)) +
	ggsave(loc)
gp

 ilk <- "s"
   y <- A$s
(loc <- paste("Plots/", Ex, ver, part, ilk, ".png", sep=""))
(main <- paste(ilk, "Normal Q-Q Plot"))
v = quantile(y[!is.na(y)], c(0.25, 0.75))
h = qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
int <- v[1L] - slope * h[1L]
gp <- ggplot(A, aes(sample=y)) + 
	stat_qq(col='grey45') +
	geom_abline(slope = slope, intercept = int) + 
	ggtitle(main) + 
	xlab("Theoretical Quantiles") + 
	ylab("Observed Quantiles") +
	theme(plot.title = element_text(hjust = 0.5)) +
	ggsave(loc)
gp

 ilk <- "w"
   y <- A$w
(loc <- paste("Plots/", Ex, ver, part, ilk, ".png", sep=""))
(main <- paste(ilk, "Normal Q-Q Plot"))
v = quantile(y[!is.na(y)], c(0.25, 0.75))
h = qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
int <- v[1L] - slope * h[1L]
gp <- ggplot(A, aes(sample=y)) + 
	stat_qq(col='grey45') +
	geom_abline(slope = slope, intercept = int) + 
	ggtitle(main) + 
	xlab("Theoretical Quantiles") + 
	ylab("Observed Quantiles") +
	theme(plot.title = element_text(hjust = 0.5)) +
	ggsave(loc)
gp


##########################################################
     part <- "Box"
##########################################################

y <- X$w
x <- X$mon
yl <- "w"
xl <- "Month"
(loc <- paste("Plots/", Ex, ver, part, xl, yl, ".png", sep=""))
quartz(w=7, h=7)
bwplot(x~y|X$year, main=paste(yl,"vs",xl,sep=" "), xlab=yl, ylab=xl, col="red", horizontal=TRUE)
quartz.save(loc, type="png")


yl <- "w"
xl <- "Seeing"
(loc <- paste("Plots/", Ex, ver, part, xl, yl, ".png", sep=""))
(main=paste(yl,"vs.",xl))
gp <- ggplot(X, aes(x=as.factor(see),y=w)) + 
	geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
#	scale_y_continuous(trans=scales::log2_trans()) +
	stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
	ggtitle(main) + 
	xlab(xl) + #scale_x_discrete(labels=c("level1",. . . )) +
	ylab(yl) +
	theme(plot.title = element_text(hjust = 0.5))
	ggsave(loc)
gp


yl <- "w"
xl <- "SILSO"
(loc <- paste("Plots/", Ex, ver, part, xl, yl, ".png", sep=""))
(main=paste(yl,"vs.",xl))
gp <- ggplot(X, aes(x=as.factor(silso),y=w)) + 
	geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
#	scale_y_continuous(trans=scales::log2_trans()) +
	stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
	ggtitle(main) + 
	xlab(xl) + #scale_x_discrete(labels=c("level1",. . . )) +
	ylab(yl) +
	theme(plot.title = element_text(hjust = 0.5))
	ggsave(loc)
gp

yl <- "w"
xl <- "Year"
(loc <- paste("Plots/", Ex, ver, part, xl, yl, ".png", sep=""))
(main=paste(yl,"vs.",xl))
gp <- ggplot(X, aes(x=as.factor(year),y=w)) + 
	geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
#	scale_y_continuous(trans=scales::log2_trans()) +
	stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
	ggtitle(main) + 
	xlab(xl) + #scale_x_discrete(labels=c("level1",. . . )) +
	ylab(yl) +
	theme(plot.title = element_text(hjust = 0.5))
	ggsave(loc)
gp
