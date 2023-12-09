# Install packages
#install.packages(c("knitr", "dplyr", "survival", "ggplot2", "here", "tibble"))
#install.packages(c("lubridate", "ggsurvfit", "gtsummary", "tidycmprsk"))
#install.packages("devtools")
library(knitr)
library(dplyr)
library(survival)
library(ggplot2)
library(tibble)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(devtools)

#####
rect <- function(data) {

  probdata = data.frame(days=data$Lifespan, censor=data$Censoring)
  probdata = probdata[order(probdata$days),]
  days <- probdata$days
  censor <- probdata$censor

  # BASIC STATISTICS
  meandays <- mean(days)
  mediandays <- median(days)
  SD = sd(days)
  iqr = IQR(days)

  # Modify data for analysis
  # (Directly calculate K-M survival rate, account for censored data)
  days = c(0,days)
  censor = c(1,censor)
  # "interval" survival rate
  survrate <- rep(0,length(days))
  censored = 0
  survrate[1] <- 1
  for (i in 2:length(days)) {
    if (censor[i] == 0) {
      censored = censored+1
    }
    survrate[i] = 1-(1/((length(days)-i+2)-censored))
  }

  # K-M survival rate
  prob <- cumprod(survrate)

  # Fit logistic regression model
  model <- glm(prob ~ days, data=data, family=binomial(link="logit"))

  # Define new data frame with predicted values
  newdata <- data.frame(days=seq(0, max(days),len=500))

  # Use fitted model to predict values of vs
  newdata$prob = predict(model, newdata, type="response")

  # Plot logistic regression curve
  plot(prob ~ days, data=data, col="steelblue")
  lines(prob ~ days, newdata, lwd=2)

  df <- data.frame(x=c(0, 500))
  eq = function(x){exp(coef(model)[2]*x+coef(model)[1])/(1+exp(coef(model)[2]*x+coef(model)[1]))}

  # Find first derivative
  xvec = (-1:1500)/2

  der1 <- rep(0, length(xvec))
  for (i in 2:(length(xvec)-1)) {
    der1[i] <- (eq(xvec[i+1])-eq(xvec[i-1]))/2
  }

  plot(xvec, der1)

  # Find second derivative
  der2 <- rep(0, length(xvec))
  for (i in 3:(length(xvec)-2)) {
    der2[i] <- (der1[i+1]-der1[i-1])/2
  }

  plot(xvec, der2)

  # FASTEST DECLINE
  # INCREASES with more rectangularity
  # lim -> 1
  # the absolute minimum of the first derivative, largest negative slope
  #
  # survival curve
  FD <- min(der1)

  FDx <- xvec[mean(which(der1[xvec]==FD))/2]
  abline(v=FDx)

  # SHARPEST CORNER
  # INCREASES with more rectangularity
  # lim -> 1
  # the absolute minimum of the second derivative, "sharpest corner"
  #
  # gives the maximum of how fast velocity declines, AKA the most
  # convex point on the graph
  SC <- min(der2)

  SCx <- xvec[mean(which(der2[xvec]==SC))/2]
  abline(v=SCx)

  # QUICKEST PLATEAU
  # INCREASES with more rectangularity
  # lim-> 1
  # the absolute minimum of the second derivative
  #
  # gives the maximum of how fast velocity increases, AKA the most
  # concave point on the graph
  QP <- max(der2)

  QPx <- xvec[mean(which(der2[xvec]==QP))/2]
  abline(v=QPx)

  # PROLATE INDEX
  # INCREASES with more rectangularity
  # lim -> 1
  #
  # angle between the vertical line at the QP and the line between the QP and
  # the SC (between minimum and maximum curvature)
  QPv = c(0, eq(QPx))
  SCv = c(SCx-QPx, eq(SCx)-eq(QPx))
  theta <- acos( sum(QPv*SCv) / ( sqrt(sum(QPv * QPv)) * sqrt(sum(SCv * SCv)) ) )
  PI = cos(theta)

  #png("my_plot_test.png")

  plot(prob ~ days, data=data, col="lightsteelblue3",
       xlab = "Time (Days)",
       ylab = "Probability of Survival")
  title(stringi::stri_c("Survival Curve"))
  lines(prob ~ days, newdata, lwd=2)
  abline(v=FDx, col="seagreen3", lty="dashed", lwd=2)
  abline(v=SCx, col="firebrick3", lty="dashed", lwd=2)
  abline(v=QPx, col="royalblue2", lty="dashed", lwd=2)
  legend(x="bottomleft", legend = c("Fastest Decline", "Sharpest Corner",
                                    "Quickest Plateau"), lty = c("dashed", "dashed"),
         col = c("seagreen3", "firebrick3", "royalblue2"), lwd = rep(2,3))

  #dev.off()

  # FIXED RECTANGLE
  # INCREASES with more rectangularity
  # lim -> 1
  #
  # the percentage of the curve covering the area under the rectangle
  # with fixed height 1 and width of 350*

  # Find integral of survival curve
  int = 0
  bound = 350

  for (i in 1:(length(days)-1)) {
    if (days[i+1]<bound) {
      int = int + (days[i+1]-days[i])*prob[i]
      icount = i
    }
  }

  intFR = int + (350-days[icount])*prob[icount]

  FR = intFR/bound

  # MOVING RECTANGLE
  # INCREASES with more rectangularity
  # lim -> 1
  #
  # the percentage of the curve covering the area under the rectangle
  # with fixed height 1 and variable width - determined by where prob = 0.01
  int = 0
  eps = 0.01

  for (i in 1:(length(days)-1)) {
    if (prob[i+1]>=eps) {
      int = int + (days[i+1]-days[i])*prob[i]
      icount = i
    }
  }

  intMR = int

  MR = intMR/days[icount+1]

  # IQR
  iqr = IQR(data$Lifespan)

  # GINI COEFFICIENT
  # DECREASES with more rectangularity
  # lim -> 0
  #
  # measure of inequality (variability) in age of death
  # more classical measure using statistical variation
  int2G = 0
  intG = 0

  for (i in 1:(length(days)-1)) {
    intG = intG + (days[i+1]-days[i])*prob[i]
    int2G = int2G + (days[i+1]-days[i])*(prob[i])^2
  }

  G = 1-int2G/intG

  # KEYFITZ'S H
  # DECREASES with more rectangularity
  # lim -> 0
  #
  # measure of inequality (variability) in age of death
  # essentially, how much a reduction in death rates improves life expectancy
  intLOG = 0
  probLOG = log(prob)

  for (i in 1:(length(days)-1)) {
    intLOG = intLOG + (days[i+1]-days[i])*prob[i]*probLOG[i]
  }

  KFH = -intLOG/intG

  # Consolidate data
  finaldf = data.frame('Mean'=meandays, 'Median'=mediandays,
                      'IQR'=iqr, 'Standard Deviation'=SD, 'Gini Coefficient'=G,
                      'Keyfitz H'=KFH, 'Fixed Rectangle'=FR,
                      'Moving Rectangle'=MR, 'Fastest Decline'=-FD,
                      'Sharpest Corner'=-SC, 'Quickest Plateau'=QP,
                      'Prolate Index'=PI)

  return("finaldf" <<- finaldf)
}
#####

#####
multirect <- function(data,cat1,cat2,cat3) {
  if ('Lifespan' %in% names(data)) {
    Lifespan = NULL
    for (i in 1:length(data$ID)) {
      span <- interval(mdy(data$Start_day[i]), mdy(data$End_day[i]))
      lifespan <- as.duration(span)/(3600*24)
      Lifespan = c(Lifespan, lifespan)
    }
    data = data.frame(data, "Lifespan"=Lifespan)
  }

  finaldf = data.frame('Mean'=NULL, 'Median'=NULL,
                       'IQR'=NULL, 'Standard Deviation'=NULL, 'Gini Coefficient'=NULL,
                       'Keyfitz H'=NULL, 'Fixed Rectangle'=NULL,
                       'Moving Rectangle'=NULL, 'Fastest Decline'=NULL,
                       'Sharpest Corner'=NULL, 'Quickest Plateau'=NULL,
                       'Prolate Index'=NULL)

  split_group = group_split(data,{{cat1}})

  if (missing(cat2)) {
    cat1n = deparse(substitute(cat1))

    finaldf = data.frame(cat1=NULL, finaldf)

    for (d in 1:length(split_group)) {
      data <- split_group[[d]]

      tempdf = rect(data)
      tempdf = data.frame(cat1=data[[cat1n]][1], tempdf)
      finaldf <- rbind(finaldf, tempdf, deparse.level = 1)
    }

    names(finaldf)[1] = cat1n

  } else if (missing(cat3)) {
    cat1n = deparse(substitute(cat1))
    cat2n = deparse(substitute(cat2))

    finaldf = data.frame(cat1=NULL, cat2=NULL, finaldf)

    for (d in 1:length(split_group)) {
      data <- split_group[[d]]

      split_group2 = group_split(data,{{cat2}})

      for (f in 1:length(split_group2)) {
        data <- split_group2[[f]]

        tempdf = rect(data)
        tempdf = data.frame(cat1=data[[cat1n]][1], cat2=data[[cat2n]][1], tempdf)
        finaldf <- rbind(finaldf, tempdf, deparse.level = 1)
      }}

    names(finaldf)[1] = cat1n
    names(finaldf)[2] = cat2n

  } else {
    cat1n = deparse(substitute(cat1))
    cat2n = deparse(substitute(cat2))
    cat3n = deparse(substitute(cat3))

    finaldf = data.frame(cat1=NULL, cat2=NULL, cat3=NULL, finaldf)

    for (d in 1:length(split_group)) {
      data <- split_group[[d]]

      split_group2 = group_split(data,{{cat2}})

      for (f in 1:length(split_group2)) {
        data <- split_group2[[f]]

        split_group3 = group_split(data,{{cat3}})

        for (g in 1:length(split_group3)) {
          data <- split_group3[[g]]

          tempdf = rect(data)
          tempdf = data.frame(cat1=data[[cat1n]][1], cat2=data[[cat2n]][1],
                              cat3=data[[cat3n]][1], tempdf)
          finaldf <- rbind(finaldf, tempdf, deparse.level = 1)
        }}}

    names(finaldf)[1] = cat1n
    names(finaldf)[2] = cat2n
    names(finaldf)[3] = cat3n

  }
  return("finaldf" <<- finaldf)
}
#####

## COMPARISON

## LOG TEST FUNCTION
#####
logtest <- function(data,cat1,cat2) {
  cat1n = deparse(substitute(cat1))
  print(cat1n)
  cat2n = deparse(substitute(cat2))
  print(cat2n)
  cdata1 = group_split({{data}}, {{cat1}})
  names = c()
  if (missing(cat2)) {
    cdata2 = cdata1
    for (n in 1:length(cdata1)) {
      names = c(names,cdata1[[n]][cat1n][[1]][1])
    }
  } else {
    cdatatemp = cdata1
    cdata1 = list()
    for (i in 1:length(cdatatemp)) {
      cdata1 = c(cdata1, group_split(cdatatemp[[i]], {{cat2}}))
    }
    for (n in 1:length(cdata1)) {
      names = c(names,paste(cdata1[[n]][cat1n][[1]][1],cdata1[[n]][cat2n][[1]][1]))
    }
    cdata2 = cdata1

  }
  logvalue = replicate(length(names),numeric(length(names)))
  temp = data_frame('pvalue'=1)
  #####
  for (i in 1:length(names)) {
    for (j in 1:length(names)) {
      tempdf = list(cdata1[[i]], cdata2[[j]])
      tempdf = Reduce(function(x, y) merge(x, y, all=TRUE), tempdf)
      print(tempdf)
      if (missing(cat2)) {
        if (cdata1[[i]][[cat1n]][1] != cdata2[[j]][[cat1n]][1]) {
          temp <- survdiff(Surv(Lifespan) ~ tempdf[[cat1n]], data = tempdf)
        } else {
          temp$pvalue <- 1
        }
      } else {
        if (cdata1[[i]][[cat1n]][1] == cdata2[[j]][[cat1n]][1]) {
          if (cdata1[[i]][[cat2n]][1] == cdata2[[j]][[cat2n]][1]) {
            temp$pvalue <- 1
          } else {
            temp <- survdiff(Surv(Lifespan) ~ tempdf[[cat2n]], data = tempdf)
          }
        } else {
          if (cdata1[[i]][[cat2n]][1] == cdata2[[j]][[cat2n]][1]) {
            temp <- survdiff(Surv(Lifespan) ~ tempdf[[cat1n]], data = tempdf)
          } else {
            temp <- survdiff(Surv(Lifespan) ~ tempdf[[cat1n]]+tempdf[[cat2n]], data = tempdf)
          }
        }
      }
      logvalue[i,j] = temp$pvalue
      print(logvalue)
    }
  }
  #####
  hlv = logvalue
  hlv[logvalue==1]=-2
  hlv[logvalue<=0.05]=2
  hlv[logvalue>0.05]=0

  heatmap(hlv, symm=TRUE, Rowv = NA, Colv = NA)

  logvalue = array(data=logvalue, dim=c(length(names),length(names)), dimnames=list(names, names))

  return("logvalue" <<- logvalue)
}
#####





