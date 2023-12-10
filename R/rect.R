#' Calculate rectangularity of a survival curve
#'
#' @param data Data frame containing lifespan data and optional censoring data.
#'
#' @return A data frame containing rectangularity metrics.
#' @export
#'
#' @examples
#' data <- data.frame('Censoring'=c(1,0,0,0),
#'   'Lifespan'=c(2,7,8,3))
#' rect(data)
rect <- function(data) {

  if ('Censoring' %in% names(data)) {
    probdata = data.frame(days=data$Lifespan, censor=data$Censoring)
  } else {
    probdata = data.frame(days=data$Lifespan, censor=rep(1,length(data$Lifespan)))
  }
  probdata = probdata[order(probdata$days),]
  days <- probdata$days
  censor <- probdata$censor

  # BASIC STATISTICS
  meandays <- mean(days)
  mediandays <- stats::median(days)
  SD = stats::sd(days)
  iqr = stats::IQR(days)

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
  model <- stats::glm(prob ~ days, data=data, family=stats::binomial(link="logit"))

  # Define new data frame with predicted values
  newdata <- data.frame(days=seq(0, max(days),len=500))

  # Use fitted model to predict values of vs
  newdata$prob = stats::predict(model, newdata, type="response")

  # Plot logistic regression curve
  plot(prob ~ days, data=data, col="steelblue")
  graphics::lines(prob ~ days, newdata, lwd=2)

  df <- data.frame(x=c(0, 500))
  eq = function(x){exp(stats::coef(model)[2]*x+stats::coef(model)[1])/(1+exp(stats::coef(model)[2]*x+stats::coef(model)[1]))}

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
  graphics::abline(v=FDx)

  # SHARPEST CORNER
  # INCREASES with more rectangularity
  # lim -> 1
  # the absolute minimum of the second derivative, "sharpest corner"
  #
  # gives the maximum of how fast velocity declines, AKA the most
  # convex point on the graph
  SC <- min(der2)

  SCx <- xvec[mean(which(der2[xvec]==SC))/2]
  graphics::abline(v=SCx)

  # QUICKEST PLATEAU
  # INCREASES with more rectangularity
  # lim-> 1
  # the absolute minimum of the second derivative
  #
  # gives the maximum of how fast velocity increases, AKA the most
  # concave point on the graph
  QP <- max(der2)

  QPx <- xvec[mean(which(der2[xvec]==QP))/2]
  graphics::abline(v=QPx)

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
  graphics::title(stringi::stri_c("Survival Curve"))
  graphics::lines(prob ~ days, newdata, lwd=2)
  graphics::abline(v=FDx, col="seagreen3", lty="dashed", lwd=2)
  graphics::abline(v=SCx, col="firebrick3", lty="dashed", lwd=2)
  graphics::abline(v=QPx, col="royalblue2", lty="dashed", lwd=2)
  graphics::legend(x="bottomleft", legend = c("Fastest Decline", "Sharpest Corner",
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
  # iqr = stats::IQR(data$Lifespan)

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
