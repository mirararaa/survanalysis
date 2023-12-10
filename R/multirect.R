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
