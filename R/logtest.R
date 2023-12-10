#' Multiple curve log-rank test
#'
#' @param data Data frame containing lifespan data for at least two groups.
#' @param cat1 Group to separate data by.
#' @param cat2 Optional second group to separate data by.
#'
#' @return A data frame of the significance level between each pair of groups.
#' @export
#'
#' @examples
#' data <- data.frame('Group'=c('A','B','B','A','B','A','C','C','C'),
#'   'Lifespan'=c(2,7,8,3,6,1,15,16,18))
#' logtest(data, Group)
logtest <- function(data,cat1,cat2) {
  cat1n = deparse(substitute(cat1))
  cat2n = deparse(substitute(cat2))
  cdata1 = dplyr::group_split({{data}}, {{cat1}})
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
      cdata1 = c(cdata1, dplyr::group_split(cdatatemp[[i]], {{cat2}}))
    }
    for (n in 1:length(cdata1)) {
      names = c(names,paste(cdata1[[n]][cat1n][[1]][1],cdata1[[n]][cat2n][[1]][1]))
    }
    cdata2 = cdata1

  }
  logvalue = replicate(length(names),numeric(length(names)))
  temp = data.frame('pvalue'=1)
  for (i in 1:length(names)) {
    for (j in 1:length(names)) {
      tempdf = list(cdata1[[i]], cdata2[[j]])
      tempdf = Reduce(function(x, y) merge(x, y, all=TRUE), tempdf)
      if (missing(cat2)) {
        if (cdata1[[i]][[cat1n]][1] != cdata2[[j]][[cat1n]][1]) {
          temp <- survival::survdiff(survival::Surv(Lifespan) ~ tempdf[[cat1n]], data = tempdf)
        } else {
          temp$pvalue <- 1
        }
      } else {
        if (cdata1[[i]][[cat1n]][1] == cdata2[[j]][[cat1n]][1]) {
          if (cdata1[[i]][[cat2n]][1] == cdata2[[j]][[cat2n]][1]) {
            temp$pvalue <- 1
          } else {
            temp <- survival::survdiff(Surv(Lifespan) ~ tempdf[[cat2n]], data = tempdf)
          }
        } else {
          if (cdata1[[i]][[cat2n]][1] == cdata2[[j]][[cat2n]][1]) {
            temp <- survival::survdiff(survival::Surv(Lifespan) ~ tempdf[[cat1n]], data = tempdf)
          } else {
            temp <- survival::survdiff(survival::Surv(Lifespan) ~ tempdf[[cat1n]]+tempdf[[cat2n]], data = tempdf)
          }
        }
      }
      logvalue[i,j] = temp$pvalue
    }
  }
  #####
  hlv = logvalue
  hlv[logvalue==1]=-2
  hlv[logvalue<=0.05]=2
  hlv[logvalue>0.05]=0
  stats::heatmap(hlv, symm=TRUE, Rowv = NA, Colv = NA)
  #####

  logvalue = array(data=logvalue, dim=c(length(names),length(names)), dimnames=list(names, names))

  return("logvalue" <<- logvalue)
}
