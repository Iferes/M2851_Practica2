library(nortest)

normTest <- function(data) {
  ad_val = (ad.test(data)$p.value > alpha)
  ks_val = (ks.test(data, pnorm, mean(data), sd(data))$p.value > alpha)
  sh_val = (shapiro.test(data)$p.value > alpha)
  cat(ad_val,ks_val,sh_val,"\t")
}

alpha = 0.05
col.names = colnames(students)
cat("Distribucion normal: \n")
for (i in 1:ncol(students)) {
  if (is.integer(students[,i]) | is.numeric(students[,i])) {
    cat(col.names[i])
    cat("\t")
    normTest(students[,i])
    cat("\n")
  }
}

normTest(students[students$sex=="M",c("traveltime")])
View(students)
