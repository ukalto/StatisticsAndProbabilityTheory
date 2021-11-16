#Task 1
workloads = c(25,13,7,9,44,3,2,33)
median(med)
#11
quantile(workloads, c(3/4, 1/3), type=2)
#75% 33.33333% 
#29  7
#Task 2
setwd("C:/Users/maxig/Desktop/Uni/Statistik/StatisticsAndProbabilityTheory/HW 6")
load("algorithms.Rdata")
boxplot(runningtimes, ylab = "Runningtimes", col=4, horizontal = TRUE)

