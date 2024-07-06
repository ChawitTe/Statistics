HighDose<-c(12,13)
LowDose<-c(6,6,7)
score <- c(HighDose,LowDose)
gr <- factor(rep(c("A", "B"), c(length(HighDose), length(LowDose))))
library(perm)
permTS(score ~ gr, alternative="greater",)$p.value


wait<-c(49.48,43.30,85.97,46.92,49.18,79.30,47.35,46.52,59.68,42.89,49.29,68.69,41.61,46.81,43.75,46.55,42.33,71.48,78.95,42.06)
nowait<-c(36.30,42.07,39.97,39.33,33.76,33.91,39.65,84.92,40.70,39.65,39.48,35.38,75.07,36.46,38.73,33.88,34.39,60.52,53.63,50.62)
score <- c(wait,nowait)
gr <- factor(rep(c("A", "B"), c(length(wait), length(nowait))))
library(perm)
permTS(score ~ gr, alternative="greater",)$p.value



males <- c(5, 7, 9, 10, 12, 12, 12, 13, 13, 15, 15, 20)
females <- c(5, 7, 7, 8, 10, 10, 11, 12, 12, 14, 14, 14, 16, 18, 20, 20, 20, 22, 23, 25, 40)
score <- c(males,females)
gr <- factor(rep(c("A", "B"), c(length(males), length(females))))
library(perm)
permTS(score ~ gr, alternative="greater",)$p.value