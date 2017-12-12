library(lattice)
library(dplyr)

modefunc <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x,ux))
  ux[tab == max(tab)]
}

out <- file(file.choose(), open = "wt")
sink(out)

attackdata = read.csv(file.choose())

statrow = c("1", "2", "3", "4", "5", "6", "7",
            "Terran1", "Terran2", "Terran3", "Terran4", "Terran5", 
            "Terran6", "Terran7", "Protoss1", "Protoss2", "Protoss3", 
            "Protoss4", "Protoss5", "Protoss6", "Protoss7",
            "Zerg1", "Zerg2", "Zerg3", "Zerg4", "Zerg5", "Zerg6", "Zerg7")
statcol = c("mean", "median", "max", "min", "sd", "var", "CI-", "CI+")
statmat <- matrix(, nrow = 28, ncol = 8)
rownames(statmat) <- statrow
colnames(statmat) <- statcol

zergattack = subset(attackdata, Race == "Zerg")
protossattack = subset(attackdata, Race == "Protoss")
terranattack = subset(attackdata, Race == "Terran")

Z=1.96

for (i in 1:nrow(statmat))
{
    j = i %% 7
    if (j == 0)
      { j = 7}
    if (i < 8){
    CImean = mean(attackdata[(attackdata$AI.Level == j),3])
    n = length(attackdata[(attackdata$AI.Level == j),3])
    CIsd = sd(attackdata[(attackdata$AI.Level == j), 3])
    statmat[i,1] = mean(attackdata[(attackdata$AI.Level == j),3])
    statmat[i,2] = median(attackdata[(attackdata$AI.Level == j),3])
    statmat[i,3] = max(val <- c(attackdata[attackdata$AI.Level == j, 3]))
    statmat[i,4] = min(val <- c(attackdata[attackdata$AI.Level == j, 3]))
    statmat[i,5] = sd(attackdata[attackdata$AI.Level == j, 3])
    statmat[i,6] = var(attackdata[attackdata$AI.Level == j, 3])
    statmat[i,7] = CImean-Z*(CIsd/sqrt(n))
    statmat[i,8] = CImean+Z*(CIsd/sqrt(n))
  }
  if (i > 7 & i < 15){
    CImean = mean(terranattack[(terranattack$AI.Level == j),3])
    n = length(terranattack[(terranattack$AI.Level == j),3])
    CIsd = sd(terranattack[(terranattack$AI.Level == j), 3])
    statmat[i,1] = mean(terranattack[(terranattack$AI.Level == j),3])
    statmat[i,2] = median(terranattack[(terranattack$AI.Level == j),3])
    statmat[i,3] = max(val <- c(terranattack[terranattack$AI.Level == j, 3]))
    statmat[i,4] = min(val <- c(terranattack[terranattack$AI.Level == j, 3]))
    statmat[i,5] = sd(terranattack[terranattack$AI.Level == j, 3])
    statmat[i,6] = var(terranattack[terranattack$AI.Level == j, 3])
    statmat[i,7] = CImean-Z*(CIsd/sqrt(n))
    statmat[i,8] = CImean+Z*(CIsd/sqrt(n))
  }
  if (i > 14 & i < 22)
  {
    CImean = mean(protossattack[(protossattack$AI.Level == j),3])
    n = length(protossattack[(protossattack$AI.Level == j),3])
    CIsd = sd(protossattack[(protossattack$AI.Level == j), 3])
    statmat[i,1] = mean(protossattack[(protossattack$AI.Level == j),3])
    statmat[i,2] = median(protossattack[(protossattack$AI.Level == j),3])
    statmat[i,3] = max(val <- c(protossattack[protossattack$AI.Level == j, 3]))
    statmat[i,4] = min(val <- c(protossattack[protossattack$AI.Level == j, 3]))
    statmat[i,5] = sd(protossattack[protossattack$AI.Level == j, 3])
    statmat[i,6] = var(protossattack[protossattack$AI.Level == j, 3])
    statmat[i,7] = CImean-Z*(CIsd/sqrt(n))
    statmat[i,8] = CImean+Z*(CIsd/sqrt(n))
  }
  if (i > 21)
  {
    CImean = mean(zergattack[(zergattack$AI.Level == j),3])
    n = length(zergattack[(zergattack$AI.Level == j),3])
    CIsd = sd(zergattack[(zergattack$AI.Level == j), 3])
    statmat[i,1] = mean(zergattack[(zergattack$AI.Level == j),3])
    statmat[i,2] = median(zergattack[(zergattack$AI.Level == j),3])
    statmat[i,3] = max(val <- c(zergattack[zergattack$AI.Level == j, 3]))
    statmat[i,4] = min(val <- c(zergattack[zergattack$AI.Level == j, 3]))
    statmat[i,5] = sd(zergattack[zergattack$AI.Level == j, 3])
    statmat[i,6] = var(zergattack[zergattack$AI.Level == j, 3])
    statmat[i,7] = CImean-Z*(CIsd/sqrt(n))
    statmat[i,8] = CImean+Z*(CIsd/sqrt(n))
  }
}

cat("\nStatistics Based on AI Level and Race\n")
print(statmat)

#xyplot(AI.Level ~ Attack.State, data = attackdata)

balattackdata <- attackdata
balattackdata$AI.Level <-factor(balattackdata$AI.Level, 
                             levels = c(1, 2, 3, 4, 5, 6, 7), 
                             labels = c("L1", "L2", "L3", "L4", "L5", "L6", "L7"))
freqtable <- table(balattackdata$AI.Level, balattackdata$Race)
for (i in 1:nrow(freqtable))
{
  if (freqtable[i,1] == freqtable[i, 2] & freqtable[i,1] == freqtable[i,3])
  {
    print("Balanced")
  }
  else
  {
    j = which.min(freqtable[i,])
    for (k in 1:3)
    {
      if (freqtable[i,k] > freqtable[i,j])
      {
        rown = row.names(freqtable)[i]
        coln = colnames(freqtable)[k]
        diff = freqtable[i,k] - freqtable[i,j]
        
        remrows = which(balattackdata$AI.Level == rown & balattackdata$Race == coln)
        remrows = sample(remrows, diff)
        balattackdata <- balattackdata[-remrows,]
      }
    }
  }
}

#ggboxplot(balattackdata, x = "AI.Level", y = "Attack.State", color = "Race")
#boxplot(Attack.State ~ AI.Level * Race, data=balattackdata, frame = FALSE, col = c("#00AFBB", "#E7B800"), ylab="Attack Time")
#interaction.plot(x.factor = balattackdata$AI.Level, trace.factor = balattackdata$Race, response = balattackdata$Attack.State, fun = mean, type = "b", legend = TRUE, xlab = "AI", ylab = "Attack Tick", pch=c(1,19), col = c("red", "blue", "green"))

cat("\nTwo Way ANOVA\n")
bal.aov1 <- aov(Attack.State ~ AI.Level * Race, data = balattackdata)
print(summary(bal.aov1))
#plot(bal.aov1,2)

cat("\nSingle Linear Regression Ignoring Race\n")
attack.mod1 = lm(Attack.State ~ AI.Level, data = balattackdata)
print(summary(attack.mod1))

cat("\nTwo Way Linear Regression\n")
attack.mod2 <- lm(Attack.State ~ AI.Level + Race, data = balattackdata)
print(summary(attack.mod2))

sink()