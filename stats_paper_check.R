# The effects of birth order on personality traits and feelings of academic sibling rivalry

# Julia Badger & Peter Reddy (2009)


# I have decided to investigate the data from this study. The sample size is suspiciously small, while the 
# statistical results are reported to be significant. I want to check the power of their analysis and see
# what is the probability of arriving to significant results with simulated data of the same parameters (N, M, SD).


# data for rivalry

t.vect_riv <- vector(length=10000)
p.vect_riv <- vector(length=10000)

for(i in 1:10000){
  firstborn_rival <- rnorm(23, mean = 50.18, sd = 16.11)
  lastborn_rival <- rnorm(23, mean = 60.38, sd = 13.53)
  or_riv <- as.factor(c(rep(0:1, each = 23, len = 46)))
  df_riv <- data.frame(c(rbind(firstborn_rival, lastborn_rival)), or_riv)
  mtest_riv <- summary.aov(aov(df_riv$c.rbind.firstborn_rival..lastborn_rival.. ~ or_riv))
  t.vect_riv[i] <- mtest_riv[[1]][["F value"]]
  p.vect_riv[i] <- mtest_riv[[1]][["Pr(>F)"]]
}

# significant results of rivalry data

table(p.vect_riv < 0.05)

# data for conscientiousness

t.vect_cons <- vector(length=10000)
p.vect_cons <- vector(length=10000)

for(i in 1:10000){
  firstborn_cons <- rnorm(23, mean = 54.45, sd = 7.1)
  lastborn_cons <- rnorm(23, mean = 38.88, sd = 28.64)
  or_cons <- as.factor(c(rep(0:1, each = 23, len = 46)))
  df_cons <- data.frame(c(rbind(firstborn_cons, lastborn_cons)), or_cons)
  mtest_cons <- summary.aov(aov(df_cons$c.rbind.firstborn_cons..lastborn_cons.. ~ or_cons))
  t.vect_cons[i] <- mtest_cons[[1]][["F value"]]
  p.vect_cons[i] <- mtest_cons[[1]][["Pr(>F)"]]
}

# significant results of conscientiousness data

table(p.vect_cons < 0.05)

# find significant results for clearly false-positive tests


t.vect_falsepos <- vector(length=10000)
p.vect_falsepos <- vector(length=10000)

for(i in 1:10000){
  firstborn_falsepos <- rnorm(23, mean = 50, sd = 50)
  lastborn_falsepos <- rnorm(23, mean = 50, sd = 50)
  or_falsepos <- as.factor(c(rep(0:1, each = 23, len = 46)))
  df_falsepos <- data.frame(c(rbind(firstborn_falsepos, lastborn_falsepos)), or_falsepos)
  mtest_falsepos <- summary.aov(aov(df_falsepos[,1] ~ or_falsepos))
  t.vect_falsepos[i] <- mtest_falsepos[[1]][["F value"]]
  p.vect_falsepos[i] <- mtest_falsepos[[1]][["Pr(>F)"]]
}

table(p.vect_falsepos < 0.05)

# false-positive tests tend to result in significance in cca 5% of the cases
# however! tests with data from study lead to significance in LESS THAN 5% OF THE CASES

# let's plot it in hist

hg_fb_rivalry <- hist(rnorm(23, mean = 54.45, sd = 7.1), plot = FALSE) # Save first histogram data
hg_lb_rivalry <- hist(rnorm(23, mean = 60.38, sd = 13.53), plot = FALSE) # Save 2nd histogram data

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

plot(hg_fb_rivalry, col = c1) # Plot 1st histogram using a transparent color
plot(hg_lb_rivalry, col = c2, add = TRUE) # Add 2nd histogram using different color

# Conclusion:
# The proportion of significant results from the original data tends to converge to less than 5 %. 
# This is probably caused by using ANOVA where one of the two compared groups has huge variance. 
# In combination with a rather low number of DoF (computed from the sample size), the ANOVA rarely
# arrives to significant results. 
