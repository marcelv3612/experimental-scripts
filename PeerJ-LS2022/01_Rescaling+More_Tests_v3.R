##### STATISTICAL TESTS - Scaling Big Five from 2-10 to 1-10 #####
## Adjust Big Five scores from scale 2-10 to 1-10
# Linear rescaling function from 2-10 to 1-10
rescale_to_1_10 <- function(x) {
  return(1 + ((x - 2) / (10 - 2)) * (10 - 1))
}

# Apply the rescaling to the Big Five columns
outhc$B5_O <- rescale_to_1_10(outhc$B5_O)
outhc$B5_C <- rescale_to_1_10(outhc$B5_C)
outhc$B5_E <- rescale_to_1_10(outhc$B5_E)
outhc$B5_A <- rescale_to_1_10(outhc$B5_A)
outhc$B5_N <- rescale_to_1_10(outhc$B5_N)

outhc %$% table(HD,max_mean)
outhc %>% group_by(HD) %>% summarize(mean(B5_O), 
                                     sd(B5_O), 
                                     mean(B5_C), 
                                     sd(B5_C), 
                                     mean(B5_E), 
                                     sd(B5_E), 
                                     mean(B5_A), 
                                     sd(B5_A), 
                                     mean(B5_N), 
                                     sd(B5_N)) %>% 
  data.frame

# Performing statistical tests after scaling

# Student' t-tests and Shapiro-Wilk normality tests

# Note for reproducibility: If running with the initials further censored, 
# the original order of the clusters becomes changed. The members of clusters and
# results are same, but the HD numbers must be swapped.
# Update: we have swapped the HD numbers to reflect the ordering of clusters
# that comes out with initials first letter being replaced by the 'x' char.
# Tables of BFI before and after transformations are as follows:
# Original:
#  HD mean.B5_O.  sd.B5_O. mean.B5_C. sd.B5_C. mean.B5_E.  sd.B5_E. mean.B5_A.  sd.B5_A.
# 1  1   8.555556 1.0137938   5.222222 1.201850   5.222222 1.3944334   5.666667 1.6583124
# 2  2   4.500000 0.7559289   5.500000 1.069045   5.750000 1.2817399   6.250000 1.3887301
# 3  3   7.000000 0.0000000   6.000000 2.828427   7.500000 0.7071068   8.500000 0.7071068
# 4  4   9.500000 0.5773503   8.000000 1.825742   6.750000 0.9574271   7.500000 1.0000000
# 5  5   6.500000 1.7320508   6.250000 1.892969   3.250000 0.5000000   6.750000 1.5000000
# mean.B5_N.  sd.B5_N.
# 1   5.333333 1.5000000
# 2   4.625000 0.9161254
# 3   9.500000 0.7071068
# 4   2.500000 0.5773503
# 5   8.000000 1.4142136
#
# Cluster ordering after censoring first char with ‘x’.
# HD mean.B5_O.  sd.B5_O. mean.B5_C. sd.B5_C. mean.B5_E.  sd.B5_E. mean.B5_A.  sd.B5_A.
# 1  1   6.500000 1.7320508   6.250000 1.892969   3.250000 0.5000000   6.750000 1.5000000
# 2  2   8.555556 1.0137938   5.222222 1.201850   5.222222 1.3944334   5.666667 1.6583124
# 3  3   4.500000 0.7559289   5.500000 1.069045   5.750000 1.2817399   6.250000 1.3887301
# 4  4   9.500000 0.5773503   8.000000 1.825742   6.750000 0.9574271   7.500000 1.0000000
# 5  5   7.000000 0.0000000   6.000000 2.828427   7.500000 0.7071068   8.500000 0.7071068
# mean.B5_N.  sd.B5_N.
# 1   8.000000 1.4142136
# 2   5.333333 1.5000000
# 3   4.625000 0.9161254
# 4   2.500000 0.5773503
# 5   9.500000 0.7071068
#
# After transformation to the 1-10 scale
# HD mean.B5_O.  sd.B5_O. mean.B5_C. sd.B5_C. mean.B5_E.  sd.B5_E. mean.B5_A.  sd.B5_A.
# 1  1     6.0625 1.9485572    5.78125 2.129591    2.40625 0.5625000    6.34375 1.6875000
# 2  2     8.3750 1.1405180    4.62500 1.352082    4.62500 1.5687375    5.12500 1.8656014
# 3  3     3.8125 0.8504201    4.93750 1.202676    5.21875 1.4419574    5.78125 1.5623214
# 4  4     9.4375 0.6495191    7.75000 2.053960    6.34375 1.0771055    7.18750 1.1250000
# 5  5     6.6250 0.0000000    5.50000 3.181981    7.18750 0.7954951    8.31250 0.7954951
# mean.B5_N.  sd.B5_N.
# 1   7.750000 1.5909903
# 2   4.750000 1.6875000
# 3   3.953125 1.0306411
# 4   1.562500 0.6495191
# 5   9.437500 0.7954951

# After transformation to the 1-10 scale and reordering back to the original cluster ordering
# OC mean.B5_O.  sd.B5_O. mean.B5_C. sd.B5_C. mean.B5_E.  sd.B5_E. mean.B5_A. sd.B5_A.
# 1  1   8.171875 1.2830827   3.953125 1.521092   3.953125 1.7648297   4.515625 2.098802
# 2  2   3.039062 0.9567226   4.304688 1.353010   4.621094 1.6222020   5.253906 1.757612
# 3  3   6.203125 0.0000000   4.937500 3.579728   6.835938 0.8949320   8.101562 0.894932
# 4  4   9.367188 0.7307089   7.468750 2.310705   5.886719 1.2117437   6.835938 1.265625
# 5  5   5.570312 2.1921268   5.253906 2.395789   1.457031 0.6328125   5.886719 1.898438
# mean.B5_N.  sd.B5_N.
# 1  4.0937500 1.8984375
# 2  3.1972656 1.1594712
# 3  9.3671875 0.8949320
# 4  0.5078125 0.7307089
# 5  7.4687500 1.7898640



outhc %>% filter(max_mean == "Pil") %>% select(B5_O)

# Create a transformation table for HD to OC mapping
outhc$OC <- NA  # Initialize a new column for Original Cluster

# Map HD values to OC based on the transformation table
outhc$OC[outhc$HD == 1] <- 5  # OC5 = HD1
outhc$OC[outhc$HD == 2] <- 1  # OC1 = HD2
outhc$OC[outhc$HD == 3] <- 2  # OC2 = HD3
outhc$OC[outhc$HD == 4] <- 4  # OC4 = HD4
outhc$OC[outhc$HD == 5] <- 3  # OC3 = HD5

outhc %$% table(OC,max_mean)
outhc %>% group_by(OC) %>% summarize(mean(B5_O), 
                                     sd(B5_O), 
                                     mean(B5_C), 
                                     sd(B5_C), 
                                     mean(B5_E), 
                                     sd(B5_E), 
                                     mean(B5_A), 
                                     sd(B5_A), 
                                     mean(B5_N), 
                                     sd(B5_N)) %>% 
  data.frame

# Calculate the sample mean of Openness (B5_O)
mean_openness <- mean(outhc$B5_O)
print(mean_openness)

# Original cluster 1
t.test(outhc %>% filter(OC == 1) %>% select(B5_O),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_O))),
       alternative = "greater") 

# Original cluster 2
t.test(outhc %>% filter(OC == 2) %>% select(B5_N),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_N))),
       alternative = "less")

t.test(outhc %>% filter(OC == 2) %>% select(B5_A),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_A))),
       alternative = "two.sided") 

# Original cluster 3
t.test(outhc %>% filter(OC == 3) %>% select(B5_E),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_E))),
       alternative = "greater")

t.test(outhc %>% filter(OC == 3) %>% select(B5_A),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_A))),
       alternative = "greater")

t.test(outhc %>% filter(OC == 3) %>% select(B5_N),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_N))),
       alternative = "greater") 

# Original cluster 4
t.test(outhc %>% filter(OC == 4) %>% select(B5_O),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_O))),
       alternative = "greater") 

t.test(outhc %>% filter(OC == 4) %>% select(B5_C),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_C))),
       alternative = "greater") 

# Original cluster 5
t.test(outhc %>% filter(OC == 5) %>% select(B5_N),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_N))),
       alternative = "greater") 

t.test(outhc %>% filter(HD == 5) %>% select(B5_E),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_E))),
       alternative = "two.sided") 

t.test(outhc %>% filter(HD == 5) %>% select(B5_A),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_A))),
       alternative = "two.sided") 

shapiro.test(as.numeric(outhc[,"B5_O"]))
shapiro.test(as.numeric(outhc[,"B5_C"]))
shapiro.test(as.numeric(outhc[,"B5_E"]))
shapiro.test(as.numeric(outhc[,"B5_A"]))
shapiro.test(as.numeric(outhc[,"B5_N"]))


TukeyHSD(aov(mean_motivation ~ Role, data = motivation_by_role))
