
# treba najskor spustiť 07_... riadok 110
outhc %>% filter(max_mean == "Pil") %>% select(B5_O)

t.test(outhc %>% filter(HD == 1) %>% select(B5_O),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_O))),
       alternative = "greater") 


t.test(outhc %>% filter(HD == 2) %>% select(B5_E),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_E))),
       alternative = "greater") 

t.test(outhc %>% filter(HD == 2) %>% select(B5_A),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_A))),
       alternative = "greater") 


t.test(outhc %>% filter(HD == 3) %>% select(B5_E),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_E))),
       alternative = "less") 

t.test(outhc %>% filter(HD == 3) %>% select(B5_N),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_N))),
       alternative = "greater") 




t.test(outhc %>% filter(max_mean == "Pil") %>% select(B5_O),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_O))),
       alternative = "greater") 


t.test(outhc %>% filter(max_mean == "Nav") %>% select(B5_E),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_E))),
       alternative = "greater") 

t.test(outhc %>% filter(max_mean == "Nav") %>% select(B5_A),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_A))),
       alternative = "greater") 


t.test(outhc %>% filter(max_mean == "Solo") %>% select(B5_E),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_E))),
       alternative = "less") 

t.test(outhc %>% filter(max_mean == "Solo") %>% select(B5_N),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_N))),
       alternative = "greater") 




exp = Stats %>% group_by(Initials) %>% summarize(exp = min(Experience_yrs))

outhc %>% inner_join(exp) %>% 
  group_by(max_mean) %>% 
  summarize(mn = mean(as.numeric(exp)), n = n())



summary(aov(exp ~ max_mean, outhc %>% inner_join(exp)))

pil_bs = sample()

ggplot(outhc, aes(x= B5_O)) + geom_histogram(bins = 8)
ggplot(outhc, aes(x= B5_C)) + geom_histogram(bins = 8)
ggplot(outhc, aes(x= B5_E)) + geom_histogram(bins = 8)
ggplot(outhc, aes(x= B5_A)) + geom_histogram(bins = 8)
ggplot(outhc, aes(x= B5_N)) + geom_histogram(bins = 8)

shapiro.test(as.numeric(outhc[,"B5_O"]))
shapiro.test(as.numeric(outhc[,"B5_C"]))
shapiro.test(as.numeric(outhc[,"B5_E"]))
shapiro.test(as.numeric(outhc[,"B5_A"]))
shapiro.test(as.numeric(outhc[,"B5_N"]))


# Create a contingency table for Role vs Cluster assignments (HD)
contingency_table <- table(outhc$HD, outhc$max_mean)
contingency_table

# Run Pearson's Chi-squared test
chisq_test <- chisq.test(contingency_table)
chisq_test

#Pearson's Chi-squared test
#data:  contingency_table
#X-squared = 12.735, df = 4, p-value = 0.01264

# Run Fisher's Exact Test
fisher_test <- fisher.test(contingency_table)
fisher_test

#data:  contingency_table
#p-value = 0.01937
#alternative hypothesis: two.sided

# Run Monte Carlo simulation for Chi-squared test
chisq_test_simulation <- chisq.test(contingency_table, simulate.p.value = TRUE, B = 2000)
chisq_test_simulation

#Pearson's Chi-squared test with simulated p-value (based on 2000 replicates)

#data:  contingency_table
#X-squared = 12.735, df = NA, p-value = 0.009495

