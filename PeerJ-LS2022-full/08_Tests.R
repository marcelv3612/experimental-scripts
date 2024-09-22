
# First the file 07 must be ran
outhc %>% filter(max_mean == "Pil") %>% select(B5_O)

t.test(outhc %>% filter(HD == 1) %>% select(B5_O),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_O))),
       alternative = "greater") 

t.test(outhc %>% filter(HD == 2) %>% select(B5_N),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_N))),
       alternative = "greater") 

t.test(outhc %>% filter(HD == 2) %>% select(B5_A),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_A))),
       alternative = "greater") 

t.test(outhc %>% filter(HD == 3) %>% select(B5_A),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_A))),
       alternative = "greater") 

t.test(outhc %>% filter(HD == 3) %>% select(B5_E),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_E))),
       alternative = "less") 

t.test(outhc %>% filter(HD == 3) %>% select(B5_N),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_N))),
       alternative = "greater") 

t.test(outhc %>% filter(HD == 4) %>% select(B5_O),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_O))),
       alternative = "greater") 

t.test(outhc %>% filter(HD == 4) %>% select(B5_C),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_C))),
       alternative = "greater") 

t.test(outhc %>% filter(HD == 5) %>% select(B5_N),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_N))),
       alternative = "greater") 

t.test(outhc %>% filter(HD == 5) %>% select(B5_E),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_E))),
       alternative = "less") 

t.test(outhc %>% filter(HD == 5) %>% select(B5_A),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_A))),
       alternative = "less") 

shapiro.test(as.numeric(outhc[,"B5_O"]))
shapiro.test(as.numeric(outhc[,"B5_C"]))
shapiro.test(as.numeric(outhc[,"B5_E"]))
shapiro.test(as.numeric(outhc[,"B5_A"]))
shapiro.test(as.numeric(outhc[,"B5_N"]))



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

