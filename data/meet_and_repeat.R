#libraries

library(dplyr)
library(tidyr)

#data wrangling

BPRS <- read.csv("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", stringsAsFactors = F, sep = "", header = T)

RATS <- read.csv("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", stringsAsFactors = F, sep = "\t", header = T)

View(BPRS)

str(BPRS)

colnames(BPRS)

summary(BPRS)


View(RATS)

str(RATS)

colnames(RATS)

summary(RATS)


BPRS$treatment <- as.factor(BPRS$treatment)

BPRS$subject <- as.factor(BPRS$subject)

RATS$ID <- as.factor(RATS$ID)

RATS$Group <- as.factor(RATS$Group)

# Data sets to long form

BPRSL <-  BPRS %>% 
  gather(key = weeks, value = bprs, -treatment, -subject) %>%
  mutate(week = as.integer(substr(weeks, 5, 5)))


RATSL <- RATS %>%
  gather(key = WD, value = Weight, -ID, -Group) %>%
  mutate(Time = as.integer(substr(WD, 3,4))) 

# Now we take a second look at the data
View(BPRSL)

str(BPRSL)

colnames(BPRSL)

summary(BPRSL)


View(RATSL)

str(RATSL)

colnames(RATSL)

summary(RATSL)

# Creating csv data set

write.csv(RATSL, file = "RATSL_WRANGLED.csv", row.names = T)
write.csv(BPRSL, file = "BPRS_WRANGLED.Rdata")

