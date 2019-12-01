library(dplyr)
library(stringr)

hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")


str(hd)
dim(hd)
summary(hd)

str(gii)
dim(gii)
summary(gii)

colnames(hd)
colnames(gii)

colnames(hd) <- c("hdi_rank", "country","HDI","life_exp", "exp_education", "mean_education", "gni", "gni_minus_hdi")
colnames(gii) <- c("gii_rank", "country", "gii", "maternal_mortality_rat", "adolescent_birth_rat", "female_parliament", "fem_sec_ed", "male_sec_ed", "lfpr_fem", "lfpr_male")

gii <- mutate(gii, sec_ed_rat =  fem_sec_ed/male_sec_ed)

gii <- mutate(gii, lfpr_ratio = lfpr_fem/lfpr_male)

human <- inner_join(hd, gii, by = "country")

View(human)


###Week 5

str(human)
human$gni <- str_replace(human$gni, pattern=",", replace ="") %>% as.numeric()
human <- select(human, - hdi_rank, - HDI, - mean_education,  - gni_minus_hdi, -gii_rank,  - gii, -fem_sec_ed,  - male_sec_ed, -lfpr_fem, - lfpr_male)
colnames(human)

human <- filter(human, complete.cases(human) == T)
human$country


last <- nrow(human) - 7
human <- human[1:last, ]
rownames(human) <- human$country
human <- select(human, - country)

write.csv(human, file = "human.csv", row.names = T)
