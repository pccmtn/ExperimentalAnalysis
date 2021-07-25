library(tidyverse)
library(ggplot2)
library(data.table)
library(QuantPsyc)
library(stargazer)

data_new <- Cog_Diss_Final_Final_July_13_2021_10_24
data_final <- data_new[- 1, ]
data_f <- data_final[- 1, ]



thermoc <- c('QID94_1', "QID96_1", "QID98_1", "QID100_4")

data_f <- data_f %>% 
  pivot_longer(., thermoc, 
               names_to = NULL,
               values_to = 'ThermoCandidatePre') %>% 
  filter(!is.na(ThermoCandidatePre))

thermoo <- c("QID95_1", "QID97_1", "QID99_1", "QID101_1")

data_f <- data_f %>% 
  pivot_longer(., thermoo, 
               names_to = NULL,
               values_to = 'ThermoOpponentPre') %>% 
  filter(!is.na(ThermoOpponentPre))


data_f <- data_f %>% 
  rename(ID = QID4,PriorEcon = QID92_1, PriorSocial = QID93_1, AttitudeEconPre = QID18_9, BeliefsElection = QID30_1, PostEcon = QID88_1, PostSocial = QID89_1, AttitudeEconPost = Q119_9, ThermoCandidatePost =QID110_1, ThermoOpponentPost = Q116_1 ,Support = clicks, Bias = QID24, Age = QID7, Gender = QID8, Income = QID9, Ethicity = QID10, Political = QID12)

data_f <- data_f %>% 
  rename(Time = `Duration (in seconds)`)

##### select the variables


df <- data_f %>% 
  dplyr::select(ID, PriorEcon, PriorSocial, AttitudeEconPre, ThermoCandidatePre, ThermoOpponentPre, BeliefsElection, PostEcon, PostSocial, AttitudeEconPost, ThermoCandidatePost, ThermoOpponentPost, Support, Bias, Age, Gender, Income, Ethicity, Political, Time, Candidate, Treatment, Win)

### transform the variables

df <- df %>% 
  mutate(Gender = ifelse(Gender == 'Female', 1, 0))

df <- df %>% 
  mutate(Income = as.factor(Income))

df <- df %>% 
  mutate(Age = as.factor(Age))

df <- df %>% 
  mutate(White = ifelse(Ethicity == 'Caucasian/White', 1, 0))

df <- df %>% 
  mutate(Income = as.factor(Income))

df <- df %>% 
  mutate(Political = as.factor(Political))

df <- df %>% 
  mutate(Bias = as.factor(Bias))



df <- df %>% 
  mutate(Candidate = as.factor(Candidate))

df <- df %>% 
  mutate(Treatment = as.factor(Treatment))

df <- df %>% 
  mutate(Win = as.factor(Win))

df <- df %>% 
  mutate(PriorEcon = as.integer(PriorEcon))

df <- df %>% 
  mutate(PriorSocial = as.integer(PriorSocial))

df <- df %>% 
  mutate(AttitudeEconPre = as.integer(AttitudeEconPre))

df <- df %>% 
  mutate(AttitudeSocialPre = as.integer(100 - AttitudeEconPre))

df <- df %>% 
  mutate(ThermoCandidatePre = as.integer(ThermoCandidatePre))

df <- df %>% 
  mutate(ThermoOpponentPre = as.integer(ThermoOpponentPre))

df <- df %>% 
  mutate(BeliefsElection = as.integer(BeliefsElection))

df <- df %>% 
  mutate(PostEcon = as.integer(PostEcon))


df <- df %>% 
  mutate(PostSocial = as.integer(PostSocial))

df <- df %>% 
  mutate(AttitudeEconPost = as.integer(AttitudeEconPost))

df <- df %>% 
  mutate(AttitudeSocialPost = as.integer(100 - AttitudeEconPost))

df <- df %>% 
  mutate(ThermoCandidatePost = as.integer(ThermoCandidatePost))

df <- df %>% 
  mutate(ThermoOpponentPost = as.integer(ThermoOpponentPost))



df <- df %>% 
  mutate(Support = as.integer(Support))

df <- df %>% 
  mutate(Time = as.integer(Time))

df <- df %>% 
  mutate(Bonus = ifelse((Treatment == 2 & Support > 31) | (Treatment == 3 & Support > 526) | (Treatment == 1 & Win == 1 & Support > 31), 1, 0 ))


############## I have pooled people in the two treatments in one unique treatment for the main analysis

df <- df %>% 
  mutate(Treated = ifelse(Treatment == 2 | Treatment == 3, 1, 0))


########## Descriptive Statistics and randomization checks

mean(df$Time)

by(df$Gender, df$Treated, mean)

df %>% group_by(Age, Treated) %>% tally() %>% ungroup %>% pivot_wider(id_cols = Age, names_from = Treated, values_from = n)

df %>% group_by(White, Treated) %>% tally() %>% ungroup %>% pivot_wider(id_cols = White, names_from = Treated, values_from = n)

df %>% group_by(Political, Treated) %>% tally() %>% ungroup %>% pivot_wider(id_cols = Political, names_from = Treated, values_from = n)

df %>% group_by(Candidate, Treated) %>% tally() %>% ungroup %>% pivot_wider(id_cols = Candidate, names_from = Treated, values_from = n)

df %>% group_by(Bias, Treated) %>% tally() %>% ungroup %>% pivot_wider(id_cols = Bias, names_from = Treated, values_from = n)

by(df$PriorEcon, df$Treated, mean)

by(df$PriorSocial, df$Treated, mean)

by(df$Support, df$Treated, mean)


by(df$AttitudeEconPre, df$Treated, mean)

by(df$AttitudeSocialPre, df$Treated, mean)

t.test(df$AttitudeEconPre,df$AttitudeSocialPre , alternative = "two.sided", paired = T)

t.test(df$AttitudeEconPost,df$AttitudeSocialPost , alternative = "two.sided", paired = T)

t.test(df$AttitudeEconPre,df$AttitudeEconPost , alternative = "two.sided", paired = T)

t.test(df$AttitudeSocialPre,df$AttitudeSocialPost , alternative = "two.sided", paired = T)

############ Compute a measure of Econ distance and Social distance. People are on average further from their candidates along the Social Dimension

df <- df %>% 
  mutate(EconDistance = ifelse(Candidate == "X" | Candidate == "W", abs(PriorEcon - 90), abs(PriorEcon - 10)))

df <- df %>% 
  mutate(SocialDistance = ifelse(Candidate == "X" | Candidate == "Y", abs(PriorSocial - 90), abs(PriorSocial - 10)))


t.test(df$SocialDistance,df$EconDistance , alternative = "two.sided", paired = T)




###### Polarization as distance from your candidate

df <- df %>% 
  mutate(EconPolarization = ifelse(Candidate == "X" | Candidate == "W", abs(PriorEcon - 90) - abs(PostEcon - 90), abs(PriorEcon - 10) - abs(PostEcon - 10)))

df <- df %>% 
  mutate(SocialPolarization = ifelse(Candidate == "X" | Candidate == "Y", abs(PriorSocial - 90) - abs(PostSocial - 90), abs(PriorSocial - 10) - abs(PostSocial - 10)))

####### People move closer to their candidates along the Social dimension which was the one they were the furthest

t.test(df$SocialPolarization,df$EconPolarization , alternative = "two.sided", paired = T)


######### Compute the index of global polarization

df <- df %>% 
  mutate(GlobalPolarization = EconPolarization + SocialPolarization)

####### Weighted polarization

df <- df %>% 
  mutate(WEconPolarization = ifelse(Candidate == "X" | Candidate == "W", (AttitudeEconPre/100)*(abs(PriorEcon - 90) - abs(PostEcon - 90)),(AttitudeEconPre/100)*( abs(PriorEcon - 10) - abs(PostEcon - 10))))

df <- df %>% 
  mutate(WSocialPolarization = ifelse(Candidate == "X" | Candidate == "Y", (AttitudeSocialPre/100)*(abs(PriorSocial - 90) - abs(PostSocial - 90)),(AttitudeSocialPre/100)*( abs(PriorSocial - 10) - abs(PostSocial - 10))))

df <- df %>% 
  mutate(WGlobalPolarization = WEconPolarization + WSocialPolarization)


########### Assess whether people in the treatment move closer to their candidates along the Econ dimension


t.test(df$EconPolarization[df$Treated == 0], df$EconPolarization[df$Treated == 1], alternative = "two.sided", paired = F)

by(df$EconPolarization, df$Treated, mean)

########### Assess whether people in the treatment move closer to their candidates along the Social dimension


t.test(df$SocialPolarization[df$Treated == 0], df$SocialPolarization[df$Treated == 1], alternative = "two.sided", paired = F)

by(df$SocialPolarization, df$Treated, mean)

########### Assess whether people in the treatment move closer to their candidates globally


t.test(df$GlobalPolarization[df$Treated == 0], df$GlobalPolarization[df$Treated == 1], alternative = "two.sided", paired = F)

by(df$GlobalPolarization, df$Treated, mean)

########### Assess whether people in the treatment move closer to their candidates globally (weighted)


t.test(df$WGlobalPolarization[df$Treated == 0], df$WGlobalPolarization[df$Treated == 1], alternative = "two.sided", paired = F)

by(df$WGlobalPolarization, df$Treated, mean)


########## Polarization as a measure of Euclidean distance

df <- df %>% 
  mutate(EconPolarizationPre = ifelse(Candidate == "X" | Candidate == "W", (PriorEcon - 90)^2 , (PriorEcon - 10)^2))

df <- df %>% 
  mutate(SocialPolarizationPre = ifelse(Candidate == "X" | Candidate == "Y", (PriorSocial - 90)^2 , (PriorSocial - 10)^2))

df <- df %>% 
  mutate(EconPolarizationPost = ifelse(Candidate == "X" | Candidate == "W", (PostEcon - 90)^2, (PostEcon - 10)^2))

df <- df %>% 
  mutate(SocialPolarizationPost = ifelse(Candidate == "X" | Candidate == "Y", (PostSocial - 90)^2, (PostSocial - 10)^2))

df <- df %>% 
  mutate(EuclideanPolarizationPre = sqrt(EconPolarizationPre+SocialPolarizationPre))

df <- df %>% 
  mutate(EuclideanPolarizationPost = sqrt(EconPolarizationPost+SocialPolarizationPost))

df <- df %>% 
  mutate(DiffPol = (EuclideanPolarizationPre - EuclideanPolarizationPost))


########################## Weighted Euclidean distance


df <- df %>% 
  mutate(WEconPolarizationPre = ifelse(Candidate == "X" | Candidate == "W", (AttitudeEconPre/100)*(PriorEcon - 90)^2 , (AttitudeEconPre/100)*(PriorEcon - 10)^2))

df <- df %>% 
  mutate(WSocialPolarizationPre = ifelse(Candidate == "X" | Candidate == "Y", (AttitudeSocialPre/100)*(PriorSocial - 90)^2 ,(AttitudeSocialPre/100)*(PriorSocial - 10)^2))

df <- df %>% 
  mutate(WEconPolarizationPost = ifelse(Candidate == "X" | Candidate == "W", (AttitudeEconPost/100)*(PostEcon - 90)^2, (AttitudeEconPost/100)*(PostEcon - 10)^2))

df <- df %>% 
  mutate(WSocialPolarizationPost = ifelse(Candidate == "X" | Candidate == "Y", (AttitudeSocialPost/100)*(PostSocial - 90)^2, (AttitudeSocialPost/100)*(PostSocial - 10)^2))

df <- df %>% 
  mutate(WEuclideanPolarizationPre = sqrt(WEconPolarizationPre+WSocialPolarizationPre))

df <- df %>% 
  mutate(WEuclideanPolarizationPost = sqrt(WEconPolarizationPost+WSocialPolarizationPost))

df <- df %>% 
  mutate(WDiffPol = (WEuclideanPolarizationPre - WEuclideanPolarizationPost))

######### Pooling all of the observations there is no difference in pre and post polarization. We need to explore it by treatment

t.test(df$EuclideanPolarizationPre[df$Treated == 1],df$EuclideanPolarizationPost[df$Treated == 1], alternative = "two.sided", paired = T)

t.test(df$DiffPol[df$Treated == 0],df$DiffPol[df$Treated == 1] , alternative = "two.sided", paired = F)

t.test(df$WEuclideanPolarizationPre[df$Treated == 1],df$WEuclideanPolarizationPost[df$Treated == 1], alternative = "two.sided", paired = T)

t.test(df$WDiffPol[df$Treated == 0],df$WDiffPol[df$Treated == 1] , alternative = "two.sided", paired = F)


#################### Dropping people who are inconsistent ####################

######### check if you like your candidate more than the Opponent. Being in the treatment does not differentially impact being inconsistent.

df <- df %>% 
  mutate(Error = ifelse(ThermoCandidatePre < ThermoOpponentPre, 1, 0))

by(df$Error, df$Treatment, mean)

fit <- lm(Error ~ 1 + Treated + Gender + Age + White + Political, df)
summary(fit)


stargazer(list(fit),intercept.bottom = TRUE, no.space = TRUE)



df1 <- df %>% 
  filter( Error == 0)


mean(df1$Time)

Gender <- df1 %>% 
  group_by(Gender, Treated) %>% 
  tally() %>% 
  group_by(Treated) %>% 
  mutate(s = sum(n)) %>% 
  ungroup %>% 
  mutate(p = n/s) %>% 
  ungroup %>% pivot_wider(id_cols = Gender, names_from = Treated, values_from = p) %>% 
  mutate(name = 'Gender', Gender = as.character(Gender)) %>% 
  rename(var = Gender) 

chisq.test(df1$Treated, df1$Gender, correct=FALSE)

Age <- df1 %>% 
  group_by(Age, Treated) %>% 
  tally() %>% 
  group_by(Treated) %>% 
  mutate(s = sum(n)) %>% 
  ungroup %>% 
  mutate(p = n/s) %>% 
  ungroup %>% pivot_wider(id_cols = Age, names_from = Treated, values_from = p)%>% 
  mutate(name = 'Age', Age = as.character(Age)) %>% 
  rename(var = Age)

chisq.test(df1$Treated, df1$Age, correct=FALSE)

White <- df1 %>% group_by(White, Treated) %>% 
  tally() %>% 
  group_by(Treated) %>% 
  mutate(s = sum(n)) %>% 
  ungroup %>% 
  mutate(p = n/s) %>% 
  ungroup %>% pivot_wider(id_cols = White, names_from = Treated, values_from = p) %>% 
  mutate(name = 'White', White = as.character(White))%>% 
  rename(var = White)

chisq.test(df1$Treated, df1$White, correct=FALSE)

Political <- df1 %>% group_by(Political, Treated) %>% tally() %>% 
  group_by(Treated) %>% 
  mutate(s = sum(n)) %>% 
  ungroup %>% 
  mutate(p = n/s) %>% 
  ungroup %>% pivot_wider(id_cols = Political, names_from = Treated, values_from = p) %>% 
  mutate(name = 'Political', Political = as.character(Political)) %>% 
  rename(var = Political)

chisq.test(df1$Treated, df1$Political, correct=FALSE)

bind_rows(Gender, Age) %>% 
  bind_rows(., White) %>% 
  bind_rows(., Political) %>% 
  dplyr::select(-name) %>% 
  xtable::xtable(digits = 3) %>% 
  xtable::print.xtable(include.rownames = F)

Candidate <- df1 %>% group_by(Candidate, Treated) %>% 
  tally() %>% 
  group_by(Treated) %>% 
  mutate(s = sum(n)) %>% 
  ungroup %>% 
  mutate(p = n/s) %>% 
  ungroup %>%  pivot_wider(id_cols = Candidate, names_from = Treated, values_from = p)

chisq.test(df1$Treated, df1$Candidate, correct=FALSE)


Candidate %>% 
  xtable::xtable(digits = 3) %>% 
  xtable::print.xtable(include.rownames = F)

df1 %>% group_by(Bias, Treated) %>% 
  tally() %>% 
  group_by(Treated) %>% 
  mutate(s = sum(n)) %>% 
  ungroup %>% 
  mutate(p = n/s) %>% 
  ungroup %>% pivot_wider(id_cols = Bias, names_from = Treated, values_from = p)

by(df1$PriorEcon, df1$Treated, mean)

t.test(df1$PriorEcon[df1$Treated==0],df1$PriorEcon[df1$Treated==1] , alternative = "two.sided", paired = F)


by(df1$PriorSocial, df1$Treated, mean)

t.test(df1$PriorSocial[df1$Treated==0],df1$PriorSocial[df1$Treated==1] , alternative = "two.sided", paired = F)



######### TABLE

##########################

############ Compute a measure of Econ distance and Social distance. People are on average further from their candidates along the Social Dimension



t.test(df1$SocialDistance,df1$EconDistance , alternative = "two.sided", paired = T)





###### Polarization as distance from your candidate



####### People move closer to their candidates along the Social dimension which was the one they were the furthest

t.test(df$SocialPolarization,df$EconPolarization , alternative = "two.sided", paired = T)





########### Assess whether people in the treatment move closer to their candidates along the Econ dimension


t.test(df1$EconPolarization[df1$Treated == 0], df1$EconPolarization[df1$Treated == 1], alternative = "two.sided", paired = F)

t.test(df1$EconPolarization[df1$Treated == 0], df1$EconPolarization[df1$Treated == 1], alternative = "less", paired = F)


t.test(df1$EconPolarization[df1$Treated == 1], mu = 0, alternative = "greater")

######### GET IT


df1 <- df1 %>% 
  mutate(ChangeEconRel = EconPolarization/EconDistance)

df4 <- df1 %>% 
  filter(!is.na(ChangeEconRel))

df4 <- df4 %>% 
  filter(ChangeEconRel != "-Inf")

t.test(df4$ChangeEconRel[df1$Treated == 1], mu = 0, alternative = "two.sided")


by(df$EconPolarization, df$Treated, mean)

########### Assess whether people in the treatment move closer to their candidates along the Social dimension


t.test(df1$SocialPolarization[df1$Treated == 0], df1$SocialPolarization[df1$Treated == 1], alternative = "two.sided", paired = F)

t.test(df1$SocialPolarization[df1$Treated == 0], df1$SocialPolarization[df1$Treated == 1], alternative = "less", paired = F)


t.test(df1$SocialPolarization[df1$Treated == 1], mu = 0, alternative = "two.sided", paired = F)


######### GET IT


by(df1$SocialPolarization, df1$Treated, mean)

########### Assess whether people in the treatment move closer to their candidates globally


t.test(df1$GlobalPolarization[df1$Treated == 0], df1$GlobalPolarization[df1$Treated == 1], alternative = "two.sided", paired = F)

t.test(df1$GlobalPolarization[df1$Treated == 1], df1$GlobalPolarization[df1$Treated == 0], alternative = "greater", paired = F)


t.test(df1$GlobalPolarization, mu = 0, alternative = "two.sided")

t.test(df1$GlobalPolarization[df1$Treatment == 1], mu = 0, alternative = "two.sided")

t.test(df1$GlobalPolarization[df1$Treatment == 2], mu = 0, alternative = "two.sided")

t.test(df1$GlobalPolarization[df1$Treatment == 3], mu = 0, alternative = "two.sided")

t.test(df1$GlobalPolarization[df1$Treated == 1], mu = 0, alternative = "greater")

######### GET IT

by(df1$GlobalPolarization, df1$Treated, mean)

########### Assess whether people in the treatment move closer to their candidates globally (weighted)


t.test(df1$WGlobalPolarization[df1$Treated == 0], df1$WGlobalPolarization[df1$Treated == 1], alternative = "two.sided", paired = F)

t.test(df1$WGlobalPolarization[df1$Treated == 0], df1$WGlobalPolarization[df1$Treated == 1], alternative = "less", paired = F)


t.test(df1$WGlobalPolarization[df1$Treated == 1], mu = 0, alternative = "greater", paired = F)


######### GET IT


by(df$WGlobalPolarization, df$Treated, mean)



######### Pooling all of the observations there is no difference in pre and post polarization. We need to explore it by treatment

t.test(df1$EuclideanPolarizationPre[df1$Treated == 0],df1$EuclideanPolarizationPost[df1$Treated == 0], alternative = "greater", paired = T)

t.test(df1$EuclideanPolarizationPre[df1$Treated == 1],df1$EuclideanPolarizationPost[df1$Treated == 1], alternative = "greater", paired = T)

######### GET IT

t.test(df1$DiffPol[df1$Treated == 0],df1$DiffPol[df1$Treated == 1] , alternative = "two.sided", paired = F)

t.test(df1$DiffPol[df1$Treated == 0],df1$DiffPol[df1$Treated == 1] , alternative = "less", paired = F)

######### GET IT


######################## Graphs ##############################
##############################################################



df2 <- df1 %>% 
  group_by(Candidate) %>% summarise(Average_EconPre = mean(PriorEcon), Average_SocialPre = mean(PriorSocial), Average_EconPost = mean(PostEcon), Average_SocialPost = mean(PostSocial))

df2 <- df2 %>% 
  filter(Candidate == 'X'| Candidate == 'Z' | Candidate == 'Y'| Candidate == 'W')

###################### Graph pooling all the observations

(df2 %>% ggplot(aes(x = c(0:100), y = c(0:100)))+
    geom_point(aes(x = Average_EconPre, y = Average_SocialPre, color = 'Pre-Support Preference'))+
    geom_point(aes(x = Average_EconPost, y = Average_SocialPost, color = 'Post-Support Preferences'))+
    geom_point(aes(x = 90, y = 90, color = 'Candidate X'))+
    geom_point(aes(x = 10, y = 10, color = 'Candidate Z'))+
    geom_point(aes(x = 10, y = 90, color = 'Candidate Y'))+
    geom_point(aes(x = 90, y = 10, color = 'Candidate W'))+
    coord_cartesian(xlim = c(0,100), ylim = c(0,100)) +
    labs(x = 'Economic Issue Position', y= 'Social Issue Position', color = NULL)) %>% ggsave(filename = 'Polarization.png', device = 'png', units = 'in', width = 6, height = 4)

###################### Graph Control and Treatment

df3 <- df1 %>% 
  group_by(Candidate, Treated) %>% 
  summarise(Average_EconPre = mean(PriorEcon), 
            Average_SocialPre = mean(PriorSocial), 
            Average_EconPost = mean(PostEcon), 
            Average_SocialPost = mean(PostSocial))

df3 <- df3 %>% 
  filter(Candidate == 'X'| Candidate == 'Z' | Candidate == 'Y'| Candidate == 'W') 


(df3 %>% ggplot(aes(x = c(0:100), y = c(0:100)))+
    geom_point(aes(x = Average_EconPre, y = Average_SocialPre, color = 'Pre-Support Preference'))+
    geom_point(aes(x = Average_EconPost, y = Average_SocialPost, color = 'Post-Support Preferences'))+
    geom_point(aes(x = 90, y = 90, color = 'Candidate X'))+
    geom_point(aes(x = 10, y = 10, color = 'Candidate Z'))+
    geom_point(aes(x = 10, y = 90, color = 'Candidate Y'))+
    geom_point(aes(x = 90, y = 10, color = 'Candidate W'))+
    coord_cartesian(xlim = c(0,100), ylim = c(0,100)) +
    labs(x = 'Economic Issue Position', y= 'Social Issue Position', color = NULL)) %>% ggsave(filename = 'Polarization.png', device = 'png', units = 'in', width = 6, height = 4)

uniqueInitials <- c('W', 'X', 'Y', 'Z')
initialShapes <- unlist(lapply(uniqueInitials, utf8ToInt))

(df3 %>% 
    mutate(Treated = ifelse(Treated == 1, 'Treatment', 'Control'),
           Candidate = paste0('Candidate', Candidate)) %>% 
    ggplot(aes(x = c(0:100), y = c(0:100)))+
    geom_point(aes(x = Average_EconPre, y = Average_SocialPre, 
                   color = 'Pre-Support Preference'), size = 3)+
    geom_point(aes(x = Average_EconPost, y = Average_SocialPost, 
                   color = 'Post-Support Preferences'), size = 3)+
    geom_point(aes(x = 90, y = 90), color = 'red', shape = utf8ToInt('X'), size = 5)+
    geom_point(aes(x = 10, y = 10), color = 'forestgreen', shape = utf8ToInt('Z'), size = 5)+
    geom_point(aes(x = 10, y = 90), color = 'blue', shape = utf8ToInt('Y'), size = 5)+
    geom_point(aes(x = 90, y = 10), color = 'orange', shape = utf8ToInt('W'), size = 5)+
    coord_cartesian(xlim = c(0,100), ylim = c(0,100)) +
    theme_bw()  + 
    facet_wrap(.~Treated) +
    guides( shape = F) +
    labs(x = 'Economic Issue Position', y= 'Social Issue Position', color = NULL) ) %>% 
  ggsave(filename = 'Polarization_by_treatment.png', device = 'png', units = 'in', width = 8, height = 4)




############ Regression Analysis #######################


########## polarization as index

fit1 <- lm(EconPolarization ~ 1 + Treated +  ThermoCandidatePre  + Gender + Age + White + Political, df1)
summary(fit1)

fit2 <- lm(SocialPolarization ~ 1 + Treated +  ThermoCandidatePre  + Gender + Age + White + Political, df1)
summary(fit2)

fit3 <- lm(GlobalPolarization ~ 1 + Treated + ThermoCandidatePre + Gender + Age + White + Political, df1)
summary(fit3)

fit4 <- lm(WGlobalPolarization ~ 1 + Treated + ThermoCandidatePre  + Gender + Age + White + Political, df1)
summary(fit4)




fit11 <- lm(EconPolarization ~ 1 + Support  + BeliefsElection +  ThermoCandidatePre  + Gender + Age + White + Political, df1 %>% filter(Treated==1))
summary(fit11)

fit21 <- lm(SocialPolarization ~ 1 + Support  + BeliefsElection +  ThermoCandidatePre  + Gender + Age + White + Political, df1 %>% filter(Treated==1))
summary(fit21)

fit31 <- lm(GlobalPolarization ~ 1 + Support + BeliefsElection + ThermoCandidatePre + Gender + Age + White + Political, df1 %>% filter(Treated==1))
summary(fit31)

fit41 <- lm(WGlobalPolarization ~ 1 + Support + BeliefsElection + ThermoCandidatePre  + Gender + Age + White + Political, df1 %>% filter(Treated==1))
summary(fit41)


stargazer(list(fit1, fit2, fit3, fit4),intercept.bottom = TRUE, no.space = TRUE)
stargazer(list(fit11, fit21, fit31, fit41),intercept.bottom = TRUE, no.space = TRUE)


########## polarization as Euclidean distance. We regress the change in euclidean distance on treatment dummy and then we focus on the treated to check the drivers of this preference change



fit111 <- lm(DiffPol ~ 1 + Treated +  ThermoCandidatePre  + Gender + Age + White + Political, df1)
summary(fit111)

fit112 <- lm(DiffPol ~ 1 + Support  + BeliefsElection +  ThermoCandidatePre  + Gender + Age + White + Political, df1 %>% filter(Treated==1))
summary(fit112)



stargazer(list(fit111),intercept.bottom = TRUE, no.space = TRUE)

stargazer(list(fit112),intercept.bottom = TRUE, no.space = TRUE)



#################Secondary Analysis#######################
###########################################################


################### Change in Attitudes Before and After Election

t.test(df1$AttitudeEconPre, df1$AttitudeSocialPre, alternative = "two.sided", paired = T)

t.test(df1$AttitudeEconPre[df1$Treated == 0], df1$AttitudeEconPre[df1$Treated == 1], alternative = "two.sided", paired = F)

t.test(df1$AttitudeEconPre[df1$Treated == 0], df1$AttitudeEconPost[df1$Treated == 0], alternative = "two.sided", paired = T)

t.test(df1$AttitudeEconPre[df1$Treated == 1], df1$AttitudeEconPost[df1$Treated == 1], alternative = "two.sided", paired = T)


t.test(df1$AttitudeEconPost[df1$Treated == 0], df1$AttitudeEconPost[df1$Treated == 1], alternative = "two.sided", paired = F)

############### GET IT


df1 <- df1 %>% 
  mutate(DiffAttitudeEcon = AttitudeEconPost-AttitudeEconPre)

t.test(df1$DiffAttitudeEcon[df1$Treated == 0], df1$DiffAttitudeEcon[df1$Treated == 1], alternative = "less", paired = F)


fitAttitude <- lm(DiffAttitudeEcon ~ 1 + Treated + ThermoCandidatePre  + Gender + Age + White + Political, df1 )
summary(fitAttitude)

stargazer(list(fitAttitude),intercept.bottom = TRUE, no.space = TRUE)



################### 

df1 <- df1 %>% 
  mutate(EconImportance = ifelse(AttitudeEconPre >50, 1, 0))

df1 <- df1 %>% 
  mutate(PrefChange = ifelse(EconImportance == 1, EconPolarization - SocialPolarization, SocialPolarization-EconPolarization ))

t.test(df1$PrefChange, mu = 0, alternative = "greater")

t.test(df1$PrefChange[df1$Treated == 1], mu = 0, alternative = "greater")

####### GET IT


############ Distance

df1 <- df1 %>% 
  mutate(Distance = ifelse(EconDistance >SocialDistance, 1, 0))

df1 <- df1 %>% 
  mutate(PrefChangeD = ifelse(Distance == 1, EconPolarization - SocialPolarization, SocialPolarization-EconPolarization ))

t.test(df1$PrefChangeD, mu = 0, alternative = "greater")

t.test(df1$PrefChangeD[df1$Treated == 1], mu = 0, alternative = "greater")

####### GET IT

################### Change in ThermoCandidate Before and After Election


t.test(df1$ThermoCandidatePre[df1$Treated == 0], df1$ThermoCandidatePre[df1$Treated == 1], alternative = "two.sided", paired = F)

t.test(df1$ThermoCandidatePre[df1$Treated == 0], df1$ThermoCandidatePost[df1$Treated == 0], alternative = "two.sided", paired = T)

t.test(df1$ThermoCandidatePre[df1$Treated == 1], df1$ThermoCandidatePost[df1$Treated == 1], alternative = "two.sided", paired = T)


t.test(df1$ThermoCandidatePost[df1$Treated == 0], df1$ThermoCandidatePost[df1$Treated == 1], alternative = "two.sided", paired = F)

###### GET IT

df1 <- df1 %>% 
  mutate(DiffThermoCand = ThermoCandidatePost-ThermoCandidatePre)

fitCandidate <- lm(DiffThermoCand ~ 1 + Treated + Gender + Age + White + Political, df1 )
summary(fitCandidate)

stargazer(list(fitCandidate),intercept.bottom = TRUE, no.space = TRUE)

################### Change in ThermoOpponent Before and After Election


t.test(df1$ThermoOpponentPre[df1$Treated == 0], df1$ThermoOpponentPre[df1$Treated == 1], alternative = "two.sided", paired = F)

t.test(df1$ThermoOpponentPre[df1$Treated == 0], df1$ThermoOpponentPost[df1$Treated == 0], alternative = "two.sided", paired = T)

t.test(df1$ThermoOpponentPre[df1$Treated == 1], df1$ThermoCandidatePost[df1$Treated == 1], alternative = "two.sided", paired = T)


t.test(df1$ThermoOpponentPost[df1$Treated == 0], df1$ThermoOpponentPost[df1$Treated == 1], alternative = "two.sided", paired = F)

########### GET IT

df1 <- df1 %>% 
  mutate(DiffThermoOpp = ThermoOpponentPost-ThermoOpponentPre)

fitOpponent <- lm(DiffThermoOpp ~ 1 + Treated + Gender + Age + White + Political, df1 )
summary(fitOpponent)

stargazer(list(fitOpponent),intercept.bottom = TRUE, no.space = TRUE)

################### Differential Support of Winning and Losing 

df1 <- df1 %>% 
  mutate(Winning = ifelse(Bonus == 1, 1, 0)) 

################### Impact on measures of Polarization index

fit1111 <- lm(EconPolarization ~ 1 + Winning +  ThermoCandidatePre  + Gender + Age + White + Political, df1 %>% filter(Treated==1))
summary(fit1111)

fit2222 <- lm(SocialPolarization ~ 1 + Winning +  ThermoCandidatePre  + Gender + Age + White + Political, df1 %>% filter(Treated==1))
summary(fit2222)

fit3333 <- lm(GlobalPolarization ~ 1 + Winning + ThermoCandidatePre + Gender + Age + White + Political, df1 %>% filter(Treated==1))
summary(fit3333)

fit4444 <- lm(WGlobalPolarization ~ 1 + Winning + ThermoCandidatePre  + Gender + Age + White + Political, df1 %>% filter(Treated==1))
summary(fit4444)


fit11111 <- lm(EconPolarization ~ 1 + Support  + Winning + BeliefsElection +  ThermoCandidatePre  + Gender + Age + White + Political, df1 %>% filter(Treated==1))
summary(fit11111)

fit22222 <- lm(SocialPolarization ~ 1 + Support + Winning  + BeliefsElection +  ThermoCandidatePre  + Gender + Age + White + Political, df1 %>% filter(Treated==1))
summary(fit22222)

fit33333 <- lm(GlobalPolarization ~ 1 + Support + Winning + BeliefsElection + ThermoCandidatePre + Gender + Age + White + Political, df1 %>% filter(Treated==1))
summary(fit33333)

fit44444 <- lm(WGlobalPolarization ~ 1 + Support + Winning + BeliefsElection + ThermoCandidatePre  + Gender + Age + White + Political, df1 %>% filter(Treated==1))
summary(fit44444)


stargazer(list(fit1111, fit2222, fit3333, fit4444),intercept.bottom = TRUE, no.space = TRUE)


stargazer(list(fit11111, fit22222, fit33333, fit44444 ),intercept.bottom = TRUE, no.space = TRUE)

################### Impact on Polarization as Euclidean distance


fit11112 <- lm(DiffPol ~ 1 + Winning +  ThermoCandidatePre  + Gender + Age + White + Political, df1 %>% filter(Treated==1))
summary(fit11112)

fit11122 <- lm(DiffPol ~ 1 + Support + Winning  + BeliefsElection +  ThermoCandidatePre  + Gender + Age + White + Political, df1 %>% filter(Treated==1))
summary(fit11122)

fit11132 <- lm(WDiffPol ~ 1 + Winning +  ThermoCandidatePre  + Gender + Age + White + Political, df1 %>% filter(Treated==1))
summary(fit11132)

fit11142 <- lm(WDiffPol ~ 1 + Support + Winning  + BeliefsElection +  ThermoCandidatePre  + Gender + Age + White + Political, df1 %>% filter(Treated==1))
summary(fit11142)

stargazer(list(fit11112, fit11132),intercept.bottom = TRUE, no.space = TRUE)


stargazer(list(fit11122, fit11142),intercept.bottom = TRUE, no.space = TRUE)


################### Differential Support of Winning and Losing 

################### Change in ThermoCandidate Before and After Election


DiffThermoCand <- lm(DiffThermoCand ~ 1 + Support  + Winning + BeliefsElection  + Gender + Age + White + Political, df1 %>% filter(Treated==1))
summary(DiffThermoCand)

stargazer(list(DiffThermoCand),intercept.bottom = TRUE, no.space = TRUE)


################### Change in ThermoOpponent Before and After Election


DiffThermoOpp <- lm(DiffThermoOpp ~ 1 + Support  + Winning + BeliefsElection + Gender + Age + White + Political, df1 %>% filter(Treated==1))
summary(DiffThermoOpp)


stargazer(list(DiffThermoCand, DiffThermoOpp),intercept.bottom = TRUE, no.space = TRUE)


################# How support affects beliefs about election


fitBeliefs <- lm(BeliefsElection ~ 1 + Support  + Gender + Age + White + Political + EconDistance + SocialDistance, df1 %>% filter(Treated==1))
summary(fitBeliefs)

stargazer(list(fitBeliefs),intercept.bottom = TRUE, no.space = TRUE)



################# How Candidates rating affects support


fitSupport <- lm(Support ~ 1 + ThermoCandidatePre  + Gender + Age + White + Political, df1 %>% filter(Treated==1))
summary(fitSupport)

stargazer(list(fitSupport),intercept.bottom = TRUE, no.space = TRUE)



################# 2SLS

library("ivreg")


m_iv1 <- ivreg(EconPolarization ~ 1 + ThermoCandidatePre + Gender + Age + White + Political |
                 Support + Gender + Age + White + Political,
               data=df1 %>% filter(Treated==1))
summary(m_iv1)


m_iv2 <- ivreg(SocialPolarization ~ 1 + ThermoCandidatePre + Gender + Age + White + Political |
                 Support + Gender + Age + White + Political,
               data=df1 %>% filter(Treated==1))
summary(m_iv2)



m_iv3 <- ivreg(GlobalPolarization ~ 1 + ThermoCandidatePre + Gender + Age + White + Political |
                 Support + Gender + Age + White + Political,
               data=df1 %>% filter(Treated==1))
summary(m_iv3)



m_iv4 <- ivreg(WGlobalPolarization ~ 1 + ThermoCandidatePre + Gender + Age + White + Political |
                 Support + Gender + Age + White + Political,
               data=df1 %>% filter(Treated==1))
summary(m_iv4)


stargazer(list(m_iv1, m_iv2, m_iv3, m_iv4),intercept.bottom = TRUE, no.space = TRUE)

confint(m_iv1, 'ThermoCandidatePre', level=0.95)
confint(m_iv2, 'ThermoCandidatePre', level=0.95)
confint(m_iv3, 'ThermoCandidatePre', level=0.95)
confint(m_iv4, 'ThermoCandidatePre', level=0.95)



