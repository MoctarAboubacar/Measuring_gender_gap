### Is there a systematic bias against women in [organization]?
# Do women, controlling for position rank, contract, head of unit status, unit of work and duty station location, have lower salaries than men?
# Note that this analysis cannot capture systematic hiring of women at positions lower than their years of experience...for that we would need each individual's years of experience. Nor for lack of data can it account for variation in pay due to staff being at a higher step than other staff. And given the results below, there is little evidence here to indicate that there is a gender pay gap in [organization], in 2018/2019.

#### setup ####
## packages
require(tidyverse)
require(ggthemes)
require(stargazer)
require(lmtest)
require(sandwich)


## directory and load data
setwd("C:/Users/Leurre/Desktop")
file <- "2019staffcosts.csv"
dat <- read_csv(file)
glimpse(dat)

#### data cleaning ####
dat$Unit <- as.factor(dat$Unit)
dat$Location <- as.factor(dat$Location)

dat <- dat%>%
  mutate(location = as.factor(case_when(Location == "CO" ~ "CO",
                                 Location == "CO_Ministry" ~ "CO",
                                 Location == "CO_NPC" ~ "CO",
                                 Location == "Dhangadhi" ~ "Field",
                                 Location == "Field" ~ "Field",
                                 Location == "Nepalgunj" ~ "Field",
                                 Location == "SO_Doti" ~ "Field",
                                 Location == "SO_Surkhet" ~ "Field")))

# Female var as factor
dat <- dat%>%
  filter(MA_Female != "?" & MA_Female != "NA")
dat$MA_Female <- factor(dat$MA_Female,
                        levels = c(0,1),
                        labels = c("Male", "Female"))

# Nat'l and int'l as factor
dat$`Staff Category` <- factor(dat$`Staff Category`,
                               labels = c("International", "National"))

# contract type as factor
dat <- dat%>%
  mutate(contract =  (case_when(`Contract Type` == "FTP" ~ "FTP",
                      `Contract Type` == "CST" ~ "CST",
                      `Contract Type` == "FT NO" ~ "FT",
                      `Contract Type` == "FT" ~ "FT",
                      `Contract Type` == "SC NO" ~ "SC",
                      `Contract Type` == "SSA NO" ~ "SSA",
                      `Contract Type` == "FT GS" ~ "FT",
                      `Contract Type` == "SSA GS" ~ "SSA",
                      `Contract Type` == "SC GS" ~ "SC")))
dat$contract <- factor(dat$contract)

# unit within the office as factor
dat$Unit <- as.factor(dat$Unit)

#### EDA ####
# structure of data
glimpse(dat)

# dealing with outliers and special cases
dat.skew <- dat%>%
  filter(Permonth < 13000) # 13000 is pretty arbitrary but it right skews out after like 8k or so, so this is ok
dat.natl <- dat%>%
  filter(`Staff Category` == "National")
dat.intl <- dat%>%
  filter(`Staff Category` == "International")

#natl vs intl staff
dat%>%
  group_by(`Staff Category`)%>%
  summarize(mean(Permonth),
            median(Permonth))
ggplot(dat.skew, aes(x = `Staff Category`, y = Permonth))+
  geom_boxplot()+
  ggtitle("Monthly Salaries for National and International Staff")+
  ylab("")

# distribution of monthly salaries (removed outliers)
ggplot(dat.skew, aes(Permonth))+
  geom_histogram()

# Natl staff salary distribution
ggplot(dat.natl, aes(Permonth, fill = MA_Female))+
  geom_histogram()

# Intl staff salary distribution
ggplot(dat.intl, aes(Permonth, fill = MA_Female))+
  geom_histogram()

# men v women, national staff salaries boxplot
ggplot(dat.natl, aes(x = MA_Female, y = Permonth))+
  geom_boxplot()+
  ylab("")+
  xlab("Gender")+
  ggtitle("National Staff Salary Distritbution")


# what is the ratio of women to men in the CO?
sum(dat$MA_Female == "Female")/ nrow(dat) # 32.8%, or 42 people

# What is the distribution of salaries for women vs. men in the CO?
ggplot(dat.skew, aes(x = MA_Female, y = Permonth))+
  geom_boxplot() #medians are similar, with more spread for women and more high outliers for men

# by contract type, alaries are distributed as expected
ggplot(dat, aes(x = contract, y = Permonth))+
  geom_boxplot()

# by position rank + contract type, salary looks about as expected
ggplot(dat, aes(x = MA_position_rank, y = Permonth, color = contract))+
  geom_jitter()

# Of the 42 women, a higher percentage are SSAs as well as consultants, compared to men
ggplot(dat, aes(x = MA_Female, fill = contract))+
  geom_bar(position = "fill")+
  labs(title = "Percentage per Contract Type",
       caption = "Note: CST is international consultant, FT is national fixed term, FTP is fixed term professional, SC is service contract, SSA is special seervice agreement")+
  ylab("")
ggplot(dat, aes(x = MA_Female, fill = contract))+
  geom_bar(position = "stack")

# In SSAs and Consultancies, women represent a larger % than men. However women make up only between 25% and 33% of FTs, international FTs and SC contracts
ggplot(dat, aes(x = contract, fill = MA_Female))+
  geom_bar(position = "fill")

# women in different units
ggplot(dat, aes(x = Unit, fill = MA_Female))+
  geom_bar(position = "fill")

ggplot(dat, aes(x = MA_position_rank, fill = MA_Female))+
  geom_bar()+
  labs(title = "Staff Grade Ranking by Gender",
       x = "Grade Ranking",
       y = "Count",
       caption = "Note: ranking from 1 (most senior) to 13 (least senior)")

#### regression modelling ####
# get dataframe to use: The issue is [persons 1 and 2]. We should not include their salaries as-is because they are When Actually Employed. We delete these two cases
# FTPS are excluded from the analysis because their process of selection is international, and so does not reflect on the hiring practices within the Country Office

dat.model <- dat%>%
  filter(Name != "Person 1",
         Name != "person 2")%>%
  mutate(lnwage = log(Permonth))

# Model X: structural
modelx <- lm(lnwage ~ contract + location + MA_unit_head, data = dat.model)
summary(modelx)
# Model Y: Personal attainment
modely <- lm(lnwage ~ contract + location + MA_unit_head + MA_position_rank, data = dat.model)
summary(modely)
#Model Z: Personal traits
modelz <- lm(lnwage ~ contract + location + MA_unit_head + MA_position_rank + MA_Female, data = dat.model)
summary(modelz)

# regression diagnostics
plot(modelx)
plot(modely)
plot(modelz)

# robust standard errors - decrease the p value of Female but only by a bit. Made Management unit statistically significant, and increased the p value of Unit head (remains stat sig)
coeftest(modelz, vcov. = vcovHC(model3, type = "HC1"))

# extract the models in a table in html (LaTex is for another day...)
stargazer(modelx, modely, modelz,
          type = "html",
          dep.var.labels = "Log Monthly Salary",
          covariate.labels = c("FT (vs CST)", "FTP (vs CST)", "SC (vs CST)", "SSA (vs CST)", "Field posting",  "Unit head", "Position rank", "Female"),
          header = F,
          out = "Models.htm")

# The assumption of residual independence: we estimate that the clustering of staff within units and contract type addresses the main clustered structure of the data

# The assumption of residual normality

#regression assumptions are 
#(1) unbiased error--linearity assumption
#(2) no measurement error
#(3) residual independence --> you can assume residuals independence if you've sampled properly from a non-clustered population
#(4) residual normality --> inspect the distribution of standardized residuals (Normal Quantile plots)
#(5) residual homoscedasticity --> plot residuals against predicted value; plot residuals against predictors. Beware of formalized tests for this



