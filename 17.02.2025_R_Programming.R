#Working with factors and categorical variables
#Use forcats in R programming to change factor levels


library(tidyverse)
library(forcats)
install.packages("patchwork")
library(patchwork)
?forcats
data()
View(gss_cat)
names(gss_cat)

glimpse(gss_cat)

# The order of categorical variables matters
unique(gss_cat$race)

gss_cat %>% 
  pull(race) %>% 
  unique()

count(gss_cat,race,sort=T)

count(gss_cat,race, sort=T)


gss_cat %>% 
  count(race)

gss_cat %>% 
  pull(race) %>% 
  levels()

gss_cat %>% 
  pull(race) %>% 
  levels()

gss_cat %>% 
  select(race) %>% 
  table()


gss_cat %>% 
  select(race) %>% 
  table()


gss_cat %>% 
  mutate(race=fct_drop(race)) %>% 
  pull(race) %>% 
  levels()


gss_cat %>% 
  drop_na(tvhours) %>% 
  group_by(relig) %>% 
  summarise(mean_tv=mean(tvhours)) %>% 
  ggplot(aes(mean_tv,relig))+
  geom_point(size=4)


gss_cat %>% 
  drop_na(tvhours) %>% 
  group_by(relig) %>% 
  summarise(mean_tv=mean(tvhours)) %>% 
  ggplot(aes(mean_tv,relig))+
  geom_point(size=3)

gss_cat %>% 
  drop_na(tvhours) %>% 
  group_by(relig) %>% 
  summarise(mean_tv=mean(tvhours)) %>% 
  mutate(relig=fct_reorder(relig,mean_tv)) %>% 
  ggplot(aes(mean_tv,relig))+
  geom_point(size=4)


gss_cat %>% 
  drop_na(tvhours) %>% 
  group_by(relig) %>% 
  summarise(mean_tv=mean(tvhours)) %>% 
  mutate(relig=fct_reorder(relig,mean_tv)) %>% 
  ggplot(aes(mean_tv,relig)) %>% 
  geom_point(size=4)


gss_cat %>% 
  drop_na(age) %>% 
  filter(rincome!="Not applicable") %>%
  group_by(rincome) %>% 
  summarise(mean_age=mean(age)) %>% 
  ggplot(aes(mean_age,rincome)) + 
  geom_point(size=4)


gss_cat %>% 
  drop_na(age) %>% 
  filter(rincome!="Not Applicable") %>% 
  group_by(rincome) %>% 
  summarise(mean_age=mean(age)) %>% 
  ggplot(aes(mean_age,rincome)) + 
  geom_point(size=3)



gss_cat %>% 
  drop_na(age) %>% 
  filter(rincome!="Not Applicable") %>% 
  group_by(rincome) %>% 
  summarise(mean_age=mean(age)) %>% 
  mutate(rincome=fct_rev(rincome)) %>% 
  ggplot(aes(mean_age,rincome))+
  geom_point(size=4)

gss_cat %>% 
  drop_na(age) %>% 
  filter(rincome!="Not Applicable") %>% 
  group_by(rincome) %>% 
  summarise(mean_age=mean(age)) %>% 
  mutate(rincome=fct_rev(rincome)) %>% 
  ggplot(aes(mean_age,rincome))+
  geom_point(size=4)


?forcats
gss_cat %>% 
  count(marital,sort=T)


gss_cat %>% 
  ggplot(aes(marital))+
  geom_bar()

gss_cat %>% 
  mutate(marital=fct_infreq(marital)) %>% 
  count(marital)


gss_cat %>% 
  mutate(marital=fct_infreq(marital)) %>% 
  count(marital)

gss_cat %>% 
  mutate(marital=fct_infreq(marital)) %>% 
  mutate(marital=fct_rev(marital)) %>% 
  count(marital)


gss_cat %>% 
  mutate(marital=fct_infreq(marital)) %>% 
  mutate(marital=fct_rev(marital)) %>% 
  count(marital)

gss_cat %>% 
  mutate(marital=marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital))+
  geom_bar(fill="steelblue",alpha=.5)+
  theme_bw()


gss_cat %>% 
  mutate(marital=marital %>% fct_infreq() %>% fct_rev()) %>% 
  ggplot(aes(marital))+
  geom_bar(fill="red",alpha=.7)+
  theme_bw()


#Recode factors
view(gss_cat)


gss_cat %>% 
  count(partyid,sort=T)




gss_cat %>% 
  mutate(partyid = fct_recode(partyid,
                            "Republican,strong"    = "Strong republican",
                            "Republican,weak"      = "Not str republican",
                            "Independent,near rep"         = "Ind, near rep",
                            "Independent, near dem"= "Ind,near dem",
                            "Democrat,weak"        = "Not str democrat",
                            "Democrat,strong"      = "Strong democrat",
                            "Other"                = "No answer",
                            "Other"                = "Other party",
                            "Other"                = "Don't know"
  )) %>% 
  count(partyid)


gss_cat %>% 
  count(partyid,sort=T)

gss_cat %>% 
  mutate(partyid=fct_collapse(partyid,
                              Other=c("No answer","Don't know","Other party"),
                              rep=c("Strong republican","Not str republican"),
                              ind=c("Ind,near rep","Independent","Ind,near dem"),
                              dem=c("Not str democrat","Strong democrat")
                              )) %>% 
  count(partyid)


gss_cat %>% 
  count(relig,sort=T)


gss_cat %>% 
  mutate(relig=fct_lump(relig,n=2)) %>% 
  count(relig)

gss_cat %>% 
  mutate(relig=fct_lump(relig,n=2)) %>% 
  count(relig)

gss_cat %>% 
  mutate(relig=fct_lump(relig,n=2)) %>% 
  count(relig)


gss_cat %>% 
  count(relig,sort=T)


gss_cat %>% 
  filter(!is.na(age)) %>% 
  filter(marital %in% c("Never married",
                        "Married",
                        "Widowed")) %>% 
  count(age,marital) %>% 
  group_by(age) %>% 
  mutate(prop=n/sum(n)) %>% 
  ggplot(aes(age,prop,colour=marital))+
  geom_point(size=2,na.rm=TRUE)+
  theme_minimal()

gss_cat %>% 
  filter(!is.na(age)) %>% 
  filter(marital %in% c("Married",
                        "Never married",
                        "Divorced",
                        "Widowed",
                        "Separated")) %>% 
  count(age,marital) %>% 
  group_by(age) %>% 
  mutate(prop=n/sum(n)) %>% 
  ggplot(aes(age,prop,colour=marital))+
  geom_point(size=2, na.rm=TRUE)+
  theme_minimal()

gss_cat %>% 
  count(marital,sort=T)


#Favorite R packages

#Its all time tidyverse


library(lubridate)
now()
ymd('2017-03-30')
mdy("March30th, 2017")
dmy(30-Mar-2017)
today()
make_difftime()
library(nycflights13)
install.packages("nycflights13")
library(nycflights13)
view(nycflights13)
glimpse(flights)
View(flights)
names(flights)
flights %>% 
  select("origin","year","month","day","hour","minute") %>% 
  head(4)

flights %>% 
  select("origin","year","month","day","hour","minute") %>% 
  head(4)


flights %>% 
  mutate(flight_date=ymd_hm(paste(year,month,day,hour,minute))) %>% 
  select(origin,dest,flight_date) %>% 
  head(4)


flights %>% 
  mutate(flight_date=ymd_hm(paste(year,month,day,hour,minute))) %>% 
  select(origin,dest,flight_date) %>% 
  head(4)



