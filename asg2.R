TB_burden_countries_2020.02.26 <- read.csv("C:/Users/JAYAGN/Desktop/TB_burden_countries_2020-02-26.csv", header=TRUE)
TB <- data.frame(TB_burden_countries_2020.02.26)
rm(TB_burden_countries_2020.02.26)
View(TB)
year_2000 <- filter(TB,year == 2000)
year_2018 <- filter(TB,year == 2018)
View(year_2000)
year_2001 <- filter(TB,year == 2001)
rm(year_2001)
afg <- filter(TB,country == "Afghanistan")
View(afg)
TB2 <- na.omit(TB,cols ="c_cdr")
cor(TB2$e_mort_exc_tbhiv_100k,TB2$c_cdr)

r<-TB2$e_inc_tbhiv_100k/TB2$e_mort_tbhiv_100k
TB2(TB2$e_mort_num,TB2$c_cdr) <- complete.cases(TB[,"c_cdr"])
data[TB2]
DF(!is.na(TB$c_cdr))
TB2 <- TB[!is.na(TB$c_cdr),]
cor(TB2$e_mort_tbhiv_num,TB2$e_inc_tbhiv_num)
TB2 <- TB[!is.na(TB$e_mort_exc_tbhiv_num),]
TB2 <- TB[!is.na(TB$c_cdr),]
TB2 <- TB[!is.na(TB$e_inc_tbhiv_num),]
TB2 <- TB2[!is.na(TB2$c_cdr),]
cor(TB2$e_mort_num,TB2$c_cdr)
cor(afg$e_mort_num,afg$c_cdr)
growth_rate = TB %>% group_by(country) %>% mutate(Growth = ( e_pop_num - lag(e_pop_num))/lag(e_pop_num))
mort_rate = TB2 %>% group_by(country) %>% mutate(mort_rate = ( e_mort_num/e_inc_num)*100)
cor(TB2$mort_rate,TB2$c_cdr)
mort_rate_afg = afg %>% mutate(mort_rate = ( e_mort_num/e_inc_num)*100)
ggplot(data = mort_rate_afg,aes(x=c_cdr,y=mort_rate)) + geom_point()
mort_rate_with_survive = mort_rate %>% group_by(country) %>% mutate(survival_rate = 100 - mort_rate)
cor(mort_rate_with_survive$c_cdr,mort_rate_with_survive$survival_rate,method = 'spearman')
cor(mort_rate_afg$c_cdr,mort_rate_afg$mort_rate)
year_00_18 <- filter(TB2,year %in% c("2000","2018"))
growth_rate_all = year_00_18 %>% group_by(country) %>% mutate(Growth = (e_pop_num -lag(e_pop_num))/lag(e_pop_num))
growth_rate <- year_00_18 %>% 
  group_by(iso_numeric) %>% 
  mutate(growth = e_pop_num - lag(e_pop_num))
require(data.table)
setDT(year_00_18)
year_00_18[ , growth := e_pop_num - shift(e_pop_num), by = country]
