#### Read data ####
library("dplyr")
library("lubridate")
library("Rmisc")
library("ggplot2")
library(reshape2)

data = read.csv("CLEANED-UP-DATA-2.CSV")

data[is.na(data)] = 0
data[,1] = NULL

data$newtime = as.POSIXct(paste(data$Date, as.character(data$Int)),
                           format = "%m/%d/%Y %H:%M:%S",
                           tz = "GMT")
data = data %>%
  mutate(month = month(newtime),
         week = week(newtime),
         day = yday(newtime)) %>%
  mutate(contact = ifelse(month %in% c(3,4,5,6,7), "Hybrid", "Protected")) %>%
  filter(complete.cases(month))

#### Data manip. ####
data.overall = data %>% # For overall budget of each elephant
  group_by(Elephant) %>%
  dplyr::summarise(lo = sum(LO),
            ff = sum(FF),
            ag = sum(AG),
            so = sum(SO),
            so2 = sum(SO2),
            ee = sum(EE),
            fe = sum(FE),
            st = sum(ST),
            int= sum(IN),
            oa = sum(OA),
            nv = sum(NV),
            total = sum(lo,ff, ag,so, so2, ee, fe,st,int,oa,nv),
            lo.prop = sum(lo) / total,
            ff.prop = sum(ff) / total,
            ag.prop = sum(ag)/total,
            so.prop = sum(so)/total,
            so2.prop = sum(so2)/total,
            ee.prop = sum(ee)/total,
            fe.prop = sum(fe)/total,
            st.prop = sum(st)/total,
            int.prop = sum(int)/total,
            oa.prop = sum(oa)/total,
            nv.prop = sum(nv)/total,
            month = first(month),
            week = as.factor(first(week))) %>%
  mutate_if(is.numeric, ~ . * 100) %>%
  select(Elephant, lo.prop, ff.prop, ag.prop, so.prop,
         so2.prop, ee.prop, fe.prop, st.prop, int.prop,
         oa.prop, nv.prop)

actbud = data %>% # For overall budget of each elephant between contact types
  group_by(Elephant, contact) %>%
  dplyr::summarise(lo = sum(LO),
                   ff = sum(FF),
                   ag = sum(AG),
                   so = sum(SO),
                   so2 = sum(SO2),
                   ee = sum(EE),
                   fe = sum(FE),
                   st = sum(ST),
                   int= sum(IN),
                   oa = sum(OA),
                   nv = sum(NV),
                   total = sum(lo,ff, ag,so, so2, ee, fe,st,int,oa,nv),
                   Loco = sum(lo) / total, #rename this
                   Feed = sum(ff) / total,
                   Aggr = sum(ag)/total,
                   Social = sum(so)/total,
                   HuSocial = sum(so2)/total,
                   Enrich = sum(ee)/total,
                   FeedEnrich = sum(fe)/total,
                   Stereo = sum(st)/total,
                   Inactive = sum(int)/total,
                   OthActive = sum(oa)/total,
                   NotVis = sum(nv)/total,
                   month = first(month),
                   week = as.factor(first(week))) %>%
  mutate_if(is.numeric, ~ . * 100) %>%
  select(Elephant, contact, Loco, Feed, Aggr, Social,
         HuSocial, Enrich, FeedEnrich, Stereo, Inactive,
         OthActive, NotVis)%>%
  melt(id.vars = c("Elephant", "contact"))


data.contact = data %>% # For overall budget of each elephant between contact types
  group_by(Elephant, contact) %>%
  dplyr::summarise(lo = sum(LO),
                   ff = sum(FF),
                   ag = sum(AG),
                   so = sum(SO),
                   so2 = sum(SO2),
                   ee = sum(EE),
                   fe = sum(FE),
                   st = sum(ST),
                   int= sum(IN),
                   oa = sum(OA),
                   nv = sum(NV),
                   total = sum(lo,ff, ag,so, so2, ee, fe,st,int,oa,nv),
                   Loco = sum(lo) / total, #rename this
                   Feed = sum(ff) / total,
                   Aggr = sum(ag)/total,
                   Social = sum(so)/total,
                   HuSocial = sum(so2)/total,
                   Enrich = sum(ee)/total,
                   FeedEnrich = sum(fe)/total,
                   Stereo = sum(st)/total,
                   Inactive = sum(int)/total,
                   OthActive = sum(oa)/total,
                   NotVis = sum(nv)/total,
                   month = first(month),
                   week = as.factor(first(week))) %>%
  mutate_if(is.numeric, ~ . * 100) %>%
  select(Elephant, contact, Loco, Feed, Aggr, Social,
         HuSocial, Enrich, FeedEnrich, Stereo, Inactive,
         OthActive, NotVis)

data.contact.diff = data.contact %>%
  group_by(Elephant) %>%
  dplyr::mutate(Loco = Loco[contact == "Protected"] - Loco[contact == "Hybrid"],
                Feed = Feed[contact == "Protected"] - Feed[contact == "Hybrid"],
                Aggr = Aggr[contact == "Protected"] - Aggr[contact == "Hybrid"],
                Social = Social[contact == "Protected"] - Social[contact == "Hybrid"],
                HuSocial = HuSocial[contact == "Protected"] - HuSocial[contact == "Hybrid"],
                Enrich = Enrich[contact == "Protected"] - Enrich[contact == "Hybrid"],
                FeedEnrich = FeedEnrich[contact == "Protected"] - FeedEnrich[contact == "Hybrid"],
                Stereo = Stereo[contact == "Protected"] - Stereo[contact == "Hybrid"],
                Inactive = Inactive[contact == "Protected"] - Inactive[contact == "Hybrid"],
                OthActive = OthActive[contact == "Protected"] - OthActive[contact == "Hybrid"],
                NotVis = NotVis[contact == "Protected"] - NotVis[contact == "Hybrid"])%>%
  filter(contact == "Protected") %>%
  select(Elephant, Loco, Feed, Aggr, Social,
         HuSocial, Enrich, FeedEnrich, Stereo, Inactive,
         OthActive, NotVis) %>%
  melt(id.vars = c("Elephant"))

ggplot(data = data.contact.diff) +
  geom_point(aes(x = variable, y = value, color = Elephant, shape = Elephant), size = 4.0, position = position_dodge(width = 0.90)) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  #facet_grid(.~Elephant) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab("Behaviours") +
  ylab("Difference [%]")


#### Shannon weekly index ####
SDI.weekly = data %>%
  group_by(Elephant, month, week, contact) %>%
  dplyr::summarise(lo = sum(LO),
                   ff = sum(FF),
                   ag = sum(AG),
                   so = sum(SO),
                   so2 = sum(SO2),
                   ee = sum(EE),
                   fe = sum(FE),
                   st = sum(ST),
                   int= sum(IN),
                   oa = sum(OA),
                   nv = sum(NV),
                   total = sum(lo, ff,ag,so, so2, ee, fe, st,int,oa,nv),
                   lo.prop = lo/total,
                   ff.prop = ff/total,
                   ag.prop = ag/total,
                   so.prop = so/total,
                   so2.prop = so2/total,
                   ee.prop = ee/total,
                   fe.prop = fe/total,
                   st.prop = st/total,
                   int.prop = int/total,
                   oa.prop = oa/total,
                   nv.prop = nv/total,
                   lo.h = lo.prop*log(lo.prop),
                   ff.h = ff.prop*log(ff.prop),
                   ag.h = ag.prop*log(ag.prop),
                   so.h = so.prop*log(so.prop),
                   so2.h = so2.prop*log(so2.prop),
                   ee.h = ee.prop*log(ee.prop),
                   fe.h = fe.prop*log(fe.prop),
                   st.h = st.prop*log(st.prop),
                   int.h = int.prop*log(int.prop),
                   oa.h = oa.prop*log(oa.prop),
                   nv.h = nv.prop*log(nv.prop),
                   month = first(month),
                   contact = first(contact),
                   week = first(week))

SDI.weekly[is.na(SDI.weekly)] = 0

SDI.weekly = SDI.weekly %>%
  group_by(Elephant, month, week, contact) %>%
  dplyr::mutate(H = -sum(lo.h, ff.h, ag.h, so.h, so2.h, ee.h, fe.h, st.h, int.h, oa.h, nv.h))

SDI.overall = SDI.weekly %>%
  group_by(Elephant, contact) %>%
  dplyr::summarise(mean = mean(H),
                   n = length(H),
                   se = sd(H)/sqrt(length(H)))


a0 = aov(H~contact * Elephant, data = SDI.weekly)
summary(a0)
TukeyHSD(a0)

model.0 = lmer(H~1 + (1|Elephant), data = SDI.weekly)
model.1 = lmer(H~contact + (1|Elephant), data = SDI.weekly)
model.sel(model.0, model.1)
model.1
summary(model.1)

res <- resid(model.1)
plot(fitted(model.1), res)
abline(0,0)
#### Shannon monthly index ####
SDI.monthly = data %>%
  group_by(Elephant, month, contact) %>%
  dplyr::summarise(lo = sum(LO),
                   ff = sum(FF),
                   ag = sum(AG),
                   so = sum(SO),
                   so2 = sum(SO2),
                   ee = sum(EE),
                   fe = sum(FE),
                   st = sum(ST),
                   int= sum(IN),
                   oa = sum(OA),
                   nv = sum(NV),
                   total = sum(lo, ff,ag,so, so2, ee, fe, st,int,oa,nv),
                   lo.prop = lo/total,
                   ff.prop = ff/total,
                   ag.prop = ag/total,
                   so.prop = so/total,
                   so2.prop = so2/total,
                   ee.prop = ee/total,
                   fe.prop = fe/total,
                   st.prop = st/total,
                   int.prop = int/total,
                   oa.prop = oa/total,
                   nv.prop = nv/total,
                   lo.h = lo.prop*log(lo.prop),
                   ff.h = ff.prop*log(ff.prop),
                   ag.h = ag.prop*log(ag.prop),
                   so.h = so.prop*log(so.prop),
                   so2.h = so2.prop*log(so2.prop),
                   ee.h = ee.prop*log(ee.prop),
                   fe.h = fe.prop*log(fe.prop),
                   st.h = st.prop*log(st.prop),
                   int.h = int.prop*log(int.prop),
                   oa.h = oa.prop*log(oa.prop),
                   nv.h = nv.prop*log(nv.prop),
                   month = first(month),
                   contact = first(contact))

SDI.monthly[is.na(SDI.monthly)] = 0


SDI.monthly = SDI.monthly %>%
  group_by(Elephant, month, contact) %>%
  dplyr::summarise(H = -sum(lo.h, ff.h, ag.h, so.h, so2.h, ee.h, fe.h, st.h, int.h, oa.h, nv.h))



#### Fecal Analysis ####
fecal = read.csv("ele-fecal-raw.csv")
fecal$Date = fecal[,1]
fecal[,1] = NULL
anyNA(fecal)

fecal$newdate = as.POSIXct(fecal$Date,
                           format = "%d.%m.%y",
                           tz = "GMT")
head(fecal)

fecal = fecal %>%
  group_by(ID) %>%
  dplyr::mutate(month = month(newdate),
                day = yday(newdate),
                year = year(newdate))

fecal.pre = fecal %>%
  filter(year %in% c("2017", "2018", "2019"))
fecal.pre$contact = "Hybrid"
fecal.mix = fecal %>%
  filter(year %in% c("2020")) %>%
  dplyr::mutate(contact = ifelse(month %in% c(1,2,3,4,5,6,7), "Hybrid", "Protected"))
fecal.post = fecal %>%
  filter(year %in% c("2021", "2022"))
fecal.post$contact = "Protected"

fecal.new = rbind(fecal.pre, fecal.mix, fecal.post)

fecal.new %>%
  group_by(ID, year, contact) %>%
  dplyr::summarise(mean = mean(Cortisol))

fecal.yearly.df = fecal.new %>%
  group_by(ID, year, contact) %>%
  dplyr::summarise(mean = mean(Cortisol),
                   CI = CI(Cortisol)[2] - CI(Cortisol)[3],
                   min = mean-CI,
                   max = mean + CI) %>%
  dplyr::select(mean, CI, contact, year)

fecal.monthly.df = fecal.new %>%
  group_by(ID, year, month, contact) %>%
  dplyr::summarise(mean = mean(Cortisol),
                   CI = CI(Cortisol)[2] - CI(Cortisol)[3],
                   min = mean-CI,
                   max = mean + CI) %>%
  dplyr::select(mean, CI, contact, year, month)

a1 = aov(mean~contact*year, data = fecal.monthly.df)
summary(a1)
a1

#### LMER Analysis ####
library(lme4)
library(MuMIn)
lm0 = lmer(mean~0 + (1|ID),
           data = fecal.monthly.df)
lm1 = lmer(mean~contact + (1|ID),
           data = fecal.monthly.df)
lm2 = lmer(mean~year + (1|ID),
           data = fecal.monthly.df)
model.sel(lm0, lm1, lm2)

#### Merge 2020 SID + fecal results together ####
merged.df.1 = fecal.monthly.df %>%
  filter(ID %in% c("Gambir", "Jati", "Komali"), year == 2020,
         month %in% c(3:12))

merged.df.2 = SDI.monthly %>%
  filter(Elephant %in% c("Gambir", "Jati", "Komali"))

merged.df = cbind(merged.df.2, merged.df.1)
# merged.df %>%
#   group_by(Elephant) %>%
#   dplyr::summarise(R = cor(H, mean),
#                    R2 = R^2)
# Gam: -0.5, Jati: 0.31, Kom: -0.65


#### Area under curve ####
# contact-based comparison
fecal.auc = fecal.new %>%
  group_by(ID) %>%
  dplyr::mutate(new.month = ifelse(year == "2019", month + 12,
                                   ifelse(year == "2020", month + 24,
                                          ifelse(year == "2021", month + 36,
                                                 ifelse(year == "2022", month + 48, month)))))

fecal.auc = fecal.auc %>%
  group_by(ID, contact, new.month) %>%
  dplyr::summarise(mean = mean(Cortisol)) %>%
  select(new.month, mean, contact, ID)

protect.auc = fecal.auc %>%
  filter(contact == "Protected") %>%
  group_by(ID) %>%
  select(new.month, mean)

protect.auc.array = protect.auc$mean

hybrid.auc = fecal.auc %>%
  filter(contact == "Hybrid") %>%
  group_by(ID) %>%
  select(new.month, mean) %>%
  tail(57)

hybrid.auc.array = hybrid.auc$mean

# the below does not seem very necessary at the moment
AUC.df = data.frame(
  month = 1:57,
  protect.auc = protect.auc.array,
  hybrid.auc = hybrid.auc.array,
  protect.auc.rescale = protect.auc.array/sum(protect.auc.array),
  hybrid.auc.rescale = hybrid.auc.array/sum(hybrid.auc.array) #rescaled
)

####a. Calculate AUC overlap ####
library("DescTools")

  x = data.frame(
  month = AUC.df$month,
  protect.auc.rescale = AUC.df$protect.auc.rescale,
  hybrid.auc.rescale = AUC.df$hybrid.auc.rescale,
  overlap = AUC.df$hybrid.auc.rescale - AUC.df$protect.auc.rescale
)

x = x %>%
  mutate(overlap = ifelse(overlap < 0, hybrid.auc.rescale, protect.auc.rescale))

AUC(x = x$month, y = x$overlap, absolutearea = TRUE) # 0.7618537

#### End of script ####
