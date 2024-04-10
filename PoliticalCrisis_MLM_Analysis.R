####----------------------------------------------------------------------------------------------------------------
##MLM model VS. OLS model:
ols_PoliCrisis<-lm(formula = scale(PerceivedEthnicThreat)~1,
                   data = AllCountry_PoliticalCrisis)
mlm_PoliCrisis<-lmer(formula = scale(PerceivedEthnicThreat)~1+(1|cntry)+(1|CountryYear),
                     data = AllCountry_PoliticalCrisis)
anova(mlm_PoliCrisis, ols_PoliCrisis, test="Chisq")


####----------------------------------------------------------------------------------------------------------------
##First: an empty model to see the variances at each level:
M0_PoliCrisis<-lmer(formula = scale(PerceivedEthnicThreat)~1+(1|cntry)+(1|CountryYear),
                    data = AllCountry_PoliticalCrisis)
summary(M0_PoliCrisis)

u0j<-ranef(M0_PoliCrisis)
u0j_df<-as.data.frame(u0j)
u0j_df_CountryYear<-u0j_df %>%
        filter(grpvar=="CountryYear")

u0j_df_Country<-u0j_df %>%
        filter(grpvar=="cntry")

ggplot(u0j_df_Country, aes(sample=condval))+stat_qq()+stat_qq_line()
ggplot(u0j_df_CountryYear, aes(sample=condval))+stat_qq()+stat_qq_line()

M0_PoliCrisis_FML<-lmer(formula = scale(PerceivedEthnicThreat)~1+(1|cntry)+(1|CountryYear),
                    data = AllCountry_PoliticalCrisis,
                    REML = FALSE)
summary(M0_PoliCrisis_FML)

#Variation on higher-level total: 11.22%
(0.02431+0.08809)/(0.02431+0.08809+0.88948)

##Variation on country level: 8.79%
(0.08809)/(0.02431+0.08809+0.88948)

#Variation on countryYear total: 2.43%
(0.02431)/(0.02431+0.08809+0.88948)

####----------------------------------------------------------------------------------------------------------------
##Then we add the time_tj inside:
M1_timeLinear<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+
                            (1+time_tj|cntry)+(1|CountryYear),
                    data = AllCountry_PoliticalCrisis)
summary(M1_timeLinear)


####----------------------------------------------------------------------------------------------------------------
##Then we add only the time dummy inside:
M2_timeDummy<-lmer(formula = scale(PerceivedEthnicThreat)~1+Time2013+
                           Time2014+Time2015+Time2016+Time2017+Time2018+Time2019+
                           (1|cntry)+(1|CountryYear),
                   data = AllCountry_PoliticalCrisis)
summary(M2_timeDummy)

sink("./For Prof. Alexander/Model with TimeDummies.txt")
screenreg(list(M2_timeDummy), custom.model.names = c("Model with Time Dummies"),
          custom.coef.names = c("intercept", "Time2013(Ref.=2012)",
                                "Time2014(Ref.=2012)", "Time2015(Ref.=2012)",
                                "Time2016(Ref.=2012)", "Time2017(Ref.=2012)",
                                "Time2018(Ref.=2012)","Time2019(Ref.=2012)"),
          digits = 5)
sink()

####----------------------------------------------------------------------------------------------------------------
##Then we have time_tj and the time dummy 2015 inside:
M3_Time2015_LinearTime<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+Time2015+
                                     (1+time_tj|cntry)+(1|CountryYear),
                             data = AllCountry_PoliticalCrisis)
summary(M3_Time2015_LinearTime)

M3_u<-ranef(M3_Time2015_LinearTime,condVar=TRUE)
dotplot(M3_u, scales=(list(x=list(relation='free'))))
##No countries have significant effect, so no random effect

####----------------------------------------------------------------------------------------------------------------
##Then we have time_tj and time_tj_square inside:
M4_wTimeSquare<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+time_tj_square+
                             (1+time_tj|cntry)+(1|CountryYear),
                     data = AllCountry_PoliticalCrisis)
summary(M4_wTimeSquare)

M4_wTimeSquare_RanEF<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+time_tj_square+
                                   (1+time_tj+time_tj_square|cntry)+(1|CountryYear),
                           data = AllCountry_PoliticalCrisis)
summary(M4_wTimeSquare_RanEF)

M4_u_total<-ranef(M4_wTimeSquare_RanEF, condVar=TRUE)
dotplot(M4_u_total, scale=(list(x=list(relation='free'))))

##plotting the evolution by country
AllCountry_PoliticalCrisis$TimePrediction<-predict(M4_wTimeSquare_RanEF, re.form=~(1+time_tj+time_tj_square|cntry))
AllCountry_PoliticalCrisis$AveTimePrediction<-predict(M4_wTimeSquare_RanEF, re.form=NA)


##The fixed effect time evolution for all countries
ggplot(data = AllCountry_PoliticalCrisis, aes(x=time_tj, y=PerceivedEthnicThreat))+
        geom_line(data = AllCountry_PoliticalCrisis, aes(x=time_tj, y=AveTimePrediction), linewidth=1, color="red")+
        xlab("time points")+ylab("Perceived Ethnic Threat (Standardized)")

##Separate the countries to four region
AllCountry_PoliticalCrisis$region<-sub("BE", "WesternEurope", AllCountry_PoliticalCrisis$cntry)
AllCountry_PoliticalCrisis$region<-sub("CH", "WesternEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("DE", "WesternEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("FR", "WesternEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("GB", "WesternEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("IE", "WesternEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("NL", "WesternEurope", AllCountry_PoliticalCrisis$region)

AllCountry_PoliticalCrisis$region<-sub("ES", "SouthernEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("PT", "SouthernEurope", AllCountry_PoliticalCrisis$region)

AllCountry_PoliticalCrisis$region<-sub("CZ", "EasternEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("EE", "EasternEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("HU", "EasternEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("LT", "EasternEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("PL", "EasternEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("SI", "EasternEurope", AllCountry_PoliticalCrisis$region)

AllCountry_PoliticalCrisis$region<-sub("FI", "NorthernEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("NO", "NorthernEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("SE", "NorthernEurope", AllCountry_PoliticalCrisis$region)

##Subset Eastern and Northern Europe to make the plot:
East_NorthEU<-AllCountry_PoliticalCrisis %>%
        filter(region=="EasternEurope" | region=="NorthernEurope")

ggplot(data = East_NorthEU, aes(x=time_tj, y=PerceivedEthnicThreat))+
        geom_line(data = East_NorthEU, aes(x=time_tj, y=TimePrediction, color=cntry, group=cntry), linewidth=0.8)+
        facet_grid(.~region)+
        xlab("time points")+ylab("Perceived Ethnic Threat (Standardized)")

##Subset SouthernEurope and Western Europe
South_WestEU<-AllCountry_PoliticalCrisis %>%
        filter(region=="SouthernEurope" | region=="WesternEurope")

ggplot(data = South_WestEU, aes(x=time_tj, y=PerceivedEthnicThreat))+
        geom_line(data = South_WestEU, aes(x=time_tj, y=TimePrediction, color=cntry, group=cntry), linewidth=0.8)+
        facet_grid(.~region)+
        xlab("time points")+ylab("Perceived Ethnic Threat (Standardized)")

####----------------------------------------------------------------------------------------------------------------
##Then we have time_tj, time_tj_square, and dummy 2015 inside:
M5_time_timesq_2015<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+
                                  Time2015+time_tj_square+
                                  (1+time_tj+time_tj_square|cntry)+(1|CountryYear),
                          data = AllCountry_PoliticalCrisis)
summary(M5_time_timesq_2015)

M5_utotal<-ranef(M5_time_timesq_2015, condVar=TRUE)
dotplot(M5_utotal, scales=list(x=list(relation='free')))

sink('./For Prof. Alexander/time_dummy_timeSquare_Models.txt')
screenreg(list(M1_timeLinear,M3_Time2015_LinearTime, M5_time_timesq_2015),
          custom.model.names = c("M1","M2","M3"),
          custom.coef.names = c("Intercept","time_tj","Time2015","time_square"))
sink()

##Plotting to see the evolution
AllCountry_PoliticalCrisis$TimePred<-predict(M5_time_timesq_2015, re.form=~(1+time_tj+time_tj_square|cntry))
AllCountry_PoliticalCrisis$AveTimePred<-predict(M5_time_timesq_2015, re.form=NA)

ggplot(data = AllCountry_PoliticalCrisis, aes(x=time_tj, y=PerceivedEthnicThreat))+
        geom_line(data = AllCountry_PoliticalCrisis, aes(x=time_tj, y=TimePred, group=cntry, color=cntry))

ggplot(data = AllCountry_PoliticalCrisis, aes(x=time_tj, y=PerceivedEthnicThreat))+
        geom_line(data = AllCountry_PoliticalCrisis, aes(x=time_tj, y=AveTimePred), color="red", linewidth=1)

ggplot(data = AllCountry_PoliticalCrisis, aes(x=time_tj, y=TimePred, group=cntry, color=cntry))+
        geom_smooth(method="lm", formula = y ~ poly(x, 3), se = FALSE)

ggplot(data = AllCountry_PoliticalCrisis, aes(x=time_tj, y=PerceivedEthnicThreat, group=cntry, color=cntry))+
        geom_smooth(method="lm", formula = y ~ poly(x, 3), se = FALSE)

##Separate the countries to four region
AllCountry_PoliticalCrisis$region<-sub("BE", "WesternEurope", AllCountry_PoliticalCrisis$cntry)
AllCountry_PoliticalCrisis$region<-sub("CH", "WesternEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("DE", "WesternEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("FR", "WesternEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("GB", "WesternEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("IE", "WesternEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("NL", "WesternEurope", AllCountry_PoliticalCrisis$region)

AllCountry_PoliticalCrisis$region<-sub("ES", "SouthernEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("PT", "SouthernEurope", AllCountry_PoliticalCrisis$region)

AllCountry_PoliticalCrisis$region<-sub("CZ", "EasternEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("EE", "EasternEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("HU", "EasternEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("LT", "EasternEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("PL", "EasternEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("SI", "EasternEurope", AllCountry_PoliticalCrisis$region)

AllCountry_PoliticalCrisis$region<-sub("FI", "NorthernEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("NO", "NorthernEurope", AllCountry_PoliticalCrisis$region)
AllCountry_PoliticalCrisis$region<-sub("SE", "NorthernEurope", AllCountry_PoliticalCrisis$region)

##Subset Eastern and Northern Europe to make the plot:
East_NorthEU<-AllCountry_PoliticalCrisis %>%
        filter(region=="EasternEurope" | region=="NorthernEurope")

ggplot(data = East_NorthEU, aes(x=time_tj, y=TimePred, group=cntry, color=cntry))+
        geom_smooth(method="lm", formula = y ~ poly(x, 3), se = FALSE)+facet_grid(.~region)+
        xlab("time points")+ylab("Perceived Ethnic Threat")


##Subset SouthernEurope and Western Europe
South_WestEU<-AllCountry_PoliticalCrisis %>%
        filter(region=="SouthernEurope" | region=="WesternEurope")

ggplot(data = South_WestEU, aes(x=time_tj, y=TimePred, group=cntry, color=cntry))+
        geom_smooth(method="lm", formula = y ~ poly(x, 3), se = FALSE)+facet_grid(.~region)+
        xlab("time points")+ylab("Perceived Ethnic Threat")

####----------------------------------------------------------------------------------------------------------------
####----------------------------------------------------------------------------------------------------------------
##Use the model with only time_tj and dummy 2015, NOT the time_tj_square:
#
#Add individual variables in:
Model_Dummy15_IndVar<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+Time2015+
                                   GenderFac+AgeGroup+scale(eduyrs)+MainAct+
                                   scale(IncomeInsecure)+scale(urbanization)+
                                   scale(religiosity)+LeftRight+
                                   (1+time_tj|cntry)+(1|CountryYear),
                           data = AllCountry_PoliticalCrisis)
summary(Model_Dummy15_IndVar)

##Add country-level variables in:
#
#First Country Income Insecurity
Model_Dummy15_IncomeInse<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+Time2015+
                                     GenderFac+AgeGroup+scale(eduyrs)+MainAct+
                                     scale(IncomeInsecure)+scale(urbanization)+
                                     scale(religiosity)+LeftRight+
                                     scale(IncomeInsecure_CrossSec)+scale(IncomeInsecure_Longi)+
                                     (1+time_tj|cntry)+(1|CountryYear),
                             data = AllCountry_PoliticalCrisis)
summary(Model_Dummy15_IncomeInse)
#Not significant
#
#Secondly, add country unemployment rate
Model_Dummy15_Unemploy<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+Time2015+
                                       GenderFac+AgeGroup+scale(eduyrs)+MainAct+
                                       scale(IncomeInsecure)+scale(urbanization)+
                                       scale(religiosity)+LeftRight+
                                     scale(Unemploy_CrossSec)+scale(Unemploy_Longi)+
                                       (1+time_tj|cntry)+(1|CountryYear),
                               data = AllCountry_PoliticalCrisis)
summary(Model_Dummy15_Unemploy)
#Unemployment Rate Longitudinal Term significant - positive
#
#Thirdly, add country GDP growth inside
Model_Dummy15_GDPGrowth<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+Time2015+
                                     GenderFac+AgeGroup+scale(eduyrs)+MainAct+
                                     scale(IncomeInsecure)+scale(urbanization)+
                                     scale(religiosity)+LeftRight+
                                      scale(GDPGrowth_CrossSec)+scale(GDPGrowth_Longi)+
                                     (1+time_tj|cntry)+(1|CountryYear),
                             data = AllCountry_PoliticalCrisis)
summary(Model_Dummy15_GDPGrowth)
#GDP Growth Longitudinal Term significant - negative
#
#Lastly, put all three country level indicators in:
Model_Dummy15_CountryLevel<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+Time2015+
                                      GenderFac+AgeGroup+scale(eduyrs)+MainAct+
                                      scale(IncomeInsecure)+scale(urbanization)+
                                      scale(religiosity)+LeftRight+
                                         scale(IncomeInsecure_CrossSec)+scale(IncomeInsecure_Longi)+
                                         scale(Unemploy_CrossSec)+scale(Unemploy_Longi)+
                                         scale(GDPGrowth_CrossSec)+scale(GDPGrowth_Longi)+
                                      (1+time_tj|cntry)+(1|CountryYear),
                              data = AllCountry_PoliticalCrisis)
summary(Model_Dummy15_CountryLevel)

##Put all the results together
sink("./For Prof. Alexander/Dummy15_models.txt")
screenreg(list(M3_Time2015_LinearTime, Model_Dummy15_IndVar, Model_Dummy15_CountryLevel),
          custom.model.names = c("Dummy15_M1","Dummy15_M2","Dummy15_M3"),
          custom.coef.names = c("Intercept","time_tj","Time2015",
                                "male(ref.=female)","14-24 yo (ref.=45-54 yo)",
                                "25-34 yo (ref.=45-54 yo)", "35-44 yo (ref.=45-54 yo)",
                                "55-64 yo (ref.=45-54 yo)","65-74 yo (ref.=45-54 yo)",
                                "75 yo and over (ref.=45-54 yo)", "education",
                                "paid work (ref.=Unemployed)","In education(ref.=Unemployed)",
                                "Disabled(ref.=Unemployed)", "Housework(ref.=Unemployed)",
                                "retired(ref.=Unemployed)","other(ref.=Unemployed)",
                                "Income Insecurity","Urbanization","Religiosity",
                                "center(ref.=left)","right(ref.=left)","missing(ref.=left)",
                                "IncomeInsecure_CrossSec","IncomeInsecure_Longi",
                                "Unemploy_CrossSec","Unemploy_Longi",
                                "GDPGrowth_CrossSec","GDPGrowth_Longi"))
sink()




####----------------------------------------------------------------------------------------------------------------
####----------------------------------------------------------------------------------------------------------------
##Use the model with only time_tj and time_tj_square, NOT the Dummy 15:
#
#Add individual variables in:
Model_TimeSQ_IndVar<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+time_tj_square+
                                  GenderFac+AgeGroup+scale(eduyrs)+MainAct+
                                  scale(IncomeInsecure)+scale(urbanization)+
                                  scale(religiosity)+LeftRight+
                                  (1+time_tj+time_tj_square|cntry)+(1|CountryYear),
                          data = AllCountry_PoliticalCrisis)
summary(Model_TimeSQ_IndVar)
#
##Add country-level variables in:
#
#First Country Income Insecurity
Model_TimeSQ_IncomeInse<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+time_tj_square+
                                  GenderFac+AgeGroup+scale(eduyrs)+MainAct+
                                  scale(IncomeInsecure)+scale(urbanization)+
                                  scale(religiosity)+LeftRight+
                                      scale(IncomeInsecure_CrossSec)+scale(IncomeInsecure_Longi)+
                                  (1+time_tj+time_tj_square|cntry)+(1|CountryYear),
                          data = AllCountry_PoliticalCrisis)
summary(Model_TimeSQ_IncomeInse)
#Country Income Insecurity Longitudinal significant - positive
#
#Secondly, add country unemployment rate
Model_TimeSQ_Unemploy<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+time_tj_square+
                                      GenderFac+AgeGroup+scale(eduyrs)+MainAct+
                                      scale(IncomeInsecure)+scale(urbanization)+
                                      scale(religiosity)+LeftRight+
                                      scale(Unemploy_CrossSec)+scale(Unemploy_Longi)+
                                      (1+time_tj+time_tj_square|cntry)+(1|CountryYear),
                              data = AllCountry_PoliticalCrisis)
summary(Model_TimeSQ_Unemploy)
#Unemployment Rate Longitudinal Term significant - positive
#
#Thirdly, add country GDP Growth
Model_TimeSQ_GDPGrowth<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+time_tj_square+
                                    GenderFac+AgeGroup+scale(eduyrs)+MainAct+
                                    scale(IncomeInsecure)+scale(urbanization)+
                                    scale(religiosity)+LeftRight+
                                     scale(GDPGrowth_CrossSec)+scale(GDPGrowth_Longi)+
                                    (1+time_tj+time_tj_square|cntry)+(1|CountryYear),
                            data = AllCountry_PoliticalCrisis)
summary(Model_TimeSQ_GDPGrowth)
#GDPGrowth Longitudinal Term significant - negative
#
#Lastly, add all country level economic indicators inside
Model_TimeSQ_CountryAll<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+time_tj_square+
                                      GenderFac+AgeGroup+scale(eduyrs)+MainAct+
                                      scale(IncomeInsecure)+scale(urbanization)+
                                      scale(religiosity)+LeftRight+
                                      scale(IncomeInsecure_CrossSec)+scale(IncomeInsecure_Longi)+
                                      scale(Unemploy_CrossSec)+scale(Unemploy_Longi)+
                                      scale(GDPGrowth_CrossSec)+scale(GDPGrowth_Longi)+
                                      (1+time_tj+time_tj_square|cntry)+(1|CountryYear),
                              data = AllCountry_PoliticalCrisis)
summary(Model_TimeSQ_CountryAll)

sink("./For Prof. Alexander/TimeSQ_models.txt")
screenreg(list(M4_wTimeSquare_RanEF, Model_TimeSQ_IndVar, Model_TimeSQ_CountryAll),
          custom.model.names = c("TimeSQ_M1","TimeSQ_M2","TimeSQ_M3"),
          custom.coef.names = c("Intercept","time_tj","Time_Square",
                                "male(ref.=female)","14-24 yo (ref.=45-54 yo)",
                                "25-34 yo (ref.=45-54 yo)", "35-44 yo (ref.=45-54 yo)",
                                "55-64 yo (ref.=45-54 yo)","65-74 yo (ref.=45-54 yo)",
                                "75 yo and over (ref.=45-54 yo)", "education",
                                "paid work (ref.=Unemployed)","In education(ref.=Unemployed)",
                                "Disabled(ref.=Unemployed)", "Housework(ref.=Unemployed)",
                                "retired(ref.=Unemployed)","other(ref.=Unemployed)",
                                "Income Insecurity","Urbanization","Religiosity",
                                "center(ref.=left)","right(ref.=left)","missing(ref.=left)",
                                "IncomeInsecure_CrossSec","IncomeInsecure_Longi",
                                "Unemploy_CrossSec","Unemploy_Longi",
                                "GDPGrowth_CrossSec","GDPGrowth_Longi"))
sink()




####----------------------------------------------------------------------------------------------------------------
####----------------------------------------------------------------------------------------------------------------
##Use the model with time_tj, time_tj_square and the Dummy 15:
#
#Add individual variables in:
Model_allTime_IndVar<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+
                                   Time2015+time_tj_square+
                                   GenderFac+AgeGroup+scale(eduyrs)+MainAct+
                                   scale(IncomeInsecure)+scale(urbanization)+
                                   scale(religiosity)+LeftRight+
                                   (1+time_tj+time_tj_square|cntry)+(1|CountryYear),
                           data = AllCountry_PoliticalCrisis)
summary(Model_allTime_IndVar)
#
##Add country-level variables in:
#
#First Country Income Insecurity
Model_AllTime_IncomeInse<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+
                                       Time2015+time_tj_square+
                                       GenderFac+AgeGroup+scale(eduyrs)+MainAct+
                                       scale(IncomeInsecure)+scale(urbanization)+
                                       scale(religiosity)+LeftRight+
                                       scale(IncomeInsecure_CrossSec)+scale(IncomeInsecure_Longi)+
                                       (1+time_tj+time_tj_square|cntry)+(1|CountryYear),
                               data = AllCountry_PoliticalCrisis,
                               REML = FALSE)
summary(Model_AllTime_IncomeInse)
#Country Income Insecurity Longitudinal term significant - positive
#
#Secondly, add country unemployment rate
Model_AllTime_Unemploy<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+
                                     Time2015+time_tj_square+
                                     GenderFac+AgeGroup+scale(eduyrs)+MainAct+
                                     scale(IncomeInsecure)+scale(urbanization)+
                                     scale(religiosity)+LeftRight+
                                     scale(Unemploy_CrossSec)+scale(Unemploy_Longi)+
                                     (1+time_tj+time_tj_square|cntry)+(1|CountryYear),
                             data = AllCountry_PoliticalCrisis)
summary(Model_AllTime_Unemploy)
#Unemployment rate longitudinal significant - positive
#
#Thirdly, add country GDP Growth
Model_AllTime_GDPGrowth<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+
                                      Time2015+time_tj_square+
                                      GenderFac+AgeGroup+scale(eduyrs)+MainAct+
                                      scale(IncomeInsecure)+scale(urbanization)+
                                      scale(religiosity)+LeftRight+
                                      scale(GDPGrowth_CrossSec)+scale(GDPGrowth_Longi)+
                                      (1+time_tj+time_tj_square|cntry)+(1|CountryYear),
                              data = AllCountry_PoliticalCrisis)
summary(Model_AllTime_GDPGrowth)
#GDP longitudinal significant - negative
#
#Lastly, add all country level economic indicators inside
Model_allTime_CountryEconomy<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+
                                           Time2015+time_tj_square+
                                           GenderFac+AgeGroup+scale(eduyrs)+MainAct+
                                           scale(IncomeInsecure)+scale(urbanization)+
                                           scale(religiosity)+LeftRight+
                                           scale(IncomeInsecure_CrossSec)+scale(IncomeInsecure_Longi)+
                                           scale(Unemploy_CrossSec)+scale(Unemploy_Longi)+
                                           scale(GDPGrowth_CrossSec)+scale(GDPGrowth_Longi)+
                                           (1+time_tj+time_tj_square|cntry)+(1|CountryYear),
                                   data = AllCountry_PoliticalCrisis)
summary(Model_allTime_CountryEconomy)
#
#Try without the random slope of time_tj_square
Model_allTime_CountryEconomy<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+
                                           Time2015+time_tj_square+
                                           GenderFac+AgeGroup+scale(eduyrs)+MainAct+
                                           scale(IncomeInsecure)+scale(urbanization)+
                                           scale(religiosity)+LeftRight+
                                           scale(IncomeInsecure_CrossSec)+scale(IncomeInsecure_Longi)+
                                           scale(Unemploy_CrossSec)+scale(Unemploy_Longi)+
                                           scale(GDPGrowth_CrossSec)+scale(GDPGrowth_Longi)+
                                           (1+time_tj|cntry)+(1|CountryYear),
                                   data = AllCountry_PoliticalCrisis)
summary(Model_allTime_CountryEconomy)

sink("./For Prof. Alexander/NEW Time_Dummy2015_TimeSq_FullModels.txt")
screenreg(list(M3_Time2015_LinearTime, M5_time_timesq_2015, Model_allTime_IndVar, Model_allTime_CountryEconomy),
          custom.model.names = c("M1", "M2", "M3", "M4"),
          custom.coef.names = c("Intercept","time","Time Point 2015","Time Sqaure",
                                "male(ref.=female)","14-24 yo (ref.=45-54 yo)",
                                "25-34 yo (ref.=45-54 yo)", "35-44 yo (ref.=45-54 yo)",
                                "55-64 yo (ref.=45-54 yo)","65-74 yo (ref.=45-54 yo)",
                                "75 yo and over (ref.=45-54 yo)", "years of education",
                                "paid work (ref.=Unemployed)","In education(ref.=Unemployed)",
                                "Disabled(ref.=Unemployed)", "Housework(ref.=Unemployed)",
                                "retired(ref.=Unemployed)","other(ref.=Unemployed)",
                                "Income Insecurity","Urbanization","Religiosity",
                                "center(ref.=left)","right(ref.=left)","missing(ref.=left)",
                                "IncomeInsecure_CrossSec","IncomeInsecure_Longi",
                                "Unemploy_CrossSec","Unemploy_Longi",
                                "GDPGrowth_CrossSec","GDPGrowth_Longi"),
          digits = 3)
sink()



####------------------------------------------------------------------------------------------------------------
##Plotting with all variables as control:
AllCountry_PoliticalCrisis$PredWControl<-predict(Model_allTime_CountryEconomy, re.form=~(1+time_tj|cntry))

ggplot(data = AllCountry_PoliticalCrisis, aes(x=time_tj, y=PerceivedEthnicThreat))+
        geom_line(data = AllCountry_PoliticalCrisis, aes(x=time_tj, y=PredWControl, color=cntry))

ggplot(data = AllCountry_PoliticalCrisis, aes(x=time_tj, y=PredWControl, group=cntry, color=cntry))+
        geom_smooth(method = "lm", formula = y~poly(x,3), se=FALSE)
ggplot(data = South_WestEU, aes(x=time_tj, y=PredWControl, group=cntry, color=cntry))+
        geom_smooth(method = "lm", formula = y~poly(x,3), se=FALSE)+facet_grid(.~region)



###-------------------------------------------------------------------------------------------------------------
##Try to put the cross-sectional term in to explain the random slope
Model_allTime_timeRSwGDP<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+
                                           Time2015+time_tj_square+
                                           GenderFac+AgeGroup+scale(eduyrs)+MainAct+
                                           scale(IncomeInsecure)+scale(urbanization)+
                                           scale(religiosity)+LeftRight+
                                           scale(IncomeInsecure_CrossSec)+scale(IncomeInsecure_Longi)+
                                           scale(Unemploy_CrossSec)+scale(Unemploy_Longi)+
                                           scale(GDPGrowth_CrossSec)+scale(GDPGrowth_Longi)+
                                    scale(GDPGrowth_CrossSec):time_tj+
                                           (1+time_tj|cntry)+(1|CountryYear),
                                   data = AllCountry_PoliticalCrisis)
summary(Model_allTime_timeRSwGDP)
#Interaction between time and cross-sec GDP growth significant
#
Model_allTime_timeRSwUnemp<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+
                                       Time2015+time_tj_square+
                                       GenderFac+AgeGroup+scale(eduyrs)+MainAct+
                                       scale(IncomeInsecure)+scale(urbanization)+
                                       scale(religiosity)+LeftRight+
                                       scale(IncomeInsecure_CrossSec)+scale(IncomeInsecure_Longi)+
                                       scale(Unemploy_CrossSec)+scale(Unemploy_Longi)+
                                       scale(GDPGrowth_CrossSec)+scale(GDPGrowth_Longi)+
                                       scale(Unemploy_CrossSec):time_tj+
                                       (1+time_tj|cntry)+(1|CountryYear),
                               data = AllCountry_PoliticalCrisis)
summary(Model_allTime_timeRSwUnemp)
#Interaction between time and cross-sec unemp growth NOT significant
#
Model_allTime_timeRSwIncIns<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+
                                         Time2015+time_tj_square+
                                         GenderFac+AgeGroup+scale(eduyrs)+MainAct+
                                         scale(IncomeInsecure)+scale(urbanization)+
                                         scale(religiosity)+LeftRight+
                                         scale(IncomeInsecure_CrossSec)+scale(IncomeInsecure_Longi)+
                                         scale(Unemploy_CrossSec)+scale(Unemploy_Longi)+
                                         scale(GDPGrowth_CrossSec)+scale(GDPGrowth_Longi)+
                                         scale(IncomeInsecure_CrossSec):time_tj+
                                         (1+time_tj|cntry)+(1|CountryYear),
                                 data = AllCountry_PoliticalCrisis)
summary(Model_allTime_timeRSwIncIns)
#Interaction between time and cross-sec income insecure NOT significant

sink("./For Prof. Alexander/NEW NEW Time_Dummy2015_TimeSq_FullModels.txt")
screenreg(list(M3_Time2015_LinearTime, M5_time_timesq_2015, Model_allTime_IndVar, Model_allTime_CountryEconomy, Model_allTime_RS_test),
          custom.model.names = c("M1", "M2", "M3", "M4", "M5"),
          custom.coef.names = c("Intercept","time","Time Point 2015","Time Sqaure",
                                "male(ref.=female)","14-24 yo (ref.=45-54 yo)",
                                "25-34 yo (ref.=45-54 yo)", "35-44 yo (ref.=45-54 yo)",
                                "55-64 yo (ref.=45-54 yo)","65-74 yo (ref.=45-54 yo)",
                                "75 yo and over (ref.=45-54 yo)", "years of education",
                                "paid work (ref.=Unemployed)","In education(ref.=Unemployed)",
                                "Disabled(ref.=Unemployed)", "Housework(ref.=Unemployed)",
                                "retired(ref.=Unemployed)","other(ref.=Unemployed)",
                                "Income Insecurity","Urbanization","Religiosity",
                                "center(ref.=left)","right(ref.=left)","missing(ref.=left)",
                                "IncomeInsecure_CrossSec","IncomeInsecure_Longi",
                                "Unemploy_CrossSec","Unemploy_Longi",
                                "GDPGrowth_CrossSec","GDPGrowth_Longi",
                                "time*GDPGrowth"),
          digits = 3)
sink()


###----------------------------------------------------------------------------------------------------------------
###Test joint effect of the cross-sectional components of GDP and Unemployment:
Model_AllTime_IncomeInse<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+
                                       Time2015+time_tj_square+
                                       GenderFac+AgeGroup+scale(eduyrs)+MainAct+
                                       scale(IncomeInsecure)+scale(urbanization)+
                                       scale(religiosity)+LeftRight+
                                       scale(IncomeInsecure_CrossSec)+scale(IncomeInsecure_Longi)+
                                       (1+time_tj+time_tj_square|cntry)+(1|CountryYear),
                               data = AllCountry_PoliticalCrisis,
                               REML = FALSE)
summary(Model_AllTime_IncomeInse)
#
#Add the Cross-sectional term of GDP and Unemployment:
Model_AllTime_GDPUnemCrossSec<-lmer(formula = scale(PerceivedEthnicThreat)~1+time_tj+
                                       Time2015+time_tj_square+
                                       GenderFac+AgeGroup+scale(eduyrs)+MainAct+
                                       scale(IncomeInsecure)+scale(urbanization)+
                                       scale(religiosity)+LeftRight+
                                       scale(IncomeInsecure_CrossSec)+scale(IncomeInsecure_Longi)+
                                       scale(Unemploy_CrossSec)+
                                       scale(GDPGrowth_CrossSec)+
                                       (1+time_tj+time_tj_square|cntry)+(1|CountryYear),
                               data = AllCountry_PoliticalCrisis,
                               REML = FALSE)
summary(Model_AllTime_GDPUnemCrossSec)
#LR test
anova(Model_AllTime_GDPUnemCrossSec, Model_AllTime_IncomeInse, test="Chisq")
