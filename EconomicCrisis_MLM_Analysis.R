####--------------------------------------------------------------------------------------------------------
####Descriptive graph of the evolution

###First, make the standardized perceived ethnic threat in the data frame
AllCountry_EconomicCrisis<-AllCountry_EconomicCrisis %>% 
        mutate(Stand_Threat=scale(PerceivedEthnicThreat))

###For Eastern Europe: BG,EE,HU,PL,SI,SK,RU,UA
EastEU<-AllCountry_EconomicCrisis %>%
        filter(cntry=="BG" | cntry=="EE" | cntry=="HU" | cntry=="PL" | cntry=="SI" 
               | cntry=="SK" | cntry=="RU" | cntry=="UA")

EastEU_Threat<-EastEU %>%
        group_by(cntry, inwyys) %>%
        summarise(StandThreatMean=mean(Stand_Threat,na.rm = TRUE))

ggplot(EastEU_Threat, aes(x=inwyys, y=StandThreatMean, group=cntry))+
        geom_line(size=0.9)+
        geom_point(aes(shape=cntry),size=3.5)+
        scale_shape_manual(values=c(0,1,2,3,4,5,7,8))+
        xlab("Year")+ylab("Perceived Ethnic Threat (Standardized)")+
        labs(title = "Eastern Europe")


###For Southern Europe: CY,ES,PT
SouthEU<-AllCountry_EconomicCrisis %>%
        filter(cntry=="CY" | cntry=="ES" | cntry=="PT")

SouthEU_Threat<-SouthEU %>%
        group_by(cntry,inwyys) %>%
        summarise(StandThreatMean=mean(Stand_Threat,na.rm=TRUE))

ggplot(SouthEU_Threat,aes(x=inwyys, y=StandThreatMean, group=cntry))+
        geom_line(size=0.9)+
        geom_point(aes(shape=cntry),size=3.5)+
        xlab("Year")+ylab("Perceived Ethnic Threat (Standardized)")+
        labs(title="Southern Europe")

###for Western Europe: BE,CH,DE,FR,GB,IE,NL
WestEU<-AllCountry_EconomicCrisis %>%
        filter(cntry=="BE" | cntry=="CH" | cntry=="DE" | cntry=="FR"
               | cntry=="GB" | cntry=="IE" | cntry=="NL")

WestEU_Threat<-WestEU %>%
        group_by(cntry,inwyys) %>%
        summarise(StandThreatMean=mean(Stand_Threat,na.rm=TRUE))

ggplot(WestEU_Threat,aes(x=inwyys, y=StandThreatMean, group=cntry))+
        geom_line(size=0.9)+
        geom_point(aes(shape=cntry),size=3.5)+
        scale_shape_manual(values=c(1,2,7,8,13,14,15))+
        xlab("Year")+ylab("Perceived Ethnic Threat (Standardized)")+
        labs(title="Western Europe")


###for Northern Europe: FI, NO, SE, DK
NorthEU<-AllCountry_EconomicCrisis %>%
        filter(cntry=="FI" | cntry=="NO" | cntry=="SE" | cntry=="DK")

NorthEU_Threat<-NorthEU %>%
        group_by(cntry, inwyys) %>%
        summarise(StandThreatMean=mean(Stand_Threat,na.rm = TRUE))

ggplot(NorthEU_Threat, aes(x=inwyys, y=StandThreatMean, group=cntry))+
        geom_line(size=0.9)+
        geom_point(aes(shape=cntry),size=3.5)+
        scale_shape_manual(values=c(0,2,8,9))+
        xlab("Year")+ylab("Perceived Ethnic Threat (Standardized)")+
        labs(title = "Northern Europe")
        

####---------------------------------------------------------------------------------------------------
##M0: Empty model with only time_tj
###First we fit the empty models with standardized perceived ethnic threat
EmptyModel_noQuadTerm_stad<-lmer(scale(PerceivedEthnicThreat)~1+time_tj+(1+time_tj|cntry)+(1|CountryYear),
                                 data = AllCountry_EconomicCrisis)

summary(EmptyModel_noQuadTerm_stad)

##ICC=(0.123956)/(0.123956+0.880333)=12.34% --> 12.34% of the variations exist at the country level 


##Plot the random slope of time_tj for different countries:
#First subset the data to fit 133358 rows (This can be used for the Mean Scale later as well):
AllCountry_EconomicCrisis_M0<-AllCountry_EconomicCrisis %>%
        select(PerceivedEthnicThreat, time_tj, cntry, CountryYear)
AllCountry_EconomicCrisis_M0<-na.omit(AllCountry_EconomicCrisis_M0)

CountryGrouping<-function(data, VarName){
        output<-as.character()
        for (i in seq_along(data[[VarName]])){
                if (data[i,VarName]=='BG'| data[i,VarName]=='EE'|data[i,VarName]=="HU"|
                    data[i,VarName]=='PL'|data[i,VarName]=='SI'|data[i,VarName]=='SK'|
                    data[i,VarName]=='RU'|data[i,VarName]=='UA'){
                        output[i]<-'EasternEurope'
                }
                if (data[i,VarName]=='CY'| data[i,VarName]=='ES'|data[i,VarName]=="PT"){
                        output[i]<-'SouthernEurope'
                }
                if (data[i,VarName]=='BE'| data[i,VarName]=='CH'|data[i,VarName]=="DE"|
                    data[i,VarName]=='FR'|data[i,VarName]=='GB'|data[i,VarName]=='IE'|
                    data[i,VarName]=='NL'){
                        output[i]<-'WesternEurope'
                }
                if (data[i,VarName]=='FI'| data[i,VarName]=='NO'|data[i,VarName]=="SE"|
                    data[i,VarName]=='DK'){
                        output[i]<-'NorthernEurope'
                }
        }
        return(output)
}

AllCountry_EconomicCrisis_M0$Region<-CountryGrouping(data = AllCountry_EconomicCrisis_M0,
                                                     VarName = "cntry")

#Predict the M0 in a new column in standardized Scale:
AllCountry_EconomicCrisis_M0$M0Pre_Stad<-predict(EmptyModel_noQuadTerm_stad)

##Plotting for standardized scale
RanSloTime_byCountries_Stad<-ggplot(data = AllCountry_EconomicCrisis_M0, 
                                    aes(x=time_tj,y=M0Pre_Stad, group=cntry, color=cntry)) + 
        geom_smooth(se=FALSE,method="lm")

ggplot(data = AllCountry_EconomicCrisis_M0, 
       aes(x=time_tj,y=M0Pre_Stad, group=cntry, color=cntry)) + 
        geom_smooth(se=FALSE,method="lm")+facet_grid(.~Region)

####-------------------------------------------------------------------------------------------
##Subset the data to have only Eastern and Northern Europe
EastNorthEurope<-AllCountry_EconomicCrisis_M0 %>%
        filter(Region=="EasternEurope" | Region=="NorthernEurope")

ggplot(data = EastNorthEurope, aes(x=time_tj,y=M0Pre_Stad, group=cntry, color=cntry))+
        geom_smooth(se=FALSE, method="lm")+facet_grid(.~Region) + 
        coord_cartesian(ylim = c(-0.6,0.9))+
        xlab("time points")+ylab("Perceived Ethnic Threat (Standardized)")

##Subset the data to have Western and Southern Europe
WestSouthEurope<-AllCountry_EconomicCrisis_M0 %>%
        filter(Region=="SouthernEurope" | Region=="WesternEurope")

ggplot(data = WestSouthEurope, aes(x=time_tj, y=M0Pre_Stad, group=cntry,color=cntry))+
        geom_smooth(se=FALSE, method = "lm") + facet_grid(.~Region)+
        coord_cartesian(ylim = c(-0.6,0.9))+
        xlab("time points")+ylab("Perceived Ethnic Threat (Standardized)")


####---------------------------------------------------------------------------------------------------
##M1: Empty model with only time_tj and time_tj square
###First we fit the empty models with standardized perceived ethnic threat
EmptyModel_QuadTerm_stad<-lmer(scale(PerceivedEthnicThreat)~1+time_tj+time_tj_square+
                                       (1+time_tj+time_tj_square|cntry)+(1|CountryYear),
                               data = AllCountry_EconomicCrisis)

summary(EmptyModel_QuadTerm_stad)

####---------------------------------------------------------------------------------------------------
##M2: model with dummy 2008 --> corresponding to the table M1
##First without Dummy 2008 as random effect
M2_Dummy2008_stad<-lmer(scale(PerceivedEthnicThreat)~1+Time2008+time_tj+
                                (1+time_tj|cntry)+(1|CountryYear),
                        data = AllCountry_EconomicCrisis)
summary(M2_Dummy2008_stad)

M2_Dummy2008Ran_stad<-lmer(scale(PerceivedEthnicThreat)~1+Time2008+time_tj+
                                   (1+time_tj+Time2008|cntry)+(1|CountryYear),
                           data = AllCountry_EconomicCrisis)
summary(M2_Dummy2008Ran_stad)

##To plot the catepillar plot of uj:
u_total<-ranef(M2_Dummy2008Ran_stad,condVar=TRUE)
dotplot(u_total, scales=(list(x=list(relation='free'))))

####Model with dummy 2010
M2_Dummy2010_stad<-lmer(scale(PerceivedEthnicThreat)~1+Time2010+time_tj+
                                (1+time_tj|cntry)+(1|CountryYear),
                        data = AllCountry_EconomicCrisis)
summary(M2_Dummy2010_stad)

M2_Dummy2010Ran_stad<-lmer(scale(PerceivedEthnicThreat)~1+Time2010+time_tj+
                                   (1+time_tj+Time2010|cntry)+(1|CountryYear),
                           data = AllCountry_EconomicCrisis)
summary(M2_Dummy2010Ran_stad)

u_total<-ranef(M2_Dummy2010Ran_stad,condVar=TRUE)
dotplot(u_total, scales=(list(x=list(relation='free'))))


####---------------------------------------------------------------------------------------------------
##M3: model with dummy 2010
M3_Dummy2010_stad<-lmer(scale(PerceivedEthnicThreat)~1+Time2010+time_tj+
                                (1+time_tj|cntry)+(1|CountryYear),
                        data = AllCountry_EconomicCrisis)
summary(M3_Dummy2010_stad)



####---------------------------------------------------------------------------------------------------
##M4: model with dummy 2008 and all individual variables --> correspond to M2 in the table
M4_Dummy2008_IndVar_stad<-lmer(scale(PerceivedEthnicThreat)~1+Time2008+time_tj+
                                       GenderFac+AgeGroup+scale(eduyrs)+MainAct+scale(IncomeInsecure)+
                                       scale(urbanization)+scale(religiosity)+LeftRight+
                                       (1+time_tj+Time2008|cntry)+(1|CountryYear),
                               data = AllCountry_EconomicCrisis)
summary(M4_Dummy2008_IndVar_stad)


####---------------------------------------------------------------------------------------------------
##M5_1: model with dummy 2008, all individual variables and unemployment rate cross-sectional and longitudinal term
M5_UnemployRate_Stad<-lmer(scale(PerceivedEthnicThreat)~1+Time2008+time_tj+
                                   GenderFac+AgeGroup+scale(eduyrs)+MainAct+scale(IncomeInsecure)+
                                   scale(urbanization)+scale(religiosity)+LeftRight+
                                   scale(Unemploy_CrossSec)+scale(Unemploy_Longi)+
                                   (1+time_tj+Time2008|cntry)+(1|CountryYear),
                           data = AllCountry_EconomicCrisis)

summary(M5_UnemployRate_Stad)

##M5_2: model with dummy 2008, all individual variables and GDP growth cross-sectional and longitudinal term
M5_GDPGrowth_Stad<-lmer(scale(PerceivedEthnicThreat)~1+Time2008+time_tj+
                                GenderFac+AgeGroup+scale(eduyrs)+MainAct+scale(IncomeInsecure)+
                                scale(urbanization)+scale(religiosity)+LeftRight+
                                scale(GDPGrowth_CrossSec)+scale(GDPGrowth_Longi)+
                                (1+time_tj+Time2008|cntry)+(1|CountryYear),
                        data = AllCountry_EconomicCrisis)
summary(M5_GDPGrowth_Stad)

##M5_3: model with dummy 2008, all individual variables and income insecurity cross-sectional and longitudinal term
M5_IncomeInsecure_Stad<-lmer(scale(PerceivedEthnicThreat)~1+Time2008+time_tj+
                                     GenderFac+AgeGroup+scale(eduyrs)+MainAct+scale(IncomeInsecure)+
                                     scale(urbanization)+scale(religiosity)+LeftRight+
                                     scale(IncomeInsecure_CrossSec)+scale(IncomeInsecure_Longi)+
                                     (1+time_tj+Time2008|cntry)+(1|CountryYear),
                             data = AllCountry_EconomicCrisis)
summary(M5_IncomeInsecure_Stad)

####---------------------------------------------------------------------------------------------------
##M6: model with dummy 2008, all individual variables 
##and unemployment rate cross-sectional and longitudinal term
##and GDP growth per capita cross-sectional and longitudinal term
##and country income insecurity cross-sectional and longitudinal term
M6_UnemployRate_GDPGrowth_IncomeInsecure_Stad<-lmer(scale(PerceivedEthnicThreat)~1+Time2008+time_tj+
                                             GenderFac+AgeGroup+scale(eduyrs)+MainAct+scale(IncomeInsecure)+
                                             scale(urbanization)+scale(religiosity)+LeftRight+
                                             scale(Unemploy_CrossSec)+scale(Unemploy_Longi)+
                                             scale(GDPGrowth_CrossSec)+scale(GDPGrowth_Longi)+
                                             scale(IncomeInsecure_CrossSec)+scale(IncomeInsecure_Longi)+
                                             (1+time_tj+Time2008|cntry)+(1|CountryYear),
                                     data = AllCountry_EconomicCrisis)
summary(M6_UnemployRate_GDPGrowth_IncomeInsecure_Stad)

####---------------------------------------------------------------------------------------------------
##M7: model with interaction term of income insecurity
##First make the random slope for income insecurity

M7_IncomeInsecureRan_Stad<-lmer(scale(PerceivedEthnicThreat)~1+Time2008+time_tj+
                                             GenderFac+AgeGroup+scale(eduyrs)+MainAct+scale(IncomeInsecure)+
                                             scale(urbanization)+scale(religiosity)+LeftRight+
                                             scale(Unemploy_CrossSec)+scale(Unemploy_Longi)+
                                             scale(GDPGrowth_CrossSec)+scale(GDPGrowth_Longi)+
                                        scale(IncomeInsecure_CrossSec)+scale(IncomeInsecure_Longi)+
                                             (1+time_tj+Time2008|cntry)+(1+scale(IncomeInsecure)|CountryYear),
                                     data = AllCountry_EconomicCrisis)
summary(M7_IncomeInsecureRan_Stad)

M7_IncomeInsecure_GDP_Inter<-lmer(scale(PerceivedEthnicThreat)~1+Time2008+time_tj+
                                               GenderFac+AgeGroup+scale(eduyrs)+MainAct+scale(IncomeInsecure)+
                                               scale(urbanization)+scale(religiosity)+LeftRight+
                                               scale(Unemploy_CrossSec)+scale(Unemploy_Longi)+
                                               scale(GDPGrowth_CrossSec)+scale(GDPGrowth_Longi)+
                                          scale(IncomeInsecure_CrossSec)+scale(IncomeInsecure_Longi)+
                                               scale(IncomeInsecure):scale(GDPGrowth_Longi)+
                                       (1+time_tj+Time2008|cntry)+(1+scale(IncomeInsecure)|CountryYear),
                                       data = AllCountry_EconomicCrisis)
summary(M7_IncomeInsecure_GDP_Inter)

M7_IncomeInsecure_Unemploy_Inter<-lmer(scale(PerceivedEthnicThreat)~1+Time2008+time_tj+
                                               GenderFac+AgeGroup+scale(eduyrs)+MainAct+scale(IncomeInsecure)+
                                               scale(urbanization)+scale(religiosity)+LeftRight+
                                               scale(Unemploy_CrossSec)+scale(Unemploy_Longi)+
                                               scale(GDPGrowth_CrossSec)+scale(GDPGrowth_Longi)+
                                               scale(IncomeInsecure_CrossSec)+scale(IncomeInsecure_Longi)+
                                               scale(IncomeInsecure):scale(Unemploy_Longi)+
                                               (1+time_tj+Time2008|cntry)+(1+scale(IncomeInsecure)|CountryYear),
                                       data = AllCountry_EconomicCrisis)
summary(M7_IncomeInsecure_Unemploy_Inter)
