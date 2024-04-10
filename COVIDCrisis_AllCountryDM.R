####To bind all the countries together into one data frame for Political Crisis
AllCountry_COVIDCrisis<-rbind(AT_COVIDCrisis, BE_COVIDCrisis, BG_COVIDCrisis,
                                  CH_COVIDCrisis, CY_COVIDCrisis, CZ_COVIDCrisis,
                                  DE_COVIDCrisis, EE_COVIDCrisis, ES_COVIDCrisis,
                                  FI_COVIDCrisis, FR_COVIDCrisis, GB_COVIDCrisis,
                                  HR_COVIDCrisis, HU_COVIDCrisis, IE_COVIDCrisis,
                                  IS_COVIDCrisis, IT_COVIDCrisis, LT_COVIDCrisis,
                                  LV_COVIDCrisis, ME_COVIDCrisis, NL_COVIDCrisis,
                                  NO_COVIDCrisis, PL_COVIDCrisis, PT_COVIDCrisis,
                                  RS_COVIDCrisis, SE_COVIDCrisis, SI_COVIDCrisis,
                                  SK_COVIDCrisis)

rm(list=c("AT_COVIDCrisis", "BE_COVIDCrisis", "BG_COVIDCrisis",
   "CH_COVIDCrisis", "CY_COVIDCrisis", "CZ_COVIDCrisis",
   "DE_COVIDCrisis", "EE_COVIDCrisis", "ES_COVIDCrisis",
   "FI_COVIDCrisis", "FR_COVIDCrisis", "GB_COVIDCrisis",
   "HR_COVIDCrisis", "HU_COVIDCrisis", "IE_COVIDCrisis",
   "IS_COVIDCrisis", "IT_COVIDCrisis", "LT_COVIDCrisis",
   "LV_COVIDCrisis", "ME_COVIDCrisis", "NL_COVIDCrisis",
   "NO_COVIDCrisis", "PL_COVIDCrisis", "PT_COVIDCrisis",
   "RS_COVIDCrisis", "SE_COVIDCrisis", "SI_COVIDCrisis",
   "SK_COVIDCrisis"))

####To make the time(tj) variable
AllCountry_COVIDCrisis$inwyys<-as.numeric(AllCountry_COVIDCrisis$inwyys)
AllCountry_COVIDCrisis<-AllCountry_COVIDCrisis %>%
        mutate(time_tj=inwyys-2018)

####To make the time(tj)_Square variable
AllCountry_COVIDCrisis<-AllCountry_COVIDCrisis %>%
        mutate(time_tj_square=time_tj^2)


####----------------------------------------------------------------------------------------------------------
####To make the variable to identify the second level: country-year
AllCountry_COVIDCrisis$CountryYear<-paste(AllCountry_COVIDCrisis$cntry, 
                                          AllCountry_COVIDCrisis$inwyys,
                                          sep = "-")

####----------------------------------------------------------------------------------------------------------
####Treat the missing values of the 3 threat items:
defineNA_77<-function(data){
        output<-data
        for (i in seq_along(names(data))) {
                output[,i]<-as.numeric(sub(77,NA,data[,i]))    
        }
        return(output)
}

defineNA_88<-function(data){
        output<-data
        for (i in seq_along(names(data))) {
                output[,i]<-as.numeric(sub(88,NA,data[,i]))    
        }
        return(output)
}

defineNA_99<-function(data){
        output<-data
        for (i in seq_along(names(data))) {
                output[,i]<-as.numeric(sub(99,NA,data[,i]))    
        }
        return(output)
}

AllCountry_COVIDCrisis[,4:6]<-defineNA_77(AllCountry_COVIDCrisis[,4:6])
AllCountry_COVIDCrisis[,4:6]<-defineNA_88(AllCountry_COVIDCrisis[,4:6])
AllCountry_COVIDCrisis[,4:6]<-defineNA_99(AllCountry_COVIDCrisis[,4:6])

rm(defineNA_77)
rm(defineNA_88)
rm(defineNA_99)


####Reverse the three threat items so that the higher value correspond to higher threat
AllCountry_COVIDCrisis<-AllCountry_COVIDCrisis %>%
        mutate(EcoThreat=(-1)*imbgeco+10,
               CulThreat=(-1)*imueclt+10,
               GenThreat=(-1)*imwbcnt+10)


####Make the mean scale of the 3 items:
MeanScale_3items<-function(NumericVector1, NumericVector2, NumericVector3){
        output<-as.numeric()
        for (i in seq_along(NumericVector1)){
                if (is.na(NumericVector1[i])){
                        output[i]<-NA
                }
                if (is.na(NumericVector2[i])){
                        output[i]<-NA
                }
                if (is.na(NumericVector3[i])){
                        output[i]<-NA
                }
                else {
                        output[i]<-round((NumericVector1[i]+NumericVector2[i]+NumericVector3[i])/3,digits = 3)     
                }
        }
        return(output)
}

AllCountry_COVIDCrisis$PerceivedEthnicThreat<-MeanScale_3items(NumericVector1 = AllCountry_COVIDCrisis$EcoThreat,
                                                                   NumericVector2 = AllCountry_COVIDCrisis$CulThreat,
                                                                   NumericVector3 = AllCountry_COVIDCrisis$GenThreat)

rm(MeanScale_3items)


####----------------------------------------------------------------------------------------------------------
####Make the gender as factor
AllCountry_COVIDCrisis$GenderFac<-sub(1, "male", AllCountry_COVIDCrisis$gndr)
AllCountry_COVIDCrisis$GenderFac<-sub(2, "female", AllCountry_COVIDCrisis$GenderFac)
AllCountry_COVIDCrisis$GenderFac<-sub(9, NA, AllCountry_COVIDCrisis$GenderFac)

AllCountry_COVIDCrisis$GenderFac<-factor(AllCountry_COVIDCrisis$GenderFac,
                                             levels = c("female","male"))


####----------------------------------------------------------------------------------------------------------
####Turn age into age group:
##Transform age metric into age categorical
##Make a function that will get rid of the NA as well
MetricToCategorical<-function(data, NumVarName){
        output<-as.character()
        for (i in seq_along(data[[NumVarName]])){
                if (data[i,NumVarName]>=999){
                        output[i]<-NA
                }
                if(data[i,NumVarName]>=14 & data[i,NumVarName]<=24){
                        output[i]<-"14-24 yo"
                }
                if(data[i,NumVarName]>=25 & data[i,NumVarName]<=34){
                        output[i]<-"25-34 yo"
                }
                if(data[i,NumVarName]>=35 & data[i,NumVarName]<=44){
                        output[i]<-"35-44 yo"
                }
                if(data[i,NumVarName]>=45 & data[i,NumVarName]<=54){
                        output[i]<-"45-54 yo"
                }
                if(data[i,NumVarName]>=55 & data[i,NumVarName]<=64){
                        output[i]<-"55-64 yo"
                }
                if(data[i,NumVarName]>=65 & data[i,NumVarName]<=74){
                        output[i]<-"65-74 yo"
                }
                if(data[i,NumVarName]>=75){
                        output[i]<-"75 yo and over"
                }
        }
        return(output)
}

AllCountry_COVIDCrisis$AgeGroup<-MetricToCategorical(data = AllCountry_COVIDCrisis,
                                                         NumVarName = "agea")

##Turn the AgeGroup from character to factor
AllCountry_COVIDCrisis$AgeGroup<-factor(AllCountry_COVIDCrisis$AgeGroup,
                                            levels = c("45-54 yo","14-24 yo","25-34 yo",
                                                       "35-44 yo","55-64 yo","65-74 yo",
                                                       "75 yo and over"))

rm(MetricToCategorical)


####----------------------------------------------------------------------------------------------------------
####Treat missing values of the eduyrs
AllCountry_COVIDCrisis$eduyrs<-as.numeric(sub(76,NA,AllCountry_COVIDCrisis$eduyrs))
AllCountry_COVIDCrisis$eduyrs<-as.numeric(sub(77,NA,AllCountry_COVIDCrisis$eduyrs))
AllCountry_COVIDCrisis$eduyrs<-as.numeric(sub(88,NA,AllCountry_COVIDCrisis$eduyrs))
AllCountry_COVIDCrisis$eduyrs<-as.numeric(sub(99,NA,AllCountry_COVIDCrisis$eduyrs))


####----------------------------------------------------------------------------------------------------------
###Main activities categories: 
#1. paid work
#2. Education == In education
#3. Unemployed, looking for job == Unemployed
#4. Unemployed, not looking for job == Unemployed
#5. Permanently sick or disabled == Disabled
#6. Retired == Retired
#7. Community or military service == Other
#8. Housework, looking after children, others == Doing Housework
#9. Other == Other

AllCountry_COVIDCrisis$MainAct<-sub(77, NA, AllCountry_COVIDCrisis$mnactic)
AllCountry_COVIDCrisis$MainAct<-sub(88, NA, AllCountry_COVIDCrisis$MainAct)
AllCountry_COVIDCrisis$MainAct<-sub(99, NA, AllCountry_COVIDCrisis$MainAct)
AllCountry_COVIDCrisis$MainAct<-sub(1,"paid work", AllCountry_COVIDCrisis$MainAct)
AllCountry_COVIDCrisis$MainAct<-sub(2, "In education", AllCountry_COVIDCrisis$MainAct)
AllCountry_COVIDCrisis$MainAct<-sub(3, "Unemployed", AllCountry_COVIDCrisis$MainAct)
AllCountry_COVIDCrisis$MainAct<-sub(4, "Unemployed", AllCountry_COVIDCrisis$MainAct)
AllCountry_COVIDCrisis$MainAct<-sub(5, "Disabled", AllCountry_COVIDCrisis$MainAct)
AllCountry_COVIDCrisis$MainAct<-sub(6, "retired", AllCountry_COVIDCrisis$MainAct)
AllCountry_COVIDCrisis$MainAct<-sub(7, "Other", AllCountry_COVIDCrisis$MainAct)
AllCountry_COVIDCrisis$MainAct<-sub(8, "Doing Housework", AllCountry_COVIDCrisis$MainAct)
AllCountry_COVIDCrisis$MainAct<-sub(9, "Other", AllCountry_COVIDCrisis$MainAct)

AllCountry_COVIDCrisis$MainAct<-factor(AllCountry_COVIDCrisis$MainAct,
                                           levels = c("Unemployed","paid work","In education",
                                                      "Disabled","Doing Housework",
                                                      "retired","Other"))


####----------------------------------------------------------------------------------------------------------
##Subjective Income Insecurity:
##Treating NA:
AllCountry_COVIDCrisis$hincfel<-as.numeric(sub(7,NA,AllCountry_COVIDCrisis$hincfel))
AllCountry_COVIDCrisis$hincfel<-as.numeric(sub(8,NA,AllCountry_COVIDCrisis$hincfel))
AllCountry_COVIDCrisis$hincfel<-as.numeric(sub(9,NA,AllCountry_COVIDCrisis$hincfel))

##Change the colname into SubjectiveIncomeInsecurity
colnames(AllCountry_COVIDCrisis)[11]<-"IncomeInsecure"


####----------------------------------------------------------------------------------------------------------
##Urbanization:
##First treat the NA:
AllCountry_COVIDCrisis$domicil<-as.numeric(sub(7,NA,AllCountry_COVIDCrisis$domicil))
AllCountry_COVIDCrisis$domicil<-as.numeric(sub(8,NA,AllCountry_COVIDCrisis$domicil))
AllCountry_COVIDCrisis$domicil<-as.numeric(sub(9,NA,AllCountry_COVIDCrisis$domicil))


##Reverse the scale to make the new variable named Urbanization
AllCountry_COVIDCrisis<-AllCountry_COVIDCrisis %>%
        mutate(urbanization=(-1)*domicil+6)


####----------------------------------------------------------------------------------------------------------
##Religiosity
##Treating NA:
AllCountry_COVIDCrisis$rlgdgr<-as.numeric(sub(77,NA,AllCountry_COVIDCrisis$rlgdgr))
AllCountry_COVIDCrisis$rlgdgr<-as.numeric(sub(88,NA,AllCountry_COVIDCrisis$rlgdgr))
AllCountry_COVIDCrisis$rlgdgr<-as.numeric(sub(99,NA,AllCountry_COVIDCrisis$rlgdgr))

##change the column name to religiosity
colnames(AllCountry_COVIDCrisis)[13]<-"religiosity"


####----------------------------------------------------------------------------------------------------------
##Left-right Scale
##Turning the scale into a categorical variables: left, center, right, and missing
MetricToCategorical2<-function(data, NumVarName){
        output<-as.character()
        for (i in seq_along(data[[NumVarName]])){
                if (data[i,NumVarName]>=77){
                        output[i]<-"missing"
                }
                if(data[i,NumVarName]>=0 & data[i,NumVarName]<=4){
                        output[i]<-"left"
                }
                if(data[i,NumVarName]==5){
                        output[i]<-"center"
                }
                if(data[i,NumVarName]>=6 & data[i,NumVarName]<=10){
                        output[i]<-"right"
                }
        }
        return(output)
}

AllCountry_COVIDCrisis$LeftRight<-MetricToCategorical2(data = AllCountry_COVIDCrisis,
                                                           NumVarName = "lrscale")

AllCountry_COVIDCrisis$LeftRight<-factor(AllCountry_COVIDCrisis$LeftRight,
                                             levels = c("left","center","right","missing"))

rm(MetricToCategorical2)


####----------------------------------------------------------------------------------------------------------
##time-point for crisis:
#Dummy of 2018:
MakeDummy_2018<-function(data, NumVarName){
        output<-as.numeric()
        for (i in seq_along(data[[NumVarName]])){
                if (data[i,NumVarName]<2018 | data[i,NumVarName]>2018){
                        output[i]<-0
                }
                else {
                        output[i]<-1   
                }
        }
        return(output)
}

AllCountry_COVIDCrisis$Time2018<-MakeDummy_2018(data = AllCountry_COVIDCrisis,
                                                    NumVarName = "inwyys")

rm(MakeDummy_2018)

#Dummy of 2019:
MakeDummy_2019<-function(data, NumVarName){
        output<-as.numeric()
        for (i in seq_along(data[[NumVarName]])){
                if (data[i,NumVarName]<2019 | data[i,NumVarName]>2019){
                        output[i]<-0
                }
                else {
                        output[i]<-1   
                }
        }
        return(output)
}

AllCountry_COVIDCrisis$Time2019<-MakeDummy_2019(data = AllCountry_COVIDCrisis,
                                                NumVarName = "inwyys")

rm(MakeDummy_2019)

#Dummy of 2020:
MakeDummy_2020<-function(data, NumVarName){
        output<-as.numeric()
        for (i in seq_along(data[[NumVarName]])){
                if (data[i,NumVarName]<2020 | data[i,NumVarName]>2020){
                        output[i]<-0
                }
                else {
                        output[i]<-1   
                }
        }
        return(output)
}

AllCountry_COVIDCrisis$Time2020<-MakeDummy_2020(data = AllCountry_COVIDCrisis,
                                                NumVarName = "inwyys")

rm(MakeDummy_2020)

#Dummy of 2021:
MakeDummy_2021<-function(data, NumVarName){
        output<-as.numeric()
        for (i in seq_along(data[[NumVarName]])){
                if (data[i,NumVarName]<2021 | data[i,NumVarName]>2021){
                        output[i]<-0
                }
                else {
                        output[i]<-1   
                }
        }
        return(output)
}

AllCountry_COVIDCrisis$Time2021<-MakeDummy_2021(data = AllCountry_COVIDCrisis,
                                                NumVarName = "inwyys")

rm(MakeDummy_2021)

#Dummy of 2022:
MakeDummy_2022<-function(data, NumVarName){
        output<-as.numeric()
        for (i in seq_along(data[[NumVarName]])){
                if (data[i,NumVarName]<2022 | data[i,NumVarName]>2022){
                        output[i]<-0
                }
                else {
                        output[i]<-1   
                }
        }
        return(output)
}

AllCountry_COVIDCrisis$Time2022<-MakeDummy_2022(data = AllCountry_COVIDCrisis,
                                                NumVarName = "inwyys")

rm(MakeDummy_2022)



####----------------------------------------------------------------------------------------------------------
##Change the colname of MeanUnemploymentRate to Unemploy_CrossSec
colnames(AllCountry_COVIDCrisis)[18]<-"Unemploy_CrossSec"

####----------------------------------------------------------------------------------------------------------
##Make the longitudinal term of Unemployment rate after subtracting the mean unemployment:
AllCountry_COVIDCrisis<-AllCountry_COVIDCrisis %>%
        mutate(Unemploy_Longi=UnemploymentRate-Unemploy_CrossSec)


####----------------------------------------------------------------------------------------------------------
##Change the colname of MeanGDPGrowth to GDPGrowth_CrossSec
colnames(AllCountry_COVIDCrisis)[19]<-"GDPGrowth_CrossSec"


####----------------------------------------------------------------------------------------------------------
##Make the longitudinal term of GDP growth per capita after subtracting the mean of GDP growth per capita:
AllCountry_COVIDCrisis<-AllCountry_COVIDCrisis %>%
        mutate(GDPGrowth_Longi=GDPGrowth-GDPGrowth_CrossSec)


####----------------------------------------------------------------------------------------------------------
##Make the cross-sectional term of Country Income Insecurity:
AllCountry_COVIDCrisis<-AllCountry_COVIDCrisis %>%
        group_by(cntry) %>%
        mutate(IncomeInsecure_CrossSec=mean(IncomeInsecure, na.rm=TRUE))

##Make the country-year level income insecurity:
AllCountry_COVIDCrisis<-AllCountry_COVIDCrisis %>%
        group_by(CountryYear) %>%
        mutate(IncomeInsecure_Full=mean(IncomeInsecure, na.rm=TRUE))

##Make the longitudinal term of country income insecurity:
AllCountry_COVIDCrisis<-AllCountry_COVIDCrisis %>%
        mutate(IncomeInsecure_Longi=IncomeInsecure_Full-IncomeInsecure_CrossSec)


###-----------------------------------------------------------------------------------------------------------
##List wise deletion to get rid of all the NA
AllCountry_COVIDCrisis<-na.omit(AllCountry_COVIDCrisis)
