####To bind all the countries together into one data frame for Economic Crisis
AllCountry_EconomicCrisis<-rbind(BE_EconomicCrisis, BG_EconomicCrisis, CY_EconomicCrisis,
                                 DK_EconomicCrisis, EE_EconomicCrisis, FI_EconomicCrisis,
                                 FR_EconomicCrisis, DE_EconomicCrisis, HU_EconomicCrisis,
                                 IE_EconomicCrisis, NL_EconomicCrisis, NO_EconomicCrisis,
                                 PL_EconomicCrisis, PT_EconomicCrisis, RU_EconomicCrisis, 
                                 SK_EconomicCrisis, SI_EconomicCrisis, ES_EconomicCrisis, 
                                 SE_EconomicCrisis, CH_EconomicCrisis, UA_EconomicCrisis,
                                 GB_EconomicCrisis)

####To make the time(tj) variable
AllCountry_EconomicCrisis$time_tj<-as.numeric(sub(2006, 0, AllCountry_EconomicCrisis$inwyys))
AllCountry_EconomicCrisis$time_tj<-as.numeric(sub(2007, 1, AllCountry_EconomicCrisis$time_tj))
AllCountry_EconomicCrisis$time_tj<-as.numeric(sub(2008, 2, AllCountry_EconomicCrisis$time_tj))
AllCountry_EconomicCrisis$time_tj<-as.numeric(sub(2009, 3, AllCountry_EconomicCrisis$time_tj))
AllCountry_EconomicCrisis$time_tj<-as.numeric(sub(2010, 4, AllCountry_EconomicCrisis$time_tj))
AllCountry_EconomicCrisis$time_tj<-as.numeric(sub(2011, 5, AllCountry_EconomicCrisis$time_tj))
AllCountry_EconomicCrisis$time_tj<-as.numeric(sub(2012, 6, AllCountry_EconomicCrisis$time_tj))
AllCountry_EconomicCrisis$time_tj<-as.numeric(sub(2013, 7, AllCountry_EconomicCrisis$time_tj))

####To make the time(tj)_Square variable
Square_function<-function(NumericVector){
        output<-as.numeric()
        for (i in seq_along(NumericVector)){
                output[i]<-NumericVector[i]^2
        }
        return(output)
}

AllCountry_EconomicCrisis$time_tj_square<-Square_function(AllCountry_EconomicCrisis$time_tj)


####----------------------------------------------------------------------------------------------------------
####To make the variable to identify the second level: country-year
AllCountry_EconomicCrisis$CountryYear<-paste(AllCountry_EconomicCrisis$cntry, AllCountry_EconomicCrisis$inwyys,
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

AllCountry_EconomicCrisis[,4:6]<-defineNA_77(AllCountry_EconomicCrisis[,4:6])
AllCountry_EconomicCrisis[,4:6]<-defineNA_88(AllCountry_EconomicCrisis[,4:6])
AllCountry_EconomicCrisis[,4:6]<-defineNA_99(AllCountry_EconomicCrisis[,4:6])

####Reverse the three threat items so that the higher value correspond to higher threat
ReverseCoding_scale10<-function(data,VarName){
        output<-as.numeric()
        for(i in seq_along(data[[VarName]])){
                ifelse(is.na(data[i,VarName]), output[i]<-NA, 
                       output[i]<-(-1)*data[i,VarName]+10)
        }
        return(output)
}

AllCountry_EconomicCrisis$EcoThreat<-ReverseCoding_scale10(data = AllCountry_EconomicCrisis,
                                                           VarName = "imbgeco")
AllCountry_EconomicCrisis$CulThreat<-ReverseCoding_scale10(data = AllCountry_EconomicCrisis,
                                                           VarName = "imueclt")
AllCountry_EconomicCrisis$GenThreat<-ReverseCoding_scale10(data = AllCountry_EconomicCrisis,
                                                           VarName = "imwbcnt")


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

AllCountry_EconomicCrisis$PerceivedEthnicThreat<-MeanScale_3items(NumericVector1 = AllCountry_EconomicCrisis$EcoThreat,
                                                                  NumericVector2 = AllCountry_EconomicCrisis$CulThreat,
                                                                  NumericVector3 = AllCountry_EconomicCrisis$GenThreat)


####----------------------------------------------------------------------------------------------------------
####Make the gender as factor
AllCountry_EconomicCrisis$GenderFac<-sub(1, "male", AllCountry_EconomicCrisis$gndr)
AllCountry_EconomicCrisis$GenderFac<-sub(2, "female", AllCountry_EconomicCrisis$GenderFac)
AllCountry_EconomicCrisis$GenderFac<-sub(9, NA, AllCountry_EconomicCrisis$GenderFac)

AllCountry_EconomicCrisis$GenderFac<-factor(AllCountry_EconomicCrisis$GenderFac,
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

AllCountry_EconomicCrisis$AgeGroup<-MetricToCategorical(data = AllCountry_EconomicCrisis,
                                                        NumVarName = "agea")

##Turn the AgeGroup from character to factor
AllCountry_EconomicCrisis$AgeGroup<-factor(AllCountry_EconomicCrisis$AgeGroup,
                                           levels = c("45-54 yo","14-24 yo","25-34 yo",
                                                      "35-44 yo","55-64 yo","65-74 yo",
                                                      "75 yo and over"))


####----------------------------------------------------------------------------------------------------------
####Treat missing values of the eduyrs
AllCountry_EconomicCrisis$eduyrs<-as.numeric(sub(77,NA,AllCountry_EconomicCrisis$eduyrs))
AllCountry_EconomicCrisis$eduyrs<-as.numeric(sub(88,NA,AllCountry_EconomicCrisis$eduyrs))
AllCountry_EconomicCrisis$eduyrs<-as.numeric(sub(99,NA,AllCountry_EconomicCrisis$eduyrs))



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
####Question: where to find self-employed, higher service class, white collar and blue collar?

AllCountry_EconomicCrisis$MainAct<-sub(77, NA, AllCountry_EconomicCrisis$mnactic)
AllCountry_EconomicCrisis$MainAct<-sub(88, NA, AllCountry_EconomicCrisis$MainAct)
AllCountry_EconomicCrisis$MainAct<-sub(99, NA, AllCountry_EconomicCrisis$MainAct)
AllCountry_EconomicCrisis$MainAct<-sub(1,"paid work", AllCountry_EconomicCrisis$MainAct)
AllCountry_EconomicCrisis$MainAct<-sub(2, "In education", AllCountry_EconomicCrisis$MainAct)
AllCountry_EconomicCrisis$MainAct<-sub(3, "Unemployed", AllCountry_EconomicCrisis$MainAct)
AllCountry_EconomicCrisis$MainAct<-sub(4, "Unemployed", AllCountry_EconomicCrisis$MainAct)
AllCountry_EconomicCrisis$MainAct<-sub(5, "Disabled", AllCountry_EconomicCrisis$MainAct)
AllCountry_EconomicCrisis$MainAct<-sub(6, "retired", AllCountry_EconomicCrisis$MainAct)
AllCountry_EconomicCrisis$MainAct<-sub(7, "Other", AllCountry_EconomicCrisis$MainAct)
AllCountry_EconomicCrisis$MainAct<-sub(8, "Doing Housework", AllCountry_EconomicCrisis$MainAct)
AllCountry_EconomicCrisis$MainAct<-sub(9, "Other", AllCountry_EconomicCrisis$MainAct)

AllCountry_EconomicCrisis$MainAct<-factor(AllCountry_EconomicCrisis$MainAct,
                                          levels = c("Unemployed","paid work","In education",
                                                     "Disabled","Doing Housework",
                                                     "retired","Other"))


####----------------------------------------------------------------------------------------------------------
##Subjective Income Insecurity:
##Treating NA:
AllCountry_EconomicCrisis$hincfel<-as.numeric(sub(7,NA,AllCountry_EconomicCrisis$hincfel))
AllCountry_EconomicCrisis$hincfel<-as.numeric(sub(8,NA,AllCountry_EconomicCrisis$hincfel))
AllCountry_EconomicCrisis$hincfel<-as.numeric(sub(9,NA,AllCountry_EconomicCrisis$hincfel))

##Change the colname into SubjectiveIncomeInsecurity
colnames(AllCountry_EconomicCrisis)[11]<-"IncomeInsecure"

####----------------------------------------------------------------------------------------------------------
##Urbanization:
##First treat the NA:
AllCountry_EconomicCrisis$domicil<-as.numeric(sub(7,NA,AllCountry_EconomicCrisis$domicil))
AllCountry_EconomicCrisis$domicil<-as.numeric(sub(8,NA,AllCountry_EconomicCrisis$domicil))
AllCountry_EconomicCrisis$domicil<-as.numeric(sub(9,NA,AllCountry_EconomicCrisis$domicil))

##Reverse the scale to make the new variable named Urbanization
ReverseCoding_scale5<-function(data,VarName){
        output<-as.numeric()
        for(i in seq_along(data[[VarName]])){
                ifelse(is.na(data[i,VarName]), output[i]<-NA, 
                       output[i]<-(-1)*data[i,VarName]+6)
        }
        return(output)
}

AllCountry_EconomicCrisis$urbanization<-ReverseCoding_scale5(data = AllCountry_EconomicCrisis,
                                                             VarName = "domicil")


####----------------------------------------------------------------------------------------------------------
##Religiosity
##Treating NA:
AllCountry_EconomicCrisis$rlgdgr<-as.numeric(sub(77,NA,AllCountry_EconomicCrisis$rlgdgr))
AllCountry_EconomicCrisis$rlgdgr<-as.numeric(sub(88,NA,AllCountry_EconomicCrisis$rlgdgr))
AllCountry_EconomicCrisis$rlgdgr<-as.numeric(sub(99,NA,AllCountry_EconomicCrisis$rlgdgr))

##change the column name to religiosity
colnames(AllCountry_EconomicCrisis)[13]<-"religiosity"


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

AllCountry_EconomicCrisis$LeftRight<-MetricToCategorical2(data = AllCountry_EconomicCrisis,
                                                          NumVarName = "lrscale")

AllCountry_EconomicCrisis$LeftRight<-factor(AllCountry_EconomicCrisis$LeftRight,
                                            levels = c("left","center","right","missing"))


####----------------------------------------------------------------------------------------------------------
##time-point for crisis:
#Dummy of 2008:
MakeDummy_2008<-function(data, NumVarName){
        output<-as.numeric()
        for (i in seq_along(data[[NumVarName]])){
                if (data[i,NumVarName]<2008 | data[i,NumVarName]>2008){
                        output[i]<-0
                }
                else {
                        output[i]<-1   
                }
        }
        return(output)
}

AllCountry_EconomicCrisis$Time2008<-MakeDummy_2008(data = AllCountry_EconomicCrisis,
                                                   NumVarName = "inwyys")


#Dummy of 2010:
MakeDummy_2010<-function(data, NumVarName){
        output<-as.numeric()
        for (i in seq_along(data[[NumVarName]])){
                if (data[i,NumVarName]<2010 | data[i,NumVarName]>2010){
                        output[i]<-0
                }
                else {
                        output[i]<-1   
                }
        }
        return(output)
}

AllCountry_EconomicCrisis$Time2010<-MakeDummy_2010(data = AllCountry_EconomicCrisis,
                                                   NumVarName = "inwyys")


####----------------------------------------------------------------------------------------------------------
##Change the colname of MeanUnemploymentRate to Unemploy_CrossSec
colnames(AllCountry_EconomicCrisis)[18]<-"Unemploy_CrossSec"


####----------------------------------------------------------------------------------------------------------
##Make the longitudinal term of Unemployment rate after subtracting the mean unemployment:
AllCountry_EconomicCrisis<-AllCountry_EconomicCrisis %>%
        mutate(Unemploy_Longi=UnemploymentRate-Unemploy_CrossSec)

####----------------------------------------------------------------------------------------------------------
##Change the colname of MeanGDPGrowth to GDPGrowth_CrossSec
colnames(AllCountry_EconomicCrisis)[19]<-"GDPGrowth_CrossSec"


####----------------------------------------------------------------------------------------------------------
##Make the longitudinal term of GDP growth per capita after subtracting the mean of GDP growth per capita:
AllCountry_EconomicCrisis<-AllCountry_EconomicCrisis %>%
        mutate(GDPGrowth_Longi=GDPGrowth-GDPGrowth_CrossSec)

####----------------------------------------------------------------------------------------------------------
##Make the cross-sectional term of Income Insecurity:
AllCountry_EconomicCrisis<-AllCountry_EconomicCrisis %>%
        group_by(cntry) %>%
        mutate(IncomeInsecure_CrossSec=mean(IncomeInsecure, na.rm=TRUE))

##Make the longitudinal term of Income Insecurity:
AllCountry_EconomicCrisis<-AllCountry_EconomicCrisis %>%
        group_by(CountryYear) %>%
        mutate(IncomeInsecure_Full=mean(IncomeInsecure,na.rm=TRUE))

AllCountry_EconomicCrisis<-AllCountry_EconomicCrisis %>%
        mutate(IncomeInsecure_Longi=IncomeInsecure_Full-IncomeInsecure_CrossSec)
