####To bind all the countries together into one data frame for Political Crisis
AllCountry_PoliticalCrisis<-rbind(BE_PoliticalCrisis, CZ_PoliticalCrisis, EE_PoliticalCrisis,
                                  FI_PoliticalCrisis, FR_PoliticalCrisis, DE_PoliticalCrisis,
                                  HU_PoliticalCrisis, IE_PoliticalCrisis, LT_PoliticalCrisis,
                                  NL_PoliticalCrisis, NO_PoliticalCrisis, PL_PoliticalCrisis,
                                  PT_PoliticalCrisis, SI_PoliticalCrisis, ES_PoliticalCrisis,
                                  SE_PoliticalCrisis, CH_PoliticalCrisis, GB_PoliticalCrisis)

####To make the time(tj) variable
AllCountry_PoliticalCrisis$time_tj<-as.numeric(sub(2012, 0, AllCountry_PoliticalCrisis$inwyys))
AllCountry_PoliticalCrisis$time_tj<-as.numeric(sub(2013, 1, AllCountry_PoliticalCrisis$time_tj))
AllCountry_PoliticalCrisis$time_tj<-as.numeric(sub(2014, 2, AllCountry_PoliticalCrisis$time_tj))
AllCountry_PoliticalCrisis$time_tj<-as.numeric(sub(2015, 3, AllCountry_PoliticalCrisis$time_tj))
AllCountry_PoliticalCrisis$time_tj<-as.numeric(sub(2016, 4, AllCountry_PoliticalCrisis$time_tj))
AllCountry_PoliticalCrisis$time_tj<-as.numeric(sub(2017, 5, AllCountry_PoliticalCrisis$time_tj))
AllCountry_PoliticalCrisis$time_tj<-as.numeric(sub(2018, 6, AllCountry_PoliticalCrisis$time_tj))
AllCountry_PoliticalCrisis$time_tj<-as.numeric(sub(2019, 7, AllCountry_PoliticalCrisis$time_tj))

####To make the time(tj)_Square variable
AllCountry_PoliticalCrisis<-AllCountry_PoliticalCrisis %>%
        mutate(time_tj_square=(time_tj)^2)

####----------------------------------------------------------------------------------------------------------
####To make the variable to identify the second level: country-year
AllCountry_PoliticalCrisis$CountryYear<-paste(AllCountry_PoliticalCrisis$cntry, AllCountry_PoliticalCrisis$inwyys,
                                              sep="-")


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

AllCountry_PoliticalCrisis[,4:6]<-defineNA_77(AllCountry_PoliticalCrisis[,4:6])
AllCountry_PoliticalCrisis[,4:6]<-defineNA_88(AllCountry_PoliticalCrisis[,4:6])
AllCountry_PoliticalCrisis[,4:6]<-defineNA_99(AllCountry_PoliticalCrisis[,4:6])

####Reverse the three threat items so that the higher value correspond to higher threat
AllCountry_PoliticalCrisis<-AllCountry_PoliticalCrisis %>%
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

AllCountry_PoliticalCrisis$PerceivedEthnicThreat<-MeanScale_3items(NumericVector1 = AllCountry_PoliticalCrisis$EcoThreat,
                                                                   NumericVector2 = AllCountry_PoliticalCrisis$CulThreat,
                                                                   NumericVector3 = AllCountry_PoliticalCrisis$GenThreat)


####----------------------------------------------------------------------------------------------------------
####Make the gender as factor
AllCountry_PoliticalCrisis$GenderFac<-sub(1, "male", AllCountry_PoliticalCrisis$gndr)
AllCountry_PoliticalCrisis$GenderFac<-sub(2, "female", AllCountry_PoliticalCrisis$GenderFac)
AllCountry_PoliticalCrisis$GenderFac<-sub(9, NA, AllCountry_PoliticalCrisis$GenderFac)

AllCountry_PoliticalCrisis$GenderFac<-factor(AllCountry_PoliticalCrisis$GenderFac,
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

AllCountry_PoliticalCrisis$AgeGroup<-MetricToCategorical(data = AllCountry_PoliticalCrisis,
                                                        NumVarName = "agea")

##Turn the AgeGroup from character to factor
AllCountry_PoliticalCrisis$AgeGroup<-factor(AllCountry_PoliticalCrisis$AgeGroup,
                                           levels = c("45-54 yo","14-24 yo","25-34 yo",
                                                      "35-44 yo","55-64 yo","65-74 yo",
                                                      "75 yo and over"))


####----------------------------------------------------------------------------------------------------------
####Treat missing values of the eduyrs
AllCountry_PoliticalCrisis$eduyrs<-as.numeric(sub(77,NA,AllCountry_PoliticalCrisis$eduyrs))
AllCountry_PoliticalCrisis$eduyrs<-as.numeric(sub(88,NA,AllCountry_PoliticalCrisis$eduyrs))
AllCountry_PoliticalCrisis$eduyrs<-as.numeric(sub(99,NA,AllCountry_PoliticalCrisis$eduyrs))

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

AllCountry_PoliticalCrisis$MainAct<-sub(77, NA, AllCountry_PoliticalCrisis$mnactic)
AllCountry_PoliticalCrisis$MainAct<-sub(88, NA, AllCountry_PoliticalCrisis$MainAct)
AllCountry_PoliticalCrisis$MainAct<-sub(99, NA, AllCountry_PoliticalCrisis$MainAct)
AllCountry_PoliticalCrisis$MainAct<-sub(1,"paid work", AllCountry_PoliticalCrisis$MainAct)
AllCountry_PoliticalCrisis$MainAct<-sub(2, "In education", AllCountry_PoliticalCrisis$MainAct)
AllCountry_PoliticalCrisis$MainAct<-sub(3, "Unemployed", AllCountry_PoliticalCrisis$MainAct)
AllCountry_PoliticalCrisis$MainAct<-sub(4, "Unemployed", AllCountry_PoliticalCrisis$MainAct)
AllCountry_PoliticalCrisis$MainAct<-sub(5, "Disabled", AllCountry_PoliticalCrisis$MainAct)
AllCountry_PoliticalCrisis$MainAct<-sub(6, "retired", AllCountry_PoliticalCrisis$MainAct)
AllCountry_PoliticalCrisis$MainAct<-sub(7, "Other", AllCountry_PoliticalCrisis$MainAct)
AllCountry_PoliticalCrisis$MainAct<-sub(8, "Doing Housework", AllCountry_PoliticalCrisis$MainAct)
AllCountry_PoliticalCrisis$MainAct<-sub(9, "Other", AllCountry_PoliticalCrisis$MainAct)

AllCountry_PoliticalCrisis$MainAct<-factor(AllCountry_PoliticalCrisis$MainAct,
                                          levels = c("Unemployed","paid work","In education",
                                                     "Disabled","Doing Housework",
                                                     "retired","Other"))
####----------------------------------------------------------------------------------------------------------
##Subjective Income Insecurity:
##Treating NA:
AllCountry_PoliticalCrisis$hincfel<-as.numeric(sub(7,NA,AllCountry_PoliticalCrisis$hincfel))
AllCountry_PoliticalCrisis$hincfel<-as.numeric(sub(8,NA,AllCountry_PoliticalCrisis$hincfel))
AllCountry_PoliticalCrisis$hincfel<-as.numeric(sub(9,NA,AllCountry_PoliticalCrisis$hincfel))

##Change the colname into SubjectiveIncomeInsecurity
colnames(AllCountry_PoliticalCrisis)[11]<-"IncomeInsecure"

####----------------------------------------------------------------------------------------------------------
##Urbanization:
##First treat the NA:
AllCountry_PoliticalCrisis$domicil<-as.numeric(sub(7,NA,AllCountry_PoliticalCrisis$domicil))
AllCountry_PoliticalCrisis$domicil<-as.numeric(sub(8,NA,AllCountry_PoliticalCrisis$domicil))
AllCountry_PoliticalCrisis$domicil<-as.numeric(sub(9,NA,AllCountry_PoliticalCrisis$domicil))


##Reverse the scale to make the new variable named Urbanization
AllCountry_PoliticalCrisis<-AllCountry_PoliticalCrisis %>%
        mutate(urbanization=(-1)*domicil+6)

####----------------------------------------------------------------------------------------------------------
##Religiosity
##Treating NA:
AllCountry_PoliticalCrisis$rlgdgr<-as.numeric(sub(77,NA,AllCountry_PoliticalCrisis$rlgdgr))
AllCountry_PoliticalCrisis$rlgdgr<-as.numeric(sub(88,NA,AllCountry_PoliticalCrisis$rlgdgr))
AllCountry_PoliticalCrisis$rlgdgr<-as.numeric(sub(99,NA,AllCountry_PoliticalCrisis$rlgdgr))

##change the column name to religiosity
colnames(AllCountry_PoliticalCrisis)[13]<-"religiosity"

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

AllCountry_PoliticalCrisis$LeftRight<-MetricToCategorical2(data = AllCountry_PoliticalCrisis,
                                                          NumVarName = "lrscale")

AllCountry_PoliticalCrisis$LeftRight<-factor(AllCountry_PoliticalCrisis$LeftRight,
                                            levels = c("left","center","right","missing"))



####----------------------------------------------------------------------------------------------------------
##time-point for crisis:
#Dummy of 2012:
MakeDummy_2012<-function(data, NumVarName){
        output<-as.numeric()
        for (i in seq_along(data[[NumVarName]])){
                if (data[i,NumVarName]<2012 | data[i,NumVarName]>2012){
                        output[i]<-0
                }
                else {
                        output[i]<-1   
                }
        }
        return(output)
}

AllCountry_PoliticalCrisis$Time2012<-MakeDummy_2012(data = AllCountry_PoliticalCrisis,
                                                   NumVarName = "inwyys")


#Dummy of 2013:
MakeDummy_2013<-function(data, NumVarName){
        output<-as.numeric()
        for (i in seq_along(data[[NumVarName]])){
                if (data[i,NumVarName]<2013 | data[i,NumVarName]>2013){
                        output[i]<-0
                }
                else {
                        output[i]<-1   
                }
        }
        return(output)
}

AllCountry_PoliticalCrisis$Time2013<-MakeDummy_2013(data = AllCountry_PoliticalCrisis,
                                                    NumVarName = "inwyys")

#Dummy of 2014:
MakeDummy_2014<-function(data, NumVarName){
        output<-as.numeric()
        for (i in seq_along(data[[NumVarName]])){
                if (data[i,NumVarName]<2014 | data[i,NumVarName]>2014){
                        output[i]<-0
                }
                else {
                        output[i]<-1   
                }
        }
        return(output)
}

AllCountry_PoliticalCrisis$Time2014<-MakeDummy_2014(data = AllCountry_PoliticalCrisis,
                                                    NumVarName = "inwyys")

#Dummy of 2015:
MakeDummy_2015<-function(data, NumVarName){
        output<-as.numeric()
        for (i in seq_along(data[[NumVarName]])){
                if (data[i,NumVarName]<2015 | data[i,NumVarName]>2015){
                        output[i]<-0
                }
                else {
                        output[i]<-1   
                }
        }
        return(output)
}

AllCountry_PoliticalCrisis$Time2015<-MakeDummy_2015(data = AllCountry_PoliticalCrisis,
                                                    NumVarName = "inwyys")

#Dummy of 2016:
MakeDummy_2016<-function(data, NumVarName){
        output<-as.numeric()
        for (i in seq_along(data[[NumVarName]])){
                if (data[i,NumVarName]<2016 | data[i,NumVarName]>2016){
                        output[i]<-0
                }
                else {
                        output[i]<-1   
                }
        }
        return(output)
}

AllCountry_PoliticalCrisis$Time2016<-MakeDummy_2016(data = AllCountry_PoliticalCrisis,
                                                    NumVarName = "inwyys")

#Dummy of 2017:
MakeDummy_2017<-function(data, NumVarName){
        output<-as.numeric()
        for (i in seq_along(data[[NumVarName]])){
                if (data[i,NumVarName]<2017 | data[i,NumVarName]>2017){
                        output[i]<-0
                }
                else {
                        output[i]<-1   
                }
        }
        return(output)
}

AllCountry_PoliticalCrisis$Time2017<-MakeDummy_2017(data = AllCountry_PoliticalCrisis,
                                                    NumVarName = "inwyys")

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

AllCountry_PoliticalCrisis$Time2018<-MakeDummy_2018(data = AllCountry_PoliticalCrisis,
                                                    NumVarName = "inwyys")

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

AllCountry_PoliticalCrisis$Time2019<-MakeDummy_2019(data = AllCountry_PoliticalCrisis,
                                                    NumVarName = "inwyys")

####----------------------------------------------------------------------------------------------------------
##Change the colname of MeanUnemploymentRate to Unemploy_CrossSec
colnames(AllCountry_PoliticalCrisis)[18]<-"Unemploy_CrossSec"

####----------------------------------------------------------------------------------------------------------
##Make the longitudinal term of Unemployment rate after subtracting the mean unemployment:
AllCountry_PoliticalCrisis<-AllCountry_PoliticalCrisis %>%
        mutate(Unemploy_Longi=UnemploymentRate-Unemploy_CrossSec)


####----------------------------------------------------------------------------------------------------------
##Change the colname of MeanGDPGrowth to GDPGrowth_CrossSec
colnames(AllCountry_PoliticalCrisis)[19]<-"GDPGrowth_CrossSec"

####----------------------------------------------------------------------------------------------------------
##Make the longitudinal term of GDP growth per capita after subtracting the mean of GDP growth per capita:
AllCountry_PoliticalCrisis<-AllCountry_PoliticalCrisis %>%
        mutate(GDPGrowth_Longi=GDPGrowth-GDPGrowth_CrossSec)


####----------------------------------------------------------------------------------------------------------
##Make the cross-sectional term of Country Income Insecurity:
AllCountry_PoliticalCrisis<-AllCountry_PoliticalCrisis %>%
        group_by(cntry) %>%
        mutate(IncomeInsecure_CrossSec=mean(IncomeInsecure, na.rm=TRUE))

##Make the country-year level income insecurity:
AllCountry_PoliticalCrisis<-AllCountry_PoliticalCrisis %>%
        group_by(CountryYear) %>%
        mutate(IncomeInsecure_Full=mean(IncomeInsecure, na.rm=TRUE))

##Make the longitudinal term of country income insecurity:
AllCountry_PoliticalCrisis<-AllCountry_PoliticalCrisis %>%
        mutate(IncomeInsecure_Longi=IncomeInsecure_Full-IncomeInsecure_CrossSec)

###-----------------------------------------------------------------------------------------------------------
##List wise deletion to get rid of all the NA
AllCountry_PoliticalCrisis<-na.omit(AllCountry_PoliticalCrisis)
