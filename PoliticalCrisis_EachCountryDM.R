######--------------------------------------------------------------------------------------------------------
##Data management for Belgium
##Read BE Round 6-9 in:
BE_Round6to9<-read.csv("./Political Crisis Data - CSV/BE-6-7-8-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
BE_Round6to9 <- BE_Round6to9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
BE_Round6to9 <- BE_Round6to9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(BE_Round6to9$inwyys)

##subset BE for each year:
BE_2012<-BE_Round6to9 %>%
        filter(inwyys==2012)
BE_2014<-BE_Round6to9 %>%
        filter(inwyys==2014)
BE_2015<-BE_Round6to9 %>%
        filter(inwyys==2015)
BE_2016<-BE_Round6to9 %>%
        filter(inwyys==2016)
BE_2017<-BE_Round6to9 %>%
        filter(inwyys==2017)
BE_2018<-BE_Round6to9 %>%
        filter(inwyys==2018)
BE_2019<-BE_Round6to9 %>%
        filter(inwyys==2019)

##Give the unemployment rate of the previous year to each year
BE_2012<-BE_2012 %>%
        mutate(UnemploymentRate=rep(0.071,times=1560))
BE_2014<-BE_2014 %>%
        mutate(UnemploymentRate=rep(0.084,times=1439))
BE_2015<-BE_2015 %>%
        mutate(UnemploymentRate=rep(0.085,times=54))
BE_2016<-BE_2016 %>%
        mutate(UnemploymentRate=rep(0.085,times=1373))
BE_2017<-BE_2017 %>%
        mutate(UnemploymentRate=rep(0.078,times=94))
BE_2018<-BE_2018 %>%
        mutate(UnemploymentRate=rep(0.071,times=1244))
BE_2019<-BE_2019 %>%
        mutate(UnemploymentRate=rep(0.060,times=192))


##Give the GDP growth of the previous year to each year
BE_2012<-BE_2012 %>%
        mutate(GDPGrowth=rep(0.004,times=1560))
BE_2014<-BE_2014 %>%
        mutate(GDPGrowth=rep(0.000,times=1439))
BE_2015<-BE_2015 %>%
        mutate(GDPGrowth=rep(0.011,times=54))
BE_2016<-BE_2016 %>%
        mutate(GDPGrowth=rep(0.015,times=1373))
BE_2017<-BE_2017 %>%
        mutate(GDPGrowth=rep(0.008,times=94))
BE_2018<-BE_2018 %>%
        mutate(GDPGrowth=rep(0.012,times=1244))
BE_2019<-BE_2019 %>%
        mutate(GDPGrowth=rep(0.013,times=192))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
BE_PoliticalCrisis<-rbind(BE_2012, BE_2014, BE_2015, BE_2016, BE_2017, BE_2018, BE_2019)
BE_PoliticalCrisis<-BE_PoliticalCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
BE_PoliticalCrisis<-BE_PoliticalCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("BE_2012","BE_2014", "BE_2015", "BE_2016", "BE_2017","BE_2018","BE_2019",
            "BE_Round6to9"))




######--------------------------------------------------------------------------------------------------------
##Data management for Czech Republic
##Read CZ Round 6-9 in:
CZ_Round6to9<-read.csv("./Political Crisis Data - CSV/CZ-6-7-8-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
CZ_Round6to9<-CZ_Round6to9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
CZ_Round6to9 <- CZ_Round6to9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(CZ_Round6to9$inwyys)

##subset CZ for each year:
CZ_2013<-CZ_Round6to9 %>%
        filter(inwyys==2013)
CZ_2014<-CZ_Round6to9 %>%
        filter(inwyys==2014)
CZ_2015<-CZ_Round6to9 %>%
        filter(inwyys==2015)
CZ_2016<-CZ_Round6to9 %>%
        filter(inwyys==2016)
CZ_2018<-CZ_Round6to9 %>%
        filter(inwyys==2018)
CZ_2019<-CZ_Round6to9 %>%
        filter(inwyys==2019)


##Give the unemployment rate of the previous year to each year
CZ_2013<-CZ_2013 %>%
        mutate(UnemploymentRate=rep(0.070,times=1813))
CZ_2014<-CZ_2014 %>%
        mutate(UnemploymentRate=rep(0.070,times=635))
CZ_2015<-CZ_2015 %>%
        mutate(UnemploymentRate=rep(0.061,times=1336))
CZ_2016<-CZ_2016 %>%
        mutate(UnemploymentRate=rep(0.050,times=2176))
CZ_2018<-CZ_2018 %>%
        mutate(UnemploymentRate=rep(0.029,times=1099))
CZ_2019<-CZ_2019 %>%
        mutate(UnemploymentRate=rep(0.022,times=1188))

##Give the GDP growth of the previous year to each year
CZ_2013<-CZ_2013 %>%
        mutate(GDPGrowth=rep(-0.009,times=1813))
CZ_2014<-CZ_2014 %>%
        mutate(GDPGrowth=rep(-0.001,times=635))
CZ_2015<-CZ_2015 %>%
        mutate(GDPGrowth=rep(0.022,times=1336))
CZ_2016<-CZ_2016 %>%
        mutate(GDPGrowth=rep(0.052,times=2176))
CZ_2018<-CZ_2018 %>%
        mutate(GDPGrowth=rep(0.049,times=1099))
CZ_2019<-CZ_2019 %>%
        mutate(GDPGrowth=rep(0.029,times=1188))

##across all time points in this country
CZ_PoliticalCrisis<-rbind(CZ_2013, CZ_2014, CZ_2015, CZ_2016, CZ_2018, CZ_2019)
CZ_PoliticalCrisis<-CZ_PoliticalCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
CZ_PoliticalCrisis<-CZ_PoliticalCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("CZ_2013","CZ_2014", "CZ_2015", "CZ_2016", "CZ_2018","CZ_2019",
            "CZ_Round6to9"))



######--------------------------------------------------------------------------------------------------------
##Data management for Estonia
##Read EE Round 6-9 in:
EE_Round6to9<-read.csv("./Political Crisis Data - CSV/EE-6-7-8-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
EE_Round6to9<-EE_Round6to9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
EE_Round6to9 <- EE_Round6to9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(EE_Round6to9$inwyys)

##subset EE for each year:
EE_2012<-EE_Round6to9 %>%
        filter(inwyys==2012)
EE_2013<-EE_Round6to9 %>%
        filter(inwyys==2013)
EE_2014<-EE_Round6to9 %>%
        filter(inwyys==2014)
EE_2016<-EE_Round6to9 %>%
        filter(inwyys==2016)
EE_2017<-EE_Round6to9 %>%
        filter(inwyys==2017)
EE_2018<-EE_Round6to9 %>%
        filter(inwyys==2018)
EE_2019<-EE_Round6to9 %>%
        filter(inwyys==2019)

##Give the unemployment rate of the previous year to each year
EE_2012<-EE_2012 %>%
        mutate(UnemploymentRate=rep(0.123,times=1613))
EE_2013<-EE_2013 %>%
        mutate(UnemploymentRate=rep(0.100,times=68))
EE_2014<-EE_2014 %>%
        mutate(UnemploymentRate=rep(0.086,times=1293))
EE_2016<-EE_2016 %>%
        mutate(UnemploymentRate=rep(0.064,times=1382))
EE_2017<-EE_2017 %>%
        mutate(UnemploymentRate=rep(0.069,times=142))
EE_2018<-EE_2018 %>%
        mutate(UnemploymentRate=rep(0.058,times=1296))
EE_2019<-EE_2019 %>%
        mutate(UnemploymentRate=rep(0.054,times=146))

##Give the GDP growth of the previous year to each year
EE_2012<-EE_2012 %>%
        mutate(GDPGrowth=rep(0.076,times=1613))
EE_2013<-EE_2013 %>%
        mutate(GDPGrowth=rep(0.036,times=68))
EE_2014<-EE_2014 %>%
        mutate(GDPGrowth=rep(0.018,times=1293))
EE_2016<-EE_2016 %>%
        mutate(GDPGrowth=rep(0.018,times=1382))
EE_2017<-EE_2017 %>%
        mutate(GDPGrowth=rep(0.031,times=142))
EE_2018<-EE_2018 %>%
        mutate(GDPGrowth=rep(0.057,times=1296))
EE_2019<-EE_2019 %>%
        mutate(GDPGrowth=rep(0.034,times=146))

##across all time points in this country
EE_PoliticalCrisis<-rbind(EE_2012, EE_2013, EE_2014, EE_2016, EE_2017, EE_2018, EE_2019)
EE_PoliticalCrisis<-EE_PoliticalCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
EE_PoliticalCrisis<-EE_PoliticalCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("EE_2012", "EE_2013", "EE_2014", "EE_2016", "EE_2017", "EE_2018", "EE_2019",
            "EE_Round6to9"))


######--------------------------------------------------------------------------------------------------------
##Data management for Finland
##Read FI Round 6-9 in:
FI_Round6to9<-read.csv("./Political Crisis Data - CSV/FI-6-7-8-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
FI_Round6to9<-FI_Round6to9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
FI_Round6to9 <- FI_Round6to9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(FI_Round6to9$inwyys)

##subset for each year:
FI_2012<-FI_Round6to9 %>%
        filter(inwyys==2012)
FI_2013<-FI_Round6to9 %>%
        filter(inwyys==2013)
FI_2014<-FI_Round6to9 %>%
        filter(inwyys==2014)
FI_2015<-FI_Round6to9 %>%
        filter(inwyys==2015)
FI_2016<-FI_Round6to9 %>%
        filter(inwyys==2016)
FI_2017<-FI_Round6to9 %>%
        filter(inwyys==2017)
FI_2018<-FI_Round6to9 %>%
        filter(inwyys==2018)
FI_2019<-FI_Round6to9 %>%
        filter(inwyys==2019)

##Give the unemployment rate of the previous year to each year
FI_2012<-FI_2012 %>%
        mutate(UnemploymentRate=rep(0.078,times=1788))
FI_2013<-FI_2013 %>%
        mutate(UnemploymentRate=rep(0.077,times=290))
FI_2014<-FI_2014 %>%
        mutate(UnemploymentRate=rep(0.082,times=1804))
FI_2015<-FI_2015 %>%
        mutate(UnemploymentRate=rep(0.087,times=163))
FI_2016<-FI_2016 %>%
        mutate(UnemploymentRate=rep(0.094,times=1467))
FI_2017<-FI_2017 %>%
        mutate(UnemploymentRate=rep(0.088,times=353))
FI_2018<-FI_2018 %>%
        mutate(UnemploymentRate=rep(0.086,times=1411))
FI_2019<-FI_2019 %>%
        mutate(UnemploymentRate=rep(0.074,times=242))

##Give the GDP growth of the previous year to each year
FI_2012<-FI_2012 %>%
        mutate(GDPGrowth=rep(0.021,times=1788))
FI_2013<-FI_2013 %>%
        mutate(GDPGrowth=rep(-0.019,times=290))
FI_2014<-FI_2014 %>%
        mutate(GDPGrowth=rep(-0.014,times=1804))
FI_2015<-FI_2015 %>%
        mutate(GDPGrowth=rep(-0.008,times=163))
FI_2016<-FI_2016 %>%
        mutate(GDPGrowth=rep(0.002,times=1467))
FI_2017<-FI_2017 %>%
        mutate(GDPGrowth=rep(0.025,times=353))
FI_2018<-FI_2018 %>%
        mutate(GDPGrowth=rep(0.030,times=1411))
FI_2019<-FI_2019 %>%
        mutate(GDPGrowth=rep(0.010,times=242))

##across all time points in this country
FI_PoliticalCrisis<-rbind(FI_2012, FI_2013, FI_2014, FI_2015,
                          FI_2016, FI_2017, FI_2018, FI_2019)
FI_PoliticalCrisis<-FI_PoliticalCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
FI_PoliticalCrisis<-FI_PoliticalCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("FI_2012", "FI_2013", "FI_2014", "FI_2015",
            "FI_2016", "FI_2017", "FI_2018", "FI_2019",
            "FI_Round6to9"))



######--------------------------------------------------------------------------------------------------------
##Data management for France
##Read FR Round 6-9 in:
FR_Round6to9<-read.csv("./Political Crisis Data - CSV/FR-6-7-8-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
FR_Round6to9<-FR_Round6to9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
FR_Round6to9 <- FR_Round6to9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(FR_Round6to9$inwyys)

##subset for each year:
FR_2013<-FR_Round6to9 %>%
        filter(inwyys==2013)
FR_2014<-FR_Round6to9 %>%
        filter(inwyys==2014)
FR_2015<-FR_Round6to9 %>%
        filter(inwyys==2015)
FR_2016<-FR_Round6to9 %>%
        filter(inwyys==2016)
FR_2017<-FR_Round6to9 %>%
        filter(inwyys==2017)
FR_2018<-FR_Round6to9 %>%
        filter(inwyys==2018)
FR_2019<-FR_Round6to9 %>%
        filter(inwyys==2019)

##Give the unemployment rate of the previous year to each year
FR_2013<-FR_2013 %>%
        mutate(UnemploymentRate=rep(0.094,times=1699))
FR_2014<-FR_2014 %>%
        mutate(UnemploymentRate=rep(0.099,times=1372))
FR_2015<-FR_2015 %>%
        mutate(UnemploymentRate=rep(0.103,times=275))
FR_2016<-FR_2016 %>%
        mutate(UnemploymentRate=rep(0.103,times=1255))
FR_2017<-FR_2017 %>%
        mutate(UnemploymentRate=rep(0.101,times=535))
FR_2018<-FR_2018 %>%
        mutate(UnemploymentRate=rep(0.094,times=975))
FR_2019<-FR_2019 %>%
        mutate(UnemploymentRate=rep(0.090,times=732))

##Give the GDP growth of the previous year to each year
FR_2013<-FR_2013 %>%
        mutate(GDPGrowth=rep(-0.002,times=1699))
FR_2014<-FR_2014 %>%
        mutate(GDPGrowth=rep(0.001,times=1372))
FR_2015<-FR_2015 %>%
        mutate(GDPGrowth=rep(0.005,times=275))
FR_2016<-FR_2016 %>%
        mutate(GDPGrowth=rep(0.008,times=1255))
FR_2017<-FR_2017 %>%
        mutate(GDPGrowth=rep(0.008,times=535))
FR_2018<-FR_2018 %>%
        mutate(GDPGrowth=rep(0.020,times=975))
FR_2019<-FR_2019 %>%
        mutate(GDPGrowth=rep(0.015,times=732))

##across all time points in this country
FR_PoliticalCrisis<-rbind(FR_2013, FR_2014, FR_2015, FR_2016, 
                          FR_2017, FR_2018, FR_2019)
FR_PoliticalCrisis<-FR_PoliticalCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
FR_PoliticalCrisis<-FR_PoliticalCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("FR_2013", "FR_2014", "FR_2015", "FR_2016", 
            "FR_2017", "FR_2018", "FR_2019",
            "FR_Round6to9"))


######--------------------------------------------------------------------------------------------------------
##Data management for Germany
##Read DE Round 6-9 in:
DE_Round6to9<-read.csv("./Political Crisis Data - CSV/DE-6-7-8-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
DE_Round6to9<-DE_Round6to9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
DE_Round6to9 <- DE_Round6to9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(DE_Round6to9$inwyys)

##subset for each year:
DE_2012<-DE_Round6to9 %>%
        filter(inwyys==2012)
DE_2013<-DE_Round6to9 %>%
        filter(inwyys==2013)
DE_2014<-DE_Round6to9 %>%
        filter(inwyys==2014)
DE_2015<-DE_Round6to9 %>%
        filter(inwyys==2015)
DE_2016<-DE_Round6to9 %>%
        filter(inwyys==2016)
DE_2017<-DE_Round6to9 %>%
        filter(inwyys==2017)
DE_2018<-DE_Round6to9 %>%
        filter(inwyys==2018)
DE_2019<-DE_Round6to9 %>%
        filter(inwyys==2019)

##Give the unemployment rate of the previous year to each year
DE_2012<-DE_2012 %>%
        mutate(UnemploymentRate=rep(0.058,times=2419))
DE_2013<-DE_2013 %>%
        mutate(UnemploymentRate=rep(0.054,times=178))
DE_2014<-DE_2014 %>%
        mutate(UnemploymentRate=rep(0.052,times=2171))
DE_2015<-DE_2015 %>%
        mutate(UnemploymentRate=rep(0.050,times=515))
DE_2016<-DE_2016 %>%
        mutate(UnemploymentRate=rep(0.046,times=2198))
DE_2017<-DE_2017 %>%
        mutate(UnemploymentRate=rep(0.041,times=279))
DE_2018<-DE_2018 %>%
        mutate(UnemploymentRate=rep(0.038,times=1567))
DE_2019<-DE_2019 %>%
        mutate(UnemploymentRate=rep(0.034,times=415))

##Give the GDP growth of the previous year to each year
DE_2012<-DE_2012 %>%
        mutate(GDPGrowth=rep(0.059,times=2419))
DE_2013<-DE_2013 %>%
        mutate(GDPGrowth=rep(0.002,times=178))
DE_2014<-DE_2014 %>%
        mutate(GDPGrowth=rep(0.002,times=2171))
DE_2015<-DE_2015 %>%
        mutate(GDPGrowth=rep(0.018,times=515))
DE_2016<-DE_2016 %>%
        mutate(GDPGrowth=rep(0.006,times=2198))
DE_2017<-DE_2017 %>%
        mutate(GDPGrowth=rep(0.014,times=279))
DE_2018<-DE_2018 %>%
        mutate(GDPGrowth=rep(0.023,times=1567))
DE_2019<-DE_2019 %>%
        mutate(GDPGrowth=rep(0.007,times=415))

##across all time points in this country
DE_PoliticalCrisis<-rbind(DE_2012, DE_2013, DE_2014, DE_2015,
                           DE_2016, DE_2017, DE_2018, DE_2019)
DE_PoliticalCrisis<-DE_PoliticalCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
DE_PoliticalCrisis<-DE_PoliticalCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("DE_2012", "DE_2013", "DE_2014", "DE_2015",
            "DE_2016", "DE_2017", "DE_2018", "DE_2019",
            "DE_Round6to9"))


######--------------------------------------------------------------------------------------------------------
##Data management for Hungary
##Read HU Round 6-9 in:
HU_Round6to9<-read.csv("./Political Crisis Data - CSV/HU-6-7-8-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
HU_Round6to9<-HU_Round6to9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
HU_Round6to9 <- HU_Round6to9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(HU_Round6to9$inwyys)

##subset for each year:
HU_2012<-HU_Round6to9 %>%
        filter(inwyys==2012)
HU_2013<-HU_Round6to9 %>%
        filter(inwyys==2013)
HU_2015<-HU_Round6to9 %>%
        filter(inwyys==2015)
HU_2017<-HU_Round6to9 %>%
        filter(inwyys==2017)
HU_2019<-HU_Round6to9 %>%
        filter(inwyys==2019)

##Give the unemployment rate of the previous year to each year
HU_2012<-HU_2012 %>%
        mutate(UnemploymentRate=rep(0.110,times=1749))
HU_2013<-HU_2013 %>%
        mutate(UnemploymentRate=rep(0.110,times=115))
HU_2015<-HU_2015 %>%
        mutate(UnemploymentRate=rep(0.077,times=1583))
HU_2017<-HU_2017 %>%
        mutate(UnemploymentRate=rep(0.051,times=1518))
HU_2019<-HU_2019 %>%
        mutate(UnemploymentRate=rep(0.037,times=1574))

##Give the GDP growth of the previous year to each year
HU_2012<-HU_2012 %>%
        mutate(GDPGrowth=rep(0.022,times=1749))
HU_2013<-HU_2013 %>%
        mutate(GDPGrowth=rep(-0.007,times=115))
HU_2015<-HU_2015 %>%
        mutate(GDPGrowth=rep(0.045,times=1583))
HU_2017<-HU_2017 %>%
        mutate(GDPGrowth=rep(0.025,times=1518))
HU_2019<-HU_2019 %>%
        mutate(GDPGrowth=rep(0.055,times=1574))

##across all time points in this country
HU_PoliticalCrisis<-rbind(HU_2012, HU_2013, HU_2015, HU_2017, HU_2019)
HU_PoliticalCrisis<-HU_PoliticalCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
HU_PoliticalCrisis<-HU_PoliticalCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("HU_2012", "HU_2013", "HU_2015", "HU_2017", "HU_2019",
            "HU_Round6to9"))


######--------------------------------------------------------------------------------------------------------
##Data management for Ireland
##Read IE Round 6-9 in:
IE_Round6to9<-read.csv("./Political Crisis Data - CSV/IE-6-7-8-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
IE_Round6to9<-IE_Round6to9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
IE_Round6to9 <- IE_Round6to9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(IE_Round6to9$inwyys)

##subset for each year:
IE_2012<-IE_Round6to9 %>%
        filter(inwyys==2012)
IE_2013<-IE_Round6to9 %>%
        filter(inwyys==2013)
IE_2014<-IE_Round6to9 %>%
        filter(inwyys==2014)
IE_2015<-IE_Round6to9 %>%
        filter(inwyys==2015)
IE_2016<-IE_Round6to9 %>%
        filter(inwyys==2016)
IE_2017<-IE_Round6to9 %>%
        filter(inwyys==2017)
IE_2018<-IE_Round6to9 %>%
        filter(inwyys==2018)
IE_2019<-IE_Round6to9 %>%
        filter(inwyys==2019)

##Give the unemployment rate of the previous year to each year
IE_2012<-IE_2012 %>%
        mutate(UnemploymentRate=rep(0.153,times=820))
IE_2013<-IE_2013 %>%
        mutate(UnemploymentRate=rep(0.154,times=1396))
IE_2014<-IE_2014 %>%
        mutate(UnemploymentRate=rep(0.137,times=1566))
IE_2015<-IE_2015 %>%
        mutate(UnemploymentRate=rep(0.119,times=478))
IE_2016<-IE_2016 %>%
        mutate(UnemploymentRate=rep(0.099,times=138))
IE_2017<-IE_2017 %>%
        mutate(UnemploymentRate=rep(0.084,times=2128))
IE_2018<-IE_2018 %>%
        mutate(UnemploymentRate=rep(0.067,times=314))
IE_2019<-IE_2019 %>%
        mutate(UnemploymentRate=rep(0.057,times=1476))

##Give the GDP growth of the previous year to each year
IE_2012<-IE_2012 %>%
        mutate(GDPGrowth=rep(0.008,times=820))
IE_2013<-IE_2013 %>%
        mutate(GDPGrowth=rep(-0.006,times=1396))
IE_2014<-IE_2014 %>%
        mutate(GDPGrowth=rep(0.006,times=1566))
IE_2015<-IE_2015 %>%
        mutate(GDPGrowth=rep(0.080,times=478))
IE_2016<-IE_2016 %>%
        mutate(GDPGrowth=rep(0.233,times=138))
IE_2017<-IE_2017 %>%
        mutate(GDPGrowth=rep(0.006,times=2128))
IE_2018<-IE_2018 %>%
        mutate(GDPGrowth=rep(0.081,times=314))
IE_2019<-IE_2019 %>%
        mutate(GDPGrowth=rep(0.071,times=1476))

##across all time points in this country
IE_PoliticalCrisis<-rbind(IE_2012, IE_2013, IE_2014, IE_2015,
                          IE_2016, IE_2017, IE_2018, IE_2019)
IE_PoliticalCrisis<-IE_PoliticalCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
IE_PoliticalCrisis<-IE_PoliticalCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("IE_2012", "IE_2013", "IE_2014", "IE_2015",
            "IE_2016", "IE_2017", "IE_2018", "IE_2019",
            "IE_Round6to9"))

######--------------------------------------------------------------------------------------------------------
##Data management for Lithuania
##Read LT Round 6-9 in:
LT_Round6to9<-read.csv("./Political Crisis Data - CSV/LT-6-7-8-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
LT_Round6to9<-LT_Round6to9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
LT_Round6to9 <- LT_Round6to9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(LT_Round6to9$inwyys)

##subset for each year:
LT_2013<-LT_Round6to9 %>%
        filter(inwyys==2013)
LT_2015<-LT_Round6to9 %>%
        filter(inwyys==2015)
LT_2017<-LT_Round6to9 %>%
        filter(inwyys==2017)
LT_2019<-LT_Round6to9 %>%
        filter(inwyys==2019)

##Give the unemployment rate of the previous year to each year
LT_2013<-LT_2013 %>%
        mutate(UnemploymentRate=rep(0.134,times=1887))
LT_2015<-LT_2015 %>%
        mutate(UnemploymentRate=rep(0.107,times=1988))
LT_2017<-LT_2017 %>%
        mutate(UnemploymentRate=rep(0.079,times=1884))
LT_2019<-LT_2019 %>%
        mutate(UnemploymentRate=rep(0.062,times=1691))

##Give the GDP growth of the previous year to each year
LT_2013<-LT_2013 %>%
        mutate(GDPGrowth=rep(0.052,times=1887))
LT_2015<-LT_2015 %>%
        mutate(GDPGrowth=rep(0.044,times=1988))
LT_2017<-LT_2017 %>%
        mutate(GDPGrowth=rep(0.038,times=1884))
LT_2019<-LT_2019 %>%
        mutate(GDPGrowth=rep(0.050,times=1691))

##across all time points in this country
LT_PoliticalCrisis<-rbind(LT_2013, LT_2015, LT_2017, LT_2019)
LT_PoliticalCrisis<-LT_PoliticalCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
LT_PoliticalCrisis<-LT_PoliticalCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("LT_2013", "LT_2015", "LT_2017", "LT_2019",
            "LT_Round6to9"))


######--------------------------------------------------------------------------------------------------------
##Data management for Netherlands
##Read NL Round 6-9 in:
NL_Round6to9<-read.csv("./Political Crisis Data - CSV/NL-6-7-8-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
NL_Round6to9<-NL_Round6to9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
NL_Round6to9 <- NL_Round6to9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(NL_Round6to9$inwyys)

##subset for each year:
NL_2012<-NL_Round6to9 %>%
        filter(inwyys==2012)
NL_2013<-NL_Round6to9 %>%
        filter(inwyys==2013)
NL_2014<-NL_Round6to9 %>%
        filter(inwyys==2014)
NL_2015<-NL_Round6to9 %>%
        filter(inwyys==2015)
NL_2016<-NL_Round6to9 %>%
        filter(inwyys==2016)
NL_2017<-NL_Round6to9 %>%
        filter(inwyys==2017)
NL_2018<-NL_Round6to9 %>%
        filter(inwyys==2018)
NL_2019<-NL_Round6to9 %>%
        filter(inwyys==2019)

##Give the unemployment rate of the previous year to each year
NL_2012<-NL_2012 %>%
        mutate(UnemploymentRate=rep(0.050,times=1317))
NL_2013<-NL_2013 %>%
        mutate(UnemploymentRate=rep(0.058,times=322))
NL_2014<-NL_2014 %>%
        mutate(UnemploymentRate=rep(0.072,times=1608))
NL_2015<-NL_2015 %>%
        mutate(UnemploymentRate=rep(0.074,times=74))
NL_2016<-NL_2016 %>%
        mutate(UnemploymentRate=rep(0.069,times=1326))
NL_2017<-NL_2017 %>%
        mutate(UnemploymentRate=rep(0.060,times=186))
NL_2018<-NL_2018 %>%
        mutate(UnemploymentRate=rep(0.048,times=1353))
NL_2019<-NL_2019 %>%
        mutate(UnemploymentRate=rep(0.038,times=90))

##Give the GDP growth of the previous year to each year
NL_2012<-NL_2012 %>%
        mutate(GDPGrowth=rep(0.011,times=1317))
NL_2013<-NL_2013 %>%
        mutate(GDPGrowth=rep(-0.014,times=322))
NL_2014<-NL_2014 %>%
        mutate(GDPGrowth=rep(-0.004,times=1608))
NL_2015<-NL_2015 %>%
        mutate(GDPGrowth=rep(0.011,times=74))
NL_2016<-NL_2016 %>%
        mutate(GDPGrowth=rep(0.015,times=1326))
NL_2017<-NL_2017 %>%
        mutate(GDPGrowth=rep(0.016,times=186))
NL_2018<-NL_2018 %>%
        mutate(GDPGrowth=rep(0.023,times=1353))
NL_2019<-NL_2019 %>%
        mutate(GDPGrowth=rep(0.018,times=90))

##across all time points in this country
NL_PoliticalCrisis<-rbind(NL_2012, NL_2013, NL_2014, NL_2015,
                          NL_2016, NL_2017, NL_2018, NL_2019)
NL_PoliticalCrisis<-NL_PoliticalCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
NL_PoliticalCrisis<-NL_PoliticalCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("NL_2012", "NL_2013", "NL_2014", "NL_2015",
            "NL_2016", "NL_2017", "NL_2018", "NL_2019",
            "NL_Round6to9"))


######--------------------------------------------------------------------------------------------------------
##Data management for Norway
##Read NO Round 6-9 in:
NO_Round6to9<-read.csv("./Political Crisis Data - CSV/NO-6-7-8-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
NO_Round6to9<-NO_Round6to9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
NO_Round6to9 <- NO_Round6to9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(NO_Round6to9$inwyys)

##subset for each year:
NO_2012<-NO_Round6to9 %>%
        filter(inwyys==2012)
NO_2013<-NO_Round6to9 %>%
        filter(inwyys==2013)
NO_2014<-NO_Round6to9 %>%
        filter(inwyys==2014)
NO_2016<-NO_Round6to9 %>%
        filter(inwyys==2016)
NO_2018<-NO_Round6to9 %>%
        filter(inwyys==2018)
NO_2019<-NO_Round6to9 %>%
        filter(inwyys==2019)

##Give the unemployment rate of the previous year to each year
NO_2012<-NO_2012 %>%
        mutate(UnemploymentRate=rep(0.032,times=1231))
NO_2013<-NO_2013 %>%
        mutate(UnemploymentRate=rep(0.031,times=152))
NO_2014<-NO_2014 %>%
        mutate(UnemploymentRate=rep(0.034,times=1245))
NO_2016<-NO_2016 %>%
        mutate(UnemploymentRate=rep(0.043,times=1333))
NO_2018<-NO_2018 %>%
        mutate(UnemploymentRate=rep(0.042,times=546))
NO_2019<-NO_2019 %>%
        mutate(UnemploymentRate=rep(0.038,times=663))

##Give the GDP growth of the previous year to each year
NO_2012<-NO_2012 %>%
        mutate(GDPGrowth=rep(-0.002,times=1231))
NO_2013<-NO_2013 %>%
        mutate(GDPGrowth=rep(0.014,times=152))
NO_2014<-NO_2014 %>%
        mutate(GDPGrowth=rep(-0.002,times=1245))
NO_2016<-NO_2016 %>%
        mutate(GDPGrowth=rep(0.008,times=1333))
NO_2018<-NO_2018 %>%
        mutate(GDPGrowth=rep(0.016,times=546))
NO_2019<-NO_2019 %>%
        mutate(GDPGrowth=rep(0.002,times=663))

##across all time points in this country
NO_PoliticalCrisis<-rbind(NO_2012, NO_2013, NO_2014, NO_2016, NO_2018, NO_2019)
NO_PoliticalCrisis<-NO_PoliticalCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
NO_PoliticalCrisis<-NO_PoliticalCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("NO_2012", "NO_2013", "NO_2014", "NO_2016", "NO_2018", "NO_2019",
            "NO_Round6to9"))


######--------------------------------------------------------------------------------------------------------
##Data management for Poland
##Read PL Round 6-9 in:
PL_Round6to9<-read.csv("./Political Crisis Data - CSV/PL-6-7-8-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
PL_Round6to9<-PL_Round6to9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
PL_Round6to9 <- PL_Round6to9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(PL_Round6to9$inwyys)

##subset for each year:
PL_2012<-PL_Round6to9 %>%
        filter(inwyys==2012)
PL_2013<-PL_Round6to9 %>%
        filter(inwyys==2013)
PL_2015<-PL_Round6to9 %>%
        filter(inwyys==2015)
PL_2016<-PL_Round6to9 %>%
        filter(inwyys==2016)
PL_2017<-PL_Round6to9 %>%
        filter(inwyys==2017)
PL_2018<-PL_Round6to9 %>%
        filter(inwyys==2018)
PL_2019<-PL_Round6to9 %>%
        filter(inwyys==2019)

##Give the unemployment rate of the previous year to each year
PL_2012<-PL_2012 %>%
        mutate(UnemploymentRate=rep(0.096,times=1816))
PL_2013<-PL_2013 %>%
        mutate(UnemploymentRate=rep(0.101,times=13))
PL_2015<-PL_2015 %>%
        mutate(UnemploymentRate=rep(0.090,times=1561))
PL_2016<-PL_2016 %>%
        mutate(UnemploymentRate=rep(0.075,times=1336))
PL_2017<-PL_2017 %>%
        mutate(UnemploymentRate=rep(0.062,times=294))
PL_2018<-PL_2018 %>%
        mutate(UnemploymentRate=rep(0.049,times=949))
PL_2019<-PL_2019 %>%
        mutate(UnemploymentRate=rep(0.039,times=501))

##Give the GDP growth of the previous year to each year
PL_2012<-PL_2012 %>%
        mutate(GDPGrowth=rep(0.050,times=1816))
PL_2013<-PL_2013 %>%
        mutate(GDPGrowth=rep(0.015,times=13))
PL_2015<-PL_2015 %>%
        mutate(GDPGrowth=rep(0.039,times=1561))
PL_2016<-PL_2016 %>%
        mutate(GDPGrowth=rep(0.045,times=1336))
PL_2017<-PL_2017 %>%
        mutate(GDPGrowth=rep(0.030,times=294))
PL_2018<-PL_2018 %>%
        mutate(GDPGrowth=rep(0.051,times=949))
PL_2019<-PL_2019 %>%
        mutate(GDPGrowth=rep(0.059,times=501))

##across all time points in this country
PL_PoliticalCrisis<-rbind(PL_2012, PL_2013, PL_2015, PL_2016,
                          PL_2017, PL_2018, PL_2019)
PL_PoliticalCrisis<-PL_PoliticalCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
PL_PoliticalCrisis<-PL_PoliticalCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("PL_2012", "PL_2013", "PL_2015", "PL_2016",
            "PL_2017", "PL_2018", "PL_2019",
            "PL_Round6to9"))

######--------------------------------------------------------------------------------------------------------
##Data management for Portugal
##Read PT Round 6-9 in:
PT_Round6to9<-read.csv("./Political Crisis Data - CSV/PT-6-7-8-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
PT_Round6to9<-PT_Round6to9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
PT_Round6to9 <- PT_Round6to9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(PT_Round6to9$inwyys)

##subset for each year:
PT_2012<-PT_Round6to9 %>%
        filter(inwyys==2012)
PT_2013<-PT_Round6to9 %>%
        filter(inwyys==2013)
PT_2015<-PT_Round6to9 %>%
        filter(inwyys==2015)
PT_2016<-PT_Round6to9 %>%
        filter(inwyys==2016)
PT_2017<-PT_Round6to9 %>%
        filter(inwyys==2017)
PT_2018<-PT_Round6to9 %>%
        filter(inwyys==2018)
PT_2019<-PT_Round6to9 %>%
        filter(inwyys==2019)

##Give the unemployment rate of the previous year to each year
PT_2012<-PT_2012 %>%
        mutate(UnemploymentRate=rep(0.127,times=373))
PT_2013<-PT_2013 %>%
        mutate(UnemploymentRate=rep(0.155,times=1610))
PT_2015<-PT_2015 %>%
        mutate(UnemploymentRate=rep(0.139,times=1128))
PT_2016<-PT_2016 %>%
        mutate(UnemploymentRate=rep(0.124,times=210))
PT_2017<-PT_2017 %>%
        mutate(UnemploymentRate=rep(0.111,times=906))
PT_2018<-PT_2018 %>%
        mutate(UnemploymentRate=rep(0.089,times=87))
PT_2019<-PT_2019 %>%
        mutate(UnemploymentRate=rep(0.070,times=803))

##Give the GDP growth of the previous year to each year
PT_2012<-PT_2012 %>%
        mutate(GDPGrowth=rep(-0.016,times=373))
PT_2013<-PT_2013 %>%
        mutate(GDPGrowth=rep(-0.037,times=1610))
PT_2015<-PT_2015 %>%
        mutate(GDPGrowth=rep(0.013,times=1128))
PT_2016<-PT_2016 %>%
        mutate(GDPGrowth=rep(0.022,times=210))
PT_2017<-PT_2017 %>%
        mutate(GDPGrowth=rep(0.023,times=906))
PT_2018<-PT_2018 %>%
        mutate(GDPGrowth=rep(0.038,times=87))
PT_2019<-PT_2019 %>%
        mutate(GDPGrowth=rep(0.030,times=803))

##across all time points in this country
PT_PoliticalCrisis<-rbind(PT_2012, PT_2013, PT_2015, PT_2016,
                          PT_2017, PT_2018, PT_2019)
PT_PoliticalCrisis<-PT_PoliticalCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
PT_PoliticalCrisis<-PT_PoliticalCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))


rm(list = c("PT_2012", "PT_2013", "PT_2015", "PT_2016",
            "PT_2017", "PT_2018", "PT_2019",
            "PT_Round6to9"))



######--------------------------------------------------------------------------------------------------------
##Data management for Slovenia
##Read SI Round 6-9 in:
SI_Round6to9<-read.csv("./Political Crisis Data - CSV/SI-6-7-8-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
SI_Round6to9<-SI_Round6to9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
SI_Round6to9 <- SI_Round6to9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(SI_Round6to9$inwyys)

##subset for each year:
SI_2012<-SI_Round6to9 %>%
        filter(inwyys==2012)
SI_2014<-SI_Round6to9 %>%
        filter(inwyys==2014)
SI_2015<-SI_Round6to9 %>%
        filter(inwyys==2015)
SI_2016<-SI_Round6to9 %>%
        filter(inwyys==2016)
SI_2017<-SI_Round6to9 %>%
        filter(inwyys==2017)
SI_2018<-SI_Round6to9 %>%
        filter(inwyys==2018)
SI_2019<-SI_Round6to9 %>%
        filter(inwyys==2019)

##Give the unemployment rate of the previous year to each year
SI_2012<-SI_2012 %>%
        mutate(UnemploymentRate=rep(0.082,times=1124))
SI_2014<-SI_2014 %>%
        mutate(UnemploymentRate=rep(0.101,times=1062))
SI_2015<-SI_2015 %>%
        mutate(UnemploymentRate=rep(0.097,times=34))
SI_2016<-SI_2016 %>%
        mutate(UnemploymentRate=rep(0.090,times=1133))
SI_2017<-SI_2017 %>%
        mutate(UnemploymentRate=rep(0.080,times=7))
SI_2018<-SI_2018 %>%
        mutate(UnemploymentRate=rep(0.066,times=1103))
SI_2019<-SI_2019 %>%
        mutate(UnemploymentRate=rep(0.051,times=39))

##Give the GDP growth of the previous year to each year
SI_2012<-SI_2012 %>%
        mutate(GDPGrowth=rep(0.007,times=1124))
SI_2014<-SI_2014 %>%
        mutate(GDPGrowth=rep(-0.012,times=1062))
SI_2015<-SI_2015 %>%
        mutate(GDPGrowth=rep(0.027,times=34))
SI_2016<-SI_2016 %>%
        mutate(GDPGrowth=rep(0.021,times=1133))
SI_2017<-SI_2017 %>%
        mutate(GDPGrowth=rep(0.031,times=7))
SI_2018<-SI_2018 %>%
        mutate(GDPGrowth=rep(0.047,times=1103))
SI_2019<-SI_2019 %>%
        mutate(GDPGrowth=rep(0.041,times=39))

##across all time points in this country
SI_PoliticalCrisis<-rbind(SI_2012, SI_2014, SI_2015, SI_2016,
                          SI_2017, SI_2018, SI_2019)
SI_PoliticalCrisis<-SI_PoliticalCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
SI_PoliticalCrisis<-SI_PoliticalCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("SI_2012", "SI_2014", "SI_2015", "SI_2016",
            "SI_2017", "SI_2018", "SI_2019",
            "SI_Round6to9"))


######--------------------------------------------------------------------------------------------------------
##Data management for Spain
##Read ES Round 6-9 in:
ES_Round6to9<-read.csv("./Political Crisis Data - CSV/ES-6-7-8-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
ES_Round6to9<-ES_Round6to9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
ES_Round6to9 <- ES_Round6to9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(ES_Round6to9$inwyys)

##subset for each year:
ES_2013<-ES_Round6to9 %>%
        filter(inwyys==2013)
ES_2015<-ES_Round6to9 %>%
        filter(inwyys==2015)
ES_2017<-ES_Round6to9 %>%
        filter(inwyys==2017)
ES_2019<-ES_Round6to9 %>%
        filter(inwyys==2019)

##Give the unemployment rate of the previous year to each year
ES_2013<-ES_2013 %>%
        mutate(UnemploymentRate=rep(0.248,times=1613))
ES_2015<-ES_2015 %>%
        mutate(UnemploymentRate=rep(0.244,times=1685))
ES_2017<-ES_2017 %>%
        mutate(UnemploymentRate=rep(0.196,times=1643))
ES_2019<-ES_2019 %>%
        mutate(UnemploymentRate=rep(0.153,times=1176))

##Give the GDP growth of the previous year to each year
ES_2013<-ES_2013 %>%
        mutate(GDPGrowth=rep(-0.030,times=1613))
ES_2015<-ES_2015 %>%
        mutate(GDPGrowth=rep(0.017,times=1685))
ES_2017<-ES_2017 %>%
        mutate(GDPGrowth=rep(0.030,times=1643))
ES_2019<-ES_2019 %>%
        mutate(GDPGrowth=rep(0.018,times=1176))

##across all time points in this country
ES_PoliticalCrisis<-rbind(ES_2013, ES_2015, ES_2017, ES_2019)
ES_PoliticalCrisis<-ES_PoliticalCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
ES_PoliticalCrisis<-ES_PoliticalCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("ES_2013", "ES_2015", "ES_2017", "ES_2019",
            "ES_Round6to9"))


######--------------------------------------------------------------------------------------------------------
##Data management for Sweden
##Read SE Round 6-9 in:
SE_Round6to9<-read.csv("./Political Crisis Data - CSV/SE-6-7-8-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
SE_Round6to9<-SE_Round6to9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
SE_Round6to9 <- SE_Round6to9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(SE_Round6to9$inwyys)

##subset for each year:
SE_2012<-SE_Round6to9 %>%
        filter(inwyys==2012)
SE_2013<-SE_Round6to9 %>%
        filter(inwyys==2013)
SE_2014<-SE_Round6to9 %>%
        filter(inwyys==2014)
SE_2015<-SE_Round6to9 %>%
        filter(inwyys==2015)
SE_2016<-SE_Round6to9 %>%
        filter(inwyys==2016)
SE_2017<-SE_Round6to9 %>%
        filter(inwyys==2017)
SE_2018<-SE_Round6to9 %>%
        filter(inwyys==2018)
SE_2019<-SE_Round6to9 %>%
        filter(inwyys==2019)

##Give the unemployment rate of the previous year to each year
SE_2012<-SE_2012 %>%
        mutate(UnemploymentRate=rep(0.078,times=916))
SE_2013<-SE_2013 %>%
        mutate(UnemploymentRate=rep(0.080,times=666))
SE_2014<-SE_2014 %>%
        mutate(UnemploymentRate=rep(0.081,times=1508))
SE_2015<-SE_2015 %>%
        mutate(UnemploymentRate=rep(0.080,times=11))
SE_2016<-SE_2016 %>%
        mutate(UnemploymentRate=rep(0.074,times=1232))
SE_2017<-SE_2017 %>%
        mutate(UnemploymentRate=rep(0.070,times=106))
SE_2018<-SE_2018 %>%
        mutate(UnemploymentRate=rep(0.067,times=870))
SE_2019<-SE_2019 %>%
        mutate(UnemploymentRate=rep(0.064,times=405))

##Give the GDP growth of the previous year to each year
SE_2012<-SE_2012 %>%
        mutate(GDPGrowth=rep(0.024,times=916))
SE_2013<-SE_2013 %>%
        mutate(GDPGrowth=rep(-0.013,times=666))
SE_2014<-SE_2014 %>%
        mutate(GDPGrowth=rep(0.003,times=1508))
SE_2015<-SE_2015 %>%
        mutate(GDPGrowth=rep(0.016,times=11))
SE_2016<-SE_2016 %>%
        mutate(GDPGrowth=rep(0.034,times=1232))
SE_2017<-SE_2017 %>%
        mutate(GDPGrowth=rep(0.008,times=106))
SE_2018<-SE_2018 %>%
        mutate(GDPGrowth=rep(0.012,times=870))
SE_2019<-SE_2019 %>%
        mutate(GDPGrowth=rep(0.008,times=405))

##across all time points in this country
SE_PoliticalCrisis<-rbind(SE_2012, SE_2013, SE_2014, SE_2015,
                          SE_2016, SE_2017, SE_2018, SE_2019)
SE_PoliticalCrisis<-SE_PoliticalCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
SE_PoliticalCrisis<-SE_PoliticalCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))


rm(list = c("SE_2012", "SE_2013", "SE_2014", "SE_2015",
            "SE_2016", "SE_2017", "SE_2018", "SE_2019",
            "SE_Round6to9"))


######--------------------------------------------------------------------------------------------------------
##Data management for Switzerland
##Read CH Round 6-9 in:
CH_Round6to9<-read.csv("./Political Crisis Data - CSV/CH-6-7-8-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
CH_Round6to9<-CH_Round6to9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
CH_Round6to9 <- CH_Round6to9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(CH_Round6to9$inwyys)

##subset for each year:
CH_2012<-CH_Round6to9 %>%
        filter(inwyys==2012)
CH_2013<-CH_Round6to9 %>%
        filter(inwyys==2013)
CH_2014<-CH_Round6to9 %>%
        filter(inwyys==2014)
CH_2015<-CH_Round6to9 %>%
        filter(inwyys==2015)
CH_2016<-CH_Round6to9 %>%
        filter(inwyys==2016)
CH_2017<-CH_Round6to9 %>%
        filter(inwyys==2017)
CH_2018<-CH_Round6to9 %>%
        filter(inwyys==2018)
CH_2019<-CH_Round6to9 %>%
        filter(inwyys==2019)

##Give the unemployment rate of the previous year to each year
CH_2012<-CH_2012 %>%
        mutate(UnemploymentRate=rep(0.044,times=933))
CH_2013<-CH_2013 %>%
        mutate(UnemploymentRate=rep(0.045,times=144))
CH_2014<-CH_2014 %>%
        mutate(UnemploymentRate=rep(0.048,times=1016))
CH_2015<-CH_2015 %>%
        mutate(UnemploymentRate=rep(0.048,times=53))
CH_2016<-CH_2016 %>%
        mutate(UnemploymentRate=rep(0.048,times=1005))
CH_2017<-CH_2017 %>%
        mutate(UnemploymentRate=rep(0.049,times=53))
CH_2018<-CH_2018 %>%
        mutate(UnemploymentRate=rep(0.048,times=1000))
CH_2019<-CH_2019 %>%
        mutate(UnemploymentRate=rep(0.047,times=38))

##Give the GDP growth of the previous year to each year
CH_2012<-CH_2012 %>%
        mutate(GDPGrowth=rep(0.007,times=933))
CH_2013<-CH_2013 %>%
        mutate(GDPGrowth=rep(0.001,times=144))
CH_2014<-CH_2014 %>%
        mutate(GDPGrowth=rep(0.006,times=1016))
CH_2015<-CH_2015 %>%
        mutate(GDPGrowth=rep(0.011,times=53))
CH_2016<-CH_2016 %>%
        mutate(GDPGrowth=rep(0.005,times=1005))
CH_2017<-CH_2017 %>%
        mutate(GDPGrowth=rep(0.010,times=53))
CH_2018<-CH_2018 %>%
        mutate(GDPGrowth=rep(0.004,times=1000))
CH_2019<-CH_2019 %>%
        mutate(GDPGrowth=rep(0.021,times=38))

##across all time points in this country
CH_PoliticalCrisis<-rbind(CH_2012, CH_2013, CH_2014, CH_2015,
                          CH_2016, CH_2017, CH_2018, CH_2019)
CH_PoliticalCrisis<-CH_PoliticalCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
CH_PoliticalCrisis<-CH_PoliticalCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("CH_2012", "CH_2013", "CH_2014", "CH_2015",
            "CH_2016", "CH_2017", "CH_2018", "CH_2019",
            "CH_Round6to9"))


######--------------------------------------------------------------------------------------------------------
##Data management for Great Britain
##Read GB Round 6-9 in:
GB_Round6to9<-read.csv("./Political Crisis Data - CSV/GB-6-7-8-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
GB_Round6to9<-GB_Round6to9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
GB_Round6to9 <- GB_Round6to9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(GB_Round6to9$inwyys)

##subset for each year:
GB_2012<-GB_Round6to9 %>%
        filter(inwyys==2012)
GB_2013<-GB_Round6to9 %>%
        filter(inwyys==2013)
GB_2014<-GB_Round6to9 %>%
        filter(inwyys==2014)
GB_2015<-GB_Round6to9 %>%
        filter(inwyys==2015)
GB_2016<-GB_Round6to9 %>%
        filter(inwyys==2016)
GB_2017<-GB_Round6to9 %>%
        filter(inwyys==2017)
GB_2018<-GB_Round6to9 %>%
        filter(inwyys==2018)
GB_2019<-GB_Round6to9 %>%
        filter(inwyys==2019)

##Give the unemployment rate of the previous year to each year
GB_2012<-GB_2012 %>%
        mutate(UnemploymentRate=rep(0.080,times=1728))
GB_2013<-GB_2013 %>%
        mutate(UnemploymentRate=rep(0.079,times=211))
GB_2014<-GB_2014 %>%
        mutate(UnemploymentRate=rep(0.075,times=1378))
GB_2015<-GB_2015 %>%
        mutate(UnemploymentRate=rep(0.061,times=490))
GB_2016<-GB_2016 %>%
        mutate(UnemploymentRate=rep(0.053,times=1477))
GB_2017<-GB_2017 %>%
        mutate(UnemploymentRate=rep(0.048,times=141))
GB_2018<-GB_2018 %>%
        mutate(UnemploymentRate=rep(0.043,times=1607))
GB_2019<-GB_2019 %>%
        mutate(UnemploymentRate=rep(0.040,times=227))

##Give the GDP growth of the previous year to each year
GB_2012<-GB_2012 %>%
        mutate(GDPGrowth=rep(0.004,times=1728))
GB_2013<-GB_2013 %>%
        mutate(GDPGrowth=rep(0.008,times=211))
GB_2014<-GB_2014 %>%
        mutate(GDPGrowth=rep(0.011,times=1378))
GB_2015<-GB_2015 %>%
        mutate(GDPGrowth=rep(0.024,times=490))
GB_2016<-GB_2016 %>%
        mutate(GDPGrowth=rep(0.014,times=1477))
GB_2017<-GB_2017 %>%
        mutate(GDPGrowth=rep(0.012,times=141))
GB_2018<-GB_2018 %>%
        mutate(GDPGrowth=rep(0.020,times=1607))
GB_2019<-GB_2019 %>%
        mutate(GDPGrowth=rep(0.008,times=227))

##across all time points in this country
GB_PoliticalCrisis<-rbind(GB_2012, GB_2013, GB_2014, GB_2015,
                          GB_2016, GB_2017, GB_2018, GB_2019)
GB_PoliticalCrisis<-GB_PoliticalCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
GB_PoliticalCrisis<-GB_PoliticalCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("GB_2012", "GB_2013", "GB_2014", "GB_2015",
            "GB_2016", "GB_2017", "GB_2018", "GB_2019",
            "GB_Round6to9"))