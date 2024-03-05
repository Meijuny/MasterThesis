######--------------------------------------------------------------------------------------------------------
##Data management for Belgium
##Read BE Round 3-6 in:
BE_Round3to6<-read.csv("./Economic Crisis Data - CSV/BE-3-4-5-6.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
BE_Round3to6 <- BE_Round3to6 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
BE_Round3to6 <- BE_Round3to6 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(BE_Round3to6$inwyys)

##subset BE for each year:
BE_2006<-BE_Round3to6 %>%
        filter(inwyys==2006)
BE_2007<-BE_Round3to6 %>%
        filter(inwyys==2007)
BE_2008<-BE_Round3to6 %>%
        filter(inwyys==2008)
BE_2009<-BE_Round3to6 %>%
        filter(inwyys==2009)
BE_2010<-BE_Round3to6 %>%
        filter(inwyys==2010)
BE_2011<-BE_Round3to6 %>%
        filter(inwyys==2011)
BE_2012<-BE_Round3to6 %>%
        filter(inwyys==2012)

##Give the unemployment rate of the previous year to each year
BE_2006<-BE_2006 %>%
        mutate(UnemploymentRate=rep(0.084,times=1023))
BE_2007<-BE_2007 %>%
        mutate(UnemploymentRate=rep(0.083,times=583))
BE_2008<-BE_2008 %>%
        mutate(UnemploymentRate=rep(0.075,times=890))
BE_2009<-BE_2009 %>%
        mutate(UnemploymentRate=rep(0.070,times=642))
BE_2010<-BE_2010 %>%
        mutate(UnemploymentRate=rep(0.079,times=710))
BE_2011<-BE_2011 %>%
        mutate(UnemploymentRate=rep(0.083,times=762))
BE_2012<-BE_2012 %>%
        mutate(UnemploymentRate=rep(0.071,times=1560))

##Give the GDP growth per capita of the previous year to each year
BE_2006<-BE_2006 %>%
        mutate(GDPGrowth=rep(0.018,times=1023))
BE_2007<-BE_2007 %>%
        mutate(GDPGrowth=rep(0.019,times=583))
BE_2008<-BE_2008 %>%
        mutate(GDPGrowth=rep(0.029,times=890))
BE_2009<-BE_2009 %>%
        mutate(GDPGrowth=rep(-0.003,times=642))
BE_2010<-BE_2010 %>%
        mutate(GDPGrowth=rep(-0.028,times=710))
BE_2011<-BE_2011 %>%
        mutate(GDPGrowth=rep(0.019,times=762))
BE_2012<-BE_2012 %>%
        mutate(GDPGrowth=rep(0.004,times=1560))

##Bind the data together and give the mean mean of unemployment rate across all time points
BE_EconomicCrisis<-rbind(BE_2006, BE_2007, BE_2008, BE_2009, BE_2010, BE_2011, BE_2012)
BE_EconomicCrisis<-BE_EconomicCrisis %>%
        mutate(MeanUnEmploymentRate=mean(UnemploymentRate))
BE_EconomicCrisis<-BE_EconomicCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))



######--------------------------------------------------------------------------------------------------------
##Data management for Bulgaria
##Read BG Round 3-6 in:
BG_Round3to6<-read.csv("./Economic Crisis Data - CSV/BG-3-4-5-6.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
BG_Round3to6 <- BG_Round3to6 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
BG_Round3to6 <- BG_Round3to6 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(BG_Round3to6$inwyys)

##subset BG for each year:
BG_2006<-BG_Round3to6 %>%
        filter(inwyys==2006)
BG_2009<-BG_Round3to6 %>%
        filter(inwyys==2009)
BG_2010<-BG_Round3to6 %>%
        filter(inwyys==2010)
BG_2011<-BG_Round3to6 %>%
        filter(inwyys==2011)
BG_2013<-BG_Round3to6 %>%
        filter(inwyys==2013)

##Give the unemployment rate of the previous year to each year
BG_2006<-BG_2006 %>%
        mutate(UnemploymentRate=rep(0.101,times=1139))
BG_2009<-BG_2009 %>%
        mutate(UnemploymentRate=rep(0.056,times=1806))
BG_2010<-BG_2010 %>%
        mutate(UnemploymentRate=rep(0.068,times=76))
BG_2011<-BG_2011 %>%
        mutate(UnemploymentRate=rep(0.103,times=1895))
BG_2013<-BG_2013 %>%
        mutate(UnemploymentRate=rep(0.123,times=1839))

##Give the GDP growth per capita of the previous year to each year
BG_2006<-BG_2006 %>%
        mutate(GDPGrowth=rep(0.079,times=1139))
BG_2009<-BG_2009 %>%
        mutate(GDPGrowth=rep(0.069,times=1806))
BG_2010<-BG_2010 %>%
        mutate(GDPGrowth=rep(-0.027,times=76))
BG_2011<-BG_2011 %>%
        mutate(GDPGrowth=rep(0.022,times=1895))
BG_2013<-BG_2013 %>%
        mutate(GDPGrowth=rep(0.013,times=1839))

##Bind the data together and give the mean mean of unemployment rate across all time points
BG_EconomicCrisis<-rbind(BG_2006, BG_2009, BG_2010, BG_2011, BG_2013)
BG_EconomicCrisis<-BG_EconomicCrisis %>%
        mutate(MeanUnEmploymentRate=mean(UnemploymentRate))
BG_EconomicCrisis<-BG_EconomicCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

######--------------------------------------------------------------------------------------------------------
##Data management for Cyprus
##Read CY Round 3-6 in:
CY_Round3to6<-read.csv("./Economic Crisis Data - CSV/CY-3-4-5-6.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
CY_Round3to6 <- CY_Round3to6 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
CY_Round3to6 <- CY_Round3to6 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(CY_Round3to6$inwyys)

##subset CY for each year:
CY_2006<-CY_Round3to6 %>%
        filter(inwyys==2006)
CY_2008<-CY_Round3to6 %>%
        filter(inwyys==2008)
CY_2011<-CY_Round3to6 %>%
        filter(inwyys==2011)
CY_2012<-CY_Round3to6 %>%
        filter(inwyys==2012)
CY_2013<-CY_Round3to6 %>%
        filter(inwyys==2013)

##Give the unemployment rate of the previous year to each year
CY_2006<-CY_2006 %>%
        mutate(UnemploymentRate=rep(0.053,times=924))
CY_2008<-CY_2008 %>%
        mutate(UnemploymentRate=rep(0.039,times=1082))
CY_2011<-CY_2011 %>%
        mutate(UnemploymentRate=rep(0.063,times=971))
CY_2012<-CY_2012 %>%
        mutate(UnemploymentRate=rep(0.079,times=948))
CY_2013<-CY_2013 %>%
        mutate(UnemploymentRate=rep(0.118,times=26))

##Give the GDP Growth per capita of the previous year to each year
CY_2006<-CY_2006 %>%
        mutate(GDPGrowth=rep(0.034,times=924))
CY_2008<-CY_2008 %>%
        mutate(GDPGrowth=rep(0.029,times=1082))
CY_2011<-CY_2011 %>%
        mutate(GDPGrowth=rep(-0.004,times=971))
CY_2012<-CY_2012 %>%
        mutate(GDPGrowth=rep(-0.021,times=948))
CY_2013<-CY_2013 %>%
        mutate(GDPGrowth=rep(-0.049,times=26))

##Bind the data together and give the mean mean of unemployment rate across all time points
CY_EconomicCrisis<-rbind(CY_2006, CY_2008, CY_2011, CY_2012, CY_2013)
CY_EconomicCrisis<-CY_EconomicCrisis %>%
        mutate(MeanUnEmploymentRate=mean(UnemploymentRate))
CY_EconomicCrisis<-CY_EconomicCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))


#####----------------------------------------------------------------------------------------------------------
##Data managemment for Denmark
##Read DK Round 3-6 in:
DK_Round3to6<-read.csv("./Economic Crisis Data - CSV/DK-3-4-5-6.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
DK_Round3to6 <- DK_Round3to6 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
DK_Round3to6 <- DK_Round3to6 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(DK_Round3to6$inwyys)

##subset DK for each year:
DK_2006<-DK_Round3to6 %>%
        filter(inwyys==2006)
DK_2007<-DK_Round3to6 %>%
        filter(inwyys==2007)
DK_2008<-DK_Round3to6 %>%
        filter(inwyys==2008)
DK_2009<-DK_Round3to6 %>%
        filter(inwyys==2009)
DK_2010<-DK_Round3to6 %>%
        filter(inwyys==2010)
DK_2011<-DK_Round3to6 %>%
        filter(inwyys==2011)
DK_2013<-DK_Round3to6 %>%
        filter(inwyys==2013)

##Give the unemployment rate of the previous year to each year
DK_2006<-DK_2006 %>%
        mutate(UnemploymentRate=rep(0.048,times=1089))
DK_2007<-DK_2007 %>%
        mutate(UnemploymentRate=rep(0.039,times=304))
DK_2008<-DK_2008 %>%
        mutate(UnemploymentRate=rep(0.038,times=1459))
DK_2009<-DK_2009 %>%
        mutate(UnemploymentRate=rep(0.037,times=32))
DK_2010<-DK_2010 %>%
        mutate(UnemploymentRate=rep(0.064,times=983))
DK_2011<-DK_2011 %>%
        mutate(UnemploymentRate=rep(0.078,times=466))
DK_2013<-DK_2013 %>%
        mutate(UnemploymentRate=rep(0.078,times=1516))

##Give the GDP growth per capita of the previous year to each year
DK_2006<-DK_2006 %>%
        mutate(GDPGrowth=rep(0.021,times=1089))
DK_2007<-DK_2007 %>%
        mutate(GDPGrowth=rep(0.036,times=304))
DK_2008<-DK_2008 %>%
        mutate(GDPGrowth=rep(0.005,times=1459))
DK_2009<-DK_2009 %>%
        mutate(GDPGrowth=rep(-0.011,times=32))
DK_2010<-DK_2010 %>%
        mutate(GDPGrowth=rep(-0.054,times=983))
DK_2011<-DK_2011 %>%
        mutate(GDPGrowth=rep(0.014,times=466))
DK_2013<-DK_2013 %>%
        mutate(GDPGrowth=rep(-0.001,times=1516))

##Bind the data together and give the mean mean of unemployment rate across all time points
DK_EconomicCrisis<-rbind(DK_2006, DK_2007, DK_2008, DK_2009, DK_2010, DK_2011, DK_2013)
DK_EconomicCrisis<-DK_EconomicCrisis %>%
        mutate(MeanUnEmploymentRate=mean(UnemploymentRate))
DK_EconomicCrisis<-DK_EconomicCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

#####----------------------------------------------------------------------------------------------------------
##Data managemment for Estonia
##Read EE Round 3-6 in:
EE_Round3to6<-read.csv("./Economic Crisis Data - CSV/EE-3-4-5-6.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
EE_Round3to6 <- EE_Round3to6 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
EE_Round3to6 <- EE_Round3to6 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(EE_Round3to6$inwyys)

##subset DK for each year:
EE_2006<-EE_Round3to6 %>%
        filter(inwyys==2006)
EE_2007<-EE_Round3to6 %>%
        filter(inwyys==2007)
EE_2008<-EE_Round3to6 %>%
        filter(inwyys==2008)
EE_2009<-EE_Round3to6 %>%
        filter(inwyys==2009)
EE_2012<-EE_Round3to6 %>%
        filter(inwyys==2012)
EE_2013<-EE_Round3to6 %>%
        filter(inwyys==2013)

##Give the unemployment rate of the previous year to each year
EE_2006<-EE_2006 %>%
        mutate(UnemploymentRate=rep(0.08,times=342))
EE_2007<-EE_2007 %>%
        mutate(UnemploymentRate=rep(0.059,times=573))
EE_2008<-EE_2008 %>%
        mutate(UnemploymentRate=rep(0.046,times=276))
EE_2009<-EE_2009 %>%
        mutate(UnemploymentRate=rep(0.055,times=749))
EE_2012<-EE_2012 %>%
        mutate(UnemploymentRate=rep(0.123,times=1613))
EE_2013<-EE_2013 %>%
        mutate(UnemploymentRate=rep(0.1,times=68))

##Give the GDP growth per capita of the previous year to each year
EE_2006<-EE_2006 %>%
        mutate(GDPGrowth=rep(0.102,times=342))
EE_2007<-EE_2007 %>%
        mutate(GDPGrowth=rep(0.104,times=573))
EE_2008<-EE_2008 %>%
        mutate(GDPGrowth=rep(0.081,times=276))
EE_2009<-EE_2009 %>%
        mutate(GDPGrowth=rep(-0.049,times=749))
EE_2012<-EE_2012 %>%
        mutate(GDPGrowth=rep(0.076,times=1613))
EE_2013<-EE_2013 %>%
        mutate(GDPGrowth=rep(0.036,times=68))

###Special part for Estonia Round 5 as the interview year is registered under supqyr instead of inwyys
EE_Round5<-read.csv("./Economic Crisis Data - CSV/EE-5.csv")

####subset the data into EE for 2010 and EE for 2011
EE_2010<-EE_Round5 %>% 
        filter(supqyr==2010)
EE_2011<-EE_Round5 %>%
        filter(supqyr==2011)

####subset the data to include only the respondents born in the country
EE_2010<-EE_2010 %>% 
        filter(brncntr==1)
EE_2011<-EE_2011 %>%
        filter(brncntr==1)

####subset the data to include only the respondents who are the citizens of the country
EE_2010<-EE_2010 %>% 
        filter(ctzcntr==1)
EE_2011<-EE_2011 %>% 
        filter(ctzcntr==1)

####subset the data to include only the respondents who do not belong to the ethnic minority group
####and also delete whoever did not indicate whether they are from a minority group
EE_2010<-EE_2010 %>%
        filter(blgetmg==2)
EE_2011<-EE_2011 %>%
        filter(blgetmg==2)

####Select the variables relevant to the analysis
EE_2010<-EE_2010 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,supqyr)
EE_2011<-EE_2011 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,supqyr)

####Give the unemployment values of the previous year to each year
EE_2010<-EE_2010 %>%
        mutate(UnemploymentRate=rep(0.136,times=931))
EE_2011<-EE_2011 %>%
        mutate(UnemploymentRate=rep(0.167,times=442))

####Give the GDP growth per capita of the previous year to each year
EE_2010<-EE_2010 %>%
        mutate(GDPGrowth=rep(-0.145,times=931))
EE_2011<-EE_2011 %>%
        mutate(GDPGrowth=rep(0.027,times=442))

##Change the column name to be cohesive as the rest of the countries 
colnames(EE_2010)[15]<-"inwyys"
colnames(EE_2011)[15]<-"inwyys"


##Bind the data together and give the mean mean of unemployment rate across all time points
EE_EconomicCrisis<-rbind(EE_2006, EE_2007, EE_2008, EE_2009, EE_2010, EE_2011, EE_2012, EE_2013)
EE_EconomicCrisis<-EE_EconomicCrisis %>%
        mutate(MeanUnEmploymentRate=mean(UnemploymentRate))
EE_EconomicCrisis<-EE_EconomicCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))


#####----------------------------------------------------------------------------------------------------------
##Data managemment for Finland
##Read FI Round 3-6 in:
FI_Round3to6<-read.csv("./Economic Crisis Data - CSV/FI-3-4-5-6.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
FI_Round3to6 <- FI_Round3to6 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
FI_Round3to6 <- FI_Round3to6 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(FI_Round3to6$inwyys)

##subset FI for each year:
FI_2006<-FI_Round3to6 %>%
        filter(inwyys==2006)
FI_2008<-FI_Round3to6 %>%
        filter(inwyys==2008)
FI_2009<-FI_Round3to6 %>%
        filter(inwyys==2009)
FI_2010<-FI_Round3to6 %>%
        filter(inwyys==2010)
FI_2012<-FI_Round3to6 %>%
        filter(inwyys==2012)
FI_2013<-FI_Round3to6 %>%
        filter(inwyys==2013)

##Give the unemployment rate of the previous year to each year
FI_2006<-FI_2006 %>%
        mutate(UnemploymentRate=rep(0.084,times=1823))
FI_2008<-FI_2008 %>%
        mutate(UnemploymentRate=rep(0.068,times=1860))
FI_2009<-FI_2009 %>%
        mutate(UnemploymentRate=rep(0.064,times=258))
FI_2010<-FI_2010 %>%
        mutate(UnemploymentRate=rep(0.083,times=1797))
FI_2012<-FI_2012 %>%
        mutate(UnemploymentRate=rep(0.078,times=1788))
FI_2013<-FI_2013 %>%
        mutate(UnemploymentRate=rep(0.077,times=290))

##Give the GDP growth per capita of the previous year to each year
FI_2006<-FI_2006 %>%
        mutate(GDPGrowth=rep(0.024,times=1823))
FI_2008<-FI_2008 %>%
        mutate(GDPGrowth=rep(0.049,times=1860))
FI_2009<-FI_2009 %>%
        mutate(GDPGrowth=rep(0.003,times=258))
FI_2010<-FI_2010 %>%
        mutate(GDPGrowth=rep(-0.085,times=1797))
FI_2012<-FI_2012 %>%
        mutate(GDPGrowth=rep(0.021,times=1788))
FI_2013<-FI_2013 %>%
        mutate(GDPGrowth=rep(-0.019,times=290))

##Bind the data together and give the mean mean of unemployment rate across all time points
FI_EconomicCrisis<-rbind(FI_2006, FI_2008, FI_2009, FI_2010, FI_2012, FI_2013)
FI_EconomicCrisis<-FI_EconomicCrisis %>%
        mutate(MeanUnEmploymentRate=mean(UnemploymentRate))
FI_EconomicCrisis<-FI_EconomicCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))


#####----------------------------------------------------------------------------------------------------------
##Data managemment for France
##Read FR Round 3-6 in:
FR_Round3to6<-read.csv("./Economic Crisis Data - CSV/FR-3-4-5-6.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
FR_Round3to6 <- FR_Round3to6 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
FR_Round3to6 <- FR_Round3to6 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(FR_Round3to6$inwyys)

##subset DK for each year:
FR_2006<-FR_Round3to6 %>%
        filter(inwyys==2006)
FR_2007<-FR_Round3to6 %>%
        filter(inwyys==2007)
FR_2008<-FR_Round3to6 %>%
        filter(inwyys==2008)
FR_2009<-FR_Round3to6 %>%
        filter(inwyys==2009)
FR_2010<-FR_Round3to6 %>%
        filter(inwyys==2010)
FR_2011<-FR_Round3to6 %>%
        filter(inwyys==2011)
FR_2013<-FR_Round3to6 %>%
        filter(inwyys==2013)

##Give the unemployment rate of the previous year to each year
FR_2006<-FR_2006 %>%
        mutate(UnemploymentRate=rep(0.085,times=1193))
FR_2007<-FR_2007 %>%
        mutate(UnemploymentRate=rep(0.084,times=564))
FR_2008<-FR_2008 %>%
        mutate(UnemploymentRate=rep(0.077,times=1770))
FR_2009<-FR_2009 %>%
        mutate(UnemploymentRate=rep(0.071,times=71))
FR_2010<-FR_2010 %>%
        mutate(UnemploymentRate=rep(0.087,times=1281))
FR_2011<-FR_2011 %>%
        mutate(UnemploymentRate=rep(0.089,times=238))
FR_2013<-FR_2013 %>%
        mutate(UnemploymentRate=rep(0.094,times=1699))

##Give the GDP growth per capita of the previous year to each year
FR_2006<-FR_2006 %>%
        mutate(GDPGrowth=rep(0.009,times=1193))
FR_2007<-FR_2007 %>%
        mutate(GDPGrowth=rep(0.017,times=564))
FR_2008<-FR_2008 %>%
        mutate(GDPGrowth=rep(0.018,times=1770))
FR_2009<-FR_2009 %>%
        mutate(GDPGrowth=rep(-0.003,times=71))
FR_2010<-FR_2010 %>%
        mutate(GDPGrowth=rep(-0.034,times=1281))
FR_2011<-FR_2011 %>%
        mutate(GDPGrowth=rep(0.014,times=238))
FR_2013<-FR_2013 %>%
        mutate(GDPGrowth=rep(-0.002,times=1699))

##Bind the data together and give the mean mean of unemployment rate across all time points
FR_EconomicCrisis<-rbind(FR_2006, FR_2007, FR_2008, FR_2009, FR_2010, FR_2011, FR_2013)
FR_EconomicCrisis<-FR_EconomicCrisis %>%
        mutate(MeanUnEmploymentRate=mean(UnemploymentRate))
FR_EconomicCrisis<-FR_EconomicCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))


#####----------------------------------------------------------------------------------------------------------
##Data managemment for Germany
##Read DE Round 3-6 in:
DE_Round3to6<-read.csv("./Economic Crisis Data - CSV/DE-3-4-5-6.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
DE_Round3to6 <- DE_Round3to6 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
DE_Round3to6 <- DE_Round3to6 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(DE_Round3to6$inwyys)

##subset DE for each year:
DE_2006<-DE_Round3to6 %>%
        filter(inwyys==2006)
DE_2007<-DE_Round3to6 %>%
        filter(inwyys==2007)
DE_2008<-DE_Round3to6 %>%
        filter(inwyys==2008)
DE_2009<-DE_Round3to6 %>%
        filter(inwyys==2009)
DE_2010<-DE_Round3to6 %>%
        filter(inwyys==2010)
DE_2011<-DE_Round3to6 %>%
        filter(inwyys==2011)
DE_2012<-DE_Round3to6 %>%
        filter(inwyys==2012)
DE_2013<-DE_Round3to6 %>%
        filter(inwyys==2013)

##Give the unemployment rate of the previous year to each year
DE_2006<-DE_2006 %>%
        mutate(UnemploymentRate=rep(0.112,times=2362))
DE_2007<-DE_2007 %>%
        mutate(UnemploymentRate=rep(0.103,times=252))
DE_2008<-DE_2008 %>%
        mutate(UnemploymentRate=rep(0.087,times=1919))
DE_2009<-DE_2009 %>%
        mutate(UnemploymentRate=rep(0.075,times=536))
DE_2010<-DE_2010 %>%
        mutate(UnemploymentRate=rep(0.077,times=1780))
DE_2011<-DE_2011 %>%
        mutate(UnemploymentRate=rep(0.07,times=899))
DE_2012<-DE_2012 %>%
        mutate(UnemploymentRate=rep(0.058,times=2419))
DE_2013<-DE_2013 %>%
        mutate(UnemploymentRate=rep(0.054,times=178))

##Give the GDP per capita of the previous year to each year
DE_2006<-DE_2006 %>%
        mutate(GDPGrowth=rep(0.008,times=2362))
DE_2007<-DE_2007 %>%
        mutate(GDPGrowth=rep(0.039,times=252))
DE_2008<-DE_2008 %>%
        mutate(GDPGrowth=rep(0.031,times=1919))
DE_2009<-DE_2009 %>%
        mutate(GDPGrowth=rep(0.012,times=536))
DE_2010<-DE_2010 %>%
        mutate(GDPGrowth=rep(-0.055,times=1780))
DE_2011<-DE_2011 %>%
        mutate(GDPGrowth=rep(0.043,times=899))
DE_2012<-DE_2012 %>%
        mutate(GDPGrowth=rep(0.059,times=2419))
DE_2013<-DE_2013 %>%
        mutate(GDPGrowth=rep(0.002,times=178))

##Bind the data together and give the mean mean of unemployment rate across all time points
DE_EconomicCrisis<-rbind(DE_2006, DE_2007, DE_2008, DE_2009, DE_2010, DE_2011, DE_2012, DE_2013)
DE_EconomicCrisis<-DE_EconomicCrisis %>%
        mutate(MeanUnEmploymentRate=mean(UnemploymentRate))
DE_EconomicCrisis<-DE_EconomicCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))


#####----------------------------------------------------------------------------------------------------------
##Data managemment for Hungary
##Read HU Round 3-6 in:
HU_Round3to6<-read.csv("./Economic Crisis Data - CSV/HU-3-4-5-6.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
HU_Round3to6 <- HU_Round3to6 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
HU_Round3to6 <- HU_Round3to6 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(HU_Round3to6$inwyys)

##subset HU for each year:
HU_2006<-HU_Round3to6 %>%
        filter(inwyys==2006)
HU_2007<-HU_Round3to6 %>%
        filter(inwyys==2007)
HU_2009<-HU_Round3to6 %>%
        filter(inwyys==2009)
HU_2010<-HU_Round3to6 %>%
        filter(inwyys==2010)
HU_2012<-HU_Round3to6 %>%
        filter(inwyys==2012)
HU_2013<-HU_Round3to6 %>%
        filter(inwyys==2013)

##Give the unemployment rate of the previous year to each year
HU_2006<-HU_2006 %>%
        mutate(UnemploymentRate=rep(0.072,times=1264))
HU_2007<-HU_2007 %>%
        mutate(UnemploymentRate=rep(0.075,times=139))
HU_2009<-HU_2009 %>%
        mutate(UnemploymentRate=rep(0.078,times=1429))
HU_2010<-HU_2010 %>%
        mutate(UnemploymentRate=rep(0.1,times=1443))
HU_2012<-HU_2012 %>%
        mutate(UnemploymentRate=rep(0.11,times=1749))
HU_2013<-HU_2013 %>%
        mutate(UnemploymentRate=rep(0.11,times=115))

##Give the GDP per capita of the previous year to each year
HU_2006<-HU_2006 %>%
        mutate(GDPGrowth=rep(0.045,times=1264))
HU_2007<-HU_2007 %>%
        mutate(GDPGrowth=rep(0.041,times=139))
HU_2009<-HU_2009 %>%
        mutate(GDPGrowth=rep(0.012,times=1429))
HU_2010<-HU_2010 %>%
        mutate(GDPGrowth=rep(-0.065,times=1443))
HU_2012<-HU_2012 %>%
        mutate(GDPGrowth=rep(0.022,times=1749))
HU_2013<-HU_2013 %>%
        mutate(GDPGrowth=rep(-0.007,times=115))

##Bind the data together and give the mean mean of unemployment rate across all time points
HU_EconomicCrisis<-rbind(HU_2006, HU_2007, HU_2009, HU_2010, HU_2012, HU_2013)
HU_EconomicCrisis<-HU_EconomicCrisis %>%
        mutate(MeanUnEmploymentRate=mean(UnemploymentRate))
HU_EconomicCrisis<-HU_EconomicCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))


#####----------------------------------------------------------------------------------------------------------
##Data managemment for Ireland
##Read IE Round 3-6 in:
IE_Round3to6<-read.csv("./Economic Crisis Data - CSV/IE-3-4-5-6.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
IE_Round3to6 <- IE_Round3to6 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
IE_Round3to6 <- IE_Round3to6 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(IE_Round3to6$inwyys)

##subset IE for each year:
IE_2006<-IE_Round3to6 %>%
        filter(inwyys==2006)
IE_2007<-IE_Round3to6 %>%
        filter(inwyys==2007)
IE_2009<-IE_Round3to6 %>%
        filter(inwyys==2009)
IE_2010<-IE_Round3to6 %>%
        filter(inwyys==2010)
IE_2011<-IE_Round3to6 %>%
        filter(inwyys==2011)
IE_2012<-IE_Round3to6 %>%
        filter(inwyys==2012)
IE_2013<-IE_Round3to6 %>%
        filter(inwyys==2013)

##Give the unemployment rate of the previous year to each year
IE_2006<-IE_2006 %>%
        mutate(UnemploymentRate=rep(0.043,times=542))
IE_2007<-IE_2007 %>%
        mutate(UnemploymentRate=rep(0.044,times=945))
IE_2009<-IE_2009 %>%
        mutate(UnemploymentRate=rep(0.068,times=1124))
IE_2010<-IE_2010 %>%
        mutate(UnemploymentRate=rep(0.126,times=336))
IE_2011<-IE_2011 %>%
        mutate(UnemploymentRate=rep(0.145,times=2030))
IE_2012<-IE_2012 %>%
        mutate(UnemploymentRate=rep(0.153,times=934))
IE_2013<-IE_2013 %>%
        mutate(UnemploymentRate=rep(0.154,times=1396))

##Give the GDP growth per capita of the previous year to each year
IE_2006<-IE_2006 %>%
        mutate(GDPGrowth=rep(0.035,times=542))
IE_2007<-IE_2007 %>%
        mutate(GDPGrowth=rep(0.022,times=945))
IE_2009<-IE_2009 %>%
        mutate(GDPGrowth=rep(-0.064,times=1124))
IE_2010<-IE_2010 %>%
        mutate(GDPGrowth=rep(-0.061,times=336))
IE_2011<-IE_2011 %>%
        mutate(GDPGrowth=rep(0.011,times=2030))
IE_2012<-IE_2012 %>%
        mutate(GDPGrowth=rep(0.008,times=934))
IE_2013<-IE_2013 %>%
        mutate(GDPGrowth=rep(-0.006,times=1396))

##Bind the data together and give the mean mean of unemployment rate across all time points
IE_EconomicCrisis<-rbind(IE_2006, IE_2007, IE_2009, IE_2010, IE_2011, IE_2012, IE_2013)
IE_EconomicCrisis<-IE_EconomicCrisis %>%
        mutate(MeanUnEmploymentRate=mean(UnemploymentRate))
IE_EconomicCrisis<-IE_EconomicCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))


#####----------------------------------------------------------------------------------------------------------
##Data managemment for Netherlands
##Read NL Round 3-6 in:
NL_Round3to6<-read.csv("./Economic Crisis Data - CSV/NL-3-4-5-6.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
NL_Round3to6 <- NL_Round3to6 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
NL_Round3to6 <- NL_Round3to6 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(NL_Round3to6$inwyys)

##subset NL for each year:
NL_2006<-NL_Round3to6 %>%
        filter(inwyys==2006)
NL_2007<-NL_Round3to6 %>%
        filter(inwyys==2007)
NL_2008<-NL_Round3to6 %>%
        filter(inwyys==2008)
NL_2009<-NL_Round3to6 %>%
        filter(inwyys==2009)
NL_2010<-NL_Round3to6 %>%
        filter(inwyys==2010)
NL_2011<-NL_Round3to6 %>%
        filter(inwyys==2011)
NL_2012<-NL_Round3to6 %>%
        filter(inwyys==2012)
NL_2013<-NL_Round3to6 %>%
        filter(inwyys==2013)

##Give the unemployment rate of the previous year to each year
NL_2006<-NL_2006 %>%
        mutate(UnemploymentRate=rep(0.059,times=1270))
NL_2007<-NL_2007 %>%
        mutate(UnemploymentRate=rep(0.050,times=417))
NL_2008<-NL_2008 %>%
        mutate(UnemploymentRate=rep(0.042,times=1011))
NL_2009<-NL_2009 %>%
        mutate(UnemploymentRate=rep(0.036,times=561))
NL_2010<-NL_2010 %>%
        mutate(UnemploymentRate=rep(0.043,times=1067))
NL_2011<-NL_2011 %>%
        mutate(UnemploymentRate=rep(0.050,times=589))
NL_2012<-NL_2012 %>%
        mutate(UnemploymentRate=rep(0.050,times=1317))
NL_2013<-NL_2013 %>%
        mutate(UnemploymentRate=rep(0.058,times=322))


##Give the GDP growth per capita of the previous year to each year
NL_2006<-NL_2006 %>%
        mutate(GDPGrowth=rep(0.018,times=1270))
NL_2007<-NL_2007 %>%
        mutate(GDPGrowth=rep(0.033,times=417))
NL_2008<-NL_2008 %>%
        mutate(GDPGrowth=rep(0.035,times=1011))
NL_2009<-NL_2009 %>%
        mutate(GDPGrowth=rep(0.018,times=561))
NL_2010<-NL_2010 %>%
        mutate(GDPGrowth=rep(-0.042,times=1067))
NL_2011<-NL_2011 %>%
        mutate(GDPGrowth=rep(0.008,times=589))
NL_2012<-NL_2012 %>%
        mutate(GDPGrowth=rep(0.011,times=1317))
NL_2013<-NL_2013 %>%
        mutate(GDPGrowth=rep(-0.014,times=322))

##Bind the data together and give the mean mean of unemployment rate across all time points
NL_EconomicCrisis<-rbind(NL_2006, NL_2007, NL_2008, NL_2009, NL_2010, NL_2011, NL_2012, NL_2013)
NL_EconomicCrisis<-NL_EconomicCrisis %>% 
        mutate(MeanUnEmploymentRate=mean(UnemploymentRate))
NL_EconomicCrisis<-NL_EconomicCrisis %>% 
        mutate(MeanGDPGrowth=mean(GDPGrowth))



#####----------------------------------------------------------------------------------------------------------
##Data managemment for Norway
##Read NO Round 3-6 in:
NO_Round3to6<-read.csv("./Economic Crisis Data - CSV/NO-3-4-5-6.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
NO_Round3to6 <- NO_Round3to6 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
NO_Round3to6 <- NO_Round3to6 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(NO_Round3to6$inwyys)

##Year of 2007 is not available
##subset NO for each year:
NO_2006<-NO_Round3to6 %>%
        filter(inwyys==2006)
NO_2008<-NO_Round3to6 %>%
        filter(inwyys==2008)
NO_2009<-NO_Round3to6 %>%
        filter(inwyys==2009)
NO_2010<-NO_Round3to6 %>%
        filter(inwyys==2010)
NO_2011<-NO_Round3to6 %>%
        filter(inwyys==2011)
NO_2012<-NO_Round3to6 %>%
        filter(inwyys==2012)
NO_2013<-NO_Round3to6 %>%
        filter(inwyys==2013)

##Give the unemployment rate of the previous year to each year
NO_2006<-NO_2006 %>%
        mutate(UnemploymentRate=rep(0.044,times=1591))
NO_2008<-NO_2008 %>%
        mutate(UnemploymentRate=rep(0.025,times=1371))
NO_2009<-NO_2009 %>%
        mutate(UnemploymentRate=rep(0.025,times=21))
NO_2010<-NO_2010 %>%
        mutate(UnemploymentRate=rep(0.031,times=1199))
NO_2011<-NO_2011 %>%
        mutate(UnemploymentRate=rep(0.035,times=151))
NO_2012<-NO_2012 %>%
        mutate(UnemploymentRate=rep(0.032,times=1231))
NO_2013<-NO_2013 %>%
        mutate(UnemploymentRate=rep(0.031,times=152))

##Give the GDP growth per capita of the previous year to each year
NO_2006<-NO_2006 %>%
        mutate(GDPGrowth=rep(0.020,times=1591))
NO_2008<-NO_2008 %>%
        mutate(GDPGrowth=rep(0.019,times=1371))
NO_2009<-NO_2009 %>%
        mutate(GDPGrowth=rep(-0.008,times=21))
NO_2010<-NO_2010 %>%
        mutate(GDPGrowth=rep(-0.032,times=1199))
NO_2011<-NO_2011 %>%
        mutate(GDPGrowth=rep(-0.005,times=151))
NO_2012<-NO_2012 %>%
        mutate(GDPGrowth=rep(-0.002,times=1231))
NO_2013<-NO_2013 %>%
        mutate(GDPGrowth=rep(0.014,times=152))



##Bind the data together and give the mean mean of unemployment rate across all time points
NO_EconomicCrisis<-rbind(NO_2006, NO_2008, NO_2009, NO_2010, NO_2011, NO_2012, NO_2013)
NO_EconomicCrisis<-NO_EconomicCrisis %>% 
        mutate(MeanUnEmploymentRate=mean(UnemploymentRate))
NO_EconomicCrisis<-NO_EconomicCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))



#####----------------------------------------------------------------------------------------------------------
##Data managemment for Poland
##Read PL Round 3-6 in:
PL_Round3to6<-read.csv("./Economic Crisis Data - CSV/PL-3-4-5-6.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
PL_Round3to6 <- PL_Round3to6 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
PL_Round3to6 <- PL_Round3to6 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(PL_Round3to6$inwyys)

##Year of 2007 has only 2 observations and Year of 2013 has only 13 observations
##Take out 2007 but keep 2013 for the moment
##subset PL for each year:
PL_2006<-PL_Round3to6 %>%
        filter(inwyys==2006)
PL_2008<-PL_Round3to6 %>%
        filter(inwyys==2008)
PL_2009<-PL_Round3to6 %>%
        filter(inwyys==2009)
PL_2010<-PL_Round3to6 %>%
        filter(inwyys==2010)
PL_2011<-PL_Round3to6 %>%
        filter(inwyys==2011)
PL_2012<-PL_Round3to6 %>%
        filter(inwyys==2012)
PL_2013<-PL_Round3to6 %>%
        filter(inwyys==2013)

##Give the unemployment rate of the previous year to each year
PL_2006<-PL_2006 %>%
        mutate(UnemploymentRate=rep(0.178,times=1645))
PL_2008<-PL_2008 %>%
        mutate(UnemploymentRate=rep(0.096,times=1396))
PL_2009<-PL_2009 %>%
        mutate(UnemploymentRate=rep(0.071,times=169))
PL_2010<-PL_2010 %>%
        mutate(UnemploymentRate=rep(0.082,times=1648))
PL_2011<-PL_2011 %>%
        mutate(UnemploymentRate=rep(0.096,times=32))
PL_2012<-PL_2012 %>%
        mutate(UnemploymentRate=rep(0.096,times=1816))
PL_2013<-PL_2013 %>%
        mutate(UnemploymentRate=rep(0.101,times=13))

##Give the GDP growth per capita of the previous year to each year
PL_2006<-PL_2006 %>%
        mutate(GDPGrowth=rep(0.036,times=1645))
PL_2008<-PL_2008 %>%
        mutate(GDPGrowth=rep(0.071,times=1396))
PL_2009<-PL_2009 %>%
        mutate(GDPGrowth=rep(0.042,times=169))
PL_2010<-PL_2010 %>%
        mutate(GDPGrowth=rep(0.028,times=1648))
PL_2011<-PL_2011 %>%
        mutate(GDPGrowth=rep(0.032,times=32))
PL_2012<-PL_2012 %>%
        mutate(GDPGrowth=rep(0.050,times=1816))
PL_2013<-PL_2013 %>%
        mutate(GDPGrowth=rep(0.015,times=13))

##Bind the data together and give the mean mean of unemployment rate across all time points
PL_EconomicCrisis<-rbind(PL_2006, PL_2008, PL_2009, PL_2010, PL_2011, PL_2012, PL_2013)
PL_EconomicCrisis<-PL_EconomicCrisis %>% 
        mutate(MeanUnEmploymentRate=mean(UnemploymentRate))
PL_EconomicCrisis<-PL_EconomicCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))





#####----------------------------------------------------------------------------------------------------------
##Data managemment for Portugal
##Read PT Round 3-6 in:
PT_Round3to6<-read.csv("./Economic Crisis Data - CSV/PT-3-4-5-6.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
PT_Round3to6 <- PT_Round3to6 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
PT_Round3to6 <- PT_Round3to6 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(PT_Round3to6$inwyys)

##subset PT for each year:
PT_2006<-PT_Round3to6 %>%
        filter(inwyys==2006)
PT_2007<-PT_Round3to6 %>%
        filter(inwyys==2007)
PT_2008<-PT_Round3to6 %>%
        filter(inwyys==2008)
PT_2009<-PT_Round3to6 %>%
        filter(inwyys==2009)
PT_2010<-PT_Round3to6 %>%
        filter(inwyys==2010)
PT_2011<-PT_Round3to6 %>%
        filter(inwyys==2011)
PT_2012<-PT_Round3to6 %>%
        filter(inwyys==2012)
PT_2013<-PT_Round3to6 %>%
        filter(inwyys==2013)

##Give the unemployment rate of the previous year to each year
PT_2006<-PT_2006 %>%
        mutate(UnemploymentRate=rep(0.076,times=665))
PT_2007<-PT_2007 %>%
        mutate(UnemploymentRate=rep(0.077,times=1203))
PT_2008<-PT_2008 %>%
        mutate(UnemploymentRate=rep(0.080,times=922))
PT_2009<-PT_2009 %>%
        mutate(UnemploymentRate=rep(0.075,times=1232))
PT_2010<-PT_2010 %>%
        mutate(UnemploymentRate=rep(0.094,times=172))
PT_2011<-PT_2011 %>%
        mutate(UnemploymentRate=rep(0.108,times=1805))
PT_2012<-PT_2012 %>%
        mutate(UnemploymentRate=rep(0.127,times=373))
PT_2013<-PT_2013 %>%
        mutate(UnemploymentRate=rep(0.155,times=1610))

##Give the GDP growth per capita of the previous year to each year
PT_2006<-PT_2006 %>%
        mutate(GDPGrowth=rep(0.006,times=665))
PT_2007<-PT_2007 %>%
        mutate(GDPGrowth=rep(0.014,times=1203))
PT_2008<-PT_2008 %>%
        mutate(GDPGrowth=rep(0.023,times=922))
PT_2009<-PT_2009 %>%
        mutate(GDPGrowth=rep(0.002,times=1232))
PT_2010<-PT_2010 %>%
        mutate(GDPGrowth=rep(-0.032,times=172))
PT_2011<-PT_2011 %>%
        mutate(GDPGrowth=rep(0.017,times=1805))
PT_2012<-PT_2012 %>%
        mutate(GDPGrowth=rep(-0.016,times=373))
PT_2013<-PT_2013 %>%
        mutate(GDPGrowth=rep(-0.037,times=1610))

##Bind the data together and give the mean mean of unemployment rate across all time points
PT_EconomicCrisis<-rbind(PT_2006, PT_2007, PT_2008, PT_2009, PT_2010, PT_2011, PT_2012, PT_2013)
PT_EconomicCrisis<-PT_EconomicCrisis %>% 
        mutate(MeanUnEmploymentRate=mean(UnemploymentRate))
PT_EconomicCrisis<-PT_EconomicCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))




#####----------------------------------------------------------------------------------------------------------
##Data managemment for Russia
##Read RU Round 3-6 in:
RU_Round3to6<-read.csv("./Economic Crisis Data - CSV/RU-3-4-5-6.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
RU_Round3to6 <- RU_Round3to6 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
RU_Round3to6 <- RU_Round3to6 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(RU_Round3to6$inwyys)

##Year of 2013 is not available
##subset RU for each year:
RU_2006<-RU_Round3to6 %>%
        filter(inwyys==2006)
RU_2007<-RU_Round3to6 %>%
        filter(inwyys==2007)
RU_2008<-RU_Round3to6 %>%
        filter(inwyys==2008)
RU_2009<-RU_Round3to6 %>%
        filter(inwyys==2009)
RU_2010<-RU_Round3to6 %>%
        filter(inwyys==2010)
RU_2011<-RU_Round3to6 %>%
        filter(inwyys==2011)
RU_2012<-RU_Round3to6 %>%
        filter(inwyys==2012)

##Give the unemployment rate of the previous year to each year
RU_2006<-RU_2006 %>%
        mutate(UnemploymentRate=rep(0.071,times=1895))
RU_2007<-RU_2007 %>%
        mutate(UnemploymentRate=rep(0.071,times=23))
RU_2008<-RU_2008 %>%
        mutate(UnemploymentRate=rep(0.060,times=1645))
RU_2009<-RU_2009 %>%
        mutate(UnemploymentRate=rep(0.062,times=433))
RU_2010<-RU_2010 %>%
        mutate(UnemploymentRate=rep(0.083,times=49))
RU_2011<-RU_2011 %>%
        mutate(UnemploymentRate=rep(0.074,times=2076))
RU_2012<-RU_2012 %>%
        mutate(UnemploymentRate=rep(0.065,times=1964))

##Give the GDP growth per capita of the previous year to each year
RU_2006<-RU_2006 %>%
        mutate(GDPGrowth=rep(0.068,times=1895))
RU_2007<-RU_2007 %>%
        mutate(GDPGrowth=rep(0.086,times=23))
RU_2008<-RU_2008 %>%
        mutate(GDPGrowth=rep(0.087,times=1645))
RU_2009<-RU_2009 %>%
        mutate(GDPGrowth=rep(0.052,times=433))
RU_2010<-RU_2010 %>%
        mutate(GDPGrowth=rep(-0.078,times=49))
RU_2011<-RU_2011 %>%
        mutate(GDPGrowth=rep(0.045,times=2076))
RU_2012<-RU_2012 %>%
        mutate(GDPGrowth=rep(0.042,times=1964))

##Bind the data together and give the mean mean of unemployment rate across all time points
RU_EconomicCrisis<-rbind(RU_2006, RU_2007, RU_2008, RU_2009, RU_2010, RU_2011, RU_2012)
RU_EconomicCrisis<-RU_EconomicCrisis %>% 
        mutate(MeanUnEmploymentRate=mean(UnemploymentRate))
RU_EconomicCrisis<-RU_EconomicCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))





#####----------------------------------------------------------------------------------------------------------
##Data management for Slovakia
##Read SK Round 3-6 in:
SK_Round3to6<-read.csv("./Economic Crisis Data - CSV/SK-3-4-5-6.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
SK_Round3to6 <- SK_Round3to6 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
SK_Round3to6 <- SK_Round3to6 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(SK_Round3to6$inwyys)

##subset SK for each year:
SK_2006<-SK_Round3to6 %>%
        filter(inwyys==2006)
SK_2007<-SK_Round3to6 %>%
        filter(inwyys==2007)
SK_2008<-SK_Round3to6 %>%
        filter(inwyys==2008)
SK_2009<-SK_Round3to6 %>%
        filter(inwyys==2009)
SK_2010<-SK_Round3to6 %>%
        filter(inwyys==2010)
SK_2011<-SK_Round3to6 %>%
        filter(inwyys==2011)
SK_2012<-SK_Round3to6 %>%
        filter(inwyys==2012)
SK_2013<-SK_Round3to6 %>%
        filter(inwyys==2013)

##Give the unemployment rate of the previous year to each year
SK_2006<-SK_2006 %>%
        mutate(UnemploymentRate=rep(0.163,times=746))
SK_2007<-SK_2007 %>%
        mutate(UnemploymentRate=rep(0.134,times=752))
SK_2008<-SK_2008 %>%
        mutate(UnemploymentRate=rep(0.111,times=1009))
SK_2009<-SK_2009 %>%
        mutate(UnemploymentRate=rep(0.095,times=616))
SK_2010<-SK_2010 %>%
        mutate(UnemploymentRate=rep(0.120,times=1160))
SK_2011<-SK_2011 %>%
        mutate(UnemploymentRate=rep(0.144,times=470))
SK_2012<-SK_2012 %>%
        mutate(UnemploymentRate=rep(0.136,times=1284))
SK_2013<-SK_2013 %>%
        mutate(UnemploymentRate=rep(0.14,times=411))


##Give the GDP growth per capita of the previous year to each year
SK_2006<-SK_2006 %>%
        mutate(GDPGrowth=rep(0.066,times=746))
SK_2007<-SK_2007 %>%
        mutate(GDPGrowth=rep(0.085,times=752))
SK_2008<-SK_2008 %>%
        mutate(GDPGrowth=rep(0.108,times=1009))
SK_2009<-SK_2009 %>%
        mutate(GDPGrowth=rep(0.055,times=616))
SK_2010<-SK_2010 %>%
        mutate(GDPGrowth=rep(-0.056,times=1160))
SK_2011<-SK_2011 %>%
        mutate(GDPGrowth=rep(0.066,times=470))
SK_2012<-SK_2012 %>%
        mutate(GDPGrowth=rep(0.025,times=1284))
SK_2013<-SK_2013 %>%
        mutate(GDPGrowth=rep(0.011,times=411))

##Bind the data together and give the mean mean of unemployment rate across all time points
SK_EconomicCrisis<-rbind(SK_2006, SK_2007, SK_2008, SK_2009, SK_2010, SK_2011, SK_2012, SK_2013)
SK_EconomicCrisis<-SK_EconomicCrisis %>% 
        mutate(MeanUnEmploymentRate=mean(UnemploymentRate))
SK_EconomicCrisis<-SK_EconomicCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))




#####----------------------------------------------------------------------------------------------------------
##Data management for Slovenia
##Read SI Round 3-6 in:
SI_Round3to6<-read.csv("./Economic Crisis Data - CSV/SI-3-4-5-6.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
SI_Round3to6 <- SI_Round3to6 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
SI_Round3to6 <- SI_Round3to6 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(SI_Round3to6$inwyys)

##Year of 2007 and 2013 is not available while year of 2009 has only 3 observations
##subset SI for each year:
SI_2006<-SI_Round3to6 %>%
        filter(inwyys==2006)
SI_2008<-SI_Round3to6 %>%
        filter(inwyys==2008)
SI_2010<-SI_Round3to6 %>%
        filter(inwyys==2010)
SI_2011<-SI_Round3to6 %>%
        filter(inwyys==2011)
SI_2012<-SI_Round3to6 %>%
        filter(inwyys==2012)

##Give the unemployment rate of the previous year to each year
SI_2006<-SI_2006 %>%
        mutate(UnemploymentRate=rep(0.065,times=1289))
SI_2008<-SI_2008 %>%
        mutate(UnemploymentRate=rep(0.048,times=1133))
SI_2010<-SI_2010 %>%
        mutate(UnemploymentRate=rep(0.059,times=1016))
SI_2011<-SI_2011 %>%
        mutate(UnemploymentRate=rep(0.072,times=165))
SI_2012<-SI_2012 %>%
        mutate(UnemploymentRate=rep(0.082,times=1124))

##Give the GDP growth per capita of the previous year to each year
SI_2006<-SI_2006 %>%
        mutate(GDPGrowth=rep(0.036,times=1289))
SI_2008<-SI_2008 %>%
        mutate(GDPGrowth=rep(0.064,times=1133))
SI_2010<-SI_2010 %>%
        mutate(GDPGrowth=rep(-0.084,times=1016))
SI_2011<-SI_2011 %>%
        mutate(GDPGrowth=rep(0.009,times=165))
SI_2012<-SI_2012 %>%
        mutate(GDPGrowth=rep(0.007,times=1124))

##Bind the data together and give the mean mean of unemployment rate across all time points
SI_EconomicCrisis<-rbind(SI_2006, SI_2008, SI_2010, SI_2011, SI_2012)
SI_EconomicCrisis<-SI_EconomicCrisis %>% 
        mutate(MeanUnEmploymentRate=mean(UnemploymentRate))
SI_EconomicCrisis<-SI_EconomicCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))




#####----------------------------------------------------------------------------------------------------------
##Data management for Spain
##Read ES Round 3-6 in:
ES_Round3to6<-read.csv("./Economic Crisis Data - CSV/ES-3-4-5-6.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
ES_Round3to6 <- ES_Round3to6 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
ES_Round3to6 <- ES_Round3to6 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(ES_Round3to6$inwyys)

##Year of 2010 and 2012 is not available while year of 2009 has only 15 observations
##subset ES for each year:
ES_2006<-ES_Round3to6 %>%
        filter(inwyys==2006)
ES_2007<-ES_Round3to6 %>%
        filter(inwyys==2007)
ES_2008<-ES_Round3to6 %>%
        filter(inwyys==2008)
ES_2009<-ES_Round3to6 %>%
        filter(inwyys==2009)
ES_2011<-ES_Round3to6 %>%
        filter(inwyys==2011)
ES_2013<-ES_Round3to6 %>%
        filter(inwyys==2013)

##Give the unemployment rate of the previous year to each year
ES_2006<-ES_2006 %>%
        mutate(UnemploymentRate=rep(0.092,times=1253))
ES_2007<-ES_2007 %>%
        mutate(UnemploymentRate=rep(0.084,times=427))
ES_2008<-ES_2008 %>%
        mutate(UnemploymentRate=rep(0.082,times=2284))
ES_2009<-ES_2009 %>%
        mutate(UnemploymentRate=rep(0.113,times=15))
ES_2011<-ES_2011 %>%
        mutate(UnemploymentRate=rep(0.199,times=1658))
ES_2013<-ES_2013 %>%
        mutate(UnemploymentRate=rep(0.248,times=1613))

##Give the GDP growth per capita of the previous year to each year
ES_2006<-ES_2006 %>%
        mutate(GDPGrowth=rep(0.019,times=1253))
ES_2007<-ES_2007 %>%
        mutate(GDPGrowth=rep(0.024,times=427))
ES_2008<-ES_2008 %>%
        mutate(GDPGrowth=rep(0.017,times=2284))
ES_2009<-ES_2009 %>%
        mutate(GDPGrowth=rep(-0.007,times=15))
ES_2011<-ES_2011 %>%
        mutate(GDPGrowth=rep(-0.003,times=1658))
ES_2013<-ES_2013 %>%
        mutate(GDPGrowth=rep(-0.030,times=1613))

##Bind the data together and give the mean mean of unemployment rate across all time points
ES_EconomicCrisis<-rbind(ES_2006, ES_2007, ES_2008, ES_2009, ES_2011, ES_2013)
ES_EconomicCrisis<-ES_EconomicCrisis %>% 
        mutate(MeanUnEmploymentRate=mean(UnemploymentRate))
ES_EconomicCrisis<-ES_EconomicCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))



#####----------------------------------------------------------------------------------------------------------
##Data management for Sweden
##Read SE Round 3-6 in:
SE_Round3to6<-read.csv("./Economic Crisis Data - CSV/SE-3-4-5-6.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
SE_Round3to6 <- SE_Round3to6 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
SE_Round3to6 <- SE_Round3to6 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(SE_Round3to6$inwyys)

##subset SE for each year:
SE_2006<-SE_Round3to6 %>%
        filter(inwyys==2006)
SE_2007<-SE_Round3to6 %>%
        filter(inwyys==2007)
SE_2008<-SE_Round3to6 %>%
        filter(inwyys==2008)
SE_2009<-SE_Round3to6 %>%
        filter(inwyys==2009)
SE_2010<-SE_Round3to6 %>%
        filter(inwyys==2010)
SE_2011<-SE_Round3to6 %>%
        filter(inwyys==2011)
SE_2012<-SE_Round3to6 %>%
        filter(inwyys==2012)
SE_2013<-SE_Round3to6 %>%
        filter(inwyys==2013)

##Give the unemployment rate of the previous year to each year
SE_2006<-SE_2006 %>%
        mutate(UnemploymentRate=rep(0.075,times=1488))
SE_2007<-SE_2007 %>%
        mutate(UnemploymentRate=rep(0.071,times=197))
SE_2008<-SE_2008 %>%
        mutate(UnemploymentRate=rep(0.062,times=1448))
SE_2009<-SE_2009 %>%
        mutate(UnemploymentRate=rep(0.062,times=140))
SE_2010<-SE_2010 %>%
        mutate(UnemploymentRate=rep(0.083,times=908))
SE_2011<-SE_2011 %>%
        mutate(UnemploymentRate=rep(0.086,times=386))
SE_2012<-SE_2012 %>%
        mutate(UnemploymentRate=rep(0.078,times=916))
SE_2013<-SE_2013 %>%
        mutate(UnemploymentRate=rep(0.080,times=666))

##Give the GDP growth per capita of the previous year to each year
SE_2006<-SE_2006 %>%
        mutate(GDPGrowth=rep(0.024,times=1488))
SE_2007<-SE_2007 %>%
        mutate(GDPGrowth=rep(0.041,times=197))
SE_2008<-SE_2008 %>%
        mutate(GDPGrowth=rep(0.027,times=1448))
SE_2009<-SE_2009 %>%
        mutate(GDPGrowth=rep(-0.012,times=140))
SE_2010<-SE_2010 %>%
        mutate(GDPGrowth=rep(-0.052,times=908))
SE_2011<-SE_2011 %>%
        mutate(GDPGrowth=rep(0.051,times=386))
SE_2012<-SE_2012 %>%
        mutate(GDPGrowth=rep(0.024,times=916))
SE_2013<-SE_2013 %>%
        mutate(GDPGrowth=rep(-0.013,times=666))


##Bind the data together and give the mean mean of unemployment rate across all time points
SE_EconomicCrisis<-rbind(SE_2006, SE_2007, SE_2008, SE_2009, SE_2010, SE_2011, SE_2012, SE_2013)
SE_EconomicCrisis<-SE_EconomicCrisis %>% 
        mutate(MeanUnEmploymentRate=mean(UnemploymentRate))
SE_EconomicCrisis<-SE_EconomicCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))




#####----------------------------------------------------------------------------------------------------------
##Data management for Switzerland
##Read CH Round 3-6 in:
CH_Round3to6<-read.csv("./Economic Crisis Data - CSV/CH-3-4-5-6.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
CH_Round3to6 <- CH_Round3to6 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
CH_Round3to6 <- CH_Round3to6 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(CH_Round3to6$inwyys)

##subset CH for each year:
CH_2006<-CH_Round3to6 %>%
        filter(inwyys==2006)
CH_2007<-CH_Round3to6 %>%
        filter(inwyys==2007)
CH_2008<-CH_Round3to6 %>%
        filter(inwyys==2008)
CH_2009<-CH_Round3to6 %>%
        filter(inwyys==2009)
CH_2010<-CH_Round3to6 %>%
        filter(inwyys==2010)
CH_2011<-CH_Round3to6 %>%
        filter(inwyys==2011)
CH_2012<-CH_Round3to6 %>%
        filter(inwyys==2012)
CH_2013<-CH_Round3to6 %>%
        filter(inwyys==2013)

##Give the unemployment rate of the previous year to each year
CH_2006<-CH_2006 %>%
        mutate(UnemploymentRate=rep(0.044,times=1132))
CH_2007<-CH_2007 %>%
        mutate(UnemploymentRate=rep(0.040,times=269))
CH_2008<-CH_2008 %>%
        mutate(UnemploymentRate=rep(0.036,times=937))
CH_2009<-CH_2009 %>%
        mutate(UnemploymentRate=rep(0.034,times=398))
CH_2010<-CH_2010 %>%
        mutate(UnemploymentRate=rep(0.041,times=930))
CH_2011<-CH_2011 %>%
        mutate(UnemploymentRate=rep(0.048,times=164))
CH_2012<-CH_2012 %>%
        mutate(UnemploymentRate=rep(0.044,times=933))
CH_2013<-CH_2013 %>%
        mutate(UnemploymentRate=rep(0.045,times=144))

##Give the GDP growth per capita of the previous year to each year
CH_2006<-CH_2006 %>%
        mutate(GDPGrowth=rep(0.021,times=1132))
CH_2007<-CH_2007 %>%
        mutate(GDPGrowth=rep(0.034,times=269))
CH_2008<-CH_2008 %>%
        mutate(GDPGrowth=rep(0.030,times=937))
CH_2009<-CH_2009 %>%
        mutate(GDPGrowth=rep(0.015,times=398))
CH_2010<-CH_2010 %>%
        mutate(GDPGrowth=rep(-0.035,times=930))
CH_2011<-CH_2011 %>%
        mutate(GDPGrowth=rep(0.022,times=164))
CH_2012<-CH_2012 %>%
        mutate(GDPGrowth=rep(0.007,times=933))
CH_2013<-CH_2013 %>%
        mutate(GDPGrowth=rep(0.001,times=144))

##Bind the data together and give the mean mean of unemployment rate across all time points
CH_EconomicCrisis<-rbind(CH_2006, CH_2007, CH_2008, CH_2009, CH_2010, CH_2011, CH_2012, CH_2013)
CH_EconomicCrisis<-CH_EconomicCrisis %>% 
        mutate(MeanUnEmploymentRate=mean(UnemploymentRate))
CH_EconomicCrisis<-CH_EconomicCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))




#####----------------------------------------------------------------------------------------------------------
##Data management for Ukraine
##Read UA Round 3-6 in:
UA_Round3to6<-read.csv("./Economic Crisis Data - CSV/UA-3-4-5-6.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
UA_Round3to6 <- UA_Round3to6 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
UA_Round3to6 <- UA_Round3to6 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(UA_Round3to6$inwyys)

##Year of 2008, 2010, and 2012 is not available
##subset CH for each year:
UA_2006<-UA_Round3to6 %>%
        filter(inwyys==2006)
UA_2007<-UA_Round3to6 %>%
        filter(inwyys==2007)
UA_2009<-UA_Round3to6 %>%
        filter(inwyys==2009)
UA_2011<-UA_Round3to6 %>%
        filter(inwyys==2011)
UA_2013<-UA_Round3to6 %>%
        filter(inwyys==2013)

##Give the unemployment rate of the previous year to each year
UA_2006<-UA_2006 %>%
        mutate(UnemploymentRate=rep(0.072,times=1522))
UA_2007<-UA_2007 %>%
        mutate(UnemploymentRate=rep(0.068,times=112))
UA_2009<-UA_2009 %>%
        mutate(UnemploymentRate=rep(0.064,times=1503))
UA_2011<-UA_2011 %>%
        mutate(UnemploymentRate=rep(0.081,times=1476))
UA_2013<-UA_2013 %>%
        mutate(UnemploymentRate=rep(0.075,times=1814))

##Give the GDP growth per capita of the previous year to each year
UA_2006<-UA_2006 %>%
        mutate(GDPGrowth=rep(0.038,times=1522))
UA_2007<-UA_2007 %>%
        mutate(GDPGrowth=rep(0.083,times=112))
UA_2009<-UA_2009 %>%
        mutate(GDPGrowth=rep(0.028,times=1503))
UA_2011<-UA_2011 %>%
        mutate(GDPGrowth=rep(0.045,times=1476))
UA_2013<-UA_2013 %>%
        mutate(GDPGrowth=rep(0.004,times=1814))

##Bind the data together and give the mean mean of unemployment rate across all time points
UA_EconomicCrisis<-rbind(UA_2006, UA_2007, UA_2009, UA_2011, UA_2013)
UA_EconomicCrisis<-UA_EconomicCrisis %>% 
        mutate(MeanUnEmploymentRate=mean(UnemploymentRate))
UA_EconomicCrisis<-UA_EconomicCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))




#####----------------------------------------------------------------------------------------------------------
##Data management for the UK
##Read GB Round 3-6 in:
GB_Round3to6<-read.csv("./Economic Crisis Data - CSV/GB-3-4-5-6.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
GB_Round3to6 <- GB_Round3to6 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
GB_Round3to6 <- GB_Round3to6 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##To check which year we have data available:
table(GB_Round3to6$inwyys)

##Year of 2007 has only 16 observations and year of 2009 only has 34 observations
##subset CH for each year:
GB_2006<-GB_Round3to6 %>%
        filter(inwyys==2006)
GB_2007<-GB_Round3to6 %>%
        filter(inwyys==2007)
GB_2008<-GB_Round3to6 %>%
        filter(inwyys==2008)
GB_2009<-GB_Round3to6 %>%
        filter(inwyys==2009)
GB_2010<-GB_Round3to6 %>%
        filter(inwyys==2010)
GB_2011<-GB_Round3to6 %>%
        filter(inwyys==2011)
GB_2012<-GB_Round3to6 %>%
        filter(inwyys==2012)
GB_2013<-GB_Round3to6 %>%
        filter(inwyys==2013)

##Give the unemployment rate of the previous year to each year
GB_2006<-GB_2006 %>%
        mutate(UnemploymentRate=rep(0.048,times=2069))
GB_2007<-GB_2007 %>%
        mutate(UnemploymentRate=rep(0.053,times=16))
GB_2008<-GB_2008 %>%
        mutate(UnemploymentRate=rep(0.053,times=2001))
GB_2009<-GB_2009 %>%
        mutate(UnemploymentRate=rep(0.056,times=34))
GB_2010<-GB_2010 %>%
        mutate(UnemploymentRate=rep(0.075,times=1939))
GB_2011<-GB_2011 %>%
        mutate(UnemploymentRate=rep(0.078,times=127))
GB_2012<-GB_2012 %>%
        mutate(UnemploymentRate=rep(0.080,times=1728))
GB_2013<-GB_2013 %>%
        mutate(UnemploymentRate=rep(0.079,times=211))

##Give the GDP growth per capita of the previous year to each year
GB_2006<-GB_2006 %>%
        mutate(GDPGrowth=rep(0.020,times=2069))
GB_2007<-GB_2007 %>%
        mutate(GDPGrowth=rep(0.016,times=16))
GB_2008<-GB_2008 %>%
        mutate(GDPGrowth=rep(0.018,times=2001))
GB_2009<-GB_2009 %>%
        mutate(GDPGrowth=rep(-0.010,times=34))
GB_2010<-GB_2010 %>%
        mutate(GDPGrowth=rep(-0.053,times=1939))
GB_2011<-GB_2011 %>%
        mutate(GDPGrowth=rep(0.014,times=127))
GB_2012<-GB_2012 %>%
        mutate(GDPGrowth=rep(0.004,times=1728))
GB_2013<-GB_2013 %>%
        mutate(GDPGrowth=rep(0.008,times=211))

##Bind the data together and give the mean mean of unemployment rate across all time points
GB_EconomicCrisis<-rbind(GB_2006, GB_2007, GB_2008, GB_2009, GB_2010, GB_2011, GB_2012, GB_2013)
GB_EconomicCrisis<-GB_EconomicCrisis %>% 
        mutate(MeanUnEmploymentRate=mean(UnemploymentRate))
GB_EconomicCrisis<-GB_EconomicCrisis  %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))