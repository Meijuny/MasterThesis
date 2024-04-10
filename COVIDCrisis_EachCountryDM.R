######--------------------------------------------------------------------------------------------------------
##Data management for Austria
##Read AT Round 10 in:
AT10<-read.csv("./COVID crisis Data-CSV/AT-10.csv")

##change the scwsds to inwyys
AT10$InterviewTime<-strptime(AT10$scwsds, "%Y-%m-%d %H:%M:%S")
AT10$inwyys<-format(AT10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
AT10 <- AT10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
AT10 <- AT10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read AT Round 9 in:
AT9<-read.csv("./COVID crisis Data-CSV/AT-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
AT9 <- AT9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
AT9 <- AT9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Bind both rounds data together:
AT_Round9to10<-rbind(AT9, AT10)

##To check which year we have data available:
table(AT_Round9to10$inwyys)

##subset for each year:
AT_2018<-AT_Round9to10 %>%
        filter(inwyys==2018)
AT_2019<-AT_Round9to10 %>%
        filter(inwyys==2019)
AT_2021<-AT_Round9to10 %>%
        filter(inwyys==2021)

##Give the unemployment rate of the previous year to each year
AT_2018<-AT_2018 %>%
        mutate(UnemploymentRate=rep(0.055,times=2029))
AT_2019<-AT_2019 %>%
        mutate(UnemploymentRate=rep(0.048,times=149))
AT_2021<-AT_2021 %>%
        mutate(UnemploymentRate=rep(0.054,times=1074))

##Give the GDP growth of the previous year to each year
AT_2018<-AT_2018 %>%
        mutate(GDPGrowth=rep(0.016, times=2029))
AT_2019<-AT_2019 %>%
        mutate(GDPGrowth=rep(0.019, times=149))
AT_2021<-AT_2021 %>%
        mutate(GDPGrowth=rep(-0.070, times=1074))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
AT_COVIDCrisis<-rbind(AT_2018, AT_2019, AT_2021)
AT_COVIDCrisis<-AT_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
AT_COVIDCrisis<-AT_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("AT_2018", "AT_2019", "AT_2021", "AT_Round9to10",
            "AT9","AT10"))





######--------------------------------------------------------------------------------------------------------
##Data management for Belgium
##Read BE Round 10 in:
BE10<-read.csv("./COVID crisis Data-CSV/BE-10.csv")

##change the scwsds to inwyys
BE10$InterviewTime<-strptime(BE10$inwds, "%Y-%m-%d %H:%M:%S")
BE10$inwyys<-format(BE10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
BE10 <- BE10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
BE10 <- BE10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read BE Round 9 in:
BE9<-read.csv("./COVID crisis Data-CSV/BE-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
BE9 <- BE9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
BE9 <- BE9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##Bind both rounds data together:
BE_Round9to10<-rbind(BE9, BE10)

##To check which year we have data available:
table(BE_Round9to10$inwyys)

##subset for each year:
BE_2018<-BE_Round9to10 %>%
        filter(inwyys==2018)
BE_2019<-BE_Round9to10 %>%
        filter(inwyys==2019)
BE_2021<-BE_Round9to10 %>%
        filter(inwyys==2021)
BE_2022<-BE_Round9to10 %>%
        filter(inwyys==2022)

##Give the unemployment rate of the previous year to each year
BE_2018<-BE_2018 %>%
        mutate(UnemploymentRate=rep(0.071,times=1244))
BE_2019<-BE_2019 %>%
        mutate(UnemploymentRate=rep(0.060,times=192))
BE_2021<-BE_2021 %>%
        mutate(UnemploymentRate=rep(0.055,times=262))
BE_2022<-BE_2022 %>%
        mutate(UnemploymentRate=rep(0.063,times=852))

##Give the GDP growth of the previous year to each year
BE_2018<-BE_2018 %>%
        mutate(GDPGrowth=rep(0.012, times=1244))
BE_2019<-BE_2019 %>%
        mutate(GDPGrowth=rep(0.013, times=192))
BE_2021<-BE_2021 %>%
        mutate(GDPGrowth=rep(-0.057, times=262))
BE_2022<-BE_2022 %>%
        mutate(GDPGrowth=rep(0.064, times=852))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
BE_COVIDCrisis<-rbind(BE_2018, BE_2019, BE_2021, BE_2022)
BE_COVIDCrisis<-BE_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
BE_COVIDCrisis<-BE_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("BE_2018", "BE_2019", "BE_2021", "BE_2022", "BE_Round9to10",
            "BE9","BE10"))




######--------------------------------------------------------------------------------------------------------
##Data management for Bulgaria
##Read BG Round 10 in:
BG10<-read.csv("./COVID crisis Data-CSV/BG-10.csv")

##change the scwsds to inwyys
BG10$InterviewTime<-strptime(BG10$inwds, "%Y-%m-%d %H:%M:%S")
BG10$inwyys<-format(BG10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
BG10 <- BG10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
BG10 <- BG10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read BG Round 9 in:
BG9<-read.csv("./COVID crisis Data-CSV/BG-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
BG9 <- BG9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
BG9 <- BG9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##Bind both rounds data together:
BG_Round9to10<-rbind(BG9, BG10)

##To check which year we have data available:
table(BG_Round9to10$inwyys)

##subset for each year:
BG_2018<-BG_Round9to10 %>%
        filter(inwyys==2018)
BG_2021<-BG_Round9to10 %>%
        filter(inwyys==2021)

##Give the unemployment rate of the previous year to each year
BG_2018<-BG_2018 %>%
        mutate(UnemploymentRate=rep(0.062,times=1876))
BG_2021<-BG_2021 %>%
        mutate(UnemploymentRate=rep(0.051,times=2640))

##Give the GDP growth of the previous year to each year
BG_2018<-BG_2018 %>%
        mutate(GDPGrowth=rep(0.035, times=1876))
BG_2021<-BG_2021 %>%
        mutate(GDPGrowth=rep(-0.034, times=2640))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
BG_COVIDCrisis<-rbind(BG_2018, BG_2021)
BG_COVIDCrisis<-BG_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
BG_COVIDCrisis<-BG_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("BG_2018", "BG_2021", "BG_Round9to10",
            "BG9","BG10"))





######--------------------------------------------------------------------------------------------------------
##Data management for Croatia
##Read HR Round 10 in:
HR10<-read.csv("./COVID crisis Data-CSV/HR-10.csv")

##change the scwsds to inwyys
HR10$InterviewTime<-strptime(HR10$inwds, "%Y-%m-%d %H:%M:%S")
HR10$inwyys<-format(HR10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
HR10 <- HR10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
HR10 <- HR10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read HR Round 9 in:
HR9<-read.csv("./COVID crisis Data-CSV/HR-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
HR9 <- HR9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
HR9 <- HR9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##Bind both rounds data together:
HR_Round9to10<-rbind(HR9, HR10)

##To check which year we have data available:
table(HR_Round9to10$inwyys)

##subset for each year:
HR_2019<-HR_Round9to10 %>%
        filter(inwyys==2019)
HR_2020<-HR_Round9to10 %>%
        filter(inwyys==2020)
HR_2021<-HR_Round9to10 %>%
        filter(inwyys==2021)

##Give the unemployment rate of the previous year to each year
HR_2019<-HR_2019 %>%
        mutate(UnemploymentRate=rep(0.084,times=1410))
HR_2020<-HR_2020 %>%
        mutate(UnemploymentRate=rep(0.066,times=100))
HR_2021<-HR_2021 %>%
        mutate(UnemploymentRate=rep(0.075,times=1410))

##Give the GDP growth of the previous year to each year
HR_2019<-HR_2019 %>%
        mutate(GDPGrowth=rep(0.037, times=1410))
HR_2020<-HR_2020 %>%
        mutate(GDPGrowth=rep(0.040, times=100))
HR_2021<-HR_2021 %>%
        mutate(GDPGrowth=rep(-0.082, times=1410))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
HR_COVIDCrisis<-rbind(HR_2019, HR_2020, HR_2021)
HR_COVIDCrisis<-HR_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
HR_COVIDCrisis<-HR_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("HR_2019", "HR_2020", "HR_2021", "HR_Round9to10",
            "HR9","HR10"))



######--------------------------------------------------------------------------------------------------------
##Data management for Cyprus
##Read CY Round 10 in:
CY10<-read.csv("./COVID crisis Data-CSV/CY-10.csv")

##change the scwsds to inwyys
CY10$InterviewTime<-strptime(CY10$scwsds, "%Y-%m-%d %H:%M:%S")
CY10$inwyys<-format(CY10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
CY10 <- CY10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
CY10 <- CY10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read CY Round 9 in:
CY9<-read.csv("./COVID crisis Data-CSV/CY-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
CY9 <- CY9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
CY9 <- CY9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Bind both rounds data together:
CY_Round9to10<-rbind(CY9, CY10)

##To check which year we have data available:
table(CY_Round9to10$inwyys)

##subset for each year:
CY_2018<-CY_Round9to10 %>%
        filter(inwyys==2018)
CY_2019<-CY_Round9to10 %>%
        filter(inwyys==2019)
CY_2022<-CY_Round9to10 %>%
        filter(inwyys==2022)

##Give the unemployment rate of the previous year to each year
CY_2018<-CY_2018 %>%
        mutate(UnemploymentRate=rep(0.111,times=499))
CY_2019<-CY_2019 %>%
        mutate(UnemploymentRate=rep(0.084,times=199))
CY_2022<-CY_2022 %>%
        mutate(UnemploymentRate=rep(0.075,times=448))

##Give the GDP growth of the previous year to each year
CY_2018<-CY_2018 %>%
        mutate(GDPGrowth=rep(0.048, times=499))
CY_2019<-CY_2019 %>%
        mutate(GDPGrowth=rep(0.044, times=199))
CY_2022<-CY_2022 %>%
        mutate(GDPGrowth=rep(0.056, times=448))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
CY_COVIDCrisis<-rbind(CY_2018, CY_2019, CY_2022)
CY_COVIDCrisis<-CY_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
CY_COVIDCrisis<-CY_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("CY_2018", "CY_2019", "CY_2022", "CY_Round9to10",
            "CY9","CY10"))




######--------------------------------------------------------------------------------------------------------
##Data management for Czech
##Read CZ Round 10 in:
CZ10<-read.csv("./COVID crisis Data-CSV/CZ-10.csv")

##change the scwsds to inwyys
CZ10$InterviewTime<-strptime(CZ10$inwds, "%Y-%m-%d %H:%M:%S")
CZ10$inwyys<-format(CZ10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
CZ10 <- CZ10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
CZ10 <- CZ10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
CZ9<-read.csv("./COVID crisis Data-CSV/CZ-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
CZ9 <- CZ9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
CZ9 <- CZ9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##Bind both rounds data together:
CZ_Round9to10<-rbind(CZ9, CZ10)

##To check which year we have data available:
table(CZ_Round9to10$inwyys)

##subset for each year:
CZ_2018<-CZ_Round9to10 %>%
        filter(inwyys==2018)
CZ_2019<-CZ_Round9to10 %>%
        filter(inwyys==2019)
CZ_2021<-CZ_Round9to10 %>%
        filter(inwyys==2021)


##Give the unemployment rate of the previous year to each year
CZ_2018<-CZ_2018 %>%
        mutate(UnemploymentRate=rep(0.029,times=1099))
CZ_2019<-CZ_2019 %>%
        mutate(UnemploymentRate=rep(0.022,times=1188))
CZ_2021<-CZ_2021 %>%
        mutate(UnemploymentRate=rep(0.025,times=2360))

##Give the GDP growth of the previous year to each year
CZ_2018<-CZ_2018 %>%
        mutate(GDPGrowth=rep(0.049, times=1099))
CZ_2019<-CZ_2019 %>%
        mutate(GDPGrowth=rep(0.029, times=1188))
CZ_2021<-CZ_2021 %>%
        mutate(GDPGrowth=rep(-0.057, times=2360))


##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
CZ_COVIDCrisis<-rbind(CZ_2018, CZ_2019, CZ_2021)
CZ_COVIDCrisis<-CZ_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
CZ_COVIDCrisis<-CZ_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("CZ_2018", "CZ_2019", "CZ_2021", "CZ_Round9to10",
            "CZ9","CZ10"))




######--------------------------------------------------------------------------------------------------------
##Data management for Estonia
##Read EE Round 10 in:
EE10<-read.csv("./COVID crisis Data-CSV/EE-10.csv")

##change the scwsds to inwyys
EE10$InterviewTime<-strptime(EE10$inwds, "%Y-%m-%d %H:%M:%S")
EE10$inwyys<-format(EE10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
EE10 <- EE10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
EE10 <- EE10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
EE9<-read.csv("./COVID crisis Data-CSV/EE-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
EE9 <- EE9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
EE9 <- EE9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##Bind both rounds data together:
EE_Round9to10<-rbind(EE9, EE10)

##To check which year we have data available:
table(EE_Round9to10$inwyys)

##subset for each year:
EE_2018<-EE_Round9to10 %>%
        filter(inwyys==2018)
EE_2019<-EE_Round9to10 %>%
        filter(inwyys==2019)
EE_2021<-EE_Round9to10 %>%
        filter(inwyys==2021)


##Give the unemployment rate of the previous year to each year
EE_2018<-EE_2018 %>%
        mutate(UnemploymentRate=rep(0.058,times=1296))
EE_2019<-EE_2019 %>%
        mutate(UnemploymentRate=rep(0.054,times=146))
EE_2021<-EE_2021 %>%
        mutate(UnemploymentRate=rep(0.070,times=1266))

##Give the GDP growth of the previous year to each year
EE_2018<-EE_2018 %>%
        mutate(GDPGrowth=rep(0.057, times=1296))
EE_2019<-EE_2019 %>%
        mutate(GDPGrowth=rep(0.034, times=146))
EE_2021<-EE_2021 %>%
        mutate(GDPGrowth=rep(-0.007, times=1266))


##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
EE_COVIDCrisis<-rbind(EE_2018, EE_2019, EE_2021)
EE_COVIDCrisis<-EE_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
EE_COVIDCrisis<-EE_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("EE_2018", "EE_2019", "EE_2021", "EE_Round9to10",
            "EE9","EE10"))




######--------------------------------------------------------------------------------------------------------
##Data management for Finland
##Read FI Round 10 in:
FI10<-read.csv("./COVID crisis Data-CSV/FI-10.csv")

##change the scwsds to inwyys
FI10$InterviewTime<-strptime(FI10$inwds, "%Y-%m-%d %H:%M:%S")
FI10$inwyys<-format(FI10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
FI10 <- FI10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
FI10 <- FI10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
FI9<-read.csv("./COVID crisis Data-CSV/FI-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
FI9 <- FI9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
FI9 <- FI9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##Bind both rounds data together:
FI_Round9to10<-rbind(FI9, FI10)

##To check which year we have data available:
table(FI_Round9to10$inwyys)

##subset for each year:
FI_2018<-FI_Round9to10 %>%
        filter(inwyys==2018)
FI_2019<-FI_Round9to10 %>%
        filter(inwyys==2019)
FI_2021<-FI_Round9to10 %>%
        filter(inwyys==2021)
FI_2022<-FI_Round9to10 %>%
        filter(inwyys==2022)


##Give the unemployment rate of the previous year to each year
FI_2018<-FI_2018 %>%
        mutate(UnemploymentRate=rep(0.086,times=1411))
FI_2019<-FI_2019 %>%
        mutate(UnemploymentRate=rep(0.074,times=242))
FI_2021<-FI_2021 %>%
        mutate(UnemploymentRate=rep(0.078,times=1510))
FI_2022<-FI_2022 %>%
        mutate(UnemploymentRate=rep(0.076,times=7))

##Give the GDP growth of the previous year to each year
FI_2018<-FI_2018 %>%
        mutate(GDPGrowth=rep(0.030, times=1411))
FI_2019<-FI_2019 %>%
        mutate(GDPGrowth=rep(0.010, times=242))
FI_2021<-FI_2021 %>%
        mutate(GDPGrowth=rep(-0.025, times=1510))
FI_2022<-FI_2022 %>%
        mutate(GDPGrowth=rep(0.030, times=7))


##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
FI_COVIDCrisis<-rbind(FI_2018, FI_2019, FI_2021, FI_2022)
FI_COVIDCrisis<-FI_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
FI_COVIDCrisis<-FI_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("FI_2018", "FI_2019", "FI_2021","FI_2022", "FI_Round9to10",
            "FI9","FI10"))




######--------------------------------------------------------------------------------------------------------
##Data management for France
##Read FR Round 10 in:
FR10<-read.csv("./COVID crisis Data-CSV/FR-10.csv")

##change the scwsds to inwyys
FR10$InterviewTime<-strptime(FR10$inwds, "%Y-%m-%d %H:%M:%S")
FR10$inwyys<-format(FR10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
FR10 <- FR10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
FR10 <- FR10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
FR9<-read.csv("./COVID crisis Data-CSV/FR-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
FR9 <- FR9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
FR9 <- FR9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##Bind both rounds data together:
FR_Round9to10<-rbind(FR9, FR10)

##To check which year we have data available:
table(FR_Round9to10$inwyys)

##subset for each year:
FR_2018<-FR_Round9to10 %>%
        filter(inwyys==2018)
FR_2019<-FR_Round9to10 %>%
        filter(inwyys==2019)
FR_2021<-FR_Round9to10 %>%
        filter(inwyys==2021)


##Give the unemployment rate of the previous year to each year
FR_2018<-FR_2018 %>%
        mutate(UnemploymentRate=rep(0.094,times=975))
FR_2019<-FR_2019 %>%
        mutate(UnemploymentRate=rep(0.090,times=732))
FR_2021<-FR_2021 %>%
        mutate(UnemploymentRate=rep(0.080,times=1734))

##Give the GDP growth of the previous year to each year
FR_2018<-FR_2018 %>%
        mutate(GDPGrowth=rep(0.020, times=975))
FR_2019<-FR_2019 %>%
        mutate(GDPGrowth=rep(0.015, times=732))
FR_2021<-FR_2021 %>%
        mutate(GDPGrowth=rep(-0.078, times=1734))


##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
FR_COVIDCrisis<-rbind(FR_2018, FR_2019, FR_2021)
FR_COVIDCrisis<-FR_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
FR_COVIDCrisis<-FR_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("FR_2018", "FR_2019", "FR_2021", "FR_Round9to10",
            "FR9","FR10"))



######--------------------------------------------------------------------------------------------------------
##Data management for Germany
##Read DE Round 10 in:
DE10<-read.csv("./COVID crisis Data-CSV/DE-10.csv")

##change the scwsds to inwyys
DE10$InterviewTime<-strptime(DE10$scwass, "%Y-%m-%d %H:%M:%S")
DE10$inwyys<-format(DE10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
DE10 <- DE10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
DE10 <- DE10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
DE9<-read.csv("./COVID crisis Data-CSV/DE-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
DE9 <- DE9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
DE9 <- DE9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Bind both rounds data together:
DE_Round9to10<-rbind(DE9, DE10)

##To check which year we have data available:
table(DE_Round9to10$inwyys)

##subset for each year:
DE_2018<-DE_Round9to10 %>%
        filter(inwyys==2018)
DE_2019<-DE_Round9to10 %>%
        filter(inwyys==2019)
DE_2021<-DE_Round9to10 %>%
        filter(inwyys==2021)
DE_2022<-DE_Round9to10 %>%
        filter(inwyys==2022)

##Give the unemployment rate of the previous year to each year
DE_2018<-DE_2018 %>%
        mutate(UnemploymentRate=rep(0.038,times=1567))
DE_2019<-DE_2019 %>%
        mutate(UnemploymentRate=rep(0.034,times=415))
DE_2021<-DE_2021 %>%
        mutate(UnemploymentRate=rep(0.039,times=4723))
DE_2022<-DE_2022 %>%
        mutate(UnemploymentRate=rep(0.036,times=3))

##Give the GDP growth of the previous year to each year
DE_2018<-DE_2018 %>%
        mutate(GDPGrowth=rep(0.023, times=1567))
DE_2019<-DE_2019 %>%
        mutate(GDPGrowth=rep(0.007, times=415))
DE_2021<-DE_2021 %>%
        mutate(GDPGrowth=rep(-0.039, times=4723))
DE_2022<-DE_2022 %>%
        mutate(GDPGrowth=rep(0.031, times=3))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
DE_COVIDCrisis<-rbind(DE_2018, DE_2019, DE_2021, DE_2022)
DE_COVIDCrisis<-DE_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
DE_COVIDCrisis<-DE_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("DE_2018", "DE_2019", "DE_2021", "DE_2022", "DE_Round9to10",
            "DE9","DE10"))





######--------------------------------------------------------------------------------------------------------
##Data management for Hungary
##Read HU Round 10 in:
HU10<-read.csv("./COVID crisis Data-CSV/HU-10.csv")

##change the scwsds to inwyys
HU10$InterviewTime<-strptime(HU10$inwds, "%Y-%m-%d %H:%M:%S")
HU10$inwyys<-format(HU10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
HU10 <- HU10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
HU10 <- HU10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
HU9<-read.csv("./COVID crisis Data-CSV/HU-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
HU9 <- HU9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
HU9 <- HU9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##Bind both rounds data together:
HU_Round9to10<-rbind(HU9, HU10)

##To check which year we have data available:
table(HU_Round9to10$inwyys)

##subset for each year:
HU_2019<-HU_Round9to10 %>%
        filter(inwyys==2019)
HU_2021<-HU_Round9to10 %>%
        filter(inwyys==2021)

##Give the unemployment rate of the previous year to each year
HU_2019<-HU_2019 %>%
        mutate(UnemploymentRate=rep(0.037,times=1574))
HU_2021<-HU_2021 %>%
        mutate(UnemploymentRate=rep(0.043,times=1770))

##Give the GDP growth of the previous year to each year
HU_2019<-HU_2019 %>%
        mutate(GDPGrowth=rep(0.055, times=1574))
HU_2021<-HU_2021 %>%
        mutate(GDPGrowth=rep(-0.043, times=1770))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
HU_COVIDCrisis<-rbind(HU_2019, HU_2021)
HU_COVIDCrisis<-HU_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
HU_COVIDCrisis<-HU_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("HU_2019", "HU_2021", "HU_Round9to10",
            "HU9","HU10"))





######--------------------------------------------------------------------------------------------------------
##Data management for Iceland
##Read IS Round 10 in:
IS10<-read.csv("./COVID crisis Data-CSV/IS-10.csv")

##change the scwsds to inwyys
IS10$InterviewTime<-strptime(IS10$inwds, "%Y-%m-%d %H:%M:%S")
IS10$inwyys<-format(IS10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
IS10 <- IS10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
IS10 <- IS10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
IS9<-read.csv("./COVID crisis Data-CSV/IS-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
IS9 <- IS9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
IS9 <- IS9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##Bind both rounds data together:
IS_Round9to10<-rbind(IS9, IS10)

##To check which year we have data available:
table(IS_Round9to10$inwyys)

##subset for each year:
IS_2019<-IS_Round9to10 %>%
        filter(inwyys==2019)
IS_2020<-IS_Round9to10 %>%
        filter(inwyys==2020)
IS_2021<-IS_Round9to10 %>%
        filter(inwyys==2021)
IS_2022<-IS_Round9to10 %>%
        filter(inwyys==2022)

##Give the unemployment rate of the previous year to each year
IS_2019<-IS_2019 %>%
        mutate(UnemploymentRate=rep(0.027,times=704))
IS_2020<-IS_2020 %>%
        mutate(UnemploymentRate=rep(0.035,times=71))
IS_2021<-IS_2021 %>%
        mutate(UnemploymentRate=rep(0.055,times=790))
IS_2022<-IS_2022 %>%
        mutate(UnemploymentRate=rep(0.060,times=37))

##Give the GDP growth of the previous year to each year
IS_2019<-IS_2019 %>%
        mutate(GDPGrowth=rep(0.021, times=704))
IS_2020<-IS_2020 %>%
        mutate(GDPGrowth=rep(-0.004, times=71))
IS_2021<-IS_2021 %>%
        mutate(GDPGrowth=rep(-0.087, times=790))
IS_2022<-IS_2022 %>%
        mutate(GDPGrowth=rep(0.028, times=37))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
IS_COVIDCrisis<-rbind(IS_2019, IS_2020, IS_2021, IS_2022)
IS_COVIDCrisis<-IS_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
IS_COVIDCrisis<-IS_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("IS_2019","IS_2020", "IS_2021", "IS_2022", "IS_Round9to10",
            "IS9","IS10"))




######--------------------------------------------------------------------------------------------------------
##Data management for Ireland
##Read IE Round 10 in:
IE10<-read.csv("./COVID crisis Data-CSV/IE-10.csv")

##change the scwsds to inwyys
IE10$InterviewTime<-strptime(IE10$inwds, "%Y-%m-%d %H:%M:%S")
IE10$inwyys<-format(IE10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
IE10 <- IE10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
IE10 <- IE10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
IE9<-read.csv("./COVID crisis Data-CSV/IE-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
IE9 <- IE9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
IE9 <- IE9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##Bind both rounds data together:
IE_Round9to10<-rbind(IE9, IE10)

##To check which year we have data available:
table(IE_Round9to10$inwyys)

##subset for each year:
IE_2018<-IE_Round9to10 %>%
        filter(inwyys==2018)
IE_2019<-IE_Round9to10 %>%
        filter(inwyys==2019)
IE_2021<-IE_Round9to10 %>%
        filter(inwyys==2021)
IE_2022<-IE_Round9to10 %>%
        filter(inwyys==2022)

##Give the unemployment rate of the previous year to each year
IE_2018<-IE_2018 %>%
        mutate(UnemploymentRate=rep(0.067,times=314))
IE_2019<-IE_2019 %>%
        mutate(UnemploymentRate=rep(0.057,times=1476))
IE_2021<-IE_2021 %>%
        mutate(UnemploymentRate=rep(0.056,times=21))
IE_2022<-IE_2022 %>%
        mutate(UnemploymentRate=rep(0.062,times=1399))

##Give the GDP growth of the previous year to each year
IE_2018<-IE_2018 %>%
        mutate(GDPGrowth=rep(0.081, times=314))
IE_2019<-IE_2019 %>%
        mutate(GDPGrowth=rep(0.071, times=1476))
IE_2021<-IE_2021 %>%
        mutate(GDPGrowth=rep(0.055, times=21))
IE_2022<-IE_2022 %>%
        mutate(GDPGrowth=rep(0.140, times=1399))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
IE_COVIDCrisis<-rbind(IE_2018, IE_2019, IE_2021, IE_2022)
IE_COVIDCrisis<-IE_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
IE_COVIDCrisis<-IE_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("IE_2018","IE_2019", "IE_2021", "IE_2022", "IE_Round9to10",
            "IE9","IE10"))




######--------------------------------------------------------------------------------------------------------
##Data management for Italy
##Read IT Round 10 in:
IT10<-read.csv("./COVID crisis Data-CSV/IT-10.csv")

##change the scwsds to inwyys
IT10$InterviewTime<-strptime(IT10$inwds, "%Y-%m-%d %H:%M:%S")
IT10$inwyys<-format(IT10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
IT10 <- IT10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
IT10 <- IT10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
IT9<-read.csv("./COVID crisis Data-CSV/IT-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
IT9 <- IT9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
IT9 <- IT9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##Bind both rounds data together:
IT_Round9to10<-rbind(IT9, IT10)

##To check which year we have data available:
table(IT_Round9to10$inwyys)

##subset for each year:
IT_2018<-IT_Round9to10 %>%
        filter(inwyys==2018)
IT_2019<-IT_Round9to10 %>%
        filter(inwyys==2019)
IT_2021<-IT_Round9to10 %>%
        filter(inwyys==2021)
IT_2022<-IT_Round9to10 %>%
        filter(inwyys==2022)

##Give the unemployment rate of the previous year to each year
IT_2018<-IT_2018 %>%
        mutate(UnemploymentRate=rep(0.112,times=92))
IT_2019<-IT_2019 %>%
        mutate(UnemploymentRate=rep(0.106,times=2343))
IT_2021<-IT_2021 %>%
        mutate(UnemploymentRate=rep(0.092,times=414))
IT_2022<-IT_2022 %>%
        mutate(UnemploymentRate=rep(0.095,times=2010))

##Give the GDP growth of the previous year to each year
IT_2018<-IT_2018 %>%
        mutate(GDPGrowth=rep(0.018, times=92))
IT_2019<-IT_2019 %>%
        mutate(GDPGrowth=rep(0.011, times=2343))
IT_2021<-IT_2021 %>%
        mutate(GDPGrowth=rep(-0.085, times=414))
IT_2022<-IT_2022 %>%
        mutate(GDPGrowth=rep(0.089, times=2010))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
IT_COVIDCrisis<-rbind(IT_2018, IT_2019, IT_2021, IT_2022)
IT_COVIDCrisis<-IT_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
IT_COVIDCrisis<-IT_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("IT_2018","IT_2019", "IT_2021", "IT_2022", "IT_Round9to10",
            "IT9","IT10"))





######--------------------------------------------------------------------------------------------------------
##Data management for Latvia
##Read LV Round 10 in:
LV10<-read.csv("./COVID crisis Data-CSV/LV-10.csv")

##change the scwsds to inwyys
LV10$InterviewTime<-strptime(LV10$scwass, "%Y-%m-%d %H:%M:%S")
LV10$inwyys<-format(LV10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
LV10 <- LV10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
LV10 <- LV10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
LV9<-read.csv("./COVID crisis Data-CSV/LV-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
LV9 <- LV9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
LV9 <- LV9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Bind both rounds data together:
LV_Round9to10<-rbind(LV9, LV10)

##To check which year we have data available:
table(LV_Round9to10$inwyys)

##subset for each year:
LV_2019<-LV_Round9to10 %>%
        filter(inwyys==2019)
LV_2020<-LV_Round9to10 %>%
        filter(inwyys==2020)
LV_2021<-LV_Round9to10 %>%
        filter(inwyys==2021)
LV_2022<-LV_Round9to10 %>%
        filter(inwyys==2022)

##Give the unemployment rate of the previous year to each year
LV_2019<-LV_2019 %>%
        mutate(UnemploymentRate=rep(0.074,times=651))
LV_2020<-LV_2020 %>%
        mutate(UnemploymentRate=rep(0.063,times=73))
LV_2021<-LV_2021 %>%
        mutate(UnemploymentRate=rep(0.081,times=670))
LV_2022<-LV_2022 %>%
        mutate(UnemploymentRate=rep(0.075,times=17))

##Give the GDP growth of the previous year to each year
LV_2019<-LV_2019 %>%
        mutate(GDPGrowth=rep(0.048, times=651))
LV_2020<-LV_2020 %>%
        mutate(GDPGrowth=rep(0.013, times=73))
LV_2021<-LV_2021 %>%
        mutate(GDPGrowth=rep(-0.028, times=670))
LV_2022<-LV_2022 %>%
        mutate(GDPGrowth=rep(0.076, times=17))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
LV_COVIDCrisis<-rbind(LV_2019, LV_2020, LV_2021, LV_2022)
LV_COVIDCrisis<-LV_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
LV_COVIDCrisis<-LV_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("LV_2019", "LV_2020", "LV_2021", "LV_2022", "LV_Round9to10",
            "LV9","LV10"))





######--------------------------------------------------------------------------------------------------------
##Data management for Lithuania
##Read LT Round 10 in:
LT10<-read.csv("./COVID crisis Data-CSV/LT-10.csv")

##change the scwsds to inwyys
LT10$InterviewTime<-strptime(LT10$inwds, "%Y-%m-%d %H:%M:%S")
LT10$inwyys<-format(LT10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
LT10 <- LT10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
LT10 <- LT10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
LT9<-read.csv("./COVID crisis Data-CSV/LT-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
LT9 <- LT9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
LT9 <- LT9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##Bind both rounds data together:
LT_Round9to10<-rbind(LT9, LT10)

##To check which year we have data available:
table(LT_Round9to10$inwyys)

##subset for each year:
LT_2019<-LT_Round9to10 %>%
        filter(inwyys==2019)
LT_2021<-LT_Round9to10 %>%
        filter(inwyys==2021)

##Give the unemployment rate of the previous year to each year
LT_2019<-LT_2019 %>%
        mutate(UnemploymentRate=rep(0.062,times=1691))
LT_2021<-LT_2021 %>%
        mutate(UnemploymentRate=rep(0.085,times=1591))

##Give the GDP growth of the previous year to each year
LT_2019<-LT_2019 %>%
        mutate(GDPGrowth=rep(0.050, times=1691))
LT_2021<-LT_2021 %>%
        mutate(GDPGrowth=rep(-0.001, times=1591))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
LT_COVIDCrisis<-rbind(LT_2019, LT_2021)
LT_COVIDCrisis<-LT_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
LT_COVIDCrisis<-LT_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("LT_2019", "LT_2021", "LT_Round9to10",
            "LT9","LT10"))




######--------------------------------------------------------------------------------------------------------
##Data management for Montenegro
##Read ME Round 10 in:
ME10<-read.csv("./COVID crisis Data-CSV/ME-10.csv")

##change the scwsds to inwyys
ME10$InterviewTime<-strptime(ME10$inwds, "%Y-%m-%d %H:%M:%S")
ME10$inwyys<-format(ME10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
ME10 <- ME10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
ME10 <- ME10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
ME9<-read.csv("./COVID crisis Data-CSV/ME-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
ME9 <- ME9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
ME9 <- ME9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##Bind both rounds data together:
ME_Round9to10<-rbind(ME9, ME10)

##To check which year we have data available:
table(ME_Round9to10$inwyys)

##subset for each year:
ME_2019<-ME_Round9to10 %>%
        filter(inwyys==2019)
ME_2021<-ME_Round9to10 %>%
        filter(inwyys==2021)
ME_2022<-ME_Round9to10 %>%
        filter(inwyys==2022)

##Give the unemployment rate of the previous year to each year
ME_2019<-ME_2019 %>%
        mutate(UnemploymentRate=rep(0.152,times=881))
ME_2021<-ME_2021 %>%
        mutate(UnemploymentRate=rep(0.179,times=613))
ME_2022<-ME_2022 %>%
        mutate(UnemploymentRate=rep(0.169,times=594))

##Give the GDP growth of the previous year to each year
ME_2019<-ME_2019 %>%
        mutate(GDPGrowth=rep(0.051, times=881))
ME_2021<-ME_2021 %>%
        mutate(GDPGrowth=rep(-0.152, times=613))
ME_2022<-ME_2022 %>%
        mutate(GDPGrowth=rep(0.134, times=594))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
ME_COVIDCrisis<-rbind(ME_2019, ME_2021, ME_2022)
ME_COVIDCrisis<-ME_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
ME_COVIDCrisis<-ME_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("ME_2019", "ME_2021", "ME_2022", "ME_Round9to10",
            "ME9","ME10"))





######--------------------------------------------------------------------------------------------------------
##Data management for Netherlands
##Read NL Round 10 in:
NL10<-read.csv("./COVID crisis Data-CSV/NL-10.csv")

##change the scwsds to inwyys
NL10$InterviewTime<-strptime(NL10$inwds, "%Y-%m-%d %H:%M:%S")
NL10$inwyys<-format(NL10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
NL10 <- NL10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
NL10 <- NL10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
NL9<-read.csv("./COVID crisis Data-CSV/NL-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
NL9 <- NL9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
NL9 <- NL9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##Bind both rounds data together:
NL_Round9to10<-rbind(NL9, NL10)

##To check which year we have data available:
table(NL_Round9to10$inwyys)

##subset for each year:
NL_2018<-NL_Round9to10 %>%
        filter(inwyys==2018)
NL_2019<-NL_Round9to10 %>%
        filter(inwyys==2019)
NL_2021<-NL_Round9to10 %>%
        filter(inwyys==2021)
NL_2022<-NL_Round9to10 %>%
        filter(inwyys==2022)

##Give the unemployment rate of the previous year to each year
NL_2018<-NL_2018 %>%
        mutate(UnemploymentRate=rep(0.048,times=1353))
NL_2019<-NL_2019 %>%
        mutate(UnemploymentRate=rep(0.038,times=90))
NL_2021<-NL_2021 %>%
        mutate(UnemploymentRate=rep(0.038,times=857))
NL_2022<-NL_2022 %>%
        mutate(UnemploymentRate=rep(0.042,times=483))

##Give the GDP growth of the previous year to each year
NL_2018<-NL_2018 %>%
        mutate(GDPGrowth=rep(0.023, times=1353))
NL_2019<-NL_2019 %>%
        mutate(GDPGrowth=rep(0.018, times=90))
NL_2021<-NL_2021 %>%
        mutate(GDPGrowth=rep(-0.044, times=857))
NL_2022<-NL_2022 %>%
        mutate(GDPGrowth=rep(0.056, times=483))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
NL_COVIDCrisis<-rbind(NL_2018, NL_2019, NL_2021, NL_2022)
NL_COVIDCrisis<-NL_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
NL_COVIDCrisis<-NL_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("NL_2018","NL_2019", "NL_2021", "NL_2022", "NL_Round9to10",
            "NL9","NL10"))






######--------------------------------------------------------------------------------------------------------
##Data management for Norway
##Read NO Round 10 in:
NO10<-read.csv("./COVID crisis Data-CSV/NO-10.csv")

##change the scwsds to inwyys
NO10$InterviewTime<-strptime(NO10$inwds, "%Y-%m-%d %H:%M:%S")
NO10$inwyys<-format(NO10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
NO10 <- NO10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
NO10 <- NO10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
NO9<-read.csv("./COVID crisis Data-CSV/NO-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
NO9 <- NO9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
NO9 <- NO9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##Bind both rounds data together:
NO_Round9to10<-rbind(NO9, NO10)

##To check which year we have data available:
table(NO_Round9to10$inwyys)

##subset for each year:
NO_2018<-NO_Round9to10 %>%
        filter(inwyys==2018)
NO_2019<-NO_Round9to10 %>%
        filter(inwyys==2019)
NO_2021<-NO_Round9to10 %>%
        filter(inwyys==2021)
NO_2022<-NO_Round9to10 %>%
        filter(inwyys==2022)

##Give the unemployment rate of the previous year to each year
NO_2018<-NO_2018 %>%
        mutate(UnemploymentRate=rep(0.042,times=546))
NO_2019<-NO_2019 %>%
        mutate(UnemploymentRate=rep(0.038,times=663))
NO_2021<-NO_2021 %>%
        mutate(UnemploymentRate=rep(0.044,times=1017))
NO_2022<-NO_2022 %>%
        mutate(UnemploymentRate=rep(0.044,times=233))

##Give the GDP growth of the previous year to each year
NO_2018<-NO_2018 %>%
        mutate(GDPGrowth=rep(0.016, times=546))
NO_2019<-NO_2019 %>%
        mutate(GDPGrowth=rep(0.002, times=663))
NO_2021<-NO_2021 %>%
        mutate(GDPGrowth=rep(-0.019, times=1017))
NO_2022<-NO_2022 %>%
        mutate(GDPGrowth=rep(0.033, times=233))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
NO_COVIDCrisis<-rbind(NO_2018, NO_2019, NO_2021, NO_2022)
NO_COVIDCrisis<-NO_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
NO_COVIDCrisis<-NO_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("NO_2018","NO_2019", "NO_2021", "NO_2022", "NO_Round9to10",
            "NO9","NO10"))





######--------------------------------------------------------------------------------------------------------
##Data management for Poland
##Read PL Round 10 in:
PL10<-read.csv("./COVID crisis Data-CSV/PL-10.csv")

##change the scwsds to inwyys
PL10$InterviewTime<-strptime(PL10$scwass, "%Y-%m-%d %H:%M:%S")
PL10$inwyys<-format(PL10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
PL10 <- PL10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
PL10 <- PL10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
PL9<-read.csv("./COVID crisis Data-CSV/PL-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
PL9 <- PL9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
PL9 <- PL9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Bind both rounds data together:
PL_Round9to10<-rbind(PL9, PL10)

##To check which year we have data available:
table(PL_Round9to10$inwyys)

##subset for each year:
PL_2018<-PL_Round9to10 %>%
        filter(inwyys==2018)
PL_2019<-PL_Round9to10 %>%
        filter(inwyys==2019)
PL_2021<-PL_Round9to10 %>%
        filter(inwyys==2021)
PL_2022<-PL_Round9to10 %>%
        filter(inwyys==2022)

##Give the unemployment rate of the previous year to each year
PL_2018<-PL_2018 %>%
        mutate(UnemploymentRate=rep(0.049,times=949))
PL_2019<-PL_2019 %>%
        mutate(UnemploymentRate=rep(0.039,times=501))
PL_2021<-PL_2021 %>%
        mutate(UnemploymentRate=rep(0.032,times=46))
PL_2022<-PL_2022 %>%
        mutate(UnemploymentRate=rep(0.034,times=644))

##Give the GDP growth of the previous year to each year
PL_2018<-PL_2018 %>%
        mutate(GDPGrowth=rep(0.051, times=949))
PL_2019<-PL_2019 %>%
        mutate(GDPGrowth=rep(0.059, times=501))
PL_2021<-PL_2021 %>%
        mutate(GDPGrowth=rep(-0.018, times=46))
PL_2022<-PL_2022 %>%
        mutate(GDPGrowth=rep(0.074, times=644))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
PL_COVIDCrisis<-rbind(PL_2018, PL_2019, PL_2021, PL_2022)
PL_COVIDCrisis<-PL_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
PL_COVIDCrisis<-PL_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("PL_2018", "PL_2019", "PL_2021", "PL_2022", "PL_Round9to10",
            "PL9","PL10"))







######--------------------------------------------------------------------------------------------------------
##Data management for Portugal
##Read PT Round 10 in:
PT10<-read.csv("./COVID crisis Data-CSV/PT-10.csv")

##change the scwsds to inwyys
PT10$InterviewTime<-strptime(PT10$inwds, "%Y-%m-%d %H:%M:%S")
PT10$inwyys<-format(PT10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
PT10 <- PT10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
PT10 <- PT10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
PT9<-read.csv("./COVID crisis Data-CSV/PT-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
PT9 <- PT9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
PT9 <- PT9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##Bind both rounds data together:
PT_Round9to10<-rbind(PT9, PT10)

##To check which year we have data available:
table(PT_Round9to10$inwyys)

##subset for each year:
PT_2018<-PT_Round9to10 %>%
        filter(inwyys==2018)
PT_2019<-PT_Round9to10 %>%
        filter(inwyys==2019)
PT_2021<-PT_Round9to10 %>%
        filter(inwyys==2021)
PT_2022<-PT_Round9to10 %>%
        filter(inwyys==2022)

##Give the unemployment rate of the previous year to each year
PT_2018<-PT_2018 %>%
        mutate(UnemploymentRate=rep(0.089,times=87))
PT_2019<-PT_2019 %>%
        mutate(UnemploymentRate=rep(0.070,times=803))
PT_2021<-PT_2021 %>%
        mutate(UnemploymentRate=rep(0.068,times=1276))
PT_2022<-PT_2022 %>%
        mutate(UnemploymentRate=rep(0.066,times=340))

##Give the GDP growth of the previous year to each year
PT_2018<-PT_2018 %>%
        mutate(GDPGrowth=rep(0.038, times=87))
PT_2019<-PT_2019 %>%
        mutate(GDPGrowth=rep(0.030, times=803))
PT_2021<-PT_2021 %>%
        mutate(GDPGrowth=rep(-0.084, times=1276))
PT_2022<-PT_2022 %>%
        mutate(GDPGrowth=rep(0.051, times=340))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
PT_COVIDCrisis<-rbind(PT_2018, PT_2019, PT_2021, PT_2022)
PT_COVIDCrisis<-PT_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
PT_COVIDCrisis<-PT_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("PT_2018","PT_2019", "PT_2021", "PT_2022", "PT_Round9to10",
            "PT9","PT10"))





######--------------------------------------------------------------------------------------------------------
##Data management for Serbia
##Read RS Round 10 in:
RS10<-read.csv("./COVID crisis Data-CSV/RS-10.csv")

##change the scwsds to inwyys
RS10$InterviewTime<-strptime(RS10$scwass, "%Y-%m-%d %H:%M:%S")
RS10$inwyys<-format(RS10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
RS10 <- RS10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
RS10 <- RS10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
RS9<-read.csv("./COVID crisis Data-CSV/RS-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
RS9 <- RS9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
RS9 <- RS9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Bind both rounds data together:
RS_Round9to10<-rbind(RS9, RS10)

##To check which year we have data available:
table(RS_Round9to10$inwyys)

##subset for each year:
RS_2018<-RS_Round9to10 %>%
        filter(inwyys==2018)
RS_2019<-RS_Round9to10 %>%
        filter(inwyys==2019)
RS_2022<-RS_Round9to10 %>%
        filter(inwyys==2022)

##Give the unemployment rate of the previous year to each year
RS_2018<-RS_2018 %>%
        mutate(UnemploymentRate=rep(0.135,times=1491))
RS_2019<-RS_2019 %>%
        mutate(UnemploymentRate=rep(0.127,times=137))
RS_2022<-RS_2022 %>%
        mutate(UnemploymentRate=rep(0.101,times=663))

##Give the GDP growth of the previous year to each year
RS_2018<-RS_2018 %>%
        mutate(GDPGrowth=rep(0.026, times=1491))
RS_2019<-RS_2019 %>%
        mutate(GDPGrowth=rep(0.051, times=137))
RS_2022<-RS_2022 %>%
        mutate(GDPGrowth=rep(0.087, times=663))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
RS_COVIDCrisis<-rbind(RS_2018, RS_2019, RS_2022)
RS_COVIDCrisis<-RS_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
RS_COVIDCrisis<-RS_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("RS_2018", "RS_2019", "RS_2022", "RS_Round9to10",
            "RS9","RS10"))





######--------------------------------------------------------------------------------------------------------
##Data management for Slovakia
##Read SK Round 10 in:
SK10<-read.csv("./COVID crisis Data-CSV/SK-10.csv")

##change the scwsds to inwyys
SK10$InterviewTime<-strptime(SK10$inwds, "%Y-%m-%d %H:%M:%S")
SK10$inwyys<-format(SK10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
SK10 <- SK10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
SK10 <- SK10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
SK9<-read.csv("./COVID crisis Data-CSV/SK-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
SK9 <- SK9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
SK9 <- SK9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##Bind both rounds data together:
SK_Round9to10<-rbind(SK9, SK10)

##To check which year we have data available:
table(SK_Round9to10$inwyys)

##subset for each year:
SK_2019<-SK_Round9to10 %>%
        filter(inwyys==2019)
SK_2021<-SK_Round9to10 %>%
        filter(inwyys==2021)

##Give the unemployment rate of the previous year to each year
SK_2019<-SK_2019 %>%
        mutate(UnemploymentRate=rep(0.065,times=1013))
SK_2021<-SK_2021 %>%
        mutate(UnemploymentRate=rep(0.067,times=1357))

##Give the GDP growth of the previous year to each year
SK_2019<-SK_2019 %>%
        mutate(GDPGrowth=rep(0.039, times=1013))
SK_2021<-SK_2021 %>%
        mutate(GDPGrowth=rep(-0.034, times=1357))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
SK_COVIDCrisis<-rbind(SK_2019, SK_2021)
SK_COVIDCrisis<-SK_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
SK_COVIDCrisis<-SK_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("SK_2019", "SK_2021", "SK_Round9to10",
            "SK9","SK10"))






######--------------------------------------------------------------------------------------------------------
##Data management for Slovenia
##Read SI Round 10 in:
SI10<-read.csv("./COVID crisis Data-CSV/SI-10.csv")

##change the scwsds to inwyys
SI10$InterviewTime<-strptime(SI10$inwds, "%Y-%m-%d %H:%M:%S")
SI10$inwyys<-format(SI10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
SI10 <- SI10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
SI10 <- SI10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
SI9<-read.csv("./COVID crisis Data-CSV/SI-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
SI9 <- SI9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
SI9 <- SI9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##Bind both rounds data together:
SI_Round9to10<-rbind(SI9, SI10)

##To check which year we have data available:
table(SI_Round9to10$inwyys)

##subset for each year:
SI_2018<-SI_Round9to10 %>%
        filter(inwyys==2018)
SI_2019<-SI_Round9to10 %>%
        filter(inwyys==2019)
SI_2020<-SI_Round9to10 %>%
        filter(inwyys==2020)
SI_2021<-SI_Round9to10 %>%
        filter(inwyys==2021)


##Give the unemployment rate of the previous year to each year
SI_2018<-SI_2018 %>%
        mutate(UnemploymentRate=rep(0.066,times=1103))
SI_2019<-SI_2019 %>%
        mutate(UnemploymentRate=rep(0.051,times=39))
SI_2020<-SI_2020 %>%
        mutate(UnemploymentRate=rep(0.045,times=589))
SI_2021<-SI_2021 %>%
        mutate(UnemploymentRate=rep(0.050,times=525))


##Give the GDP growth of the previous year to each year
SI_2018<-SI_2018 %>%
        mutate(GDPGrowth=rep(0.047, times=1103))
SI_2019<-SI_2019 %>%
        mutate(GDPGrowth=rep(0.041, times=39))
SI_2020<-SI_2020 %>%
        mutate(GDPGrowth=rep(0.028, times=589))
SI_2021<-SI_2021 %>%
        mutate(GDPGrowth=rep(-0.049, times=525))


##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
SI_COVIDCrisis<-rbind(SI_2018, SI_2019, SI_2020, SI_2021)
SI_COVIDCrisis<-SI_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
SI_COVIDCrisis<-SI_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("SI_2018","SI_2019", "SI_2020", "SI_2021", "SI_Round9to10",
            "SI9","SI10"))





######--------------------------------------------------------------------------------------------------------
##Data management for Spain
##Read ES Round 10 in:
ES10<-read.csv("./COVID crisis Data-CSV/ES-10.csv")

##change the scwsds to inwyys
ES10$InterviewTime<-strptime(ES10$scwass, "%Y-%m-%d %H:%M:%S")
ES10$inwyys<-format(ES10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
ES10 <- ES10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
ES10 <- ES10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
ES9<-read.csv("./COVID crisis Data-CSV/ES-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
ES9 <- ES9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
ES9 <- ES9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Bind both rounds data together:
ES_Round9to10<-rbind(ES9, ES10)

##To check which year we have data available:
table(ES_Round9to10$inwyys)

##subset for each year:
ES_2019<-ES_Round9to10 %>%
        filter(inwyys==2019)
ES_2020<-ES_Round9to10 %>%
        filter(inwyys==2020)
ES_2022<-ES_Round9to10 %>%
        filter(inwyys==2022)

##Give the unemployment rate of the previous year to each year
ES_2019<-ES_2019 %>%
        mutate(UnemploymentRate=rep(0.153,times=1176))
ES_2020<-ES_2020 %>%
        mutate(UnemploymentRate=rep(0.141,times=240))
ES_2022<-ES_2022 %>%
        mutate(UnemploymentRate=rep(0.148,times=1535))

##Give the GDP growth of the previous year to each year
ES_2019<-ES_2019 %>%
        mutate(GDPGrowth=rep(0.018, times=1176))
ES_2020<-ES_2020 %>%
        mutate(GDPGrowth=rep(0.013, times=240))
ES_2022<-ES_2022 %>%
        mutate(GDPGrowth=rep(0.063, times=1535))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
ES_COVIDCrisis<-rbind(ES_2019, ES_2020, ES_2022)
ES_COVIDCrisis<-ES_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
ES_COVIDCrisis<-ES_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("ES_2019", "ES_2020", "ES_2022", "ES_Round9to10",
            "ES9","ES10"))





######--------------------------------------------------------------------------------------------------------
##Data management for Sweden
##Read SE Round 10 in:
SE10<-read.csv("./COVID crisis Data-CSV/SE-10.csv")

##change the scwsds to inwyys
SE10$InterviewTime<-strptime(SE10$scwass, "%Y-%m-%d %H:%M:%S")
SE10$inwyys<-format(SE10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
SE10 <- SE10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
SE10 <- SE10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
SE9<-read.csv("./COVID crisis Data-CSV/SE-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
SE9 <- SE9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
SE9 <- SE9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Bind both rounds data together:
SE_Round9to10<-rbind(SE9, SE10)

##To check which year we have data available:
table(SE_Round9to10$inwyys)

##subset for each year:
SE_2018<-SE_Round9to10 %>%
        filter(inwyys==2018)
SE_2019<-SE_Round9to10 %>%
        filter(inwyys==2019)
SE_2021<-SE_Round9to10 %>%
        filter(inwyys==2021)
SE_2022<-SE_Round9to10 %>%
        filter(inwyys==2022)

##Give the unemployment rate of the previous year to each year
SE_2018<-SE_2018 %>%
        mutate(UnemploymentRate=rep(0.067,times=870))
SE_2019<-SE_2019 %>%
        mutate(UnemploymentRate=rep(0.064,times=405))
SE_2021<-SE_2021 %>%
        mutate(UnemploymentRate=rep(0.083,times=1160))
SE_2022<-SE_2022 %>%
        mutate(UnemploymentRate=rep(0.087,times=10))

##Give the GDP growth of the previous year to each year
SE_2018<-SE_2018 %>%
        mutate(GDPGrowth=rep(0.012, times=870))
SE_2019<-SE_2019 %>%
        mutate(GDPGrowth=rep(0.008, times=405))
SE_2021<-SE_2021 %>%
        mutate(GDPGrowth=rep(-0.029, times=1160))
SE_2022<-SE_2022 %>%
        mutate(GDPGrowth=rep(0.055, times=10))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
SE_COVIDCrisis<-rbind(SE_2018, SE_2019, SE_2021, SE_2022)
SE_COVIDCrisis<-SE_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
SE_COVIDCrisis<-SE_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("SE_2018", "SE_2019", "SE_2021", "SE_2022", "SE_Round9to10",
            "SE9","SE10"))





######--------------------------------------------------------------------------------------------------------
##Data management for Switzerland
##Read CH Round 10 in:
CH10<-read.csv("./COVID crisis Data-CSV/CH-10.csv")

##change the scwsds to inwyys
CH10$InterviewTime<-strptime(CH10$inwds, "%Y-%m-%d %H:%M:%S")
CH10$inwyys<-format(CH10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
CH10 <- CH10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
CH10 <- CH10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
CH9<-read.csv("./COVID crisis Data-CSV/CH-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
CH9 <- CH9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
CH9 <- CH9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##Bind both rounds data together:
CH_Round9to10<-rbind(CH9, CH10)

##To check which year we have data available:
table(CH_Round9to10$inwyys)

##subset for each year:
CH_2018<-CH_Round9to10 %>%
        filter(inwyys==2018)
CH_2019<-CH_Round9to10 %>%
        filter(inwyys==2019)
CH_2021<-CH_Round9to10 %>%
        filter(inwyys==2021)
CH_2022<-CH_Round9to10 %>%
        filter(inwyys==2022)

##Give the unemployment rate of the previous year to each year
CH_2018<-CH_2018 %>%
        mutate(UnemploymentRate=rep(0.048,times=1000))
CH_2019<-CH_2019 %>%
        mutate(UnemploymentRate=rep(0.047,times=38))
CH_2021<-CH_2021 %>%
        mutate(UnemploymentRate=rep(0.048,times=791))
CH_2022<-CH_2022 %>%
        mutate(UnemploymentRate=rep(0.051,times=263))

##Give the GDP growth of the previous year to each year
CH_2018<-CH_2018 %>%
        mutate(GDPGrowth=rep(0.004, times=1000))
CH_2019<-CH_2019 %>%
        mutate(GDPGrowth=rep(0.021, times=38))
CH_2021<-CH_2021 %>%
        mutate(GDPGrowth=rep(-0.029, times=791))
CH_2022<-CH_2022 %>%
        mutate(GDPGrowth=rep(0.046, times=263))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
CH_COVIDCrisis<-rbind(CH_2018, CH_2019, CH_2021, CH_2022)
CH_COVIDCrisis<-CH_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
CH_COVIDCrisis<-CH_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("CH_2018","CH_2019", "CH_2021", "CH_2022", "CH_Round9to10",
            "CH9","CH10"))






######--------------------------------------------------------------------------------------------------------
##Data management for the UK
##Read GB Round 10 in:
GB10<-read.csv("./COVID crisis Data-CSV/GB-10.csv")

##change the scwsds to inwyys
GB10$InterviewTime<-strptime(GB10$inwds, "%Y-%m-%d %H:%M:%S")
GB10$inwyys<-format(GB10$InterviewTime, format="%Y")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group -- NOT in Round 10
GB10 <- GB10 %>%
        filter(brncntr==1 & ctzcntr==1)

##Select the necessary variables:
GB10 <- GB10 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)


##Read Round 9 in:
GB9<-read.csv("./COVID crisis Data-CSV/GB-9.csv")

##Subset the data to include only the respondents born in the country
##who is also citizen of the country 
##who does not belong to the ehtnic minority group
GB9 <- GB9 %>%
        filter(brncntr==1 & ctzcntr==1 & blgetmg==2)

##Select the necessary variables:
GB9 <- GB9 %>%
        select(idno,essround,cntry,imbgeco,imueclt,imwbcnt,gndr,agea,eduyrs,mnactic,hincfel,domicil,rlgdgr,lrscale,inwyys)

##Bind both rounds data together:
GB_Round9to10<-rbind(GB9, GB10)

##To check which year we have data available:
table(GB_Round9to10$inwyys)

##subset for each year:
GB_2018<-GB_Round9to10 %>%
        filter(inwyys==2018)
GB_2019<-GB_Round9to10 %>%
        filter(inwyys==2019)
GB_2021<-GB_Round9to10 %>%
        filter(inwyys==2021)
GB_2022<-GB_Round9to10 %>%
        filter(inwyys==2022)

##Give the unemployment rate of the previous year to each year
GB_2018<-GB_2018 %>%
        mutate(UnemploymentRate=rep(0.043,times=1607))
GB_2019<-GB_2019 %>%
        mutate(UnemploymentRate=rep(0.040,times=227))
GB_2021<-GB_2021 %>%
        mutate(UnemploymentRate=rep(0.045,times=540))
GB_2022<-GB_2022 %>%
        mutate(UnemploymentRate=rep(0.048,times=446))

##Give the GDP growth of the previous year to each year
GB_2018<-GB_2018 %>%
        mutate(GDPGrowth=rep(0.020, times=1607))
GB_2019<-GB_2019 %>%
        mutate(GDPGrowth=rep(0.008, times=227))
GB_2021<-GB_2021 %>%
        mutate(GDPGrowth=rep(-0.107, times=540))
GB_2022<-GB_2022 %>%
        mutate(GDPGrowth=rep(0.088, times=446))

##Bind the data together and give the mean of unemployment rate and GDP growth 
##across all time points in this country
GB_COVIDCrisis<-rbind(GB_2018, GB_2019, GB_2021, GB_2022)
GB_COVIDCrisis<-GB_COVIDCrisis %>%
        mutate(MeanUnemployRate=mean(UnemploymentRate))
GB_COVIDCrisis<-GB_COVIDCrisis %>%
        mutate(MeanGDPGrowth=mean(GDPGrowth))

rm(list = c("GB_2018","GB_2019", "GB_2021", "GB_2022", "GB_Round9to10",
            "GB9","GB10"))
