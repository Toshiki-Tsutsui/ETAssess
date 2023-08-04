#data import-----------
iamc<-read_csv("../data/iiasa_database/txt/global_17_IAMC.csv",col_names=TRUE,
               col_types = cols(SCENARIO=col_character(),
                                REGION=col_character(),
                                VARIABLE=col_character(),
                                UNIT=col_character()))
iamc$MODEL<-NULL
iamc<-pivot_longer(iamc,cols = c('2010','2015','2020','2025','2030'),
                   names_to = "year",values_to = "value")

#---------------------------------------

#change scenario and country name-----------------

iamc<-iamc %>% mutate(OECD_category=recode(REGION,"WLD"="World","JPN"="OECD","USA"="OECD","CAN"="OECD","XE25"="OECD",
                                           "XOC"="OECD","TUR"="OECD","XER"="OECD",
                                           "CIS"="Non-OECD","CHN"="Non-OECD","IND"="Non-OECD",
                                           "XSE"="Non-OECD","XSA"="Non-OECD","BRA"="Non-OECD",
                                           "XLM"="Non-OECD","XME"="Non-OECD",
                                           "XNF"="Non-OECD","XAF"="Non-OECD"))
for (li_sce in 1:length(scenario_name$scenario)) {
  iamc$SCENARIO<-str_replace(iamc$SCENARIO,pattern = paste(scenario_name[li_sce,1]),
                             replacement =paste(scenario_name[li_sce,2])) 
}
for (li_con in 1:length(country_code$country)) {
  iamc$REGION<-str_replace(iamc$REGION,pattern = paste(country_code[li_con,1]),
                             replacement =paste(country_code[li_con,2])) 
}
iamc$SCENARIO<-factor(iamc$SCENARIO,levels = scenario_name$scenario)
iamc$REGION<-factor(iamc$REGION,levels = country_code$country)
iamc$OECD_category<-factor(iamc$OECD_category,levels = c("OECD","Non-OECD","World"))

#--------------------------------------------

#make data to compare baseline scenario------------------

iamc_diff<-filter(iamc,SCENARIO=="Baseline") %>% 
  rename(baseline=value) %>% 
  select(REGION,VARIABLE,UNIT,year,baseline) %>% 
  right_join(iamc,by=c("REGION"="REGION","VARIABLE"="VARIABLE","UNIT"="UNIT","year"="year")) %>% 
  filter(SCENARIO!="Baseline") %>% 
  mutate(bau_percent=(1-value/baseline)*100)
iamc_diff
iamc_diff$SCENARIO<-factor(iamc_diff$SCENARIO,levels = scenario_name$scenario)
#------------------------------------------------

#remove unnecessary variables--------
rm(li_con,li_sce)

#----------------