#file and parameter search with regular expressions---------------------------------------------
#-----------------------------------------------------------------------------------------------
#param_search         search parameter names by regular expressions

#data import function------------------------------------------------------------------------------------

get_param<-function(param_search){
    param<-rgdx.param("../data/global/analysis.gdx",param_search)
    for (li_sce in 1:length(scenario_name$scenario)) {
      param$SCENARIO<-str_replace(param$SCENARIO,pattern = paste(scenario_name[li_sce,1]),
                                  replacement =paste(scenario_name[li_sce,2])) 
  }
  return(param)
}

#make dataframe------------------------------------------------------
df_GDP0<-get_param("GDP")
df_pbop0<-get_param("PBOP")
df_GDP_s0<-get_param("GDP_s")
df_Loss_dcp_gdp0<-get_param("Loss_dcp_gdp")

#balance of payment---------------------------------------------
df_GDP_17<-rename(df_GDP0,year=Y,region=R) %>% 
  filter(year==2030) %>% 
  filter(region %in% c(cge17)) %>% 
  filter(SCENARIO %in% c("INDC_w/ET","2020NDC_w/ET")) 
df_trade<-rename(df_pbop0,year=Y,region=R,category=.i4,trade=PBOP) %>% 
  filter(year==2030) %>% 
  filter(SCENARIO %in% c("INDC_w/ET","2020NDC_w/ET")) %>% 
  filter(region %in% cge17) %>% 
  filter(category=="trade") %>% 
  select(-category)
df_exp<-rename(df_GDP_s0,year=Y,region=R,category=INS_MCR,export=GDP_s) %>% 
  filter(year==2030) %>% 
  filter(SCENARIO %in% c("INDC_w/ET","2020NDC_w/ET")) %>% 
  filter(region %in% cge17) %>% 
  filter(category=="ROW") %>% 
  select(-category)
df_imp<-rename(df_GDP_s0,year=Y,region=R,category=INS_MCR,import=GDP_s) %>% 
  filter(year==2030) %>% 
  filter(SCENARIO %in% c("INDC_w/ET","2020NDC_w/ET")) %>% 
  filter(region %in% cge17) %>% 
  filter(category=="IMP") %>% 
  select(-category)


df_ghgimp_market<-rename(df_pbop0,year=Y,region=R,category=.i4,value=PBOP) %>% 
  filter(year==2030) %>% 
  filter(SCENARIO %in% c("INDC_w/ET","2020NDC_w/ET")) %>% 
  filter(region %in% cge17) %>% 
  filter(category=="GHGC") %>% 
  select(-category) %>% 
  rename(Money=value) %>% 
  left_join(df_GDP_17) %>% 
  mutate(Money_in_GDP=Money/GDP*100) %>% 
  left_join(df_imp) %>%
  left_join(df_exp) %>% 
  mutate(Money_in_trade=100*Money/mean(abs(import)+abs(export))) %>% 
  select(-c(GDP,import,export)) %>% 
  mutate(Money=Money/1000) %>% 
  mutate(region=factor(region,levels=cge17))

df_ghgimp_market_wider<-df_ghgimp_market %>% 
  pivot_wider(names_from = "SCENARIO",values_from = c("Money","Money_in_GDP","Money_in_trade")) %>% 
  select(-year) %>% 
  mutate(region=recode(region,"WLD"="World","JPN"="Japan","USA"="USA","CAN"="Canada","XE25"="EU25",
                       "XOC"="Oceania","TUR"="Turkey","XER"="Rest of Europe",
                       "CIS"="Former Soviet Union","CHN"="China","IND"="India",
                       "XSE"="Southeast Asia","XSA"="Rest of Asia","BRA"="Brazil",
                       "XLM"="Rest of South America","XME"="Middle East",
                       "XNF"="North Africa","XAF"="Rest of Africa")) %>% 
  mutate(region=factor(region,levels=c("Japan","USA","Canada","EU25","Oceania","Turkey","Rest of Europe","Former Soviet Union","China","India",
                                       "Southeast Asia","Rest of Asia","Brazil","Rest of South America","Middle East","North Africa","Rest of Africa"))) %>% 
  arrange(region)

filter(df_ghgimp_market_wider,`Money_2020NDC_w/ET`>=0) %>%
  filter(region!="WLD") %>% 
  select(`Money_2020NDC_w/ET`) %>% 
  sum()
filter(df_ghgimp_market_wider,`Money_INDC_w/ET`>=0) %>%
  filter(region!="WLD") %>% 
  select(`Money_INDC_w/ET`) %>% 
  sum()

#-----------------------------------------------------------------------------------------------
df_Loss_dcp_gdp<-rename(df_Loss_dcp_gdp0,"Region"="R","Sector"="SCO2_S","Year"="Y") %>% 
  filter(Region %in% c("World","R5OECD90+EU","Non-OECD")) %>% 
  filter(Year %in% c(2030)) 

VA_loss<-filter(df_Loss_dcp_gdp,decele %in% c("va")) %>%
  mutate(decele=recode(decele,"va"="Value_added")) %>% 
  mutate(SCENARIO=factor(SCENARIO,levels=c("INDC_w/oET","INDC_w/ET","2020NDC_w/oET","2020NDC_w/ET"))) %>% 
  mutate(Region=recode(Region,"World"="World","R5OECD90+EU"="OECD","Non-OECD"="Non-OECD")) %>% 
  mutate(Region=factor(Region,levels=c("World","OECD","Non-OECD"))) %>% 
  filter(Sector!="OTH")
analysis_VA_loss<-filter(df_Loss_dcp_gdp,decele %in% c("output","residual2","va_output")) %>% 
  mutate(decele=recode(decele,"output"="Output Change","residual2"="Residual","va_output"="Value-added_Output ratio")) %>% 
  mutate(decele=factor(decele,levels=c("Output Change","Value-added_Output ratio","Residual"))) %>% 
  mutate(SCENARIO=factor(SCENARIO,levels=c("INDC_w/oET","INDC_w/ET","2020NDC_w/oET","2020NDC_w/ET"))) %>% 
  mutate(Sector=factor(Sector,levels=c("BIO","SER","TRS","IND","PWR","OEN","AGR","FFE","OTH"))) %>% 
  mutate(Region=recode(Region,"World"="World","R5OECD90+EU"="OECD","Non-OECD"="Non-OECD")) %>% 
  mutate(Region=factor(Region,levels=c("World","OECD","Non-OECD"))) %>% 
  filter(Sector!="OTH")
