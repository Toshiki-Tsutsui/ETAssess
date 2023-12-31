#GHG emissions trajectory--------------------------------------------------
iamc_GHG<-filter(iamc,VARIABLE %in% c("Emissions|Kyoto Gases"))%>% 
  filter(SCENARIO %in% c("Baseline","INDC_w/oET","2020NDC_w/oET")) %>% 
  filter(year %in% c(2015,2020,2025,2030)) %>% 
  filter((SCENARIO!="Baseline" & year %in% c(2020,2025,2030))|(SCENARIO=="Baseline")) %>% 
  mutate(SCENARIO=recode(SCENARIO,"Baseline"="Baseline",
                         "INDC_w/oET"="INDC",
                         "2020NDC_w/oET"="2020NDC")) %>% 
  aggregate(value~year+SCENARIO+VARIABLE+UNIT+OECD_category,sum)
iamc_GHG$OECD_category<-factor(iamc_GHG$OECD_category,
                               levels = c("World","OECD","Non-OECD"))
iamc_GHG$SCENARIO<-factor(iamc_GHG$SCENARIO,
                          levels = c("Baseline","INDC","2020NDC"))
g1<-ggplot()+
  geom_line(iamc_GHG,mapping = aes(x=year,y=value/1000,group=SCENARIO,color=SCENARIO),linewidth=1)+
  ylab(bquote('GHG emissions (Gt'~CO[2]~'eq/yr)'))+
  facet_wrap(~OECD_category,scales = "free_y")+
  ylim(c(5,NA))+
  scale_color_manual(values = c("blue","orange","red"),labels=c(Baseline="Baseline",INDC="INDC",'2020NDC'="2020NDC"))+
  theme_1+
  theme(axis.text.x = element_text(angle = 45,size=16,hjust=1,vjust=1))
plot(g1)
ggsave("../output/GHG_emission_timescale.png",plot = g1,width = 10,height = 5,dpi=300)


#GHG absolute emissions-------------------------------------------------------------------------
iamc_GHG_region<-filter(iamc,VARIABLE %in% c("Emissions|Kyoto Gases"))%>% 
  filter(SCENARIO %in% c("Baseline","INDC_w/oET","INDC_w/ET","2020NDC_w/oET","2020NDC_w/ET")) %>% 
  filter(year %in% c(2030)) %>% 
  mutate(value=value/1000)
iamc_GHG_region$SCENARIO<-factor(iamc_GHG_region$SCENARIO,
                          levels = c("Baseline","INDC_w/oET","INDC_w/ET","2020NDC_w/oET","2020NDC_w/ET"))
iamc_GHG_region_wider<-iamc_GHG_region %>% 
  select(SCENARIO,REGION,value) %>% 
  pivot_wider(names_from = "SCENARIO",values_from = value) %>% 
  mutate(REGION=recode(REGION,"WLD"="World","JPN"="Japan","USA"="USA","CAN"="Canada","XE25"="EU25",
                       "XOC"="Oceania","TUR"="Turkey","XER"="Rest of Europe",
                       "CIS"="Former Soviet Union","CHN"="China","IND"="India",
                       "XSE"="Southeast Asia","XSA"="Rest of Asia","BRA"="Brazil",
                       "XLM"="Rest of South America","XME"="Middle East",
                       "XNF"="North Africa","XAF"="Rest of Africa","World"="World")) %>% 
  mutate(REGION=factor(REGION,levels=c("Japan","USA","Canada","EU25","Oceania","Turkey","Rest of Europe","Former Soviet Union","China","India",
                                       "Southeast Asia","Rest of Asia","Brazil","Rest of South America","Middle East","North Africa","Rest of Africa","World"))) %>% 
  arrange(REGION) %>% 
  select(REGION,Baseline,'INDC_w/oET','INDC_w/ET','2020NDC_w/oET','2020NDC_w/ET')

g<-gt(iamc_GHG_region_wider) %>% 
  fmt_number(columns = c("Baseline","INDC_w/oET","INDC_w/ET","2020NDC_w/oET","2020NDC_w/ET"),n_sigfig  = 3) %>% 
  tab_options(table.width = pct(25),
              table_body.hlines.width = 0,
              column_labels.border.top.width = 2, 
              column_labels.border.top.color = "black", 
              column_labels.border.bottom.width = 2,
              column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black" ) %>% 
  cols_align(align = "center", columns = c("Baseline","INDC_w/oET","INDC_w/ET","2020NDC_w/oET","2020NDC_w/ET")) %>% 
  cols_align(align = "left", columns = c("REGION")) %>% 
  opt_table_font("Times New Roman")
gtsave(g,"../output/GHG_region.docx")


#GHG reduction rates relative to baseline-----------------------
iamc_GHG_diff<-filter(iamc_diff,VARIABLE=="Emissions|Kyoto Gases") %>% 
  filter(year==2030) %>% 
  select(-value,-baseline)
iamc_GHG_diff$REGION<-factor(iamc_GHG_diff$REGION,levels = country_code$country)
iamc_GHG_diff$SCENARIO<-factor(iamc_GHG_diff$SCENARIO,levels =c("INDC_w/oET","INDC_w/ET","2020NDC_w/oET","2020NDC_w/ET"))

g2<-ggplot()+
  geom_point(iamc_GHG_diff,mapping=aes(x=REGION,y=bau_percent,shape=SCENARIO,color=SCENARIO),size=3,stroke=1.3)+
  scale_fill_discrete()+
  scale_shape_manual(values = c(`INDC_w/oET`=16,`INDC_w/ET`=4,`2020NDC_w/oET`=16,`2020NDC_w/ET`=4))+
  scale_color_manual(values = c(`INDC_w/oET`="orange",`INDC_w/ET`="orange",`2020NDC_w/oET`="red",`2020NDC_w/ET`="red"))+
  theme_1+
  ylab("GHG emissions reduction rates (%)")
plot(g2)
ggsave('../output/GHG_reduction.png',plot = g2,width = 8,height = 6,dpi=300)


#carbon price---------------------------------------------------
iamc_carbonP<-filter(iamc,VARIABLE=="Price|Carbon") %>% 
  filter(SCENARIO!="Baseline") %>% 
  pivot_wider(names_from = "SCENARIO",values_from = "value",values_fill=0) %>% 
  filter(REGION!="World") %>% 
  filter(year==2030)
iamc_carbonP_longer<-iamc_carbonP %>% 
  pivot_longer(cols=-c("REGION","VARIABLE","UNIT","year","OECD_category"),
               names_to="SCENARIO",values_to="value") %>% 
  filter(SCENARIO %in% c("INDC_w/oET","INDC_w/ET","2020NDC_w/oET","2020NDC_w/ET"))
iamc_carbonP_longer$REGION<-factor(iamc_carbonP_longer$REGION,levels = country_code$country)
iamc_carbonP_longer$SCENARIO<-factor(iamc_carbonP_longer$SCENARIO,levels = scenario_name$scenario)
g<-ggplot()+
  geom_point(iamc_carbonP_longer,mapping=aes(x=REGION,y=value,shape=SCENARIO,color=SCENARIO),size=3,stroke=1.3)+
  ylab(bquote('Carbon Price (US$2010/t'~CO[2]~')'))+
  scale_shape_manual(values = c(`INDC_w/oET`=16,`INDC_w/ET`=4,`2020NDC_w/oET`=16,`2020NDC_w/ET`=4))+
  scale_color_manual(values = c(`INDC_w/oET`="orange",`INDC_w/ET`="orange",`2020NDC_w/oET`="red",`2020NDC_w/ET`="red"))+
  theme_1
plot(g)
ggsave('../output/carbon_price.png',plot = g,width = 8,height = 6,dpi=300)


#GDP loss------------------------------------------------------
iamc_gdp<-filter(iamc,VARIABLE=="Policy Cost|GDP Loss rate") %>% 
  filter(SCENARIO %in% c("INDC_w/oET","INDC_w/ET","2020NDC_w/oET","2020NDC_w/ET")) %>% 
  filter(year==2030)

g4<-ggplot()+
  geom_point(iamc_gdp,
             mapping=aes(x=REGION,y=value,shape=SCENARIO,color=SCENARIO),size=3,stroke=1.3)+
  ylab("GDP loss rate(%)")+
  scale_shape_manual(values = c(`INDC_w/oET`=16,`INDC_w/ET`=4,`2020NDC_w/oET`=16,`2020NDC_w/ET`=4))+
  scale_color_manual(values = c(`INDC_w/oET`="orange",`INDC_w/ET`="orange",`2020NDC_w/oET`="red",`2020NDC_w/ET`="red"))+
  theme_1
plot(g4)
ggsave('../output/gdp.png',plot = g4,width = 8,height = 6,dpi=300)


#Primary Energy-----------------------------------------------------------
iamc_PE<-filter(iamc,VARIABLE %in% c("Primary Energy|Biomass|w/o CCS","Primary Energy|Gas|w/ CCS",
                                 "Primary Energy|Biomass|w/ CCS","Primary Energy|Nuclear",
                                 "Primary Energy|Other","Primary Energy|Coal|w/o CCS",
                                 "Primary Energy|Gas|w/o CCS","Primary Energy|Geothermal",
                                 "Primary Energy|Hydro","Primary Energy|Oil|w/o CCS",
                                 "Primary Energy|Solar","Primary Energy|Wind",
                                 "Primary Energy|Coal|w/ CCS","Primary Energy|Oil|w/ CCS"))%>% 
  filter(SCENARIO %in% c("2020NDC_w/oET","2020NDC_w/ET","INDC_w/oET","INDC_w/ET","Baseline")) %>% 
  filter(year==2030) %>% 
  aggregate(value~SCENARIO+VARIABLE+year+UNIT+OECD_category,sum)
iamc_PE$VARIABLE<-str_remove_all(iamc_PE$VARIABLE,pattern = "Primary Energy\\|") %>% 
  factor(levels=c("Other","Geothermal","Biomass|w/ CCS","Biomass|w/o CCS",
                  "Wind","Solar","Nuclear","Hydro","Gas|w/ CCS",
                  "Gas|w/o CCS","Oil|w/ CCS","Oil|w/o CCS","Coal|w/ CCS","Coal|w/o CCS" ))
iamc_PE$OECD_category<-factor(iamc_PE$OECD_category,levels = c("World","OECD","Non-OECD"))

g3<-ggplot(data=iamc_PE)+
  geom_bar(iamc_PE,mapping=aes(x=SCENARIO,y=value,fill=VARIABLE),
           stat = "identity",width = 0.7)+
  ylab("Primary Energy(EJ/yr)")+
  facet_wrap(~OECD_category,ncol=4,scales = "free_y")+
  scale_fill_manual(values = palette_PE)+
  guides(fill = guide_legend(reverse = F))+
  theme_2
plot(g3)
ggsave('../output/Primary_Engergy.png',plot=g3,width=10,height=5,dpi=300)


#Final Energy-----------------------------------------------------------
iamc_FE<-filter(iamc,VARIABLE %in% c("Final Energy|Electricity","Final Energy|Gases",
                                     "Final Energy|Heat","Final Energy|Hydrogen",
                                     "Final Energy|Liquids","Final Energy|Solids"))%>% 
  filter(SCENARIO %in% c("2020NDC_w/oET","2020NDC_w/ET","INDC_w/oET","INDC_w/ET","Baseline")) %>% 
  filter(year==2030) %>% 
  aggregate(value~SCENARIO+VARIABLE+year+UNIT+OECD_category,sum)
iamc_FE$VARIABLE<-str_remove_all(iamc_FE$VARIABLE,pattern = "Final Energy\\|") %>% 
  factor(levels=c("Electricity","Gases","Heat","Hydrogen" ,"Liquids","Solids"))
iamc_FE$OECD_category<-factor(iamc_FE$OECD_category,levels = c("World","OECD","Non-OECD"))

g3<-ggplot(data=iamc_FE)+
  geom_bar(iamc_FE,mapping=aes(x=SCENARIO,y=value,fill=VARIABLE),
           stat = "identity",width = 0.7)+
  ylab("Final Energy(EJ/yr)")+
  facet_wrap(~OECD_category,scales = "free_y")+
  scale_fill_manual(values = c("gold","skyblue","red","navy","grey","chocolate"))+
  theme_2
plot(g3)
ggsave('../output/Final_Engergy_fuel.png',plot=g3,width=10,height=5,dpi=300)

iamc_FE<-filter(iamc,VARIABLE %in% c("Final Energy|Industry","Final Energy|Other Sector",
                                     "Final Energy|Residential","Final Energy|Commercial",
                                     "Final Energy|Transportation","Final Energy|Non-Energy Use"))%>% 
  filter(SCENARIO %in% c("2020NDC_w/oET","2020NDC_w/ET","INDC_w/oET","INDC_w/ET","Baseline")) %>% 
  filter(year==2030) %>% 
  aggregate(value~year+SCENARIO+VARIABLE+UNIT+OECD_category,sum) 
iamc_FE$VARIABLE<-str_remove_all(iamc_FE$VARIABLE,pattern = "Final Energy\\|") %>% 
  factor(levels=c("Other Sector","Non-Energy Use","Commercial","Residential",
                  "Transportation","Industry" ))
iamc_FE$OECD_category<-factor(iamc_FE$OECD_category,levels = c("World","OECD","Non-OECD"))
g3<-ggplot(data=iamc_FE)+
  geom_bar(iamc_FE,mapping=aes(x=SCENARIO,y=value,fill=VARIABLE),
           stat = "identity",width = 0.7)+
  ylab("Final Energy(EJ/yr)")+
  facet_wrap(~OECD_category,scales = "free_y")+
  scale_fill_manual(values = c("gold","skyblue","red","navy","grey","chocolate"))+
  theme_2
plot(g3)
ggsave('../output/Final_Engergy_sector.png',plot=g3,width=10,height=5,dpi=300)


#powergeneration------------------------------------------
iamc_PG<-filter(iamc,VARIABLE %in% c("Secondary Energy|Electricity|Biomass|w/o CCS","Secondary Energy|Electricity|Gas|w/ CCS",
                                     "Secondary Energy|Electricity|Biomass|w/ CCS","Secondary Energy|Electricity|Nuclear",
                                     "Secondary Energy|Electricity|Other","Secondary Energy|Electricity|Coal|w/o CCS",
                                     "Secondary Energy|Electricity|Gas|w/o CCS","Secondary Energy|Electricity|Geothermal",
                                     "Secondary Energy|Electricity|Hydro","Secondary Energy|Electricity|Oil|w/o CCS",
                                     "Secondary Energy|Electricity|Solar","Secondary Energy|Electricity|Wind",
                                     "Secondary Energy|Electricity|Coal|w/ CCS","Secondary Energy|Electricity|Oil|w/ CCS"))%>% 
  filter(SCENARIO %in% c("2020NDC_w/oET","2020NDC_w/ET","INDC_w/oET","INDC_w/ET","Baseline")) %>% 
  filter(year==2030) %>% 
  aggregate(value~year+SCENARIO+VARIABLE+UNIT+OECD_category,sum) 
iamc_PG$VARIABLE<-str_remove_all(iamc_PG$VARIABLE,pattern = "Secondary Energy\\|Electricity\\|") %>% 
  factor(levels=c("Other","Geothermal","Biomass|w/ CCS","Biomass|w/o CCS",
                  "Wind","Solar","Nuclear","Hydro","Gas|w/ CCS",
                  "Gas|w/o CCS","Oil|w/ CCS","Oil|w/o CCS","Coal|w/ CCS","Coal|w/o CCS" ))
iamc_PG$OECD_category<-factor(iamc_PG$OECD_category,levels = c("World","OECD","Non-OECD"))

g<-ggplot(data=iamc_PG)+
  geom_bar(iamc_PG,mapping=aes(x=SCENARIO,y=value,fill=VARIABLE),
           stat = "identity",width = 0.7)+
  ylab("Power Generation(EJ/yr)")+
  facet_wrap(~OECD_category,ncol=4,scales = "free_y")+
  scale_fill_manual(values = palette_PE)+
  guides(fill = guide_legend(reverse = TRUE))+
  theme_2
plot(g)
ggsave('../output/Power_Generation.png',plot=g,width=10,height=5,dpi=300)

