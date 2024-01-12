#GHG import-------------------------
g<-gt(df_ghgimp_market_wider) %>% 
  fmt_number(columns = c("Money_in_GDP_2015NDC_w/ET","Money_in_GDP_2020NDC_w/ET",
                         "Money_in_trade_2015NDC_w/ET","Money_in_trade_2020NDC_w/ET"),n_sigfig  = 2) %>% 
  fmt_number(columns = c("Money_2015NDC_w/ET","Money_2020NDC_w/ET"),n_sigfig = 3) %>% 
  tab_spanner(label = "Money (billion US 2005$)", columns = c("Money_2015NDC_w/ET","Money_2020NDC_w/ET")) %>% 
  tab_spanner(label = "Money in GDP", columns = c("Money_in_GDP_2015NDC_w/ET","Money_in_GDP_2020NDC_w/ET")) %>% 
  tab_spanner(label = "Money in trade", columns = c("Money_in_trade_2015NDC_w/ET","Money_in_trade_2020NDC_w/ET")) %>% 
  cols_label(region = md("Region"), 'Money_2015NDC_w/ET' = md("2015NDC_w/ET"), 'Money_2020NDC_w/ET' = md("2020NDC_w/ET"), 
             'Money_in_GDP_2015NDC_w/ET' = md("2015NDC_w/ET"), 'Money_in_GDP_2020NDC_w/ET' = md("2020NDC_w/ET"),
             'Money_in_trade_2015NDC_w/ET' = md("2015NDC_w/ET"), 'Money_in_trade_2020NDC_w/ET' = md("2020NDC_w/ET"))  %>%
  tab_options(table.width = pct(25),
              table_body.hlines.width = 0,
              column_labels.border.top.width = 2, 
              column_labels.border.top.color = "black", 
              column_labels.border.bottom.width = 2,
              column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black" ) %>% 
  cols_align(align = "center", columns = c("Money_2015NDC_w/ET","Money_2020NDC_w/ET","Money_in_GDP_2015NDC_w/ET",
                                           "Money_in_GDP_2020NDC_w/ET","Money_in_trade_2015NDC_w/ET","Money_in_trade_2020NDC_w/ET")) %>% 
  cols_align(align = "left", columns = c("region")) %>% 
  opt_table_font("Times New Roman")
gtsave(g,"../output/Emissions_import.docx")

g<-ggplot()+
  geom_bar(analysis_VA_loss,mapping=aes(x=Sector,y=Loss_dcp_gdp*100,fill=decele),stat = "identity")+
  geom_point(VA_loss,mapping=aes(x=Sector,y=Loss_dcp_gdp*100))+
  geom_abline(slope = 0,intercept = 0,linetype=2)+
  facet_grid(Region~SCENARIO,scales = "free_y")+
  ylab("Change in % of GDP(%)")+
  scale_fill_manual(values = c("Output Change"="grey","Value-added_Output ratio"="orange","Residual"="purple"))+
  theme_1+
  theme(axis.text.x = element_text(angle = 45,size=14,hjust=1,vjust=1))+
  guides(shape = guide_legend(ncol = 2,byrow=TRUE))
plot(g)
ggsave('../output/Value_added_dcp.png',plot = g,width = 13,height = 10,dpi=300)
ggsave('../output/Value_added_dcp.svg',plot = g,width = 13,height = 10,dpi=300)

g<-ggplot()+
  geom_bar(filter(df_GDP_s_diff,category!="Total"),mapping=aes(x=SCENARIO,y=bau_percent*100,fill=category),stat = "identity")+
  geom_point(filter(df_GDP_s_diff,category=="Total"),mapping=aes(x=SCENARIO,y=bau_percent*100))+
  geom_abline(slope = 0,intercept = 0,linetype=2)+
  facet_wrap(~region,ncol=4)+
  ylab("GDP change rates(%)")+
  scale_fill_manual(values = c("Consumption"="grey","Export"="orange","Import"="purple"))+
  theme_1+
  theme(axis.text.x = element_text(angle = 45,size=14,hjust=1,vjust=1))+
  theme(panel.spacing.x = unit(3, "lines"))
plot(g)
ggsave('../output/GDP_dcp.png',plot = g,width = 14.5,height = 10,dpi=300)
ggsave('../output/GDP_dcp.svg',plot = g,width = 14.5,height = 10,dpi=300)
