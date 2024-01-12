#packages----------------
install.packages("tidyverse",dependencies = TRUE)
install.packages("rlist",dependencies = TRUE)
install.packages("pipeR",dependencies = TRUE)
install.packages("devtools",dependencies = TRUE)
install.packages("gt",dependencies = TRUE)
install.packages("webshot2",dependencies = TRUE)
install.packages("svglite",dependencies = TRUE)
library(tidyverse)
library(rlist)
library(pipeR)
library(devtools)
library(gt)
library(webshot2)
library(svglite)

install_github("GAMS-dev/gdxrrw/gdxrrw@v1.0.10")
library(gdxrrw)

#make folder-----------------------------
if(!dir.exists("../output")){
  dir.create("../output")
}

#define----------------------------------
cge17<-c("WLD","JPN","USA","CAN","XE25","XOC","TUR","XER",
         "CIS","CHN","IND","XSE","XSA","BRA","XLM","XME","XNF","XAF")
cge17_code<-c("WLD"="World","JPN"="Japan","USA"="USA","CAN"="Canada","XE25"="EU25",
              "XOC"="Oceania","TUR"="Turkey","XER"="Rest of Europe",
              "CIS"="Former Soviet Union","CHN"="China","IND"="India",
              "XSE"="Southeast Asia","XSA"="Rest of Asia","BRA"="Brazil",
              "XLM"="Rest of South America","XME"="Middle East",
              "XNF"="North Africa","XAF"="Rest of Africa")
country_code<-read.csv("../define/country_code.csv")
scenario_name<-read.csv("../define/scenario_name.csv")

#theme--------------------
theme_1<-  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text =element_text(size=16),axis.text.x = element_text(angle = 45,size=16,hjust=1,vjust=1),
        legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        strip.background = element_blank(),
        plot.title=element_text(hjust=0))

theme_2<-theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text =element_text(size=16),axis.text.x = element_text(angle=45,size=16,hjust=1,vjust=1),
        legend.position = "right",
        axis.title.x=element_blank(),
        legend.title = element_blank(),
        strip.background = element_blank(),
        plot.title=element_text(hjust=0))

palette_PE <- c("Coal|w/o CCS"="#000000","Coal|w/ CCS"="#7f878f","Oil|w/o CCS"="#ff2800",
                "Oil|w/ CCS"="#ffd1d1","Gas|w/o CCS"="#9a0079","Gas|w/ CCS"="#c7b2de",
                "Hydro"="#0041ff","Nuclear"="#663300","Solar"="#b4ebfa","Wind"="#ff9900",
                "Biomass|w/o CCS"="#35a16b","Biomass|w/ CCS"="#cbf266","Geothermal"="#edc58f",
                "Other"="#ffff99")

#run---------------------------
source("./make_data_gdx.R",echo=TRUE)
source("./make_plot_gdx.R",echo=TRUE)
source("./make_data_iiasa.R",echo=TRUE)
source("./make_plot_iiasa.R",echo=TRUE)
warnings()