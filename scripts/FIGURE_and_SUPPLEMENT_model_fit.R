rm(list = ls())
require(toolboxH)
require(ggplot2)
require(ggthemes)
require(scales)
require(lubridate)
require(here)
require(plotly)
require(patchwork)
initializeSkript()

source(here("scripts/functions_model_240112.R"))

maxdate_diagrams = as_date('2022-09-30')


############################################################################################################.# ## # # LOAD  =====
############################################################################################################.        

federal_countryinfo = fread(here("data/FIGURE_and_SUPPLEMENT_general/federal_countries.csv"))

## # load reported dat ####

epidata = fread(here("data/FIGURE_and_SUPPLEMENT_general/s1030_2_datint_ecdc_DE_BL_2023-03-26_v5_agestrat.txt"), dec = ",") # from RKI  https://github.com/robert-koch-institut/
epidata[, region:= federal_countryinfo[match_hk(epidata$CountryExp, federal_countryinfo$dataname), region]]


## # load variant frequencies ####
vocdata = fread(here("data/FIGURE_and_SUPPLEMENT_general/s245_1_simplified_coronavariants_DE_until2023-11-08.txt")) # RKI and ECDC via https://github.com/robert-koch-institut/SARS-CoV-2-Sequenzdaten_aus_Deutschland/ and https://www.ecdc.europa.eu/sites/default/files/documents/PathogenVariant_public_mappings.csv
vocdata[,.N, .(LINEAGE_model)]

vocdata2 = vocdata[LINEAGE_model %in% c("Alpha", "Delta", "BA.1", "BA.2", "BA.4+BA.5"), .(date = as_date(DateRep),
                                                                                          proportion = proz7d,LINEAGE_model)]
vocdata2[, variant2 := ifelse(LINEAGE_model=="Alpha", "alpha",
                              ifelse(LINEAGE_model=="Delta", "delta",
                                     ifelse(LINEAGE_model=="BA.1", "BA1",
                                            ifelse(LINEAGE_model=="BA.2", "BA2",
                                                   ifelse(LINEAGE_model=="BA.4+BA.5", "BA4+5",LINEAGE_model)))))]
vocdata2[, variant2b:= factor(variant2, levels = c("WT","alpha", "delta", "BA1", "BA2", "BA4+5"))]

datesVariantsEmerging = vocdata2[proportion>0.05 & date>as_date("2020-12-01") &proportion<0.7 , .(date, proportion, variant2)][duplicated(variant2)==F][order(date)]
datesVariantsEmerging


# ## integrating function-----
integrate_tensor_data = function(myfn, myepidata, datesVariantsEmerging, region, maxdate = maxdate_diagrams) {
  # myfn =  paste0(here("data/FIGURE_and_SUPPLEMENT_general/", todo$tensor_fn))[1]
  # myepidata = epidata[region == todo[tensor_fn ==todo$tensor_fn[1], region]]
  message("Working on simulation ", myfn, " ========================================================================")
  resi = c()
  resi$tensor =     load_obj(myfn)
  resi$region = region

  
  # epidate data wrangling
  resi$epidata = myepidata[DateRep <= maxdate]
  resi$inzdata = melt(resi$epidata, id.vars = c("DateRep", "Altersgruppe"), measure.vars = c("NewConfCases7","NewNormalStation7", "covid_inICU_upscaled", 'AllDeaths'))[DateRep <= maxdate]
  resi$inzdata[, outcome:= ifelse(variable =='NewConfCases7', "reported",
                                  ifelse(variable =='NewNormalStation7', "Hospital",
                                         ifelse(variable =="covid_inICU_upscaled", "ICU",
                                                ifelse(variable =="AllDeaths", "died", variable))))]
  
  resi$inzdata[, .N, .(variable,outcome)]
  
  resi$inzdata[,type := 'reported']
  
  
  resi$data = extractTensordata(resi$tensor, firstdate = as_date("2020-03-04"), scenarioname = resi$region, maxdate = maxdate)
  
  
  resi$datesVariant5proz_model = resi$data$variantdata[dataVariant_anteil>0.05 , .(date=datum, proportion=dataVariant_anteil, variant2)][duplicated(variant2)==F][order(date)][variant2!="WT"]
  
  resi$datesVariant5proz_model[, date_5proz_reported := datesVariantsEmerging[match_hk(resi$datesVariant5proz_model$variant2, datesVariantsEmerging$variant2),date]]
  resi$datesVariant5proz_model
  
  # # modelfit ----
  resi$plotdat_modelfit = rbind(
    resi$data$outputlayerdata[variable %in% c("daily_Total", "daily_Normal",'daily_Critical', 'cumul_Death') ,
                              
                              .(datum,
                                agegroup2,
                                outcome,
                                value,
                                type = 'modelled'
                              )
    ],
    resi$inzdata[,.(datum = as_date(DateRep),
                    agegroup2 = Altersgruppe,
                    outcome,
                    value,
                    type)]
  )
   
  
  resi$plotdat_modelfit[, outcome := factor(outcome, levels = c("reported","Hospital", "ICU", "died"))]
  
  resi
}


############################################################################################################.
# ## plan TODOS -----
############################################################################################################.

todo = data.table(tensor_fn = dir(here("data/FIGURE_and_SUPPLEMENT_general/"), pattern = "Simulations"))

todo[, id := tensor_fn %>% str_replace("SimulationsLand_", "") %>% str_split( "_|[A-Z]") %>% sapply("[", 1)]

todo[, region := federal_countryinfo[match_hk(id, federal_countryinfo$ID), region]]


all_integrated_data = lapply(todo$tensor_fn, function(mytensor_fn) {
  # mytensor_fn = todo$tensor_fn[1]
  integrate_tensor_data(myfn = paste0(here("data/FIGURE_and_SUPPLEMENT_general/", mytensor_fn)),
                        myepidata = epidata[region == todo[tensor_fn ==mytensor_fn, region]],
                        datesVariantsEmerging,
                        region= todo[tensor_fn ==mytensor_fn, region])
  }
                             ) 

names(all_integrated_data) = todo$region


############################################################################################################.
# Modellfit plus infectivity all regions----
############################################################################################################.

plot_testpos_infectivity_allele = function(myintegrated_data, maxdate_diagrams) {
  # myintegrated_data = all_integrated_data[[1]]
  
  p_fitage_reported =ggplot(myintegrated_data$plotdat_modelfit[agegroup2 %nin% c("unbekannt") & datum <= maxdate_diagrams& outcome =="reported"], aes(datum, value,col = agegroup2,  lty = type)) + 
    geom_line(lwd = 1) +
    scale_color_wsj( ) +
    facet_wrap(~outcome, scales = "free", ncol = 4) +
    # scale_y_log10(label= label_comma(accuracy = 1), sec.axis = dup_axis()) +
    # scale_y_continuous(label= label_comma(accuracy = 1), breaks = pretty_breaks(10), sec.axis = dup_axis()) +
    
    scale_x_date(breaks = date_breaks(width = "2 month"), labels =  date_format("%b-%y"))+
    xlab("") +
    
    # geom_vline( xintercept = datesVariantsEmerging$date, lty = 2, alpha = 0.4) +
    
    # annotate("text",x = datesVariantsEmerging$date+7, y = rep(1700000, 5),label = datesVariantsEmerging$variant2, angle = 90) +
    theme_minimal(base_size = 14) + ylab("Number") +
    scale_alpha_manual(values = c(0.2, 1))+
    scale_size_manual(values = c(0.8, 1.75)) +
    # labs(lty = "", col = "") +
    # guides(lwd = "none", alpha = "none") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 ),
          legend.position = "top") +
    labs(col = "", lty = "") +
    scale_linetype_manual(values = c(1,2))+
    coord_cartesian(xlim = c(min(myintegrated_data$inzdata$DateRep ), maxdate_diagrams)) +  geom_vline( xintercept = myintegrated_data$datesVariant5proz_model$date, lty = 2)  + 
    annotate("text",x = myintegrated_data$datesVariant5proz_model$date+7, y = rep(200000, nrow(myintegrated_data$datesVariant5proz_model)),label = myintegrated_data$datesVariant5proz_model$variant2, angle = 90)  + 
    scale_y_log10(label= label_comma(accuracy = 1), limits = c(8, max(myintegrated_data$plotdat_modelfit$value, na.rm = T)))  
  
  p_fitage_reported
  
  p_infectivity = myintegrated_data$data$infectivityplot +  
    scale_color_wsj( ) + 
    ylab("infecting rate b1")+
    scale_x_date(breaks = date_breaks(width = "2 month"), labels =  date_format("%b-%y")) + 
    theme(axis.text.x = element_blank()) +
    coord_cartesian(xlim = c(min(myintegrated_data$inzdata$DateRep ), maxdate_diagrams),
                    ylim = c(0, 0.8) + geom_vline( xintercept = myintegrated_data$datesVariant5proz_model$date, lty = 2)) +
    guides(color = "none")
  
  
  
  
  # add variants ----
  p_var = ggplot(myintegrated_data$data$variantdata_outputlayer[variant2 %nin% c("Om.Subv.","BAY")], aes(datum, dataVariant_anteil, col = variant2, lty = type)) + geom_line(lwd = 1) + theme_minimal(base_size = 14) +
    scale_y_continuous(labels = label_percent(accuracy = 1)) + 
    theme(legend.position = "bottom", axis.title.x = element_blank(), axis.text.x = element_blank()) + labs(col = "") + 
    coord_cartesian(xlim = c(min(myintegrated_data$inzdata$DateRep ), maxdate_diagrams)) +guides(col=guide_legend(nrow=1,byrow=TRUE)) +  geom_vline( xintercept = myintegrated_data$datesVariant5proz_model$date, lty = 2) +ylab("Variant") +
    guides(lty = "none") + labs(color = "")
  p_var
  
  
  p_testpos_detail = p_fitage_reported / (p_infectivity +scale_y_continuous(labels = label_comma(accuracy = 0.1))) + p_var+ patchwork::plot_layout(heights = c(3.5,1, 0.5)) + plot_annotation(title = myintegrated_data$region)
  
  print(p_testpos_detail)
  p_testpos_detail
  
  
}


pdf(here("results/SUPPLEMENT_Fit_Testpos_Infectivity_Variant_log_allBL.pdf"), 12,10)
all_testpos_plots = lapply(all_integrated_data, function(x) plot_testpos_infectivity_allele(myintegrated_data = x, maxdate_diagrams))
dev.off()

for(i in todo$region) {
  # # i = todo$region[1]
  jpeg2(here(paste0("results/SUPPLEMENT_Fit_Testpos_Infectivity_Variant_log_", i, ".jpg")), 12,10)
  print(all_testpos_plots[[i]])
  dev.off()
}


############################################################################################################.
# Modellfit severity all regions----
############################################################################################################.

plot_hosp_icu_death = function(myintegrated_data, maxdate_diagrams) {
  # myintegrated_data = all_integrated_data[[1]]
  myintegrated_data$plotdat_modelfit[, outcome := factor(outcome, levels = c("reported","Hospital", "ICU", "died"))]
  
  p_fitage =ggplot(myintegrated_data$plotdat_modelfit[agegroup2 %nin% c("unbekannt") & outcome != "reported" & datum <= maxdate_diagrams & value>=1], aes(datum, value,col = agegroup2,  lty = type)) + geom_line(lwd = 1) + 
    scale_color_wsj( ) + 
    facet_wrap(~outcome, scales = "free", ncol = 3) +
    # scale_y_log10(label= label_comma(accuracy = 1), sec.axis = dup_axis()) +
    # scale_y_continuous(label= label_comma(accuracy = 1), breaks = pretty_breaks(10)) +
    scale_y_log10(label= label_comma(accuracy = 1)) +
    scale_x_date(breaks = date_breaks(width = "2 month"), labels =  date_format("%b-%y"))+
    xlab("") +
    
    # geom_vline( xintercept = datesVariantsEmerging$date, lty = 2, alpha = 0.4) +
    
    # annotate("text",x = datesVariantsEmerging$date+7, y = rep(1700000, 5),label = datesVariantsEmerging$variant2, angle = 90) +
    theme_minimal(base_size = 14) + ylab("Number") +
    scale_alpha_manual(values = c(0.2, 1))+
    scale_size_manual(values = c(0.8, 1.75)) +
    # labs(lty = "", col = "") +
    # guides(lwd = "none", alpha = "none") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 ), 
          legend.position = "top") + 
    labs(col = "", lty = "") +
    scale_linetype_manual(values = c(1,2))+
    coord_cartesian(xlim = c(min(myintegrated_data$inzdata$DateRep ), maxdate_diagrams))+
  
    geom_vline( xintercept = myintegrated_data$myintegrated_data$datesVariant5proz_model$date, lty = 2)+
    geom_vline( xintercept = myintegrated_data$datesVariant5proz_model$date, lty = 2)  +
    annotate("text",x = (myintegrated_data$datesVariant5proz_model$date+21)[1], y = rep(Inf, nrow(myintegrated_data$datesVariant5proz_model))[1],label = paste0(myintegrated_data$datesVariant5proz_model$variant2[1], "         "), angle = 90 ) +
    annotate("text",x = (myintegrated_data$datesVariant5proz_model$date+21)[2], y = rep(Inf, nrow(myintegrated_data$datesVariant5proz_model))[2],label = paste0(myintegrated_data$datesVariant5proz_model$variant2[2], "         "), angle = 90) +
    annotate("text",x = (myintegrated_data$datesVariant5proz_model$date+21)[3], y = rep(Inf, nrow(myintegrated_data$datesVariant5proz_model))[3],label = paste0(myintegrated_data$datesVariant5proz_model$variant2[3], "         "), angle = 90) +
    annotate("text",x = (myintegrated_data$datesVariant5proz_model$date+21)[4], y = rep(Inf, nrow(myintegrated_data$datesVariant5proz_model))[4],label = paste0(myintegrated_data$datesVariant5proz_model$variant2[4], "         "), angle = 90) +
    annotate("text",x = (myintegrated_data$datesVariant5proz_model$date+21)[5], y = rep(Inf, nrow(myintegrated_data$datesVariant5proz_model))[5],label = paste0(myintegrated_data$datesVariant5proz_model$variant2[5], "            "), angle = 90)+ 
    plot_annotation(title = myintegrated_data$region)
  
  
  p_fitage 
   
  print(p_fitage)
  p_fitage
  
  
}


pdf(here("results//SUPPLEMENT_Fit_severity_log_allBL.pdf"), 15,7)
all_severity_plots = lapply(all_integrated_data, function(x) plot_hosp_icu_death(myintegrated_data = x, maxdate_diagrams))
dev.off()

for(i in todo$region) {
  # # i = todo$region[1]
  jpeg2(here(paste0("results/SUPPLEMENT_Fit_severity_log_", i, ".jpg")), 15,7)
  print(all_severity_plots[[i]])
  dev.off()
}


############################################################################################################.
# Modellfit GERMANY infectivity plus severity ----
############################################################################################################.
infectivityplot_DE = all_testpos_plots[["Germany"]]
severityplot_DE = all_severity_plots[["Germany"]]

p_fit_ger =infectivityplot_DE/ severityplot_DE + plot_layout(nrow = 4,guides = 'collect') + plot_annotation(tag_levels = "A") & theme(legend.position = "top",plot.tag = element_text(face = 'bold', size = 19))


pdf(here("results//FIGURE_Fit_Germany_testpos_severity.pdf"), 14,14)
p_fit_ger
dev.off()

############################################################################################################.
# # finalize Skript
############################################################################################################.

finalizeSkript()

