jpeg2 = function(...) {jpeg(..., units = "in", quality = 100, res = 600)}

extractTensordata <- function(tensor, firstdate, scenarioname = NULL, extractOutputlayer=T, showplots=F) {
  # tensor = copy(tensor_DE);scenarioname = "tensor_DE"; firstdate = as_date("2020-03-04")
  
  # tensor=tensor_DE
  # firstdate = as_date("2020-03-04")
  # scenarioname = "Germany"
  # extractOutputlayer=T
  message("Tensor has time-steps from ", tensor$start_step, " to ", tensor$end_step)
  #################################################.
  # >extract info about compartments----
  # early _e_ compartments
  sim_arri_e_pre = tensor$sim_arri_e # 3-dimensionaler Tensor von " Infectable " Kompartimenten
  dimnames(sim_arri_e_pre)[[1]] = 1:length(dimnames(sim_arri_e_pre)[[1]])
  
  sim_arri_e = reshape2::melt(sim_arri_e_pre) %>% data.table(keep.rownames = T)
  setnames(sim_arri_e, c("rn","time","compartment", "agegroup", "value"))
  stopifnot(all(c("S", "Vac0", "Vac1", "Vac2", "Vac3") %in% sim_arri_e$compartment))
  sim_arri_e[,compartment := factor(compartment, levels =c( "Vac1", "Vac2","Vac3", "Vac0","S", "R1", "R2", "R3"))]
  sim_arri_e[, .N, compartment]
  if(showplots==TRUE) sim_arri_e[,boxplot(value~compartment)$x]
  
  stopifnot(sim_arri_e[grepl("^R", compartment ), sum(value, na.rm = T)]==0)
  sim_arri_e = sim_arri_e[grepl("^R", compartment )==F] # kann raus, weil 0
  
  # unique(sim_arri_e$time)
  sim_arri_e[, time := as.numeric(time)]                  
  sim_arri_e=sim_arri_e[order(time)]
  sim_arri_e[, datum := firstdate-1 + time]
  
  stopifnot(all(paste0("age_", 1:5) %in% sim_arri_e$agegroup))
  
  sim_arri_e[, agegroup2:= ifelse(agegroup =="age_1", "A00-A14",
                                  ifelse(agegroup =="age_2", "A15-A34",
                                         ifelse(agegroup =="age_3", "A35-A59",
                                                ifelse(agegroup =="age_4", "A60-A79",
                                                       ifelse(agegroup =="age_5", "A80+",agegroup)))))]                  
  
  unique(sim_arri_e$compartment)
  
  sim_arri_e[,compartmentGrob := str_replace_all(compartment, "[0-9]", "")]
  unique(sim_arri_e$compartmentGrob)
  
  
  
  sim_arri_e[,compartmentGrober := ifelse(grepl("^E|^I|^C|^StCare",compartment), "Infected",
                                          ifelse(compartment %in% c("S","Vac0") , "S/Vac0",
                                                 
                                                 compartment %>% as.character()))]
  unique(sim_arri_e$compartmentGrober)
  
  
  unique(sim_arri_e$compartmentGrober)
  sim_arri_e[,.N,.(compartment, compartmentGrob, compartmentGrober)][order(compartmentGrober)] %>% data.frame()
  
  
  
  sim_arri_e[, variant:= "uninfected"]
  sim_arri_e[, variant2:= "uninfected"]
  
  
  # p0=ggplot(sim_arri_e[grepl("^R", compartment)==F], aes(datum, value, col = agegroup2)) + geom_line() +facet_wrap(~compartment, scale = "free") + scale_y_continuous(labels = label_comma(accuracy = 1), breaks = pretty_breaks(10)) # R manchmal als Rest drinne
  
  # ### infected compartment----
  sim_arri_l_pre = tensor$sim_arri_l # 5-dimensionaler Tensor von " Infected " Kompartimenten
  dimnames(sim_arri_l_pre)[[1]] = 1:length(dimnames(sim_arri_l_pre)[[1]])
  
  
  sim_arri_l = reshape2::melt(sim_arri_l_pre) %>% data.table(keep.rownames = T)
  setnames(sim_arri_l, c("rn","time","compartment","immunstate","variant", "agegroup", "value"))
  
  sim_arri_l[, time := as.numeric(time)]
  unique(sim_arri_l$time)
  sim_arri_l=sim_arri_l[order(time)]
  sim_arri_l[, datum := firstdate-1 + time]
  
  stopifnot(all(paste0("age_", 1:5) %in% sim_arri_l$agegroup))
  # stopifnot(all(c("E","IA1","IA2"  ,"IA3","IS1","IS2","IS3","StCare","C1","C2","C3","C4","D", "R1","R2","R3") %in% sim_arri_l$compartment))
  sim_arri_l$compartment %>% unique()
  stopifnot(all(c("E","IS1","IS2","IS3","IS4","StCare","C1","C2","C3", "D", "R1","R2","R3") %in% sim_arri_l$compartment))
  
  
  
  sim_arri_l[, agegroup2:= ifelse(agegroup =="age_1", "A00-A14",
                                  ifelse(agegroup =="age_2", "A15-A34",
                                         ifelse(agegroup =="age_3", "A35-A59",
                                                ifelse(agegroup =="age_4", "A60-A79",
                                                       ifelse(agegroup =="age_5", "A80+",agegroup)))))]
  
  
  
  
  
  
  sim_arri_l[, variant2:= ifelse(variant =="alpha", "WT",
                                 ifelse(variant =="engl", "alpha",
                                        ifelse(variant =="omicron", "BA1",
                                               ifelse(variant =="BA5", "BA4+5",as.character(variant)))))]
  recodervariant = sim_arri_l[, .N, .(variant,variant2)]
  
  message("--------------------------------------------\nRecoding variant names\n",paste(recodervariant$variant, collapse = ", ") ,"\nto \n", paste(recodervariant$variant2, collapse = ", "))
  factorlevels =  sim_arri_l[, unique(variant2)]
  sim_arri_l[, variant2:= factor(variant2, levels = factorlevels)]
  
  
  sim_arri_l[,compartmentGrob := str_replace_all(compartment, "[0-9]", "")]
  unique(sim_arri_l$compartmentGrob)
  # allcompartmentGrob
  # sim_arri_l[, compartmentGrob := factor(compartmentGrob, levels = allcompartmentGrob)]
  
  sim_arri_l[,compartmentGrober := ifelse(grepl("^E|^I|^C|^StCare",compartment), "Infected",
                                          ifelse(compartment %in% c("S","Vac0") , "S/Vac0",
                                                 
                                                 compartment %>% as.character()))]
  # allcompartmentGrober
  # sim_arri_l[, compartmentGrober := factor(compartmentGrober, levels = allcompartmentGrober)]
  
  unique(sim_arri_l$compartmentGrober)
  sim_arri_l[,.N,.(compartment, compartmentGrob, compartmentGrober)][order(compartmentGrober)] %>% data.frame()
  
  
  
  # merge sim_arri_e and sim_arri_l
  
  
  qlist5 = venn2(sim_arri_e$compartment, sim_arri_l$compartment, plotte = showplots)
  stopifnot(length(qlist5$q1)==0)
  
  
  sim_arri_e[, variant:= "uninfected"]
  sim_arri_e[, variant2:= "uninfected"]
  sim_arri_e[, immunstate2:= ifelse(compartment %in% c("S", "Vac0"), "Unvacc.",
                                    ifelse(compartment %in% c("Vac1", "Vac2"), "Vac1+2 (some waning)",
                                           ifelse(compartment %in% c("Vac3"), "Vac3 (more waning)",compartment)))]
  
  
  immutable = sim_arri_e[, .N, .(compartment, immunstate2)]
  immutable[, immunestate_yuri:= ifelse(compartment %in% c("S", "Vac0"), "Unv",
                                        ifelse(compartment %in% c("Vac1", "Vac2"), "Vac",
                                               ifelse(compartment %in% c("Vac3"), "Wan",compartment)))]
  
  
  sim_arri_l[, immunstate2:= immutable[match_hk(sim_arri_l$immunstate, immutable$immunestate_yuri, makeunique = T, importcol = immutable$immunstate2),immunstate2]]
  
  sim_arri_l[,.N, .(compartment,immunstate2, immunstate)] %>% data.frame()
  
  
  
  
  venn2(names(sim_arri_e), names(sim_arri_l), plotte = showplots)
  
  sim_arri_e[, immunstate:= immutable[match_hk(sim_arri_e$immunstate2, immutable$immunstate2, makeunique = T, importcol = immutable$immunestate_yuri),immunestate_yuri]]
  
  # venn2(sim_arri_e$immunstate, sim_arri_l$immunstate)
  
  sim_arri_pre = rbind(sim_arri_e, sim_arri_l)
  
  # sim_arri_pre[, compartment := factor(compartment, levels = allcompartment)]
  # sim_arri_pre
  
  
  
  
  
  
  
  
  
  
  
  
  
  # add all class
  stopifnot(all(grepl("all", sim_arri_pre$agegroup))==F)
  stopifnot(all(grepl("all", sim_arri_pre$agegroup2))==F)
  
  sim_arri_toadd = sim_arri_pre[,.(value = sum(value),
                                   agegroup   = "all",
                                   agegroup2   = "all"), .(time, datum,  compartment, variant, variant2 ,immunstate ,immunstate2 ,compartmentGrob ,compartmentGrober)]
  
  
  sim_arri = rbind(sim_arri_pre[, -'rn'],
                   sim_arri_toadd)
  setorder(sim_arri, datum)
  sim_arri[, compartmentGrober := factor(compartmentGrober, levels = unique(compartmentGrober))]
  levels(sim_arri$compartmentGrober)
  
  sim_arri[, outcome := ifelse(compartmentGrob %in% c("E", "IA", "IS", "StCare"), "infiziert",
                               ifelse(compartmentGrob=="C", "ICU",
                                      ifelse(compartmentGrob=="D", "verstorben",as.character(NA))))]
  
  
  
  
  
  
  
  
  # sim_arri[, immunstate2 := ifelse(immunstate =="Unv", "naiv",
  # ifelse(immunstate=="Vac", "Protection high (R1&2,Vac1&2)",
  # ifelse(immunstate=="Wan", "Waned  (R3,Vac3)", immunstate)))]
  sim_arri[, .N, .(immunstate,immunstate2)]
  
  
  allcompartment = c("S",
                     "Vac0", "E", "IA1", "IA2",
                     "IA3", "IS1", "IS2", "IS3",  "C1", "C2", "C3", "C4", "StCare",
                     "D","Vac3", "R3","Vac2", "R2",  "Vac1", "R1")
  sim_arri[,compartment := factor(compartment, allcompartment)]
  
  allcompartmentGrob = c("S", "E","I", "IA", "IS","StCare", "C", "D","Vac",  "R")
  sim_arri[, compartmentGrob := factor(compartmentGrob, levels = allcompartmentGrob)]
  
  allcompartmentGrober = c("S/Vac0", "Infected", "D",paste0("Vac", 1:3),  paste0("R", 1:3))
  sim_arri[, compartmentGrober := factor(compartmentGrober, levels = allcompartmentGrober)]
  
  
  if(is.null(scenarioname)==F ){
    sim_arri$scenario = scenarioname
    compartmentdata = moveColFront(sim_arri, 'scenario')
  } else compartmentdata = sim_arri
  
  
  ##################################################.
  # >extract info about inzident cases IN MODEL----
  # load reported data 
  simu_reported_pre =  tensor$sum_increas_IS %>% as.data.table(keep.rownames = T) # sum_increas_IS ist zwei dimensionale Tensor für kumulative registrierte neue Fälle (ohne Meldenverzug, nach Zeit und Altersgruppe) # YURIFRAGE - mit symptomatic plus asymptomatic? This is not the data, that can be directly compared with reported data. this is slot TODO YURI
  setnames(simu_reported_pre, c("num","immunstate","variant", "agegroup", "cumvalue"))
  
  simu_reported_pre[, immunstate2 := immutable[match_hk(simu_reported_pre$immunstate, immutable$immunestate_yuri, makeunique = T, importcol = immutable$immunstate2),immunstate2]]
  
  
  # unique(simu_reported_pre$num)
  
  simu_reported_pre[, datum := firstdate-1 + as.numeric(num)]
  
  stopifnot(all(paste0("age_", 1:5) %in% simu_reported_pre$agegroup))
  
  
  simu_reported_pre[, agegroup2:= ifelse(agegroup =="age_1", "A00-A14",
                                         ifelse(agegroup =="age_2", "A15-A34",
                                                ifelse(agegroup =="age_3", "A35-A59",
                                                       ifelse(agegroup =="age_4", "A60-A79",
                                                              ifelse(agegroup =="age_5", "A80+",agegroup)))))] 
  
  
  simu_reported_pre[, variant2:= ifelse(variant =="alpha", "WT",
                                        ifelse(variant =="engl", "alpha",
                                               ifelse(variant =="omicron", "BA1",
                                                      ifelse(variant =="BA5", "BA4+5",as.character(variant)))))]
  
  recodervariant_simu_reported = simu_reported_pre[, .N, .(variant,variant2)]
  stopifnot(identical(recodervariant_simu_reported[,paste(variant, variant2)] %>% sort(),
                      recodervariant[,paste(variant, variant2)] %>% sort())) # check for same encoding
  
  factorlevels 
  stopifnot(all(simu_reported_pre$variant2 %in% factorlevels  ))
  simu_reported_pre[, variant2:= factor(variant2, levels = factorlevels)]
  
  simu_reported_pre[, outcome := "incidence modelintern"] # war im ersten Skripten als 'reported' bezeichnet
  
  
  
  # add all agegroup
  
  simu_reported_toadd = simu_reported_pre[, .(cumvalue = sum(cumvalue, na.rm = T), 
                                              agegroup = "all",
                                              agegroup2 = "all"), .(num,datum, immunstate,immunstate2, variant, variant2, outcome) ]
  
  simu_reported = rbind(simu_reported_pre, simu_reported_toadd)
  setorder(simu_reported, agegroup2, immunstate, variant, variant2, datum,outcome)
  # simu_reported[immunstate ==  immunstate[1] & agegroup2 == agegroup2[3] &variant == variant[3]]
  simu_reported[, value :=  cumvalue-c(0, cumvalue[1:(.N-1)]), .(outcome, agegroup2, immunstate, variant, variant2, outcome)]
  
  
  ggplot(simu_reported[value>=1], aes(datum, value, col = immunstate2)) + geom_line() + facet_grid(agegroup2~variant2, scales = "free") + scale_y_log10(label= label_comma(accuracy = 1), sec.axis = dup_axis()) 
  
  
  plot_kumulInzByImmune = ggplot(simu_reported[value>0], aes(datum, value, col  = immunstate2))+ theme_minimal(base_size = 14) + theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 0)) + geom_line(lwd = 1) + facet_grid(agegroup2 ~ variant2, scales = "free", space = "free")  + ylab(unique(simu_reported$outcome)) + ggtitle(scenarioname)
  if(showplots==TRUE) print(plot_kumulInzByImmune)
  
  if(is.null(scenarioname)==F ){
    simu_reported$scenario = scenarioname
    variantdata = moveColFront(simu_reported, 'scenario')
  }
  
  ##################################################.
  # >extrct data for outputlayer-----
  # outputlayerdata
  # 
  tensor$datum = unique(simu_reported_pre$datum) %>% sort()
  
  if(extractOutputlayer) { 
    sm = tensor$res_daily
    tail(tensor$res_dailyoutputs$all$simulated_daily$Total)
    tail(tensor$outputs$`0_14`$simulated_daily$Total)
    tail(tensor$outputs$`35_59`$simulated_daily$Total)
    
    agestodo = names(tensor$outputs)
    agestodo
    
    if(is.null(agestodo)==F){
      outputs2 = lapply(agestodo, function(myage) {
        # myage= agestodo[3]
        output_myage  = tensor$outputs[[myage]]
        output_myage2 = data.table(time  = output_myage$time,
                                   datum = as_date(output_myage$time))
        
        
        for(my_daily_outcome in names(output_myage$simulated_daily)) {
          # my_daily_outcome =names(output_myage$simulated_daily)[1]
          output_myage2[[paste0('daily_', my_daily_outcome)]] = output_myage$simulated_daily[[my_daily_outcome]]
        }
        output_myage2
        
        for(my_cumul_outcome in names(output_myage$simulated_cumul)) {
          # my_cumul_outcome =names(output_myage$simulated_cumul)[1]
          output_myage2[[paste0('cumul_', my_cumul_outcome)]] = output_myage$simulated_cumul[[my_cumul_outcome]]
        }
        
        if(output_myage2[time ==max(time), daily_Death==0 & is.na(daily_Death)==F]) { # bug laut Yuri tel 7.9.22, hier fix dafuer
          output_myage2[time ==max(time), daily_Death :=NA]
          output_myage2[time ==max(time), cumul_Death :=NA]
        }
        
        output_myage2[, agegroup := myage]
        output_myage2
      }) %>% rbindlist()
    } else {
      stopifnot(identical(ncol(tensor$res_daily$Total), length(tensor$datum) ))
      ouputs1a = data.table(datum=tensor$datum %>% as_date(),
                            type = "daily_Total",
                            (tensor$res_daily$Total %>% t()) %>% 
                              as.data.table()) %>% 
        melt.data.table(id.vars = c("datum",'type'),  variable.name="agegroup")
      
      ouputs1b = data.table(datum=tensor$datum %>% as_date(),
                            type = "daily_Death",
                            (tensor$res_daily$Death %>% t()) %>% 
                              as.data.table()) %>% 
        melt.data.table(id.vars = c("datum",'type'),  variable.name="agegroup")
      
      
      ouputs1c = data.table(datum=tensor$datum %>% as_date(),
                            type = "daily_Critical",
                            (tensor$res_daily$Critical %>% t()) %>% 
                              as.data.table()) %>% 
        melt.data.table(id.vars = c("datum",'type'), variable.name="agegroup")
      
      
      ouputs1d = data.table(datum=tensor$datum %>% as_date(),
                            type = "daily_Normal",
                            (tensor$res_daily$Normal %>% t()) %>% 
                              as.data.table()) %>% 
        melt.data.table(id.vars = c("datum",'type'),  variable.name="agegroup")
      
      outputs2 = rbind(ouputs1a, 
                       ouputs1b, 
                       ouputs1c, 
                       ouputs1d)
      
      unique(outputs2$agegroup)
      
      outputs2[, agegroup2:= ifelse(agegroup =="0_14", "A00-A14",
                                    ifelse(agegroup =="15_34", "A15-A34",
                                           ifelse(agegroup =="35_59", "A35-A59",
                                                  ifelse(agegroup =="60_79", "A60-A79",
                                                         ifelse(agegroup =="80", "A80+",
                                                                ifelse(agegroup =="all", "all",agegroup))))))] 
      
      
      
      
      outputs2[,.N, .(agegroup, agegroup2)]
      # ggplot(outputs2, aes(datum, value, col = agegroup)) + geom_line() + facet_wrap(~type, scales = "free")
      
      
      outputs3 = dcast.data.table(outputs2, datum + agegroup+agegroup2~type, value.var = "value")
      setorder(outputs3, agegroup,datum)
      
      outputs3[, cumul_Total := cumsum(daily_Total), agegroup2]
      outputs3[, cumul_Critical := cumsum(daily_Critical), agegroup2]
      outputs3[, cumul_Death := cumsum(daily_Death), agegroup2]
      outputs3[, cumul_Normal := cumsum(daily_Normal), agegroup2]
      
      
      outputs3m = melt(outputs3, id.vars = c("datum", "agegroup2"), measure.vars = c("daily_Total","daily_Death", "daily_Critical", "daily_Normal","cumul_Total", "cumul_Death", "cumul_Critical", "cumul_Normal"))
      outputs3m[, datatype := str_split(variable, "_") %>% sapply(., "[", 1)]
      outputs3m[, outcome_yuri := str_split(variable, "_") %>% sapply(., "[", 2)]
      outputs3m$outcome_yuri %>% unique()
      outputs3m[, outcome :=ifelse(outcome_yuri =="Total", "reported",
                                   ifelse(outcome_yuri =="Critical", "ICU",
                                          ifelse(outcome_yuri =="Death", "died",
                                                 ifelse(outcome_yuri =="Normal", "Hospital",
                                                        outcome_yuri))))]
      
      
      outputs3m[,.N, .(datatype, outcome, outcome_yuri,variable)]
      
      
      plot_outputlayer = ggplot(outputs3m, aes(datum, value, col = outcome)) + geom_line() + facet_grid(datatype~agegroup2, scales = "free") + scale_y_log10()
      if(showplots==TRUE) print(plot_outputlayer)
      
      
      
      if(is.null(scenarioname)==F ){
        outputs3m$scenario = scenarioname
        outputs3m = moveColFront(outputs3m, 'scenario')
      }
      
    }
  }
  ##################################################.
  # # >extract info about variant frequencies currently in the model----
  variantcompartments = sim_arri_l[grepl("^D$|^R", compartment)==F, unique(compartment)]
  novariantcompartments = sim_arri_l[grepl("^D$|^R", compartment)==T, unique(compartment)]
  message("--------------------------------------------\nCalculating virus variant frequencies over time from compartments\n", paste(variantcompartments, collapse = ", "), "\n (and not from compartments ", paste(novariantcompartments, collapse = ", "), ")")
  novariantcompartments = sim_arri_l[grepl("^D$|^R", compartment)==T, unique(compartment)]
  
  dataVariant = sim_arri_l[grepl("^D$|^R", compartment)==F, .(value = sum(value)), .(datum, variant2)]
  p1 = ggplot(dataVariant, aes(datum, value, col = variant2)) + geom_line()
  
  dataVariant[, dataVariant_anteil :=  value / sum(value), datum]
  
  p2 = ggplot(dataVariant, aes(datum, dataVariant_anteil, col = variant2)) + geom_line()
  require(patchwork)
  variantplot = (p1+p2+ plot_layout(guides = "collect") + plot_annotation(title = scenarioname))
  if(showplots==TRUE) print(variantplot)
  
  if(is.null(scenarioname)==F ){
    dataVariant$scenario = scenarioname
    variantdata = moveColFront(dataVariant, 'scenario')
  }
  
  
  ### extract variants as measured in the outputlayer
  ##considering only tested individuals
  message("Now calculating virus variant frequencies in the output layer")
  mutant_outputlayer = tensor$res_daily$allele_frac %>% as.data.table(keep.rownames = T)
  mutant_outputlayerm = melt(mutant_outputlayer, id.vars = c("rn", "dates", "numdates"))
  mutant_outputlayerm[, variant := str_split(variable, "_") %>% sapply(., "[", 1)]
  mutant_outputlayerm[, type := str_split(variable, "_") %>% sapply(., "[", 2)]
  mutant_outputlayerm[,.N,.(variant, type)] %>% data.frame()
  
  stopifnot(all (c("alpha", "engl", "delta","omicron","BA2","BA5") %in% mutant_outputlayerm$variant))
  
  mutant_outputlayerm[, variant2 := ifelse(variant=="alpha", "WT",
                                           ifelse(variant=="engl", "alpha",
                                                  ifelse(variant=="delta", "delta",
                                                         ifelse(variant=="omicron", "BA1",
                                                                ifelse(variant=="BA2", "BA2",
                                                                       ifelse(variant=="BA5", "BA4+5",variant))))))]
  
  
  mutant_outputlayerm[, .N,.(variant, variant2)]
  mutant_outputlayerm[, variant2 := factor(variant2, levels = c("WT", "alpha", "delta","BA1","BA2","BA4+5"))]
  
  #renaming
  mutant_outputlayerm2 = mutant_outputlayerm[, .(datum  = dates %>% as_date(), variant2, dataVariant_anteil=value, type )]
  
  if(is.null(scenarioname)==F ){
    mutant_outputlayerm2$scenario = scenarioname
    mutant_outputlayerm2 = moveColFront(mutant_outputlayerm2, 'scenario')
  }
  
  
  mutant_outputlayerm2[, type := ifelse(type=="measured", "reported",
                                        ifelse(type =="simulated", "modelled", type))]
  
  p_mutant_outputlayerm = ggplot(mutant_outputlayerm2, aes(datum,dataVariant_anteil, lty = type, col = variant2 , group = paste(type, variant2))) + geom_line(alpha=0.5)
  
  p_mutant_outputlayerm
  
  
  # ## compare them
  # variantdata
  # mutant_outputlayerm$type
  # comparetab = rbind(variantdata[, .(dates = datum %>% as_date(), variant2, value=dataVariant_anteil, type ="oldmodel")],
  #                    mutant_outputlayerm[, .(dates = dates %>% as_date(), variant2, value, type )])
  # 
  # comparetab[,.N, .(type)]
  # p_comparetabfreq = ggplot(comparetab, aes(dates,value, lty = type, col = variant2 , group = paste(type, variant2))) + geom_line(size = 1)
  # p_comparetabfreq
  
  ##################################################.
  # # >extract info about infectivity ----
  # 
  
  infectivitydata = tensor$treat_arr %>% as.data.table(keep.rownames = T)
  infectivitydata[, time := as.numeric(rn)]                  
  infectivitydata=infectivitydata[order(time)]
  infectivitydata[, datum := firstdate-1 + time]
  
  infectivitydatam = melt(infectivitydata, id.vars = c("rn","datum", "time"))
  
  infectivitydatam[, agegroup2:= ifelse(variable =="age_1", "A00-A14",
                                        ifelse(variable =="age_2", "A15-A34",
                                               ifelse(variable =="age_3", "A35-A59",
                                                      ifelse(variable =="age_4", "A60-A79",
                                                             ifelse(variable =="age_5", "A80+",variable)))))] 
  
  
  
  infectivityplot =ggplot(infectivitydatam, aes(datum, value, col = agegroup2 )) + 
    geom_line(lwd=1) + 
    theme_minimal(base_size = 14) +
    scale_y_continuous(breaks = pretty_breaks(10), labels = label_comma(accuracy = 0.1), sec.axis = dup_axis()) +
    ylab("Infectivity b1") +
    scale_x_date( breaks = date_breaks(width = "1 months"))+ #limits = range(vaccdata$DateRep), 
    theme(axis.title.x = element_blank(),
          legend.position = c(0.21, 0.7),
          axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 0)) + 
    labs(lty = "")+
    scale_color_viridis_d(direction = -1) +
    # geom_vline( xintercept = datesDominance$date, lty = 2, alpha = 0.5) +
    
    # guides(col = "none") +
    # geom_vline( xintercept = as_date(c("2020-03-22", "2020-12-14", "2021-11-22")), lty = 2, alpha = 0.4, col = "red", lwd = 1) +
    coord_cartesian(ylim = c(0, 1))
  infectivityplot
  
  
  # >zusammenbauen ----
  res = c()
  res$scenario = ifelse(is.null(scenarioname), "", scenarioname)
  res$incidencedata = simu_reported
  res$incidenceplot = plot_kumulInzByImmune
  res$compartmentdata = compartmentdata
  res$variantplot = variantplot
  res$variantdata = variantdata
  
  res$variantplot_outputlayer = p_mutant_outputlayerm
  res$variantdata_outputlayer = mutant_outputlayerm2
  
  if(extractOutputlayer==T) res$outputlayerdata = outputs3m
  if(extractOutputlayer==T) res$outputlayerplot = plot_outputlayer
  res$infectivitydata = infectivitydatam
  res$infectivityplot = infectivityplot
  
  res
}
