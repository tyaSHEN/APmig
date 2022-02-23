### International migration in the Asia-Pacific region: Application of a generation-distribution model to overcome missing data
### ###################################
# summaries of flows with uncertainty
######################################
# code written by: Qing Guan, Tianyu Shen, Arkadiusz Wisniowski
######################################
# last revised 16/04/2021
######################################

library(tidyverse)
library(reshape2)
library(gridExtra)
library(scales)
library(openxlsx)
library(Amelia)

wd=getwd()
dir.create(file.path(wd, paste0("outputs ",Sys.Date())), showWarnings = FALSE)

#### Step 1: generation  ####
#input: generation model covariates prepared in "1-1 R code to prepare generation inputs"
#input2: lag covariates in input1, covariates small as factor
#input2_a: subset of input3 with only eu=1 (31  IMEM countries)
#input3: log covariates in input2 and calculated zscore for all covarites except for small and year (produce estimate with independent variable: year)
#input4: produce estimate without independent variable: year 

#splitting calculation into 20 times to ease memory,
## !!! please change 20 to lower numbers to test !!!
for (z in 1:20) {
  #import generation input data (covariates)
  input=read.csv("inputs/generation input (mock).csv", as.is = T) 

  ## regression
  input2 <- c()
  # calculate lagged values for each covariates
  for (c in unique(input$i_iso3)) {
    temp <- input %>% filter(i_iso3==c)
    temp$l_ln_i_pop_z <- sapply(1:nrow(temp), function(x) temp$i_pop[x-1]) %>% unlist() %>% append(NA,after = 0) # append NA after the zero value (add NA to the first row)
    temp$l_ln_i_gdp_z <- sapply(1:nrow(temp), function(x) temp$i_gdp2[x-1]) %>% unlist() %>% append(NA,after = 0)
    temp$l_ln_i_depr_z <- sapply(1:nrow(temp), function(x) temp$i_depr[x-1]) %>% unlist() %>% append(NA,after = 0)
    temp$l_ln_i_urbanpop_z <- sapply(1:nrow(temp), function(x) temp$i_urbanpop[x-1]) %>% unlist() %>% append(NA,after = 0)
    temp$l_ln_i_fele_z <- sapply(1:nrow(temp), function(x) temp$i_fele[x-1]) %>% unlist() %>% append(NA,after = 0)
    temp$l_ln_i_pc_mig_z <- sapply(1:nrow(temp), function(x) temp$i_pc_mig[x-1]) %>% unlist() %>% append(NA,after = 0)
    temp$small <- sapply(1:nrow(temp), function(x) temp$small[x-1]) %>% unlist() %>% append(NA,after = 0)
    input2 <- rbind(input2,temp)
  }
  rm(temp,c)
  
  # dichotomous variable recode to factor format
  input2$small <- as.factor(input2$small)
  
  # calcualte log value of each covariates, and scale to 0~1 (z-score standardlization)
  {input2_a <- subset(input2, eu==1)
    input2_a$l_ln_i_pop_z <- scale(log(input2_a$l_ln_i_pop_z)) %>% as.vector()
    input2_a$l_ln_i_gdp_z <- scale(log(input2_a$l_ln_i_gdp_z)) %>% as.vector()
    input2_a$l_ln_i_depr_z <- scale(input2_a$l_ln_i_depr_z) %>% as.vector()
    input2_a$l_ln_i_urbanpop_z <- scale(input2_a$l_ln_i_urbanpop_z) %>% as.vector()
    input2_a$l_ln_i_fele_z <- scale(input2_a$l_ln_i_fele_z) %>% as.vector()
    input2_a$l_ln_i_pc_mig_z <- scale(input2_a$l_ln_i_pc_mig_z) %>% as.vector()
    input2_a$ln_year_z <- scale(input2_a$year) %>% as.vector()
    
    input2_b <- subset(input2, ap==1)
    input2_b$l_ln_i_pop_z <- scale(log(input2_b$l_ln_i_pop_z)) %>% as.vector()
    input2_b$l_ln_i_gdp_z <- scale(log(input2_b$l_ln_i_gdp_z)) %>% as.vector()
    input2_b$l_ln_i_depr_z <- scale(input2_b$l_ln_i_depr_z) %>% as.vector()
    input2_b$l_ln_i_urbanpop_z <- scale(input2_b$l_ln_i_urbanpop_z) %>% as.vector()
    input2_b$l_ln_i_fele_z <- scale(input2_b$l_ln_i_fele_z) %>% as.vector()
    input2_b$l_ln_i_pc_mig_z <- scale(input2_b$l_ln_i_pc_mig_z) %>% as.vector()
    input2_b$ln_year_z <- scale(input2_b$year) %>% as.vector()
    
    input3 <- rbind(input2_a,input2_b)
    rm(input2_b)
  }
  
  memory.limit(size=500000)
  # Two models with full and reduced covariates, rows with NA in ep=1 data (year=2001 rows) are omit (default=na.omit)
  load("inputs/imem (mock).RData")
  temp.raw <- temp1 %>% filter(i_iso3!="LIE")
  iters = unique(temp.raw$iteration)
  
  dir.create(file.path(wd, paste0("outputs ",Sys.Date(),"/",z)), showWarnings = FALSE)
  
  temp1 = temp.raw %>% filter(iteration %in% iters[(50*(z-1)+1):(z*50)])
  
  temp2 = temp1 %>% 
    left_join(input2_a %>% 
                select(-ln_emig_rate), 
              by = c("i_iso3","year", "i_name"))
  
  input3 <- input3 %>% filter(i_iso3!="LIE")
  input3_t <- input3 %>% select(1,2,3,5,6,16:21)
  input3_t <- input3_t %>% filter(!is.na(small))
  imputed3=amelia(x = input3_t, idvars = c("small","ap"), m = 8, cs = "i_iso3", ts = "year",intercs=T, p2s = 0)
  
  iters = unique(temp1$iteration)
  input3= c()
  t=1
  for (i in 1:8){
    if(is.na(imputed3$imputations[[i]])){next()}
    temp = imputed3$imputations[[i]] %>% left_join(expand_grid(iteration=iters[(10*(t-1)+1):(t*10)], year=1999:2019))
    input3 = rbind(input3, temp)
    if(t==5){break()}
    t=t+1
  }
  
  input3 = input3 %>% filter(!is.na(small))
  input3$ln_emig_rate_pred=NA_real_
  for (i in unique(temp1$iteration)) {
    mod1 <- lm(ln_emig_rate ~ l_ln_i_pop_z + small + l_ln_i_gdp_z + l_ln_i_depr_z + l_ln_i_urbanpop_z + l_ln_i_fele_z + l_ln_i_pc_mig_z, data = temp2 %>% filter(iteration==i))
    input3$ln_emig_rate_pred[which(input3$iteration==i)] <- predict(mod1, input3[which(input3$iteration==i),])
    print(i)
  }
  
  #model parameters
  summary(mod1)

  input3$est_e <- exp(input3$ln_emig_rate_pred) * input3$i_pop
  
  ggplot(data = input3 %>% filter(ap==0,iteration==2),aes(x = year,y= exp(ln_emig_rate_pred)*100,group= year)) + geom_boxplot()+ggtitle("eu_input_data")
  #ggsave(paste0("outputs ",Sys.Date(), "/eu_input_data.png"))
  ggplot(data = input3 %>% filter(ap==1),aes(x = year,y= exp(ln_emig_rate_pred)*100,group= year)) + geom_boxplot() +facet_wrap(~i_iso3,scales = "free_y")
  #ggsave(paste0("outputs ",Sys.Date(), "/au_output_data.png"))
  assign(paste0("input3.",z),input3)
  save(list = c(paste0("input3.",z)), file=paste0("outputs ",Sys.Date(), "/",z,"/Step 1 Generation model.RData"))
  
  
  #### Step 2: distribution #### 
  rm(list=setdiff(ls(), c("input3","z","wd")))
  
  ### input ###
  # emig_total: generation model output (all input eu and output ap countries)
  # APemig_total: geneartion model output for AP countries
  # APemig: distribution model with origins and destinations
  # stock, trade: distribution covariates
  
  emig_total=input3
  
  # emig_total=read.csv("log_emig_rate_zscore_estimates.csv",as.is = T)
  
  
  #subset AP countries
  APemig_total=filter(emig_total,ap==1,year%in%c(2000:2019))
  
  #to county number of countries use i_iso3 as i_name has blank cells and different names for same country (e.g. LAOs)
  length(unique(APemig_total$i_iso3)) #53
  
  names(APemig_total)
  APemig_total=APemig_total %>% select(i_iso3,year,est_e, iteration)
  head(APemig_total)
  # save(APemig_total,file="APemig_total.Rdata")

  stock = read_csv("inputs/stock gross (mock).csv")
  trade <- read_csv("inputs/trade (mock).csv")
  #pop <- read_csv("inputs/pop/pop.csv")
  
  #for (i in ls(pattern = "df")){
  # add j_iso3 column
  j_iso3=expand_grid(j_iso3=c(unique(APemig_total$i_iso3),"AFR","EUR","ROA","SCA"),
                     iteration=unique(APemig_total$iteration), 
                     year=unique(APemig_total$year))
  APemig=left_join(APemig_total,j_iso3)
  rm(j_iso3)
  
  #2. add variables used for distribution
  #2.2 migrant stock
  # f_migstock from UN 2019, with four ROW regions breakdown
  APemig=left_join(APemig,stock[,-4:-5])
  
  #diagonal
  APemig = APemig %>%
    mutate(migstock_gross=ifelse(i_iso3==j_iso3,NA,migstock_gross))
  
  #2.5 trade
  
  APemig=left_join(APemig,trade) 
  
  #diagonal
  APemig = APemig %>% mutate(trade=ifelse(i_iso3==j_iso3,NA,trade))
  
  #3. calcuate shares
  #share by trade
  APemig<-as.data.frame(APemig %>% # use this one, trade with TWN added
                          group_by(i_iso3,year,iteration) %>%
                          mutate(est_e_trade2 = est_e*(trade/sum(trade,na.rm = T))) %>% ungroup())
  
  #share by migrant stock
  APemig<-as.data.frame(APemig %>% # use this one, Taiwan added, and reciprocal stock
                          group_by(i_iso3,year,iteration) %>%
                          mutate(est_e_stock3 = est_e*(migstock_gross/sum(migstock_gross,na.rm = T))) %>%
                          ungroup())
  
  
  #average the shares of migrant stock and trade
  #for each sub-dataframe 
  
  APemig<-APemig %>% # use this one
    group_by(i_iso3,j_iso3,year,iteration) %>%
    mutate(est_e_ave2=sum(est_e_stock3,est_e_trade2,na.rm = T)/2) %>%
    ungroup()
  

  assign(paste0("APemig.",z),APemig)
  
  save(list=c(paste0("APemig.",z)),file=paste0("outputs ",Sys.Date(), "/",z,"/Step 2 Distribution.RData"))
  
  
  #### Step 3d: add 4 ROW regions ####
  rm(list=setdiff(ls(), c("APemig","stock","trade","pop","z","wd")))
  
  #APemig_ROW: add four ROW areas to i_iso3 in distribution outputs APemig from distribution folder, distributed using migrant stock, trade, and average
  #stock_d, trade_d: filter 4 ROW regions data
  #stock_a, trade_a: sum of inflow from all AP countries data
  #flow_a: sum of migrant inflow form all AP countries
  
  head(APemig)
  #rename stock and trade distribution columns
  APemig= APemig %>% 
    rename(est_e_trade=est_e_trade2, est_e_stock=est_e_stock3)
  APemig = APemig %>% select(-trade,-migstock_gross)
  
  unique(APemig$i_iso3)
  unique(APemig$j_iso3)
  
  ROW=expand_grid(i_name=NA, 
                  i_iso3=c("EUR","ROA","AFR","SCA"),
                  year=unique(APemig$year), 
                  est_e=NA, 
                  iteration= unique(APemig$iteration), 
                  j_iso3=unique(APemig$i_iso3),
                  est_e_trade=NA,est_e_stock=NA,est_e_ave2=NA)
  
  APemig_ROW=bind_rows(APemig,ROW)
  rm(ROW)
  
  APemig_ROW=left_join(APemig_ROW,stock)
  APemig_ROW=left_join(APemig_ROW,trade)
  
  # stock ratio
  stock_a<-APemig_ROW %>%
    filter(!i_iso3%in%c("EUR","ROA","AFR","SCA"),!j_iso3%in%c("EUR","ROA","AFR","SCA"))%>%
    group_by(j_iso3,year,iteration) %>%
    summarise(stock_ap=sum(migstock_gross,na.rm = T)) %>%
    ungroup()
  head(stock_a)
  
  
  stock_d<-APemig_ROW %>%
    filter(i_iso3%in%c("EUR","ROA","AFR","SCA"))
  
  stock_d=left_join(stock_d,stock_a)
  head(stock_d)
  rm(stock_a)
  
  stock_d = stock_d %>%
    mutate(ratio=migstock_gross/stock_ap)
  
  # flow
  flow_a<-APemig_ROW %>%
    filter(!i_iso3%in%c("EUR","ROA","AFR","SCA"),!j_iso3%in%c("EUR","ROA","AFR","SCA"))%>%
    group_by(j_iso3,year, iteration) %>%
    summarise(est_e_ave_ap=sum(est_e_ave2,na.rm = T)) %>%
    ungroup()
  
  head(flow_a)
  flow_a=flow_a %>% select(year,j_iso3,est_e_ave_ap,iteration)
  stock_d=left_join(stock_d,flow_a)
  head(stock_d)
  
  stock_d=stock_d %>% 
    mutate(est_e_ave_ROW_mig=ratio*est_e_ave_ap)
  
  ## modified: added trade ratio and pop
  # trade ratio
  trade_a<-APemig_ROW %>%
    filter(!i_iso3%in%c("EUR","ROA","AFR","SCA"),!j_iso3%in%c("EUR","ROA","AFR","SCA"))%>%
    group_by(j_iso3,year, iteration) %>%
    summarise(trade_ap=sum(trade,na.rm = T)) %>%
    ungroup()
  head(trade_a)

  
  trade_d<-APemig_ROW %>%
    filter(i_iso3%in%c("EUR","ROA","AFR","SCA"))
  
  trade_d=left_join(trade_d,trade_a)
  head(trade_d)
  rm(trade_a)
  
  trade_d = trade_d %>%
    mutate(ratio=trade/trade_ap)
  
  #flow
  trade_d=left_join(trade_d,flow_a)
  
  trade_d = trade_d %>%
    mutate(est_e_ave_ROW_tra=ratio*est_e_ave_ap)

  APemig_ROW=left_join(APemig_ROW,stock_d %>% select(year,i_iso3,j_iso3,est_e_ave_ROW_mig,iteration))
  rm(stock_d)
  APemig_ROW=left_join(APemig_ROW,trade_d %>% select(year,i_iso3,j_iso3,est_e_ave_ROW_tra, iteration))
  rm(trade_d)
  
  APemig_ROW = APemig_ROW %>%
    mutate(est_e_ave_ROW = (est_e_ave_ROW_mig+est_e_ave_ROW_tra)/2)

  head(APemig_ROW)
  APemig_ROW = APemig_ROW %>%
    mutate(est_e_ave1=ifelse(i_iso3%in%c("EUR","ROA","AFR","SCA"),est_e_ave_ROW,est_e_ave2)) # now only j=TAIWAN, and diagonal cells do not have an value
  head(APemig_ROW)
  rm(stock,APemig)
  
  APemig_ROW = APemig_ROW %>%
    mutate(est_e_ave1=replace_na(est_e_ave1,0))
  
  assign(paste0("APemig_ROW.",z),APemig_ROW)
  save(list= (paste0("APemig_ROW.",z)),file=paste0("outputs ",Sys.Date(), "/",z,"/Step 3a Distribution with ROW.RData"))
  
  
  #### Step 3e: corrlation adjustment ####
  rm(list=setdiff(ls(), c("APemig_ROW","z","wd")))

  # 1. create perfect correlation
  APemig_ROW_t=APemig_ROW
  APemig_ROW_t = APemig_ROW_t %>%
    rename(i_iso3_t=i_iso3, 
           j_iso3_t=j_iso3, 
           est_i_ave1=est_e_ave1,
           j_name=i_name)

   
  APemig_ROW_t=left_join(APemig_ROW,APemig_ROW_t %>% select(i_iso3_t,year, j_iso3_t,est_i_ave1, iteration),by=c("year"="year","i_iso3"="j_iso3_t","j_iso3"="i_iso3_t","iteration"="iteration"))

  
  
  APemig_ROW_t$i_iso3_f=as.numeric(factor(APemig_ROW_t$i_iso3,levels=unique(APemig_ROW_t$j_iso3)))
  APemig_ROW_t$j_iso3_f=as.numeric(factor(APemig_ROW_t$j_iso3,levels=unique(APemig_ROW_t$j_iso3)))
  
  APemig_perfect=APemig_ROW_t
  APemig_perfect$est_i_perfect=APemig_perfect$est_e
  
  # 2. add Rest of world to rest of world zero flows 
  names(APemig_perfect)
  APemig_perfect <- APemig_perfect %>% select(i_name,i_iso3,year,est_e,j_iso3,est_e_ave1,est_i_ave1,i_iso3_f,j_iso3_f,est_i_perfect, iteration)
  ROW_ROW=expand_grid(i_name=NA,
                      i_iso3=c("AFR","EUR","ROA","SCA"), 
                      year=unique(APemig_perfect$year),
                      est_e=0,
                      j_iso3=c("AFR","EUR","ROA","SCA"),
                      est_e_ave1=0,
                      est_i_ave1=0,est_i_perfect=0,
                      iteration=unique(APemig_perfect$iteration)) %>%
    mutate(i_iso3_f=53+as.numeric(as_factor(i_iso3)),
           j_iso3_f=53+as.numeric(as_factor(j_iso3)))
  
  # ROW_ROW=data.frame(i_name=NA,i_iso3=rep(c("AFR","EUR","ROA","SCA"),20*4),year=rep(rep(unique(APemig_perfect$year),each=4),4),est_e=0,j_iso3=rep(c("AFR","EUR","ROA","SCA"),each=4*20),est_e_ave1=0,est_i_ave1=0,i_iso3_f=rep(54:57,20*4),j_iso3_f=rep(54:57,each=4*20),est_i_perfect=0)
  APemig_perfect= bind_rows(APemig_perfect,ROW_ROW)

  #order origin and destination to prepare for matrix
  APemig_perfect=arrange(APemig_perfect,i_iso3_f,j_iso3_f,year) # this is a 57*57*16 full matrix
  head(APemig_perfect)
  
  # 3. IPF by year data
  OUTPUT = list()
  SEED = list()
  iters=unique(APemig_ROW$iteration)
  for (j in iters){
    for (i in 2000:2019){
      flow=filter(APemig_perfect,year==i, iteration==j)
      
      flow<-as.data.frame(flow %>%
                            group_by(i_iso3,year) %>%
                            mutate(est_e2 = sum(est_e_ave1,na.rm = T)))
      rowcontrol=as.matrix(unique(flow$est_e2))
      colcontrol=as.matrix(t(unique(flow$est_e2))) #same as rowcontrol
      seed=matrix(flow$est_e_ave1,nrow=57,ncol=57,byrow=T) #distribution results as seed
      
      # Initial set up
      maxiter <- 2000 
      closure <- 0.0001
      
      result <- seed
      colnames(result)=1:57 # for index purpose
      rownames(result)=1:57 # for index purpose
      rowcheck <- 1
      colcheck <- 1
      checksum <- 1
      iter <- 0
      
      # Loop for iterative proportion fitting
      while((checksum>closure) && (iter<maxiter))
      {
        coltotal <- colSums(result,na.rm = T)
        # to relax the four ROW columns 
        colfactor <- ifelse(colnames(result)%in%c(1:53),colcontrol/coltotal,1) # ratio between last and this interation's column totals
        
        result <- sweep(result, 2, colfactor, "*") #multiplying each column of result matrix by colfactor value 
        
        rowtotal <- rowSums(result,na.rm = T)
        rowfactor <- ifelse(rownames(result)%in%c(1:53),rowcontrol/rowtotal,1)
        result <- sweep(result, 1, rowfactor, "*")
        
        rowcheck <- max(abs(rowtotal[1:53] - rowcontrol[1:53]))
        colcheck <- max(abs(coltotal[1:53] - colcontrol[1:53]))
        checksum <- max(rowcheck,colcheck)
        
        iter <- iter + 1
      }
      

      if (iter==2000) print(paste("tolerance=", checksum))

      colnames(result)=unique(flow$i_iso3)
      rownames(result)=unique(flow$i_iso3)
      result=round(result,0)
      OUTPUT[[as.character(j)]][[as.character(i)]]=result

      colnames(seed)=unique(flow$i_iso3)
      rownames(seed)=unique(flow$i_iso3)
      seed=round(seed,0)
      SEED[[as.character(j)]][[as.character(i)]]=seed
      rm(colcontrol,rowcontrol,colfactor,rowfactor,checksum,closure,colcheck,coltotal,iter,maxiter,rowcheck,rowtotal)
    }
    print(paste("iter=", j))
  }
  rm(i)
  
  ### Average IPF output and seed ###
  m_seed=c()    # m for Matrix           
  for (j in iters){
    for (i in 2000:2019){
      tmp=as.data.frame(SEED[[as.character(j)]][[as.character(i)]])
      tmp$year=i
      tmp$iteration=j
      m_seed=rbind(m_seed,tmp)
    }
  }
  rm(tmp,i,j)
  
  m_output=c()             
  for (j in iters){
    for (i in 2000:2019){
      tmp=as.data.frame(OUTPUT[[as.character(j)]][[as.character(i)]])
      tmp$year=i
      tmp$iteration=j
      m_output=rbind(m_output,tmp)
    }
  }
  rm(tmp,i,j)
  
  m_estimate= 0.5*m_seed[,1:59]+0.5*m_output[,1:59]
  m_estimate$o_name=rownames(m_estimate)[1:57]
  rm(m_output,m_seed)
  m_estimate=cbind(m_estimate[,58:60],m_estimate[,-58:-60])
  
  names(m_estimate)
  l_estimate=melt(m_estimate,id.vars = c("o_name","year","iteration")) %>% #long format 
    rename(d_name=variable)
  
  head(l_estimate)
  class(l_estimate$value) # numeric
  
  assign(paste0("l_estimate_no_adj.",z),l_estimate)
  
  save(list = c(paste0("l_estimate_no_adj.",z)),file=paste0("outputs ",Sys.Date(), "/",z,"/Step 3b No adjustment.RData")) # save matrix format estimates
  
  
  #### Step 3f: Ad hoc adjustment ####
  # adjust HKG, MAC, SGP immigration by UN net
  rm(list=setdiff(ls(), c("l_estimate","z","wd")))
  l_estimate_sel <- l_estimate %>% filter(o_name %in% c("HKG","MAC","SGP")) %>% group_by(year,o_name, iteration) %>% summarise(eflow = sum(value)) %>% ungroup()
  pop <- read_csv("inputs/pop (mock).csv")
  
  l_estimate_sel <- left_join(l_estimate_sel,pop, by=c("year"="year","o_name"="i_iso3.y"))
  l_estimate_sel$emi.rate <- l_estimate_sel$eflow/l_estimate_sel$pop
  l_estimate_sel$year.group <- "2000-2004" 
  l_estimate_sel$year.group[which(l_estimate_sel$year %in% c("2005","2006","2007","2008","2009"))] <- "2005-2009" 
  l_estimate_sel$year.group[which(l_estimate_sel$year %in% c("2010","2011","2012","2013","2014"))] <- "2010-2014" 
  l_estimate_sel$year.group[which(l_estimate_sel$year %in% c("2015","2016","2017","2018","2019"))] <- "2015-2019"
  
  Country.list <- read_csv("inputs/AP Country list.csv")
  UN.net.rate <- read_csv("inputs/UN net rate (mock).csv")
  UN.net.rate <- left_join(Country.list,UN.net.rate,by = c("code"="i_iso3"))
  UN.net.rate.long <- pivot_longer(UN.net.rate,cols = c(4:7),names_to = "year.group")
  UN.net.rate.long$year.group <- recode(UN.net.rate.long$year.group, `2000-2005` = "2000-2004",`2005-2010` = "2005-2009",`2010-2015` = "2010-2014",`2015-2020` = "2015-2019")
  
  UN.net.rate.sel <- UN.net.rate.long %>% filter(code %in% c("HKG","MAC","SGP")) %>% select(3,4,5)
  l_estimate_sel <- left_join(l_estimate_sel,UN.net.rate.sel,by=c("year.group"="year.group","o_name"="code"))
  l_estimate_sel$immi.rate <-  l_estimate_sel$emi.rate+l_estimate_sel$value
  
  # include the reported data from NZL CAN AUS KOR 
  ANK <- read_csv("inputs/ANK (mock).csv")[,c(1:4)]
  ANK <- ANK %>% left_join(expand_grid(iteration=unique(l_estimate$iteration), year=1999:2019))
  colnames(ANK)[2] <- "o_name"
  l_estimate_sel$iflow=l_estimate_sel$immi.rate*l_estimate_sel$pop
  l_estimate_sel <- l_estimate_sel[,c(1,2,3,4,10)]
  l_estimate_sel_full <- full_join(l_estimate_sel,ANK)
  
  
  # merge the adjusted number with the rest
  l_estimate_o_total <- l_estimate %>% group_by(o_name,year,iteration) %>% summarise(value_e= sum(value)) %>% ungroup()
  l_estimate_d_total <- l_estimate %>% group_by(d_name,year, iteration) %>% summarise(value_i= sum(value)) %>% ungroup()
  l_estimate_total <- inner_join(l_estimate_o_total,l_estimate_d_total, by = c("o_name" = "d_name", "year"="year", "iteration"="iteration"))
  l_estimate_total <- left_join(l_estimate_total,l_estimate_sel_full,by = c("o_name", "year", "iteration"))
  
  l_estimate_total$eflow <- ifelse(is.na(l_estimate_total$eflow),l_estimate_total$value_e,l_estimate_total$eflow)
  l_estimate_total$iflow <- ifelse(is.na(l_estimate_total$iflow),l_estimate_total$value_i,l_estimate_total$iflow)
  l_estimate_total=l_estimate_total[-c(4,5)]
  
  # calculate the proportion of the immigration of a country to all immigration excluding the 7 adjusted countries
  l_estimate_total_sub <- subset(l_estimate_total,!o_name%in%unique(l_estimate_sel_full$o_name))
  l_estimate_total_sub <- l_estimate_total_sub %>% group_by(year,iteration) %>% mutate(ipro = iflow/sum(iflow)) %>% ungroup()
  
  iters=unique(l_estimate$iteration)
  l_estimate_total_adj <- subset(l_estimate_total,o_name%in%unique(l_estimate_sel_full$o_name))
  for (j in iters){
    for (y in unique(l_estimate_total$year)) {
      # emigration - immigration (force the total immigration equal to emigration)
      d = colSums(l_estimate_total[which(l_estimate_total$year==y & l_estimate_total$iteration==j),c(4)])-colSums(l_estimate_total[which(l_estimate_total$year==y & l_estimate_total$iteration==j),c(5)])
      temp = l_estimate_total_sub %>% filter(year==y, iteration==j) %>% mutate(gap = ipro*d) %>% mutate(iflow=iflow+gap) %>% select(-6,-7)
      l_estimate_total_adj <- rbind(l_estimate_total_adj,temp)
    }
  }
  colSums(l_estimate_total_adj[l_estimate_total_adj$iteration==iters[1],c(4,5)])
  
  l_estimate$o_name <- factor(l_estimate$o_name,levels=l_estimate$o_name[1:57])
  l_estimate_total_adj$o_name <- factor(l_estimate_total_adj$o_name,levels=l_estimate$o_name[1:57])
  l_estimate_total_adj <- arrange(l_estimate_total_adj,o_name)
  
  IPF2 = list()
  for (j in iters){
    for (i in 2000:2019){
      
      rowcontrol=as.matrix(filter(l_estimate_total_adj,year==i,iteration==j)$eflow)
      colcontrol=as.matrix(t(filter(l_estimate_total_adj,year==i,iteration==j)$iflow)) #same as rowcontrol
      
      seed=matrix(subset(l_estimate,year==i & iteration==j)$value,nrow=57,ncol=57,byrow=F) #distribution results as seed
      
      # Initial set up
      maxiter <- 2000 
      closure <- 0.0001
      
      result <- seed
      colnames(result)=1:57 # for index purpose
      rownames(result)=1:57 # for index purpose
      rowcheck <- 1
      colcheck <- 1
      checksum <- 1
      iter <- 0
      
      # Loop for iterative proportion fitting
      while((checksum>closure) && (iter<maxiter))
      {
        coltotal <- colSums(result,na.rm = T)
        # to relax the four ROW columns 
        colfactor <- ifelse(colnames(result)%in%c(1:53),colcontrol/coltotal,1) # ratio between last and this interation's column totals
        
        result <- sweep(result, 2, colfactor, "*") #multiplying each column of result matrix by colfactor value 
        
        rowtotal <- rowSums(result,na.rm = T)
        rowfactor <- ifelse(rownames(result)%in%c(1:53),rowcontrol/rowtotal,1)
        result <- sweep(result, 1, rowfactor, "*")
        
        rowcheck <- max(abs(rowtotal[1:53] - rowcontrol[1:53]))
        colcheck <- max(abs(coltotal[1:53] - colcontrol[1:53]))
        checksum <- max(rowcheck,colcheck)
        
        iter <- iter + 1
      }
      

      if (iter==2000) print(paste("tolerance=", checksum))
      
      colnames(result)=as.character(unique(l_estimate$o_name))
      rownames(result)=as.character(unique(l_estimate$o_name))
      result=round(result,0)
      IPF2[[as.character(j)]][[as.character(i)]]=result

      rm(colcontrol,rowcontrol,colfactor,rowfactor,checksum,closure,colcheck,coltotal,iter,maxiter,rowcheck,rowtotal)
    }
    print(j)
  }
  rm(i,j)

  m_output=c()     
  for (j in iters){
    for (i in 2000:2019){
      tmp=as.data.frame(IPF2[[as.character(j)]][[as.character(i)]])
      tmp$year=i
      tmp$iteration=j
      m_output=rbind(m_output,tmp)
    }
  }
  rm(tmp,i,j)
  
  m_estimate=m_output
  m_estimate$o_name=rownames(m_estimate)[1:57]
  rm(m_output)
  m_estimate=cbind(m_estimate[,58:60],m_estimate[,-58:-60])
  
  assign(paste0("m_estimate.",z),m_estimate)
  
  save(list=c(paste0("m_estimate.",z)),file=paste0("outputs ",Sys.Date(), "/",z,"/Matrix result.RData"))
  
  # save(m_estimate,file="Gen_Dis_estimates_matrix.dta") # save matrix format estimates
  write_csv(m_estimate,paste0("outputs ",Sys.Date(), "/",z,"/Matrix result ",z,".csv"))
  
  names(m_estimate)
  l_estimate=melt(m_estimate,id.vars = c("o_name","year","iteration")) #long format 
  names(l_estimate)[4]="d_name"
  head(l_estimate)
  class(l_estimate$value) # numeric
  
  assign(paste0("l_estimate.",z),l_estimate)
  
  save(list=c(paste0("l_estimate.",z)),file=paste0("outputs ",Sys.Date(), "/",z,"/Long result.RData"))
  print(z)
  z=z+1
  rm(list=setdiff(ls(), c("z","wd")))
}

### combine results together ####
### Step 1 Generation model
input3 =c()
for (z in 1:20) {
  load(paste0("outputs ",Sys.Date(), "/",z,"/Step 1 Generation model.RData"))
  input3=rbind(input3,get(paste0("input3.",z)))
}
save(list=c("input3"),file=paste0("outputs ",Sys.Date(), "/Step 1.RData"))
rm(list=setdiff(ls(), c("wd")))

### Step 3d Distribution with ROW
APemig =c()
for (z in 1:20) {
  load(paste0("outputs ",Sys.Date(), "/",z,"/Step 3a Distribution with ROW.RData"))
  APemig=rbind(APemig,get(paste0("APemig_ROW.",z))[,c(1,2,4,5,17)])
}
save(list=c("APemig"),file=paste0("outputs ",Sys.Date(), "/Step 3d.RData"))
rm(list=setdiff(ls(), c("wd")))

### Step 3e forced correlation
No.adhoc =c()
for (z in 1:20) {
  load(paste0("outputs ",Sys.Date(), "/",z,"/Step 3b No adjustment.RData"))
  No.adhoc=rbind(No.adhoc,get(paste0("l_estimate_no_adj.",z)))
}
save(list=c("No.adhoc"),file=paste0("outputs ",Sys.Date(), "/Step 3e.RData"))
rm(list=setdiff(ls(), c("wd")))


### !!!final result
l_estimate =c()
for (z in 1:20) {
  load(paste0("outputs ",Sys.Date(), "/",z,"/Long result.RData"))
  l_estimate=rbind(l_estimate,get(paste0("l_estimate.",z)))
}
save(list=c("l_estimate"),file=paste0("outputs ",Sys.Date(), "/Step 3f.RData"))
# summarize data
res_od=l_estimate %>% 
  group_by(year,o_name,d_name) %>%
  #calculating quantiles over all iterations
  summarise(q10=quantile(value,0.1),
            q50=median(value),
            q90=quantile(value,0.9))



