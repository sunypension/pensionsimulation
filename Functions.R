#########################################################################################################
#########################################################################################################
#                                                                                                       #
#                                          DESCRIPTION                                                  #
#   functions.R contains the logic for the entire model.                                                #
#                                                                                                       #
#   The server makes function calls to functions.R whenever there is a change in user input             #
#                                                                                                       #
#########################################################################################################
#########################################################################################################

################################################################################################
##################### Basic functions for Pension Simulation Calculator#################
################################################################################################

############################ LIBRARY IMPORT ##############################
# scales: R library to build for scaling data to aesthetics and allow
# dollar and percent conversions
# REFERENCE: https://cran.r-project.org/web/packages/scales/scales.pdf
##########################################################################
library(scales)

############################ LIBRARY IMPORT ##############################
# magrittr: The magrittr is a package with two aims: to decrease development
# time and to improve readability and maintainability of code. 
# REFERENCE: https://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html
##########################################################################
library(magrittr)

############################# R COMMAND ##################################
# rm() removes a variable from the workspace. A list of all the variables 
# in the workspace is passed as a parameter to rm to clear all variables 
# in the workspace to ensure that the model uses only the updated values
# sent by the user for computations
##########################################################################
rm(list = ls())

################## Setting the dollar format for the dollar() function used in the program #################
dollar_format(prefix = "", suffix = "$",largest_with_cents = 1e-2, big.mark = ",", negative_parens = FALSE)


#################################################################################################
########################################## Load RData ###########################################
#################################################################################################

# Mortality.Rdata consists of the mortality tables in the form of a data frame
load('Data/Mortality.RData') 

# Termination.Rdata consists of the termination rates in the form of a data frame
load('Data/Termination.RData')

#################################################################################################
############################## Age information for simulation ###################################
#################################################################################################

############## Setter method for age information ###############

age_information <<- function(i_ea,retire) {
  ea <<- i_ea # entry age of members,  multiple entry ages are not introduced yet
  retire <<- retire  # retirement age
  max_age <<- 85 # maximum age limit
  min_ea<<-30 # minimum entry age
  max_ea<<-45 # maximum entry age
  min_mort_age<<-18 # minimum age in the mortality tables
  max_mort_age<<-120 # maximum age in the mortality tables 
  age <<- seq(i_ea,retire)   # age vector from 30-65
  age_xr <<- seq(retire + 1,max_age)  # age vector from 66-100
  yos_xy <<- seq(ea:retire) - 1  # years of service vector
  yos_r_max<<- seq(retire:max_age)-1 # years of service vector from retirenment to max_age
  yos_r_max_mort<<- seq(retire:max_mort_age)-1 # years of service vector from retirenment to max_mort_age
  
  rx <<- (retire - age)  # future years before retirement vector
  
}

############ Generating age group labels with a 5 year gap #####################
############ between population entry age and max age ##########################

age_grp_labels<<- function(i_ea,age_limit){
  return(seq(i_ea,age_limit,5))
}

#############################################################################################
################################## Population generation ####################################
#############################################################################################

generate_pop<-function(ea,retire,pop_type,pop_size,median){
  
  ### calling age information to get age bounds for population ###
  age_information(ea,retire)
  
  ### generating empty vector of size of aage range ###
  pop1<-numeric(retire-ea+1)
  
  ### Conditional statements to generate population based on user input for population ###
  if (pop_type == 'Customize curve')
    pop1 <-round(rtnorm(pop_size,median,15,left = ea-1,right = retire+1))
  if (pop_type == 'Over mature')
    pop1 <- round(rtnorm(pop_size,retire,15,left = ea-1,right = retire+1))
  if (pop_type == 'Under mature')
    pop1 <- round(rtnorm(pop_size,ea,15,left = ea-1,right = retire+1))
  if (pop_type == 'Uniform'){
    repcount <- round(pop_size/(retire-ea))
    for(i in ea:retire)
      pop1<-c(pop1,rep(i,repcount))
    pop1<-pop1[pop1!=0]
  }
  
  ### truncating population exceeding the age range ###  
  pop1<-pop1[pop1!=ea-1]
  pop1<-pop1[pop1!=retire+1]
  
  ### return population vector to server.R ###
  return(pop1)
}

############## Generate retirees population ############
# population_retirees<-function(ea,retire,mort,population){
#   ### Getting active population frequencies for each age group ###
#   pop_freq<-table(population)
#   ### generating retiree population frquencies for each age after retirement to max_age using mortality table ###
#   pop2<-ceiling(pop_freq[length(pop_freq)]*cumprod(1-get_mort(ea,retire,mort)[(retire-ea+1):(max_age-ea+1)]))
#   ### empty vector ###
#   pop3<-0
#   ### age just after retirement ###
#   j<-retire+1
#   ### translating the retiree frquency to population for age distribution plot ###
#   for(i in pop2){
#     pop3<-c(pop3,rep(j,i))
#     j=j+1
#     if(j==max_age+1)
#       break;
#   }
#   ### truncating any active members in the population of retiress ###
#   pop3<-pop3[2:length(pop3)]
#   
#   ### return population vector to server.R ###
#   return(pop3)
# }

############## Generate retirees population ############
population_retirees<-function(ea,retire,population){
  ### Getting active population frequencies for each age group ###
  pop_freq<-table(population)
  ### generating retiree population frquencies for each age after retirement to max_age without xonsidering decline after max age ###
  pop2<-rep(pop_freq[length(pop_freq)], max_age - retire) 
  ### empty vector ###
  pop3<-0
  ### age just after retirement ###
  j<-retire+1
  ### translating the retiree frquency to population for age distribution plot ###
  for(i in pop2){
    pop3<-c(pop3,rep(j,i))
    j=j+1
    if(j==max_age+1)
      break;
  }
  ### truncating any active members in the population of retiress ###
  pop3<-pop3[2:length(pop3)]
  
  ### return population vector to server.R ###
  return(pop3)
}

############# Age group generation after max age ################
pop_after_max_age <- function(ea, retire){
  age_information(ea, retire)
  return(rep(0, max_mort_age - max_age))
}

############ total population ###################
get_total_pop<- function(ea,retire,pop_type,pop_size,median){
  active <- generate_pop(ea,retire,pop_type,pop_size,median)
  retirees <- population_retirees(ea, retire, active)
  after_max_age <- pop_after_max_age(ea, retire)
  return(c(table(c(active, retirees)), after_max_age))
}


#########################################################################################
################################ Salary functions #######################################
#########################################################################################

############## Actual Salary ############
get_act_sal <- function(ea,retire,a_sgr) {
  age_information(ea,retire)
  # actual salary at age x
  return(c((1 + a_sgr) ^ yos_xy))
}

############## Expected Salary ############
get_exp_sal <- function(ea,retire,sgr) {
  age_information(ea,retire)
  # expected salary at age x
  return(c((1 + sgr) ^ yos_xy))
}

############## Accumlated Salary ############
get_acc_sal <- function(ea,retire,a_sgr) {
  act_sal <- get_act_sal(ea,retire,a_sgr)
  #acculumated salary at age x
  return(c(cumsum(act_sal)))
}

############## Expected Salary at retirement ############
get_exp_sal_r <- function(ea,retire,a_sgr,sgr) {
  act_sal <- get_act_sal(ea,retire,a_sgr)
  exp_sal_r <- act_sal * ((1 + sgr) ^ rx) # expected salary at age r in year x
  return(exp_sal_r)
}

##############  Salary for constant inflation rate for different entry ages ############
get_sal_vector_ea <- function(ea,retire,sal,inflation,sgr){
  return((((1 + (inflation / 100)) ^ yos_xy)/(1 + ((sgr) / 100)) ^ yos_xy)*sal)
}

############## Entry salary from current Salary for constant inflation rate ############
get_sal_ca_ea<- function(ea,ca,retire,sal,inflation,sgr){
  return(((1 + ((sgr) / 100)) ^ yos_xy[length(age)-(retire-ca)])/((1 + (inflation / 100)) ^ yos_xy[length(age)-(retire-ca)])*sal)
}

###################################################################################################
############################### Mortality table ###################################################
###################################################################################################
get_mort <- function(ea,retire,mort) {
  ### load age information ###
  age_information(ea,retire)
  annuitant_mort<-(mort+1) # skips a column to access annuitants in the data frame
  ### generating a vector of mortality rates with active members and annuitants based on user 
  ### input for mortality table
  mortality_tab<-c(mort_tables[[mort]][(ea-min_mort_age+1):(retire-min_mort_age+1)],
                   mort_tables[[(annuitant_mort)]][(retire-min_mort_age+2):(max_mort_age-min_mort_age+1)])
  ### return the new vector without any NAs to calling code ###
  return(na.omit(mortality_tab))
}

##################################################################################################
############################### Survival probabilities ###########################################
##################################################################################################

######### Survival probablity from x to x+1 ##################
get_xpmx <- function(ea,retire,mort) {
  age_information(ea,retire)
  xpmx <- (1 - get_mort(ea,retire,mort))
  return(xpmx)
}

############# Survival probablity from x to retirement age #################
get_rpmx <- function(ea,retire,mort) {
  xpmx <- get_xpmx(ea,retire,mort)
  ### generating vector by reversing original xpmx vector and taking a 
  ### cumulative product and reversing this result again
  rpmx <- c(rev(cumprod(rev(xpmx[1:(length(age) - 1)]))),1)
  return(rpmx)
}

############# Survival probablity from retirement age to certain age (120 max)#########
get_xpmr <- function(ea,retire,mort) {
  xpmx <- get_xpmx(ea,retire,mort)
  rpmx <- get_rpmx(ea,retire,mort)
  # new vector of numeri type
  xpmr <- numeric()
  # generating first element of the vector for retirement age
  xpmr[1] <- xpmx[length(age)] * rpmx[length(rpmx)]
  # generating the rest of the vector by obtaining product of previous element 
  # and mortality rate for age 'i+length(age)' ranging from 66-120 if retirement age is 65
  for (i in 1:(max_mort_age - retire - 1))
    xpmr[i + 1] = xpmx[length(age) + i] * xpmr[i]
  
  return(xpmr[xpmr!=0])
}

#############################################################################################
########################### Benefit calculations ############################################
#############################################################################################

################ Accumulated benefits #############################
get_acc_benefits <- function(ea,retire,a_sgr,afc,bf) {
  acc_sal <- get_acc_sal(ea,retire,a_sgr)
  ### computation for accumulated benefits after afc period 
  after_afc<-(acc_sal[(afc+2):length(acc_sal)] - acc_sal[2:(length(age)-afc)]) * yos_xy[(afc+2):length(yos_xy)]*bf/afc
  ### binding the accumulated benefit for entry age, first afc years
  ### and computation for accumulated benefits after afc period 
  acc_benefits <- c(0,(acc_sal[2:(afc+1)] - acc_sal[1]) * bf,after_afc)
  return(acc_benefits)  
}

############################ Accumlated benefits at retirement #############################
get_acc_benefit_r <- function(ea,retire,a_sgr,sgr,afc,bf) {
  ### choosing expected salary at retirement vector partitions based on afc period
  if(afc>1)
    exp_sal_r <-get_exp_sal_r(ea,retire,a_sgr,sgr)[1:(length(age) - afc)]
  else
    exp_sal_r <-get_exp_sal_r(ea,retire,a_sgr,sgr)
  
  ### storing expected and actual salary vectors
  exp_sal <- get_exp_sal(ea,retire,sgr)
  act_sal <- get_act_sal(ea,retire,a_sgr)
  
  ### getting age bounds for averaging actual salary and expected salaries in the last afc years
  index_upper <- length(age) - afc
  index_lower <- length(age)
  
  ### create an empty vector for accumalted benefits at retirement
  acc_ben_avg<-numeric()
  acc_benefit_r <- exp_sal_r * (sum((1 + sgr) ^ seq(0,(-afc + 1))) / afc) * bf * (length(age) - 1)
  #acc_benefit_r <- exp_sal_r * (sum((1 + sgr) ^ seq(0,(afc + 1))) / afc) * bf * (length(age) - 1)
  # creating a accumalted benefit at retirement average for last afc yesr if afc is greater than 1 year
  if(afc>1){
    for (i in seq((index_upper+1):index_lower)-1)
      acc_ben_avg<-c(acc_ben_avg,mean(na.omit(c(act_sal[(index_upper+1):(index_upper+1+i)],exp_sal[(index_upper+i+2):index_lower])))*bf*(length(age) - 1))
    acc_ben_avg[length(acc_ben_avg)]<-mean(act_sal[(index_upper+1):index_lower])*bf*(length(age) - 1)
  }
  
  # binding result for accumalted benefits and average for last afc years
  acc_benefit_r <- c(acc_benefit_r,acc_ben_avg)
  
  return(acc_benefit_r)
}

############################ Accumulated benefit at 65 #############################
get_acc_benefit_65 <- function(ea,retire,a_sgr,sgr,afc,bf) {
  acc_benefit_r<-get_acc_benefit_r(ea,retire,a_sgr,sgr,afc,bf)
  # access last element of previous function to return accumuulated benefit at 65
  acc_benefit_65 <- acc_benefit_r[length(acc_benefit_r)]
  return(acc_benefit_65)
}

############################################################################################
############################### Discount factor functions ##################################
############################################################################################

########### discount factor from x to r #########################
get_vrx <- function(ea,retire,i) {
  age_information(ea,retire)
  vrx <- c(1 / ((1 + i) ^ (rx)))
  return(vrx)
}

########### discount factor from x to r & r to 100 #########################
get_vxr <- function(ea,retire,i) {
  # generating discount vector for entry age to maximum age of the model(100)
  vxr <- c(get_vrx(ea,retire,i),c(1 / ((1 + i) ^ (seq(
    (retire + 1):max_mort_age)))))
  return(vxr)
}

########################################################################################################
################################### Termination rate functions #########################################
########################################################################################################

####################### termination rates ##########################
get_qxt <- function(ea,retire) {
  # load age information
  age_information(ea,retire)
  # minumum age in the 2003 SOA Pension Plan Turnover Study (the select and ultimate table)
  min_qxt_age<<-18
  # empty vector of numeric type
  term_rate<-numeric()
  # creating termination rate vector based on entry age and years of service
  for(i in yos_xy){
    if(yos_xy[i+1]<2)
      term_rate<-c(term_rate,term_rate_xl$`Service<2`[ea-min_qxt_age+1+i]/100)
    else if(yos_xy[i+1]>=2 && yos_xy[i+1]<=4)
      term_rate<-c(term_rate,term_rate_xl$`Service=2,3,4`[ea-min_qxt_age+1+i]/100)
    else if(yos_xy[i+1]>=5 && yos_xy[i+1]<=9)
      term_rate<-c(term_rate,term_rate_xl$`Service=5-9`[ea-min_qxt_age+1+i]/100)
    else
      term_rate<-c(term_rate,term_rate_xl$`Service>=10`[ea-min_qxt_age+1+i]/100)
  }
  term_rate[is.na(term_rate)]<-0
  return(term_rate)
}

######### Probablity to continue to next year ###############
get_xpxT <- function(ea,retire,mort) {
  xpmx <- c(get_xpmx(ea,retire,mort)[1:length(age)])
  # product of (1-termination rate) and (1-mortality rate)
  xpxT <- xpmx * (1 - get_qxt(ea,retire))
  return(xpxT)
}

########## Probablity to continue to 65 #####################
get_rxpxT <- function(ea,retire,mort) {
  # product of (1-termination rate) and (1- cumulative mortality rate)
  rxpxT<-get_rpmx(ea,retire,mort)* (1 - get_qxt(ea,retire))
  return(rxpxT)
}

#######################################################################################################
############################### Annuity functions ####################################################
######################################################################################################

############ Straight Life Annuity factor for 1 dollar ##############
get_ar <- function(ea,retire,i,cola,mort) {
  # sum of product of discount factor after retirement, probablity 
  #of survival after 65, cola and years of service after retirement 
  ar <-
    sum(get_vxr(ea,retire,i)[(length(get_vrx(ea,retire,i))):length(get_vxr(ea,retire,i))] * 
          c(get_rpmx(ea,retire,mort)[length(get_rpmx(ea,retire,mort))],na.omit(get_xpmr(ea,retire,mort))) *
          ((1 + cola) ^ yos_r_max_mort))
  
  return(ar)
}

############ period certain Annuity ##############
get_am <- function(ea,retire,i,amortization) {
  vxr <- get_vxr(ea,retire,i)
  # sum of discount factor from entry age to entry age + amortization period
  am <- sum(vxr[1:(amortization - 1)])
  return(am)
}

############# Temporary Annuity for a certain entry age ##############
get_tla <- function(ea,retire,i,sgr,mort,a){
  
  # age vector till retirement age-1
  age <- age[1:(length(age) - 1)]
  
  # empty vectors for survival probablity,discount vector and (current salary/entry salary) at age 'a'
  np <- numeric(length(age))
  v <- numeric(length(age))
  sts <- numeric(length(age))
  np <- v <- sts <- c(rep.int(0,(length(age))))
  xpmx <- get_xpmx(ea,retire,mort)[1:length(age)]
  
  # generating vectors for survival probablity,discount vector and (current salary/entry salary) at age 'a'
  if (a >= ea) {
    np[a - ea + 1] <- v[a - ea + 1] <- sts[a - ea + 1] <- 1
    for (j in (a - ea + 1):(length(age))) {
      np[j + 1] = xpmx[j] * np[j]
      v[j + 1] = v[j] / (1 + i)
      sts[j + 1] = sts[j] * (1 + sgr)
    }
  }
  
  # generating temporary ammuity
  tla <- sum((np * v * sts)[1:length(age)])
  
  return(tla)
}

############## Temporary Annuity ##################
get_tla_t <- function(ea,retire,i,sgr,mort) {
  age_information(ea,retire)
  tla_t <- as.numeric(length(age))
  # calling the temporary annuity function for all ahes betwwen entry age and retirement age
  for (k in 1:(length(age) - 1))
    tla_t[k] <- c(get_tla(ea,retire,i,sgr,mort,age[k]))
  return(tla_t)
}

########################  Annuity after r ##############################
get_a_after_r <- function(ea,retire,i,cola,mort) {
  # cumulative sum of product of discount factor after retirement, probablity 
  #of survival after 65, cola and years of service after retirement 
  
  after_ar <-
    rev(cumsum(rev(get_vxr(ea,retire,i)[length(get_vrx(ea,retire,i)):length(get_vxr(ea,retire,i))] * 
                     c(get_rpmx(ea,retire,mort)[length(get_rpmx(ea,retire,mort))],get_xpmr(ea,retire,mort)) *
                     ((1 + cola) ^ yos_r_max_mort))))
  
  return(after_ar[2:length(after_ar)])
}

###################### Replacement rate ##########################
get_replacement_rate <- function(ea,retire,a_sgr,sgr,afc,bf) {
  acc_benefit_65<-get_acc_benefit_65(ea,retire,a_sgr,sgr,afc,bf)
  act_sal <- get_act_sal(ea,retire,a_sgr)
  # compute replacement rate , accumulated benefit at 65/salary at 65
  repl_rate<- round((acc_benefit_65/act_sal[length(act_sal)])*100,2)
  return(repl_rate)
}

################### Retirement Annuity ###########################
get_retirement_annuity<-function(ea,retire,a_sgr,sgr,afc,bf){
  # annuity at retirement
  acc_benefit<-get_acc_benefit_65(ea,retire,a_sgr,sgr,afc,bf)
  return(acc_benefit)
}

#############################################################################################
############################ Present value for future benefits ##############################
#############################################################################################

#################### PVFB from x to r ###################################
get_rPVFBx <- function(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort) {
  # product of discount factor to retirement age ,probablity to continue 
  # to retirement age,accumulated benefits at 65 and annuity factor for a dollar 
  rPVFBx <-
    get_vrx(ea,retire,i) * get_rxpxT(ea,retire,mort) * get_acc_benefit_r(ea,retire,a_sgr,sgr,afc,bf) * get_ar(ea,retire,i,cola,mort)
  return(rPVFBx)
}


############################ Present value Future benefits after retirement  ############################
get_rPVFBx_after_r <- function(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort) {
  a_after_r <- get_a_after_r(ea,retire,i,cola,mort)
  # product of accumulated benefit at 65 and annuity factor for one dollar after retirement
  rPVFBx <- get_acc_benefit_65(ea,retire,a_sgr,sgr,afc,bf) * a_after_r
  
  return(rPVFBx)
}

##################################################################################
######################### grading function #######################################
##################################################################################
get_gxv <- function(ea,retire,vesting) {
  ### repeat zeros for vesting period and a 100 from vesting+1 to retirement
  gxv <- c(rep(0,vesting), rep(100,(length(age) - vesting)))
  return(gxv)
}

##################################################################################
################################ Vesting cost ####################################
##################################################################################
get_vesting_cost <- function(ea,retire,i,a_sgr,cola,afc,bf,mort,vesting) {
  bx <- get_acc_benefits(ea,retire,a_sgr,afc,bf)
  # product of grading function , accumulated benefits and annuity factor
  vc <- (get_gxv(ea,retire,vesting) / 100) * bx * get_ar(ea,retire,i,cola,mort)
  return(vc)
}

###############################################################################
################################ term cost ####################################
###############################################################################
get_term_cost <- function(ea,retire,i,a_sgr,cola,afc,bf,mort,vesting) {
  rpmx <- get_rpmx(ea,retire,mort)
  rpmx <- c(rpmx[2:length(rpmx)],1)
  vc <- get_vesting_cost(ea,retire,i,a_sgr,cola,afc,bf,mort,vesting)
  # product of vesting cost, termination rate, probablity to live to 65 and discount vector
  tc <- vc * get_qxt(ea,retire) * rpmx * get_vrx(ea,retire,i)
  return(tc)
}

###############################################################################
############################ PVTC #############################################
###############################################################################

###################### PVTC for a certain age #################################
get_PVTC <- function(ea,retire,i,a_sgr,cola,afc,bf,mort,vesting,a){
  age <- age[1:(length(age) - 1)]
  # term cost
  tc <- get_term_cost(ea,retire,i,a_sgr,cola,afc,bf,mort,vesting)
  # creating empty vector for probablity of surviving to next year,discount factor
  np <- numeric(length(age))
  v <- numeric(length(age))
  np <- v <- c(rep.int(0,(length(age))))
  
  # probablitiy to continue to next year
  xpmx <- get_xpmx(ea,retire,mort)[1:length(age)]
  
  # computing vectors for probablity of surviving to next year,discount factor
  if (a >= ea) {
    np[a - ea + 1] <- v[a - ea + 1] <- 1
    for (j in (a - ea + 1):(length(age))) {
      np[j + 1] = xpmx[j] * np[j]
      v[j + 1] = v[j] / (1 + i)
    }
  }
  
  # PVTC for age 'a'
  pvtc <- sum(np * v * tc)
  return(pvtc)
}

############ PVTC for all ages between entry age and retirement ################
get_PVTC_t <- function(ea,retire,i,a_sgr,cola,afc,bf,mort,vesting){
  age_information(ea,retire)
  pvtc_t <- as.numeric(length(age))
  for (k in 1:(length(age) - 1))
    pvtc_t[k] <-
    c(get_PVTC(ea,retire,i,a_sgr,cola,afc,bf,mort,vesting,age[k]))
  return(c(pvtc_t,0))
}

####################################################################
########################## Statistics ##############################
####################################################################

######################################
#after_max_age <- pop_after_max_age

# store max after retiree population frequency # 02212017
#after_max_age_pop<-after_max_age
###############################################
#added after_max_age

######################## Age group stats ###########################

get_stat<-function(ea,retire,pop_type,pop_size,median,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting){
  # store max after retiree population frequency # 02212017
  total_pop<-get_total_pop(ea,retire,pop_type,pop_size,median)
  # store active population
  active_pop<-total_pop[1:(retire - ea + 1)]
  # store retiree population frequency
  retire_pop<-total_pop[(retire - ea + 2): (max_age - ea +1)]
  # get payroll, a repitition of 0's is added after 65
  payroll<-c(get_act_sal(ea,retire,a_sgr)*active_pop,rep(0, (max_mort_age - retire)))
  # get normal cost, a repitition of 0's is added after 65
  nc<-get_NC(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)*total_pop
  # get actuarial accrued liability
  #aal<-c(active_pop,retire_pop)*get_AAL(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)
  aal<-total_pop*get_AAL(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)
  # create a data fram with age, payroll,normal cost and aal
  #dat<-data.frame(Age=seq(ea,max_age),Payroll=payroll,"Normal Cost"=nc,AAL=aal)-022120
  dat<-data.frame(Age=seq(ea,max_mort_age),Payroll=payroll,"Normal Cost"=nc,AAL=aal)
  # calculate total payroll
  total_payroll<-sum(payroll)
  # calculate total normal cost
  total_nc<-sum(nc)
  # calculate total AAL
  total_aal<-sum(aal)
  # split the data frame according to age groups
  stat<-split(dat,cut(dat$Age,age_grp_labels(ea,max_mort_age),include.lowest = TRUE))
  # create a new variable stats
  stats<-NULL
  for(j in 1:length(stat)){
    ## conditional statement to check if label has '(','-' or ')' and replace them
    if(grepl('\\(',names(stat)[j])){
      num<-as.numeric(substr(x = names(stat)[j],2,3))
      names(stat)[j]<-gsub(num,num+1,names(stat)[j])
    }
    names(stat)[j]<-gsub(',','-',names(stat)[j])
    names(stat)[j]<-gsub('\\(','\\[',names(stat)[j])
    
    ## store stat in stats as a collective summary for each age group
    stats<-rbind(stats,data.frame(names(stat)[j], 
                                  (sum(stat[[j]]$Payroll)/total_payroll)*100,
                                  (sum(stat[[j]]$Normal.Cost)/total_nc)*100,
                                  (sum(stat[[j]]$AAL)/total_aal)*100))
  }
  ## assign column names to the data frame generated
  names(stats)<-c("Age Group","Salary/Total Salary (%)","Normal Cost/Total Normal Cost (%)","AAL/Total AAL (%)")
  return(stats)
}

########################### Summary of cost for the plan ###########################
get_summary<-function(pop,ea,retire,median_p,median_dr,median_sgr,i,a_sgr,sgr,pgr,cola,afc,bf,cm,mort,vesting,amortization){
  # store fundinf level
  percent<-percent(median_p)
  # store total AAL(changed max_age to max_mort_age)
  total_aal<-sum(pop* get_AAL(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting))
  # store total normal cost
  total_nc<-sum(get_NC(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)*pop)
  # store total assets(changed max_age to max_mort_age)
  total_assets<-sum(pop*get_median_asset(ea,retire,median_p,median_dr,median_sgr,a_sgr,cola,afc,bf,cm,mort,vesting))
  # store total UAAL
  total_uaal<-total_aal-total_assets
  # store total ARC
  total_adc<-sum(get_ARC(pop,ea,retire,median_p,median_dr,median_sgr,i,a_sgr,sgr,pgr,cola,afc,bf,cm,mort,vesting,amortization))
  # store total Payroll
  payroll<-sum(c(get_act_sal(ea,retire,a_sgr), rep(0, max_mort_age - retire))*pop)
  # store NC/Payroll for plan
  nc_pay<-percent(total_nc/payroll)
  # store UAAL/Payroll for plan
  uaal_pay<-percent(total_uaal/payroll)
  # store ARC/Payroll for plan
  arc_pay<-percent(total_adc/payroll)
  # convert to dollar format
  total_aal<-dollar(total_aal)
  total_nc<-dollar(total_nc)
  total_uaal<-dollar(total_uaal)
  total_adc<-dollar(total_adc)
  # vector of plan costs
  sum_elem<-c("Funding Ratio","Total AAL","Total Normal Cost","Total UAAL","Total ADC","NC/Payroll","UAAL/Payroll","ARC/Payroll")
  # vector of plan cost values
  sum_val<-c(percent,total_aal,total_nc,total_uaal,total_adc,nc_pay,uaal_pay,arc_pay)
  # generating summary data frame
  summary<-as.data.frame(cbind(sum_elem,sum_val))
  names(summary)<-c("Plan Costs","Estimate")
  return(summary)
}


##################################################################################
############################## AAL for cost methods ##############################
##################################################################################
get_AAL <- function(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting) {
  # load age information
  age_information(ea,retire)
  rPVFBx <- get_rPVFBx(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort)
  pvtc <- get_PVTC_t(ea,retire,i,a_sgr,cola,afc,bf,mort,vesting)
  # k is common factor in the AAL calculations for both cost moethods
  k <- rPVFBx + pvtc
  
  # conditional check and AAL calculation for different cost methods
  if (cm == 'EAN')
    # AAL for EAN = (rPVFBx + pvtc) - (NC * Temporary annuity)
    aal <-
    k - (get_NC(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)[1:(retire -ea + 1)] * c(get_tla_t(ea,retire,i,sgr,mort),get_tla(ea,retire,i,sgr,mort,age[length(age)])))
  else
    # AAL for PUC = (rPVFBx + pvtc) * (years of service/ years to retirement)
    aal <- k * ((yos_xy) / (age[length(age)] - age[1]))
  #return(c(aal,get_rPVFBx_after_r(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort)))
return(c(aal,get_rPVFBx_after_r(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort)))
#return(c(aal,get_rPVFBx_after_r(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort)[1:(max_age-retire)]))
}

#### Function to generate pie chart data for total AAL ####
#### NOT USED IN THE CURRENT MODEL ####
get_aal_pop<-function(pop,ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting){
  # creating a data frame with age and aal columns
  dat<-data.frame(Age=seq(ea,max_age),aal=pop*get_AAL(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting))
  # split the data frame as per age groups into a list of smaller dataframes 
  AAL_POP<-split(dat,cut(dat$Age,age_grp_labels(ea,max_age),include.lowest = TRUE))
  #defining aal for first age group
  aal_sum<-sum(AAL_POP[[1]]$aal.Freq)
  #defining aal for thre remaining age groups
  for(i in 2:length(names(AAL_POP)))
    aal_sum<-c(aal_sum,sum(AAL_POP[[i]]$aal.Freq))
  return(cbind(names(AAL_POP),aal_sum))
}



##################################################################################
############################## NC for cost methods ##############################
get_NC <- function(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting) {
  age_information(ea,retire)
  rPVFBx <- get_rPVFBx(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort)
  pvtc <- get_PVTC_t(ea,retire,i,a_sgr,cola,afc,bf,mort,vesting)
  exp_sal <- get_exp_sal(ea,retire,sgr)
  # k is common factor in the NC calculations for both cost moethods
  k <-(rPVFBx[1] + pvtc[1]) / (exp_sal[1] * get_tla_t(ea,retire,i,sgr,mort)[1])
  # conditional check and normal cost calculation for different cost methods
  if (cm == 'EAN'){
    # NC formula simulation excel sheet uses PVTC,rPVFBx at entry age 
    nc <- (k) * exp_sal
  }
  else{
    # Normal cost for PUC = (rPVFBx + pvtc) /  years to retirement
    nc <- (rPVFBx+ pvtc) / (age[length(age)] - age[1])
  }
  return(c(nc,rep(0, max_mort_age - retire)))
}
#################################################################################

#### Function to generate pie chart data for total Normal cost ####
#### NOT USED IN THE CURRENT MODEL ####

get_nc_pop<-function(pop,ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting){
  # creating a data frame with age and normal cost columns
  dat<-data.frame(Age=seq(ea,retire),nc=pop*get_NC(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting))
  # split the data frame as per age groups into a list of smaller dataframes 
  NC_POP<-split(dat,cut(dat$Age,age_grp_labels(ea,retire),include.lowest = TRUE))
  #defining aal for first age group
  nc_sum<-sum(NC_POP[[1]]$nc.Freq)
  #defining aal for thre remaining age groups
  for(i in 2:length(names(NC_POP)))
    nc_sum<-c(nc_sum,sum(NC_POP[[i]]$nc.Freq))
  return(cbind(names(NC_POP),nc_sum))
}

###########################################################################################
######################## Median asset conditions ##########################################
###########################################################################################
get_median_asset <-
  function(ea,retire,median_p,median_dr,median_sgr,a_sgr,cola,afc,bf,cm,mort,vesting) {
    # median_p is the funding level set by the user
    median_sgr<-a_sgr
    # getting median asset
    median_asset <- median_p * get_AAL(ea,retire,median_dr,median_sgr,a_sgr,cola,afc,bf,cm,mort,vesting)
    return(median_asset)
  }


############################################################################
############################## Funding ratio ###############################
############################################################################
get_FR <-
  function(pop,ea,retire,median_p,median_dr,median_sgr,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting) {
    # load age information
    age_information(ea,retire)
    # get median asset
    median_asset<-get_median_asset(ea,retire,median_p,median_dr,median_sgr,a_sgr,cola,afc,bf,cm,mort,vesting)
    # get AAL as per cost method
    aal<-get_AAL(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)
    # funding ratio: Assets/AAL
    fr <- sum(pop * median_asset) / sum(pop * aal)
    return(fr)
  }

################################################################################################
############################### Annual Required Contribution ###################################
################################################################################################
get_ARC <-
  function(pop,ea,retire,median_p,median_dr,median_sgr,i,a_sgr,sgr,pgr,cola,afc,bf,cm,mort,vesting,amortization) {
    # Load age information
    age_information(ea,retire)
    # get active population
    pop<-c(as.vector(pop)[1:length(age)], rep(0, max_mort_age - retire))
    # to check discount rate - growth rate is 0 then set variable new_pgr value to avoid spikes 
    # in the discount rate sensitivity graphs for ARC
    new_pgr<-ifelse(i-pgr==0,0.00009,0)
    # get normal cost
    nc<- get_NC(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)
    # get AAL 
    aal<-get_AAL(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)
    # get assets
    median_asset<-get_median_asset(ea,retire, median_p,median_dr,median_sgr,a_sgr,cola,afc,bf,cm,mort,vesting)
     # get Unfunded liability
    uaal_pay<-(aal-median_asset)
    pmt<- get_PMT(i,amortization,uaal_pay)
    # compute ARC
    arc<- pop * (nc + ((pmt/(i-pgr+new_pgr))*(1-(((1+pgr)/(1+i))^amortization))))
    return(na.omit(arc))
  }


###################### PMT Excel function #########################
get_PMT <- function(rate, nper,pv, fv=0, type=0){
  pmt = (rate*(fv+pv*(1+ rate)^nper))/((1+rate*type)*(1-(1+ rate)^nper))
 return(-pmt)
}
