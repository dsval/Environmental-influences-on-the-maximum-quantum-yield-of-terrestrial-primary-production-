#2. Calculations
source("1_Define_functions.R")
##calc phi_0 
###AMERIFLUX
###################################################################################################
# 1. load the data, subset where ppfd below canopy is available
################################################################################################
##load sites
# ameriflx.sites<-read.csv("C:/Users/Silwood.SPHB-XLAP-075/OneDrive - Imperial College London/Documents/papers/3_Temperature_dependence_intricsic_quantum_yield/data/Ameriflux_sites_mapped.csv", stringsAsFactors = FALSE)
# ##load metadata
# BADM <- read.table(file = "C:/dsval/Ameriflux/AMF_AA-Net_BIF_LEGACY_20211111.csv", header = TRUE, sep = ",", fileEncoding = "windows-1252", quote = "\"", stringsAsFactors = FALSE, comment.char = "", na.strings = "")
# ##load filenames
filenames.ameriflux<- list.files(path="X:/projects/leverhulme_wildfires_life_sciences/live/data/Ameriflux", pattern = ".*._H.*.csv$", full.names=TRUE)
# ##exclude sites where  canopy is low (grasslands, crops) adn ppdf below canopy appears
# ameriflx.ppfd_bc<-subset(ameriflx.sites,
# 					!(ameriflx.sites$Vegetation.Abbreviation..IGBP. %in% c('CRO','GRA'))
# 					& ameriflx.sites$avail_bbfd_bc)
# ## subset sites wuere direct fapar is measured
# ameriflx.fapar<-subset(ameriflx.sites,ameriflx.sites$avail_fapar)
# ### merge sites
# avail_sites<-rbind(ameriflx.ppfd_bc,ameriflx.fapar)
# avail_sites=avail_sites[order(avail_sites$Site.Id) , ]
# ##### subset filenames where ppfd_bc/fapar data available
# write.csv(avail_sites,"C:/Users/Silwood.SPHB-XLAP-075/OneDrive - Imperial College London/Documents/papers/3_Temperature_dependence_intricsic_quantum_yield/data/avail_sites_amf.csv")
#load(file="C:/Users/Silwood.SPHB-XLAP-075/OneDrive - Imperial College London/Documents/papers/3_Temperature_dependence_intricsic_quantum_yield/data/phi_0_ame_Tleaf.RData")
avail_sites<-read.csv("C:/Users/Silwood/OneDrive - Imperial College London/Documents/papers/3_Temperature_dependence_intricsic_quantum_yield/data/avail_sites_amf.csv", stringsAsFactors = FALSE)
sites_files<-do.call(rbind,strsplit(basename(filenames.ameriflux),'_'))[,2]
filenames.ameriflux<-subset(filenames.ameriflux,sites_files %in% avail_sites$Site.Id)
load(file="X:/projects/leverhulme_wildfires_life_sciences/live/data/Ameriflux/sites_fapar/phi_0_AMF_Tair_HYP.RData")

###################################################################################################
# 02. estimate phi_0 
###################################################################################################
# test2<-get_phi_0_T(filenames.ameriflux[2],avail_sites$Elevation..m.[2],Tleaf = FALSE)
# ##estimate phi0 with leaf temperature
# library(parallel)
# cl<-makeCluster(4,'SOCK')
# parallel:::clusterEvalQ(cl, lapply(c('data.table','rpmodel','REddyProc'), library, character.only = TRUE))
# parallel:::clusterExport(cl, c("get_phi_0_T","filenames.ameriflux","avail_sites"),envir=environment())
# phi_0_ame_Tleaf<-parallel:::clusterMap(cl = cl, fun=get_phi_0_T, filename=filenames.ameriflux,elev=avail_sites$Elevation..m.,MoreArgs = list(Tleaf = TRUE))
# names(phi_0_ame_Tleaf)<-avail_sites$Site.Id
# save(phi_0_ame_Tleaf,file="C:/Users/Silwood.SPHB-XLAP-075/OneDrive - Imperial College London/Documents/papers/3_Temperature_dependence_intricsic_quantum_yield/data/phi_0_ame_Tleaf.RData")
# # #####################################################
# # ###estimate phi0 with air temperature
# phi_0_ame_Tair<-parallel:::clusterMap(cl = cl, fun=get_phi_0_T, filename=filenames.ameriflux,elev=avail_sites$Elevation..m.,MoreArgs = list(Tleaf = FALSE))
# names(phi_0_ame_Tair)<-avail_sites$Site.Id
# save(phi_0_ame_Tair,file="C:/Users/Silwood.SPHB-XLAP-075/OneDrive - Imperial College London/Documents/papers/3_Temperature_dependence_intricsic_quantum_yield/data/phi_0_ame_Tair.RData")
# 
# stopCluster(cl)
##############################################################################################
###FLUX DATA KIT
##############################################################################################
#get phi0 fluxdatakit sites
library(rpmodel)
#library(rsplash)
library(xts)
##################################################################
###0.1. Load data
##################################################################
fluxkit.sites<-read.csv("X:/projects/leverhulme_wildfires_life_sciences/live/data/FLUXDATAKIT_FLUXNET/metadata/FluxData_kit_MD.csv", stringsAsFactors = FALSE)

# filenames.fluxkit<- list.files(path="/rds/general/user/ds6915/projects/leverhulme_wildfires_life_sciences/live/data/FLUXDATAKIT_FLUXNET", pattern = ".*.HH.*.csv$", full.names=TRUE)
filenames.fluxkit<- list.files(path="x:/projects/leverhulme_wildfires_life_sciences/live/data/FLUXDATAKIT_FLUXNET", pattern = ".*.HH.*.csv$", full.names=TRUE)
###################################################################################################
# 02. estimate phi_0 
###################################################################################################
# test2_nee<-get_phi_0_T_flx(filenames.fluxkit[112],fluxkit.sites$elev[112],use_Tsurf=FALSE, use_NEE=TRUE)
# test2_gpp<-get_phi_0_T_flx(filenames.fluxkit[112],fluxkit.sites$elev[112],use_Tsurf=FALSE, use_NEE=FALSE)
##estimate phi0 with leaf temperature
# library(parallel)
# cl<-makeCluster(24,'SOCK')
# parallel:::clusterEvalQ(cl, lapply(c('data.table','rpmodel','xts'), library, character.only = TRUE))
# parallel:::clusterExport(cl, c("get_phi_0_T_flx","filenames.fluxkit","fluxkit.sites"),envir=environment())
# # phi_0_FDK_GPP_tair<-parallel:::clusterMap(cl = cl, fun=get_phi_0_T_flx, filename=filenames.fluxkit,elev=fluxkit.sites$elev,MoreArgs = list(use_Tsurf=FALSE, use_NEE=TRUE))
# phi_0_FDK_NEE_tair<-parallel:::clusterMap(cl = cl, fun=get_phi_0_T_flx, filename=filenames.fluxkit,elev=fluxkit.sites$elev,MoreArgs = list(use_Tsurf=FALSE, use_NEE=TRUE))
# names(phi_0_FDK_NEE_tair)<-fluxkit.sites$site_id
# save(phi_0_FDK_NEE_tair,file="/rds/general/user/ds6915/projects/leverhulme_wildfires_life_sciences/live/data/FLUXDATAKIT_FLUXNET/phi_0_FDK_NEE_tair.RData")
# stopCluster(cl)
# gc()














##Fit humped shapes
#####AMERIFLUX
###################################################################################################
# 03. fit optimum cuadratic
###################################################################################################
phi_0_fTair_nls<-lapply(phi_0_AMF_Tair_HYP,get_opt_phi)
phi_0_fTair_nls<-do.call(rbind,phi_0_fTair_nls)
###################################################################################################
# 03. fit Arrhenius curve
###################################################################################################
phi_0_fTair_arr_v1<-lapply(phi_0_AMF_Tair_HYP,wrap_optim_arr)
phi_0_fTair_arr_v1<-do.call(rbind,phi_0_fTair_arr_v1)
###correlations for dent and hd
Hd_lm<-lm(Hd~dent+0,data=phi_0_fTair_arr_v1)
plot(phi_0_fTair_arr_v1$dent,phi_0_fTair_arr_v1$Hd,ylim=c(0,400000))
abline(0,Hd_lm$coefficients[1])
###########################################################################
### second approximation
phi_0_fTair_arr_v2<-lapply(phi_0_AMF_Tair_HYP,wrap_optim_arr_v2)
phi_0_fTair_arr_v2<-do.call(rbind,phi_0_fTair_arr_v2)
##########################################################
### third approximation
phi_0_fTair_arr_v3<-lapply(phi_0_AMF_Tair_HYP,wrap_optim_arr_v3)
phi_0_fTair_arr_v3<-do.call(rbind,phi_0_fTair_arr_v3)
########################################################
##add Topt arrhenius
Rgas <- 8.3145 #J/mol/K
phi_0_fTair_arr_v1$Topt_arr<-(phi_0_fTair_arr_v1$Hd/(phi_0_fTair_arr_v1$dent-Rgas*log(phi_0_fTair_arr_v1$Ha/(phi_0_fTair_arr_v1$Hd-phi_0_fTair_arr_v1$Ha))))-273.15
#################################################################################################
# 03. add predicted values
###################################################################################################
phi_0_ame_Tair<-mapply(add_pred,phi_0_AMF_Tair_HYP,
	maxphi0=phi_0_fTair_arr_v1$maxphio,
	optemp=phi_0_fTair_nls$Topt,
	spread=phi_0_fTair_nls$r,
	Ha=phi_0_fTair_arr_v1$Ha,
	Hd=phi_0_fTair_arr_v1$Hd,
	dent=phi_0_fTair_arr_v1$dent,
	SIMPLIFY = F)

###################################################################################################
# 03. add basic info
###################################################################################################
#add site name
add_site<-function(df,site){
	df$site<-rep(site,length(df[,1]))
	df
}
phi_0_ame_Tair<-mapply(add_site,phi_0_ame_Tair,avail_sites$Site.Id,SIMPLIFY = F)
phi_0_ame_Tair.df<-do.call(rbind,phi_0_ame_Tair)

phi_0_ame_Tair.df<-merge(phi_0_ame_Tair.df,avail_sites,by.x='site',by.y='Site.Id')

#phi_0_ame_Tair.df<-subset(phi_0_ame_Tair.df, !is.na(phi_0_ame_Tair.df$phi_0))
length(unique(phi_0_ame_Tair.df$site))
###################################################################################################
# FLUX DATA KIT
###################################################################################################
###fit humped FDK
#load(file="X:/projects/leverhulme_wildfires_life_sciences/live/data/FLUXDATAKIT_FLUXNET/phi_0_FDK_GPP_tair.RData")
load(file="X:/projects/leverhulme_wildfires_life_sciences/live/data/FLUXDATAKIT_FLUXNET/phi_0_FDK_NEE_tair.RData")
fluxkit.sites<-read.csv("x:/projects/leverhulme_wildfires_life_sciences/live/data/FLUXDATAKIT_FLUXNET/metadata/FluxData_kit_MD.csv", stringsAsFactors = FALSE)
#phi_0_FDK_NEE_tair=phi_0_FDK_GPP_tair
###################################################################################################
# 03. fit optimum cuadratic
###################################################################################################
phi_0_fTair_fdk_nls<-lapply(phi_0_FDK_NEE_tair,get_opt_phi)
phi_0_fTair_fdk_nls<-do.call(rbind,phi_0_fTair_fdk_nls)
###################################################################################################
# 03. fit Arrhenius curve
###################################################################################################
Rgas <- 8.3145 #J/mol/K
phi_0_fTair_fdk_arr_v1<-lapply(phi_0_FDK_NEE_tair,wrap_optim_arr) #without geral pattern for hd~deltaS
phi_0_fTair_fdk_arr_v1<-do.call(rbind,phi_0_fTair_fdk_arr_v1)
################################

######################### second approximation
phi_0_fTair_fdk_arr_v2<-lapply(phi_0_FDK_NEE_tair,wrap_optim_arr_v2) #without geral pattern for hd~deltaS
phi_0_fTair_fdk_arr_v2<-do.call(rbind,phi_0_fTair_fdk_arr_v2)
phi_0_fTair_fdk_arr_v1$Topt_arr<-(phi_0_fTair_fdk_arr_v1$Hd/(phi_0_fTair_fdk_arr_v1$dent-Rgas*log(phi_0_fTair_fdk_arr_v1$Ha/(phi_0_fTair_fdk_arr_v1$Hd-phi_0_fTair_fdk_arr_v1$Ha))))-273.15

#################################################################################################
# 03. add predicted values
###################################################################################################
phi_0_fdk_Tair<-mapply(add_pred,phi_0_FDK_NEE_tair,
	maxphi0=phi_0_fTair_fdk_arr_v1$maxphio,
	optemp=phi_0_fTair_fdk_nls$Topt,
	spread=phi_0_fTair_fdk_nls$r,
	Ha=phi_0_fTair_fdk_arr_v1$Ha,
	Hd=phi_0_fTair_fdk_arr_v1$Hd,
	dent=phi_0_fTair_fdk_arr_v1$dent,
	SIMPLIFY = F)

#################################################################################################
# 03. merge
################################################################################################
add_site<-function(df,site){
	df$site<-rep(site,length(df[,1]))
	df
}
phi_0_fdk_Tair<-mapply(add_site,phi_0_fdk_Tair,fluxkit.sites$site_id,SIMPLIFY = F)
phi_0_fdk_Tair.df<-do.call(rbind,phi_0_fdk_Tair)

phi_0_fdk_Tair.df<-merge(phi_0_fdk_Tair.df,fluxkit.sites,by.x='site',by.y='site_id')
length(unique(phi_0_fdk_Tair.df$site))



#biogeo patterns
library(nlstools)
library(raster)
#########################################################################################################
####    load the data
#########################################################################################################
budy<-brick("X:/projects/leverhulme_wildfires_life_sciences/live/data/splash_1901-2020_CRUv4.05_monthly/Budyko_vars.nc");gc()
AI<-budy[[2]]
mGDD0<-raster("X:/home/WORK/data_input/mGDD0_1901-2018_CRUv405.nc");gc()

##extract geo
avail_sites$AI_spl<-as.numeric(extract(AI,cbind(x=avail_sites$Longitude..degrees.,y=avail_sites$Latitude..degrees.)))
fluxkit.sites$AI_spl<-as.numeric(extract(AI,cbind(x=fluxkit.sites$lon,y=fluxkit.sites$lat)))
avail_sites$CRU_mGDD0<-as.numeric(extract(mGDD0,cbind(x=avail_sites$Longitude..degrees.,y=avail_sites$Latitude..degrees.)))
fluxkit.sites$CRU_mGDD0<-as.numeric(extract(mGDD0,cbind(x=fluxkit.sites$lon,y=fluxkit.sites$lat)))
#################### replace data whith short records with splash simulations

avail_sites$record<-(avail_sites$Data.End-avail_sites$Data.Start)+1
########################## use climatology for sites with at least 5 years of record
avail_sites$AI_spl[avail_sites$record>=5 & !is.na(avail_sites$DI)]<-avail_sites$DI[avail_sites$record>=5 & !is.na(avail_sites$DI)]
fluxkit.sites$AI_spl[fluxkit.sites$nyrs>=5 & !is.na(fluxkit.sites$AI)]<-fluxkit.sites$AI[fluxkit.sites$nyrs>=5 & !is.na(fluxkit.sites$AI)]
####### combine all the data
######################################################################################
combined_arr_par<-rbind(phi_0_fTair_arr_v2,phi_0_fTair_fdk_arr_v2)
combined_arr_par$AI<-c(avail_sites$AI_spl,fluxkit.sites$AI_spl)
#combined_arr_par$EI<-c(avail_sites$EI,fluxkit.sites$EI)
combined_arr_par$mGDD0<-c(avail_sites$CRU_mGDD0,fluxkit.sites$CRU_mGDD0)
combined_arr_par$log10_mGDD0<-log10(combined_arr_par$mGDD0)
combined_arr_par$sites<-c(avail_sites$Site.Id,fluxkit.sites$site_id)
combined_arr_par$Biome<-c(avail_sites$Vegetation.Abbreviation..IGBP.,fluxkit.sites$biome)
combined_arr_par$source<-c(rep('in situ',length(avail_sites$AI_spl)),rep('RS',length(fluxkit.sites$AI_spl)))

#########################################################################################################
####    fit the params optimizer
#########################################################################################################
ai_eq_op<-wrap_optim_AI(subset(combined_arr_par,combined_arr_par$source=='in situ'))
mgdd0_eq_op<-wrap_optim_dent(subset(combined_arr_par,combined_arr_par$source=='in situ'))

#########################################################################################################
####    fit the params nls
#########################################################################################################

ds_mgdd0_nls<-nls(dent~dS0*mGDD0^(-dS_mgdd),data=na.omit(subset(combined_arr_par,combined_arr_par$source=='in situ')),start=c(dS0=1540,dS_mgdd=0.43370))

pre_dent<-propagate::predictNLS(ds_mgdd0_nls,newdata=data.frame(mGDD0=seq(1,40,1)),interval="confidence",nsim = 10000,alpha = 0.05)$summary

#########################################################################################################
####    prediction maps
#########################################################################################################
Rgas <- 8.3145            	# ideal gas constant J/mol/K
dS0 = mgdd0_eq_op$dS0_conf_50				# max entropy change(Sandoval et al., in.prep.)
dS_mgdd =mgdd0_eq_op$dS_mgdd_conf_50		# rate entropy change with temperature phio max (Sandoval et al., in.prep.)
Ha <- mean(combined_arr_par$Ha,na.rm=T)	      		# activation energy J/mol (Sandoval et al., in.prep.)



##map Phio max
maxphio_map<-(ai_eq_op$maxphio/(1+(AI)^ai_eq_op$m_conf_50)^ai_eq_op$n_conf_50)
#map dent
DeltaS = dS0*mGDD0^(-dS_mgdd )
##calc deaactivation energy J/mol (Sandoval et al., in.prep.)
Hd<- 295 *DeltaS
Topt<-(Hd/(DeltaS-Rgas*log(Ha/(Hd-Ha))))-273.15
gc()
#########################################################################################################
####    optimum temp
#########################################################################################################
# windows()
# plot(mGDD0,Topt,xlim=c(0,35),ylim=c(0,35))
# abline(0,1)

#points(fluxkit.sites$CRU_mGDD0,phi_0_fTair_fdk_nls$Topt,col=2)
