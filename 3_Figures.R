#3. Figures
############################################################################
#Fig 1
library("ggsci")
cols_intro<-pal_jco()(12)
pg_width=18
pg_heigth=24
#################################################################
##


#windows(pg_width/2.54,(pg_width/2.54)*0.33)
png("C:/Users/Silwood/OneDrive - Imperial College London/Documents/papers/3_Temperature_dependence_intricsic_quantum_yield/manuscript/figures_newPhyto/Fig1_phioT_literaturev2.png",width =pg_width,height    = pg_width*0.333,units     = "cm",res = 1200)
#windows(width = pg_width*0.5,height    = pg_width*0.5*0.55)
par(mfrow=c(1,2),mar=c(2,3,1,0.2),mgp=c(1.2,0.2,0),tcl=-0.1,mai=c(0.4,0.7,0.2,0.3))
#########################################################################################################
####plot non photorespiratory
#########################################################################################################
#Bernacchi2003
curve(0.087*(0.352 + 0.022 * x - 0.00034 * x^2),from=0,to=50,col=cols_intro[1],ylab=expression(phi1[0]~(mol~CO[2]~~mol^{-1}*photon)),xlab='',cex.lab=11/12,lwd=1.5,ylim=c(0,0.125),cex.axis=9/12)
#June2004
#curve(0.087*exp(-((x-33)/23.88)^2),from=0,to=50,add=T,col=cols_intro[2],lwd=1.5)
#Dwyer2007
curve(0.087*(-0.158 + 0.0297*x-0.000335*x^2),from=25,to=50,add=T,col=cols_intro[3],lwd=1.5)
#Yin 2014
curve(0.087*(0.4915 + 0.0237*x- 0.00053*x^2),add=T,col=cols_intro[4],lwd=1.5)
# FvCB  
phio_Fvcb_700<-calc_phi0_Farq(tc=0:50,elev=000,co2=750,f=0.15)
lines(0:50,phio_Fvcb_700,col='black',lwd=1.5,lty=2)
text =  c('Bernacchi et al. (2003)','Dwyer et al. (2007)','Yin et al. (2014)', 'von Caemmerer (2000)')
legend('topleft',legend=text,col=c(cols_intro[c(1,3:4)],'black'),lty=c(rep(1,3),2),lwd=rep(1.7,5),bty='o',title='',cex=5/12, y.intersp=0.75,x.intersp=0.5)
plotrix::corner.label(label=substitute(paste(bold('(a)'))),figcorner=T,cex=12/12,x=1)
dev.next()
#########################################################################################################
####plot photorespiratory
#########################################################################################################
#Zhang2006
curve((0.0089*exp(15406 * ( (1/298.15)-(1/(x+273.15)))))*(1/(1000*44*1e-6)),from=0,to=50,add=F,col=cols_intro[6],ylab=expression(phi1~(mol~CO[2]~~mol^{-1}*photon)),xlab='',cex.lab=11/12,lwd=1.5,ylim=c(0,0.125),cex.axis=9/12)
#Bibi2008
curve((0.0003*x^2 - 0.0234*x + 1.2255)*0.087,from=10,to=40,add=T,col=cols_intro[7],lwd=1.5)
#Rogers2019
#lines(c(5,15,25),phio_Rogers2019,col=cols_intro[8],lwd=1.5)
#Neri2024 trop tree "param-spreadsheet" file
a=0.742;m1=-22.401;s1=20.901;m2=49.983;s2=6.556;T_neri=0:60
lines(T_neri,0.087*a*0.5*(pracma::erf((T_neri-m1)/s1)+pracma::erf((m2-T_neri)/s2)),col=cols_intro[8],lwd=1.5)
#Neri2024 trop deciduos tree 
a=0.744;m1=6.999;s1=11.667;m2=49.319;s2=3.711;T_neri=0:60
lines(T_neri,0.087*a*0.5*(pracma::erf((T_neri-m1)/s1)+pracma::erf((m2-T_neri)/s2)),col=cols_intro[9],lwd=1.5)
#Neri2024 C4 grass
a=0.783;m1=-0.019;s1=13.745;m2=44.645;s2=7.99;T_neri=0:60
lines(T_neri,0.087*a*0.5*(pracma::erf((T_neri-m1)/s1)+pracma::erf((m2-T_neri)/s2)),col=cols_intro[10],lwd=1.5)
#curve((0.0003*x^2 - 0.0234*x + 1.2255)*0.087,from=10,to=40,add=T,col=cols_intro[8],lwd=1.5)
# FvCB  
phio_Fvcb_400<-calc_phi0_Farq(tc=0:50,elev=000,co2=400,f=0.15)
lines(0:50,phio_Fvcb_400,col='black',lwd=1.5,lty=2)
# legend('bottomright',legend=c('Zhang et al. (2006)','Bibi et al. (2008)','Rogers et al. (2019)', 'von Caemmerer (2000)'),col=cols_intro[6:9],lwd=rep(3,4),bty='o',title='',cex=5/12,y.intersp=0.75,x.intersp=0.5)
legend('topright',legend=c('Zhang et al. (2006)','Bibi et al. (2008)','Neri et al. (2024):BET-Tr','Neri et al. (2024):BDT-Tr','Neri et al. (2024):C4-G', 'von Caemmerer (2000)'),col=c(cols_intro[c(6:10)],'black'),lty=c(rep(1,5),2),lwd=rep(1.7,6),bty='o',title='',cex=5/12,y.intersp=0.75,x.intersp=0.5)
mtext(expression(T[Leaf]~(degree*C)), side = 1, outer = T, line = -0.83, las=0,cex=12/12)
plotrix::corner.label(label=substitute(paste(bold('(b)'))),figcorner=T,cex=12/12,x=1)
dev.off()


#Fig 2
#plot methods
library("ggsci")
#cols_intro<-pal_nejm(alpha=0.7)(4)
cols_intro<-scales::viridis_pal(alpha=0.3)(4)
cols_big<-scales::viridis_pal(alpha=1)(4)
pg_width=18
pg_heigth=24
#windows(pg_width/2.54,(pg_width/2.54)*0.33)
png("C:/Users/Silwood/OneDrive - Imperial College London/Documents/papers/3_Temperature_dependence_intricsic_quantum_yield/manuscript/figures_newPhyto/Fig2_methods.png",width =pg_width,height    = pg_width*0.333,units     = "cm",res = 1200)

layout(matrix(c(1,1,2), nrow = 1, ncol = 3, byrow = TRUE))
par(mar=c(2,3,1,0.2),mgp=c(2,0.2,0),tcl=-0.1,mai=c(0.4,0.5,0.2,0.3))
###################################################
plot(samples[[22]]$Iabs[samples[[22]]$Iabs>10],samples[[22]]$NEE[samples[[22]]$Iabs>10],
	ylab=expression(-NEE~(mu*mol~CO[2]~m^{-2}~s^{-1})),
	xlab=expression(I[abs]%*%m[j]~(mu*mol~photon~m^{-2}~s^{-1}) ),
	col=cols_intro[4],cex=0.3,pch=19,ylim=c(-5,30),xlim=c(0,2000),cex.lab=12/12)
points(max_LRC[[22]],col=cols_big[4],cex=0.7,pch=19)
curve((phi_o$phi_0[22]*x*(1/sqrt(1+(phi_o$phi_0[22]*x/max(max_LRC[[22]]$y,na.rm=T))^2)))-phi_o$Reco_I_0[22],add=T,col=cols_big[4])	
###################################################
points(samples[[13]]$Iabs[samples[[13]]$Iabs>10],samples[[13]]$NEE[samples[[13]]$Iabs>10],col=cols_intro[3],cex=0.3,pch=19)
points(max_LRC[[13]],col=cols_big[3],cex=0.7,pch=19)
curve((phi_o$phi_0[13]*x*(1/sqrt(1+(phi_o$phi_0[13]*x/max(max_LRC[[13]]$y,na.rm=T))^2)))-phi_o$Reco_I_0[13],add=T,col=cols_big[3])	
###################################################
points(samples[[11]]$Iabs[samples[[11]]$Iabs>10],samples[[11]]$NEE[samples[[11]]$Iabs>10],col=cols_intro[1],cex=0.3,pch=19)
points(max_LRC[[11]],col=cols_big[1],cex=0.7,pch=19)
curve((phi_o$phi_0[11]*x*(1/sqrt(1+(phi_o$phi_0[11]*x/max(max_LRC[[11]]$y,na.rm=T))^2)))-phi_o$Reco_I_0[11],add=T,col=cols_big[1])
legend('topleft',legend=c("10°C \u2264 T < 11°C",
	"12°C \u2264 T < 13°C",
	"21°C \u2264 T < 22°C"),
	col=cols_big[c(1,3,4)],lwd=rep(1.,3),bty='o',title='',cex=10/12,y.intersp=0.75,x.intersp=0.5)	
plotrix::corner.label(x=-1,label=substitute(paste(bold('(a)'))),figcorner=T,cex=12/12)
###################################################

plot(phi_o$T_sample,phi_o$phi_0,xlim=c(0,40),pch=19,col='grey',
	ylab=expression(phi1[0]~(mol~CO[2]~~mol^{-1}*photon)),
	xlab=expression(T[air]~(degree*C),cex.lab=12/12)
	)
points(phi_o$T_sample[11],phi_o$phi_0[11],pch=19,col=cols_big[1])
segments(phi_o$T_sample[11],0,phi_o$T_sample[11],phi_o$phi_0[11],col=cols_big[1],lty=3,lwd=0.5)
segments(0,phi_o$phi_0[11],phi_o$T_sample[11],phi_o$phi_0[11],col=cols_big[1],lty=3,lwd=0.5)
###################################################
points(phi_o$T_sample[13],phi_o$phi_0[13],pch=19,col=cols_big[3])
segments(phi_o$T_sample[13],0,phi_o$T_sample[13],phi_o$phi_0[13],col=cols_big[3],lty=3,lwd=0.5)
segments(0,phi_o$phi_0[13],phi_o$T_sample[13],phi_o$phi_0[13],col=cols_big[3],lty=3,lwd=0.5)
###################################################
points(phi_o$T_sample[22],phi_o$phi_0[22],pch=19,col=cols_big[4])
segments(phi_o$T_sample[22],0,phi_o$T_sample[22],phi_o$phi_0[22],col=cols_big[4],lty=3,lwd=0.5)
segments(0,phi_o$phi_0[22],phi_o$T_sample[22],phi_o$phi_0[22],col=cols_big[4],lty=3,lwd=0.5)
plotrix::corner.label(x=-1,label=substitute(paste(bold('(b)'))),figcorner=T,cex=12/12)
dev.off()
#

#Fig 3 separated
###################################################################################################
# Figure 1. intro models empirical leaf level
###################################################################################################
library("ggsci")
library(dplyr)
library(ggdist)
library(ggplot2)
library(patchwork)
library(gridGraphics)
cols_intro<-pal_jco(alpha = 0.5)(6)
palette_amf_flx<-c(cols_intro[1],cols_intro[2])
pg_width=18
pg_heigth=24
cm2in =0.393701

###################################################################################################
# Figure 2a. basic stats
###################################################################################################

phi_0_fdk_Tair.df$AI[is.infinite(phi_0_fdk_Tair.df$AI)]<-NA

boxplotdata_phi0<-data.frame(phi_0=c(phi_0_ame_Tair.df$phi_0,phi_0_fdk_Tair.df$phi_0),
	Tair=c(phi_0_ame_Tair.df$T_sample,phi_0_fdk_Tair.df$T_sample),
	Biome=as.character(c(phi_0_ame_Tair.df$Vegetation.Abbreviation..IGBP.,phi_0_fdk_Tair.df$biome)),
	Climate=as.character(c(phi_0_ame_Tair.df$Climate.Class.Abbreviation..Koeppen.,phi_0_fdk_Tair.df$clim_zone)),
	fAPAR=c(rep('in situ',length(phi_0_ame_Tair.df$Climate.Class.Abbreviation..Koeppen.)),rep('RS',length(phi_0_fdk_Tair.df$clim_zone))),
	sites=c(phi_0_ame_Tair.df$site,phi_0_fdk_Tair.df$site),stringsAsFactors=F)
###fix inconsistencies
boxplotdata_phi0$Climate[boxplotdata_phi0$Climate=='Bsk']<-'BSk'
##move Douglas lake to Wetlands
boxplotdata_phi0$Biome[boxplotdata_phi0$Biome=='WAT']<-'WET'
boxplotdata_phi0<-rmOutliers(boxplotdata_phi0)
# Calculate the number of unique sites per ecosystem
site_counts <- aggregate(sites ~ Biome, data = boxplotdata_phi0, function(x) length(unique(x)))
####################################################################################################
####################################################################################################
clds_phi_0<-get_cld(boxplotdata_phi0,'phi_0','Biome')

vplt1<-{	ggplot(data=boxplotdata_phi0,aes(x=reorder(Biome,-phi_0,na.rm = TRUE), y=phi_0,color=fAPAR,fill=fAPAR)) +
		theme_bw()+
		ylim(0,0.1) +
		ggdist::stat_halfeye(adjust = 0.67, ## bandwidth
		width = .9, 
		alpha = 0.5,
		color = NA, ## remove slab interval
		position = "identity"
		)+
		theme(text = element_text(size=12)) +
		scale_color_manual(values = palette_amf_flx) +
		scale_fill_manual(values = palette_amf_flx) +
		coord_cartesian(clip = "off")+
		scale_y_continuous(limits = c(0,0.11))
}

png("C:/Users/Silwood/OneDrive - Imperial College London/Documents/papers/3_Temperature_dependence_intricsic_quantum_yield/manuscript/figures_newPhyto/Fig3_a.png",width =pg_width,height    = pg_heigth/4,units     = "cm",res = 1200)

vplt1+
	geom_point(inherit.aes = FALSE,data = clds_phi_0,aes(x = group,y=phi_0),color = 'black', size = 1)+
	geom_errorbar(inherit.aes = FALSE,data = clds_phi_0, aes(x = group, ymin = phi_0 - sd, ymax = phi_0 + sd)
	,color = 'black', width = 0.2)+ 
	geom_text(inherit.aes = FALSE,data = clds_phi_0, aes(x = group, y = (phi_0 + sd), label = Letters), size = 2.5,
	vjust=-0.3, hjust =1.2, color = "black")+
	geom_text(inherit.aes = FALSE,data = site_counts, aes(x = Biome, y = 0.11, label = paste0("n=", sites)), size = 2,
	, color = "black")+
	labs(y = expression(phi1[0]~(mol~CO[2]~~mol^{-1}*photon)),	x = expression(Biome),tags='(a)')+
	theme(plot.tag = element_text(face = 'bold',size=12))
dev.off()	
	
####################################################################################################

###################################################################################################
# Figure 2b and 2c
###################################################################################################

png("C:/Users/Silwood/OneDrive - Imperial College London/Documents/papers/3_Temperature_dependence_intricsic_quantum_yield/manuscript/figures_newPhyto/Fig3b_c.png",width =pg_width,height    = pg_heigth/3.5,units     = "cm",res = 1200)
#windows(width =pg_width,height    = pg_heigth/4,units     = "cm")
par(mfrow=c(1,2),mar=c(2,2.7,0.5,2.5),mgp=c(0.5,0.2,0),tcl=-0.1)
##########amf######xlim=c(-0.5,1.5) xlim=c(0,15)
plot.scat.dens_col(log10(phi_0_ame_Tair.df$DI[phi_0_ame_Tair.df$Vegetation.Abbreviation..IGBP.!='CRO']),
	(phi_0_ame_Tair.df$phi_0[phi_0_ame_Tair.df$Vegetation.Abbreviation..IGBP.!='CRO']),
	phi_0_ame_Tair.df$zone[phi_0_ame_Tair.df$Vegetation.Abbreviation..IGBP.!='CRO'],
	legendpos='topright',alpha=20,cex=0.6,cex.axis=10/12,colarrows='gray40',cex.stats=6/12,
	ylim=c(0,0.125),xlim=c(-0.5,1.2),
	xlab='',
	ylab='',cex.lab=12/12,main='')
plotrix::corner.label(label=substitute(paste(bold('(b)'))),figcorner=T,cex=12/12)
mtext(expression(phi1[0]~(mol~CO[2]~~mol^{-1}*photon)), side = 2, outer = F, line = 1,cex=12/12)
##########flx######
plot.scat.dens_col(log10(phi_0_fdk_Tair.df$AI[phi_0_fdk_Tair.df$biome!='CRO']),
	(phi_0_fdk_Tair.df$phi_0[phi_0_fdk_Tair.df$biome!='CRO']),
	phi_0_fdk_Tair.df$clim_zone[phi_0_fdk_Tair.df$biome!='CRO'],
	legendpos='topright', alpha=20,cex=0.6,cex.axis=10/12,colarrows='gray40',cex.stats=6/12,
	ylim=c(0,0.125),xlim=c(-0.5,1.2),
	xlab='',
	ylab='',cex.lab=12/12,main='')
plotrix::corner.label(label=substitute(paste(bold('(c)'))),figcorner=T,cex=12/12)
#put.fig.letter(label='(a)', location='topleft', font=2,cex=1.5)
#dev.next()log='x',
#par(mar=c(4,4.2,3,1),mgp=c(2.8,1,0)) xlim=c(0.0,12)
#put.fig.letter(label='(b)', location='topleft', font=2,cex=1.5)
mtext(expression(log[10]~AI~('-')), side = 1, outer = T, line = -0.8, las=0,cex=12/12)
dev.off()

#Figs 4 separated
library("ggsci")
library(ggplot2)
col_percs<-pal_jco()(6)

single_plot<-ggplot(boxplotdata_phi0,aes(x = Tair, y = phi_0),color='black') +
	geom_point(size=0.05,alpha=0.2)+
	stat_summary(fun.y = mean,geom = "point",col=col_percs[1],size=0.7)+
	stat_summary(fun.y = function(x){quantile(x,0.75,na.rm=T)},geom = "point",col=col_percs[2],size=0.7)+
	stat_summary(fun.y = function(x){quantile(x,0.95,na.rm=T)},geom = "point",col=col_percs[4],size=0.7)+
	labs(y = expression(phi1[0]~(mol~CO[2]~~mol^{-1}*photon)),	x = expression(T[air]~(degree*C)),tag='(a)') +
	theme_bw() +
	facet_wrap( ~fAPAR)+
	theme(legend.position = 'bottom',legend.margin=margin(0,0,0,0), legend.box.spacing = unit(0, "pt"))+
	theme(plot.margin=unit(c(0,5,0,0),"mm"),panel.spacing=unit(1, "lines"))+
	theme(axis.text.x = element_text(angle = 00, vjust = 0.1,size=7))+
	theme(axis.text.y = element_text(size=7))+
	theme(strip.text=element_text(size=7))+
	theme(axis.title=element_text(size=10),plot.tag = element_text(face = 'bold',size=12))+
	coord_cartesian(xlim = c(0.0, 42), ylim = c(0.0,0.125),expand=FALSE )

clean_data<-ggplot_build(single_plot)

########################
clean_data<-do.call(rbind,clean_data$data[2:4])
clean_data$fAPAR<-ifelse(clean_data$PANEL==1,'in situ','RS')
clean_data$Level<-ifelse(clean_data$colour=="#0073C2FF",'Mean',ifelse(clean_data$colour=="#EFC000FF",'75th Percentile','95th Percentile'))
###################################################################################################
# Figure 4a
###################################################################################################
gc()
png("C:/Users/Silwood/OneDrive - Imperial College London/Documents/papers/3_Temperature_dependence_intricsic_quantum_yield/manuscript/figures_newPhyto/Fig4a.png",width =pg_width-7,height    = 6.5,units     = "cm",res = 1200)
single_plot+
	geom_smooth(data=clean_data,aes(x = x, y = y,color=Level,fill=Level),method = "loess",fullrange = FALSE,level=0.95,alpha=0.5,linewidth=0.7, span = 0.8)+
	labs(y = expression(phi1[0]~(mol~CO[2]~~mol^{-1}*photon)),	x = expression(T[air]~(degree*C))) +
	theme_bw() +
	facet_wrap( ~fAPAR)+
	theme(legend.position = 'bottom',legend.margin=margin(0,0,0,0), legend.box.spacing = unit(0, "pt"))+
	theme(plot.margin=unit(c(0,5,0,0),"mm"),panel.spacing=unit(1, "lines"))+
	theme(axis.text.x = element_text(angle = 00, vjust = 0.1,size=7))+
	theme(axis.text.y = element_text(size=7))+
	theme(strip.text=element_text(size=7))+
	theme(axis.title=element_text(size=10))+
	coord_cartesian(xlim = c(0.0, 42), ylim = c(0.0,0.125),expand=FALSE )+
	scale_fill_manual(values = unique(clean_data$colour)[c(2,3,1)])+
	scale_colour_manual(values = unique(clean_data$colour)[c(2,3,1)])
dev.off()
###################################################################################################
# Figure 4b
###################################################################################################

png("C:/Users/Silwood/OneDrive - Imperial College London/Documents/papers/3_Temperature_dependence_intricsic_quantum_yield/manuscript/figures_newPhyto/Fig4b.png",width =pg_width-4,height    = 8,units     = "cm",res = 1200)

ggplot(subset(boxplotdata_phi0,!(boxplotdata_phi0$Biome %in% c('CSH','CVM','WSA','SNO'))),aes(x = Tair, y = phi_0,color=fAPAR)) +
	facet_wrap( ~Biome)+
	geom_jitter(size=0.02,alpha=0.3)+
	geom_smooth( aes(x = Tair, y = phi_0,color=fAPAR,fill=fAPAR),method = "loess",fullrange = TRUE,level=0.95,alpha=0.5,linewidth=0.5, span = 0.8)+
	labs(y = expression(phi1[0]~(mol~CO[2]~~mol^{-1}*photon)),	x = expression(T[air]~(degree*C)),tag='(b)') +
	theme_bw() +
	theme(legend.position = 'right',legend.margin=margin(0,0,0,10, unit = "pt"), legend.box.spacing = unit(0, "pt"))+
	theme(plot.margin=unit(c(0,0,0,0),"mm"))+
	theme(axis.text.x = element_text(angle = 00, vjust = 0.5,size=7))+
	theme(axis.text.y = element_text(size=7))+
	theme(strip.text.x=element_text(size=7,margin = margin(0.5,0.5,0.5,0.5, "mm")))+
	theme(axis.title=element_text(size=10))+
	theme(legend.text=element_text(size=6),legend.title=element_text(size=7))+
	scale_color_manual(values = palette_amf_flx) +
	scale_fill_manual(values = palette_amf_flx) +
	coord_cartesian(ylim = c(0.0001, 0.12))
dev.off()


#Fig 5 map select sites
##get avail yrs data
avail_sites$record<-(avail_sites$Data.End-avail_sites$Data.Start)+1
fluxkit.sites_plot<-subset(fluxkit.sites,!(fluxkit.sites$site_id %in% avail_sites$Site.Id))

all_sites_md<-data.frame(site= c(avail_sites$Site.Id,fluxkit.sites_plot$site_id),
					Biome=c(avail_sites$Vegetation.Abbreviation..IGBP.,fluxkit.sites_plot$biome),
					Climate=c(avail_sites$zone,fluxkit.sites_plot$clim_zone),
					fAPAR=c(rep('in situ',length(avail_sites$zone)),rep('RS',length(fluxkit.sites_plot$site_id))),
					lat=c(avail_sites$Latitude..degrees.,fluxkit.sites_plot$lat),
					lon=c(avail_sites$Longitude..degrees.,fluxkit.sites_plot$lon),
					NSE=c(phi_0_fTair_arr_v1$NSE,subset(phi_0_fTair_fdk_arr_v1$NSE,row.names(phi_0_fTair_fdk_arr_v1) %in% fluxkit.sites_plot$site_id)),
					record= c(avail_sites$record,fluxkit.sites_plot$nyrs))
all_sites_md<-subset(all_sites_md,!is.na(all_sites_md$NSE))					
					

sites_OSH<-subset(all_sites_md,all_sites_md$Biome=='OSH')
sites_CSH<-subset(all_sites_md,all_sites_md$Biome=='CSH')
sites_DBF<-subset(all_sites_md,all_sites_md$Biome=='DBF')
sites_EBF<-subset(all_sites_md,all_sites_md$Biome=='EBF')
sites_ENF<-subset(all_sites_md,all_sites_md$Biome=='ENF')
sites_GRA<-subset(all_sites_md,all_sites_md$Biome=='GRA')
sites_SAV<-subset(all_sites_md,all_sites_md$Biome=='SAV')
sites_WET<-subset(all_sites_md,all_sites_md$Biome=='WET')
sites_WSA<-subset(all_sites_md,all_sites_md$Biome=='WSA')





#Fig4a_plot climate map results
add_inset<-function(sites,db,pos=c(0.01,0.2),title,leg.pos=1,md,corner){
	#get md
	md<-subset(md,md$site %in% sites)
	#get data
	bigpar<-par()
	db<-subset(db,names(db)%in% sites)
	###plot arrows
	newpch<-4:7
	if(corner==1){
		xcorn=((pos[1]+0.1)*360-180)
		ycorn<-grconvertY((pos[2]+0.22), from = "ndc", to = "user")
	}else if(corner==2){
		xcorn=((pos[1]*360)-180)+28
		ycorn<-grconvertY(pos[2]+0.11, from = "ndc", to = "user")-10
	}else if(corner==3){
		xcorn=(((pos[1]+0.1)*360)-180)
		ycorn<-grconvertY(pos[2], from = "ndc", to = "user")
	}else if(corner==4){
		xcorn=(((pos[1]+0.18)*360)-180)-5
		ycorn<-grconvertY((pos[2]+0.11), from = "ndc", to = "user")+5
	}
	if(ycorn>0){ycorn=ycorn+13}
	for(i in 1:length(sites)){
		lines(seq(md$lon[i],xcorn,length.out=4), seq(md$lat[i], ycorn,length.out=4), lwd=0.5,lty='3D')
	}
	##add legends points type = "b",pch=newpch[i],cex=0.4,lty=i
	ylegsites=95-(leg.pos*12)
	par(fig =c(pos[1],pos[1]+0.25,pos[2],pos[2]+0.28) ,mar=c(3,3,1,3),oma=c(0,0,0,0),mgp=c(0.5,0.0,0), new = TRUE)  
	
	plot(db[[1]]$T_sample,db[[1]]$phi_0,col=1,bty="l",tck=-0.02,xaxt ="n",
		pch=newpch[1],cex=0.3,cex.lab=0.5,cex.axis=0.5,xlim=c(0,40),ylim=c(0,0.12),
		ylab=expression(phi1[0]),
		xlab='')
	
	title(title, cex.main= 0.5,line = 0.2)
	axis(1, at = seq(0,40,2), label = rep("", 21), tck = -0.01)
	## add the labels
	axis(1, at = seq(0,40,2), line = -0.4, lwd = 0, cex.axis = 0.5)
	#add x axis title
	title(xlab = expression(T[air]~(degree*C)), line = 0.2, cex.lab = 0.55)
	lines(db[[1]]$T_sample,db[[1]]$phi0_arr,col=1,lwd=0.5,lty=1)
	for(i in 2:length(db)){
		points(db[[i]]$T_sample,db[[i]]$phi_0,col=1,pch=newpch[i],cex=0.3)	
		lines(db[[i]]$T_sample,db[[i]]$phi0_arr,col=1,lwd=0.5,lty=i)
	}
	legend(40,0.12,legend =sites, pch = newpch[1:length(sites)], lty = 1:length(sites),cex=0.5,bty='n',xpd=T,pt.cex=0.4)
	
	par(bigpar)
		
}

###merge dbs
phio_db<-c(phi_0_ame_Tair,phi_0_fdk_Tair)
phio_db<-phio_db[!duplicated(names(phio_db))]



#Fig4a_map biomes map results v2
#################################
#read igbp
#################################
library(raster)
igbp_map<-raster("C:/Users/Silwood/OneDrive - Imperial College London/Documents/papers/3_Temperature_dependence_intricsic_quantum_yield/data/biomes.tif");gc()
igbp_md<-data.frame(LC_Type1=1:17,igbp=c('ENF','EBF','DNF','DBF','MF','CSH','OSH','WSA','SAV','GRA','WET','CRO','URB','CVM','SNO','BSV','WAT'),stringsAsFactors =F)
## assign color table
igbp_color<-paste0('#',c(
	'05450a', '086a10', '54a708', '78d203', '009900', 'c6b044', 'dcd159',
	'dade48', 'fbff13', 'b6ff05', '27ff87', 'c24f44', 'a5a5a5', 'ff6d4c',
	'69fff8', 'f9ffa4'
))


###################################################################################################
# Figure methods sites
###################################################################################################

png("C:/Users/Silwood/OneDrive - Imperial College London/Documents/papers/3_Temperature_dependence_intricsic_quantum_yield/manuscript/figures_newPhyto/Fig5.png",width = (9.7),height    = 9.7/1.6,units     = "in",res = 300)
#windows(width = (9.7),height    = 9.7/1.6)
par(mar=c(3,1,1,1),oma=c(2,1,4,1),mgp=c(2.25,1,0))
# my_window <- extent(-175,175,-85,85)
# # plot(my_window, col=NA, xlab='Longitude', ylab='Latitude')
# # plot(kopmap,add=T)
image(igbp_map,maxpixels=1e+08,col =igbp_color[1:16],xlab='', ylab='',axes=F,ylim=c(-100,100),xlim=c(-230,250))
abline(h=-97)
#abline(h=80)
# legend('bottom',legend=levels(kopmap@data@attributes[[1]]$zone),bty = "n",fill=kopmap@legend@colortable[2:31],cex = .75,xpd = T,horiz=F,y.intersp = 0.8,x.intersp = 0.8,border=NA)
legend(-100,-98,legend=igbp_map@data@attributes[[1]]$category[2:17],bty ,bty = "n",fill=igbp_color[1:16],ncol=9,inset=c(1,-0.12),xpd = T,cex=0.8)
points(all_sites_md$lon[all_sites_md$fAPAR=='in situ'],
	all_sites_md$lat[all_sites_md$fAPAR=='in situ'],
	pch=1,
	cex=sqrt(all_sites_md$record[all_sites_md$fAPAR=='in situ']/9))
points(all_sites_md$lon[all_sites_md$fAPAR=='RS'],
	all_sites_md$lat[all_sites_md$fAPAR=='RS']
	,pch=0,cex=sqrt(all_sites_md$record[all_sites_md$fAPAR=='RS']/15),col='gray10')
legend(-150,-98,title='fAPAR source',legend=c('in situ','RS'),pch=c(1,0),cex=0.8,pt.cex=1.2,bty='n',xpd=T,inset=c(-0.12,0))
### add insets
##top

add_inset(sites_OSH$site[c(3,17,6)],db=phio_db, pos=c(0.05,0.72),title='OSH',leg.pos=1,md=all_sites_md,corner=3)
add_inset(sites_DBF$site[c(1,25,29)],phio_db,pos=c(0.28,0.72),title='DBF',leg.pos=2,md=all_sites_md,corner=3)
add_inset(sites_ENF$site[c(39, 45, 54)],phio_db,pos=c(0.5,0.72),title='ENF',leg.pos=2,md=all_sites_md,corner=3)
add_inset(sites_GRA$site[c(18:19, 40)],phio_db,pos=c(0.75,0.72),title='GRA',leg.pos=2,md=all_sites_md,corner=3)
###bottom
add_inset(sites_WET$site[c(17:18, 24)],phio_db,pos=c(0.05,0.115),title='WET',leg.pos=2,md=all_sites_md,corner=1)
add_inset(sites_EBF$site[c(1, 11, 15)],phio_db,pos=c(0.355,0.115),title='EBF',leg.pos=2,md=all_sites_md,corner=1)
add_inset(sites_SAV$site[c(8:9, 14)],phio_db,pos=c(0.6,0.115),title='SAV',leg.pos=2,md=all_sites_md,corner=1)

#right
add_inset(sites_WSA$site[c(1:2, 4)],phio_db,pos=c(0.75,0.362),title='WSA ',leg.pos=2,md=all_sites_md,corner=2)



dev.off()
# 





#Fig 6 model eval

##Arrenius per site
png("C:/Users/Silwood/OneDrive - Imperial College London/Documents/papers/3_Temperature_dependence_intricsic_quantum_yield/manuscript/figures_newPhyto/Fig6.png",width =pg_width,height    = pg_heigth/3.5,units     = "cm",res = 1200)
par(mfrow=c(1,2),mar=c(2.5,2.7,2,2.5),mgp=c(0.5,0.2,0),tcl=-0.1)
plot.eval.dens(phi_0_ame_Tair.df$phi0_arr,(phi_0_ame_Tair.df$phi_0),phi_0_ame_Tair.df$Vegetation.Abbreviation..IGBP.,cex=0.6,cex.axis=8/12,
	colarrows='gray40',cex.stats=6/12,
	xlim=c(0.01,.125),ylim=c(0.01,0.125),main='',
	xlab='',
	ylab='',cex.lab=10/12,alpha=70)
plotrix::corner.label(label=substitute(paste(bold('(a)'))),figcorner=T,cex=12/12)	
mtext(expression(Observed~~phi1[0]~(mol~CO[2]~~mol^{-1}*photon)), side = 2, outer = F, line = 1,cex=10/12)
dev.next()
plot.eval.dens(phi_0_fdk_Tair.df$phi0_arr,(phi_0_fdk_Tair.df$phi_0),phi_0_fdk_Tair.df$biome,cex=0.6,cex.axis=8/12,
	colarrows='gray40',cex.stats=6/12,
	xlim=c(0.01,0.125),ylim=c(0.01,0.125),main='',
	xlab='',
	ylab='',cex.lab=10/12,alpha=70)
plotrix::corner.label(label=substitute(paste(bold('(b)'))),figcorner=T,cex=12/12)
mtext(expression(Simulated~~phi1[0]~(mol~CO[2]~~mol^{-1}*photon)), side = 1, outer = T, line = -0.8, las=0,cex=10/12)
dev.off()


#Fig 7
library("ggsci")
cols_intro<-pal_jco(alpha = 0.5)(6)
palette_amf_flx<-c(cols_intro[1],cols_intro[2])
pg_width=18
pg_heigth=24
cm2in =0.393701
#########################################################################################################
####     AI vs maxphi0
#########################################################################################################

png("C:/Users/Silwood/OneDrive - Imperial College London/Documents/papers/3_Temperature_dependence_intricsic_quantum_yield/manuscript/figures_newPhyto/Fig7ab.png",width = pg_width+2,height    = pg_width*(1/3)*(1/1.33),units     = "cm",res = 1200)
#windows(width = pg_width*cm2in,height    = cm2in*pg_width*(1/3)*(1/1.33))
layout(matrix(c(1,2,2), nrow = 1, ncol = 3, byrow = TRUE))
#layout(matrix(c(1,2,2,3,4,4), nrow = 2, ncol = 3, byrow = TRUE))
# dev.list()
#layout(matrix(c(1, 2, 3, 4), nrow=2, ncol=2, byrow=TRUE), widths=c(1, 2))
#layout.show(4)
palette(palette_amf_flx)
# par(mfrow=c(2,2))
par(mar=c(2,2.3,1,1),mgp=c(1,0.2,0),tcl=-0.1)
plot(combined_arr_par$AI[combined_arr_par$Biome!='CRO'],
	combined_arr_par$maxphio[combined_arr_par$Biome!='CRO'],
	cex=(0.7/as.numeric(as.factor(combined_arr_par$source)))^0.5,xlim=c(0,14),ylim=c(0,0.125),pch=19,col=as.factor(combined_arr_par$source),
	cex.axis=10/12,bty = "l",
	xlab='AI (-)',
	ylab=expression(hat(phi1[0])~(mol~CO[2]~~mol^{-1}*photon)),cex.lab=12/12,main='',cex.main=10/12)
#########################################################################################################
curve((ai_eq_op$maxphio/(1+(x)^ai_eq_op$m_conf_50)^ai_eq_op$n_conf_50),add=T)
curve((ai_eq_op$maxphio/(1+(x)^ai_eq_op$m_conf_2.5)^ai_eq_op$n_conf_2.5),add=T,lty=2)
curve((ai_eq_op$maxphio/(1+(x)^ai_eq_op$m_conf_97.5)^ai_eq_op$n_conf_97.5),add=T,lty=2)

#########################################################################################################
basicPlotteR::addTextLabels(
	combined_arr_par$AI[combined_arr_par$Biome!='CRO' & combined_arr_par$AI>3 & combined_arr_par$maxphio>0.08],
	combined_arr_par$maxphio[combined_arr_par$Biome!='CRO' & combined_arr_par$AI>3 & combined_arr_par$maxphio>0.08],	
	col.label = "black",cex.label = 7/12, 
	labels = combined_arr_par$sites[combined_arr_par$Biome!='CRO' & combined_arr_par$AI>3 & combined_arr_par$maxphio>0.08]
	)
plotrix::corner.label(label=substitute(paste(bold('(a)'))),figcorner=T,cex=12/12)	
##Giulia's
#curve(pmin(1,0.62*x^-0.45)*0.11,add=T,col=2)
#mtext(expression(hat(phi1[0])~(mol~CO[2]~~mol^{-1}*photon)), side = 2, outer = F, line = 1,cex=8/12)
#########################################################################################################
par(mar=c(1,2,1,5.5),mgp=c(1,0.2,0),tcl=0.1)
image(crop(maxphio_map,extent(-180,180,-56,85)),col=(aetpalet),maxpixels=300000,main='',axes=T,cex.axis=10/12,axis.args=list( cex.axis=7/12,mgp=c(1,0.4,0),tcl=-0.1),cex.main=10/12,xlab='',ylab='',zlim=c(0,0.12),xlim=c(-170,170),
	legend.args=list(text=expression(hat(phi1[0])),cex=10/12),yaxt="n")
axis(side=2, at=c(-50,0,50),cex.axis=10/12)
plotrix::corner.label(label=substitute(paste(bold('(b)'))),figcorner=T,cex=12/12)	
plot(crop(maxphio_map,extent(-180,180,-56,85)),col=(aetpalet),legend.only=TRUE,main='',axes=T,cex.axis=10/12,axis.args=list( cex.axis=10/12,mgp=c(1,0.4,0),tcl=-0.1),cex.main=10/12,xlab='',ylab='',zlim=c(0,0.12),xlim=c(-170,170),
	legend.args=list(text=expression(hat(phi1[0])),cex=12/12),yaxt="n")
dev.off()


##########################################################################################################
#########################################################################################################
####     Entropy vs TmGDD0
#########################################################################################################
#########################################################################################################
##########################################################################################################
png("C:/Users/Silwood/OneDrive - Imperial College London/Documents/papers/3_Temperature_dependence_intricsic_quantum_yield/manuscript/figures_newPhyto/Fig7cd.png",width = pg_width+2,height    = pg_width*(1/3)*(1/1.33),units     = "cm",res = 1200)
#windows(width = pg_width*cm2in,height    = cm2in*pg_width*(1/3)*(1/1.33))
layout(matrix(c(1,2,2), nrow = 1, ncol = 3, byrow = TRUE))
palette(palette_amf_flx)
par(mar=c(2,2.3,1,1),mgp=c(1,0.2,0),tcl=-0.1)
plot(combined_arr_par$mGDD0[combined_arr_par$Biome!='CRO'],
	combined_arr_par$dent[combined_arr_par$Biome!='CRO'],
	cex=(0.7/as.numeric(as.factor(combined_arr_par$source)))^0.5,xlim=c(3,35),ylim=c(100,2000),pch=19,col=as.factor(combined_arr_par$source),
	cex.axis=10/12,bty = "l",
	xlab=expression(mGDD[0]~(degree*C)),
	ylab=expression(Delta*S~(J~mol^{-1}~K^{-1})),cex.lab=12/12,main='',cex.main=10/12)
##########################################################################################################
lines(seq(1,40,1),pre_dent$Sim.Median,lty=1)
lines(seq(1,40,1),pre_dent$`Prop.2.5%`,lty=2)
lines(seq(1,40,1),pre_dent$`Prop.97.5%`,lty=2)
##########################################################################################################
basicPlotteR::addTextLabels(
	combined_arr_par$mGDD0[combined_arr_par$Biome!='CRO' & combined_arr_par$AI>3 & combined_arr_par$maxphio>0.08],
	combined_arr_par$dent[combined_arr_par$Biome!='CRO' & combined_arr_par$AI>3 & combined_arr_par$maxphio>0.08],	
	col.label = "black",cex.label = 5/12, 
	labels = combined_arr_par$sites[combined_arr_par$Biome!='CRO' & combined_arr_par$AI>3 & combined_arr_par$maxphio>0.08]
)
plotrix::corner.label(label=substitute(paste(bold('(c)'))),figcorner=T,cex=12/12)	

#########################################################################################################
par(mar=c(1,2,1,5.5),mgp=c(1,0.2,0),tcl=0.1)
image(crop(Topt,extent(-180,180,-56,85)),col=rev(aetpalet),maxpixels=300000,main='',axes=T,cex.axis=10/12,axis.args=list( cex.axis=10/12,mgp=c(1,0.4,0),tcl=-0.1),cex.main=12/12,xlab='',ylab='',zlim=c(18,26),xlim=c(-170,170),
	legend.args=list(text=expression(T[opt]),cex=10/12),yaxt="n")
axis(side=2, at=c(-50,0,50),cex.axis=10/12)
plotrix::corner.label(label=substitute(paste(bold('(d)'))),figcorner=T,cex=12/12)	
plot(crop(Topt,extent(-180,180,-56,85)),col=rev(aetpalet),legend.only=T,main='',axes=T,cex.axis=10/12,axis.args=list( cex.axis=10/12,mgp=c(2,0.4,0),tcl=-0.1),cex.main=10/12,xlab='',ylab='',zlim=c(18,26),xlim=c(-170,170),
	legend.args=list(text=expression(T[opt]),cex=12/12),yaxt="n")
dev.off()





	#Fig 8
	#low light test
	###########################################
	#### Run Jen Jhnoson model
	tc=seq(0.5,50,0.5);ppfd=150
	####FAPAR
	JFB_par$Abs = 0.85
	
	test1=JFB_c3_c4(
		Qin = ppfd,  					   # PAR, umol PPFD m-2 s-1
		Tin = tc,                # Leaf temperature, C
		Cin = rep(700, 100),                    # Mesophyll CO2, ubar
		Oin = rep(0.0209, 100),                  # Atmospheric O2, bar
		Pin = rep(1, 100),                       # Total pressure, bar
		pars=JFB_par
	)
	phi0_sndvl=calc_phi0_new(tc,25,1.9)
	#photo respi
	test_prs=JFB_c3_c4(
		Qin = ppfd,  					   # PAR, umol PPFD m-2 s-1
		Tin = tc,                # Leaf temperature, C
		Cin = rep(400, 100),                    # Mesophyll CO2, ubar
		Oin = rep(0.209, 100),                  # Atmospheric O2, bar
		Pin = rep(1, 100),                       # Total pressure, bar
		pars=JFB_par
	)
	pg_width=18
	pg_heigth=24
	png("C:/Users/Silwood/OneDrive - Imperial College London/Documents/papers/3_Temperature_dependence_intricsic_quantum_yield/manuscript/figures_newPhyto/Fig8_jen_comp.png",width =pg_width,height    = pg_width*0.333,units     = "cm",res = 1200)
	########################################################################################
	########################################################################################
	par(mfrow=c(1,3),mar=c(1,3,1.5,0.5),mgp=c(1.5,0.2,0),tcl=-0.1,oma=c(1, 0, 1, 0),mai=c(0.4,0.4,0.1,0.5))
	plot(tc,test1$phiPSI,type='l',lty=3,ylab=expression(Phi),xlab='',ylim=c(0,1),cex.lab=1.2,cex.axis=11/12)
	lines(tc,test1$phiPSII,lty=2);
	legend('topleft',c(expression(Phi[PSI]),expression(Phi[PSII]),expression(kq[Cyt*b6f])),lty=c(3,2,1),lwd=c(2,2,2),box.col='white',cex=10/12)
	# Add the second time series on the same plot
	par(new = TRUE)  # Overlay a new plot on the existing one
	plot(tc,test1$kq, type = "l", lwd = 1, xaxt = "n",yaxt='n',xlab = "", ylab = "", ylim = range(test1$kq))
	# Customize axes for the second y-axis on the right
	axis(side = 4, col = "black", col.axis = 'black',cex.axis=11/12)
	mtext(expression(kq[Cyt*b6f]~(mol~PQH[2]~~mol^{-1}~sites~s^{-1})), side = 4, line = 2, col ='black',cex=9/12)	
	plotrix::corner.label(x=-1,label=substitute(paste(bold('(a)'))),figcorner=T,cex=12/12)
	#dev.next()
	########################################################################################
	########################################################################################
	par(mai=c(0.4,0.4,0.1,0.5))
	plot(tc,test1$Gstar*1e5,type='l',lty=2,ylab=expression(Gamma^{'*'}~(Pa)),xlab='',ylim=range(test1$Gstar*1e5),cex.lab=1.2,cex.axis=11/12)
	legend('topleft',c(expression(n[L]),expression(Gamma^{'*'})),lty=c(1,2),lwd=c(2,2),box.col='white')
	# Add the second time series on the same plot
	par(new = TRUE)  # Overlay a new plot on the existing one
	plot(tc,test1$nl, type = "l", lwd = 1, xaxt = "n",yaxt='n',xlab = "", ylab = "", ylim = range(test1$nl))
	# Customize axes for the second y-axis on the right
	axis(side = 4, col = "black", col.axis = 'black',cex.axis=11/12)
	mtext(expression(n[L]~(mol~ATP~~mol^{-1}~e^{'-'})), side = 4, line = 2, col ='black',cex=9/12)	
	plotrix::corner.label(x=-1,label=substitute(paste(bold('(b)'))),figcorner=T,cex=12/12)
	#dev.next()
	########################################################################################
	######################################################################################## plot(test1$Ag_a/(150/1e6))
	par(mai=c(0.4,0.5,0.1,0.4))
	plot(tc,test1$Ag_a/(ppfd/1e6),type='l',lty=2,ylab=expression(phi1[0]~(mol~CO[2]~~mol^{-1}*photon)),
		xlab='',ylim=c(0,0.1),cex.lab=1.1,cex.axis=11/12)
	# Add the second time series on the same plot
	lines(tc,phi0_sndvl,lty=1);
	legend('topleft',c('Present study','Johnson & Berry (2021)'),lty=c(1,2),lwd=c(2,2),bty='n',cex=9/12)
	plotrix::corner.label(x=-1,label=substitute(paste(bold('(c)'))),figcorner=T,cex=12/12)
	mtext(expression(T[Leaf]~(degree*C)), side = 1, outer = T, line = 0.11,cex=12/12)
	dev.off()	


#Fig 9
library(ggplot2)
library("ggsci")
cols_intro<-pal_d3('category10')(9)
cols_intro=c(cols_intro,'black')
names(cols_intro)<-c('BETHY','CanESM','CLM 4.5','ED2','GDAY','IBIS','JULES','LM3','ORCHIDEE','This study')
###load
ROG19_lrc_data <- read.table(file = "C:/Users/Silwood/OneDrive - Imperial College London/Documents/papers/3_Temperature_dependence_intricsic_quantum_yield/data/Rogers_etal_NGEEArctic_LightResponse/example_output/Rogers_etal_NGEEArctic_LightResponse/Example_fitted_A-Q_response_curves.csv", header = TRUE, sep = ",", fileEncoding = "windows-1252", quote = "\"", stringsAsFactors = FALSE, comment.char = "", na.strings = "")
ROG19_lrc_data[ROG19_lrc_data==-9999]<-NA
# ## add temp factor
ROG19_lrc_data$Tgroup<-ifelse(ROG19_lrc_data$Mean_Tleaf<10,5,ifelse(ROG19_lrc_data$Mean_Tleaf<20,15,25))
### add model estimations
##setup1

ROG19_lrc_data$BETHY = calc_phi0_Farq(tc=ROG19_lrc_data$Mean_Tleaf,elev=10,co2=ROG19_lrc_data$Mean_CO2S,f=0.44,abst=0.88,chi=ROG19_lrc_data$Mean_Ci_Ca,Gstaropt='clm')
ROG19_lrc_data$CLM = calc_phi0_Farq(tc=ROG19_lrc_data$Mean_Tleaf,elev=10,co2=ROG19_lrc_data$Mean_CO2S,f=0.15,abst=0.85,chi=ROG19_lrc_data$Mean_Ci_Ca,Gstaropt='clm')
ROG19_lrc_data$GDAY = calc_phi0_Farq(tc=ROG19_lrc_data$Mean_Tleaf,elev=10,co2=ROG19_lrc_data$Mean_CO2S,f=0.48,abst=0.85,chi=ROG19_lrc_data$Mean_Ci_Ca,Gstaropt='clm')
ROG19_lrc_data$Orchidee= calc_phi0_Farq(tc=ROG19_lrc_data$Mean_Tleaf,elev=10,co2=ROG19_lrc_data$Mean_CO2S,f=0.26,abst=0.84,chi=ROG19_lrc_data$Mean_Ci_Ca,Gstaropt='clm') 	
# CLM4.5, G’DAY, Orchidee
##setup2
ROG19_lrc_data$CanESM = calc_phi0_Farq(tc=ROG19_lrc_data$Mean_Tleaf,elev=10,co2=ROG19_lrc_data$Mean_CO2S,f=0.36,abst=0.85,chi=ROG19_lrc_data$Mean_Ci_Ca,Gstaropt='jules')
ROG19_lrc_data$JULES = calc_phi0_Farq(tc=ROG19_lrc_data$Mean_Tleaf,elev=10,co2=ROG19_lrc_data$Mean_CO2S,f=0.36,abst=0.85,chi=ROG19_lrc_data$Mean_Ci_Ca,Gstaropt='jules')
# CanESM,
# JULES
# 
# setup3
ROG19_lrc_data$IBIS = calc_phi0_Farq(tc=ROG19_lrc_data$Mean_Tleaf,elev=10,co2=ROG19_lrc_data$Mean_CO2S,f=0.36,abst=0.86,chi=ROG19_lrc_data$Mean_Ci_Ca,Gstaropt='ibis')
ROG19_lrc_data$ED2 = calc_phi0_Farq(tc=ROG19_lrc_data$Mean_Tleaf,elev=10,co2=ROG19_lrc_data$Mean_CO2S,f=0.36,abst=0.73,chi=ROG19_lrc_data$Mean_Ci_Ca,Gstaropt='ibis')
ROG19_lrc_data$LM3 = calc_phi0_Farq(tc=ROG19_lrc_data$Mean_Tleaf,elev=10,co2=ROG19_lrc_data$Mean_CO2S,f=0.52,abst=0.85,chi=ROG19_lrc_data$Mean_Ci_Ca,Gstaropt='ibis')
### present study
#phio_Bar<-calc_phi0_new(tc=ROG19_lrc_data$Mean_Tleaf,mGDD0 = mGDD0_Bar,AI=AI_Bar)
phio_Bar<-calc_phi0_new(tc=ROG19_lrc_data$Mean_Tleaf,mGDD0 = mGDD0_Bar,AI=1.23)
mj_bar<-calc_phi0_Farq(tc=ROG19_lrc_data$Mean_Tleaf,elev=10,co2=ROG19_lrc_data$Mean_CO2S,f=0.0,abst=0.85,chi=ROG19_lrc_data$Mean_Ci_Ca,Gstaropt='pmodel')*8

ROG19_lrc_data$phio_Bar=phio_Bar*mj_bar

# ED2, IBIS, LM3
#windows(7*1.33,7)
png("C:/Users/Silwood/OneDrive - Imperial College London/Documents/papers/3_Temperature_dependence_intricsic_quantum_yield/manuscript/figures_newPhyto/Fig9_mod_comp.png",width = 8*1.5,height    = 8,units     = "cm",res = 1200)
ggplot() +
	theme_bw()+
	geom_smooth(data = ROG19_lrc_data,mapping = aes(x = Mean_Tleaf, y = phio_Bar,color='This study'),
	method = "loess",fullrange = FALSE,level=0.99,alpha=0.5,linewidth=0.7,se = FALSE, span=1.2)+
	geom_smooth(data = ROG19_lrc_data,mapping = aes(x = Mean_Tleaf, y = BETHY,color='BETHY'),
	method = "loess",fullrange = FALSE,level=0.99,alpha=0.5,linewidth=0.7,se = FALSE, span=1.2)+
	geom_smooth(data = ROG19_lrc_data,mapping = aes(x = Mean_Tleaf, y = CanESM,color='CanESM'),
	method = "loess",fullrange = FALSE,level=0.99,alpha=0.5,linewidth=0.7,se = FALSE, span=1.2)+
	geom_smooth(data = ROG19_lrc_data,mapping = aes(x = Mean_Tleaf, y = CLM, color='CLM 4.5'),
	method = "loess",fullrange = FALSE,level=0.99,alpha=0.5,linewidth=0.7,se = FALSE, span=1.2)+
	geom_smooth(data = ROG19_lrc_data,mapping = aes(x = Mean_Tleaf, y = ED2, color='ED2'),
	method = "loess",fullrange = FALSE,level=0.99,alpha=0.5,linewidth=0.7,se = FALSE, span=1.2)+
	geom_smooth(data = ROG19_lrc_data,mapping = aes(x = Mean_Tleaf, y = GDAY, color='GDAY'),
	method = "loess",fullrange = FALSE,level=0.99,alpha=0.5,linewidth=0.7,se = FALSE, span=1.2)+
	geom_smooth(data = ROG19_lrc_data,mapping = aes(x = Mean_Tleaf, y = IBIS, color='IBIS'),
	method = "loess",fullrange = FALSE,level=0.99,alpha=0.5,linewidth=0.7,se = FALSE, span=1.2)+
	geom_smooth(data = ROG19_lrc_data,mapping = aes(x = Mean_Tleaf, y = JULES, color='JULES'),
	method = "loess",fullrange = FALSE,level=0.99,alpha=0.5,linewidth=0.7,se = FALSE, span=1.2)+
	geom_smooth(data = ROG19_lrc_data,mapping = aes(x = Mean_Tleaf, y = LM3, color='LM3'),
	method = "loess",fullrange = FALSE,level=0.99,alpha=0.5,linewidth=0.7,se = FALSE, span=1.2)+
	geom_smooth(data = ROG19_lrc_data,mapping = aes(x = Mean_Tleaf, y = Orchidee, color='ORCHIDEE'),
	method = "loess",fullrange = FALSE,level=0.99,alpha=0.5,linewidth=0.7,se = FALSE, span=1.2)+

	stat_boxplot(data = ROG19_lrc_data,aes(x=Tgroup, y=aQY,group = Tgroup),geom = "errorbar",width = 0.50) +
	geom_boxplot(data = ROG19_lrc_data,aes(x=Tgroup, y=aQY,group = Tgroup),varwidth = TRUE) +
	theme(text = element_text(size=12)) +
	theme(axis.title=element_text(size=10))+
	theme(legend.text=element_text(size=7),legend.title=element_text(size=7),legend.key.size=unit(.6,"lines"))+
	scale_color_manual(values = cols_intro) +
	xlab("")+
	labs(y = expression(phi1~(mol~CO[2]~~mol^{-1}*photon)),	x = expression(T[Leaf]~(degree*C)),	color = "")+
	coord_cartesian(ylim = c(0, 0.10),xlim=c(0,30))
dev.off()
# pg_width=8.2
# 
# 
#windows(width = pg_width*0.5,height    = pg_width*0.5*0.55)
