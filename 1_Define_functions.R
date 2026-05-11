#1. Define functions
#################################################################
################################################################
### define functions
#################################################################
################################################################
# evaluation sim plot

plot.eval.dens<-function(sim,obs, factor=NULL,cex.stats=1.0,legendpos='bottomright',colarrows='grey60',cex=1.5, ...){
	# testing
	# sim=wn_sim_well;obs=wn_obs_well$sm;igbp=wn_obs_well$IGBP
	# end testing
	sim<-as.numeric(sim)
	obs<-as.numeric(obs)
	#get stats
	stats<-hydroGOF::gof(as.numeric(sim),as.numeric(obs),digits=3, na.rm = TRUE)
	#stats<-c(stats[17,],stats[4,],stats[9,])
	if(!is.null(factor)){
		factor=as.factor(factor)
		sim_group<-aggregate(sim,by=list(factor),FUN=mean,na.rm=TRUE)
		sim_group_sd<-aggregate(sim,by=list(factor),FUN=sd,na.rm=TRUE)
		obs_group<-aggregate(obs,by=list(factor),FUN=mean,na.rm=TRUE)
		obs_group_sd<-aggregate(obs,by=list(factor),FUN=sd,na.rm=TRUE)
		types<-obs_group$Group.1
	}

	#get lm
	fit<-lm(as.numeric(obs)~as.numeric(sim))
	subtitle <- bquote( italic(R)^2 == .(round(stats[17], digits = 2)) ~~
		RMSE == .(round(stats[4], digits = 3)) ~~
		bias == .(round(stats[1], digits = 3)) ~~
		slope == .(round(fit$coefficients[2], digits = 3)) ~~
		italic(N) == .(length(sim)) )	
	
	symbs<-c(15:18,21:25,3:4,9:10,12)
	##plot
	LSD::heatscatter(sim,obs,colpal=c('grey95', 'blue', 'green', 'yellow', 'red'),bty='l',cex=cex, ...)
	##add regression lines to the plot
	abline(0,1,lwd=1)
	abline(coef(fit)[1:2],lty=2,lwd=1,col="red")
	##add the stats to the plot
	mtext(subtitle,side=3, line=0.5, adj=0, cex=cex,outer=F)
	if(!is.null(factor)){
		arrows(sim_group$x-sim_group_sd$x,obs_group$x,sim_group$x+sim_group_sd$x,obs_group$x , length=0.05, angle=90, code=3,col=colarrows,lwd=0.7)
		arrows(sim_group$x,obs_group$x-obs_group_sd$x,sim_group$x,obs_group$x+obs_group_sd$x , length=0.05, angle=90, code=3,col=colarrows,lwd=0.7)
		points(sim_group$x,obs_group$x,pch=symbs[as.integer(types)],cex=cex,col='black',bg='grey')
		legend(legendpos,pch=symbs[as.integer(types)],col=rep('black',length(types)),legend=types,bty="n",pt.bg=rep('grey',length(types)),cex=cex.stats,pt.cex=cex.stats)
	}
	
}

# budyko plot

budyko_plot<-function(x,y, factor=NULL,cex.stats=1.0, ...){
	# obs daily always
	# testing
	# sim=wn_sim_well;obs=wn_obs_well$sm;igbp=wn_obs_well$IGBP
	# end testing
	x<-as.numeric(x)
	y<-as.numeric(y)
	
	#stats<-hydroGOF::gof(as.numeric(sim),as.numeric(obs),digits=3, na.rm = TRUE)
	
	if(!is.null(factor)){
		factor<-as.factor(factor)
		sim_group<-aggregate(x,by=list(factor),FUN=mean,na.rm=TRUE)
		sim_group_sd<-aggregate(x,by=list(factor),FUN=sd,na.rm=TRUE)
		obs_group<-aggregate(y,by=list(factor),FUN=mean,na.rm=TRUE)
		obs_group_sd<-aggregate(y,by=list(factor),FUN=sd,na.rm=TRUE)
		types<-obs_group$Group.1
	}


	# rp = vector('expression',3)
	# rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
	# 	list(MYVALUE = format(stats[1],dig=2)))[2]
	# rp[2] = substitute(expression(italic(RMSE) == MYOTHERVALUE), 
	# 	list(MYOTHERVALUE = format(stats[2], digits = 2)))[2]
	# rp[3] = substitute(expression(italic(NSE) == MYOTHERVALUE), 
	# 	list(MYOTHERVALUE = format(stats[3], digits = 2)))[2]
	fit<-lm(as.numeric(y)~as.numeric(x))
	coefs<-summary(fit)
	#coefs$r.squared
	
	r<-cor.test(x,y)
	if(coefs$coefficients[,4][2]<=0.001){
		pval='p < 0.001'
	}else if((coefs$coefficients[,4][2]>0.001)&(coefs$coefficients[,4][2]<=0.01)){
		pval='p < 0.01'
	}else if((coefs$coefficients[,4][2]>0.01)&(coefs$coefficients[,4][2]<=0.05)){
		pval='p < 0.05'
	}else if((coefs$coefficients[,4][2]>0.05)&(coefs$coefficients[,4][2]<=0.1)){
		pval='p < 0.1'
	}else if((coefs$coefficients[,4][2]>0.1)){
		pval='n.s.'
	}
	if(r$estimate>0){
		subtitle <- bquote( y == .(round(coefs$coefficients[1,1],2))~+~
			.(round(coefs$coefficients[2,1],2))~ x~~','~
			italic(R)^2 == .(round(r$estimate,2))~','~~
			italic(.(pval))
		)	
	}else{
		subtitle <- bquote( y == .(round(coefs$coefficients[1,1],2))~
			.(round(coefs$coefficients[2,1],2))~ x~~','~
			italic(R)^2 == .(round(r$estimate,2))~','~~
			italic(.(pval))
		)	
	}


	
	# windows(40,40)
	#colramp = colorRampPalette(c('grey70', 'blue', 'green', 'yellow', 'red'), alpha = TRUE)
	colramp=c('grey70', 'blue', 'green', 'yellow', 'red')
	#colramp=c('grey80', 'grey80', 'grey80')
	symbs<-c(15:18,21:25,3:4,9:10,12)
	# smoothScatter(as.numeric(obs)~as.numeric(sim),cex=0.8,colramp=colramp,nbin=500, ...)
	#LSD::heatscatter(x,y,colpal=c('grey', 'blue', 'green', 'yellow', 'red'),main='',bty='l',...)
	#LSD::heatscatter(x,y,colpal=colramp,bty='l',...)
	plot(x,y,col=1:length(x), ...)
	abline(0,1,lty=2,lwd=0.5,col='black')
	abline(h=1,lty=2,lwd=0.5,col='black')
	abline(v=1,lty=2,lwd=0.5,col='black')
	segments(0,0,1,1,col='black')
	segments(1,1,max(x,na.rm=T),1,col='black')
	#abline(coef(fit)[1:2],lty=2,lwd=1,col="red")
	# legend("topleft",paste(names(stats),"=", as.character(stats)),bty="n")
	# legend(x=-0.1,y=1.05,legend=rp,bty="n",cex = 0.68)
	# legend("topleft",legend=rp,bty="n",cex = 1)#
	#mtext(subtitle,side=3, line=1, adj=0, cex=cex.stats,outer=F)
	if(!is.null(factor)){
		arrows(sim_group$x-sim_group_sd$x,obs_group$x,sim_group$x+sim_group_sd$x,obs_group$x , length=0.05, angle=90, code=3,col='black')
		arrows(sim_group$x,obs_group$x-obs_group_sd$x,sim_group$x,obs_group$x+obs_group_sd$x , length=0.05, angle=90, code=3,col='black')
		points(sim_group$x,obs_group$x,pch=symbs[as.integer(types)],cex=1.7,col='black',bg='grey')
		legend("bottomright",pch=symbs[as.integer(types)],col=rep('black',length(types)),legend=types,bty="n",pt.bg=rep('grey',length(types)),cex=cex.stats,pt.cex=cex.stats)
	}
	
}

# scater dens plot

plot.scat.dens<-function(x,y, factor=NULL,cex.stats=1.0,legendpos='bottomright',colarrows='grey60',cex=1.5, ...){
	# obs daily always
	# testing
	# sim=wn_sim_well;obs=wn_obs_well$sm;igbp=wn_obs_well$IGBP
	# end testing
	x<-as.numeric(x)
	y<-as.numeric(y)
	
	#stats<-hydroGOF::gof(as.numeric(sim),as.numeric(obs),digits=3, na.rm = TRUE)
	
	if(!is.null(factor)){
		factor<-as.factor(factor)
		sim_group<-aggregate(x,by=list(factor),FUN=mean,na.rm=TRUE)
		sim_group_sd<-aggregate(x,by=list(factor),FUN=sd,na.rm=TRUE)
		obs_group<-aggregate(y,by=list(factor),FUN=mean,na.rm=TRUE)
		obs_group_sd<-aggregate(y,by=list(factor),FUN=sd,na.rm=TRUE)
		types<-obs_group$Group.1
	}


	# rp = vector('expression',3)
	# rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
	# 	list(MYVALUE = format(stats[1],dig=2)))[2]
	# rp[2] = substitute(expression(italic(RMSE) == MYOTHERVALUE), 
	# 	list(MYOTHERVALUE = format(stats[2], digits = 2)))[2]
	# rp[3] = substitute(expression(italic(NSE) == MYOTHERVALUE), 
	# 	list(MYOTHERVALUE = format(stats[3], digits = 2)))[2]
	fit<-lm(as.numeric(y)~as.numeric(x))
	coefs<-summary(fit)
	#coefs$r.squared
	
	r<-cor.test(x,y)
	if(coefs$coefficients[,4][2]<=0.001){
		pval='p < 0.001'
	}else if((coefs$coefficients[,4][2]>0.001)&(coefs$coefficients[,4][2]<=0.01)){
		pval='p < 0.01'
	}else if((coefs$coefficients[,4][2]>0.01)&(coefs$coefficients[,4][2]<=0.05)){
		pval='p < 0.05'
	}else if((coefs$coefficients[,4][2]>0.05)&(coefs$coefficients[,4][2]<=0.1)){
		pval='p < 0.1'
	}else if((coefs$coefficients[,4][2]>0.1)){
		pval='n.s.'
	}
	if(r$estimate>0){
		subtitle <- bquote( y == .(sprintf(coefs$coefficients[1,1], fmt = '%#.2f'))~+~
			.(sprintf(coefs$coefficients[2,1],fmt = '%#.2f'))~ x~~','~
			italic(R)^2 == .(sprintf(r$estimate^2,fmt = '%#.2f'))~','~~
			italic(.(pval))
		)	
	}else{
		subtitle <- bquote( y == .(sprintf(coefs$coefficients[1,1], fmt = '%#.2f'))~
			.(sprintf(coefs$coefficients[2,1],fmt = '%#.2f'))~ x~~','~
			italic(R)^2 == .(sprintf(r$estimate^2,fmt = '%#.2f'))~','~~
			italic(.(pval))
		)	
	}

		
	# windows(40,40)
	#colramp = colorRampPalette(c('grey70', 'blue', 'green', 'yellow', 'red'), alpha = TRUE)
	#colramp=c('grey70', 'blue', 'green', 'yellow', 'red')
	#colramp=c('grey90', 'grey90', 'grey90')
	colramp=c('grey90', 'grey80', 'grey60')
	#symbs<-c(15:18,21:25,3:4,9:10,12)
	symbs<-c(15:18,21:25,0:14)
	# smoothScatter(as.numeric(obs)~as.numeric(sim),cex=0.8,colramp=colramp,nbin=500, ...)
	#LSD::heatscatter(x,y,colpal=c('grey', 'blue', 'green', 'yellow', 'red'),main='',bty='l',...)
	LSD::heatscatter(x,y,colpal=colramp,bty='l',cex=cex, ...)
	abline(coef(fit)[1:2],lty=2,lwd=1,col="black")
	# legend("topleft",paste(names(stats),"=", as.character(stats)),bty="n")
	# legend(x=-0.1,y=1.05,legend=rp,bty="n",cex = 0.68)
	# legend("topleft",legend=rp,bty="n",cex = 1)#
	mtext(subtitle,side=3, line=1, adj=0, cex=cex.stats,outer=F)
	if(!is.null(factor)){
		arrows(sim_group$x-sim_group_sd$x,obs_group$x,sim_group$x+sim_group_sd$x,obs_group$x , length=0.05, angle=90, code=3,col=colarrows,lwd=0.5)
		arrows(sim_group$x,obs_group$x-obs_group_sd$x,sim_group$x,obs_group$x+obs_group_sd$x , length=0.05, angle=90, code=3,col=colarrows,lwd=0.5)
		points(sim_group$x,obs_group$x,pch=symbs[as.integer(types)],cex=1.7,col='black',bg='grey')
		legend(legendpos,pch=symbs[as.integer(types)],col=rep('black',length(types)),legend=types,bty="n",pt.bg=rep('grey',length(types)),cex=cex.stats,pt.cex=cex.stats)
	}
	
}

# scater dens plot

plot.scat.dens_col<-function(x,y, factor=NULL,cex.stats=1.0,legendpos='bottomright',colarrows='grey60',cex=1.5, ...){
	# obs daily always
	# testing
	# sim=wn_sim_well;obs=wn_obs_well$sm;igbp=wn_obs_well$IGBP
	# end testing
	x<-as.numeric(x)
	y<-as.numeric(y)
	
	#stats<-hydroGOF::gof(as.numeric(sim),as.numeric(obs),digits=3, na.rm = TRUE)
	
	if(!is.null(factor)){
		factor<-as.factor(factor)
		sim_group<-aggregate(x,by=list(factor),FUN=mean,na.rm=TRUE)
		sim_group_sd<-aggregate(x,by=list(factor),FUN=sd,na.rm=TRUE)
		obs_group<-aggregate(y,by=list(factor),FUN=mean,na.rm=TRUE)
		obs_group_sd<-aggregate(y,by=list(factor),FUN=sd,na.rm=TRUE)
		types<-obs_group$Group.1
	}


	# rp = vector('expression',3)
	# rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
	# 	list(MYVALUE = format(stats[1],dig=2)))[2]
	# rp[2] = substitute(expression(italic(RMSE) == MYOTHERVALUE), 
	# 	list(MYOTHERVALUE = format(stats[2], digits = 2)))[2]
	# rp[3] = substitute(expression(italic(NSE) == MYOTHERVALUE), 
	# 	list(MYOTHERVALUE = format(stats[3], digits = 2)))[2]
	fit<-lm(as.numeric(y)~as.numeric(x))
	coefs<-summary(fit)
	#coefs$r.squared
	
	r<-cor.test(x,y)
	if(coefs$coefficients[,4][2]<=0.001){
		pval='p < 0.001'
	}else if((coefs$coefficients[,4][2]>0.001)&(coefs$coefficients[,4][2]<=0.01)){
		pval='p < 0.01'
	}else if((coefs$coefficients[,4][2]>0.01)&(coefs$coefficients[,4][2]<=0.05)){
		pval='p < 0.05'
	}else if((coefs$coefficients[,4][2]>0.05)&(coefs$coefficients[,4][2]<=0.1)){
		pval='p < 0.1'
	}else if((coefs$coefficients[,4][2]>0.1)){
		pval='n.s.'
	}
	if(r$estimate>0){
		subtitle <- bquote( y == .(round(coefs$coefficients[1,1],2))~+~
			.(round(coefs$coefficients[2,1],2))~ x~~','~
			r == .(round(r$estimate,2))~','~~
			italic(.(pval))
		)	
	}else{
		subtitle <- bquote( y == .(round(coefs$coefficients[1,1],2))~
			.(round(coefs$coefficients[2,1],2))~ x~~','~
			r == .(round(r$estimate,2))~','~~
			italic(.(pval))
		)	
	}


	
	# windows(40,40)
	#colramp = colorRampPalette(c('grey70', 'blue', 'green', 'yellow', 'red'), alpha = TRUE)
	colramp=c('grey90', 'blue', 'green', 'yellow', 'red')
	#colramp=c('grey90', 'grey90', 'grey90')
	#colramp=c('grey60', 'grey60', 'grey60')
	#symbs<-c(15:18,21:25,3:4,9:10,12)
	symbs<-c(15:18,21:25,0:14)
	# smoothScatter(as.numeric(obs)~as.numeric(sim),cex=0.8,colramp=colramp,nbin=500, ...)
	#LSD::heatscatter(x,y,colpal=c('grey', 'blue', 'green', 'yellow', 'red'),main='',bty='l',...)
	LSD::heatscatter(x,y,colpal=colramp,bty='l',cex=cex, ...)
	#abline(coef(fit)[1:2],lty=2,lwd=1,col="red")
	# legend("topleft",paste(names(stats),"=", as.character(stats)),bty="n")
	# legend(x=-0.1,y=1.05,legend=rp,bty="n",cex = 0.68)
	# legend("topleft",legend=rp,bty="n",cex = 1)#
	#mtext(subtitle,side=3, line=1, adj=0, cex=cex.stats,outer=F)
	if(!is.null(factor)){
		arrows(sim_group$x-sim_group_sd$x,obs_group$x,sim_group$x+sim_group_sd$x,obs_group$x , length=0.05, angle=90, code=3,col=colarrows,lwd=0.5)
		arrows(sim_group$x,obs_group$x-obs_group_sd$x,sim_group$x,obs_group$x+obs_group_sd$x , length=0.05, angle=90, code=3,col=colarrows,lwd=0.5)
		points(sim_group$x,obs_group$x,pch=symbs[as.integer(types)],cex=cex,col='black',bg='grey')
		legend(legendpos,pch=symbs[as.integer(types)],col=rep('black',length(types)),legend=types,bty="n",pt.bg=rep('grey',length(types)),cex=cex.stats,pt.cex=cex.stats)
	}
	
}

##define palettes
##########################################################################
#per site palette
##########################################################################
# write.csv(data.frame(x=fig3df$T_sample,y=fig3df$phi_0,label=fig3df$Dataset),"C:/Users/Silwood/OneDrive - Imperial College London/Documents/papers/3_Temperature_dependence_intricsic_quantum_yield/data/palette_data_network.csv",row.names = FALSE)

###create plattes
palette_phi_site<-c("#feeacd", "#614874", "#6e6c7d", "#27d41a", "#5aabff", "#44fffe", "#fec9e0", "#fe952e", "#62f97a", "#166179", "#fe47df", "#cbfaee", "#23ddb5", "#bdaaff", "#755b44", "#fe96be", "#9c9ca4", "#00858c", "#02ac4c", "#255ede", "#8e296c", "#946e2d", "#70a700", "#c3ff17", "#81c0cc", "#cc5047", "#fea4a5", "#b4b1a8", "#c4ad4e", "#ebcc28", "#af0c1c", "#fec462", "#6c8610", "#d66d33", "#3c3ae3", "#c42962", "#a343a3", "#b033e7", "#fe2311", "#148644", "#f696ff", "#c7bcd6", "#00612c", "#dbfab9","#feeacd", "#614874", "#6e6c7d", "#27d41a", "#5aabff", "#44fffe", "#fec9e0", "#fe952e", "#62f97a")
#names(palette_phi_site)<-fluxnet2015.sites$SITE_ID
#palette(palette_phi_site)
##########################################################################
#IGBP palette
##########################################################################
igbp_color<-paste0('#',c(
	'05450a', '086a10', '54a708', '78d203', '009900', 'c6b044', 'dcd159',
	'dade48', 'fbff13', 'b6ff05', '27ff87', 'c24f44', 'a5a5a5', 'ff6d4c',
	'69fff8', 'f9ffa4','1c0dff'
))
# get koppen and igdb md
igbp_md<-data.frame(LC_Type1=1:17,igbp=c('ENF','EBF','DNF','DBF','MF','CSH','OSH','WSA','SAV','GRA','WET','CRO','URB','CVM','SNO','BSV','WAT'),stringsAsFactors =F)
igbp_md$color<-igbp_color

##palettes
library(rasterVis)
##########################################################################
#3.create palettes
##########################################################################
rdbgee<-rev(c('040274', '040281', '0502a3', '0502b8', '0502ce', '0502e6',
	'0602ff', '235cb1', '307ef3', '269db1', '30c8e2', '32d3ef',
	'3be285', '3ff38f', '86e26f', '3ae237', 'b5e22e', 'd6e21f',
	'fff705', 'ffd611', 'ffb613', 'ff8b13', 'ff6e08', 'ff500d',
	'ff0000', 'de0101', 'c21301', 'a71001', '911003'))
grenbr<-colorRampPalette(c('#FFFFFF', '#CE7E45', '#DF923D', '#F1B555', '#FCD163', '#99B718', '#74A901',
	'#66A000', '#529400', '#023B01'), space = "Lab")
grenbr<-colorRampPalette(c( '#CE7E45', '#DF923D', '#F1B555', '#FCD163', '#99B718', '#74A901',
	'#66A000', '#529400', '#023B01'), space = "Lab")
soil_palette2<-grenbr(800)
rdbgee<-paste0('#',rdbgee)
rdbgee<-colorRampPalette(rdbgee, alpha=TRUE)
swepal<-colorRampPalette(c("white", "blue", "purple", "cyan", "green", "yellow", "red"), space = "Lab")
swepalpalet<-swepal(500)
aetpalet<-rdbgee(500)
#aetpalet<-rdbgee(100)
gc()

redb<- colorRampPalette(c("red",'lightyellow2','lightyellow1', "blue"), space = "Lab")	
palrb<-redb(800)
colramp = colorRampPalette(c('white','grey90', 'blue', 'green', 'yellow', 'red'), alpha = TRUE)

put.fig.letter <- function(label, location="topleft", x=NULL, y=NULL, 
                           offset=c(0, 0), ...) {
  if(length(label) > 1) {
    warning("length(label) > 1, using label[1]")
  }
  if(is.null(x) | is.null(y)) {
    coords <- switch(location,
                     topleft = c(0.04,0.94),
                     topcenter = c(0.5525,0.98),
                     topright = c(0.985, 0.98),
                     bottomleft = c(0.015, 0.02), 
                     bottomcenter = c(0.5525, 0.02), 
                     bottomright = c(0.985, 0.02),
                     c(0.015, 0.98) )
  } else {
    coords <- c(x,y)
  }
  #  topleft = c(0.015,0.98),
  this.x <- grconvertX(coords[1] + offset[1], from="nfc", to="user")
  this.y <- grconvertY(coords[2] + offset[2], from="nfc", to="user")
  text(labels=label[1], x=this.x, y=this.y, xpd=T, ...)
}

#define functions
library(rpmodel)
library(REddyProc)
library(data.table)
###################################################################################################
# # 04. remove outliers
# #################################################################################
rmOutliers<-function(df){
	df<-sapply(df,FUN=function(x){if(is.numeric(x)){x[x %in% boxplot.stats(x)$out]<-NA};x},simplify = F)
	as.data.frame(df)
}
###################################################################################################
# # estimate phi_0
# ####### linear

###hyperbolic saturation
get_phi_0_T_hyp<-function(filename,elev,Tleaf=TRUE){
	#filename=filenames.ameriflux[7];elev=avail_sites$Elevation..m.[7];Tleaf=FALSE
	###############################################################################################
	# 02.define the constants
	###############################################################################################
	###############################################################################################
	# 02.define the constants
	###############################################################################################
	kPo <- 101325       # standard atmosphere, Pa (Allen, 1973)
	kR <- 8.31447       # universal gas constant, J/mol/K (Moldover et al., 1988)
	kTo <- 288.15       # base temperature, K (Berberan-Santos et al., 1997)
	alb_s_vis<-0.17 	#soil albedo in the visible region of the spectrum (Feister, 1995)
	ksig <- 5.670374e-8    # Stefan-Boltzmann constant https://physics.nist.gov/cgi-bin/cuu/Value?sigma
	kalb_vis <- 0.03    # visible light albedo (Sellers, 1985)
	kfFEC <- 2.04       # from-flux-to-energy, umol/J (Meek et al., 1984)
	#############################################################################################################
	####1.0 read the data
	#############################################################################################################
	fluxnet_data<-data.table::fread(filename,  header=T, quote="\"", sep=",",na.strings = c("NA",'-9999'),colClasses='numeric',integer64="character")
	###1.1 remove outliers
	#fluxnet_data<-rmOutliers(fluxnet_data)
	#############################################################################################################
	#####3.0 get above canopy ppfd_in measurements
	#############################################################################################################
	
	nameppfd_in<-grep("^PPFD_IN", names(fluxnet_data), value = TRUE)
	nameppfd_in_qc<-grep("^PPFD_IN.*.QC", names(fluxnet_data), value = TRUE)
	nameppfd_in<-nameppfd_in[!(nameppfd_in %in% nameppfd_in_qc)]
	if(length(nameppfd_in)>1){
		PPFD_IN<-fluxnet_data[,..nameppfd_in]
		### replace error, equipment sensitivity
		PPFD_IN[PPFD_IN<0]<-0.0
		fluxnet_data$PPFD_IN<-rowMeans(PPFD_IN,na.rm=T)
	}else{
		fluxnet_data$PPFD_IN<-fluxnet_data[,..nameppfd_in]
	}
	fluxnet_data$PPFD_IN[fluxnet_data$PPFD_IN<0]<-0
	#############################################################################################################
	#####2.0 use direct fapar measurements if available
	#############################################################################################################
	namefapar<-grep("^FAPAR", names(fluxnet_data), value = TRUE)
	if(length(namefapar)>0 ){
		fluxnet_data$fapar<-fluxnet_data$FAPAR/100
	}else{
		#############################################################################################################
		#####2.0 get below canopy ppfd_in measurements
		#############################################################################################################
		nameppfd_in<-grep("^PPFD_BC_IN", names(fluxnet_data), value = TRUE)
		nameppfd_in_qc<-grep("^PPFD_BC_IN.*.QC", names(fluxnet_data), value = TRUE)
		nameppfd_in<-nameppfd_in[!(nameppfd_in %in% nameppfd_in_qc)]
		if(length(nameppfd_in)>1){
			PPFD_BC_IN<-fluxnet_data[,..nameppfd_in]
			### replace error, equipment sensitivity
			PPFD_BC_IN[PPFD_BC_IN<0]<-0.0
			fluxnet_data$PPFD_BC_IN<-rowMeans(PPFD_BC_IN,na.rm=T)
		}else{
			fluxnet_data$PPFD_BC_IN<-fluxnet_data[,..nameppfd_in]
		}
		fluxnet_data$PPFD_BC_IN[fluxnet_data$PPFD_BC_IN<0]<-0
		#############################################################################################################
		#####2.0 get ppfd outgoing
		#############################################################################################################
		nameppfd_out<-grep("^PPFD_OUT", names(fluxnet_data), value = TRUE)
		nameppfd_out_qc<-grep("^PPFD_OUT.*.QC", names(fluxnet_data), value = TRUE)
		nameppfd_out<-nameppfd_out[!(nameppfd_out %in% nameppfd_out_qc)]
		if(length(nameppfd_out)>1){
			PPFD_OUT<-fluxnet_data[,..nameppfd_out]
			### replace error, equipment sensitivity
			PPFD_OUT[PPFD_OUT<0]<-0.0
			fluxnet_data$PPFD_OUT<-rowMeans(PPFD_OUT,na.rm=T)
		}else if (length(nameppfd_out)==1){
			fluxnet_data$PPFD_OUT<-fluxnet_data[,..nameppfd_out]
		}else{
			nameswout<-grep("^SW_OUT", names(fluxnet_data), value = TRUE)
			if(length(nameswout)>1){
				SW_OUT<-fluxnet_data[,..nameswout]
				### replace error, equipment sensitivity
				SW_OUT[SW_OUT<0]<-0.0
				fluxnet_data$SW_OUT<-rowMeans(SW_OUT,na.rm=T)
				fluxnet_data$PPFD_OUT<-(kfFEC*(1 - kalb_vis)*(fluxnet_data$SW_OUT))
			}
		}
		
		fluxnet_data$PPFD_BC_IN[fluxnet_data$PPFD_BC_IN<0]<-0
		#############################################################################################################
		#####4. calc fapar and get Iabs
		#############################################################################################################
		fluxnet_data$fapar<-(fluxnet_data$PPFD_IN-fluxnet_data$PPFD_OUT-fluxnet_data$PPFD_BC_IN*(1-alb_s_vis))/fluxnet_data$PPFD_IN
		fluxnet_data$fapar[fluxnet_data$fapar<0]<-0
		fluxnet_data$fapar[fluxnet_data$fapar>1]<-1
	}
	
	
	
	fluxnet_data$Iabs<-fluxnet_data$fapar*fluxnet_data$PPFD_IN
	#############################################################################################################
	#####5.0 get NEE
	#############################################################################################################
	#choose NEE column if null, choose FC (carbon flux column)
	# if(length(grep("NEE", names(fluxnet_data), value = TRUE))==0 & length(grep("FC", names(fluxnet_data), value = TRUE))>=1){
	# 	namec<-grep("FC", names(fluxnet_data), value = TRUE)[1]
	# 	fluxnet_data$NEE_f<-fluxnet_data[,..namec]
	# 	fluxnet_data$NEE_fqc=rep(0,length(fluxnet_data[,1])) 
	# }else{
	# 	namec<-grep("NEE", names(fluxnet_data), value = TRUE)[1]
	# 	fluxnet_data$NEE_f<-fluxnet_data[,..namec]
	# 	fluxnet_data$NEE_fqc=rep(0,length(fluxnet_data[,1])) 
	# }
	# #some NEE_PI report NAs..??
	# if(sum(is.na(fluxnet_data$NEE_f))==length(fluxnet_data$NEE_f)){
	# 	namec<-grep("FC", names(fluxnet_data), value = TRUE)[1]
	# 	fluxnet_data$NEE_f<-fluxnet_data[,..namec]
	# }
	namec<-grep("FC", names(fluxnet_data), value = TRUE)
	namec_qc<-grep("^FC.*.QC", names(fluxnet_data), value = TRUE)
	namec<-namec[!(namec %in% namec_qc)]
	fluxnet_data$NEE<-rowMeans(fluxnet_data[,..namec],na.rm=T)
	
	fluxnet_data$NEE<-fluxnet_data$NEE*-1
	if(Tleaf){
		#############################################################################################################
		#####6.0 get surface temperature
		#############################################################################################################
		###############################################################################################
		# get surf temp
		###############################################################################################
		name_tcan<-grep("T_CANOPY", names(fluxnet_data), value = TRUE)
		name_tcan_qc<-grep("^T_CANOPY.*.QC", names(fluxnet_data), value = TRUE)
		name_tcan<-name_tcan[!(name_tcan %in% name_tcan_qc)]
		########## clac with air temperature
		
		if(length(name_tcan)>=1){
			fluxnet_data$T_CANOPY<-rowMeans(fluxnet_data[,..name_tcan],na.rm=T)
			
			#Calculate surfate temperature assuming emissivity 0.97
		}else{
			
			if(!is.null(fluxnet_data$LW_OUT)){
				if(!is.null(fluxnet_data$LW_IN_F)){
					fluxnet_data$T_CANOPY<-(((fluxnet_data$LW_OUT-(1-0.97)*fluxnet_data$LW_IN_F)/(ksig*0.97))^(1/4))-273.15
				}else if(!is.null(fluxnet_data$LW_IN)){
					fluxnet_data$T_CANOPY<-(((fluxnet_data$LW_OUT-(1-0.97)*fluxnet_data$LW_IN)/(ksig*0.97))^(1/4))-273.15
				}
				
			}else{
				return(NULL)
			}			
			
			
		}	
	}else{
		name_tcan<-grep("^TA", names(fluxnet_data), value = TRUE)
		name_tau<-grep("^TAU", names(fluxnet_data), value = TRUE)
		name_tcan_qc<-grep("^TA.*.QC", names(fluxnet_data), value = TRUE)
		name_tcan<-name_tcan[!(name_tcan %in% name_tcan_qc)]
		name_tcan<-name_tcan[!(name_tcan %in% name_tau)]
		if(length(name_tcan)>=1){
			fluxnet_data$T_CANOPY<-rowMeans(fluxnet_data[,..name_tcan],na.rm=T)
		}
	}
	###############################################################################################
	# get CO2
	###############################################################################################
	name_co2<-grep("^CO2", names(fluxnet_data), value = TRUE)
	name_co2_qc<-grep("^CO2.*.QC", names(fluxnet_data), value = TRUE)
	name_co2<-name_co2[!(name_co2 %in% name_co2_qc)]
	if(length(name_co2)>=1){
		fluxnet_data$CO2<-rowMeans(fluxnet_data[,..name_co2],na.rm=T)
	}else{
		fluxnet_data$CO2<-rep(NA,length(fluxnet_data[,1]))
	}
	###############################################################################################
	# get VPD
	###############################################################################################
	####get RH average
	if(length(grep("^RH$", names(fluxnet_data), value = TRUE))==0){
		namerh<-grep("^RH_", names(fluxnet_data), value = TRUE)
		fluxnet_data$RH<-rowMeans(fluxnet_data[,..namerh],na.rm=T)
		fluxnet_data$RH[fluxnet_data$RH>100]<-99.99
		
	}
	#calc vpd hPa if not provided
	if(length(grep("^VPD", names(fluxnet_data), value = TRUE))==0){
		fluxnet_data$RH[fluxnet_data$RH>100]<-99.99
		fluxnet_data$VPD_PI <- fCalcVPDfromRHandTair(rH=fluxnet_data$RH, Tair=fluxnet_data$T_CANOPY)
		
	}else if(length(grep("^VPD", names(fluxnet_data), value = TRUE))>=1){
		namevpd<-grep("^VPD_", names(fluxnet_data), value = TRUE)
		fluxnet_data$VPD_PI<-rowMeans(fluxnet_data[,..namevpd],na.rm=T)
		#fluxnet_data$VPD_PI<-fluxnet_data$VPD
	}
	fluxnet_data$VPD_PI[fluxnet_data$VPD_PI<0]<-0.0001
	###VPD frm hPa to Pa
	fluxnet_data$VPD_PI<-fluxnet_data$VPD_PI*100
	###############################################################################################
	# # Atmospheric pressure, Pa
	###############################################################################################
	
	if(!is.null(fluxnet_data$PA)){
		patm <-1000*fluxnet_data$PA
		if(!is.null(elev)){
			patm[is.na(patm)]<-calc_patm(elev)
		}
		
		
	}else if (is.null(fluxnet_data$PA) & !is.null(elev)){
		patm <- calc_patm(elev)
	}else{
		patm<-rep(NA,length(fluxnet_data[,1]))
	}
	###############################################################################################
	# compute mj using the least cost hypothesis
	###############################################################################################
	calc_mj<-function(tc,co2,vpd,Patm){
		#co2=df$CO2
		# tc=27-0.0065*elev	
		# fapar=0.85
		beta=146
		#calc patm (Aa)
		#atm<-patm(elev)
		#calc Gamma*
		gamstar<-rpmodel::calc_gammastar(tc,Patm)
		#calc ca Pa
		ca=rpmodel::co2_to_ca(co2,Patm)
		#viscosity
		kPo <- 101325.0       # standard atmosphere, Pa (Allen, 1973)
		kTo <- 25.0           # base temperature, deg C (Prentice, unpublished)
		ns      <- rpmodel::viscosity_h2o( tc, Patm )  # Pa s
		ns25    <- rpmodel::viscosity_h2o( kTo, kPo )  # Pa s
		ns_star <- ns / ns25  # (unitless)
		#calc michelis mentel
		kmm<-rpmodel::calc_kmm(tc,Patm)
		#calc chi
		## leaf-internal-to-ambient CO2 partial pressure (ci/ca) ratio
		xi  <- sqrt( (beta * ( kmm + gamstar ) ) / ( 1.6 * ns_star ) )
		chi <- gamstar / ca + ( 1.0 - gamstar / ca ) * xi / ( xi + sqrt(vpd) )
		ci=ca*chi
		mj<-((ci-gamstar)/(ci+2*gamstar))	# as.numeric(max(kphio,na.rm=T))
		#return(list(ci=ci,mj=mj))
		return(mj)
		#fapar instead of abs*(1-f) 
	}
	fluxnet_data$mj<-calc_mj(fluxnet_data$T_CANOPY,fluxnet_data$CO2,fluxnet_data$VPD_PI,patm)
	###############################################################################################
	# ccorrect absorbed Iabs by mj
	###############################################################################################
	fluxnet_data$Iabs<-fluxnet_data$Iabs*fluxnet_data$mj
	
	###1.1 remove outliers
	#fluxnet_data<-rmOutliers(fluxnet_data)
	#############################################################################################################
	#####7.0 sample temperatures
	#############################################################################################################
	maxtemp<-round(max(fluxnet_data$T_CANOPY,na.rm=T))
	TA_sampling<-1:maxtemp
	subset_TA<-function(T_sample,df){
		df<-subset(df,df$T_CANOPY<T_sample & df$T_CANOPY>=(T_sample-1))
		df
	}
	
	samples<-mapply(subset_TA,TA_sampling,MoreArgs = list(df=fluxnet_data),SIMPLIFY = F)
	#############################################################################################################
	#####8.0 get the upper envelopes 
	#############################################################################################################
	upper_env<-function(x,y,xbins){
		get_midpoint <- function(cut_label) {
			mean(as.numeric(unlist(strsplit(gsub("\\(|\\)|\\[|\\]", "", as.character(cut_label)), ","))))
		}
		xcuts<-cut(x,breaks=xbins)
		#lower: function(x){quantile(x,0.25)}
		yagg<-aggregate(y,by=list(xcuts),function(x){quantile(x,0.95,na.rm=T)})
		#yagg<-aggregate(y,by=list(xcuts),function(x){max(x,na.rm=T)})
		#yagg<-aggregate(y,by=list(xcuts),median,na.rm=T)
		yagg$xMidpoint <- sapply(yagg$Group.1, get_midpoint)
		# mody <- loess(yagg$x~yagg$xMidpoint)
		# data.frame(x=yagg$xMidpoint,y=mody$fitted)
		data.frame(x=yagg$xMidpoint,y=yagg$x)
	}
	#############################################################################################################
	#####8.0 get the median per interval
	#############################################################################################################
	median_per_int<-function(x,y,xbins){
		get_midpoint <- function(cut_label) {
			mean(as.numeric(unlist(strsplit(gsub("\\(|\\)|\\[|\\]", "", as.character(cut_label)), ","))))
		}
		xcuts<-cut(x,breaks=xbins)
		#lower: function(x){quantile(x,0.25)}
		#yagg<-aggregate(y,by=list(xcuts),function(x){quantile(x,0.95,na.rm=T)})
		#yagg<-aggregate(y,by=list(xcuts),function(x){max(x,na.rm=T)})
		yagg<-aggregate(y,by=list(xcuts),median,na.rm=T)
		yagg$xMidpoint <- sapply(yagg$Group.1, get_midpoint)
		# mody <- loess(yagg$x~yagg$xMidpoint)
		# data.frame(x=yagg$xMidpoint,y=mody$fitted)
		data.frame(x=yagg$xMidpoint,y=yagg$x)
	}
	##function to fit the upper envelope
	
	get_LIC_par<-function(df){
		if(length(df$Iabs)>3 & round(max(df$Iabs,na.rm = T))>50){
			max_Iabs<-round(max(df$Iabs,na.rm = T))
			df_up_env<-upper_env(df$Iabs,df$NEE,seq(0,max_Iabs,50))
			
		}else{
			df_up_env<-data.frame(x=rep(NA,length(df$Iabs)),y=rep(NA,length(df$Iabs)))
		}
		
		
		df_up_env
		
	}
	
	max_LRC<-lapply(samples,get_LIC_par)
	# get_LIC_par(samples[[3]])
	# 
	# max_LRC<-list()
	# for(i in 1:49){
	# 	max_LRC[[i]]<-get_LIC_par(samples[[i]])
	# }
	
	#############################################################################################################
	#####9.0 fit phi_o
	#############################################################################################################
	fit_phi_0<-function(df){
		
		# clcheck<-try(phi_lm<-lm(y~x,data=df[df$x<300,]), silent=TRUE)
		# 
		# suma_phi_lm<-summary(phi_lm)
		# if (suma_phi_lm$coefficients[1,1]>0){
		# 	phi_lm<-lm(y~0+x,data=df[df$x<300,])
		# }
		Jmax=max(df$y,na.rm=T)*4
		clcheck<-try(phi_nls<-nls(y~phi0*x*(1/sqrt(1+(4*phi0*x/Jmax)^2)),data=df,start=c(phi0=0.05)), silent=TRUE)
		#curve(phi0_init*x*(1/sqrt(1+(4*phi0_init*x/Jmax)^2)),add=T)
		
		
		if(class(clcheck)=="try-error" | sum(df$x<300)<4 | sum(!is.na(df$y))<4){
			result<-rep(NA,5)
			names(result)<-c("phi_0", "phi_0.SE","phi_0.pval","R2","Reco_I_0"  )
			result<-as.data.frame(t(result))
		}else{
			#############
			#without any correction
			# phi0_fT_coef<-summary(phi_nls)
			# 	result<-data.frame(phi_0=phi0_fT_coef$coefficients[1,1],phi_0.SE=phi0_fT_coef$coefficients[1,2],
			# 		phi_0.pval=phi0_fT_coef$coefficients[1,4],R2= hydroGOF::br2(predict(phi_nls,df),df$y),Reco_I_0=phi0_fT_coef$coefficients[2,1])
			#applying correction when low values of A are observed at saturation
			init_approx<-predict(phi_nls,df)
			df_new=df
			#df_new<-subset(df,df$y>=init_approx)
			df_new$y[!is.na(df$y)& df$y<init_approx]<-init_approx[!is.na(df$y)& df$y<init_approx]
			### if points too few
			if (length(df_new$y)<4 | df_new$x[1]>500){
				df_new=df
			}
			phi0_init<-max(as.numeric(coef(phi_nls)),0.0001)
			
			clcheck<-try(phi_nls<-nls(y~(phi0*x*(1/sqrt(1+(4*phi0*x/Jmax)^2)))-Reco_I_0,data=df_new,start=c(phi0=phi0_init,Reco_I_0=0.5),
				algorithm="port",lower=c(phi0_init*0.25,-1), upper=c(0.12,5.0)), silent=TRUE)
		if(class(clcheck)=="try-error"){
			result<-rep(NA,5)
			names(result)<-c("phi_0", "phi_0.SE","phi_0.pval","R2","Reco_I_0"  )
			result<-as.data.frame(t(result))
		}else{
			phi0_fT_coef<-summary(phi_nls)
			result<-data.frame(phi_0=phi0_fT_coef$coefficients[1,1],phi_0.SE=phi0_fT_coef$coefficients[1,2],
				phi_0.pval=phi0_fT_coef$coefficients[1,4],R2= hydroGOF::br2(predict(phi_nls,df_new),df_new$y),Reco_I_0=phi0_fT_coef$coefficients[2,1])
		}
		
		# curve((phi0_fT_coef$coefficients[1,1]*x*(1/sqrt(1+(4*phi0_fT_coef$coefficients[1,1]*x/Jmax)^2)))-phi0_fT_coef$coefficients[2,1],add=T)	
		
		
	}
	
	return(result)
	
}


# phi_o<-list()
# for(i in 1:34){
# 	phi_o[[i]]<-fit_phi_0(max_LRC[[i]])
# }

phi_o<-lapply(max_LRC,fit_phi_0)
phi_o<-do.call(rbind,phi_o)
phi_o$T_sample<-TA_sampling-0.5
##get rid anomalous values
phi_o$phi_0[phi_o$phi_0<0 | phi_o$phi_0>=0.120]<-NA
##get relative efficiency
phi_o$PSII_eff<-phi_o$phi_0/max(phi_o$phi_0,na.rm=T)

phi_o

}
#Fluxnet local
###################################################################################################
# # estimate phi_0
# #################################################################################
get_phi_0_T_flx<-function(filename,elev,use_Tsurf=FALSE, use_NEE=FALSE){
	#filename=filenames.fluxkit[112];fluxkit.sites$elev[112];use_Tsurf=FALSE;use_NEE=TRUE
	###############################################################################################
	# 02.define the constants
	###############################################################################################
	kPo <- 101325       # standard atmosphere, Pa (Allen, 1973)
	kR <- 8.31447       # universal gas constant, J/mol/K (Moldover et al., 1988)
	kTo <- 288.15       # base temperature, K (Berberan-Santos et al., 1997)
	alb_s_vis<-0.17 	#soil albedo in the visible region of the spectrum (Feister, 1995)
	ksig <- 5.670374e-8    # Stefan-Boltzmann constant https://physics.nist.gov/cgi-bin/cuu/Value?sigma
	kalb_vis <- 0.03    # visible light albedo (Sellers, 1985)
	kfFEC <- 2.04       # from-flux-to-energy, umol/J (Meek et al., 1984)
	kfus <- 334000      # latent heat of fusion J/Kg (Monteith & Unsworth, 1990)
	kG <- 9.80665       # gravitational acceleration, m/s^2 (Allen, 1973)
	#############################################################################################################
	####1.0 read the data
	#############################################################################################################
	fluxnet_data<-data.table::fread(filename,  header=T, quote="\"", sep=",",na.strings = c("NA",'-9999'),colClasses='numeric',integer64="character")
	###1.1 remove outliers
	#fluxnet_data<-rmOutliers(fluxnet_data)
	ind<-strptime(fluxnet_data$TIMESTAMP_START,format="%Y%m%d%H%M",tz="GMT")
	time.freq<-abs(as.numeric(ind[1]-ind[2], units = "hours"))
	t_conv_f<-3600*time.freq
	site<-do.call(rbind,strsplit(basename(filename),'_'))[,2]
	#############################################################################################################
	#####3.0 get above canopy ppfd_in measurements umol/m2/s
	#############################################################################################################
	
	if(!is.null(fluxnet_data$PPFD_IN)){
		fluxnet_data$PPFD_IN[fluxnet_data$PPFD_IN<=0]<-NA
	}else{
		fluxnet_data$PPFD_IN<-(kfFEC*(1 - kalb_vis)*(fluxnet_data$SW_IN_F_MDS))
		fluxnet_data$PPFD_IN[fluxnet_data$PPFD_IN<=0]<-NA
		
	}
	#### get only good quality data
	fluxnet_data$PPFD_IN[fluxnet_data$SW_IN_F_MDS_QC>2]<-NA
	
	
	#############################################################################################################
	#####2.0 get fapar
	#############################################################################################################
	fluxnet_data$Iabs<-fluxnet_data$FPAR*fluxnet_data$PPFD_IN
	#############################################################################################################
	#####5.0 get NEE
	#############################################################################################################
	if(use_NEE){
		fluxnet_data$NEE<-fluxnet_data$NEE_VUT_REF*-1
	}else{
		if(!is.null(fluxnet_data$GPP_NT_VUT_REF)){
			fluxnet_data$NEE<-fluxnet_data$GPP_NT_VUT_REF
		}else{
			fluxnet_data$NEE<-fluxnet_data$GPP_DT_VUT_REF
		}
	}
	
	
	###subset only observations
	fluxnet_data<-subset(fluxnet_data,fluxnet_data$NEE_VUT_REF_QC<=2)
	
	if(use_Tsurf){
		#############################################################################################################
		#####6.0 get surface temperature
		#############################################################################################################
		###############################################################################################
		# get surf temp
		###############################################################################################
		name_tcan<-grep("T_CANOPY", names(fluxnet_data), value = TRUE)
		name_tcan_qc<-grep("^T_CANOPY.*.QC", names(fluxnet_data), value = TRUE)
		name_tcan<-name_tcan[!(name_tcan %in% name_tcan_qc)]
		########## clac with air temperature
		
		if(length(name_tcan)>=1){
			fluxnet_data$T_CANOPY<-rowMeans(fluxnet_data[,..name_tcan],na.rm=T)
			
			#Calculate surfate temperature assuming emissivity 0.97
		}else{
			
			if(!is.null(fluxnet_data$LW_OUT)){
				if(!is.null(fluxnet_data$LW_IN_F)){
					fluxnet_data$T_CANOPY<-(((fluxnet_data$LW_OUT-(1-0.97)*fluxnet_data$LW_IN_F)/(ksig*0.97))^(1/4))-273.15
				}else if(!is.null(fluxnet_data$LW_IN)){
					fluxnet_data$T_CANOPY<-(((fluxnet_data$LW_OUT-(1-0.97)*fluxnet_data$LW_IN)/(ksig*0.97))^(1/4))-273.15
				}
				
			}else{
				return(NULL)
			}			
			
			
		}	
	}else{
		fluxnet_data$T_CANOPY<-fluxnet_data$TA_F_MDS
	}
	###############################################################################################
	# get CO2
	###############################################################################################
	name_co2<-grep("^CO2", names(fluxnet_data), value = TRUE)
	name_co2_qc<-grep("^CO2.*.QC", names(fluxnet_data), value = TRUE)
	name_co2<-name_co2[!(name_co2 %in% name_co2_qc)]
	if(length(name_co2)>=1){
		fluxnet_data$CO2<-rowMeans(fluxnet_data[,..name_co2],na.rm=T)
	}else{
		fluxnet_data$CO2<-rep(NA,length(fluxnet_data[,1]))
	}
	###############################################################################################
	# get VPD
	###############################################################################################
	###VPD frm hPa to Pa
	fluxnet_data$VPD_PI<-fluxnet_data$VPD_F_MDS*100
	fluxnet_data$VPD_PI[fluxnet_data$VPD_PI<0]<-0.0001
	###############################################################################################
	# # Atmospheric pressure, Pa
	###############################################################################################
	
	if(!is.null(fluxnet_data$PA_F)){
		patm <-1000*fluxnet_data$PA_F
		if(!is.null(elev)){
			patm[is.na(patm)]<-calc_patm(elev)
		}
		
		
	}else if (is.null(fluxnet_data$PA_F) & !is.null(elev)){
		patm <- calc_patm(elev)
	}else{
		patm<-rep(NA,length(fluxnet_data[,1]))
	}
	###############################################################################################
	# compute mj using the least cost hypothesis
	###############################################################################################
	calc_mj<-function(tc,co2,vpd,Patm){
		#co2=df$CO2
		# tc=27-0.0065*elev	
		# fapar=0.85
		beta=146
		#calc patm (Aa)
		#atm<-calc_patm(elev)
		#calc Gamma*
		gamstar<-rpmodel::calc_gammastar(tc,Patm)
		#calc ca Pa
		ca=rpmodel::co2_to_ca(co2,Patm)
		#viscosity
		kPo <- 101325.0       # standard atmosphere, Pa (Allen, 1973)
		kTo <- 25.0           # base temperature, deg C (Prentice, unpublished)
		ns      <- rpmodel::viscosity_h2o( tc, Patm )  # Pa s
		ns25    <- rpmodel::viscosity_h2o( kTo, kPo )  # Pa s
		ns_star <- ns / ns25  # (unitless)
		#calc michelis mentel
		kmm<-rpmodel::calc_kmm(tc,Patm)
		#calc chi
		## leaf-internal-to-ambient CO2 partial pressure (ci/ca) ratio
		xi  <- sqrt( (beta * ( kmm + gamstar ) ) / ( 1.6 * ns_star ) )
		chi <- gamstar / ca + ( 1.0 - gamstar / ca ) * xi / ( xi + sqrt(vpd) )
		ci=ca*chi
		mj<-((ci-gamstar)/(ci+2*gamstar))	# as.numeric(max(kphio,na.rm=T))
		#return(list(ci=ci,mj=mj))
		return(mj)
		#fapar instead of abs*(1-f) 
	}
	fluxnet_data$mj<-calc_mj(fluxnet_data$T_CANOPY,fluxnet_data$CO2,fluxnet_data$VPD_PI,patm)
	###############################################################################################
	# ccorrect absorbed Iabs by mj
	###############################################################################################
	fluxnet_data$Iabs<-fluxnet_data$Iabs*fluxnet_data$mj
	
	###1.1 remove outliers
	#fluxnet_data<-rmOutliers(fluxnet_data)
	#############################################################################################################
	#####7.0 sample temperatures
	#############################################################################################################
	maxtemp<-round(max(fluxnet_data$T_CANOPY,na.rm=T))
	TA_sampling<-1:maxtemp
	subset_TA<-function(T_sample,df){
		df<-subset(df,df$T_CANOPY<T_sample & df$T_CANOPY>=(T_sample-1))
		df
	}
	
	samples<-mapply(subset_TA,TA_sampling,MoreArgs = list(df=fluxnet_data),SIMPLIFY = F)
	#############################################################################################################
	#####8.0 get the upper envelopes 
	#############################################################################################################
	upper_env<-function(x,y,xbins){
		get_midpoint <- function(cut_label) {
			mean(as.numeric(unlist(strsplit(gsub("\\(|\\)|\\[|\\]", "", as.character(cut_label)), ","))))
		}
		xcuts<-cut(x,breaks=xbins)
		#lower: function(x){quantile(x,0.25)}
		yagg<-aggregate(y,by=list(xcuts),function(x){quantile(x,0.95,na.rm=T)})
		#yagg<-aggregate(y,by=list(xcuts),function(x){max(x,na.rm=T)})
		#yagg<-aggregate(y,by=list(xcuts),median,na.rm=T)
		yagg$xMidpoint <- sapply(yagg$Group.1, get_midpoint)
		# mody <- loess(yagg$x~yagg$xMidpoint)
		# data.frame(x=yagg$xMidpoint,y=mody$fitted)
		data.frame(x=yagg$xMidpoint,y=yagg$x)
	}
	#############################################################################################################
	#####8.0 get the median per interval
	#############################################################################################################
	median_per_int<-function(x,y,xbins){
		get_midpoint <- function(cut_label) {
			mean(as.numeric(unlist(strsplit(gsub("\\(|\\)|\\[|\\]", "", as.character(cut_label)), ","))))
		}
		xcuts<-cut(x,breaks=xbins)
		#lower: function(x){quantile(x,0.25)}
		#yagg<-aggregate(y,by=list(xcuts),function(x){quantile(x,0.95,na.rm=T)})
		#yagg<-aggregate(y,by=list(xcuts),function(x){max(x,na.rm=T)})
		yagg<-aggregate(y,by=list(xcuts),median,na.rm=T)
		yagg$xMidpoint <- sapply(yagg$Group.1, get_midpoint)
		# mody <- loess(yagg$x~yagg$xMidpoint)
		# data.frame(x=yagg$xMidpoint,y=mody$fitted)
		data.frame(x=yagg$xMidpoint,y=yagg$x)
	}
	##function to fit the upper envelope
	
	get_LIC_par<-function(df){
		if(length(df$Iabs)>3 & round(max(df$Iabs,na.rm = T))>50){
			max_Iabs<-round(max(df$Iabs,na.rm = T))
			df_up_env<-upper_env(df$Iabs,df$NEE,seq(0,max_Iabs,50))
			
		}else{
			df_up_env<-data.frame(x=rep(NA,length(df$Iabs)),y=rep(NA,length(df$Iabs)))
		}
		
		
		df_up_env
		
	}
	
	max_LRC<-lapply(samples,get_LIC_par)
	# get_LIC_par(samples[[3]])
	# 
	# max_LRC<-list()
	# for(i in 1:49){
	# 	max_LRC[[i]]<-get_LIC_par(samples[[i]])
	# }
	
	#############################################################################################################
	#####9.0 fit phi_o
	#############################################################################################################
	fit_phi_0<-function(df){
		
		# clcheck<-try(phi_lm<-lm(y~x,data=df[df$x<300,]), silent=TRUE)
		# 
		# suma_phi_lm<-summary(phi_lm)
		# if (suma_phi_lm$coefficients[1,1]>0){
		# 	phi_lm<-lm(y~0+x,data=df[df$x<300,])
		# }
		Jmax=max(df$y,na.rm=T)*4
		clcheck<-try(phi_nls<-nls(y~phi0*x*(1/sqrt(1+(4*phi0*x/Jmax)^2)),data=df,start=c(phi0=0.05)), silent=TRUE)
		#curve(phi0_init*x*(1/sqrt(1+(4*phi0_init*x/Jmax)^2)),add=T)
		
		
		if(class(clcheck)=="try-error" | sum(df$x<300)<4 | sum(!is.na(df$y))<4){
			result<-rep(NA,5)
			names(result)<-c("phi_0", "phi_0.SE","phi_0.pval","R2","Reco_I_0"  )
			result<-as.data.frame(t(result))
		}else{
			#############
			#without any correction
			# phi0_fT_coef<-summary(phi_nls)
			# 	result<-data.frame(phi_0=phi0_fT_coef$coefficients[1,1],phi_0.SE=phi0_fT_coef$coefficients[1,2],
			# 		phi_0.pval=phi0_fT_coef$coefficients[1,4],R2= hydroGOF::br2(predict(phi_nls,df),df$y),Reco_I_0=phi0_fT_coef$coefficients[2,1])
			#applying correction when low values of A are observed at saturation
			init_approx<-predict(phi_nls,df)
			df_new=df
			#df_new<-subset(df,df$y>=init_approx)
			#df_new$y[df$y<init_approx]<-init_approx[df$y<init_approx]
			df_new$y<-pmin(df$y,init_approx,na.rm=T)
			### if points too few
			if (length(df_new$y)<4 | df_new$x[1]>500){
				df_new=df
			}
			phi0_init<-max(as.numeric(coef(phi_nls)),0.0001)
			if(phi0_init>0.125){phi0_init=0.087}
			################NOTE##########
			## for testing the algorithm with GPP instead of NEE, set the values of Reco_I_0=0.0 in the nls function.
			if(use_NEE){
				lowlims=c(phi0_init*0.25,-0.001)
				uplims=c(0.125,15.0)
			}else{
				lowlims=c(phi0_init*0.25,-0.001)
				uplims=c(0.125,0.001)
			}
			
			clcheck<-try(phi_nls<-nls(y~(phi0*x*(1/sqrt(1+(4*phi0*x/Jmax)^2)))-Reco_I_0,
				data=df_new,start=c(phi0=phi0_init,Reco_I_0=0.000),
				algorithm="port",lower=lowlims, upper=uplims), 
			silent=TRUE)
		if(class(clcheck)=="try-error"){
			result<-rep(NA,5)
			names(result)<-c("phi_0", "phi_0.SE","phi_0.pval","R2","Reco_I_0"  )
			result<-as.data.frame(t(result))
		}else{
			phi0_fT_coef<-summary(phi_nls)
			result<-data.frame(phi_0=phi0_fT_coef$coefficients[1,1],phi_0.SE=phi0_fT_coef$coefficients[1,2],
				phi_0.pval=phi0_fT_coef$coefficients[1,4],R2= hydroGOF::br2(predict(phi_nls,df_new),df_new$y),Reco_I_0=phi0_fT_coef$coefficients[2,1])
			
			
		}
		
		# curve((phi0_fT_coef$coefficients[1,1]*x*(1/sqrt(1+(4*phi0_fT_coef$coefficients[1,1]*x/Jmax)^2)))-phi0_fT_coef$coefficients[2,1],add=T)	
		
		
	}
	
	return(result)
	
}




phi_o<-lapply(max_LRC,fit_phi_0)
phi_o<-do.call(rbind,phi_o)
phi_o$T_sample<-TA_sampling-0.5
##get rid anomalous values
phi_o$phi_0[phi_o$phi_0<0 | phi_o$phi_0>=0.12]<-NA
##get relative efficiency
phi_o$PSII_eff<-phi_o$phi_0/max(phi_o$phi_0,na.rm=T)

phi_o

}

#########################################
###Tukey comparisons
#####################################
get_cld<- function(df, var,grp){
	fm <- as.formula(paste(var, "~", grp))
	
	# analysis of variance
	anova <- aov(fm, data = df)
	tukey_test <- TukeyHSD(anova)
	# # # Extract labels and factor levels from Tukey post-hoc 
	Tukey.levels <- tukey_test[[grp]][,4]
	Tukey.labels <- data.frame(multcompView::multcompLetters(Tukey.levels)['Letters'])
	# bp <- boxplot(fm,data=df,plot=F)
	# upper<-as.numeric(bp$stats[4,])
	# # 
	# # #I need to put the labels in the same order as in the boxplot :
	#Tukey.labels$group=rownames(Tukey.labels)
	# # Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
	# #return(Tukey.labels)
	# Tukey.labels$upper<-upper
	Tukey.labels$group<-as.character(row.names(Tukey.labels))
	# Tukey.labels
	#####################ggplot
	# table with factors and 3rd quantile
	Tk <- group_by(df, eval(parse(text = grp))) %>%
	summarise(mean=mean(eval(parse(text = var)),na.rm=T),sd=sd(eval(parse(text = var)),na.rm=T) ,quant = quantile(eval(parse(text = var)), probs = 0.75,na.rm=T))
	#%>%	# arrange(desc(mean))
	names(Tk)<-c(grp,var,'sd','quant')
	# extracting the compact letter display and adding to the Tk table
	# cld <- as.data.frame.list(cld$feed)
	# Tk$cld <- cld$Letters
	Tukey.labels<-merge(Tukey.labels,Tk, by.x='group',by.y=grp)
	Tukey.labels
	}
###############################################################################################
# fit optima nls simple cuadratic eqn
###############################################################################################	
get_stats_table<-function(df, var,grp){
	tukey_result<- get_cld(df, var,grp)
	basic_stats<-phi_0_ame_Tair.df %>%                               # Summary by group using dplyr
		group_by(Vegetation.Abbreviation..IGBP.) %>% 
		summarize(min = min(phi_0,na.rm=T),
		median = median(phi_0,na.rm=T),
		mean = mean(phi_0,na.rm=T),
		SD = sd(phi_0,na.rm=T),
		max = max(phi_0,na.rm=T))
	basic_stats$CV<-(basic_stats$SD/basic_stats$mean)*100
	basic_stats<-merge(clds_phi_0,basic_stats,by.x='group',by.y='Vegetation.Abbreviation..IGBP.')
	
	
	round_df <- function(df, digits) {
		nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
		
		df[,nums] <- round(df[,nums], digits = digits)
		
		(df)
	}
	
	basic_stats<-round_df(basic_stats, digits=4)
	basic_stats$range<-paste0(basic_stats$min,'-',basic_stats$max)
	
	
}
###############################################################################################
# fit optima nls simple cuadratic eqn
###############################################################################################	
	
	get_opt_phi<-function(df){
		if(sum(!is.na(df$phi_0))>=4){
			##bounds
			maxphio_init<-as.numeric(quantile(df$phi_0,0.95,na.rm=T))
			tmin<-min(df$T_sample[!is.na(df$phi_0)])
			tmax<-max(df$T_sample[!is.na(df$phi_0)])
			clcheck<-try(op_phi_0<-nls(phi_0~phio_init-r*((T_sample)-Topt)^2,data=df,start=c(phio_init=maxphio_init,r=1e-4,Topt=20),
				algorithm="port", 	lower=c(maxphio_init*0.8,1e-5,tmin), upper=c(maxphio_init*1.1,3e-4,tmax)),
			silent=TRUE)
		
		if(class(clcheck)=="try-error"){
			result=data.frame(phio_init=NA, r=NA, Topt=NA,RSS=NA)
		}else{
			pars<-coefficients(op_phi_0)
			rss <- sum(resid(op_phi_0)^2)
			result=data.frame(phio_init=maxphio_init, r=pars['r'], Topt=pars['Topt'],RSS=rss)
		}
	}else{
		result=data.frame(phio_init=NA, r=NA, Topt=NA,RSS=NA)
	}	
	return(result)
}
###############################################################################################
# Modified arrhenius eqn no acclimation
###############################################################################################	
no_acc_f_arr<-function (tcleaf,Ha =71513,Hd= 2e+05,dent=649) {
	
	###10.1111/nph.16883
	Rgas <- 8.3145 #J/mol/K
	##fix for optimization
	if(!is.na(Ha)& !is.na(Hd)& Ha>Hd){
		Ha<-Hd-1
	}
	Top<-Hd/(dent-Rgas*log(Ha/(Hd-Ha)))
	tkleaf <- tcleaf + 273.15
	###################change to Medlyn et al. (2002)
	f1= exp((Ha*(tkleaf-Top))/(Top*Rgas*tkleaf)) 
	f2=1+exp((Top*dent-Hd)/(Top*Rgas))
	f3=1+exp((tkleaf*dent-Hd)/(tkleaf*Rgas))
	
	farr<- f1*(f2/f3)
	
	return(farr)
}
###############################################################################################
# Modified arrhenius eqn no acclimation
###############################################################################################	
f_arr_nls<-function (T_sample,Ha =71513,dent=649) {
	
	###10.1111/nph.16883
	Rgas <- 8.3145 #J/mol/K
	Hd=295*dent
	maxphio=0.11
	##fix for optimization
	# if(!is.na(Ha)& !is.na(Hd)& Ha>Hd){
	# 	Ha<-Hd-1
	# }
	Top<-Hd/(dent-Rgas*log(Ha/(Hd-Ha)))
	tkleaf <- T_sample + 273.15
	###################change to Medlyn et al. (2002)
	f1= exp((Ha*(tkleaf-Top))/(Top*Rgas*tkleaf)) 
	f2=1+exp((Top*dent-Hd)/(Top*Rgas))
	f3=1+exp((tkleaf*dent-Hd)/(tkleaf*Rgas))
	
	farr<- maxphio*f1*(f2/f3)
	
	return(farr)
}
###############################################################################################
# fit optima Arrhenius unbounded parameters
###############################################################################################	

library(dfoptim)

wrap_optim_arr<-function(df){
	
	if(sum(!is.na(df$phi_0))>=4){
		maxphio=quantile(df$phi_0,0.95,na.rm=T)
		optimize_phi0<-function(coeffs){
			
			phi_T=maxphio*no_acc_f_arr(tcleaf=df$T_sample,Ha =coeffs[1],Hd=coeffs[2],dent=coeffs[3])
			-hydroGOF::NSE(as.numeric(phi_T),as.numeric(df$phi_0))
			#msep_sim=MSEP_comp(as.numeric(phi_T),as.numeric(df$phi_0))
			#msep_sim['MSEP']
			
		}
		par_optim_ksat<-nmkb(par=c(70000,2e+05,649),fn=optimize_phi0,lower = c(20000,100000,100), upper =c(200000,4e+05,2000))
		result=data.frame(maxphio=maxphio,Ha =par_optim_ksat$par[1],Hd=par_optim_ksat$par[2],dent=par_optim_ksat$par[3],NSE=-1*par_optim_ksat$value)		
	}else{
		result=data.frame(maxphio=NA,Ha =NA,Hd=NA,dent=NA,NSE=NA)	
	}
	
	
	return(result)
}
###############################################################################################
# fit optima Arrhenius 2nd apprximation bounded parameters
###############################################################################################
wrap_optim_arr_v2<-function(df){
	
	if(sum(!is.na(df$phi_0))>=4){
		maxphio=quantile(df$phi_0,0.95,na.rm=T)
		optimize_phi0<-function(coeffs){
			
			Hd_coef<- 295 *coeffs[2]
			#Hd_coef<-coeffs[3]*coeffs[2]
			phi_T=coeffs[3]*no_acc_f_arr(tcleaf=df$T_sample,Ha =coeffs[1],Hd=Hd_coef,dent=coeffs[2])
			-hydroGOF::NSE(as.numeric(phi_T),as.numeric(df$phi_0))
			#msep_sim=MSEP_comp(as.numeric(phi_T),as.numeric(df$phi_0))
			#msep_sim['MSEP']
			
		}
		##1st approx
		#par_optim_ksat<-nmkb(par=c(70000,649),fn=optimize_phi0,lower = c(50000,300), upper =c(80000,800))
		##glob approx
		par_optim_ksat<-optim(par=c(71000,649,maxphio),fn=optimize_phi0,lower = c(59001,200,maxphio*0.75), upper =c(110000,2000,maxphio*1.25))
		result=data.frame(maxphio=par_optim_ksat$par[3], 
			Ha =par_optim_ksat$par[1],
			Hd=295*par_optim_ksat$par[2],
			dent=par_optim_ksat$par[2],
			NSE=-1*par_optim_ksat$value)		
	}else{
		result=data.frame(maxphio=NA,Ha =NA,Hd=NA,dent=NA,NSE=NA)	
	}
	
	
	return(result)
}
###############################################################################################
# fit optima Arrhenius 3rd approxx
###############################################################################################	
wrap_optim_arr_v3<-function(df){
	
	if(sum(!is.na(df$phi_0))>=4){
		maxphio=quantile(df$phi_0,0.95,na.rm=T)
		optimize_phi0<-function(coeffs){
			
			#Hd_coef<- 295 *coeffs[2]
			#Hd_coef<-coeffs[3]*coeffs[2]
			phi_T=coeffs[3]*no_acc_f_arr(tcleaf=df$T_sample,Ha = 75000,Hd=coeffs[1],dent=coeffs[2])
			#-hydroGOF::NSE(as.numeric(phi_T),as.numeric(df$phi_0))
			msep_sim=MSEP_comp(as.numeric(phi_T),as.numeric(df$phi_0))
			msep_sim['MSEP']
		}
		##1st approx
		#par_optim_ksat<-nmkb(par=c(70000,649),fn=optimize_phi0,lower = c(50000,300), upper =c(80000,800))
		##glob approx
		par_optim_ksat<-optim(par=c(200000,649,maxphio),fn=optimize_phi0,lower = c(100000,300,maxphio*0.8), upper =c(300000,2000,maxphio*1.1))
		result=data.frame(maxphio=par_optim_ksat$par[3], 
			Ha =75000,
			Hd=par_optim_ksat$par[1],
			dent=par_optim_ksat$par[2],
			MSEP=par_optim_ksat$value)		
	}else{
		result=data.frame(maxphio=NA,Ha =NA,Hd=NA,dent=NA,MSEP=NA)	
	}
	
	
	return(result)
}
###############################################################################################
# add predictions to dfs
###############################################################################################	
add_pred<-function(df,maxphi0,optemp,spread,Ha,Hd,dent){
	
	df$phi0_nls<-maxphi0-spread*((df$T_sample)-optemp)^2
	df$phi0_arr<-maxphi0*no_acc_f_arr(tcleaf=df$T_sample,
		Ha =Ha,Hd=Hd,dent=dent)
	df
}

MSEP_comp<-function(sim,obs,...){
	# ************************************************************************
	# Name:     MSEP_comp
	# Inputs:   - double - vector (sim), simulations
	#           - double - vector (obs), observations
	# Returns:  double, vector MSEP,MC,SC,RC
	# Features: This function calculates the mean squared error of prediction and ots components:bias error (MC), slope error (SC) and random error (RC)
	# * Ref:    Haefner, 2005. Modeling Biological Systems: Principles and Applications. Springer
	# ************************************************************************
	###############################################################################################
	# 01.define vars
	###############################################################################################
	X=mean(sim,na.rm=T);Y=mean(obs,na.rm=T);Sx=sd(sim,na.rm=T);Sy=sd(obs,na.rm=T);r=cor(sim,obs,use="complete.obs")
	#bias error
	MCe=(X-Y)^2
	##slope error
	SCe=(Sx-r*Sy)^2
	##random error
	RCe=(1-r^2)*Sy^2
	###mean squared error of prediction
	MSEP=MCe+SCe+RCe
	err=c(MSEP=MSEP,MC=MCe/MSEP,SC=SCe/MSEP,RC=RCe/MSEP)
	return(err)
	
	
}
###############################################################################################
# Theoretical phi0
###############################################################################################

calc_phi0_Farq<-function(tc,elev,co2,f=0.15,chi=0.7,abst=0.85,Gstaropt='pmodel'){
	
	patm<-rpmodel::calc_patm(elev)
	dhac <- 79430
	dhao <- 36380
	kco <- 209476
	kc25 <- 39.97
	ko25 <- 27480
	Rgas <- 8.3145 #J/mol/K
	ca<-rpmodel::co2_to_ca(co2,patm)
	tk <- tc + 273.15
	if(Gstaropt=='pmodel'){
		photo_resp<-rpmodel::calc_gammastar(tc,patm)
	}else if(Gstaropt=='clm'){
		Tref = 25+273.15
		G_ref=42.75 #umol/mol
		G_ref=rpmodel::co2_to_ca(G_ref,patm) # Pa
		Ea=37.83*1000 #activation energy J/mol
		photo_resp=G_ref * exp((Ea*(tk-Tref))/(Tref*Rgas*tk))
		
	}else if(Gstaropt=='jules'){
		Oi=210 # oxygen intercellular concentration mmol/mol
		Oi = rpmodel::co2_to_ca(Oi*1000,patm) # to Pa
		Tref = 25+273.15
		tau_ref = 2600
		Q10= 0.57
		
		photo_resp = Oi/(2*(tau_ref*Q10^((tk-Tref)/10)))
		
		
	}else if(Gstaropt=='ibis'){
		Oi=210 # oxygen intercellular concentration mmol/mol
		Oi = rpmodel::co2_to_ca(Oi*1000,patm) # to Pa
		Tref = 15-273.15
		tc=tc-273.15
		tau_ref = 4500
		Ea_tau = -5000
		
		photo_resp = Oi/(2*(tau_ref*exp(Ea_tau * ((1/Tref)-(1/tc)))))
		
		
	}
	# #Vomax<-exp(22.98−60110/(tk*Rgas))
	# Vomax<-exp(22.98−60110/(tk*Rgas))
	# #Vcmax<-exp(26.35−65330/(tk*Rgas))	
	# #Vcmax<-no_acc_f_arr(tc)
	# Vcmax=rpmodel::ftemp_inst_vcmax(tc)
	# 	
	# kc <- kc25 * ftemp_arrh(tk, dha = dhac)
	# ko <- ko25 * ftemp_arrh(tk, dha = dhao)
	# po <- kco * (1e-06) * patm
	# 
	# #
	# #ko_kc = 0.21 #at 25C Farquhar, 1980
	# ko_kc = 0.21*(Vomax/Vcmax)
	# 
	# r_ox_crbx<- ko_kc* (po/ko)/(ca/kc)
	# 
	# phi_0<-(1-f)*(1-0.5*r_ox_crbx)/(8+8*r_ox_crbx)
	
	ci=chi*ca
	mj=(ci-photo_resp)/(ci+2*photo_resp)
	
	phi_0= (1-f)*abst*(1/8)*mj
	
	
	return(phi_0)
	
	
}

wrap_optim_AI<-function(df){
	maxphio=quantile(df$maxphio,0.95,na.rm=T)
	optimize_phi0<-function(coeffs){
		m=coeffs[1];n=coeffs[2]
		phi_T_peak=(maxphio/(1+(df$AI)^m)^n)
		#-hydroGOF::NSE(as.numeric(phi_T_peak),as.numeric(df$maxphio))
		msep_sim=MSEP_comp(as.numeric(phi_T_peak),as.numeric(df$maxphio))
		msep_sim['MSEP']
		
	}
	par_optim_ksat<-nmkb(par=c(6.8,0.07),fn=optimize_phi0,lower = c(4,0.05), upper =c(10,1))
	############## confidence function
	optimize_phi0<-function(coeffs,new_maxphio){
		m=coeffs[1];n=coeffs[2]
		phi_T_peak=(maxphio/(1+(df$AI)^m)^n)
		#-hydroGOF::NSE(as.numeric(phi_T_peak),as.numeric(df$maxphio))
		msep_sim=MSEP_comp(as.numeric(phi_T_peak),as.numeric(new_maxphio))
		msep_sim['MSEP']
		
	}
	#result=data.frame(maxphio=maxphio,m =par_optim_ksat$par[1],n=par_optim_ksat$par[2],NSE=-1*par_optim_ksat$value)		
	#return(result)
	#par_optim_ksat
	
	df$fitted <- (0.11/(1+(df$AI)^par_optim_ksat$par[1])^par_optim_ksat$par[2])
	df$residuals <- df$maxphio - df$fitted
	
	library(boot)
	#set.seed(42)
	bootres <- boot(df, function(DF, i) {
		new_maxphio <- df$fitted + df[i, "residuals"]
		new_par_optim_ksat<-nmkb(par=c(6.8,0.07),fn=optimize_phi0,new_maxphio=new_maxphio,lower = c(4,0.05), upper =c(10,1))
		new_par_optim_ksat$par
		}, R = 10000)
	confide<-apply(bootres$t, 2, quantile, probs = c(0.025, 0.5, 0.975))
	
	result=data.frame(maxphio=maxphio,m =par_optim_ksat$par[1],n=par_optim_ksat$par[2],MSEP=par_optim_ksat$value,
		m_conf_2.5=confide[1,1],n_conf_2.5=confide[1,2],m_conf_97.5=confide[3,1],n_conf_97.5=confide[3,2],m_conf_50=confide[2,1],n_conf_50=confide[2,2])	
	return(result)
}


wrap_optim_dent<-function(df){
	
	optimize_dent<-function(coeffs){
		dS0=coeffs[1];dS_mgdd=coeffs[2]
		dent_sim = dS0 * df$mGDD0^(-dS_mgdd)
		#-hydroGOF::NSE(as.numeric(phi_T_peak),as.numeric(df$maxphio))
		msep_sim=MSEP_comp(as.numeric(dent_sim),as.numeric(df$dent))
		msep_sim['MSEP']
		
	}
	par_optim_ksat<-nmkb(par=c(900,0.2),fn=optimize_dent,lower = c(600,0.05), upper =c(20000,1))
	############## confidence function
	optimize_dent<-function(coeffs,new_dent){
		dS0=coeffs[1];dS_mgdd=coeffs[2]
		dent_sim = dS0 * df$mGDD0^(-dS_mgdd)
		#-hydroGOF::NSE(as.numeric(phi_T_peak),as.numeric(df$maxphio))
		msep_sim=MSEP_comp(as.numeric(dent_sim),as.numeric(new_dent))
		msep_sim['MSEP']
		
	}
	#result=data.frame(maxphio=maxphio,m =par_optim_ksat$par[1],n=par_optim_ksat$par[2],NSE=-1*par_optim_ksat$value)		
	#return(result)
	#par_optim_ksat
	
	df$fitted <- par_optim_ksat$par[1] * df$mGDD0^(-par_optim_ksat$par[2])
	df$residuals <- df$dent - df$fitted
	
	library(boot)
	#set.seed(42)
	bootres <- boot(df, function(DF, i) {
		new_dent <- df$fitted + df[i, "residuals"]
		new_par_optim_ksat<-nmkb(par=c(900,0.2),fn=optimize_dent,new_dent=new_dent,lower = c(600,0.05), upper =c(20000,1))
		new_par_optim_ksat$par
		}, R = 10000)
	confide<-apply(bootres$t, 2, quantile, probs = c(0.025, 0.5, 0.975))
	
	result=data.frame(dS0 =par_optim_ksat$par[1],dS_mgdd=par_optim_ksat$par[2],MSEP=par_optim_ksat$value,
		dS0_conf_2.5=confide[1,1],dS_mgdd_conf_2.5=confide[1,2],dS0_conf_97.5=confide[3,1],dS_mgdd_conf_97.5=confide[3,2],dS0_conf_50=confide[2,1],dS_mgdd_conf_50=confide[2,2])	
	return(result)
}







#calc_phi0 new
calc_phi0_new<-function(tc,mGDD0=NA,AI){
	# ************************************************************************
	# Name:     calc_phi0
	# Inputs:   - double - scalar (AI), climatological aridity index, defined as PET/P
	#           - double - vector (tc), air temperature, degrees C
	#           - double - scalar (mGDD0), mean temperature during growing degree days with tc>0
	# Returns:  double, intrinsic quantum yield at temperature tc, mol CO2/mol photon
	# Features: This function calculates the temperature and aridity dependence of the
	#           Intrinsic quantum Yield
	# * Ref:    Sandoval, Flo, Morfopoulus and Prentice
	#		    The temperature effect on the intrinsic quantum yield at the ecosystem level
	#             in prep.;
	#             doi:
	# ************************************************************************
	###############################################################################################
	# 01.define the parameters/constants
	###############################################################################################
	phi_o_theo <- 0.1179906      	# theoretical maximum phi0 (Long, 1993;Sandoval et al., in.prep.)
	m <- 9.533923              		# curvature parameter phio max (Sandoval et al., in.prep.) IN SITU FAPAR!!!!
	n <- 0.07455258           	# curvature parameter phio max (Sandoval et al., in.prep.)IN SITU FAPAR!!!!
	#m <- 0.4               	# curvature parameter phio max (Sandoval et al., in.prep.) OPTIMIZED FLUX DATA KIT !!!!
	# n <- 1.01           		# curvature parameter phio max (Sandoval et al., in.prep.)OPTIMIZED FLUX DATA KIT !!!!
	Rgas <- 8.3145            	# ideal gas constant J/mol/K
	dS0 = 3334.209				# max entropy change(Sandoval et al., in.prep.)
	dS_mgdd = 0.6402007 			# rate entropy change with temperature phio max (Sandoval et al., in.prep.)
	Ha <- 70991.96	      		# activation energy J/mol (Sandoval et al., in.prep.)
	#Ha <- 62000     		# activation energy J/mol (Sandoval et al., in.prep.)
	#if mGDD0 is missing, calculate
	if(is.na(mGDD0)){
		mGDD0<-mean(tc[tc>0],na.rm=T)
	}
	##calc activation entropy, J/mol/K (Sandoval et al., in.prep.)
	#DeltaS = 1558.853-50.223*mGDD0
	#DeltaS = dS0-dS_mgdd*mGDD0
	##power law from flux data kit
	DeltaS = dS0*mGDD0^(-dS_mgdd)
	##calc deaactivation energy J/mol (Sandoval et al., in.prep.)
	Hd<- 295 *DeltaS
	
	###############################################################################################
	# 02.define the functions
	###############################################################################################
	
	no_acc_f_arr<-function (tcleaf,Ha =71513,Hd= 2e+05,dent=649) {
		
		###10.1111/nph.16883
		Rgas <- 8.3145 #J/mol/K
		##fix for optimization
		if(!is.na(Ha)& !is.na(Hd)& Ha>Hd){
			Ha<-Hd-1
		}
		Top<-Hd/(dent-Rgas*log(Ha/(Hd-Ha)))
		tkleaf <- tcleaf + 273.15
		###################change to Medlyn et al. (2002)
		f1= exp((Ha*(tkleaf-Top))/(Top*Rgas*tkleaf)) 
		f2=1+exp((Top*dent-Hd)/(Top*Rgas))
		f3=1+exp((tkleaf*dent-Hd)/(tkleaf*Rgas))
		
		farr<- f1*(f2/f3)
		
		return(farr)
	}
	###############################################################################################
	# 03.calculate maximum phi0
	###############################################################################################
	phi_o_peak = (phi_o_theo/(1 + (AI)^m)^n)
	###############################################################################################
	# 04.calculate temperature dependence of phi0
	###############################################################################################
	phi0_fT<-no_acc_f_arr(tcleaf = tc,Ha=Ha,Hd=Hd,dent=DeltaS)
	###############################################################################################
	# 05.calculate phi0
	###############################################################################################
	phi0 = phi_o_peak*phi0_fT
	return(phi0)
	
}

################################################################################################
