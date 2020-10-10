library(sf)
library(httr)
library(tidyverse)
library(ows4R)
#from
#https://inbo.github.io/tutorials/tutorials/spatial_wfs_services/

#test emodnet human act
wfs_ha<-"https://ows.emodnet-humanactivities.eu/wfs"#/wfs?SERVICE=WFS&VERSION=1.1.0&request=GetFeature&typeName=",name,"&OUTPUTFORMAT=csv&WGS84BoundingBox=",bbox)
#get capabilities to have the list of data
bwk_client <- WFSClient$new(wfs_ha,
			    serviceVersion = "1.1.0")
bwk_client
#get capabilities
bwk_client$getCapabilities()
#get features
featlist<-bwk_client$getFeatureTypes(pretty=TRUE)
#truc intéressant
#emodnet:aggregateareas

##########################################################
#emodet:aggregateareas
#a test to get the data
#url <- parse_url(wfs_ha)
#url$query <- list(service = "WFS",
#		  version = "1.1.0",
#		  request = "GetFeature", 
#		  typename = 'emodnet:aggregateareas', 
#		  bbox= "49.2,-1.5,49.8,0", 
#		  outputFormat = "application/json")
#request <- build_url(url)
#aggareas<-read_sf(request)
#a control plot
#ggplot(aggareas)+geom_sf(aes(fill=code))+borders("world") +coord_sf(ylim=c(49.2,49.8),xlim=c(-1.5,0.5))
#save the data
#save(aggareas,file="aggareas.rdata")

#a fct to get some data
gethumact<-function(nom="emodnet:aggregateareas",request="GetFeature"){
	url<-parse_url("https://ows.emodnet-humanactivities.eu/wfs")
	#nom="emodnet:aggregateareas"
	url$query <- list(service = "WFS",
			  version = "1.1.0",
			  request = "", 
			  typename="",
			  bbox= "49.2,-1.5,49.8,0", 
			  outputFormat = "application/json")
	url$query$typename<-nom
	url$query$request<-request
	#url$query<-list(typename =nom)
	request <- build_url(url)
	uu<-read_sf(request)
	return(uu)
}

gethumact<-function(nom="emodnet:aggregateareas",request="GetFeature"){
	url<-parse_url("https://ows.emodnet-humanactivities.eu/wfs")
  url<-parse_url("https://ows.emodnet-seabedhabitats.eu/emodnet_view/wfs")
	#nom="emodnet:aggregateareas"
	url$query <- list(service = "WFS",
			  version = "1.1.0",
			  request = "", 
			  typename="",
			  bbox= "49.2,-1.5,49.8,0", 
			  outputFormat = "application/json")
	url$query$typename<-nom
	url$query$request<-request
	#url$query<-list(typename =nom)
	request <- build_url(url)
	uu<-read_sf(request)
	return(uu)
}


#get the dataaaaa
#nat2000
nat2000<-gethumact("natura2000areas")
plot(nat2000)
saveRDS(nat2000,file="nat2000.rds")
#aggregates areas
aggareas<-gethumact('aggregateareas') 
plot(aggareas)
saveRDS(aggareas,file="aggareas.rds")
munpol<-gethumact('munitionspoly') 
plot(munpol)
saveRDS(munpol,file="munpol.rds")
munpt<-gethumact('munitions') 
plot(munpt)
saveRDS(munpt,file="munpt.rds")
windfarms<-gethumact('windfarmspoly') 
plot(windfarms)
saveRDS(windfarms,file="windfarms.rds")

#a map
ggplot(windfarms)+geom_sf()+borders("world") +coord_sf(ylim=c(49.2,49.8),xlim=c(-1.5,0.5))

#fishing density : a wcs 
wcs_ha<-"https://ows.emodnet-humanactivities.eu/wcs"
#get capabilities to have the list of data
bwk_client <- WFSClient$new(wfs_ha, 
			    serviceVersion = "1.0.0")
bwk_client
#get capabilities
bwk_client$getCapabilities()
#get features
featlist<-bwk_client$getFeatureTypes(pretty=TRUE)
#now a dirty function to get the data sequentially
getais<-function(nom="emodnet:2017_01_st_All",xmin=-1.5,xmax=0.5,ymin=49.2,ymax=49.8){
	url<-paste0("https://ows.emodnet-humanactivities.eu/wcs?service=wcs&version=1.0.0&request=getcoverage&coverage=",nom,"&crs=EPSG:4326&BBOX=",xmin,",",ymin,",",xmax,",",ymax,"&format=image/tiff&interpolation=nearest&resx=0.00833333&resy=0.00833333")
	nomfich<-tempfile(nom)
  	utils::download.file(url,nomfich,quiet=TRUE,mode="wb")
	#return the corresponding raster"
	img<-raster::raster(nomfich)
	names(img)<-nom
	print(nom)
	return(img)
}

#now loop on the param name: name syntax year_month_st_typevessel
#12 type of vessels from 00 to 12
#time from 2017-01 to 2019-10
#generate name
allnom<-expand.grid(vesstype=sprintf("%02d",0:12),month=sprintf("%02d",1:12),year=2017:2019)
allnom<-paste0("emodnet:",paste(alltime$year,alltime$month,"st",alltime$vesstype,sep="_"))
allnom<-allnom[!grepl("2019_11",allnom)]
allnom<-allnom[!grepl("2019_12",allnom)]
#linouuuux
allais<-parallel::mclapply(allnom,getais,mc.cores=parallel::detectCores())
#allais<-lapply(allnom,getais)
allais<-stack(allais)
writeRaster(allais,filename="allais",format="raster",overwrite=T)


#a test
fishing<-allais[[which(grepl("st_01",names(allais)))]]
levelplot(stack(pipo)+1,zscaleLog=T)
