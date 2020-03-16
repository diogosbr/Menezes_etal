require(raster)
require(dplyr)

registros = read.table("Eschweilera.csv", h = T, sep = ";")
head(registros)

coordinates(registros) = ~lon+lat


lista = list.files(pattern = "raw_mean_ensemble_mean.tif$", full.names = T, recursive = T)
lista

#nomes.past = strsplit(lista, "/")[c(7:15)]

mod.past = stack(lista[c(7:15)])
mod.current = stack(lista[c(16)])
mod.fut.45 = stack(lista[c(1,3,5)])
mod.fut.85 = stack(lista[c(2,4,6)])

stand <- function(x) {(x-min(mod.current[], na.rm = T))/(max(mod.current[], na.rm = T)-min(mod.current[], na.rm = T))}
mod.current <- calc(mod.current, stand)

cat(paste("Standardizing models from 0 to 1", "\n"))
for(i in 1:dim(mod.past)[3]) {
  stand <-
    function(x) {
      (x - min(mod.past[[i]][], na.rm = T)) / (max(mod.past[[i]][], na.rm = T) - min(mod.past[[i]][], na.rm = T))
    }
  bb <- calc(mod.past[[i]], stand)
  bb
  if (i == 1) {
    cc = stack(bb)
    names(cc)[i] = names(mod.past)[i]
  }
  else{
    cc = stack(cc, bb)
    names(cc)[i] = names(mod.past)[i]
  }
  if(i == dim(mod.past)[3]){
    mod.past = cc
    rm(cc)
  }
}

cat(paste("Standardizing models from 0 to 1", "\n"))
for(i in 1:dim(mod.fut.45)[3]) {
  stand <-
    function(x) {
      (x - min(mod.fut.45[[i]][], na.rm = T)) / (max(mod.fut.45[[i]][], na.rm = T) - min(mod.fut.45[[i]][], na.rm = T))
    }
  bb <- calc(mod.fut.45[[i]], stand)
  bb
  if (i == 1) {
    cc = stack(bb)
    names(cc)[i] = names(mod.fut.45)[i]
  }
  else{
    cc = stack(cc, bb)
    names(cc)[i] = names(mod.fut.45)[i]
  }
  if(i == dim(mod.fut.45)[3]){
    mod.fut.45 = cc
    rm(cc)
  }
}


cat(paste("Standardizing models from 0 to 1", "\n"))
for(i in 1:dim(mod.fut.85)[3]) {
  stand <-
    function(x) {
      (x - min(mod.fut.85[[i]][], na.rm = T)) / (max(mod.fut.85[[i]][], na.rm = T) - min(mod.fut.85[[i]][], na.rm = T))
    }
  bb <- calc(mod.fut.85[[i]], stand)
  bb
  if (i == 1) {
    cc = stack(bb)
    names(cc)[i] = names(mod.fut.85)[i]
  }
  else{
    cc = stack(cc, bb)
    names(cc)[i] = names(mod.fut.85)[i]
  }
  if(i == dim(mod.fut.85)[3]){
    mod.fut.85 = cc
    rm(cc)
  }
}

par(mfrow = c(3,3))
plot(mod.past)

par(mfrow = c(3,3))
plot(mod.fut.45)
plot(mod.fut.85)


mod.past.mean = mean(mod.past)
mod.current.mean = mean(mod.current)
mod.fut.45.mean = mean(mod.fut.45)
mod.fut.85.mean = mean(mod.fut.85)

par(mfrow = c(2,2))
plot(mod.past.mean, main = 'past')
plot(mod.current.mean, main = 'current')
plot(mod.fut.45.mean, main = 'fut45')
plot(mod.fut.85.mean, main = 'fut85')

#refugios
refugios = mod.past.mean - mod.current.mean
plot(refugios)
points(registros, pch = 16, cex=0.5)

plot(refugios>0.4)
points(registros, pch = 16, cex=0.5)

area.ref = refugios>0.4

#areas prioritarias rcp4.5
a.prio = mod.fut.45.mean - mod.current.mean
plot(a.prio)
points(registros, pch = 16, cex=0.5)

plot(a.prio>0.4)
points(registros, pch = 16, cex=0.5)

#areas prioritarias rcp8.5
a.prio2 = mod.fut.85.mean - mod.current.mean
plot(a.prio2)
points(registros, pch = 16, cex=0.5)

plot(a.prio2>0.4)
points(registros, pch = 16, cex=0.5)


area.ref = refugios>0.4
area.prio = a.prio>0.4
area.prio2 = a.prio2>0.4

corDegrade <- colorRampPalette(c("red","yellow"))
corDegrade <- colorRampPalette(c(1:10))
barplot(1:10, col=corDegrade(10))
corDegrade(10)

paleta = c("transparent", "#FFFF00","#00CD00")
plot(sum(area.ref,area.prio,area.prio2), col = c("red", "black", "green"))
plot(sum(area.ref,area.prio,area.prio2), col = paleta)

#áreas prio4.5
paleta = c("transparent", "#00CD00")
plot(sum(area.ref,area.prio), col = c("red", "darkgreen"))
plot(sum(area.ref,area.prio), col = paleta)
points(registros, pch = 16, cex=0.5)

#áreas prio8.5
paleta = c("transparent", "#FFFF00")
plot(sum(area.ref,area.prio2), col = c("red", "darkgreen"))
plot(sum(area.ref,area.prio2), col = paleta)


writeRaster(mod.past.mean, filename = "past_mean.tif")
writeRaster(mod.current.mean, filename = "current_mean.tif")
writeRaster(mod.fut.45.mean, filename = "fut.45_mean.tif")
writeRaster(mod.fut.85.mean, filename = "fut.85_mean.tif")

writeRaster(refugios, filename = "regugios_cont.tif")
writeRaster(refugios>0.4, filename = "refugios_bin_04.tif")
writeRaster(a.prio, filename = "area_prio45_cont.tif")
writeRaster(a.prio>0.4, filename = "area_prio45_bin.tif")
writeRaster(a.prio2, filename = "area_prio85_cont.tif")
writeRaster(a.prio2>0.4, filename = "area_prio85_bin.tif")

writeRaster(mod.past.mean>0.2, filename = "past_bin.tif")
writeRaster(mod.current.mean>0.2, filename = "current_bin.tif")
writeRaster(mod.fut.45.mean>0.2, filename = "fut.45_bin.tif")
writeRaster(mod.fut.85.mean>0.2, filename = "fut.85_bin.tif")

plot((mod.current.mean>0.2)+(mod.past.mean>0.2))
writeRaster((mod.current.mean>0.2)+(mod.past.mean>0.2), filename = "refugio_glacial_paper.tif")
#writeRaster(sum(mod.past.mean>0.2,mod.current.mean>0.2,mod.fut.45.mean>0.2,mod.fut.85.mean>0.2), filename = "soma_all.tif")

writeRaster((mod.current.mean>0.2)+(mod.fut.45.mean>0.2)+(mod.fut.85.mean>0.2), filename = "area_prioritaria_paper.tif")

past.bin = mod.past.mean>0.2
current.bin = mod.current.mean>0.2
fut45.bin = mod.fut.45.mean>0.2
fut85.bin = mod.fut.85.mean>0.2


plot(past.bin)
plot(current.bin)
plot(fut45.bin)
plot(fut85.bin)

plot(past.bin+
current.bin+
fut45.bin+
fut85.bin)
points(registros, pch = 16, cex=0.5)


past.bin
current.bin
fut45.bin
fut85.bin

mod.past.mean
mod.current.mean
mod.fut.45.mean
mod.fut.85.mean

#refugios paper
plot((mod.current.mean>0.2)-(mod.past.mean>0.2))
plot((mod.current.mean>0.2)-(mod.past.mean>0.2))
plot(((mod.current.mean>0.2)+(mod.past.mean>0.2))>1)


#prioritarias paper
table(values((mod.current.mean>0.2)+(mod.fut.45.mean>0.2)+(mod.fut.85.mean>0.2)))[4]

pncd = rgdal::readOGR("./GIS/PNCD.shp")

pncd.adeq = crop(mask(((mod.current.mean>0.2)+(mod.fut.45.mean>0.2)+(mod.fut.85.mean>0.2)) ,pncd),pncd)
table(values(pncd.adeq))[4]

plot(pncd.adeq)

((mod.current.mean>0.2)+(mod.fut.45.mean>0.2)+(mod.fut.85.mean>0.2))


plot(mod.current.mean)

sum(values(mod.current.mean>0.2), na.rm = T)-

sum(values(mod.past.mean>0.2), na.rm = T)


#numero de pixels da área de estudos
n.tot = length(na.exclude(values(mod.past.mean)))
n.tot

#numero de pixel de presença passado
n.past = table(values(mod.past.mean>0.2))[2]
n.past

#numero de pixel de presença atual
n.cur = table(values(mod.current.mean>0.2))[2]
n.cur

#numero de pixel de presença 45
n.fut45 = table(values(mod.fut.45.mean>0.2))[2]
n.fut45

#numero de pixel de presença 85
n.fut85 = table(values(mod.fut.85.mean>0.2))[2]
n.fut85

n.past*100/n.tot
n.cur*100/n.tot
(n.cur-n.past)*100/n.tot

(n.cur-n.fut45)*100/n.tot

(n.cur-n.fut85)*100/n.tot


#---------------------#
#Calculando EOO    ####
#---------------------#

#EOO
require(dismo)

EOO = convHull(registros)
EOO
str(EOO)
EOO@polygons

plot(EOO@polygons, axes = T, las = 1)
points(registros)

rgdal::writeOGR(EOO@polygons, "eoo", "eoo.shp", driver = "ESRI Shapefile")

(mod.current.mean>0.2) %>%
  crop(., EOO@polygons) %>%
  mask(.,EOO@polygons) -> mod.eoo
mod.eoo
plot(mod.eoo)
table(values(mod.eoo))
values(mod.eoo)[values(mod.eoo)!=1] = NA

writeRaster(mod.eoo, "modelo_EOO_bin.tif")

table(getValues(mod.eoo)==1)

writeOGR(EOO@polygons, "./GIS", "EOO", driver="ESRI Shapefile")


ma.frag = readOGR("./GIS/FragMA_Project.shp")

plot(mod.eoo)
plot(ma.frag, add=T, col = "darkgreen", ext = extent(mod.eoo))

mod.eoo %>%
  mask(.,ma.frag) -> mod.frag.eoo

plot(mod.frag.eoo)
table(values(mod.frag.eoo))
