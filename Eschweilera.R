
#carregando pacotes ####
require(raster)
library(rgdal)
require(dplyr)
library(parallel)
library(foreach)
library(doParallel)
require(ModelR)


library(dismo)

#-----------------------------#
# MODELAGEM #####
#----------------------------#


#importando os pontos ####
registros = read.table("Eschweilera.csv", h = T, sep = ";")
head(registros)
tail(registros)
table(is.na(registros))

pts = registros
coordinates(pts) = ~lon+lat

#lista de especies
especies <- unique(registros$sp)
especies

#quantos registros por especie
sort(table(registros$sp), decreasing = T)

#importando variaveis climaticas
env = stack(list.files("../env/Current", pattern = ".tif$", full.names = T))
env


ini = Sys.time()

# modelagem
ocorrencias <-
  registros[registros$sp == especie, c("lon", "lat")]
do_enm(
  species_name = especie,
  occurrences = ocorrencias,
  predictors = env,
  models_dir = "./modelos/",
  ## argumentos de setupsdmdata():
  #lon = "lon",#caso as colunas estejam nomeadas diferentes dá para botar aqui
  #lat = "lat",#idem
  buffer_type = NULL,
  seed = 512,
  clean_dupl = F,
  clean_nas = F,
  geo_filt = F,
  geo_filt_dist = NULL,
  plot_sdmdata = T,
  n_back = 1000,
  partition_type = "crossvalidation",
  cv_n = 5,
  cv_partitions = 4,
  ## argumentos de do_any()
  project_model = T,
  proj_data_folder = "../env/PROJ",
  mask = NULL,
  write_png = T,
  #argumentos de do_enm():
  bioclim = TRUE,
  maxent = TRUE,
  glm = F,
  rf = F,
  svm.k = F,
  #svm agora é svm.k do pacote kernlab
  svm.e = T,
  #svm2 agora é svm.e do pacote e1071
  domain = F,
  mahal = F,
  centroid = FALSE #NEW! mas é lento, eu não botaria
  #mindist = ...,#NEW! mas é lento, eu não botaria
)


Sys.time() - ini


mailR::send.mail(from = "diogosbr@gmail.com",
                 to = c("diogosbr@gmail.com"),
                 #cc = c("CC Recipient <cc.recipient@gmail.com>"),
                 #bcc = c("BCC Recipient <bcc.recipient@gmail.com>"),
                 subject = "Palmeiras Finished",
                 body = paste0("Seu job terminou!", "\n durou:", Sys.time()-ini ),
                 smtp = list(host.name = "aspmx.l.google.com", port = 25),
                 authenticate = FALSE,
                 send = TRUE)



(ini = Sys.time())
#Criando a tabela com os valores de desempenho do modelo.
for(especie in especies){
  lista <- list.files(paste0('./modelos/', especie, '/present/partitions'), pattern = ".txt", full.names = T)
  aval <- c()
  for (i in 1:(length(lista) - 2)) {
    a <- read.table(lista[i])
    aval <- rbind(aval,a)
    row.names(aval) <- NULL
    print(head(aval,10))
    write.table(aval,
                paste0("./modelos/",'/', especie, ".csv"),
                row.names = F, sep = ";")
  }
}
Sys.time()-ini

#--------------------#
# FINAL ####
#--------------------#

#importando os pontos ####
registros = read.table("Eschweilera.csv", h = T, sep = ";")
head(registros)
tail(registros)

#lista de especies
especies <- unique(registros$sp)

especie = especies[1]
ocorrencias <-
  registros[registros$sp == especie, c("lon", "lat")]

source('~/MEGA/Modelagem/Isiara/final_model1.R', encoding = 'UTF-8', echo=TRUE)
source('~/MEGA/Modelagem/Isiara/ensemble_model1.R', encoding = 'UTF-8', echo=TRUE)


#presente e futuro?

#renomeando pastas
shell(paste('rename', "old", "new"))

nomes = dir('./modelos/tetrapetala',full.names = F)
nomes


(ini = Sys.time())

for(i in 1:16){
  setwd("./modelos/tetrapetala")
  shell(paste('rename', nomes[i], "present"))
  setwd("../..")

  #gerando os ensembles por algoritmo
  final_model1(
    species_name = especie,
    #algorithms = c("centroid"),
    algorithms = c("bioclim", "maxent", "svm.e"),
    select_par = "TSS",
    select_par_val = 0.7,
    models_dir = "./modelos",
    which_models = c("bin_consensus", "bin_mean", "cut_mean", "raw_mean"),
    #as opcoes são: "raw_mean",
    #"bin_mean", "cut_mean", "bin_mean_th", "bin_consensus",
    #"cut_mean_th", dá para botar várias
    write_png = T
  )

  #gerando o modelo final
  ensemble_model1(
    species_name = especie,
    occurrences = ocorrencias,
    models_dir = "./modelos",
    which_models = c("bin_consensus", "bin_mean", "cut_mean", "raw_mean"),
    consensus = T,
    consensus_level = 0.5,
    write_png = T,
    #veja se é isso mesmo aqui
    write_raw_map = FALSE,
    scale_models = F
  )

  setwd("./modelos/tetrapetala")
  shell(paste('rename', "present", nomes[i]))
  setwd("../..")
}

Sys.time()-ini

