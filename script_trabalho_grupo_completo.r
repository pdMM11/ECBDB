library(RCurl)
library(curl)

metabolights_studies_list=function(){
  ftp_base="ftp://ftp.ebi.ac.uk/pub/databases/metabolights/studies/public/"
  listStudies_character=RCurl::getURL(ftp_base, dirlistonly=T)
  listStudies_vector=strsplit(listStudies_character, "\n")[[1]]
  return(listStudies_vector)
}

get_files_list_per_assay=function(studyID, directory){
  ftp_base=paste("ftp://ftp.ebi.ac.uk/pub/databases/metabolights/studies/public/", studyID, "/", sep="")
  
  assays_files=list()
  
  #i_file=readLines(paste(ftp_base, "i_Investigation.txt", sep=""))
  curl::curl_download(paste(ftp_base, "i_Investigation.txt", sep=""), paste(directory, "i_Investigation.txt", sep="/"))
  i_file=readLines(paste(directory, "i_Investigation.txt", sep="/"))
  
  assays_names_line=grep("^Study Assay File Name", i_file, value=T)
  assays_names=grep("^a_", strsplit(assays_names_line, "\"")[[1]], value=T)
  #assay_files=paste(ftp_base, assays_names, sep="")
  for (i in 1:length(assays_names)) curl::curl_download(paste(ftp_base, assays_names[i], sep=""), paste(directory, assays_names[i], sep="/"), quiet=F)
  assay_files=paste(directory, assays_names, sep="/")
  
  
  for (i in 1:length(assay_files)){
    assay=read.table(assay_files[i], header=T)
    if("Free.Induction.Decay.Data.File"%in%colnames(assay)) files_column="Free.Induction.Decay.Data.File"
    else files_column="Raw.Spectral.Data.File"
    
    assay_info=assay[,c("Sample.Name", files_column)]
    colnames(assay_info)=c("Samples", "Files")
    
    assays_files[[paste("Assay", i)]]=assay_info
  }
  
  return(assays_files)
}

get_metabolights_study_samples_files=function(studyID, assay, directory){
  assays_in_study=get_files_list_per_assay(studyID, directory)
  
  samples_files=assays_in_study[[assay]]
  colnames(samples_files)=NULL
  write.csv(samples_files, paste(directory, "samples_files.csv", sep="/"), row.names=F)
}

get_metabolights_study_files_assay=function(studyID, assay, directory){
  
  files_per_assay=get_files_list_per_assay(studyID, directory)
  files_to_download=as.character(files_per_assay[[assay]][,"Files"])
  
  ftp_base=paste("ftp://ftp.ebi.ac.uk/pub/databases/metabolights/studies/public/", studyID, "/", sep="")
  files_to_download_paths=paste(ftp_base, files_to_download, sep="")
  
  files_dest=paste(directory, assay, "data", files_to_download, sep="/")
  if (!dir.exists(paste(directory, assay, sep="/"))){
    dir.create(paste(directory, assay, sep="/"))
    dir.create(paste(directory, assay, "data", sep="/"))
  } 
  else if (!dir.exists(paste(directory, assay, "data", sep="/"))){
    dir.create(paste(directory, assay, "data", sep="/"))
  }
  
  for (i in 1:length(files_to_download)) curl::curl_download(files_to_download_paths[i], files_dest[i], quiet=F)
}


get_metabolights_study_metadata_assay=function(studyID, assay, directory){
  
  #Get factor names:
  ftp_base=paste("ftp://ftp.ebi.ac.uk/pub/databases/metabolights/studies/public/", studyID, "/", sep="")
  
  #i_file=readLines(paste(ftp_base, "i_Investigation.txt", sep=""))
  curl::curl_download(paste(ftp_base, "i_Investigation.txt", sep=""), paste(directory, "i_Investigation.txt", sep="/"))
  i_file=readLines(paste(directory, "i_Investigation.txt", sep="/"))
  
  factors_names=strsplit(grep("^Study Factor Name", i_file, value=T), "\"")[[1]]
  factors_names_vec=grep("\t", factors_names, value=T, invert=T)
  
  factors=c()
  for (factor in factors_names_vec){
    factors=c(factors, paste("Factor.Value.", paste(strsplit(factor, " ")[[1]], collapse="."), ".", sep=""))
  }
  
  #Get samples in the assay:
  files_per_assay=get_files_list_per_assay(studyID, directory)
  samples_in_assay=as.character(files_per_assay[[assay]][,1])
  
  #Get file with the metadata information:
  sample_file_o=strsplit(grep("^Study File Name", i_file, value=T), "\"")[[1]][2]
  sample_file=paste(ftp_base, sample_file_o, sep="")
  #sample_metadata=read.table(sample_file, header=T)
  curl::curl_download(sample_file, paste(directory, sample_file_o, sep="/"))
  sample_metadata=read.table(paste(directory, sample_file_o, sep="/"), header=T)
  
  
  metadata=as.data.frame(sample_metadata[,factors])
  rownames(metadata)=as.character(sample_metadata$Sample.Name)
  metadata=as.data.frame(metadata[samples_in_assay,])
  colnames(metadata)=factors_names_vec
  rownames(metadata)=as.character(samples_in_assay)
  
  metadata_filepath=paste(directory, "/metadata", assay, ".csv", sep="")
  write.csv(metadata, metadata_filepath)
}

get_metabolights_study=function(studyID, directory){
  cat("Getting assays...\n")
  assays_in_study=get_files_list_per_assay(studyID, directory)
  cat("Done.\n")
  
  for (assay in 1:length(assays_in_study)){
    cat("Assay ", assay)
    cat("\nGetting files from assay ", assay, "...")
    get_metabolights_study_files_assay(studyID, assay, directory)
    cat("\nDone.")
    cat("\nGetting metadata from assay ", assay, "...")
    get_metabolights_study_metadata_assay(studyID, assay, directory)
    cat("\nDone")
    cat("\nGetting samples_files file from assay ", assay, "...")
    samples_files=assays_in_study[[assay]]
    colnames(samples_files)=NULL
    write.csv(samples_files, paste(directory, "samples_files.csv", sep="/"), row.names=F)
    cat("\nDone")
  }
}


#####

get_metabolights_study_files_assay("MTBLS654", 1, "C:/Users/Pedro/OneDrive/Documentos/UMinho/Extracao Conhecimento Bases de dados Biologicas/Trabalho") #Ficheiros de dados
get_metabolights_study_metadata_assay("MTBLS654", 1, "C:/Users/Pedro/OneDrive/Documentos/UMinho/Extracao Conhecimento Bases de dados Biologicas/Trabalho") #Ficheiro de metadados
get_metabolights_study_samples_files("MTBLS654", 1, "C:/Users/Pedro/OneDrive/Documentos/UMinho/Extracao Conhecimento Bases de dados Biologicas/Trabalho") #Ficheiro samples_files, que faz a correspondencia

get_files_list_per_assay("MTBLS654","C:/Users/Pedro/OneDrive/Documentos/UMinho/Extracao Conhecimento Bases de dados Biologicas/Trabalho")

get_metabolights_study("MTBLS654", "C:/Users/Pedro/OneDrive/Documentos/UMinho/Extracao Conhecimento Bases de dados Biologicas/Trabalho") #Substituir o segundo argumento pela directoria onde querem guardar o estudo

library(specmine)

kimchi_data = read_varian_spectra_raw('E:/UMinho/2semestre/ECBDB/trabalhogrupo',
                                      metadata_file="C:/Users/Pedro/OneDrive/Documentos/UMinho/Extracao Conhecimento Bases de dados Biologicas/Trabalho/metadata1.csv",
                                      m.header_col=T, m.header_row=T, m.sep=",",
                                      samples.names="C:/Users/Pedro/OneDrive/Documentos/UMinho/Extracao Conhecimento Bases de dados Biologicas/Trabalho/samples_files.csv",
                                      zero_filling=T, apodization=T, zipped=T,
                                      description="", label.x="ppm", label.values="intensity")
#Estes comandos supostamente para importar os dados de espetros d??o erro, provavelmente
#porque faltam os zips de dados das amostras de tempo 40 dias
#Deste modo, os dados tem que ser obtidos diretamente do metabolights, n??o podendo
#ser usado download automatico. Ficheiro m_jeotgal_metabolite_profiling_NMR_spectroscopy_v2_maf.tsv


#####
#Carregamento de dados, metadados...

kimchi_meta=read.csv("s_jeotgal.txt", sep='\t') #Metadados. Descrevem as amostras
kimchi_instrumentation=read.csv("a_jeotgal_metabolite_profiling_NMR_spectroscopy.txt", sep='\t', stringsAsFactors = F)
#Dados da instrument????o do NMR
kimchi_data=read.csv("m_jeotgal_metabolite_profiling_NMR_spectroscopy_v2_maf.tsv", header = TRUE, sep = "\t")
#Dados contendo concentra????es (?) de metabolitos para cada amostra. Neste caso j?? est??o
#incluidas todas as amostras de 40 dias

dim(kimchi_data)
dim(kimchi_instrumentation)
dim(kimchi_meta)

#Verificar as classes das colunas
unlist(lapply(kimchi_data, class))
unlist(lapply(kimchi_instrumentation, class))
unlist(lapply(kimchi_meta, class))
#Todas as concentra????es s??o valores num??ricos


#####
#Tratamento de valores omissos

nas = unlist(lapply(lapply(kimchi_data, is.na), sum)) #Contar o n?? de NAs por coluna
nas = unname(nas)
colsNA = which(nas==41) #Quais as colunas sem qualquer valor
kimchi_data[colsNA]=NULL #Remover as colunas completamente vazias
#Executando outra vez, verifica-se que j?? n??o h?? colunas s?? com NAs
nas = unlist(lapply(lapply(kimchi_data, is.na), sum)); nas = unname(nas)
#No entanto h?? uma coluna com um NA, que ?? necess??rio tratar por se tratar de uma concentra????o inexistente
kimchi_data[which(nas==1)] #Coluna X0MKA, na posi????o 41 tem um NA
kimchi_data[41,'metabolite_identification'] #Falta um valor de concentra????o para o succinato, na condi????o 0MKA,
#ou seja, tempo 0 em kimchi com myeolchi-jeot. Para tratar este valor o mais apropriado ser?? substituir pelo valor
#do outro replicado, 0MKB
kimchi_data[41 ,'X0MKA'] = kimchi_data[41, 'X0MKB']


nas = unlist(lapply(lapply(kimchi_instrumentation, is.na), sum)) #Contar o n?? de NAs por coluna
nas = unname(nas)
colsNA = which(nas==80) #Quais as colunas sem qualquer valor
kimchi_instrumentation[colsNA]=NULL #Remover as colunas completamente vazias
#Executando outra vez, verifica-se que j?? n??o h?? colunas s?? com NAs. 
nas = unlist(lapply(lapply(kimchi_data, is.na), sum)); nas = unname(nas)
#Existem, no entanto colunas com N/A
NAs = grep('N/A', kimchi_instrumentation)
kimchi_instrumentation[NAs]=NULL #Remover colunas com N/A


nas = unlist(lapply(lapply(kimchi_meta, is.na), sum)) #Contar o n?? de NAs por coluna
nas = unname(nas)
colsNA = which(nas==80) #Quais as colunas sem qualquer valor
kimchi_meta[colsNA]=NULL #Remover as colunas completamente vazias
#Executando outra vez, verifica-se que j?? n??o h?? colunas s?? com NAs
nas = unlist(lapply(lapply(kimchi_data, is.na), sum)); nas = unname(nas)


#####
#Prepara????o dos ficheiros de dados e metadados para analisar

concs = grep('numeric', lapply(kimchi_data, class))
conc_metab = kimchi_data[concs]
dim(conc_metab) #41 metabolitos e 80 amostras
row.names(conc_metab) = kimchi_data$metabolite_identification #Colocar nome dos metabolitos nas linhas
conc_metab[1:10, 1:10]


#Faz sort das colunas, para n??o estarem todas misturadas, organizando as amostras por tempo e tipo
conc_metab=conc_metab[ , order(names(conc_metab))]

x1=colnames(conc_metab)
x2=gsub('.{3}$', '', x1)
x2_b=gsub('X[0-9]{1,2}', '', x1)
x3=as.numeric(gsub("X", "", x2))
x3=sort(x3)
kimchi_meta = cbind(kimchi_meta,factor(x3)) #acrescentar o n?? de dias de fermenta????o aos metadados
colnames(kimchi_meta)[length(kimchi_meta)] = "Fermentation_days"
x3= paste("X", as.character(x3), sep="")
x4=paste(x3, x2_b, sep="")
conc_metab=conc_metab[ , x4]

rownames(kimchi_meta) = paste("X", kimchi_meta$Sample.Name, sep="")

#Vale a pena escrever todos os metadados?
write.csv(conc_metab,"concentracao_metabolitos.csv")
write.csv(kimchi_meta,"metadados.csv")


#####
#Sumaria????o dos dados
library(lattice)
library(ggplot2)

#Primeiro realiza-se a transposi????o dos dados para facilitar as opera????es a realizar
conc_trans = as.data.frame(t(conc_metab[,1:ncol(conc_metab)]))
dim(conc_trans)

sapply(conc_trans, mean)
sapply(conc_trans, sd)
sapply(conc_trans, median)


#Amostras A e B ser??o agregadas
grouped=data.frame()
for (i in seq(1,80,2)) {
  x=(conc_trans[i,]+conc_trans[i+1,])/2
  y=rownames(x)
  y=gsub('A', '', y)
  y=gsub('X', '', y)
  rownames(x)=y
  grouped = rbind(grouped, x)
} #Este loop junta os replicados A e B de cada amostra, fazendo a m??dia entre os 2

#N??o sei se ser?? necess??ria standardiza????o. Vale a pena comparar metabolitos diferentes?
boxplot(grouped, col='orange') #Como h?? alguns valores pr??ximos de zero,
#a transforma????o logaritmica n??o ser?? a mais apropriada. Realiza-se um simples scale
grouped_scale = as.data.frame(scale(grouped))

sapply(grouped_scale, mean)
sapply(grouped_scale, sd)
sapply(grouped_scale, median)

boxplot(grouped_scale, col='lightgreen')


#N??o far?? sentido falar em outliers neste momento, porque cada metabolito ?? medido para muitas amostras diferentes


#Este barplots permitem comparar concentra????es de metabolitos entre amostras, em tempos diferentes
barplot(t(rbind(grouped$fructose[1:4], grouped$fructose[5:8], grouped$fructose[9:12], grouped$fructose[13:16], grouped$fructose[17:20], grouped$fructose[21:24], grouped$fructose[25:28], grouped$fructose[29:32], grouped$fructose[33:36], grouped$fructose[37:40])),
        col=c(1:4), beside = T, names.arg = c(0,2,5,8,12,16,20,25,30,40), xlab = 'Tempo de fermenta????o em dias',
        ylab='Concentra????o em mM', legend.text = c('CK', 'MK', 'NK', 'SK'), main='Frutose')

barplot(t(rbind(grouped$glycine[1:4], grouped$glycine[5:8], grouped$glycine[9:12], grouped$glycine[13:16], grouped$glycine[17:20], grouped$glycine[21:24], grouped$glycine[25:28], grouped$glycine[29:32], grouped$glycine[33:36], grouped$glycine[37:40])),
        col=c(1:4), beside = T, names.arg = c(0,2,5,8,12,16,20,25,30,40), xlab = 'Tempo de fermenta????o em dias',
        ylab='Concentra????o em mM', legend.text = c('CK', 'MK', 'NK', 'SK'), main='Glicina')

barplot(t(rbind(grouped$acetate[1:4], grouped$acetate[5:8], grouped$acetate[9:12], grouped$acetate[13:16], grouped$acetate[17:20], grouped$acetate[21:24], grouped$acetate[25:28], grouped$acetate[29:32], grouped$acetate[33:36], grouped$acetate[37:40])),
        col=c(1:4), beside = T, names.arg = c(0,2,5,8,12,16,20,25,30,40), xlab = 'Tempo de fermenta????o em dias',
        ylab='Concentra????o em mM', legend.text = c('CK', 'MK', 'NK', 'SK'), main='Acetato')
#O acetato apresenta concentra????es diferentes das apresentadas no artigo deste estudo.

boxplot(grouped$acetate[1:4], grouped$acetate[5:8], grouped$acetate[9:12], grouped$acetate[13:16], grouped$acetate[17:20], grouped$acetate[21:24], grouped$acetate[25:28], grouped$acetate[29:32], grouped$acetate[33:36], grouped$acetate[37:40],
        names = c(0,2,5,8,12,16,20,25,30,40), main='Conc. de acetato nas 4 amostras, por dia', col=c(0,2,5,8,12,16,20,25,30,40))
boxplot(grouped$acetate[seq(1,40,4)], grouped$acetate[seq(2,40,4)], grouped$acetate[seq(3,40,4)], grouped$acetate[seq(4,40,4)],
        col = c(1:4), names = c('CK', 'MK', 'NK', 'SK'), main='Conc. de acetato por amostra ao longo dos 40 dias')
#Os valores relativos ao acetato s??o anormais. Nota-se que a partir dos 12 dias a concentra????o desce drasticamente
#e as concentra????es n??o s??o consistentes


#####
#Estat??stica univariada e tudo isso

library(specmine)

MTBLS654=read_dataset_csv(filename.data="concentracao_metabolitos.csv", filename.meta="metadados.csv",
                          type="concentrations", description = "Concentrations data taken from MetaboLights study MTBLS654",
                          label.x = "Names", label.values = "Concentrations", format="col")

MTBLS654_CK_NK=subset_samples_by_metadata_values(MTBLS654,"Factor.Value.Kimchi.",c("kimchi without jeotgal","kimchi with salinity adjusted with salt instead of jeotgal"))
MTBLS654_CK_NK_ttest=tTests_dataset(MTBLS654_CK_NK, "Factor.Value.Kimchi.")
rownames(MTBLS654_CK_NK_ttest[which(MTBLS654_CK_NK_ttest$p.value<0.05), ])

MTBLS654_MK_SK=subset_samples_by_metadata_values(MTBLS654,"Factor.Value.Kimchi.",c("kimchi with myeolchi-jeot","kimchi with saeu-jeot"))
MTBLS654_MK_SK_ttest=tTests_dataset(MTBLS654_MK_SK, "Factor.Value.Kimchi.")
rownames(MTBLS654_MK_SK_ttest[which(MTBLS654_MK_SK_ttest$p.value<0.05), ])
#como n?o ha diferencas estatisticamente significativas entre CK e NK, todas as comparacoes serao apenas entre CK e as c/ jeotgal

MTBLS654_CK_SK=subset_samples_by_metadata_values(MTBLS654,"Factor.Value.Kimchi.",c("kimchi without jeotgal","kimchi with saeu-jeot"))
MTBLS654_CK_SK_ttest=tTests_dataset(MTBLS654_CK_SK, "Factor.Value.Kimchi.")
rownames(MTBLS654_CK_SK_ttest[which(MTBLS654_CK_SK_ttest$p.value<0.05), ])

MTBLS654_CK_MK=subset_samples_by_metadata_values(MTBLS654,"Factor.Value.Kimchi.",c("kimchi without jeotgal","kimchi with myeolchi-jeot"))
MTBLS654_CK_MK_ttest=tTests_dataset(MTBLS654_CK_MK, "Factor.Value.Kimchi.")
rownames(MTBLS654_CK_MK_ttest[which(MTBLS654_CK_MK_ttest$p.value<0.05), ])

MTBLS654_0_40=subset_samples_by_metadata_values(MTBLS654,"Fermentation_days",c("0day","2day"))
MTBLS654_0_40_ttest=tTests_dataset(MTBLS654_0_40, "Fermentation_days")
#diferencas entre medias de glucose nao sao estatisticamente significativas; mas frutose e estatisticamente  significativas


aov0_40=aov_all_vars(MTBLS654_0_40, "Fermentation_days")


rownames(aov0_40[aov0_40$pvalues<0.05,])


rownames(aov0_40[aov0_40$pvalues<0.05,]) %in% aa_list
sum(rownames(aov0_40[aov0_40$pvalues<0.05,]) %in% aa_list)/length(aa_list)
#apenas 30% dos amino?cidos tem diferencas estatisticamente significativas

mul_anova=multifactor_aov_all_vars(MTBLS654, c("Factor.Value.Kimchi.","Fermentation_days"), "Factor.Value.Kimchi.*Fermentation_days")

res_multi_anova=multifactor_aov_pvalues_table(mul_anova)

res_multi_anova_var=multifactor_aov_varexp_table(mul_anova)


#trimethylamine N-oxide(TMAO), trimethylamine (TMA),dimethylamine (DMA) diminui rapidamente
MTBLS654_0_20=subset_samples_by_metadata_values(MTBLS654,"Fermentation_days",c("0day","20day"))
MTBLS654_0_20_ttest=tTests_dataset(MTBLS654_0_20, "Fermentation_days")
#todos metabolitos apresentam diferencas estatisticas estatisticamente significativas 

MTBLS654_30_40=subset_samples_by_metadata_values(MTBLS654,"Fermentation_days",c("30day","40day"))
MTBLS654_30_40_ttest=tTests_dataset(MTBLS654_30_40, "Fermentation_days")
#so o trimethylamine N-oxide(TMAO) e que nao apresenta diferencas estatisticamente significativas

#####

aa_list=c('alanine',"arginine","aspartate(1-)","glutamate(2-)",
          "glutamine","glycine","isoleucine","leucine","L-lysine",
          "methionine","phenylalanine","proline","serine","threonine",
          "tryptophan","tyrosine","valine")
ttest=c()
for (i in 1:length(aa_list)){
  
aa_0=as.numeric(conc_metab[aa_list[i],grep("X0",colnames(conc_metab))])
aa_40=as.numeric(conc_metab[aa_list[i],grep("X40",colnames(conc_metab))])
if (shapiro.test(aa_0)$p.value>0.05 && shapiro.test(aa_40)$p.value>0.05){
  ttest=c(ttest,t.test(aa_40,aa_0,"less")$p.value)
}
else{
  ttest=c(ttest,wilcox.test(aa_40,aa_0,"less")$p.value) 
}
}
sum(ttest>0.05)
aa_list[ttest>0.05] #diferen??as estatisticamente n??o significativas (10)
aa_list[ttest<0.05] #diferen??as estatisticamente significativas (7)
#afirma????o estar?? correta se apenas falar em termos de amostras, mas pelo t.test n??o e verdade

sum(wtest>0.05)
aa_list[wtest>0.05] #diferen??as estatisticamente n??o significativas (10)
aa_list[wtest<0.05] #diferen??as estatisticamente significativas (7)
#afirma????o estar?? correta se apenas falar em termos de amostras, mas pelo t.test n??o e verdade


###################
####teste para verificar diferencas de propor????es entre as fracoes de conc finais / inicias de frutose e glucose
#se pvalue < que 0.05, entao teste estatistiamente significativo, e frutose menor que glucose
frut_0=conc_metab['fructose',grep("X0",colnames(conc_metab))]
frut_40=conc_metab['fructose',grep("X40",colnames(conc_metab))]
prop_frut=as.numeric(frut_40)/as.numeric(frut_0)
prop_frut_mean=sum(prop_frut)/length(prop_frut)


glu_0=conc_metab['glucose',grep("X0",colnames(conc_metab))]
glu_40=conc_metab['glucose',grep("X40",colnames(conc_metab))]
prop_glu=as.numeric(glu_40)/as.numeric(glu_0)
prop_glu_mean=sum(prop_glu)/length(prop_glu)

prop.test(prop_frut_mean,length(frut_0),p=prop_glu_mean,"less")
#pvalue aproximadamente 0.0001 < 0.05: frutose e consumida mais rapidamente que glucose


#################
#diminuicao de frutose semelhante entre todas amostras

frut_0=as.numeric(conc_metab['fructose',grep("X0",colnames(conc_metab))])
frut_40=as.numeric(cbind(conc_metab['fructose',grep("X40",colnames(conc_metab))]))
prop_frut=frut_40/frut_0
vet_props=c()
prop_mean=mean(prop_frut)
for (i in 1:length(prop_frut)){vet_props=c(vet_props,prop_mean)}
chisq.test(rbind(frut_40,frut_0),p=vet_props)
# pvalue<0.05, pelo que as proporcoes nao encontram diferencas significativas: 
#afirma????o verdadeira

require(stats)

aa_list=c('alanine',"arginine","aspartate(1-)","glutamate(2-)",
          "glutamine","glycine","isoleucine","leucine","L-lysine",
          "methionine","phenylalanine","proline","serine","threonine",
          "tryptophan","tyrosine","valine")
ttest=c()
for (i in 1:length(aa_list)){
  
  aa_0=as.numeric(conc_metab[aa_list[i],grep("X0",colnames(conc_metab))])
  aa_40=as.numeric(conc_metab[aa_list[i],grep("X40",colnames(conc_metab))])
  if (shapiro.test(aa_0)$p.value>0.05 && shapiro.test(aa_40)$p.value>0.05){
    ttest=c(ttest,t.test(aa_40,aa_0,"less")$p.value)
  }
  else{
    ttest=c(ttest,wilcox.test(aa_40,aa_0,"less")$p.value) 
  }
}
sum(ttest>0.05)
aa_list[ttest>0.05] #diferen?as estatisticamente n?o significativas (10)
aa_list[ttest<0.05] #diferen?as estatisticamente significativas (7)
#afirma??o estar? correta se apenas falar em termos de amostras, mas pelo t.test n?o e verdade

sum(wtest>0.05)
aa_list[wtest>0.05] #diferen?as estatisticamente n?o significativas (10)
aa_list[wtest<0.05] #diferen?as estatisticamente significativas (7)
#afirma??o estar? correta se apenas falar em termos de amostras, mas pelo t.test n?o e verdade


###################
####teste para verificar diferencas de propor??es entre as fracoes de conc finais / inicias de frutose e glucose
#se pvalue < que 0.05, entao teste estatistiamente significativo, e frutose menor que glucose
frut_0=conc_metab['fructose',grep("X0",colnames(conc_metab))]
frut_40=conc_metab['fructose',grep("X40",colnames(conc_metab))]
prop_frut=as.numeric(frut_40)/as.numeric(frut_0)
prop_frut_mean=sum(prop_frut)/length(prop_frut)


glu_0=conc_metab['glucose',grep("X0",colnames(conc_metab))]
glu_40=conc_metab['glucose',grep("X40",colnames(conc_metab))]
prop_glu=as.numeric(glu_40)/as.numeric(glu_0)
prop_glu_mean=sum(prop_glu)/length(prop_glu)

prop.test(prop_frut_mean,length(frut_0),p=prop_glu_mean,"less")
#pvalue aproximadamente 0.0001 < 0.05: frutose e consumida mais rapidamente que glucose


#################
#diminuicao de frutose semelhante entre todas amostras

frut_0=as.numeric(conc_metab['fructose',grep("X0",colnames(conc_metab))])
frut_40=as.numeric(cbind(conc_metab['fructose',grep("X40",colnames(conc_metab))]))
prop_frut=frut_40/frut_0
vet_props=c()
prop_mean=mean(prop_frut)
for (i in 1:length(prop_frut)){vet_props=c(vet_props,prop_mean)}
chisq.test(rbind(frut_40,frut_0),p=vet_props)
# pvalue<0.05, pelo que as proporcoes nao encontram diferencas significativas: 
#afirma??o verdadeira

##############
#acetato,lactato e mannitol maior em SK e MK durante fermentacao
#trimethylamine N-oxide(TMAO), trimethylamine (TMA),dimethylamine (DMA) diminuem rapidamente, mas depois
#"estabelizam"
metab_interesse=c("acetate","lactate","mannitol","trimethylamine N-oxide","trimethylamine","dimethylamine")

#Acetato: normalmente maior em SK e MK na maioria das amostras
barplot(t(rbind(grouped$acetate[1:4], grouped$acetate[5:8], grouped$acetate[9:12], grouped$acetate[13:16], grouped$acetate[17:20], grouped$acetate[21:24], grouped$acetate[25:28], grouped$acetate[29:32], grouped$acetate[33:36], grouped$acetate[37:40])),
        col=c(1:4), beside = T, names.arg = c(0,2,5,8,12,16,20,25,30,40), xlab = 'Tempo de fermenta??o em dias',
        ylab='Concentra??o em mM', legend.text = c('CK', 'MK', 'NK', 'SK'), main='acetate')
#lactato: normalmente maior em SK e MK na maioria das amostras
barplot(t(rbind(grouped$lactate[1:4], grouped$lactate[5:8], grouped$lactate[9:12], grouped$lactate[13:16], grouped$lactate[17:20], grouped$lactate[21:24], grouped$lactate[25:28], grouped$lactate[29:32], grouped$lactate[33:36], grouped$lactate[37:40])),
        col=c(1:4), beside = T, names.arg = c(0,2,5,8,12,16,20,25,30,40), xlab = 'Tempo de fermenta??o em dias',
        ylab='Concentra??o em mM', legend.text = c('CK', 'MK', 'NK', 'SK'), main='lactate')
#nannitol: normalmente maior em SK e MK na maioria das amostras
barplot(t(rbind(grouped$mannitol[1:4], grouped$mannitol[5:8], grouped$mannitol[9:12], grouped$mannitol[13:16], grouped$mannitol[17:20], grouped$mannitol[21:24], grouped$mannitol[25:28], grouped$mannitol[29:32], grouped$mannitol[33:36], grouped$mannitol[37:40])),
        col=c(1:4), beside = T, names.arg = c(0,2,5,8,12,16,20,25,30,40), xlab = 'Tempo de fermenta??o em dias',
        ylab='Concentra??o em mM', legend.text = c('CK', 'MK', 'NK', 'SK'), main='mannitol')

#trimethylamine N-oxide: decresce r?pido, mas concentra??es estabelizam por volta  dos 20 dias
barplot(t(rbind(grouped$"trimethylamine N-oxide"[1:4], grouped$"trimethylamine N-oxide"[5:8], grouped$"trimethylamine N-oxide"[9:12], grouped$"trimethylamine N-oxide"[13:16], grouped$"trimethylamine N-oxide"[17:20], grouped$"trimethylamine N-oxide"[21:24], grouped$"trimethylamine N-oxide"[25:28], grouped$"trimethylamine N-oxide"[29:32], grouped$"trimethylamine N-oxide"[33:36], grouped$"trimethylamine N-oxide"[37:40])),
        col=c(1:4), beside = T, names.arg = c(0,2,5,8,12,16,20,25,30,40), xlab = 'Tempo de fermenta??o em dias',
        ylab='Concentra??o em mM', legend.text = c('CK', 'MK', 'NK', 'SK'), main="trimethylamine N-oxide")
#trimethylamine N-oxide: decresce r?pido, mas concentra??es estabelizam por volta  dos 16 dias
barplot(t(rbind(grouped$trimethylamine[1:4], grouped$trimethylamine[5:8], grouped$trimethylamine[9:12], grouped$trimethylamine[13:16], grouped$trimethylamine[17:20], grouped$trimethylamine[21:24], grouped$trimethylamine[25:28], grouped$trimethylamine[29:32], grouped$trimethylamine[33:36], grouped$trimethylamine[37:40])),
        col=c(1:4), beside = T, names.arg = c(0,2,5,8,12,16,20,25,30,40), xlab = 'Tempo de fermenta??o em dias',
        ylab='Concentra??o em mM', legend.text = c('CK', 'MK', 'NK', 'SK'), main="trimethylamine")
#dimethylamine N-oxide: decresce r?pido, mas concentra??es estabelizam por volta  dos 16 dias
barplot(t(rbind(grouped$dimethylamine[1:4], grouped$dimethylamine[5:8], grouped$dimethylamine[9:12], grouped$dimethylamine[13:16], grouped$dimethylamine[17:20], grouped$dimethylamine[21:24], grouped$dimethylamine[25:28], grouped$dimethylamine[29:32], grouped$dimethylamine[33:36], grouped$dimethylamine[37:40])),
        col=c(1:4), beside = T, names.arg = c(0,2,5,8,12,16,20,25,30,40), xlab = 'Tempo de fermenta??o em dias',
        ylab='Concentra??o em mM', legend.text = c('CK', 'MK', 'NK', 'SK'), main="dimethylamine")

#####
#alanina, arginina, glutamato, isoleucina, leucina, lisina, valina e GABA mais elevados em MK e SK do que em SK e NK

#Durante os 40 dias, alanina, arginina, glutamato e lisina em SK ligeiramente maiores do que CK e NK; significativamente maiores em MK
barplot(t(rbind(grouped$alanine[1:4], grouped$alanine[5:8], grouped$alanine[9:12], grouped$alanine[13:16], grouped$alanine[17:20], grouped$alanine[21:24], grouped$alanine[25:28], grouped$alanine[29:32], grouped$alanine[33:36], grouped$alanine[37:40])),
        col=c(1:4), beside = T, names.arg = c(0,2,5,8,12,16,20,25,30,40), xlab = 'Tempo de fermenta??o em dias',
        ylab='Concentra??o em mM', legend.text = c('CK', 'MK', 'NK', 'SK'), main='Alanina')

barplot(t(rbind(grouped$arginine[1:4], grouped$arginine[5:8], grouped$arginine[9:12], grouped$arginine[13:16], grouped$arginine[17:20], grouped$arginine[21:24], grouped$arginine[25:28], grouped$arginine[29:32], grouped$arginine[33:36], grouped$arginine[37:40])),
        col=c(1:4), beside = T, names.arg = c(0,2,5,8,12,16,20,25,30,40), xlab = 'Tempo de fermenta??o em dias',
        ylab='Concentra??o em mM', legend.text = c('CK', 'MK', 'NK', 'SK'), main='Arginina')

barplot(t(rbind(grouped$`glutamate(2-)`[1:4], grouped$`glutamate(2-)`[5:8], grouped$`glutamate(2-)`[9:12], grouped$`glutamate(2-)`[13:16], grouped$`glutamate(2-)`[17:20], grouped$`glutamate(2-)`[21:24], grouped$`glutamate(2-)`[25:28], grouped$`glutamate(2-)`[29:32], grouped$`glutamate(2-)`[33:36], grouped$`glutamate(2-)`[37:40])),
        col=c(1:4), beside = T, names.arg = c(0,2,5,8,12,16,20,25,30,40), xlab = 'Tempo de fermenta??o em dias',
        ylab='Concentra??o em mM', legend.text = c('CK', 'MK', 'NK', 'SK'), main='Glutamato')

barplot(t(rbind(grouped$`L-lysine`[1:4], grouped$`L-lysine`[5:8], grouped$`L-lysine`[9:12], grouped$`L-lysine`[13:16], grouped$`L-lysine`[17:20], grouped$`L-lysine`[21:24], grouped$`L-lysine`[25:28], grouped$`L-lysine`[29:32], grouped$`L-lysine`[33:36], grouped$`L-lysine`[37:40])),
        col=c(1:4), beside = T, names.arg = c(0,2,5,8,12,16,20,25,30,40), xlab = 'Tempo de fermenta??o em dias',
        ylab='Concentra??o em mM', legend.text = c('CK', 'MK', 'NK', 'SK'), main='Lisina')

#################
#Concentra??es de isoleucina,leucina e valina em CK e Nk menores do que 2mM
barplot(t(rbind(grouped$isoleucine[1:4], grouped$isoleucine[5:8], grouped$isoleucine[9:12], grouped$isoleucine[13:16], grouped$isoleucine[17:20], grouped$isoleucine[21:24], grouped$isoleucine[25:28], grouped$isoleucine[29:32], grouped$isoleucine[33:36], grouped$isoleucine[37:40])),
        col=c(1:4), beside = T, names.arg = c(0,2,5,8,12,16,20,25,30,40), xlab = 'Tempo de fermenta??o em dias',
        ylab='Concentra??o em mM', legend.text = c('CK', 'MK', 'NK', 'SK'), main='Isoleucina')

barplot(t(rbind(grouped$leucine[1:4], grouped$leucine[5:8], grouped$leucine[9:12], grouped$leucine[13:16], grouped$leucine[17:20], grouped$leucine[21:24], grouped$leucine[25:28], grouped$leucine[29:32], grouped$leucine[33:36], grouped$leucine[37:40])),
        col=c(1:4), beside = T, names.arg = c(0,2,5,8,12,16,20,25,30,40), xlab = 'Tempo de fermenta??o em dias',
        ylab='Concentra??o em mM', legend.text = c('CK', 'MK', 'NK', 'SK'), main='Leucina')

barplot(t(rbind(grouped$valine[1:4], grouped$valine[5:8], grouped$valine[9:12], grouped$valine[13:16], grouped$valine[17:20], grouped$valine[21:24], grouped$valine[25:28], grouped$valine[29:32], grouped$valine[33:36], grouped$valine[37:40])),
        col=c(1:4), beside = T, names.arg = c(0,2,5,8,12,16,20,25,30,40), xlab = 'Tempo de fermenta??o em dias',
        ylab='Concentra??o em mM', legend.text = c('CK', 'MK', 'NK', 'SK'), main='Valina')

#concentra??es em MK e SK dobraram para esses metabolitos (em rela??o a CK e NK, no final da fermenta??o)
isoleuc_40MK=as.numeric(conc_metab['isoleucine',grep("40MK",colnames(conc_metab))])
isoleuc_40SK=as.numeric(conc_metab['isoleucine',grep("40SK",colnames(conc_metab))])
isoleuc_40CK=as.numeric(conc_metab['isoleucine',grep("40CK",colnames(conc_metab))])
isoleuc_40NK=as.numeric(conc_metab['isoleucine',grep("40NK",colnames(conc_metab))])

prop.test(mean(c(isoleuc_40CK,isoleuc_40NK)), mean(c(isoleuc_40MK,isoleuc_40SK)))

leuc_40MK=as.numeric(conc_metab['leucine',grep("40MK",colnames(conc_metab))])
leuc_40SK=as.numeric(conc_metab['leucine',grep("40SK",colnames(conc_metab))])
leuc_40CK=as.numeric(conc_metab['leucine',grep("40CK",colnames(conc_metab))])
leuc_40NK=as.numeric(conc_metab['leucine',grep("40NK",colnames(conc_metab))])

prop.test(mean(c(leuc_40CK,leuc_40NK)), mean(c(leuc_40MK,leuc_40SK)))

val_40MK=as.numeric(conc_metab['valine',grep("40MK",colnames(conc_metab))])
val_40SK=as.numeric(conc_metab['valine',grep("40SK",colnames(conc_metab))])
val_40CK=as.numeric(conc_metab['valine',grep("40CK",colnames(conc_metab))])
val_40NK=as.numeric(conc_metab['valine',grep("40NK",colnames(conc_metab))])

prop.test(mean(c(val_40CK,val_40NK)), mean(c(val_40MK,val_40SK)))

#################
#GABA aumentou e depois diminuiu nas 4 condi??es; ligeiramente maior em MK e SK ate aos 16 dias
barplot(t(rbind(grouped$`gamma-aminobutyrate`[1:4], grouped$`gamma-aminobutyrate`[5:8], grouped$`gamma-aminobutyrate`[9:12], grouped$`gamma-aminobutyrate`[13:16], grouped$`gamma-aminobutyrate`[17:20], grouped$`gamma-aminobutyrate`[21:24], grouped$`gamma-aminobutyrate`[25:28], grouped$`gamma-aminobutyrate`[29:32], grouped$`gamma-aminobutyrate`[33:36], grouped$`gamma-aminobutyrate`[37:40])),
        col=c(1:4), beside = T, names.arg = c(0,2,5,8,12,16,20,25,30,40), xlab = 'Tempo de fermenta??o em dias',
        ylab='Concentra??o em mM', legend.text = c('CK', 'MK', 'NK', 'SK'), main="GABA")


source("https://bioconductor.org/biocLite.R")
biocLite()
biocLite("KEGGREST", "KEGGgraph")
install.packages("https://github.com/cytoscape/cyjShiny/archive/v0.0.7.tar.gz", repos=NULL, method="libcurl")
library(rcytoscapejs)

pathway_analysis(colnames(grouped), 'lsa00051', nodeNames="kegg", nodeTooltip=F, map.zoom=F,
                 map.layout="preset", map.width=NULL, map.height=NULL)



mtbls654 = readRDS('mtbls654.Rdata')


mtbls654_peaks=detect_nmr_peaks_from_dataset(mtbls654)

# NMR: 0 dias de fermenta??o

plot_peaks(mtbls654_peaks, "Kimchi", samples = c("0CKA","0NKA"), cex = 0.5, legend.place = "topright")
title(main = "0 dias de fermenta??o: s/ jeotgal")

plot_peaks(mtbls654_peaks, "Kimchi", samples = c("0MKA","0SKA"), cex = 0.5, legend.place = "topright")
title(main = "0 dias de fermenta??o: c/ jeotgal")

plot_peaks(mtbls654_peaks, "Kimchi", samples = c("0CKA","0NKA","0MKA","0SKA"), cex = 0.5, legend.place = "topright")
title(main = "0 dias de fermenta??o")

# NMR: 12 dias de fermenta??o

plot_peaks(mtbls654_peaks, "Kimchi", samples = c("12CKA","12NKA"), cex = 0.5, legend.place = "topright")
title(main = "12 dias de fermenta??o: s/ jeotgal")

plot_peaks(mtbls654_peaks, "Kimchi", samples = c("12MKA","12SKA"), cex = 0.5, legend.place = "topright")
title(main = "12 dias de fermenta??o: c/ jeotgal")

plot_peaks(mtbls654_peaks, "Kimchi", samples = c("12CKA","12NKA","12MKA","12SKA"), cex = 0.5, legend.place = "topright")
title(main = "12 dias de fermenta??o")


# NMR: 20 dias de fermenta??o

plot_peaks(mtbls654_peaks, "Kimchi", samples = c("20CKA","20NKA"), cex = 0.5, legend.place = "topright")
title(main = "20 dias de fermenta??o: s/ jeotgal")

plot_peaks(mtbls654_peaks, "Kimchi", samples = c("20MKA","20SKA"), cex = 0.5, legend.place = "topright")
title(main = "20 dias de fermenta??o: c/ jeotgal")

plot_peaks(mtbls654_peaks, "Kimchi", samples = c("20CKA","20NKA","20MKA","20SKA"), cex = 0.5, legend.place = "topright")
title(main = "20 dias de fermenta??o")

# NMR: 30 dias de fermenta??o

plot_peaks(mtbls654_peaks, "Kimchi", samples = c("30CKA","30NKA"), cex = 0.5, legend.place = "topright")
title(main = "30 dias de fermenta??o: s/ jeotgal")

plot_peaks(mtbls654_peaks, "Kimchi", samples = c("30MKA","30SKA"), cex = 0.5, legend.place = "topright")
title(main = "30 dias de fermenta??o: c/ jeotgal")

plot_peaks(mtbls654_peaks, "Kimchi", samples = c("30CKA","30NKA","30MKA","30SKA"), cex = 0.5, legend.place = "topright")
title(main = "30 dias de fermenta??o")


#em fun??o ao tipo de amostra
fold_change(MTBLS654,"Factor.Value.Kimchi.",ref.value="kimchi without jeotgal")
plot_fold_change(MTBLS654, fold_change(MTBLS654,"Factor.Value.Kimchi.",ref.value="kimchi without jeotgal"), 2)
#leucina,acetato,isoleucina e valina tem log2 > 1

#em fun??o ao n? de dias de fermenta??o
fold_change(MTBLS654,"Fermentation_days",ref.value="0day")
plot_fold_change(MTBLS654, fold_change(MTBLS654,"Fermentation_days",ref.value="0day"), 2)



dados_metab=read.csv("name_map.csv.txt", sep=',') #Metadados. Descrevem as amostras
dados_metab=na.exclude(dados_metab$KEGG)
KEGG=dados_metab
pathway_analysis(KEGG, 'lsa01230', nodeNames="kegg", nodeTooltip=F, map.zoom=F, map.layout="preset", map.width=NULL, map.height=NULL)
