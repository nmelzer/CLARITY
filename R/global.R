## global settings


BreedOverview<-approach.table<-approaches<-breed.table<- breeds<-geneticMap_summary<-no.chr<-NULL

###################################################################################################################################
## for sidebar inputs
no.chr=c("All",paste(1:29))
breeds=c("", "Holstein-CH","Holstein-DE","Fleckvieh","BrownSwiss","Braunvieh","Angus","Simmental","Limousin")
approaches=c("Deterministic_male","Likelihood_male","HMM_male","HMM_female","HMM_average")


###################################################################################################################################
#### setting colors and shapes for breeds
breed.table=as.data.frame(cbind(Name=c("Holstein-CH","Holstein-DE","Fleckvieh","BrownSwiss","Braunvieh","Angus","Simmental","Limousin"),
                                Abbreviation=c("HC","HD","F","BS","B","A","S","L"),
                                Color=c("#E27a53","#336666","#CDCD00","#414487FF","#6695CC","#C8A2C8","#8B5A00","#5D4760"),
                                Color2=c("#AAAAAA","#7D7D7D","#C3C3C3","#7F7D9C","#787275","#ADADC9","#808080","#C5C6D0")))


###################################################################################################################################
## setting colors and shape for approaches
approach.table=as.data.frame(cbind(
  "Approach"=c("Deterministic_male","Likelihood_male","HMM_male","HMM_female","HMM_average"),
  "Name"=c("Deterministic approach (male)","Likelihood-based approach (male)","HMM-based approach (male)","HMM-based approach (female)","HMM-based approach (average)" ),
  "Abbreviation"=c("Dm","Lm","Hm","Hf","Ha"),# used in files
  "Color"=c("dodgerblue2","cadetblue3","#E69F00","#CC79A7","#CD4F39"), ## approach color for hotspot
  "Color2"=c("#AAAAAA",NA,"#7D7D7D","#C3C3C3","#BABABA"),#, ##  approach color for no hotspot
  "Shape1"=c(15,100,17,18,19)  ##
))


###################################################################################################################################
#### load and prepare data which are used for the likelihood quality signal
OverviewBreed<-NULL

load(system.file("extdata","general/OverviewBreeds.Rdata",package="CLARITY"))
BreedOverview=cbind(OverviewBreed, round(OverviewBreed$N2/100)*100,ceiling((OverviewBreed$n2/OverviewBreed$N2)/100)*100)
colnames(BreedOverview)[7:8]=c("N2.new","Averag.n2")

###################################################################################################################################
#### load all genetic map summaries
genetic_map_summary<-NULL

geneticMap_summary = mapply(function(x) {
  load(system.file("extdata", paste0(x, "/genetic_map_summary.Rdata"), package = "CLARITY"))
  map.sum=as.data.frame(genetic_map_summary)
  colnames(map.sum)[c(8,10,13,16,19)]=c("Dm_cM/Mb","Lm_cM/Mb", "Hm_cM/Mb","Hf_cM/Mb","Ha_cM/Mb")
  map.sum
}, breed.table$Name, SIMPLIFY = FALSE)
