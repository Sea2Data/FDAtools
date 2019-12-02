source("biotic.R")
samples <- parse_biotic("data/biotic_year_2018.xml")
commercial_sampling <- #missiontype 1,2,3,9,10,11
  
torsk <- c("torsk", "slettvar", "knurr", "hyse", "lomre", "rødspette", "sei", "piggvar", "mulle", "smørflyndre")
arter <- c("164712","172749", "167044","164744", "172888", "172902", "164727", "172748", "169418", "172873") #merk deprekert kode for piggvar 616195

