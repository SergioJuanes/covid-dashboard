library(tidyverse)
library(textclean)
library(pdftools)
library(miscTools)
library(parsedate)

#change wd to be out of the folder
setwd("..")
#read_csv(url("https://raw.githubusercontent.com/SergioJuanes/covid-dashboard/main/data/spaincovidiaedad.csv"))

#read csv from data folder
spain_covid_edad <- read_csv("./data/spaincovidiaedad.csv")

number_file <- max(spain_covid_edad$number_file)+1
pdf_file <- tryCatch(
  pdf_text(paste0("https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov/documentos/Actualizacion_",
                  number_file,"_COVID-19.pdf", 
                  collapse = "")),
  error=function(e) NULL
)
if(!is.null(pdf_file)){
  datos_unlist <- unlist(str_split(pdf_file[1], "[\\r\\n]+"))
  datos_unlist <- data.frame(str_split_fixed(str_trim(datos_unlist), "\\s{2,}", 10))
  fecha_doc <- as.Date(format(parsedate::parse_date(datos_unlist[[1]][5]), "%Y-%m-%d"))
  if (is.na(fecha_doc)) {
    fecha_doc <- as.Date(format(parsedate::parse_date(datos_unlist[[1]][6]), "%Y-%m-%d"))
  }
  if (fecha_doc == max(spain_covid_edad$fecha)) {
    #if (as.POSIXlt(fecha)$wday == 5) {
    #  fecha = fecha + 3
    #} else {
    fecha_doc = fecha_doc + 1
    #}
  }
  
  datos_unlist <- unlist(str_split(pdf_file[7], "[\\r\\n]+"))
  coms <- c("Andalucía", "Aragón", "Principado de Asturias", "Islas Baleares", "Islas Canarias", "Cantabria", "Castilla-La Mancha", "Castilla y León", "Cataluña", "Ceuta", "Comunidad Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid", "Melilla", "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco", "La Rioja", "España")
  list <- c()
  
  for (i in which(grepl(coms[1], datos_unlist, fixed = TRUE)):(which(grepl(coms[1], datos_unlist, fixed = TRUE))+21)){
    list <- c(list, datos_unlist[i])
  }
  df <- data.frame(CCAA=NA, menos11=NA, de12a19=NA, de20a29=NA, de30a39=NA, de40a49=NA, de50a59=NA, de60a69=NA, de70a79=NA, mas89=NA, fecha=NA, number_file=NA)[numeric(0), ]
  
  j <- 0
  for (i in 1:length(list)){
    limpio <- list[i]
    limpio<-gsub('\\.','',limpio)
    limpio<-gsub(',','.',limpio)
    
    limpio <- as.numeric(unlist(regmatches(limpio,
                                           gregexpr("[[:digit:]]+\\.*[[:digit:]]*",limpio))
    )      )
    
    if (length(limpio)==9){
      j <- j+1
      df_com <- data.frame(CCAA=coms[j], menos11=limpio[1], de12a19=limpio[2], de20a29=limpio[3], de30a39=limpio[4], de40a49=limpio[5], de50a59=limpio[6], de60a69=limpio[7], de70a79=limpio[8], mas89=limpio[9], fecha=fecha_doc, number_file=number_file)
      df <- rbind(df, df_com)
    }
  }
  df$CCAA <- as.character(df$CCAA)
  names(df) <- names(spain_covid_edad)
  spain_covid_edad <- rbind(spain_covid_edad, df)
  write_csv(spain_covid_edad, "./data/spaincovidiaedad.csv")
}
