#-------------------------------------------------------------
#------ TWO LINEAR STORAGE MODEL WITH SOIL ROUTINE -----------
#-------------------------------------------------------------
s2ls <- function(Precip, PET, startdate="1900-01-01", h_0=100, h_min=30, a=0.1,
                 k_fast=20 , k_slow=200, ini.soil=0, ini.fast=0, ini.slow=0){

  
  
  if (length(Precip)!= length(PET)) 
    {print("error: no eqaul length of time series")}
  
  # initialize variables
  AET       <- numeric(length=length(PET))
  infil     <- numeric(length=length(Precip))
  res_soil  <- numeric(length=length(Precip)) #reservoir soil
  res_fast  <- numeric(length=length(Precip)) #reservoir of fast storage 
  res_slow  <- numeric(length=length(Precip)) #reservoir of slow depletion
  out_fast  <- numeric(length=length(Precip)) #outflow of fast depleting reservoir
  out_slow  <- numeric(length=length(Precip)) #outflow of slow depleting reservoir
  q_out     <- numeric(length=length(Precip)) # Output Variable = discharge (Q)
  
  # #-------- define varibales for function test ----------
  # Precip <- exm_dat$Rain
  # PET <- exm_dat$PET_tho
  # #------------------------------------
  # h_0 <- 100
  # #Grenzwert des Soil reservoirs eg. Feldkapazität [mm]
  # h_min <- 30
  # #Grenzwert bei dem aus dem Bodenreservoir
  # #keine Evapotranspiration mehr stattfindet
  # #dann wird evp = Null gesetzt
  # a <- 0.27
  # #Verteilungskoeffizient zwischen schnellem und langsamen Speicher
  # k_fast <- 0.06
  # #linearer Speicherabflusskoeffizinet FAST
  # k_slow <- 0.006
  # #linearer Speicherabflusskoeffizient SLOW
  # ini.soil <- 70
  # ini.fast <- 0
  # ini.slow <- 0
  # #-------------------------------------------------------

  #------- Soil Storage -----------

  res_soil[1] <- Precip[1]+ ini.soil   # initialize soil water storage
  
  AET[1] <- PET[1]*(res_soil[1]/h_0)
  
  res_soil[1] <- res_soil[1]- AET[1]
    
  if (res_soil[1] > h_0){
      infil[1] <- res_soil[1] - h_0
      res_soil[1] <- res_soil[1] -infil[1]}
  
  for (i in 2:length(res_soil)){    
      res_soil[i] <- res_soil[i-1] + Precip[i]    # first add precip to soil sotrage
      if (res_soil[i]/h_0 <=1 ){
            AET[i]  <- PET[i] *(res_soil[i]/h_0)} # clalculate AET as % form PET depending on soil_storage
      else {AET[i]  <- PET[i]}
      res_soil[i] <- res_soil[i]- AET[i]          # subtrating AET from soil storage
    
      
  if (res_soil[i] < 0){    #Wenn der Bodenspeicher unter NULL fällen würde
      res_soil[i] <- 0 }   #dann bleibt er NULL 
    
  if (res_soil[i] > h_0){                 # if soil storage exceeds h_0
      infil[i] <- res_soil[i] - h_0         # excees water inflitrates
      res_soil[i] <- res_soil[i] -infil[i]} # soil storage stays h_0
    }

  #----- schneller Speicher -------
    res_fast[1] <- a*infil[1]+ ini.fast #inititailieren des schnellen Abflussspeichers
    out_fast[1] <- (1/k_fast) *res_fast[1]
    res_fast[1] <- res_fast[1] - out_fast[1]
    
  for (i in 2:length(res_soil)){
    res_fast[i] <- res_fast[i-1] + a*infil[i]
    out_fast[i] <- (1/k_fast) * res_fast[i]
    res_fast[i] <- res_fast[i] - out_fast[i]
  }
  #---- langsamer Speicher -------
    res_slow[1] <- (1-a)*infil[1]+ ini.slow #inititailieren des langsamen Abflussspeichers
    out_slow[1] <- (1/k_slow) *res_slow[1]
    res_slow[1] <- res_slow[1] - out_slow[1]
    
  for (i in 2:length(res_soil)){
    res_slow[i] <- res_slow[i-1] + (1-a)*infil[i]
    out_slow[i] <- (1/k_slow) * res_slow[i]
    res_slow[i] <- res_slow[i] - out_slow[i]
  }
  q_out <- out_fast + out_slow

  date <- as.Date(1:length(Precip)-1, origin = startdate)

  return(data.frame(date, q_out, out_fast, out_slow, Precip,infil, PET, AET ,res_soil, res_slow, res_fast))
}
#---------------
#Beispeil
#resu <- s2ls(Precip=exm_dat$Rain, PET=exm_dat$PET_tho, startdate=exm_dat$DATE[1], a=0.1)
#summary(resu)
# hist(log(resu$Precip), breaks=150)
