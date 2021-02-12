#' Reporting for the coupled DIETER Model 
#'
#'
#' *Warning* The function modifies the "REMIND_generic_<scenario>.mif" file by appending the
#' additional reporting variables.
#'
#' 
#' @param dieterDatafile full path with name of dieter gdx file.
#' @param output_dir path of the output folder, default is current folder.
#' @param input_dir path from which REMIND MIF file would be read, default is current folder.
#' @author Chen Gong Pratik Agrawal
#'

#' @importFrom data.table fread fwrite setDT
#' @importFrom quitte read.quitte read.gdx 
#' @importFrom readr write_rds
#' @importFrom stringr str_extract str_trim
#' @export
reportDIETER <- function(dieterDatafile = "report_DIETER.gdx",output_dir=".",input_dir="."
                         ) {
  #test
  # dieterData = "report_DIETER.gdx"
  dieterData = dieterDatafile
  
  sub_folder = "Dieter/"
  datapath <- function(fname){
    file.path(output_dir, sub_folder, fname)
  }
  
  
  
  gdxToQuitte_hourly <- function(gdxfile){
   # file = gdxfile
    file <- datapath(fname=gdxfile)
    out_hourly <- NULL
    
    ######################################################################################################################## 
    rep_hrs <- read.gdx(gdxName = file, requestList.name =  'report_hours', factors = FALSE, squeeze = FALSE) 
    
    names(rep_hrs) <- c("gdxfile", "model", "year", "country", "variable", "hour", "value")
    
    out_h <- rep_hrs %>% 
      dplyr::select(~model, ~year, ~variable, ~country, ~hour,~value) %>%
      mutate_(hour = as.numeric(stringr::str_extract(~hour, "[0-9]+"))) %>% 
      dplyr::group_by_(~model, ~variable, ~year,  ~country) %>%
      complete(hour = (1:8760)) %>%
      replace(is.na(~.), 0) %>%
      dplyr::ungroup(~model, ~variable, ~year,  ~country) %>%
      mutate_(Model = ~model, Scenario = paste0("baseline"), Region = ~country,
             Hour = ~hour, Tech = "all Tech") %>% 
      mutate_(Variable = ~variable, Period = ~year,
             Year =~year,
             Value = round(~value, digits = 4)) %>%
      dplyr::arrange(~Period) %>% 
      dplyr::select(~Model,~Scenario,~Region,~Variable,~Year,~Period,~Tech,~Value,~Hour)
    
    ###################################################################
    rep_techHrs <- read.gdx(gdxName = file, requestList.name = 'report_tech_hours', factors = FALSE, squeeze = FALSE) 
    
    names(rep_techHrs) <- c("gdxfile", "model","year","country","variable", "tech", "hour", "value")
    
    out_th <- rep_techHrs %>% 
      dplyr::select(~model, ~year, ~tech,~variable, ~country, ~hour,~value) %>%
      mutate_(hour = as.numeric(stringr::str_extract(~hour, "[0-9]+"))) %>% 
      mutate_(tech = as.character(~tech)) %>%
      group_by_(~model, ~tech, ~hour, ~variable, ~country) %>%
      mutate_(year = as.numeric(~year)) %>% 
      complete(year = c(2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100)) %>%
      
      dplyr::ungroup(~model, ~tech, ~hour, ~variable, ~country) %>% 
      group_by_(~model, ~year, ~variable, ~country,~tech) %>%
      complete(hour = (1:8760)) %>%
      replace(is.na(~.), 0) %>%
      dplyr::ungroup(~model, ~year, ~variable, ~country,~tech) %>% 
      mutate_(Model = ~model, Scenario = paste0("baseline"),  Region = ~country,
             Hour = ~hour, Year = ~year, Tech = ~tech) %>%
      mutate_(Variable = ~variable, Period = ~year,
             Value = round(~value, digits = 4)) %>%
      dplyr::arrange(~Year) %>% 
      dplyr::select(~Model,~Scenario,~Region,~Variable, ~Year,~Period, ~Tech, ~Value,~Hour)
    #################################################################
    
    out_hourly <- rbind(out_hourly, out_h)
    out_hourly <- rbind(out_hourly, out_th)
    
    #Unit column
    
    out_hourly$Unit<- substring(~out_hourly$Variable, regexpr("\\(", ~out_hourly$Variable))
    out_hourly <-  out_hourly %>% mutate_(Unit =~if_else(~grepl("\\(",Unit),~Unit,"NA") )
    out_hourly$Variable <- mapply(gsub, "\\(.*", "",~out_hourly$Variable)
    out_hourly$Unit <- mapply(gsub, "\\(", "",  ~out_hourly$Unit)
    out_hourly$Unit <- mapply(gsub, "\\)", "",  ~out_hourly$Unit)
    
    
    return(out_hourly)
    
  }
  
  gdxToQuitte_annual <- function(gdxfile){
    #file = gdxfile
    file <- datapath(fname=gdxfile)
    out_annual <- NULL
    ###########################################################################################################################
    rep <- read.gdx(gdxName = file, requestList.name = 'report', factors = FALSE, squeeze = FALSE) 
    
    names(rep) <- c("gdxfile", "model","year", "country","variable", "value")
    out <- rep %>% 
      mutate_(Model = ~model, Scenario = paste0("baseline"), 
             Region = ~country, Year = ~year, Value = ~round(value, digits = 4), 
             Tech = "all Tech",
             Variable = ~variable,
             Period = "annual") %>%
      dplyr::arrange(~Year) %>%
      dplyr::select(~Model,~Scenario,~Region,~Variable, ~Year,~Period, ~Tech, ~Value)
    
    #################################################################
    rep_Tech <- read.gdx(gdxName = file, requestList.name = 'report_tech', factors = FALSE, squeeze = FALSE) 
    
    names(rep_Tech) <- c("gdxfile", "model","year", "country","variable", "tech", "value")
    out_t <- rep_Tech %>% 
      dplyr::select(~model, ~year, ~tech,~variable, ~country,~value) %>%
      group_by_(~model, ~tech, ~variable, ~country) %>%
      mutate_(year = ~as.numeric(year)) %>%
      complete(year = c(2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100)) %>%
      replace(is.na(~.), 0) %>%
      dplyr::ungroup(~model, ~tech, ~variable, ~country) %>% 
      
      mutate_(Model = ~model, Scenario = paste0("baseline"), 
             Region = ~country, Year = ~year, Value = ~round(value, digits = 4), 
             Tech = ~tech,
             Variable = ~variable,
             Period = "annual"
      ) %>%
      dplyr::arrange(~Year) %>%
      dplyr::select(~Model,~Scenario,~Region,~Variable, ~Year,~Period, ~Tech, ~Value)
    
    #################################################################
    out_annual <- rbind(out_annual, out)
    out_annual <- rbind(out_annual, out_t)
    
    #Get Unit column from Variable column
    
    out_annual$Unit<- substring(out_annual$Variable, regexpr("\\(", out_annual$Variable))
    out_annual <-  out_annual %>% mutate(Unit =~if_else(~grepl("\\(",Unit),~Unit,"NA") )
    
    out_annual$Variable <- mapply(gsub, "\\(.*", "",~out_annual$Variable)
    out_annual$Unit <- mapply(gsub, "\\(", "",  ~out_annual$Unit)
    out_annual$Unit <- mapply(gsub, "\\)", "",  ~out_annual$Unit)
    
    
    return(out_annual)
    
  }
  
  output_folder <- output_dir
  input_folder <- input_dir
  #load main mif file
  name_mif <- list.files(input_folder, pattern = "REMIND_generic", full.names = F)
  miffile_name <- name_mif[!grepl("withoutPlu", name_mif)]
  name_mif <- file.path(input_folder, name_mif[!grepl("withoutPlu", name_mif)])
  
  stopifnot(typeof(name_mif) == "character")
  miffile <- fread(name_mif, sep=";", header=T)
  
  
  
  out_annual <- gdxToQuitte_annual(dieterData)
  
  annual_new <- data.table::dcast(out_annual, ... ~ Year, value.var="Value")
  
  
  # check if these years are already present, if not then add
  if(!'2005' %in% colnames(annual_new)){
    annual_new$`2005` <- NA
  }
  
  if(!'2110' %in% colnames(annual_new)){
    annual_new$`2110` <- NA
  }
  if(!'2130' %in% colnames(annual_new)){
    annual_new$`2130`<- NA
  }
  if(!'2150' %in% colnames(annual_new)){
    annual_new$`2150` <- NA
  }
  
  
  #technology mapping
  dieter.tech.mapping <- c(CCGT = "CCGT",
                           #lig = "Lignite",
                           Solar = "Solar",
                           Wind_on = "Wind",
                           bio = "Biomass",
                           OCGT_eff = "OCGT",
                           ror = "Hydro",
                           nuc = "Nuclear"
                           #hc = "Hard coal",
                           #coal = "Coal (Lig + HC)",
  )
  
  
  
  dieter.tech.mapping.standalone <- c(dieter.tech.mapping,
                                      lig = "Lignite",
                                      hc = "Hard coal",
                                      coal = "Coal (Lig + HC)",
                                      NULL)
  
  annual_new$Tech <- as.factor(annual_new$Tech)
  annual_remind <-  annual_new
  
  annual_new <- annual_new %>% quitte::revalue.levels_(Tech = dieter.tech.mapping.standalone)
  
  
  annual_new$Variable <- paste0(stringr::str_trim(annual_new$Variable,side = c("both")),"|",annual_new$Tech)
  annual_new  <- annual_new %>% dplyr::select(~-Period,~-Tech,~-`2005`)
  
  
  data.table::setDT(annual_new)
  EOL <- if (.Platform$OS.type=="windows") ";\r\n" else ";\n"
  # save Dieter Data as a seperate mif & rds
  fwrite(annual_new, paste0(output_folder,"/Dieter_Annual.mif"), append=F, sep=";", eol=EOL)
  read.quitte(paste0(output_folder,"/Dieter_Annual.mif")) %>% 
    readr::write_rds(paste0(gsub(".mif","",paste0(output_folder,"/Dieter_Annual.mif")),'.rds'), compress = 'xz') 
  
  
  #append model = 'DIETER' to REMIND-EU mif file
  
  ##then for the switch that appends to the main mif, use coal = "Coal (Lig + HC)", 
  
  dieter.tech.mapping.REMIND <- c(dieter.tech.mapping,
                                  coal = "Coal (Lig + HC)",
                                  NULL)
  
  annual_remind <- annual_remind %>% filter(~Model == "DIETER",~Tech !='lig',~Tech !='hc')
  annual_remind <- annual_remind %>% quitte::revalue.levels_(Tech = dieter.tech.mapping.REMIND)
  

  annual_remind$Variable <- paste0(stringr::str_trim(annual_remind$Variable,side = c("both")),"|",annual_remind$Tech)
  annual_remind  <- annual_remind %>% select( ~-Period,~-Tech)
  annual_remind <- annual_remind[ ,c(1:5,24,6:23)]
  
  
  data.table::setDT(annual_remind)
  
  # append to Main REMIND file
  file.copy(from=name_mif, to=paste0(output_folder,"/",miffile_name), overwrite=TRUE, recursive=FALSE)
  fwrite(annual_remind, paste0(output_folder,"/",miffile_name), append=T, sep=";", eol=EOL)
  
  
  read.quitte(paste0(output_folder,"/",miffile_name)) %>% 
    readr::write_rds(paste0(gsub(".mif","",paste0(output_folder,"/",miffile_name)),'.rds'), compress = 'xz') 
  
  
  ## Hourly Data
  out_hourly <- gdxToQuitte_hourly(dieterData)
  
  out_hourly  <- out_hourly %>%dplyr::select( ~-Period)
  
  hourly_new <- data.table::dcast(out_hourly, ... ~ Year, value.var="Value")
  hourly_new$Tech <- as.factor(hourly_new$Tech)
  hourly_new <- hourly_new %>% quitte::revalue.levels_(Tech = dieter.tech.mapping.standalone)
  
  
  hourly_new$Variable <- paste0(stringr::str_trim(hourly_new$Variable,side = c("both")),"|",hourly_new$Tech)
  hourly_new  <- hourly_new %>%dplyr::select( ~-Tech)
  
  hourly_new <- hourly_new[ ,c(1:4,6,7:24,5)]
  
  annual_new$Hour <- NA
  
  # combine annual & hourly data
  
  HourlyandAnnual <- rbind(hourly_new,annual_new)
  
  HourlyandAnnual <-HourlyandAnnual[ ,c(1:5,24,6:23)]
  
  write.table(HourlyandAnnual, paste0(output_folder, "/Dieter_Annualhourlyreport.csv"), sep = ";", row.names = F)
  # convert to  datatable
  # create mif and RDS
  data.table::setDT(HourlyandAnnual)
  EOL <- if (.Platform$OS.type=="windows") ";\r\n" else ";\n"
  fwrite(HourlyandAnnual, paste0(output_folder,"/Dieter_Annualhourly.mif"), append=F, sep=";", eol=EOL)
  
  
  read.quitte(paste0(output_folder,"/Dieter_Annualhourly.mif")) %>% 
    readr::write_rds(paste0(output_folder,"/Dieter_Annualhourly.rds"), compress = 'xz') 
  
}





