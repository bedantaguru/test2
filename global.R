
# generic cleap up
#rm(list = ls())

# For UI

library(shiny)
library(shinysky)
library(DT)



# Proxy Settings
Sys.setenv(http_proxy="http://172.30.1.78:3128")
Sys.setenv(https_proxy="https://172.30.1.78:3128")

options(stringsAsFactors = F)

library(rvest)
library(stringr)
library(gdata)
library(plyr)
library(data.table)

# not connected to UI
update_phonebook <- function(){
  
  suppressWarnings(unlink("data/contact.rds",force = T))
  
  rti_url <- "https://rbi.org.in/Scripts/Righttoinfoact.aspx"
  xl_link <- rti_url %>% read_html() %>% html_nodes(".link1")
  xl_link_dsc <- xl_link %>% html_text() %>% tolower() %>% str_detect("directory of its officers and employees")
  xl_link <- xl_link[which(xl_link_dsc)]
  xl_link <- xl_link %>% html_attrs()
  xl_link <- xl_link[[1]]["href"] %>% str_replace(.,"http:","https:")
  dir.create("data", showWarnings = F, recursive = T)
  download.file(url = xl_link, destfile = suppressWarnings(normalizePath(paste0("data/",basename(xl_link)))), mode = "wb", quiet = T, method = "curl")
  
  return(invisible(0))
}

read_data <- function(){
  
  data_rds_file <- "data/contact.rds"
  data_xls_file <- suppressWarnings(max(list.files("data/",full.names = T, pattern = ".xls$")))
  if(is.na(data_xls_file)){
    cat("\nNo contact file.\n")
    return(NULL)
  }
  
  if(file.exists(data_rds_file)){
    dat<-readRDS(data_rds_file)
  }else{
    dat <- read.xls(data_xls_file)
    
    dat$STD.Code <- ifelse(nchar(dat$STD.Code)==2, paste0("0",dat$STD.Code), dat$STD.Code)
    dat$Class <- paste0("Class ", dat$Class)
    dat$Name <- dat$Name %>% str_replace_all(.,"^[^a-zA-Z]+","")
    dat$Contact_ID<-seq(nrow(dat))
    
    dat$Name <- str_replace_all(dat$Name,","," ")
    
    dat$Name <- str_replace_all(dat$Name,"\\s+"," ")
    tmp <- str_count(dat$Name,"[A-Z]")/nchar(dat$Name)>0.6
    
    Capitalize<- function(x){
      if(length(x)==1){
        s <- strsplit(x, " ")[[1]]
        return(paste0(toupper(substring(s, 1,1)), substring(s, 2),collapse = " "))
      }
      o<-sapply(x, Capitalize)
      names(o)<-NULL
      return(o)
    }
    
    dat$Name[tmp] <- str_replace(dat$Name[tmp],"\\(","\\( ")
    dat$Name[tmp] <- Capitalize(tolower(dat$Name[tmp]))
    dat$Name[tmp] <- str_replace(dat$Name[tmp],"\\( ","\\(")
    for(cn in c("Designation","Department","Attached.to","Name")){
      dat[[paste0(cn,"Short")]] <- dat[[cn]] %>% str_extract_all(., "[A-Z]|-i-|-in-|\\(|\\)") %>% lapply(paste0,collapse = "") %>% unlist()
    }
    rm(cn, tmp)
    
    saveRDS(dat,data_rds_file)
  }
  
  
  
  info_columns <- c("Name","DesignationShort","DepartmentShort","Attached.toShort","NameShort","Centre","Grade.Group","Designation","Department","Attached.to")
  
  item_list <- ldply(info_columns, function(cn){
    data.frame(item = unique(dat[[cn]]), desc = cn)
  })
  item_list$item<- str_trim(item_list$item)
  item_list<- item_list[nchar(item_list$item)!=0,]
  item_list$desc <- factor(item_list$desc, levels = info_columns)
  
  # locate the contact
  # 2016-09-23 : only exact match supported now. Aproximate match will be implemented later
  fetch_contact<- function(single_word){
    # generic function to fetch contacts
    get_parts<- function(match_tar, tag_item){
      if(length(match_tar)>0){
        match_items <- item_list[ match_tar, ]
        contacts <- alply(match_items, 1, function(d){
          list(contacts=dat[dat[[as.character(d$desc[1])]]==d$item[1],], tag=tag_item)
        })
        return(contacts)
      }else{
        return(NULL)
      }
    }
    
    # match exact 
    match_exact <- which(item_list$item==single_word) %>% get_parts(.,90)
    # match caseless exact 
    match_caseless_exact <- which(tolower(item_list$item)==tolower(single_word)) %>% get_parts(.,80)
    
    # combine all fetched contacts
    all_fetched_contacts <- c(match_exact, match_caseless_exact)
    
    # common check
    common_contacts <- all_fetched_contacts %>% lapply(.,"[[",1) %>% lapply(.,"[[","Contact_ID") %>% Reduce(intersect, .)
    if(length(common_contacts)>0){
      common_contacts<- list(list(contacts=dat[dat$Contact_ID %in% common_contacts, ], tag = 95))
      all_fetched_contacts<-c(all_fetched_contacts, common_contacts)
    }
    
    # re-weighting
    group_counts <- all_fetched_contacts %>% lapply(.,"[[","contacts") %>% lapply(.,nrow) %>% unlist()
    if(diff(range(group_counts))!=0){
      group_wts <- (group_counts-min(group_counts))/diff(range(group_counts))*10
      #all_fetched_contacts %>% lapply(.,"[[","tag")
      for(i in seq(length(all_fetched_contacts))){
        all_fetched_contacts[[i]][["tag"]]<-as.numeric(all_fetched_contacts[[i]][["tag"]]-group_wts[i])
      }
      rm(i)
    }
    
    # duplicate reduction
    C_ID_occurrence <- ldply( all_fetched_contacts, function(dn){
      d <- dn[["contacts"]]["Contact_ID"]
      d$match_level <- dn[["tag"]]
      d
    })
    C_ID_occurrence[[".id"]]<-NULL
    C_ID_occurrence<-unique(C_ID_occurrence)
    C_ID_occurrence_a <- aggregate(C_ID_occurrence["match_level"],C_ID_occurrence["Contact_ID"],max)
    fetched_contacts<- merge(dat, C_ID_occurrence_a)
    
    return(fetched_contacts)
  }
  
  # implementable get contacts
  get_contacts <- function(query){
    contact_blobs <- lapply(query, fetch_contact)
    
    # re-weighting
    group_counts <- contact_blobs %>% lapply(.,nrow) %>% unlist()
    if(diff(range(group_counts))!=0){
      group_wts <- (group_counts-min(group_counts))/diff(range(group_counts))*20
      
      for(i in seq(length(group_counts))){
        contact_blobs[[i]][["match_level"]]<-as.numeric(contact_blobs[[i]][["match_level"]]-group_wts[i])
      }
      rm(i)
    }
    
    contacts_out <- NULL
    
    # only common contacts implemented as of now
    common_contacts <- contact_blobs %>% lapply(.,"[[","Contact_ID") %>% Reduce(intersect, .)
    if(length(contact_blobs)>0){
      contact_blobs_common<- lapply(contact_blobs, function(d){
        d<-d[ d$Contact_ID %in% common_contacts, ]
        d<-d[order(d$Contact_ID),]
        return(d)
      })
      contact_blobs_common_wts<- contact_blobs_common %>% lapply(.,"[[","match_level") %>% Reduce(function(x,y){x+y},.)
      contact_blobs_common <- contact_blobs_common[[1]]
      contact_blobs_common$match_level <- contact_blobs_common_wts
      
      contacts_out<- contact_blobs_common
    }
    
    # highest matching sets can be implemented here
    
    return(contacts_out)
    
  }
  
  # expandable search tag
  
  expand_item <- function(item_list_in){
    Short_desc<- item_list$desc[str_detect(item_list$desc,"Short$")] %>% unique() %>% as.character()
    item_list_in_In_Short_desc <- item_list[ (item_list$item %in% item_list_in) & (item_list$desc %in% Short_desc),]
    if(nrow(item_list_in_In_Short_desc)>0){
      expand1 <- alply(item_list_in_In_Short_desc, 1, function(d){
        do<-dat[dat[[as.character(d$desc[1])]]==d$item[1],]
        unique(do[[str_replace(d$desc[1],"Short$","")]])
      }) %>% unlist() %>% unique()
      item_list_in <- union(item_list_in, expand1)
    }
    item_list_out <- item_list[item_list$item %in% item_list_in,]
    item_list_out <- item_list_out[order(item_list_out$item),]
    item_list_out <- item_list_out[order(item_list_out$desc),]
    return(item_list_out$item)
  }
  
  item_list_single_no_desc <- unique(item_list$item)
  
  out_items <- list(contacts_data = dat, item_list = item_list_single_no_desc,getContacts=get_contacts, item_list_with_desc = item_list, expandItem=expand_item)
  
  return(out_items)
  
}

# global variables
# static initial loading of data
contactDetails <- read_data()
is_init_search <- T
