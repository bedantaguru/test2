
shinyServer(function(input, output, session) {
  
  observe({
    # reactive dendency
    if(is.null(input$in_query)){
      is_init_search <<- T
    }
    
  })
  
  output$ui_further_drill<- renderUI({
    # reactive dendency
    input$in_query
    
    # global variable dendency 
    if(is_init_search & !is.null(input$in_query)){
      part_query <- paste0(isolate(input$in_query), collapse = "")
      
      if(nchar(part_query)>0){
        part_query<- tolower(part_query)
        d<- contactDetails$item_list_with_desc
        
        # all the attempts
        shortlist<- contactDetails$item_list[str_detect(tolower(contactDetails$item_list), part_query)]
        if(length(shortlist)>1000){
          shortlist<- d$item[d$desc!="Name"][str_detect(tolower(d$item[d$desc!="Name"]), part_query)]
          if(length(shortlist)>1000){
            shortlist<- contactDetails$item_list[str_detect(tolower(contactDetails$item_list), paste0("^",part_query))]
          }
        }
        
        # process if less number of option is present
        if(length(shortlist)<1000){
          shortlist_names <-d$item[d$desc=="NameShort"][str_detect(tolower(d$item[d$desc=="NameShort"]), part_query)]
          shortlist<-union(shortlist, shortlist_names)
          shortlist <- contactDetails$expandItem(shortlist)
          # final implement
          if(length(shortlist)<2000 & length(shortlist)>0){
            is_init_search<<-F
            return(select2Input("in_query_final", "Search Here Now",choices=shortlist,selected=""))
          }
        }
        
        rm(d)
      }
    }
    
    return(NULL)
  })
  
  contact_data<- reactive({
    if(!is.null(input$in_query_final)){
      do<-contactDetails$getContacts(input$in_query_final)
      if(nrow(do)<10){
        do<-do[c("Name","Designation","Phone","Extension","Department")]
        return(do)
      }
    }
    return(NULL)
  })
  
  output$out_contacts <- renderDataTable({
    
    contact_data()
    
  })
  
})
