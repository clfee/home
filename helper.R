## Helper function

# combine data 
new_file_org <- function(dt){
  dt <- dt |>
    filter(Executive.First.Name != "",
           Phone != "")|>
    select(Company.Name,Address,City,Location.ZIP.Code,Phone, 
           Primary.SIC.Code,Primary.SIC.Description,Executive.First.Name,Executive.Last.Name,
           Executive.Gender,Executive.Title,Professional.Title)|>
    mutate(Website = "",
           Primary.SIC.Year.Appeared = 2024,
           Company.Description = "")}

old_file_org <- function(dt){
  dt <- dt |>
    filter(Executive.First.Name != "",
           Phone.Number.Combined != "")|>
    mutate(Phone = Phone.Number.Combined ,
           Location.ZIP.Code = ZIP.Code)|>
    select(Company.Name,Executive.First.Name,
           Executive.Last.Name,Phone,Website,Address,City,
           Location.ZIP.Code,Primary.SIC.Code,Primary.SIC.Description,
           Company.Description,Executive.Gender,Executive.Title,
           Professional.Title,Primary.SIC.Year.Appeared)}

comb_file_org <-  function(old_dt, new_dt, call_date = "2024/07/", source_ = "self" ){
  new_ <- new_file_org(new_dt)
  old_ <- old_file_org(old_dt)
  comb_dt<- rbind(new_ , old_)|>
    select(Company.Name,Executive.First.Name,
           Executive.Last.Name,Phone,Website,Address,City, 
           Location.ZIP.Code,Primary.SIC.Code,Primary.SIC.Description,
           Company.Description,Executive.Gender)|>
    mutate(
      accepting_cc = "Y/N",
      current_merchant_provider = "",
      call_duration = "",
      first_call = call_date,
      first_call_duration = "",
      second_call = call_date,
      second_call_duration = "",
      request_follow_up = "Y/N",
      conversion_status = "Y/N",
      lead_source = source_,
      bank_profile = "Y/N" )
  unique(comb_dt)
  
}

# Data Processing 
get_range <- function(var, data=train){
  target = data %>% distinct(!!sym(var)) %>% pull(1)
  
  if(is.numeric(target) | is.integer(target)){
    str1 = paste(range(target), collapse=", ")
    return(paste0("[",str1,"]"))} 
  else{
    str1 = paste(target,collapse = "、")
    str2 = paste0("range",length(target),"values")
    return(paste0(str1,str2))} }
#
get_type <- function(var, data=train){
  target = data %>% distinct(!!sym(var)) %>% pull(1)
  
  if(is.numeric(target) | is.integer(target)){
    return("continuous")} 
  else{
    return("discrete")} }


#
cal_freq <- function(var, data= train){
  if(get_type(colnames(data)[var])=="discrete"){
    dt <- as.data.frame(table(data[,var]))
    dt$namex <- colnames(data)[var]
    dt$Freqx <- dt$Freq/nrow(data)
    is.na(dt) <- "missing"
    return(dt[,c("namex","Var1","Freqx")])}else{ dt <- NULL}
}

## Glimpse data and output the descriptions
my_glimpse = function(data = bank, file=F){
  
  temp = tibble("vars" = data %>% colnames)
  
  if(file==F){
    
    temp %<>%
      rowwise() %>%
      transmute(vars,
                type_ = get_type(vars),
                range_ = get_range(vars))
    return(temp)
  }else{
    temp = tibble("vars" = data %>% colnames)
    temp %<>%
      rowwise() %>%
      transmute(var_tyep=NA,
                var_name = NA,
                vars,
                type_ = get_type(vars),
                var_content=NA,
                range_ = get_range(vars)) 
    write.csv(temp,file)
    return(temp)
  }
}

# data prep for one-hot 
library(data.table)
category_cor <- function(data_, var_, target_){
 # df <- data.table(data_)
  a <- data_ |> select(all_of(var_))
  unique_x <- unique(a)|>pull(var_)
  table_one_hot <- data.table(data_ |> select(any_of(c(var_, target_))))
  table_one_hot[, (paste0(unique_x,"_")) := lapply(unique_x, function(x) as.integer(a == x))]
  
  cormat <- round(cor(table_one_hot[,-1]),2)
  cormat[lower.tri(cormat)]<- NA
  cormat <-  melt(cormat) 
  colnames(cormat) <- c("X1","X2","value")
  cormat|>
    ggplot(aes(x=X2, y=X1, fill=value)) + 
    geom_tile()+
    geom_text(aes(X2, X1, label = value), color = "white", size = 4)+
    theme_minimal() +
    labs(y = NULL, x = NULL)
}


