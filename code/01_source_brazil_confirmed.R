source("Code/00_functions.R")
library(googledrive)
library(googlesheets4)

# assigning Drive credentials in the case the script is verified manually  
if (!"email" %in% ls()){
  email <- "kikepaila@gmail.com"
}

googledrive::drive_auth(email = email)
gs4_auth(email = email)


# looking at the spreadsheets in Brazil Drive folder 
content <- 
  drive_ls("https://drive.google.com/drive/u/0/folders/1Jsb9Ymq7fGrMyJ4ZcsvdgWJqVydGxTGf")

# spreadsheets to exclude
excl <- c("Brazil_all_states input template",
          "Brazil_input_info")

links_br <- 
  content %>% 
  filter(!name %in% excl) %>% 
  dplyr::pull(id)

out <- NULL
for(ss_i in links_br){
  temp <- 
    read_sheet(ss_i, 
               sheet = "database", 
               na = "NA", 
               col_types= "cccccciccd",
               range = "database!A:J")
  out <- 
    out %>% 
    bind_rows(temp)
}

unique(out$Age)

out2 <- 
  out %>% 
  filter(Sex == "b") %>% 
  mutate(date = dmy(Date)) %>% 
  group_by(Country, Region, date, Measure) %>% 
  summarise(Value = sum(Value)) %>% 
  ungroup() %>% 
  group_by(Country, Region, Measure) %>% 
  mutate(new = Value - lag(Value)) %>% 
  drop_na() %>% 
  select(-Value) %>% 
  spread(Measure, new) %>% 
  rename(country = Country,
         div = Region,
         dts = Deaths)

write_rds(out2, "data_inter/confirmed_brazil_state.rds")
out2 <- read_rds("data_inter/confirmed_brazil_state.rds")
