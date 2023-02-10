db2
unique(db2$ctr_reg)
unique(db2$raw_mothag6)
unique(db2$raw_edumo04)

# ejemplo con un grupo
chunk <- 
  db2 %>% 
  filter(ctr_reg == "COL-Bogota D.C.",
         raw_mothag6 == "25-29",
         raw_edumo04 == "Secon")


model_glm <- 
  glm(raw_nbirth.current ~ t + trim_1 + trim_2 + trim_3 + trim_4, 
      weights = w,
      data = chunk, 
      family = quasipoisson(link = "log"))  
