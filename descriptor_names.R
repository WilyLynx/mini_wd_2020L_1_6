descriptors <- read_csv("../../2020L-WizualizacjaDanych/data/descriptors.csv") 

find_descriptor_name <- function(possible_descriptors){
  names <- descriptors %>% 
  filter(descriptors %in% possible_descriptors) %>% 
  group_by(descriptors_raw) %>% 
  summarise(n=n())
  return(names)
}


##### PLOT 7 ####                   
find_descriptor_name(c("ST8_005","ST8_5"))
find_descriptor_name(c("NZ9_003","NZ9_3"))
find_descriptor_name(c("NZ7_014","NZ9_14"))
find_descriptor_name(c("ST5_019","ST5_19"))
find_descriptor_name(c("ST7_007","ST7_7"))


##### PLOT 8 ####                   
find_descriptor_name(c("NZ4_016","NZ4_16"))
find_descriptor_name(c("NZ5_009","NZ5_9"))
find_descriptor_name(c("NZ1_008","NZ1_8"))
find_descriptor_name(c("NZ3_003","NZ3_3"))
find_descriptor_name(c("ST9_014","ST9_14"))

##### PLOT 9 ####                   
find_descriptor_name(c("HS6_005","HS6_5"))
find_descriptor_name(c("NZ1_006","NZ1_6"))
find_descriptor_name(c("ST10_003","ST10_3"))
find_descriptor_name(c("ST10_008","ST10_8"))
find_descriptor_name(c("ST8_008","ST8_8"))
