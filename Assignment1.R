library(dplyr)
library(tidyr)
library(readr)
refine <- read_csv("~/GitHub/springboard/refine.csv")


# arrange 
refine <- arrange(refine, company)

# clean company names
refine <- refine %>% 
  mutate(company = ifelse(company %in% c("fillips", "phillipS", "Phillips", "phillps", "phlips", "phllips"), "phillips", company)) %>% 
  mutate(company = ifelse(company %in% c("AKZO", "akz0", "AKZO", "ak zo", "Akzo"), "akzo", company)) %>% 
  mutate(company = ifelse(company %in% c("fillips", "phillipS", "Phillips", "phillps", "phlips", "phllips"), "phillips", company)) %>%
  mutate(company = ifelse(company %in% c("van Houten", "Van Houten", "Van Houten"), "van houten", company)) 

# separate product code and number
library(tidyr)
refine <- refine %>%  
  separate('Product code / number', c("product_code", "product_number"), sep="-")

# add product categories
refine <- refine %>%
  mutate(product_category = ifelse(product_code=="x", "Laptop",
                                   ifelse(product_code == "p", "Smartphone",
                                          ifelse(product_code == "q", "Tablet",
                                                 ifelse(product_code == "v", "TV", "NA")))))

# add full address for geocoding
refine <- refine %>%
  unite(full_address, address:country, sep=", ", remove = TRUE)

# dummy variables
refine <- refine %>%
  mutate(product_smartphone = ifelse(product_code=="p", "1", "0"),
         product_tablet = ifelse(product_code=="q", "1", "0"),
         product_laptop = ifelse(product_code=="x", "1", "0"),
         product_tv = ifelse(product_code=="v", "1", "0"))

View(refine)





