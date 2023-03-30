#mysql config
config_host = 'my.host'
config_user='user'
config_password='pw'
config_dbname='db'

#meter grouping
group_list <- c("Group1", "Group2", "Group3")

#tables arranged
tables <- list(pzem580=c("Group1_meter1","Group1_meter2"),
               pzem582=c("Group2_meter1","Group2_meter2"),
               pzemTerry=c("Group3_meter1","Group3_meter2","Group3_meter3"))

# define some credentials
credentials <- data.frame(
  user = c("username", "adminuser"), # mandatory
  password = c("01234", "56789"), # mandatory
  start = c("2019-04-15"), # optinal (all others)
  expire = c(NA, "2019-12-31"),
  admin = c(FALSE, TRUE),
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)