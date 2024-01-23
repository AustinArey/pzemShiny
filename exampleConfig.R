# mysql config
config_host <- "my.host"
config_user <- "user"
config_password <- "pw"
config_dbname <- "db"

# tables arranged
tables <- list(
  group1 = c("group1_meter1", "group1_meter2"),
  group2 = c("group2_meter1", "group2_meter2"),
  group3 = c("group3_meter1", "group3_meter2", "group3_meter3")
)

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
