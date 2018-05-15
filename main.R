# Procedure for near-time tracking of user performance
# Writes log on local storage and outputs plots to visually track pf
# Script can be run 5 mins apart at minium (at best: 30 mins)

# Redirect stdout to logfile
scriptLog <- file("scriptLog", open = "wt")
sink(scriptLog, type = "message")

# Load required libs
library(config)
library(here)
library(ggplot2)
library(scales)
library(dplyr)
library(lubridate)
library(stringr)
library(ggiraph)
library(xlsx)

# Quit if sysdate == weekend ------------------------------------------------------------
stopifnot(!(strftime(Sys.Date(), '%u') == 7 | hour(Sys.time()) >= 18))

# Create default dirs
dir.create(here::here("Reports"), showWarnings = FALSE)

##########################################################################################
# Extract Data ###########################################################################
##########################################################################################

# Set JAVA_HOME, set max. memory, and load rJava library
Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jre1.8.0_171")
options(java.parameters = "-Xmx2g")
library(rJava)

# Output Java version
.jinit()
print(.jcall("java/lang/System", "S", "getProperty", "java.version"))

# Load RJDBC library
library(RJDBC)

# Create connection driver and open connection
jdbcDriver <-
  JDBC(driverClass = "oracle.jdbc.OracleDriver", classPath = "C:\\Users\\PoorJ\\Desktop\\ojdbc7.jar")

# Get Kontakt credentials
kontakt <-
  config::get("kontakt",
              file = "C:\\Users\\PoorJ\\Projects\\config.yml")

# Open connection
jdbcConnection <-
  dbConnect(
    jdbcDriver,
    url = kontakt$server,
    user = kontakt$uid,
    password = kontakt$pwd
  )

# Get SQL scripts
readQuery <-
  function(file)
    paste(readLines(file, warn = FALSE), collapse = "\n")

get_data <-
  readQuery(here::here("SQL", "db_query.sql"))

# Run queries
t_pf_tracker <- dbGetQuery(jdbcConnection, get_data)

# Close connection
dbDisconnect(jdbcConnection)


#########################################################################################
# Data Transformation ###################################################################
#########################################################################################

t_user_osszesen <- group_by(t_pf_tracker, IDOPONT, CSOPORT, LOGIN) %>%
  summarize(
    DB = length(F_IVK),
    ATL = mean(CKLIDO),
    MED = median(CKLIDO)
  ) %>%
  mutate(FELADAT = "VÉGÖSSZEG")
t_user_osszesen <- t_user_osszesen[, c(1, 2, 3, 7, 4, 5, 6)]

t_user_telj <- mutate(t_pf_tracker, FELADAT = TEVEKENYSEG) %>%
  group_by(IDOPONT, CSOPORT, LOGIN, FELADAT) %>%
  summarize(
    DB = length(F_IVK),
    ATL = mean(CKLIDO),
    MED = median(CKLIDO)
  )

t_user_telj <- rbind(t_user_telj, t_user_osszesen)

# Set up libs on local storage
dir.create(here::here("Reports", floor_date(Sys.Date(), "day")))
dir.create(here::here("Reports", floor_date(Sys.Date(), "day"), "DOLGOZO"))
dir.create(here::here("Reports", floor_date(Sys.Date(), "day"), "CSOPORT"))

# Set-up daily log file and save to local storage
flist <- list.files(here::here("Reports", floor_date(Sys.Date(), "day")), ".csv")

if (length(flist) == 0) {
  t_naplo <- t_user_telj
} else {
  t_naplo <- read.csv(here::here(
    "Reports", floor_date(Sys.Date(), "day"),
    paste0("t_naplo_", floor_date(Sys.Date()), ".csv")
  ), stringsAsFactors = FALSE)
  t_user_telj <- t_user_telj[!t_user_telj$IDOPONT %in% levels(t_naplo$IDOPONT), ] # Exclude duplicates
  t_naplo <- rbind(t_naplo, data.frame(t_user_telj))
}

write.csv(t_naplo,
  here::here(
    "Reports", floor_date(Sys.Date(), "day"),
    paste0("t_naplo_", floor_date(Sys.Date()), ".csv")
  ),
  row.names = FALSE
)    

# Aggregate
t_naplo <- t_naplo %>%
  filter(day(ymd_hms(IDOPONT)) == day(Sys.Date())) %>% # Exclude duplicates
  ungroup() %>%
  mutate(
    DATUM = ymd_hms(IDOPONT),
    IDOPONT = format(ymd_hms(IDOPONT), "%H:%M"),
    USER = as.factor(paste(CSOPORT, LOGIN))
  ) %>%
  rename(DB_TOTAL = DB) %>%
  arrange(LOGIN, FELADAT, IDOPONT) %>%
  group_by(LOGIN, FELADAT) %>%
  mutate(DB = lag(DB_TOTAL)) %>%
  ungroup() %>%
  tidyr::replace_na(list(DB = 0)) %>%
  mutate(DB = DB_TOTAL - DB)

t_idopont <- data.frame(IDOPONT = unique(t_naplo$IDOPONT), stringsAsFactors = FALSE) %>%
  arrange(IDOPONT) %>%
  mutate(
    TOL = lag(IDOPONT),
    IG = IDOPONT
  ) %>%
  tidyr::replace_na(list(TOL = "-")) %>%
  mutate(IDOSZAK = paste0(TOL, "-", IG)) %>%
  select(IDOPONT, IDOSZAK)

t_naplo <- left_join(t_naplo, t_idopont, by = "IDOPONT")
t_naplo <- t_naplo[, c(8, 11, 2, 3, 9, 4, 5, 10, 6, 7)]

write.csv(t_naplo,
  here::here(
    "Reports", floor_date(Sys.Date(), "day"),
    paste0("t_user_telj_", floor_date(Sys.Date()), ".csv")
  ),
  row.names = FALSE
)


#########################################################################################
# Output generation #####################################################################
#########################################################################################

# Plot tasks per user -------------------------------------------------------------------
for (i in levels(t_naplo$USER)) {
  t_user <- t_naplo[t_naplo[["USER"]] == i, ]
  t_user <- arrange(t_user, DATUM, IDOSZAK, FELADAT)

  p1 <- ggplot(t_user, aes(x = IDOSZAK, y = DB)) +
    geom_bar(stat = "identity", fill = "#0072B2") +
    theme(strip.text.y = element_text(angle = 0)) +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_text(
      data = t_user[t_user$DB != 0, ], aes(label = DB),
      size = 3.4, fontface = "bold", colour = "black", vjust = 1, hjust = 0.5
    ) +
    facet_grid(. ~ FELADAT) +
    ggtitle(sprintf("%s napon belüli teljesítménye", i))

  i <- chartr("áéóõöûüíÁÉÓÕÖÛÜÍ", "aeooouuiAEOOOUUI", i)

  # Save to local storage
  ggsave(here::here(
    "Reports", floor_date(Sys.Date(), "day"), "DOLGOZO",
    paste0(i, ".png")
  ),
  p1,
  width = 12, height = 7, dpi = 300
  )
  
  # Copy to publish folder
  try(file.copy(
    here::here(
      "Reports", floor_date(Sys.Date(), "day"), "DOLGOZO",
      paste0(i, ".png")),
    "C:/Users/PoorJ/Publish/Live/Napi-Darabok/DOLGOZO",
    overwrite = T
  ))
  
}

# Plot tasks per group ------------------------------------------------------------------
t_utso <- t_naplo %>%
  # filter(DATUM == max(unique(DATUM))) %>%
  mutate(
    CSOPORT = as.factor(CSOPORT),
    DATUM = ymd_hms(DATUM),
    IDOPONT = format(ymd_hms(DATUM), "%H:%M")
  ) %>%
  group_by(IDOPONT, CSOPORT, FELADAT) %>%
  summarize(DB_TOTAL = sum(DB_TOTAL))

t_idopont <- unique(t_utso$IDOPONT)

for (i in levels(t_utso$CSOPORT)) {
  t_csoport <- t_utso %>%
    filter(CSOPORT == i) %>%
    arrange(IDOPONT, FELADAT)

  p1 <- ggplot(t_csoport, aes(x = IDOPONT, y = DB_TOTAL)) +
    geom_bar(stat = "identity", fill = "#0072B2") +
    theme(strip.text.x = element_text(angle = 90, size = 10)) +
    theme(axis.text.x = element_text(angle = 90)) +
    facet_grid(. ~ FELADAT) +
    ggtitle(sprintf("%s csoport napi göngyölt teljesítménye %s-kor", i, t_idopont))

  i <- chartr("áéóõöûüíÁÉÓÕÖÛÜÍ", "aeooouuiAEOOOUUI", i)

  # Save to local storage
  ggsave(here::here(
    "Reports", floor_date(Sys.Date(), "day"), "CSOPORT",
    paste0(i, ".png")
  ),
  p1,
  width = 12, height = 7, dpi = 300
  )
  
 # Copy to publish folder
try(file.copy(
  here::here(
    "Reports", floor_date(Sys.Date(), "day"), "CSOPORT",
    paste0(i, ".png")
  ),
  "C:/Users/PoorJ/Publish/Live/Napi-Darabok/CSOPORT",
  overwrite = T
))
  
}

# Save xlsx with cumulated values to local storage
t_kum_dolgozo <- select(t_naplo, -c(IDOSZAK, USER, DB, ATL, MED)) %>%
  filter(FELADAT != "VÉGÖSSZEG" & DATUM == max(DATUM)) %>%
  mutate(DATUM = as.character(DATUM))
attributes(t_kum_dolgozo)$class <- c("data.frame")
write.xlsx(t_kum_dolgozo,
  here::here(
    "Reports", floor_date(Sys.Date(), "day"),
    paste0("t_kumulalt_most_", floor_date(Sys.Date(), "day"), ".xlsx")
  ),
  row.names = FALSE
)

# Copy to publish folder
try(file.copy(
  here::here(
    "Reports", floor_date(Sys.Date(), "day"),
    paste0("t_kumulalt_most_", floor_date(Sys.Date(), "day"), ".xlsx")
  ),
  "C:/Users/PoorJ/Publish/Live/Napi-Darabok",
  overwrite = T
))

# Redirect stdout back to console
sink(type = "message")
close(scriptLog)