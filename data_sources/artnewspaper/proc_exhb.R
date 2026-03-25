## * processing individual exhibitions (2000-2005) rather than museums with gemini

library(ellmer)
library(stringr)
library(pmdata)

dt_tanp05_raw <- gd_tanp05_raw() # get raw data
dt_tanp05_struc <- gd_tanp05_struc(dt_tanp05_raw, limit = 10000) # get LLM data
## dt_tanp05_struc <- dt_output_par2# old version using global data
gd_tanp05_asses(dt_tanp05_struc) # assess 
gd_tanp05_mnlcheck(dt_tanp05_struc) %>% print(n=80)

dt_tanp05_struc[, .(yearly_visitors = sum(total_visitors)), .(venue_name, city)][order(-yearly_visitors)] %>%
    .[grepl("Museum of Modern", venue_name)]

dt_tanp05_struc[, .(yearly_visitors = sum(total_visitors)), .(venue_name, city)][order(-yearly_visitors)] %>%
    .[grepl("New York|NYC|NY", city)]


dt_tanp05_struc[, .(yearly_visitors = sum(total_visitors)), .(venue_name, city)][order(-yearly_visitors)] %>% print(n=80)

dt_tanp05_struc[venue_name == "Museum of Modern Art" & city == "New York"]



gd_proc_exhb(dt_tanp05_struc, "tanp05_")


## ** add tanp04

## structure looks pretty much identical to 05 -> can reuse code

dt_tanp04_raw <- data.table(text = readLines("~/Dropbox/phd/pmdata/data_sources/artnewspaper/tanp_04_raw.csv"))

dt_tanp04_raw[, `:=`(cnt_comma = str_count(text, ","), cnt_slash = str_count(text, "/"),
                     cnt_dash = str_count(text, "-"))]


dt_tanp04_raw[, .N, cnt_comma]
dt_tanp04_raw[, .N, cnt_slash]


dt_tanp04_raw[cnt_slash == 4, id_show := 1:.N] %>%
    setnafill(type = "nocb", cols = "id_show") %>%
    .[, lines := .N, id_show]

dt_tanp04_raw[, .N, lines]
dt_tanp04_raw[lines > 6]

dt_tanp04_raw[grepl("sublime landscape", text)] # -> 223
dt_tanp04_raw[grepl("Urbino", text)] # -> 230

dt_tanp04_raw[, max(id_show)]

## dt_tanp04_struc <- gd_tanp05_struc(dt_tanp04_raw[id_show < 200], limit = 99999)
## dt_tanp04_struc2 <- gd_tanp05_struc(dt_tanp04_raw[id_show >= 200], limit = 99999)


dt_tanp04_raw[, nchar := nchar(text)]
dt_tanp04_raw[order(-nchar)]

dt_tanp04_raw[id_show == 297]

fwrite(dt_tanp04_struc2, "~/Dropbox/phd/pmdata/data_sources/artnewspaper/llm/tanp04_llm_1.csv")

dt_tanp04_struc <- fread("/home/johannes/Dropbox/phd/pmdata/data_sources/artnewspaper/tanp_04_struc.csv")
gd_tanp05_asses(dt_tanp04_struc) # assess 


dt_tanp04_struc[grepl("Rover", show_name)]

gd_proc_exhb(dt_tanp04_struc, "tanp04_")

## ** add tanp03

dt_tanp03_raw <- data.table(text = readLines("~/Dropbox/phd/pmdata/data_sources/artnewspaper/llm/tanp03_raw.csv"))
dt_tanp03_raw[, `:=`(cnt_slash = str_count(text, "/"))]

dt_tanp03_raw[, .N, cnt_slash]

dt_tanp03_raw[cnt_slash == 4, id_show := 1:.N] %>%
    setnafill(type = "nocb", cols = "id_show") %>%
    .[, lines := .N, id_show]

dt_tanp03_raw[, .N, lines]

## gd_tanp05_struc(dt_tanp03_raw, limit = 9999)
dt_tanp03_struc <- fread("/home/johannes/Dropbox/phd/pmdata/data_sources/artnewspaper/tanp_03_struc.csv")
gd_tanp05_asses(dt_tanp03_struc)

gd_proc_exhb(dt_tanp03_struc, id_prefix = "tanp03_")



## ** add tanp02
dt_tanp02_raw <- data.table(text = readLines("~/Dropbox/phd/pmdata/data_sources/artnewspaper/llm/tanp02_raw.csv"))
dt_tanp02_raw[, `:=`(cnt_slash = str_count(text, "/"))]

dt_tanp02_raw[cnt_slash == 4, id_show := 1:.N] %>%
    setnafill(type = "nocb", cols = "id_show") %>%
    .[, lines := .N, id_show]

dt_tanp02_raw[, .N, lines]
dt_tanp02_raw[lines > 10] %>% print(n=80)


## gd_tanp05_struc(dt_tanp02_raw, limit = 9999)

dt_tanp02_struc <- fread("/home/johannes/Dropbox/phd/pmdata/data_sources/artnewspaper/tanp_02_struc.csv")

gd_tanp05_asses(dt_tanp02_struc)
gd_proc_exhb(dt_tanp02_struc, "tanp02_")


## ** add tanp01

dt_tanp01_raw <- data.table(text = readLines("~/Dropbox/phd/pmdata/data_sources/artnewspaper/llm/tanp01_raw.csv"))
dt_tanp01_raw[, `:=`(cnt_slash = str_count(text, "/"))]

dt_tanp01_raw[cnt_slash == 4, id_show := 1:.N] %>%
    setnafill(type = "nocb", cols = "id_show") %>%
    .[, lines := .N, id_show]

dt_tanp01_raw[, .N, lines]
dt_tanp01_raw[lines > 10] %>% print(n=80)

## gd_tanp05_struc(dt_tanp01_raw[id_show < 300], limit = 9999)
## gd_tanp05_struc(dt_tanp01_raw[id_show %between% c(300,370)], limit = 9999)
## gd_tanp05_struc(dt_tanp01_raw[id_show %between% c(371,400)], limit = 9999)
## gd_tanp05_struc(dt_tanp01_raw[id_show %between% c(401,500)], limit = 9999)
## gd_tanp05_struc(dt_tanp01_raw[id_show > 500], limit = 9999)

dt_tanp01_struc <- fread("/home/johannes/Dropbox/phd/pmdata/data_sources/artnewspaper/tanp_01_struc.csv")
dt_tanp01_struc[, .N, id_show][N>1]

gd_proc_exhb(dt_tanp01_struc, "tanp01_")



## ** tanp00

dt_tanp00_raw <- data.table(text = readLines("~/Dropbox/phd/pmdata/data_sources/artnewspaper/llm/tanp00_raw.csv"))
dt_tanp00_raw[, `:=`(cnt_slash = str_count(text, "/"))]

dt_tanp00_raw[cnt_slash == 4, id_show := 1:.N] %>%
    setnafill(type = "nocb", cols = "id_show") %>%
    .[, lines := .N, id_show]

dt_tanp00_raw[, .N, lines]
dt_tanp00_raw[lines > 10] %>% print(n=80)
dt_tanp00_raw %>% tail(n=20)


gd_tanp05_struc(dt_tanp00_raw[id_show < 200], limit = 9999)
gd_tanp05_struc(dt_tanp00_raw[id_show %between% c(200, 400)], limit = 9999)
gd_tanp05_struc(dt_tanp00_raw[id_show > 400], limit = 9999)

dt_tanp00_struc <- fread("/home/johannes/Dropbox/phd/pmdata/data_sources/artnewspaper/tanp_00_struc.csv")

gd_tanp05_asses(dt_tanp00_struc)

gd_proc_exhb(dt_tanp00_struc, "tanp00_")

43.7755362,11.2631732



## ** tanp08

dt_tanp08_raw <- data.table(
    text = readLines("~/Dropbox/phd/pmdata/data_sources/artnewspaper/csv/raw/tanp08_raw.csv"))

l_months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

dt_tanp08_raw[, cnt_months := str_count(text, paste0(l_months, collapse = "|"))] %>%
    .[, cnt_slash := str_count(text, "-")] %>%
    .[, cnt_int := str_count(text, "\\d+")]

dt_tanp08_raw[, .N, .(cnt_months, cnt_slash, cnt_int)]

dt_tanp08_raw[((cnt_months == 2 & cnt_slash == 1 & cnt_int > 0) |
              (cnt_months == 1 & cnt_slash ==1 & cnt_int == 2)) &
              text != "Art Deco, 1910-39",
              id_show := 1:.N] %>%
    setnafill(type = "nocb", cols = "id_show") %>%
    .[, lines := .N, id_show]

dt_tanp08_raw[, .N, lines]
dt_tanp08_raw[lines > 10]
dt_tanp08_raw[lines < 4]

dt_tanp08_raw[id_show %in% c(84,85)]


gd_tanp05_struc(dt_tanp08_raw[id_show < 200], limit = 9999)
gd_tanp05_struc(dt_tanp08_raw[id_show %between% c(200, 400)], limit = 9999)
gd_tanp05_struc(dt_tanp08_raw[id_show %between% c(401, 600)], limit = 9999)
gd_tanp05_struc(dt_tanp08_raw[id_show > 600], limit = 9999)

dt_tanp08_raw[cnt_months == 1]

dt_tanp08_struc <- fread("~/Dropbox/phd/pmdata/data_sources/artnewspaper/csv/struc/tanp_08_struc.csv")

dt_tanp08_struc[!grepl("2007", start_date)] %>% print(n=80)
## date is messy, all kinds of different formats
## don't deal with it, assume 2008


pmdata:::gd_tanp05_asses(dt_tanp08_struc)

gd_proc_exhb(dt_tanp08_struc, "tanp08_")


