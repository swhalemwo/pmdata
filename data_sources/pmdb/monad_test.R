## * monad testing

## ** rmonad

args <- commandArgs(trailingOnly = T)
options(width = 110)
d <- `[`
## dtx <- adt(iris)
## dtx <- iris
## class(dtx)
## class(dtx) <- c("data.frame", "tbl_df")
## class(dtx) <- c("data.frame")
library(rmonad)
## make core dataset
## m <- as_monad(atb(head(iris))) %>>%
##     dplyr::select(
##                sepal_length = Sepal.Length,
##                sepal_width = Sepal.Width,
##                species = Species
##            ) %>%

adt(iris) %>>% 
    ## `oldClass<-`(c("data.table", "data.frame")) %>>%
    ## `oldClass<-`(c("tbl_df", "tbl", "data.frame")) %>>%
    head(30) %>% tag("header") %>>%
    d(, .(Sepal.Length, Sepal.Width)) %>>%
    head(10)

## %>>%
##    `oldClass<-`(c("tbl", "data.frame"))

## atb(iris) %>% class()
## atb(iris) %>>% 
##     head(30) %>>%
##     select(Sepal.Length, Sepal.Width)


## iris %>>%
##     adt() %>>%
##     d(, .(Sepal.Length, Sepal.Width))

m <- as_monad(head(dtx)) %>>%
    d(Sepal.Length > 7) %>% 
    ## cache value with tag 'iris'
    tag('iris') %>>%
                                        # some downstream stuff
    nrow
m

                                        # Now can pick from the tagged node
m2 <- view(m, 'iris') %>>% {
    qplot(
        x=sepal_length,
        y=sepal_width,
        color=species,
        data=.
    )} %>% f('plot')
                                        # and repeat however many times we like 

m3 <- view(m2, 'iris') %>>% summary %>% f('sum')

mtabulate(m3)

view(m3, "sum") %>% esc
view(m3, "plot") %>% esc

plot(m3)



replace_null <- function(x){  
    x <- purrr::map(x, ~ replace(.x, is.null(.x), NA_character_))
    purrr::map(x, ~ if(is.list(.x)) replace_null(.x) else .x) }

## replace_null(get_tag(m3)) %>% unlist()

get_vvalue <- function(m, key) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    ids <- get_id(m)
    
    keys_flat <- get_tag(m, index = ids) %>% replace_null %>% unlist
    
    
    get_tag(m, tag = "cnt_DEU")
    get_id(m, tag = key)
    
    

    if (key %!in% keys_flat) {stop("key not in monad")}

    ## get_value(m)[[which(keys_flat == key)]]
    get_value(m, index = which(na.omit(keys_flat) == key))[[1]]

    get_value(m, index = 3)
    
}
library(rmonad)

## x <- dt_pmdb_excl2 %>>% .[iso3c == "DEU"]

## x <- 1:5 %>>% sqrt %>>% sqrt %>>% sqrt

## monad pipeline test
## lambdas
x <- dt_pmdb_excl2[, .(iso3c, ID, year_opened)] %>>% (\(x)
    if (T){message(min(x$year_opened, na.rm = T)) ;x}) %>>% (\(x)
        x[!is.na(iso3c), N := .N, iso3c] %>% .[N >= 10] %>% .[order(-N)])

## curly brackets
dt_pmdb_excl2[, .(iso3c, ID, year_opened)] %>>% 
    {.[, .N, iso3c]} %>>% {
        message(nrow(.)); .} %>>% {
            .[N >= 10]} %>%
    missues

## assigning data.table::[ to d()
d <- `[`


## try self joins: doesn't work super well, needs curly brackets or lambda
dt_pmdb_excl2[, .(iso3c, ID, year_opened)] %>>% {.} %>>%
    {.[., on = "year_opened", allow.cartesian = T]} %>>% 
    d(!is.na(iso3c), .N, iso3c)
## d(esc(.), on= "year_opened", allow.cartesian = T)
## {.[., on= "year_opened", allow.cartesian = T]}
## (\(x) x[x,  on= "year_opened", allow.cartesian = T])


## try cross-join: 
dt_pmdb_excl2[, .(iso3c, ID, year_opened)] %>%
    .[CJ(year_opened), .SD, on = "year_opened", allow.cartesian = T] %>% na.omit()
## don't think CJs work well on pair-wise data checks tho: doesn't preserve match characteristics


## dt_pmdb_excl2[, .(iso3c, ID, year_opened)] %>%
##     .[., on = "year_opened", allow.cartesian = T] %>% na.omit()
## dt_pmdb_excl2[, .(iso3c, ID, year_opened)] %>%
##     .[CJ(year_opened), on = .(year_opened), allow.cartesian = T]

f <- make_recacher(memory_cache)

## try tagging some arbitrary information based on pipeline
m <- dt_pmdb_excl2 %>>% d(, .(iso3c, ID, year_opened)) %>% tag("input") %>>%
    d(, .N, iso3c) %>% tag("agg_cry") %>^% # branching into separate pipeline
    ## tag(d(, .N), "nbr_crys") %v>% # tag("nbr_crys") %>>%
    {.[, .N]} %>% tag("nbr_crys") %>>% 
    d(N >= 10, .SD[order(-N)]) %>% tag("filter")
m




view(m, "agg_cry")
view(m, "nbr_crys")
view(m, "filter")
get_tag(m)
plot(m)

m %>% mtabulate
m %>% missues

## try tagging with cachemaker

                                        # f <- make_recacher(memory_cache)

m3 <- as_monad(dt_pmdb_excl2) %>% tag("input") %>>%
    d(, .(iso3c, ID, year_opened)) %>>% # %>% tag("col_selection") %>>%
    d(, .N, iso3c) %>% tag("agg") %>^%
    ## {.[, .N] %>>% (\(x) x+1)} %>% tag("nbr_crys") %>^% # try branches with multiple items
    d(iso3c == "DEU", N) %>% tag("cnt_DEU") %>^%
    d(, .N) %>% tag("nbr_crys") %>>% # multiple 1-item branches working
    d(N > 10) %>% tag("filter1") %>>%
    d(N > 20) %>% tag("filter2") 
## funnel(view(., "filter1")) %*>% rbind # when piping, previous item gets implicitly passed as first item
## (\(x) funnel(view(x, "filter1"), view(x, "filter2")) %*>% rbind) # alternative funnel test
m3
plot(m3, label = "code")

view(m3, "nbr_crys")
view(m3, "filtered")

plot(m3, label = "code")
mtabulate(m3)
missues(m3)
get_value(m3, index = 2)[[1]]
get_vvalue(m3, "cnt_DEU")
get_vvalue(m3, "nbr_crys")
get_vvalue(m3, "filter2")

get_id(m3) 
get_tag(m3) %>% replace_null %>% unlist()

## funnelling together different stages
m4 <- funnel(view(m3, "filter1"), view(m3, "filter2")) %*>%
    ## rbind %>% plot # most basic rbind
    (\(x,y) x[!y, on = "iso3c"]) # more substantial stuff: use some to subset

funnel(view(m3, "filter1"), view(m3, "filter2")) %*>% rbind


get_tag(m4)
plot(m4, label = "code")

get_vvalue(m3, "nbr_crys")




## try to get MWE
library(dplyr)
library(rmonad)

m <- as_monad(as_tibble(iris)) %>% tag('input')%>>%
    dplyr::filter(Species == "setosa") %>% tag('filter1') %>^%
    nrow %>% tag("nrow") %>>%
    dplyr::filter(Sepal.Width > 3) %>% tag("filter2")

get_tag(m)
view(m, "nrow")

m2 <- as_monad(as_tibble(iris)) %>% tag('input')%>>%
    dplyr::filter(Species == "setosa") %>% tag('filter1') %>^%
    nrow %>% tag("nrow") %>^%
    ncol %>% tag("ncol") %>>%
    dplyr::filter(Sepal.Width > 3) %>% tag("filter2")

get_tag(m2)

detach("package:rmonad", unload = T)
library(rmonad)

mx <- 10 %>^% {. %t>% identity %>% tag("jj")} %>>% sqrt
get_value(mx)
get_tag(mx)

ttag <- function(x, tagx) {
  ## if (as.character(match.call()[[1]]) %in% fstd){browser()}
  ## print(parent.frame())
  ## eval.parent(substitute(x %>>% identity %>% tag(tagx)))
  print("start")
  cmd_str <- "10 %>>% identity %>% tag('xx')"
  print(cmd_str)
  ## eval.parent(parse(text = cmd_str), n = n)
  eval(substitute(quote(parse(text = cmd_str))))
  ## print("end")

  ## eval.parent(as_monad(x) %>% tag(tagx), n = 1)
  ## eval(as_monad(x) %>% tag(tagx))
  
  ## invisible(x)
  ## invisible(tag(x, tagx))
  
}

mj <- 10 %>% ttag("jj") %v>% sqrt 
mj

view(mj, "jj")
get_tag(mj)
get_value(mj)

mj <- 10 %>^% {. %>>% identity %>% tag('jj')} %>>% sqrt
mj <- 10 %>^% {`%>>%`(., identity) %>% tag('jj')} %>>% sqrt # more passing
mj <- 10 %>^% {tag(`%>>%`(., identity), "jj")} %>>% sqrt

tagb <- function(input, tag) {
  ## tag(`%>>%`(input, identity), tag)
  envir <- parent.frame(1)
  print(environmentName(envir))
  eval(tag(`%>>%`(substitute(input, envir), substitute(identity, envir)), tag), envir = envir)

  ## cmd <- "tag(`%>>%`(11, identity), 'jj')"
  ## eval(parse(text = cmd), envir = envir)
}
10 %>^% {tagb(., "jj")} %>>% sqrt


`%tb2%` <- function(lhs, rhs) {
  envir <- parent.frame()
  cmd <- list(bind, substitute(lhs), substitute(rhs))
  eval(as.call(cmd), envir = envir)
}

10 %>^% {. %tb2% identity %>% tag("jj")} %>>% sqrt

## make.codeBuf.hook <- function(expr,loc=NULL) {
##   ## call original make.codeBuf to get the buffer
##   cb <- make.codeBuf.orig(expr,loc)
##   ## some code removed 
##   ## we will wrap original putcode with our own
##   cb$.putcode <- cb$putcode
##   cb$putcode <- function(…)
##   {
##     ## ... see implementation on github
##   }
##   environment(cb$putcode) <- environment(cb$.putcode)
  
##   cb$.putconst <- cb$putconst
##   cb$putconst <- function(x)
##   {
##     ## ... see implementation on github 
##   }
##   environment(cb$putconst) <- environment(cb$.putconst)
  
##   ## wite cb back to make.codeBuf environment so we can access it there
##   assign(“cb”,value = cb,envir = environment(cb$.putcode))
##   cb
## }
















get_tag(mj)
get_value(mj, tag = "jj")




mj <- 10 %>^% {ttag(., "jj")} %>>% sqrt
mj

get_value(mj)
get_tag(mj)
get_id(mj)
view(mj, "jj")
mj2 <- tag(mj, "xx", index = 2)

get_tag(mj2)
get_value(mj2)

mc <- adt(mtcars) %>% as_monad %>% tag("input") %>>% # head %>>%
  ## (\(x) nrow(x)) %>% tag("nrow") %>%
  {if (nrow(.) > 20) {stop("more than 20 rows")} else {message("no_error")}} %>% tag("nrow_check") %>%
  ## (\(x) if (nrow(x) > 20) "error") %>% tag("nrow") %>%
  (\(x) view(x, "input")) %>>%
  d(mpg > 20) %>% tag("nrow") %>>%
  d(cyl == 4) %>% tag('nrow')

get_tag(mc)
get_value(mc, tag = "nrow_check")
get_value(mc, tag = "nrow")
plot(mc)


missues(mc)

    
  



mi <- 10 %>^% {as_monad(.) %>% tag("jj")} %>>% sqrt
get_value(mi)
get_tag(mi)

brrr <- function(x) {
  message(environmentName(environment()))
  x}


ms <- 10 %>^% {. %>>% identity %>% tag(environmentName(environment()))} %>>% sqrt
get_tag(ms)
get_value(ms)
plot(ms, label = "code")


ftag <- function(exp, tagx) {
  eval.parent(exp) %>>% identity %>% tag(tagx)}

mx <- as_monad(as_tibble(iris)) %>% tag('input') %>>%
  dplyr::filter(Species == "setosa") %>% tag('filter1') %>^%
  ## {nrow(.) %>>% (\(x) x+1) %>% tag("nrow")} %>>% works
  ## {nrow(.) %>>% identity %>% tag("nrow")} %>>% ## works
  ## { nrow(.) %>>% identity %>% tag("nrow")} %>>%
  {nrow(.) %>% ttag(tagx = "nrow")} %>>%
  dplyr::filter(Sepal.Width > 3) %>% tag("filter2")
mx

view(mx, "nrow")

as_monad(as_tibble(iris)) %t>% dplyr::filter(Species == "setosa")
as_monad(as_tibble(iris)) %>>% dplyr::filter(Species == "setosa")

as_monad(as_tibble(iris)) %>>% dplyr::filter(Species == "setosa") %>^%
  ## {as_monad(nrow(.)) %>% tag("nrow")} %>>%
  {nrow(.) %>>% identity %>% tag("nrow")} %>>%
  dplyr::filter(Sepal.Length > 5)



extract_metadata
  


as_monad(as_tibble(iris)) %t>% dplyr::filter(Species == "setosa")



mk <- as_monad(as_tibble(iris)) %>% tag('input') %>>%
    dplyr::filter(Species == "setosa") %tt>%
    dplyr::filter(Sepal.Width > 3) %>^%
    {nrow(.) %>>% identity} %>>%
    dplyr::filter(Sepal.Length > 5)
mk


get_tag(m)
view(mx, "nrow")
plot(mx, label = "code")
get_value(mx, tag = "nrow")
get_vvalue


m4
plot(m, label = "code")

get_value(m, tag = "nrow")
view(m, "nrow")
view(m4, "filter3")
get_id(m)
get_tag(m)

get_tag(m4) %>% replace_null %>% unlist()
get_tag(m4, index = get_id(m4)) %>% replace_null %>% unlist
get_value(m4)
get_vvalue(m4, "nbr")






x <- 1:10 %>^% dgamma(10, 1) %>^% dgamma(10, 5) %^>% cor
plot(x)


## try with effect handling (%>_%)
m2 <- dt_pmdb_excl2 %>>% d(, .(iso3c, ID, year_opened)) %>% tag("input") %>>%
    d(, .N, iso3c) %>_%
    d(, .N) %>% tag("nbr_crys") %>>%
    d(N >10)
view(m2, "nbr_crys")
plot(m2)


## concatenating

m2 <- as_monad(runif(100)) %>% tag("runif_input") %>>% sum %>% tag("runif") %__%
    rexp(100) %>>% sum %>% tag("rexp") %__%
    rnorm(20) %>>% sum %>% tag("rnorm") %__%
    rnorm(10) %>>% sum %>% tag("rnorm2")

m2 %>% esc
plot(m2)
mtabulate(m2)

program <- {
    x = 2
    y = 5
    x * y
} %__% {
    letters %>% sqrt
} %__% {
    10 * x
}



## dt_pmdb_excl2 %>>% d(, .(iso3c, ID, year_opened))
mx <- dt_pmdb_excl2 %>>% d()
atb(mtcars) %>>% {.}

print(mx, verbose = T)
print.Rmonad


atb(mtcars) %>>% select(mpg, cyl)
adt(mtcars) %>>% d(, .(mpg, cyl))

.single_value

print.data.table
data.table::as.data.frame.data.table

print.Rmonad



## if (dt_pmdb_
##  dt_pmdb_excl2[is.na(ID)]

## ** chronicler

install.packages("chronicler")
library(chronicler)

r_na.omit <- record(na.omit)
r_head <- record(head)
d <- record(`[`)
data.table.

mtcars %>% adt() %>% data.table(i = NULL, j=.N)


dtx[, .(ID, country, year_opened)] %>%
    r_na.omit() %>=%
    r_head() %>=%
    d(, .N)



    

    
