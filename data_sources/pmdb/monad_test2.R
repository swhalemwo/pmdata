# * monad test2
library(rmonad)
    
d <- `[`

## ** infix tag, pretty clumsy, replaced by tag2

`%t>%` <- function(lhs, rhs) {
    envir = parent.frame()
    ## id_cmd <- "identity" 
    ## cmd <- list(bind, substitute(lhs), substitute(id_cmd)) %>% as.call
    cmd <- list(bind, substitute(lhs), "identity") %>% as.call
    ## wrap monadify into tag command
    cmd2 <- list(tag, cmd, rhs) %>% as.call
    eval(cmd2, envir = envir)
}
## fstd <- "%t>%"
## mq <- 10 %>^% {. %t>% identijjty %>% tag("jj")}
mq <- 10 %>^% {. %t>% "check1"} %>>% sqrt
mq
get_tag(mq)
get_value(mq, tag = "check1")


mp <- dt_pmdb_excl %>^%
    ## {if(.[,any(ID,is.na)]) {stop("some IDs are NA")} %t>% "IDcheck_na"}
    {.[, .N] %t>% "nrow_start"} %>^%
    {.[is.na(ID), .(name, country)] %t>% "PMs_with_naID"} %>>%
    d(!is.na(ID)) %>^% {.[, .N] %t>% "nrow_IDnonNA"} %>>%
    d(, .N, iso3c)

m1 %>% p
plot(mp, label = \(x) if (class(get_value(x) == "numeric")) {get_value(x)} else {NULL})
plot(mp, label = \(x) if get_value(x))



get_tag(mp) %>% unlist()

get_value(mp)


 %>>%
    d(, .(country, year_opened, ID))

missues(mp)

10 %>^% {. %>>% identity %>% tag("nrow")} %>>% sqrt %>% get_value()


## ** version 2: more complicated branching

## *** no custom funcs

m2 <- adt(iris) %>>% as_monad %>% tag("input") %>>%
    nrow %>% tag("nrow") %>% view("input") %>>%
    d(Species == "setosa") %>>%
    d(Sepal.Length > 5)

view(m2, "nrow")
get_value(m2, tag = "nrow")

plot(m2)




## tag2 <- function(m, expr, tag) {
##     if (as.character(match.call()[[1]]) %in% fstd){browser()}

##     ## get id of head 
##     head_id <- which(igraph::vertex_attr(m@graph)$name == m@head)
##     head_hash <- m@head


##     ## check if head has tag; if not set use hash
##     head_tag <- get_tag(m, index = head_id) %>% unlist
##     if (is.null(head_tag)) {
##         m <- tag(m, head_hash)
##         head_tag <- head_hash
##     }

##     ## evaluate expression
##     cmd <- list(bind, substitute(m), substitute(expr))
##     envir <- parent.frame()
##     m <- eval(as.call(cmd), envir = envir)

##     ## assign tag and revert to old head
##     m <- m %>% tag(tag)
##     m <- m %>% view(head_tag)
##     m
    
## }


tag2(m2, nrow, "final_nrow") %>>% head %>>% tail(3) %>% plot

## works
adt(mtcars) %>>% as_monad %>% tag2(d(, .N, cyl), "dt_agg") %>>% d(cyl == 4) %>>% head %>% plot

## also working
adt(mtcars) %>>% as_monad %>% tag2((\(x) x[, .N, cyl] %>% .[, .N]), "dt_agg") %>>% d(cyl == 4) %>>% head %>% plot


## check multiple branches
adt(mtcars) %>>% as_monad %>% tag2(d(, .N, cyl), "cyl_cnt") %>% tag2(nrow, "nrow") %>>% d(cyl==4) %>>% head %>% plot

## error checking


me <- adt(mtcars) %>>% as_monad %>%
    tag2((\(x) if(nrow(x)>0) {stop("")}), "nrow test") %>>% ## manuall 
    ## tag2((\(x) expect_true(nrow(x) == 0)), "nrow test") %>>% # expect_true
    ## tag2((\(x) test_that("there are rows", nrow(x) > 0)), "nrow test") %>>% ## test_that: requires expect_true
    ## tag2((\(x) head(x) %>>% nrow), "nrow") %>>% try some multi-line test that doesn't work
    d(cyl==4) %>>% head

plot(me)



m <- as_monad(as_tibble(iris)) %>% tag('input') %>>%
    dplyr::filter(Species == "setosa") %>% tag('filter1') %>%
    tag2(nrow, "nrow") %>>%
    dplyr::filter(Sepal.Width > 3) %>% tag("filter2") %>>% head

m %__% view(m, "nrow") %>% funnel() %*>% (\(x, y) adt(x)[, nrow := y]) %>% plot


funnel(m, view(m, "nrow")) %*>% (\(x,y) adt(x)[, nrow := y]) %>% plot(label = 'code')

## test %__% again 
m %__% view(m, "nrow") %*>% (\(x,y) adt(x)[, nrow := y]) %>% plot(label = "code")



plot(m, label = \(x) sprintf("%s: %s", get_id(x), get_tag(x)))
get_value(m, tag = "nrow")

## expect_true


missues(me)
mtabulate(me)





%>>% tag2(nrow, "nrow") %>% plot

 %>>% d(cyl == 4) plot



adt(mtcars) %>% d(, .N, cyl) %>% .[, .N]
   
    
