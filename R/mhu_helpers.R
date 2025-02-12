bracket_median.old <- function(lower_bounds,weights,quantile=0.5){
  lb <- unique(lower_bounds) |> sort()
  if (length(lb)==0) {
    return(NA)
  }
  if (length(lb)==1&&is.na(lb)) {
    return(NA)
  }
  w <- lb |>
    lapply(\(ll)sum(weights[which(lower_bounds==ll)])) |>
    unlist() |>
    setNames(lb)
  total <- sum(w)
  s <- 0
  old_s <- 0
  index <- 1
  target=total*quantile
  while (s<target & index<=length(w)) {
    old_s <- s
    s <- s + as.numeric(w[index])
    index=index+1
  }
  index<-index-1
  result <- lb[index-1]+(lb[index]-lb[index-1])*(target-old_s)/(s-old_s)
}

bracket_median <- function(lower_bounds,weights,quantile=0.5){
  d<-tibble(l=lower_bounds,w=weights) |> 
    summarize(n=sum(w),.by=l) |>
    arrange(l) |>
    mutate(s=n/sum(n),c=cumsum(s))
  
  index=which(d$c>=quantile)[1]
  if (index==1) {
    x0 <- 0
  } else {
    x0 <- d$c[index-1]
  }
  x1=d$c[index]
  y0 <- d$l[index]
  if (index<nrow(d)) {
    y1 <- d$l[index+1]
  } else {
    y1 <- d$l[index]*1.3
  }
  
  result <- y0+(y1-y0)*(quantile-x0)/(x1-x0)

  result
}

add_income <- function(data){
  data |>
    mutate(CFINC_est=(CFINC_lower+CFINC_upper)/2) |> 
    mutate(TOTINC=coalesce(TOTINC,0)) |>
    mutate(Income=case_when(Couple=="Couple" ~ CFINC_est,
      TRUE ~ TOTINC)) |>
    mutate(Income=coalesce(Income,TOTINC)) |>
    mutate(Income=coalesce(Income,0)) 
}  

get_ate_from_models <- function(fits,variables=NULL,newdata=NULL){
  names(fits) |>
    map_df(\(m) {
      fit <- fits[[m]]
      if (is.null(newdata)) {
        if (is.null(fit$data)) {
          ate <- marginaleffects::avg_comparisons(fit,variables=variables) 
        } else {
          ate <- marginaleffects::avg_comparisons(fit,wts=fit$data$WEIGHT,variables=variables) 
        }
      } else {
        ate <- marginaleffects::avg_comparisons(fit,wts="WEIGHT",newdata=newdata,variables=variables) 
      }
      ate |>
        mutate(model=m)
    }) |>
    as_tibble()
}
