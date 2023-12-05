corr_mixed_assoc = function(df, cor_method="spearman", adjust_cramersv_bias=TRUE){
  df_comb = base::expand.grid(base::names(df), base::names(df),  stringsAsFactors = F) |> purrr::set_names("X1", "X2")
  
  is_nominal = function(x) class(x) %in% c("factor", "character")
  is_numeric <- function(x) { is.integer(x) || purrr::is_double(x)}
  
  f = function(xName,yName) {
    x =  dplyr::pull(df, xName)
    y =  dplyr::pull(df, yName)
    
    result = if(is_nominal(x) && is_nominal(y)){
      cv = rcompanion::cramerV(as.character(x), as.character(y), bias.correct = adjust_cramersv_bias)
      data.frame(xName, yName, assoc=cv, type="cramersV")
      
    }else if(is_numeric(x) && is_numeric(y)){
      correlation = stats::cor(x, y, method=cor_method, use="complete.obs")
      data.frame(xName, yName, assoc=correlation, type="correlation")
      
    }else if(is_numeric(x) && is_nominal(y)){
      r_squared = summary(lm(x ~ y))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else if(is_nominal(x) && is_numeric(y)){
      r_squared = summary(lm(y ~x))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else {
      warning(paste("unmatched column type combination: ", class(x), class(y)))
    }
    
    # finally add complete obs number and ratio to table
    result|> dplyr::mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) |>  dplyr::rename(x=xName, y=yName)
  }
  
  # apply function to each variable combination
  purrr::map2_df(df_comb$X1, df_comb$X2, f)
}


