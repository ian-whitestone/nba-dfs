roll_variable_mean = function(d, target, windows) {
  require(dplyr)
  require(lazyeval)
  
  exprl = list()
  i = 1
  
  for (x in windows) {
    exprl[[i]] = interp(~ lag(roll_mean(tar, w, align = 'right', fill = NA), 1), tar=as.name(target), w=x)
    i = i + 1
  }
  names = paste(target, windows, sep="_")
  exprl = setNames(exprl, names)
  
  d = mutate_(d, .dots = exprl)
  
  for (n in 2:length(names)) {
    expr = interp(~ ifelse(is.na(long), short, long), long=as.name(names[n]), short=as.name(names[n-1]))
    exprl = setNames(list(expr), names[n])
    d = mutate_(d, .dots = exprl)
  }
  
  return(d)
}

roll_variable_rate = function(d, target, normalizer, name, windows) {
  require(dplyr)
  require(lazyeval)
  
  exprl = list()
  i = 1
  
  for (x in windows) {
    exprl[[i]] = interp(~ lag(roll_sum(tar, w, align = 'right', fill = NA) / 
                              roll_sum(norm, w, align = 'right', fill = NA)  , 1), 
                        tar=as.name(target), norm=as.name(normalizer), w=x)
    i = i + 1
  }
  names = paste(name, windows, sep="_")
  exprl = setNames(exprl, names)
  
  d = mutate_(d, .dots = exprl)
  
  for (n in 2:length(names)) {
    expr = interp(~ ifelse(is.na(long), short, long), long=as.name(names[n]), short=as.name(names[n-1]))
    exprl = setNames(list(expr), names[n])
    d = mutate_(d, .dots = exprl)
  }
  
  return(d)
}