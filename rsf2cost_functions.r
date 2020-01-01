## tranformation functions
# functions to transform rspf values into cost values. 


negexp_transform = function(x, b) {
  # 1 - (1 - exp(-b * x) / (1 - exp(-b)))
  x * (-b) %>% 
    exp()*-1 %>%
    `+`(1) %>% 
    `/`(1 - exp(-b)) %>% 
    `*`(-1) %>% 
    `+`(1)
}

neglog_transform = function(x, base = exp(1)) {
  x %>% 
    log(base = base)
}

constant_transform = function(x, k = 1) {
  k
}

make_negexp = function(b) {
  function(x) {
    negexp_transform(x, b)
  }
}
