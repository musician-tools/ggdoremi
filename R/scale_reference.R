
return_drm_3octaves <- function(){
  
drm3 <- c("D", "R", "M", "F", "S", "L", "T", 
  "d", "r", "m", "f", "s", "l", "t", 
  "1", "2", "3", "4", "5", "6", "7", "8")
    
factor(drm3, levels = drm3)
    
  
}

return_rel_freq_1octave <- function(){
  
  c(1, 9/8, 81/64,4/3, 3/2, 27/16, 243/128)
  
}

return_rel_freq_3octave <- function(){
  
  multiplier1octave <- return_rel_freq_1octave()
  
  c(multiplier1octave/2, multiplier1octave, multiplier1octave*2, 4)
  
}

return_drm_df <- function(base_freq = 440){  #A above middle C
  
  data.frame(drm = return_drm_3octaves(),
             doremi = c(rep(c("do", "re", "mi", "fa", "sol", "la", "ti"), 3), "do") %>% forcats::fct_inorder(),
             freq = base_freq*return_rel_freq_3octave())
  
}
