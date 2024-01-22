aes_drm <- function(){
  
    ggplot2::aes(x = id_in_phrase, y = as.numeric(drm))
  
}


scale_xy_drm <- function(){
  
  list(ggplot2::scale_y_continuous(limits = c(4, 16)),
  ggplot2::scale_x_continuous(
    expand = ggplot2::expansion(mult = c(.1, .1))))
                                
}

facet_drm <- function(){
  
  facet_wrap(~ id_phrase, ncol = 2, scales = "free_x")
  
}


stamp_drm_staff <- function(){
  
    geom_hline(yintercept = c(8,10,12,14,16), color = "grey")
  
}

geom_note_link <- function(){
  
  geom_line(linetype = "dashed", color = "gray")
  
}


geom_note <- function(alpha = .2, size = 26, shape = 19, ...){
  
  ggplot2::geom_point(alpha = alpha, size = size, shape = shape, ...)
  
}

geom_lyric <- function(){
  
  ggplot2::geom_text(aes(label = lyric), size = 8)
  
  
}

geom_drm <- function(){
  
  ggplot2::geom_text(aes(label = drm), size = 8)
  
  
}


theme_drm <- function(){
  
  list(ggplot2::theme(legend.position = "none"),
  ggplot2::theme_minimal() ,
  ggplot2::theme(panel.grid = ggplot2::element_blank()))
  
}


drm2gg <- function(drm_df){
  
  drm_df |> 
  ggplot2::ggplot() + 
  aes_drm() + 
  stamp_drm_staff() +
  geom_note_link() +
  geom_note() + 
  geom_lyric() + 
  facet_drm() + 
  scale_xy_drm() + 
  theme_drm() 
  
}

