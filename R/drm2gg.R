aes_drm <- function(){
  
    ggplot2::aes(x = id_in_phrase, y = as.numeric(drm))
  
}


scale_xy_drm <- function(y_limits = c(4, 16)){
  
  list(ggplot2::scale_y_continuous(limits = y_limits),
  ggplot2::scale_x_continuous(
    expand = ggplot2::expansion(mult = c(.15, .15))))
                                
}

facet_drm <- function(ncol = 2){
  
  facet_wrap(~ id_phrase, ncol = ncol, scales = "free_x")
  
}


stamp_drm_staff <- function(color = "grey"){
  
    geom_hline(yintercept = c(8,10,12,14,16), color = color)
  
}

geom_note_link <- function(linetype = "dashed", color = "gray"){
  
  geom_line(linetype = linetype, color = color)
  
}


geom_note <- function(alpha = .2, size = 26, shape = 19, ...){
  
  list(ggplot2::geom_point(alpha = 1, size = size, shape = shape, fill = "white", color = "white", ...),
  ggplot2::geom_point(alpha = alpha, size = size, shape = shape, ...)
  )
  
}

geom_lyric <- function(size = 8){
  
  ggplot2::geom_text(aes(label = lyric), size = size)
  
}

geom_drm <- function(size = 8){
  
  ggplot2::geom_text(aes(label = drm), size = size)
  
}


geom_doremi <- function(size = 8){
  
  ggplot2::geom_text(aes(label = doremi), size = size)
  
}




theme_drm <- function(){
  
  list(
    ggplot2::theme_void(),
    ggplot2::theme(legend.position = "none"),
  ggplot2::theme(panel.grid = ggplot2::element_blank()),
  ggplot2::theme(panel.spacing.x =  
          unit(0, "lines")),
  ggplot2::theme(panel.spacing.y =  
          unit(.20, "lines")),
  ggplot2::theme(strip.text = element_blank()))
    
  
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

