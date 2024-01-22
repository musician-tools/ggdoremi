return_rhythm_baby_beluga <- function(){ 
      rhythm <- "12a3a4a12341234a123412a3a4a12a34a1a234a1234"
    measure <- "1111111222233333444455555556666666777778888"    
beat_in_measure  <- "12233441234123441234122334412a3441123441234"
ind_voiced      <- "1111111111011111111011111111011111111111000"
 
data.frame(rhythm, measure, beat_in_measure, ind_voiced)

  }

# time_span <- function(num_measures = 12){
#   
# measure <- c( "1" ,"e", "n", "a", "2", "e",
#  "n" ,"a", "3", "e", "n", "a",
#  "4" ,"e", "n", "a")
#   
# time_frame <- 1:(length(measure)*num_measures)
# 
# data.frame(time_frame,
# time_frame_element = rep(measure, num_measures))
# 
# }


# time_span() |> dplyr::bind_cols(data.frame(rythm=return_rythem_baby_beluga()))

