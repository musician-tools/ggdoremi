return_phrases_lyrics_baby_beluga <- function(){
"Ba-by Be-lu-ga in the deep blue sea
Swim so wild and you swim so free,
heav-en ab-o-ove and the sea be-low,
just a lit-tle white whale on the go,
Ba-by Be-lu-ga,
Oh Ba-by Be-lu-ga,
Is the wa-ter warm, 
is your ma-ma home,
with you so hap-py"
}

return_phrases_drm_baby_beluga <- function(){
  
"mmrdSdrmmd mmsffmrr 
ffrTSTrfrT ssssfmrrd 
lfdlf lsmdsm 
rmfrm rmfrm rsfmr"
  
}

parse_phrases_drm <- function(drm_phrases = return_phrases_drm_baby_beluga(), base_freq = 440){
  
  levels <- return_drm_3octaves()
  
  drm_phrases <- strsplit(drm_phrases,  "\\s+")[[1]]
  
  data.frame(sung_notes = drm_phrases) |> 
  tibble::tibble() |> 
  dplyr::mutate(id_phrase = dplyr::row_number()) |> 
  dplyr::mutate(sung_notes_parsed = stringr::str_split(sung_notes, "")) |> 
  tidyr::unnest(cols = c(sung_notes_parsed)) |> 
  dplyr::mutate(sung_notes_parsed = factor(sung_notes_parsed, levels)) |> 
  dplyr::group_by(id_phrase) |> 
  dplyr::mutate(id_in_phrase = dplyr::row_number()) |>
  dplyr::select(id_phrase, id_in_phrase, drm = sung_notes_parsed) |> 
  dplyr::left_join(return_drm_df(base_freq = base_freq))
  
}

parse_phrases_lyrics <- function(lyrics_phrases){
  
    lyrics_phrases <- strsplit(lyrics_phrases,  "\\n")[[1]]
  
  tibble::tibble(words = lyrics_phrases) |> 
  dplyr::mutate(id_phrase = dplyr::row_number()) |> 
  dplyr::mutate(words_parsed = stringr::str_split(words, " |-")) |> 
  tidyr::unnest(cols = c(words_parsed)) |> 
  dplyr::group_by(id_phrase) |> 
  dplyr::mutate(id_in_phrase = dplyr::row_number()) |>
  dplyr::select(id_phrase, id_in_phrase, lyric = words_parsed)
  
}
