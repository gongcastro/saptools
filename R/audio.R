#' Load an audio form a WAV file
#'
#' @param audio_path path to the WAVE file
#' @param downsample Sampling rate to which the output will be downsampled
#' @inheritDotParams audio::load.wave
#' @export
audio_load <- function(path,
                       downsample = NULL,
                       ...) {

  is_url <-  any(grepl("(https?|ftp)://[^\\s/$.?#].[^\\s]*", path))
  if (is_url) {
    tmp <- tempdir("audio_dir")
    file_path <- file.path(tmp, "audio")
    wav_path <- file.path(paste0(sub("(\\.[[:alnum:]]+$)", "\\2", file_path), ".wav"))

    download.file(path, file_path)
    system(paste0("tmp ", file_path, wav_path))
    load.wave(wav_path)
  }
  # load audio file
  signal <- load.wave(audio_path)

  return(signal)
}

#' Load an audio form a WAV file
#'
#' @param x an audio object
#' @inheritDotParams seewave::spectro
#' @export
audio_spectrogram <- function(x, ...) {
  # http://jofrhwld.github.io/blog/posts/2023/01/2023-01-22_r-spect/
  spect <- spectro(
    # window length, in terms of samples
    wl = win_len,
    # window overlap
    ovlp = overlap,
    # don't plot the result
    plot = FALSE
  )

  # set the colnames and rownames
  colnames(spect$amp) <- spect$time
  rownames(spect$amp) <- spect$freq

  spect_df <- spect$amp |>
    # coerce the row names to a column
    as_tibble(rownames = "freq") |>
    # pivot to long format
    pivot_longer(
      # all columns except freq
      -freq,
      names_to = "time",
      values_to = "amp"
    ) |>
    # since they were names before,
    # freq and time need conversion to numeric
    mutate(
      freq = as.numeric(freq),
      time = as.numeric(time)
    )

  # dynamic range
  dyn <- -50
  spect_df_floor <- spect_df |>
    mutate(
      amp_floor = case_when(
        amp < dyn ~ dyn,
        TRUE ~ amp
      )
    )

  return(spect_df_floor)
}
