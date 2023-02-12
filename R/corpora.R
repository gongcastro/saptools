#' Get CHILDES lexical frequencies
#'
#' @param collection CHILDES corpora from where to fetch transcriptions. Takes "Eng-NA" (North American English by default). See \href{https://childes.talkbank.org/access/}{CHILDES Index to corpora} to see options
#' @param age_range Numeric vector of lenght two indicating the minimum and maximum age range of interest for which to comoute lexical frequencies in the CHILDES corpora. Frequencies will be summarised across this age range using the mean
#' @param ... Additional arguments passed to \code{childesr::get_types}
#' @export
get_childes_frequencies <- function(collection = "Eng-NA",
                                    age_range = c(10, 36),
                                    ...) {
  suppressMessages({
    roles <- c(
      "Mother",
      "Father",
      "Investigator",
      "Sibling",
      "Sister",
      "Grandmother",
      "Adult",
      "Friend",
      "Brother",
      "Visitor",
      "Relative",
      "Grandfather",
      "Teacher",
      "Student"
    )

    counts <- get_types(collection = collection, role = roles, ...)

    speaker_ids <- distinct(
      counts,
      collection_id,
      corpus_id,
      transcript_id,
      speaker_id
    )

    speakers <- speaker_ids |>
      left_join(
        get_speaker_statistics(collection = collection),
        by = c(
          "collection_id",
          "corpus_id",
          "speaker_id",
          "transcript_id"
        )
      ) |>
      select(
        collection_id,
        corpus_id,
        transcript_id,
        speaker_id,
        num_tokens
      )

    childes <- counts |>
      left_join(speakers,
        by = c(
          "collection_id",
          "corpus_id",
          "speaker_id",
          "transcript_id"
        )
      ) |>
      mutate(
        id = as.character(id),
        age_months = target_child_age,
        age_bin = as.integer(floor(age_months / 6) * 6),
        token = tolower(gloss)
      ) |>
      group_by(age_bin, token, target_child_id, transcript_id) |>
      summarise(
        transcript_count = sum(count),
        transcript_num_tokens = sum(num_tokens),
        .groups = "drop"
      ) |>
      filter(between(
        age_bin,
        age_range[1],
        age_range[2]
      )) |>
      group_by(token) |>
      summarise(
        freq_count = mean(transcript_count),
        total_count = mean(transcript_num_tokens),
        n_children = length(unique(target_child_id)),
        .groups = "drop"
      ) |>
      mutate(
        freq_million = freq_count / total_count * 1e6,
        freq_zipf = log10(freq_million) + 3
      ) |>
      relocate(
        token,
        n_children,
        freq_count,
        freq_million,
        freq_zipf
      )
  })

  return(childes)
}
