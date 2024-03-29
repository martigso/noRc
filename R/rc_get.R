#' Retrieve a MP votes on a single roll call vote
#'
#' @description A function for retrieving which MPs voted what on a single vote
#' in the Norwegian parliament.
#'
#' @usage rc_get(voteid = NA, include_voteinfo = FALSE, include_mpinfo = FALSE)
#'
#' @param voteid Integer. One or more vote ids to retrieve from the API.
#' @param include_voteinfo Logical. Whether or not to include information on the vote.
#' @param include_mpinfo Logical. Whether or not to include information on the MPs.
#' @param good_manners Numeric. Seconds to wait between calls to the API.
#' Should be used if `voteid` is a vector longer than 1.
#' @param ... Other arguments passed to [rollcall()]
#'
#' @return A rollcall object with the specified votes
#'
#' @seealso [stortingscrape::get_vote()]
#'
#' @examples
#' \dontrun{
#'
#' v <- rc_get(12345)
#' summary(v, verbose = TRUE)
#'
#' }
#'
#' @import rvest httr stortingscrape pscl tidyr fuzzyjoin pbapply
#'
#' @export
#'
rc_get <- function(voteid = NA,
                   include_voteinfo = FALSE,
                   include_mpinfo = FALSE,
                   good_manners = 2){

  # Setup
  message("Starting")
  legisdata <- NULL
  votedata <- NULL
  raw_result <- list()
  message(paste0("...... Getting roll call results for ",
                 length(voteid),
                 " votes."))

  # Getting vote results based on amount of requested votes
  if(length(voteid) > 1){

    # Retrieving result for specified votes
    raw_result <- pblapply(as.character(voteid), function(i){

      tmp <- get_result_vote(i)

      Sys.sleep(good_manners)

      return(tmp)

    })

    # Binding votes together
    raw_result <- do.call(rbind, raw_result)

  } else if(length(voteid) == 1){

    raw_result <- get_result_vote(voteid)

  } else{

    stop("'voteid' needs to be a vector of length 1 or more")

  }

  # Re-coding results to numeric values
  raw_result$vote[which(raw_result$vote == "ikke_tilstede")] <- -1
  raw_result$vote[which(raw_result$vote == "for")] <- 1
  raw_result$vote[which(raw_result$vote == "mot")] <- 0

  # Making sure we do not have duplicates
  raw_result <- unique(raw_result)

  # Making data frame wide and filtering out observations with missing id
  raw_result_wide <- raw_result |>
    dplyr::filter(is.na(mp_id) == FALSE) |>
    tidyr::pivot_wider(id_cols = "mp_id",
                       names_from = "vote_id",
                       names_prefix = "voteid",
                       values_from = "vote") |>
    dplyr::arrange(mp_id) |>
    data.frame()

  # Changing rownames to MP ids
  rownames(raw_result_wide) <- raw_result_wide$mp_id

  # Removing the MP id variable
  raw_result_wide$mp_id <- NULL

  # If vote info is wanted...
  if(include_voteinfo == TRUE){

    message(paste0("...... Getting vote information"))

    # Load parliamentary periods data
    data("parl_periods", package = "noRc")

    # Load vote information data
    data("vote_info", package = "noRc")

    # Subsetting only relevant votes
    votedata <- vote_info[which(vote_info$vote_id %in% voteid), ]

    # Removing redundant variables
    votedata$case_id <- votedata$response_date <- votedata$version <- NULL

    # Making sure there are no duplicates in vote information data
    votedata <- unique(votedata)

    # Converting date to class "date"
    votedata$date <- as.Date(votedata$vote_datetime)

    # Converting parliamentary period start and end to class "date"
    parl_periods$from <- as.Date(parl_periods$from)
    parl_periods$to <- as.Date(parl_periods$to)

    # Fuzzy joining (date interval) vote data and parliamentary period data
    votedata <- fuzzy_left_join(votedata,
                                parl_periods[, c("id", "from", "to")],
                                by = c("vote_datetime" = "from",
                                       "vote_datetime" = "to"),
                                match_fun = list(`>=`, `<=`)) |>
      dplyr::select(!c("from", "to")) |>
      dplyr::rename(period_id = id) |>
      data.frame()

  }

  # If MP meta data is wanted...
  if(include_mpinfo == TRUE){

    message(paste0("...... Getting MP information"))

    # Loading MP data
    data("mp_info", package = "noRc")
    mp_info <- do.call(rbind, mp_info)

    # Subsetting only MPs in the vote results
    legisdata <- mp_info[which(mp_info$mp_id %in%
                                 rownames(raw_result_wide)), ]

    # If vote information data is NULL...
    if(is.null(votedata)){

      # Fix county and party id for MPs
      legisdata <- legisdata |>
        dplyr::group_by(mp_id) |>
        dplyr::summarize(county = paste0(unique(county),
                                         collapse = "|"),
                         party_id = paste0(unique(party_id),
                                           collapse = "|")) |>
        data.frame()
    } else {

      # Else, subset MP data to only MPs from the relevant periods
      legisdata <- legisdata[which(legisdata$parl_period_id %in%
                                     unique(votedata$period_id)), ]

      # Fix county and party ids for MPs
      legisdata <- legisdata |>
        dplyr::group_by(mp_id, parl_period_id) |>
        dplyr::summarize(county = paste0(unique(county), collapse = "|"),
                         party_id = paste0(unique(party_id), collapse = "|"),
                         .groups = "keep") |>
        data.frame()

    }

    # Checking if there is missing information for any MPs
    if(nrow(legisdata) != length(rownames(raw_result_wide))){

      message("............ Found missing MPs -- trying to fix")

      # Identifying missing MPs
      miss_mp_ids <- setdiff(rownames(raw_result_wide), legisdata$mp_id)

      # Retrieving data on missing MPs
      miss_mps <- lapply(miss_mp_ids, function(x){

        # see ?stortingscrape::get_mp_bio
        tmp <- get_mp_bio(x, good_manners)
        tmp <- tmp$parl_periods
        tmp$mp_id <- x

        return(tmp)

      })

      # Binding new MP data
      miss_mps <- do.call(rbind, miss_mps)

      # Making sure the MP is in the data
      miss_mps <- miss_mps[which(miss_mps$parl_period_id %in%
                                   unique(votedata$period_id)), ]

      # Fixing party id and county affiliation for missing MPs
      miss_mps <- miss_mps |>
        dplyr::group_by(mp_id, parl_period_id) |>
        dplyr::summarize(county = paste0(unique(county), collapse = "|"),
                         party_id = paste0(unique(party_id), collapse = "|"),
                         .groups = "keep") |>
        data.frame()

      # Binding all rows of missing MPs and original MP data
      legisdata <-
        dplyr::bind_rows(legisdata, miss_mps) |>
        dplyr::arrange(mp_id)
    }

    # If the MP data is not the same lenght as the vote result data ...
    if(nrow(legisdata) != nrow(raw_result_wide)){

      # Collapse duplicated MPs
      legisdata <- legisdata |>
        dplyr::group_by(mp_id) |>
        dplyr::summarize(
          parl_period_id = paste0(unique(parl_period_id),
                                  collapse = "|"),
          county = paste0(unique(county),
                          collapse = "|"),
          party_id = paste0(unique(party_id),
                            collapse = "|"),
          .groups = "keep") |>
        data.frame()
    }
  }

  # Using pscl::rollcall to construct rollcall data frame
  rc <- rollcall(data = raw_result_wide,
                 yea = 1,
                 nay = 0,
                 notInLegis = -1,
                 missing = NA,
                 legis.names = rownames(raw_result_wide),
                 vote.names = gsub("voteid", "", colnames(raw_result_wide)),
                 legis.data = legisdata,
                 vote.data = votedata,
                 desc = paste0("Vote results for votes in Stortinget"),
                 source = "data.stortinget.no")


  return(rc)


}
