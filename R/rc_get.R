#' Retrieve a MP votes on a single roll call vote
#'
#' @description A function for retrieving which MPs voted what on a single vote
#' in the Norwegian parliament.
#'
#' @usage rc_get(voteid = NA, include_voteinfo = FALSE, include_mpinfo = FALSE)
#'
#' @param voteid Integer. One or more vote ids to retreive from the API.
#' @param include_voteinfo Logical. Whether or not to include information on the vote.
#' @param include_mpinfo Logical. Wheteher or not to include information on the MPs.
#' @param good_manners Numeric. Seconds to wait between calls to the API.
#' Should be used if `voteid` is a vector longer than 1.
#' @param ... Other arguments passed to `rollcall()`
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
#' @import rvest httr stortingscrape pscl tidyr fuzzyjoin
#'
#' @export
#'
rc_get <- function(voteid = NA,
                   include_voteinfo = FALSE,
                   include_mpinfo = FALSE,
                   good_manners = 2){

  message("Starting")

  raw_result <- list()
  message(paste0("...... Getting roll call results for ", length(voteid),
                 " votes."))


  if(length(voteid) > 1){

    for(i in as.character(voteid)){
      raw_result[[i]] <- get_result_vote(i)

      Sys.sleep(good_manners)
    }

    raw_result <- do.call(rbind, raw_result)
  } else if(length(voteid) == 1){
    raw_result <- get_result_vote(voteid)
  } else{
    stop("'voteid' needs to be a vector of length 1 or more")
  }


  raw_result$vote[which(raw_result$vote == "ikke_tilstede")] <- -1
  raw_result$vote[which(raw_result$vote == "for")] <- 1
  raw_result$vote[which(raw_result$vote == "mot")] <- 0

  raw_result_wide <- raw_result %>%
    dplyr::filter(is.na(mp_id) == FALSE) %>%
    pivot_wider(., id_cols = "mp_id",
                names_from = "vote_id",
                names_prefix = "voteid",
                values_from = "vote") %>%
    dplyr::arrange(mp_id) %>%
    data.frame()

  rownames(raw_result_wide) <- raw_result_wide$mp_id
  raw_result_wide$mp_id <- NULL


  if(include_voteinfo == TRUE){

    message(paste0("...... Getting vote information"))

    data("parl_periods", package = "noRc")
    data("vote_info", package = "noRc")

    votedata <- vote_info[which(vote_info$vote_id %in% voteid), ]

    votedata$case_id <- votedata$response_date <- votedata$version <- NULL

    votedata <- unique(votedata)
    votedata$date <- as.Date(votedata$vote_datetime)

    parl_periods$from <- as.Date(parl_periods$from)
    parl_periods$to <- as.Date(parl_periods$to)

    votedata <- fuzzy_left_join(votedata, parl_periods[, c("id", "from", "to")],
                             by = c("vote_datetime" = "from",
                                    "vote_datetime" = "to"),
                             match_fun = list(`>=`, `<=`)) %>%
      dplyr::select(!c("from", "to")) %>%
      dplyr::rename(period_id = id) %>%
      data.frame()

  }

  if(include_mpinfo == TRUE){

    message(paste0("...... Getting MP information"))

    data("mp_info")
    mp_info <- do.call(rbind, mp_info)

    legisdata <- mp_info[which(mp_info$mp_id %in%
                                 rownames(raw_result_wide)), ]
    if(is.null(votedata)){
      legisdata <- legisdata %>%
        dplyr::group_by(mp_id) %>%
        dplyr::summarize(county = paste0(unique(county), collapse = "|"),
                         party_id = paste0(unique(party_id), collapse = "|")) %>%
        data.frame()
    } else {

      legisdata <- legisdata[which(legisdata$parl_period_id %in% unique(votedata$period_id)), ]

      legisdata <- legisdata %>%
        dplyr::group_by(mp_id, parl_period_id) %>%
        dplyr::summarize(county = paste0(unique(county), collapse = "|"),
                         party_id = paste0(unique(party_id), collapse = "|"),
                         .groups = "keep") %>%
        data.frame()

    }

    if(nrow(legisdata) != length(rownames(raw_result_wide))){
      message("............ Found missing MPs -- trying to fix")

      miss_mp_ids <- setdiff(rownames(raw_result_wide), legisdata$mp_id)

      miss_mps <- lapply(miss_mp_ids, function(x){

        tmp <- get_mp_bio(x, good_manners)
        tmp <- tmp$parl_periods
        tmp$mp_id <- x

        return(tmp)

      })

      miss_mps <- do.call(rbind, miss_mps)
      miss_mps <- miss_mps[which(miss_mps$parl_period_id %in%
                                   unique(votedata$period_id)), ]

      miss_mps <- miss_mps %>%
        dplyr::group_by(mp_id, parl_period_id) %>%
        dplyr::summarize(county = paste0(unique(county), collapse = "|"),
                         party_id = paste0(unique(party_id), collapse = "|"),
                         .groups = "keep") %>%
        data.frame()

      legisdata <-
        dplyr::bind_rows(legisdata, miss_mps) %>%
        dplyr::arrange(mp_id)
    }


  }


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
