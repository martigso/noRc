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
#' @import rvest httr stortingscrape pscl tidyr
#'
#' @export
#'
rc_get <- function(voteid = NA,
                   include_voteinfo = FALSE,
                   include_mpinfo = FALSE,
                   good_manners = 2){

  raw_result <- list()
  for(i in as.character(voteid)){
    raw_result[[i]] <- get_result_vote(i)

    Sys.sleep(good_manners)
  }

  raw_result <- do.call(rbind, raw_result)


  raw_result$vote[which(raw_result$vote == "ikke_tilstede")] <- -1
  raw_result$vote[which(raw_result$vote == "for")] <- 1
  raw_result$vote[which(raw_result$vote == "mot")] <- 0

  raw_result_wide <- raw_result %>%
    pivot_wider(., id_cols = "mp_id",
                names_from = "vote_id",
                names_prefix = "voteid",
                values_from = "vote") %>%
    data.frame()

  rownames(raw_result_wide) <- raw_result_wide$mp_id
  raw_result_wide$mp_id <- NULL


  if(include_voteinfo == TRUE){

  } else {
    votedata <- NULL
  }

  if(include_mpinfo == TRUE){

  } else {
    legisdata <- NULL
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
