#' Retrieve a MP votes on a single roll call vote
#'
#' @description A function for retrieving which MPs voted what on a single vote
#' in the Norwegian parliament.
#'
#' @usage rc_get(voteid = NA, include_voteinfo = FALSE, include_mpinfo = FALSE)
#'
#' @return A data.frame with the following variables:
#'
#'    |                       |                                                                          |
#'    |:----------------------|:-------------------------------------------------------------------------|
#'    | **response_date**     | Date of data retrieval                                                   |
#'    | **version**           | Data version from the API                                                |
#'    | **vote_id**           | Id of vote                                                               |
#'    | **mp_id**             | MP id                                                                    |
#'    | **party_id**          | Party id                                                                 |
#'    | **vote**              | Vote: for, mot (against), ikke_tilstede (absent)                         |
#'    | **permanent_sub_for** | Id of the MP originally holding the seat, if the substitute is permanent |
#'    | **sub_for**           | Id of the MP originally holding the seat                                 |
#'
#' @md
#'
#' @seealso [stortingscrape::get_vote()]
#'
#'
#' @examples
#' \dontrun{
#'
#' v <- rc_get(12345)
#'
#' }
#'
#' @import rvest httr stortingscrape pscl tidyr
#'
#' @export
#'
rc_get <- function(voteid = NA,
                   include_voteinfo = FALSE,
                   include_mpinfo = FALSE){

  raw_result <- get_result_vote(voteid)

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
                 desc = paste0("Vote results for vote '", voteid, "' in Stortinget"),
                 source = "data.stortinget.no")


  return(rc)


}
