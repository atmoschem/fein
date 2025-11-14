#' Emissions factors from European European Environment Agency
#'
#' @param nfr String: nomenclature for reporting
#' @param tier numeric 1 or 2
#' @param fuel String for type of fuel.
#' @param abatement String.
#' @param pol String for pollutant.
#' @param returnfdb logical return directly db
#' @return Return a data.table
#' @importFrom data.table setDT
#' @keywords  emission factors
#' @export
#' @examples {
#' # ef_eea(category = "NAO SEI")
#' }
ef_emep <- function(
  nfr = "1.A.1.a",
  tier,
  fuel,
  abatement,
  pol,
  returnfdb = FALSE
) {
  eea <- data.table::setDT(sysdata$eea)
  #eea <- eea[grepl("1.A.1", NFR)]
  if (returnfdb) {
    return(eea)
  }
  # nfr ####
  if (length(nfr) > 1) {
    stop("One nfr at a time please")
  }

  NFR <- tiers <- Fuel <- Abatement <- Pollutant <- NULL
  nfrs <- eea[grepl("1.A", NFR), unique(NFR)]
  if (!nfr %in% nfrs) {
    stop(cat("only these nfr  allowed: ", nfrs, sep = " "))
  }
  ef <- eea[NFR == nfr]

  # tier ####

  if (length(tier) > 1) {
    stop("One tier at a time please")
  }

  tiersx <- ef[, unique(tiers)]
  if (!tier %in% tiersx) {
    stop(cat("only these tiers  allowed: ", tiersx, sep = " "))
  }
  ef <- ef[tiers == tier]

  # fuel ####

  if (length(fuel) > 1) {
    stop("One fuel at a time please")
  }

  fuels <- ef[, unique(Fuel)]
  if (!fuel %in% fuels) {
    stop(cat("only these fuels  allowed: ", fuels, sep = " "))
  }
  ef <- ef[Fuel == fuel]

  # Abatement ####

  if (missing(abatement)) {
    abatement <- ""
  }

  abas <- ef[, unique(Abatement)]
  if (!abatement %in% abas) {
    stop(cat("only these abatement  allowed: ", abas, sep = " "))
  }
  ef <- ef[Abatement == abatement]

  # pol ####

  pols <- ef[, unique(Pollutant)]
  if (any(!pol %in% pols)) {
    stop(cat("only these Pollutants  allowed: ", pols, sep = " "))
  }
  ef <- ef[Pollutant %in% pol]
  return(ef)
}
