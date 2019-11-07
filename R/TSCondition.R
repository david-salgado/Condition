#' Method to stablish conditions for inteval-distance intervals
#'
#' \code{TSCondition} allows for establishing conditions in data sets for applying interval-distance edits.
#'
#' This method checks which units meet the conditions specified in the input \code{Param} to apply
#' the validity control.
#'
#' @param object Object of class \link{StQ} or \link{StQList} with data units and variables to set
#' the conditions.
#'
#' @param Param Object of class \link{TSConditionParam} with params to calculate condition values.
#'
#' @return object of class \code{StQ} with the following columns: the columns needed to identify the units
#' in the input \code{object} and a columns names 'Condition' with values 1L y 0L depending on
#' whether unit satisfies the condition or not.
#'
#' @seealso \code{\link[Condition:TSConditionParam-class]{TSConditionParam}}
#'
#' @examples
#' \dontrun{
#' TSCondition(object, Param)
#' }
#'
#' @export
setGeneric("TSCondition", function(object, Param) {standardGeneric("TSCondition")})
#' @rdname TSCondition
#'
#' @include ConditionParam-class.R TSConditionParam-class.R
#'
#' @import data.table StQ
#'
#' @export
setMethod(
  f = "TSCondition",
  signature = c("StQList", "TSConditionParam"),
  function(object, Param){

    ObjectClass <- class(object)[[1]]
    if (ObjectClass != 'StQList') stop('[Condition:: TSCondition] object must be an object of class StQList')

    ParamClass <- class(Param)
    if (ParamClass != 'TSConditionParam') stop('[Condition:: TSCondition] Param must be an object of class TSConditionParam.')

    EditName <- Param@EditName
    VarName <- Param@VarName
    NPeriods <- Param@NPeriods
    na.imp <- Param@na.imp
    ConditionName <- Param@ConditionName

    DD <- getDD(object)
    DD <- DD[[length(DD)]]

    IDDDConditionName <- UnitToIDDDNames(ConditionName, DD)
    if (is.null(IDDDConditionName)) stop('[Condition:: TSCondition] ConditionName in Param is not in the object data dictionary.')

    IDDDVarName <- UnitToIDDDNames(VarName, DD)
    if (is.null(IDDDVarName)) stop('[Condition:: TSCondition] VarName in Param is not in the object data dictionary.')

    # Extraemos el conjunto de datos
    LastStQ <- getData(object)[[length(getData(object))]]
    Units <- getUnits(LastStQ)

    IDQual <- getIDQual(DD, 'MicroData')
    Periods <- getPeriods(object)
    Periods <- Periods[(length(Periods) - NPeriods + 1):length(Periods)]
    Data <- subPeriods(object, Periods)
    IDDDvars <- ExtractNames(IDDDVarName)
    Data.StQ <- StQListToStQ(Data)
    Data.StQ <- Data.StQ[IDDD == IDDDvars]
    Data.dt <- dcast_StQ(Data.StQ)[, .SD, .SDcols = c(IDQual, 'Period',IDDDVarName)]

    out <- Data.dt[, lapply(.SD, sum), by = IDQual, .SDcols = IDDDVarName]
    out[, (IDDDConditionName) := 0L]
    out <- merge(Units, out, all.x = TRUE, by = IDQual)

    out <- out[as.numeric(get(IDDDVarName)) < .Machine$double.eps, (IDDDConditionName) := 1L]
    out[is.na(get(IDDDConditionName)), (IDDDConditionName) := na.imp]
    out[, (IDDDVarName) := NULL]
    out <- out[, 'IDEdit' := EditName]

    out <- melt_StQ(out, DD)


    return(out)

  }
)
