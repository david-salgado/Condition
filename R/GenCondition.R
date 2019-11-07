#' Method to stablish conditions for inteval-distance intervals
#'
#' \code{GenCondition} allows for establishing conditions in data sets for applying interval-distance edits.
#'
#' This method checks which units meet the conditions specified in the input \code{Param} to apply
#' the validity control.
#'
#' @param object Object of class \link{StQ} or \link{StQList} with data units and variables to set
#' the conditions.
#'
#' @param Param Object of class \link{ConditionParam} with params to calculate condition values.
#'
#' @return the same input \code{object} of class \link{StQ} or \link{StQList} including the specified
#' condition.
#'
#' @seealso \code{\link[Condition:GenConditionParam-class]{GenConditionParam}}
#'
#' @examples
#' \dontrun{
#' GenCondition(object, Param)
#' }
#'
#' @export
setGeneric("GenCondition", function(object, Param) {standardGeneric("GenCondition")})
#' @rdname GenCondition
#'
#' @include ConditionParam-class.R GenConditionParam-class.R
#'
#' @import data.table StQ
#'
#' @export
setMethod(
  f = "GenCondition",
  signature = c("StQ", "GenConditionParam"),
  function(object, Param){

    ObjectClass <- class(object)[[1]]
    if (ObjectClass != 'StQ') stop('[Condition:: GenCondition] object must be an object of class StQ.')

    ParamClass <- class(Param)
    if (ParamClass != 'GenConditionParam') stop('[Condition:: GenCondition] Param must be an object of class GenConditionParam.')

    DD <- getDD(object)
    ConditionName <- Param@ConditionName
    IDDDConditionName <- UnitToIDDDNames(ConditionName, DD)
    if (is.null(IDDDConditionName)) stop('[Condition:: GenCondition] ConditionName in Param is not in the object data dictionary.')

    EditName <- copy(Param)@EditName
    ConditionExp <- copy(Param)@ConditionExp
    by <- copy(Param)@by
    keyVar <- copy(Param)@keyVar
    lag <- copy(Param)@lag
    na.imp <- copy(Param)@na.imp

    Units <- getUnits(object)

    Data <- StQ::dcast_StQ(object)
    setnames(Data, names(Data), IDDDToUnitNames(names(Data), DD))

    if (is.null(by)) {

      out <- Data[, (ConditionName) := ifelse(eval(parse(text = ConditionExp)), 1, 0)]


    }else {

      out <- Data[, (ConditionName) := ifelse(eval(parse(text = ConditionExp)), 1, 0), by = by]

    }

    IDQual <- getIDQual(DD, 'MicroData')

    setnames(out, names(out), UnitToIDDDNames(names(out), DD))
    out <- merge(Units, out, all.x = TRUE, by = IDQual)

    out[is.na(get(IDDDConditionName)), (IDDDConditionName) := na.imp]

    out <- out[,c(IDQual, IDDDConditionName), with = FALSE]
    out[, 'IDEdit' := EditName]


    out <- melt_StQ(out, DD)
    #out <- object + out

    return(out)

  }
)
#' @rdname GenCondition
#'
#' @include ConditionParam-class.R GenConditionParam-class.R
#'
#' @import data.table StQ
#'
#' @export
setMethod(
  f = "GenCondition",
  signature = c("StQList", "GenConditionParam"),
  function(object, Param){

    ObjectClass <- class(object)[[1]]
    if (ObjectClass != 'StQList') stop('[Condition:: GenCondition] object must be an object of class StQList.')

    ParamClass <- class(Param)
    if (ParamClass != 'GenConditionParam') stop('[Condition:: GenCondition] Param must be an object of class GenConditionParam.')

    StQ.list <- getData(object)
    out <- lapply(StQ.list, GenCondition, Param)
    names(out) <- names(StQ.list)
    #!!!!Ver si la salida es un StQList!!!!!!
    return(out)

  }
)

