#' Method to stablish conditions for inteval-distance intervals
#'
#' \code{FixedCondition} allows for establishing conditions in data sets for applying interval-distance edits.
#'
#' This method checks which units meet the conditions specified in the input \code{Param} to apply
#' the validity control.
#'
#' @param object Object of class \link{StQ} or \link{StQList} with data units and variables to set
#' the conditions.
#'
#' @param Param Object of class \link{ConditionParam} with params to calculate condition values.
#' Depending on the type of condition.
#'
#' @return the same input \code{object} of class \link{StQ} or \link{StQList} including the specified
#' condition.
#'
#' @seealso \code{\link[Condition:FixedConditionParam-class]{FixedConditionParam}}
#'
#' @examples
#' \dontrun{
#' FixedCondition(object, Param)
#' }
#'
#' @export
setGeneric("FixedCondition", function(object, Param) {standardGeneric("FixedCondition")})
#' @rdname FixedCondition
#'
#' @include ConditionParam-class.R FixedConditionParam-class.R
#'
#' @import data.table StQ
#'
#' @export
setMethod(
  f = "FixedCondition",
  signature = c("StQ", "FixedConditionParam"),
  function(object, Param){

    ObjectClass <- class(object)[[1]]
    if (ObjectClass != 'StQ') stop('[Condition:: FixedCondition] object must be an object of class StQ.')

    ParamClass <- class(Param)
    if (ParamClass != 'FixedConditionParam') stop('[Condition:: FixedCondition] Param must be an object of class FixedConditionParam.')

    ConditionName <- Param@ConditionName
    DD <- getDD(object)
    IDDDConditionName <- UnitToIDDDNames(ConditionName, DD)
    if (is.null(IDDDConditionName)) stop('[Condition:: FixedCondition] ConditionName in Param is not in the object data dictionary.')

    Units <- getUnits(object)
    out <- Units[, (ConditionName):= Param@Value]
    out <- Units[, 'IDEdit' := Param@EditName]
    out <- melt_StQ(out, DD)
    #out <- object + out

    return(out)

  }
)
#' @rdname FixedCondition
#'
#' @include ConditionParam-class.R FixedConditionParam-class.R
#'
#' @import data.table StQ
#'
#' @export
setMethod(
  f = "FixedCondition",
  signature = c("StQList", "FixedConditionParam"),
  function(object, Param){

    ObjectClass <- class(object)[[1]]
    if (ObjectClass[[1]] != 'StQList') stop('[Condition:: FixedCondition] object must be an object of class StQList.')

    ParamClass <- class(Param)
    if (ParamClass != 'FixedConditionParam') stop('[Condition:: FixedCondition] Param must be an object of class FixedConditionParam.')


    StQ.list <- getData(object)
    out <- lapply(StQ.list, FixedCondition, Param)
    names(out) <- names(StQ.list)

    return(out)

  }
)

