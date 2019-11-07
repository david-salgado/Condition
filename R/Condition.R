#' Method to stablish conditions for inteval-distance intervals
#'
#' \code{Condition} allows for establishing conditions in data sets for applying interval-distance edits.
#'
#' This method checks which units meet the conditions specified in the input \code{Param} to apply
#' the validity control.
#'
#' @param object Object of class \link{StQ} or \link{StQList} with data units and variables to set
#' the conditions.
#'
#' @param Param Object of class \link{ConditionParam} with params to calculate condition values.
#'
#' @return the same input \code{object} of class \link{StQ} including the specified condition. The 
#' units in the output object are those in the \link{StQ} input object or the last \link{StQ} in the
#' \link{StQList} input object.
#'
#' @seealso \code{\link[Condition:FixedConditionParam-class]{FixedConditionParam}}
#' \code{\link[Condition:TSConditionParam-class]{TSConditionParam}} \code{\link[Condition:GenConditionParam-class]{GenConditionParam}}
#'
#' @examples
#' \dontrun{
#' Condition(object, Param)
#' }
#'
#' @export
setGeneric("Condition", function(object, Param) {standardGeneric("Condition")})
#' @rdname Condition
#'
#' @include ConditionParam-class.R GenConditionParam-class.R
#'
#' @import data.table StQ
#'
#' @export
setMethod(
  f = "Condition",
  signature = c("StQ","GenConditionParam"),
  function(object, Param){

    ObjectClass <- class(object)[[1]]
    if (!(ObjectClass %in% c('StQ'))) stop('[Condition:: Condition] object must be an object of class StQ.')

    output <- GenCondition(object, Param)
    return(output)

  }
)
#' @rdname Condition
#'
#' @include ConditionParam-class.R GenConditionParam-class.R
#'
#' @import data.table StQ
#'
#' @export
setMethod(
  f = "Condition",
  signature = c("StQList","GenConditionParam"),
  function(object, Param){

    ObjectClass <- class(object)[[1]]
    if (!(ObjectClass %in% c('StQList'))) stop('[Condition:: Condition] object must be an object of class StQList.')

    output <- object[[length(getData(object))]]
    output <- GenCondition(output, Param)
    #output <- GenCondition(object, Param)
    #output <- output[[length(output)]]

    return(output)

  }
)
#' @rdname Condition
#'
#' @include ConditionParam-class.R FixedConditionParam-class.R
#'
#' @import StQ
#'
#' @export
setMethod(
  f = "Condition",
  signature = c("StQ","FixedConditionParam"),
  function(object, Param){

    ObjectClass <- class(object)[[1]]
    if (!(ObjectClass %in% c('StQ'))) stop('[Condition:: Condition] object must be an object of class StQ.')

    output <- FixedCondition(object, Param)
    return(output)

  }
)
#' @rdname Condition
#'
#' @include ConditionParam-class.R FixedConditionParam-class.R
#'
#' @import StQ
#'
#' @export
setMethod(
  f = "Condition",
  signature = c("StQList","FixedConditionParam"),
  function(object, Param){

    ObjectClass <- class(object)[[1]]
    if (!(ObjectClass %in% c('StQList'))) stop('[Condition:: Condition] object must be an object of class StQList.')

    output <- object[[length(getData(object))]]
    output <- FixedCondition(output, Param)

    return(output)

  }
)
#' @rdname Condition
#'
#' @include ConditionParam-class.R TSConditionParam-class.R
#'
#' @import StQ
#'
#' @export
setMethod(
  f = "Condition",
  signature = c("StQList","TSConditionParam"),
  function(object, Param){

    ObjectClass <- class(object)[[1]]
    if (!(ObjectClass %in% c('StQList'))) stop('[Condition:: Condition] object must be an object of class StQList.')

    output <- TSCondition(object, Param)

    return(output)

  }
)

