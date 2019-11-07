setClassUnion('ConditionParam', c('TSConditionParam', 'GenConditionParam', 'FixedConditionParam'))
#' Clase S4 ConditionParam para los parámetros del método Condition
#'
#' Definición de la clase S4 \code{ConditionParam} que define los parámetros que
#' utiliza el método \code{\link{Condition}}.
#'
#' \code{ConditionParam} es la unión de las clases \linkS4class{GenConditionParam},
#' \linkS4class{FixedConditionParam} y \linkS4class{TSConditionParam}.
#'
#' #@include GenConditionParam-class.R FixedConditionParam-class.R TSConditionParam-class.R
#'
#' @rdname ConditionParam-class.R
#'
#' @export
