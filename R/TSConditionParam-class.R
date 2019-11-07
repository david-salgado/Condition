setClassUnion('characterOrNULL', c('character', 'NULL')); setClassUnion('integerOrNULL', c('integer', 'NULL'))
#' Clase S4 TSConditionParam para los parámetros del método Condition
#'
#' Definición de la clase S4 \code{TSConditionParam} que contiene los parámetros
#' que utiliza el método \code{\link{Condition}}.
#'
#' Esta clase define condiciones sobre la serie temporal de las variables que
#' deben verificarse para que el control sea efectivo sobre la unidad.
#'
#' @slot EditName \code{Vector} de tipo \code{integer} de longitud 1 con el nombre
#' del Edit para el que se establece la condición.
#'
#' @slot VarName \code{Vector} de tipo \code{integer} de longitud 1 con el nombre
#' de la variable sobre la que se establece la condición.
#'
#' @slot NPeriods \code{Vector} de tipo \code{integer} de longitud 1 con el número
#' de periodos hacia atrás con los que vamos a calcular la mediana.
#'
#' @slot na.imp \code{vector} de tipo \code{integer} de longitud 1, que indica si
#' los valores NA se imputan a 0L o a 1L.
#'
#' @slot ConditionName \code{Vector} de tipo \code{integer} de longitud 1 con el
#' nombre de la variable generada que contiene la condición.
#'
#' @examples
#' # Un prototipo vacío
#' new(Class = 'TSConditionParam')
#' # Ejemplo no vacío
#' new(Class = 'TSConditionParam',
#'     EditName = 'EMPREMFIJ_W_1',
#'     NPeriods = 3L,
#'     VarName = 'c121',
#'     ConditionName = 'Condicion')
#' @export
setClass(Class = 'TSConditionParam',
         slots = c(EditName = 'characterOrNULL',
                  na.imp = 'integer',
                  ConditionName = 'character',
                  VarName = 'character',
                  NPeriods = 'integer'),
                  prototype = list(EditName = character(0),
                                   na.imp  = 0L,
                                   ConditionName = 'Condicion',
                                   VarName = character(0),
                                   NPeriods = 6L),
                  validity = function(object){
                      msg <- NULL
                      if (!is.null(object@VarName) & length(object@VarName) != 1)
                      msg <- c(msg, "[Condition:: validity TSConditionParam] 'VarName' must be a character vector of length 1.")

                      if ((length(object@NPeriods) != 1 || object@NPeriods <= 0L))
                      msg <- c(msg, "[Condition:: validity TSConditionParam] 'NPeriods' must be an integer vector of length 1 with positive value.")

                      if (is.null(msg)) TRUE else msg
                  }
            )



