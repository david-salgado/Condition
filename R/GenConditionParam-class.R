setClassUnion('characterOrNULL', c('character', 'NULL')); setClassUnion('integerOrNULL', c('integer', 'NULL'))
#' Clase S4 GenConditionParam para los parámetros del método Condition
#'
#' Definición de la clase S4 \code{GenConditionParam} que contiene los parámetros
#' que utiliza el método \code{\link{Condition}}.
#'
#' Esta clase define condiciones generales sobre las variables que deben verificarse
#' para que el control sea efectivo sobre la unidad.
#'
#' @slot EditName \code{Vector} de tipo \code{integer} de longitud 1 con el nombre
#' del Edit para el que se establece la condición.
#'
#' @slot ConditionExp \code{vector} de tipo \code{character} con la expresión que
#' define la condición que se debe cumplir para que el control de validación
#' tenga efecto.
#'
#' @slot by \code{NULL} o \code{vector} de tipo \code{character} con los nombres
#' de las variables por las que queremos agrupar. Por defecto toma el valor
#' \code{NULL}.
#'
#' @slot keyVar \code{NULL} o \code{vector} de tipo \code{character} con los nombres
#' de las variables que se utilizan como clave. Por defecto toma el valor
#' \code{NULL}.
#'
#' @slot lag \code{NULL} o \code{vector} de tipo \code{integer} de longitud 1 con
#' el valor del retardo. Por defecto toma el valor \code{2L}.
#'
#' @slot na.imp \code{vector} de tipo \code{integer} de longitud 1, que indica si
#' los valores NA se imputan a 0L o a 1L.
#'
#' @slot ConditionName \code{Vector} de tipo \code{integer} de longitud 1 con el
#' nombre de la variable generada que contiene la condición.
#'
#' @examples
#' # Un prototipo vacío
#' new(Class = 'GenConditionParam')
#' # Ejemplo no vacío
#' new(Class = 'GenConditionParam',
#'     EditName = 'LCN_W_1',
#'     ConditionExp = 'is.na(ccaa)',
#'     keyVar = 'NOrden',
#'     ConditionName = 'Condicion')
#' @export
setClass(Class = 'GenConditionParam',
         slots = c(EditName = 'characterOrNULL',
                   na.imp = 'integerOrNULL',
                   ConditionName = 'characterOrNULL',
                   ConditionExp = "characterOrNULL",
                   by = "characterOrNULL",
                   lag = "integerOrNULL",
                   keyVar = "characterOrNULL"),
         prototype = list(EditName = character(0),
                          na.imp  = 0L,
                          ConditionName = 'Condicion',
                          ConditionExp = NULL,
                          by = NULL,
                          lag = NULL,
                          keyVar = NULL),
         validity =
         function(object){
            msg <- NULL
            if (!is.null(object@lag) & (length(object@lag) != 1 || object@lag <= 0L))
                   msg <- c(msg, "[Condition:: validity GenConditionParam] lag must be an integer vector of length 1 with positive value.")

            if (is.null(msg)) TRUE else msg
         }
         )





