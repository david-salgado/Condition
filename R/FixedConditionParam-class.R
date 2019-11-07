setClassUnion('characterOrNULL', c('character', 'NULL')); setClassUnion('integerOrNULL', c('integer', 'NULL'))
#' Clase S4 FixedConditionParam para los parámetros del método Condition
#'
#' Definición de la clase S4 \code{FixedConditionParam} que contiene los parámetros
#' que utiliza el método \code{\link{Condition}}.
#'
#' Esta clase define una condición fija que determina si el contro es o no efectivo
#' sobre el conjunto de unidades.
#'
#' @slot EditName \code{Vector} de tipo \code{integer} de longitud 1 con el nombre
#' del Edit para el que se establece la condición.
#'
#' @slot Value \code{vector} de tipo \code{integer} que contiene el valor fijo que
#' se asigna a la condicion. Toma los valores 1L si se debe aplicar el control de
#' validación a todas las unidades o 0L si no se debe aplicar a ninguna unidad.
#'
#' @slot na.imp \code{vector} de tipo \code{integer} de longitud 1, que indica a
#' que valor (0L o 1L) se imputan los valores NA.
#'
#' @slot ConditionName \code{Vector} de tipo \code{integer} de longitud 1 con el
#' nombre asignado a la variable que contiene la condición. Por defecto, su valor
#' es "Condicion". En cualquier caso, debe corresponder a una de las variables
#' incluidas en la componente DD (diccionario de datos) del objeto de clase StQ o
#' StQList de entrada.
#'
#' @examples
#' \dontrun{
#' # Un prototipo vacío
#' new(Class = 'FixedConditionParam')
#' # Ejemplo no vacío
#' new(Class = 'FixedConditionParam',
#'     EditName = 'LCN_W_1',
#'     Value = 1L,
#'     na.imp = 0L,
#'     ConditionName = 'Condicion')
#'     }
#'
#' @export
setClass(Class = 'FixedConditionParam',
         slots = c(EditName = 'characterOrNULL',
                   na.imp = 'integerOrNULL',
                   ConditionName = 'characterOrNULL',
                   Value = "integerOrNULL"),
         prototype = list(EditName = character(0),
                          na.imp  = 0L,
                          ConditionName = 'Condicion',
                          Value = 0L),
         validity =
         function(object){
           msg <- NULL
           if (!(object@Value %in%  c(0L, 1L)))
             msg <- c(msg, "[Condition:: validity FixedConditionParam] 'Value' must be an integer vector with values 0 or 1.")

           if (is.null(msg)) TRUE else msg
           }
         )






