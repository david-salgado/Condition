setClassUnion('characterOrNULL', c('character', 'NULL')); setClassUnion('integerOrNULL', c('integer', 'NULL'))
#' @title Constructor de objetos de clase \linkS4class{ConditionParam}
#'
#' @description This constructor returns an object of class \linkS4class{ConditionParam}.
#' The input parameter is a named \code{list} of objects of class \linkS4class{vector}. Notice
#' that the names of this \code{list} must be any of 'EditName', 'VarName', 'Value',
#' 'ConditionExp', 'by', 'keyVar', 'lag', 'na.imp', 'NPeriods' or 'ConditionName'.
#'
#' @param Param lista con los siguientes campos:
#'
#' \itemize{
#' \item Class \code{Vector} de tipo \code{integer} de longitud 1 el tipo de la
#' condición a crear.
#'
#' \item  EditName \code{Vector} de tipo \code{integer} de longitud 1 con el nombre
#' del Edit para el que se establece la condición.
#'
#' \item VarName \code{Vector} de tipo \code{integer} de longitud 1 con el nombre
#' de la variable sobre la que se establece la condición.
#'
#' \item Value  \code{Vector} de tipo \code{integer} con valores 1L y 0L, indicando
#' a qué unidades se debe o no aplicarse el control. Será \code{Value = 1L}
#' si queremos aplicarlo a todas la unidades.
#'
#' \item ConditionExp \code{vector} de tipo \code{character} con la expresión que
#' define la condición que se debe cumplir para que el control de validación
#' tenga efecto.
#'
#' \item by \code{NULL} o \code{vector} de tipo \code{character} con los nombres
#' de las variables por las que queremos agrupar. Por defecto toma el valor
#' \code{NULL}.
#'
#' \item keyVar \code{NULL} o \code{vector} de tipo \code{character} con los nombres
#' de las variables que se utilizan como clave. Por defecto toma el valor
#' \code{NULL}.
#'
#' \item lag \code{NULL} o \code{vector} de tipo \code{integer} de longitud 1 con
#' el valor del retardo. Por defecto toma el valor \code{2L}.
#'
#' \item NPeriods \code{Vector} de tipo \code{integer} de longitud 1 con el número
#' de periodos hacia atrás con los que vamos a calcular la mediana.
#'
#' \item na.imp \code{vector} de tipo \code{integer} de longitud 1, que indica a
#' que valor (0L o 1L) se imputan los valores NA.
#'
#' \item ConditionName \code{Vector} de tipo \code{integer} de longitud 1 con el
#' nombre asignado a la variable que contiene la condición. Por defecto, su valor
#' es "Condicion". En cualquier caso, debe corresponder a una de las variables
#' incluidas en la componente DD (diccionario de datos) del objeto de clase StQ o
#' StQList de entrada.
#' }
#'
#' @return An object of class \linkS4class{ConditionParam} with components specified in the input
#' parameter Data. Depending on the specified components, the output object will be
#' of classes \code{GenConditionParam}, \code{FixedConditionParam} or \code{TSConditionParam}.
#'
#' @seealso \code{\link[Condition:FixedConditionParam-class]{FixedConditionParam}}
#' \code{\link[Condition:TSConditionParam-class]{TSConditionParam}} \code{\link[Condition:GenConditionParam-class]{GenConditionParam}}
#'
#'
#' @examples
#' \dontrun{
#' CondParam <- BuildConditionParam(Class = 'FixedConditionParam',
#'                                  EditName = 'LCN_W_1',
#'                                  na.imp = 1L,
#'                                  value = 1L)
#' CondParam
#' }
#'
#' @include ConditionParam-class.R GenConditionParam-class.R FixedConditionParam-class.R TSConditionParam-class.R
#'
#' @import data.table StQ
#'
#' @export
BuildConditionParam <- function(Param = list(Class = 'ConditionParam',
                                             EditName,
                                             na.imp = 0L,
                                             ConditionName,
                                             VarName,
                                             Value,
                                             ConditionExp,
                                             by = NULL,
                                             keyVar = NULL,
                                             lag = NULL,
                                             NPeriods = integer(0))){

  if (is.null(Param$EditName)) stop("[Condition:: BuildConditionParam] 'EditName' must be a character vector of length 1.")
  #
  #
  if (is.null(Param$ConditionName)) ConditionName <- 'Condicion'

  if (Param$Class == 'GenConditionParam') ConditionParam <- new(Class = Param$Class,
                                                          EditName = Param$EditName,
                                                          na.imp = Param$na.imp,
                                                          ConditionName = Param$ConditionName,
                                                          ConditionExp = Param$ConditionExp,
                                                          by = Param$by,
                                                          keyVar = Param$keyVar,
                                                          lag = Param$lag)

  if (Param$Class == 'TSConditionParam') ConditionParam <- new(Class = Param$Class,
                                                            EditName = Param$EditName,
                                                            na.imp = Param$na.imp,
                                                            ConditionName = Param$ConditionName,
                                                            VarName = Param$VarName,
                                                            NPeriods = Param$NPeriods)

  if (Param$Class == 'FixedConditionParam') ConditionParam <- new(Class = Param$Class,
                                                                  EditName = Param$EditName,
                                                                  na.imp = Param$na.imp,
                                                                  ConditionName = Param$ConditionName,
                                                                  Value = Param$Value)

  return(ConditionParam)
}
