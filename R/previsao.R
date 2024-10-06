#' Função para prever novos valores usando coeficientes de um modelo existente.
#' @title Previsão
#'
#' @param coeficientes Um vetor de coeficientes obtidos da função regressao_linear.
#' @param new_data Um data frame contendo novos dados para previsão.
#' @return Um vetor de predições.
#' @export
previsao <- function(coeficientes, new_data) {
  if (missing(coeficientes) || is.null(coeficientes)) {
    stop("O parâmetro 'coeficientes' é necessário.")
  }
  if (!is.data.frame(new_data)) {
    stop("A entrada 'new_data' deve ser um data frame.")
  }

  new_X <- as.matrix(new_data)
  new_X <- cbind(1, new_X)  # Adiciona coluna para o intercepto
  predicoes <- new_X %*% coeficientes
  return(predicoes)
}
