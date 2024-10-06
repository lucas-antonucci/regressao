#' Função para realizar regressão linear manualmente e retornar coeficientes, resíduos e gráficos.
#' @title Regressão Linear
#'
#' @param formula Uma fórmula representando o modelo de regressão linear.
#' @param data Um data frame contendo as variáveis na fórmula.
#' @return Uma lista contendo coeficientes, resíduos, valores preditos e gráficos.
#' @examples
#' data(mtcars)
#' resultado <- regressao_linear_manual(mpg ~ wt + hp, data = mtcars)
#' plot(resultado$grafico_predito_observado)
#' @export
regressao_linear <- function(formula, data) {

  if (!inherits(formula, "formula")) {
    stop("A entrada 'formula' deve ser uma fórmula.")
  }
  if (!is.data.frame(data)) {
    stop("A entrada 'data' deve ser um data frame.")
  }

  response_var <- as.character(formula[[2]])
  predictor_vars <- all.vars(formula)[-1]

  X <- as.matrix(data[predictor_vars])
  X <- cbind(1, X)
  y <- as.matrix(data[[response_var]])

  beta <- solve(t(X) %*% X) %*% t(X) %*% y

  y_hat <- X %*% beta

  residuos <- y - y_hat

  grafico_predito_observado <- plot(y_hat, y,
                                    xlab = "Valores Preditos",
                                    ylab = "Valores Observados",
                                    main = "Gráfico: Predito vs Observado")
  abline(0, 1, col = "red")

  resultado <- list(
    coeficientes = beta,
    residuos = residuos,
    valores_preditos = y_hat,
    grafico_predito_observado = grafico_predito_observado
  )

  return(resultado)
}


