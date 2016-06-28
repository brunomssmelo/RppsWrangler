#' Importacao de dados contidos no Demonstrativo da Politica de Investimentos (DPIN).
#'
#' Esta funcao extrai os dados contidos no Demonstrativo da Politica de Investimentos (DPIN)
#' dos Regimes Proprios de Previdencia Social disponibilizado, em formato pdf, nos sites de alguns
#' Institutos de Previdencia de entes federativos que possuem RPPS.
#'
#' @param arqDpin uma string contendo o nome do arquivo pdf correspondente ao "DPIN".
#' @return um objeto do tipo data.frame contendo o resumo da politica de investimentos do RPPS.
<<<<<<< HEAD
#' @author Bruno M. S. S. Melo
=======
#' @author Bruno M. S. S Melo
>>>>>>> origin/master
#' @details
#' A funcao extrai a tabela contendo o resumo da alocacao de recursos definida no "DPIN".
#' @examples
#' \dontrun{
#' dfDpin <- mtpsImportaDpin("DPIN.pdf")
#' }
#' @seealso \code{tabulizer::extract_tables}
#' @export
mtpsImportaDpin <- function(arqDpin) {

  lsExtractedDpin <- lapply(tabulizer::extract_tables(arqDpin), `Encoding<-`, 'UTF-8')

  fieldNames <- c(
    # "Renda Fixa - Art. 7º",
    "Títulos Tesouro Nacional - SELIC - Art. 7º, I, \"a\"",
    "FI 100% títulos TN - Art. 7º, I, \"b\"",
    "Operações Compromissadas - Art. 7º, II",
    "FI Renda Fixa/Referenciados RF - Art. 7º, III",
    "FI de Renda Fixa - Art. 7º, IV",
    "Poupança - Art. 7º, V",
    "FI em Direitos Creditórios – Aberto - Art. 7º, VI",
    "FI em Direitos Creditórios – Fechado - Art. 7º, VII, \"a\"",
    "FI Renda Fixa Crédito Privado - Art. 7º, VII, \"b\"",
    # "Renda Variável - Art. 8º",
    "FI Ações referenciados - Art. 8º, I",
    "FI de Índices Referenciados em Ações - Art. 8º, II",
    "FI em Ações - Art. 8º, III",
    "FI Multimercado - aberto - Art. 8º, IV",
    "FI em Participações - fechado - Art. 8º, V",
    "FI Imobiliário - cotas negociadas em bolsa - Art. 8º, VI"
  )

  tokens <- stringr::str_replace_all(iconv(enc2native(fieldNames), to = "ASCII//TRANSLIT"), "\"", "")

  numPdfPages <- length(lsExtractedDpin)

  fullText <- ""
  for (p in 1:numPdfPages) {

    page  <- lsExtractedDpin[[p]]
    fullText <- stringr::str_c(fullText,
                               stringr::str_c(apply(page, 1, stringr::str_c, collapse=''), collapse = " "))
  }
  fullText <- stringr::str_replace_all(iconv(enc2native(fullText), to = "ASCII//TRANSLIT"), "\"", "")

  # Localiza a posição dos tokens em todo o texto
  posTokens <- stringr::str_locate_all(fullText,tokens)

  # Cria vetor com "flags" indicando se determinado token foi encontrado
  hasToken <- sapply(posTokens, 'length')>0

  suppressWarnings({
    try({

      dfDpin <- data.frame()
      for (i in 1:length(posTokens)) {

        if (hasToken[i]) {

          strChunk <- stringr::str_sub(fullText,posTokens[[i]][2],nchar(fullText))

          valPos <- stringr::str_locate_all(strChunk,"[\\d]+,\\d\\d")

          dfDpin <- rbind(
            dfDpin,
            data.frame(
              SEGMENTO = ifelse(i < 10, "RENDA FIXA", "RENDA VARIAVEL"),
              TIPO = tokens[i],
              LIMITE_RESOLUCAO = as.numeric(stringr::str_replace(stringr::str_sub(strChunk, valPos[[1]][1,1], valPos[[1]][1,2]),",",".")),
              ESTRATEGIA_ALOCACAO = as.numeric(stringr::str_replace(stringr::str_sub(strChunk, valPos[[1]][2,1], valPos[[1]][2,2]),",","."))
            )
          )
        }
      }
    }, silent = T)}) #try
  return(dfDpin)
}
