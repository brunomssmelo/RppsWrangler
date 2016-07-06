#' Importacao de dados contidos no Demonstrativo da Politica de Investimentos (DPIN)
#'
#' Esta funcao extrai os dados contidos no Demonstrativo da Politica de Investimentos (DPIN)
#' dos Regimes Proprios de Previdencia Social disponibilizado, em formato pdf, nos sites de alguns
#' Institutos de Previdencia de entes federativos que possuem RPPS.
#'
#' @param arqNome uma string contendo o nome do arquivo correspondente ao "DPIN".
#' @param arqTipo uma string contendo o tipo do arquivo de origem (pdf ou xml)
#' @return um objeto do tipo data.frame contendo o resumo da politica de investimentos do RPPS.
#' @author Bruno M. S. S Melo
#' @details
#' A funcao extrai a tabela contendo o resumo da alocacao de recursos definida no "DPIN".
#' OBS.: Por enquanto a função so extrai de arquivos do tipo "pdf".
#' @examples
#' \dontrun{
#' dfDpin <- mtpsImportaDpin("DPIN.pdf")
#' }
#' @seealso \code{tabulizer::extract_tables}
#' @export
mtpsImportaDpin <- function(arqNome, arqTipo) {

  if (arqTipo == "pdf") {
    lsExtractedDpin <- lapply(tabulizer::extract_tables(arqNome), `Encoding<-`, 'UTF-8')
  } else {
    stop("Apenas arquivos do tipo pdf podem ser extraidos na presente versao.")
  }

  browser()

  fieldNames <- c(
    # "Renda Fixa - Art. 7º",
    "Titulos Tesouro Nacional - SELIC - Art. 7o, I, \"a\"",
    "FI 100% titulos TN - Art. 7o, I, \"b\"",
    "Operacoes Compromissadas - Art. 7o, II",
    "FI Renda Fixa/Referenciados RF - Art. 7o, III",
    "FI de Renda Fixa - Art. 7o, IV",
    "Poupanca - Art. 7o, V",
    "FI em Direitos Creditorios - Aberto - Art. 7o, VI",
    "FI em Direitos Creditorios - Fechado - Art. 7o, VII, \"a\"",
    "FI Renda Fixa Credito Privado - Art. 7o, VII, \"b\"",
    # "Renda Variável - Art. 8º",
    "FI Acoes referenciados - Art. 8o, I",
    "FI de Indices Referenciados em Acoes - Art. 8o, II",
    "FI em Acoes - Art. 8o, III",
    "FI Multimercado - aberto - Art. 8o, IV",
    "FI em Participacoes - fechado - Art. 8o, V",
    "FI Imobiliario - cotas negociadas em bolsa - Art. 8o, VI"
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
