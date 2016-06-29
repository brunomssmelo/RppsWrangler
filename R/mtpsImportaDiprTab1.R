#' Importacao de dados contidos no Demonstrativo de Informacoes Previdenciarias e Repasses (DIPR)
#'
#' Esta funcao extrai os dados contidos no DIPR dos Regimes Proprios de Previdencia Social disponibilizado,
#' em formato pdf, no site do Min. do Trabalho e Previdencia Social
#'
#' @param lsExtractedDipr saida da funcao \code{tabulizer::extract_tables} aplicada num arquivo pdf contendo a DIPR.
#' @return um objeto do tipo data.frame contendo a tabela desejada.
#' @author Bruno M. S. S Melo
#' @details
#' extrai tabela contendo as BASES DE CALCULO DAS CONTRIBUICOES DEVIDAS, RELATIVAS AS FOLHAS DO ENTE.
mtpsImportaDiprTab1 <- function(lsExtractedDipr) {

  # retira caracteres acentuados
  tab1 <- iconv(enc2native(lsExtractedDipr[[1]]), to = "ASCII//TRANSLIT")

  # retira separadores de milhares
  tab1 <- gsub(x = tab1, pattern = "([0-9])\\.([0-9])", replacement= "\\1\\2")

  # converte separador decimal de virgula para ponto
  tab1 <- gsub(x = tab1, pattern = "([0-9])\\,([0-9])", replacement= "\\1.\\2")

  # aglutina as colunas de tab1
  rawTable <- apply(tab1, 1, stringr::str_c, collapse='')

  # colapsa todo o texto da tabela numa unica string
  rawText <- stringr::str_c(rawTable, collapse = " ")

  token_desc <- c(
    "a) Aos servidores",
    "b) Aos servidores afastados com beneficios pagos pela Unidade Gestora (auxilio-",
    "c) Aos aposentados",
    "d) Aos pensionistas",
    "12 - Dos SERVIDORES",
    "13 - Dos APOSENTADOS",
    "14 - Dos PENSIONISTAS")

  full_desc <- c(
    "patronal realativa aos servidores",
    "patronal realativa aos servidores afastados com beneficios pagos pela Unidade Gestora",
    "patronal realativa aos aposentados",
    "patronal realativa aos pensionistas",
    "dos SERVIDORES",
    "dos APOSENTADOS",
    "dos PENSIONISTAS")

  token_bimestre <- c(
    "JAN/FEV", "MAR/ABR", "MAI/JUN",
    "JUL/AGO", "SET/OUT", "NOV/DEZ")

  #descobre o bimestre:
  bimestre <- which(sapply(stringr::str_locate_all(rawText,token_bimestre),'length')>0)

  dfTab <- data.frame()
  for (i in 1:length(rawTable)) {

    # descobre o tipo de informacao
    id <- which(sapply(stringr::str_locate_all(tab1[i,1], stringr::fixed(token_desc)),'length')>0)

    if (length(id)>0) {

      # encontra a posicao dos campos numericos e guarda num data.frame
      dfNumPos <- as.data.frame(stringr::str_locate_all(rawTable[i], pattern = "\\d+.\\d\\d"))

      # extrai os valores numericos
      values <- apply(dfNumPos, 1, function(x) {as.numeric(stringr::str_sub(rawTable[i], x[1], x[2]))})

      # completa com zeros
      values <- c(rep(0,6-length(values)),values)

      dfTab <- rbind(
        dfTab,
        data.frame(
          ID = id,
          DESCRICAO = full_desc[id],
          MES = bimestre*2-1,
          PLANO_PREV = values[1],
          PLANO_FIN = values[3],
          TOTAL_RPPS = values[5]
        ),
        data.frame(
          ID = id,
          DESCRICAO = full_desc[id],
          MES = bimestre*2,
          PLANO_PREV = values[2],
          PLANO_FIN = values[4],
          TOTAL_RPPS = values[6]
        )
      )
    }
  }

  dfTab <- dfTab[with(dfTab, order(MES, ID)),]
}

