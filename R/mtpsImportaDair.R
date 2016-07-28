#' Importacao de dados contidos no DAIR disponiblizado no site do Min. do Trabalho e Previdencia Social.
#'
#' Esta funcao extrai os dados contidos no Demonstrativo de Aplicacoes e Investimentos dos Recursos (DAIR)
#' dos Regimes Proprios de Previdencia Social disponibilizado, em formato pdf, no site  do Min. do Trabalho e
#' Previdencia Social.
#'
#' @param caminho uma string contendo o nome do arquivo correspondente ao "DAIR". Caso o parametro "tipo"
#'        seja "dir.pdf" ou "dir.xml", devera ser passada uma lista de strings contendo o caminho para um
#'        ou mais diretorios.
#' @param tipo uma string contendo o tipo do arquivo de origem (pdf ou xml)
#' @return um objeto do tipo data.frame contendo a relacao das aplicacoes financeiras do RPPS.
#' @author Bruno M. S. S. Melo
#' @details
#' O parametro \code{tipo} indica o tipo da origem:
#' \itemize{
#'   \item \code{tipo = "pdf"} o \code{caminho} aponta para um arquivo do tipo pdf
#'   \item \code{tipo = "dir.pdf"} o \code{caminho} aponta para um diretorio contendo arquivos do tipo pdf
#'   \item \code{tipo = "xml"} o \code{caminho} aponta para um arquivo do tipo xml
#'   \item \code{tipo = "dir.xml"} o \code{caminho} aponta para um diretorio contendo arquivos do tipo xml
#' }
#' Obs. 1: Na presente versao apenas as aplicacoes financeiras realizadas em fundos de investimento sao extraidas.
#'
#' Obs. 2: Por enquanto apenas as origens dos tipos "pdf" e "dir.pdf" estao implementadas.
#'
#' Problema conhecido: Numa mesma sessao do R deve-se evitar a importacao de arquivos pdf de mesmo nome,
#' ainda que estejam em diretorios distintos, dado que apenas os dados do primeiro arquivo "homonino" serao
#' importados.
#' @examples
#' \dontrun{
#' dfDair <- mtpsImportaDair(caminho = "DAIR.pdf", tipo = "pdf")
#' }
#' @seealso \code{tabulizer::extract_tables}
#' @export
mtpsImportaDair <- function(caminho, tipo) {

  dfDair <- data.frame()

  if (tipo == "pdf") {
    dfDair <- mtpsImportaDairPdf(caminho[1])
  }

  browser()

  if (tipo == "dir.pdf") {

    pdfFiles <- NULL
    for (k in 1:length(caminho)) {
      if (!dir.exists(caminho[k])) {
        warning(paste( c("Diretorio ", caminho[k], " nao encontrado."), collapse = ""))

        next
      }

      pdfFiles <- c(
        pdfFiles,
        list.files(caminho[k], pattern = "(\\.[Pp][Dd][Ff]$)", full.names = T)
      )
    }

    dfDair <- do.call("rbind", lapply(pdfFiles, mtpsImportaDairPdf))

    # construcao de tabela de informacoes cadastrais

    # relaciona a posicao da 1a ocorrencia de cada fundo no data.frame unificado
    id <- !duplicated(dfDair$CNPJ_FUNDO)

    # constroi a tabela:
    dfCadastro <- dfDair[id,c("SEGMENTO",
                              "TIPO_ATIVO",
                              "INSTITUICAO_FINANCEIRA",
                              "CNPJ_INSTITUICAO_FINANCEIRA",
                              "FUNDO",
                              "CNPJ_FUNDO",
                              "INDICE_REFERENCIA")]


    # homogeiniza as informacoes dos fundos ao longo de todo o data.frame dfDair
    dfDair <- merge(
      x = dfDair[, c("ENTE", "DATA_POSICAO", "CNPJ_FUNDO",
                     "QTD_COTAS", "VALOR_ATUAL_COTA",
                     "VALOR_TOTAL_ATUAL", "PL_FUNDO",
                     "PERC_RECURSOS_RPPS", "PERC_PL_FUNDO")],
      y = dfCadastro,
      by.x = c("CNPJ_FUNDO"),
      by.y = c("CNPJ_FUNDO")
    )

  }

  dfDair$FUNDO <- iconv(enc2native(as.character(dfDair$FUNDO)), to = "ASCII//TRANSLIT")
  dfDair$CNPJ_FUNDO <- gsub("[./-]", "", as.character(dfDair$CNPJ_FUNDO))
  dfDair$CNPJ_INSTITUICAO_FINANCEIRA <- gsub("[./-]", "", as.character(dfDair$CNPJ_INSTITUICAO_FINANCEIRA))
  dfDair$INDICE_REFERENCIA <- as.character(dfDair$INDICE_REFERENCIA)
  dfDair$PERC_PL_FUNDO <- as.character(dfDair$PERC_PL_FUNDO)
  dfDair$PERC_RECURSOS_RPPS <- as.character(dfDair$PERC_RECURSOS_RPPS)
  dfDair$DATA_POSICAO <- as.Date(dfDair$DATA_POSICAO, format = "%d/%m/%Y")

  dfDair
}
