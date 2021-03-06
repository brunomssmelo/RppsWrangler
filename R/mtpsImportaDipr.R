#' Importacao de dados contidos no Demonstrativo de Informacoes Previdenciarias e Repasses (DIPR)
#'
#' Esta funcao extrai os dados contidos no DIPR dos Regimes Proprios de Previdencia Social disponibilizado,
#' em formato pdf, no site do Min. do Trabalho e Previdencia Social
#'
#' @param caminho uma string contendo o nome do arquivo pdf correspondente ao "DIPR".
#' @param tipo uma string contendo o tipo da origem: pdf, dir.pdf, xml ou dir.xml)
#' @param opcao lista com as opcoes desejadas. Ver detalhes e exemplos abaixo.
#' @return uma lista de objetos do tipo data.frame contendo as tabelas desejadas.
#' @author Bruno M. S. S Melo
#' @details
#' A funcao extrai tabelas contidas no DIPR de acordo com o estabelecido no parametro \code{opcao}:
#' OBS.: Por enquanto a função so extrai de arquivos do tipo "pdf".
#' \itemize{
#'   \item \code{opcao = 1} extrai tabela contendo as BASES DE CALCULO DAS CONTRIBUICOES DEVIDAS, RELATIVAS AS FOLHAS DO ENTE.
#'   \item \code{opcao = 2} extrai tabela contendo as CONTRIBUICOES REPASSADAS.
#'   \item \code{opcao = 3} extrai tabela contendo as DEDUCOES.
#'   \item \code{opcao = 4} extrai tabela contendo os APORTES E TRANSFERENCIAS DE RECURSOS.
#'   \item \code{opcao = 5} extrai tabela contendo os PARCELAMENTOS.
#'   \item \code{opcao = 6} extrai tabela contendo as BASES DE CALCULO DAS CONTRIBUICOES DEVIDAS, RELATIVAS A FOLHA DA UNIDADE GESTORA
#'   \item \code{opcao = 7} extrai tabela contendo as CONTRIBUICOES ARRECADADAS PELA UNIDADE GESTORA
#'   \item \code{opcao = 8} extrai tabela contendo a REMUNERACAO BRUTA (somatorio das folhas do ENTE e da UNIDADE GESTORA)
#'   \item \code{opcao = 9} extrai tabela contendo o Nº DE BENEFICIARIOS (somatorio das folhas do ENTE e da UNIDADE GESTORA)
#' }
#' O parametro \code{tipo} indica o tipo da origem:
#' \itemize{
#'   \item \code{tipo = "pdf"} o \code{caminho} aponta para um arquivo do tipo pdf
#'   \item \code{tipo = "dir.pdf"} o \code{caminho} aponta para um diretorio contendo arquivos do tipo pdf
#'   \item \code{tipo = "xml"} o \code{caminho} aponta para um arquivo do tipo xml
#'   \item \code{tipo = "dir.xml"} o \code{caminho} aponta para um diretorio contendo arquivos do tipo xml
#' }
#' @examples
#' \dontrun{
#' tabList <- mtpsImportaDpin("caminho = DIPR.pdf", tipo = "pdf", opcao = 1)
#'
#' tabList <- mtpsImportaDpin("caminho = DIPR.pdf", tipo = "pdf", opcao = c(1,3,4)
#' }
#' @seealso \code{tm::readPDF}
#' @export
mtpsImportaDipr <- function(caminho, tipo, opcao) {

  tabList <- list()

  opcao <- intersect(1:9,opcao)

  if (length(opcao) < 1) {stop("Opcao nao fornecida ou invalida.")}

  if (tipo == "pdf") {
    # lsExtractedDipr <- lapply(tabulizer::extract_tables(caminho), `Encoding<-`, 'UTF-8')
    txtExtractedDipr <- utilExtractPdfText(uri = arqNome, enconding = 'UTF-8')
  } else {
    stop("Apenas arquivos do tipo pdf podem ser extraidos na presente versao.")
  }

  for (o in 1:length(opcao)) {

    switch (as.character(opcao[o]),
            "1" = {tabList[[length(tabList)+1]] <- mtpsImportaDiprTab1(txtExtractedDipr)},
            "2" = tabList, #{tabList[[length(tabList)+1]] <- mtpsImportaDiprTab2(caminho)}
            "3" = tabList, #{tabList[[length(tabList)+1]] <- mtpsImportaDiprTab3(caminho)}
            "4" = tabList, #{tabList[[length(tabList)+1]] <- mtpsImportaDiprTab4(caminho)}
            "5" = tabList, #{tabList[[length(tabList)+1]] <- mtpsImportaDiprTab5(caminho)}
            "6" = tabList, #{tabList[[length(tabList)+1]] <- mtpsImportaDiprTab6(caminho)}
            "7" = tabList, #{tabList[[length(tabList)+1]] <- mtpsImportaDiprTab7(caminho)}
            "8" = tabList, #{tabList[[length(tabList)+1]] <- mtpsImportaDiprTab8(caminho)}
            "9" = tabList  #{tabList[[length(tabList)+1]] <- mtpsImportaDiprTab9(caminho)}
    )
  }

  return(tabList)
}
