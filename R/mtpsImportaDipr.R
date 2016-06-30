#' Importacao de dados contidos no Demonstrativo de Informacoes Previdenciarias e Repasses (DIPR)
#'
#' Esta funcao extrai os dados contidos no DIPR dos Regimes Proprios de Previdencia Social disponibilizado,
#' em formato pdf, no site do Min. do Trabalho e Previdencia Social
#'
#' @param arqNome uma string contendo o nome do arquivo pdf correspondente ao "DIPR".
#' @param arqTipo uma string contendo o tipo do arquivo de origem (pdf ou xml)
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
#' @examples
#' \dontrun{
#' tabList <- mtpsImportaDpin("arqNome = DIPR.pdf", arqTipo = "pdf", opcao = 1)
#'
#' tabList <- mtpsImportaDpin("arqNome = DIPR.pdf", arqTipo = "pdf", opcao = c(1,3,4)
#' }
#' @seealso \code{tabulizer::extract_tables}
#' @export
mtpsImportaDipr <- function(arqNome, arqTipo, opcao) {

  tabList <- list()

  opcao <- intersect(1:9,opcao)

  if (length(opcao) < 1) {stop("Opcao nao fornecida ou invalida.")}

  if (arqTipo == "pdf") {
    lsExtractedDipr <- lapply(tabulizer::extract_tables(arqNome), `Encoding<-`, 'UTF-8')
  } else {
    stop("Apenas arquivos do tipo pdf podem ser extraidos na presente versao.")
  }

  for (o in 1:length(opcao)) {

    switch (as.character(opcao[o]),
            "1" = {tabList[[length(tabList)+1]] <- mtpsImportaDiprTab1(lsExtractedDipr)},
            "2" = tabList, #{tabList[[length(tabList)+1]] <- mtpsImportaDiprTab2(arqNome)}
            "3" = tabList, #{tabList[[length(tabList)+1]] <- mtpsImportaDiprTab3(arqNome)}
            "4" = tabList, #{tabList[[length(tabList)+1]] <- mtpsImportaDiprTab4(arqNome)}
            "5" = tabList, #{tabList[[length(tabList)+1]] <- mtpsImportaDiprTab5(arqNome)}
            "6" = tabList, #{tabList[[length(tabList)+1]] <- mtpsImportaDiprTab6(arqNome)}
            "7" = tabList, #{tabList[[length(tabList)+1]] <- mtpsImportaDiprTab7(arqNome)}
            "8" = tabList, #{tabList[[length(tabList)+1]] <- mtpsImportaDiprTab8(arqNome)}
            "9" = tabList  #{tabList[[length(tabList)+1]] <- mtpsImportaDiprTab9(arqNome)}
    )
  }

  return(tabList)
}
