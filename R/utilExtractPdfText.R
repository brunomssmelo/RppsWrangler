#' Importacao de dados contidos no DAIR disponiblizado no site do Min. do Trabalho e Previdencia Social.
#'
#' Esta funcao utiliza a funcao \code{readPDF}, do pacote \code{tm} (Text Mining) para extrair
#' o texto contido num arquivo pdf.
#'
#' @param uri uma string contendo o nome do arquivo correspondente ao "DAIR".
#' @return um objeto do tipo \code{character} contendo o texto extraido do arquivo pdf.
#' @author Bruno M. S. S. Melo
#' @examples
#' \dontrun{
#' dfDair <- fullText <- extractPdfText(uri = "DAIR.pdf", enconding = 'UTF-8')
#' }
#' @seealso \code{tm::readPDF}
utilExtractPdfText <- function(uri, enconding = 'UTF-8'){

  pdfText <- tm::readPDF(control = list(text = "-layout"))(elem = list(uri = uri),
                                                           language = "en",
                                                           id = "id1")
  pdfText <- paste(pdfText[[1]], collapse = ' ')

  Encoding(pdfText) <- enconding
  pdfText <- iconv(enc2native(pdfText), to = "ASCII//TRANSLIT")
}
