#' Extracao de texto contido em arquivo do tipo pdf.
#'
#' Esta funcao utiliza a funcao \code{readPDF}, do pacote \code{tm} (Text Mining) para extrair
#' o texto contido num arquivo pdf.
#'
#' @param uri uma string contendo o nome do arquivo (ou caminho completo) do arquivo pdf.
#' @return um objeto do tipo \code{character} contendo o texto extraido do arquivo pdf.
#' @author Bruno M. S. S. Melo
#' @examples
#' \dontrun{
#' fullText <- extractPdfText(uri = "DAIR.pdf", enconding = 'UTF-8')
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
