## two temporary files:
file1 <- tempfile(fileext = ".pdf")
file2 <- tempfile(fileext = ".pdf")

## Two identical graphs:
pdf(file1, compress = FALSE)
plot(mtcars)
dev.off()
Sys.sleep(1.5)
pdf(file2, compress = FALSE)
plot(mtcars)
dev.off()

## They have different timestamps
system2("diff", args = list(file1, file2))

## Remove the timestamps
index1 <- !(grepl("CreationDate", readLines(file1)) |
           grepl("ModDate", readLines(file1)))
writeLines(readLines(file1)[index1], file1)

index2 <- !(grepl("CreationDate", readLines(file2)) |
           grepl("ModDate", readLines(file2)))
writeLines(readLines(file2)[index1], file2)

## They are the same
system2("diff", args = list(file1, file2))

## And are still valid pdfs according to specification 1.4:
## https://www.adobe.com/content/dam/acom/en/devnet/pdf/pdfs/pdf_reference_archives/PDFReference.pdf
## and specification 1.7:
## https://www.adobe.com/content/dam/acom/en/devnet/acrobat/pdfs/PDF32000_2008.pdf
## which is what we print from.
browseURL(file1)
browseURL(file2)
