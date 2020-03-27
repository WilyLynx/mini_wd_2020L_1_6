import org.jsoup.Jsoup
import java.io.File

const val BaseUrl = "https://projekty.ncn.gov.pl/index.php?s="
const val Path = "..\\data\\grants.csv"
const val Separator = "#"

fun main() {
    scrapExisting()
}

fun scrapExisting() {
    File(Path).bufferedWriter().use { out ->
        out.write(NcnData.headers(Separator))
        var i = 1
        while(true) {
            val data = scrapData(i)
            if (data != null) {
                out.newLine()
                out.write(data.toCsvRecord(Separator))
                println("Scrapped pages: $i")
            } else {
                break
            }
            i++
        }
    }
}

fun scrap(n: Int) {
    File(Path).bufferedWriter().use { out ->
        out.write(NcnData.headers(Separator))
        for (i in 1..n) {
            val data = scrapData(i)
            if (data != null) {
                out.newLine()
                out.write(data.toCsvRecord(Separator))
                println("Scrapped pages: $i")
            }
        }
    }
}

fun scrapData(i: Int): NcnData? {
    val url = BaseUrl + i.toString()
    val document = Jsoup.connect(url).get()
    if (document != null) {
        val element = document.select("div.important").first()
        if (element != null) {
            return element.extractNcnData()
        }
    }
    return null
}