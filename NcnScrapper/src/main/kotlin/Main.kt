import org.jsoup.Jsoup
import java.io.File

const val BaseUrl = "https://projekty.ncn.gov.pl/index.php?s="


fun main() {
    val path = "grants.csv"
    File(path).bufferedWriter().use { out ->
        out.write(ncnDataHeaders())
        out.newLine()
        for (i in 1..10) {
            val data = scrapData(i)
            if (data != null) {
                out.write(data.toCsvRecord())
                out.newLine()
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