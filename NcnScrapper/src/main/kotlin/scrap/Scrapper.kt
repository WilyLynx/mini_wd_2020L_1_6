package scrap

import Separator
import model.NcnData
import org.jsoup.Jsoup
import java.io.File

class Scrapper {
    private val baseUrl = "https://projekty.ncn.gov.pl/index.php?s="
    private val path = "..\\data\\grants.csv"


    fun run () {
        scrapExisting()
    }

    private fun scrapExisting() {
        File(path).bufferedWriter().use { out ->
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
        File(path).bufferedWriter().use { out ->
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

    private fun scrapData(i: Int): NcnData? {
        val url = baseUrl + i.toString()
        val document = Jsoup.connect(url).get()
        if (document != null) {
            val element = document.select("div.important").first()
            if (element != null) {
                return element.extractNcnData()
            }
        }
        return null
    }
}