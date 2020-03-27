package model

import java.io.File
import kotlin.reflect.full.memberProperties

data class NcnData(
    var title: String,
    var id: String,
    var keywords: String,
    var descriptors: String,
    var panel: String,
    var panelDescription: String,
    var institution: String,
    var voivodeship: String,
    var manager: String,
    var coinvestigators: String,
    var contest: String,
    var announced: String,
    var budget: String,
    var start: String,
    var duration: String,
    var status: String
) {
    override fun toString(): String {
        return NcnData::class.memberProperties.joinToString("\n") { "${it.name}: ${it.get(this)}" }
    }

    fun toCsvRecord(sep: String): String {
        return NcnData::class.memberProperties.joinToString(sep) { "\"${it.get(this).toString().trim()}\"" }
    }

    companion object {

        fun fromRecord(record: String, separator: String): NcnData {
            val array = record.subSequence(1, record.length - 1).split("\"$separator\"")
            return NcnData(
                announced = array[0],
                budget = array[1],
                coinvestigators = array[2],
                contest = array[3],
                descriptors = array[4],
                duration = array[5],
                id = array[6],
                institution = array[7],
                keywords = array[8],
                manager = array[9],
                panel = array[10],
                panelDescription = array[11],
                start = array[12],
                status = array[13],
                title = array[14],
                voivodeship = array[15]
            )
        }

        fun headers(sep: String) = NcnData::class.memberProperties.joinToString(sep) { "\"${it.name}\"" }

        fun writeToFile(filePath: String, separator: String, data: List<NcnData>) {
            File(filePath).bufferedWriter().use { out ->
                out.write(headers(separator))
                for (record in data) {
                    out.newLine()
                    out.write(record.toCsvRecord(separator))
                }
            }
        }

        fun readFromFile(filePath: String, sep: String): List<NcnData> {
            return File(filePath).readLines().drop(1).map {
                fromRecord(
                    it,
                    sep
                )
            }
        }
    }
}