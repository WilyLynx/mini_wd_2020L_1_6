import java.io.File
import kotlin.reflect.full.memberProperties

data class ClearedNcnData(
    var descriptors: String,
    var panel: String,
    var coinvestigators: String,
    var contest: String,
    var announced: String,
    var budget: String,
    var start: String,
    var duration: String
) {
    override fun toString(): String {
        return ClearedNcnData::class.memberProperties.joinToString("\n") { "${it.name}: ${it.get(this)}" }
    }

    fun toCsvRecord(sep: String): String {
        return ClearedNcnData::class.memberProperties.joinToString(sep) { "\"${it.get(this).toString().trim()}\"" }
    }

    companion object {

        fun fromRecord(record: String, separator: String): ClearedNcnData {
            val array = record.subSequence(1, record.length - 1).split("\"$separator\"")
            return ClearedNcnData(
                announced = array[0],
                budget = array[1],
                coinvestigators = array[2],
                contest = array[3],
                descriptors = array[4],
                duration = array[5],
                panel = array[6],
                start = array[7]
            )
        }

        fun headers(sep: String) = ClearedNcnData::class.memberProperties.joinToString(sep) { "\"${it.name}\"" }

        fun writeToFile(filePath: String, separator: String, data: List<ClearedNcnData>) {
            File(filePath).bufferedWriter().use { out ->
                out.write(headers(separator))
                for (record in data) {
                    out.newLine()
                    out.write(record.toCsvRecord(separator))
                }
            }
        }

        fun readFromFile(filePath: String, sep: String): List<ClearedNcnData> {
            return File(filePath).readLines().drop(1).map { fromRecord(it, sep) }
        }
    }
}