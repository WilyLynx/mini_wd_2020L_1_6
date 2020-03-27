import model.ClearedNcnData
import model.NcnData
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

class Tester {

    fun run() {
        printDescriptors()
    }

    private fun checkContainsSeparator() {
        val filePath = "..\\data\\grants_fixed.csv"
        val data = NcnData.readFromFile(filePath, Separator)
            .map { it.toCsvRecord(Separator) }
            .filter { it.contains(";") }

        data.forEach { println(it) }
        println(data.size)
    }

    private fun printDescriptors() {
        val filePath = "..\\data\\grants_mapped.csv"

        val data = ClearedNcnData
            .readFromFile(filePath, ClearSeparator)
            .flatMap { it.descriptors.split(ClearListItemSeparator) }
            .distinct()
            .sorted()

        data.forEach { println(it) }
        println(data.size)
    }


    private fun minMaxDescriptors() {
        val filePath = "..\\data\\grants_fixed.csv"
        val descriptorData = NcnData.readFromFile(filePath, Separator)
            .flatMap {
                val d = it.start.split("-")
                val data = LocalDateTime.of(d[0].toInt(), d[1].toInt(), d[2].toInt(), 0, 0)
                it.descriptors
                    .split(ListItemSeparator)
                    .map { x -> x.split(": ") }
                    .map { x -> DescriptorData(Descriptor(x[0], x[1]), data) }
            }

        val minMaxInfo = descriptorData
            .groupBy { it.descriptor }
            .map {
                val desc = it.key
                val dates = it.value.map { d -> d.announced }
                val min = dates.min()
                val max = dates.max()
                DescriptorGroupData(desc, min, max)
            }

        minMaxInfo
            .sortedBy { it.announcedMax }
            .groupBy { it.descriptor.id }
            .filter { it.value.size > 1 }
            .forEach{
                println(it.key)
                it.value.forEach { s -> println(s)  }
            }
    }

    data class DescriptorData(
        val descriptor: Descriptor,
        val announced: LocalDateTime
    )

    data class DescriptorGroupData(
        val descriptor: Descriptor,
        val announcedMin: LocalDateTime?,
        val announcedMax: LocalDateTime?
    ) {
        private fun formatDate(date: LocalDateTime?): String {
            return if (date != null) {
                date.format(DateTimeFormatter.ISO_DATE)
            } else {
                "....-..-.."
            }
        }

        override fun toString(): String {
            val id = descriptor.id
            val idFormatted = id + " ".repeat(8 - id.length)
            val minFormatted = formatDate(announcedMin)
            val maxFormatted = formatDate(announcedMax)
            return "$idFormatted: $minFormatted - $maxFormatted ${descriptor.desc}"
        }
    }

    data class Descriptor(
        val id: String,
        val desc: String
    )
}