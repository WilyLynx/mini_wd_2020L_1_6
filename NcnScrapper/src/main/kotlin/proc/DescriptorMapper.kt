package proc

import ClearListItemSeparator
import ClearSeparator
import model.ClearedNcnData

class DescriptorMapper {

    fun run() {
        val sourcePath = "..\\data\\grants_cleared.csv"
        val resultPath = "..\\data\\grants_mapped.csv"

        val data = ClearedNcnData.readFromFile(sourcePath, ClearSeparator)
        data.forEach { mapDescriptors(it) }
        ClearedNcnData.writeToFile(resultPath, ClearSeparator, data)
    }

    private fun mapDescriptors(record: ClearedNcnData) {
        record.descriptors = record.descriptors.split(ClearListItemSeparator).map { mapDescriptor(it) }.joinToString(ClearListItemSeparator)

    }

    private fun mapDescriptor(descriptor: String): String {
        val splitted = descriptor.split("_")
        val subPanel = splitted[0]
        val number = splitted[1]
        return subPanel + "_"+ "0".repeat(3 - number.length) + number
    }
}