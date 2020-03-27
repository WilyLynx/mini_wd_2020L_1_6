package scrap

import ListItemSeparator
import Separator
import model.NcnData

class Fixer  {
    fun run() {
        val oldSeparator = ","
        val newSeparator = Separator
        val sourcePath = "..\\data\\grants.csv"
        val resultPath = "..\\data\\grants_fixed.csv"

        val data = NcnData.readFromFile(sourcePath, oldSeparator)
        data.forEach { fixRecord(it) }
        NcnData.writeToFile(resultPath, newSeparator, data)
    }

    fun fixRecord(record: NcnData) {
        val oldListItemSeparator = "|||"
        if (record.coinvestigators.contains("<strong>")) {
            record.coinvestigators = ""
        }
        record.keywords = record.keywords.replace(oldListItemSeparator, ListItemSeparator)
        record.descriptors = record.descriptors.replace(oldListItemSeparator, ListItemSeparator)
    }
}