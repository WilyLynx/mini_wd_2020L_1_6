const val OldSeparator = "*"
const val NewSeparator = "#"
const val NewListItemSeparator = ">"

fun main() {
    val sourcePath = "..\\data\\grants.csv"
    val resultPath = "..\\data\\grants_fixed.csv"

    val data = NcnData.readFromFile(sourcePath, OldSeparator)
    data.forEach { fixRecord(it) }
    NcnData.writeToFile(resultPath, NewSeparator, data)
}

fun fixRecord(record: NcnData) {
    if (record.coinvestigators.contains("<strong>")) {
        record.coinvestigators = ""
    }
    record.keywords = record.keywords.replace(ListItemSeparator, NewListItemSeparator)
    record.descriptors = record.descriptors.replace(ListItemSeparator, NewListItemSeparator)
}